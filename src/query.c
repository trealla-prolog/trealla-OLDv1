#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#else
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

#define Trace if (q->trace /*&& !consulting*/) trace_call

static const unsigned INITIAL_NBR_HEAP_CELLS = 16000;
static const unsigned INITIAL_NBR_QUEUE_CELLS = 1000;
static const unsigned INITIAL_NBR_GOALS = 1000;
static const unsigned INITIAL_NBR_SLOTS = 1000;
static const unsigned INITIAL_NBR_CHOICES = 1000;
static const unsigned INITIAL_NBR_TRAILS = 1000;

unsigned g_string_cnt = 0, g_interned_cnt = 0;
int g_tpl_interrupt = 0;

typedef enum { CALL, EXIT, REDO, NEXT, FAIL } box_t;

// Note: when in commit there is a provisional choice point
// that we should skip over, hence the '1' ...

static bool any_choices(const query *q, const frame *f)
{
	if (q->cp == (q->in_commit ? 1 : 0))
		return false;

	const choice *ch = q->in_commit ? GET_PREV_CHOICE() : GET_CURR_CHOICE();
	return ch->cgen > f->cgen;
}

static void trace_call(query *q, cell *c, pl_idx_t c_ctx, box_t box)
{
	if (!c || !c->fn || is_empty(c))
		return;

#if 0
	if (is_builtin(c))
		return;
#endif

	if (box == CALL)
		box = q->retry?REDO:q->resume?NEXT:CALL;

#if 1
	const char *src = C_STR(q, c);

	if (!strcmp(src, ",") || !strcmp(src, ";") || !strcmp(src, "->") || !strcmp(src, "*->"))
		return;
#endif

	fprintf(stderr, " [%llu:f%u:cp%u] ", (unsigned long long)q->step++, q->st.curr_frame, q->cp);

	fprintf(stderr, "%s ",
		box == CALL ? "CALL" :
		box == EXIT ? "EXIT" :
		box == REDO ? "REDO" :
		box == NEXT ? "NEXT" :
		box == FAIL ? "FAIL":
		"????");

#if DEBUG
	frame *f = GET_CURR_FRAME();
	fprintf(stderr, "{ch%u:tp%u:fp%u:sp%u:hp%u} ",
		any_choices(q, f),
		q->st.tp, q->st.fp, q->st.sp, q->st.hp);
#endif

	int save_depth = q->max_depth;
	q->max_depth = 10;
	q->quoted = true;
	print_term(q, stderr, c, c_ctx, -1);
	q->quoted = false;
	fprintf(stderr, "\n");
	fflush(stderr);
	q->max_depth = save_depth;

	if (q->creep) {
		msleep(500);
	}
}

static USE_RESULT bool check_trail(query *q)
{
	if (q->st.tp > q->max_trails) {
		if (q->st.tp >= q->trails_size) {
			pl_idx_t new_trailssize = alloc_grow((void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*4/3);
			if (!new_trailssize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->trails_size = new_trailssize;
		}

		q->max_trails = q->st.tp;
	}

	return true;
}

static USE_RESULT bool check_choice(query *q)
{
	if (q->cp > q->max_choices) {
		if (q->cp >= q->choices_size) {
			pl_idx_t new_choicessize = alloc_grow((void**)&q->choices, sizeof(choice), q->cp, q->choices_size*4/3);
			if (!new_choicessize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->choices_size = new_choicessize;
		}

		q->max_choices = q->cp;
	}

	return true;
}

static USE_RESULT bool check_frame(query *q)
{
	if (q->st.fp > q->max_frames) {
		if (q->st.fp >= q->frames_size) {
			pl_idx_t new_framessize = alloc_grow((void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*4/3);
			if (!new_framessize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->frames_size = new_framessize;
		}

		q->max_frames = q->st.fp;
	}

	return true;
}

static USE_RESULT bool check_slot(query *q, unsigned cnt)
{
	pl_idx_t nbr = q->st.sp + cnt;

	if (nbr > q->max_slots) {
		while (nbr >= q->slots_size) {
			pl_idx_t new_slotssize = alloc_grow((void**)&q->slots, sizeof(slot), nbr, q->slots_size*4/3);
			if (!new_slotssize) {
				q->is_oom = q->error = true;
				return false;
			}

			memset(q->slots+q->slots_size, 0, sizeof(slot) * (new_slotssize - q->slots_size));
			q->slots_size = new_slotssize;
		}

		q->max_slots = nbr;
	}

	return true;
}

bool more_data(query *q, db_entry *dbe)
{
	if (!dbe->next)
		return false;

	const frame *f = GET_CURR_FRAME();
	const db_entry *next = dbe->next;

	while (next && !can_view(f, next))
		next = next->next;

	return next ? true : false;
}

static bool is_ground(const cell *c)
{
	pl_idx_t nbr_cells = c->nbr_cells;

	for (pl_idx_t i = 0; i < nbr_cells; i++, c++) {
		if (is_variable(c))
			return false;
	}

	return true;
}

static bool is_next_key(query *q, clause *r)
{
	if (q->st.iter) {
		if (map_is_next(q->st.iter, NULL))
			return true;

		return false;
	}

	if (!q->st.curr_clause->next || q->st.definite)
		return false;

	if (q->st.arg1_is_ground && r->arg1_is_unique)
		return false;

	if (q->st.arg2_is_ground && r->arg2_is_unique)
		return false;

	if (q->st.arg3_is_ground && r->arg3_is_unique)
		return false;

	db_entry *next = q->st.curr_clause->next;

	if (!next)
		return false;

	// Attempt look-ahead on 1st arg...

	r = &next->cl;

	if (q->st.arg1_is_ground && !next->next
		&& (q->st.key->arity == 1) && is_ground(r->cells+1)) {
		if (compare(q, q->st.key, q->st.curr_frame, r->cells, q->st.curr_frame)) {
			return false;
		}
	}

	return true;
}

static void next_key(query *q)
{
	if (q->st.iter) {
		if (!map_next(q->st.iter, (void*)&q->st.curr_clause)) {
			q->st.curr_clause = NULL;
			map_done(q->st.iter);
			q->st.iter = NULL;
		}
	} else if (!q->st.definite)
		q->st.curr_clause = q->st.curr_clause->next;
	else
		q->st.curr_clause = NULL;
}

const char *dump_id(const void *k, const void *v, const void *p)
{
	const query *q = (query*)p;
	uint64_t id = (uint64_t)k;
	static char tmpbuf[1024];
	sprintf(tmpbuf, "%llu", (unsigned long long)id);
	return tmpbuf;
}

static bool find_key(query *q, predicate *pr, cell *key)
{
	q->st.definite = false;
	q->st.arg1_is_ground = false;
	q->st.arg2_is_ground = false;
	q->st.arg3_is_ground = false;
	q->st.key = key;
	q->st.iter = NULL;

	if (!pr->idx) {
		q->st.curr_clause = pr->head;

		if (!key->arity || pr->is_multifile || pr->is_dynamic)
			return true;

		cell *arg1 = key + 1, *arg2 = NULL, *arg3 = NULL;

		if (key->arity > 1) {
			arg2 = arg1 + arg1->nbr_cells;

			if (key->arity > 2)
				arg3 = arg2 + arg2->nbr_cells;
		}

		arg1 = deref(q, arg1, q->st.curr_frame);

		if (arg2) {
			arg2 = deref(q, arg2, q->st.curr_frame);

			if (arg3)
				arg3 = deref(q, arg3, q->st.curr_frame);
		}

		if (q->pl->opt && is_ground(arg1))
			q->st.arg1_is_ground = true;

		if (q->pl->opt && arg2 && is_ground(arg2))
			q->st.arg2_is_ground = true;

		if (q->pl->opt && arg3 && is_ground(arg3))
			q->st.arg3_is_ground = true;

		return true;
	}

	//sl_dump(pr->idx, dump_key, q);

	may_error(init_tmp_heap(q));
	q->st.key = key = deep_clone_to_tmp(q, key, q->st.curr_frame);

	cell *arg1 = key->arity ? key + 1 : NULL;
	map *idx = pr->idx;

	if (arg1 && (is_variable(arg1) || pr->is_var_in_first_arg)) {
		if (!pr->idx2) {
			q->st.curr_clause = pr->head;
			return true;
		}

		cell *arg2 = arg1 + arg1->nbr_cells;

		if (is_variable(arg2)) {
			q->st.curr_clause = pr->head;
			return true;
		}

		q->st.key = key = arg2;
		idx = pr->idx2;
	}

#define DEBUGIDX 0

#if DEBUGIDX
	DUMP_TERM("search, term = ", key, q->st.curr_frame);
#endif

	q->st.curr_clause = NULL;
	miter *iter;

	if (!(iter = map_find_key(idx, key)))
		return false;

#define TESTINGIDX 0

#if TESTINGIDX

	// This returns results in index order but not database order
	// as is required for normal Prolog operations. Just used
	// for testing purposes only...

	if (!map_next_key(iter, (void*)&q->st.curr_clause)) {
		map_done(iter);
		return false;
	}

	q->st.iter = iter;
	return true;
#endif

	// If the index search has found just one (definite) solution
	// then we can use it with no problems. If more than one then
	// results must be returned in database order, so prefetch all
	// the results and return them sorted as an iterator...

	map *tmp_idx = NULL;
	const db_entry *dbe;
	unsigned cnt = 0;

	while (map_next_key(iter, (void*)&dbe)) {
		CHECK_INTERRUPT();

#if DEBUGIDX
		DUMP_TERM("   got, key = ", dbe->cl.cells, q->st.curr_frame);
#endif

		if (!tmp_idx) {
			tmp_idx = map_create(NULL, NULL, NULL);
			map_allow_dups(tmp_idx, false);
			map_set_tmp(tmp_idx);
		}

		map_app(tmp_idx, (void*)dbe->db_id, (void*)dbe);
		cnt++;
	}

	map_done(iter);

	//printf("*** cnt=%u\n", cnt);

	if (!tmp_idx)
		return false;

	//sl_dump(tmp_idx, dump_id, q);

	iter = map_first(tmp_idx);

	if (!map_next(iter, (void*)&q->st.curr_clause)) {
		map_done(iter);
		return false;
	}

	q->st.iter = iter;
	return true;
}

size_t scan_is_chars_list2(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes, bool *has_var, bool *is_partial)
{
	*is_partial = *has_var = false;
	size_t is_chars_list = 0;
	LIST_HANDLER(l);
	int cnt = 0;

	while (is_iso_list(l)
		&& (q->st.m->flags.double_quote_chars || allow_codes)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(l);
		cell *c = deref(q, h, l_ctx);

		if (is_variable(c)) {
			*has_var = true;
			return 0;
		}

		if (!is_integer(c) && !is_iso_atom(c)) {
			is_chars_list = 0;
			return 0;
		}

		if (is_integer(c) && !allow_codes) {
			is_chars_list = 0;
			return 0;
		}

		if (is_integer(c)) {
			int ch = get_int(c);
			char tmp[20];
			put_char_utf8(tmp, ch);
			size_t len = len_char_utf8(tmp);
			is_chars_list += len;
		} else {
			const char *src = C_STR(q, c);
			size_t len = len_char_utf8(src);

			if (len != C_STRLEN(q, c)) {
				is_chars_list = 0;
				return 0;
			}

			is_chars_list += len;
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
		cnt++;
	}

	if (is_variable(l)) {
		is_chars_list = 0;
		*has_var = *is_partial = true;
	} else if (is_string(l))
		;
	else if (!is_interned(l) || !is_nil(l))
		is_chars_list = 0;

	return is_chars_list;
}

size_t scan_is_chars_list(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes)
{
	bool has_var, is_partial;
	return scan_is_chars_list2(q, l, l_ctx, allow_codes, &has_var, &is_partial);
}

static void add_trail(query *q, pl_idx_t c_ctx, unsigned c_var_nbr, cell *attrs, pl_idx_t attrs_ctx)
{
	if (check_trail(q) != true) {
		q->error = false;
		return;
	}

	trail *tr = q->trails + q->st.tp++;
	tr->ctx = c_ctx;
	tr->var_nbr = c_var_nbr;
	tr->attrs = attrs;
	tr->attrs_ctx = attrs_ctx;
}

static void unwind_trail(query *q, const choice *ch)
{
	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + --q->st.tp;
		const frame *f = GET_FRAME(tr->ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		unshare_cell(&e->c);
		e->c.tag = TAG_EMPTY;
		e->c.attrs = tr->attrs;
		e->c.attrs_ctx = tr->attrs_ctx;
		e->mark = false;
	}
}

void undo_me(query *q)
{
	q->tot_retries++;
	const choice *ch = GET_CURR_CHOICE();
	unwind_trail(q, ch);
}

bool try_me(query *q, unsigned nbr_vars)
{
	may_error(check_slot(q, MAX_ARITY));
	frame *f = GET_FRAME(q->st.fp);
	f->nbr_slots = f->nbr_vars = nbr_vars;
	f->base_slot_nbr = q->st.sp;
	slot *e = GET_FIRST_SLOT(f);

	for (unsigned i = 0; i < nbr_vars; i++, e++) {
		//unshare_cell(&e->c);
		e->c.tag = TAG_EMPTY;
		e->c.attrs = NULL;
		e->mark = false;
	}

	q->run_hook = false;
	q->cycle_error = false;
	q->check_unique = false;
	q->has_vars = false;
	q->no_tco = false;
	q->tot_matches++;
	return true;
}

static void trim_heap(query *q, const choice *ch)
{
	for (page *a = q->pages; a;) {
		if (a->nbr < ch->st.curr_page)
			break;

		for (pl_idx_t i = 0; i < a->max_hp_used; i++) {
			cell *c = a->heap + i;
			unshare_cell(c);
			c->tag = TAG_EMPTY;
			c->attrs = NULL;
		}

		page *save = a;
		q->pages = a = a->next;
		free(save->heap);
		free(save);
	}

#if 0
	const page *a = q->pages;

	for (pl_idx_t i = ch->st.hp; a && (i < a->max_hp_used) && (i < q->st.hp); i++) {
		cell *c = a->heap + i;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
		c->attrs = NULL;
	}
#endif
}

void drop_choice(query *q)
{
	if (!q->cp)
		return;

	choice *ch = GET_CURR_CHOICE();

	if (ch->st.iter) {
		map_done(ch->st.iter);
		ch->st.iter = NULL;
	}

	--q->cp;
}

inline static pl_idx_t redo_choice(query *q)
{
	return q->cp ? --q->cp : 0;
}

bool retry_choice(query *q)
{
LOOP:

	if (!q->cp)
		return false;

	pl_idx_t curr_choice = redo_choice(q);
	const choice *ch = GET_CHOICE(curr_choice);
	unwind_trail(q, ch);

	if (ch->catchme_exception || ch->soft_cut || ch->did_cleanup)
		goto LOOP;

	trim_heap(q, ch);
	q->st = ch->st;
	q->save_m = NULL;

	frame *f = GET_CURR_FRAME();
	f->ugen = ch->ugen;
	f->cgen = ch->frame_cgen;
	f->nbr_vars = ch->nbr_vars;
	f->nbr_slots = ch->nbr_slots;
	f->overflow = ch->overflow;

	if (q->st.iter && false) {
		map_done(q->st.iter);
		q->st.iter = NULL;
	}

	return true;
}

static frame *push_frame(query *q, unsigned nbr_vars)
{
	pl_idx_t new_frame = q->st.fp++;
	const frame *curr_f = GET_CURR_FRAME();
	frame *f = GET_FRAME(new_frame);
	const cell *next_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;

	// Avoid long chains of useless returns...

	if (is_end(next_cell) && !next_cell->val_ret && curr_f->prev_cell) {
		f->prev_frame = curr_f->prev_frame;
		f->prev_cell = curr_f->prev_cell;
	} else {
		f->prev_frame = q->st.curr_frame;
		f->prev_cell = q->st.curr_cell;
	}

	f->cgen = ++q->cgen;
	f->is_complex = false;
	f->is_last = false;
	f->overflow = 0;
	q->st.sp += nbr_vars;
	q->st.curr_frame = new_frame;
	return f;
}

static void reuse_frame(query *q, unsigned nbr_vars)
{
	const frame *newf = GET_FRAME(q->st.fp);
	frame *f = GET_CURR_FRAME();

	const choice *ch = GET_CURR_CHOICE();
	q->st.sp = ch->st.sp;

	const slot *from = GET_FIRST_SLOT(newf);
	slot *to = GET_FIRST_SLOT(f);

	for (pl_idx_t i = 0; i < nbr_vars; i++) {
		unshare_cell(&to->c);
		*to++ = *from++;
	}

#if 0
	// If the new frame is smaller then the current one.
	// I don't think this is possible at the moment...

	for (unsigned i = nbr_vars; i < f->nbr_vars; i++, to++) {
		unshare_cell(&to->c);
		to->c.tag = TAG_EMPTY;
		to->c.attrs = NULL;
	}
#endif

	f->cgen = newf->cgen;
	f->nbr_slots = nbr_vars;
	f->nbr_vars = nbr_vars;
	f->overflow = 0;

	q->st.sp = f->base_slot_nbr + nbr_vars;
	q->tot_tcos++;
}

void trim_trail(query *q)
{
	if (q->undo_hi_tp)
		return;

	if (!q->cp) {
		q->st.tp = 0;
		return;
	}

	const choice *ch = GET_CURR_CHOICE();
	pl_idx_t tp = ch->st.tp;

	while (q->st.tp > tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static bool check_slots(const query *q, const frame *f, const clause *r)
{
	if (r != NULL) {
		if (f->nbr_vars != r->nbr_vars)
			return false;
	}

	for (unsigned i = 0; i < f->nbr_vars; i++) {
		const slot *e = GET_SLOT(f, i);

		if (is_indirect(&e->c) && (e->ctx != q->st.curr_frame))
			return false;

		if (is_managed(&e->c))
			return false;
	}

	return true;
}

void unshare_predicate(query *q, predicate *pr)
{
	if (!pr || !pr->ref_cnt)
		return;

	if (--pr->ref_cnt != 0)
		return;

	if (!pr->dirty_list)
		return;

	db_entry *dbe = pr->dirty_list;

	while (dbe) {
		// First unlink it from the predicate

		if (dbe->prev)
			dbe->prev->next = dbe->next;

		if (dbe->next)
			dbe->next->prev = dbe->prev;

		if (pr->head == dbe)
			pr->head = dbe->next;

		if (pr->tail == dbe)
			pr->tail = dbe->prev;

		// Now move it to query dirtylist

		db_entry *save = dbe->dirty;
		dbe->dirty = q->dirty_list;
		q->dirty_list = dbe;
		dbe = save;
	}

	pr->dirty_list = NULL;
}

static void commit_me(query *q, clause *r)
{
	q->in_commit = true;
	frame *f = GET_CURR_FRAME();
	f->mid = q->st.m->id;
	q->st.m = q->st.curr_clause->owner->m;
	bool recursive = is_tail_recursive(q->st.curr_cell);
	bool choices = any_choices(q, f);
	bool slots_ok = !q->retry && check_slots(q, f, r);
	bool implied_first_cut = q->check_unique && !q->has_vars && r->is_unique;
	bool last_match = implied_first_cut || r->is_first_cut || !is_next_key(q, r);
	bool tco = recursive && !choices && slots_ok && last_match;

#if 0
	printf("*** tco=%d, q->no_tco=%d, last_match=%d, rec=%d, any_choices=%d, slots_ok=%d, r->nbr_vars=%u, r->nbr_temporaries=%u\n",
		tco, q->no_tco, last_match, recursive, choices, slots_ok, r->nbr_vars, r->nbr_temporaries);
#endif

	// For now. It would also be good to reduce the number of
	// slots by the number of temporaries...

	if (q->no_tco && (r->nbr_vars != r->nbr_temporaries))
		tco = false;

	if (tco && q->pl->opt)
		reuse_frame(q, r->nbr_vars);
	else
		f = push_frame(q, r->nbr_vars);

	if (last_match) {
		f->is_complex = q->st.curr_clause->cl.is_complex;
		f->is_last = true;
		q->st.curr_clause = NULL;
		unshare_predicate(q, q->st.pr);
		drop_choice(q);
		trim_trail(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_clause = q->st.curr_clause;
		ch->cgen = f->cgen;
	}

	q->st.iter = NULL;
	q->st.curr_cell = get_body(r->cells);
	q->in_commit = false;
}

void stash_me(query *q, const clause *r, bool last_match)
{
	pl_idx_t cgen = q->cgen;

	if (last_match) {
		unshare_predicate(q, q->st.pr2);
		drop_choice(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_clause2 = q->st.curr_clause2;
		ch->cgen = cgen = ++q->cgen;
	}

	unsigned nbr_vars = r->nbr_vars;
	pl_idx_t new_frame = q->st.fp++;
	frame *f = GET_FRAME(new_frame);
	f->prev_frame = q->st.curr_frame;
	f->prev_cell = NULL;
	f->cgen = cgen;
	f->overflow = 0;

	q->st.sp += nbr_vars;
}

bool push_choice(query *q)
{
	may_error(check_choice(q));
	const frame *f = GET_CURR_FRAME();
	pl_idx_t curr_choice = q->cp++;
	choice *ch = GET_CHOICE(curr_choice);
	*ch = (choice){0};
	ch->st = q->st;
	ch->ugen = f->ugen;
	ch->frame_cgen = ch->cgen = f->cgen;
	ch->nbr_vars = f->nbr_vars;
	ch->nbr_slots = f->nbr_slots;
	ch->overflow = f->overflow;
	return true;
}

// A barrier is used when making a call, it sets a
// new choice generation so that normal cuts are contained.
// A '$inner_cut' though will also remove the barrier...

bool push_barrier(query *q)
{
	may_error(push_choice(q));
	frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();
	ch->cgen = f->cgen = ++q->cgen;
	ch->barrier = true;
	return true;
}

// Set a special flag so that '$cut_if_det' knows to also
// remove the barrier if it needs to...

bool push_call_barrier(query *q)
{
	may_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->call_barrier = true;
	return true;
}

bool push_catcher(query *q, enum q_retry retry)
{
	may_error(push_call_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->catcher = true;

	if (retry == QUERY_RETRY)
		ch->catchme_retry = true;
	else if (retry == QUERY_EXCEPTION)
		ch->catchme_exception = true;

	return true;
}

void cut_me(query *q, bool inner_cut, bool soft_cut)
{
	frame *f = GET_CURR_FRAME();

	while (q->cp) {
		choice *ch = GET_CURR_CHOICE();
		choice *save_ch = ch;

		while (soft_cut && (ch >= q->choices)) {
			if (ch->barrier && (ch->cgen == f->cgen)) {
				if (ch == save_ch) {
					f->cgen--;
					q->cp--;
					return;
				}

				ch->soft_cut = true;
				f->cgen--;
				return;
			}

			ch--;
		}

		// A normal cut can't break out of a barrier...

		if (!inner_cut && ch->barrier && (ch->cgen == f->cgen))
			break;

		// Whereas an inner cut can...

		if (ch->cgen < f->cgen) {
			if (inner_cut)
				f->cgen--;

			break;
		}

		if (ch->st.iter) {
			map_done(ch->st.iter);
			ch->st.iter = NULL;
		}

		unshare_predicate(q, ch->st.pr2);
		unshare_predicate(q, ch->st.pr);
		q->cp--;

		if (ch->register_cleanup && !ch->did_cleanup) {
			ch->did_cleanup = true;
			cell *c = ch->st.curr_cell;
			c = deref(q, c, ch->st.curr_frame);
			pl_idx_t c_ctx = q->latest_ctx;
			c = deref(q, c+1, c_ctx);
			c_ctx = q->latest_ctx;
			cell *tmp = deep_clone_to_heap(q, c, c_ctx);
			do_cleanup(q, tmp);
			break;
		}

#if 0
		if (ch->is_tail_rec) {
			printf("*** here2\n");
			frame *f_prev = GET_FRAME(f->prev_frame);
			f->prev_frame = f_prev->prev_frame;
			f->prev_cell = f_prev->prev_cell;
			*f_prev = *f;
			q->st.curr_frame--;
			q->st.fp--;
			q->tot_tcos++;
		}
#endif
	}

	if (!q->cp && !q->undo_hi_tp) {
		q->st.tp = 0;
	}
}

// If the call is det then the barrier can be dropped...

bool cut_if_det(query *q)
{
	const frame *f = GET_CURR_FRAME();
	const choice *ch = GET_CURR_CHOICE();

	if (ch->call_barrier && (ch->cgen == f->cgen)) {
		drop_choice(q);
		return true;
	}

	return false;
}

// Proceed to next goal in frame...

static void proceed(query *q)
{
	q->st.curr_cell += q->st.curr_cell->nbr_cells;

	while (is_end(q->st.curr_cell)) {
		if (q->st.curr_cell->val_ret) {
			frame *f = GET_CURR_FRAME();
			f->cgen = q->st.curr_cell->cgen;
			q->st.m = q->pl->modmap[q->st.curr_cell->mid];
		}

		if (!(q->st.curr_cell = q->st.curr_cell->val_ret))
			break;
	}
}

// Resume previous frame...

static bool resume_frame(query *q)
{
	if (!q->st.curr_frame)
		return false;

	const frame *f = GET_CURR_FRAME();

#if 0
	if (q->cp) {
		const choice *ch = GET_CURR_CHOICE();
		printf("*** resume f->cgen=%u, ch->cgen=%u\n", f->cgen, ch->cgen);
	}
#endif

#if 0
	if (q->st.curr_clause) {
		clause *r = &q->st.curr_clause->cl;

		if ((q->st.curr_frame == (q->st.fp-1))
			&& q->pl->opt
			&& r->is_tail_rec
			&& !any_choices(q, f)
			&& check_slots(q, f, r))
			q->st.fp--;
	}
#endif

#if 0
	//if ((q->st.curr_frame == (q->st.fp-1)) && f->is_last)
	//	fprintf(stderr, "*** resume f->is_last=%d, f->is_complex=%d, any_choices=%d\n", f->is_last, f->is_complex, any_choices(q, f));

	if ((q->st.curr_frame == (q->st.fp-1)) && f->is_last
		&& q->pl->opt
		&& !f->is_complex
		&& !any_choices(q, f)
		&& check_slots(q, f, NULL)) {
		//fprintf(stderr, "*** trim\n");
		q->st.fp--;
	}
#endif

	q->st.curr_cell = f->prev_cell;
	q->st.curr_frame = f->prev_frame;
	f = GET_CURR_FRAME();
	q->st.m = q->pl->modmap[f->mid];
	return true;
}

// NOTE: there is no reason this couldn't include a context.
// Currently it is assumed everything has been copied into the
// heap area and all variables are local. It should also be
// possible to make it an offset rather than a pointer by
// including the page nbr.

void make_indirect(cell *tmp, cell *c)
{
	tmp->tag = TAG_PTR;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_ptr = c;
}

#define MAX_VARS (1L<<30)

unsigned create_vars(query *q, unsigned cnt)
{
	frame *f = GET_CURR_FRAME();

	if (!cnt)
		return f->nbr_vars;

	if ((q->st.sp + cnt) > MAX_VARS) {
		printf("*** Ooops %s %d\n", __FILE__, __LINE__);
		return 0;
	}

	unsigned var_nbr = f->nbr_vars;

	if (check_slot(q, var_nbr+cnt) != true)
		return 0;

	if ((f->base_slot_nbr + f->nbr_slots) >= q->st.sp) {
		f->nbr_slots += cnt;
		q->st.sp = f->base_slot_nbr + f->nbr_slots;
	} else if (!f->overflow) {
		f->overflow = q->st.sp;
		q->st.sp += cnt;
	} else if ((f->overflow + (f->nbr_vars - f->nbr_slots)) == q->st.sp) {
		q->st.sp += cnt;
	} else {
		pl_idx_t save_overflow = f->overflow;
		f->overflow = q->st.sp;
		pl_idx_t cnt2 = f->nbr_vars - f->nbr_slots;
		memmove(q->slots+f->overflow, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2 + cnt;
	}

	slot *e = GET_SLOT(f, f->nbr_vars);

	for (unsigned i = 0; i < cnt; i++, e++) {
		e->c.tag = TAG_EMPTY;
		e->c.attrs = NULL;
		e->mark = false;
	}

	f->nbr_vars += cnt;
	return var_nbr;
}

void set_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	while (is_variable(&e->c)) {
		c = &e->c;
		c_ctx = e->ctx;
		f = GET_FRAME(c_ctx);
		e = GET_SLOT(f, c->var_nbr);
	}

	cell *c_attrs = is_empty(&e->c) ? e->c.attrs : NULL;
	pl_idx_t c_attrs_ctx = e->c.attrs_ctx;

	if (c_attrs)
		q->run_hook = true;

	if (q->cp || c_attrs)
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);

	if (is_structure(v)) {
		make_indirect(&e->c, v);
	} else {
		share_cell(v);
		e->c = *v;
	}

	if (is_structure(v)
		&& (c_ctx != q->st.curr_frame)
		&& (v_ctx == q->st.curr_frame))
			q->no_tco = true;

	e->c.flags &= ~FLAG_REF;
	e->ctx = v_ctx;

	if (q->flags.occurs_check != OCCURS_CHECK_FALSE)
		e->mark = true;
}

void reset_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx, bool trailing)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	while (is_variable(&e->c)) {
		c = &e->c;
		c_ctx = e->ctx;
		f = GET_FRAME(c_ctx);
		e = GET_SLOT(f, c->var_nbr);
	}

	if (is_structure(v)) {
		make_indirect(&e->c, v);
	} else {
		share_cell(v);
		e->c = *v;
	}

	e->ctx = v_ctx;

	if (q->cp && trailing)
		add_trail(q, c_ctx, c->var_nbr, NULL, 0);
}

// Match HEAD :- BODY.

USE_RESULT bool match_rule(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (!q->retry) {
		cell *head = deref(q, get_head(p1), p1_ctx);
		cell *c = head;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_function(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, NULL), found)
				return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

			q->st.curr_clause2 = NULL;
			return false;
		}

		if (!pr->is_dynamic)
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

		q->st.curr_clause2 = pr->head;
		share_predicate(q->st.pr2 = pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else {
		q->st.curr_clause2 = q->st.curr_clause2->next;
	}

	if (!q->st.curr_clause2) {
		unshare_predicate(q, q->st.pr2);
		return false;
	}

	may_error(check_frame(q));
	may_error(push_choice(q));
	cell *p1_body = deref(q, get_logical_body(p1), p1_ctx);
	cell *orig_p1 = p1;
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.curr_clause2; q->st.curr_clause2 = q->st.curr_clause2->next) {
		CHECK_INTERRUPT();

		if (!can_view(f, q->st.curr_clause2))
			continue;

		clause *r = &q->st.curr_clause2->cl;
		cell *c = r->cells;
		bool needs_true = false;
		p1 = orig_p1;
		cell *c_body = get_logical_body(c);

		if (p1_body && is_variable(p1_body) && !c_body) {
			p1 = deref(q, get_head(p1), p1_ctx);
			c = get_head(c);
			needs_true = true;
		}

		may_error(try_me(q, r->nbr_vars));

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			int ok;

			if (needs_true) {
				p1_body = deref(q, p1_body, p1_ctx);
				pl_idx_t p1_body_ctx = q->latest_ctx;
				cell tmp = (cell){0};
				tmp.tag = TAG_INTERNED;
				tmp.nbr_cells = 1;
				tmp.val_off = g_true_s;
				ok = unify(q, p1_body, p1_body_ctx, &tmp, q->st.curr_frame);
			} else
				ok = true;

			return ok;
		}

		undo_me(q);
	}

	drop_choice(q);
	unshare_predicate(q, q->st.pr2);
	return false;
}

// Match HEAD.
// Match HEAD :- true.

USE_RESULT bool match_clause(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *c = p1;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_function(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin(q->pl, C_STR(q, p1), p1->arity, &found, NULL), found) {
				if (is_retract != DO_CLAUSE)
					return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
				else
					return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			}

			q->st.curr_clause2 = NULL;
			return false;
		}

		if (!pr->is_dynamic) {
			if (is_retract == DO_CLAUSE)
				return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			else
				return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
		}

		q->st.curr_clause2 = pr->head;
		share_predicate(q->st.pr2=pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else {
		q->st.curr_clause2 = q->st.curr_clause2->next;
	}

	if (!q->st.curr_clause2) {
		unshare_predicate(q, q->st.pr2);
		return false;
	}

	may_error(check_frame(q));
	may_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.curr_clause2; q->st.curr_clause2 = q->st.curr_clause2->next) {
		CHECK_INTERRUPT();

		if (!can_view(f, q->st.curr_clause2))
			continue;

		clause *r = &q->st.curr_clause2->cl;
		cell *head = get_head(r->cells);
		cell *body = get_logical_body(r->cells);

		// Retract(HEAD) should ignore rules (and directives)

		if ((is_retract == DO_RETRACT) && body)
			continue;

		may_error(try_me(q, r->nbr_vars));

		if (unify(q, p1, p1_ctx, head, q->st.fp))
			return true;

		undo_me(q);
	}

	drop_choice(q);
	unshare_predicate(q, q->st.pr2);
	return false;
}

static USE_RESULT bool match_head(query *q)
{
	if (!q->retry) {
		cell *c = q->st.curr_cell;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_function(c)) {
			pr = search_predicate(q->st.m, c);
			q->save_m = q->st.m;

			if (!pr) {
				if (!is_end(c) && !(is_interned(c) && !strcmp(C_STR(q, c), "initialization")))
					if (q->st.m->flags.unknown == UNK_ERROR)
						return throw_error(q, c, q->st.curr_frame, "existence_error", "procedure");
					else
						return false;
				else
					q->error = true;

				return false;
			}

			c->match = pr;
		}

		find_key(q, pr, c);
		share_predicate(q->st.pr=pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else
		next_key(q);

	if (!q->st.curr_clause) {
		unshare_predicate(q, q->st.pr);
		return false;
	}

	may_error(check_frame(q));
	may_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.curr_clause; next_key(q)) {
		CHECK_INTERRUPT();

		if (!can_view(f, q->st.curr_clause))
			continue;

		clause *r = &q->st.curr_clause->cl;
		cell *head = get_head(r->cells);
		may_error(try_me(q, r->nbr_vars));

		if (unify(q, q->st.curr_cell, q->st.curr_frame, head, q->st.fp)) {
			if (q->error) {
				unshare_predicate(q, q->st.pr);
				drop_choice(q);
				return false;
			}

			commit_me(q, r);
			return true;
		}

		undo_me(q);
	}

	drop_choice(q);
	unshare_predicate(q, q->st.pr);
	return false;
}

static bool any_outstanding_choices(query *q)
{
	if (!q->cp)
		return false;

	const choice *ch = GET_CURR_CHOICE();

	while (ch->barrier) {
		drop_choice(q);
		ch--;
	}

	return q->cp ? true : false;
}

static bool consultall(query *q, cell *l, pl_idx_t l_ctx)
{
	if (is_string(l)) {
		char *s = DUP_STR(q, l);

		if (!load_file(q->p->m, s, false)) {
			free(s);
			return throw_error(q, l, q->st.curr_frame, "existence_error", "source_sink");
		}

		free(s);
		return true;
	}

	LIST_HANDLER(l);

	while (is_list(l)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_list(h)) {
			if (consultall(q, h, h_ctx) != true)
				return false;
		} else {
			char *s = DUP_STR(q, h);

			if (!load_file(q->p->m, s, false)) {
				free(s);
				return throw_error(q, h, q->st.curr_frame, "existence_error", "source_sink");
			}

			free(s);
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	return true;
}

bool start(query *q)
{
	q->yielded = false;
	bool done = false;

	while (!done && !q->error) {
#ifndef _WIN32
		if (g_tpl_interrupt == SIGALRM) {
			g_tpl_interrupt = 0;
			bool ok = throw_error(q, q->st.curr_cell, q->st.curr_frame, "time_limit_exceeded", "timed_out");

			if (ok == false)
				q->retry = true;

			continue;
		}
#endif

		if (g_tpl_interrupt) {
			int ok = check_interrupt(q);

			if (!q->st.curr_cell)
				break;

			switch (ok) {
				case 1:
					return true;
				case -1:
					q->retry = true;
				default:
					continue;
			}
		}

		if (q->retry) {
			Trace(q, q->st.curr_cell, q->st.curr_frame, FAIL);

			if (!retry_choice(q))
				break;
		}

		if (is_variable(q->st.curr_cell)) {
			if (!fn_call_0(q, q->st.curr_cell))
				continue;
		}

		q->tot_goals++;
		q->did_throw = false;
		Trace(q, q->st.curr_cell, q->st.curr_frame, CALL);
		cell *save_cell = q->st.curr_cell;
		pl_idx_t save_ctx = q->st.curr_frame;
		q->run_hook = q->cycle_error = false;
		q->before_hook_tp = q->st.tp;

		if (is_builtin(q->st.curr_cell)) {
			if (!q->st.curr_cell->fn) {					// NO-OP
				q->tot_goals--;
				q->st.curr_cell++;
				continue;
			}

			bool status;

#if USE_FFI
			if (q->st.curr_cell->fn_ptr && q->st.curr_cell->fn_ptr->ffi) {
				if (q->st.curr_cell->fn_ptr->function)
					status = wrapper_for_function(q, q->st.curr_cell->fn_ptr);
				else
					status = wrapper_for_predicate(q, q->st.curr_cell->fn_ptr);
			} else
#endif
				status = q->st.curr_cell->fn(q);

			if ((status == false) && !q->is_oom) {
				q->retry = QUERY_RETRY;

				if (q->yielded)
					break;

				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook && !q->in_hook)
				may_error(do_post_unification_hook(q, true));

			proceed(q);
		} else if (is_list(q->st.curr_cell)) {
			if (consultall(q, q->st.curr_cell, q->st.curr_frame) != true) {
				q->retry = true;
				continue;
			}

			proceed(q);
		} else {
			if (!is_callable(q->st.curr_cell)) {
				DISCARD_RESULT throw_error(q, q->st.curr_cell, q->st.curr_frame, "type_error", "callable");
			} else if ((match_head(q) != true) && !q->is_oom) {
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook && !q->in_hook) {
				may_error(do_post_unification_hook(q, false));
			}
		}

		q->run_hook = false;

		if (q->is_oom) {
			q->is_oom = q->error = false;

			if (throw_error(q, q->st.curr_cell, q->st.curr_frame, "resource_error", "memory") != true) {
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}
		}

		//if (g_tpl_interrupt)
		//	continue;

		Trace(q, save_cell, save_ctx, EXIT);
		q->resume = false;
		q->retry = QUERY_OK;

		while (!q->st.curr_cell || is_end(q->st.curr_cell)) {
			if (!resume_frame(q)) {
				while (q->cp) {
					choice *ch = GET_CURR_CHOICE();

					if (!ch->barrier)
						break;

					drop_choice(q);
				}

				if (q->p && !q->run_init && any_outstanding_choices(q)) {
					if (!check_redo(q))
						break;

					return true;
				}

				done = q->status = true;
				break;
			}

			q->resume = true;
			proceed(q);
		}
	}

	if (!q->p)
		return true;

	if (q->halt)
		q->error = false;
	else if (q->do_dump_vars && !q->abort && q->status && !q->error)
		dump_vars(q, false);

	return true;
}

#ifdef _WIN32

#define MS_PER_SEC      1000ULL     // MS = milliseconds
#define US_PER_MS       1000ULL     // US = microseconds
#define HNS_PER_US      10ULL       // HNS = hundred-nanoseconds (e.g., 1 hns = 100 ns)
#define NS_PER_US       1000ULL

#define HNS_PER_SEC     (MS_PER_SEC * US_PER_MS * HNS_PER_US)
#define NS_PER_HNS      (100ULL)    // NS = nanoseconds
#define NS_PER_SEC      (MS_PER_SEC * US_PER_MS * NS_PER_US)

static int clock_gettime_monotonic(struct timespec *tv)
{
	static LARGE_INTEGER ticksPerSec = {0};
	LARGE_INTEGER ticks;
	double seconds;

	if (!ticksPerSec.QuadPart) {
		QueryPerformanceFrequency(&ticksPerSec);
		if (!ticksPerSec.QuadPart) {
			errno = ENOTSUP;
			return -1;
		}
	}

	QueryPerformanceCounter(&ticks);
	seconds = (double) ticks.QuadPart / (double) ticksPerSec.QuadPart;
	tv->tv_sec = (time_t)seconds;
	tv->tv_nsec = (long)((ULONGLONG)(seconds * NS_PER_SEC) % NS_PER_SEC);
	return 0;
}

static int clock_gettime_realtime(struct timespec *tv)
{
	FILETIME ft;
	ULARGE_INTEGER hnsTime;
	GetSystemTimeAsFileTime(&ft);
	hnsTime.LowPart = ft.dwLowDateTime;
	hnsTime.HighPart = ft.dwHighDateTime;

	// To get POSIX Epoch as baseline, subtract the number of hns intervals from Jan 1, 1601 to Jan 1, 1970.
	hnsTime.QuadPart -= (11644473600ULL * HNS_PER_SEC);

	// modulus by hns intervals per second first, then convert to ns, as not to lose resolution
	tv->tv_nsec = (long) ((hnsTime.QuadPart % HNS_PER_SEC) * NS_PER_HNS);
	tv->tv_sec = (long) (hnsTime.QuadPart / HNS_PER_SEC);
	return 0;
}

static int my_clock_gettime(clockid_t type, struct timespec *tp)
{
	if (type == CLOCK_MONOTONIC)
		return clock_gettime_monotonic(tp);
	else if (type == CLOCK_REALTIME)
		return clock_gettime_realtime(tp);

    errno = ENOTSUP;
    return -1;
}
#else
#define my_clock_gettime clock_gettime
#endif

uint64_t cpu_time_in_usec(void)
{
	struct timespec now = {0};
	my_clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &now);
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

uint64_t get_time_in_usec(void)
{
	struct timespec now = {0};
	my_clock_gettime(CLOCK_REALTIME, &now);
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

bool execute(query *q, cell *cells, unsigned nbr_vars)
{
	q->pl->did_dump_vars = false;
	q->st.curr_cell = cells;
	q->st.sp = nbr_vars;
	q->abort = false;
	q->cycle_error = false;
	q->is_redo = false;

	// There is initially a frame (hence fp=0 is valid), so
	// this points to the next available frame...
	q->st.fp = 1;

	// There may not be a choice-point, so this points to the
	// next available choice-point
	q->cp = 0;

	frame *f = q->frames + q->st.curr_frame;
	f->nbr_vars = nbr_vars;
	f->nbr_slots = nbr_vars;
	f->ugen = ++q->pl->ugen;
	return start(q);
}

static void purge_dirty_list(query *q)
{
	int cnt = 0;

	while (q->dirty_list) {
		db_entry *dbe = q->dirty_list;
		q->dirty_list = dbe->dirty;
		clear_rule(&dbe->cl);
		free(dbe);
		cnt++;
	}

	//if (cnt) printf("Info: query purged %d\n", cnt);
}

void destroy_query(query *q)
{
	while (q->st.qnbr > 0) {
		free(q->tmpq[q->st.qnbr]);
		q->st.qnbr--;
	}

	for (page *a = q->pages; a;) {
		for (pl_idx_t i = 0; i < a->max_hp_used; i++) {
			cell *c = a->heap + i;
			unshare_cell(c);
		}

		page *save = a;
		a = a->next;
		free(save->heap);
		free(save);
	}

	for (int i = 0; i < MAX_QUEUES; i++) {
		for (pl_idx_t j = 0; j < q->qp[i]; j++) {
			cell *c = q->queue[i]+j;
			unshare_cell(c);
		}

		free(q->queue[i]);
	}

	slot *e = q->slots;

	for (pl_idx_t i = 0; i < q->st.sp; i++, e++)
		unshare_cell(&e->c);

	mp_int_clear(&q->tmp_ival);
	purge_dirty_list(q);
	free(q->trails);
	free(q->choices);
	free(q->slots);
	free(q->frames);
	free(q->tmp_heap);
	free(q);
}

query *create_query(module *m, bool is_task)
{
	static atomic_t uint64_t g_query_id = 0;

	query *q = calloc(1, sizeof(query));
	ensure(q);
	q->qid = g_query_id++;
	q->pl = m->pl;
	q->st.m = m;
	q->trace = m->pl->trace;
	q->flags = m->flags;
	q->time_started = q->get_started = get_time_in_usec();
	q->time_cpu_last_started = q->time_cpu_started = cpu_time_in_usec();
	q->st.prob = 1.0;
	mp_int_init(&q->tmp_ival);

	// Allocate these now...

	q->frames_size = is_task ? INITIAL_NBR_GOALS/10 : INITIAL_NBR_GOALS;
	q->slots_size = is_task ? INITIAL_NBR_SLOTS/10 : INITIAL_NBR_SLOTS;
	q->choices_size = is_task ? INITIAL_NBR_CHOICES/10 : INITIAL_NBR_CHOICES;
	q->trails_size = is_task ? INITIAL_NBR_TRAILS/10 : INITIAL_NBR_TRAILS;

	bool error = false;
	CHECK_SENTINEL(q->frames = calloc(q->frames_size, sizeof(frame)), NULL);
	CHECK_SENTINEL(q->slots = calloc(q->slots_size, sizeof(slot)), NULL);
	CHECK_SENTINEL(q->choices = calloc(q->choices_size, sizeof(choice)), NULL);
	CHECK_SENTINEL(q->trails = calloc(q->trails_size, sizeof(trail)), NULL);

	// Allocate these later as needed...

	q->h_size = is_task ? INITIAL_NBR_HEAP_CELLS/10 : INITIAL_NBR_HEAP_CELLS;
	q->tmph_size = is_task ? INITIAL_NBR_CELLS/10 : INITIAL_NBR_CELLS;

	for (int i = 0; i < MAX_QUEUES; i++)
		q->q_size[i] = is_task ? INITIAL_NBR_QUEUE_CELLS/10 : INITIAL_NBR_QUEUE_CELLS;

	if (error) {
		destroy_query (q);
		q = NULL;
	}

	return q;
}

query *create_sub_query(query *q, cell *curr_cell)
{
	query *subq = create_query(q->st.m, true);
	if (!subq) return NULL;
	subq->parent = q;
	subq->st.fp = 1;
	subq->is_task = true;

	cell *tmp = clone_to_heap(subq, 0, curr_cell, 1);
	pl_idx_t nbr_cells = tmp->nbr_cells;
	make_end(tmp+nbr_cells);
	subq->st.curr_cell = tmp;

	frame *fsrc = GET_FRAME(q->st.curr_frame);
	frame *fdst = subq->frames;
	fdst->nbr_vars = fsrc->nbr_vars;
	slot *e = GET_FIRST_SLOT(fsrc);

	for (unsigned i = 0; i < fsrc->nbr_vars; i++, e++) {
		cell *c = deref(q, &e->c, e->ctx);
		cell tmp = (cell){0};
		tmp.tag = TAG_VAR;
		tmp.var_nbr = i;
		tmp.val_off = g_anon_s;
		set_var(subq, &tmp, 0, c, q->latest_ctx);
	}

	subq->st.sp = fsrc->nbr_vars;
	return subq;
}

