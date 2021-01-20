#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#else
#include <unistd.h>
#endif

#include "internal.h"
#include "history.h"
#include "builtins.h"
#include "cdebug.h"

#define Trace if (q->trace /*&& !consulting*/) trace_call

int g_tpl_interrupt = 0;

#ifdef FAULTINJECT_ENABLED
faultinject_t FAULTINJECT_NAME;
#endif

typedef enum { CALL, EXIT, REDO, NEXT, FAIL } box_t;

#ifdef _WIN32
uint64_t get_time_in_usec(void)
{
	static const uint64_t epoch = 116444736000000000ULL;
	FILETIME file_time;
	SYSTEMTIME system_time;
	ULARGE_INTEGER u;
	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	u.LowPart = file_time.dwLowDateTime;
	u.HighPart = file_time.dwHighDateTime;
	return (u.QuadPart - epoch) / 10 + (1000ULL * system_time.wMilliseconds);
}
#else
uint64_t get_time_in_usec(void)
{
	struct timespec now;
	clock_gettime(CLOCK_REALTIME, &now);
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}
#endif

size_t alloc_grow(void** addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	assert(min_elements <= max_elements);
	FAULTINJECT(errno = ENOMEM; return 0);
	size_t elements = max_elements;
	void* mem;

	do {
		mem = realloc(*addr, elem_size * elements);
		if (mem) break;
		elements = min_elements + (elements-min_elements)/2;
		message("memory pressure reduce %lu to %lu", max_elements, elements);
	}
	 while (elements > min_elements);

	if (!mem)
		return 0;

	*addr = mem;
	return elements;
}

static USE_RESULT pl_state check_trail(query *q)
{
	if (q->st.tp > q->max_trails) {
		q->max_trails = q->st.tp;

		if (q->st.tp >= q->trails_size) {
			FAULTINJECT(errno = ENOMEM; q->error = true; return pl_error);

			idx_t new_trailssize = alloc_grow((void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*2);
			if (!new_trailssize) {
				q->error = true;
				return pl_error;
			}

			q->trails_size = new_trailssize;
		}
	}
	return pl_success;
}

static USE_RESULT pl_state check_choice(query *q)
{
	if (q->cp > q->max_choices) {
		q->max_choices = q->cp;

		if (q->cp >= q->choices_size) {
			FAULTINJECT(errno = ENOMEM; q->error = true; return pl_error);

			idx_t new_choicessize = alloc_grow((void**)&q->choices, sizeof(choice), q->cp, q->choices_size*2);
			if (!new_choicessize) {
				q->error = true;
				return pl_error;
			}

			q->choices_size = new_choicessize;
		}
	}
	return pl_success;
}

static USE_RESULT pl_state check_frame(query *q)
{
	if (q->st.fp > q->max_frames) {
		q->max_frames = q->st.fp;

		if (q->st.fp >= q->frames_size) {
			FAULTINJECT(errno = ENOMEM; q->error = true; return pl_error);

			idx_t new_framessize = alloc_grow((void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*2);
			if (!new_framessize) {
				q->error = true;
				return pl_error;
			}

			q->frames_size = new_framessize;
		}
	}
	return pl_success;
}

static USE_RESULT pl_state check_slot(query *q, unsigned cnt)
{
	idx_t nbr = q->st.sp + cnt + MAX_ARITY;

	if (nbr > q->max_slots) {
		q->max_slots = q->st.sp;

		if (nbr >= q->slots_size) {
			FAULTINJECT(errno = ENOMEM; q->error = true; return pl_error);

			idx_t new_slotssize = alloc_grow((void**)&q->slots, sizeof(slot), nbr, q->slots_size*2>nbr?q->slots_size*2:nbr);
			if (!new_slotssize) {
				q->error = true;
				return pl_error;
			}

			q->slots_size = new_slotssize;
		}
	}
	return pl_success;
}

static void trace_call(query *q, cell *c, box_t box)
{
	if (!c->fn)
		return;

	if (is_empty(c))
		return;

#if 0
	if (is_builtin(c))
		return;
#endif

	const char *src = GET_STR(c);

	if (!strcmp(src, ",") || !strcmp(src, ";") || !strcmp(src, "->"))
		return;

	fprintf(stderr, " [%llu] ", (unsigned long long)q->step++);
	fprintf(stderr, "%s ",
		box == CALL ? "CALL" :
		box == EXIT ? "EXIT" :
		box == REDO ? "REDO" :
		box == NEXT ? (isatty(2)?"\e[32mNEXT\e[0m" : "NEXT") :
		box == FAIL ? (isatty(2) ? "\e[31mFAIL\e[0m" : "FAIL") :
		"????");

#if DEBUG
	frame *g = GET_FRAME(q->st.curr_frame);
	fprintf(stderr, "{f(%u:v=%u:s=%u):ch%u:tp%u:cp%u:fp%u:sp%u:hp%u} ",
		q->st.curr_frame, g->nbr_vars, g->nbr_slots, g->any_choices,
		q->st.tp, q->cp, q->st.fp, q->st.sp, q->st.hp);
#endif

	int save_depth = q->max_depth;
	q->max_depth = 1000;
	print_term(q, stderr, c, q->st.curr_frame, -1);
	fprintf(stderr, "\n");
	fflush(stderr);
	q->max_depth = save_depth;
}

static void unwind_trail(query *q, const choice *ch)
{
	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + --q->st.tp;

		if (ch->pins) {
			if (ch->pins & (1 << tr->var_nbr))
				continue;
		}

		const frame *g = GET_FRAME(tr->ctx);
		slot *e = GET_SLOT(g, tr->var_nbr);
		FREE_STR(&e->c);
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = NULL;
	}
}

void undo_me(query *q)
{
	idx_t curr_choice = q->cp - 1;
	const choice *ch = q->choices + curr_choice;
	unwind_trail(q, ch);
}

void try_me(const query *q, unsigned nbr_vars)
{
	frame *g = GET_FRAME(q->st.fp);
	g->nbr_slots = nbr_vars;
	g->nbr_vars = nbr_vars;
	g->ctx = q->st.sp;
	slot *e = GET_SLOT(g, 0);

	for (unsigned i = 0; i < nbr_vars; i++, e++) {
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = NULL;
	}
}

static void trim_heap(query *q, const choice *ch)
{
	for (arena *a = q->arenas; a;) {
		if (a->nbr <= ch->st.anbr)
			break;

		for (idx_t i = 0; i < a->hp; i++) {
			cell *c = a->heap + i;
			FREE_STR(c);
			c->val_type = TYPE_EMPTY;
		}

		arena *save = a;
		q->arenas = a = a->next;
		free(save->heap);
		free(save);
	}

	const arena *a = q->arenas;

	for (idx_t i = ch->st.hp; a && (i < a->hp); i++) {
		cell *c = a->heap + i;
		FREE_STR(c);
		c->val_type = TYPE_EMPTY;
	}
}

idx_t drop_choice(query *q)
{
	idx_t curr_choice = --q->cp;
	return curr_choice;
}

bool retry_choice(query *q)
{
	if (!q->cp)
		return false;

	idx_t curr_choice = drop_choice(q);
	const choice *ch = q->choices + curr_choice;
	unwind_trail(q, ch);

	Trace(q, q->st.curr_cell, FAIL);

	if (ch->catchme2)
		return retry_choice(q);

	trim_heap(q, ch);
	sl_done(q->st.iter);
	q->st = ch->st;

	frame *g = GET_FRAME(q->st.curr_frame);
	g->nbr_vars = ch->nbr_vars;
	g->nbr_slots = ch->nbr_slots;
	g->any_choices = ch->any_choices;
	g->overflow = ch->overflow;
	return true;
}

static void make_frame(query *q, unsigned nbr_vars, bool last_match)
{
	frame *g = GET_FRAME(q->st.curr_frame);

	if (!last_match)
		g->any_choices = true;

	idx_t new_frame = q->st.fp++;
	g = GET_FRAME(new_frame);
	g->prev_frame = q->st.curr_frame;
	g->curr_cell = q->st.curr_cell;
	g->cgen = q->cgen;
	g->overflow = 0;
	g->any_choices = false;
	g->did_cut = false;

	q->st.sp += nbr_vars;
	q->st.curr_frame = new_frame;
}

static void trim_trail(query *q)
{
	idx_t curr_choice = q->cp - 1;
	const choice *ch = q->choices + curr_choice;

	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static void reuse_frame(query *q, unsigned nbr_vars)
{
	frame *g = GET_FRAME(q->st.curr_frame);
	g->nbr_slots = nbr_vars;
	g->nbr_vars = nbr_vars;
	g->overflow = 0;
	g->any_choices = true;
	g->did_cut = false;

	idx_t curr_choice = q->cp - 1;
	const choice *ch = q->choices + curr_choice;
	q->st.sp = ch->st.sp;

	if (!q->no_tco && q->m->opt) {
		frame *new_g = GET_FRAME(q->st.fp);

		for (unsigned i = 0; i < nbr_vars; i++) {
			slot *e = GET_SLOT(g, i);
			FREE_STR(&e->c);
		}

		slot *from = GET_SLOT(new_g, 0);
		slot *to = GET_SLOT(g, 0);
		memmove(to, from, sizeof(slot)*nbr_vars);
		q->st.sp = g->ctx + nbr_vars;
	} else {
		g->ctx = q->st.sp;
		q->st.sp += nbr_vars;
	}

	if (q->cp)
		trim_trail(q);
	else
		q->st.tp = 0;

	q->tot_tcos++;
}

static bool check_slots(const query *q, frame *g, term *t)
{
	if (t && (g->nbr_vars != t->nbr_vars))
		return false;

	for (unsigned i = 0; i < g->nbr_vars; i++) {
		const slot *e = GET_SLOT(g, i);

		if (is_indirect(&e->c) || is_string(&e->c))
			return false;
	}

	return true;
}

static void commit_me(query *q, term *t)
{
	frame *g = GET_FRAME(q->st.curr_frame);
	g->m = q->m;
	q->m = q->st.curr_clause->m;
	q->st.iter = NULL;
	bool last_match = !q->st.curr_clause->next || t->first_cut;
	bool recursive = (last_match || g->did_cut) && (q->st.curr_cell->flags&FLAG_TAIL_REC);
	bool tco = recursive && !g->any_choices && check_slots(q, g, t);

	if (last_match) {
		idx_t curr_choice = q->cp - 1;
		choice *ch = q->choices + curr_choice;
		sl_done(ch->st.iter);
		drop_choice(q);
	} else {
		idx_t curr_choice = q->cp - 1;
		choice *ch = q->choices + curr_choice;
		ch->st.curr_clause = q->st.curr_clause;
	}

	if (tco && q->cp)
		reuse_frame(q, t->nbr_vars);
	else
		make_frame(q, t->nbr_vars, last_match);

	if (t->cut_only)
		q->st.curr_cell = NULL;
	else
		q->st.curr_cell = get_body(t->cells);

	memset(q->nv_mask, 0, MAX_ARITY);
}

void stash_me(query *q, term *t, bool last_match)
{
	if (last_match)
		drop_choice(q);
	else {
		frame *g = GET_FRAME(q->st.curr_frame);
		g->any_choices = true;
		idx_t curr_choice = q->cp - 1;
		choice *ch = q->choices + curr_choice;
		ch->st.curr_clause2 = q->st.curr_clause2;
	}

	unsigned nbr_vars = t->nbr_vars;
	idx_t new_frame = q->st.fp++;
	frame *g = GET_FRAME(new_frame);
	g->prev_frame = q->st.curr_frame;
	g->curr_cell = NULL;
	g->cgen = q->cgen;
	g->overflow = 0;
	g->any_choices = false;
	g->did_cut = false;

	q->st.sp += nbr_vars;
}

pl_state make_choice(query *q)
{
	may_error(check_frame(q));
	may_error(check_choice(q));

	idx_t curr_choice = q->cp++;
	choice *ch = q->choices + curr_choice;
	ch->st = q->st;
	ch->cgen = ++q->cgen;
	ch->local_cut = false;
	ch->catchme1 = false;
	ch->catchme2 = false;
	ch->pins = 0;

	frame *g = GET_FRAME(q->st.curr_frame);
	may_error(check_slot(q, g->nbr_vars));
	ch->nbr_vars = g->nbr_vars;
	ch->nbr_slots = g->nbr_slots;
	ch->any_choices = g->any_choices;
	ch->overflow = g->overflow;

	return pl_success;
}

pl_state make_barrier(query *q)
{
	may_error(make_choice(q));
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	ch->local_cut = true;
	return pl_success;
}

pl_state make_catcher(query *q, enum q_retry retry)
{
	may_error(make_choice(q));
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;

	if (retry == QUERY_RETRY)
		ch->catchme1 = true;
	else if (retry == QUERY_EXCEPTION)
		ch->catchme2 = true;

	return pl_success;
}

void cut_me(query *q, bool local_cut)
{
	frame *g = GET_FRAME(q->st.curr_frame);
	g->any_choices = !local_cut;	// ???

	if (!local_cut)
		g->did_cut = true;

	while (q->cp) {
		idx_t curr_choice = q->cp - 1;
		choice *ch = q->choices + curr_choice;

		//printf("*** ch->cgen=%u, g->cgen=%u, q->cgen=%u\n", ch->cgen, g->cgen, q->cgen);

		if (ch->cgen < g->cgen)
			break;

		if (ch->st.qnbr != q->st.qnbr) {
			free(q->tmpq[q->st.qnbr]);
			q->tmpq[q->st.qnbr] = NULL;
			q->st.qnbr = ch->st.qnbr;
		}

		sl_done(ch->st.iter);
		q->cp--;

		if ((ch->local_cut && local_cut) &&
			(ch->cgen == q->st.curr_cell->cgen))
			break;
	}

	if (!q->cp)
		q->st.tp = 0;
}

static void follow_me(query *q)
{
	q->st.curr_cell += q->st.curr_cell->nbr_cells;

	while (q->st.curr_cell && is_end(q->st.curr_cell))
		q->st.curr_cell = q->st.curr_cell->val_ptr;
}

static bool resume_frame(query *q)
{
	if (!q->st.curr_frame)
		return false;

	frame *g = GET_FRAME(q->st.curr_frame);

#if 0
	int det = check_slots(q, g, NULL);

	if (det && !g->any_choices && (q->st.curr_frame == (q->st.fp-1)) && q->m->opt)
		q->st.fp--;
#endif

	cell *curr_cell = g->curr_cell;
	g = GET_FRAME(q->st.curr_frame=g->prev_frame);
	q->st.curr_cell = curr_cell;
	q->m = g->m;
	return true;
}

void make_indirect(cell *tmp, cell *c)
{
	tmp->val_type = TYPE_INDIRECT;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_ptr = c;
}

unsigned create_vars(query *q, unsigned cnt)
{
	frame *g = GET_FRAME(q->st.curr_frame);

	if (!cnt)
		return g->nbr_vars;

	unsigned var_nbr = g->nbr_vars;

	may_error(check_slot(q, cnt));

	if ((g->ctx + g->nbr_slots) >= q->st.sp) {
		g->nbr_slots += cnt;
		q->st.sp = g->ctx + g->nbr_slots;
	} else if (!g->overflow) {
		g->overflow = q->st.sp;
		q->st.sp += cnt;
	} else if ((g->overflow + (g->nbr_vars - g->nbr_slots)) == q->st.sp) {
		q->st.sp += cnt;
	} else {
		idx_t save_overflow = g->overflow;
		g->overflow = q->st.sp;
		idx_t cnt2 = g->nbr_vars - g->nbr_slots;
		memmove(q->slots+g->overflow, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2 + cnt;
	}

	for (unsigned i = 0; i < cnt; i++) {
		slot *e = GET_SLOT(g, g->nbr_vars+i);
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = NULL;
	}

	g->nbr_vars += cnt;
	return var_nbr;
}

pl_state set_var(query *q, cell *c, idx_t c_ctx, cell *v, idx_t v_ctx)
{
	frame *g = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(g, c->var_nbr);
	cell *frozen = NULL;

	if (is_empty(&e->c) && e->c.attrs && !is_list_or_nil(e->c.attrs))
		frozen = e->c.attrs;

	e->ctx = v_ctx;

	if (is_structure(v))
		make_indirect(&e->c, v);
	else if (is_nonconst_blob(v)) {
		e->c = *v;

		if (is_tmp(v)) {
			TAKE_STR(v);
			e->c.flags &= ~FLAG_TMP;
		} else
			DUP_STR(&e->c,v);
	} else
		e->c = *v;

	if (frozen)
		call_attrs(q, frozen);

	if (!q->cp)
		return pl_success;

	may_error (check_trail(q));
	trail *tr = q->trails + q->st.tp++;
	tr->var_nbr = c->var_nbr;
	tr->ctx = c_ctx;
	return pl_success;
}

void reset_value(query *q, cell *c, idx_t c_ctx, cell *v, idx_t v_ctx)
{
	frame *g = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(g, c->var_nbr);

	while (is_variable(&e->c)) {
		c = &e->c;
		c_ctx = e->ctx;
		g = GET_FRAME(c_ctx);
		e = GET_SLOT(g, c->var_nbr);
	}

	e->ctx = v_ctx;

	if (v->arity && !is_string(v))
		make_indirect(&e->c, v);
	else if (is_nonconst_blob(v)) {
		e->c = *v;

		if (is_tmp(v)) {
			TAKE_STR(v);
			e->c.flags &= ~FLAG_TMP;
		} else
			DUP_STR(&e->c,v);
	} else
		e->c = *v;
}

bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);

static bool unify_structure(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth)
{
	if (p1->arity != p2->arity)
		return false;

	if (p1->val_off != p2->val_off)
		return false;

	unsigned arity = p1->arity;
	p1++; p2++;

	while (arity--) {
		cell *c1 = deref(q, p1, p1_ctx);
		idx_t c1_ctx = q->latest_ctx;
		cell *c2 = deref(q, p2, p2_ctx);
		idx_t c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return true;
}

static bool unify_int(__attribute__((unused)) query *q, cell *p1, cell *p2)
{
	if (is_rational(p2))
		return (p1->val_num == p2->val_num) && (p1->val_den == p2->val_den);

	return false;
}

static bool unify_float(__attribute__((unused)) query *q, cell *p1, cell *p2)
{
	if (is_float(p2))
		return p1->val_flt == p2->val_flt;

	return false;
}

static bool unify_literal(query *q, cell *p1, cell *p2)
{
	if (is_literal(p2))
		return p1->val_off == p2->val_off;

	if (is_cstring(p2) && (LEN_STR(p1) == LEN_STR(p2)))
		return !memcmp(GET_STR(p2), QUERY_GET_POOL(p1->val_off), LEN_STR(p1));

	return false;
}

static bool unify_cstring(query *q, cell *p1, cell *p2)
{
	if (is_cstring(p2) && (LEN_STR(p1) == LEN_STR(p2)))
		return !memcmp(GET_STR(p1), GET_STR(p2), LEN_STR(p1));

	if (is_literal(p2) && (LEN_STR(p1) == LEN_STR(p2)))
		return !memcmp(GET_STR(p1), QUERY_GET_POOL(p2->val_off), LEN_STR(p1));

	return false;
}

static bool unify_list(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth)
{
	LIST_HANDLER(p1);
	LIST_HANDLER(p2);

	while (is_list(p1) && is_list(p2)) {
		cell *h1 = LIST_HEAD(p1);
		cell *c1 = deref(q, h1, p1_ctx);
		idx_t c1_ctx = q->latest_ctx;

		cell *h2 = LIST_HEAD(p2);
		cell *c2 = deref(q, h2, p2_ctx);
		idx_t c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

struct dispatch {
	uint8_t val_type;
	bool (*fn)(query*, cell*, cell*);
};

static const struct dispatch g_disp[] =
{
	{TYPE_EMPTY, NULL},
	{TYPE_VARIABLE, NULL},
	{TYPE_LITERAL, unify_literal},
	{TYPE_CSTRING, unify_cstring},
	{TYPE_INTEGER, unify_int},
	{TYPE_BIGNUM, NULL},
	{TYPE_FLOAT, unify_float},
	{0}
};

bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth)
{
	if (!depth)
		q->cycle_error = false;

	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return false;
	}

	if (is_variable(p1) && is_variable(p2)) {
		if (p2_ctx > p1_ctx)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2_ctx < p1_ctx)
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		else if (p2->var_nbr != p1->var_nbr)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		return true;
	}

	if (is_variable(p1)) {
		if (is_structure(p2) && (p2_ctx >= q->st.curr_frame))
			q->no_tco = true;

		set_var(q, p1, p1_ctx, p2, p2_ctx);
		return true;
	}

	if (is_variable(p2)) {
		if (is_structure(p1) && (p1_ctx >= q->st.curr_frame))
			q->no_tco = true;

		set_var(q, p2, p2_ctx, p1, p1_ctx);
		return true;
	}

	if (is_string(p1) && is_string(p2))
		return unify_cstring(q, p1, p2);

	if (is_list(p1) && is_list(p2))
		return unify_list(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	if (p1->arity || p2->arity)
		return unify_structure(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	return g_disp[p1->val_type].fn(q, p1, p2);
}

// Match HEAD :- BODY.

USE_RESULT pl_state match_rule(query *q, cell *p1, idx_t p1_ctx)
{
	if (q->retry) {
		q->st.curr_clause2 = q->st.curr_clause2->next;
	} else {
		cell *head = get_head(p1);
		cell *c = head;

		if (!is_literal(c)) {
			// For now convert it to a literal
			idx_t off = index_from_pool(q->m->pl, GET_STR(c));
			may_idx_error(off);
			FREE_STR(c);
			c->val_off = off;
			c->val_type = TYPE_LITERAL;
			c->flags = 0;
		}

		predicate *h = find_matching_predicate(q->m, head);

		if (!h) {
			if (check_builtin(q->m->pl, GET_STR(head), head->arity))
				return throw_error(q, head, "permission_error", "modify,static_procedure");

			q->st.curr_clause2 = NULL;
			return false;
		} else {
			if (!h->is_dynamic)
				return throw_error(q, head, "permission_error", "modify,static_procedure");

			q->st.curr_clause2 = h->head;
		}
	}

	if (!q->st.curr_clause2)
		return pl_failure;

	may_error(make_choice(q));
	cell *p1_body = get_logical_body(p1);
	cell *orig_p1 = p1;

	for (; q->st.curr_clause2; q->st.curr_clause2 = q->st.curr_clause2->next) {
		if (q->st.curr_clause2->t.deleted)
			continue;

		term *t = &q->st.curr_clause2->t;
		cell *c = t->cells;
		bool needs_true = false;
		p1 = orig_p1;
		cell *c_body = get_logical_body(c);

		if (p1_body && is_variable(p1_body) && !c_body) {
			p1 = get_head(p1);
			c = get_head(c);
			needs_true = true;
		}

		try_me(q, t->nbr_vars);
		q->tot_matches++;
		q->no_tco = false;

		if (unify_structure(q, p1, p1_ctx, c, q->st.fp, 0)) {
			if (needs_true) {
				p1_body = deref(q, p1_body, p1_ctx);
				idx_t p1_body_ctx = q->latest_ctx;
				cell tmp = {0};
				tmp.val_type = TYPE_LITERAL;
				tmp.nbr_cells = 1;
				tmp.val_off = g_true_s;
				return unify(q, p1_body, p1_body_ctx, &tmp, q->st.curr_frame);
			}

			return pl_success;
		}

		undo_me(q);
	}

	drop_choice(q);
	return pl_failure;
}

// Match HEAD.
// Match HEAD :- true.

USE_RESULT pl_state match_clause(query *q, cell *p1, idx_t p1_ctx, int is_retract)
{
	if (q->retry) {
		q->st.curr_clause2 = q->st.curr_clause2->next;
	} else {
		cell *c = p1;

		if (!is_literal(c)) {
			// For now convert it to a literal
			idx_t off = index_from_pool(q->m->pl, GET_STR(c));
			may_idx_error(off);
			FREE_STR(c);
			c->val_off = off;
			c->val_type = TYPE_LITERAL;
			c->flags = 0;
		}

		predicate *h = find_matching_predicate(q->m, p1);

		if (!h) {
			if (check_builtin(q->m->pl, GET_STR(p1), p1->arity)) {
				if (is_retract != DO_CLAUSE)
					return throw_error(q, p1, "permission_error", "modify,static_procedure");
				else
					return throw_error(q, p1, "permission_error", "access,private_procedure");
			}

			q->st.curr_clause2 = NULL;
			return pl_failure;
		} else {
			if (!h->is_dynamic) {
				if (is_retract != DO_CLAUSE)
					return throw_error(q, p1, "permission_error", "modify,static_procedure");
				else
					return throw_error(q, p1, "permission_error", "access,private_procedure");
			}

			q->st.curr_clause2 = h->head;
		}
	}

	if (!q->st.curr_clause2)
		return pl_failure;

	may_error(make_choice(q));

	for (; q->st.curr_clause2; q->st.curr_clause2 = q->st.curr_clause2->next) {
		if (q->st.curr_clause2->t.deleted)
			continue;

		term *t = &q->st.curr_clause2->t;
		cell *head = get_head(t->cells);
		cell *body = get_logical_body(t->cells);

		// Retract(HEAD) should ignore rules (and directives)

		if ((is_retract == DO_RETRACT) && body)
			continue;

		try_me(q, t->nbr_vars);
		q->tot_matches++;
		q->no_tco = false;

		if (unify_structure(q, p1, p1_ctx, head, q->st.fp, 0))
			return pl_success;

		undo_me(q);
	}

	drop_choice(q);
	return pl_failure;
}

#if 0
static const char *dump_key(void *p, const void *p1)
{
	query *q = (query*)p;
	cell *c = (cell*)p1;
	static char tmpbuf[1024];
	print_term_to_buf(q, tmpbuf, sizeof(tmpbuf), c, q->st.curr_frame, 0, 0, 0);
	return tmpbuf;
}
#endif

static void next_key(query *q)
{
	if (q->st.iter) {
		if (!sl_nextkey(q->st.iter, (void**)&q->st.curr_clause)) {
			q->st.curr_clause = NULL;
			q->st.iter = NULL;
		}
	} else if (q->st.curr_clause)
		q->st.curr_clause = q->st.curr_clause->next;
}

static USE_RESULT pl_state match_head(query *q)
{
	if (!q->retry) {
		cell *c = q->st.curr_cell;
		predicate *h;

		if (is_literal(c)) {
			h = c->match;
		} else {
			// For now convert it to a literal
			idx_t off = index_from_pool(q->m->pl, GET_STR(c));
			if (off == ERR_IDX) {
				q->error = true;
				return pl_error;
			}

			FREE_STR(c);
			c->val_off = off;
			c->val_type = TYPE_LITERAL;
			c->flags = 0;
			h = NULL;
		}

		if (!h) {
			h = find_matching_predicate(q->m, c);

			if (!h) {
				if (!is_end(c) && !(is_literal(c) && !strcmp(GET_STR(c), "initialization")))
					if (q->m->flag.unknown == 1)
						return throw_error(q, c, "existence_error", "procedure");
					else
						return pl_failure;
				else
					q->error = true;

				return pl_error;
			} else
				c->match = h;
		}

		h->gen++;

		if (h->index) {
			cell *key = deep_clone_to_heap(q, c, q->st.curr_frame);
			unsigned arity = key->arity;
			bool all_vars = true;

			for (cell *c = key + 1; arity--; c += c->nbr_cells) {
				if (!is_variable(c)) {
					all_vars = false;
					break;
				}
			}

			if (!all_vars) {
				q->st.iter = sl_findkey(h->index, key);
				next_key(q);
			} else {
				q->st.curr_clause = h->head;
			}
		} else {
			q->st.curr_clause = h->head;
		}
	} else
		next_key(q);

	if (!q->st.curr_clause)
		return pl_failure;

	may_error(make_choice(q));

	for (; q->st.curr_clause; next_key(q)) {
		if (q->st.curr_clause->t.deleted &&
			(q->st.curr_clause->t.gen < q->st.curr_clause->parent->gen))
			continue;

		term *t = &q->st.curr_clause->t;
		cell *head = get_head(t->cells);
		try_me(q, t->nbr_vars);
		q->tot_matches++;
		q->no_tco = false;

		if (unify_structure(q, q->st.curr_cell, q->st.curr_frame, head, q->st.fp, 0)) {
			Trace(q, q->st.curr_cell, EXIT);

			if (q->error)
				return pl_error;

			commit_me(q, t);
			return pl_success;
		}

		undo_me(q);
	}

	drop_choice(q);
	return pl_failure;
}

static void dump_vars(query *q)
{
	parser *p = q->p;
	frame *g = GET_FRAME(0);
	int any = 0;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		slot *e = GET_SLOT(g, i);

		if (is_empty(&e->c))
			continue;

		q->latest_ctx = e->ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
			q->latest_ctx = e->ctx;
		} else
			c = deref(q, &e->c, e->ctx);

		if (!strcmp(p->vartab.var_name[i], "_"))
			continue;

		if (any)
			fprintf(stdout, ",\n");

		fprintf(stdout, "%s = ", p->vartab.var_name[i]);
		int save = q->quoted;
		q->quoted = 1;
		print_term(q, stdout, c, q->latest_ctx, -2);
		q->quoted = save;
		any++;
	}

	if (any) {
		fprintf(stdout, ".\n");
		fflush(stdout);
	}

	q->m->dump_vars = any;
}

pl_state run_query(query *q)
{
	q->yielded = false;

	while (!q->error) {
		if (g_tpl_interrupt) {
			printf("\nAction (a)bort, (c)ontinue, (e)xit: ");
			fflush(stdout);
			int ch = history_getch();
			printf("%c\n", ch);

			if (ch == 'c') {
				g_tpl_interrupt = 0;
				continue;
			}

			if (ch == 'a') {
				g_tpl_interrupt = 0;
				q->abort = true;
				return pl_success;
			}

			if (ch == 'e') {
				signal(SIGINT, NULL);
				q->halt = true;
				return pl_success;
			}
		}

		if (q->retry) {
			if (!retry_choice(q)) {
				break;
			}
		}

		if (is_variable(q->st.curr_cell)) {
			if (!call_me(q, q->st.curr_cell))
				continue;
		}

		q->tot_goals++;
		q->did_throw = false;
		Trace(q, q->st.curr_cell, q->retry?REDO:q->resume?NEXT:CALL);

		if (!(q->st.curr_cell->flags&FLAG_BUILTIN)) {
			if (is_iso_list(q->st.curr_cell)) {
				consultall(q->m->p, q->st.curr_cell);
				follow_me(q);
			} else {
				if (!is_callable(q->st.curr_cell))
					DISCARD_RESULT throw_error(q, q->st.curr_cell, "type_error", "callable");
				else if (match_head(q) != pl_success) {
					q->retry = QUERY_RETRY;
					q->tot_retries++;
					continue;
				}
			}
		} else {
			if (!q->st.curr_cell->fn) {
				q->tot_goals--;
				q->st.curr_cell++;					// NO-OP
				continue;
			}

			if (!q->st.curr_cell->fn(q)) {
				q->retry = QUERY_RETRY;

				if (q->yielded)
					break;

				q->tot_retries++;
				continue;
			}

			Trace(q, q->st.curr_cell, EXIT);

			if (q->error)
				break;

			follow_me(q);
		}

		q->resume = false;
		q->retry = QUERY_OK;

		while (!q->st.curr_cell || is_end(q->st.curr_cell)) {
			if (!resume_frame(q)) {
				if (q->cp && q->p && !q->run_init) {
					int ch = 0;
					dump_vars(q);
					fflush(stdout);

					for (;;) {
						ch = history_getch();
						//printf("%c\n", ch);

						if ((ch == 'h') || (ch == '?')) {
							printf("Action (a)bort, (e)xit, (r)edo:\n");
							fflush(stdout);
							continue;
						}

						if ((ch == 'r') || (ch == ';')) {
							q->retry = QUERY_RETRY;
							break;
						}

						if (ch == 'a') {
							g_tpl_interrupt = 0;
							q->abort = true;
							return pl_success;
						}

						if (ch == 'e') {
							signal(SIGINT, NULL);
							q->halt = true;
							return pl_success;
						}
					}

					if ((ch == 'r') || (ch == ';'))
						break;
				}

				q->status = true;
				return pl_success;
			}

			q->resume = true;
			follow_me(q);
		}
	}

	if (!q->p)
		return pl_success;

	if (q->halt)
		q->error = false;
	else if (q->dump_vars && !q->abort && q->status)
		dump_vars(q);

	return pl_success;
}

pl_state query_execute(query *q, term *t)
{
	q->m->dump_vars = false;
	q->st.curr_cell = t->cells;
	q->st.sp = t->nbr_vars;
	q->st.curr_frame = 0;
	q->st.fp = 1;
	q->time_started = get_time_in_usec();
	q->abort = false;
	q->cycle_error = false;

	frame *g = q->frames + q->st.curr_frame;
	g->nbr_vars = t->nbr_vars;
	g->nbr_slots = t->nbr_vars;
	pl_state ret = run_query(q);
	sl_done(q->st.iter);
	return ret;
}

