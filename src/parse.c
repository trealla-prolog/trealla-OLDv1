#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <float.h>
#include <sys/time.h>

#include "internal.h"
#include "history.h"
#include "library.h"
#include "trealla.h"
#include "builtins.h"
#include "utf8.h"

static const unsigned INITIAL_TOKEN_SIZE = 100;		// bytes
static const unsigned INITIAL_POOL_SIZE = 64000;	// bytes

static const unsigned INITIAL_NBR_CELLS = 100;		// cells
static const unsigned INITIAL_NBR_HEAP = 8000;		// cells
static const unsigned INITIAL_NBR_QUEUE = 1000;		// cells

static const unsigned INITIAL_NBR_GOALS = 1000;
static const unsigned INITIAL_NBR_SLOTS = 1000;
static const unsigned INITIAL_NBR_CHOICES = 1000;
static const unsigned INITIAL_NBR_TRAILS = 1000;

#define JUST_IN_TIME_COUNT 50
#define DUMP_ERRS 0

stream g_streams[MAX_STREAMS] = {{0}};
idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
idx_t g_anon_s, g_clause_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
idx_t g_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
idx_t g_plus_s, g_minus_s;
unsigned g_cpu_count = 4;
char *g_tpl_lib = NULL;
int g_ac = 0, g_avc = 1;
char **g_av = NULL, *g_argv0 = NULL;

static atomic_t int g_tpl_count = 0;

static const struct op_table g_ops[] =
{
	{":-", OP_XFX, 1200},
	{":-", OP_FX, 1200},
	{"-->", OP_XFX, 1200},
	{"?-", OP_FX, 1200},
	{";", OP_XFY, 1100},
	{"|", OP_XFY, 1100},
	{"->", OP_XFY, 1050},
	{"*->", OP_XFY, 1050},
	{",", OP_XFY, 1000},

	//{"op", OP_FX, 1150},
	//{"public", OP_FX, 1150},
	//{"dynamic", OP_FX, 1150},
	//{"persist", OP_FX, 1150},
	//{"initialization", OP_FX, 1150},
	//{"set_prolog_flag", OP_FX, 1150},
	//{"module", OP_FX, 1150},
	//{"use_module", OP_FX, 1150},
	//{"ensure_loaded", OP_FX, 1150},

	{"\\+", OP_FY, 900},
	{"is", OP_XFX, 700},
	{"=", OP_XFX, 700},
	{"\\=", OP_XFX, 700},
	{"==", OP_XFX, 700},
	{"\\==", OP_XFX, 700},
	{"=:=", OP_XFX, 700},
	{"=\\=", OP_XFX, 700},
	{"<", OP_XFX, 700},
	{"=<", OP_XFX, 700},
	{">", OP_XFX, 700},
	{">=", OP_XFX, 700},
	{"@<", OP_XFX, 700},
	{"@=<", OP_XFX, 700},
	{"@>", OP_XFX, 700},
	{"@>=", OP_XFX, 700},
	{"=..", OP_XFX, 700},
	{":", OP_XFY, 600},
	{"+", OP_YFX, 500},
	{"-", OP_YFX, 500},
	{"?", OP_FX, 500},

	{"/\\", OP_YFX, 500},
	{"\\/", OP_YFX, 500},

	{"*", OP_YFX, 400},
	{"/", OP_YFX, 400},
	{"//", OP_YFX, 400},
	{"div", OP_YFX, 400},
	{"rdiv", OP_YFX, 400},
	{"rem", OP_YFX, 400},
	{"mod", OP_YFX, 400},
	{"xor", OP_YFX, 400},
	{"<<", OP_YFX, 400},
	{">>", OP_YFX, 400},
	{"**", OP_XFX, 200},
	{"^", OP_XFY, 200},
	{"\\", OP_FY, 200},
	{"-", OP_FY, 200},
	{"+", OP_FY, 200},

	//{"$", OP_FX, 1},

	{0,0,0}
};

static idx_t is_in_pool(__attribute__((unused)) prolog *pl, const char *name)
{
	const void *val;

	if (sl_get(pl->symtab, name, &val))
		return (idx_t)(unsigned long)val;

	return ERR_IDX;
}

static idx_t add_to_pool(prolog *pl, const char *name)
{
	if (!name) return ERR_IDX;
	idx_t offset = pl->pool_offset;
	size_t len = strlen(name);

	while ((offset+len+1+1) >= pl->pool_size) {
		FAULTINJECT(errno = ENOMEM; return ERR_IDX);
		size_t nbytes = pl->pool_size * 2;
		char *tmp = realloc(pl->pool, nbytes);
		if (!tmp) return ERR_IDX;
		pl->pool = tmp;
		memset(pl->pool + pl->pool_size, 0, nbytes - pl->pool_size);
		pl->pool_size = nbytes;
	}

	memcpy(pl->pool + offset, name, len+1);
	pl->pool_offset += len + 1;
	const char *key = strdup(name);
	sl_set(pl->symtab, key, (void*)(unsigned long)offset);
	return offset;
}

idx_t index_from_pool(prolog *pl, const char *name)
{
	if (!name) return ERR_IDX;
	idx_t offset = is_in_pool(pl, name);

	if (offset != ERR_IDX)
		return offset;

	return add_to_pool(pl, name);
}

unsigned get_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	for (const struct op_table *ptr = m->ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier) *specifier = ptr->specifier;
			return ptr->priority;
		}
	}

	for (const struct op_table *ptr = m->def_ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier) *specifier = ptr->specifier;
			return ptr->priority;
		}
	}

	if (hint_prefix)
		return get_op(m, name, specifier, false);

	return 0;
}

unsigned get_op2(module *m, const char *name, unsigned specifier)
{
	for (const struct op_table *ptr = m->ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier == ptr->specifier)
				return ptr->priority;
		}
	}

	for (const struct op_table *ptr = m->def_ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier == ptr->specifier)
				return ptr->priority;
		}
	}

	return 0;
}

bool set_op(module *m, const char *name, unsigned specifier, unsigned priority)
{
	unsigned ot = 0, pri = 0;
	int hint = IS_PREFIX(specifier);

	if ((pri = get_op(m, name, &ot, hint)) != 0) {
		if ((ot == specifier) && priority)
			return true;
	}

	struct op_table *ptr = m->def_ops;

	for (; ptr->name; ptr++) {
		if (strcmp(ptr->name, name))
			continue;

		if (!ptr->specifier)
			continue;

		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			free(ptr->name);
			ptr->name = strdup("");
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			return true;
		}

		ptr->specifier = specifier;
		ptr->priority = priority;
		m->loaded_ops = false;
		return true;
	}

	ptr = m->ops;

	for (; ptr->name; ptr++) {
		if (strcmp(ptr->name, name))
			continue;

		if (!ptr->specifier)
			continue;

		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			free(ptr->name);
			ptr->name = strdup("");
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			return true;
		}

		ptr->specifier = specifier;
		ptr->priority = priority;
		m->loaded_ops = false;
		return true;
	}

	if (!priority)
		return true;

	ptr = m->ops;

	for (; ptr->name; ptr++) {
		if (!ptr->specifier)
			break;
	}

	if (!ptr->name) {
		if (!m->spare_ops)
			return false;

		m->spare_ops--;
	} else {
		free(ptr->name);
	}

	ptr->name = strdup(name);
	ptr->specifier = specifier;
	ptr->priority = priority;
	m->loaded_ops = false;
	return true;
}

static const char *get_filename(const char *path)
{
	const char *ptr = strrchr(path, '/');

	if (!ptr)
		return path;

	return ptr+1;
}

cell *list_head(cell *l, cell *tmp)
{
	if (!is_string(l))
		return l + 1;

	const char *src = is_static(l) ? l->val_str : (char*)l->val_strb->cstr + l->strb_off;
	size_t len = len_char_utf8(src);
	tmp->val_type = TYPE_CSTRING;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	memcpy(tmp->val_chr, src, len);
	tmp->val_chr[len] = '\0';
	return tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!l) return NULL;

	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->nbr_cells;
	}

	const char *src = is_static(l) ? l->val_str : (char*)l->val_strb->cstr;
	size_t str_len = is_static(l) ? (size_t)l->str_len : (size_t)l->val_strb->len - l->strb_off;
	size_t len = len_char_utf8(src);

	if (str_len == len) {
		tmp->val_type = TYPE_LITERAL;
		tmp->nbr_cells = 1;
		tmp->arity = 0;
		tmp->flags = 0;
		tmp->val_off = g_nil_s;
		return tmp;
	}

	if (is_static(l)) {
		tmp->flags = FLAG_BLOB | FLAG2_STATIC | FLAG_STRING;
		tmp->nbr_cells = 1;
		tmp->arity = 2;
		tmp->val_str = l->val_str + len;
		tmp->str_len = l->str_len - len;
		return tmp;
	}

	copy_cells(tmp, l, 1);
	tmp->strb_off = l->strb_off + len;
	tmp->strb_len = l->strb_len - len;
	return tmp;
}

module *find_next_module(prolog *pl, module *m)
{
	if (!m)
		return pl->modules;

	return m->next;
}

module *find_module(prolog *pl, const char *name)
{
	for (module *m = pl->modules; m; m = m->next) {
		if (!strcmp(m->name, name))
			return m;
	}

	return NULL;
}

static bool check_directive(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_clause_s) && (c->arity == 1))
		return true;

	return false;
}

bool check_rule(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_clause_s) && (c->arity == 2))
		return true;

	return false;
}

cell *get_head(cell *c)
{
	if (check_rule(c))
		return c + 1;

	return c;
}

cell *get_body(cell *c)
{
	if (check_rule(c)) {
		c = c + 1;
		c += c->nbr_cells;

		if (is_end(c))
			return NULL;

		return c;
	}

	return NULL;
}

cell *get_logical_body(cell *c)
{
	cell *body = get_body(c);

	if (!body)
		return NULL;

	// A body of just 'true' is equivalent to no body at all,
	// and of course vice-versa.

	if (!body->arity && is_literal(body) && (body->val_off == g_true_s))
		return NULL;

	return body;
}

predicate *find_predicate(module *m, cell *c)
{
	cell tmp = *c;
	tmp.val_type = TYPE_LITERAL;
	tmp.flags = FLAG_KEY;
	tmp.nbr_cells = 1;

	if (is_cstring(c))
		tmp.val_off = index_from_pool(m->pl, MODULE_GET_STR(c));

	sliter *iter = sl_findkey(m->index, &tmp);
	predicate *h = NULL;

	while (sl_nextkey(iter, (void*)&h)) {
		if (h->is_abolished)
			continue;

		sl_done(iter);
		return h;
	}

	return NULL;
}

static predicate *find_matching_predicate_internal(module *m, cell *c, __attribute__((unused)) bool quiet)
{
#if 0
	module *save_m = m;
#endif
	module *tmp_m = NULL;

	while (m) {
		predicate *h = find_predicate(m, c);

#if 0
		if (!quiet && h && (m != save_m) && !h->is_public
			&& strcmp(MODULE_GET_STR(c), "dynamic")
			&& strcmp(MODULE_GET_STR(c), "module")) {
			fprintf(stdout, "Warning: match not a public method %s/%u\n", MODULE_GET_STR(c), c->arity);
			break;
		}
#endif

		if (h)
			return h;

		if (!tmp_m)
			m = tmp_m = m->pl->modules;
		else
			m = m->next;
	}

	return NULL;
}

predicate *find_matching_predicate(module *m, cell *c)
{
	return find_matching_predicate_internal(m, c, false);
}

predicate *find_matching_predicate_quiet(module *m, cell *c)
{
	return find_matching_predicate_internal(m, c, true);
}

predicate *find_functor(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	tmp.arity = arity;
	return find_predicate(m, &tmp);
}

static void push_property(module *m, const char *name, unsigned arity, const char *type)
{
	if (name[0] == '$')
		return;

	size_t buflen = 1024;
	char *tmpbuf = malloc(buflen);
	format_property(&tmpbuf, &buflen, tmpbuf, name, arity, type);
	parser *p = create_parser(m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	parser_tokenize(p, false, false);
	destroy_parser(p);
	free(tmpbuf);
}

static predicate *create_predicate(module *m, cell *c)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	predicate *h = calloc(1, sizeof(predicate));
	ensure(h);
	h->next = m->head;
	m->head = h;

	h->key = *c;
	h->key.val_type = TYPE_LITERAL;
	h->key.flags = FLAG_KEY;
	h->key.nbr_cells = 1;

	if (is_cstring(c))
		h->key.val_off = index_from_pool(m->pl, MODULE_GET_STR(c));

	sl_app(m->index, &h->key, h);
	return h;
}

static void set_multifile_in_db(module *m, const char *name, idx_t arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		push_property(m, name, arity, "multifile");
		h->is_multifile = true;
	} else
		m->error = true;
}

static bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, idx_t arity)
{
	module *m = find_module(pl, mod);
	if (!m) return false;

	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	if (tmp.val_off == ERR_IDX) return false;
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) return false;
	return h->is_multifile ? true : false;
}

static int compkey(const void *param, const void *ptr1, const void *ptr2)
{
	assert(ptr1 && ptr2);

	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;
	const module *m = (const module*)param;

	if (is_integer(p1) && is_integer(p2)) {
		if (p1->val_num < p2->val_num)
			return -1;
		else if (p1->val_num > p2->val_num)
			return 1;
	} else if (is_float(p1) && is_float(p2)) {
		if (p1->val_flt < p2->val_flt)
			return -1;
		else if (p1->val_flt > p2->val_flt)
			return 1;
	} else if (is_key(p1) && is_key(p2)) {
		if (p1->arity == p2->arity) {
			if (p1->val_off == p2->val_off)
				return 0;
		}

		int ok = strcmp(MODULE_GET_STR(p1), MODULE_GET_STR(p2));
		if (ok) return ok;

		if (p1->arity < p2->arity)
			return -1;

		if (p1->arity > p2->arity)
			return 1;

		return 0;
	} else if (is_atom(p1) && is_atom(p2) && (p1->arity == p2->arity)) {
		return strcmp(MODULE_GET_STR(p1), MODULE_GET_STR(p2));
	} else if (is_structure(p1) && is_structure(p2)) {
		if (p1->arity < p2->arity)
			return -1;

		if (p1->arity > p2->arity)
			return 1;

		if (p1->val_off != p2->val_off) {
			int i = strcmp(MODULE_GET_STR(p1), MODULE_GET_STR(p2));

			if (i != 0)
				return i;
		}

		int arity = p1->arity;
		p1++; p2++;

		while (arity--) {
			int i = compkey(m, p1, p2);

			if (i != 0)
				return i;

			p1 += p1->nbr_cells;
			p2 += p2->nbr_cells;
		}
	}

	return 0;
}

static clause* assert_begin(module *m, term *t, bool consulting)
{
	cell *c = t->cells;

	if (!check_directive(c))
		c = get_head(t->cells);

	if (!c) {
		fprintf(stdout, "Error: not a fact or clause\n");
		return NULL;
	}

	if (is_cstring(c)) {
		idx_t off = index_from_pool(m->pl, MODULE_GET_STR(c));
		if (off == ERR_IDX) return NULL;
		DECR_REF(c);
		c->val_type = TYPE_LITERAL;
		c->val_off = off;
		c->flags = 0;
	}

	predicate *h = find_predicate(m, c);

	if (h && !consulting && !h->is_dynamic) {
		fprintf(stdout, "Error: not dynamic '%s'/%u\n", MODULE_GET_STR(c), c->arity);
		return NULL;
	}

	if (!h) {
		h = create_predicate(m, c);
		ensure(h);

		if (check_directive(t->cells))
			h->check_directive = true;

		if (!consulting) {
			push_property(m, MODULE_GET_STR(c), c->arity, "dynamic");
			h->is_dynamic = true;
		} else {
			if (m->prebuilt) {
				push_property(m, MODULE_GET_STR(c), c->arity, "built_in");
				push_property(m, MODULE_GET_STR(c), c->arity, "private");
			}

			push_property(m, MODULE_GET_STR(c), c->arity, "static");
		}

		if (consulting && m->make_public) {
			push_property(m, MODULE_GET_STR(c), c->arity, "public");
			h->is_public = true;
		}

	}

	if (m->prebuilt)
		h->is_prebuilt = true;

	int nbr_cells = t->cidx;
	clause *r = calloc(sizeof(clause)+(sizeof(cell)*nbr_cells), 1);
	if (!r) {
		h->is_abolished = true;
		return NULL;
	}

	r->owner = h;
	memcpy(&r->t, t, sizeof(term));
	r->t.nbr_cells = copy_cells(r->t.cells, t->cells, nbr_cells);
	r->t.ugen_created = ++m->pl->ugen;
	r->m = m;
	return r;
}

static void reindex_predicate(module *m, predicate *h)
{
	h->index = sl_create1(compkey, m);
	ensure(h->index);

	for (clause *r = h->head; r; r = r->next) {
		cell *c = get_head(r->t.cells);

		if (!r->t.ugen_erased)
			sl_app(h->index, c, r);
	}
}

static void assert_commit(module *m, term *t, clause *r, predicate *h, bool append)
{
	cell *c = get_head(r->t.cells);
	cell *p1 = c + 1;

	if (h->is_persist)
		r->t.persist = true;

	if (h->key.arity) {
		if (!h->index && is_structure(p1))
			h->is_noindex = true;

		if (h->index && is_structure(p1)) {
			h->is_noindex = true;
			h->index_save = h->index;
			h->index = NULL;
		}

		if (!h->index && (h->cnt > JUST_IN_TIME_COUNT)
			&& !m->pl->noindex && !h->is_noindex)
			reindex_predicate(m, h);

		if (h->index) {
			if (!append)
				sl_set(h->index, c, r);
			else
				sl_app(h->index, c, r);
		}
	}

	t->cidx = 0;
}

clause *asserta_to_db(module *m, term *t, bool consulting)
{
	clause *r = assert_begin(m, t, consulting);
	if (!r) return NULL;
	predicate *h = r->owner;

	if (h->head)
		h->head->prev = r;

	r->next = h->head;
	h->head = r;
	h->cnt++;

	if (!h->tail)
		h->tail = r;

	assert_commit(m, t, r, h, false);
	return r;
}

clause *assertz_to_db(module *m, term *t, bool consulting)
{
	clause *r = assert_begin(m, t, consulting);
	if (!r) return NULL;
	predicate *h = r->owner;

	if (h->tail)
		h->tail->next = r;

	r->prev = h->tail;
	h->tail = r;
	h->cnt++;

	if (!h->head)
		h->head = r;

	assert_commit(m, t, r, h, true);
	return r;
}

static bool retract_from_db(module *m, clause *r)
{
	if (r->t.ugen_erased)
		return false;

	r->owner->cnt--;
	r->t.ugen_erased = ++m->pl->ugen;
	return true;
}

void add_to_dirty_list(query *q, clause *r)
{
	if (!retract_from_db(q->m, r))
		return;

	r->dirty = q->dirty_list;
	q->dirty_list = r;
}

static void query_purge_dirty_list(query *q)
{
	while (q->dirty_list) {
		clause *r = q->dirty_list;
		q->dirty_list = r->dirty;

		if (r->prev)
			r->prev->next = r->next;

		if (r->next)
			r->next->prev = r->prev;

		if (r->owner->head == r)
			r->owner->head = r->next;

		if (r->owner->tail == r)
			r->owner->tail = r->prev;

		r->dirty = q->m->dirty_list;
		q->m->dirty_list = r;
	}
}

static void module_purge_dirty_list(module *m)
{
	while (m->dirty_list) {
		clause *r = m->dirty_list;
		m->dirty_list = r->dirty;
		//clear_term(&r->t);
		free(r);
	}
}

clause *find_in_db(module *m, uuid *ref)
{
	for (predicate *h = m->head; h; h = h->next) {
		for (clause *r = h->head ; r; r = r->next) {
			if (r->t.ugen_erased)
				continue;

			if (!memcmp(&r->u, ref, sizeof(uuid)))
				return r;
		}
	}

	return NULL;
}

clause *erase_from_db(module *m, uuid *ref)
{
	clause *r = find_in_db(m, ref);
	if (!r) return 0;
	r->t.ugen_erased = ++m->pl->ugen;
	return r;
}

static void set_noindex_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h)
		h->is_noindex = true;
	else
		m->error = true;
}

void set_discontiguous_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		push_property(m, name, arity, "discontiguous");
		h->is_discontiguous = true;
	} else
		m->error = true;
}

static void set_dynamic_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		push_property(m, name, arity, "dynamic");
		h->is_dynamic = true;
	} else
		m->error = true;
}

static void set_meta_predicate_in_db(module *m, cell *c)
{
	const char *name = MODULE_GET_STR(c);
	unsigned arity = c->arity;
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		query q = (query){0};
		q.m = m;
		char *dst = print_term_to_strbuf(&q, c, 0, 0);
		push_property(m, name, arity, dst);
		free(dst);
		h->is_meta_predicate = true;
	} else
		m->error = true;
}

static void set_persist_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off == ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		push_property(m, name, arity, "dynamic");
		push_property(m, name, arity, "persist");
		h->is_dynamic = true;
		h->is_persist = true;
		m->use_persist = true;
	} else
		m->error = true;
}

void clear_term(term *t)
{
	if (!t)
		return;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;
		DECR_REF(c);
		c->val_type = TYPE_EMPTY;
	}

	t->cidx = 0;
}

static bool make_room(parser *p)
{
	if (p->t->cidx == p->t->nbr_cells) {
		idx_t nbr_cells = p->t->nbr_cells * 2;

		term *t = realloc(p->t, sizeof(term)+(sizeof(cell)*nbr_cells));
		if (!t) {
			p->error = true;
			return false;
		}

		p->t = t;
		p->t->nbr_cells = nbr_cells;
	}

	return true;
}

static cell *make_cell(parser *p)
{
	make_room(p);
	cell *ret = p->t->cells + p->t->cidx++;
	*ret = (cell){0};
	return ret;
}

void destroy_parser(parser *p)
{
	free(p->token);
	clear_term(p->t);
	free(p->t);
	free(p);
}

parser *create_parser(module *m)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	parser *p = calloc(1, sizeof(parser));
	ensure(p);
	p->token = calloc(p->token_size=INITIAL_TOKEN_SIZE+1, 1);
	idx_t nbr_cells = INITIAL_NBR_CELLS;
	p->t = calloc(sizeof(term)+(sizeof(cell)*nbr_cells), 1);
	p->t->nbr_cells = nbr_cells;
	p->start_term = true;
	p->line_nbr = 1;
	p->m = m;
	p->error = false;
	p->flag = m->flag;

	if (!p->token || !p->t) {
		destroy_parser(p);
		p = NULL;
	}

	return p;
}

void destroy_query(query *q)
{
	while (q->st.qnbr > 0) {
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		q->st.qnbr--;
	}

	for (arena *a = q->arenas; a;) {
		for (idx_t i = 0; i < a->hp; i++) {
			cell *c = a->heap + i;
			DECR_REF(c);
		}

		arena *save = a;
		a = a->next;
		free(save->heap);
		free(save);
	}

	for (int i = 0; i < MAX_QUEUES; i++) {
		for (idx_t j = 0; j < q->qp[i]; j++) {
			cell *c = q->queue[i]+j;
			DECR_REF(c);
		}

		free(q->queue[i]);
	}

	slot *e = q->slots;

	for (idx_t i = 0; i < q->st.sp; i++, e++)
		DECR_REF(&e->c);

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
	q->m = m;
	q->trace = m->pl->trace;
	q->flag = m->flag;

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

	q->h_size = is_task ? INITIAL_NBR_HEAP/10 : INITIAL_NBR_HEAP;
	q->tmph_size = is_task ? INITIAL_NBR_CELLS/10 : INITIAL_NBR_CELLS;

	for (int i = 0; i < MAX_QUEUES; i++)
		q->q_size[i] = is_task ? INITIAL_NBR_QUEUE/10 : INITIAL_NBR_QUEUE;

	if (error) {
		destroy_query (q);
		q = NULL;
	}

	return q;
}

query *create_task(query *q, cell *curr_cell)
{
	query *subq = create_query(q->m, true);
	if (!subq) return NULL;
	subq->parent = q;
	subq->st.fp = 1;
	subq->is_task = true;

	cell *tmp = clone_to_heap(subq, 0, curr_cell, 1); //cehteh: checkme
	idx_t nbr_cells = tmp->nbr_cells;
	make_end(tmp+nbr_cells);
	subq->st.curr_cell = tmp;

	frame *gsrc = GET_FRAME(q->st.curr_frame);
	frame *gdst = subq->frames;
	gdst->nbr_vars = gsrc->nbr_vars;
	slot *e = GET_SLOT(gsrc, 0);

	for (unsigned i = 0; i < gsrc->nbr_vars; i++, e++) {
		cell *c = deref(q, &e->c, e->ctx);
		cell tmp = (cell){0};
		tmp.val_type = TYPE_VARIABLE;
		tmp.var_nbr = i;
		tmp.val_off = g_anon_s;
		set_var(subq, &tmp, 0, c, q->latest_ctx);
	}

	subq->st.sp = gsrc->nbr_vars;
	return subq;
}

void consultall(parser *p, cell *l)
{
	LIST_HANDLER(l);

	while (is_list(l)) {
		cell *h = LIST_HEAD(l);
		module_load_file(p->m, PARSER_GET_STR(h));
		l = LIST_TAIL(l);
	}
}

char *relative_to(const char *basefile, const char *relfile)
{
	char *tmpbuf = malloc(strlen(basefile) + strlen(relfile) + 256);
	char *ptr = tmpbuf;

	if (!strncmp(relfile, "../", 3)) {
		strcpy(tmpbuf, basefile);
		ptr = tmpbuf + strlen(tmpbuf) - 1;

		while ((ptr != tmpbuf) && (*ptr != '/'))
			ptr--;

		if (ptr != tmpbuf)
			*ptr++ = '/';

		*ptr = '\0';
	}

	strcpy(ptr, relfile);
	return tmpbuf;
}

static void directives(parser *p, term *t)
{
	p->skip = false;

	if (!is_literal(t->cells))
		return;

	if (is_list(t->cells) && p->command) {
		consultall(p, t->cells);
		p->skip = true;
		return;
	}

	if (strcmp(PARSER_GET_STR(t->cells), ":-") || (t->cells->arity != 1))
		return;

	cell *c = t->cells + 1;

	if (!is_literal(c))
		return;

	const char *dirname = PARSER_GET_STR(c);

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "include") && (p1->arity == 1)) {
		if (!is_atom(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		unsigned save_line_nbr = p->line_nbr;
		char *filename = relative_to(p->m->filename, name);

		if (!module_load_file(p->m, filename)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s\n", filename);

			free(filename);
			p->error = true;
			return;
		}

		p->line_nbr = save_line_nbr;
		free(filename);
		return;
	}

	if (!strcmp(dirname, "ensure_loaded") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		char *filename = relative_to(p->m->filename, name);
		deconsult(p->m->pl, filename);
		unsigned save_line_nbr = p->line_nbr;

		if (!module_load_file(p->m, filename)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s\n", filename);

			free(filename);
			p->error = true;
			return;
		}

		p->line_nbr = save_line_nbr;
		free(filename);
		return;
	}

	if (!strcmp(dirname, "module") && (c->arity == 2)) {
		cell *p2 = c + 2;
		const char *name = "";
		char tmpbuf[1024];

		if (is_variable(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module name not an atom\n");

			p->error = true;
			return;
		} else
			name = PARSER_GET_STR(p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;
			return;
		}

		p->m = create_module(p->m->pl, name);
		if (!p->m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

			p->error = true;
			return;
		}

		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			cell *head = LIST_HEAD(p2);

			if (is_structure(head)) {
				if (strcmp(PARSER_GET_STR(head), "/") && strcmp(PARSER_GET_STR(head), "//"))
					return;

				cell *f = head+1, *a = f+1;
				if (!is_literal(f)) return;
				if (!is_integer(a)) return;
				cell tmp = *f;
				tmp.arity = a->val_num;

				if (!strcmp(PARSER_GET_STR(head), "//"))
					tmp.arity += 2;

				predicate *h = find_predicate(p->m, &tmp);
				if (!h) h = create_predicate(p->m, &tmp);
				if (!h) {
					destroy_module(p->m);
					p->m = NULL;
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: predicate creation failed\n");

					p->error = true;
					return;
				}

				h->is_public = true;
			}

			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_structure(p1)) return;
	}

	if (!strcmp(dirname, "use_module") && (c->arity >= 1)) {
		if (!is_atom(p1) && !is_structure(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		char dstbuf[1024*2];

		if (!strcmp(name, "library")) {
			p1 = p1 + 1;
			if (!is_literal(p1)) return;
			name = PARSER_GET_STR(p1);
			module *m;

			if ((m = find_module(p->m->pl, name)) != NULL) {
				if (!m->fp)
					do_db_load(m);

				return;
			}

			if (!strcmp(name, "between") ||
				!strcmp(name, "terms") ||
				!strcmp(name, "types") ||
				!strcmp(name, "files"))
				return;

			for (library *lib = g_libs; lib->name; lib++) {
				if (strcmp(lib->name, name))
					continue;

				char *src = strndup((const char*)lib->start, (lib->end-lib->start));
				STRING_INIT(s1);
				STRING_CAT2(s1, "library/", lib->name);
				m = module_load_text(p->m, src, STRING_BUF(s1));
				STRING_DONE(s1);
				free(src);

				if (m != p->m)
					do_db_load(m);

				return;
			}

			query q = (query){0};
			q.m = p->m;
			snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
			char *dst = dstbuf + strlen(dstbuf);
			idx_t ctx = 0;
			print_term_to_buf(&q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, false, 0);
			name = dstbuf;
		}

		char *filename = relative_to(p->m->filename, name);

		if (!module_load_file(p->m, filename)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: using module file: %s\n", filename);

			p->error = true;
			free(filename);
			return;
		}

		free(filename);
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;
		if (!is_literal(p2)) return;

		if (!strcmp(PARSER_GET_STR(p1), "double_quotes")) {
			if (!strcmp(PARSER_GET_STR(p2), "atom")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_atom = true;
			} else if (!strcmp(PARSER_GET_STR(p2), "codes")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_atom = false;
				p->m->flag.double_quote_codes = true;
			} else if (!strcmp(PARSER_GET_STR(p2), "chars")) {
				p->m->flag.double_quote_atom = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_chars = true;
			} else {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: unknown value\n");

				p->error = true;
				return;
			}
		} else if (!strcmp(PARSER_GET_STR(p1), "character_escapes")) {
			if (!strcmp(PARSER_GET_STR(p2), "true") || !strcmp(PARSER_GET_STR(p2), "on"))
				p->m->flag.character_escapes = true;
			else if (!strcmp(PARSER_GET_STR(p2), "false") || !strcmp(PARSER_GET_STR(p2), "off"))
				p->m->flag.character_escapes = false;
		} else if (!strcmp(PARSER_GET_STR(p1), "prefer_rationals")) {
			if (!strcmp(PARSER_GET_STR(p2), "true") || !strcmp(PARSER_GET_STR(p2), "on"))
				p->m->flag.prefer_rationals = true;
			else if (!strcmp(PARSER_GET_STR(p2), "false") || !strcmp(PARSER_GET_STR(p2), "off"))
				p->m->flag.prefer_rationals = false;
		} else if (!strcmp(PARSER_GET_STR(p1), "rational_syntax")) {
			if (!strcmp(PARSER_GET_STR(p2), "natural"))
				p->m->flag.rational_syntax_natural = true;
			else if (!strcmp(PARSER_GET_STR(p2), "compatibility"))
				p->m->flag.rational_syntax_natural = false;
		} else {
			fprintf(stdout, "Warning: unknown flag: %s\n", PARSER_GET_STR(p1));
		}

		p->flag = p->m->flag;
		return;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		cell *p2 = c + 2, *p3 = c + 3;

		if (!is_integer(p1) || !is_literal(p2) || (!is_atom(p3) && !is_list(p3))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: unknown op\n");

			p->error = true;
			return;
		}

		unsigned specifier;
		const char *spec = PARSER_GET_STR(p2);

		if (!strcmp(spec, "fx"))
			specifier = OP_FX;
		else if (!strcmp(spec, "fy"))
			specifier = OP_FY;
		else if (!strcmp(spec, "xf"))
			specifier = OP_XF;
		else if (!strcmp(spec, "yf"))
			specifier = OP_YF;
		else if (!strcmp(spec, "xfx"))
			specifier = OP_XFX;
		else if (!strcmp(spec, "xfy"))
			specifier = OP_XFY;
		else if (!strcmp(spec, "yfx"))
			specifier = OP_YFX;
		else {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: unknown op spec val_type\n");
			return;
		}

		LIST_HANDLER(p3);

		while (is_list(p3)) {
			cell *h = LIST_HEAD(p3);

			if (is_atom(h)) {
				if (!set_op(p->m, PARSER_GET_STR(h), specifier, p1->val_num)) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: could not set op\n");

					continue;
				}
			}

			p3 = LIST_TAIL(p3);
		}

		if (is_atom(p3) && !is_nil(p3)) {
			if (!set_op(p->m, PARSER_GET_STR(p3), specifier, p1->val_num)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op\n");

				return;
			}
		}

		return;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);

			if (is_literal(h) && (!strcmp(PARSER_GET_STR(h), "/") || !strcmp(PARSER_GET_STR(h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;
			if (!is_atom(c_name)) continue;
			cell *c_arity = h + 2;
			if (!is_integer(c_arity)) continue;
			unsigned arity = c_arity->val_num;

			if (!strcmp(PARSER_GET_STR(h), "//"))
				arity += 2;

			//printf("*** %s => %s / %u\n", dirname, PARSER_GET_STR(c_name), arity);

			if (!strcmp(dirname, "dynamic")) {
				set_dynamic_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				set_persist_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = PARSER_GET_STR(c_name);

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(p->m->pl, mod, name, arity)) {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: not multile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return;
					}
				}
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, PARSER_GET_STR(c_name), arity);
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return;

	while (is_literal(p1)) {
		if (!strcmp(PARSER_GET_STR(p1), "/") && (p1->arity == 2)) {
			cell *c_name = p1 + 1;
			if (!is_atom(c_name)) return;
			cell *c_arity = p1 + 2;
			if (!is_integer(c_arity)) return;
			unsigned arity = c_arity->val_num;

			if (!strcmp(PARSER_GET_STR(p1), "//"))
				arity += 2;

			if (!strcmp(dirname, "dynamic")) {
				set_dynamic_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				set_persist_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "meta_predicate")) {
				set_meta_predicate_in_db(p->m, c_name);
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = PARSER_GET_STR(c_name);

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(p->m->pl, mod, name, arity)) {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: not multile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return;
					}
				}
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, PARSER_GET_STR(c_name), arity);
			}

			p1 += p1->nbr_cells;
		} else if (!strcmp(dirname, "meta_predicate")) {
			set_meta_predicate_in_db(p->m, p1);
			p1 += p1->nbr_cells;
		} else if (!strcmp(PARSER_GET_STR(p1), ",") && (p1->arity == 2))
			p1 += 1;
		else
			break;
	}

	return;
}

static void parser_xref_cell(parser *p, term *t, cell *c, predicate *parent)
{
	const char *functor = PARSER_GET_STR(c);
	module *m = p->m;

	unsigned specifier;
	bool hint_prefix = c->arity == 1;

	if ((c->arity == 2)
		&& !GET_OP(c)
		&& strcmp(functor, "{}")
		&& get_op(m, functor, &specifier, hint_prefix)) {
		SET_OP(c, specifier);
	}

	bool found = false;

	if ((c->fn = get_builtin(p->m->pl, functor, c->arity, &found)) != NULL) {
		c->flags |= FLAG_BUILTIN;
		return;
	}

	if (found) {
		c->flags |= FLAG_BUILTIN;
		return;
	}

	if ((functor[0] != ':') && strchr(functor+1, ':')) {
		char tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
		m = find_module(p->m->pl, tmpbuf1);

		if (m) {
			c->val_off = index_from_pool(p->m->pl, tmpbuf2);
			ensure(c->val_off != ERR_IDX);
		}
		else
			m = p->m;
	}

	module *tmp_m = NULL;

	while (m) {
		predicate *h = find_predicate(m, c);

		if ((c+c->nbr_cells) >= (t->cells+t->cidx-1)) {
			c->flags |= FLAG_TAIL;

			if (parent && (h == parent)) {
				c->flags |= FLAG_TAIL_REC;
				t->tail_rec = true;
			}
		}

		if (h) {
			if ((m != p->m) && !h->is_public
				&& strcmp(PARSER_GET_STR(c), "dynamic")
				&& strcmp(PARSER_GET_STR(c), "module")) {
				//fprintf(stdout, "Warning: xref not a public method %s/%u\n", PARSER_GET_STR(c), c->arity);
				//p->error = true;
				break;
			}

			c->match = h;
			break;
		}

		if (!tmp_m)
			m = tmp_m = m->pl->modules;
		else
			m = m->next;
	}
}

void parser_xref(parser *p, term *t, predicate *parent)
{
	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_literal(c))
			continue;

		parser_xref_cell(p, t, c, parent);
	}
}

static void parser_xref_db(parser *p)
{
	for (predicate *h = p->m->head; h; h = h->next) {
		for (clause *r = h->head; r; r = r->next)
			parser_xref(p, &r->t, h);
	}

	p->end_of_term = false;
}

static void check_first_cut(parser *p)
{
	cell *c = get_body(p->t->cells);
	int cut_only = true;

	if (!c)
		return;

	while (!is_end(c)) {
		if (!(c->flags&FLAG_BUILTIN))
			break;

		if (!strcmp(PARSER_GET_STR(c), ","))
			;
		else if (!IS_OP(c) && !strcmp(PARSER_GET_STR(c), "!")) {
			p->t->first_cut = true;
			break;
		} else {
			cut_only = false;
			break;
		}

		c += c->nbr_cells;
	}

	if (p->t->first_cut && cut_only)
		p->t->cut_only = true;
}

static idx_t get_varno(parser *p, const char *src)
{
	int anon = !strcmp(src, "_");
	size_t offset = 0;
	int i = 0;

	while (p->vartab.var_pool[offset]) {
		if (!strcmp(p->vartab.var_pool+offset, src) && !anon)
			return i;

		offset += strlen(p->vartab.var_pool+offset) + 1;
		i++;
	}

	size_t len = strlen(src);

	if ((offset+len+1) >= MAX_VAR_POOL_SIZE) {
		fprintf(stdout, "Error: variable pool exhausted\n");
		p->error = true;
		return 0;
	}

	memcpy(p->vartab.var_pool+offset, src, len+1);
	return i;
}

void parser_assign_vars(parser *p, unsigned start, bool rebase)
{
	if (!p || p->error)
		return;

	p->start_term = true;
	p->nbr_vars = 0;
	memset(&p->vartab, 0, sizeof(p->vartab));
	term *t = p->t;
	t->nbr_vars = 0;
	t->first_cut = false;
	t->cut_only = false;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_variable(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, PARSER_GET_STR(c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_ARITY) {
			fprintf(stdout, "Error: max vars per term reached\n");
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = PARSER_GET_STR(c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			c->flags |= FLAG2_FIRST_USE;
			t->nbr_vars++;
			p->nbr_vars++;
		}
	}

	for (idx_t i = 0; i < t->nbr_vars; i++) {
		if (p->consulting && !p->do_read_term && (p->vartab.var_used[i] == 1) &&
			(p->vartab.var_name[i][strlen(p->vartab.var_name[i])-1] != '_') &&
			(*p->vartab.var_name[i] != '_')) {
			if (!p->m->pl->quiet)
				fprintf(stdout, "Warning: singleton: %s, near line %u, file '%s'\n", p->vartab.var_name[i], p->line_nbr, get_filename(p->m->filename));
		}
	}

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_variable(c))
			continue;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG2_ANON;
	}

	cell *c = make_cell(p);
	ensure(c);
	c->val_type = TYPE_END;
	c->nbr_cells = 1;
	check_first_cut(p);
	p->t->is_fact = !get_logical_body(p->t->cells);
}

static cell *insert_here(parser *p, cell *c, cell *p1)
{
	idx_t c_idx = c - p->t->cells, p1_idx = p1 - p->t->cells;
	make_room(p);

	cell *last = p->t->cells + (p->t->cidx - 1);
	idx_t cells_to_move = p->t->cidx - p1_idx;
	cell *dst = last + 1;

	while (cells_to_move--)
		*dst-- = *last--;

	p1 = p->t->cells + p1_idx;
	p1->val_type = TYPE_LITERAL;
	p1->flags = 0;//FLAG_BUILTIN;
	p1->fn = NULL;
	p1->val_off = g_call_s;
	p1->nbr_cells = 2;
	p1->arity = 1;

	p->t->cidx++;
	return p->t->cells + c_idx;
}

cell *check_body_callable(parser *p, cell *c)
{
	if (IS_XFX(c) || IS_XFY(c)) {
		if (!strcmp(PARSER_GET_STR(c), ",")
			|| !strcmp(PARSER_GET_STR(c), ";")
			|| !strcmp(PARSER_GET_STR(c), "->")
			|| !strcmp(PARSER_GET_STR(c), ":-")) {
			cell *lhs = c + 1;
			cell *tmp;

			if ((tmp = check_body_callable(p, lhs)) != NULL)
				return tmp;

			cell *rhs = lhs + lhs->nbr_cells;

			if ((tmp = check_body_callable(p, rhs)) != NULL)
				return tmp;
		}
	}

	return !is_callable(c) && !is_variable(c) ? c : NULL;
}

static cell *term_to_body_conversion(parser *p, cell *c)
{
	idx_t c_idx = c - p->t->cells;

	if (IS_XFX(c) || IS_XFY(c)) {
		if (!strcmp(PARSER_GET_STR(c), ",")
			|| !strcmp(PARSER_GET_STR(c), ";")
			|| !strcmp(PARSER_GET_STR(c), "->")
			|| !strcmp(PARSER_GET_STR(c), ":-")) {
			cell *lhs = c + 1;

			if (is_variable(lhs)) {
				c = insert_here(p, c, lhs);
				lhs = c + 1;
			} else
				lhs = term_to_body_conversion(p, lhs);

			cell *rhs = lhs + lhs->nbr_cells;
			c = p->t->cells + c_idx;

			if (is_variable(rhs))
				c = insert_here(p, c, rhs);
			else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + lhs->nbr_cells + rhs->nbr_cells;
		}
	}

	if (IS_FY(c)) {
			if (!strcmp(PARSER_GET_STR(c), "\\+")) {
			cell *rhs = c + 1;

			if (is_variable(rhs)) {
				c = insert_here(p, c, rhs);
				rhs = c + 1;
			} else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + rhs->nbr_cells;
		}
	}

	return p->t->cells + c_idx;
}

void parser_term_to_body(parser *p)
{
	term_to_body_conversion(p, p->t->cells);
	p->t->cells->nbr_cells = p->t->cidx - 1;
}

static bool attach_ops(parser *p, idx_t start_idx)
{
	idx_t lowest = IDX_MAX, work_idx, end_idx = p->t->cidx - 1;
	bool do_work = false, bind_le = false;

	for (idx_t i = start_idx; i < p->t->cidx;) {
		cell *c = p->t->cells + i;

		//printf("*** OP0 %s type=%u, specifier=%u, pri=%u\n", PARSER_GET_STR(c), c->val_type, GET_OP(c), c->priority);

		if ((c->nbr_cells > 1) || !is_literal(c) || !c->priority) {
			i += c->nbr_cells;
			continue;
		}

		if ((i == start_idx) && (i == end_idx)) {
			c->priority = 0;
			i++;
			continue;
		}

		if (bind_le ? c->priority <= lowest : c->priority < lowest) {
			lowest = c->priority;
			work_idx = i;
			do_work = true;
		}

		bind_le = IS_XFY(c) || IS_FY(c) ? true : false;
		i++;
	}

	if (!do_work)
		return false;

	idx_t last_idx = 0;

	for (idx_t i = start_idx; i <= end_idx;) {
		cell *c = p->t->cells + i;

		if ((c->nbr_cells > 1) || !is_literal(c) || !c->priority) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		if ((c->priority != lowest) || (i != work_idx)) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		//printf("*** OP1 %s type=%u, specifier=%u, pri=%u\n", PARSER_GET_STR(c), c->val_type, GET_OP(c), c->priority);

		c->val_type = TYPE_LITERAL;
		c->arity = 1;

		// Prefix...

		if (IS_FX(c)) {
			cell *rhs = c + 1;

			if (IS_FX(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line nbr %d\n", p->line_nbr);

				p->error = true;
				return false;
			}

			rhs += rhs->nbr_cells;

			if ((((idx_t)(rhs - p->t->cells)) < end_idx)
				&& IS_XF(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line nbr %d\n", p->line_nbr);

				p->error = true;
				return false;
			}
		}

		if (IS_FX(c) || IS_FY(c)) {
			cell *rhs = c + 1;
			c->nbr_cells += rhs->nbr_cells;
			idx_t off = (idx_t)(rhs - p->t->cells);

			if (off > end_idx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: missing operand to '%s', line nbr %d\n", PARSER_GET_STR(c), p->line_nbr);

				p->error = true;
				return false;
			}

			break;
		}

		// Postfix...

		cell *rhs = c + 1;
		cell save = *c;

		if (IS_XF(rhs) && (rhs->priority == c->priority)) {
			if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line nbr %d\n", p->line_nbr);

			p->error = true;
			return false;
		}

		if (IS_XF(c) || IS_YF(c)) {
			cell *lhs = p->t->cells + last_idx;
			save.nbr_cells += lhs->nbr_cells;
			idx_t cells_to_move = lhs->nbr_cells;
			lhs = c - 1;

			while (cells_to_move--)
				*c-- = *lhs--;

			*c = save;
			break;
		}

		// Infix...

		idx_t off = (idx_t)(rhs - p->t->cells);

		if (off > end_idx) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: missing operand to '%s', line nbr %d\n", PARSER_GET_STR(c), p->line_nbr);

			p->error = true;
			return false;
		}

		cell *lhs = p->t->cells + last_idx;
		save.nbr_cells += lhs->nbr_cells;
		idx_t cells_to_move = lhs->nbr_cells;
		lhs = c - 1;

		while (cells_to_move--)
			*c-- = *lhs--;

		*c = save;
		c->nbr_cells += rhs->nbr_cells;
		c->arity = 2;

		if (IS_XFX(c)) {
			cell *next = c + c->nbr_cells;
			i = next - p->t->cells;

			if ((i <= end_idx)
				&& (IS_XFX(next))
				&& (next->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line nbr %d\n", p->line_nbr);

				p->error = true;
				return false;
			}
		}

		break;
	}

	return true;
}

static bool parser_attach(parser *p, idx_t start_idx)
{
	while (attach_ops(p, start_idx))
		;

	return !p->error;
}

void parser_reset(parser *p)
{
	clear_term(p->t);
	p->t->cidx = 0;
	p->start_term = true;
	p->line_nbr = 0;
}

static bool parser_dcg_rewrite(parser *p)
{
	if (p->error || !is_literal(p->t->cells) || (p->t->cells->arity != 2))
		return false;

	if (strcmp(PARSER_GET_STR(p->t->cells), "-->"))
		return false;

	// Being conservative here (for now) and using
	// temp parser/query objects...

	query *q = create_query(p->m, false);
	ensure(q);
	char *dst = print_term_to_strbuf(q, p->t->cells, 0, -1);
	char *src = malloc(strlen(dst)+256);
	ensure(src);
	sprintf(src, "dcg_translate((%s),_TermOut).", dst);
	free(dst);

	parser *p2 = create_parser(p->m);
	ensure(p2);
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = src;
	parser_tokenize(p2, false, false);
	parser_xref(p2, p2->t, NULL);
	query_execute(q, p2->t);
	free(src);
	frame *g = GET_FRAME(0);
	src = NULL;

	for (unsigned i = 0; i < p2->t->nbr_vars; i++) {
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

		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		src = print_term_to_strbuf(q, c, q->latest_ctx, -1);
		strcat(src, ".");
		break;
	}

	if (!src) {
		destroy_parser(p2);
		destroy_query(q);
		p->error = true;
		return false;
	}

	parser_reset(p2);
	p2->srcptr = src;
	parser_tokenize(p2, false, false);
	free(src);

	// Take the completed term...

	clear_term(p->t);
	free(p->t);
	p->t = p2->t;
	p2->t = NULL;
	p->nbr_vars = p2->nbr_vars;

	destroy_parser(p2);
	destroy_query(q);
	return true;
}

static cell *make_literal(parser *p, idx_t offset)
{
	cell *c = make_cell(p);
	c->val_type = TYPE_LITERAL;
	c->nbr_cells = 1;
	c->val_off = offset;
	return c;
}

static int get_octal(const char **srcptr)
{
	const char *src = *srcptr;
	int v = 0;

	while (*src == '0')
		src++;

	while ((*src >= '0') && (*src <= '7')) {
		v *= 8;
		char ch = *src++;
		v += ch - '0';
	}

	*srcptr = src;
	return v;
}

static int get_hex(const char **srcptr, int n)
{
	const char *src = *srcptr;
	int v = 0;

	while ((n > 0) && (*src == '0')) {
		src++; n--;
	}

	while ((n > 0) && (((*src >= '0') && (*src <= '9')) ||
		((*src >= 'a') && (*src <= 'f')) ||
		((*src >= 'A') && (*src <= 'F')))) {
		v *= 16;
		char ch = *src++;
		n--;

		if ((ch >= 'a') && (ch <= 'f'))
			v += 10 + (ch - 'a');
		else if ((ch >= 'A') && (ch <= 'F'))
			v += 10 + (ch - 'A');
		else
			v += ch - '0';
	}

	*srcptr = src;
	return v;
}

const char *g_escapes = "\e\a\f\b\t\v\r\n\x20\x7F";
const char *g_anti_escapes = "eafbtvrnsd";

static int get_escape(const char **_src, bool *error)
{
	const char *src = *_src;
	int ch = *src++;
	const char *ptr = strchr(g_anti_escapes, ch);

	if (ptr)
		ch = g_escapes[ptr-g_anti_escapes];
	else if (isdigit(ch) || (ch == 'x') || (ch == 'u') || (ch == 'U')) {
		int unicode = 0;

		if (ch == 'U') {
			ch = get_hex(&src, 8);
			unicode = 1;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4);
			unicode = 1;
		} else if (ch == 'x')
			ch = get_hex(&src, 999);
		else {
			src--;
			ch = get_octal(&src);
		}

		if (!unicode && (*src++ != '\\')) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: syntax error, closing \\ missing\n");
			*_src = src;
			*error = true;
			return 0;
		}
	} else if ((ch != '\\') && (ch != '"') && (ch != '\'') && (ch != '\r') && (ch != '\n')) {
		*_src = src;
		*error = true;
		return 0;
	}

	*_src = src;
	return ch;
}

#define isbdigit(ch) (((ch) >= '0') && ((ch) <= '1'))
#define isodigit(ch) (((ch) >= '0') && ((ch) <= '7'))

static int parse_number(parser *p, const char **srcptr, bool neg)
{
	p->v.val_num = 0;
	p->v.val_den = 1;
	p->v.flags = 0;
	const char *s = *srcptr;

	if ((*s == '.') && isdigit(s[1])) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error parsing number, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

		p->error = true;
		return -1;
	}

	if (!isdigit(*s))
		return 0;

	if ((*s == '0') && (s[1] == '\'')) {
		s += 2;
		int v;

		if (*s == '\\') {
			s++;
			v = get_escape(&s, &p->error);
		} else
			v = get_char_utf8(&s);

		p->v.val_type = TYPE_INTEGER;
		p->v.val_num = v;
		if (neg) p->v.val_num = -p->v.val_num;
		*srcptr = s;
		return 1;
	}

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
	__int128_t v = 0;
#else
	int_t v = 0;
#endif

	if ((*s == '0') && (s[1] == 'b')) {
		s += 2;

		while (isbdigit(*s)) {
			v <<= 1;
			v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, integer overflow, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		if (isdigit(*s) || isalpha_utf8(peek_char_utf8(s))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing binary number, line %u, file '%s\n", p->line_nbr, get_filename(p->m->filename));

			p->error = true;
			return -1;
		}

		p->v.val_type = TYPE_INTEGER;
		p->v.flags |= FLAG_BINARY;
		p->v.val_num = (int_t)v;
		if (neg) p->v.val_num = -p->v.val_num;
		*srcptr = s;
		return 1;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		while (isodigit(*s)) {
			v <<= 3;
			v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, integer overflow, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		if (isdigit(*s) || isalpha_utf8(peek_char_utf8(s))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing octal number, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

			p->error = true;
			return -1;
		}

		p->v.val_type = TYPE_INTEGER;
		p->v.flags |= FLAG_OCTAL;
		p->v.val_num = (int_t)v;
		if (neg) p->v.val_num = -p->v.val_num;
		*srcptr = s;
		return 1;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		while (isxdigit(*s)) {
			v <<= 4;

			if ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))
				v += 10 + (toupper(*s) - 'A');
			else
				v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, integer overflow, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		if (isdigit(*s) || isalpha_utf8(peek_char_utf8(s))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing hex number, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

			p->error = true;
			return -1;
		}

		p->v.val_type = TYPE_INTEGER;
		p->v.flags |= FLAG_HEX;
		p->v.val_num = (int_t)v;
		if (neg) p->v.val_num = -p->v.val_num;
		*srcptr = s;
		return 1;
	}

	char *tmpptr = (char*)s;

	while (isdigit(*s)) {
		v *= 10;
		v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		if ((v > INT64_MAX) || (v < INT64_MIN)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, integer overflow, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

			p->error = true;
			return -1;
		}
#endif

		s++;
	}

	if ((*s == '.') && isdigit(s[1])) {
		p->v.val_type = TYPE_FLOAT;
		p->v.val_flt = strtod(s=tmpptr, &tmpptr);
		if (neg) p->v.val_flt = -p->v.val_flt;
		*srcptr = tmpptr;
		return 1;
	}

	if (isalpha_utf8(peek_char_utf8(s))) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

		p->error = true;
		return -1;
	}

	p->v.val_type = TYPE_INTEGER;
	p->v.val_num = (int_t)v;
		if (neg) p->v.val_num = -p->v.val_num;
	int try_rational = 0;

#if 0
	module *m = p->m;

	if ((*s == 'r') || (*s == 'R'))
		try_rational = 1;
	else if ((*s == '/') && p->flag.rational_syntax_natural)
		try_rational = 1;
#endif

	if (!try_rational) {
		strtod(tmpptr, &tmpptr);

		if ((tmpptr[-1] == '.')  || isspace(tmpptr[-1]))
			tmpptr--;

		*srcptr = tmpptr;
		s = *srcptr;
		if ((*s == '(') || isalpha_utf8(peek_char_utf8(s))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

			p->error = true;
			return -1;
		}

		return 1;
	}

	s++;
	v = 0;

	while ((*s >= '0') && (*s <= '9')) {
		v *= 10;
		v += *s - '0';
		s++;
	}

	p->v.val_den = (int_t)v;
	cell tmp;
	tmp.val_num = p->v.val_num;
	tmp.val_den = p->v.val_den;
	do_reduce(&tmp);
	p->v.val_num = tmp.val_num;
	p->v.val_den = tmp.val_den;
	if (neg) p->v.val_num = -p->v.val_num;
	*srcptr = s;
	return 1;
}

static int is_matching_pair(char **dst, char **src, int lh, int rh)
{
	char *s = *src, *d = *dst;

	if (*s != lh)
		return 0;

	while (s++, isspace(*s))
		;

	if (*s != rh)
		return 0;

	s++;
	*d++ = lh;
	*d++ = rh;
	*d = '\0';
	*dst = d;
	*src = s;
	return 1;
}

static bool valid_float(const char *src)
{
	if (*src == '-')
		src++;

	while (isdigit(*src))
		src++;

	if (*src != '.')
		return false;

	src++;

	if (!isdigit(*src))
		return false;

	return true;
}

static const char *eat_space(parser *p)
{
	const char *src = p->srcptr;
	bool done;

	do {
		done = true;

		while (isspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}

		while ((*src == '%') && !p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			src++;

			while (isspace(*src)) {
				if (*src == '\n')
					p->line_nbr++;

				src++;
			}

			done = false;
			continue;
		}

		while ((!*src || (*src == '%')) && p->fp) {
			if (getline(&p->save_line, &p->n_line, p->fp) == -1)
				return NULL;

			p->srcptr = p->save_line;
			src = p->srcptr;

			while (isspace(*src)) {
				if (*src == '\n')
					p->line_nbr++;

				src++;
			}

			done = false;
			continue;
		}

		do {
			if (!p->comment && (src[0] == '/') && (src[1] == '*')) {
				p->comment = true;
				src += 2;
				continue;
			}

			if (p->comment && (src[0] == '*') && (src[1] == '/')) {
				p->comment = false;
				src += 2;
				done = false;
				continue;
			}

			if (*src == '\n')
				p->line_nbr++;

			if (p->comment)
				src++;

			if (!*src && p->comment && p->fp) {
				if (getline(&p->save_line, &p->n_line, p->fp) == -1)
					return NULL;

				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->comment);
	}
	 while (!done);

	while (isspace(*src)) {
		if (*src == '\n')
			p->line_nbr++;

		src++;
	}

	return src;
}

static bool get_token(parser *p, int last_op)
{
	if (p->error)
		return false;

	const char *src = p->srcptr;
	char *dst = p->token;
	*dst = '\0';
	bool neg = false;
	p->v.val_type = TYPE_LITERAL;
	p->quote_char = 0;
	p->string = p->is_quoted = p->is_variable = p->is_op = false;

	if (p->dq_consing && (*src == '"')) {
		*dst++ = ']';
		*dst = '\0';
		p->srcptr = (char*)++src;
		p->dq_consing = 0;
		return true;
	}

	if (p->dq_consing < 0) {
		*dst++ = ',';
		*dst = '\0';
		p->dq_consing = 1;
		return true;
	}

	if (p->dq_consing) {
		int ch = get_char_utf8(&src);

		if ((ch == '\\') && p->flag.character_escapes) {
			ch = get_escape(&src, &p->error);

			if (p->error) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d\n", p->srcptr, p->line_nbr);

				p->error = true;
				return false;
			}
		}

		dst += snprintf(dst, 8, "%u", ch);
		*dst = '\0';
		p->srcptr = (char*)src;
		p->v.val_num = ch;
		p->v.val_type = TYPE_INTEGER;
		p->dq_consing = -1;
		return true;
	}

	if (!(src = eat_space(p)))
		return false;

	if (!*src) {
		p->srcptr = (char*)src;
		return false;
	}

	// -ve numbers (note there are no explicitly +ve numbers)

	if ((*src == '-') && last_op) {
		const char *save_src = src++;

		while (isspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}

		if (isdigit(*src)) {
			if (*save_src == '-')
				neg = true;
		} else
			src = save_src;
	}

	// Numbers...

	const char *tmpptr = src;

	if ((*src != '-') && parse_number(p, &src, neg)) {
		if ((size_t)(src-tmpptr) >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token+offset;
		}

		strncpy(dst, tmpptr, src-tmpptr);
		dst[src-tmpptr] = '\0';

		if ((strchr(dst, '.') || strchr(dst, 'e') || strchr(dst, 'E')) && !strchr(dst, '\'')) {
			if (!valid_float(p->token)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, float, line %u, file '%s'\n", p->line_nbr, get_filename(p->m->filename));

				p->error = true;
				return false;
			}
		}

		p->srcptr = (char*)src;
		return true;
	}

	// Quoted strings...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quote_char = *src++;
		p->is_quoted = true;

		if ((p->quote_char == '"') && p->flag.double_quote_codes) {
			*dst++ = '[';

			if (*src == p->quote_char) {
				*dst++ = ']';
				*dst = '\0';
				p->srcptr = (char*)++src;
				return true;
			}

			*dst = '\0';
			p->dq_consing = 1;
			p->quote_char = 0;
			p->srcptr = (char*)src;
			return true;
		} else if ((p->quote_char == '"') && p->flag.double_quote_chars)
			p->string = true;

		for (;;) {
			while (*src) {
				int ch = get_char_utf8(&src);

				if ((ch == p->quote_char) && (*src == ch)) {
					ch = *src++;
				} else if (ch == p->quote_char) {
					if ((ch == '"') && !*p->token && p->string) {
						dst += put_char_utf8(dst, ch='[');
						dst += put_char_utf8(dst, ch=']');
						*dst = '\0';
						p->string = false;
					}

					p->quote_char = 0;
					break;
				}

				if ((ch == '\\') && p->flag.character_escapes) {
					int ch2 = *src;
					ch = get_escape(&src, &p->error);

					if (!p->error) {
						if (ch2 == '\n') {
							//p->line_nbr++;
							continue;
						}
					} else {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d\n", p->srcptr, p->line_nbr);

						p->error = true;
						return false;
					}
				}

				size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

				if (len >= p->token_size) {
					size_t offset = dst - p->token;
					p->token = realloc(p->token, p->token_size*=2);
					ensure(p->token);
					dst = p->token+offset;
				}

				dst += put_char_utf8(dst, ch);
				*dst = '\0';
			}

			if (p->quote_char && p->fp) {
				if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = (char*)src;
					return true;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			if (get_op(p->m, p->token, NULL, false)) {
				p->is_op = true;

				if (!strcmp(p->token, ","))
					p->quote_char = -1;
			} else
				p->quote_char = -1;

			p->toklen = dst - p->token;
			p->srcptr = (char*)src;
			return true;
		}
	}

	int ch = peek_char_utf8(src);

	// Atoms...

	ensure(!p->error, "fallen through from above");

	if (isalpha_utf8(ch) || (ch == '_')) {
		while (isalnum_utf8(ch) || (ch == '_') ||
			((ch == ':') && find_module(p->m->pl, p->token))) {

			if ((src[0] == ':') && (src[1] == ':'))	// HACK
				break;

			ch = get_char_utf8(&src);

			size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

			if (len >= p->token_size) {
				size_t offset = dst - p->token;
				p->token = realloc(p->token, p->token_size*=2);
				ensure(p->token);
				dst = p->token+offset;
			}

			dst += put_char_utf8(dst, ch);
			*dst = '\0';
			ch = peek_char_utf8(src);
		}

		if (isupper(*p->token) || (*p->token == '_'))
			p->is_variable = true;
		else if (get_op(p->m, p->token, NULL, false))
			p->is_op = true;

		if (isspace(ch)) {
			p->srcptr = (char*)src;
			src = eat_space(p);

			if (!p->is_op && (*src == '(')) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, or operator expected, line %d: %s, %s\n", p->line_nbr, p->token, p->srcptr);

				p->error = true;
			}
		}

		p->srcptr = (char*)src;
		return true;
	}

	if (is_matching_pair(&dst, (char**)&src, '[',']') ||
		is_matching_pair(&dst, (char**)&src, '{','}')) {
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	if ((src[0] == '=') && (src[1] == '.') && (src[2] == '.')) {
		dst += sprintf(dst, "=..");
		p->srcptr = (char*)src+3;
		return (dst - p->token) != 0;
	}


	if (src[0] == '!') {
		*dst++ = *src++;
		*dst = '\0';
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	static const char *s_delims = ".!(){}[]|_,`'\"\t\r\n ";

	while (*src) {
		ch = get_char_utf8(&src);

		size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

		if (len >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token+offset;
		}

		dst += put_char_utf8(dst, ch);
		*dst = '\0';

		if (strchr(s_delims, ch))
			break;


		ch = peek_char_utf8(src);

		if (strchr(s_delims, ch) || isalnum_utf8(ch) || (ch == '_'))
			break;
	}

	p->is_op = get_op(p->m, p->token, NULL, false);
	p->srcptr = (char*)src;
	return true;
}

bool is_valid_list(query *q, cell *p1, idx_t p1_ctx, bool allow_partials)
{
	if (!is_list(p1) && !is_nil(p1))
		return false;

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		LIST_HEAD(p1);
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return is_nil(p1) || (allow_partials && is_variable(p1));
}

size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, bool allow_integers)
{
	idx_t save_ctx = q ? q->latest_ctx : l_ctx;
	size_t is_chars_list = 0;
	LIST_HANDLER(l);
	int depth = 0;

	while (is_iso_list(l) && q->m->flag.double_quote_chars) {
		if (depth++ >= MAX_DEPTH) {
			is_chars_list = 0;
			break;
		}

		cell *h = LIST_HEAD(l);
		cell *c = q ? deref(q, h, l_ctx) : h;

		if (is_integer(c) && !allow_integers) {
			is_chars_list = 0;
			break;
		} else if (!is_integer(c) && !is_atom(c)) {
			is_chars_list = 0;
			break;
		}

		if (is_integer(c)) {
			int ch = c->val_num;
			char tmp[20];
			put_char_utf8(tmp, ch);
			size_t len = len_char_utf8(tmp);
			is_chars_list += len;
		} else {
			const char *src = GET_STR(c);
			size_t len = len_char_utf8(src);

			if (len != LEN_STR(c)) {
				is_chars_list = 0;
				break;
			}

			is_chars_list += len;
		}

		l = LIST_TAIL(l);
		l = q ? deref(q, l, l_ctx) : l;
		if (q) l_ctx = q->latest_ctx;
	}

	if (is_variable(l))
		is_chars_list = 0;
	else if (is_string(l))
		;
	else if (!is_literal(l) || (l->val_off != g_nil_s))
		is_chars_list = 0;

	if (q) q->latest_ctx = save_ctx;
	return is_chars_list;
}

void fix_list(cell *c)
{
	idx_t cnt = c->nbr_cells;

	while (is_iso_list(c)) {
		c->nbr_cells = cnt;
		c = c + 1;					// skip .
		cnt -= 1 + c->nbr_cells;
		c = c + c->nbr_cells;		// skip head
	}
}

unsigned parser_tokenize(parser *p, bool args, bool consing)
{
	idx_t begin_idx = p->t->cidx, arg_idx = p->t->cidx, save_idx = 0;
	bool last_op = true, is_func = false, was_consing = false;
	bool last_bar = false;
	unsigned arity = 1;
	p->depth++;

	while (get_token(p, last_op)) {
		if (p->error)
			break;

		//fprintf(stdout, "Debug: token '%s' quoted=%d, val_type=%u, op=%d, lastop=%d, string=%d\n", p->token, p->quote_char, p->v.val_type, p->is_op, last_op, p->string);

		if (!p->quote_char && !strcmp(p->token, ".")
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '|')) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (DUMP_ERRS || !p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, line nbr %u\n", p->line_nbr);

				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (parser_attach(p, 0)) {

				if (p->t->cells->nbr_cells < (p->t->cidx-1)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: syntax error, operator expected '%s', line nbr %u\n", p->token, p->line_nbr);

					p->error = true;
				}

				parser_assign_vars(p, p->read_term, false);
				parser_term_to_body(p);

				if (p->consulting && !p->skip) {
					if (!parser_dcg_rewrite(p))
						directives(p, p->t);

					if (p->already_loaded)
						break;

					cell *h = get_head(p->t->cells);

					if (is_cstring(h)) {
						idx_t off = index_from_pool(p->m->pl, PARSER_GET_STR(h));
						if (off == ERR_IDX) {
							p->error = true;
							break;
						}

						DECR_REF(h);
						h->val_type = TYPE_LITERAL;
						h->val_off = off;
						h->flags = 0;
					}

					if (!p->error && !assertz_to_db(p->m, p->t, 1)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: '%s', line nbr %u\n", p->token, p->line_nbr);

						p->error = true;
					}
				}
			}

			p->end_of_term = true;
			last_op = true;

			if (p->one_shot)
				break;

			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "[")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			p->nesting_brackets++;
			parser_tokenize(p, true, true);
			last_bar = false;

			if (p->error)
				break;

			make_literal(p, g_nil_s);
			c = p->t->cells + save_idx;
			c->nbr_cells = p->t->cidx - save_idx;
			fix_list(c);
			p->start_term = false;
			last_op = false;
			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "{")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, g_braces_s);
			ensure(c);
			c->arity = 1;
			p->start_term = true;
			p->nesting_braces++;
			parser_tokenize(p, false, false);
			last_bar = false;

			if (p->error)
				break;

			c = p->t->cells+save_idx;
			c->nbr_cells = p->t->cidx - save_idx;
			p->start_term = false;
			last_op = false;
			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "(")) {
			p->start_term = true;
			p->nesting_parens++;
			unsigned tmp_arity = parser_tokenize(p, is_func, false);
			last_bar = false;

			if (p->error)
				break;

			if (is_func) {
				cell *c = p->t->cells + save_idx;
				c->arity = tmp_arity;
				c->nbr_cells = p->t->cidx - save_idx;
			}

			is_func = false;
			last_op = false;
			p->start_term = false;
			continue;
		}

		if (!p->quote_char && args && !consing && p->is_op
			&& strcmp(p->token, ",")
			) {
			unsigned specifier = 0;
			unsigned priority = get_op(p->m, p->token, &specifier, last_op);

			if (!last_op && (priority > 1000)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: suggest parens around operator '%s', line %d: %s\n", p->token, p->line_nbr, p->srcptr);

				p->error = true;
				break;
			}
		}

		if (!p->quote_char && !strcmp(p->token, ",") && consing) {
			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing element\n");

				p->error = true;
				break;
			}

			if (was_consing) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parsing list1\n");

				p->error = true;
				break;
			}

			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			last_op = true;
			continue;
		}

		if (!p->quote_char && args && !strcmp(p->token, ",")) {
			parser_attach(p, arg_idx);
			arg_idx = p->t->cidx;

			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing arg\n");

				p->error = true;
				break;
			}

			arity++;

			if (arity > MAX_ARITY) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: max arity reached, line %d: %s\n", p->line_nbr, p->srcptr);

				p->error = true;
				break;
			}

			last_op = true;
			continue;
		}

		if (!p->is_quoted && consing && p->start_term && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list2\n");

			p->error = true;
			break;
		}

		if (!p->is_quoted && was_consing && consing && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list3\n");

			p->error = true;
			break;
		}

		if (!p->is_quoted && consing && !strcmp(p->token, "|")) {
			last_bar = true;
			was_consing = true;
			//consing = false;
			continue;
		}

		if (!p->is_quoted && was_consing && last_bar && !strcmp(p->token, "]")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list4\n");

			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!strcmp(p->token, ",") || !strcmp(p->token, "]") || !strcmp(p->token, ")") || !strcmp(p->token, "}"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, start of term expected, line %d: %s\n", p->line_nbr, p->srcptr);

			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, ")")) {
			p->nesting_parens--;
			parser_attach(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "]")) {
			p->nesting_brackets--;
			parser_attach(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "}")) {
			p->nesting_braces--;
			parser_attach(p, begin_idx);
			return arity;
		}

		if (p->is_variable && (*p->srcptr == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, line %d: %s\n", p->line_nbr, p->srcptr);

			p->error = true;
			break;
		}

		unsigned specifier = 0;
		int priority = get_op(p->m, p->token, &specifier, last_op);

		if (!strcmp(p->token, "!") &&
			((*p->srcptr == ',') || (*p->srcptr == '.')))
			p->quote_char = 1;

		if (p->quote_char) {
			specifier = 0;
			priority = 0;
		}

		if (priority && (last_op || last_bar)
			&& !IS_POSTFIX(specifier)
			&& ((*p->srcptr == ',')
				|| (*p->srcptr == ')')
				|| (*p->srcptr == '|')
				|| (*p->srcptr == ']')
				|| (*p->srcptr == '}')
			)) {
			specifier = 0;
			priority = 0;
		}

#if 0
		if (priority
			&& ((specifier == OP_XF) || (specifier == OP_YF))
			&& last_op) {
			specifier = 0;
			priority = 0;
		}
#endif

		// Operators in canonical form..

		if (last_op && priority && (*p->srcptr == '(')) {
			p->v.val_type = TYPE_LITERAL;
			specifier = 0;
			priority = 0;
			p->quote_char = 0;
		}

		last_op = strcmp(p->token, ")") && priority;
		int func = (p->v.val_type == TYPE_LITERAL) && !specifier && (*p->srcptr == '(');

		if (func) {
			is_func = true;
			p->is_op = false;
			save_idx = p->t->cidx;
		}

#if 0
		if (p->is_op && !priority) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, or operator expected, line %d: %s, %s\n", p->line_nbr, p->token, p->srcptr);

			p->error = true;
			break;
		}
#endif

		p->start_term = false;
		cell *c = make_cell(p);
		c->nbr_cells = 1;
		c->val_type = p->v.val_type;
		SET_OP(c,specifier);
		c->priority = priority;
		bool found = false;

		if (p->v.val_type == TYPE_INTEGER) {
			c->val_num = p->v.val_num;
			c->val_den = p->v.val_den;
			c->flags = p->v.flags;
		}
		else if (p->v.val_type == TYPE_FLOAT) {
			c->val_flt = p->v.val_flt;
		} else if ((!p->is_quoted || func || p->is_op || p->is_variable ||
			(get_builtin(p->m->pl, p->token, 0, &found), found) ||
			!strcmp(p->token, "[]")) && !p->string) {

			if (func && !strcmp(p->token, "."))
				c->priority = 0;

			if (p->is_variable)
				c->val_type = TYPE_VARIABLE;

			if (p->is_quoted)
				c->flags |= FLAG2_QUOTED;

			c->val_off = index_from_pool(p->m->pl, p->token);
			ensure(c->val_off != ERR_IDX);
		} else {
			c->val_type = TYPE_CSTRING;

			if ((p->toklen < MAX_SMALL_STRING) && !p->string)
				memcpy(c->val_chr, p->token, p->toklen+1);
			else {
				if (p->string) {
					c->flags |= FLAG_STRING;
					c->arity = 2;
				}

				c->flags |= FLAG_BLOB;
				SET_STR(c, p->token, p->toklen, 0);
			}
		}

		last_bar = false;
	}

	p->depth--;
	return !p->error;
}

static bool parser_run(parser *p, const char *src, int dump)
{
	if (*src == '.') {
		fprintf(stdout, "Error: syntax error, unexpected end of clause\n");
		return false;
	}

	p->srcptr = (char*)src;
	p->line_nbr = 0;
	parser_tokenize(p, false, false);

	if (!p->error && !p->end_of_term && !p->run_init) {
		fprintf(stdout, "Error: syntax error, missing operand or operator\n");
		p->error = true;
	}

	if (p->error)
		return false;

	if (p->skip) {
		p->m->pl->status = 1;
		return true;
	}

	if (!parser_attach(p, 0))
		return false;

	parser_assign_vars(p, 0, false);

	if (!p->command)
		parser_dcg_rewrite(p);

	parser_xref(p, p->t, NULL);

	query *q = create_query(p->m, false);
	if (!q) return false;
	q->p = p;
	q->do_dump_vars = dump;
	q->run_init = p->run_init;
	query_execute(q, p->t);
	p->m->pl->halt = q->halt;
	p->m->pl->halt_code = q->halt_code;
	p->m->pl->status = q->status;

	if (!p->m->pl->quiet && !p->directive && dump && q->m->pl->stats) {
		fprintf(stdout,
			"Goals %llu, Matches %llu, Max frames %u, Max choices %u, Max trails: %u, Backtracks %llu, TCOs:%llu\n",
			(unsigned long long)q->tot_goals, (unsigned long long)q->tot_matches,
			q->max_frames, q->max_choices, q->max_trails,
			(unsigned long long)q->tot_retries, (unsigned long long)q->tot_tcos);
	}

	query_purge_dirty_list(q);

	if (dump)
		module_purge_dirty_list(q->m);

	bool ok = !q->error;
	p->m = q->m;
	destroy_query(q);
	return ok;
}

module *module_load_text(module *m, const char *src, const char *filename)
{
	parser *p = create_parser(m);
	if (!p) return NULL;

	char *save_filename = p->m->filename;
	p->m->filename = strdup(filename);
	p->consulting = true;
	p->srcptr = (char*)src;
	parser_tokenize(p, false, false);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->t->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	if (!p->error) {
		parser_xref_db(p);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->m->pl->halt = false;
		p->directive = true;

		if (p->run_init == true) {
			p->command = true;

			if (parser_run(p, "(:- initialization(G)), retract((:- initialization(_))), G", 0))
				p->m->pl->halt = true;
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	m = p->m;
	free(p->m->filename);
	p->m->filename = save_filename;
	destroy_parser(p);
	return m;
}

bool module_load_fp(module *m, FILE *fp)
{
	parser *p = create_parser(m);
	if (!p) return false;

	p->consulting = true;
	p->fp = fp;
	bool ok = false;

	do {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1)
			break;

		p->srcptr = p->save_line;
		parser_tokenize(p, false, false);
		ok = !p->error;
	}
	 while (ok && !p->already_loaded);

	free(p->save_line);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->t->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	if (!p->error && !p->already_loaded) {
		parser_xref_db(p);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->directive = true;

		if (p->run_init == true) {
			p->command = true;

			if (parser_run(p, "(:- initialization(G)), retract((:- initialization(_))), G", 0))
				p->m->pl->halt = true;
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	ok = !p->error;
	destroy_parser(p);
	return ok;
}

bool module_load_file(module *m, const char *filename)
{
	char *save_filename = m->filename;
	m->filename = strdup(filename);

	if (!strcmp(filename, "user")) {
		for (int i = 0; i < MAX_STREAMS; i++) {
			stream *str = &g_streams[i];

			if (!strcmp(str->name, "user_input")) {
				m->filename = strdup("./");
				int ok = module_load_fp(m, str->fp);
				clearerr(str->fp);
				free(m->filename);
				m->filename = save_filename;
				return ok;
			}
		}
	}

	size_t len = strlen(filename);
	char *tmpbuf = malloc(len + 20);
	memcpy(tmpbuf, filename, len+1);

	if (tmpbuf[0] == '~') {
		const char *ptr = getenv("HOME");

		if (ptr) {
			tmpbuf = realloc(tmpbuf, strlen(ptr) + 10 + strlen(filename) + 20);
			strcpy(tmpbuf, ptr);
			strcat(tmpbuf, filename+1);
		}
	}

	char *realbuf = NULL;

	if (!(realbuf = realpath(tmpbuf, NULL))) {
		strcpy(tmpbuf, filename);
		strcat(tmpbuf, ".pl");

		if (!(realbuf = realpath(tmpbuf, NULL))) {
			free(tmpbuf);
			free(m->filename);
			m->filename = save_filename;
			return 0;
		}
	}

	free(tmpbuf);
	FILE *fp = fopen(realbuf, "r");

	if (!fp) {
		free(realbuf);
		free(m->filename);
		m->filename = save_filename;
		return 0;
	}

	free(m->filename);
	m->filename = strdup(realbuf);
	bool ok = module_load_fp(m, fp);
	fclose(fp);
	free(realbuf);
	free(m->filename);
	m->filename = save_filename;
	return ok;
}

static void module_save_fp(module *m, FILE *fp, int canonical, int dq)
{
	(void) dq;
	idx_t ctx = 0;
	query q = (query){0};
	q.m = m;

	for (predicate *h = m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.ugen_erased)
				continue;

			if (canonical)
				print_canonical(&q, fp, r->t.cells, ctx, 0);
			else
				print_canonical(&q, fp, r->t.cells, ctx, 0);

			fprintf(fp, "\n");
		}
	}
}

bool module_save_file(module *m, const char *filename)
{
	FILE *fp = fopen(filename, "w");

	if (!fp) {
		fprintf(stdout, "Error: file '%s' cannot be created\n", filename);
		return false;
	}

	module_save_fp(m, fp, 0, 0);
	fclose(fp);
	return true;
}

static void make_rule(module *m, const char *src)
{
	m->prebuilt = true;
	bool save = m->p->consulting;
	m->p->consulting = true;
	m->p->srcptr = (char*)src;
	m->p->line_nbr = 0;
	parser_tokenize(m->p, false, false);
	m->prebuilt = false;
	m->p->consulting = save;
}

void destroy_module(module *m)
{
	module_purge_dirty_list(m);

	while (m->tasks) {
		query *task = m->tasks->next;
		destroy_query(m->tasks);
		m->tasks = task;
	}

	sl_destroy(m->index);

	for (predicate *h = m->head; h;) {
		predicate *save = h->next;

		for (clause *r = h->head; r;) {
			clause *save = r->next;
			clear_term(&r->t);
			free(r);
			r = save;
		}

		sl_destroy(h->index);
		sl_destroy(h->index_save);
		free(h);
		h = save;
	}

	if (m->pl->modules == m) {
		m->pl->modules = m->next;
	} else {
		for (module *tmp = m->pl->modules; tmp; tmp = tmp->next) {
			if (tmp->next == m) {
				tmp->next = m->next;
				break;
			}
		}
	}

	if (m->fp)
		fclose(m->fp);

	for (struct op_table *ptr = m->def_ops; ptr->name; ptr++)
		free(ptr->name);

	for (struct op_table *ptr = m->ops; ptr->name; ptr++)
		free(ptr->name);

	destroy_parser(m->p);
	free(m->filename);
	free(m->name);
	free(m);
}

module *create_module(prolog *pl, const char *name)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	module *m = calloc(1, sizeof(module));
	ensure(m);

	m->pl = pl;
	m->filename = strdup("./");
	m->name = strdup(name);
	m->flag.unknown = UNK_ERROR;
	m->flag.double_quote_chars = true;
	m->flag.character_escapes = true;
	m->spare_ops = MAX_OPS;
	m->error = false;
	struct op_table *ptr2 = m->def_ops;

	for (const struct op_table *ptr = g_ops; ptr->name; ptr++, ptr2++) {
		ptr2->name = strdup(ptr->name);
		ptr2->specifier = ptr->specifier;
		ptr2->priority = ptr->priority;
	}

	m->index = sl_create1(compkey, m);
	ensure(m->index);
	m->p = create_parser(m);
	ensure(m->p);

#include "rules.c"

	parser *p = create_parser(m);
	if (p) {
		p->consulting = true;
		parser_xref_db(p);
		destroy_parser(p);
	}

	if (!m->name || !m->p || m->error || !p) {
		destroy_module(m);
		m = NULL;
	}

	m->next = pl->modules;
	pl->modules = m;
	return m;
}

bool deconsult(prolog *pl, const char *filename)
{
	module *m = find_module(pl, filename);
	if (!m) return false;
	destroy_module(m);
	return true;
}

bool get_halt(prolog *pl) { return pl->halt; }
bool get_status(prolog *pl) { return pl->status; }
bool get_dump_vars(prolog *pl) { return pl->did_dump_vars; }
int get_halt_code(prolog *pl) { return pl->halt_code; }

void set_trace(prolog *pl) { pl->trace = true; }
void set_quiet(prolog *pl) { pl->quiet = true; }
void set_stats(prolog *pl) { pl->stats = true; }
void set_noindex(prolog *pl) { pl->noindex = true; }
void set_opt(prolog *pl, int level) { pl->opt = level; }

bool pl_eval(prolog *pl, const char *s)
{
	parser *p = create_parser(pl->curr_m);
	if (!p) return false;
	size_t len = strlen(s);
	char *cmd = malloc(len+2);
	if(!cmd) return false;
	memcpy(cmd, s, len+1);

	if (cmd[strlen(s)-1] != '.')
		strcat(cmd, ".");

	p->command = true;
	bool ok = parser_run(p, cmd, 1);
	free(cmd);
	pl->curr_m = p->m;
	destroy_parser(p);
	return ok;
}

bool pl_consult_fp(prolog *pl, FILE *fp, const char *filename)
{
	char *save_filename = pl->m->filename;
	pl->m->filename = strdup(filename);
	int ok = module_load_fp(pl->m, fp);
	free(pl->m->filename);
	pl->m->filename = save_filename;
	return ok;
}

bool pl_consult(prolog *pl, const char *filename)
{
	return module_load_file(pl->m, filename);
}

static void g_destroy(prolog *pl)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &g_streams[i];

		if (str->fp) {
			if ((str->fp != stdin)
				&& (str->fp != stdout)
				&& (str->fp != stderr))
				fclose(str->fp);

			free(str->filename);
			free(str->mode);
			free(str->name);
			str->filename = NULL;
			str->name = NULL;
			str->mode = NULL;
		}

		if (str->p)
			destroy_parser(str->p);

		str->p = NULL;
	}

	memset(g_streams, 0, sizeof(g_streams));

	while (pl->modules)
		destroy_module(pl->modules);

	free(g_tpl_lib);
	sl_destroy(pl->funtab);
	sl_destroy(pl->symtab);
	pl->symtab = NULL;
	free(pl->pool);
	pl->pool_offset = 0;
	pl->pool = NULL;
}

static int my_strcmp(__attribute__((unused)) const void *p, const void *k1, const void *k2)
{
	return strcmp(k1, k2);
}

static bool g_init(prolog *pl)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	pl->pool = calloc(pl->pool_size=INITIAL_POOL_SIZE, 1);
	if (pl->pool) {
		bool error = false;

		CHECK_SENTINEL(pl->symtab = sl_create2((void*)my_strcmp, free), NULL);

		if (!error) {
			CHECK_SENTINEL(g_false_s = index_from_pool(pl, "false"), ERR_IDX);
			CHECK_SENTINEL(g_true_s = index_from_pool(pl, "true"), ERR_IDX);
			CHECK_SENTINEL(g_plus_s = index_from_pool(pl, "+"), ERR_IDX);
			CHECK_SENTINEL(g_minus_s = index_from_pool(pl, "-"), ERR_IDX);
			CHECK_SENTINEL(g_pair_s = index_from_pool(pl, ":"), ERR_IDX);
			CHECK_SENTINEL(g_empty_s = index_from_pool(pl, ""), ERR_IDX);
			CHECK_SENTINEL(g_anon_s = index_from_pool(pl, "_"), ERR_IDX);
			CHECK_SENTINEL(g_dot_s = index_from_pool(pl, "."), ERR_IDX);
			CHECK_SENTINEL(g_call_s = index_from_pool(pl, "call"), ERR_IDX);
			CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "braces"), ERR_IDX);
			CHECK_SENTINEL(g_unify_s = index_from_pool(pl, "="), ERR_IDX);
			CHECK_SENTINEL(g_on_s = index_from_pool(pl, "on"), ERR_IDX);
			CHECK_SENTINEL(g_off_s = index_from_pool(pl, "off"), ERR_IDX);
			CHECK_SENTINEL(g_sys_var_s = index_from_pool(pl, "$VAR"), ERR_IDX);
			CHECK_SENTINEL(g_cut_s = index_from_pool(pl, "!"), ERR_IDX);
			CHECK_SENTINEL(g_nil_s = index_from_pool(pl, "[]"), ERR_IDX);
			CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "{}"), ERR_IDX);
			CHECK_SENTINEL(g_fail_s = index_from_pool(pl, "fail"), ERR_IDX);
			CHECK_SENTINEL(g_clause_s = index_from_pool(pl, ":-"), ERR_IDX);
			CHECK_SENTINEL(g_sys_elapsed_s = index_from_pool(pl, "$elapsed"), ERR_IDX);
			CHECK_SENTINEL(g_sys_queue_s = index_from_pool(pl, "$queue"), ERR_IDX);
			CHECK_SENTINEL(g_eof_s = index_from_pool(pl, "end_of_file"), ERR_IDX);
			CHECK_SENTINEL(g_lt_s = index_from_pool(pl, "<"), ERR_IDX);
			CHECK_SENTINEL(g_gt_s = index_from_pool(pl, ">"), ERR_IDX);
			CHECK_SENTINEL(g_eq_s = index_from_pool(pl, "="), ERR_IDX);
			CHECK_SENTINEL(g_stream_property_s = index_from_pool(pl, "$stream_property"), ERR_IDX);

			g_streams[0].fp = stdin;
			CHECK_SENTINEL(g_streams[0].filename = strdup("stdin"), NULL);
			CHECK_SENTINEL(g_streams[0].name = strdup("user_input"), NULL);
			CHECK_SENTINEL(g_streams[0].mode = strdup("read"), NULL);
			g_streams[0].eof_action = eof_action_reset;

			g_streams[1].fp = stdout;
			CHECK_SENTINEL(g_streams[1].filename = strdup("stdout"), NULL);
			CHECK_SENTINEL(g_streams[1].name = strdup("user_output"), NULL);
			CHECK_SENTINEL(g_streams[1].mode = strdup("append"), NULL);
			g_streams[1].eof_action = eof_action_reset;

			g_streams[2].fp = stderr;
			CHECK_SENTINEL(g_streams[2].filename = strdup("stderr"), NULL);
			CHECK_SENTINEL(g_streams[2].name = strdup("user_error"), NULL);
			CHECK_SENTINEL(g_streams[2].mode = strdup("append"), NULL);
			g_streams[2].eof_action = eof_action_reset;
		}

		if (error) {
			g_destroy(pl);
			return NULL;
		}
	}
	return pl->pool ? true : false;
}

void pl_destroy(prolog *pl)
{
	if (!pl) return;

	destroy_module(pl->m);

	if (!--g_tpl_count)
		g_destroy(pl);

	free(pl);
}

prolog *pl_create()
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	prolog *pl = calloc(1, sizeof(prolog));

	if (!g_tpl_count++ && !g_init(pl)) {
		free(pl);
		return NULL;
	}

	if (!g_tpl_lib) {
		char *ptr = getenv("TPL_LIBRARY_PATH");

		if (ptr)
			g_tpl_lib = strdup(ptr);
	}

	if (!g_tpl_lib) {
		g_tpl_lib = realpath(g_argv0, NULL);

		if (g_tpl_lib) {
			char *src = g_tpl_lib + strlen(g_tpl_lib) - 1;

			while ((src != g_tpl_lib) && (*src != '/'))
				src--;

			*src = '\0';
			g_tpl_lib = realloc(g_tpl_lib, strlen(g_tpl_lib)+40);
			strcat(g_tpl_lib, "/library");
		} else
			g_tpl_lib = strdup("../library");
	}

	pl->funtab = sl_create2((void*)my_strcmp, NULL);

	if (pl->funtab)
		load_builtins(pl);

	//printf("Library: %s\n", g_tpl_lib);

	pl->m = create_module(pl, "user");
	if (pl->m) {
		pl->curr_m = pl->m;
		pl->s_last = 0;
		pl->s_cnt = 0;
		pl->seed = 0;
		pl->current_input = 0;		// STDIN
		pl->current_output = 1;		// STDOUT
		pl->current_error = 2;		// STDERR

		set_multifile_in_db(pl->m, "term_expansion", 2);
		set_noindex_in_db(pl->m, "$stream_property", 2);
		set_noindex_in_db(pl->m, "$current_op", 3);

		set_dynamic_in_db(pl->m, "$current_op", 3);
		set_dynamic_in_db(pl->m, "$predicate_property", 2);
		set_dynamic_in_db(pl->m, "$stream_property", 2);
		set_dynamic_in_db(pl->m, "term_expansion", 2);
		set_dynamic_in_db(pl->m, "initialization", 1);
		set_dynamic_in_db(pl->m, ":-", 1);

		pl->m->prebuilt = true;

#if USE_LDLIBS
		for (library *lib = g_libs; lib->name; lib++) {
			if (!strcmp(lib->name, "apply") ||
				//!strcmp(lib->name, "dcgs") ||
				//!strcmp(lib->name, "charsio") ||
				//!strcmp(lib->name, "format") ||
				//!strcmp(lib->name, "http") ||
				//!strcmp(lib->name, "atts") ||
				!strcmp(lib->name, "lists")) {
				size_t len = lib->end-lib->start;
				char *src = malloc(len+1);
				ensure(src); //cehteh: checkthis
				memcpy(src, lib->start, len);
				src[len] = '\0';
				assert(pl->m);
				STRING_INIT(s1);
				STRING_CAT2(s1, "library/", lib->name);
				module_load_text(pl->m, src, STRING_BUF(s1));
				STRING_DONE(s1);
				free(src);
			}
		}
#else
		module_load_file(pl->m, "library/apply.pl");
		//module_load_file(pl->m, "library/dcgs.pl");
		//module_load_file(pl->m, "library/charsio.pl");
		//module_load_file(pl->m, "library/format.pl");
		//module_load_file(pl->m, "library/http.pl");
		//module_load_file(pl->m, "library/atts.pl");
		module_load_file(pl->m, "library/lists.pl");
#endif

		pl->m->prebuilt = false;
	}

	if (!pl->m || pl->m->error || !pl->m->filename) {
		pl_destroy(pl);
		pl = NULL;
	}

	return pl;
}

