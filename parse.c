#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <float.h>
#include <sys/time.h>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#else
#include <unistd.h>
#endif

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

struct prolog_ {
	module *m, *curr_m;
};

stream g_streams[MAX_STREAMS] = {{0}};
skiplist *g_symtab = NULL;
char *g_pool = NULL;
idx_t g_empty_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
idx_t g_anon_s, g_clause_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s;
idx_t g_sys_elapsed_s, g_sys_queue_s, g_false_s, g_braces_s;
unsigned g_cpu_count = 4;

static idx_t g_pool_offset = 0, g_pool_size = 0;
static int g_tpl_count = 0;
char *g_tpl_lib = NULL;

int g_ac = 0, g_avc = 1;
char **g_av = NULL, *g_argv0 = NULL;

static struct op_table g_ops[] =
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

	{"op", OP_FX, 1150},
	{"public", OP_FX, 1150},			// NOT USED
	{"dynamic", OP_FX, 1150},
	{"persist", OP_FX, 1150},
	{"initialization", OP_FX, 1150},
	{"set_prolog_flag", OP_FX, 1150},
	{"module", OP_FX, 1150},
	{"use_module", OP_FX, 1150},
	{"ensure_loaded", OP_FX, 1150},

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

	{"*", OP_YFX, 400},
	{"/", OP_YFX, 400},
	{"//", OP_YFX, 400},
	{"div", OP_YFX, 400},
	{"rdiv", OP_YFX, 400},
	{"\\/", OP_YFX, 400},
	{"/\\", OP_YFX, 400},
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

static char* ensure_strdup(const char* src)
{
	assert(src);
	char* ret = strdup(src);
	ensure(ret);
	return ret;
}

static idx_t is_in_pool(const char *name)
{
	const void *val;

	if (sl_get(g_symtab, name, &val))
		return (idx_t)(unsigned long)val;

	return ERR_IDX;
}

static idx_t add_to_pool(const char *name)
{
	if (!name) return ERR_IDX;
	idx_t offset = g_pool_offset;
	size_t len = strlen(name);

	while ((offset+len+1+1) >= g_pool_size) {
		FAULTINJECT(errno = ENOMEM; return ERR_IDX);
		size_t nbytes = g_pool_size * 2;
		char *tmp = realloc(g_pool, nbytes);
		if (!tmp) return ERR_IDX;
		g_pool = tmp;
		memset(g_pool+g_pool_size, 0, nbytes-g_pool_size);
		g_pool_size = nbytes;
	}

	strcpy(g_pool+offset, name);
	g_pool_offset += len + 1;
	const char *key = strdup(name);
	sl_set(g_symtab, key, (void*)(unsigned long)offset);
	return offset;
}

idx_t index_from_pool(const char *name)
{
	if (!name) return ERR_IDX;
	idx_t offset = is_in_pool(name);

	if (offset != ERR_IDX)
		return offset;

	return add_to_pool(name);
}

unsigned get_op(module *m, const char *name, unsigned *optype, bool *userop, bool hint_prefix)
{
	assert(m);
	assert(name);

	for (const struct op_table *ptr = m->ops; ptr->name; ptr++) {
		if (hint_prefix && (ptr->optype != OP_FX) && (ptr->optype != OP_FY))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (optype) *optype = ptr->optype;
			if (userop) *userop = true;
			return ptr->precedence;
		}
	}

	for (const struct op_table *ptr = g_ops; ptr->name; ptr++) {
		if (hint_prefix && (ptr->optype != OP_FX) && (ptr->optype != OP_FY))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (optype) *optype = ptr->optype;
			if (userop) *userop = false;
			return ptr->precedence;
		}
	}

	if (hint_prefix)
		return get_op(m, name, optype, userop, false);

	return 0;
}

bool set_op(module *m, const char *name, unsigned optype, unsigned precedence)
{
	ensure (m && name);

	unsigned ot = 0, prec = 0;
	bool userop = false;
	int hint = IS_PREFIX(optype);

	if ((prec = get_op(m, name, &ot, &userop, hint)) != 0) {

		if (ot == optype)
			return true;
	}

	struct op_table *ptr = m->ops;

	for (; ptr->name; ptr++) {
		if (!strcmp(ptr->name, name) && (ptr->optype == optype)) {
			ptr->name = name;
			ptr->optype = optype;
			ptr->precedence = precedence;
			return true;
		}
	}

	if (!m->user_ops)
		return false;

	m->user_ops--;
	ptr->name = strdup(name);
	ptr->optype = optype;
	ptr->precedence = precedence;
	return true;
}

module *g_modules = NULL;

cell *list_head(cell *l)
{
	assert(l);

	if (!is_string(l))
		return l + 1;

	size_t n = len_char_utf8(l->val_str);

	if (!n)
		n = 1;

	static cell tmp;
	tmp.val_type = TYPE_CSTRING;
	tmp.nbr_cells = 1;
	tmp.flags = 0;
	tmp.arity = 0;
	memcpy(tmp.val_chr, l->val_str, n);
	tmp.val_chr[n] = '\0';
	return &tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!l) return NULL;
	assert(tmp);

	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->nbr_cells;
	}

	size_t n = len_char_utf8(l->val_str);

	if (!n)
		n = 1;

	if ((l->len_str - n) != 0) {
		tmp->val_type = TYPE_CSTRING;
		tmp->flags = FLAG_BLOB|FLAG_CONST|FLAG_STRING;
		tmp->nbr_cells = 1;
		tmp->arity = 2;
		tmp->val_str = l->val_str + n;
		tmp->len_str = l->len_str - n;
		return tmp;
	}

	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_off = g_nil_s;
	return tmp;
}

module *find_next_module(module *m)
{
	if (!m)
		return g_modules;

	return m->next;
}

module *find_module(const char *name)
{
	assert(name);

	for (module *m = g_modules; m; m = m->next) {
		if (!strcmp(m->name, name))
			return m;
	}

	return NULL;
}

cell *get_head(cell *c)
{
	assert(c);

	if (!is_literal(c))
		return NULL;

	if (c->val_off != g_clause_s)
		return c;

	return c + 1;
}

cell *get_body(cell *c)
{
	assert(c);

	if (!is_literal(c))
		return NULL;

	if (c->val_off != g_clause_s)
		return NULL;

	c = c + 1;
	c += c->nbr_cells;

	if (is_end(c))
		return NULL;

	return c;
}

static predicate *find_predicate(module *m, cell *c)
{
	assert(m);
	assert(c);

	cell tmp = *c;
	tmp.val_type = TYPE_LITERAL;
	tmp.flags = FLAG_KEY;
	tmp.nbr_cells = 1;

	if (is_cstring(c)) {
		tmp.val_type = TYPE_LITERAL;
		tmp.val_off = index_from_pool(GET_STR(c));
	}

	sliter *iter = sl_findkey(m->index, &tmp);
	predicate *h = NULL;

	while (sl_nextkey(iter, (void*)&h)) {
		if (!h->is_abolished) {
			sl_done(iter);
			return h;
		}
	}

	return NULL;
}

static predicate *find_matching_predicate_internal(module *m, cell *c, bool quiet)
{
	assert(c);

	module *save_m = m;
	module *tmp_m = NULL;

	while (m) {
		predicate *h = find_predicate(m, c);

		if (!quiet && h && (m != save_m) && !h->is_public &&
			strcmp(GET_STR(c), "dynamic") && strcmp(GET_STR(c), "module")) {
			fprintf(stdout, "Warning: match not a public method %s/%u\n", GET_STR(c), c->arity);
			break;
		}

		if (h)
			return h;

		if (!tmp_m)
			m = tmp_m = g_modules;
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
	assert(m && name);

	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(name);
	tmp.arity = arity;
	return find_predicate(m, &tmp);
}

static predicate *create_predicate(module *m, cell *c)
{
	assert(m && c);
	assert(is_literal(c));

	FAULTINJECT(errno = ENOMEM; return NULL);
	predicate *h = calloc(1, sizeof(predicate));
	ensure(h);
	h->next = m->head;
	m->head = h;

	h->key = *c;
	h->key.val_type = TYPE_LITERAL;
	h->key.flags = FLAG_KEY;
	h->key.nbr_cells = 1;
	sl_set(m->index, &h->key, h);
	return h;
}

void set_multifile_in_db(module *m, const char *name, idx_t arity)
{
	if (!m) return;
	assert(name);

	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);
	if (h)
		h->is_multifile = true;
	else
		m->error = true;  //cehteh: not 100% sure about this
}

static bool is_multifile_in_db(const char *mod, const char *name, idx_t arity)
{
	assert(mod);

	module *m = find_module(mod);
	if (!m) return false;

	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(name);
	if (tmp.val_off == ERR_IDX) return false;
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) return false;
	return h->is_multifile ? true : false;
}

static int compkey(const void *ptr1, const void *ptr2)
{
	assert(ptr1 && ptr2);

	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;

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

		int ok = strcmp(GET_STR(p1), GET_STR(p2));
		if (ok) return ok;

		if (p1->arity < p2->arity)
			return -1;

		if (p1->arity > p2->arity)
			return 1;

		return 0;
	} else if (is_atom(p1) && is_atom(p2) && (p1->arity == p2->arity)) {
		return strcmp(GET_STR(p1), GET_STR(p2));
	} else if (is_structure(p1) && is_structure(p2)) {
		if (p1->arity < p2->arity)
			return -1;

		if (p1->arity > p2->arity)
			return 1;

		if (p1->val_off != p2->val_off) {
			int i = strcmp(GET_STR(p1), GET_STR(p2));

			if (i != 0)
				return i;
		}

		int arity = p1->arity;
		p1++; p2++;

		while (arity--) {
			int i = compkey(p1, p2);

			if (i != 0)
				return i;

			p1 += p1->nbr_cells;
			p2 += p2->nbr_cells;
		}
	}

	return 0;
}

static void reindex_predicate(predicate *h)
{
	assert(h);
	h->index = sl_create(compkey);
	ensure(h->index);

	for (clause *r = h->head; r; r = r->next) {
		cell *c = get_head(r->t.cells);
		sl_app(h->index, c, r);
	}
}


static clause* assert_begin(module *m, term *t, bool consulting)
{
	if (!m || !t)
		return NULL;

	if (is_cstring(t->cells)) {
		cell *c = t->cells;
		idx_t off = index_from_pool(GET_STR(c));
		if (is_nonconst_blob(c)) free(c->val_str);
		c->val_off = off;
		ensure (c->val_off != ERR_IDX);
		c->val_type = TYPE_LITERAL;
		c->flags = 0;
	}

	cell *c = get_head(t->cells);

	if (!c) {
		fprintf(stdout, "Error: not a fact or clause\n");
		return NULL;
	}

	if (is_cstring(c)) {
		idx_t off = index_from_pool(GET_STR(c));
		if(off == ERR_IDX)
			return NULL;
		if (is_nonconst_blob(c)) free(c->val_str);
		c->val_off = off;
		c->val_type = TYPE_LITERAL;
		c->flags = 0;
	}

	predicate *h = find_predicate(m, c);

	if (h && !consulting && !h->is_dynamic) {
		fprintf(stdout, "Error: not dynamic '%s'/%u\n", GET_STR(c), c->arity);
		return NULL;
	}

	if (!h) {
		h = create_predicate(m, c);
		ensure(h);

		if (!consulting)
			h->is_dynamic = true;

		if (consulting && m->make_public)
			h->is_public = true;
	}

	if (m->prebuilt)
		h->is_prebuilt = true;

	int nbr_cells = t->cidx;
	clause *r = calloc(sizeof(clause)+(sizeof(cell)*nbr_cells), 1);
	if (!r) {
		h->is_abolished = true;
		return NULL;
	}

	r->parent = h;
	memcpy(&r->t, t, sizeof(term));
	r->t.nbr_cells = copy_cells(r->t.cells, t->cells, nbr_cells);
	r->m = m;

	if (!consulting) {
		for (idx_t i = 0; i < r->t.cidx; i++) {
			cell *c = r->t.cells + i;

			if (is_blob(c) && is_const_cstring(c))
				c->flags |= FLAG_DUP;
		}
	}

	return r;
}


static void assert_commit(module *m, term *t, clause *r, predicate *h, bool append)
{
	cell *c = get_head(r->t.cells);

	if (h->index && h->key.arity) {
		if (!append)
			sl_set(h->index, c, r);
		else
			sl_app(h->index, c, r);
	}

	t->cidx = 0;

	if (h->is_persist)
		r->t.persist = true;

	if (!h->index && h->key.arity && is_structure(c+1))
		h->is_noindex = true;

	if (h->index && h->key.arity && is_structure(c+1)) {
		h->is_noindex = true;
		sl_destroy(h->index);
		h->index = NULL;
	}

	if (!h->index && (h->cnt > JUST_IN_TIME_COUNT) && h->key.arity && !m->noindex && !h->is_noindex)
		reindex_predicate(h);
}

clause *asserta_to_db(module *m, term *t, bool consulting)
{
	clause *r = assert_begin(m, t, consulting);
	if (!r) return NULL;
	predicate *h = r->parent;

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
	predicate *h = r->parent;

	if (h->tail)
		h->tail->next = r;

	h->tail = r;
	h->cnt++;

	if (!h->head)
		h->head = r;

	assert_commit(m, t, r, h, true);
	return r;
}

clause *retract_from_db(module *m, clause *r)
{
	r->parent->cnt--;
	r->t.deleted = true;
	m->dirty = true;
	return r;
}

clause *find_in_db(module *m, uuid *ref)
{
	for (predicate *h = m->head; h; h = h->next) {
		for (clause *r = h->head ; r; r = r->next) {
			if (r->t.deleted)
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
	r->t.deleted = true;
	m->dirty = true;
	return r;
}

void set_dynamic_in_db(module *m, const char *name, unsigned arity)
{
	if (!m) return;

	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		h->is_dynamic = true;

		if (!h->index && !m->noindex) {
			h->index = sl_create(compkey);
			ensure(h->index);
		}
	} else
		m->error = true;
}

static void set_persist_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = index_from_pool(name);
	ensure(tmp.val_off == ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		h->is_dynamic = true;
		h->is_persist = true;

		if (!h->index
		    && !m->noindex
		    && !(h->index = sl_create(compkey))) {
			m->error = true;
		}

		m->use_persist = true;
	} else
		m->error = true;
}

void clear_term_nodelete(term *t)
{
	if (!t)
		return;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;
		c->val_type = TYPE_EMPTY;
		c->val_str = NULL;
	}

	t->cidx = 0;
}

void clear_term(term *t)
{
	if (!t)
		return;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (is_blob(c) && !is_dup_cstring(c))
			free(c->val_str);

		c->val_type = TYPE_EMPTY;
		c->val_str = NULL;
	}

	t->cidx = 0;
}

static cell *make_cell(parser *p)
{
	if (p->t->cidx == p->t->nbr_cells) {
		idx_t nbr_cells = p->t->nbr_cells * 2;

		term *t = realloc(p->t, sizeof(term)+(sizeof(cell)*nbr_cells));
		if (!t) {
			p->error = true;
			return NULL;
		}
		p->t = t;
		p->t->nbr_cells = nbr_cells;
	}

	cell *ret = p->t->cells + p->t->cidx++;
	*ret = (cell){0};
	return ret;
}


void destroy_parser(parser *p)
{
	if (p) {
		clear_term(p->t);
		free(p->token);
		free(p->t);
		free(p);
	}
}

parser *create_parser(module *m)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	parser *p = calloc(1, sizeof(parser));
	if (p) {
		p->token = calloc(p->token_size=INITIAL_TOKEN_SIZE+1, 1);
		idx_t nbr_cells = INITIAL_NBR_CELLS;
		p->t = calloc(sizeof(term)+(sizeof(cell)*nbr_cells), 1);
		p->t->nbr_cells = nbr_cells;
		p->start_term = true;
		p->line_nbr = 1;
		p->m = m;
		p->error = false;
		if (!p->token || !p->t) {
			destroy_parser(p);
			p = NULL;
		}
	}
	return p;
}

void destroy_parser_nodelete(parser *p)
{
	if (p) {
		clear_term_nodelete(p->t);
		free(p->token);
		free(p->t);
		free(p);
	}
}

void destroy_query(query *q)
{
	if (!q) return;

	free(q->trails);
	free(q->choices);

	while (q->st.qnbr > 0) {
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		q->st.qnbr--;
	}

	for (arena *a = q->arenas; a;) {
		for (idx_t i = 0; i < a->hp; i++) {
			cell *c = a->heap + i;

			if (is_nonconst_blob(c))
				free(c->val_str);
			else if (is_integer(c) && ((c)->flags&FLAG_STREAM)) {
				stream *str = &g_streams[c->val_num];

				if (str->fp) {
					if ((str->fp != stdin)
						&& (str->fp != stdout)
						&& (str->fp != stderr))
						fclose(str->fp);

					free(str->filename);
					free(str->mode);
					free(str->data);
					free(str->name);
					memset(str, 0, sizeof(stream));
				}
			}
		}

		arena *save = a;
		a = a->next;
		free(save->heap);
		free(save);
	}

	for (int i = 0; i < MAX_QUEUES; i++)
		free(q->queue[i]);

	slot *e = q->slots;

	for (idx_t i = 0; i < q->st.sp; i++, e++) {
		cell *c = &e->c;

		if (is_nonconst_blob(c))
			free(c->val_str);
	}

	free(q->slots);
	free(q->frames);
	free(q->tmp_heap);
	free(q);
}

query *create_query(module *m, bool is_task)
{
	static uint64_t g_query_id = 0;

	query *q = calloc(1, sizeof(query));
	if (q) {
		q->qid = g_query_id++;
		q->m = m;
		q->trace = m->trace;
		q->current_input = 0;		// STDIN
		q->current_output = 1;		// STDOUT

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
	}

	ensure(q);
	return q;
}

query *create_task(query *q, cell *curr_cell)
{
	query *subq = create_query(q->m, true);
	if (subq) {
		subq->parent = q;
		subq->st.fp = 1;
		subq->is_task = true;
		subq->current_input = q->current_input;
		subq->current_output = q->current_output;

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
			cell tmp = {0};
			tmp.val_type = TYPE_VARIABLE;
			tmp.var_nbr = i;
			tmp.val_off = g_anon_s;
			set_var(subq, &tmp, 0, c, q->latest_ctx);
		}

		subq->st.sp = gsrc->nbr_vars;
	}
	return subq;
}

static void dump_vars(query *q, parser *p)
{
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

void consultall(parser *p, cell *l)
{
	while (is_list(l)) {
		cell *h = LIST_HEAD(l);
		module_load_file(p->m, GET_STR(h));
		l = LIST_TAIL(l);
	}
}

char *relative_to(const char *basefile, const char *relfile)
{
	char *tmpbuf = malloc(strlen(basefile) + strlen(relfile) + 256);

	if (!strncmp(relfile, "../", 3)) {
		strcpy(tmpbuf, basefile);
		char *ptr = tmpbuf + strlen(tmpbuf) - 1;

		while ((ptr != tmpbuf) && (*ptr != '/'))
			ptr--;

		if (ptr != tmpbuf)
			*ptr++ = '/';

		*ptr = '\0';
		strcat(ptr, relfile);
	} else
		strcpy(tmpbuf, relfile);

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

	if (strcmp(GET_STR(t->cells), ":-") || (t->cells->arity != 1))
		return;

	cell *c = t->cells + 1;

	if (!is_literal(c))
		return;

	const char *dirname = GET_STR(c);

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		cell *p1 = c + 1;
		if (!is_literal(p1)) return;
		const char *name = GET_STR(p1);
		unsigned save_line_nbr = p->line_nbr;
		char *tmpbuf = relative_to(p->m->filename, name);

		if (!module_load_file(p->m, tmpbuf)) {
			fprintf(stdout, "Error: not found: %s\n", tmpbuf);
			free(tmpbuf);
			p->error = true;
			return;
		}

		p->line_nbr = save_line_nbr;
		free(tmpbuf);
		return;
	}

	if (!strcmp(dirname, "ensure_loaded") && (c->arity == 1)) {
		cell *p1 = c + 1;
		if (!is_atom(p1)) return;
		const char *name = GET_STR(p1);
		char *tmpbuf = relative_to(p->m->filename, name);
		deconsult(tmpbuf);

		if (!module_load_file(p->m, tmpbuf)) {
			fprintf(stdout, "Error: not found: %s\n", tmpbuf);
			free(tmpbuf);
			p->error = true;
			return;
		}

		free(tmpbuf);
		return;
	}

	if (!strcmp(dirname, "module") && (c->arity == 2)) {
		cell *p1 = c + 1, *p2 = c + 2;
		if (!is_literal(p1)) return;
		const char *name = GET_STR(p1);

		if (find_module(name)) {
			fprintf(stdout, "Error: module already loaded: %s\n", name);
			p->error = true;
			return;
		}

		p->m = create_module(name);
		if (!p->m) {
			fprintf(stdout, "Error: module creation failed: %s\n", name);
			p->error = true;
			return;
		}

		while (is_iso_list(p2)) {
			cell *head = LIST_HEAD(p2);

			if (is_structure(head)) {
				if (strcmp(GET_STR(head), "/") && strcmp(GET_STR(head), "//"))
					return;

				cell *f = head+1, *a = f+1;
				if (!is_literal(f)) return;
				if (!is_integer(a)) return;
				cell tmp = *f;
				tmp.arity = a->val_num;

				if (!strcmp(GET_STR(head), "//"))
					tmp.arity += 2;

				predicate *h = find_predicate(p->m, &tmp);
				if (!h) h = create_predicate(p->m, &tmp);
				if (!h) {
					//fprintf(stdout, "Error: predicate creation failed\n");
					destroy_module(p->m);
					p->m = NULL;
					p->error = true;
					return;
				}
				h->is_public = true;
			}

			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "use_module") && (c->arity == 2)) {
		printf("Error: use_module/2 not implemented\n");
	}

	if (!strcmp(dirname, "use_module") && (c->arity == 1)) {
		cell *p1 = c + 1;
		if (!is_literal(p1)) return;
		const char *name = GET_STR(p1);
		char dstbuf[1024*2];

		if (!strcmp(name, "library")) {
			p1 = p1 + 1;
			if (!is_literal(p1)) return;
			name = GET_STR(p1);
			module *m;

			if ((m = find_module(name)) != NULL) {
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
				m = module_load_text(p->m, src);
				free(src);

				if (m != p->m)
					do_db_load(m);

				return;
			}

			query q = {0};
			q.m = p->m;
			snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
			char *dst = dstbuf + strlen(dstbuf);
			idx_t ctx = 0;
			print_term_to_buf(&q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, 0, 0);
			name = dstbuf;
		}

		char *tmpbuf = relative_to(p->m->filename, name);
		char *save = strdup(p->m->filename);

		if (!module_load_file(p->m, tmpbuf)) {
			fprintf(stdout, "Error: not found: %s\n", tmpbuf);

			if (p->m->filename != save) {
				free(p->m->filename);
				p->m->filename = save;
			}

			p->error = true;
			free(tmpbuf);
			return;
		}

		free(p->m->filename);
		p->m->filename = save;
		free(tmpbuf);
	}

	if (!strcmp(dirname, "dynamic") && (c->arity >= 1)) {
		cell *p1 = c + 1;

		while (is_literal(p1)) {
			if (is_literal(p1) && !strcmp(GET_STR(p1), "/") && (p1->arity == 2)) {
				cell *c_name = p1 + 1;
				if (!is_atom(c_name)) return;
				cell *c_arity = p1 + 2;
				if (!is_integer(c_arity)) return;
				set_dynamic_in_db(p->m, GET_STR(c_name), c_arity->val_num);
				p1 += p1->nbr_cells;
			} else if (!strcmp(GET_STR(p1), ","))
				p1 += 1;
		}

		return;
	}

	if (!strcmp(dirname, "multifile") && (c->arity >= 1)) {
		cell *p1 = c + 1;

		while (is_literal(p1)) {
			if (is_literal(p1) && (!strcmp(GET_STR(p1), "/") || !strcmp(GET_STR(p1), "//")) && (p1->arity == 2)) {
				cell *c_name = p1 + 1;
				if (!is_atom(c_name)) return;
				cell *c_arity = p1 + 2;
				if (!is_integer(c_arity)) return;
				const char *src = GET_STR(c_name);
				unsigned arity = c_arity->val_num;

				if (!strcmp(GET_STR(p1), "//"))
					arity += 2;

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(mod, name, arity)) {
						fprintf(stdout, "Error: not multile %s:%s/%u\n", mod, name, (unsigned)arity);
						p->error = true;
						return;
					}
				}

				p1 += p1->nbr_cells;
			} else if (!strcmp(GET_STR(p1), ","))
				p1 += 1;
			else {
				fprintf(stdout, "Warning: unknown multifile, line nbr %u\n", p->line_nbr);
				//p->error = true;
				return;
			}
		}

		return;
	}

	if (!strcmp(dirname, "persist") && (c->arity >= 1)) {
		cell *p1 = c + 1;

		while (is_literal(p1)) {
			if (is_literal(p1) && !strcmp(GET_STR(p1), "/") && (p1->arity == 2)) {
				cell *c_name = p1 + 1;
				if (!is_atom(c_name)) return;
				cell *c_arity = p1 + 2;
				if (!is_integer(c_arity)) return;
				set_persist_in_db(p->m, GET_STR(c_name), c_arity->val_num);
				if (p->m->error) {
					p->error = true;
					return;
				}
				p1 += p1->nbr_cells;
			} else if (!strcmp(GET_STR(p1), ","))
				p1 += 1;
		}

		return;
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p1 = c + 1, *p2 = c + 2;
		if (!is_literal(p1)) return;
		if (!is_literal(p2)) return;

		if (!strcmp(GET_STR(p1), "double_quotes")) {
			if (!strcmp(GET_STR(p2), "atom")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_atom = true;
			} else if (!strcmp(GET_STR(p2), "codes")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_atom = false;
				p->m->flag.double_quote_codes = true;
			} else if (!strcmp(GET_STR(p2), "chars")) {
				p->m->flag.double_quote_atom = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_chars = true;
			} else {
				fprintf(stdout, "Error: unknown value\n");
				p->error = true;
				return;
			}
		} else if (!strcmp(GET_STR(p1), "character_escapes")) {
			if (!strcmp(GET_STR(p2), "true"))
				p->m->flag.character_escapes = true;
			else if (!strcmp(GET_STR(p2), "false"))
				p->m->flag.character_escapes = false;
		} else if (!strcmp(GET_STR(p1), "prefer_rationals")) {
			if (!strcmp(GET_STR(p2), "true"))
				p->m->flag.prefer_rationals = true;
			else if (!strcmp(GET_STR(p2), "false"))
				p->m->flag.prefer_rationals = false;
		} else if (!strcmp(GET_STR(p1), "rational_syntax")) {
			if (!strcmp(GET_STR(p2), "natural"))
				p->m->flag.rational_syntax_natural = true;
			else if (!strcmp(GET_STR(p2), "compatibility"))
				p->m->flag.rational_syntax_natural = false;
		} else {
			fprintf(stdout, "Warning: unknown flag: %s\n", GET_STR(p1));
		}

		return;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		cell *p1 = c + 1, *p2 = c + 2, *p3 = c + 3;

		if (!is_integer(p1) || !is_literal(p2) || !is_atom(p3)) {
			fprintf(stdout, "Error: unknown op\n");
			p->error = true;
			return;
		}

		unsigned optype;
		const char *spec = GET_STR(p2);

		if (!strcmp(spec, "fx"))
			optype = OP_FX;
		else if (!strcmp(spec, "fy"))
			optype = OP_FY;
		else if (!strcmp(spec, "xf"))
			optype = OP_XF;
		else if (!strcmp(spec, "xfx"))
			optype = OP_XFX;
		else if (!strcmp(spec, "xfy"))
			optype = OP_XFY;
		else if (!strcmp(spec, "yf"))
			optype = OP_YF;
		else if (!strcmp(spec, "yfx"))
			optype = OP_YFX;
		else {
			fprintf(stdout, "Error: unknown op spec val_type\n");
			return;
		}

		if (!set_op(p->m, GET_STR(p3), optype, p1->val_num)) {
			fprintf(stdout, "Error: could not set op\n");
			return;
		}

		return;
	}
}

void parser_xref(parser *p, term *t, predicate *parent)
{
	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_literal(c))
			continue;

		const char *functor = GET_STR(c);
		module *m = p->m;

		unsigned optype;
		bool userop, hint_prefix = c->arity == 1;

		if ((c->arity == 2)
		    && !GET_OP(c)
		    && strcmp(functor, "{}")
		    && get_op(m, functor, &optype, &userop, hint_prefix)) {
			SET_OP(c, optype);
		}

		if ((c->fn = get_builtin(m, functor, c->arity)) != NULL) {
			c->flags |= FLAG_BUILTIN;
			continue;
		}

		if (check_builtin(m, functor, c->arity)) {
			c->flags |= FLAG_BUILTIN;
			continue;
		}

		if ((functor[0] != ':') && strchr(functor+1, ':')) {
			char tmpbuf1[256], tmpbuf2[256];
			tmpbuf1[0] = tmpbuf2[0] = '\0';
			sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
			tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
			m = find_module(tmpbuf1);

			if (m)
			{
				c->val_off = index_from_pool(tmpbuf2);
				ensure(c->val_off != ERR_IDX);
			}
			else
				m = p->m;
		}

		module *tmp_m = NULL;

		while (m) {
			predicate *h = find_predicate(m, c);

			if ((c+c->nbr_cells) >= (t->cells+t->cidx-1)) {
				if (parent && (h == parent))
					c->flags |= FLAG_TAIL_REC;
			}

			if (h && (m != p->m) && !h->is_public &&
				strcmp(GET_STR(c), "dynamic") && strcmp(GET_STR(c), "module")) {
				//fprintf(stdout, "Warning: xref not a public method %s/%u\n", GET_STR(c), c->arity);
				//p->error = true;
				break;
			}

			if (h) {
				c->match = h;
				break;
			}

			if (!tmp_m)
				m = tmp_m = g_modules;
			else
				m = m->next;
		}
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

		if (!strcmp(GET_STR(c), ","))
			;
		else if (!strcmp(GET_STR(c), "!")) {
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

	strcpy(p->vartab.var_pool+offset, src);
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
			c->var_nbr = get_varno(p, GET_STR(c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_ARITY) {
			fprintf(stdout, "Error: max vars per term reached\n");
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = GET_STR(c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			c->flags |= FLAG_FIRST_USE;
			t->nbr_vars++;
			p->nbr_vars++;
		}
	}

	for (idx_t i = 0; i < t->nbr_vars; i++) {
		if (p->consulting && (p->vartab.var_used[i] == 1) &&
			(p->vartab.var_name[i][strlen(p->vartab.var_name[i])-1] != '_') &&
			(*p->vartab.var_name[i] != '_')) {
			if (!p->m->quiet)
				fprintf(stdout, "Warning: singleton: %s, line %d\n", p->vartab.var_name[i], (int)p->line_nbr);
		}
	}

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_variable(c))
			continue;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG_ANON;
	}


	cell *c = make_cell(p);
	ensure(c);
	memset(c, 0, sizeof(cell)); //cehteh: make_cell should return a initialized cell?
	c->val_type = TYPE_END;
	c->nbr_cells = 1;

	check_first_cut(p);
}

static bool attach_ops(parser *p, idx_t start_idx)
{
	assert(p);

	idx_t lowest = IDX_MAX, work_idx;
	bool do_work = false;

	for (idx_t i = start_idx; i < p->t->cidx;) {
		cell *c = p->t->cells + i;

		if (c->nbr_cells > 1) {
			i += c->nbr_cells;
			continue;
		}

		if (!is_literal(c) || !c->precedence) {
			i++;
			continue;
		}

		if (IS_XFY(c) || IS_FY(c)) {
			if (c->precedence <= lowest) {
				lowest = c->precedence;
				work_idx = i;
				do_work = true;
			}
		} else {
			if (c->precedence < lowest) {
				lowest = c->precedence;
				work_idx = i;
				do_work = true;
			}
		}

		i++;
	}

	if (!do_work)
		return false;

	idx_t last_idx = 0;

	for (idx_t i = start_idx; i < p->t->cidx;) {
		cell *c = p->t->cells + i;

		if (c->nbr_cells > 1) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		if (!is_literal(c) || !c->precedence) {
			last_idx = i;
			i++;
			continue;
		}

		if ((c->precedence != lowest) || (i != work_idx)) {
			last_idx = i;
			i++;
			continue;
		}

		c->val_type = TYPE_LITERAL;
		c->arity = 1;

		// Prefix...

		if (IS_FX(c) || IS_FY(c)) {
			last_idx = i;
			c->nbr_cells += (c+1)->nbr_cells;
			i += c->nbr_cells;
			idx_t off = (idx_t)((c+1)-p->t->cells);

			if (off >= p->t->cidx) {
				//fprintf(stdout, "Error: missing operand to '%s'\n", GET_STR(c));
				p->error = true;
				c->arity = 0;
				return false;
			}

			continue;
		}

		// Infix...

		if (!IS_XF(c) && !IS_YF(c)) {
			idx_t off = (idx_t)((c+1)-p->t->cells);

			if (off >= p->t->cidx) {
				//fprintf(stdout, "Error: missing operand to '%s'\n", GET_STR(c));
				p->error = true;
				return false;
			}

			c->arity = 2;
		}

		// Infix and Postfix...

		cell save = *c;

		if (!IS_XF(c) && !IS_YF(c))
			save.nbr_cells += (c+1)->nbr_cells;

		cell *c_last = p->t->cells + last_idx;
		idx_t cells_to_move = c_last->nbr_cells;
		c_last = c-1;

		while (cells_to_move--)
			*c-- = *c_last--;

		*c = save;
		c->nbr_cells += (c+1)->nbr_cells;
		i += c->nbr_cells;
		break;
	}

	return true;
}

bool parser_attach(parser *p, idx_t start_idx)
{
	while (attach_ops(p, start_idx))
		;

	return !p->error;
}

void parser_reset(parser *p)
{
	p->t->cidx = 0;
	p->start_term = true;
}

static void parser_dcg_rewrite(parser *p)
{
	if (!p || p->error)
		return;

	if (!is_literal(p->t->cells))
		return;

	if (strcmp(GET_STR(p->t->cells), "-->") || (p->t->cells->arity != 2))
		return;

	query *q = create_query(p->m, false);
	ensure(q);
	char *dst = print_term_to_strbuf(q, p->t->cells, 0, -1);
	char *src = malloc(strlen(dst)+256);
	ensure(src);
	sprintf(src, "dcg_translate((%s),_TermOut).", dst);
	free(dst);

	// Being conservative here (for now) and using
	// temp parser/query objects...

	parser *p2 = create_parser(p->m);
	ensure(p2);
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

	destroy_query(q);

	if (!src) {
		destroy_parser_nodelete(p2);
		p->error = true;
		return;
	}

#if 1
	destroy_parser(p2);
	p2 = create_parser(p->m);
	ensure(p2);
	p2->skip = true;
#else
	parser_reset(p2);
#endif

	p2->srcptr = src;
	parser_tokenize(p2, false, false);
	free(src);

	clear_term(p->t);
	free(p->t);
	p->t = p2->t;
	p->nbr_vars = p2->nbr_vars;
	p2->t = NULL;
	destroy_parser(p2);
}

static cell *make_literal(parser *p, idx_t offset)
{
	if (!p || p->error)
		return NULL;

	assert(p->m); //if (!p->m) return NULL; ? is p->m expected to hold a reference in all (non-error) cases?

	if (offset == ERR_IDX)
		return NULL;

	cell *c = make_cell(p);
	memset(c, 0, sizeof(cell));
	c->val_type = TYPE_LITERAL;
	c->nbr_cells = 1;
	c->val_off = offset;
	return c;
}

static int parse_number(parser *p, const char **srcptr, int_t *val_num, int_t *val_den)
{
	*val_den = 1;
	const char *s = *srcptr;
	int neg = 0;

	if (*s == '-') {
		neg = 1;
		s++;
	} else if (*s == '+')
		s++;

	if (!isdigit(*s))
		return 0;

	if ((*s == '0') && (s[1] == '\'')) {
		s += 2;
		int v = get_char_utf8(&s);
		*val_num = v;
		if (neg) *val_num = -*val_num;
		*srcptr = s;
		return 1;
	}

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
	__int128_t v = 0;
#else
	uint_t v = 0;
#endif

	if ((*s == '0') && (s[1] == 'b')) {
		s += 2;

		while ((*s == '0') || (*s == '1')) {
			v <<= 1;

			if (*s == '1')
				v |= 1;

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				fprintf(stdout, "Error: integer overflow, parsing number, line %d\n", p->line_nbr);
				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		*((uint_t*)val_num) = v;
		if (neg) *val_num = -*val_num;
		*srcptr = s;
		return 1;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		while ((*s >= '0') && (*s <= '7')) {
			v *= 8;
			v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				fprintf(stdout, "Error: integer overflow, parsing number, line %d\n", p->line_nbr);
				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		*((uint_t*)val_num) = v;
		if (neg) *val_num = -*val_num;
		*srcptr = s;
		return 1;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		while (((*s >= '0') && (*s <= '9')) || ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))) {
			v *= 16;

			if ((toupper(*s) >= 'A') && (toupper(*s) <= 'F'))
				v += 10 + (toupper(*s) - 'A');
			else
				v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			if ((v > INT64_MAX) || (v < INT64_MIN)) {
				fprintf(stdout, "Error: integer overflow, parsing number, line %d\n", p->line_nbr);
				p->error = true;
				return -1;
			}
#endif

			s++;
		}

		*((uint_t*)val_num) = v;
		if (neg) *val_num = -*val_num;
		*srcptr = s;
		return 1;
	}

	char *tmpptr = (char*)s;

	while ((*s >= '0') && (*s <= '9')) {
		v *= 10;
		v += *s - '0';

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		if ((v > INT64_MAX) || (v < INT64_MIN)) {
			fprintf(stdout, "Error: integer overflow, parsing number, line %d\n", p->line_nbr);
			p->error = true;
			return -1;
		}
#endif

		s++;
	}

	*((uint_t*)val_num) = v;
	if (neg) *val_num = -*val_num;
	int try_rational = 0;

#if 0
	module *m = p->m;

	if ((*s == 'r') || (*s == 'R'))
		try_rational = 1;
	else if ((*s == '/') && m->flag.rational_syntax_natural)
		try_rational = 1;
#endif

	if (!try_rational) {
		strtod(tmpptr, &tmpptr);
		if (tmpptr[-1] == '.') tmpptr--;
		*srcptr = tmpptr;
		s = *srcptr;

		if ((*s == '(') || (isalpha(*s)) || 0) {
			fprintf(stdout, "Error: syntax error, parsing number, line %d\n", p->line_nbr);
			p->error = true;
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

	*((uint_t*)val_den) = v;

	cell tmp;
	tmp.val_num = *val_num;
	tmp.val_den = *val_den;
	do_reduce(&tmp);
	*val_num = tmp.val_num;
	*val_den = tmp.val_den;
	*srcptr = s;
	return 1;
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

const char *g_escapes = "\e\a\f\b\t\v\r\n";
const char *g_anti_escapes = "eafbtvrn";

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
			fprintf(stdout, "Error: syntax error, closing \\ missing\n");
			*_src = src;
			*error = true;
			return 0;
		}
	}

	*_src = src;
	return ch;
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


static bool get_token(parser *p, int last_op)
{
	if (!p || p->error)
		return false;

	const char *src = p->srcptr;
	char *dst = p->token;
	int neg = 0;
	p->val_type = TYPE_LITERAL;
	p->quoted = 0;
	p->string = p->was_quoted = p->is_variable = p->is_op = false;
	*dst = '\0';

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

		if ((ch == '\\') && p->m->flag.character_escapes) {
			ch = get_escape(&src, &p->error);

			if (p->error) {
				fprintf(stdout, "Error: sysntax error, illegal character escape, line %d\n", p->line_nbr);
				p->error = true;
				return false;
			}
		}

		dst += sprintf(dst, "%u", ch);
		*dst = '\0';
		p->srcptr = (char*)src;
		p->val_type = TYPE_INTEGER;
		p->dq_consing = -1;
		return true;
	}

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
	}

	while ((!*src || (*src == '%')) && p->fp) {
		if (*src == '%')
			p->line_nbr++;

		if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
			return false;
		}

		p->srcptr = p->save_line;
		src = p->srcptr;

		while (isspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}
	}

	while (isspace(*src)) {
		if (*src == '\n')
			p->line_nbr++;

		src++;
	}

	if (!*src) {
		p->srcptr = (char*)src;
		return false;
	}

	if (*src == '%')
		return false;

	do {
		if (!p->comment && (src[0] == '/') && (src[1] == '*')) {
			p->comment = true;
			src += 2;
			continue;
		}

		if (p->comment && (src[0] == '*') && (src[1] == '/')) {
			p->comment = false;
			src += 2;
			p->srcptr = (char*)src;
			return get_token(p, last_op);
		}

		if (p->comment)
			src++;

		if (!*src && p->comment && p->fp) {
			if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
				p->srcptr = (char*)src;
				return true;
			}

			src = p->srcptr = p->save_line;
				p->line_nbr++;
		}
	}
	 while (*src && p->comment);

	// (+/-)tive numbers...

	if (((*src == '-') || (*src == '+')) && last_op) {
		const char *save_src = src++;

		while (isspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}

		if (isdigit(*src)) {
			if (*save_src == '-')
				neg = 1;
		} else
			src = save_src;
	}

	// Numbers...

	const char *tmpptr = src;
	int_t v = 0, d = 1;

	if ((*src != '-') && (*src != '+') && parse_number(p, &src, &v, &d)) {
		if (neg)
			*dst++ = '-';

		// There is room for a number...

		if ((size_t)(src-tmpptr) >= p->token_size) {
			size_t len = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token+len;
		}

		strncpy(dst, tmpptr, src-tmpptr);
		dst[src-tmpptr] = '\0';

		if ((strchr(dst, '.') || strchr(dst, 'e') || strchr(dst, 'E')) && !strchr(dst, '\''))
			p->val_type = TYPE_FLOAT;
		else
			p->val_type = TYPE_INTEGER;

		p->srcptr = (char*)src;
		return true;
	}

	// Quoted strings...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quoted = *src++;
		p->was_quoted = true;

		if ((p->quoted == '"') && p->m->flag.double_quote_codes) {
			*dst++ = '[';
			*dst = '\0';
			p->srcptr = (char*)src;
			p->dq_consing = 1;
			p->quoted = 0;
			return true;
		} else if ((p->quoted == '"') && p->m->flag.double_quote_chars)
			p->string = true;

		for (;;) {
			while (*src) {
				int ch = get_char_utf8(&src);

				if ((ch == p->quoted) && (*src == ch)) {
					ch = *src++;
				} else if (ch == p->quoted) {
					if ((ch == '"') && !*p->token && p->string) {
						dst += put_char_utf8(dst, ch='[');
						dst += put_char_utf8(dst, ch=']');
						*dst = '\0';
						p->was_quoted = p->string = false;
					}

					p->quoted = 0;
					break;
				}

				if ((ch == '\\') && p->m->flag.character_escapes) {
					int ch2 = *src;
					ch = get_escape(&src, &p->error);

					if (!p->error) {
						if (ch2 == '\n') {
							p->line_nbr++;
							break;
						}
					} else {
						fprintf(stdout, "Error: syntax error, illegal character escape, line %d\n", p->line_nbr);
						p->error = true;
						return false;
					}
				}

				size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

				if (len >= p->token_size) {
					size_t len = dst - p->token;
					p->token = realloc(p->token, p->token_size*=2);
					ensure(p->token);
					dst = p->token+len;
				}

				dst += put_char_utf8(dst, ch);
				*dst = '\0';
			}

			if (p->quoted && p->fp) {
				if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = (char*)src;
					return true;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			bool userop = false;

			if (get_op(p->m, p->token, NULL, &userop, false)) {
				//if (userop)
					p->is_op = true;

				if (!strcmp(p->token, ","))
					p->quoted = 1;
			} else
				p->quoted = 1;

			p->len_str = dst - p->token;
			p->srcptr = (char*)src;
			return true;
		}
	}

	int ch = peek_char_utf8(src);

	// Atoms...

	ensure(!p->error, "fallen through from above");

	if (isalpha_utf8(ch) || (ch == '_')) {
		while (isalnum_utf8(ch) || (ch == '_') ||
			((ch == ':') && find_module(p->token))) {

			if ((src[0] == ':') && (src[1] == ':'))	// HACK
				break;

			ch = get_char_utf8(&src);

			size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

			if (len >= p->token_size) {
				size_t len = dst - p->token;
				p->token = realloc(p->token, p->token_size*=2);
				ensure(p->token);
				dst = p->token+len;
			}

			dst += put_char_utf8(dst, ch);
			*dst = '\0';
			ch = peek_char_utf8(src);
		}

		if (isupper(*p->token) || (*p->token == '_'))
			p->is_variable = true;
		else if (get_op(p->m, p->token, NULL, NULL, 0))
			p->is_op = true;

		p->srcptr = (char*)src;
		return true;
	}

	if (is_matching_pair(&dst, (char**)&src, '[',']') ||
		is_matching_pair(&dst, (char**)&src, '{','}')) {
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	if (src[0] == '!') {
		*dst++ = *src++;
		*dst = '\0';
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	static const char *s_delims = "(){}[]|_, `'\"\t\r\n";
	p->is_op = true;

	while (*src) {
		ch = get_char_utf8(&src);
		size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

		if (len >= p->token_size) {
			size_t len = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token+len;
		}

		dst += put_char_utf8(dst, ch);
		*dst = '\0';

		if (strchr(s_delims, ch))
			break;

		ch = peek_char_utf8(src);

		if (strchr(s_delims, ch) || isalnum_utf8(ch) || (ch == '_'))
			break;
	}

	p->srcptr = (char*)src;
	return true;
}

size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, int tolerant)
{
	idx_t save_ctx = q ? q->latest_ctx : l_ctx;
	size_t is_chars_list = 0;

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		cell *c = q ? deref(q, h, l_ctx) : h;

		if (is_integer(c) && !tolerant) {
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
	assert(p);
	idx_t begin_idx = p->t->cidx, save_idx = 0;
	bool last_op = true, is_func = false;
	unsigned arity = 1;
	p->depth++;

	while (get_token(p, last_op)) {
		if (p->error)
			break;

		//fprintf(stdout, "Debug: token '%s' quoted=%d, val_type=%u, op=%d, lastop=%d\n", p->token, p->quoted, p->val_type, p->is_op, last_op);

		if (!p->quoted
		    && !strcmp(p->token, ".")
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '|')) {
			if (parser_attach(p, 0)) {
				parser_assign_vars(p, p->read_term, false);

				if (p->consulting && !p->skip) {
					parser_dcg_rewrite(p);
					directives(p, p->t);

					if (!p->error && !assertz_to_db(p->m, p->t, 1)) {
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

		if (!p->quoted && !strcmp(p->token, "[")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			parser_tokenize(p, true, true);

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

		if (!p->quoted && !strcmp(p->token, "{")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, index_from_pool("{}"));
			ensure(c);
			c->arity = 1;
			p->start_term = true;
			parser_tokenize(p, false, false);

			if (p->error)
				break;

			c = p->t->cells+save_idx;
			c->nbr_cells = p->t->cidx - save_idx;
			p->start_term = false;
			last_op = false;
			continue;
		}

		if (!p->quoted && !strcmp(p->token, "(")) {
			p->start_term = true;
			unsigned tmp_arity = parser_tokenize(p, is_func, false);

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

		if (!p->quoted && !strcmp(p->token, ",") && consing) {
			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			last_op = true;
			continue;
		}

		if (!p->quoted && !strcmp(p->token, ",") && args) {
			arity++;

			if (arity > MAX_ARITY) {
				fprintf(stdout, "Error: max arity reached, line %d: %s\n", p->line_nbr, p->srcptr);
				p->error = true;
				break;
			}

			last_op = true;
			continue;
		}

		if (!p->was_quoted && consing && !strcmp(p->token, "|")) {
			consing = 0;
			continue;
		}

		if (!p->quoted && p->start_term &&
			(!strcmp(p->token, ",") || !strcmp(p->token, "]") || !strcmp(p->token, ")") || !strcmp(p->token, "}"))) {
			fprintf(stdout, "Error: syntax error, start of term expected, line %d: %s\n", p->line_nbr, p->srcptr);
			p->error = true;
			break;
		}

		if (!p->quoted && (!strcmp(p->token, ")") || !strcmp(p->token, "]") || !strcmp(p->token, "}"))) {
			parser_attach(p, begin_idx);
			return arity;
		}

		if (p->is_variable && (*p->srcptr == '(')) {
			fprintf(stdout, "Error: syntax error, line %d: %s\n", p->line_nbr, p->srcptr);
			p->error = true;
			break;
		}

		unsigned optype = 0;
		bool userop = false;
		int precedence = get_op(p->m, p->token, &optype, &userop, last_op);

		if (p->quoted /*&& !userop*/) {
			optype = 0;
			precedence = 0;
		}

		if (precedence && (
			(*p->srcptr == ',') || (*p->srcptr == ')') ||
			(*p->srcptr == '|') || (*p->srcptr == ']') ||
			(*p->srcptr == '}') )) {
			optype = 0;
			precedence = 0;
		}

		// Operators in canonical form..

		if (last_op && precedence && (*p->srcptr == '(')) {
			p->val_type = TYPE_LITERAL;
			optype = 0;
			precedence = 0;
			p->quoted = 0;
		}

		last_op = strcmp(p->token, ")") && precedence;
		int func = (p->val_type == TYPE_LITERAL) && !optype && (*p->srcptr == '(');

		if (func) {
			is_func = true;
			p->is_op = false;
			save_idx = p->t->cidx;
		}

#if 0
		if (p->is_op && !precedence) {
			fprintf(stdout, "Error: syntax error, or operator expected, line %d: %s, %s\n", p->line_nbr, p->token, p->srcptr);
			p->error = true;
			break;
		}
#endif

		p->start_term = false;
		cell *c = make_cell(p);
		memset(c, 0, sizeof(cell));
		c->nbr_cells = 1;
		c->val_type = p->val_type;
		SET_OP(c,optype);
		c->precedence = precedence;

		if (p->val_type == TYPE_INTEGER) {
			const char *src = p->token;
			parse_number(p, &src, &c->val_num, &c->val_den);

			if (strstr(p->token, "0o"))
				c->flags |= FLAG_OCTAL;
			else if (strstr(p->token, "0x"))
				c->flags |= FLAG_HEX;
			else if (strstr(p->token, "0b"))
				c->flags |= FLAG_BINARY;
		}
		else if (p->val_type == TYPE_FLOAT)
			c->val_flt = atof(p->token);
		else if ((!p->was_quoted || func || p->is_op || p->is_variable ||
				check_builtin(p->m, p->token, 0)) && !p->string) {
			if (func && !strcmp(p->token, "."))
				c->precedence = 0;

			if (p->is_variable)
				c->val_type = TYPE_VARIABLE;

			if (p->was_quoted)
				c->flags |= FLAG_QUOTED;

			c->val_off = index_from_pool(p->token);
			ensure(c->val_off != ERR_IDX);
		} else {
			c->val_type = TYPE_CSTRING;

			if (p->string) {
				c->flags |= FLAG_STRING;
				c->arity = 2;
			}

			if ((strlen(p->token) < MAX_SMALL_STRING) && !p->string)
				strcpy(c->val_chr, p->token);
			else {
				if (p->consulting || p->skip)
					c->flags |= FLAG_CONST;

				c->flags |= FLAG_BLOB;

				if (p->string) {
					c->len_str = p->len_str;
					c->val_str = malloc(p->len_str+1);
					ensure(c->val_str);
					memcpy(c->val_str, p->token, p->len_str);
					c->val_str[p->len_str] = '\0';
				} else {
					c->val_str = ensure_strdup(p->token);
					c->len_str = strlen(p->token);
				}
			}
		}
	}

	p->depth--;
	return !p->error;
}

static void module_purge(module *m)
{
	if (!m || !m->dirty)
		return;

	unsigned cnt = 0;

	for (predicate *h = m->head; h; h = h->next) {
		clause *last = NULL;

		for (clause *r = h->head; r;) {
			if (!r->t.deleted) {
				last = r;
				r = r->next;
				continue;
			}

			if (h->head == r)
				h->head = r->next;

			if (h->tail == r)
				h->tail = last;

			if (last)
				last->next = r->next;

			clause *next = r->next;
			clear_term(&r->t);
			free(r);
			r = next;
			cnt++;
		}
	}

	if (!m->quiet)
		printf("Purge %u deleted rules\n", cnt);

	m->dirty = 0;
}

static bool parser_run(parser *p, const char *src, int dump)
{
	p->srcptr = (char*)src;

	if (!parser_tokenize(p, false, false))
		return false;

	if (p->skip) {
		p->m->status = 1;
		return true;
	}

	if (!parser_attach(p, 0))
		return false;

	parser_assign_vars(p, 0, false);

	if (p->command)
		parser_dcg_rewrite(p);

	parser_xref(p, p->t, NULL);
	bool ok = false;
	query *q = create_query(p->m, false);
	if (q) {
		q->run_init = p->run_init;
		query_execute(q, p->t);

		if (q->halt)
			q->error = false;
		else if (dump && !q->abort && q->status)
			dump_vars(q, p);

		p->m->halt = q->halt;
		p->m->halt_code = q->halt_code;
		p->m->status = q->status;

		if (!p->m->quiet && !p->directive && dump && q->m->stats) {
			fprintf(stdout,
				"Goals %llu, Matches %llu, Max frames %u, Max choices %u, Max trails: %u, Backtracks %llu, TCOs:%llu\n",
				(unsigned long long)q->tot_goals, (unsigned long long)q->tot_matches,
				q->max_frames, q->max_choices, q->max_trails,
				(unsigned long long)q->tot_retries, (unsigned long long)q->tot_tcos);
		}

		ok = !q->error;
		p->m = q->m;
		destroy_query(q);
	}

	if (dump)
		module_purge(p->m);

	return ok;
}

module *module_load_text(module *m, const char *src)
{
	if (!m) return NULL;

	parser *p = create_parser(m);
	if (!p) return NULL;

	p->consulting = true;
	p->srcptr = (char*)src;
	parser_tokenize(p, false, false);

	if (!p->error && !p->end_of_term && p->t->cidx) {
		fprintf(stdout, "Error: syntax error, incomplete statement\n");
		p->error = true;
	}

	if (!p->error) {
		parser_xref_db(p);
		int save = p->m->quiet;
		p->m->quiet = true;
		p->m->halt = false;
		p->directive = true;

		if (p->run_init == true) {
			p->command = true;

			if (parser_run(p, "initialization(G), retract(initialization(_)), G", 0))
				p->m->halt = true;
		}

		p->command = p->directive = false;
		p->m->quiet = save;
	}

	m = p->m;
	destroy_parser(p);
	return m;
}

bool module_load_fp(module *m, FILE *fp, const char *filename)
{
	if (!m) return false;

	bool ok = false;
	parser *p = create_parser(m);
	if (p) {
		free(p->m->filename);
		p->m->filename = strdup(filename);
		p->consulting = true;
		p->fp = fp;

		do {
			if (getline(&p->save_line, &p->n_line, p->fp) == -1)
				break;

			p->srcptr = p->save_line;
			ok = parser_tokenize(p, false, false);
		}
		while (ok);

		free(p->save_line);

		if (!p->error && !p->end_of_term && p->t->cidx) {
			fprintf(stdout, "Error: syntax error, incomplete statement\n");
			p->error = true;
		}

		if (!p->error) {
			parser_xref_db(p);
			int save = p->m->quiet;
			p->m->quiet = true;
			p->directive = true;

			if (p->run_init == true) {
				p->command = true;

				if (parser_run(p, "initialization(G), retract(initialization(_)), G", 0))
					p->m->halt = true;
			}

			p->command = p->directive = false;
			p->m->quiet = save;
		}

		ok = !p->error;
	}

	destroy_parser(p);
	return ok;
}

bool module_load_file(module *m, const char *filename)
{
	if (!m) return false;

	if (!strcmp(filename, "user")) {
		for (int i = 0; i < MAX_STREAMS; i++) {
			stream *str = &g_streams[i];

			if (!strcmp(str->name, "user_input")) {
				int ok = module_load_fp(m, str->fp, "./");
				clearerr(str->fp);
				return ok;
			}
		}
	}

	char *tmpbuf = malloc(strlen(filename) + 20);
	strcpy(tmpbuf, filename);

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
			return 0;
		}
	}

	free(tmpbuf);
	FILE *fp = fopen(realbuf, "r");

	if (!fp) {
		free(realbuf);
		return 0;
	}

	bool ok = module_load_fp(m, fp, realbuf);
	fclose(fp);
	free(realbuf);
	return ok;
}

static void module_save_fp(module *m, FILE *fp, int canonical, int dq)
{
	if (!m) return;

	(void) dq;
	idx_t ctx = 0;
	query q = {0};
	q.m = m;

	for (predicate *h = m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.deleted)
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
	if (!m) return false;

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
	if (!m) return;

	m->prebuilt = true;
	parser *p = create_parser(m);
	if (p)
	{
		p->consulting = true;
		p->srcptr = (char*)src;
		parser_tokenize(p, false, false);
		m->prebuilt = false;
		destroy_parser(p);
	} else {
		m->error = true;
	}
}

void destroy_module(module *m)
{
	if (!m) return;

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
		free(h);
		h = save;
	}

	if(g_modules == m) {
		g_modules = m->next;
	} else {
		for (module *tmp = g_modules; tmp; tmp = tmp->next) {
			if(tmp->next == m) {
				tmp->next = m->next;
				break;
			}
		}
	}

	if (m->fp)
		fclose(m->fp);

	for (struct op_table *ptr = m->ops; ptr->name; ptr++)
		free((void*)ptr->name);

	destroy_parser(m->p);
	free(m->filename);
	free(m->name);
	free(m);
}


module *create_module(const char *name)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	module *m = calloc(1, sizeof(module));
	if (m)
	{
		m->filename = strdup("./");
		m->name = strdup(name);
		m->next = g_modules;
		g_modules = m;

		m->index = sl_create(compkey);
		ensure(m->index);
		m->p = create_parser(m);
		ensure(m->p);

		m->flag.double_quote_chars = 1;
		m->flag.character_escapes = true;
		m->flag.rational_syntax_natural = 0;
		m->flag.prefer_rationals = 0;
		m->user_ops = MAX_USER_OPS;
		m->error = false;

		make_rule(m, "call(G) :- G.");
		make_rule(m, "format(F) :- format(F, []).");
		make_rule(m, "unify_with_occurs_check(X, X) :- acyclic_term(X).");

		make_rule(m, "subsumes_term(G,S) :- "			\
			"\\+ \\+ ( "					\
			"term_variables(S, V1), "			\
			"G = S, "					\
			"term_variables(V1, V2), "			\
			"V2 == V1).");

		make_rule(m, "chars_base64(Plain,Base64,_) :- base64(Plain,Base64).");
		make_rule(m, "chars_urlenc(Plain,Url,_) :- urlenc(Plain,Url).");

		make_rule(m, "merge([], R, R) :- !.");
		make_rule(m, "merge(R, [], R) :- !.");
		make_rule(m, "merge([H1|T1], [H2|T2], Result) :- "	\
			"compare(Delta, H1, H2), !, "			\
			"merge(Delta, H1, H2, T1, T2, Result).");

		make_rule(m, "merge(>, H1, H2, T1, T2, [H2|R]) :- "	\
			"merge([H1|T1], T2, R).");
		make_rule(m, "merge(=, H1, _, T1, T2, [H1|R]) :- "	\
			"merge(T1, T2, R).");
		make_rule(m, "merge(<, H1, H2, T1, T2, [H1|R]) :- "	\
			"merge(T1, [H2|T2], R).");

		make_rule(m, "sort(L, R) :- "				\
			"length(L,N), "				\
			"sort(N, L, _, R).");

		make_rule(m, "sort(2, [X1, X2|L], L, R) :- !, "		\
			"compare(Delta, X1, X2), "			\
			"'$sort2'(Delta, X1, X2, R).");
		make_rule(m, "sort(1, [X|L], L, [X]) :- !.");
		make_rule(m, "sort(0, L, L, []) :- !.");
		make_rule(m, "sort(N, L1, L3, R) :- "			\
			"N1 is N // 2, "				\
			"plus(N1, N2, N), "				\
			"sort(N1, L1, L2, R1), "			\
			"sort(N2, L2, L3, R2), "			\
			"merge(R1, R2, R).");

		make_rule(m, "'$sort2'(<, X1, X2, [X1, X2]).");
		make_rule(m, "'$sort2'(=, X1, _,  [X1]).");
		make_rule(m, "'$sort2'(>, X1, X2, [X2, X1]).");

		make_rule(m, "mmerge([], R, R) :- !.");
		make_rule(m, "mmerge(R, [], R) :- !.");
		make_rule(m, "mmerge([H1|T1], [H2|T2], Result) :- "	\
			"compare(Delta, H1, H2), !, "			\
			"mmerge(Delta, H1, H2, T1, T2, Result).");

		make_rule(m, "mmerge(>, H1, H2, T1, T2, [H2|R]) :- "	\
			"mmerge([H1|T1], T2, R).");
		make_rule(m, "mmerge(=, H1, H2, T1, T2, [H1|R]) :- "	\
			"mmerge(T1, [H2|T2], R).");
		make_rule(m, "mmerge(<, H1, H2, T1, T2, [H1|R]) :- "	\
			"mmerge(T1, [H2|T2], R).");

		make_rule(m, "msort(L, R) :- "				\
			"length(L,N), "				\
			"msort(N, L, _, R).");

		make_rule(m, "msort(2, [X1, X2|L], L, R) :- !, "	\
			"compare(Delta, X1, X2), "			\
			"'$sort2'(Delta, X1, X2, R).");
		make_rule(m, "msort(1, [X|L], L, [X]) :- !.");
		make_rule(m, "msort(0, L, L, []) :- !.");
		make_rule(m, "msort(N, L1, L3, R) :- "			\
			"N1 is N // 2, "				\
			"plus(N1, N2, N), "				\
			"msort(N1, L1, L2, R1), "			\
			"msort(N2, L2, L3, R2), "			\
			"mmerge(R1, R2, R).");

		make_rule(m, "keycompare(Delta, (K1-_), (K2-_)) :- "	\
			"(K1 @< K2 -> Delta = '<' ; "			\
			"(K1 @> K2 -> Delta = '>' ; "			\
			"Delta = '=').");

		make_rule(m, "keysort(L, R) :- "			\
			"length(L,N), "				\
			"keysort(N, L, _, R).");

		make_rule(m, "keysort(2, [X1, X2|L], L, R) :- !, "	\
			"keycompare(Delta, X1, X2), "			\
			"'$sort2'(Delta, X1, X2, R).");
		make_rule(m, "keysort(1, [X|L], L, [X]) :- !.");
		make_rule(m, "keysort(0, L, L, []) :- !.");
		make_rule(m, "keysort(N, L1, L3, R) :- "		\
			"N1 is N // 2, "				\
			"plus(N1, N2, N), "				\
			"keysort(N1, L1, L2, R1), "			\
			"keysort(N2, L2, L3, R2), "			\
			"mmerge(R1, R2, R).");

		make_rule(m, "bagof(T,G,B) :- "				\
			"copy_term('$bagof'(T,G,B),TMP_G),"		\
			"TMP_G,"								\
			"'$bagof'(T,G,B)=TMP_G.");

		make_rule(m, "setof(T,G,B) :- "				\
			"copy_term('$bagof'(T,G,B),TMP_G),"		\
			"TMP_G,"								\
			"'$bagof'(T,G,TMP_B)=TMP_G,"			\
			"sort(TMP_B,B).");

		make_rule(m, "catch(G,E,C) :- "				\
			"copy_term('$catch'(G,E,C),TMP_G),"		\
			"'$catch'(G,E,C)=TMP_G,"				\
			"TMP_G.");

		make_rule(m, "call(G,P1) :- "				\
			"copy_term('$calln'(G,P1),TMP_G),"		\
			"'$calln'(G,P1)=TMP_G,"			\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2) :- "			\
			"copy_term('$calln'(G,P1,P2),TMP_G),"		\
			"'$calln'(G,P1,P2)=TMP_G,"			\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2,P3) :- "			\
			"copy_term('$calln'(G,P1,P2,P3),TMP_G),"	\
			"'$calln'(G,P1,P2,P3)=TMP_G,"			\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2,P3,P4) :- "			\
			"copy_term('$calln'(G,P1,P2,P3,P4),TMP_G),"	\
			"'$calln'(G,P1,P2,P3,P4)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2,P3,P4,P5) :- "			\
			"copy_term('$calln'(G,P1,P2,P3,P4,P5),TMP_G),"	\
			"'$calln'(G,P1,P2,P3,P4,P5)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2,P3,P4,P5,P6) :- "			\
			"copy_term('$calln'(G,P1,P2,P3,P4,P5,P6),TMP_G),"	\
			"'$calln'(G,P1,P2,P3,P4,P5,P6)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "call(G,P1,P2,P3,P4,P5,P6,P7) :- "			\
			"copy_term('$calln'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),"	\
			"'$calln'(G,P1,P2,P3,P4,P5,P6,P7)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "task(G,P1) :- "				\
			"copy_term('$taskn'(G,P1),TMP_G),"		\
			"'$taskn'(G,P1)=TMP_G,"			\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2) :- "			\
			"copy_term('$taskn'(G,P1,P2),TMP_G),"	\
			"'$taskn'(G,P1,P2)=TMP_G,"			\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2,P3) :- "			\
			"copy_term('$taskn'(G,P1,P2,P3),TMP_G),"	\
			"'$taskn'(G,P1,P2,P3)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2,P3,P4) :- "			\
			"copy_term('$taskn'(G,P1,P2,P3,P4),TMP_G),"	\
			"'$taskn'(G,P1,P2,P3,P4)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2,P3,P4,P5) :- "			\
			"copy_term('$taskn'(G,P1,P2,P3,P4,P5),TMP_G),"	\
			"'$taskn'(G,P1,P2,P3,P4,P5)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2,P3,P4,P5,P6) :- "			\
			"copy_term('$taskn'(G,P1,P2,P3,P4,P5,P6),TMP_G),"	\
			"'$taskn'(G,P1,P2,P3,P4,P5,P6)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "task(G,P1,P2,P3,P4,P5,P6,P7) :- "			\
			"copy_term('$taskn'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),"	\
			"'$taskn'(G,P1,P2,P3,P4,P5,P6,P7)=TMP_G,"		\
			"TMP_G.");

		make_rule(m, "phrase_from_file(P, Filename) :- "	\
			"open(Filename, read, Str, [mmap(Ms)]),"	\
			"copy_term(P, P2), P2=P,"			\
			"phrase(P2, Ms, []),"				\
			"close(Str).");

		make_rule(m, "phrase_from_file(P, Filename, Opts) :- "	\
			"open(Filename, read, Str, [mmap(Ms)|Opts])," \
			"copy_term(P, P2), P2=P,"			\
			"phrase(P2, Ms, []),"				\
			"close(Str).");

		make_rule(m, "'$append'([], L, L).");
		make_rule(m, "'$append'([H|T], L, [H|R]) :- '$append'(T, L, R).");

		make_rule(m, "phrase(GRBody, S0) :-"			\
			"phrase(GRBody, S0, [])."			\
			"phrase(GRBody, S0, S) :-"			\
			"  (	var(GRBody) -> throw(error(instantiation_error, phrase/3))" \
			"  ;	dcg_constr(GRBody) -> phrase_(GRBody, S0, S)" \
			"  ;	functor(GRBody, _, _) -> call(GRBody, S0, S)" \
			"  ;	throw(error(type_error(callable, GRBody), phrase/3))" \
			"  )."					\
			""						\
			"phrase_([], S, S)."				\
			"phrase_(!, S, S)."				\
			"phrase_((A, B), S0, S) :-"			\
			"  phrase(A, S0, S1), phrase(B, S1, S)."	\
			"phrase_((A -> B ; C), S0, S) :-"		\
			"  !,"					\
			"  (	phrase(A, S0, S1) ->"			\
			"    phrase(B, S1, S)"			\
			"  ;	phrase(C, S0, S)"			\
			"  )."					\
			"phrase_((A ; B), S0, S) :-"			\
			"  (	phrase(A, S0, S) ; phrase(B, S0, S)  )." \
			"phrase_((A | B), S0, S) :-"			\
			"  (	phrase(A, S0, S) ; phrase(B, S0, S)  )." \
			"phrase_({G}, S0, S) :-"			\
			"  (	call(G), S0 = S	 )."			\
			"phrase_(call(G), S0, S) :-"			\
			"  call(G, S0, S)."				\
			"phrase_((A -> B), S0, S) :-"			\
			"  phrase((A -> B ; fail), S0, S)."		\
			"phrase_(phrase(NonTerminal), S0, S) :-"	\
			"  phrase(NonTerminal, S0, S)."		\
			"phrase_([T|Ts], S0, S) :-"			\
			"  '$append'([T|Ts], S, S0).");

		make_rule(m, "findall(Template, Goal, List, Tail) :- "	\
			"findall(Template, Goal, List0), "					\
			"'$append'(List0, Tail, List).");

		// This is an approximation... it needs a catcher

		make_rule(m, "setup_call_cleanup(A,G,B) :- A, !, (G -> true ; (B, !, fail)).");

		// Edinburgh...

		make_rule(m, "tab(0) :- !.");
		make_rule(m, "tab(N) :- put_code(32), M is N-1, tab(M).");
		make_rule(m, "tab(_,0) :- !.");
		make_rule(m, "tab(S,N) :- put_code(S,32), M is N-1, tab(S,M).");
		make_rule(m, "get0(C) :- get_code(C).");
		make_rule(m, "get0(S,C) :- get_code(S,C).");
		make_rule(m, "display(T) :- write_canonical(T).");
		make_rule(m, "display(S,T) :- write_canonical(S,T).");
		make_rule(m, "put(C) :- put_code(C).");
		make_rule(m, "put(S,C) :- put_code(S,C).");
		make_rule(m, "see(F) :- open(F,read,S), set_input(S).");
		make_rule(m, "tell(F) :- open(F,write,S), set_output(S).");
		make_rule(m, "append(F) :- open(F,append,S), set_output(S).");

		// SWI or GNU

		make_rule(m, "current_key(K) :- variable(K), clause('$record_key'(K,_),_).");
		make_rule(m, "recorda(K,V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).");
		make_rule(m, "recordz(K,V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).");
		make_rule(m, "recorded(K,V) :- nonvar(K), clause('$record_key'(K,V),_).");
		make_rule(m, "recorda(K,V,R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V),R).");
		make_rule(m, "recordz(K,V,R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V),R).");
		make_rule(m, "recorded(K,V,R) :- nonvar(K), clause('$record_key'(K,V),_,R).");

		make_rule(m, "succ(X,Y) :- integer(X), Y is X + 1, X >= 0, !.");
		make_rule(m, "succ(X,Y) :- integer(Y), X is Y - 1, X >= 0.");

		make_rule(m, "term_to_atom(T,S) :- write_term_to_chars(S,T,[]).");
		make_rule(m, "write_term_to_atom(S,T,Opts) :- write_term_to_chars(S,Opts,T).");
		make_rule(m, "read_term_from_atom(S,T,Opts) :- read_term_from_chars(S,Opts,T).");
		make_rule(m, "absolute_file_name(R,A) :- absolute_file_name(R,A,[]).");

		// Other...

		make_rule(m, "client(U,H,P,S) :- client(U,H,P,S,[]).");
		make_rule(m, "server(H,S) :- server(H,S,[]).");


		parser *p = create_parser(m);
		if (p)
		{
			p->consulting = true;
			parser_xref_db(p);
			destroy_parser(p);
		}

		if (!m->name || !m->p || m->error || !p) {
			destroy_module(m);
			m = NULL;
		}
	}
	return m;
}


bool deconsult(const char *filename)
{
	module *m = find_module(filename);
	if (!m) return false;
	destroy_module(m);
	return true;
}

bool get_halt(prolog *pl) { return pl->m->halt; }
bool get_status(prolog *pl) { return pl->m->status; }
bool get_dump_vars(prolog *pl) { return pl->m->dump_vars; }
int get_halt_code(prolog *pl) { return pl->m->halt_code; }

void set_trace(prolog *pl) { pl->m->trace = true; }
void set_quiet(prolog *pl) { pl->m->quiet = true; }
void set_stats(prolog *pl) { pl->m->stats = true; }
void set_noindex(prolog *pl) { pl->m->noindex = true; }
void set_opt(prolog *pl, int level) { pl->m->opt = level; }

bool pl_eval(prolog *pl, const char *src)
{
	parser *p = create_parser(pl->curr_m);
	if (!p) return false;
	p->command = true;
	bool ok = parser_run(p, src, 1);
	pl->curr_m = p->m;
	destroy_parser(p);
	return ok;
}

bool pl_consult_fp(prolog *pl, FILE *fp, const char *filename)
{
	return module_load_fp(pl->m, fp, filename);
}

bool pl_consult(prolog *pl, const char *filename)
{
	return module_load_file(pl->m, filename);
}


void g_destroy()
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

	while (g_modules) {
		destroy_module(g_modules);
	}

	sl_destroy(g_symtab);
	g_symtab = NULL;
	free(g_pool);
	g_pool_offset = 0;
	g_pool = NULL;
}

void* g_init(void)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	g_pool = calloc(g_pool_size=INITIAL_POOL_SIZE, 1);
	if (g_pool) {
		bool error = false;
		CHECK_SENTINEL(g_symtab = sl_create2((void*)strcmp, free), NULL);

		if (!error) {
			CHECK_SENTINEL(g_false_s = index_from_pool("false"), ERR_IDX);
			CHECK_SENTINEL(g_true_s = index_from_pool("true"), ERR_IDX);
			CHECK_SENTINEL(g_empty_s = index_from_pool(""), ERR_IDX);
			CHECK_SENTINEL(g_anon_s = index_from_pool("_"), ERR_IDX);
			CHECK_SENTINEL(g_dot_s = index_from_pool("."), ERR_IDX);
			CHECK_SENTINEL(g_cut_s = index_from_pool("!"), ERR_IDX);
			CHECK_SENTINEL(g_nil_s = index_from_pool("[]"), ERR_IDX);
			CHECK_SENTINEL(g_braces_s = index_from_pool("{}"), ERR_IDX);
			CHECK_SENTINEL(g_fail_s = index_from_pool("fail"), ERR_IDX);
			CHECK_SENTINEL(g_clause_s = index_from_pool(":-"), ERR_IDX);
			CHECK_SENTINEL(g_sys_elapsed_s = index_from_pool("$elapsed"), ERR_IDX);
			CHECK_SENTINEL(g_sys_queue_s = index_from_pool("$queue"), ERR_IDX);
			CHECK_SENTINEL(g_eof_s = index_from_pool("end_of_file"), ERR_IDX);
			CHECK_SENTINEL(g_lt_s = index_from_pool("<"), ERR_IDX);
			CHECK_SENTINEL(g_gt_s = index_from_pool(">"), ERR_IDX);
			CHECK_SENTINEL(g_eq_s = index_from_pool("="), ERR_IDX);

			g_streams[0].fp = stdin;
			CHECK_SENTINEL(g_streams[0].filename = strdup("stdin"), NULL);
			CHECK_SENTINEL(g_streams[0].name = strdup("user_input"), NULL);
			CHECK_SENTINEL(g_streams[0].mode = strdup("read"), NULL);

			g_streams[1].fp = stdout;
			CHECK_SENTINEL(g_streams[1].filename = strdup("stdout"), NULL);
			CHECK_SENTINEL(g_streams[1].name = strdup("user_output"), NULL);
			CHECK_SENTINEL(g_streams[1].mode = strdup("append"), NULL);

			g_streams[2].fp = stderr;
			CHECK_SENTINEL(g_streams[2].filename = strdup("stderr"), NULL);
			CHECK_SENTINEL(g_streams[2].name = strdup("user_error"), NULL);
			CHECK_SENTINEL(g_streams[2].mode = strdup("append"), NULL);
		}

		if (error) {
			g_destroy();
			return NULL;
		}
	}
	return g_pool;
}


void pl_destroy(prolog *pl)
{
	if (!pl) return;

	destroy_module(pl->m);
	free(pl);

	if (!--g_tpl_count)
		g_destroy();
}

prolog *pl_create()
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	++g_tpl_count;
	if (g_tpl_count == 1 && g_init() == NULL)
		return NULL;

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
		}
	}

	//printf("Library: %s\n", g_tpl_lib);

	prolog *pl = calloc(1, sizeof(prolog));
	if (pl) {
		pl->m = create_module("user");
		if (pl->m) {
			pl->curr_m = pl->m;

			//cehteh: add api to set things in a module?
			pl->m->prebuilt = true;

			set_multifile_in_db(pl->m, "term_expansion", 2);
			set_dynamic_in_db(pl->m, "term_expansion", 2);

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
					module_load_text(pl->m, src);
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

	}

	return pl;
}

