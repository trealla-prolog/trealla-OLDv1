#include <stdlib.h>
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
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "builtins.h"
#include "heap.h"
#include "utf8.h"

static const op_table g_ops[] =
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

	{"public", OP_FX, 1150},
	{"discontiguous", OP_FX, 1150},
	{"multifile", OP_FX, 1150},
	{"attribute", OP_FX, 1150},

	{"op", OP_FX, 1150},
	{"dynamic", OP_FX, 1150},
	{"persist", OP_FX, 1150},
	//{"initialization", OP_FX, 1150},
	{"set_prolog_flag", OP_FX, 1150},
	{"module", OP_FX, 1150},
	{"use_module", OP_FX, 1150},
	{"ensure_loaded", OP_FX, 1150},
	{"meta_predicate", OP_FX, 1150},

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

predicate *create_predicate(module *m, cell *c)
{
	predicate *h = calloc(1, sizeof(predicate));
	ensure(h);
	h->prev = m->tail;

	if (m->tail)
		m->tail->next = h;

	m->tail = h;

	if (!m->head)
		m->head = h;

	h->m = m;
	h->key = *c;
	h->key.tag = TAG_LITERAL;
	h->key.flags = 0;
	h->key.nbr_cells = 1;

	m_app(m->index, &h->key, h);
	return h;
}

static int predicate_compkey(const void *ptr1, const void *ptr2, const void *param)
{
	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;
	const module *m = (const module*)param;

	if (p1->arity < p2->arity)
		return -1;

	if (p1->arity > p2->arity)
		return 1;

	if (p1->val_off == p2->val_off)
		return 0;

	return strcmp(m->pl->pool+p1->val_off, m->pl->pool+p2->val_off);
}

static int index_compkey(const void *ptr1, const void *ptr2, const void *param)
{
	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;
	const module *m = (const module*)param;

	if (is_bigint(p1)) {
		if (is_bigint(p2)) {
			return mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);
		} else if (is_smallint(p2)) {
			return mp_int_compare_value(&p1->val_bigint->ival, p2->val_integer);
		} else if (is_variable(p2))
			return 0;
	} else if (is_smallint(p1)) {
		if (is_bigint(p2)) {
			return -mp_int_compare_value(&p2->val_bigint->ival, p1->val_integer);
		} if (is_smallint(p2)) {
			if (get_smallint(p1) < get_smallint(p2))
				return -1;
			else if (get_smallint(p1) > get_smallint(p2))
				return 1;
			else
				return 0;
		} else if (is_variable(p2))
			return 0;
	} else if (is_real(p1)) {
		if (is_real(p2)) {
			if (get_real(p1) < get_real(p2))
				return -1;
			else if (get_real(p1) > get_real(p2))
				return 1;
			else
				return 0;
		} else if (is_variable(p2))
			return 0;
	} else if (is_atom(p1)) {
		if (is_atom(p2))
			return strcmp(MODULE_GET_STR(p1), MODULE_GET_STR(p2));
		else if (is_variable(p2))
			return 0;
	} else if (is_structure(p1)) {
		if (is_structure(p2)) {
			if (p1->arity < p2->arity)
				return -1;

			if (p1->arity > p2->arity)
				return 1;

			int i = strcmp(MODULE_GET_STR(p1), MODULE_GET_STR(p2));

			if (i != 0)
				return i;

			int arity = p1->arity;
			p1++; p2++;

			while (arity--) {
				int i = index_compkey(p1, p2, param);

				if (i != 0)
					return i;

				p1 += p1->nbr_cells;
				p2 += p2->nbr_cells;
			}

			return 0;
		} else if (is_variable(p2))
			return 0;
	} else if (is_variable(p1))
		return 0;
	else
		return 0;

	return 0;
}

clause *find_in_db(module *m, uuid *ref)
{
	for (predicate *h = m->head; h; h = h->next) {
		for (clause *cl = h->head ; cl; cl = cl->next) {
			if (cl->t.ugen_erased)
				continue;

			if (!memcmp(&cl->u, ref, sizeof(uuid)))
				return cl;
		}
	}

	return NULL;
}

static void push_property(module *m, const char *name, unsigned arity, const char *type)
{
	//if (name[0] == '$')
	//	return;

	char tmpbuf[1024];
	format_property(tmpbuf, sizeof(tmpbuf), name, arity, type);
	parser *p = create_parser(m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	p->internal = true;
	tokenize(p, false, false);
	destroy_parser(p);
}

clause *erase_from_db(module *m, uuid *ref)
{
	clause *cl = find_in_db(m, ref);
	if (!cl) return 0;
	cl->t.ugen_erased = ++m->pl->ugen;
	return cl;
}

void set_noindex_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
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
	tmp.tag = TAG_LITERAL;
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

void set_multifile_in_db(module *m, const char *name, idx_t arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
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

void set_dynamic_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
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

void set_meta_predicate_in_db(module *m, cell *c)
{
	const char *name = MODULE_GET_STR(c);
	unsigned arity = c->arity;
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *h = find_predicate(m, &tmp);
	if (!h) h = create_predicate(m, &tmp);

	if (h) {
		query q = (query){0};
		q.st.m = m;
		char *dst = print_term_to_strbuf(&q, c, 0, 0);
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "meta_predicate(%s)", dst);
		push_property(m, name, arity, tmpbuf);
		free(dst);
		h->is_meta_predicate = true;
	} else
		m->error = true;

	push_property(m, MODULE_GET_STR(c), c->arity, "static");
}

void set_persist_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
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

static bool check_directive(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_neck_s) && (c->arity == 1))
		return true;

	return false;
}

predicate *find_predicate(module *m, cell *c)
{
	assert(strlen(MODULE_GET_STR(c)) == MODULE_LEN_STR(c));

	cell tmp = *c;
	tmp.tag = TAG_LITERAL;
	tmp.flags = 0;
	tmp.nbr_cells = 1;

	if (is_cstring(c))
		tmp.val_off = index_from_pool(m->pl, MODULE_GET_STR(c));

	miter *iter = m_findkey(m->index, &tmp);
	predicate *h = NULL;

	while (m_nextkey(iter, (void*)&h)) {
		if (h->is_abolished)
			continue;

		m_done(iter);
		return h;
	}

	return NULL;
}

predicate *find_functor(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	tmp.arity = arity;
	return find_predicate(m, &tmp);
}

// FIXME: this should only search the current modules, not all of them.

predicate *search_predicate(module *m, cell *c)
{
	module *orig_m = m;
	predicate *h = find_predicate(m, c);

	if (h)
		return h;

	for (m = m->pl->modules; m; m = m->next) {
		if (m == orig_m)
			continue;

		h = find_predicate(m, c);

		if (h)
			return h;
	}

	return NULL;
}

unsigned get_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	miter *iter = m_findkey(m->ops, name);
	op_table *ptr;

	while (m_nextkey(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		m_done(iter);
		return ptr->priority;
	}

	iter = m_findkey(m->defops, name);

	while (m_nextkey(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		m_done(iter);
		return ptr->priority;
	}

	if (hint_prefix)
		return get_op(m, name, specifier, false);

	return 0;
}

unsigned find_op(module *m, const char *name, unsigned specifier)
{
	miter *iter = m_findkey(m->ops, name);
	op_table *ptr;

	while (m_nextkey(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			m_done(iter);
			return ptr->priority;
		}
	}

	iter = m_findkey(m->defops, name);

	while (m_nextkey(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			m_done(iter);
			return ptr->priority;
		}
	}

	return 0;
}

bool set_op(module *m, const char *name, unsigned specifier, unsigned priority)
{
	miter *iter = m_findkey(m->ops, name);
	op_table *ptr;

	while (m_nextkey(iter, (void**)&ptr)) {
		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			m_done(iter);
			return true;
		}

		ptr->priority = priority;
		ptr->specifier = specifier;
		m->loaded_ops = false;
		m_done(iter);
		return true;
	}

	iter = m_findkey(m->defops, name);

	while (m_nextkey(iter, (void**)&ptr)) {
		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			m_done(iter);
			return true;
		}

		ptr->priority = priority;
		ptr->specifier = specifier;
		m->loaded_ops = false;
		m_done(iter);
		return true;
	}

	op_table *tmp = malloc(sizeof(op_table));
	tmp->name = strdup(name);
	tmp->priority = priority;
	tmp->specifier = specifier;
	m->loaded_ops = false;
	m_app(m->ops, tmp->name, tmp);
	return true;
}

// FIXME: this should only search the current modules, not all of them.

unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	module *orig_m = m;
	unsigned priority = get_op(m, name, specifier, hint_prefix);

	if (priority)
		return priority;

	for (m = m->pl->modules; m; m = m->next) {
		if ((m == orig_m) || !m->user_ops)
			continue;

		priority = get_op(m, name, specifier, hint_prefix);

		if (priority)
			return priority;
	}

	return 0;
}

static clause* assert_begin(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	cell *c = p1;

	if (!check_directive(c))
		c = get_head(p1);

	if (!c) {
		fprintf(stdout, "Error: not a fact or clause\n");
		return NULL;
	}

	if (is_cstring(c)) {
		idx_t off = index_from_pool(m->pl, MODULE_GET_STR(c));
		if (off == ERR_IDX) return NULL;
		unshare_cell(c);
		c->tag = TAG_LITERAL;
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

		if (check_directive(p1))
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

	clause *cl = calloc(sizeof(clause)+(sizeof(cell)*(p1->nbr_cells+1)), 1);
	if (!cl) {
		h->is_abolished = true;
		return NULL;
	}

	copy_cells(cl->t.cells, p1, p1->nbr_cells);
	cl->t.cells[p1->nbr_cells] = (cell){0};
	cl->t.cells[p1->nbr_cells].tag = TAG_END;
	cl->t.nbr_vars = nbr_vars;
	cl->t.nbr_cells = p1->nbr_cells;
	cl->t.cidx = p1->nbr_cells+1;
	cl->t.ugen_created = ++m->pl->ugen;
	cl->owner = h;
	return cl;
}

static void reindex_predicate(module *m, predicate *h)
{
	h->index = m_create(index_compkey, NULL, m);
	ensure(h->index);

	for (clause *cl = h->head; cl; cl = cl->next) {
		cell *c = get_head(cl->t.cells);

		if (!cl->t.ugen_erased)
			m_app(h->index, c, cl);
	}
}

static void assert_commit(module *m, clause *cl, predicate *h, bool append)
{
	cell *c = get_head(cl->t.cells);

	if (h->is_persist)
		cl->t.persist = true;

	if (h->key.arity) {
		cell *p1 = c + 1;

		if (!h->index && is_structure(p1))
			h->is_noindex = true;

		if (h->index && is_structure(p1)) {
			h->is_noindex = true;
			h->index_save = h->index;
			h->index = NULL;
		}

		if (!h->index && (h->cnt > 50)
			&& !m->pl->noindex && !h->is_noindex)
			reindex_predicate(m, h);

		if (h->index) {
			if (!append)
				m_set(h->index, c, cl);
			else
				m_app(h->index, c, cl);
		}
	}
}

clause *asserta_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	clause *cl = assert_begin(m, nbr_vars, p1, consulting);
	if (!cl) return NULL;
	predicate *h = cl->owner;

	if (h->head)
		h->head->prev = cl;

	cl->next = h->head;
	h->head = cl;
	h->cnt++;

	if (!h->tail)
		h->tail = cl;

	assert_commit(m, cl, h, false);
	return cl;
}

clause *assertz_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	clause *cl = assert_begin(m, nbr_vars, p1, consulting);
	if (!cl) return NULL;
	predicate *h = cl->owner;

	if (h->tail)
		h->tail->next = cl;

	cl->prev = h->tail;
	h->tail = cl;
	h->cnt++;

	if (!h->head)
		h->head = cl;

	assert_commit(m, cl, h, true);
	return cl;
}

bool retract_from_db(module *m, clause *cl)
{
	if (cl->t.ugen_erased)
		return false;

	predicate *h = cl->owner;

	if (!--h->cnt) {
		m_destroy(h->index);
		m_destroy(h->index_save);
		h->index = h->index_save = NULL;
		h->head = h->tail = NULL;
	}

	cl->t.ugen_erased = ++m->pl->ugen;
	return true;
}

static void	set_loaded(module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename))
			return;

		ptr = ptr->next;
	}

	ptr = malloc(sizeof(*ptr));
	ptr->next = m->loaded_files;
	strncpy(ptr->filename, filename, PATH_MAX);
	ptr->filename[PATH_MAX-1] = '\0';
	m->loaded_files = ptr;
}

module *load_text(module *m, const char *src, const char *filename)
{
	parser *p = create_parser(m);
	if (!p) return NULL;

	char *save_filename = p->m->filename;
	p->m->filename = strdup(filename);
	p->consulting = true;
	p->srcptr = (char*)src;
	tokenize(p, false, false);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->t->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	if (!p->error) {
		xref_db(p);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->m->pl->halt = false;
		p->directive = true;

		if (p->run_init == true) {
			p->consulting = false;
			p->command = true;

			if (run(p, "(:- initialization(G)), retract((:- initialization(_))), G", false, true))
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

bool load_fp(module *m, FILE *fp, const char *filename)
{
	parser *p = create_parser(m);
	if (!p) return false;
	char *save_filename = m->filename;
	m->filename = strdup(filename);
	p->consulting = true;
	p->fp = fp;
	bool ok = false;

	virtual_term(p, "begin_of_file.");

	do {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
			virtual_term(p, "end_of_file.");
			break;
		}

		p->srcptr = p->save_line;
		tokenize(p, false, false);
		ok = !p->error;
	}
	 while (ok && !p->already_loaded);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->t->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	if (!p->error && !p->already_loaded) {
		xref_db(p);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->directive = true;

		if (p->run_init == true) {
			p->command = true;
			p->consulting = false;

			if (run(p, "(:- initialization(G)), retract((:- initialization(_))), G", false, true))
				p->m->pl->halt = true;
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	ok = !p->error;
	destroy_parser(p);
	free(m->filename);
	m->filename = save_filename;
	return ok;
}

bool load_file(module *m, const char *filename)
{
	if (!strcmp(filename, "user")) {
		for (int i = 0; i < MAX_STREAMS; i++) {
			stream *str = &g_streams[i];

			if (!strcmp(str->name, "user_input")) {
				int ok = load_fp(m, str->fp, filename);
				clearerr(str->fp);
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
			return false;
		}
	}

	free(tmpbuf);
	set_loaded(m, realbuf);
	FILE *fp = fopen(realbuf, "r");

	if (!fp) {
		free(realbuf);
		return false;
	}

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	clearerr(fp);
	char *tmp_filename = strdup(realbuf);
	bool ok = load_fp(m, fp, tmp_filename);
	fclose(fp);
	free(realbuf);
	free(tmp_filename);
	return ok;
}

static void module_save_fp(module *m, FILE *fp, int canonical, int dq)
{
	(void) dq;
	idx_t ctx = 0;
	query q = (query){0};
	q.st.m = m;

	for (predicate *h = m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		for (clause *cl = h->head; cl; cl = cl->next) {
			if (cl->t.ugen_erased)
				continue;

			if (canonical)
				print_canonical(&q, fp, cl->t.cells, ctx, 0);
			else
				print_canonical(&q, fp, cl->t.cells, ctx, 0);

			fprintf(fp, "\n");
		}
	}
}

bool save_file(module *m, const char *filename)
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

#if 0
static void make_rule(module *m, const char *src)
{
	m->prebuilt = true;
	bool save = m->p->consulting;
	m->p->consulting = true;
	m->p->srcptr = (char*)src;
	m->p->line_nbr = 0;
	tokenize(m->p, false, false);
	m->prebuilt = false;
	m->p->consulting = save;
}
#endif

void destroy_module(module *m)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		struct loaded_file *save = ptr;
		ptr = ptr->next;
		free(save);
	}

	while (m->tasks) {
		query *task = m->tasks->next;
		destroy_query(m->tasks);
		m->tasks = task;
	}

	m_destroy(m->index);
	miter *iter = m_first(m->defops);
	op_table *opptr;

	while (m_next(iter, (void**)&opptr)) {
		free(opptr->name);
		free(opptr);
	}

	m_destroy(m->defops);
	iter = m_first(m->ops);

	while (m_next(iter, (void**)&opptr)) {
		free(opptr->name);
		free(opptr);
	}

	m_destroy(m->ops);

	for (predicate *h = m->head; h;) {
		predicate *save = h->next;

		for (clause *cl = h->head; cl;) {
			clause *save = cl->next;
			clear_rule(&cl->t);
			free(cl);
			cl = save;
		}

		m_destroy(h->index);
		m_destroy(h->index_save);
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

	destroy_parser(m->p);
	free(m->filename);
	free(m->name);
	free(m);
}

module *create_module(prolog *pl, const char *name)
{
	module *m = calloc(1, sizeof(module));
	ensure(m);

	m->pl = pl;
	m->filename = strdup(name);
	m->name = strdup(name);
	m->flag.unknown = UNK_ERROR;
	m->flag.double_quote_chars = true;
	m->flag.character_escapes = true;
	m->error = false;
	m->id = index_from_pool(pl, name);
	m->defops = m_create((void*)strcmp, NULL, NULL);

	if (strcmp(name, "system")) {
		for (const op_table *ptr = g_ops; ptr->name; ptr++) {
			op_table *tmp = malloc(sizeof(op_table));
			memcpy(tmp, ptr, sizeof(op_table));
			tmp->name = strdup(ptr->name);
			m_app(m->defops, tmp->name, tmp);
		}
	}

	m->ops = m_create((void*)strcmp, NULL, NULL);
	m->index = m_create(predicate_compkey, NULL, m);
	ensure(m->index);
	m->p = create_parser(m);
	ensure(m->p);

	parser *p = create_parser(m);
	if (p) {
		p->consulting = true;
		xref_db(p);
		destroy_parser(p);
	}

	if (!m->name || !m->p || m->error || !p) {
		destroy_module(m);
		m = NULL;
	}

	m->next = pl->modules;
	pl->modules = m;
	set_dynamic_in_db(m, "goal_expansion", 2);
	set_dynamic_in_db(m, "initialization", 1);
	set_dynamic_in_db(m, ":-", 1);
	return m;
}
