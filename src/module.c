#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <float.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "internal.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

static const op_table g_ops[] =
{
	{":-", OP_XFX, 1200},
	{":-", OP_FX, 1200},
	{"-->", OP_XFX, 1200},
	{"?-", OP_FX, 1200},
	{"|", OP_XFY, 1105},
	{";", OP_XFY, 1100},
	{"->", OP_XFY, 1050},
	{"*->", OP_XFY, 1050},
	{",", OP_XFY, 1000},

	//{"public", OP_FX, 1150},
	//{"discontiguous", OP_FX, 1150},
	//{"multifile", OP_FX, 1150},
	//{"attribute", OP_FX, 1150},
	//{"op", OP_FX, 1150},
	//{"dynamic", OP_FX, 1150},
	//{"persist", OP_FX, 1150},
	//{"initialization", OP_FX, 1150},
	//{"set_prolog_flag", OP_FX, 1150},
	//{"module", OP_FX, 1150},
	//{"use_module", OP_FX, 1150},
	//{"ensure_loaded", OP_FX, 1150},

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
	//{"?", OP_FX, 500},
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

static const char *set_loaded(module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename)) {
			ptr->is_loaded = true;
			return ptr->filename;
		}

		ptr = ptr->next;
	}

	ptr = malloc(sizeof(*ptr));
	ptr->next = m->loaded_files;
	ptr->filename = strdup(filename);
	ptr->is_loaded = true;
	m->loaded_files = ptr;
	return ptr->filename;
}

static const char *set_known(module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename))
			return ptr->filename;

		ptr = ptr->next;
	}

	ptr = malloc(sizeof(*ptr));
	ptr->next = m->loaded_files;
	ptr->filename = strdup(filename);
	ptr->is_loaded = false;
	m->loaded_files = ptr;
	return ptr->filename;
}

static void set_unloaded(module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename)) {
			ptr->is_loaded = false;
			return;
		}

		ptr = ptr->next;
	}
}

static bool is_loaded(const module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (ptr->is_loaded && !strcmp(ptr->filename, filename))
			return true;

		ptr = ptr->next;
	}

	return false;
}

static void clear_loaded(const module *m)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		struct loaded_file *save = ptr;
		ptr = ptr->next;
		free(save->filename);
		free(save);
	}
}

predicate *create_predicate(module *m, cell *c)
{
	predicate *pr = calloc(1, sizeof(predicate));
	ensure(pr);
	pr->prev = m->tail;

	if (m->tail)
		m->tail->next = pr;

	m->tail = pr;

	if (!m->head)
		m->head = pr;

	pr->m = m;
	pr->key = *c;
	pr->key.tag = TAG_LITERAL;
	pr->key.nbr_cells = 1;
	pr->is_noindex = m->pl->noindex || !pr->key.arity;

	//printf("*** create %s ==> %s/%u\n", m->filename, GET_STR(m, &pr->key), pr->key.arity);

	if (GET_STR(m, c)[0] == '$')
		pr->is_noindex = true;

	m_app(m->index, &pr->key, pr);
	return pr;
}

bool add_to_dirty_list(module *m, db_entry *dbe)
{
	if (!retract_from_db(m, dbe))
		return false;

	predicate *pr = dbe->owner;
	dbe->dirty = pr->dirty_list;
	pr->dirty_list = dbe;
	return true;
}

static void destroy_predicate(module *m, predicate *pr)
{
	m_del(m->index, &pr->key);

	for (db_entry *dbe = pr->head; dbe;) {
		db_entry *save = dbe->next;

		if (!dbe->cl.ugen_erased) {
			clear_rule(&dbe->cl);
			free(dbe);
		}

		dbe = save;
	}

	for (db_entry *dbe = pr->dirty_list; dbe;) {
		db_entry *save = dbe->dirty;
		clear_rule(&dbe->cl);
		free(dbe);
		dbe = save;
	}

	m_destroy(pr->idx_save);
	m_destroy(pr->idx);
	free(pr);
}

static int predicate_cmpkey(const void *ptr1, const void *ptr2, const void *param)
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

int index_cmpkey_(const void *ptr1, const void *ptr2, const void *param, int depth)
{
	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;
	const module *m = (const module*)param;

	if (is_smallint(p1)) {
		if (is_bigint(p2)) {
			return -mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);
		} if (is_smallint(p2)) {
			if (get_smallint(p1) < get_smallint(p2))
				return -1;
			else if (get_smallint(p1) > get_smallint(p2))
				return 1;
			else
				return 0;
		} else if (!is_variable(p2))
			return -1;
	} else if (is_bigint(p1)) {
		if (is_bigint(p2)) {
			return mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);
		} else if (is_smallint(p2)) {
			return mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);
		} else if (!is_variable(p2))
			return -1;
	} else if (is_real(p1)) {
		if (is_real(p2)) {
			if (get_real(p1) < get_real(p2))
				return -1;
			else if (get_real(p1) > get_real(p2))
				return 1;
			else
				return 0;
		} else if (is_integer(p2))
			return 1;
		else if (!is_variable(p2))
			return -1;
	} else if (is_literal(p1) && !p1->arity) {
		if (is_literal(p2) && !p2->arity) {
			if (p1->val_off == p2->val_off)
				return 0;

			return strcmp(GET_STR(m, p1), GET_STR(m, p2));
		} else if (is_atom(p2))
			return strcmp(GET_STR(m, p1), GET_STR(m, p2));
		else if (is_number(p2))
			return 1;
		else if (!is_variable(p2))
			return -1;
	} else if (is_atom(p1)) {
		if (is_atom(p2))
			return strcmp(GET_STR(m, p1), GET_STR(m, p2));
		else if (is_number(p2))
			return 1;
		else if (!is_variable(p2))
			return -1;
	} else if (is_structure(p1)) {
		if (is_structure(p2)) {
			if (p1->arity < p2->arity)
				return -1;

			if (p1->arity > p2->arity)
				return 1;

			if (p1->val_off != p2->val_off)
				return strcmp(GET_STR(m, p1), GET_STR(m, p2));

			int arity = p1->arity;
			p1++; p2++;

			while (arity--) {
				int i = index_cmpkey_(p1, p2, param, depth+1);

				if (i != 0)
					return i;

				p1 += p1->nbr_cells;
				p2 += p2->nbr_cells;
			}

			return 0;
		} else if (!is_variable(p2))
			return 1;
	}

	return 0;
}

int index_cmpkey(const void *ptr1, const void *ptr2, const void *param)
{
	return index_cmpkey_(ptr1, ptr2, param, 0);
}

db_entry *find_in_db(module *m, uuid *ref)
{
	for (predicate *pr = m->head; pr; pr = pr->next) {
		for (db_entry *dbe = pr->head ; dbe; dbe = dbe->next) {
			if (dbe->cl.ugen_erased)
				continue;

			if (!memcmp(&dbe->u, ref, sizeof(uuid)))
				return dbe;
		}
	}

	return NULL;
}

static void push_property(module *m, const char *name, unsigned arity, const char *type)
{
	char tmpbuf[1024];
	format_property(m, tmpbuf, sizeof(tmpbuf), name, arity, type);
	parser *p = create_parser(m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	p->internal = true;
	tokenize(p, false, false);
	destroy_parser(p);
}

db_entry *erase_from_db(module *m, uuid *ref)
{
	db_entry *dbe = find_in_db(m, ref);
	if (!dbe) return 0;
	dbe->cl.ugen_erased = ++m->pl->ugen;
	return dbe;
}

void set_discontiguous_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "discontiguous");
		pr->is_discontiguous = true;
	} else
		m->error = true;
}

void set_multifile_in_db(module *m, const char *name, pl_idx_t arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "multifile");
		pr->is_multifile = true;
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
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "dynamic");
		pr->is_dynamic = true;
	} else
		m->error = true;
}

void set_meta_predicate_in_db(module *m, cell *c)
{
	const char *name = GET_STR(m, c);
	unsigned arity = c->arity;
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		query q = (query){0};
		q.pl = m->pl;
		q.st.m = m;
		char *dst = print_canonical_to_strbuf(&q, c, 0, 0);
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "meta_predicate(%s)", dst);
		push_property(m, name, arity, tmpbuf);
		free(dst);
		pr->is_meta_predicate = true;
	} else
		m->error = true;
}

void set_persist_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off == ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "dynamic");
		push_property(m, name, arity, "persist");
		pr->is_dynamic = true;
		pr->is_persist = true;
		m->use_persist = true;
	} else
		m->error = true;
}

static bool is_check_directive(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_neck_s) && (c->arity == 1))
		return true;

	return false;
}

void convert_to_literal(module *m, cell *c)
{
	char *src = DUP_SLICE(m, c);
	pl_idx_t off = index_from_pool(m->pl, src);
	unshare_cell(c);
	c->tag = TAG_LITERAL;
	c->val_off = off;
	c->match = NULL;
	c->flags = 0;
	free(src);
}

predicate *find_predicate(module *m, cell *c)
{
	cell tmp = *c;
	tmp.tag = TAG_LITERAL;
	tmp.flags = 0;
	tmp.nbr_cells = 1;

	if (is_cstring(c)) {
		tmp.val_off = index_from_pool(m->pl, GET_STR(m, c));
	}

	miter *iter = m_find_key(m->index, &tmp);
	predicate *pr = NULL;

	while (m_next_key(iter, (void*)&pr)) {
		if (pr->is_abolished)
			continue;

		m_done(iter);
		return pr;
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

predicate *search_predicate(module *m, cell *c)
{
	predicate *pr = find_predicate(m, c);

	if (pr)
		return pr;

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];
		pr = find_predicate(tmp_m, c);

		if (pr)
			return pr;
	}

	for (module *tmp_m = m->pl->modules; tmp_m; tmp_m = tmp_m->next) {
		if (m == tmp_m)
			continue;

		pr = find_predicate(tmp_m, c);

		if (pr) {
			m->used[m->idx_used++] = tmp_m;
			return pr;
		}
	}

	return NULL;
}

#define DUMP_KEYS 0

#if DUMP_KEYS
static const char *dump_key(const void *k, const void *v, const void *p)
{
	(void)p; (void)k;
	const op_table *op = (const op_table*)v;
	static char tmpbuf[1024];
	snprintf(tmpbuf, sizeof(tmpbuf), "'%s:%u:%u'", op->name, op->specifier, op->priority);
	return tmpbuf;
}
#endif

bool set_op(module *m, const char *name, unsigned specifier, unsigned priority)
{
	miter *iter = m_find_key(m->ops, name);
	op_table *ptr;

	while (m_next_key(iter, (void**)&ptr)) {
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

	iter = m_find_key(m->defops, name);

	while (m_next_key(iter, (void**)&ptr)) {
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
	tmp->name = set_known(m, name);
	tmp->priority = priority;
	tmp->specifier = specifier;
	m->loaded_ops = false;
	m->user_ops = true;
	m_app(m->ops, tmp->name, tmp);

#if DUMP_KEYS
	sl_dump(m->ops, dump_key, m);
	sl_dump(m->defops, dump_key, m);
#endif

	return true;
}

static unsigned find_op_internal(module *m, const char *name, unsigned specifier)
{
	miter *iter;
	op_table *ptr;

	iter = m_find_key(m->ops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			m_done(iter);
			return ptr->priority;
		}
	}

	iter = m_find_key(m->defops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			m_done(iter);
			return ptr->priority;
		}
	}

	return 0;
}

unsigned find_op(module *m, const char *name, unsigned specifier)
{
	unsigned priority = find_op_internal(m, name, specifier);

	if (priority)
		return priority;

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];

		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = find_op_internal(tmp_m, name, specifier);

		if (priority)
			return priority;
	}

	return 0;
}

static unsigned search_op_internal(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	miter *iter;
	op_table *ptr;

	iter = m_find_key(m->defops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (!IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		m_done(iter);
		return n;
	}

	iter = m_find_key(m->ops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (!IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		m_done(iter);
		return n;
	}

	iter = m_find_key(m->defops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		m_done(iter);
		return n;
	}

	iter = m_find_key(m->ops, name);

	while (m_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		m_done(iter);
		return n;
	}

	if (hint_prefix)
		return search_op_internal(m, name, specifier, false);

	return 0;
}

unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	unsigned priority = search_op_internal(m, name, specifier, hint_prefix);

	if (priority)
		return priority;

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];

		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = search_op_internal(tmp_m, name, specifier, hint_prefix);

		if (priority)
			return priority;
	}

#if 0
	for (module *tmp_m = m->pl->modules; tmp_m; tmp_m = tmp_m->next) {
		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = search_op_internal(tmp_m, name, specifier, hint_prefix);

		if (priority) {
			//m->used[m->idx_used++] = tmp_m;
			return priority;
		}
	}
#endif

	return 0;
}

static void check_rule(module *m, db_entry *dbe)
{
	predicate *pr = dbe->owner;
	clause *r = &dbe->cl;
	bool matched = false, me = false;
	bool p1_matched = false, p2_matched = false, p3_matched = false;
	cell *head = get_head(r->cells);
	cell *p1 = head + 1, *p2 = NULL, *p3 = NULL;

	if (pr->key.arity > 1)
		p2 = p1 + p1->nbr_cells;

	if (pr->key.arity > 2)
		p3 = p2 + p2->nbr_cells;

	for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
		if (!me) {
			if (&dbe->cl == r)
				me = true;

			continue;
		}

		cell *head2 = get_head(dbe->cl.cells);
		cell *h21 = head2 + 1, *h22 = NULL, *h23 = NULL;

		if (pr->key.arity > 1)
			h22 = h21 + h21->nbr_cells;

		if (pr->key.arity > 2)
			h23 = h22 + h22->nbr_cells;

		if (!index_cmpkey(p1, h21, m))
			p1_matched = true;

		if (pr->key.arity > 1) {
			if (!index_cmpkey(p2, h22, m))
				p2_matched = true;
		}

		if (pr->key.arity > 2) {
			if (!index_cmpkey(p3, h23, m))
				p3_matched = true;
		}

		if (!index_cmpkey(head, head2, m)) {
			matched = true;
			//break;
		}
	}

	if (!matched) {
		r->is_unique = true;
	}

	if (!p1_matched /*&& r->is_unique*/) {
		r->arg1_is_unique = true;
	}

	if (!p2_matched /*&& r->is_unique*/) {
		r->arg2_is_unique = true;
	}

	if (!p3_matched /*&& r->is_unique*/) {
		r->arg3_is_unique = true;
	}
}

static db_entry *assert_begin(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	cell *c = p1;

	if (!is_check_directive(c))
		c = get_head(p1);

	if (!c) {
		fprintf(stdout, "Error: not a fact or db_entry\n");
		return NULL;
	}

	predicate *pr = find_predicate(m, c);

	if (pr && !consulting && !pr->is_dynamic) {
		fprintf(stdout, "Error: not dynamic %s/%u\n", GET_STR(m, c), c->arity);
		return NULL;
	}

	if (!pr) {
		bool found = false, function = false;

		if (get_builtin(m->pl, GET_STR(m, c), c->arity, &found, &function), found && !function) {
			fprintf(stdout, "Error: permission error modifying %s/%u\n", GET_STR(m, c), c->arity);
			return NULL;
		}

		pr = create_predicate(m, c);
		ensure(pr);

		if (is_check_directive(p1))
			pr->is_check_directive = true;

		if (!consulting) {
			push_property(m, GET_STR(m, c), c->arity, "dynamic");
			pr->is_dynamic = true;
		} else {
			if (m->prebuilt) {
				push_property(m, GET_STR(m, c), c->arity, "built_in");
			}

			push_property(m, GET_STR(m, c), c->arity, "static");
		}

		if (consulting && m->make_public) {
			push_property(m, GET_STR(m, c), c->arity, "public");
			pr->is_public = true;
		}
	}

	if (!pr->is_dynamic)
		pr->is_processed = false;

	if (m->prebuilt)
		pr->is_prebuilt = true;

	db_entry *dbe = calloc(sizeof(db_entry)+(sizeof(cell)*(p1->nbr_cells+1)), 1);
	if (!dbe) {
		pr->is_abolished = true;
		return NULL;
	}

	copy_cells(dbe->cl.cells, p1, p1->nbr_cells);
	dbe->cl.cells[p1->nbr_cells] = (cell){0};
	dbe->cl.cells[p1->nbr_cells].tag = TAG_END;
	dbe->cl.nbr_vars = nbr_vars;
	dbe->cl.nbr_cells = p1->nbr_cells;
	dbe->cl.cidx = p1->nbr_cells+1;
	dbe->cl.ugen_created = ++m->pl->ugen;
	dbe->filename = m->filename;
	dbe->owner = pr;
	return dbe;
}

static void assert_commit(module *m, db_entry *dbe, predicate *pr, bool append)
{
	if (pr->db_id)
		dbe->db_id = append ? pr->db_id : -pr->db_id;

	pr->db_id++;
	pr->cnt++;

	if (pr->is_noindex || (pr->cnt < m->indexing_threshold))
		return;

	if (!pr->idx) {
		//printf("*** index %s/%u\n", GET_STR(m, &pr->key), pr->key.arity);
		pr->idx = m_create(index_cmpkey, NULL, m);
		ensure(pr->idx);
		m_allow_dups(pr->idx, true);

		for (db_entry *cl2 = pr->head; cl2; cl2 = cl2->next) {
			cell *c = get_head(cl2->cl.cells);

			if (!cl2->cl.ugen_erased)
				m_app(pr->idx, c, cl2);
		}
	}

	cell *c = get_head(dbe->cl.cells);

	if (!append)
		m_set(pr->idx, c, dbe);
	else
		m_app(pr->idx, c, dbe);
}

static bool check_multifile(module *m, predicate *pr, db_entry *dbe)
{
	if (pr->head && !pr->is_multifile && !pr->is_dynamic
		&& (GET_STR(m, &pr->key)[0] != '$')) {
		if (dbe->filename != pr->head->filename) {
			for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
				add_to_dirty_list(m, dbe);
				pr->is_processed = false;
			}

			if (dbe->owner->cnt)
				fprintf(stderr, "Warning: overwriting %s/%u\n", GET_STR(m, &pr->key), pr->key.arity);

			m_destroy(pr->idx_save);
			m_destroy(pr->idx);
			pr->idx_save = pr->idx = NULL;
			pr->head = pr->tail = NULL;
			dbe->owner->cnt = 0;
			return false;
		}
	}

	return true;
}

db_entry *asserta_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	db_entry *dbe;
	predicate *pr;

	do {
		dbe = assert_begin(m, nbr_vars, p1, consulting);
		if (!dbe) return NULL;
		pr = dbe->owner;

		if (pr->head)
			pr->head->prev = dbe;
	}
	 while (!check_multifile(m, pr, dbe));

	dbe->next = pr->head;
	pr->head = dbe;

	if (!pr->tail)
		pr->tail = dbe;

	assert_commit(m, dbe, pr, false);

	if (!consulting && 0)
		check_rule(m, dbe);

	return dbe;
}

db_entry *assertz_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting)
{
	db_entry *dbe;
	predicate *pr;

	do {
		dbe = assert_begin(m, nbr_vars, p1, consulting);
		if (!dbe) return NULL;
		pr = dbe->owner;

		if (pr->tail)
			pr->tail->next = dbe;
	}
	 while (!check_multifile(m, pr, dbe));

	dbe->prev = pr->tail;
	pr->tail = dbe;

	if (!pr->head)
		pr->head = dbe;

	assert_commit(m, dbe, pr, true);
	return dbe;
}

bool retract_from_db(module *m, db_entry *dbe)
{
	if (dbe->cl.ugen_erased)
		return false;

	dbe->owner->cnt--;
	dbe->cl.ugen_erased = ++m->pl->ugen;
	dbe->filename = NULL;
	return true;
}

static void xref_cell(module *m, clause *r, cell *c, predicate *parent)
{
	const char *functor = GET_STR(m, c);
	unsigned specifier;

	if ((c->arity == 2)
		&& !GET_OP(c)
		&& (c->val_off != g_braces_s)
		&& search_op(m, functor, &specifier, false)) {
		SET_OP(c, specifier);
	}

	bool found = false, function = false;
	c->fn = get_builtin(m->pl, functor, c->arity, &found, &function);

	if (found) {
		if (function)
			c->flags |= FLAG_FUNCTION;
		else
			c->flags |= FLAG_BUILTIN;

		return;
	}

	if ((c+c->nbr_cells) >= (r->cells+r->cidx-1)) {
		if (parent && (parent->key.val_off == c->val_off) && (parent->key.arity == c->arity)) {
			c->flags |= FLAG_TAIL_REC;
			r->is_tail_rec = true;
		}
	}
}

void xref_rule(module *m, clause *r, predicate *parent)
{
	r->arg1_is_unique = false;
	r->arg2_is_unique = false;
	r->arg3_is_unique = false;
	r->is_unique = false;
	r->is_tail_rec = false;

	cell *head = get_head(r->cells);
	cell *c = head;
	uint64_t mask = 0;

	// Check if a variable occurs more than once in the head...

	for (pl_idx_t i = 0; i < head->nbr_cells; i++, c++) {
		if (!is_variable(c))
			continue;

		uint64_t mask2 = 1ULL << c->var_nbr;

		if (mask & mask2) {
			r->is_complex = true;
			break;
		}

		mask |= mask2;
	}

	// Other stuff...

	c = r->cells;

	if (c->val_off == g_sys_record_key_s)
		return;

	for (pl_idx_t i = 0; i < r->cidx; i++) {
		cell *c = r->cells + i;
		c->flags &= ~FLAG_TAIL_REC;

		if (!is_literal(c))
			continue;

		xref_cell(m, r, c, parent);
	}
}

void xref_db(module *m)
{
	for (predicate *pr = m->head; pr; pr = pr->next) {
		if (pr->is_processed)
			continue;

		pr->is_processed = true;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next)
			xref_rule(m, &dbe->cl, pr);

		if (pr->is_dynamic || pr->idx)
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next)
			check_rule(m, dbe);
	}
}

module *load_text(module *m, const char *src, const char *filename)
{
	parser *p = create_parser(m);
	if (!p) return NULL;
	const char *save_filename = p->m->filename;
	p->m->filename = set_known(m, filename);
	p->consulting = true;
	p->srcptr = (char*)src;
	tokenize(p, false, false);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->cl->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	if (!p->error) {
		xref_db(p->m);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->m->pl->halt = false;
		p->directive = true;

		if (p->run_init) {
			p->consulting = false;
			p->command = true;
			ASTRING(src);
			ASTRING_sprintf(src, "forall(%s:retract((:- initialization(__G_))), (__G_ -> true ; format('Warning: call(~w) failed~n', [__G_])))", p->m->name);

			if (run(p, ASTRING_cstr(src), false))
				p->m->pl->status = false;

			ASTRING_free(src);
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	module *save_m = p->m;
	p->m->filename = save_filename;
	destroy_parser(p);
	return save_m;
}

static bool unload_realfile(module *m, const char *filename)
{
	for (predicate *pr = m->head; pr; pr = pr->next) {
		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.ugen_erased)
				continue;

			if (dbe->filename && !strcmp(dbe->filename, filename)) {
				if (!retract_from_db(m, dbe))
					continue;

				dbe->dirty = pr->dirty_list;
				pr->dirty_list = dbe;
				pr->is_processed = false;
			}
		}

		m_destroy(pr->idx_save);
		m_destroy(pr->idx);
		pr->idx_save = pr->idx = NULL;

		if (!pr->cnt) {
			if (!pr->is_multifile && !pr->is_dynamic)
				pr->is_abolished = true;
		} else
			xref_db(m);
	}

	set_unloaded(m, filename);
	return true;
}

bool unload_file(module *m, const char *filename)
{
	size_t len = strlen(filename);
	char *tmpbuf = malloc(len + 20);
	memcpy(tmpbuf, filename, len+1);
	strcat(tmpbuf, ".pl");

	if (tmpbuf[0] == '~') {
		const char *ptr = getenv("HOME");

		if (ptr) {
			tmpbuf = realloc(tmpbuf, strlen(ptr) + 10 + strlen(filename) + 20);
			strcpy(tmpbuf, ptr);
			strcat(tmpbuf, filename+1);
		}
	}

	char *savebuf = strdup(tmpbuf);
	char *realbuf = NULL;

	if (!(realbuf = realpath(tmpbuf, NULL))) {
		strcpy(tmpbuf, savebuf);

		if (!(realbuf = realpath(tmpbuf, NULL))) {
			free(tmpbuf);
			return false;
		}
	}

	free(savebuf);
	free(tmpbuf);
	filename = realbuf;
	return unload_realfile(m, filename);
}

module *load_fp(module *m, FILE *fp, const char *filename, bool including)
{
	parser *p = create_parser(m);
	if (!p) return NULL;
	const char *save_filename = m->filename;
	if (!including) m->filename = set_known(m, filename);
	p->consulting = true;
	p->fp = fp;
	bool ok = false;

	virtual_term(p, "begin_of_file.");
	tokenize(p, false, false);

	do {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
			virtual_term(p, "end_of_file.");
			break;
		}

		p->srcptr = p->save_line;

		if (!tokenize(p, false, false))
			break;

		ok = !p->error;
	}
	 while (ok && !p->already_loaded && !g_tpl_interrupt);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->cl->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement\n");

		p->error = true;
	}

	module *save_m = p->m;

	if (!p->error && !p->already_loaded) {
		xref_db(p->m);
		int save = p->m->pl->quiet;
		p->m->pl->quiet = true;
		p->directive = true;

		if (p->run_init) {
			p->command = true;
			p->consulting = false;
			ASTRING(src);
			ASTRING_sprintf(src, "forall(%s:retract((:- initialization(__G_))), (__G_ -> true ; format('Warning: call(~w) failed~n', [__G_])))", p->m->name);

			if (run(p, ASTRING_cstr(src), false))
				p->m->pl->status = false;

			ASTRING_free(src);
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	ok = !p->error;
	destroy_parser(p);
	m->filename = save_filename;

	if (!ok)
		unload_realfile(m, filename);

	return save_m;
}

module *load_file(module *m, const char *filename, bool including)
{
	const char *orig_filename = filename;

	if (!strcmp(filename, "user")) {
		for (int i = 0; i < MAX_STREAMS; i++) {
			stream *str = &m->pl->streams[i];
			char tmpbuf[256];
			static unsigned s_cnt = 1;
			snprintf(tmpbuf, sizeof(tmpbuf), "user_%u\n", s_cnt++);
			filename = set_loaded(m, tmpbuf);

			if (strcmp(str->name, "user_input"))
				continue;

			while (m->pl->p && m->pl->p->srcptr && *m->pl->p->srcptr) {
				m->filename = filename;
				parser *p = create_parser(m);
				if (!p) return NULL;
				p->srcptr = m->pl->p->srcptr;
				p->consulting = true;
				p->m = m;

				if (!tokenize(p, false, false))
					break;

				m->pl->p->srcptr = p->srcptr;
				destroy_parser(p);
			}

			module *save_m = load_fp(m, str->fp, filename, including);
			clearerr(str->fp);
			return save_m;
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

	char *savebuf = strdup(tmpbuf);
	char *realbuf = NULL;
	strcpy(tmpbuf, filename);
	strcat(tmpbuf, ".pl");

	if (!(realbuf = realpath(tmpbuf, NULL))) {
		strcpy(tmpbuf, savebuf);

		if (!(realbuf = realpath(tmpbuf, NULL))) {
			free(savebuf);
			free(tmpbuf);
			return NULL;
		}
	}

	free(savebuf);
	free(tmpbuf);

	if (is_loaded(m, realbuf))
		return m;

	filename = set_loaded(m, realbuf);

	struct stat st = {0};
	stat(filename, &st);

	if ((st.st_mode & S_IFMT) == S_IFDIR) {
		char *tmpbuf = malloc(strlen(orig_filename+20));
		strcpy(tmpbuf, orig_filename);
		strcat(tmpbuf, ".pl");
		m = load_file(m, tmpbuf, including);
		free(tmpbuf);
		return m;
	}

	FILE *fp = fopen(filename, "r");

	if (!fp) {
		free(realbuf);
		return NULL;
	}

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	clearerr(fp);
	module *save_m = load_fp(m, fp, filename, including);
	fclose(fp);
	free(realbuf);
	return save_m;
}

static void module_save_fp(module *m, FILE *fp, int canonical, int dq)
{
	(void) dq;
	pl_idx_t ctx = 0;
	query q = (query){0};
	q.pl = m->pl;
	q.st.m = m;

	for (predicate *pr = m->head; pr; pr = pr->next) {
		if (pr->is_prebuilt)
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.ugen_erased)
				continue;

			if (canonical)
				print_canonical(&q, fp, dbe->cl.cells, ctx, 0);
			else
				print_term(&q, fp, dbe->cl.cells, ctx, 0);

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
	m->p->line_nbr = 1;
	tokenize(m->p, false, false);
	m->prebuilt = false;
	m->p->consulting = save;
}
#endif

void destroy_module(module *m)
{
	while (m->tasks) {
		query *task = m->tasks->next;
		destroy_query(m->tasks);
		m->tasks = task;
	}

	miter *iter = m_first(m->defops);
	op_table *opptr;

	while (m_next(iter, (void**)&opptr))
		free(opptr);

	m_destroy(m->defops);
	iter = m_first(m->ops);

	while (m_next(iter, (void**)&opptr))
		free(opptr);

	m_destroy(m->ops);

	for (predicate *pr = m->head; pr;) {
		predicate *save = pr->next;
		destroy_predicate(m, pr);
		pr = save;
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

	m_destroy(m->index);
	destroy_parser(m->p);
	clear_loaded(m);
	free(m);
}

void duplicate_module(prolog *pl, module *m, const char *name)
{
	module *tmp_m = create_module(pl, name);
	tmp_m->orig = m;
}

module *create_module(prolog *pl, const char *name)
{
	module *m = calloc(1, sizeof(module));
	ensure(m);

	m->pl = pl;
	m->filename = set_known(m, name);
	m->name = set_known(m, name);
	m->flag.unknown = UNK_ERROR;
	m->flag.double_quote_chars = true;
	m->flag.character_escapes = true;
	m->error = false;
	m->id = index_from_pool(pl, name);
	m->defops = m_create((void*)strcmp, NULL, NULL);
	m_allow_dups(m->defops, false);
	m->indexing_threshold = 4096;

	if (strcmp(name, "system")) {
		for (const op_table *ptr = g_ops; ptr->name; ptr++) {
			op_table *tmp = malloc(sizeof(op_table));
			memcpy(tmp, ptr, sizeof(op_table));
			m_app(m->defops, tmp->name, tmp);
		}
	}

	m->ops = m_create((void*)strcmp, NULL, NULL);
	m_allow_dups(m->ops, false);
	m->index = m_create(predicate_cmpkey, NULL, m);
	m_allow_dups(m->index, false);
	m->p = create_parser(m);
	ensure(m->p);

	set_multifile_in_db(m, "$predicate_property", 2);
	set_multifile_in_db(m, ":-", 1);

	parser *p = create_parser(m);
	if (p) {
		p->consulting = true;
		xref_db(p->m);
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
