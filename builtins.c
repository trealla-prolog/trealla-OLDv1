#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define snprintf _snprintf
#define msleep Sleep
#define PATH_SEP "\\"
#define USE_MMAP 0
#else
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#include <unistd.h>
#if USE_MMAP
#include <sys/mman.h>
#endif
#define PATH_SEP "/"
#endif

#include "trealla.h"
#include "internal.h"
#include "network.h"
#include "base64.h"
#include "utf8.h"
#include "builtins.h"

#if USE_OPENSSL
#include "openssl/sha.h"
#endif

#define CHECK_OVERFLOW 1
#define MAX_VARS 32768

#ifndef _WIN32
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

static double rat_to_float(cell *n)
{
	return (double)n->val_num / n->val_den;
}

static int do_throw_term(query *q, cell *c);

static int do_yield_0(query *q, int msecs)
{
	q->yielded = 1;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs;
	make_choice(q);
	return 0;
}

static void pin_vars(query *q, uint32_t mask)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	ch->pins = mask;
}

static void unpin_vars(query *q)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	frame *g = GET_FRAME(q->st.curr_frame);
	uint32_t mask = 1;

	for (unsigned i = 0; i < g->nbr_vars; i++, mask <<= 1) {
		if (!(ch->pins & mask))
			continue;

		slot *e = GET_SLOT(g, i);
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = NULL;
	}

	ch->pins = 0;
}

static void set_pinned(query *q, int i)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	ch->pins |= 1 << i;
}

static int is_pinned(query *q, int i)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	return ch->pins & (1 << i) ? 1 : 0;
}

static void set_params(query *q, idx_t p1, idx_t p2)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	ch->v1 = p1;
	ch->v2 = p2;
}

static void get_params(query *q, idx_t *p1, idx_t *p2)
{
	idx_t curr_choice = q->cp - 1;
	choice *ch = q->choices + curr_choice;
	if (p1) *p1 = ch->v1;
	if (p2) *p2 = ch->v2;
}

static void make_int(cell *tmp, int_t v)
{
	tmp->val_type = TYPE_INTEGER;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	tmp->val_num = v;
	tmp->val_den = 1;
}

static void make_float(cell *tmp, double v)
{
	tmp->val_type = TYPE_FLOAT;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	tmp->val_flt = v;
}

static void make_structure(cell *tmp, idx_t offset, void *fn, unsigned arity, idx_t extra_cells)
{
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1 + extra_cells;
	tmp->flags = FLAG_BUILTIN;
	tmp->arity = arity;
	tmp->fn = fn;
	tmp->val_off = offset;
}

void make_end(cell *tmp)
{
	tmp->val_type = TYPE_END;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	tmp->match = NULL;
	tmp->val_ptr = NULL;
}

static void make_end_return(cell *tmp, cell *c)
{
	make_end(tmp);
	tmp->val_ptr = c + c->nbr_cells;
}

static void make_literal(cell *tmp, idx_t offset)
{
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_off = offset;
}

static void make_smalln(cell *tmp, const char *s, size_t n)
{
	tmp->val_type = TYPE_CSTRING;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	memcpy(tmp->val_chr, s, n);
	tmp->val_chr[n] = '\0';
}

static void make_small(cell *tmp, const char *s)
{
	size_t n = strlen(s);
	make_smalln(tmp, s, n);
}

static void init_tmp_heap(query* q)
{
	if (!q->tmp_heap)
		q->tmp_heap = calloc(q->tmph_size, sizeof(cell));

	q->tmphp = 0;
}

static cell *alloc_tmp_heap(query *q, idx_t nbr_cells)
{
	if (!q->tmp_heap) {
		if (q->tmph_size < nbr_cells)
			q->tmph_size = nbr_cells;

		q->tmp_heap = calloc(q->tmph_size, sizeof(cell));
	}

	while ((q->tmphp + nbr_cells) >= q->tmph_size) {
		q->tmph_size += q->tmph_size / 2;
		q->tmp_heap = realloc(q->tmp_heap, sizeof(cell)*q->tmph_size);
	}

	cell *c = q->tmp_heap + q->tmphp;
	q->tmphp += nbr_cells;
	memset(c, 0, sizeof(cell)*nbr_cells);
	return c;
}

static idx_t tmp_heap_used(const query *q) { return q->tmphp; }
static cell *get_tmp_heap(const query *q, idx_t i) { return q->tmp_heap + i; }

static cell *alloc_heap(query *q, idx_t nbr_cells)
{
	if (!q->arenas) {
		if (q->h_size < nbr_cells)
			q->h_size = nbr_cells;

		arena *a = calloc(1, sizeof(arena));
		a->heap = calloc(q->h_size, sizeof(cell));
		a->h_size = q->h_size;
		a->nbr = q->st.anbr++;
		q->arenas = a;
	}

	if ((q->st.hp + nbr_cells) >= q->h_size) {
		arena *a = calloc(1, sizeof(arena));
		a->next = q->arenas;

		if (q->h_size < nbr_cells) {
			q->h_size = nbr_cells;
			q->h_size += nbr_cells / 2;
		}

		a->heap = calloc(q->h_size, sizeof(cell));
		a->h_size = q->h_size;
		a->nbr = q->st.anbr++;
		q->arenas = a;
		q->st.hp = 0;
	}

	cell *c = q->arenas->heap + q->st.hp;
	memset(c, 0, sizeof(cell)*nbr_cells);
	q->st.hp += nbr_cells;
	q->arenas->hp = q->st.hp;
	return c;
}

//static idx_t heap_used(const query *q) { return q->st.hp; }
//static cell *get_heap(const query *q, idx_t i) { return q->arenas->heap + i; }

static cell *alloc_cstringn(query *q, const char *s, size_t n)
{
	cell *tmp = alloc_heap(q, 1);
	tmp->val_type = TYPE_CSTRING;
	tmp->flags = FLAG_BLOB;
	tmp->nbr_cells = 1;
	tmp->val_str = malloc(n+1);
	memcpy(tmp->val_str, s, n);
	tmp->val_str[n] = '\0';
	tmp->len_str = n;
	return tmp;
}

static void init_queue(query* q)
{
	free(q->queue[0]);
	q->queue[0] = NULL;
	q->qp[0] = 0;
}

static idx_t queue_used(const query *q) { return q->qp[0]; }
static cell *get_queue(query *q) { return q->queue[0]; }

static cell *pop_queue(query *q)
{
	if (!q->qp[0])
		return NULL;

	cell *c = q->queue[0] + q->popp;
	q->popp += c->nbr_cells;

	if (q->popp == q->qp[0])
		q->popp = q->qp[0] = 0;

	return c;
}

static cell *alloc_queue(query *q, const cell *c)
{
	if (!q->queue[0])
		q->queue[0] = calloc(q->q_size[0], sizeof(cell));

	while ((q->qp[0]+c->nbr_cells) >= q->q_size[0]) {
		q->q_size[0] += q->q_size[0] / 2;
		q->queue[0] = realloc(q->queue[0], sizeof(cell)*q->q_size[0]);
	}

	cell *dst = q->queue[0] + q->qp[0];
	q->qp[0] += copy_cells(dst, c, c->nbr_cells);
	return dst;
}

static void init_queuen(query* q)
{
	free(q->queue[q->st.qnbr]);
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
}

static idx_t queuen_used(const query *q) { return q->qp[q->st.qnbr]; }
static cell *get_queuen(query *q) { return q->queue[q->st.qnbr]; }

static cell *alloc_queuen(query *q, int qnbr, const cell *c)
{
	if (!q->queue[qnbr])
		q->queue[qnbr] = calloc(q->q_size[qnbr], sizeof(cell));

	while ((q->qp[qnbr]+c->nbr_cells) >= q->q_size[qnbr]) {
		q->q_size[qnbr] += q->q_size[qnbr] / 2;
		q->queue[qnbr] = realloc(q->queue[qnbr], sizeof(cell)*q->q_size[qnbr]);
	}

	cell *dst = q->queue[qnbr] + q->qp[qnbr];
	q->qp[qnbr] += copy_cells(dst, c, c->nbr_cells);
	return dst;
}

void alloc_list(query *q, const cell *c)
{
	init_tmp_heap(q);
	append_list(q, c);
}

void append_list(query *q, const cell *c)
{
	cell *tmp = alloc_tmp_heap(q, 1+c->nbr_cells);
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1 + c->nbr_cells;
	tmp->val_off = g_dot_s;
	tmp->arity = 2;
	tmp++;
	copy_cells(tmp, c, c->nbr_cells);
}

cell *end_list(query *q)
{
	cell *tmp = alloc_tmp_heap(q, 1);
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	idx_t nbr_cells = tmp_heap_used(q);
	tmp = alloc_heap(q, nbr_cells);
	copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	init_tmp_heap(q);
	return tmp;
}

static cell tmp_cstringn(query *q, const char *s, size_t n)
{
	cell tmp;

	if (strlen(s) < MAX_SMALL_STRING) {
		make_smalln(&tmp, s, n);
		return tmp;
	}

	tmp.val_type = TYPE_CSTRING;
	tmp.flags = FLAG_BLOB;
	tmp.val_str = malloc(n+1);
	memcpy(tmp.val_str, s, n);
	tmp.val_str[n] = '\0';
	tmp.len_str = n;
	tmp.nbr_cells = 1;
	tmp.arity = 0;
	return tmp;
}

static cell tmp_cstring(query *q, const char *s)
{
	size_t n = strlen(s);
	return tmp_cstringn(q, s, n);
}

static cell make_cstringn(query *q, const char *s, size_t n)
{
	cell tmp;

	if (n < MAX_SMALL_STRING)
		make_smalln(&tmp, s, n);
	else
		tmp = *alloc_cstringn(q, s, n);

	return tmp;
}

static cell make_cstring(query *q, const char *s)
{
	size_t n = strlen(s);
	return make_cstringn(q, s, n);
}

static cell make_string(query *q, const char *s, size_t n)
{
	cell tmp = *alloc_cstringn(q, s, n);
	tmp.flags |= FLAG_STRING;
	return tmp;
}

static void deep_clone2_to_tmp(query *q, cell *p1, idx_t p1_ctx)
{
	idx_t save_idx = tmp_heap_used(q);
	cell *tmp = alloc_tmp_heap(q, 1);
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1)) {
		if (is_blob(p1) && !is_const_cstring(p1))
			tmp->val_str = strdup(p1->val_str);

		return;
	}

	idx_t nbr_cells = p1->nbr_cells;
	p1++;

	for (idx_t i = 1; i < nbr_cells;) {
		if (is_variable(p1)) {
			cell *c = deref_var(q, p1, p1_ctx);
			deep_clone2_to_tmp(q, c, q->latest_ctx);
		} else
			deep_clone2_to_tmp(q, p1, p1_ctx);

		i += p1->nbr_cells;
		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
}

static cell *deep_clone_to_tmp(query *q, cell *p1, idx_t p1_ctx)
{
	init_tmp_heap(q);

	if (is_variable(p1)) {
		p1 = deref_var(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	deep_clone2_to_tmp(q, p1, p1_ctx);
	return q->tmp_heap;
}

cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx)
{
	if (is_variable(p1)) {
		p1 = deref_var(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	cell *tmp = alloc_heap(q, p1->nbr_cells);
	copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

void throw_error(query *q, cell *c, const char *err_type, const char *expected)
{
	cell tmp;

	if (is_literal(c))
		tmp = make_cstring(q, GET_STR(c));
	else
		tmp = *c;

	size_t len = write_term_to_buf(q, NULL, 0, &tmp, 1, 0, 0);
	char *dst = malloc(len+1);
	write_term_to_buf(q, dst, len+1, &tmp, 1, 0, 0);
	size_t len2 = (len * 2) + strlen(err_type) + strlen(expected) + LEN_STR(q->st.curr_cell) + 20;
	char *dst2 = malloc(len2+1);

	if (is_variable(c)) {
		err_type = "instantiation_error";
		snprintf(dst2, len2, "error(%s,%s/%u)", err_type, GET_STR(q->st.curr_cell), q->st.curr_cell->arity);
	} else if (!strcmp(err_type, "type_error")) {
		const char *t = expected;
		if (!strncmp(t,"iso_",4)) t = t+4;
		char tmpbuf[1024];
		strcpy(tmpbuf, t);
		char *ptr = strchr(tmpbuf, '_');
		if (ptr) *ptr = '\0';
		snprintf(dst2, len2, "error(%s(%s,%s))", err_type, tmpbuf, dst);
	} else {
		snprintf(dst2, len2, "error(%s(%s,(%s)/%u),%s/%u)", err_type, expected, dst, c->arity, GET_STR(q->st.curr_cell), q->st.curr_cell->arity);
	}

	parser *p = q->m->p;
	p->srcptr = dst2;
	parser_tokenize(p, 0, 0);
	parser_attach(p, 0);
	//parser_xref(p, p->t, NULL);
	do_throw_term(q, p->t->cells);
	clear_term(p->t);
	free(dst2);
	free(dst);
}

static int fn_iso_unify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return unify(q, p1, p1_ctx, p2, p2_ctx);
}

static int fn_iso_notunify_2(query *q)
{
	return !fn_iso_unify_2(q);
}

static int fn_iso_repeat_0(query *q)
{
	make_choice(q);
	return 1;
}

static int fn_iso_true_0(query *q)
{
	return 1;
}

static int fn_iso_fail_0(query *q)
{
	return 0;
}

static int fn_iso_halt_0(query *q)
{
	q->halt_code = q->halt = q->error = 1;
	return 0;
}

static int fn_iso_halt_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	q->halt_code = p1->val_num;
	q->halt = q->error = 1;
	return 0;
}

static int fn_iso_number_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_number(p1);
}

static int fn_iso_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_iso_atom(p1);
}

static int fn_iso_compound_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_structure(p1) || is_list(p1) ? 1 : 0;
}

static int fn_iso_atomic_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_atomic(p1);
}

static int fn_iso_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_variable(p1);
}

static int fn_iso_nonvar_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !is_variable(p1);
}

static int check_has_vars(query *q, cell *c)
{
	if (is_variable(c))
		return 1;

	idx_t save_ctx = q->latest_ctx;
	idx_t nbr = c->nbr_cells;
	c++;

	for (idx_t i = 1; i < nbr; i++, c++) {
		cell *c2 = deref_var(q, c, save_ctx);

		if (check_has_vars(q, c2))
			return 1;
	}

	return 0;
}

static int fn_iso_ground_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !check_has_vars(q, p1);
}

static int fn_iso_cut_0(query *q)
{
	cut_me(q, 0);
	return 1;
}

static int fn_local_cut_0(query *q)
{
	cut_me(q, 1);
	return 1;
}

static int fn_iso_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_callable(p1))
		return 0;

	return 1;
}

static int fn_iso_current_rule_1(query *q)
{
	GET_FIRST_ARG(p1,structure);

	if (strcmp(GET_STR(p1), "/")) {
		throw_error(q, p1, "type_error", "predicate_indicator");
		return 0;
	}

	cell *pf = deref_var(q, p1+1,p1_ctx);
	cell *pa = deref_var(q, p1+2, p1_ctx);

	if (!is_atom(pf)) {
		throw_error(q, p1, "type_error", "atom");
		return 0;
	}

	if (!is_integer(pa)) {
		throw_error(q, p1, "type_error", "integer");
		return 0;
	}

	const char *functor = GET_STR(pf);
	unsigned arity = pa->val_num;
	module *m = q->m;

	if (strchr(functor, ':')) {
		char tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
		m = find_module(tmpbuf1);
	}

	if (!m)
		m = q->m;

	module *tmp_m = NULL;

	while (m) {
		if (find_functor(m, functor, arity))
			return 1;

		if (!tmp_m)
			m = tmp_m = g_modules;
		else
			m = m->next;
	}

	if (check_builtin(q->m, functor, arity))
		return 1;

	return 0;
}

static int fn_iso_atom_chars_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");
		return 0;
	}

	if (is_variable(p1) && is_nil(p2)) {
		cell tmp;
		make_literal(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p2) && !strcmp(GET_STR(p1), "")) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!is_variable(p2)) {
		cell *head = LIST_HEAD(p2);
		cell *tail = LIST_TAIL(p2);
		head = deref_var(q, head, p2_ctx);
		q->latest_ctx = p2_ctx;

		size_t bufsiz;
		char *tmpbuf = malloc(bufsiz=256), *dst = tmpbuf;
		*tmpbuf = '\0';

		while (tail) {
			tail = deref_var(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

			if (!is_atom(head)) {
				free(tmpbuf);
				throw_error(q, head, "type_error", "atom");
				return 0;
			}

			const char *src = GET_STR(head);
			int nbytes = len_char_utf8(src);
			size_t nlen = dst - tmpbuf;

			if ((nlen+10) > bufsiz) {
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				tmpbuf[nlen] = '\0';
			}

			dst = tmpbuf+nlen;
			strncpy(dst, src, nbytes);
			dst += nbytes;
			*dst = '\0';

			if (is_literal(tail)) {
				if (tail->val_off == g_nil_s)
					break;
			}

			if (!is_list(tail)) {
				throw_error(q, tail, "type_error", "list");
				q->error = 1;
				return 0;
			}

			head = LIST_HEAD(tail);
			tail = LIST_TAIL(tail);
			head = deref_var(q, head, q->latest_ctx);
		}

		cell tmp = make_cstring(q, tmpbuf);
		free(tmpbuf);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *src = GET_STR(p1);
	int nbytes = len_char_utf8(src);
	char tmpbuf[80];
	memcpy(tmpbuf, src, nbytes);
	tmpbuf[nbytes] = '\0';
	cell tmp = tmp_cstring(q, tmpbuf);
	src += nbytes;
	alloc_list(q, &tmp);

	while (*src) {
		nbytes = len_char_utf8(src);
		cell tmp = tmp_cstringn(q, src, nbytes);
		src += nbytes;
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	fix_list(l, l->nbr_cells);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static int fn_iso_atom_codes_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");
		return 0;
	}

	if (!is_variable(p2) && is_nil(p2)) {
		cell tmp;
		make_literal(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p2) && !strcmp(GET_STR(p1), "")) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!is_variable(p2) && is_variable(p1)) {
		cell *head = LIST_HEAD(p2);
		cell *tail = LIST_TAIL(p2);
		head = deref_var(q, head, p2_ctx);

		size_t nbytes;
		char *tmpbuf = malloc(nbytes=256), *dst = tmpbuf;

		while (tail) {
			tail = deref_var(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

			if (!is_integer(head)) {
				throw_error(q, head, "type_error", "integer");
				return 0;
			}

			int_t val = head->val_num;
			char ch[10];
			put_char_utf8(ch, val);
			size_t nlen = dst - tmpbuf;

			if ((nlen+strlen(ch)) >= nbytes)
				tmpbuf = realloc(tmpbuf, nbytes*=2);

			dst = tmpbuf+nlen;
			strcpy(dst, ch);
			dst += strlen(ch);

			if (is_literal(tail)) {
				if (tail->val_off == g_nil_s)
					break;
			}

			if (!is_list(tail)) {
				throw_error(q, tail, "type_error", "list");
				return 0;
			}

			head = LIST_HEAD(tail);
			tail = LIST_TAIL(tail);
			head = deref_var(q, head, q->latest_ctx);
		}

		cell tmp = make_cstring(q, tmpbuf);
		free(tmpbuf);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *tmpbuf = GET_STR(p1);
	const char *src = tmpbuf;
	cell tmp;
	make_int(&tmp, get_char_utf8(&src));
	alloc_list(q, &tmp);

	while (*src) {
		cell tmp;
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	fix_list(l, l->nbr_cells);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static int fn_iso_number_chars_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,list_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");
		return 0;
	}

	if (!is_variable(p2)) {
		cell *head = LIST_HEAD(p2);
		cell *tail = LIST_TAIL(p2);
		head = deref_var(q, head, p2_ctx);

		int_t val = 0;

		while (tail) {
			if (!is_atom(head)) {
				throw_error(q, head, "type_error", "atom");
				return 0;
			}

			const char *src = GET_STR(head);
			int ch = *src;

			if (!isdigit(ch)) {
				throw_error(q, head, "domain_error", "digit");
				return 0;
			}

			val *= 10;
			val += ch - '0';

			if (is_literal(tail)) {
				if (tail->val_off == g_nil_s)
					break;
			}

			if (!is_list(tail)) {
				throw_error(q, tail, "type_error", "list");
				return 0;
			}

			head = LIST_HEAD(tail);
			tail = LIST_TAIL(tail);
			head = deref_var(q, head, q->latest_ctx);
		}

		cell tmp;
		make_int(&tmp, val);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[256];
	sprint_int(tmpbuf, sizeof(tmpbuf), p1->val_num, 10);
	const char *src = tmpbuf;
	cell tmp = tmp_cstringn(q, src, 1);
	alloc_list(q, &tmp);

	while (*++src) {
		cell tmp = tmp_cstringn(q, src, 1);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	fix_list(l, l->nbr_cells);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static int fn_iso_number_codes_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,iso_list_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");
		return 0;
	}

	if (!is_variable(p2)) {
		cell *head = LIST_HEAD(p2);
		cell *tail = LIST_TAIL(p2);
		head = deref_var(q, head, p2_ctx);

		int_t val = 0;

		while (tail) {
			if (!is_integer(head)) {
				throw_error(q, head, "type_error", "integer");
				return 0;
			}

			int ch = head->val_num;

			if ((ch < '0') || (ch > '9')) {
				throw_error(q, head, "domain_error", "digit");
				return 0;
			}

			val *= 10;
			val += ch - '0';

			if (is_literal(tail)) {
				if (tail->val_off == g_nil_s)
					break;
			}

			if (!is_list(tail)) {
				throw_error(q, tail, "type_error", "list");
				return 0;
			}

			head = LIST_HEAD(tail);
			tail = LIST_TAIL(tail);
			head = deref_var(q, head, q->latest_ctx);
		}

		cell tmp;
		make_int(&tmp, val);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[256];
	sprint_int(tmpbuf, sizeof(tmpbuf), p1->val_num, 10);
	const char *src = tmpbuf;
	cell tmp;
	make_int(&tmp, *src);
	alloc_list(q, &tmp);

	while (*++src) {
		cell tmp;
		make_int(&tmp, *src);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	fix_list(l, l->nbr_cells);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static int fn_iso_sub_atom_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p3,integer_or_var);
	GET_NEXT_ARG(p4,integer_or_var);
	GET_NEXT_ARG(p5,atom_or_var);
	int before = 0, len = 0;

	if (!q->retry) {
		make_choice(q);

		if (!is_variable(p2))
			before = p2->val_num;

		if (!is_variable(p3)) {
			len = p3->val_num;
			set_pinned(q, 3);
		}
	} else {
		idx_t v1, v2;
		get_params(q, &v1, &v2);
		before = v1;
		len = v2;

		if (is_pinned(q, 3)) {
			len = p3->val_num;
			before++;

			if ((before+len) > LEN_STR(p1)) {
				drop_choice(q);
				return 0;
			}
		}
	}

	if (len > (LEN_STR(p1)-before)) {
		before++;
		len = 0;
	}

	if (before > LEN_STR(p1)) {
		drop_choice(q);
		return 0;
	}

	int any = 0;

	for (int i = before; i <= LEN_STR(p1); i++) {
		for (int j = len; j <= LEN_STR(p1); j++) {
			cell tmp;
			any = 1;

			set_params(q, i, j+1);
			make_choice(q);
			make_int(&tmp, i);

			if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame)) {
				drop_choice(q);
				continue;
			}

			make_int(&tmp, j);

			if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
				drop_choice(q);
				continue;
			}

			make_int(&tmp, LEN_STR(p1)-i-j);

			if (!unify(q, p4, p4_ctx, &tmp, q->st.curr_frame)) {
				drop_choice(q);
				continue;
			}

			const char *src = GET_STR(p1) + i;
			tmp = make_cstringn(q, src, j);

			if (!unify(q, p5, p5_ctx, &tmp, q->st.curr_frame)) {
				drop_choice(q);
				continue;
			}

			any++;
			return 1;
		}
	}

	drop_choice(q);
	return 0;
}

// NOTE: this just handles the mode(-,-,+) case...

static int do_atom_concat_3(query *q)
{
	if (!q->retry) {
		GET_FIRST_ARG(p1,variable);
		GET_NEXT_ARG(p2,variable);
		GET_NEXT_ARG(p3,atom);
		cell tmp;
		make_literal(&tmp, g_empty_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		set_var(q, p2, p2_ctx, p3, q->st.curr_frame);
		make_choice(q);
		return 1;
	}

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,atom);
	const char *src = GET_STR(p3);
	size_t len = LEN_STR(p1) + len_char_utf8(src);
	char *dst1 = strndup(src, len);
	char *dst2 = strdup(src+len);
	int done = 0;

	if (!*dst2)
		done = 1;

	GET_RAW_ARG(1,p1_raw);
	GET_RAW_ARG(2,p2_raw);
	cell tmp = make_cstring(q, dst1);
	free(dst1);
	reset_value(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame);
	tmp = make_cstring(q, dst2);
	reset_value(q, p2_raw, p2_raw_ctx, &tmp, q->st.curr_frame);
	free(dst2);

	if (!done)
		make_choice(q);

	return 1;
}

static int fn_iso_atom_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1) && is_variable(p2))
		return do_atom_concat_3(q);

	if (is_variable(p3)) {
		if (!is_iso_atom(p1)) {
			throw_error(q, p1, "type_error", "atom");
			return 0;
		}

		if (!is_atom(p2)) {
			throw_error(q, p1, "type_error", "atom");
			return 0;
		}

		const char *src1, *src2;
		size_t len1, len2;
		char tmpbuf1[256], tmpbuf2[256];

		if (is_atom(p1)) {
			src1 = GET_STR(p1);
			len1 = LEN_STR(p1);
		} else {
			write_term_to_buf(q, tmpbuf1, sizeof(tmpbuf1), p1, 1, 0, 0);
			len1 = strlen(tmpbuf1);
			src1 = tmpbuf1;
		}

		if (is_atom(p2)) {
			src2 = GET_STR(p2);
			len2 = LEN_STR(p2);
		} else {
			write_term_to_buf(q, tmpbuf2, sizeof(tmpbuf2), p2, 1, 0, 0);
			len2 = strlen(tmpbuf2);
			src2 = tmpbuf2;
		}

		size_t nbytes = len1 + len2;
		char *dst = malloc(nbytes + 1);
		memcpy(dst, src1, len1);
		memcpy(dst+len1, src2, len2);
		dst[nbytes] = '\0';
		cell tmp = make_cstringn(q, dst, nbytes);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (is_variable(p1)) {
		if (strcmp(GET_STR(p3)+(LEN_STR(p3)-LEN_STR(p2)), GET_STR(p2)))
			return 0;

		char *dst = strndup(GET_STR(p3), LEN_STR(p3)-LEN_STR(p2));
		cell tmp = make_cstring(q, dst);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (is_variable(p2)) {
		if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
			return 0;

		char *dst = strdup(GET_STR(p3)+LEN_STR(p1));
		cell tmp = make_cstring(q, dst);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
		return 0;

	if (strcmp(GET_STR(p3)+LEN_STR(p1), GET_STR(p2)))
		return 0;

	return 1;
}

static int fn_iso_atom_length_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,integer_or_var);
	size_t len;

	if (is_string(p1)) {
		throw_error(q, p1, "type_error", "atom");
		return -1;
	}

	if (is_blob(p1)) {
		const char *p = GET_STR(p1);
		len = substrlen_utf8(p, p+p1->len_str);
	} else
		len = strlen_utf8(GET_STR(p1));

	cell tmp;
	make_int(&tmp, len);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int new_stream(query *q)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		if (!g_streams[i].fp)
			return i;
	}

	return -1;
}

static int get_named_stream(query *q, const char *name)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &g_streams[i];

		if (!str->name)
			continue;

		if (!strcmp(str->name, name))
			return i;
	}

	return -1;
}

static int get_stream(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_stream(q, GET_STR(p1));

		if (n < 0) {
			throw_error(q, p1, "type_error", "stream");
			return -1;
		}

		return n;
	}

	if (!is_integer(p1) || !(p1->flags&FLAG_STREAM)) {
		throw_error(q, p1, "type_error", "stream");
		return -1;
	}

	if ((p1->val_num < 0) || (p1->val_num >= MAX_STREAMS)) {
		throw_error(q, p1, "type_error", "stream");
		return -1;
	}

	if (!g_streams[p1->val_num].fp) {
		throw_error(q, p1, "type_error", "stream");
		return -1;
	}

	return p1->val_num;
}

static int fn_iso_current_input_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, q->current_input);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_iso_current_output_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, q->current_output);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_iso_set_input_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	q->current_input = get_stream(q, pstr);
	return 1;
}

static int fn_iso_set_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	q->current_output = get_stream(q, pstr);
	return 1;
}

static int fn_iso_stream_property_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,structure);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (p1->arity != 1) {
		throw_error(q, p1, "type_error", "property");
		return 0;
	}

	if (!strcmp(GET_STR(p1), "position")) {
		cell *c = p1 + 1;
		c = deref_var(q, c, q->latest_ctx);
		cell tmp;
		make_int(&tmp, ftello(str->fp));

		if (!unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame))
			return 0;

		return 1;
	}

	if (!strcmp(GET_STR(p1), "line_count") && str->p) {
		cell *c = p1 + 1;
		c = deref_var(q, c, q->latest_ctx);
		cell tmp;
		make_int(&tmp, str->p->line_nbr);

		if (!unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame))
			return 0;

		return 1;
	}

	return 1;
}

static int fn_iso_set_stream_position_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer);
	return !fseeko(str->fp, p1->val_num, SEEK_SET);
}

static int fn_iso_open_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	const char *filename = GET_STR(p1);
	const char *mode = GET_STR(p2);
	int n = new_stream(q);

	if (n < 0) {
		throw_error(q, p1, "resource_error", "too_many_open_streams");
		return 0;
	}

	stream *str = &g_streams[n];
	str->filename = strdup(filename);
	str->name = strdup(filename);
	str->mode = strdup(mode);

	if (!strcmp(mode, "read"))
		str->fp = fopen(filename, "r");
	else if (!strcmp(mode, "write"))
		str->fp = fopen(filename, "w");
	else if (!strcmp(mode, "append"))
		str->fp = fopen(filename, "a");
	else if (!strcmp(mode, "update"))
		str->fp = fopen(filename, "r+");

	if (!str->fp) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		return 0;
	}

	cell *tmp = alloc_heap(q, 1);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p3, p3_ctx, tmp, q->st.curr_frame);
	return 1;
}

static int fn_iso_open_4(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	const char *mode = GET_STR(p2);
	int n = new_stream(q);

	if (n < 0) {
		throw_error(q, p1, "resource_error", "too_many_open_streams");
		return 0;
	}

	const char *filename;
	stream *oldstr = NULL;

	if (is_structure(p1) && (p1->arity == 1) && !strcmp(GET_STR(p1), "stream")) {
		int oldn = get_stream(q, p1+1);

		if (oldn < 0) {
			throw_error(q, p1, "type_error", "not_a_stream");
			return 0;
		}

		stream *oldstr = &g_streams[oldn];
		filename = oldstr->filename;
	} else
		filename = GET_STR(p1);

	stream *str = &g_streams[n];
	str->filename = strdup(filename);
	str->name = strdup(filename);
	str->mode = strdup(mode);
	int binary = 0;

#if USE_MMAP
	cell *mmap_var = NULL;
	idx_t mmap_ctx = 0;
#endif

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref_var(q, h, p4_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!strcmp(GET_STR(c), "mmap")) {
#if USE_MMAP
				mmap_var = c + 1;
				mmap_var = deref_var(q, mmap_var, q->latest_ctx);
				mmap_ctx = q->latest_ctx;
#endif
			} else if (!strcmp(GET_STR(c), "alias")) {
				cell *name = c + 1;
				name = deref_var(q, name, q->latest_ctx);
				free(str->name);
				str->name = strdup(GET_STR(name));
			} else if (!strcmp(GET_STR(c), "type")) {
				cell *name = c + 1;
				name = deref_var(q, name, q->latest_ctx);

				if (is_atom(name) && !strcmp(GET_STR(name), "binary"))
					binary = 1;
				else if (is_atom(name) && !strcmp(GET_STR(name), "text"))
					binary = 0;
			}
		}

		p4 = LIST_TAIL(p4);
		p4 = deref_var(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;
	}

	if (oldstr) {
		int fd = fileno(oldstr->fp);

		if (!strcmp(mode, "read"))
			str->fp = fdopen(fd, binary?"rb":"r");
		else if (!strcmp(mode, "write"))
			str->fp = fdopen(fd, binary?"wb":"w");
		else if (!strcmp(mode, "append"))
			str->fp = fdopen(fd, binary?"ab":"a");
		else if (!strcmp(mode, "update"))
			str->fp = fdopen(fd, binary?"rb+":"r+");
	} else {
		if (!strcmp(mode, "read"))
			str->fp = fopen(filename, binary?"rb":"r");
		else if (!strcmp(mode, "write"))
			str->fp = fopen(filename, binary?"wb":"w");
		else if (!strcmp(mode, "append"))
			str->fp = fopen(filename, binary?"ab":"a");
		else if (!strcmp(mode, "update"))
			str->fp = fopen(filename, binary?"rb+":"r+");
	}

	if (!str->fp) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		return 0;
	}

#if USE_MMAP
	int prot = 0;

	if (!strcmp(mode, "read"))
		prot = PROT_READ;
	else
		prot = PROT_WRITE;

	if (is_variable(mmap_var)) {
		struct stat st = {0};
		stat(filename, &st);
		size_t len = st.st_size;
		int fd = fileno(str->fp);
		void *addr = mmap(0, len, prot, MAP_PRIVATE, fd, 0);
		cell tmp = {0};
		tmp.val_type = TYPE_CSTRING;
		tmp.flags = FLAG_BLOB|FLAG_STRING|FLAG_CONST_CSTRING;
		tmp.nbr_cells = 1;
		tmp.val_str = addr;
		tmp.len_str = len;
		unify(q, mmap_var, mmap_ctx, &tmp, q->st.curr_frame);
	}
#endif

	cell *tmp = alloc_heap(q, 1);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p3, p3_ctx, tmp, q->st.curr_frame);
	return 1;
}

static int fn_iso_close_1(query *q)
{
	if (q->retry)
		return 0;

	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (str->p)
		destroy_parser(str->p);

	if (n <= 2)
		return 0;

	net_close(str);
	free(str->filename);
	free(str->mode);
	free(str->data);
	free(str->name);
	memset(str, 0, sizeof(stream));
	return 1;
}

static int fn_iso_at_end_of_stream_0(query *q)
{
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	return feof(str->fp) || ferror(str->fp);
}

static int fn_iso_at_end_of_stream_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	return feof(str->fp) || ferror(str->fp);
}

static int fn_iso_flush_output_0(query *q)
{
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	fflush(str->fp);
	return !ferror(str->fp);
}

static int fn_iso_flush_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	fflush(str->fp);
	return !ferror(str->fp);
}

static int fn_iso_nl_0(query *q)
{
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	fputc('\n', str->fp);
	return !ferror(str->fp);
}

static int fn_iso_nl_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	fputc('\n', str->fp);
	return !ferror(str->fp);
}

static void parse_read_params(query *q, cell *p, stream *str)
{
	if (!is_structure(p))
		return;

	if (!strcmp(GET_STR(p), "character_escapes")) {
		if (is_literal(p+1))
			q->character_escapes = !strcmp(GET_STR(p+1), "true");
	} else if (!strcmp(GET_STR(p), "double_quotes")) {
		if (is_literal(p+1)) {
			if (!strcmp(GET_STR(p+1), "atom")) {
				q->m->flag.double_quote_codes = q->m->flag.double_quote_chars = 0;
				q->m->flag.double_quote_atom = 1;
			} else if (!strcmp(GET_STR(p+1), "chars")) {
				q->m->flag.double_quote_atom = q->m->flag.double_quote_codes = 0;
				q->m->flag.double_quote_chars = 1;
			} else if (!strcmp(GET_STR(p+1), "codes")) {
				q->m->flag.double_quote_atom = q->m->flag.double_quote_chars = 0;
				q->m->flag.double_quote_codes = 1;
			}
		}
	}
}

static int do_read_term(query *q, stream *str, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, char *src)
{
	if (!str->p)
		str->p = create_parser(q->m);

	parser *p = str->p;
	p->start_term = 1;
	p->one_shot = 1;
	p->error = 0;
	int flag_chars = q->m->flag.double_quote_chars;
	int flag_codes = q->m->flag.double_quote_codes;
	int flag_atom = q->m->flag.double_quote_atom;

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref_var(q, h, p2_ctx);
		parse_read_params(q, c, str);
		p2 = LIST_TAIL(p2);
		p2 = deref_var(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (isatty(fileno(str->fp)) && !src) {
		printf("| ");
		fflush(str->fp);
	}

	if (!src) {
		if (net_getline(&p->save_line, &p->n_line, str) == -1) {

			if (q->is_task && !feof(str->fp)) {
				clearerr(str->fp);
				do_yield_0(q, 1);
				return 0;
			}

			destroy_parser(p);
			cell tmp;
			make_literal(&tmp, g_eof_s);
			return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		}

		if (p->save_line[strlen(p->save_line)-1] == '\n')
			p->save_line[strlen(p->save_line)-1] = '\0';

		if (p->save_line[strlen(p->save_line)-1] == '\r')
			p->save_line[strlen(p->save_line)-1] = '\0';

		p->srcptr = p->save_line;
	} else
		p->srcptr = src;

	int save = q->m->flag.character_escapes;
	q->m->flag.character_escapes = q->character_escapes;
	parser_tokenize(p, 0, 0);
	q->m->flag.character_escapes = save;

	if (p->error)
		return 0;

	if (!parser_attach(p, 0))
		return 0;

	parser_xref(p, p->t, NULL);
	q->m->flag.double_quote_chars = flag_chars;
	q->m->flag.double_quote_codes = flag_codes;
	q->m->flag.double_quote_atom = flag_atom;

	cell *tmp = alloc_heap(q, p->t->cidx-1);
	copy_cells(tmp, p->t->cells, p->t->cidx-1);
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static int fn_iso_read_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	return do_read_term(q, str, p1, p1_ctx, NULL, 0, NULL);
}

static int fn_iso_read_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	return do_read_term(q, str, p1, p1_ctx, NULL, 0, NULL);
}

static int fn_iso_read_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static int fn_iso_read_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static int fn_iso_write_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	write_term_to_stream(q, str, p1, 1, 0, 0);
	return !ferror(str->fp);
}

static int fn_iso_write_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	write_term_to_stream(q, str, p1, 1, 0, 0);
	return !ferror(str->fp);
}

static int fn_iso_writeq_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	int save = q->quoted;
	q->quoted = 1;
	write_term_to_stream(q, str, p1, 1, 0, 1);
	q->quoted = save;
	return !ferror(str->fp);
}

static int fn_iso_writeq_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	int save = q->quoted;
	q->quoted = 1;
	write_term_to_stream(q, str, p1, 1, 0, 1);
	q->quoted = save;
	return !ferror(str->fp);
}

static int fn_iso_write_canonical_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	write_canonical(q, str->fp, p1, 1, 0);
	return !ferror(str->fp);
}

static int fn_iso_write_canonical_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	write_canonical(q, str->fp, p1, 1, 0);
	return !ferror(str->fp);
}

static void parse_write_params(query *q, cell *p)
{	if (!is_literal(p))
		return;

	if (!is_structure(p))
		return;

	if (!strcmp(GET_STR(p), "max_depth")) {
		if (is_integer(p+1))
			q->max_depth = p[1].val_num;
	} else if (!strcmp(GET_STR(p), "fullstop")) {
		if (is_literal(p+1))
			q->fullstop = !strcmp(GET_STR(p+1), "true");
	} else if (!strcmp(GET_STR(p), "nl")) {
		if (is_literal(p+1))
			q->nl = !strcmp(GET_STR(p+1), "true");
	} else if (!strcmp(GET_STR(p), "quoted")) {
		if (is_literal(p+1))
			q->quoted = !strcmp(GET_STR(p+1), "true");
	} else if (!strcmp(GET_STR(p), "ignore_ops")) {
		if (is_literal(p+1))
			q->ignore_ops = !strcmp(GET_STR(p+1), "true");
	}
}

static int fn_iso_write_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref_var(q, h, p2_ctx);
		parse_write_params(q, c);
		p2 = LIST_TAIL(p2);
		p2 = deref_var(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->latest_ctx = p1_ctx;
	write_term_to_stream(q, str, p1, 1, 0, q->quoted?1:0);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl)
		net_write("\n", 1, str);

	q->max_depth = q->quoted = q->nl = q->fullstop = 0;
	q->ignore_ops = 0;
	return !ferror(str->fp);
}

static int fn_iso_write_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref_var(q, h, p2_ctx);
		parse_write_params(q, c);
		p2 = LIST_TAIL(p2);
		p2 = deref_var(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->latest_ctx = p1_ctx;
	write_term_to_stream(q, str, p1, 1, 0, q->quoted?1:0);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl)
		net_write("\n", 1, str);

	q->max_depth = q->quoted = q->nl = q->fullstop = 0;
	q->ignore_ops = 0;
	return !ferror(str->fp);
}

static int fn_iso_put_char_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	const char *src = GET_STR(p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_put_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,atom);
	const char *src = GET_STR(p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_put_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	int ch = (int)p1->val_num;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_put_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer);
	int ch = (int)p1->val_num;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_put_byte_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	int ch = (int)p1->val_num;

	if ((ch > 255) || (ch < 0)) {
		throw_error(q, p1, "type_error", "byte");
		return 0;
	}

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_put_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer);
	int ch = (int)p1->val_num;

	if ((ch > 255) || (ch < 0)) {
		throw_error(q, p1, "type_error", "byte");
		return 0;
	}

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static int fn_iso_get_char_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = 0;
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (ch == '\n')
		str->did_getc = 0;

	char tmpbuf[10];
	sprintf(tmpbuf, "%c", ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_get_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,atom_or_var);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = 0;
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (ch == '\n')
		str->did_getc = 0;

	char tmpbuf[10];
	sprintf(tmpbuf, "%c", ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_get_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_get_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_get_byte_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_get_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,atom_or_var);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->did_getc = 1;
	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_char_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	if (feof(str->fp)) {
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[10];
	sprintf(tmpbuf, "%c", ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	if (feof(str->fp)) {
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[10];
	sprintf(tmpbuf, "%c", ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_code_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_byte_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_peek_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return 0;
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static void do_calc(query *q, cell *c)
{
	cell *save = q->st.curr_cell;
	q->st.curr_cell = c;
	q->calc = 1;
	c->fn(q);
	q->calc = 0;
	q->st.curr_cell = save;
}

static int_t gcd(int_t num, int_t remainder)
{
	if (remainder == 0)
		return num;

	return gcd(remainder, num % remainder);
}

void do_reduce(cell *n)
{
	int_t r = 0;

	if (n->val_den > n->val_num)
		r = gcd(n->val_den, n->val_num);
	else if (n->val_den < n->val_num)
		r = gcd(n->val_num, n->val_den);
	else
		r = gcd(n->val_num, n->val_den);

	n->val_num /= r;
	n->val_den /= r;

	if (n->val_den < 0) {
		n->val_num = -n->val_num;
		n->val_den = -n->val_den;
	}
}

#define reduce(c) if ((c)->val_den != 1) do_reduce(c)
#define calc(q,c) !(c->flags&FLAG_BUILTIN) ? *c : (do_calc(q, c), q->accum)

static int fn_iso_is_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2_tmp,any);
	q->accum.val_den = 1;
	cell p2 = calc(q, p2_tmp);
	p2.nbr_cells = 1;

	if (q->error)
		return 0;

	if (is_variable(p1) && is_rational(&p2)) {
		reduce(&p2);
		set_var(q, p1, p1_ctx, &p2, q->st.curr_frame);
		return 1;
	}

	if (is_variable(p1) && is_number(&p2)) {
		set_var(q, p1, p1_ctx, &p2, q->st.curr_frame);
		return 1;
	}

	if (is_integer(p1) && is_integer(&p2))
		return (p1->val_num == p2.val_num);

	if (is_rational(p1) && is_rational(&p2)) {
		reduce(p1); reduce(&p2);
		return (p1->val_num == p2.val_num) && (p1->val_den == p2.val_den);
	}

	if (is_float(p1) && is_float(&p2))
		return p1->val_flt == p2.val_flt;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(p1), "nan"))
		return is_float(&p2)? isnan(p2.val_flt) : 0;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(p1), "inf"))
		return is_float(&p2) ? isinf(p2.val_flt) : 0;

	throw_error(q, p1, "type_error", "number");
	return 0;
}

static int fn_iso_float_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if is_float(&p1) {
			q->accum.val_flt = p1.val_flt;
			q->accum.val_type = TYPE_FLOAT;
			return 1;
		}

		if (is_integer(&p1)) {
			q->accum.val_flt = (double)p1.val_num;
			q->accum.val_type = TYPE_FLOAT;
			return 1;
		}

		throw_error(q, &p1, "type_error", "integer_or_float");
		return 0;
	}

	return is_float(p1_tmp);
}

static int fn_iso_integer_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if is_float(&p1) {
			q->accum.val_num = (int_t)p1.val_flt;
			q->accum.val_den = 1;
			q->accum.val_type = TYPE_INTEGER;
			return 1;
		}

		if (is_rational(&p1)) {
			q->accum.val_num = p1.val_num;
			q->accum.val_den = p1.val_den;
			q->accum.val_type = TYPE_INTEGER;
			return 1;
		}

		throw_error(q, &p1, "type_error", "integer_or_float");
		return 0;
	}

	return is_integer(p1_tmp);
}

static int fn_iso_abs_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_integer(&p1))
		q->accum.val_num = llabs((long long)p1.val_num);
	else if is_float(&p1)
		q->accum.val_flt = fabs(p1.val_flt);
	else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_sign_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_integer(&p1))
		q->accum.val_num = p1.val_num < 0 ? -1 : p1.val_num > 0  ? 1 : 0;
	else if is_float(&p1)
		q->accum.val_flt = p1.val_flt < 0 ? -1 : p1.val_flt > 0  ? 1 : 0;
	else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_positive_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum = p1;
	return 1;
}

static int fn_iso_negative_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_rational(&p1))
		q->accum.val_num = -p1.val_num;
	else if (is_float(&p1))
		q->accum.val_flt = -p1.val_flt;
	else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_pi_0(query *q)
{
	q->accum.val_flt = M_PI;
	q->accum.val_type = TYPE_FLOAT;
	return 1;
}

static int fn_iso_e_0(query *q)
{
	q->accum.val_flt = M_E;
	q->accum.val_type = TYPE_FLOAT;
	return 1;
}

static int fn_iso_add_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num + p2.val_num;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = (int64_t)p1.val_num + p2.val_num;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = p1.val_num + p2.val_num;
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num += p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_INTEGER;
	} else if (is_integer(&p1) && is_float(&p2)) {
		q->accum.val_flt = (double)p1.val_num + p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = p1.val_flt + p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = p1.val_flt + p2.val_num;
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_sub_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num - p2.val_num;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = (int64_t)p1.val_num - p2.val_num;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = p1.val_num - p2.val_num;
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num -= p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_INTEGER;
	} else if (is_integer(&p1) && is_float(&p2)) {
		q->accum.val_flt = (double)p1.val_num - p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = p1.val_flt - p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = p1.val_flt - p2.val_num;
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_mul_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if ((is_integer(&p1)) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num * p2.val_num;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = (int64_t)p1.val_num * p2.val_num;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = p1.val_num * p2.val_num;
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num *= p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_den *= p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_INTEGER;
	} else if (is_integer(&p1) && is_float(&p2)) {
		q->accum.val_flt = (double)p1.val_num * p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = p1.val_flt * p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = p1.val_flt * p2.val_num;
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_exp_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = exp((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = exp(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_sqrt_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = sqrt((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = sqrt(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_log_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = log((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = log(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_truncate_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = p1.val_flt;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = p1.val_flt;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = (int_t)p1.val_flt;
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_round_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = round(p1.val_flt);

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = round(p1.val_flt);

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = (int_t)round(p1.val_flt);
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_ceiling_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = ceil(p1.val_flt);

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = ceil(p1.val_flt);

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = (int_t)ceil(p1.val_flt);
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_float_integer_part_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = p1.val_flt;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = p1.val_flt;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_flt = (int_t)p1.val_flt;
			q->accum.val_type = TYPE_FLOAT;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_float_fractional_part_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = p1.val_flt - (__int64_t)p1.val_flt;

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = p1.val_flt - (int64_t)p1.val_flt;

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_flt = p1.val_flt - (int_t)p1.val_flt;
			q->accum.val_type = TYPE_FLOAT;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_floor_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_float(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = floor(p1.val_flt);

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = floor(p1.val_flt);

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = (int_t)floor(p1.val_flt);
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else {
		throw_error(q, &p1, "type_error", "float");
		return 0;
	}

	return 1;
}

static int fn_iso_sin_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = sin((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = sin(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_cos_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = cos((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = cos(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_tan_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = tan((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = tan(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_asin_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = asin((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = asin(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_acos_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = acos((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = acos(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_atan_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		q->accum.val_flt = atan((double)p1.val_num/p1.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = atan(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_atan_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_flt = atan2((double)p1.val_num/p1.val_den, (double)p2.val_num/p2.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_rational(&p1) && is_float(&p2)) {
		q->accum.val_flt = atan2((double)p1.val_num/p1.val_den, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = atan2(p1.val_flt, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = atan2(p1.val_flt, p2.val_num);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_copysign_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		q->accum = p1;

		if (p2.val_num < 0)
			q->accum.val_num = -llabs((long long)p1.val_num);

		q->accum.val_type = TYPE_INTEGER;
	} else if (is_rational(&p1) && is_float(&p2)) {
		q->accum = p1;

		if (p2.val_flt < 0.0)
			q->accum.val_num = -llabs((long long)p1.val_num);

		q->accum.val_type = TYPE_INTEGER;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = copysign(p1.val_flt, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_rational(&p2)) {
		q->accum.val_flt = copysign(p1.val_flt, p2.val_num);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_pow_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_flt = pow((double)p1.val_num/p1.val_den, (double)p2.val_num/p2.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_rational(&p1) && is_float(&p2)) {
		q->accum.val_flt = pow((double)p1.val_num/p1.val_den, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = pow(p1.val_flt, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = pow(p1.val_flt, p2.val_num);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_powi_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = pow(p1.val_num,p2.val_num);

		if ((tmp > INT64_MAX) || (tmp < INT64_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		int64_t tmp = pow(p1.val_num,p2.val_num);

		if ((tmp > INT32_MAX) || (tmp < INT32_MIN)) {
			throw_error(q, &p1, "domain_error", "integer_overflow");
			return 0;
		} else {
#endif
			q->accum.val_num = (int_t)pow(p1.val_num,p2.val_num);
			q->accum.val_type = TYPE_INTEGER;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_flt = pow((double)p1.val_num/p1.val_den, (double)p2.val_num/p2.val_den);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_rational(&p1) && is_float(&p2)) {
		q->accum.val_flt = pow((double)p1.val_num/p1.val_den, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = pow(p1.val_flt, p2.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = pow(p1.val_flt, p2.val_num);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_divide_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_flt = (double)p1.val_num / p2.val_num;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		q->accum.val_num = p1.val_num;
		q->accum.val_den = p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else if (is_integer(&p1) && is_float(&p2)) {
		q->accum.val_flt = (double)p1.val_num / p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_flt = p1.val_flt / p2.val_flt;
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1) && is_integer(&p2)) {
		q->accum.val_flt = p1.val_flt / p2.val_num;
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_iso_divint_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num / p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_div_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = (p1.val_num - llabs((long long)(p1.val_num % p2.val_num))) / p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_mod_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
			q->accum.val_num = llabs((long long)(p1.val_num % p2.val_num));
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_max_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num >= p2.val_num ? p1.val_num : p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_min_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num <= p2.val_num ? p1.val_num : p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_xor_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num ^ p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_and_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num & p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_or_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num | p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_shl_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num << p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_shr_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num >> p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int fn_iso_neg_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_integer(&p1)) {
		q->accum.val_num = ~p1.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, int depth)
{
	if (depth == MAX_DEPTH) {
		q->cycle_error = 1;
		return 0;
	}

	if (p1->arity == 0) {
		if (is_atom(p1) && is_atom(p2))
			return strcmp(GET_STR(p1), GET_STR(p2));

		if (is_rational(p1) && is_rational(p2)) {
			cell tmp1 = *p1, tmp2 = *p2;
			tmp1.val_num *= tmp2.val_den;
			tmp2.val_num *= tmp1.val_den;
			return tmp1.val_num < tmp2.val_num ? -1 : tmp1.val_num > tmp2.val_num ? 1 : 0;
		}

		if (is_float(p1) && is_float(p2))
			return p1->val_flt < p2->val_flt ? -1 : p1->val_flt > p2->val_flt ? 1 : 0;

		if (is_rational(p1) && is_float(p2))
			return rat_to_float(p1) < p2->val_flt ? -1 : rat_to_float(p1) > p2->val_flt ? 1 : 0;

		if (is_float(p1) && is_rational(p2))
			return rat_to_float(p2) < p1->val_flt ? -1 : rat_to_float(p2) > p1->val_flt ? 1 : 0;

		if (is_variable(p1) && !is_variable(p2))
			return -1;

		if (!is_variable(p1) && is_variable(p2))
			return 1;

		if (is_variable(p1) && is_variable(p2)) {
			frame *g1 = GET_FRAME(p1_ctx);
			frame *g2 = GET_FRAME(p2_ctx);
			idx_t p1_slot = GET_SLOT(g1,p1->slot_nbr) - q->slots;
			idx_t p2_slot = GET_SLOT(g2,p2->slot_nbr) - q->slots;
			return p1_slot < p2_slot ? -1 : p1_slot > p2_slot ? 1 : 0;
		}

		return 0;
	}

	while (is_list(p1) && is_list(p2)) {
		cell *h1 = LIST_HEAD(p1);
		h1 = deref_var(q, h1, p1_ctx);
		idx_t tmp1_ctx = q->latest_ctx;
		cell *h2 = LIST_HEAD(p2);
		h2 = deref_var(q, h2, p2_ctx);
		idx_t tmp2_ctx = q->latest_ctx;

		int val = compare(q, h1, tmp1_ctx, h2, tmp2_ctx, depth+1);
		if (val) return val;

		p1 = LIST_TAIL(p1);
		p1 = deref_var(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = LIST_TAIL(p2);
		p2 = deref_var(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (p1->arity < p2->arity)
		return -1;

	if (p1->arity > p2->arity)
		return 1;

	int val = strcmp(GET_STR(p1), GET_STR(p2));
	if (val) return val;

	int arity = p1->arity;
	p1 = p1 + 1;
	p2 = p2 + 1;

	while (arity--) {
		cell *h1 = deref_var(q, p1, p1_ctx);
		idx_t tmp1_ctx = q->latest_ctx;
		cell *h2 = deref_var(q, p2, p2_ctx);
		idx_t tmp2_ctx = q->latest_ctx;

		int val = compare(q, h1, tmp1_ctx, h2, tmp2_ctx, depth+1);
		if (val) return val;

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return 0;
}

static int fn_iso_seq_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) == 0;
}

static int fn_iso_sne_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) != 0;
}

static int fn_iso_slt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) < 0;
}

static int fn_iso_sle_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) <= 0;
}

static int fn_iso_sgt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) > 0;
}

static int fn_iso_sge_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return compare(q, p1, p1_ctx, p2, p2_ctx, 0) >= 0;
}

static int fn_iso_compare_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p2) || is_variable(p3)) {
		throw_error(q, p1, "type_error", "term");
		return 0;
	}

	int status = compare(q, p2, p2_ctx, p3, p3_ctx, 0);
	cell tmp;
	make_literal(&tmp, status<0?g_lt_s:status>0?g_gt_s:g_eq_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_iso_neq_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num == p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num == p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num == p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt == p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt == p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_nne_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num != p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num != p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num != p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt != p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt != p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_nge_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num >= p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num >= p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num >= p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt >= p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt >= p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_ngt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num > p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num > p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num > p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt > p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt > p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_nle_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num <= p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num <= p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num <= p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt <= p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt <= p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_nlt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num < p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num < p2.val_num;
	} else if (is_integer(&p1) && is_float(&p2))
		return p1.val_num < p2.val_flt;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_flt < p2.val_flt;
	else if (is_float(&p1) && is_integer(&p2))
		return p1.val_flt < p2.val_num;

	throw_error(q, &p1, "type_error", "number");
	return 0;
}

static int fn_iso_arg_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,structure);
	GET_NEXT_ARG(p3,any);

	if (is_integer(p1)) {
		int arg = p1->val_num;

		if (q->retry) {
			if (++arg > p2->arity)
				return 0;

			GET_RAW_ARG(1, p1_raw);
			GET_RAW_ARG(3, p3_raw);

			p1 = p1_raw; p1_ctx = p1_raw_ctx;
			p3 = p3_raw; p3_ctx = p3_raw_ctx;

			cell tmp;
			make_int(&tmp, arg);
			reset_value(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			make_choice(q);
		}

		if ((arg < 1) || (arg > p2->arity)) {
			throw_error(q, p1, "type_error", "out_of_range");
			return 0;
		}

		cell *c = p2 + 1;

		for (int i = 1; i <= arg; i++) {
			if (i == arg)
				return unify(q, p3, p3_ctx, c, p2_ctx);

			c += c->nbr_cells;
		}
	}

	if (is_variable(p1) && is_variable(p3)) {
		cell tmp;
		make_int(&tmp, 1);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		cell *c = p2 + 1;
		set_var(q, p3, p3_ctx, c, p2_ctx);
		make_choice(q);
		return 1;
	}

	return 0;
}

static int fn_iso_univ_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_var);

	if (!is_variable(p2)) {
		cell *head = LIST_HEAD(p2);
		cell *tail = LIST_TAIL(p2);
		head = deref_var(q, head, p2_ctx);

		if (!is_atom(head) && !is_number(head) && !is_variable(head)) {
			if (is_variable(p1))
				throw_error(q, head, "type_error", "atomic");

			return 0;
		}

		size_t nbr_cells = p2->nbr_cells;
		cell *tmp = malloc(sizeof(cell)*nbr_cells* 2);
		size_t idx = 0;
		tmp[idx++] = *head;

		while (tail) {
			tail = deref_var(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

			if (is_literal(tail)) {
				if (tail->val_off == g_nil_s)
					break;
			}

			if (!is_list(tail)) {
				if (is_variable(p1))
					throw_error(q, tail, "type_error", "list");

				return 0;
			}

			head = LIST_HEAD(tail);
			tail = LIST_TAIL(tail);

			if ((idx + head->nbr_cells) >= nbr_cells) {
				nbr_cells += head->nbr_cells;
				tmp = realloc(tmp, sizeof(cell)*(nbr_cells*=2));
			}

			idx += copy_cells(tmp+idx, head, head->nbr_cells);
			tmp[0].nbr_cells += head->nbr_cells;
			tmp[0].arity++;
		}

		cell *save = tmp;

		if (is_variable(p1)) {
			cell *h = LIST_HEAD(p2);
			cell *t = LIST_TAIL(p2);
			h = deref_var(q, h, p2_ctx);

			if (!is_iso_atom(h) && is_iso_list(t)) {
				throw_error(q, h, "type_error", "atom");
				return 0;
			}

			h = save;
			cell *tmp = alloc_heap(q, idx);
			copy_cells(tmp, save, idx);
			free(save);

			if (is_literal(tmp)) {
				tmp->fn = get_builtin(q->m, GET_STR(tmp), tmp->arity);

				if (tmp->fn)
					tmp->flags |= FLAG_BUILTIN;
				else
					tmp->match = find_rule(q->m, tmp);
			}

			set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
			return 1;
		}

		free(tmp);
		cell tmp2 = *p1;
		tmp2.nbr_cells = 1;
		tmp2.arity = 0;
		tmp2.flags = 0;
		alloc_list(q, &tmp2);
		cell *c = p1 + 1;
		unsigned arity = p1->arity;

		while (arity--) {
			append_list(q, c);
			c += c->nbr_cells;
		}

		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		int ok = unify(q, l, p1_ctx, p2, p2_ctx);
		return ok;
	} else if (is_variable(p2) && is_list(p1)) {
		idx_t nbr_cells = p1->nbr_cells;
		cell *h = LIST_HEAD(p1);
		cell *t = LIST_TAIL(p1);
		t->nbr_cells = nbr_cells - h->nbr_cells - 1;	// TODO: fix list parsing
		cell tmp;
		make_literal(&tmp, g_dot_s);
		alloc_list(q, &tmp);
		append_list(q, h);
		append_list(q, t);
		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
		return 1;
	} else if (is_variable(p2)) {
		idx_t nbr_cells = p1->nbr_cells;
		cell *c = p1;
		cell tmp = *c++;
		tmp.nbr_cells = 1;
		tmp.arity = 0;
		alloc_list(q, &tmp);
		nbr_cells -= tmp.nbr_cells;
		unsigned arity = p1->arity;

		while (arity--) {
			append_list(q, c);
			c += c->nbr_cells;
		}

		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
		return 1;
	}

	return 0;
}

static void do_collect_vars(query *q, cell *p1, idx_t p1_ctx, idx_t nbr_cells, cell **slots, int *cnt)
{
	for (int i = 0; i < nbr_cells; i++, p1++) {
		cell *c = deref_var(q, p1, p1_ctx);

		if (is_structure(c)) {
			do_collect_vars(q, c+1, q->latest_ctx, c->nbr_cells-1, slots, cnt);
		} else if (is_variable(c)) {
			if (!slots[*cnt]) {
				slots[*cnt] = c;
				(*cnt)++;
			}
		}
	}
}

static int fn_iso_term_variables_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	cell *slots[MAX_ARITY] = {0};
	int cnt = 0;

	if (is_structure(p1))
		do_collect_vars(q, p1+1, p1_ctx, p1->nbr_cells-1, slots, &cnt);
	else
		do_collect_vars(q, p1, p1_ctx, p1->nbr_cells, slots, &cnt);

	cell *tmp = calloc((cnt*2)+1, sizeof(cell));
	int idx = 0;

	if (cnt) {
		int done = 0;

		for (unsigned i = 0; i < cnt; i++) {
			if (!slots[i])
				continue;

			make_literal(tmp+idx++, g_dot_s);
			tmp[idx-1].arity = 2;
			tmp[idx-1].nbr_cells = ((cnt-done)*2)+1;
			tmp[idx++] = *slots[i];
			done++;
		}

		make_literal(tmp+idx++, g_nil_s);
		tmp[0].arity = 2;
		tmp[0].nbr_cells = idx;
	} else {
		make_literal(tmp+idx++, g_nil_s);
	}

	if (is_variable(p2)) {
		cell *save = tmp;
		tmp = alloc_heap(q, idx);
		copy_cells(tmp, save, idx);
		free(save);
		set_var(q, p2, p2_ctx, tmp, q->st.curr_frame);
		return 1;
	}

	int ok = unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	free(tmp);
	return ok;
}

static cell *clone_to_heap2(query *q, int prefix, cell *p1, idx_t nbr_cells, idx_t suffix)
{
	cell *tmp = alloc_heap(q, (prefix?1:0)+nbr_cells+suffix);

	if (prefix) {
		// Needed for follow() to work
		tmp->val_type = TYPE_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	copy_cells(tmp+(prefix?1:0), p1, nbr_cells);
	cell *c = tmp + (prefix?1:0);

	for (idx_t i = 0; i < nbr_cells; i++, c++) {
		if (is_blob(c))
			c->flags |= FLAG_CONST_CSTRING;
	}

	return tmp;
}

cell *clone_to_heap(query *q, int prefix, cell *p1, idx_t suffix)
{
	return clone_to_heap2(q, prefix, p1, p1->nbr_cells, suffix);
}

static cell *copy_to_heap(query *q, cell *p1, idx_t suffix)
{
	cell *tmp = alloc_heap(q, p1->nbr_cells+suffix);
	cell *src = p1, *dst = tmp;
	frame *g = GET_FRAME(q->st.curr_frame);
	unsigned new_varno = g->nbr_vars;
	unsigned slots[MAX_ARITY] = {0};

	for (idx_t i = 0; i < p1->nbr_cells; i++, dst++, src++) {
		*dst = *src;

		if (is_blob(src))
			dst->flags |= FLAG_CONST_CSTRING;

		if (!is_variable(src))
			continue;

		if (slots[dst->slot_nbr] == 0)
			slots[dst->slot_nbr] = new_varno++;

		dst->slot_nbr = slots[dst->slot_nbr];
	}

	if (new_varno != g->nbr_vars) {
		if (!create_vars(q, new_varno-g->nbr_vars)) {
			throw_error(q, p1, "resource_error", "too_many_vars");
			return NULL;
		}
	}

	return tmp;
}

static int fn_iso_copy_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	cell *tmp1 = deep_clone_to_tmp(q, p1, p1_ctx);
	cell *tmp = copy_to_heap(q, tmp1, 0);
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static int fn_iso_clause_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,any);

	if (!do_match(q, p1))
		return 0;

	term *t = &q->st.curr_clause->t;
	cell *body = get_body(t->cells);

	if (body)
		return unify(q, p2, p2_ctx, body, q->st.curr_frame);

	cell tmp;
	make_literal(&tmp, g_true_s);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

typedef unsigned long long ullong;

static void compare_and_zero(ullong v1, ullong *v2, ullong *v)
{
	if (v1 != *v2) {
		*v2 = v1;
		*v = 0;
	}
}

#define MASK_FINAL 0x0000FFFFFFFFFFFF // Final 48 bits

static void uuid_gen(uuid *u)
{
	static ullong s_last = 0, s_cnt = 0;
	static ullong g_seed = 0;

	if (!g_seed)
		g_seed = (ullong)time(0) & MASK_FINAL;

	ullong now = get_time_in_usec();
	compare_and_zero(now, &s_last, &s_cnt);
	u->u1 = now;
	u->u2 = s_cnt++;
	u->u2 <<= 48;
	u->u2 |= g_seed;
}

static char *uuid_to_buf(const uuid *u, char *buf, size_t buflen)
{
	snprintf(buf, buflen, "%016llX-%04llX-%012llX",
		(ullong)u->u1,
		(ullong)(u->u2 >> 48),
		(ullong)(u->u2 & MASK_FINAL));

	return buf;
}

static int uuid_from_buf(const char *s, uuid *u)
{
	if (!s) {
		uuid tmp = {0};
		*u = tmp;
		return 0;
	}

	unsigned long long p1 = 0, p2 = 0, p3 = 0;

	if (sscanf(s, "%llX%*c%llX%*c%llX", &p1, &p2, &p3) != 3) {
		uuid tmp = {0};
		*u = tmp;
		return 0;
	}

	u->u1 = p1;
	u->u2 = p2 << 48;
	u->u2 |= p3 & MASK_FINAL;
	return 1;
}

enum log_type { LOG_ASSERTA=1, LOG_ASSERTZ=2, LOG_ERASE=3 };

static void db_log(query *q, clause *r, enum log_type l)
{
	int save = q->quoted;
	char tmpbuf[256];
	q->quoted = 2;

	switch(l) {
		case LOG_ASSERTA: {
			size_t len = write_term_to_buf(q, NULL, 0, r->t.cells, 1, 0, 0);
			char *dst = malloc(len+1);
			write_term_to_buf(q, dst, len+1, r->t.cells, 1, 0, 0);
			uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
			fprintf(q->m->fp, "a_(%s,'%s').\n", dst, tmpbuf);
			free(dst);
			break;
		} case LOG_ASSERTZ: {
			size_t len = write_term_to_buf(q, NULL, 0, r->t.cells, 1, 0, 0);
			char *dst = malloc(len+1);
			write_term_to_buf(q, dst, len+1, r->t.cells, 1, 0, 0);
			uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
			fprintf(q->m->fp, "z_(%s,'%s').\n", dst, tmpbuf);
			free(dst);
			break;
		} case LOG_ERASE: {
			uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
			fprintf(q->m->fp, "e_('%s').\n", tmpbuf);
			break;
		}
	}

	q->quoted = save;
}

static int fn_iso_retract_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (!do_match(q, p1))
		return 0;

	clause *r = retract_from_db(q->m, q->st.curr_clause);
	if (!r) return 0;

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ERASE);

	return 1;
}

static int fn_iso_retractall_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	rule *h = find_rule(q->m, p1);

	if (!h) {
		//set_dynamic_in_db(q->m, GET_STR(p1), p1->arity);
		return 1;
	}

	while (fn_iso_retract_1(q))
		retry_choice(q);

	return 1;
}

static int do_abolish(query *q, cell *c)
{
	rule *h = find_rule(q->m, c);
	if (!h) return 1;

	if (!h->is_dynamic) {
		fprintf(stderr, "Error: not dynamic '%s/%u'\n", GET_STR(c), c->arity);
		return 0;
	}

	for (clause *r = h->head; r;) {
		if (!q->m->loading && r->t.is_persist && !r->t.is_deleted)
			db_log(q, r, LOG_ERASE);

		clause *save = r->next;
		clear_term(&r->t);
		free(r);
		r = save;
	}

	h->is_abolished = 1;
	sl_destroy(h->index);
	h->index = NULL;
	h->head = h->tail = NULL;
	h->cnt = 0;
	return 1;
}

static int fn_iso_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (p1->arity != 2) {
		throw_error(q, p1, "type_error", "indicator");
		return 0;
	}

	const char *src = GET_STR(p1);

	if (strcmp(src, "/")) {
		throw_error(q, p1, "type_error", "indicator");
		return 0;
	}

	cell *p1_name = p1 + 1;

	if (!is_atom(p1_name)) {
		throw_error(q, p1_name, "type_error", "atom");
		return 0;
	}

	cell *p1_arity = p1 + 2;

	if (!is_integer(p1_arity)) {
		throw_error(q, p1_arity, "type_error", "integer");
		return 0;
	}

	cell tmp;
	tmp = *p1_name;
	tmp.arity = p1_arity->val_num;
	return do_abolish(q, &tmp);
}

static int fn_iso_asserta_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = copy_cells(p->t->cells, tmp, nbr_cells);
	parser_assign_vars(p);
	clause *r = asserta_to_db(q->m, p->t, 0);
	if (!r) return 0;
	uuid_gen(&r->u);

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ASSERTA);

	return 1;
}

static int fn_iso_assertz_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = copy_cells(p->t->cells, tmp, nbr_cells);
	parser_assign_vars(p);
	clause *r = assertz_to_db(q->m, p->t, 0);
	if (!r) return 0;
	uuid_gen(&r->u);

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ASSERTZ);

	return 1;
}

int call_me(query *q, cell *p1)
{
	if (q->retry)
		return 0;

	p1 = deref_var(q, p1, q->st.curr_frame);
	idx_t p1_ctx = q->latest_ctx;

	if (!is_callable(p1)) {
		throw_error(q, p1, "type_error", "callable");
		return 0;
	}

	cell *tmp;

	if (p1_ctx != q->st.curr_frame) {
		tmp = copy_to_heap(q, p1, 1);
		unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	} else
		tmp = clone_to_heap(q, 0, p1, 1);

	idx_t nbr_cells = tmp->nbr_cells;
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	make_barrier(q);
	return 1;
}

static int fn_iso_call_n(query *q)
{
	GET_FIRST_ARG(p1,callable);
	init_tmp_heap(q);
	cell *tmp2 = alloc_tmp_heap(q, p1->nbr_cells);
	copy_cells(tmp2, p1, p1->nbr_cells);
	idx_t nbr_cells = p1->nbr_cells;
	unsigned arity = p1->arity;
	unsigned args = 2;

	while (args++ <= q->st.curr_cell->arity) {
		cell *p2 = get_next_raw_arg(q);
		cell *tmp2 = alloc_tmp_heap(q, p2->nbr_cells);
		copy_cells(tmp2, p2, p2->nbr_cells);
		nbr_cells += p2->nbr_cells;
		arity++;
	}

	tmp2 = get_tmp_heap(q, 0);
	cell *tmp = clone_to_heap2(q, 1, tmp2, nbr_cells, 1);
	tmp[1].nbr_cells = nbr_cells;
	tmp[1].arity = arity;

	if ((tmp[1].fn = get_builtin(q->m, GET_STR(tmp+1), arity)) != NULL)
		tmp[1].flags |= FLAG_BUILTIN;
	else {
		tmp[1].match = find_rule(q->m, tmp+1);
		tmp[1].flags &= ~FLAG_BUILTIN;
	}

	make_end_return(tmp+1+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_iso_ifthen_2(query *q)
{
	if (q->retry)
		return 0;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, 1, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	nbr_cells += copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_barrier(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int do_ifthenelse(query *q, cell *p1, cell *p2, cell *p3)
{
	if (q->retry) {
		cell *tmp = clone_to_heap(q, 1, p3, 1);
		idx_t nbr_cells = 1 + p3->nbr_cells;
		make_end_return(tmp+nbr_cells, q->st.curr_cell);
		q->st.curr_cell = tmp;
		return 1;
	}

	cell *tmp = clone_to_heap(q, 1, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	nbr_cells += copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_barrier(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_iso_disjunction_2(query *q)
{
	if ((q->st.curr_cell+1)->fn == fn_iso_ifthen_2) {
		cell *p1 = q->st.curr_cell + 2;
		cell *p2 = p1 + p1->nbr_cells;
		cell *p3 = p2 + p2->nbr_cells;
		return do_ifthenelse(q, p1, p2, p3);
	}

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);

	if (q->retry) {
		cell *tmp = clone_to_heap(q, 1, p2, 1);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_end_return(tmp+nbr_cells, q->st.curr_cell);
		q->st.curr_cell = tmp;
		return 1;
	}

	cell *tmp = clone_to_heap(q, 1, p1, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_choice(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_iso_negation_1(query *q)
{
	if (q->retry)
		return 1;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, 1, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
	make_barrier(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_iso_once_1(query *q)
{
	if (q->retry)
		return 0;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, 1, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_barrier(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_ignore_1(query *q)
{
	if (q->retry)
		return 1;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, 1, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_barrier(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_iso_catch_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,callable);

	if (q->retry && q->exception)
		return unify(q, p2, p2_ctx, q->exception, q->st.curr_frame);

	if (q->retry == 2) {
		q->retry = 0;
		cell *tmp = clone_to_heap(q, 1, p3, 1);
		idx_t nbr_cells = 1 + p3->nbr_cells;
		make_end_return(tmp+nbr_cells, q->st.curr_cell);
		make_catcher(q, 2);
		q->st.curr_cell = tmp;
		return 1;
	}

	if (q->retry)
		return 0;

	cell *tmp = clone_to_heap(q, 1, p1, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	make_catcher(q, 1);
	q->st.curr_cell = tmp;
	return 1;
}

static int do_throw_term(query *q, cell *c)
{
	q->exception = c;

	while (retry_choice(q)) {
		choice *ch = q->choices + q->cp;

		if (!ch->catchme1)
			continue;

		q->retry = 2;

		if (!fn_iso_catch_3(q))
			continue;

		q->exception = NULL;
		return 1;
	}

	fprintf(stdout, "Error: uncaught exception... ");
	write_term(q, stdout, c, 1, 0, 0);
	fprintf(stdout, "\n");
	q->m->dump_vars = 1;
	q->exception = NULL;
	q->error = 1;
	return 0;
}

static int fn_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	q->latest_ctx = q->st.curr_frame;

	if (check_has_vars(q, c)) {
		throw_error(q, c, "instantiation_error", "instantiated");
		return 0;
	}

	if (!do_throw_term(q, c))
		return 0;

	return fn_iso_catch_3(q);
}

static int fn_iso_functor_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1)) {
		if (!is_atom(p2)){
			throw_error(q, p2, "type_error", "atom");
			return 0;
		}

		if (!is_integer(p3)){
			throw_error(q, p3, "type_error", "integer");
			return 0;
		}

		if ((p3->val_num <= 0) || (p3->val_num > MAX_ARITY/2)){
			throw_error(q, p3, "domain_error", "integer");
			return 0;
		}

		unsigned arity = p3->val_num;
		unsigned slot_nbr;

		if (!(slot_nbr = create_vars(q, arity))) {
			throw_error(q, p3, "resource_error", "too_many_vars");
			return 0;
		}

		cell *tmp = alloc_heap(q, 1+arity);
		tmp[0].val_type = TYPE_LITERAL;
		tmp[0].arity = arity;
		tmp[0].nbr_cells = 1 + arity;
		tmp[0].val_off = p2->val_off;

		for (unsigned i = 1; i <= arity; i++) {
			tmp[i].val_type = TYPE_VARIABLE;
			tmp[i].nbr_cells = 1;
			tmp[i].slot_nbr = slot_nbr++;
			tmp[i].val_off = g_anon_s;
		}

		set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
		return 1;
	}

	cell tmp = {0};
	tmp.val_type = TYPE_LITERAL;
	tmp.val_off = p1->val_off;
	tmp.nbr_cells = 1;

	if (is_string(p2))
		tmp.val_off = g_dot_s;

	if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return 0;

	make_int(&tmp, p1->arity);

	if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
		return 0;

	return 1;
}

static int fn_iso_current_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);

	if (!strcmp(GET_STR(p1), "double_quotes")) {
		cell tmp;

		if (q->m->flag.double_quote_atom)
			make_literal(&tmp, find_in_pool("atom"));
		else if (q->m->flag.double_quote_codes)
			make_literal(&tmp, find_in_pool("codes"));
		else if (q->m->flag.double_quote_chars)
			make_literal(&tmp, find_in_pool("chars"));

		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "character_escapes")) {
		cell tmp;

		if (q->m->flag.character_escapes)
			make_literal(&tmp, g_true_s);
		else
			make_literal(&tmp, g_false_s);

		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "prefer_rationals")) {
		cell tmp;

		if (q->m->flag.prefer_rationals)
			make_literal(&tmp, g_true_s);
		else
			make_literal(&tmp, g_false_s);

		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "rational_syntax")) {
		cell tmp;

		if (q->m->flag.rational_syntax_natural)
			make_literal(&tmp, find_in_pool("natural"));
		else
			make_literal(&tmp, find_in_pool("compatibility"));

		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "version_git")) {
		cell tmp;
		make_literal(&tmp, find_in_pool(VERSION));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "dialect")) {
		cell tmp;
		make_literal(&tmp, find_in_pool("trealla"));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "bounded")) {
		cell tmp;
		make_literal(&tmp, g_false_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "cpu_count")) {
		cell tmp;
		make_int(&tmp, q->m->cpu_count);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "version")) {
		unsigned v1 = 0;
		sscanf(VERSION, "v%u", &v1);
		cell tmp;
		make_int(&tmp, v1);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "version_data")) {
		unsigned v1 = 0, v2 = 0, v3 = 0;
		sscanf(VERSION, "v%u.%u.%u", &v1, &v2, &v3);
		cell *tmp = alloc_heap(q, 5);
		make_literal(&tmp[0], find_in_pool("trealla"));
		make_int(&tmp[1], v1);
		make_int(&tmp[2], v2);
		make_int(&tmp[3], v3);
		make_literal(&tmp[4], g_nil_s);
		tmp[0].arity = 4;
		tmp[0].nbr_cells = 5;
		set_var(q, p2, p2_ctx, tmp, q->st.curr_frame);
		return 1;
	} else if (!strcmp(GET_STR(p1), "argv")) {
		if (g_avc == g_ac) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
			return 1;
		}

		int i = g_avc;
		cell tmp = tmp_cstring(q, g_av[i++]);
		alloc_list(q, &tmp);

		while (i < g_ac) {
			tmp = tmp_cstring(q, g_av[i++]);
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
		return 1;
	}

	throw_error(q, p1, "domain_error", "flag");
	return 0;
}

static int fn_iso_set_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_atom(p1)) {
		throw_error(q, p1, "type_error", "atom");
		return 0;
	}

	if (!strcmp(GET_STR(p1), "cpu_count") && is_integer(p2)) {
		q->m->cpu_count = p2->val_num;
		return 1;
	}

	if (!is_atom(p2)) {
		throw_error(q, p2, "type_error", "atom");
		return 0;
	}

	if (!strcmp(GET_STR(p1), "double_quotes")) {
		if (!strcmp(GET_STR(p2), "atom")) {
			q->m->flag.double_quote_atom = 1;
			q->m->flag.double_quote_chars = q->m->flag.double_quote_codes = 0;
		} else if (!strcmp(GET_STR(p2), "codes")) {
			q->m->flag.double_quote_codes = 1;
			q->m->flag.double_quote_chars = q->m->flag.double_quote_atom = 0;
		} else if (!strcmp(GET_STR(p2), "chars")) {
			q->m->flag.double_quote_chars = 1;
			q->m->flag.double_quote_atom = q->m->flag.double_quote_codes = 0;
		} else {
		throw_error(q, p2, "domain_error", "unknown");
			return 0;
		}
	} else if (!strcmp(GET_STR(p1), "character_escapes")) {
		if (!strcmp(GET_STR(p2), "true"))
			q->m->flag.character_escapes = 1;
		else if (!strcmp(GET_STR(p2), "false"))
			q->m->flag.character_escapes = 0;
	} else if (!strcmp(GET_STR(p1), "rational_syntax")) {
		if (!strcmp(GET_STR(p2), "natural"))
			q->m->flag.rational_syntax_natural = 1;
		else if (!strcmp(GET_STR(p2), "compatibility"))
			q->m->flag.rational_syntax_natural = 0;
	} else if (!strcmp(GET_STR(p1), "prefer_rationals")) {
		if (!strcmp(GET_STR(p2), "true"))
			q->m->flag.prefer_rationals = 1;
		else if (!strcmp(GET_STR(p2), "flase"))
			q->m->flag.prefer_rationals = 0;
	} else {
		throw_error(q, p1, "domain_error", "flag");
		return 0;
	}

	return 1;
}

#ifdef __FreeBSD__
static int nodecmp(void *thunk, const void *ptr1, const void *ptr2)
#else
static int nodecmp(const void *ptr1, const void *ptr2, void *thunk)
#endif
{
	const cell *p1 = *(const cell**)ptr1;
	const cell *p2 = *(const cell**)ptr2;
	int keysort = (int)(long)thunk;

	if (is_rational(p1)) {
		if (is_rational(p2)) {
			cell tmp1 = *p1, tmp2 = *p2;
			tmp1.val_num *= tmp2.val_den;
			tmp2.val_num *= tmp1.val_den;
			if (tmp1.val_num < tmp2.val_num)
				return -1;
			else if (tmp1.val_num > tmp2.val_num)
				return 1;
			else
				return 0;
		} else if (is_float(p2)) {
			if (((double)p1->val_num/p1->val_den) < p2->val_flt)
				return -1;
			else if (((double)p1->val_num/p1->val_den) > p2->val_flt)
				return 1;
			else
				return 0;
		} else if (is_atom(p2))
			return -1;
		else if (is_variable(p2))
			return 1;
	}
	else if (is_float(p1)) {
		if (is_rational(p2)) {
			if (p1->val_flt < ((double)p2->val_num/p2->val_den))
				return -1;
			else if (p1->val_flt > ((double)p2->val_num/p2->val_den))
				return 1;
			else
				return 0;
		} else if (is_float(p2)) {
			if (p1->val_flt < p2->val_flt)
				return -1;
			else if (p1->val_flt > p2->val_flt)
				return 1;
			else
				return 0;
		} else if (is_atom(p2))
			return -1;
		else if (is_variable(p2))
			return 1;
	} else if (is_atom(p1)) {
		if (is_atom(p2))
			return strcmp(GET_STR(p1), GET_STR(p2));
		else if (is_structure(p2))
			return -1;
		else
			return 1;
	} else if (is_variable(p1)) {
		if (is_variable(p2))
			return p1->slot_nbr < p2->slot_nbr ? -1 : p1->slot_nbr > p2->slot_nbr ? 1 : 0;
		else
			return -1;
	} else if (is_structure(p1)) {
		if (is_structure(p2)) {
			if (p1->arity < p2->arity)
				return -1;

			if (p1->arity > p2->arity)
				return 1;

			int i = strcmp(GET_STR(p1), GET_STR(p2));

			if (i != 0)
				return i;

			int arity = !keysort ? p1->arity : 1;
			p1++; p2++;

			while (arity--) {
#ifdef __FreeBSD__
				int i = nodecmp(NULL, &p1, &p2);
#else
				int i = nodecmp(&p1, &p2, NULL);
#endif

				if (i != 0)
					return i;

				p1 += p1->nbr_cells;
				p2 += p2->nbr_cells;
			}

			return 0;
		} else
			return 1;
	} else
		return 0;

	return 0;
}

static cell *nodesort(query *q, cell *p1, idx_t p1_ctx, int dedup, int keysort)
{
	cell *p = deep_clone_to_tmp(q, p1, p1_ctx);
	idx_t save_size = q->tmph_size;
	size_t cnt = 0;
	cell *l = p;

	while (is_list(l)) {
		LIST_HEAD(l);
		l = LIST_TAIL(l);
		cnt++;
	}

	cell **base = malloc(sizeof(cell*)*cnt);
	size_t idx = 0;
	l = p;

	while (is_list(l)) {
		cell *h = LIST_HEAD(l);
		base[idx++] = h;
		l = LIST_TAIL(l);
	}

#ifdef __FreeBSD__
	qsort_r(base, cnt, sizeof(cell*), (void*)(long)keysort, nodecmp);
#else
	qsort_r(base, cnt, sizeof(cell*), nodecmp, (void*)(long)keysort);
#endif

	q->tmp_heap = NULL;

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
#ifdef __FreeBSD__
			if (dedup && !nodecmp((void*)(long)keysort, &base[i], &base[i-1]))
#else
			if (dedup && !nodecmp(&base[i], &base[i-1], (void*)(long)keysort))
#endif
				continue;
		}

		if (i == 0)
			alloc_list(q, base[i]);
		else
			append_list(q, base[i]);
	}

	l = end_list(q);
	free(base);
	free(q->tmp_heap);
	q->tmp_heap = p;
	q->tmph_size = save_size;
	return l;
}

static int fn_iso_sort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	cell *l = nodesort(q, p1, p1_ctx, 1, 0);
	return unify(q, l, p1_ctx, p2, p2_ctx);
}

static int fn_iso_keysort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	cell *l = nodesort(q, p1, p1_ctx, 0, 1);
	return unify(q, l, p1_ctx, p2, p2_ctx);
}

static cell *convert_to_list(query *q, cell *c, idx_t nbr_cells)
{
	if (!nbr_cells || !c->nbr_cells) {
		cell *c = alloc_tmp_heap(q, 1);
		make_literal(c, g_nil_s);
		return c;
	}

	alloc_list(q, c);
	nbr_cells -= c->nbr_cells;
	c += c->nbr_cells;

	while (nbr_cells > 0) {
		append_list(q, c);
		nbr_cells -= c->nbr_cells;
		c += c->nbr_cells;
	}

	return end_list(q);
}

static void do_sys_listn(query *q, cell *p1, idx_t p1_ctx)
{
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	fix_list(l, l->nbr_cells);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	init_queuen(q);
}

static void do_sys_listn2(query *q, cell *p1, idx_t p1_ctx, cell *tail)
{
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	l->nbr_cells--;	// drop []
	l[l->nbr_cells++] = *tail;
	fix_list(l, l->nbr_cells);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	init_queuen(q);
}

static int fn_sys_list_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell *l = convert_to_list(q, get_queue(q), queue_used(q));
	fix_list(l, l->nbr_cells);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	init_queue(q);
	return 1;
}

static int fn_sys_queue_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	cell *c = tmp;

	for (idx_t i = 0; i < tmp->nbr_cells; i++, c++) {
		if (is_variable(c))
			c->val_type = TYPE_EMPTY;
	}

	alloc_queue(q, tmp);
	return 1;
}

static int fn_sys_queuen_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	cell *tmp = deep_clone_to_tmp(q, p2, p2_ctx);
	cell *c = tmp;

	for (idx_t i = 0; i < tmp->nbr_cells; i++, c++) {
		if (is_variable(c))
			c->val_type = TYPE_EMPTY;
	}

	alloc_queuen(q, p1->val_num, tmp);
	return 1;
}

static int fn_iso_findall_3(query *q)
{
	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,callable);
	GET_NEXT_ARG(p3,any);

	if (!q->retry) {
		q->st.qnbr++;
		cell *tmp = clone_to_heap(q, 1, p2, 3+p1->nbr_cells);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
		q->tmpq[q->st.qnbr] = NULL;
		init_queuen(q);
		make_barrier(q);
		q->st.curr_cell = tmp;
		return 1;
	}

	do_sys_listn(q, p3, p3_ctx);
	q->st.qnbr--;
	return 1;
}

static int fn_findall_4(query *q)
{
	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,callable);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,variable);

	if (!q->retry) {
		q->st.qnbr++;
		cell *tmp = clone_to_heap(q, 1, p2, 3+p1->nbr_cells);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
		q->tmpq[q->st.qnbr] = NULL;
		init_queuen(q);
		make_barrier(q);
		q->st.curr_cell = tmp;
		return 1;
	}

	do_sys_listn2(q, p3, p3_ctx, p4);
	q->st.qnbr--;
	return 1;
}

static int do_collect_vars2(query *q, cell *p1, idx_t nbr_cells, cell **slots)
{
	int cnt = 0;

	for (idx_t i = 0; i < nbr_cells; i++, p1++) {
		if (is_variable(p1)) {
			if (!slots[cnt]) {
				slots[cnt] = p1;
				cnt++;
			}
		}
	}

	return cnt;
}

static uint32_t get_vars(query *q, cell *p, idx_t p_ctx)
{
	cell *slots[MAX_ARITY] = {0};
	int cnt = do_collect_vars2(q, p, p->nbr_cells, slots);
	uint32_t mask = 0;

	for (unsigned i = 0; i < cnt; i++)
		mask |= 1 << slots[i]->slot_nbr;

	return mask;
}

static cell *skip_existentials(const query *q, cell *p2, uint32_t *xs)
{
	while (is_structure(p2) && !strcmp(GET_STR(p2), "^")) {
		cell *c = p2 + 1;

		if (is_variable(c))
			*xs |= 1 << c->slot_nbr;

		p2 += 1 + c->nbr_cells;
		return skip_existentials(q, p2, xs);
	}

	return p2;
}

static int fn_iso_bagof_3(query *q)
{
	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,callable);
	GET_NEXT_ARG(p3,any);
	uint32_t xs_vars = 0;
	p2 = skip_existentials(q, p2, &xs_vars);

	// First time thru generate all solutions

	if (!q->retry) {
		q->st.qnbr++;
		cell *tmp = clone_to_heap(q, 1, p2, 3+p2->nbr_cells);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p2->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
		init_queuen(q);
		make_barrier(q);
		q->st.curr_cell = tmp;
		return 1;
	}

	if (!queuen_used(q) && !q->tmpq[q->st.qnbr]) {
		q->st.qnbr--;
		return 0;
	}

	// Take a copy

	if (!q->tmpq[q->st.qnbr]) {
		idx_t nbr_cells = queuen_used(q);
		q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
		copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
		q->tmpq_size[q->st.qnbr] = nbr_cells;
	}

	init_queuen(q);
	make_choice(q);
	uint32_t p1_vars = get_vars(q, p1, p1_ctx);
	uint32_t p2_vars = get_vars(q, p2, p2_ctx);
	uint32_t mask = (p1_vars^p2_vars) & ~xs_vars;
	pin_vars(q, mask);
	cell *c_end = q->tmpq[q->st.qnbr] + q->tmpq_size[q->st.qnbr];

	for (cell *c = q->tmpq[q->st.qnbr]; c < c_end; c += c->nbr_cells) {
		if (c->flags & FLAG_DELETED)
			continue;

		if (unify(q, p2, p2_ctx, c, q->st.curr_frame)) {
			c->flags |= FLAG_DELETED;
			cell *c1 = deep_clone_to_tmp(q, p1, q->st.curr_frame);
			alloc_queuen(q, q->st.qnbr, c1);
		}

		undo_me(q);
	}

	if (!queuen_used(q)) {
		drop_choice(q);
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		q->st.qnbr--;
		return 0;
	}

	unpin_vars(q);
	do_sys_listn(q, p3, p3_ctx);
	return 1;
}

static int fn_iso_setof_3(query *q)
{
	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,callable);
	GET_NEXT_ARG(p3,any);
	uint32_t xs_vars = 0;
	p2 = skip_existentials(q, p2, &xs_vars);

	// First time thru generate all solutions

	if (!q->retry) {
		q->st.qnbr++;
		cell *tmp = clone_to_heap(q, 1, p2, 3+p2->nbr_cells);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p2->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
		init_queuen(q);
		make_barrier(q);
		q->st.curr_cell = tmp;
		return 1;
	}

	if (!queuen_used(q) && !q->tmpq[q->st.qnbr]) {
		q->st.qnbr--;
		return 0;
	}

	// Take a copy

	if (!q->tmpq[q->st.qnbr]) {
		idx_t nbr_cells = queuen_used(q);
		q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
		copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
		q->tmpq_size[q->st.qnbr] = nbr_cells;
	}

	init_queuen(q);
	make_choice(q);
	uint32_t p1_vars = get_vars(q, p1, p1_ctx);
	uint32_t p2_vars = get_vars(q, p2, p2_ctx);
	uint32_t mask = (p1_vars^p2_vars) & ~xs_vars;
	pin_vars(q, mask);
	cell *c_end = q->tmpq[q->st.qnbr] + q->tmpq_size[q->st.qnbr];

	for (cell *c = q->tmpq[q->st.qnbr]; c < c_end; c += c->nbr_cells) {
		if (c->flags & FLAG_DELETED)
			continue;

		if (unify(q, p2, p2_ctx, c, q->st.curr_frame)) {
			c->flags |= FLAG_DELETED;
			cell *c1 = deep_clone_to_tmp(q, p1, q->st.curr_frame);
			alloc_queuen(q, q->st.qnbr, c1);
		}

		undo_me(q);
	}

	if (!queuen_used(q)) {
		drop_choice(q);
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		q->st.qnbr--;
		return 0;
	}

	unpin_vars(q);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	l = nodesort(q, l, q->st.curr_frame, 1, 0);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static int fn_iso_op_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);

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
		throw_error(q, p2, "type_error", "create");
		return 0;
	}

	int tmp_userop = 0;
	unsigned tmp_optype = 0;

	int ok = get_op(q->m, GET_STR(p3), &tmp_optype, &tmp_userop, 0);

	if (ok && !tmp_userop) {
		throw_error(q, p3, "permission_error", "create");
		return 0;
	}

	if (!set_op(q->m, GET_STR(p3), optype, p1->val_num)) {
		throw_error(q, p3, "domain_error", "create");
		return 0;
	}

	return 1;
}

static int fn_erase_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	uuid u;
	uuid_from_buf(GET_STR(p1), &u);
	clause *r = erase_from_db(q->m, &u);
	if (!r) return 0;

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ERASE);

	return 1;
}

static int fn_instance_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	uuid u;
	uuid_from_buf(GET_STR(p1), &u);
	clause *r = find_in_db(q->m, &u);
	if (!r) return 0;
	return unify(q, p2, p2_ctx, r->t.cells, q->st.curr_frame);
}

static int fn_clause_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,atom_or_var);
	term *t;

	if (!is_variable(p3)) {
		uuid u;
		uuid_from_buf(GET_STR(p3), &u);
		clause *r = find_in_db(q->m, &u);
		if (!r) return 0;
		t = &r->t;
	} else {
		if (!do_match(q, p1))
			return 0;

		char tmpbuf[128];
		uuid_to_buf(&q->st.curr_clause->u, tmpbuf, sizeof(tmpbuf));
		cell tmp = make_cstring(q, tmpbuf);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		t = &q->st.curr_clause->t;
	}

	cell *body = get_body(t->cells);

	if (body)
		return unify(q, p2, p2_ctx, body, q->st.curr_frame);

	cell tmp;
	make_literal(&tmp, g_true_s);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int do_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = copy_cells(p->t->cells, tmp, nbr_cells);
	parser_assign_vars(p);
	clause *r = asserta_to_db(q->m, p->t, 0);
	if (!r) return 0;

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(p2), &u);
		r->u = u;
	} else {
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2 = make_cstring(q, tmpbuf);
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	}

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ASSERTA);

	return 1;
}

static int fn_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,variable);
	return do_asserta_2(q);
}

static int fn_sys_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_asserta_2(q);
}

static int do_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = copy_cells(p->t->cells, tmp, nbr_cells);
	parser_assign_vars(p);
	clause *r = assertz_to_db(q->m, p->t, 0);
	if (!r) return 0;

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(p2), &u);
		r->u = u;
	} else {
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2 = make_cstring(q, tmpbuf);
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	}

	if (!q->m->loading && r->t.is_persist)
		db_log(q, r, LOG_ASSERTZ);

	return 1;
}

static int fn_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,variable);
	return do_assertz_2(q);
}

static int fn_sys_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_assertz_2(q);
}

static void save_db(FILE *fp, query *q, int logging)
{
	int save = q->quoted;
	q->quoted = 1;

	for (rule *h = q->m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		if (logging && !h->is_persist)
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.is_deleted)
				continue;

			if (logging)
				fprintf(fp, "z_(");

			write_term(q, fp, r->t.cells, 0, 0, 0);

			if (logging) {
				char tmpbuf[256];
				uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
				fprintf(fp, ",'%s')", tmpbuf);
			}

			fprintf(fp, ".\n");
		}
	}

	q->quoted = save;
}

static int fn_listing_0(query *q)
{
	save_db(stdout, q, 0);
	return 1;
}

static void save_name(FILE *fp, query *q, idx_t name, unsigned arity)
{
	module *m = q->st.curr_clause ? q->st.curr_clause->m : q->m;

	for (rule *h = m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		if (name != h->val_off)
			continue;

		if ((arity != h->arity) && (arity != -1))
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.is_deleted)
				continue;

			write_term(q, fp, r->t.cells, 0, 0, 0);
			fprintf(fp, ".\n");
		}
	}
}

static int fn_listing_1(query *q)
{
	GET_FIRST_ARG(p1,literal);
	idx_t name = p1->val_off;
	unsigned arity = -1;

	if (p1->arity) {
		cell *p2 = p1 + 1;

		if (!is_literal(p2)) {
			throw_error(q, p2, "type_error", "atom");
			q->error = 1;
			return 0;
		}

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3)) {
			throw_error(q, p3, "type_error", "integer");
			q->error = 1;
			return 0;
		}

		name = p2->val_off;
		arity = p3->val_num;
	}

	save_name(stdout, q, name, arity);
	return 1;
}

static int fn_sys_timer_0(query *q)
{
	q->time_started = get_time_in_usec();
	return 1;
}
static int fn_sys_elapsed_0(query *q)
{
	unsigned long long elapsed = get_time_in_usec();
	elapsed -= q->time_started;
	fprintf(stderr, "Time elapsed %.03g secs\n", (double)elapsed/1000/1000);
	return 1;
}

static int fn_time_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	fn_sys_timer_0(q);
	cell *tmp = clone_to_heap(q, 1, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_elapsed_s, fn_sys_elapsed_0, 0, 0);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_statistics_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_var);

	if (!strcmp(GET_STR(p1), "cputime") && is_variable(p2)) {
		unsigned long long now = get_time_in_usec();
		double elapsed = now - q->time_started;
		cell tmp;
		make_float(&tmp, elapsed/1000/1000);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	if (!strcmp(GET_STR(p1), "gctime") && is_variable(p2)) {
		cell tmp;
		make_float(&tmp, 0);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	if (!strcmp(GET_STR(p1), "runtime")) {
		unsigned long long now = get_time_in_usec();
		double elapsed = now - q->time_started;
		cell tmp;
		make_int(&tmp, elapsed/1000);
		alloc_list(q, &tmp);
		append_list(q, &tmp);
		make_literal(&tmp, g_nil_s);
		cell *l = end_list(q);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return 0;
}

static int fn_sleep_1(query *q)
{
	if (q->retry)
		return 1;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task) {
		do_yield_0(q, p1->val_num*1000);
		return 0;
	}

	sleep((unsigned)p1->val_num);
	return 1;
}

static int fn_delay_1(query *q)
{
	if (q->retry)
		return 1;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task) {
		do_yield_0(q, p1->val_num);
		return 0;
	}

	msleep((unsigned)p1->val_num);
	return 1;
}

static int fn_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int_t elapse = p1->val_num;

	if (elapse < 0)
		return 1;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return 1;

	int_t started = get_time_in_usec() / 1000;
	int_t end = started + elapse;

	while ((get_time_in_usec() / 1000) < end)
		;

	return 1;
}

static int fn_now_0(query *q)
{
	int_t secs = get_time_in_usec() / 1000 / 1000;
	q->accum.val_type = TYPE_INTEGER;
	q->accum.val_num = secs;
	return 1;
}

static int fn_now_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	int_t secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	double v = ((double)get_time_in_usec()) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_writeln_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];
	write_term_to_stream(q, str, p1, 1, 0, 0);
	fputc('\n', str->fp);
	return !ferror(str->fp);
}

static int fn_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);

	if (!q->retry && !is_variable(p3)) {
		if (p3->val_num > p2->val_num)
			return 0;

		if (p3->val_num < p1->val_num)
			return 0;

		return 1;
	} else if (!q->retry && !is_variable(p3)) {
		throw_error(q, p3, "type_error", "variable");
		return 0;
	}

	if (p1->val_num > p2->val_num)
		return 0;

	if (!q->retry) {
		set_var(q, p3, p3_ctx, p1, q->st.curr_frame);

		if (p1->val_num != p2->val_num)
			make_choice(q);

		return 1;
	}

	int_t val = p3->val_num;

	if (val == p2->val_num)
		return 0;

	val++;
	GET_RAW_ARG(3,p3_raw);
	cell tmp;
	make_int(&tmp, val);
	reset_value(q, p3_raw, p3_raw_ctx, &tmp, q->st.curr_frame);

	if (val != p2->val_num)
		make_choice(q);

	return 1;
}

static int fn_forall_2(query *q)
{
	if (q->retry)
		return 1;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, 1, p1, 0);
	clone_to_heap(q, 0, p2, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells + p2->nbr_cells;
	make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
	make_choice(q);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_split_atom_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,any);
	const char *src = GET_STR(p1);
	int ch = peek_char_utf8(GET_STR(p2));
	int pad = peek_char_utf8(GET_STR(p3));
	const char *start = src, *ptr;
	cell *l = NULL;
	int nbr = 1, in_list = 0;

	if (!*start) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	while ((ptr = strchr_utf8(start, ch)) != NULL) {
		while ((peek_char_utf8(start) == pad) && (pad != ch))
			get_char_utf8(&start);

		cell tmp = tmp_cstringn(q, start, ptr-start);

		if (nbr++ == 1)
			alloc_list(q, &tmp);
		else
			append_list(q, &tmp);

		start = ptr + 1;
		in_list = 1;
	}

	if (*start) {
		while (peek_char_utf8(start) == pad)
			get_char_utf8(&start);

		cell tmp = tmp_cstring(q, start);

		if (!in_list)
			alloc_list(q, &tmp);
		else
			append_list(q, &tmp);
	}

	l = end_list(q);
	fix_list(l, l->nbr_cells);
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

static int fn_split_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_nil(p1)) {
		throw_error(q, p1, "type_error", "atom");
		return 0;
	}

	const char *start = GET_STR(p1), *ptr;
	int ch = peek_char_utf8(GET_STR(p2));

	if ((ptr = strchr_utf8(start, ch)) != NULL) {
		cell tmp = make_string(q, start, ptr-start);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return 0;

		ptr = ptr+1;

		while (isspace(*ptr))
			ptr++;

		tmp = make_string(q, ptr, LEN_STR(p1)-(ptr-start));
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	if (!unify(q, p3, p3_ctx, p1, p1_ctx))
		return 0;

	cell tmp;
	make_literal(&tmp, g_empty_s);
	return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
}

static int fn_savefile_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	char *filename = strdup(GET_STR(p1));
	FILE *fp = fopen(filename, "wb");
	fwrite(GET_STR(p2), 1, LEN_STR(p2), fp);
	fclose(fp);
	free(filename);
	return 1;
}

static int fn_loadfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	char *filename = strdup(GET_STR(p1));
	FILE *fp = fopen(filename, "rb");

	if (!fp) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		free(filename);
		return 0;
	}

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return 0;
	}

	char *s = malloc(st.st_size+1);

	if (fread(s, 1, st.st_size, fp) != st.st_size) {
		throw_error(q, p1, "domain_error", "cannot_read");
		free(filename);
		return 0;
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp = make_string(q, s, st.st_size);
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	free(s);
	free(filename);
	return 1;
}

static int fn_getfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	char *filename = strdup(GET_STR(p1));
	FILE *fp = fopen(filename, "r");

	if (!fp) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		free(filename);
		return 0;
	}

	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while (getline(&line, &len, fp) != -1) {
		size_t len = strlen(line);
		if (line[len-1] == '\n') {
			line[len-1] = '\0';
			len--;
		}

		if (line[len-1] == '\r') {
			line[len-1] = '\0';
			len--;
		}

		cell tmp = tmp_cstringn(q, line, len);
		tmp.flags |= FLAG_STRING;

		if (nbr++ == 1)
			alloc_list(q, &tmp);
		else
			append_list(q, &tmp);

		in_list = 1;
	}

	free(line);
	fclose(fp);
	free(filename);

	if (!in_list) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return 1;
}

static void parse_host(const char *src, char *hostname, char *path, unsigned *port, int *ssl)
{
	if (!strncmp(src, "https://", 8)) {
		src += 8;
		*ssl = 1;
		*port = 443;
	} else if (!strncmp(src, "http://", 7)) {
		src += 7;
		*ssl = 0;
		*port = 80;
	}

	if (*src == ':')
		sscanf(src, ":%u/%4095s", port, path);
	else
		sscanf(src, "%1023[^:/]:%u/%4095s", hostname, port, path);

	hostname[1023] = '\0';
	path[4095] = '\0';
}

static int fn_server_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,list_or_nil);
	char hostname[1024], path[4096];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, level = 0;
	unsigned port = 80;
	strcpy(hostname, "localhost");
	path[0] = '\0';

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref_var(q, h, p3_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!strcmp(GET_STR(c), "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "keyfile")) {
				c = c + 1;

				if (is_atom(c))
					keyfile = GET_STR(c);
			} else if (!strcmp(GET_STR(c), "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = GET_STR(c);
			} else if (!strcmp(GET_STR(c), "hostname")) {
				c = c + 1;

				if (is_atom(c)) {
					strncpy(hostname, GET_STR(c), sizeof(hostname));
					hostname[sizeof(hostname)-1] = '\0';
				}
			} else if (!strcmp(GET_STR(c), "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !strcmp(GET_STR(c), "https") ? 1 : 0;
					port = 443;
				}
			} else if (!strcmp(GET_STR(c), "port")) {
				c = c + 1;

				if (is_integer(c))
					port = c->val_num;
			} else if (!strcmp(GET_STR(c), "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)c->val_num;
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref_var(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(p1);
	parse_host(url, hostname, path, &port, &ssl);
	nonblock = q->is_task;

	int fd = net_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1) {
		throw_error(q, p1, "existence_error", "server_failed");
		return 0;
	}

	int n = new_stream(q);

	if (n < 0) {
		throw_error(q, p1, "resource_error", "too_many_open_streams");
		close(fd);
		return 0;
	}

	stream *str = &g_streams[n];
	str->filename = strdup(GET_STR(p1));
	str->name = strdup(hostname);
	str->mode = strdup("update");
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->fp = fdopen(fd, "r+");
	str->ssl = ssl;
	str->level = level;
	str->sslptr = NULL;

	if (str->fp == NULL) {
		throw_error(q, p1, "existence_error", "cannot_open_stream");
		close(fd);
	}

	net_set_nonblocking(str);
	cell *tmp = alloc_heap(q, 1);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p2, p2_ctx, tmp, q->st.curr_frame);
	return 1;
}

static int fn_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	int fd = net_accept(str);

	if (fd == -1) {
		if (q->is_task) {
			do_yield_0(q, 10);
			return 0;
		}

		printf("*** here\n");
		return 0;
	}

	n = new_stream(q);

	if (n < 0) {
		throw_error(q, p1, "resource_error", "too_many_open_streams");
		close(fd);
		return 0;
	}

	stream *str2 = &g_streams[n];
	str2->filename = strdup(str->filename);
	str2->name = strdup(str->name);
	str2->mode = strdup("update");
	str2->nodelay = str->nodelay;
	str2->nonblock = str->nonblock;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		throw_error(q, p1, "existence_error", "cannot_open_stream");
		close(fd);
		return 0;
	}

	if (str->ssl) {
		str2->sslptr = net_enable_ssl(fd, str->name, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			return 0;
		}
	}

	net_set_nonblocking(str2);
	make_choice(q);
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_client_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,variable);
	GET_NEXT_ARG(p5,list_or_nil);
	char hostname[1024], path[4096];
	char *certfile = NULL;
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, level = 0;
	hostname[0] = path[0] = '\0';
	unsigned port = 80;

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref_var(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!strcmp(GET_STR(c), "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !strcmp(GET_STR(c), "true") ? 1 : 0;
			} else if (!strcmp(GET_STR(c), "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = GET_STR(c);
			} else if (!strcmp(GET_STR(c), "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !strcmp(GET_STR(c), "https") ? 1 : 0;
					port = 443;
				}
			} else if (!strcmp(GET_STR(c), "port")) {
				c = c + 1;

				if (is_integer(c))
					port = (int)c->val_num;
			} else if (!strcmp(GET_STR(c), "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)c->val_num;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref_var(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(p1);
	parse_host(url, hostname, path, &port, &ssl);
	nonblock = q->is_task;

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref_var(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!strcmp(GET_STR(c), "host")) {
				c = c + 1;

				if (is_atom(c))
					;//udp = !strcmp(GET_STR(c), "true") ? 1 : 0;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref_var(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	int fd = net_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return 0;

	int n = new_stream(q);

	if (n < 0) {
		throw_error(q, p1, "resource_error", "too_many_open_streams");
		close(fd);
		return 0;
	}

	stream *str = &g_streams[n];
	str->filename = strdup(GET_STR(p1));
	str->name = strdup(hostname);
	str->mode = strdup("update");
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r+");

	if (str->fp == NULL) {
		throw_error(q, p1, "existence_error", "cannot_open_stream");
		close(fd);
		return 0;
	}

	if (ssl) {
		str->sslptr = net_enable_ssl(fd, hostname, 0, str->level, certfile);

		if (!str->sslptr) {
			close(fd);
			return 0;
		}
	}

	if (nonblock)
		net_set_nonblocking(str);

	cell tmp = make_cstring(q, hostname);
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	tmp = make_cstring(q, path);
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	cell *tmp2 = alloc_heap(q, 1);
	make_int(tmp2, n);
	tmp2->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p4, p4_ctx, tmp2, q->st.curr_frame);
	return 1;
}

static int fn_getline_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		printf("| ");
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		perror("getline");
		free(line);
		return 0;
	}

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp = make_cstring(q, line);
	tmp.flags |= FLAG_STRING;
	free(line);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_getline_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		printf("| ");
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp)) {
			clearerr(str->fp);
			do_yield_0(q, 1);
			return 0;
		}

		return 0;
	}

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp = make_cstring(q, line);
	tmp.flags |= FLAG_STRING;
	free(line);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_bread_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,variable);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	size_t len;

	if (is_integer(p1) && (p1->val_num > 0)) {
		if (!str->data) {
			str->data = malloc(p1->val_num+1);
			str->data_len = 0;
		}

		for (;;) {
			len = p1->val_num - str->data_len;
			size_t nbytes = net_read(str->data+str->data_len, len, str);
			str->data_len += nbytes;
			str->data[str->data_len] = '\0';

			if (nbytes == len)
				break;

			if (feof(str->fp)) {
				free(str->data);
				str->data = NULL;
				return 0;
			}

			if (q->is_task) {
				clearerr(str->fp);
				do_yield_0(q, 1);
				return 0;
			}
		}

		cell tmp = make_string(q, str->data, str->data_len);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		free(str->data);
		str->data = NULL;
		return 1;
	}

	if (is_integer(p1)) {
		if (!str->data) {
			str->data = malloc((str->alloc_nbytes=1024*1)+1);
			str->data_len = 0;
		}

		size_t nbytes = net_read(str->data, str->alloc_nbytes, str);
		str->data[nbytes] = '\0';
		str->data = realloc(str->data, nbytes+1);
		cell tmp = make_string(q, str->data, nbytes);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		free(str->data);
		str->data = NULL;
		return 1;
	}

	if (!str->data) {
		str->data = malloc((str->alloc_nbytes=1024*1)+1);
		str->data_len = 0;
	}

	for (;;) {
		size_t len = str->alloc_nbytes - str->data_len;
		size_t nbytes = net_read(str->data+str->data_len, len, str);
		str->data_len += nbytes;
		str->data[str->data_len] = '\0';

		if (!nbytes || feof(str->fp))
			break;

		if (str->alloc_nbytes == str->data_len)
			str->data = realloc(str->data, (str->alloc_nbytes*=2)+1);
	}

	cell tmp1;
	make_int(&tmp1, str->data_len);
	set_var(q, p1, p1_ctx, &tmp1, q->st.curr_frame);
	cell tmp2 = make_string(q, str->data, str->data_len);
	set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	free(str->data);
	str->data = NULL;
	return 1;
}

static int fn_bwrite_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,atom);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	const char *src = GET_STR(p1);
	size_t len = LEN_STR(p1);

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (!nbytes) {
			if (feof(str->fp) || ferror(str->fp))
				return 0;
		}

		// TODO: make this yieldable

		clearerr(str->fp);
		len -= nbytes;
		src += nbytes;
	}

	return 1;
}

static int fn_read_term_from_atom_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];
	const char *p = GET_STR(p1);
	char *src = malloc(strlen(p)+10);
	sprintf(src, "%s", p);

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	int ok = do_read_term(q, str, p2, p2_ctx, p3, p3_ctx, src);
	free(src);
	return ok;
}

static int fn_term_to_atom_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_number(p1)) {
		char tmpbuf[256], *dst = tmpbuf;
		write_term_to_buf(q, dst, sizeof(tmpbuf), p1, 1, 0, 0);
		cell tmp = make_cstring(q, dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	size_t len = write_term_to_buf(q, NULL, 0, p1, 1, 0, 0);
	char *dst = malloc(len+1);
	write_term_to_buf(q, dst, len+1, p1, 1, 0, 0);
	idx_t offset;

	if (is_in_pool(dst, &offset)) {
		cell tmp;
		make_literal(&tmp, offset);
		free(dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else {
		cell tmp = make_cstring(q, dst);
		free(dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}
}

static int fn_write_term_to_atom_3(query *q)
{
	GET_FIRST_ARG(p2,any);
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p3,any);

	if (is_number(p1)) {
		char tmpbuf[256], *dst = tmpbuf;
		write_term_to_buf(q, dst, sizeof(tmpbuf), p1, 1, 0, 0);
		cell tmp = make_cstring(q, dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	size_t len = write_term_to_buf(q, NULL, 0, p1, 1, 0, 0);
	char *dst = malloc(len+1);
	write_term_to_buf(q, dst, len+1, p1, 1, 0, 0);
	idx_t offset;

	if (is_in_pool(dst, &offset)) {
		cell tmp;
		make_literal(&tmp, offset);
		free(dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else {
		cell tmp = make_cstring(q, dst);
		free(dst);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}
}

static int fn_is_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_list(p1) || is_nil(p1);
}

static int fn_is_stream_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_stream(p1);
}

static void push_task(module *m, query *task)
{
	task->next = m->tasks;

	if (m->tasks)
		m->tasks->prev = task;

	m->tasks = task;
}

static query *pop_task(module *m, query *task)
{
	if (task->prev)
		task->prev->next = task->next;

	if (task->next)
		task->next->prev = task->prev;

	if (task == m->tasks)
		m->tasks = task->next;

	return task->next;
}

static int fn_wait_0(query *q)
{
	while (!g_tpl_interrupt && q->m->tasks) {
		int_t now = get_time_in_usec() / 1000;
		query *task = q->m->tasks;
		unsigned did_something = 0, spawn_cnt = 0;

		while (!g_tpl_interrupt && task) {
			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= q->m->cpu_count)
					break;
			}

			if (task->tmo_msecs) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !task->st.curr_cell) {
				query *save = task;
				task = pop_task(q->m, task);
				destroy_query(save);
				continue;
			}

			run_query(task);
			task = task->next;
			did_something = 1;
		}

		if (!did_something)
			msleep(1);
	}

	return 1;
}

static int fn_await_0(query *q)
{
	while (!g_tpl_interrupt && q->m->tasks) {
		int_t now = get_time_in_usec() / 1000;
		query *task = q->m->tasks;
		unsigned did_something = 0, spawn_cnt = 0;

		while (!g_tpl_interrupt && task) {
			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= q->m->cpu_count)
					break;
			}

			if (task->tmo_msecs) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !q->st.curr_cell) {
				query *save = task;
				task = pop_task(q->m, task);
				destroy_query(save);
				continue;
			}

			run_query(task);

			if (!task->tmo_msecs && task->yielded) {
				did_something = 1;
				break;
			}
		}

		if (!did_something)
			msleep(1);
		else
			break;
	}

	if (!q->m->tasks)
		return 0;

	make_choice(q);
	return 1;
}

static int fn_yield_0(query *q)
{
	if (q->retry)
		return 1;

	return do_yield_0(q, 0);
}

static int fn_spawn_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	query *task = create_task(q, tmp);
	task->yielded = 1;
	task->spawned = 1;
	push_task(q->m, task);
	return 1;
}

static int fn_spawn_n(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp = alloc_heap(q, p1->nbr_cells);
	idx_t n = copy_cells(tmp, p1, p1->nbr_cells);
	unsigned arity = p1->arity;
	int args = 2;

	while (args++ <= q->st.curr_cell->arity) {
		GET_NEXT_ARG(p2,any);
		cell *tmp2 = alloc_heap(q, p2->nbr_cells);
		n += copy_cells(tmp2, p2, p2->nbr_cells);
		cell *c = tmp2;

		for (idx_t i = 0; i < p2->nbr_cells; i++, c++) {
			if (is_blob(c) && !is_const_cstring(c))
				c->val_str = strdup(c->val_str);
		}

		arity++;
	}

	tmp->nbr_cells = n;
	tmp->arity = arity;

	if ((tmp->fn = get_builtin(q->m, GET_STR(p1), arity)) != NULL)
		tmp->flags |= FLAG_BUILTIN;
	else {
		tmp->match = find_rule(q->m, tmp);
		tmp->flags &= ~FLAG_BUILTIN;
	}

	query *task = create_task(q, tmp);
	task->yielded = 1;
	task->spawned = 1;
	push_task(q->m, task);
	return 1;
}

static int fn_fork_0(query *q)
{
	cell *curr_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;
	query *task = create_task(q, curr_cell);
	task->yielded = 1;
	push_task(q->m, task);
	return 0;
}

static int fn_send_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	query *dstq = q->parent ? q->parent : q;
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);

	for (idx_t i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;

		if (is_blob(c2)) {
			size_t nbytes = c2->len_str;
			char *tmp = malloc(nbytes + 1);
			memcpy(tmp, c2->val_str, nbytes+1);
			c2->val_str = tmp;
		}
	}

	alloc_queue(dstq, c);
	q->yielded = 1;
	return 1;
}

static int fn_recv_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell *c = pop_queue(q);
	return unify(q, p1, p1_ctx, c, q->st.curr_frame);
}

static int fn_log10_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_integer(&p1)) {
		q->accum.val_flt = log10(p1.val_num);
		q->accum.val_type = TYPE_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_flt = log10(p1.val_flt);
		q->accum.val_type = TYPE_FLOAT;
	} else {
		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return 1;
}

static int fn_srandom_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	srandom(p1->val_num);
	return 1;
}

static int fn_random_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);

	if (is_variable(p1)) {
		cell tmp;
		make_float(&tmp, ((double)random())/UINT32_MAX);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	if (p1->val_num < 1) {
		throw_error(q, p1, "domain_error", "positive_integer");
		return 0;
	}

	q->accum.val_type = TYPE_INTEGER;
	q->accum.val_num = llabs((long long)(random()%p1->val_num));
	return 1;
}

static int fn_rand_0(query *q)
{
	q->accum.val_type = TYPE_INTEGER;
	q->accum.val_num = random()%RAND_MAX;
	return 1;
}

static int fn_rand_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, random()%RAND_MAX);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_msort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	cell *l = nodesort(q, p1, p1_ctx, 0, 0);
	return unify(q, l, p1_ctx, p2, p2_ctx);
}

static int do_consult(query *q, cell *p1, idx_t p1_ctx)
{
	if (is_atom(p1)) {
		const char *src = GET_STR(p1);
		deconsult(src);

		if (!module_load_file(q->m, src)) {
			throw_error(q, p1, "existence_error", "cannot_open_file");
			return 0;
		}

		return 1;
	}

	if (strcmp(GET_STR(p1), ":")) {
		throw_error(q, p1, "type_error", "not_a_file_spec");
		return 0;
	}

	cell *mod = deref_var(q, p1+1, p1_ctx);
	cell *file = deref_var(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file)) {
		throw_error(q, p1, "type_error", "not_a_file_spec");
		return 0;
	}

	module *tmp_m = create_module(GET_STR(mod));
	const char *src = GET_STR(file);
	deconsult(src);
	tmp_m->make_public = 1;

	if (!module_load_file(tmp_m, src)) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		destroy_module(tmp_m);
		return 0;
	}

	return 1;
}

static int fn_consult_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);

	if (!is_list(p1)) {
		if (!do_consult(q, p1, p1_ctx))
			return 0;

		return 1;
	}

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref_var(q, h, p1_ctx);

		if (!do_consult(q, c, q->latest_ctx))
			return 0;

		p1 = LIST_TAIL(p1);
		p1 = deref_var(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return 1;
}

static int format_integer(char *dst, int_t v, int grouping, int sep, int decimals)
{
	char tmpbuf1[256], tmpbuf2[256];
	sprint_int(tmpbuf1, sizeof(tmpbuf1), v, 10);
	const char *src = tmpbuf1 + (strlen(tmpbuf1) - 1);
	char *dst2 = tmpbuf2;
	int i = 1, j = 1;

	while (src >= tmpbuf1) {
		*dst2++ = *src--;

		if (grouping && !decimals && !(i++%grouping) && *src)
			*dst2++ = sep;

		if (decimals && (j++ == decimals)) {
			*dst2++ = '.';
			decimals = 0;
			i = 1;
		}
	}

	*dst2 = '\0';
	src = tmpbuf2 + (strlen(tmpbuf2) - 1);
	dst2 = dst;

	while (src >= tmpbuf2)
		*dst2++ = *src--;

	*dst2 = '\0';
	return dst2 - dst;
}

static int do_format(query *q, cell *str, idx_t str_ctx, cell* p1, idx_t p1_ctx, cell* p2, idx_t p2_ctx)
{
	char *srcbuf = GET_STR(p1);
	const char *src = srcbuf;
	size_t bufsiz;
	char *tmpbuf = malloc(bufsiz=strlen(src)+100);
	char *dst = tmpbuf;
	cell *c = NULL;
	size_t nbytes = bufsiz;

	while (*src) {
		int ch = get_char_utf8(&src);
		int argval = 0, noargval = 1;

		if (ch != '~') {
			dst += put_char_bare_utf8(dst, ch);
			continue;
		}

		ch = get_char_utf8(&src);

		while (isdigit(ch)) {
			noargval = 0;
			argval *= 10;
			argval += ch - '0';
			ch = get_char_utf8(&src);
			continue;
		}

		if (ch == 'N') {
			if ((dst != tmpbuf) && (dst[-1] == '\n'))
				continue;

			*dst++ = '\n';
			continue;
		}

		if (ch == 'n') {
			*dst++ = '\n';
			continue;
		}

		if (ch == '~') {
			*dst++ = '~';
			continue;
		}

		if (ch == 't')
			continue;

		if (ch == '|') {
			while ((dst - tmpbuf) < argval)
				*dst++ = ' ';

			continue;
		}

		if (!p2 || !is_list(p2))
			break;

		cell *head = LIST_HEAD(p2);
		c = deref_var(q, head, p2_ctx);
		p2 = LIST_TAIL(p2);

		if (ch == 'i')
			continue;

		int canonical = 0;
		size_t len;

		if (ch == 'k')
			canonical = 1;

		if ((ch == 'a') && !is_atom(c)) {
			free(tmpbuf);
			throw_error(q, c, "type_error", "atom");
			return 0;
		}

		if (((ch == 'd') || (ch == 'D')) && !is_integer(c)) {
			free(tmpbuf);
			throw_error(q, c, "type_error", "integer");
			return 0;
		}

		if (ch == 'c') {
			if (!is_integer(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "integer");
				return 0;
			}

			len = 10;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = put_char_utf8(dst, (int)c->val_num);
		} else if ((ch == 'e') || (ch == 'E')) {
			if (!is_float(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "float");
				return 0;
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			if (ch == 'e')
				len = sprintf(dst, "%e", c->val_flt);
			else
				len = sprintf(dst, "%E", c->val_flt);
		} else if (ch == 'f') {
			if (!is_float(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "float");
				return 0;
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = sprintf(dst, "%.*f", argval, c->val_flt);
		} else if (ch == 'I') {
			if (!is_integer(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "integer");
				return 0;
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = format_integer(dst, c->val_num, noargval?3:argval, '_', 0);
		} else if (ch == 'd') {
			if (!is_integer(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "integer");
				return 0;
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = format_integer(dst, c->val_num, 0, ',', noargval?2:argval);
		} else if (ch == 'D') {
			if (!is_integer(c)) {
				free(tmpbuf);
				throw_error(q, c, "type_error", "integer");
				return 0;
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = format_integer(dst, c->val_num, 3, ',', noargval?2:argval);
		} else {
			int saveq = q->quoted;

			if (ch == 'q')
				q->quoted = 1;

			if (is_string(c) && !q->quoted)
				q->quoted = -1;

			if (canonical)
				len = write_canonical_to_buf(q, NULL, 0, c, 1, 0);
			else
				len = write_term_to_buf(q, NULL, 0, c, 1, 0, 0);

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			if (canonical)
				len = write_canonical_to_buf(q, dst, nbytes, c, 1, 0);
			else
				len = write_term_to_buf(q, dst, nbytes, c, 1, 0, 0);

			q->quoted = saveq;
		}

		dst += len;
		nbytes -= len;
	}

	*dst = '\0';
	size_t len = dst - tmpbuf;

	if (str == NULL) {
		int n = get_named_stream(q, "user_output");
		stream *str = &g_streams[n];
		net_write(tmpbuf, len, str);
	} else if (is_structure(str) && ((strcmp(GET_STR(str),"atom") && strcmp(GET_STR(str),"string")) || (str->arity > 1) || !is_variable(str+1))) {
		free(tmpbuf);
		throw_error(q, c, "type_error", "structure");
		return 0;
	} else if (is_structure(str)) {
		cell *c = deref_var(q, str+1, str_ctx);
		cell tmp = make_cstring(q, tmpbuf);
		set_var(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
	} else if (is_stream(str)) {
		int n = get_stream(q, str);
		stream *str = &g_streams[n];
		const char *src = tmpbuf;

		while (len) {
			size_t nbytes = net_write(src, len, str);

			if (!nbytes) {
				if (feof(str->fp) || ferror(str->fp)) {
					free(tmpbuf);
					fprintf(stderr, "Error: end of file on write\n");
					return 0;
				}
			}

			clearerr(str->fp);
			len -= nbytes;
			src += nbytes;
		}
	} else {
		free(tmpbuf);
		throw_error(q, p1, "type_error", "stream");
		return 0;
	}

	free(tmpbuf);
	return 1;
}

static int fn_format_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	return do_format(q, NULL, 0, p1, p1_ctx, NULL, 0);
}

static int fn_format_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	return do_format(q, NULL, 0, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

static int fn_format_3(query *q)
{
	GET_FIRST_ARG(pstr,stream_or_structure);
	GET_NEXT_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	return do_format(q, pstr, pstr_ctx, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

#if USE_OPENSSL
static int fn_sha1_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA_DIGEST_LENGTH];
	SHA1((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp = make_string(q, tmpbuf, strlen(tmpbuf));
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_sha256_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA256_DIGEST_LENGTH];
	SHA256((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp = make_string(q, tmpbuf, strlen(tmpbuf));
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_sha512_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA512_DIGEST_LENGTH];
	SHA512((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp = make_string(q, tmpbuf, strlen(tmpbuf));
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}
#endif

static int do_b64encode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(p1);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc((len*3)+1);
	b64_encode(str, len, &dstbuf, 0, 0);
	cell tmp = make_string(q, dstbuf, strlen(dstbuf));
	free(dstbuf);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int do_b64decode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(p2);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc(len+1);
	b64_decode(str, len, &dstbuf);
	cell tmp = make_string(q, dstbuf, strlen(dstbuf));
	if (is_string(p1)) tmp.flags |= FLAG_STRING;
	free(dstbuf);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_base64_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_list(p1)) && is_variable(p2))
		return do_b64encode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_b64decode_2(q);

	throw_error(q, p1, "instantiation_error", "atom");
	return 0;
}

static char *url_encode(const char *src, int len, char *dstbuf)
{
	char *dst = dstbuf;

	// As per RFC3986 (2005)

	while (len-- > 0) {
		if (!isalnum(*src) && (*src != '-') && (*src != '_') && (*src != '.') && (*src != '~'))
			dst += sprintf(dst, "%%%02X", *src++);
		else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

char *url_decode(const char *src, char *dstbuf)
{
	char *dst = dstbuf;

	while (*src) {
		if (*src == '%') {
			src++;
			unsigned ch = 0;
			sscanf(src, "%02X", &ch);
			src += 2;
			*dst++ = (unsigned char)ch;
		} else if (*src == '+') {
			*dst++ = ' ';
			src++;
		} else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

static int do_urlencode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(p1);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc((len*3)+1);
	url_encode(str, len, dstbuf);
	cell tmp = make_string(q, dstbuf, strlen(dstbuf));
	free(dstbuf);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int do_urldecode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(p1);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc(len+1);
	url_decode(str, dstbuf);
	cell tmp = make_string(q, dstbuf, strlen(dstbuf));
	free(dstbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_urlenc_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_string(p1)) && is_variable(p2))
		return do_urlencode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_urldecode_2(q);

	throw_error(q, p1, "instantiation_error", "atom");
	return 0;
}

static int fn_string_lower_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	char *tmps = strndup(str, LEN_STR(p1));
	char *s = tmps;

	while (*s) {
		*s = tolower(*s);
		s++;
	}

	cell tmp = make_string(q, tmps, LEN_STR(p1));
	free(tmps);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_string_upper_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	char *tmps = strndup(str, LEN_STR(p1));
	char *s = tmps;

	while (*s) {
		*s = toupper(*s);
		s++;
	}

	cell tmp = make_string(q, tmps, LEN_STR(p1));
	free(tmps);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_exists_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = GET_STR(p1);
	struct stat st = {0};

	if (stat(filename, &st))
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return 0;

	return 1;
}

static int fn_delete_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = GET_STR(p1);
	remove(filename);
	return 1;
}

static int fn_rename_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	const char *filename1 = GET_STR(p1);
	const char *filename2 = GET_STR(p2);
	return !rename(filename1, filename2);
}

static int fn_time_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *filename = GET_STR(p1);
	struct stat st = {0};

	if (stat(filename, &st)) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		return 0;
	}

	cell tmp;
	make_float(&tmp, st.st_mtime);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_size_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *filename = GET_STR(p1);
	struct stat st = {0};

	if (stat(filename, &st)) {
		throw_error(q, p1, "existence_error", "cannot_open_file");
		return 0;
	}

	cell tmp;
	make_int(&tmp, st.st_size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_exists_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = GET_STR(p1);
	struct stat st = {0};

	if (stat(filename, &st))
		return 0;

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return 0;

	return 1;
}

static int fn_make_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = GET_STR(p1);
	struct stat st = {0};

	if (!stat(filename, &st))
		return 0;

	return !mkdir(filename, 0777);
}

static int fn_working_directory_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom_or_var);
	char tmpbuf[PATH_MAX], tmpbuf2[PATH_MAX];
	char *oldpath = getcwd(tmpbuf, sizeof(tmpbuf));
	snprintf(tmpbuf2, sizeof(tmpbuf2), "%s%s", oldpath, PATH_SEP);
	oldpath = tmpbuf2;
	cell tmp = make_cstring(q, oldpath);

	if (is_atom(p2)) {
		const char *pathname = GET_STR(p2);

		if (!chdir(pathname))
			return 0;
	}

	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static int fn_chdir_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = GET_STR(p1);
	return !chdir(filename);
}

static int fn_edin_skip_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = 1;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = 0;
			break;
		} else if (ch == '\n')
			str->did_getc = 0;

		if (ch == p1->val_num)
			break;
	}

	return 1;
}

static int fn_edin_skip_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = 1;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = 0;
			break;
		} else if (ch == '\n')
			str->did_getc = 0;

		if (ch == p1->val_num)
			break;
	}

	return 1;
}

static int fn_edin_tab_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (!is_integer(&p1)) {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];

	for (int i = 0; i < p1.val_num; i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static int fn_edin_tab_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (!is_integer(&p1)) {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	for (int i = 0; i < p1.val_num; i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static int fn_edin_seen_0(query *q)
{
	int n = get_named_stream(q, "user_input");
	stream *str = &g_streams[n];

	if (n <= 2)
		return 1;

	fclose(str->fp ? str->fp : str->fp);
	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->current_input = 0;
	return 1;
}

static int fn_edin_told_0(query *q)
{
	int n = get_named_stream(q, "user_output");
	stream *str = &g_streams[n];

	if (n <= 2)
		return 1;

	fclose(str->fp ? str->fp : str->fp);
	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->current_output = 0;
	return 1;
}

static int fn_edin_seeing_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name = q->current_input==0?"user":g_streams[q->current_input].name;
	cell tmp = make_cstring(q, name);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_edin_telling_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name =q->current_output==1?"user":g_streams[q->current_output].name;
	cell tmp = make_cstring(q, name);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static idx_t do_jenkins_one_at_a_time_hash(const char *key)
{
	idx_t hash = 0;

	while (*key != 0) {
		hash += *key++;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);
	return hash;
}

static int fn_term_hash_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,integer_or_var);
	size_t len = write_term_to_buf(q, NULL, 0, p1, 1, 0, 0);
	char *dst = malloc(len+1);
	write_term_to_buf(q, dst, len+1, p1, 1, 0, 0);
	cell tmp;
	make_int(&tmp, do_jenkins_one_at_a_time_hash(dst));
	free(dst);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_hex_chars_2(query *q)
{
	GET_FIRST_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p1,atom_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "atom");
		return 0;
	}

	if (is_variable(p1)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%llx", (long long)p2->val_num);
		cell tmp = make_cstring(q, tmpbuf);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	const char *src = GET_STR(p1);
	uint_t p1_val = strtoull(src, NULL, 16);

	if (is_variable(p2)) {
		cell tmp;
		make_int(&tmp, p1_val);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	return p1_val == p2->val_num;
}

static int fn_octal_chars_2(query *q)
{
	GET_FIRST_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p1,atom_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");
		return 0;
	}

	if (is_variable(p1)) {
		char tmpbuf[256];
		sprintf(tmpbuf, "%llo", (long long)p2->val_num);
		cell tmp = make_cstring(q, tmpbuf);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	const char *src = GET_STR(p1);
	uint_t p1_val = strtoull(src, NULL, 8);

	if (is_variable(p2)) {
		cell tmp;
		make_int(&tmp, p1_val);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	return p1_val == p2->val_num;
}

static int fn_rdiv_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		q->accum.val_num = p1.val_num;
		q->accum.val_den = p2.val_num;
		q->accum.val_type = TYPE_INTEGER;
	} else {
		throw_error(q, &p1, "type_error", "integer");
		return 0;
	}

	return 1;
}

static void do_real_to_fraction(double v, double accuracy, int_t *num, int_t *den)
{
	if (accuracy <= 0.0 || accuracy >= 1.0)
		abort();

	int_t sign = v < 0 ? -1 : 1;

	if (sign == -1) {
		v = fabs(v);
	}

	double maxError = sign == 0 ? accuracy : v * accuracy;
	int_t n = floor(v);
	v -= n;

	if (v < maxError) {
		*num = n * sign;
		*den = 1;
		return;
	}

	if ((1 - maxError) < v) {
		*num = (n+1) * sign;
		*den = 1;
		return;
	}

	double z = v;
	int_t previous_denominator = 0;
	int_t denominator = 1;
	int_t numerator;

	do
	{
		z = 1.0 / (z - (int_t) z);
		int_t tmp = denominator;
		denominator = denominator * (int_t)z + previous_denominator;
		previous_denominator = tmp;
		numerator = v * denominator;
	}
	while (fabs(v-(double)numerator/denominator) > maxError && (z != (int_t)z));

	*num = (n * denominator + numerator) * sign;
	*den = denominator;
	return;
}

static int fn_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_string(p1);
}

static int fn_rational_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if (is_rational(&p1)) {
			reduce(&p1);
			q->accum.val_num = p1.val_num;
			q->accum.val_den = p1.val_den;
			q->accum.val_type = TYPE_INTEGER;
			return 1;
		}

		if (is_float(&p1)) {
			do_real_to_fraction(p1.val_flt, 0.00001, &q->accum.val_num, &q->accum.val_den);
			q->accum.val_type = TYPE_INTEGER;
			return 1;
		}

		throw_error(q, &p1, "type_error", "number");
		return 0;
	}

	return is_rational(p1_tmp);
}

static int fn_getenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var)
	const char *value = getenv(GET_STR(p1));

	if (!value)
		return 0;

	cell tmp = make_cstring(q, (char*)value);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_int)

	if (is_atom(p2)) {
		setenv(GET_STR(p1), GET_STR(p2), 1);
	} else {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), p2->val_num, 10);
		setenv(GET_STR(p1), tmpbuf, 1);
	}

	return 1;
}

static int fn_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(GET_STR(p1));
	return 1;
}

static int fn_uuid_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
    uuid u;
    uuid_gen(&u);
    char tmpbuf[128];
    uuid_to_buf(&u, tmpbuf, sizeof(tmpbuf));
	cell tmp = make_cstring(q, tmpbuf);
	tmp.flags |= FLAG_STRING;
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_atomic_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1) && is_variable(p2))
		return do_atom_concat_3(q);

	if (is_variable(p3)) {
		if (!is_atomic(p1)) {
			throw_error(q, p1, "type_error", "atomic");
			return 0;
		}

		if (!is_atomic(p2)) {
			throw_error(q, p2, "type_error", "atomic");
			return 0;
		}

		const char *src1, *src2;
		size_t len1, len2;
		char tmpbuf1[256], tmpbuf2[256];

		if (is_atom(p1)) {
			len1 = LEN_STR(p1);
			src1 = GET_STR(p1);
		} else if (is_integer(p1)) {
			len1 = sprint_int(tmpbuf1, sizeof(tmpbuf1), p1->val_num, 10);
			src1 = tmpbuf1;
		} else if (is_rational(p1)) {
			len1 = sprint_int(tmpbuf1, sizeof(tmpbuf1), p1->val_num, 10);
			len1 += sprint_int(tmpbuf1+len1, sizeof(tmpbuf1)-len1, p1->val_den, 10);
			src1 = tmpbuf1;
		} else {
			len1 = sprintf(tmpbuf1, "%.17g", p1->val_flt);
			src1 = tmpbuf1;
		}

		if (is_atom(p2)) {
			len2 = LEN_STR(p2);
			src2 = GET_STR(p2);
		} else if (is_integer(p2)) {
			len2 = sprint_int(tmpbuf2, sizeof(tmpbuf2), p2->val_num, 10);
			src2 = tmpbuf2;
		} else if (is_rational(p2)) {
			len2 = sprint_int(tmpbuf2, sizeof(tmpbuf2), p2->val_num, 10);
			len2 += sprint_int(tmpbuf2+len2, sizeof(tmpbuf2)-len2, p2->val_den, 10);
			src2 = tmpbuf2;
		} else {
			len2 = sprintf(tmpbuf2, "%.17g", p2->val_flt);
			src2 = tmpbuf2;
		}

		size_t nbytes = len1 + len2;
		char *dst = malloc(nbytes + 1);
		memcpy(dst, src1, len1);
		memcpy(dst+len1, src2, len2);
		dst[nbytes] = '\0';
		cell tmp = make_cstringn(q, dst, nbytes);
		tmp.flags |= FLAG_STRING;
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (is_variable(p1)) {
		if (strcmp(GET_STR(p3)+(LEN_STR(p3)-LEN_STR(p2)), GET_STR(p2)))
			return 0;

		char *dst = strndup(GET_STR(p3), LEN_STR(p3)-LEN_STR(p2));
		cell tmp = make_cstring(q, dst);
		tmp.flags |= FLAG_STRING;
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (is_variable(p2)) {
		if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
			return 0;

		char *dst = strdup(GET_STR(p3)+LEN_STR(p1));
		cell tmp = make_cstring(q, dst);
		tmp.flags |= FLAG_STRING;
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		free(dst);
		return 1;
	}

	if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
		return 0;

	if (strcmp(GET_STR(p3)+LEN_STR(p1), GET_STR(p2)))
		return 0;

	return 1;
}

static int fn_replace_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,variable);

	int srclen = LEN_STR(p1);
	int dstlen = srclen * 2;
	const char *src = GET_STR(p1);
	const char *s1 = GET_STR(p2);
	const char *s2 = GET_STR(p3);
	int s1len = LEN_STR(p2);
	int s2len = LEN_STR(p3);
	char *dstbuf = (char*)malloc(dstlen + 1);
	char *dst = dstbuf;

	while (srclen > 0) {
		if (!strncmp(src, s1, s1len)) {
			if (dstlen < s2len) {
				size_t save_len = dst - dstbuf;
				dstlen = ((save_len)*2) + s2len;
				dstbuf = (char *)realloc(dstbuf, dstlen + 1);
				dst = dstbuf + save_len;
			}

			strcpy(dst, s2);
			dst += s2len;
			dstlen -= s2len;
			src += s1len;
			srclen -= s1len;
		} else {
			if (dstlen < 1) {
				size_t max_len = dst - dstbuf;
				dstlen = max_len *= 2;
				dstbuf = (char *)realloc(dstbuf, dstlen + 1);
				dst = dstbuf + max_len;
			}

			*dst++ = *src++;
			dstlen--;
			srclen--;
		}
	}

	*dst = '\0';
	cell tmp = make_cstring(q, dstbuf);
	tmp.flags |= FLAG_STRING;
	free(dstbuf);
	set_var(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	return 1;
}

static int fn_predicate_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var)
	cell tmp;

	rule *h = find_functor(q->m, GET_STR(p1), p1->arity);

	if (check_builtin(q->m, GET_STR(p1), p1->arity)) {
		make_literal(&tmp, find_in_pool("built_in"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h && !h->is_dynamic) {
		make_literal(&tmp, find_in_pool("built_in"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h && h->is_dynamic) {
		make_literal(&tmp, find_in_pool("dynamic"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h && h->is_persist) {
		make_literal(&tmp, find_in_pool("persist"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h && h->is_public) {
		make_literal(&tmp, find_in_pool("public"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h && h->is_public) {
		make_literal(&tmp, find_in_pool("exported"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h) {
		make_literal(&tmp, find_in_pool("visible"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	if (h) {
		make_literal(&tmp, find_in_pool("static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return 1;
	}

	return 0;
}

static int fn_numbervars_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	cell *slots[MAX_ARITY] = {0};
	int cnt = 0;

	if (is_structure(p1))
		do_collect_vars(q, p1+1, p1_ctx, p1->nbr_cells-1, slots, &cnt);
	else
		do_collect_vars(q, p1, p1_ctx, p1->nbr_cells, slots, &cnt);

	q->nv_mask = 0;
	unsigned end = q->nv_start = 0;

	for (unsigned i = 0; i < cnt; i++) {
		if (!slots[i])
			continue;

		q->nv_mask |= 1ULL << slots[i]->slot_nbr;
		end++;
	}

	return 1;
}

static int fn_numbervars_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer)
	GET_NEXT_ARG(p3,integer_or_var)

	cell *slots[MAX_ARITY] = {0};
	int cnt = 0;

	if (is_structure(p1))
		do_collect_vars(q, p1+1, p1_ctx, p1->nbr_cells-1, slots, &cnt);
	else
		do_collect_vars(q, p1, p1_ctx, p1->nbr_cells, slots, &cnt);

	q->nv_mask = 0;
	unsigned end = q->nv_start = p2->val_num;

	for (unsigned i = 0; i < cnt; i++) {
		if (!slots[i])
			continue;

		q->nv_mask |= 1ULL << slots[i]->slot_nbr;
		end++;
	}

	cell tmp;
	make_int(&tmp, end);
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

unsigned count_bits(unsigned long long mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if ((1ULL << i) & mask)
			bits++;
	}

	return bits;
}

static int fn_var_number_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,integer_or_var)
	unsigned pos = count_bits(q->nv_mask, p1->slot_nbr);
	cell tmp;
	make_int(&tmp, q->nv_start+pos);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int fn_char_type_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_int);
	GET_NEXT_ARG(p2,atom);
	int ch;

	if (is_atom(p1)) {
		if (LEN_STR(p1) != 1)
			return 0;

		ch = peek_char_utf8(GET_STR(p1));
	} else
		ch = p1->val_num;

	if (!strcmp(GET_STR(p2), "alpha"))
		return isalpha(ch);
	else if (!strcmp(GET_STR(p2), "digit"))
		return isdigit(ch);
	else if (!strcmp(GET_STR(p2), "xdigit"))
		return isxdigit(ch);
	else if (!strcmp(GET_STR(p2), "whitespace"))
		return isblank(ch) || isspace(ch);
	else if (!strcmp(GET_STR(p2), "white"))
		return isblank(ch);
	else if (!strcmp(GET_STR(p2), "space"))
		return isspace(ch);
	else if (!strcmp(GET_STR(p2), "lower"))
		return islower(ch);
	else if (!strcmp(GET_STR(p2), "upper"))
		return isupper(ch);
	else if (!strcmp(GET_STR(p2), "punct"))
		return ispunct(ch);
	else if (!strcmp(GET_STR(p2), "cntrl"))
		return iscntrl(ch);
	else if (!strcmp(GET_STR(p2), "graph"))
		return isgraph(ch);
	else if (!strcmp(GET_STR(p2), "ascii"))
		return ch < 128;
	else if (!strcmp(GET_STR(p2), "newline"))
		return ch == 10;
	else if (!strcmp(GET_STR(p2), "end_of_line"))
		return (ch >= 10) && (ch <= 13);
	else if (!strcmp(GET_STR(p2), "end_of_file"))
		return ch == -1;
	else if (!strcmp(GET_STR(p2), "quote"))
		return (ch == '\'') || (ch == '"') || (ch == '`');
	else if (!strcmp(GET_STR(p2), "period"))
		return (ch == '.') || (ch == '!') || (ch == '?');

	return 0;
}

static void restore_db(module *m, FILE *fp)
{
	parser *p = create_parser(m);
	query *q = create_query(m, 0);
	p->one_shot = 1;
	p->fp = fp;
	m->loading = 1;

	for (;;) {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1)
			break;

		p->srcptr = p->save_line;
		parser_tokenize(p, 0, 0);
		parser_xref(p, p->t, NULL);
		query_execute(q, p->t);
		clear_term(p->t);
	}

	m->loading = 0;
	destroy_query(q);
	free(p->save_line);
	destroy_parser(p);
}

void do_db_load(module *m)
{
	if (!m->use_persist)
		return;

	char filename[1024];
	snprintf(filename, sizeof(filename), "%s.db", m->name);
	char filename2[1024];
	snprintf(filename2, sizeof(filename2), "%s.TMP", m->name);
	struct stat st;

	if (!stat(filename2, &st) && !stat(filename, &st))
		remove(filename2);
	else if (!stat(filename2, &st))
		rename(filename2, filename);

	if (!stat(filename, &st)) {
		FILE *fp = fopen(filename, "rb");
		restore_db(m, fp);
		fclose(fp);
	}

	m->fp = fopen(filename, "ab");
}

static int fn_db_load_0(query *q)
{
	do_db_load(q->m);
	return 1;
}

static int fn_db_save_0(query *q)
{
	if (!q->m->fp)
		return 0;

	fclose(q->m->fp);
	char filename[1024];
	snprintf(filename, sizeof(filename), "%s.db", q->m->name);
	char filename2[1024];
	snprintf(filename2, sizeof(filename2), "%s.TMP", q->m->name);
	FILE *fp = fopen(filename2, "wb");
	if (!fp) return 0;
	save_db(q->m->fp, q, 1);
	fclose(fp);
	remove(filename);
	rename(filename2, filename);
	q->m->fp = fopen(filename, "ab");
	return 1;
}

static int fn_abolish_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);
	cell tmp = *p1;
	tmp.arity = p2->val_num;
	return do_abolish(q, &tmp);
}

static int fn_sys_lt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ < p2->val_num)
		return 1;

	drop_choice(q);
	return 1;
}

static int fn_limit_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, 1, p2, 4);
	idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_lt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p1->val_num);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_sys_gt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ <= p2->val_num)
		return 0;

	return 1;
}

static int fn_offset_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, 1, p2, 4);
	idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_gt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p1->val_num);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

void call_attrs(query *q, cell *attrs)
{
	cell *tmp = clone_to_heap(q, 1, attrs, 1);
	idx_t nbr_cells = 1 + attrs->nbr_cells;
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
}

static int fn_freeze_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,callable);

	if (is_variable(p1)) {
		cell *tmp = clone_to_heap(q, 0, p2, 0);
		frame *g = GET_FRAME(p1_ctx);
		slot *e = GET_SLOT(g, p1->slot_nbr);
		e->c.attrs = tmp;
		return 1;
	}

	cell *tmp = clone_to_heap(q, 1, p2, 1);
	idx_t nbr_cells = 1 + p2->nbr_cells;
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_frozen_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,any);
	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->slot_nbr);

	if (!e->c.attrs) {
		cell tmp;
		make_literal(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	return unify(q, p2, p2_ctx, e->c.attrs, q->st.curr_frame);
}

static int fn_sys_put_atts_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *tmp = deep_clone_to_heap(q, p2, p2_ctx);
	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->slot_nbr);
	e->c.attrs = tmp;
	return 1;
}

static int fn_sys_get_atts_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,variable);
	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->slot_nbr);

	if (!e->c.attrs) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	set_var(q, p2, p2_ctx, e->c.attrs, q->st.curr_frame);
	return 1;
}

static int fn_sys_ne_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ != p2->val_num)
		return 0;

	drop_choice(q);
	return 1;
}

static int fn_call_nth_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p2)) {
		cell *tmp = clone_to_heap(q, 1, p1, 1);
		idx_t nbr_cells = 1 + p1->nbr_cells;
		make_end_return(tmp+nbr_cells, q->st.curr_cell);
		q->st.curr_cell = tmp;
		return 1;
	}

	cell *tmp = clone_to_heap(q, 1, p1, 4);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_ne_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p2->val_num);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	q->st.curr_cell = tmp;
	return 1;
}

static int fn_dcg_translate_rule_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return 0;
}

static int fn_call_dcg_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_list_or_nil(p1))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (is_structure(p1) && (p1->val_off == g_braces_s)) {
		if (!unify(q, p2, p2_ctx, p3, p3_ctx))
			return 0;

		p1 = p1 + 1;
		cell *tmp = clone_to_heap(q, 1, p1, 1);
		idx_t nbr_cells = 1 + p1->nbr_cells;
		make_end_return(tmp+nbr_cells, q->st.curr_cell);
		q->st.curr_cell = tmp;
		return 1;
	}

	return fn_iso_call_n(q);
}

static int fn_phrase_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	GET_NEXT_ARG(p3,list_or_nil_or_var);
	return fn_call_dcg_3(q);
}

static int fn_phrase_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	cell *this = q->st.curr_cell;
	cell *tmp = clone_to_heap(q, 1, this, this->nbr_cells+2);
	idx_t nbr_cells = 1 + this->nbr_cells;
	make_literal(tmp+nbr_cells++, g_nil_s);
	make_end_return(tmp+nbr_cells, q->st.curr_cell);
	tmp[1].arity = 3;
	tmp[1].fn = fn_call_dcg_3;
	tmp[1].nbr_cells += 1;
	q->st.curr_cell = tmp;
	return 1;
}

static int do_length(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer);
	idx_t nbr = p2->val_num;
	GET_RAW_ARG(2, p2_orig);
	cell tmp;
	make_int(&tmp, ++nbr);
	reset_value(q, p2_orig, p2_orig_ctx, &tmp, q->st.curr_frame);
	make_choice(q);

	if (is_anon(p1))
		return 1;

	if (nbr >= MAX_VARS) {
		drop_choice(q);
		throw_error(q, p2, "resource_error", "too_many_vars");
		return 0;
	}

	unsigned slot_nbr;

	if (!(slot_nbr = create_vars(q, nbr))) {
		drop_choice(q);
		throw_error(q, p1, "resource_error", "too_many_vars");
		return 0;
	}

	tmp.val_type = TYPE_VARIABLE;
	tmp.nbr_cells = 1;
	tmp.flags = 0;
	tmp.val_off = g_anon_s;
	tmp.slot_nbr = slot_nbr++;
	alloc_list(q, &tmp);

	for (int i = 1; i < nbr; i++) {
		tmp.slot_nbr = slot_nbr++;
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	fix_list(l, l->nbr_cells);
	set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	return 1;
}

static int fn_length_2(query *q)
{
	if (q->retry)
		return do_length(q);

	GET_FIRST_ARG(p1,list_or_nil_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		cell tmp;
		make_int(&tmp, 0);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		make_choice(q);

		if (!is_anon(p1)) {
			make_literal(&tmp, g_nil_s);
			set_var(q, p1,p1_ctx, &tmp, q->st.curr_frame);
		}

		return 1;
	}

	if (!is_variable(p1) && is_variable(p2)) {
		if (!is_list(p1) && !is_nil(p1))
			return 0;

		int cnt = 0;

		if (is_string(p1)) {
			cnt = strlen_utf8(p1->val_str);
		} else {
			cell *l = p1;

			while (is_list(l)) {
				LIST_HEAD(l);
				l = LIST_TAIL(l);
				l = deref_var(q, l, p1_ctx);
				p1_ctx = q->latest_ctx;
				cnt++;
			}
		}

		cell tmp;
		make_int(&tmp, cnt);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return 1;
	}

	if (is_integer(p2) && !is_variable(p1)) {
		if (p2->val_num == 0) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		}

		int cnt = 0;

		if (is_string(p1)) {
			cnt = strlen_utf8(p1->val_str);
		} else {
			cell *l = p1;

			while (is_list(l)) {
				LIST_HEAD(l);
				l = LIST_TAIL(l);
				l = deref_var(q, l, p1_ctx);
				p1_ctx = q->latest_ctx;
				cnt++;
			}
		}

		return p2->val_num == cnt;
	}


	if (is_variable(p1) && is_integer(p2)) {
		if (is_anon(p1))
			return 1;

		idx_t nbr = p2->val_num;

		if (nbr >= MAX_VARS) {
			throw_error(q, p2, "resource_error", "too_many_vars");
			return 0;
		}

		if (nbr == 0) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			return 1;
		}

		unsigned slot_nbr;

		if (!(slot_nbr = create_vars(q, nbr))) {
			throw_error(q, p2, "resource_error", "too_many_vars");
			return 0;
		}

		cell tmp;
		tmp.val_type = TYPE_VARIABLE;
		tmp.nbr_cells = 1;
		tmp.flags = 0;
		tmp.val_off = g_anon_s;
		tmp.slot_nbr = slot_nbr++;
		alloc_list(q, &tmp);

		for (int i = 1; i < nbr; i++) {
			tmp.slot_nbr = slot_nbr++;
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		fix_list(l, l->nbr_cells);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
		return 1;
	}

	throw_error(q, p1, "type_error", "arg invalid");
	return 0;
}

static int fn_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	module *m = find_module(GET_STR(p1));

	if (!m) {
		throw_error(q, p1, "type_error", "module");
		return 0;
	}

	q->m = m;
	return 1;
}

static const struct builtins g_iso_funcs[] =
{
	{":-", 2, NULL, NULL},
	{":-", 1, NULL, NULL},
	{",", 2, NULL, NULL},
	//{"call", 1, NULL, NULL},

	{"->", 2, fn_iso_ifthen_2, NULL},
	{";", 2, fn_iso_disjunction_2, NULL},
	{"\\+", 1, fn_iso_negation_1, NULL},
	{"once", 1, fn_iso_once_1, NULL},
	{"catch", 3, fn_iso_catch_3, NULL},
	{"throw", 1, fn_iso_throw_1, NULL},
	{"call", 2, fn_iso_call_n, NULL},
	{"call", 3, fn_iso_call_n, NULL},
	{"call", 4, fn_iso_call_n, NULL},
	{"call", 5, fn_iso_call_n, NULL},
	{"call", 6, fn_iso_call_n, NULL},
	{"call", 7, fn_iso_call_n, NULL},
	{"call", 8, fn_iso_call_n, NULL},
	{"repeat", 0, fn_iso_repeat_0, NULL},
	{"true", 0, fn_iso_true_0, NULL},
	{"fail", 0, fn_iso_fail_0, NULL},
	{"false", 0, fn_iso_fail_0, NULL},
	{"halt", 0, fn_iso_halt_0, NULL},
	{"halt", 1, fn_iso_halt_1, NULL},
	{"integer", 1, fn_iso_integer_1, NULL},
	{"float", 1, fn_iso_float_1, NULL},
	{"number", 1, fn_iso_number_1, NULL},
	{"atom", 1, fn_iso_atom_1, NULL},
	{"atomic", 1, fn_iso_atomic_1, NULL},
	{"compound", 1, fn_iso_compound_1, NULL},
	{"var", 1, fn_iso_var_1, NULL},
	{"nonvar", 1, fn_iso_nonvar_1, NULL},
	{"ground", 1, fn_iso_ground_1, NULL},
	{"callable", 1, fn_iso_callable_1, NULL},
	{"atom_chars", 2, fn_iso_atom_chars_2, NULL},
	{"atom_codes", 2, fn_iso_atom_codes_2, NULL},
	{"number_chars", 2, fn_iso_number_chars_2, NULL},
	{"number_codes", 2, fn_iso_number_codes_2, NULL},
	{"!", 0, fn_iso_cut_0, NULL},
	{"is", 2, fn_iso_is_2, NULL},
	{"clause", 2, fn_iso_clause_2, NULL},
	{"arg", 3, fn_iso_arg_3, NULL},
	{"functor", 3, fn_iso_functor_3, NULL},
	{"=..", 2, fn_iso_univ_2, NULL},
	{"copy_term", 2, fn_iso_copy_term_2, NULL},
	{"term_variables", 2, fn_iso_term_variables_2, NULL},
	{"atom_length", 2, fn_iso_atom_length_2, NULL},
	{"atom_concat", 3, fn_iso_atom_concat_3, NULL},
	{"sub_atom", 5, fn_iso_sub_atom_5, NULL},
	{"compare", 3, fn_iso_compare_3, NULL},
	{"current_rule", 1, fn_iso_current_rule_1, NULL},

	{"open", 3, fn_iso_open_3, NULL},
	{"open", 4, fn_iso_open_4, NULL},
	{"close", 1, fn_iso_close_1, NULL},
	{"read_term", 2, fn_iso_read_term_2, NULL},
	{"read_term", 3, fn_iso_read_term_3, NULL},
	{"read", 1, fn_iso_read_1, NULL},
	{"read", 2, fn_iso_read_2, NULL},
	{"write_canonical", 1, fn_iso_write_canonical_1, NULL},
	{"write_canonical", 2, fn_iso_write_canonical_2, NULL},
	{"write_term", 2, fn_iso_write_term_2, NULL},
	{"write_term", 3, fn_iso_write_term_3, NULL},
	{"writeq", 1, fn_iso_writeq_1, NULL},
	{"writeq", 2, fn_iso_writeq_2, NULL},
	{"write", 1, fn_iso_write_1, NULL},
	{"write", 2, fn_iso_write_2, NULL},
	{"nl", 0, fn_iso_nl_0, NULL},
	{"nl", 1, fn_iso_nl_1, NULL},
	{"at_end_of_stream", 0, fn_iso_at_end_of_stream_0, NULL},
	{"at_end_of_stream", 1, fn_iso_at_end_of_stream_1, NULL},
	{"set_stream_position", 2, fn_iso_set_stream_position_2, NULL},
	{"flush_output", 0, fn_iso_flush_output_0, NULL},
	{"flush_output", 1, fn_iso_flush_output_1, NULL},
	{"put_char", 1, fn_iso_put_char_1, NULL},
	{"put_char", 2, fn_iso_put_char_2, NULL},
	{"put_code", 1, fn_iso_put_code_1, NULL},
	{"put_code", 2, fn_iso_put_code_2, NULL},
	{"put_byte", 1, fn_iso_put_byte_1, NULL},
	{"put_byte", 2, fn_iso_put_byte_2, NULL},
	{"get_char", 1, fn_iso_get_char_1, NULL},
	{"get_char", 2, fn_iso_get_char_2, NULL},
	{"get_code", 1, fn_iso_get_code_1, NULL},
	{"get_code", 2, fn_iso_get_code_2, NULL},
	{"get_byte", 1, fn_iso_get_byte_1, NULL},
	{"get_byte", 2, fn_iso_get_byte_2, NULL},
	{"peek_char", 1, fn_iso_peek_char_1, NULL},
	{"peek_char", 2, fn_iso_peek_char_2, NULL},
	{"peek_code", 1, fn_iso_peek_code_1, NULL},
	{"peek_code", 2, fn_iso_peek_code_2, NULL},
	{"peek_byte", 1, fn_iso_peek_byte_1, NULL},
	{"peek_byte", 2, fn_iso_peek_byte_2, NULL},
	{"current_input", 1, fn_iso_current_input_1, NULL},
	{"current_output", 1, fn_iso_current_output_1, NULL},
	{"set_input", 1, fn_iso_set_input_1, NULL},
	{"set_output", 1, fn_iso_set_output_1, NULL},
	{"stream_property", 2, fn_iso_stream_property_2, NULL},

	{"abolish", 1, fn_iso_abolish_1, NULL},
	{"asserta", 1, fn_iso_asserta_1, NULL},
	{"assertz", 1, fn_iso_assertz_1, NULL},
	{"retract", 1, fn_iso_retract_1, NULL},
	{"retractall", 1, fn_iso_retractall_1, NULL},

	{"=:=", 2, fn_iso_neq_2, NULL},
	{"=\\=", 2, fn_iso_nne_2, NULL},
	{">", 2, fn_iso_ngt_2, NULL},
	{">=", 2, fn_iso_nge_2, NULL},
	{"=<", 2, fn_iso_nle_2, NULL},
	{"<", 2, fn_iso_nlt_2, NULL},

	{"==", 2, fn_iso_seq_2, NULL},
	{"\\==", 2, fn_iso_sne_2, NULL},
	{"@>", 2, fn_iso_sgt_2, NULL},
	{"@>=", 2, fn_iso_sge_2, NULL},
	{"@=<", 2, fn_iso_sle_2, NULL},
	{"@<", 2, fn_iso_slt_2, NULL},

	{"+", 1, fn_iso_positive_1, NULL},
	{"-", 1, fn_iso_negative_1, NULL},
	{"abs", 1, fn_iso_abs_1, NULL},
	{"sign", 1, fn_iso_sign_1, NULL},
	{"=", 2, fn_iso_unify_2, NULL},
	{"\\=", 2, fn_iso_notunify_2, NULL},
	{"pi", 0, fn_iso_pi_0, NULL},
	{"e", 0, fn_iso_e_0, NULL},
	{"+", 2, fn_iso_add_2, NULL},
	{"-", 2, fn_iso_sub_2, NULL},
	{"*", 2, fn_iso_mul_2, NULL},
	{"/", 2, fn_iso_divide_2, NULL},
	{"//", 2, fn_iso_divint_2, NULL},
	{"div", 2, fn_iso_div_2, NULL},
	{"mod", 2, fn_iso_mod_2, NULL},
	{"rem", 2, fn_iso_mod_2, NULL},
	{"max", 2, fn_iso_max_2, NULL},
	{"min", 2, fn_iso_min_2, NULL},
	{"xor", 2, fn_iso_xor_2, NULL},
	{"/\\", 2, fn_iso_and_2, NULL},
	{"\\/", 2, fn_iso_or_2, NULL},
	{"<<", 2, fn_iso_shl_2, NULL},
	{">>", 2, fn_iso_shr_2, NULL},
	{"\\", 1, fn_iso_neg_1, NULL},
	{"**", 2, fn_iso_pow_2, NULL},
	{"^", 2, fn_iso_powi_2, NULL},
	{"exp", 1, fn_iso_exp_1, NULL},
	{"sqrt", 1, fn_iso_sqrt_1, NULL},
	{"log", 1, fn_iso_log_1, NULL},
	{"sin", 1, fn_iso_sin_1, NULL},
	{"cos", 1, fn_iso_cos_1, NULL},
	{"tan", 1, fn_iso_tan_1, NULL},
	{"asin", 1, fn_iso_asin_1, NULL},
	{"acos", 1, fn_iso_acos_1, NULL},
	{"atan", 1, fn_iso_atan_1, NULL},
	{"atan2", 2, fn_iso_atan_2, NULL},
	{"copysign", 2, fn_iso_copysign_2, NULL},
	{"truncate", 1, fn_iso_truncate_1, NULL},
	{"round", 1, fn_iso_round_1, NULL},
	{"ceiling", 1, fn_iso_ceiling_1, NULL},
	{"floor", 1, fn_iso_floor_1, NULL},
	{"float_integer_part", 1, fn_iso_float_integer_part_1, NULL},
	{"float_fractional_part", 1, fn_iso_float_fractional_part_1, NULL},
	{"current_prolog_flag", 2, fn_iso_current_prolog_flag_2, NULL},
	{"set_prolog_flag", 2, fn_iso_set_prolog_flag_2, NULL},
	{"sort", 2, fn_iso_sort_2, NULL},
	{"keysort", 2, fn_iso_keysort_2, NULL},
	{"findall", 3, fn_iso_findall_3, NULL},
	{"bagof", 3, fn_iso_bagof_3, NULL},
	{"setof", 3, fn_iso_setof_3, NULL},
	{"op", 3, fn_iso_op_3, NULL},

	//

	{"module", 1, fn_module_1, NULL},
	{"consult", 1, fn_consult_1, NULL},
	{"listing", 0, fn_listing_0, NULL},
	{"listing", 1, fn_listing_1, NULL},
	{"time", 1, fn_time_1, NULL},

	{"phrase", 2, fn_phrase_2, NULL},
	{"phrase", 3, fn_phrase_3, NULL},
	{"call_dcg", 3, fn_call_dcg_3, NULL},
	{"dcg_translate_rule", 3, fn_dcg_translate_rule_2, NULL},

	{0}
};

static const struct builtins g_other_funcs[] =
{
	// Edinburgh...

	{"seeing", 1, fn_edin_seeing_1, "-name"},
	{"telling", 1, fn_edin_telling_1, "-name"},
	{"seen", 0, fn_edin_seen_0, NULL},
	{"told", 0, fn_edin_told_0, NULL},
	{"skip", 1, fn_edin_skip_1, "+integer"},
	{"skip", 2, fn_edin_skip_2, "+stream,+integer"},
	{"tab", 1, fn_edin_tab_1, "+integer"},
	{"tab", 2, fn_edin_tab_2, "+stream,+integer"},

	// Miscellaneous...

	{"length", 2, fn_length_2, NULL},
	{"ignore", 1, fn_ignore_1, "+callable"},
	{"format", 1, fn_format_1, "+string"},
	{"format", 2, fn_format_2, "+string,+list"},
	{"format", 3, fn_format_3, "+stream,+string,+list"},
	{"findall", 4, fn_findall_4, NULL},
	{"rdiv", 2, fn_rdiv_2, "+integer,+integer"},
	{"rational", 1, fn_rational_1, "+number"},
	{"rationalize", 1, fn_rational_1, "+number"},
	{"abolish", 2, fn_abolish_2, NULL},
	{"assert", 1, fn_iso_assertz_1, NULL},

	{"string", 1, fn_atom_1, "+term"},
	{"atomic_concat", 3, fn_atomic_concat_3, NULL},
	{"replace", 4, fn_replace_4, "+orig,+from,+to,-new"},
	{"writeln", 1, fn_writeln_1, "+term"},
	{"sleep", 1, fn_sleep_1, "+integer"},
	{"delay", 1, fn_delay_1, "+integer"},
	{"busy", 1, fn_busy_1, "+integer"},
	{"now", 0, fn_now_0, NULL},
	{"now", 1, fn_now_1, "now(-integer)"},
	{"get_time", 1, fn_get_time_1, "-variable"},
	{"random", 1, fn_random_1, "?integer"},
	{"rand", 1, fn_rand_1, "?integer"},
	{"rand", 0, fn_rand_0, NULL},
	{"srandom", 1, fn_srandom_1, "+integer"},
	{"between", 3, fn_between_3, "+integer,+integer,-integer"},
	{"log10", 1, fn_log10_1, "+integer"},
	{"client", 5, fn_client_5, "+string,-string,-string,-stream,+list"},
	{"server", 3, fn_server_3, "+string,-stream,+list"},
	{"accept", 2, fn_accept_2, "+stream,-stream"},
	{"getline", 1, fn_getline_1, "-string"},
	{"getline", 2, fn_getline_2, "+stream,-string"},
	{"getfile", 2, fn_getfile_2, "+string,-list"},
	{"loadfile", 2, fn_loadfile_2, "+string,-string"},
	{"savefile", 2, fn_savefile_2, "+string,+string"},
	{"split_atom", 4, fn_split_atom_4, "+string,+sep,+pad,-list"},
	{"split", 4, fn_split_4, "+string,+string,?left,?right"},
	{"msort", 2, fn_msort_2, "+list,-list"},
	{"is_list", 1, fn_is_list_1, "+term"},
	{"list", 1, fn_is_list_1, "+term"},
	{"is_stream", 1, fn_is_stream_1, "+term"},
	{"forall", 2, fn_forall_2, "+term,+term"},
	{"term_hash", 2, fn_term_hash_2, "+term,?integer"},
	{"rename_file", 2, fn_rename_file_2, "+string,+string"},
	{"delete_file", 1, fn_delete_file_1, "+string"},
	{"exists_file", 1, fn_exists_file_1, "+string"},
	{"time_file", 2, fn_time_file_2, "+string,-real"},
	{"size_file", 2, fn_size_file_2, "+string,-integer"},
	{"exists_directory", 1, fn_exists_directory_1, "+string"},
	{"make_directory", 1, fn_make_directory_1, "+string"},
	{"working_directory", 2, fn_working_directory_2, "-string,+string"},
	{"chdir", 1, fn_chdir_1, "+string"},
	{"name", 2, fn_iso_atom_codes_2, "?string,?list"},
	{"read_term_from_atom", 3, fn_read_term_from_atom_3, "+string,-term,+list"},
	{"write_term_to_atom", 3, fn_write_term_to_atom_3, "-string,+term,+list"},
	{"term_to_atom", 2, fn_term_to_atom_2, "+term,-string"},
	{"base64", 2, fn_base64_2, "?string,?string"},
	{"urlenc", 2, fn_urlenc_2, "?string,?string"},
	{"string_lower", 2, fn_string_lower_2, "?string,?string"},
	{"string_upper", 2, fn_string_upper_2, "?string,?string"},
	{"bread", 3, fn_bread_3, "+stream,+integer,-string"},
	{"bwrite", 2, fn_bwrite_2, "+stream,-string"},
	{"hex_chars", 2, fn_hex_chars_2, "?integer,?string"},
	{"octal_chars", 2, fn_octal_chars_2, "?integer,?string"},
	{"predicate_property", 2, fn_predicate_property_2, "+callable,?string"},
	{"numbervars", 1, fn_numbervars_1, "+term"},
	{"numbervars", 3, fn_numbervars_3, "+term,+start,?end"},
	{"numbervars", 4, fn_numbervars_3, "+term,+start,?end,+list"},
	{"var_number", 2, fn_var_number_2, "+term,?integer"},
	{"char_type", 2, fn_char_type_2, "+char,+term"},
	{"code_type", 2, fn_char_type_2, "+code,+term"},
	{"uuid", 1, fn_uuid_1, "-string"},
	{"asserta", 2, fn_asserta_2, "+term,-ref"},
	{"assertz", 2, fn_assertz_2, "+term,-ref"},
	{"instance", 2, fn_instance_2, "+ref,?term"},
	{"erase", 1, fn_erase_1, "+ref"},
	{"clause", 3, fn_clause_3, "?head,?body,-ref"},
	{"$queue", 1, fn_sys_queue_1, "+term"},
	{"$list", 1, fn_sys_list_1, "-list"},
	{"getenv", 2, fn_getenv_2},
	{"setenv", 2, fn_setenv_2},
	{"unsetenv", 1, fn_unsetenv_1},
	{"load_files", 2, fn_consult_1, "+files"},
	{"statistics", 2, fn_statistics_2, "+string,-variable"},
	{"duplicate_term", 2, fn_iso_copy_term_2, "+string,-variable"},
	{"call_nth", 2, fn_call_nth_2, "+callable,+integer"},
	{"limit", 2, fn_limit_2, "+integer,+callable"},
	{"offset", 2, fn_offset_2, "+integer,+callable"},

	{"freeze", 2, fn_freeze_2, "+variable,+callable"},
	{"frozen", 2, fn_frozen_2, "+variable,+callable"},
	{"$put_atts", 2, fn_sys_put_atts_2, "+variable,+callable"},
	{"$get_atts", 2, fn_sys_get_atts_2, "+variable,+callable"},

#if USE_OPENSSL
	{"sha1", 2, fn_sha1_2, "+string,?string"},
	{"sha256", 2, fn_sha256_2, "+string,?string"},
	{"sha512", 2, fn_sha512_2, "+string,?string"},
#endif

	{"fork", 0, fn_fork_0, NULL},
	{"spawn", 1, fn_spawn_1, "+callable"},
	{"spawn", 2, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 3, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 4, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 5, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 6, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 7, fn_spawn_n, "+callable,+term,..."},
	{"spawn", 8, fn_spawn_n, "+callable,+term,..."},
	{"wait", 0, fn_wait_0, NULL},
	{"await", 0, fn_await_0, NULL},
	{"yield", 0, fn_yield_0, NULL},
	{"send", 1, fn_send_1, "+term"},
	{"recv", 1, fn_recv_1, "?term"},

	// Used for database log

	{"a_", 2, fn_sys_asserta_2, "+term,+ref"},
	{"z_", 2, fn_sys_assertz_2, "+term,+ref"},
	{"e_", 1, fn_erase_1, "+ref"},

	{"db_load", 0, fn_db_load_0, NULL},
	{"db_save", 0, fn_db_save_0, NULL},

	{0}
};

int check_builtin(module *m, const char *name, unsigned arity)
{
	for (const struct builtins *ptr = g_iso_funcs; ptr->name; ptr++) {
		if ((ptr->arity == arity) && !strcmp(ptr->name, name))
			return 1;
	}

	if (m->iso_only)
		return 0;

	for (const struct builtins *ptr = g_other_funcs; ptr->name; ptr++) {
		if ((ptr->arity == arity) && !strcmp(ptr->name, name))
			return 1;
	}

	return 0;
}

void *get_builtin(module *m, const char *name, unsigned arity)
{
	for (const struct builtins *ptr = g_iso_funcs; ptr->name; ptr++) {
		if ((ptr->arity == arity) && !strcmp(ptr->name, name))
			return ptr->fn;
	}

	if (m->iso_only)
		return NULL;

	for (const struct builtins *ptr = g_other_funcs; ptr->name; ptr++) {
		if ((ptr->arity == arity) && !strcmp(ptr->name, name))
			return ptr->fn;
	}

	return NULL;
}

void load_keywords(module *m)
{
	for (int idx = 0; g_iso_funcs[idx].name; idx++)
		m->keywords[idx] = g_iso_funcs[idx].name;

	if (m->iso_only)
		return;

	for (int idx = 0; g_other_funcs[idx].name; idx++)
		m->keywords[idx] = g_other_funcs[idx].name;
}
