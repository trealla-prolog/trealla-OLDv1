#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
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
#include <dirent.h>
#endif

#include "trealla.h"
#include "internal.h"
#include "network.h"
#include "base64.h"
#include "library.h"
#include "utf8.h"
#include "builtins.h"

#if USE_OPENSSL
#include "openssl/sha.h"
#endif

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

cell *ERR_CYCLE_CELL = &(cell){};

#if 0
static double rat_to_float(cell *n)
{
	return (double)n->val_num / n->val_den;
}
#endif

static pl_status do_yield_0(query *q, int msecs)
{
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs;
	may_error(make_choice(q));
	return pl_failure;
}

static void set_pinned(query *q, int i)
{
	choice *ch = GET_CURR_CHOICE();
	ch->pins |= 1ULL << i;
}

static int is_pinned(query *q, int i)
{
	choice *ch = GET_CURR_CHOICE();
	return ch->pins & (1ULL << i) ? 1 : 0;
}

static void set_params(query *q, idx_t p1, idx_t p2)
{
	choice *ch = GET_CURR_CHOICE();
	ch->v1 = p1;
	ch->v2 = p2;
}

static void get_params(query *q, idx_t *p1, idx_t *p2)
{
	choice *ch = GET_CURR_CHOICE();
	if (p1) *p1 = ch->v1;
	if (p2) *p2 = ch->v2;
}

static void make_variable(cell *tmp, idx_t off)
{
	tmp->val_type = TYPE_VARIABLE;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	tmp->val_off = off;
	tmp->var_nbr = 0;
}

void make_int(cell *tmp, int_t v)
{
	tmp->val_type = TYPE_INTEGER;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	tmp->val_num = v;
	tmp->val_den = 1;
}

void make_float(cell *tmp, double v)
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
	tmp->arity = tmp->flags = 0;
	tmp->match = NULL;
	tmp->val_ptr = NULL;
	tmp->cgen = ERR_IDX;
}

void make_call(query *q, cell *tmp)
{
	make_end(tmp);
	cell *c = q->st.curr_cell;
	frame *g = GET_CURR_FRAME();
	tmp->val_ptr = c + c->nbr_cells;
	tmp->cgen = g->cgen;
	tmp->mod_nbr = q->st.m->id;
}

static void make_literal(cell *tmp, idx_t offset)
{
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	tmp->val_off = offset;
}

static void make_smalln(cell *tmp, const char *s, size_t n)
{
	assert(n < MAX_SMALL_STRING);
	tmp->val_type = TYPE_CSTRING;
	tmp->nbr_cells = 1;
	tmp->arity = tmp->flags = 0;
	memcpy(tmp->val_chr, s, n);
	tmp->val_chr[n] = '\0';
}

static void make_small(cell *tmp, const char *s)
{
	size_t n = strlen(s);
	make_smalln(tmp, s, n);
}

#if 0
static void init_queue(query* q)
{
	free(q->queue[0]);
	q->queue[0] = NULL;
	q->qp[0] = 0;
}
#endif

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

static void init_queuen(query* q)
{
	free(q->queue[q->st.qnbr]);
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
}

static idx_t queuen_used(const query *q) { return q->qp[q->st.qnbr]; }
static cell *get_queuen(query *q) { return q->queue[q->st.qnbr]; }

// Defer check until end_list()

void allocate_list(query *q, const cell *c)
{
	if (!init_tmp_heap(q)) return;
	append_list(q, c);
}

// Defer check until end_list()

void append_list(query *q, const cell *c)
{
	cell *tmp = alloc_on_tmp(q, 1+c->nbr_cells);
	if (!tmp) return;
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1 + c->nbr_cells;
	tmp->val_off = g_dot_s;
	tmp->arity = 2;
	tmp->flags = 0;
	tmp++;
	copy_cells(tmp, c, c->nbr_cells);
}

USE_RESULT cell *end_list(query *q)
{
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}

static USE_RESULT cell *end_list_unsafe(query *q)
{
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->val_type = TYPE_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}

static USE_RESULT pl_status make_cstringn(cell *d, const char *s, size_t n)
{
	if (n < MAX_SMALL_STRING) {
		if (!memchr(s, 0, n)) {
			make_smalln(d, s, n);
			return pl_success;
		}
	}

	FAULTINJECT(errno = ENOMEM; return pl_error);
	d->val_type = TYPE_CSTRING;
	d->flags = FLAG_BLOB;
	d->nbr_cells = 1;
	d->arity = 0;
	SET_STR(d, s, n, 0);
	return pl_success;
}

static USE_RESULT pl_status make_cstring(cell *d, const char *s)
{
	return make_cstringn(d, s, strlen(s));
}

static USE_RESULT pl_status make_stringn(cell *d, const char *s, size_t n)
{
	FAULTINJECT(errno = ENOMEM; return pl_error);
	d->val_type = TYPE_CSTRING;
	d->flags = FLAG_BLOB;
	d->flags |= FLAG_STRING;
	d->nbr_cells = 1;
	d->arity = 2;
	SET_STR(d, s, n, 0);
	return pl_success;
}

static USE_RESULT pl_status make_string(cell *d, const char *s)
{
	return make_stringn(d, s, strlen(s));
}

static USE_RESULT pl_status make_slice(query *q, cell *d, cell *orig, size_t off, size_t n)
{
	if (n < MAX_SMALL_STRING) {
		const char *s = GET_STR(orig);

		if (!memchr(s+off, 0, n)) {
			make_smalln(d, s+off, n);
			return pl_success;
		}
	}

	if (!is_strbuf(orig)) {
		const char *s = GET_STR(orig);

		if (is_string(orig))
			return make_stringn(d, s+off, n);

		return make_cstringn(d, s+off, n);
	}

	*d = *orig;
	d->strb_off = off;
	d->strb_len = n;
	INCR_REF(orig);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_unify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return unify(q, p1, p1_ctx, p2, p2_ctx);
}

static USE_RESULT pl_status fn_iso_notunify_2(query *q)
{
	return !fn_iso_unify_2(q);
}

static USE_RESULT pl_status fn_iso_repeat_0(query *q)
{
	may_error(make_choice(q));
	return pl_success;
}

static USE_RESULT pl_status fn_iso_true_0(__attribute__((unused)) query *q)
{
	return pl_success;
}

static USE_RESULT pl_status fn_iso_fail_0(__attribute__((unused)) query *q)
{
	return pl_failure;
}

static USE_RESULT pl_status fn_iso_halt_0(query *q)
{
	q->halt_code = 0;
	q->halt = q->error = true;
	return pl_halt;
}

static USE_RESULT pl_status fn_iso_halt_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	q->halt_code = p1->val_num;
	q->halt = q->error = true;
	return pl_halt;
}

static USE_RESULT pl_status fn_iso_number_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_number(p1);
}

static USE_RESULT pl_status fn_iso_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_iso_atom(p1);
}

static USE_RESULT pl_status fn_iso_compound_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_compound(p1) ? 1 : 0;
}

static USE_RESULT pl_status fn_iso_atomic_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_atomic(p1);
}

static USE_RESULT pl_status fn_iso_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_variable(p1);
}

static USE_RESULT pl_status fn_iso_nonvar_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !is_variable(p1);
}

bool has_vars(query *q, cell *c, idx_t c_ctx, unsigned depth)
{
	if (depth >= 64000) {
		q->cycle_error = true;
		return false;
	}

	if (is_variable(c))
		return true;

	if (!is_structure(c))
		return false;

	unsigned arity = c->arity;
	c++;

	while (arity--) {
		cell *c2 = deref(q, c, c_ctx);

		if (has_vars(q, c2, q->latest_ctx, depth+1))
			return true;

		if (q->cycle_error)
			return false;

		c += c->nbr_cells;
	}

	return false;
}

static USE_RESULT pl_status fn_iso_ground_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !has_vars(q, p1, p1_ctx, 0);
}

static USE_RESULT pl_status fn_iso_cut_0(query *q)
{
	cut_me(q, false, false);
	return pl_success;
}

static USE_RESULT pl_status fn_local_cut_0(query *q)
{
	cut_me(q, true, false);
	return pl_success;
}

static USE_RESULT pl_status fn_soft_cut_0(query *q)
{
	cut_me(q, true, true);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_callable(p1);
}

static USE_RESULT pl_status fn_iso_char_code_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (is_variable(p2)) {
		const char *src = GET_STR(p1);
		size_t len = len_char_utf8(src);

		if (len != LEN_STR(p1))
			return throw_error(q, p1, "type_error", "character");

		int ch = peek_char_utf8(src);
		cell tmp;
		make_int(&tmp, ch);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_integer(p2)) {
		if (p2->val_num < 0)
			return throw_error(q, p2, "representation_error", "character_code");
	}

	if (is_variable(p1)) {
		char tmpbuf[256];
		put_char_utf8(tmpbuf, p2->val_num);
		cell tmp;
		make_small(&tmp, tmpbuf);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *src = GET_STR(p1);
	size_t len = len_char_utf8(src);

	if (len != LEN_STR(p1))
		return throw_error(q, p1, "type_error", "character");

	int ch = peek_char_utf8(src);
	return ch == p2->val_num;
}

static USE_RESULT pl_status fn_iso_atom_chars_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_iso_atom(p1) && !is_variable(p1))
		return throw_error(q, p1, "type_error", "atom");

	if (is_atom(p1) && !LEN_STR(p1) && is_nil(p2))
		return pl_success;

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

	if (is_variable(p2)) {
		cell tmp;
		may_error(make_stringn(&tmp, GET_STR(p1), LEN_STR(p1)));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	if (is_string(p2)) {
		cell tmp;
		may_error(make_cstringn(&tmp, GET_STR(p2), LEN_STR(p2)));
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head) && is_variable(p1))
				return throw_error(q, head, "type_error", "character");

			if (!is_atom(head) && !is_variable(head))
				return throw_error(q, head, "type_error", "character");

			if (is_atom(head)) {
				const char *src = GET_STR(head);
				size_t len = len_char_utf8(src);

				if (len < LEN_STR(head))
					return throw_error(q, head, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		STRING_INIT(tmpbuf);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			const char *src = GET_STR(head);
			STRING_CATn(tmpbuf, src, len_char_utf8(src));

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, "type_error", "list");

		cell tmp;
		may_error(make_cstring(&tmp, STRING_CSTR(tmpbuf)), STRING_DONE(tmpbuf));
		STRING_DONE(tmpbuf);
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	const char *src = GET_STR(p1);
	bool first = true;

	while (*src) {
		size_t len = len_char_utf8(src);
		cell tmp2;
		make_smalln(&tmp2, src, len);
		src += len;

		if (first) {
			first = false;
			allocate_list(q, &tmp2);
		} else
			append_list(q, &tmp2);
	}

	cell *tmp = end_list(q);
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_number_chars_2(query *q)
{
	GET_FIRST_ARG(p1,number_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head) && is_variable(p1))
				return throw_error(q, head, "type_error", "character");

			if (!is_atom(head) && !is_variable(head))
				return throw_error(q, head, "type_error", "character");

			if (is_atom(head)) {
				const char *src = GET_STR(head);
				size_t len = len_char_utf8(src);

				if (len < LEN_STR(head))
					return throw_error(q, head, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		char tmpbuf[256];
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head))
				return throw_error(q, head, "type_error", "atom");

			const char *src = GET_STR(head);
			int ch = *src;
			*dst++ = ch;

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, "type_error", "list");

		*dst = '\0';
		cell tmp;
		const char *src = tmpbuf;
		char *end = NULL;

		while (isspace(*src))
			src++;

		if ((src[0] == '0') && (src[1] == '\'')) {
			int val = peek_char_utf8(src+2);
			make_int(&tmp, val);
		} else if ((src[0] == '0') && (src[1] == 'x')) {
			char *end;
			int_t val = strtoll(src+2, &end, 16);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else if ((src[0] == '0') && (src[1] == 'o')) {
			char *end;
			int_t val = strtoll(src+2, &end, 8);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else if ((src[0] == '0') && (src[1] == 'b')) {
			char *end;
			int_t val = strtoll(src+2, &end, 2);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else {
			int_t val = strtoll(src, &end, 10);

			if (*end) {
				double f = strtod(src, &end);

				if (*end) {
					make_smalln(&tmp, end, 1);
					return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
				}

				make_float(&tmp, f);
			} else
				make_int(&tmp, val);
		}

		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[256];
	print_term_to_buf(q, tmpbuf, sizeof(tmpbuf), p1, p1_ctx, 1, 0, 0);
	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_iso_atom_codes_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_iso_atom(p1) && !is_variable(p1))
		return throw_error(q, p1, "type_error", "atom");

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

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_variable(p1))
				return throw_error(q, head, "type_error", "integer");

			if (!is_integer(head) && !is_variable(head))
				return throw_error(q, head, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		STRING_INIT(tmpbuf);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			int_t val = head->val_num;

			if (val < 0)
				return throw_error(q, head, "representation_error", "character_code");

			char ch[10];
			put_char_utf8(ch, val);
			STRING_CAT(tmpbuf, ch);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, "type_error", "list");

		cell tmp;
		may_error(make_cstring(&tmp, STRING_CSTR(tmpbuf)), STRING_DONE(tmpbuf));
		STRING_DONE(tmpbuf);
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	const char *tmpbuf = GET_STR(p1);
	const char *src = tmpbuf;
	cell tmp;
	make_int(&tmp, get_char_utf8(&src));
	allocate_list(q, &tmp);

	while (*src) {
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_number_codes_2(query *q)
{
	GET_FIRST_ARG(p1,number_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);
		int cnt = 0;

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!cnt && !is_integer(head) && is_variable(p1))
				return throw_error(q, head, "syntax_error", "integer");

			if (!is_integer(head) && is_variable(p1))
				return throw_error(q, head, "type_error", "integer");

			if (!is_integer(head) && !is_variable(head))
				return throw_error(q, head, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
			cnt++;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		char tmpbuf[256];
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head))
				return throw_error(q, head, "type_error", "integer");

			int val = head->val_num;

			if (val < 0)
				return throw_error(q, head, "representation_error", "character_code");

			*dst++ = val;

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, "type_error", "list");

		*dst = '\0';
		cell tmp;
		const char *src = tmpbuf;
		char *end = NULL;

		while (isspace(*src))
			src++;

		if ((src[0] == '0') && (src[1] == '\'')) {
			int val = peek_char_utf8(src+2);
			make_int(&tmp, val);
		} else if ((src[0] == '0') && (src[1] == 'x')) {
			char *end;
			int_t val = strtoll(src+2, &end, 16);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else if ((src[0] == '0') && (src[1] == 'o')) {
			char *end;
			int_t val = strtoll(src+2, &end, 8);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else if ((src[0] == '0') && (src[1] == 'b')) {
			char *end;
			int_t val = strtoll(src+2, &end, 2);
			make_int(&tmp, val);

			if (*end) {
				make_smalln(&tmp, end, 1);
				return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
			}
		} else {
			int_t val = strtoll(src, &end, 10);

			if (*end) {
				double f = strtod(src, &end);

				if (*end) {
					make_smalln(&tmp, end, 1);
					return throw_error(q, &tmp, "syntax_error", "non_numeric_character");
				}

				make_float(&tmp, f);
			} else
				make_int(&tmp, val);
		}

		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[256];
	print_term_to_buf(q, tmpbuf, sizeof(tmpbuf), p1, p1_ctx, 1, 0, 0);
	const char *src = tmpbuf;
	cell tmp;
	make_int(&tmp, *src);
	allocate_list(q, &tmp);

	while (*++src) {
		make_int(&tmp, *src);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_sub_atom_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p3,integer_or_var);
	GET_NEXT_ARG(p4,integer_or_var);
	GET_NEXT_ARG(p5,atom_or_var);
	size_t before = 0, len = 0;

	if (is_integer(p2) && (p2->val_num < 0))
		return throw_error(q, p2, "domain_error", "not_less_than_zero");

	if (is_integer(p3) && (p3->val_num < 0))
		return throw_error(q, p3, "domain_error", "not_less_than_zero");

	if (is_integer(p4) && (p4->val_num < 0))
		return throw_error(q, p4, "domain_error", "not_less_than_zero");

	if (!q->retry) {
		may_error(make_choice(q));

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
				return pl_failure;
			}
		}
	}

	if (len > (LEN_STR(p1)-before)) {
		before++;
		len = 0;
	}

	if (before > LEN_STR(p1)) {
		drop_choice(q);
		return pl_failure;
	}

	const size_t len_p1 = LEN_STR(p1);

	for (size_t i = before; i <= len_p1; i++) {
		for (size_t j = len; j <= (len_p1-i); j++) {
			set_params(q, i, j+1);
			may_error(make_choice(q));
			cell tmp;
			make_int(&tmp, i);

			if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			make_int(&tmp, j);

			if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			make_int(&tmp, len_p1-i-j);

			if (!unify(q, p4, p4_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			may_error(make_slice(q, &tmp, p1, i, j));

			if (is_atom(p5) && !strcmp(GET_STR(p5), GET_STR(&tmp))) {
				DECR_REF(&tmp);
				return pl_success;
			}

			if (is_atom(p5) && strcmp(GET_STR(p5), GET_STR(&tmp))) {
				DECR_REF(&tmp);
				retry_choice(q);
				continue;
			}

			if (!unify(q, p5, p5_ctx, &tmp, q->st.curr_frame)) {
				DECR_REF(&tmp);
				retry_choice(q);
				continue;
			}

			DECR_REF(&tmp);
			return pl_success;
		}

		len = 0;
	}

	drop_choice(q);
	return pl_failure;
}

// NOTE: this just handles the mode(-,-,+) case...

static USE_RESULT pl_status do_atom_concat_3(query *q)
{
	if (!q->retry) {
		GET_FIRST_ARG(p1,variable);
		GET_NEXT_ARG(p2,variable);
		GET_NEXT_ARG(p3,atom);
		cell tmp;
		make_literal(&tmp, g_empty_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		set_var(q, p2, p2_ctx, p3, q->st.curr_frame);
		may_error(make_choice(q));
		return pl_success;
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
	cell tmp;
	may_error(make_cstring(&tmp, dst1), free(dst1));
	free(dst1);
	reset_value(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	may_error(make_cstring(&tmp, dst2), free(dst2));
	reset_value(q, p2_raw, p2_raw_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	free(dst2);

	if (!done)
		may_error(make_choice(q));

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atom_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1) && is_variable(p2))
		return do_atom_concat_3(q);

	if (is_variable(p3)) {
		if (!is_iso_atom(p1))
			return throw_error(q, p1, "type_error", "atom");

		if (!is_iso_atom(p2))
			return throw_error(q, p2, "type_error", "atom");

		const char *src1, *src2;
		size_t len1, len2;
		char tmpbuf1[256], tmpbuf2[256];

		if (is_atom(p1)) {
			src1 = GET_STR(p1);
			len1 = LEN_STR(p1);
		} else {
			print_term_to_buf(q, tmpbuf1, sizeof(tmpbuf1), p1, p1_ctx, 1, 0, 0);
			src1 = tmpbuf1;
			len1 = strlen(tmpbuf1);
		}

		if (is_atom(p2)) {
			src2 = GET_STR(p2);
			len2 = LEN_STR(p2);
		} else {
			print_term_to_buf(q, tmpbuf2, sizeof(tmpbuf2), p2, p2_ctx, 1, 0, 0);
			src2 = tmpbuf2;
			len2 = strlen(tmpbuf2);
		}

		STRING_INIT(tmpbuf);
		STRING_CAT2n(tmpbuf, src1, len1, src2, len2);
		cell tmp;
		may_error(make_cstringn(&tmp, STRING_CSTR(tmpbuf), STRING_LEN(tmpbuf)), STRING_DONE(tmpbuf));
		STRING_DONE(tmpbuf);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	if (is_variable(p1)) {
		if (strcmp(GET_STR(p3)+(LEN_STR(p3)-LEN_STR(p2)), GET_STR(p2)))
			return pl_failure;

		char *dst = strndup(GET_STR(p3), LEN_STR(p3)-LEN_STR(p2));
		cell tmp;
		may_error(make_cstring(&tmp, dst), free(dst));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		free(dst);
		return pl_success;
	}

	if (is_variable(p2)) {
		if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
			return pl_failure;

		char *dst = strdup(GET_STR(p3)+LEN_STR(p1));
		cell tmp;
		may_error (make_cstring(&tmp, dst), free(dst));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		free(dst);
		return pl_success;
	}

	if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
		return pl_failure;

	if (strcmp(GET_STR(p3)+LEN_STR(p1), GET_STR(p2)))
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atom_length_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);

	if (!is_iso_atom(p1))
		return throw_error(q, p1, "type_error", "atom");

	if (is_integer(p2) && (p2->val_num < 0))
		return throw_error(q, p2, "domain_error", "not_less_than_zero");

	const char *p = GET_STR(p1);
	size_t len = substrlen_utf8(p, p + LEN_STR(p1));
	cell tmp;
	make_int(&tmp, len);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static int new_stream()
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		if (!g_streams[i].fp)
			return i;
	}

	return -1;
}

static int get_named_stream(const char *name)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &g_streams[i];

		if (str->name && !strcmp(str->name, name))
			return i;

		if (str->filename && !strcmp(str->filename, name))
			return i;
	}

	return -1;
}

static int get_stream(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_stream(GET_STR(p1));

		if (n < 0) {
			//DISCARD_RESULT throw_error(q, p1, "type_error", "stream");
			return -1;
		}

		return n;
	}

	if (!is_integer(p1) || !(p1->flags&FLAG_STREAM)) {
		//DISCARD_RESULT throw_error(q, p1, "type_error", "stream");
		return -1;
	}

	if ((p1->val_num < 0) || (p1->val_num >= MAX_STREAMS)) {
		//DISCARD_RESULT throw_error(q, p1, "type_error", "stream");
		return -1;
	}

	if (!g_streams[p1->val_num].fp) {
		//DISCARD_RESULT throw_error(q, p1, "existence_error", "stream");
		return -1;
	}

	return p1->val_num;
}

static bool is_closed_stream(cell *p1)
{
	if (!(p1->flags&FLAG_STREAM))
		return false;

	if ((p1->val_num < 0) || (p1->val_num >= MAX_STREAMS))
		return false;

	if (g_streams[p1->val_num].fp)
		return false;

	return true;
}

static USE_RESULT pl_status fn_iso_current_input_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->st.m->pl->current_input);
		tmp.flags |= FLAG_STREAM | FLAG_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->st.m->pl->current_input ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_current_output_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->st.m->pl->current_output);
		tmp.flags |= FLAG_STREAM | FLAG_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->st.m->pl->current_output ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_set_input_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	q->st.m->pl->current_input = n;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_set_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	q->st.m->pl->current_output = n;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_set_stream_position_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	if (!is_integer(p1))
		return throw_error(q, p1, "domain_error", "stream_position");

	if (fseeko(str->fp, p1->val_num, SEEK_SET))
		return throw_error(q, p1, "domain_error", "position");

	return pl_success;
}

static char *chars_list_to_string(query *q, cell *p_chars, idx_t p_chars_ctx, size_t len)
{
	char *tmp = malloc(len+1);
	ensure(tmp);
	char *dst = tmp;
	LIST_HANDLER(p_chars);

	while (is_list(p_chars)) {
		cell *h = LIST_HEAD(p_chars);
		h = deref(q, h, p_chars_ctx);

		if (is_integer(h)) {
			int ch = h->val_num;
			dst += put_char_utf8(dst, ch);
		} else {
			const char *p = GET_STR(h);
			int ch = peek_char_utf8(p);
			dst += put_char_utf8(dst, ch);
		}

		p_chars = LIST_TAIL(p_chars);
		p_chars = deref(q, p_chars, p_chars_ctx);
		p_chars_ctx = q->latest_ctx;
	}

	*dst = '\0';
	return tmp;
}

static void compare_and_zero(uint64_t v1, uint64_t *v2, uint64_t *v)
{
	if (v1 != *v2) {
		*v2 = v1;
		*v = 0;
	}
}

#define MASK_FINAL 0x0000FFFFFFFFFFFF // Final 48 bits

static void uuid_gen(prolog *pl, uuid *u)
{
#ifdef NDEBUG
	if (!pl->seed)
		pl->seed = (uint64_t)time(0) & MASK_FINAL;
#else
	if (!pl->seed)
		pl->seed = 0xdeadbeefULL & MASK_FINAL;
#endif

	uint64_t now = get_time_in_usec();
	compare_and_zero(now, &pl->s_last, &pl->s_cnt);
	u->u1 = now;
	u->u2 = pl->s_cnt++;
	u->u2 <<= 48;
	u->u2 |= pl->seed;
}

static char *uuid_to_buf(const uuid *u, char *buf, size_t buflen)
{
	snprintf(buf, buflen, "%016llX-%04llX-%012llX",
		 (unsigned long long)u->u1,
		 (unsigned long long)(u->u2 >> 48),
		 (unsigned long long)(u->u2 & MASK_FINAL));

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
	char *dst;
	q->quoted = 2;

	switch(l) {
	case LOG_ASSERTA:
		dst = print_term_to_strbuf(q, r->t.cells, q->st.curr_frame, 1);
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$a_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ASSERTZ:
		dst = print_term_to_strbuf(q, r->t.cells, q->st.curr_frame, 1);
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$z_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ERASE:
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$e_'('%s').\n", tmpbuf);
		break;
	}

	q->quoted = save;
}

static USE_RESULT pl_status do_retract(query *q, cell *p1, idx_t p1_ctx, int is_retract)
{
	cell *head = get_head(p1);

	if (is_variable(head))
		return throw_error(q, head, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_callable(head))
		return throw_error(q, head, "type_error", "callable");

	pl_status match;

	if (check_rule(p1))
		match = match_rule(q, p1, p1_ctx);
	else
		match = match_clause(q, p1, p1_ctx, is_retract);

	if (match != pl_success)
		return match;

	clause *r = q->st.curr_clause2;
	bool last_match = !q->st.curr_clause2->next && (is_retract == DO_RETRACT);
	stash_me(q, &r->t, last_match);
	add_to_dirty_list(q, r);

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ERASE);

	return pl_success;
}

static void add_stream_properties(query *q, int n)
{
	stream *str = &g_streams[n];
	char tmpbuf[1024*8];
	char *dst = tmpbuf;
	*dst = '\0';
	off_t pos = ftello(str->fp);
	bool at_end_of_file = false;

	if (!str->at_end_of_file && (n > 2)) {
		if (str->p) {
			if (str->p->srcptr && *str->p->srcptr) {
				int ch = get_char_utf8((const char**)&str->p->srcptr);
				str->ungetch = ch;
			}
		}

		int ch = str->ungetch ? str->ungetch : net_getc(str);

		if (str->ungetch)
			;
		else if (feof(str->fp) || ferror(str->fp)) {
			clearerr(str->fp);

			if (str->eof_action != eof_action_reset)
				at_end_of_file = true;
		} else
			str->ungetch = ch;
	}

	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, alias('%s')).\n", n, str->name);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file_name('%s')).\n", n, str->filename);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, mode(%s)).\n", n, str->mode);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, encoding(%s)).\n", n, "utf8");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, type(%s)).\n", n, str->binary ? "binary" : "text");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, line_count(%i)).\n", n, str->p ? str->p->line_nbr : 1);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, position(%llu)).\n", n, (unsigned long long)(pos != -1 ? pos : 0));
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, reposition(%s)).\n", n, (n < 3) || str->socket ? "false" : "true");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, end_of_stream(%s)).\n", n, str->at_end_of_file ? "past" : at_end_of_file ? "at" : "not");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, eof_action(%s)).\n", n, str->eof_action == eof_action_eof_code ? "eof_code" : str->eof_action == eof_action_error ? "error" : str->eof_action == eof_action_reset ? "reset" : "none");

	if (!strcmp(str->mode, "read"))
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, input).\n", n);
	else
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, output).\n", n);

#ifdef _WIN32
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, newline(dos)).\n", n);
#else
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, newline(posix)).\n", n);
#endif

	parser *p = create_parser(q->st.m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	parser_tokenize(p, false, false);
	destroy_parser(p);
}

static void del_stream_properties(query *q, int n)
{
	cell *tmp = alloc_on_heap(q, 3);
	make_literal(tmp+0, g_stream_property_s);
	make_int(tmp+1, n);
	make_variable(tmp+2, g_anon_s);
	tmp[2].var_nbr = create_vars(q, 1);
	tmp->nbr_cells = 3;
	tmp->arity = 2;
	q->retry = QUERY_OK;

	predicate *h = search_predicate(q->st.m, tmp);

	if (!h) {
		DISCARD_RESULT throw_error(q, tmp, "existence_error", "procedure");
		return;
	}

	while (do_retract(q, tmp, q->st.curr_frame, DO_STREAM_RETRACT)) {
		if (q->did_throw)
			return;

		q->retry = QUERY_RETRY;
		retry_choice(q);
	}

	q->retry = QUERY_OK;
}

static USE_RESULT pl_status do_stream_property(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	cell *c = p1 + 1;
	c = deref(q, c, p1_ctx);

	if (!strcmp(GET_STR(p1), "file_name")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->filename));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "alias")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->name));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "mode")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->mode));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "type")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->binary ? "binary" : "text"));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "reposition")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->socket || (n <= 2) ? "false" : "true"));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "encoding")) {
		cell tmp;
		may_error(make_cstring(&tmp, "utf8"));
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "newline")) {
		cell tmp;
#ifdef _WIN32
		may_error(make_cstring(&tmp, "dos"));
#else
		may_error(make_cstring(&tmp, "unix"));
#endif
		pl_status ok = unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!strcmp(GET_STR(p1), "input"))
		return !strcmp(str->mode, "read");

	if (!strcmp(GET_STR(p1), "output"))
		return strcmp(str->mode, "read");

	if (!strcmp(GET_STR(p1), "eof_action") && is_stream(pstr)) {
		cell tmp;

		if (str->eof_action == eof_action_eof_code)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "eof_code"));
		else if (str->eof_action == eof_action_error)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "error"));
		else if (str->eof_action == eof_action_reset)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "reset"));
		else
			make_literal(&tmp, index_from_pool(q->st.m->pl, "none"));

		return unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
	}

	if (!strcmp(GET_STR(p1), "end_of_stream") && is_stream(pstr)) {
		bool at_end_of_file = false;

		if (!str->at_end_of_file && (n > 2)) {
			if (str->p) {
				if (str->p->srcptr && *str->p->srcptr) {
					int ch = get_char_utf8((const char**)&str->p->srcptr);
					str->ungetch = ch;
				}
			}

			int ch = str->ungetch ? str->ungetch : net_getc(str);

			if (str->ungetch)
				;
			else if (feof(str->fp) || ferror(str->fp)) {
				clearerr(str->fp);

				if (str->eof_action != eof_action_reset)
					at_end_of_file = true;
			} else
				str->ungetch = ch;
		}

		cell tmp;

		if (str->at_end_of_file)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "past"));
		else if (at_end_of_file)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "at"));
		else
			make_literal(&tmp, index_from_pool(q->st.m->pl, "not"));

		return unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
	}

	if (!strcmp(GET_STR(p1), "position") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, ftello(str->fp));
		return unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
	}

	if (!strcmp(GET_STR(p1), "line_count") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, str->p?str->p->line_nbr:1);
		return unify(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
	}

	return pl_failure;
}

static void clear_streams_properties(query *q)
{
	cell tmp;
	make_literal(&tmp, g_stream_property_s);
	tmp.nbr_cells = 1;
	tmp.arity = 2;

	predicate *h = search_predicate(q->st.m, &tmp);

	if (h) {
		for (clause *r = h->head; r;) {
			clause *save = r->next;
			add_to_dirty_list(q, r);
			r = save;
		}

		h->cnt = 0;
	}
}

static const char *s_properties =
	"alias,file_name,mode,encoding,type,line_count,"			\
	"position,reposition,end_of_stream,eof_action,"				\
	"input,output,newline";

static USE_RESULT pl_status fn_iso_stream_property_2(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);

	if (!is_stream_or_var(pstr)) {
		if (is_closed_stream(pstr))
			return throw_error(q, pstr, "existence_error", "stream");
		else
			return throw_error(q, pstr, "domain_error", "stream");
	}

	if (p1->arity > 1)
		return throw_error(q, p1, "domain_error", "stream_property");

	if (!is_variable(p1) && !is_callable(p1))
		return throw_error(q, p1, "domain_error", "stream_property");

	if (!is_variable(pstr) && !is_variable(p1))
		return do_stream_property(q);

	if (!q->retry) {
		clear_streams_properties(q);

		for (int i = 0; i < MAX_STREAMS; i++) {
			if (!g_streams[i].fp)
				continue;

			stream *str = &g_streams[i];

			if (!str->socket)
				add_stream_properties(q, i);
		}
	}

	cell *tmp = deep_clone_to_tmp(q, q->st.curr_cell, q->st.curr_frame);
	tmp->val_off = g_stream_property_s;

	if (!match_clause(q, tmp, q->st.curr_frame, DO_CLAUSE)) {
		clear_streams_properties(q);

		if (is_callable(p1) && !strstr(s_properties, GET_STR(p1)))
			return throw_error(q, p1, "domain_error", "stream_property");

		return pl_failure;
	}

	term *t = &q->st.curr_clause2->t;
	GET_FIRST_ARG(pstrx,any);

	if (is_integer(pstrx))
		pstrx->flags |= FLAG_STREAM | FLAG_HEX;

	stash_me(q, t, false);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_open_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	const char *filename;
	const char *mode = GET_STR(p2);
	int n = new_stream();
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, "resource_error", "too_many_streams");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else if (is_atom(p1))
		filename = GET_STR(p1);
	else
		return throw_error(q, p1, "domain_error", "source_sink");

	stream *str = &g_streams[n];
	str->filename = strdup(filename);
	str->name = strdup(filename);
	str->mode = strdup(mode);
	str->eof_action = eof_action_eof_code;

	if (!strcmp(mode, "read"))
		str->fp = fopen(filename, "r");
	else if (!strcmp(mode, "write"))
		str->fp = fopen(filename, "w");
	else if (!strcmp(mode, "append"))
		str->fp = fopen(filename, "a");
	else if (!strcmp(mode, "update"))
		str->fp = fopen(filename, "r+");
	else
		return throw_error(q, p2, "domain_error", "io_mode");

	free(src);

	if (!str->fp)
		return throw_error(q, p1, "existence_error", "source_sink");

	cell *tmp = alloc_on_heap(q, 1);
	ensure(tmp);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p3, p3_ctx, tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_open_4(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	const char *mode = GET_STR(p2);
	int n = new_stream();
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, "resource_error", "too_many_streams");

	const char *filename;
	stream *oldstr = NULL;

	if (is_structure(p1) && (p1->arity == 1) && !strcmp(GET_STR(p1), "stream")) {
		int oldn = get_stream(q, p1+1);

		if (oldn < 0)
			return throw_error(q, p1, "type_error", "not_a_stream");

		stream *oldstr = &g_streams[oldn];
		filename = oldstr->filename;
	} else if (is_atom(p1))
		filename = GET_STR(p1);
	else
		return throw_error(q, p1, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	}

	stream *str = &g_streams[n];
	str->filename = strdup(filename);
	str->name = strdup(filename);
	str->mode = strdup(mode);
	str->eof_action = eof_action_eof_code;
	int binary = 0;

#if USE_MMAP
	cell *mmap_var = NULL;
	idx_t mmap_ctx = 0;
#endif

	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);

		if (is_variable(c))
			return throw_error(q, c, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_structure(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);

			if (!is_atom(name) && strcmp(GET_STR(c), "mmap"))
				return throw_error(q, c, "domain_error", "stream_option");

			if (get_named_stream(GET_STR(name)) >= 0)	// ???????
				return throw_error(q, c, "permission_error", "open,source_sink");

			if (!strcmp(GET_STR(c), "mmap")) {
#if USE_MMAP
				mmap_var = name;
				mmap_var = deref(q, mmap_var, q->latest_ctx);
				mmap_ctx = q->latest_ctx;
#endif
			} else if (!strcmp(GET_STR(c), "alias")) {
				free(str->name);
				str->name = strdup(GET_STR(name));
			} else if (!strcmp(GET_STR(c), "type")) {
				if (is_atom(name) && !strcmp(GET_STR(name), "binary")) {
					str->binary = true;
					binary = 1;
				} else if (is_atom(name) && !strcmp(GET_STR(name), "text"))
					binary = 0;
			} else if (!strcmp(GET_STR(c), "eof_action")) {
				if (is_atom(name) && !strcmp(GET_STR(name), "error")) {
					str->eof_action = eof_action_error;
				} else if (is_atom(name) && !strcmp(GET_STR(name), "eof_code")) {
					str->eof_action = eof_action_eof_code;
				} else if (is_atom(name) && !strcmp(GET_STR(name), "reset")) {
					str->eof_action = eof_action_reset;
				}
			}
		} else
			return throw_error(q, c, "domain_error", "stream_option");

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_variable(p4))
			return throw_error(q, p4, "instantiation_error", "args_not_sufficiently_instantiated");
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
		else
			return throw_error(q, p2, "domain_error", "io_mode");
	} else {
		if (!strcmp(mode, "read"))
			str->fp = fopen(filename, binary?"rb":"r");
		else if (!strcmp(mode, "write"))
			str->fp = fopen(filename, binary?"wb":"w");
		else if (!strcmp(mode, "append"))
			str->fp = fopen(filename, binary?"ab":"a");
		else if (!strcmp(mode, "update"))
			str->fp = fopen(filename, binary?"rb+":"r+");
		else
			return throw_error(q, p2, "domain_error", "io_mode");
	}

	free(src);

	if (!str->fp)
		return throw_error(q, p1, "existence_error", "source_sink");

#if USE_MMAP
	int prot = 0;

	if (!strcmp(mode, "read"))
		prot = PROT_READ;
	else
		prot = PROT_WRITE;

	if (mmap_var && is_variable(mmap_var)) {
		struct stat st = {0};
		stat(filename, &st);
		size_t len = st.st_size;
		int fd = fileno(str->fp);
		void *addr = mmap(0, len, prot, MAP_PRIVATE, fd, 0);
		cell tmp = {0};
		tmp.val_type = TYPE_CSTRING;
		tmp.flags = FLAG_BLOB | FLAG_STRING | FLAG2_STATIC;
		tmp.nbr_cells = 1;
		tmp.arity = 2;
		tmp.val_str = addr;
		tmp.str_len = len;
		set_var(q, mmap_var, mmap_ctx, &tmp, q->st.curr_frame);
	}
#endif

	cell *tmp = alloc_on_heap(q, 1);
	ensure(tmp);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p3, p3_ctx, tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if ((str->fp == stdin)
		|| (str->fp == stdout)
		|| (str->fp == stderr))
		return pl_success;

	if (q->st.m->pl->current_input == n)
		q->st.m->pl->current_input = 0;

	if (q->st.m->pl->current_output == n)
		q->st.m->pl->current_output = 1;

	if (q->st.m->pl->current_error == n)
		q->st.m->pl->current_error = 2;

	if (str->p)
		destroy_parser(str->p);

	if (!str->socket)
		del_stream_properties(q, n);

	net_close(str);
	free(str->filename);
	free(str->mode);
	free(str->data);
	free(str->name);
	memset(str, 0, sizeof(stream));
	return pl_success;
}

static USE_RESULT pl_status fn_iso_close_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,list_or_nil);

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_structure(h)
			|| strcmp(GET_STR(h), "force")
			|| strcmp(GET_STR(h+1), "true"))
			return throw_error(q, h, "domain_error", "close_option");

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (is_variable(p1))
		return throw_error(q, p1, "instantiation_error", "close_option");

	if (!is_nil(p1))
		return throw_error(q, p1, "type_error", "list");

	return fn_iso_close_1(q);
}

static USE_RESULT pl_status fn_iso_at_end_of_stream_0(query *q)
{
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return pl_failure;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return pl_success;
}

static USE_RESULT pl_status fn_iso_at_end_of_stream_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return pl_failure;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return pl_success;
}

static USE_RESULT pl_status fn_iso_flush_output_0(query *q)
{
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];
	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_flush_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_nl_0(query *q)
{
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];
	fputc('\n', str->fp);
	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_nl_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	fputc('\n', str->fp);
	fflush(str->fp);
	return !ferror(str->fp);
}

static bool collect_vars(query *q, cell *p1, idx_t p1_ctx, idx_t nbr_cells, int depth)
{
	if (depth > MAX_DEPTH)
		return false;

	for (unsigned i = 0; i < nbr_cells;) {
		cell *c = deref(q, p1, p1_ctx);
		int found = 0;

		if (is_structure(c)) {
			collect_vars(q, c+1, q->latest_ctx, c->nbr_cells-1, depth+1);
		} else if (is_variable(c)) {
			for (unsigned idx = 0; idx < q->st.m->pl->tab_idx; idx++) {
				if ((q->st.m->pl->tab1[idx] == q->latest_ctx) && (q->st.m->pl->tab2[idx] == c->var_nbr)) {
					q->st.m->pl->tab4[idx]++;
					found = 1;
					break;
				}
			}

			if (!found) {
				q->st.m->pl->tab1[q->st.m->pl->tab_idx] = q->latest_ctx;
				q->st.m->pl->tab2[q->st.m->pl->tab_idx] = c->var_nbr;
				q->st.m->pl->tab3[q->st.m->pl->tab_idx] = c->val_off;
				q->st.m->pl->tab4[q->st.m->pl->tab_idx] = 1;
				q->st.m->pl->tab5[q->st.m->pl->tab_idx] = is_anon(c) ? 1 : 0;
				q->st.m->pl->tab_idx++;
			}
		}

		i += p1->nbr_cells;
		p1 += p1->nbr_cells;
	}

	return true;
}

static bool parse_read_params(query *q, parser *p, cell *c, cell **vars, idx_t *vars_ctx, cell **varnames, idx_t *varnames_ctx, cell **sings, idx_t *sings_ctx)
{
	if (!is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, "domain_error", "read_option");
		return false;
	}

	cell *c1 = deref(q, c+1, q->latest_ctx);

	if (!strcmp(GET_STR(c), "character_escapes")) {
		if (is_literal(c1))
			p->flag.character_escapes = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "double_quotes")) {
		if (is_literal(c1)) {
			if (!strcmp(GET_STR(c1), "atom")) {
				p->flag.double_quote_codes = p->flag.double_quote_chars = false;
				p->flag.double_quote_atom = true;
			} else if (!strcmp(GET_STR(c1), "chars")) {
				p->flag.double_quote_atom = p->flag.double_quote_codes = false;
				p->flag.double_quote_chars = true;
			} else if (!strcmp(GET_STR(c1), "codes")) {
				p->flag.double_quote_atom = p->flag.double_quote_chars = false;
				p->flag.double_quote_codes = true;
			}
		}
	} else if (!strcmp(GET_STR(c), "variables")) {
		if (is_variable(c1)) {
			cell *v = c1;
			if (vars) *vars = v;
			if (vars_ctx) *vars_ctx = q->latest_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, "domain_error", "read_option");
			return false;
		}
	} else if (!strcmp(GET_STR(c), "variable_names")) {
		if (is_variable(c1)) {
			cell *v = c1;
			if (varnames) *varnames = v;
			if (varnames_ctx) *varnames_ctx = q->latest_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, "domain_error", "read_option");
			return false;
		}
	} else if (!strcmp(GET_STR(c), "singletons")) {
		if (is_variable(c1)) {
			cell *v = c1;
			if (sings) *sings = v;
			if (sings_ctx) *sings_ctx = q->latest_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, "domain_error", "read_option");
			return false;
		}
	} else {
		DISCARD_RESULT throw_error(q, c, "domain_error", "read_option");
		return false;
	}

	return true;
}

static USE_RESULT pl_status do_read_term(query *q, stream *str, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, char *src)
{
	if (!str->p)
		str->p = create_parser(q->st.m);

	parser *p = str->p;
	p->fp = str->fp;
	parser_reset(p);
	p->one_shot = true;
	p->error = false;
	cell *vars = NULL, *varnames = NULL, *sings = NULL;
	idx_t vars_ctx = 0, varnames_ctx = 0, sings_ctx = 0;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);

		if (is_variable(h))
			return throw_error(q, p2, "instantiation_error", "read_option");

		if (!parse_read_params(q, p, h, &vars, &vars_ctx, &varnames, &varnames_ctx, &sings, &sings_ctx))
			return pl_success;

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2))
		return throw_error(q, p2, "instantiation_error", "read_option");

	if (!is_nil(p2))
		return throw_error(q, p2, "type_error", "list");

	for (;;) {
#if 0
		if (isatty(fileno(str->fp)) && !src) {
			printf("| ");
			fflush(str->fp);
		}
#endif

		if (!src && (!p->srcptr || !*p->srcptr || (*p->srcptr == '\n'))) {
			if (getline(&p->save_line, &p->n_line, str->fp) == -1) {
				if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
					clearerr(str->fp);
					do_yield_0(q, 1);
					return pl_failure;
				}

				str->at_end_of_file = str->eof_action != eof_action_reset;

				if (str->eof_action == eof_action_reset)
					clearerr(str->fp);

				if (vars) {
					cell tmp;
					make_literal(&tmp, g_nil_s);
					set_var(q, vars, vars_ctx, &tmp, q->st.curr_frame);
				}

				if (varnames) {
					cell tmp;
					make_literal(&tmp, g_nil_s);
					set_var(q, varnames, varnames_ctx, &tmp, q->st.curr_frame);
				}

				if (sings) {
					cell tmp;
					make_literal(&tmp, g_nil_s);
					set_var(q, sings, sings_ctx, &tmp, q->st.curr_frame);
				}

				//destroy_parser(p);
				//str->p = NULL;

				cell tmp;
				make_literal(&tmp, g_eof_s);
				return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			}

#if 0
			if (*p->save_line && (p->save_line[strlen(p->save_line)-1] == '\n'))
				p->save_line[strlen(p->save_line)-1] = '\0';

			if (*p->save_line && (p->save_line[strlen(p->save_line)-1] == '\r'))
				p->save_line[strlen(p->save_line)-1] = '\0';
#endif

			if (!strlen(p->save_line) || (*p->save_line == '\r') || (*p->save_line == '\n')) {
				p->line_nbr++;
				continue;
			}

			p->srcptr = p->save_line;
		} else if (src)
			p->srcptr = src;

		break;
	}

	frame *g = GET_CURR_FRAME();
	p->read_term = g->nbr_vars;
	p->do_read_term = true;
	p->line_nbr = 0;
	parser_tokenize(p, false, false);
	p->do_read_term = false;
	p->read_term = 0;

	if (p->error) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return throw_error(q, &tmp, "syntax_error", "read_term");
	}

	if (!p->t->cidx) {
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	term_xref(p, p->t, NULL);

	if (p->nbr_vars) {
		if (!create_vars(q, p->nbr_vars))
			return throw_error(q, p1, "resource_error", "too_many_vars");
	}

	q->st.m->pl->tab_idx = 0;

	if (p->nbr_vars)
		collect_vars(q, p->t->cells, q->st.curr_frame, p->t->cidx-1, 0);

	if (vars) {
		unsigned cnt = q->st.m->pl->tab_idx;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->st.m->pl->tab_idx; i++) {
				make_literal(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*2)+1;
				cell v;
				make_variable(&v, q->st.m->pl->tab3[i]);
				v.var_nbr = q->st.m->pl->tab2[i];
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			ensure(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, vars, vars_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, vars, vars_ctx, &tmp, q->st.curr_frame);
		}
	}

	if (varnames) {
		unsigned cnt = 0;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab5[i])
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->st.m->pl->tab_idx; i++) {
				if (q->st.m->pl->tab5[i])
					continue;

				make_literal(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_literal(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				v.fn = fn_iso_unify_2;
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_literal(&v, q->st.m->pl->tab3[i]);
				tmp[idx++] = v;
				make_variable(&v, q->st.m->pl->tab3[i]);
				v.var_nbr = q->st.m->pl->tab2[i];
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			ensure(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, varnames, varnames_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, varnames, varnames_ctx, &tmp, q->st.curr_frame);
		}
	}

	if (sings) {
		unsigned cnt = 0;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		ensure(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab4[i] != 1)
				continue;

			if (varnames && (q->st.m->pl->tab5[i]))
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->st.m->pl->tab_idx; i++) {
				if (q->st.m->pl->tab4[i] != 1)
					continue;

				if (varnames && (q->st.m->pl->tab5[i]))
					continue;

				make_literal(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_literal(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				v.fn = fn_iso_unify_2;
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_literal(&v, q->st.m->pl->tab3[i]);
				tmp[idx++] = v;
				make_variable(&v, q->st.m->pl->tab3[i]);
				v.var_nbr = q->st.m->pl->tab2[i];
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			ensure(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, sings, sings_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, sings, sings_ctx, &tmp, q->st.curr_frame);
		}
	}

	cell *tmp = alloc_on_heap(q, p->t->cidx-1);
	ensure(tmp);
	safe_copy_cells(tmp, p->t->cells, p->t->cidx-1);
	pl_status ok = unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	clear_term(p->t);
	return ok;
}

static USE_RESULT pl_status fn_iso_read_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static USE_RESULT pl_status fn_iso_read_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static USE_RESULT pl_status fn_iso_read_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT pl_status fn_iso_read_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT pl_status fn_iso_write_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	print_term_to_stream(q, str, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	print_term_to_stream(q, str, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_writeq_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	int saveq = q->quoted;
	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->quoted = saveq;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_writeq_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	int save = q->quoted;
	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->quoted = save;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_canonical_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_canonical_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static bool parse_write_params(query *q, cell *c, cell **vnames, idx_t *vnames_ctx)
{
	if (is_variable(c)) {
		DISCARD_RESULT throw_error(q, c, "instantiation_error", "write_option");
		return false;
	}

	if (!is_literal(c) || !is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
		return false;
	}

	cell *c1 = deref(q,c+1, q->latest_ctx);
	idx_t c1_ctx = q->latest_ctx;

	if (is_variable(c1)) {
		DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
		return false;
	}

	if (!strcmp(GET_STR(c), "max_depth")) {
		if (is_integer(c1))
			q->max_depth = c[1].val_num;
	} else if (!strcmp(GET_STR(c), "fullstop")) {
		if (!is_literal(c1) || (strcmp(GET_STR(c1), "true") && strcmp(GET_STR(c1), "false"))) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		q->fullstop = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "nl")) {
		if (!is_literal(c1) || (strcmp(GET_STR(c1), "true") && strcmp(GET_STR(c1), "false"))) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		q->nl = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "quoted")) {
		if (!is_literal(c1) || (strcmp(GET_STR(c1), "true") && strcmp(GET_STR(c1), "false"))) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		q->quoted = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "ignore_ops")) {
		if (!is_literal(c1) || (strcmp(GET_STR(c1), "true") && strcmp(GET_STR(c1), "false"))) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		q->ignore_ops = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "numbervars")) {
		if (!is_literal(c1) || (strcmp(GET_STR(c1), "true") && strcmp(GET_STR(c1), "false"))) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		q->numbervars = !strcmp(GET_STR(c1), "true");
	} else if (!strcmp(GET_STR(c), "variable_names")) {
		if (!is_list_or_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
			return false;
		}

		// TODO: write_term variable_names

		cell *c1_orig = c1;
		idx_t c1_orig_ctx = c1_ctx;
		LIST_HANDLER(c1);

		while (is_list(c1)) {
			cell *h = LIST_HEAD(c1);
			h = deref(q, h, c1_ctx);

			if (!is_structure(h)) {
				DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
				return false;
			}

			if (is_literal(h)) {
				if (!is_atom(h+1)) {
					DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
					return false;
				}
				if (!is_variable(h+2)) {
					DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
					return false;
				}
			}

			c1 = LIST_TAIL(c1);
			c1 = deref(q, c1, c1_ctx);
			c1_ctx = q->latest_ctx;
		}

		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1_orig, "instantiation_error", "write_option");
			return false;
		}

		if (!is_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, "type_error", "list");
			return false;
		}

		if (vnames) *vnames = c1_orig;
		if (vnames_ctx) *vnames_ctx = c1_orig_ctx;
	} else {
		DISCARD_RESULT throw_error(q, c, "domain_error", "write_option");
		return false;
	}

	return true;
}

static USE_RESULT pl_status fn_iso_write_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	q->flag = q->st.m->flag;
	cell *p2_orig = p2, *vnames;
	idx_t vnames_ctx;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);

		if (!parse_write_params(q, h, &vnames, &vnames_ctx))
			return pl_success;

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2))
		return throw_error(q, p2_orig, "instantiation_error", "write_option");

	if (!is_nil(p2))
		return throw_error(q, p2_orig, "type_error", "list");

	q->latest_ctx = p1_ctx;
	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;

	if (q->ignore_ops)
		print_canonical_to_stream(q, str, p1, p1_ctx, 1);
	else
		print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		fflush(str->fp);
	}

	q->max_depth = q->quoted = q->nl = q->fullstop = false;
	q->ignore_ops = false;
	q->variable_names = NULL;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	q->flag = q->st.m->flag;
	cell *p2_orig = p2, *vnames;
	idx_t vnames_ctx;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);

		if (!parse_write_params(q, h, &vnames, &vnames_ctx))
			return pl_success;

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2))
		return throw_error(q, p2_orig, "instantiation_error", "write_option");

	if (!is_nil(p2))
		return throw_error(q, p2_orig, "type_error", "list");

	q->latest_ctx = p1_ctx;
	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		fflush(str->fp);
	}

	q->max_depth = q->quoted = q->nl = q->fullstop = false;
	q->ignore_ops = false;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_char_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];
	size_t len = len_char_utf8(GET_STR(p1));

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	if (len != LEN_STR(p1))
		return throw_error(q, p1, "type_error", "character");

	const char *src = GET_STR(p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,atom);
	size_t len = len_char_utf8(GET_STR(p1));

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	if (len != LEN_STR(p1))
		return throw_error(q, p1, "type_error", "character");

	const char *src = GET_STR(p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	if (is_integer(p1) && (p1->val_num <= -1))
		return throw_error(q, p1, "representation_error", "character_code");

	int ch = (int)p1->val_num;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,binary_stream");
	}

	if (is_integer(p1) && (p1->val_num <= -1))
		return throw_error(q, p1, "representation_error", "character_code");

	int ch = (int)p1->val_num;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_byte_1(query *q)
{
	GET_FIRST_ARG(p1,byte);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "output,text_stream");
	}

	if (is_integer(p1) && (p1->val_num <= -1))
		return throw_error(q, p1, "representation_error", "character_code");

	int ch = (int)p1->val_num;
	char tmpbuf[20];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,byte);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "output,stream");

	if (!str->binary)
		return throw_error(q, pstr, "permission_error", "output,text_stream");

	if (is_integer(p1) && (p1->val_num <= -1))
		return throw_error(q, p1, "representation_error", "character_code");

	int ch = (int)p1->val_num;
	char tmpbuf[20];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_get_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (ch == '\n')
		str->did_getc = false;

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (ch == '\n')
		str->did_getc = false;

	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (is_integer(p1) && (p1->val_num < -1))
		return throw_error(q, p1, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (p1->val_num < -1))
		return throw_error(q, p1, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (ch == '\n')
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr++;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	str->did_getc = true;
	str->ungetch = 0;

	if (feof(str->fp)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}


	if (feof(str->fp)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	if (feof(str->fp)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[20];
	put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_small(&tmp, tmpbuf);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (is_integer(p1) && (p1->val_num < -1))
		return throw_error(q, p1, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	if (feof(str->fp)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (p1->val_num < -1))
		return throw_error(q, p1, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	if (feof(str->fp)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	if (feof(str->fp)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_HEX;
		return throw_error(q, &tmp, "permission_error", "input,past_end_of_stream");
	}

	if (str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		do_yield_0(q, 1);
		return pl_failure;
	}

	if (feof(str->fp)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_arg_3(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,compound);
	GET_NEXT_ARG(p3,any);

	if (is_integer(p1)) {
		int arg_nbr = p1->val_num;

		if (q->retry) {
			if (++arg_nbr > p2->arity)
				return pl_failure;

			GET_RAW_ARG(1, p1_raw);
			GET_RAW_ARG(3, p3_raw);

			p1 = p1_raw; p1_ctx = p1_raw_ctx;
			p3 = p3_raw; p3_ctx = p3_raw_ctx;

			cell tmp;
			make_int(&tmp, arg_nbr);
			reset_value(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			may_error(make_choice(q));
		}

		if (arg_nbr < 0)
			return throw_error(q, p1, "domain_error", "not_less_than_zero");

		if ((arg_nbr == 0) || (arg_nbr > p2->arity))
			return pl_failure;

		cell *c = p2 + 1;

		for (int i = 1; i <= arg_nbr; i++) {
			if (i == arg_nbr) {
				c = deref(q, c, p2_ctx);
				return unify(q, p3, p3_ctx, c, q->latest_ctx);
			}

			c += c->nbr_cells;
		}
	}

	if (is_variable(p1) && is_variable(p3)) {
		cell tmp;
		make_int(&tmp, 1);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		cell *c = p2 + 1;
		c = deref(q, c, p2_ctx);
		set_var(q, p3, p3_ctx, c, q->latest_ctx);
		may_error(make_choice(q));
		return pl_success;
	}

	return throw_error(q, p1, "instantiation_error", "number");
}

static USE_RESULT pl_status fn_iso_univ_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (is_variable(p1) && is_nil(p2))
		return throw_error(q, p2, "domain_error", "non_empty_list");

	if (is_variable(p2)) {
		cell *tmp = deep_copy_to_heap(q, p1, p1_ctx, false, false);
		may_ptr_error(tmp);
		if (tmp == ERR_CYCLE_CELL)
			return throw_error(q, p1, "resource_error", "cyclic_term");

		unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
		cell tmp2 = *tmp;
		tmp2.nbr_cells = 1;
		tmp2.arity = 0;
		CLR_OP(&tmp2);
		allocate_list(q, &tmp2);
		p1 = tmp;
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			append_list(q, p1);
			p1 += p1->nbr_cells;
		}

		cell *l = end_list(q);
		may_ptr_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	if (is_variable(p1)) {
		cell *tmp = deep_copy_to_tmp(q, p2, p2_ctx, false, false);
		may_ptr_error(tmp);
		if (tmp == ERR_CYCLE_CELL)
			return throw_error(q, p1, "resource_error", "cyclic_term");

		unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
		p2 = tmp;
		unsigned arity = 0;
		idx_t save = tmp_heap_used(q);
		cell *save_p2 = p2;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *h = LIST_HEAD(p2);
			cell *c = alloc_on_tmp(q, h->nbr_cells);
			may_ptr_error(c);
			copy_cells(c, h, h->nbr_cells);
			p2 = LIST_TAIL(p2);
			arity++;
		}

		if (is_variable(p2))
			return throw_error(q, p2, "instantiation_error", "list");

		if (!is_nil(p2))
			return throw_error(q, save_p2, "type_error", "list");

		arity--;
		cell *tmp2 = get_tmp_heap(q, save);

		if (is_cstring(tmp2) /*&& arity*/) {
			cell *c = tmp2;
			idx_t off = index_from_pool(q->st.m->pl, GET_STR(tmp2));
			ensure (off != ERR_IDX);
			//DECR_REF(tmp2);
			c->val_off = off;
			c->val_type = TYPE_LITERAL;
			c->flags = 0;
		}

		if (!is_literal(tmp2) && arity)
			return throw_error(q, tmp2, "type_error", "atom");

		if (tmp2->arity && arity)
			return throw_error(q, tmp2, "type_error", "atom");

		if (tmp2->arity)
			return throw_error(q, tmp2, "type_error", "atomic");

		if (arity > MAX_ARITY)
			return throw_error(q, tmp2, "representation_error", "max_arity");

		idx_t nbr_cells = tmp_heap_used(q) - save;
		tmp = alloc_on_heap(q, nbr_cells);
		may_ptr_error(tmp);
		safe_copy_cells(tmp, tmp2, nbr_cells);
		tmp->nbr_cells = nbr_cells;
		tmp->arity = arity;
		bool found = false;

		if (is_callable(tmp)) {
			if ((tmp->fn = get_builtin(q->st.m->pl, GET_STR(tmp), tmp->arity, &found)), found)
				tmp->flags |= FLAG_BUILTIN;
			else {
				tmp->match = search_predicate(q->st.m, tmp);
				tmp->flags &= ~FLAG_BUILTIN;
			}
		}

		return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	}

	cell tmp = *p1;
	tmp.nbr_cells = 1;
	tmp.arity = 0;
	CLR_OP(&tmp);
	allocate_list(q, &tmp);
	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		append_list(q, p1);
		p1 += p1->nbr_cells;
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	return unify(q, p2, p2_ctx, l, p1_ctx);
}

static cell *do_term_variables(query *q, cell *p1, idx_t p1_ctx)
{
	frame *g = GET_CURR_FRAME();
	q->st.m->pl->varno = g->nbr_vars;
	q->st.m->pl->tab_idx = 0;
	collect_vars(q, p1, p1_ctx, p1->nbr_cells, 0);
	const unsigned cnt = q->st.m->pl->tab_idx;
	init_tmp_heap(q);
	cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
	ensure(tmp);
	unsigned idx = 0;

	if (cnt) {
		unsigned done = 0;

		for (unsigned i = 0; i < cnt; i++) {
			make_literal(tmp+idx, g_dot_s);
			tmp[idx].arity = 2;
			tmp[idx].nbr_cells = ((cnt-done)*2)+1;
			idx++;
			cell v;
			make_variable(&v, q->st.m->pl->tab3[i]);

			if (q->st.m->pl->tab1[i] != q->st.curr_frame) {
				v.flags |= FLAG2_FRESH;
				v.var_nbr = q->st.m->pl->varno++;
			} else
				v.var_nbr = q->st.m->pl->tab2[i];

			tmp[idx++] = v;
			done++;
		}

		make_literal(tmp+idx++, g_nil_s);
		tmp[0].arity = 2;
		tmp[0].nbr_cells = idx;
	} else
		make_literal(tmp, g_nil_s);

	if (cnt) {
		unsigned new_vars = q->st.m->pl->varno - g->nbr_vars;
		q->st.m->pl->varno = g->nbr_vars;

		if (new_vars) {
			if (!create_vars(q, new_vars))
				return NULL;
		}

		for (unsigned i = 0; i < cnt; i++) {
			if (q->st.m->pl->tab1[i] == q->st.curr_frame)
				continue;

			cell v, tmp2;
			make_variable(&v, g_anon_s);
			v.flags |= FLAG2_FRESH;
			v.var_nbr = q->st.m->pl->varno++;
			make_variable(&tmp2, g_anon_s);
			tmp2.flags |= FLAG2_FRESH;
			tmp2.var_nbr = q->st.m->pl->tab2[i];
			set_var(q, &v, q->st.curr_frame, &tmp2, q->st.m->pl->tab1[i]);
		}
	}

	return tmp;		// returns on tmp_heap
}

static USE_RESULT pl_status fn_iso_term_variables_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_list(p2) && !is_valid_list(q, p2, p2_ctx, true))
		return throw_error(q, p2, "type_error", "list");

	if (!is_variable(p1) && (is_atom(p1) || is_number(p1))) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *tmp = do_term_variables(q, p1, p1_ctx);

	if (!tmp)
		return throw_error(q, p1, "resource_error", "too_many_vars");

	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	may_ptr_error(tmp2);
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static cell *clone2_to_tmp(query *q, cell *p1)
{
	cell *tmp = alloc_on_tmp(q, p1->nbr_cells);
	ensure(tmp);
	copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

static cell *clone_to_tmp(query *q, cell *p1)
{
	if (!init_tmp_heap(q)) return NULL;
	return clone2_to_tmp(q, p1);
}

static cell *clone2_to_heap(query *q, bool prefix, cell *p1, idx_t nbr_cells, idx_t suffix)
{
	cell *tmp = alloc_on_heap(q, (prefix?1:0)+nbr_cells+suffix);
	ensure(tmp);

	if (prefix) {
		// Needed for follow() to work
		*tmp = (cell){0};
		tmp->val_type = TYPE_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	safe_copy_cells(tmp+(prefix?1:0), p1, nbr_cells);
	return tmp;
}

cell *clone_to_heap(query *q, bool prefix, cell *p1, idx_t suffix)
{
	return clone2_to_heap(q, prefix, p1, p1->nbr_cells, suffix);
}

static cell *copy_to_heap2(query *q, bool prefix, cell *p1, idx_t nbr_cells, idx_t suffix)
{
	cell *tmp = alloc_on_heap(q, (prefix?1:0)+nbr_cells+suffix);
	ensure(tmp);

	if (prefix) {
		// Needed for follow() to work
		*tmp = (cell){0};
		tmp->val_type = TYPE_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	cell *src = p1, *dst = tmp+(prefix?1:0);
	frame *g = GET_CURR_FRAME();
	q->st.m->pl->varno = g->nbr_vars;
	q->st.m->pl->tab_idx = 0;

	for (idx_t i = 0; i < nbr_cells; i++, dst++, src++) {
		*dst = *src;
		INCR_REF(src);

		if (!is_variable(src))
			continue;

		slot *e = GET_SLOT(g, src->var_nbr);
		idx_t slot_nbr = e - q->slots;
		int found = 0;

		for (size_t i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab1[i] == slot_nbr) {
				dst->var_nbr = q->st.m->pl->tab2[i];
				break;
			}
		}

		if (!found) {
			dst->var_nbr = q->st.m->pl->varno;
			q->st.m->pl->tab1[q->st.m->pl->tab_idx] = slot_nbr;
			q->st.m->pl->tab2[q->st.m->pl->tab_idx] = q->st.m->pl->varno++;
			q->st.m->pl->tab_idx++;
		}

		dst->flags = FLAG2_FRESH;
	}

	if (q->st.m->pl->varno != g->nbr_vars) {
		if (!create_vars(q, q->st.m->pl->varno-g->nbr_vars)) {
			DISCARD_RESULT throw_error(q, p1, "resource_error", "too_many_vars");
			return NULL;
		}
	}

	return tmp;
}

cell *copy_to_heap(query *q, bool prefix, cell *p1, idx_t suffix)
{
	return copy_to_heap2(q, prefix, p1, p1->nbr_cells, suffix);
}

static USE_RESULT pl_status fn_iso_copy_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_variable(p1) && is_variable(p2))
		return pl_success;

	if (is_atomic(p1) && is_variable(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_variable(p2) && !has_vars(q, p1, p1_ctx, 0))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (q->cycle_error)
		return throw_error(q, p1, "resource_error", "too_many_vars");

	cell *tmp = deep_copy_to_heap(q, p1, p1_ctx, false, true);

	if (!tmp || (tmp == ERR_CYCLE_CELL))
		return throw_error(q, p1, "resource_error", "too_many_vars");

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_copy_term_nat_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_variable(p1) && is_variable(p2))
		return pl_success;

	if (is_atomic(p1) && is_variable(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_variable(p2) && !has_vars(q, p1, p1_ctx, 0))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (q->cycle_error)
		return throw_error(q, p1, "resource_error", "too_many_vars");

	cell *tmp = deep_copy_to_heap(q, p1, p1_ctx, false, false);

	if (!tmp || tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "too_many_vars");

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_clause_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable_or_var);

	while (match_clause(q, p1, p1_ctx, DO_CLAUSE)) {
		term *t = &q->st.curr_clause2->t;
		cell *body = get_body(t->cells);
		pl_status ok;

		if (body)
			ok = unify(q, p2, p2_ctx, body, q->st.fp);
		else {
			cell tmp;
			make_literal(&tmp, g_true_s);
			ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		if (ok) {
			bool last_match = !q->st.curr_clause2->next;
			stash_me(q, t, last_match);
			return pl_success;
		}

		undo_me(q);
		drop_choice(q);
		q->retry = QUERY_RETRY;
	}

	return pl_failure;
}

static USE_RESULT pl_status fn_iso_retract_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	return do_retract(q, p1, p1_ctx, DO_RETRACT);
}

static USE_RESULT pl_status do_retractall(query *q, cell *p1, idx_t p1_ctx)
{
	predicate *h = search_predicate(q->st.m, get_head(p1));

	if (!h) {
		cell *head = get_head(p1);
		bool found = false;

		if (get_builtin(q->st.m->pl, GET_STR(head), head->arity, &found), found)
			return throw_error(q, head, "permission_error", "modify,static_procedure");

		return pl_success;
	}

	while (do_retract(q, p1, p1_ctx, DO_RETRACTALL)) {
		if (q->did_throw)
			return pl_success;

		q->retry = QUERY_RETRY;
		retry_choice(q);
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_retractall_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	return do_retractall(q, p1, p1_ctx);
}

static USE_RESULT pl_status do_abolish(query *q, cell *c_orig, cell *c, bool hard)
{
	predicate *h = search_predicate(q->st.m, c);
	if (!h) return pl_success;

	if (!h->is_dynamic)
		return throw_error(q, c_orig, "permission_error", "modify,static_procedure");

	for (clause *r = h->head; r; r = r->next) {
		if (!q->st.m->loading && r->t.persist && !r->t.ugen_erased)
			db_log(q, r, LOG_ERASE);

		add_to_dirty_list(q, r);
	}

	if (hard)
		h->is_abolished = true;

	sl_destroy(h->index);
	h->index = NULL;
	h->cnt = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (p1->arity != 2)
		return throw_error(q, p1, "type_error", "predicate_indicator");

	const char *src = GET_STR(p1);

	if (strcmp(src, "/") && strcmp(src, "//"))
		return throw_error(q, p1, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!strcmp(src, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, "type_error", "integer");

	if (p1_arity->val_num < 0)
		return throw_error(q, p1_arity, "domain_error", "not_less_than_zero");

	if (p1_arity->val_num > MAX_ARITY)
		return throw_error(q, p1_arity, "representation_error", "max_arity");

	bool found = false;

	if (get_builtin(q->st.m->pl, GET_STR(p1_name), p1_arity->val_num, &found), found)
		return throw_error(q, p1, "permission_error", "modify,static_procedure");

	cell tmp;
	tmp = *p1_name;
	tmp.arity = p1_arity->val_num;
	CLR_OP(&tmp);
	return do_abolish(q, p1, &tmp, true);
}

static unsigned count_non_anons(uint8_t *mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if (mask[i] > 1)
			bits++;
	}

	return bits;
}

static void do_term_assign_vars(parser *p, idx_t nbr_cells)
{
	term_assign_vars(p, 0, true);
	uint8_t vars[MAX_ARITY] = {0};

	for (idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->t->cells+i;

		if (!is_variable(c))
			continue;

		assert(c->var_nbr < MAX_ARITY);
		vars[c->var_nbr]++;
	}

	for (idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->t->cells+i;

		if (!is_variable(c))
			continue;

		unsigned var_nbr = count_non_anons(vars, c->var_nbr);

		char ch = 'A';
		ch += var_nbr % 26;
		unsigned n = var_nbr / 26;
		char tmpbuf[20];

		if (vars[c->var_nbr] == 1)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", "_");
		else if (var_nbr < 26)
			snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%c%d", ch, n);

		c->val_off = index_from_pool(p->m->pl, tmpbuf);
		c->flags = 0;
	}
}

static USE_RESULT pl_status fn_iso_asserta_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *head = get_head(p1);

	if (is_variable(head))
		return throw_error(q, head, "instantiation_error", "args _not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->st.m->pl, GET_STR(head), head->arity, &found), found) {
		if (!GET_OP(head))
			return throw_error(q, head, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(p1);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, "type_error", "callable");

	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		ensure(p->t);
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = safe_copy_cells(p->t->cells, tmp, nbr_cells);
	do_term_assign_vars(p, nbr_cells);
	term_to_body(p);
	cell *h = get_head(p->t->cells);

	if (is_cstring(h)) {
		idx_t off = index_from_pool(q->st.m->pl, GET_STR(h));
		ensure (off != ERR_IDX);
		DECR_REF(h);
		h->val_type = TYPE_LITERAL;
		h->val_off = off;
		h->flags = 0;
	}

	if (!is_literal(h))
		return throw_error(q, h, "type_error", "callable");

	clause *r = asserta_to_db(q->st.m, p->t, 0);
	may_ptr_error(r);
	uuid_gen(q->st.m->pl, &r->u);

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ASSERTA);

	return pl_success;
}

static USE_RESULT pl_status fn_iso_assertz_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *head = get_head(p1);

	if (is_variable(head))
		return throw_error(q, head, "instantiation_error", "args _not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->st.m->pl, GET_STR(head), head->arity, &found), found) {
		if (!GET_OP(head))
			return throw_error(q, head, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(p1);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, "type_error", "callable");

	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		ensure(p->t);
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = safe_copy_cells(p->t->cells, tmp, nbr_cells);
	do_term_assign_vars(p, nbr_cells);
	term_to_body(p);
	cell *h = get_head(p->t->cells);

	if (is_cstring(h)) {
		idx_t off = index_from_pool(q->st.m->pl, GET_STR(h));
		ensure (off != ERR_IDX);
		DECR_REF(h);
		h->val_type = TYPE_LITERAL;
		h->val_off = off;
		h->flags = 0;
	}

	if (!is_literal(h))
		return throw_error(q, h, "type_error", "callable");

	clause *r = assertz_to_db(q->st.m, p->t, 0);
	may_ptr_error(r);
	uuid_gen(q->st.m->pl, &r->u);

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ASSERTZ);

	return pl_success;
}

USE_RESULT pl_status fn_call_0(query *q, cell *p1)
{
	if (q->retry)
		return pl_failure;

	p1 = deref(q, p1, q->st.curr_frame);
	idx_t p1_ctx = q->latest_ctx;

	if (!is_callable(p1))
		return throw_error(q, p1, "type_error", "callable");

	cell *tmp2;

	if ((tmp2 = check_body_callable(q->st.m->p, p1)) != NULL)
		return throw_error(q, p1, "type_error", "callable");

	cell *tmp;

	if (p1_ctx != q->st.curr_frame) {
		tmp = copy_to_heap(q, false, p1, 1);
		unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	} else
		tmp = clone_to_heap(q, false, p1, 1);

	idx_t nbr_cells = 0 + p1->nbr_cells;
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_call_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp3;

	if ((tmp3 = check_body_callable(q->st.m->p, p1)) != NULL)
		return throw_error(q, p1, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, p1, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_call_n(query *q)
{
	GET_FIRST_ARG(p1,callable);
	clone_to_tmp(q, p1);
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		cell *p2 = get_next_raw_arg(q);
		clone2_to_tmp(q, p2);
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		cell *c = tmp2;
		idx_t off = index_from_pool(q->st.m->pl, GET_STR(tmp2));
		ensure (off != ERR_IDX);
		//DECR_REF(tmp2);
		c->val_off = off;
		c->val_type = TYPE_LITERAL;
		c->flags = 0;
	}

	bool found = false;

	if ((tmp2->fn = get_builtin(q->st.m->pl, GET_STR(tmp2), arity, &found)), found) {
		tmp2->flags |= FLAG_BUILTIN;
		unsigned specifier;

		if (search_op(q->st.m, GET_STR(tmp2), &specifier, false))
			SET_OP(tmp2, specifier);
	} else if (found) {
		tmp2->flags |= FLAG_BUILTIN;
		unsigned specifier;

		if (search_op(q->st.m, GET_STR(tmp2), &specifier, false))
			SET_OP(tmp2, specifier);
	} else {
		tmp2->match = search_predicate(q->st.m, tmp2);
		tmp2->flags &= ~FLAG_BUILTIN;
	}

	cell *tmp = clone_to_heap(q, true, tmp2, 1);
	make_call(q, tmp+1+tmp2->nbr_cells);
	cell *tmp3;

	if ((tmp3 = check_body_callable(q->st.m->p, tmp2)) != NULL)
		return throw_error(q, tmp2, "type_error", "callable");

	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_invoke_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,callable);

	module *m = find_module(q->st.m->pl, GET_STR(p1));

	if (!m)
		m = create_module(q->st.m->pl, GET_STR(p1));

	cell *tmp = clone_to_heap(q, true, p2, 1);
	idx_t nbr_cells = 1;

	if (!is_builtin(p2))
		tmp[nbr_cells].match = find_predicate(m, p2);

	nbr_cells += p2->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->st.m = q->save_m = m;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_once_1(query *q)
{
	if (q->retry)
		return pl_failure;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_if_then_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell tmp1;
	make_structure(&tmp1, g_once_s, fn_iso_once_1, 1, 0);
	cell *tmp = clone_to_heap(q, true, &tmp1, p1->nbr_cells+p2->nbr_cells+1);
	idx_t nbr_cells = 1;
	tmp[nbr_cells++].nbr_cells += p1->nbr_cells;	// update the once structure
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status do_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
{
	if (q->retry) {
		cell *tmp = clone_to_heap(q, true, p3, 1);
		idx_t nbr_cells = 1 + p3->nbr_cells;
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_if_2(query *q)
{
	if (q->retry)
		return pl_failure;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_soft_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status do_if_else(query *q, cell *p1, cell *p2, cell *p3)
{
	if (q->retry) {
		cell *tmp = clone_to_heap(q, true, p3, 1);
		idx_t nbr_cells = 1 + p3->nbr_cells;
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_soft_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_if_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,callable);
	return do_if_else(q, p1, p2, p3);
}

static USE_RESULT pl_status fn_iso_disjunction_2(query *q)
{
	if ((q->st.curr_cell+1)->fn == fn_iso_if_then_2) {
		cell *p1 = q->st.curr_cell + 2;
		cell *p2 = p1 + p1->nbr_cells;
		cell *p3 = p2 + p2->nbr_cells;
		return do_if_then_else(q, p1, p2, p3);
	}

	if ((q->st.curr_cell+1)->fn == fn_if_2) {
		cell *p1 = q->st.curr_cell + 2;
		cell *p2 = p1 + p1->nbr_cells;
		cell *p3 = p2 + p2->nbr_cells;
		return do_if_else(q, p1, p2, p3);
	}

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);

	if (q->retry) {
		cell *tmp = clone_to_heap(q, true, p2, 1);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_call(q, tmp+nbr_cells);
	may_error(make_choice(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_negation_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_ignore_1(query *q)
{
	if (q->retry) {
		// reset the g->cgen how?
		return pl_success;
	}

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_catch_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (q->retry && q->exception) {
		cell *tmp = deep_copy_to_heap(q, q->exception, q->st.curr_frame, false, false);
		may_ptr_error(tmp);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = clone_to_heap(q, true, p3, 1);
		make_call(q, tmp+1+p3->nbr_cells);
		may_error(make_catcher(q, QUERY_EXCEPTION));
		q->st.curr_cell = tmp;
		return pl_success;
	}

	if (q->retry)
		return pl_failure;

	// First time through? Try the primary goal...

	cell *tmp = clone_to_heap(q, true, p1, 1);
	make_call(q, tmp+1+p1->nbr_cells);
	may_error(make_catcher(q, QUERY_RETRY));
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

static USE_RESULT bool find_exception_handler(query *q, cell *e)
{
	q->exception = e;

	while (retry_choice(q)) {
		choice *ch = GET_CHOICE(q->cp);

		if (!ch->catchme_retry)
			continue;

		q->retry = QUERY_EXCEPTION;

		if (!fn_iso_catch_3(q))
			continue;

		free(q->exception);
		q->exception = NULL;
		return true;
	}

	fprintf(stdout, "uncaught exception: ");
	q->quoted = 1;
	print_term(q, stdout, e, q->st.curr_frame, 1);
	fprintf(stdout, "\n");
	q->quoted = 1;
	q->st.m->pl->did_dump_vars = true;
	free(q->exception);
	q->exception = NULL;
	q->error = true;
	return false;
}

static USE_RESULT pl_status fn_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	cell *e = malloc(sizeof(cell) * tmp->nbr_cells);
	may_ptr_error(e);
	safe_copy_cells(e, tmp, tmp->nbr_cells);

	if (!find_exception_handler(q, e))
		return pl_failure;

	return fn_iso_catch_3(q);
}

pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected)
{
	q->did_throw = true;
	idx_t c_ctx = q->latest_ctx;
	int save_quoted = q->quoted;
	q->quoted = 1;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, 1, 0, 0);
	if (len <= 0) { q->error = true; return pl_failure; }
	char *dst = malloc(len+1+1024);
	ensure(dst);
	int off = 0;

	if (q->st.m != q->st.m->pl->m) {
		off += sprintf(dst, "%s:", q->st.m->name);
	}

	len = print_term_to_buf(q, dst+off, len+1, c, c_ctx, 1, 0, 0);
	size_t len2 = (len * 2) + strlen(err_type) + strlen(expected) + LEN_STR(q->st.curr_cell) + 1024;
	char *dst2 = malloc(len2+1);
	ensure(dst2);
	q->quoted = save_quoted;

	if (!strncmp(expected, "iso_", 4))
		expected += 4;

	char tmpbuf[1024*8];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s", expected);
	char *ptr;

	if (!strcmp(err_type, "type_error")
		&& ((ptr = strstr(tmpbuf, "_or")) != NULL))
		*ptr = '\0';

	if (!strcmp(err_type, "type_error") && !strcmp(expected, "stream"))
		err_type = "existence_error";

	expected = tmpbuf;
	char functor[1024];

	if (needs_quoting(q->st.m, GET_STR(q->st.curr_cell), LEN_STR(q->st.curr_cell))) {
		char tmpbuf[1024-3];
		formatted(tmpbuf, sizeof(tmpbuf), GET_STR(q->st.curr_cell), LEN_STR(q->st.curr_cell), false);
		snprintf(functor, sizeof(functor), "'%s'", tmpbuf);
	} else
		snprintf(functor, sizeof(functor), "%s", GET_STR(q->st.curr_cell));

	if (is_variable(c)) {
		err_type = "instantiation_error";
		snprintf(dst2, len2+1, "error(%s,%s).", err_type, expected);

	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "variable")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", "uninstantiation_error", dst, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "instantiation_error")) {
		snprintf(dst2, len2+1, "error(%s,(%s)/%u).", err_type, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "representation_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "evaluation_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "syntax_error")) {
		snprintf(dst2, len2+1, "error(%s((%s,%s)),(%s)/%u).", err_type, expected, dst, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "evaluable")) {
		snprintf(dst2, len2+1, "error(%s(%s,(%s)/%u),(%s)/%u).", err_type, expected, is_callable(c)?GET_STR(c):dst, c->arity, functor, q->st.curr_cell->arity);

	} else if (!strcmp(err_type, "permission_error")
		&& is_structure(c) && strcmp(GET_STR(c), "/") && is_variable(c+1)) {
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "(%s)/%u\n", GET_STR(c), (unsigned)c->arity);
		snprintf(dst2, len2+1, "error(%s(%s,%s),(%s)/%u).", err_type, expected, tmpbuf, functor, q->st.curr_cell->arity);
	} else if (!strcmp(err_type, "permission_error")) {
		snprintf(dst2, len2+1, "error(%s(%s,%s),(%s)/%u).", err_type, expected, dst, functor, q->st.curr_cell->arity);

	} else if (IS_OP(q->st.curr_cell)) {
		snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, GET_STR(q->st.curr_cell), q->st.curr_cell->arity);

	} else {
		if (!strcmp(GET_STR(q->st.curr_cell), "$call"))
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),%s/%u).", err_type, expected, dst, "call", q->st.curr_cell->arity);
		else if (!strcmp(GET_STR(q->st.curr_cell), "$catch"))
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),%s/%u).", err_type, expected, dst, "catch", q->st.curr_cell->arity);
		else if (!strcmp(GET_STR(q->st.curr_cell), "$bagof"))
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, "bagof", q->st.curr_cell->arity);
		else
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, functor, q->st.curr_cell->arity);
	}

	//printf("*** %s\n", dst2);

	parser *p = create_parser(q->st.m);
	may_ptr_error(p);
	p->srcptr = dst2;
	frame *g = GET_CURR_FRAME();
	p->read_term = g->nbr_vars;
	parser_tokenize(p, false, false);

	if (p->nbr_vars) {
		if (!create_vars(q, p->nbr_vars)) {
			destroy_parser(p);
			free(dst2);
			free(dst);
			return throw_error(q, c, "resource_error", "too_many_vars");
		}
	}

	cell *tmp = deep_copy_to_tmp(q, p->t->cells, q->st.curr_frame, false, false);
	may_ptr_error(tmp);
	if (tmp == ERR_CYCLE_CELL) {
		destroy_parser(p);
		free(dst2);
		free(dst);
		return throw_error(q, c, "resource_error", "cyclic_term");
	}

	cell *e = malloc(sizeof(cell) * tmp->nbr_cells);
	may_ptr_error(e, destroy_parser(p));
	safe_copy_cells(e, tmp, tmp->nbr_cells);
	destroy_parser(p);
	pl_status ok = pl_failure;

	if (find_exception_handler(q, e))
		ok = fn_iso_catch_3(q);

	free(dst2);
	free(dst);
	return ok;
}

static USE_RESULT pl_status fn_iso_functor_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1)) {
		if (!is_atomic(p2))
			return throw_error(q, p2, "type_error", "atomic");

		if (!is_integer(p3))
			return throw_error(q, p3, "type_error", "integer");

		if (p3->val_num < 0)
			return throw_error(q, p3, "domain_error", "not_less_than_zero");

		if (p3->val_num > (MAX_ARITY/2))
			return throw_error(q, p3, "representation_error", "max_arity");

		if (!is_atom(p2) && (p3->val_num > 0))
			return throw_error(q, p2, "type_error", "atom");

		unsigned arity = p3->val_num;
		unsigned var_nbr = 0;

		if (arity) {
			if (!(var_nbr = create_vars(q, arity)))
				return throw_error(q, p3, "resource_error", "too_many_vars");
		}

		GET_FIRST_ARG(p1,any);
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,any);

		if (is_number(p2)) {
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		} else {
			cell *tmp = alloc_on_heap(q, 1+arity);
			ensure(tmp);
			*tmp = (cell){0};
			tmp[0].val_type = TYPE_LITERAL;
			tmp[0].arity = arity;
			tmp[0].nbr_cells = 1 + arity;

			if (is_cstring(p2))
				tmp[0].val_off = index_from_pool(q->st.m->pl, GET_STR(p2));
			else
				tmp[0].val_off = p2->val_off;

			for (unsigned i = 1; i <= arity; i++) {
				tmp[i].val_type = TYPE_VARIABLE;
				tmp[i].nbr_cells = 1;
				tmp[i].var_nbr = var_nbr++;
				tmp[i].val_off = g_anon_s;
				tmp[i].flags = FLAG2_FRESH | FLAG2_ANON;
			}

			set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
		}

		return pl_success;
	}

	cell tmp = *p1;
	tmp.nbr_cells = 1;
	tmp.arity = 0;
	CLR_OP(&tmp);

	if (is_string(p1)) {
		tmp.val_type = TYPE_LITERAL;
		tmp.val_off = g_dot_s;
		tmp.flags = 0;
	}

	if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return pl_failure;

	make_int(&tmp, p1->arity);

	if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_iso_current_rule_1(query *q)
{
	GET_FIRST_ARG(p1,structure);
	int add_two = 0;

	if (!strcmp(GET_STR(p1), "/"))
		;
	else if (!strcmp(GET_STR(p1), "//"))
		add_two = 2;
	else
		return throw_error(q, p1, "type_error", "predicate_indicator");

	cell *pf = deref(q, p1+1,p1_ctx);
	cell *pa = deref(q, p1+2, p1_ctx);

	if (!is_atom(pf))
		return throw_error(q, p1, "type_error", "atom");

	if (!is_integer(pa))
		return throw_error(q, p1, "type_error", "integer");

	const char *functor = GET_STR(pf);
	unsigned arity = pa->val_num + add_two;
	module *m = q->st.m;

	if (strchr(functor, ':')) {
		char tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
		m = find_module(q->st.m->pl, tmpbuf1);
	}

	if (!m)
		m = q->st.m;

	module *tmp_m = NULL;

	while (m) {
		if (find_functor(m, functor, arity))
			return pl_success;

		if (!tmp_m)
			m = tmp_m = q->st.m->pl->modules;
		else
			m = m->next;
	}

	bool found = false;

	if (get_builtin(q->st.m->pl, functor, arity, &found), found)
		return pl_success;

	return pl_failure;
}

static bool search_functor(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx)
{
	if (!q->retry)
		q->st.iter2 = sl_first(q->st.m->index);

	DISCARD_RESULT make_choice(q);
	predicate *h = NULL;

	while (sl_next(q->st.iter2, (void*)&h)) {
		if (h->is_abolished)
			continue;

		try_me(q, 2);
		cell tmpn, tmpa;
		make_literal(&tmpn, h->key.val_off);
		make_int(&tmpa, h->key.arity);

		if (unify(q, p1, p1_ctx, &tmpn, q->st.fp)
			&& unify(q, p2, p2_ctx, &tmpa, q->st.fp)) {
			return true;
		}

		undo_me(q);
	}

	drop_choice(q);
	return false;
}

static USE_RESULT pl_status fn_iso_current_predicate_1(query *q)
{
	GET_FIRST_ARG(p_pi,any);
	cell *p1, *p2;
	idx_t p1_ctx, p2_ctx;

	if (p_pi->arity != 2)
		return throw_error(q, p_pi, "type_error", "predicate_indicator");

	if (strcmp(GET_STR(p_pi), "/"))
		return throw_error(q, p_pi, "type_error", "predicate_indicator");

	p1 = p_pi + 1;
	p1 = deref(q, p1, p_pi_ctx);
	p1_ctx = q->latest_ctx;

	if (!is_atom(p1) && !is_variable(p1))
		return throw_error(q, p_pi, "type_error", "predicate_indicator");

	p2 = p1 + 1;
	p2 = deref(q, p2, p_pi_ctx);
	p2_ctx = q->latest_ctx;

	if ((!is_integer(p2) || (p2->val_num < 0)) && !is_variable(p2))
		return throw_error(q, p_pi, "type_error", "predicate_indicator");

	if (!search_functor(q, p1, p1_ctx, p2, p2_ctx))
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_iso_acyclic_term_1(query *q)
{
	GET_FIRST_ARG(p_term,any);
	ssize_t res = print_term_to_buf(q, NULL, 0, p_term, p_term_ctx, 1, 0, 0);
	if (res < 0) q->cycle_error = true;
	return !q->cycle_error;
}

static USE_RESULT pl_status fn_iso_current_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);

	if (!strcmp(GET_STR(p1), "double_quotes")) {
		cell tmp;

		if (q->st.m->flag.double_quote_atom)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "atom"));
		else if (q->st.m->flag.double_quote_codes)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "codes"));
		else if (q->st.m->flag.double_quote_chars)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "chars"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "char_conversion")) {
		cell tmp;

		if (q->st.m->flag.char_conversion)
			make_literal(&tmp, g_on_s);
		else
			make_literal(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "occurs_check")) {
		cell tmp;

		if (q->st.m->flag.occurs_check == 1)
			make_literal(&tmp, g_true_s);
		else if (q->st.m->flag.occurs_check == 0)
			make_literal(&tmp, g_false_s);
		else
			make_literal(&tmp, index_from_pool(q->st.m->pl, "error"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "debug")) {
		cell tmp;

		if (q->st.m->flag.debug)
			make_literal(&tmp, g_on_s);
		else
			make_literal(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "character_escapes")) {
		cell tmp;

		if (q->st.m->flag.character_escapes)
			make_literal(&tmp, g_true_s);
		else
			make_literal(&tmp, g_false_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "prefer_rationals")) {
		cell tmp;

		if (q->st.m->flag.prefer_rationals)
			make_literal(&tmp, g_true_s);
		else
			make_literal(&tmp, g_false_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "rational_syntax")) {
		cell tmp;

		if (q->st.m->flag.rational_syntax_natural)
			make_literal(&tmp, index_from_pool(q->st.m->pl, "natural"));
		else
			make_literal(&tmp, index_from_pool(q->st.m->pl, "compatibility"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "dialect")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->st.m->pl, "trealla"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "integer_rounding_function")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->st.m->pl, "toward_zero"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "bounded")) {
		cell tmp;
		make_literal(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "max_arity")) {
		cell tmp;
		make_int(&tmp, MAX_ARITY);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#if USE_INT32
	} else if (!strcmp(GET_STR(p1), "max_integer")) {
		cell tmp;
		make_int(&tmp, INT32_MAX);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "min_integer")) {
		cell tmp;
		make_int(&tmp, INT32_MIN);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#else
	} else if (!strcmp(GET_STR(p1), "max_integer")) {
		cell tmp;
		make_int(&tmp, INT64_MAX);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "min_integer")) {
		cell tmp;
		make_int(&tmp, INT64_MIN);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#endif
	} else if (!strcmp(GET_STR(p1), "cpu_count")) {
		cell tmp;
		make_int(&tmp, g_cpu_count);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "version")) {
		unsigned v1 = 0;
		sscanf(VERSION, "v%u", &v1);
		cell tmp;
		make_int(&tmp, v1);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "version_data")) {
		unsigned v1 = 0, v2 = 0, v3 = 0;
		sscanf(VERSION, "v%u.%u.%u", &v1, &v2, &v3);
		cell *tmp = alloc_on_heap(q, 5);
		ensure(tmp);
		make_literal(&tmp[0], index_from_pool(q->st.m->pl, "trealla"));
		make_int(&tmp[1], v1);
		make_int(&tmp[2], v2);
		make_int(&tmp[3], v3);
		make_literal(&tmp[4], g_nil_s);
		tmp[0].arity = 4;
		tmp[0].nbr_cells = 5;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "version_git")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->st.m->pl, VERSION));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!strcmp(GET_STR(p1), "argv")) {
		if (g_avc == g_ac) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		int i = g_avc;
		cell tmp;
		may_error(make_cstring(&tmp, g_av[i++]));
		allocate_list(q, &tmp);

		while (i < g_ac) {
			may_error(make_cstring(&tmp, g_av[i++]));
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		may_ptr_error(l);
		pl_status ok = unify(q, p2, p2_ctx, l, q->st.curr_frame);
		return ok;
	} else if (!strcmp(GET_STR(p1), "unknown")) {
		cell tmp;
		make_literal(&tmp,
			q->st.m->flag.unknown == UNK_ERROR ? index_from_pool(q->st.m->pl, "error") :
			q->st.m->flag.unknown == UNK_WARNING ? index_from_pool(q->st.m->pl, "warning") :
			q->st.m->flag.unknown == UNK_CHANGEABLE ? index_from_pool(q->st.m->pl, "changeable") :
			index_from_pool(q->st.m->pl, "fail"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	return throw_error(q, p1, "domain_error", "prolog_flag");
}

static USE_RESULT pl_status fn_iso_set_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_atom(p1))
		return throw_error(q, p1, "type_error", "atom");

	if (!strcmp(GET_STR(p1), "cpu_count") && is_integer(p2)) {
		g_cpu_count = p2->val_num;
		return pl_success;
	}

	if (!is_atom(p2) && !is_integer(p2))
		return throw_error(q, p2, "type_error", "atom");

	if (!strcmp(GET_STR(p1), "double_quotes")) {
		if (!strcmp(GET_STR(p2), "atom")) {
			q->st.m->flag.double_quote_chars = q->st.m->flag.double_quote_codes = false;
			q->st.m->flag.double_quote_atom = true;
		} else if (!strcmp(GET_STR(p2), "codes")) {
			q->st.m->flag.double_quote_chars = q->st.m->flag.double_quote_atom = false;
			q->st.m->flag.double_quote_codes = true;
		} else if (!strcmp(GET_STR(p2), "chars")) {
			q->st.m->flag.double_quote_atom = q->st.m->flag.double_quote_codes = false;
			q->st.m->flag.double_quote_chars = true;
		} else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}

		q->st.m->p->flag = q->st.m->flag;
	} else if (!strcmp(GET_STR(p1), "character_escapes")) {
		if (!strcmp(GET_STR(p2), "true") || !strcmp(GET_STR(p2), "on"))
			q->st.m->flag.character_escapes = true;
		else if (!strcmp(GET_STR(p2), "false") || !strcmp(GET_STR(p2), "off"))
			q->st.m->flag.character_escapes = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "char_conversion")) {
		if (!strcmp(GET_STR(p2), "true") || !strcmp(GET_STR(p2), "on"))
			q->st.m->flag.char_conversion = true;
		else if (!strcmp(GET_STR(p2), "false") || !strcmp(GET_STR(p2), "off"))
			q->st.m->flag.char_conversion = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "occurs_check")) {
		if (!strcmp(GET_STR(p2), "true") || !strcmp(GET_STR(p2), "on"))
			q->st.m->flag.occurs_check = 1;
		else if (!strcmp(GET_STR(p2), "false") || !strcmp(GET_STR(p2), "off"))
			q->st.m->flag.occurs_check = 0;
		else if (!strcmp(GET_STR(p2), "error"))
			q->st.m->flag.occurs_check = -1;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "rational_syntax")) {
		if (!strcmp(GET_STR(p2), "natural"))
			q->st.m->flag.rational_syntax_natural = true;
		else if (!strcmp(GET_STR(p2), "compatibility"))
			q->st.m->flag.rational_syntax_natural = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "prefer_rationals")) {
		if (!strcmp(GET_STR(p2), "true") || !strcmp(GET_STR(p2), "on"))
			q->st.m->flag.prefer_rationals = true;
		else if (!strcmp(GET_STR(p2), "false") || !strcmp(GET_STR(p2), "off"))
			q->st.m->flag.prefer_rationals = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "debug")) {
		if (!strcmp(GET_STR(p2), "true") || !strcmp(GET_STR(p2), "on"))
			q->st.m->flag.debug = true;
		else if (!strcmp(GET_STR(p2), "false") || !strcmp(GET_STR(p2), "off"))
			q->st.m->flag.debug = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, "domain_error", "flag_value");
		}
	} else if (!strcmp(GET_STR(p1), "unknown")) {
		if (!strcmp(GET_STR(p2), "fail")) {
			q->st.m->flag.unknown = UNK_FAIL;
		} else if (!strcmp(GET_STR(p2), "error")) {
			q->st.m->flag.unknown = UNK_ERROR;
		} else if (!strcmp(GET_STR(p2), "warning")) {
			q->st.m->flag.unknown = UNK_WARNING;
		} else if (!strcmp(GET_STR(p2), "changeable")) {
			q->st.m->flag.unknown = UNK_CHANGEABLE;
		}
	} else if (!strcmp(GET_STR(p1),"bounded")
		|| !strcmp(GET_STR(p1),"max_arity")
		|| !strcmp(GET_STR(p1),"max_integer")
		|| !strcmp(GET_STR(p1),"min_integer")
		|| !strcmp(GET_STR(p1),"version")
		|| !strcmp(GET_STR(p1),"version_data")
		|| !strcmp(GET_STR(p1),"version_git")
		|| !strcmp(GET_STR(p1),"dialect")
		)
		return throw_error(q, p1, "permission_error", "modify,flag");
	else
		return throw_error(q, p1, "domain_error", "prolog_flag");

	q->flag = q->st.m->flag;
	return pl_success;
}

static cell *convert_to_list(query *q, cell *c, idx_t nbr_cells)
{
	if ((!nbr_cells || !c->nbr_cells)) {
		cell *c = alloc_on_tmp(q, 1);
		if (!c) return c;
		make_literal(c, g_nil_s);
		return c;
	}

	allocate_list(q, c);
	nbr_cells -= c->nbr_cells;
	c += c->nbr_cells;

	while (nbr_cells > 0) {
		append_list(q, c);
		nbr_cells -= c->nbr_cells;
		c += c->nbr_cells;
	}

	// This function is only ever called on a queue which
	// already has a safe_copy done, so the end_list below
	// can do an unsafe copy.

	cell *l = end_list_unsafe(q);
	ensure(l);
	return l;
}

static USE_RESULT pl_status fn_sys_list_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell *l = convert_to_list(q, get_queue(q), queue_used(q));

#if 0
	frame *g = GET_CURR_FRAME();
	unsigned new_varno = g->nbr_vars;
	cell *c = l;

	for (idx_t i = 0; i < l->nbr_cells; i++, c++) {
		if (is_variable(c) && is_anon(c)) {
			c->var_nbr = new_varno++;
			c->flags = FLAG2_FRESH | FLAG2_ANON;
		}
	}

	if (new_varno != g->nbr_vars) {
		if (!create_vars(q, new_varno-g->nbr_vars))
			return throw_error(q, p1, "resource_error", "too_many_vars");
	}
#endif

	return unify(q, p1, p1_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_queue_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	alloc_on_queuen(q, 0, tmp);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_queuen_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	cell *tmp = deep_clone_to_tmp(q, p2, p2_ctx);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	alloc_on_queuen(q, p1->val_num, tmp);
	return pl_success;
}

static int collect_local_vars(cell *p1, idx_t nbr_cells, cell **slots)
{
	int cnt = 0;

	for (idx_t i = 0; i < nbr_cells; i++, p1++) {
		if (is_variable(p1)) {
			assert(p1->var_nbr < MAX_ARITY);

			if (!slots[p1->var_nbr]) {
				slots[p1->var_nbr] = p1;
				cnt++;
			}
		}
	}

	return cnt;
}

static uint64_t get_vars(cell *p)
{
	cell *slots[MAX_ARITY] = {0};
	int cnt = collect_local_vars(p, p->nbr_cells, slots);
	uint64_t mask = 0;

	if (!cnt)
		return 0;

	for (unsigned i = 0; i < MAX_ARITY; i++) {
		if (slots[i])
			mask |= 1ULL << i;
	}

	return mask;
}

static cell *skip_existentials(query *q, cell *p2, uint64_t *xs)
{
	while (is_structure(p2) && !strcmp(GET_STR(p2), "^")) {
		cell *c = ++p2;

		if (!is_variable(c)) {
			for (idx_t i = 0; i < c->nbr_cells; i++) {
				if (is_variable(c+i)) {
					assert((c+i)->var_nbr < 64);
					*xs |= 1ULL << (c+i)->var_nbr;
				}
			}
		}

		assert(c->var_nbr < 64);

		if (is_variable(c))
			*xs |= 1ULL << c->var_nbr;

		p2 += c->nbr_cells;
	}

	return p2;
}

static void pin_vars(query *q, uint64_t mask)
{
	choice *ch = GET_CURR_CHOICE();
	ch->pins = mask;
}

static void unpin_vars(query *q)
{
	choice *ch = GET_CURR_CHOICE();
	frame *g = GET_CURR_FRAME();
	uint64_t mask = 1;

	for (unsigned i = 0; i < g->nbr_vars; i++, mask <<= 1) {
		if (!(ch->pins & mask))
			continue;

		slot *e = GET_SLOT(g, i);
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = NULL;
	}

	ch->pins = 0;
}

static USE_RESULT pl_status fn_sys_findall_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	if (is_list(p3) && !is_valid_list(q, p3, p3_ctx, true))
		return throw_error(q, p3, "type_error", "list");

	if (!q->retry) {
		q->st.qnbr++;
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+1);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		may_error(make_barrier(q));
		q->st.curr_cell = tmp;
		return pl_success;
	}

	if (!queuen_used(q)) {
		q->st.qnbr--;
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	}

	// Retry takes a copy

	idx_t nbr_cells = queuen_used(q);
	q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
	ensure(q->tmpq[q->st.qnbr]);
	copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
	q->tmpq_size[q->st.qnbr] = nbr_cells;

	// Now grab matching solutions

	init_queuen(q);
	may_error(make_choice(q));
	nbr_cells = q->tmpq_size[q->st.qnbr];
	frame *g = GET_FRAME(q->st.curr_frame);

	for (cell *c = q->tmpq[q->st.qnbr]; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		try_me(q, g->nbr_vars*2);

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
			may_ptr_error(tmp);
			alloc_on_queuen(q, q->st.qnbr, tmp);
		}

		undo_me(q);
	}

	// Return matching solutions

	drop_choice(q);
	free(q->tmpq[q->st.qnbr]);
	q->tmpq[q->st.qnbr] = NULL;
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	q->st.qnbr--;
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_bagof_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	if (is_list(p3) && !is_valid_list(q, p3, p3_ctx, true))
		return throw_error(q, p3, "type_error", "list");

	uint64_t xs_vars = 0;
	p2 = skip_existentials(q, p2, &xs_vars);
	cell *tvars_tmp = do_term_variables(q, p2, p2_ctx);
	cell *tvars = malloc(sizeof(cell)*tvars_tmp->nbr_cells);
	copy_cells(tvars, tvars_tmp, tvars_tmp->nbr_cells);

	// First time thru generate all solutions

	if (!q->retry) {
		q->st.qnbr++;
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+tvars->nbr_cells+1);
		idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+tvars->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += copy_cells(tmp+nbr_cells, tvars, tvars->nbr_cells);
		make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);

		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		may_error(make_barrier(q));
		q->st.curr_cell = tmp;
		free(tvars);
		return pl_success;
	}

	if (!queuen_used(q) && !q->tmpq[q->st.qnbr]) {
		free(tvars);
		return pl_failure;
	}

	// First retry takes a copy

	if (!q->tmpq[q->st.qnbr]) {
		idx_t nbr_cells = queuen_used(q);
		q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
		ensure(q->tmpq[q->st.qnbr]);
		copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
		q->tmpq_size[q->st.qnbr] = nbr_cells;
	}

	// Now grab matching solutions

	init_queuen(q);
	may_error(make_choice(q));
	uint64_t p1_vars = get_vars(p1);
	uint64_t p2_vars = get_vars(p2);
	uint64_t mask = p1_vars ^ p2_vars ^ xs_vars;
	pin_vars(q, mask);
	idx_t nbr_cells = q->tmpq_size[q->st.qnbr];
	bool unmatched = false;
	frame *g = GET_FRAME(q->st.curr_frame);

	for (cell *c = q->tmpq[q->st.qnbr]; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {

#if 0
		fprintf(stdout, "*** ");
		print_term(q, stdout, c, p2_ctx, 1);
		fprintf(stdout, "\n");
#endif

		if (c->flags & FLAG2_PROCESSED)
			continue;

		try_me(q, g->nbr_vars*2);

		if (unify(q, tvars, p2_ctx, c, q->st.fp)) {
			c->flags |= FLAG2_PROCESSED;
			cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, true, false);
			may_ptr_error(tmp);
			alloc_on_queuen(q, q->st.qnbr, tmp);
		} else
			unmatched = true;

		undo_me(q);
	}

	// No solution?

	if (!queuen_used(q)) {
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		drop_choice(q);
		free(tvars);
		return pl_failure;
	}

	// Return matching solutions

	cell *tmp = deep_copy_to_heap(q, tvars, p2_ctx, true, false);
	may_ptr_error(tmp);
	unpin_vars(q);
	unify(q, tvars, p2_ctx, tmp, q->st.curr_frame);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));

	if (!unmatched) {
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		drop_choice(q);
		q->st.qnbr--;
	}

	free(tvars);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static pl_status do_op(query *q, cell *p3)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);

	if (!is_atom(p3))
		return throw_error(q, p3, "type_error", "atom");

	unsigned specifier;
	const char *spec = GET_STR(p2);
	unsigned pri = p1->val_num;

	if (!strcmp(spec, "fx"))
		specifier = OP_FX;
	else if (!strcmp(spec, "fy"))
		specifier = OP_FY;
	else if (!strcmp(spec, "xf"))
		specifier = OP_XF;
	else if (!strcmp(spec, "xfx"))
		specifier = OP_XFX;
	else if (!strcmp(spec, "xfy"))
		specifier = OP_XFY;
	else if (!strcmp(spec, "yf"))
		specifier = OP_YF;
	else if (!strcmp(spec, "yfx"))
		specifier = OP_YFX;
	else
		return throw_error(q, p2, "domain_error", "operator_specifier");

	if (pri && !strcmp(GET_STR(p3), "|") && (!IS_INFIX(specifier) || (pri < 1001)))
		return throw_error(q, p3, "permission_error", "create,operator");

	if (!strcmp(GET_STR(p3), "[]"))
		return throw_error(q, p3, "permission_error", "create,operator");

	if (!strcmp(GET_STR(p3), "{}"))
		return throw_error(q, p3, "permission_error", "create,operator");

	if (!strcmp(GET_STR(p3), ","))
		return throw_error(q, p3, "permission_error", "modify,operator");

	unsigned tmp_optype = 0;
	get_op(q->st.m, GET_STR(p3), &tmp_optype, false);

	if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype))
		return throw_error(q, p3, "permission_error", "create,operator");

	unsigned tmp_pri = get_op2(q->st.m, GET_STR(p3), OP_FX);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, "permission_error", "create,operator");

	tmp_pri = get_op2(q->st.m, GET_STR(p3), OP_FY);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, "permission_error", "create,operator");

	if (!set_op(q->st.m, GET_STR(p3), specifier, pri))
		return throw_error(q, p3, "resource_error", "too_many_ops");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_op_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_atom);

	if (is_integer(p1) && ((p1->val_num < 0) || (p1->val_num > 1200)))
		return throw_error(q, p1, "domain_error", "operator_priority");

	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		pl_status ok = do_op(q, h);

		if (ok != pl_success)
			return ok;

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_variable(p3))
			return throw_error(q, p3, "instantiation_error", "atom");

		if (is_nil(p3))
			return pl_success;
	}

	if (is_atom(p3))
		return do_op(q, p3);

	return pl_success;
}

static USE_RESULT pl_status fn_erase_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	uuid u;
	uuid_from_buf(GET_STR(p1), &u);
	clause *r = erase_from_db(q->st.m, &u);
	may_ptr_error(r);

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ERASE);

	return pl_success;
}

static USE_RESULT pl_status fn_instance_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	uuid u;
	uuid_from_buf(GET_STR(p1), &u);
	clause *r = find_in_db(q->st.m, &u);
	may_ptr_error(r);
	return unify(q, p2, p2_ctx, r->t.cells, q->st.curr_frame);
}

static USE_RESULT pl_status fn_clause_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable_or_var);
	GET_NEXT_ARG(p3,atom_or_var);

	for (;;) {
		term *t;

		if (!is_variable(p3)) {
			uuid u;
			uuid_from_buf(GET_STR(p3), &u);
			clause *r = find_in_db(q->st.m, &u);
			may_ptr_error(r);
			t = &r->t;
		} else {
			if (!match_clause(q, p1, p1_ctx, DO_CLAUSE))
				break;

			char tmpbuf[128];
			uuid_to_buf(&q->st.curr_clause2->u, tmpbuf, sizeof(tmpbuf));
			cell tmp;
			may_error(make_cstring(&tmp, tmpbuf));
			set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			DECR_REF(&tmp);
			t = &q->st.curr_clause2->t;
		}

		cell *body = get_body(t->cells);
		pl_status ok;

		if (body)
			ok = unify(q, p2, p2_ctx, body, q->st.fp);
		else {
			cell tmp;
			make_literal(&tmp, g_true_s);
			ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		if (ok) {
			bool last_match = !q->st.curr_clause2->next;
			stash_me(q, t, last_match);
			return pl_success;
		}

		undo_me(q);
		drop_choice(q);
		q->retry = QUERY_RETRY;
	}

	return pl_failure;
}

static USE_RESULT pl_status do_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = get_head(p1);

	if (is_variable(head))
		return throw_error(q, head, "instantiation_error", "args _not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->st.m->pl, GET_STR(head), head->arity, &found), found) {
		if (!GET_OP(head))
			return throw_error(q, head, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body && !is_callable(body))
		return throw_error(q, body, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);
	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		ensure(p->t);
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = safe_copy_cells(p->t->cells, tmp, nbr_cells);
	do_term_assign_vars(p, nbr_cells);
	term_to_body(p);
	cell *h = get_head(p->t->cells);

	if (is_cstring(h)) {
		idx_t off = index_from_pool(q->st.m->pl, GET_STR(h));
		ensure (off != ERR_IDX);
		DECR_REF(h);
		h->val_type = TYPE_LITERAL;
		h->val_off = off;
		h->flags = 0;
	}

	if (!is_literal(h))
		return throw_error(q, h, "type_error", "callable");

	clause *r = asserta_to_db(q->st.m, p->t, 0);
	may_ptr_error(r);

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(p2), &u);
		r->u = u;
	} else {
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		may_error(make_cstring(&tmp2, tmpbuf));
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		DECR_REF(&tmp2);
	}

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ASSERTA);

	return pl_success;
}

static USE_RESULT pl_status fn_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,variable);
	return do_asserta_2(q);
}

static USE_RESULT pl_status fn_sys_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_asserta_2(q);
}

static USE_RESULT pl_status do_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = get_head(p1);

	if (is_variable(head))
		return throw_error(q, head, "instantiation_error", "args _not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->st.m->pl, GET_STR(head), head->arity, &found), found) {
		if (!GET_OP(head))
			return throw_error(q, head, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body && !is_callable(body))
		return throw_error(q, body, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->t->nbr_cells) {
		p->t = realloc(p->t, sizeof(term)+(sizeof(cell)*(nbr_cells+1)));
		ensure(p->t);
		p->t->nbr_cells = nbr_cells;
	}

	p->t->cidx = safe_copy_cells(p->t->cells, tmp, nbr_cells);
	do_term_assign_vars(p, nbr_cells);
	term_to_body(p);
	cell *h = get_head(p->t->cells);

	if (is_cstring(h)) {
		idx_t off = index_from_pool(q->st.m->pl, GET_STR(h));
		ensure (off != ERR_IDX);
		DECR_REF(h);
		h->val_type = TYPE_LITERAL;
		h->val_off = off;
		h->flags = 0;
	}

	if (!is_literal(h))
		return throw_error(q, h, "type_error", "callable");

	clause *r = assertz_to_db(q->st.m, p->t, 0);
	may_ptr_error(r);

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(p2), &u);
		r->u = u;
	} else {
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		may_error(make_cstring(&tmp2, tmpbuf));
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		DECR_REF(&tmp2);
	}

	if (!q->st.m->loading && r->t.persist)
		db_log(q, r, LOG_ASSERTZ);

	return pl_success;
}

static USE_RESULT pl_status fn_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,variable);
	return do_assertz_2(q);
}

static USE_RESULT pl_status fn_sys_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_assertz_2(q);
}

static void save_db(FILE *fp, query *q, int logging)
{
	for (predicate *h = q->st.m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		if (logging && !h->is_persist)
			continue;

		const char *src = GET_STR(&h->key);

		if (src[0] == '$')
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.ugen_erased)
				continue;

			if (logging)
				fprintf(fp, "z_(");

			print_term(q, fp, r->t.cells, q->st.curr_frame, 0);

			if (logging) {
				char tmpbuf[256];
				uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
				fprintf(fp, ",'%s')", tmpbuf);
			}

			fprintf(fp, ".\n");
		}
	}
}

static USE_RESULT pl_status fn_listing_0(query *q)
{
	save_db(stdout, q, 0);
	return pl_success;
}

static void save_name(FILE *fp, query *q, idx_t name, unsigned arity)
{
	module *m = q->st.curr_clause ? q->st.curr_clause->m : q->st.m;

	for (predicate *h = m->head; h; h = h->next) {
		if (h->is_prebuilt)
			continue;

		if (name != h->key.val_off)
			continue;

		if ((arity != h->key.arity) && (arity != -1U))
			continue;

		for (clause *r = h->head; r; r = r->next) {
			if (r->t.ugen_erased)
				continue;

			print_term(q, fp, r->t.cells, q->st.curr_frame, 0);
			fprintf(fp, ".\n");
		}
	}
}

static USE_RESULT pl_status fn_listing_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	idx_t name = p1->val_off;
	unsigned arity = -1;

	if (p1->arity) {
		if (strcmp(GET_STR(p1), "/") && strcmp(GET_STR(p1), "//"))
			return throw_error(q, p1, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, "type_error", "integer");

		name = index_from_pool(q->st.m->pl, GET_STR(p2));
		arity = p3->val_num;

		if (!strcmp(GET_STR(p1), "//"))
			arity += 2;
	}

	save_name(stdout, q, name, arity);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_timer_0(query *q)
{
	q->time_started = get_time_in_usec();
	return pl_success;
}

static USE_RESULT pl_status fn_sys_elapsed_0(query *q)
{
	uint64_t elapsed = get_time_in_usec();
	elapsed -= q->time_started;
	fprintf(stdout, "Time elapsed %.03g secs\n", (double)elapsed/1000/1000);
	return pl_success;
}

static USE_RESULT pl_status fn_trace_0(query *q)
{
	q->trace = !q->trace;
	return pl_success;
}

static USE_RESULT pl_status fn_time_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	DISCARD_RESULT fn_sys_timer_0(q);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_elapsed_s, fn_sys_elapsed_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_statistics_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_var);

	if (!strcmp(GET_STR(p1), "cputime") && is_variable(p2)) {
		uint64_t now = get_time_in_usec();
		double elapsed = now - q->time_started;
		cell tmp;
		make_float(&tmp, elapsed/1000/1000);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!strcmp(GET_STR(p1), "gctime") && is_variable(p2)) {
		cell tmp;
		make_float(&tmp, 0);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!strcmp(GET_STR(p1), "runtime")) {
		uint64_t now = get_time_in_usec();
		double elapsed = now - q->time_started;
		cell tmp;
		make_int(&tmp, elapsed/1000);
		allocate_list(q, &tmp);
		append_list(q, &tmp);
		make_literal(&tmp, g_nil_s);
		cell *l = end_list(q);
		may_ptr_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return pl_failure;
}

static USE_RESULT pl_status fn_sleep_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task) {
		do_yield_0(q, p1->val_num*1000);
		return pl_failure;
	}

	sleep((unsigned)p1->val_num);
	return pl_success;
}

static USE_RESULT pl_status fn_delay_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task) {
		do_yield_0(q, p1->val_num);
		return pl_failure;
	}

	msleep((unsigned)p1->val_num);
	return pl_success;
}

static USE_RESULT pl_status fn_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int_t elapse = p1->val_num;

	if (elapse < 0)
		return pl_success;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return pl_success;

	uint_t started = get_time_in_usec() / 1000;
	uint_t end = started + elapse;

	while ((get_time_in_usec() / 1000) < end)
		;

	return pl_success;
}

static USE_RESULT pl_status fn_now_0(query *q)
{
	int_t secs = get_time_in_usec() / 1000 / 1000;
	q->accum.val_type = TYPE_INTEGER;
	q->accum.val_num = secs;
	return pl_success;
}

static USE_RESULT pl_status fn_now_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	int_t secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	double v = ((double)get_time_in_usec()) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_writeln_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	fputc('\n', str->fp);
	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);

	if (!q->retry && !is_variable(p3)) {
		if (p3->val_num > p2->val_num)
			return pl_failure;

		if (p3->val_num < p1->val_num)
			return pl_failure;

		return pl_success;
	}

	if (p1->val_num > p2->val_num)
		return pl_failure;

	if (!q->retry) {
		set_var(q, p3, p3_ctx, p1, q->st.curr_frame);

		if (p1->val_num != p2->val_num)
			may_error(make_choice(q));

		return pl_success;
	}

	int_t val = p3->val_num;

	if (val == p2->val_num)
		return pl_failure;

	val++;
	GET_RAW_ARG(3,p3_raw);
	cell tmp;
	make_int(&tmp, val);
	reset_value(q, p3_raw, p3_raw_ctx, &tmp, q->st.curr_frame);

	if (val != p2->val_num)
		may_error(make_choice(q));

	return pl_success;
}

#if 0
static USE_RESULT pl_status fn_forall_2(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);

	idx_t off = heap_used(q);
	may_ptr_error(clone_to_heap(q, true, p1, 0));
	may_ptr_error(clone_to_heap(q, false, p2, 1));

	cell *tmp = get_heap(q, off);
	idx_t nbr_cells = 1 + p1->nbr_cells + p2->nbr_cells;
	make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
	may_error(make_choice(q));
	q->st.curr_cell = tmp;
	return pl_success;
}
#endif

static USE_RESULT pl_status fn_split_atom_4(query *q)
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

		cell tmp;
		may_error(make_cstringn(&tmp, start, ptr-start));

		if (nbr++ == 1)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);

		start = ptr + 1;
		in_list = 1;
	}

	if (*start) {
		while (peek_char_utf8(start) == pad)
			get_char_utf8(&start);

		cell tmp;
		may_error(make_cstring(&tmp, start));

		if (!in_list)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);
	}

	l = end_list(q);
	may_ptr_error(l);
	pl_status ok = unify(q, p4, p4_ctx, l, q->st.curr_frame);
	return ok;
}

static USE_RESULT pl_status fn_split_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_nil(p1) || !strcmp(GET_STR(p1), "")) {
		cell tmp;
		make_literal(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return pl_failure;

		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	const char *start = GET_STR(p1), *ptr;
	int ch = peek_char_utf8(GET_STR(p2));

	if ((ptr = strchr_utf8(start, ch)) != NULL) {
		cell tmp;

		if (ptr != start)
			may_error(make_stringn(&tmp, start, ptr-start));
		else
			make_literal(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
			DECR_REF(&tmp);
			return pl_failure;
		}

		DECR_REF(&tmp);
		ptr = ptr+1;

		while (isspace(*ptr))
			ptr++;

		if (*ptr)
			may_error(make_stringn(&tmp, ptr, LEN_STR(p1)-(ptr-start)));
		else
			make_literal(&tmp, g_nil_s);

		pl_status ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return ok;
	}

	if (!unify(q, p3, p3_ctx, p1, p1_ctx))
		return pl_failure;

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_savefile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	FILE *fp = fopen(filename, "wb");
	ensure(fp);
	fwrite(GET_STR(p2), 1, LEN_STR(p2), fp);
	fclose(fp);
	free(src);
	return pl_success;
}

static USE_RESULT pl_status fn_loadfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	FILE *fp = fopen(filename, "rb");
	free(src);

	if (!fp)
		return throw_error(q, p1, "existence_error", "cannot_open_file");

	struct stat st = {0};

#ifdef _POSIX_C_SOURCE
	// the POSIX variant has no race
	if (fstat(fileno(fp), &st)) {
		return pl_error;
	}
#else
	if (stat(filename, &st)) {
		return pl_error;
	}
#endif

	char *s = malloc(st.st_size+1);
	may_ptr_error(s, fclose(fp));

	if (fread(s, 1, st.st_size, fp) != (size_t)st.st_size) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	may_error(make_stringn(&tmp, s, st.st_size), free(s));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	free(s);
	return pl_success;
}

static USE_RESULT pl_status fn_getfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		may_ptr_error(src);
		filename = src;
	} else
		filename = GET_STR(p1);

	FILE *fp = fopen(filename, "r");
	free(src);

	if (!fp) {
		free(filename);
		return throw_error(q, p1, "existence_error", "cannot_open_file");
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

		cell tmp;
		may_error(make_stringn(&tmp, line, len));

		if (nbr++ == 1)
			allocate_list(q, &tmp);
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
		may_ptr_error(l);
		set_var(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return pl_success;
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

static USE_RESULT pl_status fn_server_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,list_or_nil);
	char hostname[1024], path[4096];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, level = 0;
	unsigned port = 80;
	snprintf(hostname, sizeof(hostname), "localhost");
	path[0] = '\0';
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

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
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(p1);
	parse_host(url, hostname, path, &port, &ssl);
	nonblock = q->is_task;

	int fd = net_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1)
		return throw_error(q, p1, "existence_error", "server_failed");

	int n = new_stream();

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, "resource_error", "too_many_streams");
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
		return throw_error(q, p1, "existence_error", "cannot_open_stream");
		close(fd);
	}

	net_set_nonblocking(str);
	cell *tmp = alloc_on_heap(q, 1);
	ensure(tmp);
	make_int(tmp, n);
	tmp->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p2, p2_ctx, tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	int fd = net_accept(str);

	if (fd == -1) {
		if (q->is_task) {
			do_yield_0(q, 10);
			return pl_failure;
		}

		//printf("*** here\n");
		return pl_failure;
	}

	n = new_stream();

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, "resource_error", "too_many_streams");
	}

	stream *str2 = &g_streams[n];
	str2->filename = strdup(str->filename);
	str2->name = strdup(str->name);
	str2->mode = strdup("update");
	str->socket = true;
	str2->nodelay = str->nodelay;
	str2->nonblock = str->nonblock;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		close(fd);
		return throw_error(q, p1, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str2->sslptr = net_enable_ssl(fd, str->name, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			return pl_failure;
		}
	}

	net_set_nonblocking(str2);
	may_error(make_choice(q));
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_client_5(query *q)
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
	LIST_HANDLER(p5);

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

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
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(p1);
	parse_host(url, hostname, path, &port, &ssl);
	nonblock = q->is_task;

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!strcmp(GET_STR(c), "host")) {
				c = c + 1;

				//if (is_atom(c))
				//	;//udp = !strcmp(GET_STR(c), "true") ? 1 : 0;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	int fd = net_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return throw_error(q, p1, "resource_error", "could_not_connect");

	int n = new_stream();

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, "resource_error", "too_many_streams");
	}

	stream *str = &g_streams[n];
	str->filename = strdup(GET_STR(p1));
	str->name = strdup(hostname);
	str->mode = strdup("update");
	str->socket = true;
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r+");

	if (!str->filename || !str->name || !str->mode) {
		free(str->filename);
		free(str->name);
		free(str->mode); //cehteh: maybe from pool?
		return pl_error;
	}

	if (str->fp == NULL) {
		close(fd);
		return throw_error(q, p1, "existence_error", "cannot_open_stream");
	}

	if (ssl) {
		str->sslptr = net_enable_ssl(fd, hostname, 0, str->level, certfile);
		may_ptr_error (str->sslptr, close(fd));
	}

	if (nonblock)
		net_set_nonblocking(str);

	cell tmp;
	may_error(make_string(&tmp, hostname));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	may_error(make_string(&tmp, path));
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	cell *tmp2 = alloc_on_heap(q, 1);
	ensure(tmp2);
	make_int(tmp2, n);
	tmp2->flags |= FLAG_STREAM | FLAG_HEX;
	set_var(q, p4, p4_ctx, tmp2, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_getline_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->st.m->pl->current_input;
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
		return pl_error; // may_error when pl_error == -1
	}

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp;

	if (strlen(line))
		may_error(make_string(&tmp, line), free(line));
	else
		make_literal(&tmp, g_nil_s);

	free(line);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_getline_2(query *q)
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

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			do_yield_0(q, 1);
			return pl_failure;
		}

		return pl_failure;
	}

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp;

	if (strlen(line))
		may_error(make_string(&tmp, line), free(line));
	else
		make_literal(&tmp, g_nil_s);

	free(line);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_bread_3(query *q)
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
			ensure(str->data);
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
				return pl_failure;
			}

			if (q->is_task) {
				clearerr(str->fp);
				do_yield_0(q, 1);
				return pl_failure;
			}
		}

		cell tmp;
		may_error(make_stringn(&tmp, str->data, str->data_len), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		free(str->data);
		str->data = NULL;
		return pl_success;
	}

	if (is_integer(p1)) {
		if (!str->data) {
			str->data = malloc((str->alloc_nbytes=1024*1)+1);
			ensure(str->data);
			str->data_len = 0;
		}

		size_t nbytes = net_read(str->data, str->alloc_nbytes, str);
		str->data[nbytes] = '\0';
		str->data = realloc(str->data, nbytes+1);
		ensure(str->data);
		cell tmp;
		may_error(make_stringn(&tmp, str->data, nbytes), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		free(str->data);
		str->data = NULL;
		return pl_success;
	}

	if (!str->data) {
		str->data = malloc((str->alloc_nbytes=1024*1)+1);
		ensure(str->data);
		str->data_len = 0;
	}

	for (;;) {
		size_t len = str->alloc_nbytes - str->data_len;
		size_t nbytes = net_read(str->data+str->data_len, len, str);
		str->data_len += nbytes;
		str->data[str->data_len] = '\0';

		if (!nbytes || feof(str->fp))
			break;

		if (str->alloc_nbytes == str->data_len) {
			str->data = realloc(str->data, (str->alloc_nbytes*=2)+1);
			ensure(str->data);
		}
	}

	cell tmp1;
	make_int(&tmp1, str->data_len);
	set_var(q, p1, p1_ctx, &tmp1, q->st.curr_frame);
	cell tmp2;

	if (str->data_len)
		may_error(make_stringn(&tmp2, str->data, str->data_len), free(str->data));
	else
		make_literal(&tmp2, g_nil_s);

	set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	DECR_REF(&tmp2);
	free(str->data);
	str->data = NULL;
	return pl_success;
}

static USE_RESULT pl_status fn_bwrite_2(query *q)
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
				return pl_error; // can feof() happen on writing?
		}

		// TODO: make this yieldable

		clearerr(str->fp);
		len -= nbytes;
		src += nbytes;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_read_term_from_chars_2(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];
	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = LEN_STR(p_chars);
		src = malloc(len+1);
		ensure(src);
		memcpy(src, GET_STR(p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len) {
			return throw_error(q, p_chars, "type_error", "atom");
		}

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, "type_error", "chars");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	cell tmp;
	make_literal(&tmp, g_nil_s);
	pl_status ok = do_read_term(q, str, p_term, p_term_ctx, &tmp, q->st.curr_frame, src);
	free(src);
	return ok;
}

static USE_RESULT pl_status fn_read_term_from_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_opts,any);
	GET_NEXT_ARG(p_term,any);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = LEN_STR(p_chars);
		src = malloc(len+1+1);	// final +1 is for look-ahead
		ensure(src);
		memcpy(src, GET_STR(p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len) {
			return throw_error(q, p_chars, "type_error", "atom");
		}

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, "type_error", "chars");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	pl_status ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	free(src);
	return ok;
}

static USE_RESULT pl_status fn_read_term_from_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,any);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = LEN_STR(p_chars);
		src = malloc(len+1+1);	// final +1 is for look-ahead
		ensure(src);
		memcpy(src, GET_STR(p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len) {
			return throw_error(q, p_chars, "type_error", "atom");
		}

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, "type_error", "chars");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	pl_status ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	free(src);
	return ok;
}

static USE_RESULT pl_status fn_write_term_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p_chars,any);
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		parse_write_params(q, c, NULL, NULL);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	q->max_depth = q->quoted = q->nl = q->fullstop = false;
	q->ignore_ops = false;
	q->variable_names = NULL;
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_write_canonical_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p_chars,any);
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		parse_write_params(q, c, NULL, NULL);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	q->max_depth = q->quoted = q->nl = q->fullstop = false;
	q->ignore_ops = false;
	q->variable_names = NULL;
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_is_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_valid_list(q, p1, p1_ctx, false);
}

static USE_RESULT pl_status fn_sys_mustbe_pairlist_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_valid_list(q, p1, p1_ctx, true)
		&& !is_valid_list(q, p1, p1_ctx, false))
		return throw_error(q, p1, "instantiation_error", "tail_is_a_variable");

	if (!is_valid_list(q, p1, p1_ctx, false))
		return throw_error(q, p1, "type_error", "list");

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (is_variable(h))
			return throw_error(q, h, "instantiation_error", "not_sufficiently_instantiated");

		if (!is_literal(h) || (h->arity != 2) || (h->val_off != g_minus_s))
			return throw_error(q, h, "type_error", "pair");

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_sys_mustbe_pairlist_or_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1))
		return pl_success;

	return fn_sys_mustbe_pairlist_1(q);
}

static USE_RESULT pl_status fn_sys_mustbe_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_valid_list(q, p1, p1_ctx, true)
		&& !is_valid_list(q, p1, p1_ctx, false))
		return throw_error(q, p1, "instantiation_error", "tail_is_a_variable");

	if (!is_valid_list(q, p1, p1_ctx, false))
		return throw_error(q, p1, "type_error", "list");

	return pl_success;
}

static USE_RESULT pl_status fn_sys_mustbe_list_or_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1))
		return pl_success;

	if (!is_valid_list(q, p1, p1_ctx, true))
		return throw_error(q, p1, "type_error", "list");

	return pl_success;
}

static USE_RESULT pl_status fn_sys_mustbe_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_callable(p1))
		return throw_error(q, p1, "type_error", "callable");

	return pl_success;
}

static USE_RESULT pl_status fn_sys_mustbe_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_atom(p1))
		return throw_error(q, p1, "type_error", "atom");

	return pl_success;
}

static USE_RESULT pl_status fn_is_stream_1(query *q)
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

static USE_RESULT pl_status fn_wait_0(query *q)
{
	while (!g_tpl_interrupt && q->st.m->tasks) {
		uint_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned did_something = 0, spawn_cnt = 0;

		while (!g_tpl_interrupt && task) {
			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= g_cpu_count)
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
				task = pop_task(q->st.m, task);
				destroy_query(save);
				continue;
			}

			DISCARD_RESULT query_start(task);

			task = task->next;
			did_something = 1;
		}

		if (!did_something)
			msleep(1);
	}

	return pl_success;
}

static USE_RESULT pl_status fn_await_0(query *q)
{
	while (!g_tpl_interrupt && q->st.m->tasks) {
		uint_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned did_something = 0, spawn_cnt = 0;

		while (!g_tpl_interrupt && task) {
			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= g_cpu_count)
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
				task = pop_task(q->st.m, task);
				destroy_query(save);
				continue;
			}

			DISCARD_RESULT query_start(task);

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

	if (!q->st.m->tasks)
		return pl_failure;

	may_error(make_choice(q));
	return pl_success;
}

static USE_RESULT pl_status fn_yield_0(query *q)
{
	if (q->retry)
		return pl_success;

	do_yield_0(q, 0);
	return pl_failure;
}

static USE_RESULT pl_status fn_task_n(query *q)
{
	GET_FIRST_ARG(p1,callable);
	clone_to_tmp(q, p1);
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		cell *p2 = get_next_raw_arg(q);
		clone2_to_tmp(q, p2);
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;
	bool found = false;

	if ((tmp2->fn = get_builtin(q->st.m->pl, GET_STR(tmp2), arity, &found)), found)
		tmp2->flags |= FLAG_BUILTIN;
	else {
		tmp2->match = search_predicate(q->st.m, tmp2);
		tmp2->flags &= ~FLAG_BUILTIN;
	}

	cell *tmp = clone_to_heap(q, false, tmp2, 0);
	query *task = create_task(q, tmp);
	task->yielded = task->spawned = true;
	push_task(q->st.m, task);
	return pl_success;
}

static USE_RESULT pl_status fn_fork_0(query *q)
{
	cell *curr_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;
	query *task = create_task(q, curr_cell);
	task->yielded = true;
	push_task(q->st.m, task);
	return pl_failure;
}

static USE_RESULT pl_status fn_send_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	query *dstq = q->parent ? q->parent : q;
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	may_ptr_error(c);

	if (c == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	for (idx_t i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		INCR_REF(c2);
	}

	alloc_on_queuen(dstq, 0, c);
	q->yielded = true;
	return pl_success;
}

static USE_RESULT pl_status fn_recv_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell *c = pop_queue(q);
	return unify(q, p1, p1_ctx, c, q->st.curr_frame);
}

static USE_RESULT pl_status fn_pid_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, getpid());
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_wall_time_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, time(NULL));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_date_time_7(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,variable);
	GET_NEXT_ARG(p5,variable);
	GET_NEXT_ARG(p6,variable);
	GET_NEXT_ARG(p7,variable);
	struct tm tm;
	time_t now = time(NULL);
	localtime_r(&now, &tm);
	cell tmp;
	make_int(&tmp, tm.tm_year+1900);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mon+1);
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mday);
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_hour);
	set_var(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_min);
	set_var(q, p5, p5_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_sec);
	set_var(q, p6, p6_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, 0);
	set_var(q, p7, p7_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_shell_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	DISCARD_RESULT system(GET_STR(p1));
	return pl_success;
}

static USE_RESULT pl_status fn_shell_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	int status = system(GET_STR(p1));
	cell tmp;
	make_int(&tmp, status);
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_absolute_file_name_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p_opts,list_or_nil);
	int expand = 0;
	char *src = NULL, *filename;
	char *here = strdup(q->st.m->filename);
	char *ptr = here + strlen(here) - 1;

	while (*ptr && (*ptr != '/')) {
		ptr--;
		*ptr = '\0';
	}

	char *cwd = here;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	LIST_HANDLER(p_opts);

	while (is_list(p_opts)) {
		cell *h = LIST_HEAD(p_opts);
		h = deref(q, h, p_opts_ctx);

		if (is_structure(h) && (h->arity == 1)) {
			if (!strcmp(GET_STR(h), "expand")) {
				if (is_literal(h+1)) {
					if (!strcmp(GET_STR(h+1), "true"))
						expand = 1;
				}
			} else if (!strcmp(GET_STR(h), "relative_to")) {
				if (is_atom(h+1))
					cwd = GET_STR(h+1);
			}
		}

		p_opts = LIST_TAIL(p_opts);
		p_opts = deref(q, p_opts, p_opts_ctx);
		p_opts_ctx = q->latest_ctx;
	}

	char *tmpbuf = NULL;
	const char *s = filename;

	if (expand && (*s == '$')) {
		char envbuf[1024];
		char *dst = envbuf;
		s++;

		while (*s && (*s != '/') && ((dst-envbuf-1) != sizeof(envbuf)))
			*dst++ = *s++;

		if (*s == '/')
			s++;

		*dst = '\0';
		char *ptr = getenv(envbuf);
		if (!ptr)
			return throw_error(q, p1, "existence_error", "environment_variable");

		size_t buflen = strlen(ptr)+1+strlen(s)+1;
		tmpbuf = malloc(buflen);
		may_ptr_error(tmpbuf);
		snprintf(tmpbuf, buflen, "%s/%s", ptr, s);
		char *tmpbuf2;

		if ((tmpbuf2 = realpath(tmpbuf, NULL)) == NULL) {
			if ((tmpbuf2 = realpath(cwd, NULL)) == NULL)
				tmpbuf2 = realpath(".", NULL);

			may_ptr_error(tmpbuf2, free(tmpbuf));
			size_t buflen = strlen(tmpbuf2)+1+strlen(s)+1;
			char *tmp = malloc(buflen);
			may_ptr_error(tmp, free(tmpbuf2));
			snprintf(tmp, buflen, "%s/%s", tmpbuf2, s);
			free(tmpbuf);
			tmpbuf = tmp;
		} else {
			free(tmpbuf);
			tmpbuf = tmpbuf2;
		}
	} else {
		if ((tmpbuf = realpath(s, NULL)) == NULL) {
			if ((tmpbuf = realpath(cwd, NULL)) == NULL)
				tmpbuf = realpath(".", NULL);

			may_ptr_error(tmpbuf);

			if (*s != '/') {
				size_t buflen = strlen(tmpbuf)+1+strlen(s)+1;
				char *tmp = malloc(buflen);
				may_ptr_error(tmp, free(tmpbuf));
				snprintf(tmp, buflen, "%s/%s", tmpbuf, s);
				free(tmpbuf);
				tmpbuf = tmp;
			} else {
				tmpbuf = strdup(s);
				may_ptr_error(tmpbuf);
			}
		}
	}

	if (cwd != here)
		free(cwd);

	free(here);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, tmpbuf), free(tmpbuf); free(src));
	else
		may_error(make_cstring(&tmp, tmpbuf), free(tmpbuf); free(src));

	free(tmpbuf);
	free(src);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status do_consult(query *q, cell *p1, idx_t p1_ctx)
{
	if (is_atom(p1)) {
		const char *src = GET_STR(p1);
		deconsult(q->st.m->pl, src);

		char *filename = relative_to(q->st.m->filename, src);

		if (!module_load_file(q->st.m, filename)) {
			free(filename);
			return throw_error(q, p1, "existence_error", "filespec");
		}

		free(filename);
		return pl_success;
	}

	if (strcmp(GET_STR(p1), ":"))
		return throw_error(q, p1, "type_error", "filespec");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, "type_error", "filespec");

	module *tmp_m = create_module(q->st.m->pl, GET_STR(mod));
	char *filename = GET_STR(file);
	deconsult(q->st.m->pl, filename);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);

	if (!module_load_file(tmp_m, filename)) {
		destroy_module(tmp_m);
		free(filename);
		return throw_error(q, p1, "existence_error", "filespec");
	}

	free(filename);
	return pl_success;
}

static USE_RESULT pl_status fn_consult_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);

	if (!is_iso_list(p1)) {
		may_error(do_consult(q, p1, p1_ctx));
		return pl_success;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		may_error(do_consult(q, c, q->latest_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return pl_success;
}

#if 1
static int format_integer(char *dst, int_t v, int grouping, int sep, int decimals)
{
	char tmpbuf1[256], tmpbuf2[256];
	sprint_int(tmpbuf1, sizeof(tmpbuf1), v, 10);
	const char *src = tmpbuf1 + (strlen(tmpbuf1) - 1);
	char *dst2 = tmpbuf2;
	int i = 1, j = 1;

	while (src >= tmpbuf1) {
		*dst2++ = *src--;

		if (grouping && !decimals && !(i++ % grouping) && *src)
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

static USE_RESULT pl_status do_format(query *q, cell *str, idx_t str_ctx, cell* p1, cell* p2, idx_t p2_ctx)
{
	char *srcbuf = GET_STR(p1);
	const char *src = srcbuf;
	size_t bufsiz = strlen(src)+100;
	char *tmpbuf = malloc(bufsiz);
	may_ptr_error(tmpbuf);
	char *dst = tmpbuf;
	*dst = '\0';
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

		LIST_HANDLER(p2);

		cell *head = LIST_HEAD(p2);
		c = deref(q, head, p2_ctx);
		idx_t c_ctx = q->latest_ctx;
		p2 = LIST_TAIL(p2);

		if (ch == 'i')
			continue;

		int canonical = 0;
		size_t len;

		if (ch == 'k')
			canonical = 1;

		if ((ch == 'a') && !is_atom(c)) {
			free(tmpbuf);
			return throw_error(q, c, "type_error", "atom");
		}

		if ((ch == 's') && !is_string(c)) {
			free(tmpbuf);
			return throw_error(q, c, "type_error", "atom");
		}

		if (((ch == 'd') || (ch == 'D')) && !is_integer(c)) {
			free(tmpbuf);
			return throw_error(q, c, "type_error", "integer");
		}

		if (ch == 'c') {
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, "type_error", "integer");
			}

			len = 10;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf); //TODO: use alloc_grow
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = put_char_utf8(dst, (int)c->val_num);
		} else if ((ch == 'e') || (ch == 'E')) {
			if (!is_float(c)) {
				free(tmpbuf);
				return throw_error(q, c, "type_error", "float");
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf); //TODO: use alloc_grow
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
				return throw_error(q, c, "type_error", "float");
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf); //TODO: use alloc_grow
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = sprintf(dst, "%.*f", argval, c->val_flt);
		} else if (ch == 'I') {
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, "type_error", "integer");
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = format_integer(dst, c->val_num, noargval?3:argval, '_', 0);
		} else if (ch == 'd') {
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, "type_error", "integer");
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			len = format_integer(dst, c->val_num, 0, ',', noargval?2:argval);
		} else if (ch == 'D') {
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, "type_error", "integer");
			}

			len = 40;

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf);
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
				len = print_canonical_to_buf(q, NULL, 0, c, c_ctx, 1, false, 0);
			else
				len = print_term_to_buf(q, NULL, 0, c, c_ctx, 1, false, 0);

			while (nbytes < len) {
				size_t save = dst - tmpbuf;
				tmpbuf = realloc(tmpbuf, bufsiz*=2);
				ensure(tmpbuf);
				dst = tmpbuf + save;
				nbytes = bufsiz - save;
			}

			if (canonical)
				len = print_canonical_to_buf(q, dst, nbytes, c, c_ctx, 1, false, 0);
			else
				len = print_term_to_buf(q, dst, nbytes, c, c_ctx, 1, false, 0);

			q->quoted = saveq;
		}

		dst += len;
		nbytes -= len;
	}

	*dst = '\0';
	size_t len = dst - tmpbuf;

	if (str == NULL) {
		int n = q->st.m->pl->current_output;
		stream *str = &g_streams[n];
		net_write(tmpbuf, len, str);
	} else if (is_structure(str) && ((strcmp(GET_STR(str),"atom") && strcmp(GET_STR(str),"chars") && strcmp(GET_STR(str),"string")) || (str->arity > 1) || !is_variable(str+1))) {
		free(tmpbuf);
		return throw_error(q, c, "type_error", "structure");
	} else if (is_structure(str) && !strcmp(GET_STR(str),"atom")) {
		cell *c = deref(q, str+1, str_ctx);
		cell tmp;
		may_error(make_cstring(&tmp, tmpbuf), free(tmpbuf));
		set_var(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
	} else if (is_structure(str)) {
		cell *c = deref(q, str+1, str_ctx);
		cell tmp;

		if (strlen(tmpbuf))
			may_error(make_string(&tmp, tmpbuf), free(tmpbuf));
		else
			make_literal(&tmp, g_nil_s);

		set_var(q, c, q->latest_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
	} else if (is_stream(str)) {
		int n = get_stream(q, str);
		stream *str = &g_streams[n];
		const char *src = tmpbuf;

		while (len) {
			size_t nbytes = net_write(src, len, str);

			if (!nbytes) {
				if (feof(str->fp) || ferror(str->fp)) {
					free(tmpbuf);
					fprintf(stdout, "Error: end of file on write\n");
					return pl_error;
				}
			}

			clearerr(str->fp);
			len -= nbytes;
			src += nbytes;
		}
	} else {
		free(tmpbuf);
		return throw_error(q, p1, "type_error", "stream");
	}

	free(tmpbuf);
	return pl_success;
}

static USE_RESULT pl_status fn_format_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	return do_format(q, NULL, 0, p1, !is_nil(p2)?p2:NULL, p2_ctx);
}

static USE_RESULT pl_status fn_format_3(query *q)
{
	GET_FIRST_ARG(pstr,stream_or_structure);
	GET_NEXT_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	return do_format(q, pstr, pstr_ctx, p1, !is_nil(p2)?p2:NULL, p2_ctx);
}
#endif

#if USE_OPENSSL
static USE_RESULT pl_status fn_sha1_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA_DIGEST_LENGTH];
	SHA1((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	*dst = '\0';
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_sha256_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA256_DIGEST_LENGTH];
	SHA256((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	*dst = '\0';
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_sha512_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *str = GET_STR(p1);
	unsigned char digest[SHA512_DIGEST_LENGTH];
	SHA512((unsigned char*)str, LEN_STR(p1), digest);
	char tmpbuf[512];
	char *dst = tmpbuf;
	*dst = '\0';
	size_t buflen = sizeof(tmpbuf);

	for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
		size_t len = snprintf(dst, buflen, "%02X", digest[i]);
		dst += len;
		buflen -= len;
	}

	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}
#endif

static int do_b64encode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(p1);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc((len*3)+1);
	ensure(dstbuf);
	b64_encode(str, len, &dstbuf, 0, 0);
	cell tmp;
	may_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static int do_b64decode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(p2);
	size_t len = LEN_STR(p2);
	char *dstbuf = malloc(len+1);
	ensure(dstbuf);
	b64_decode(str, len, &dstbuf);
	cell tmp;
	may_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_base64_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_list(p1)) && is_variable(p2))
		return do_b64encode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_b64decode_2(q);

	return throw_error(q, p1, "instantiation_error", "atom");
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

static USE_RESULT pl_status do_urlencode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(p1);
	size_t len = LEN_STR(p1);
	char *dstbuf = malloc((len*3)+1);
	ensure(dstbuf);
	url_encode(str, len, dstbuf);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		may_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status do_urldecode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(p2);
	size_t len = LEN_STR(p2);
	char *dstbuf = malloc(len+1);
	ensure(dstbuf);
	url_decode(str, dstbuf);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		may_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_urlenc_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_string(p1)) && is_variable(p2))
		return do_urlencode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_urldecode_2(q);

	return throw_error(q, p1, "instantiation_error", "atom");
}

static USE_RESULT pl_status fn_string_lower_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	size_t len = LEN_STR(p1);
	char *tmps = malloc(len+1);
	may_ptr_error(tmps);
	memcpy(tmps, GET_STR(p1), len);
	tmps[len] = '\0';
	size_t n = len;

	for (char *s = tmps; n--; s++)
		*s = tolower(*s);

	cell tmp;
	may_error(make_stringn(&tmp, tmps, len), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_string_upper_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	size_t len = LEN_STR(p1);
	char *tmps = malloc(len+1);
	may_ptr_error(tmps);
	memcpy(tmps, GET_STR(p1), len);
	tmps[len] = '\0';
	size_t n = len;

	for (char *s = tmps; n--; s++)
		*s = toupper(*s);

	cell tmp;
	may_error(make_stringn(&tmp, tmps, len), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_access_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	const char *mode = GET_STR(p2);
	int amode = R_OK;

	if (!strcmp(mode, "read"))
		amode = R_OK;
	else if (!strcmp(mode, "write"))
		amode = W_OK;
	else if (!strcmp(mode, "append"))
		amode = W_OK;
	else if (!strcmp(mode, "execute"))
		amode = X_OK;
	else if (!strcmp(mode, "none")) {
		free(src);
		return pl_success;
	} else {
		free(src);
		return throw_error(q, p2, "domain_error", "mode");
	}

	struct stat st = {0};
	int status = stat(filename, &st);

	if (status && (!strcmp(mode, "read") || !strcmp(mode, "exist") || !strcmp(mode, "execute") || !strcmp(mode, "none"))) {
		free(src);
		return pl_failure;
	}

	if (status && (!strcmp(mode, "write") || !strcmp(mode, "append"))) {
		free(src);
		return pl_success;
	}

	return !access(filename, amode);
}

static USE_RESULT pl_status fn_exists_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return pl_failure;
	}

	free(src);

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_directory_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return throw_error(q, p1, "existence_error", "directory");
	}

	DIR *dirp = opendir(filename);

	if (!dirp) {
		free(src);
		return throw_error(q, p1, "existence_error", "directory");
	}

	struct dirent *dire = readdir(dirp);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dire->d_name));
	else
		may_error(make_cstring(&tmp, dire->d_name));

	allocate_list(q, &tmp);

	for (dire = readdir(dirp); dire; dire = readdir(dirp)) {
		if (is_string(p1))
			may_error(make_string(&tmp, dire->d_name));
		else
			may_error(make_cstring(&tmp, dire->d_name));

		append_list(q, &tmp);
	}

	closedir(dirp);
	free(src);
	cell *l = end_list(q);
	pl_status ok = unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return ok;
}

static USE_RESULT pl_status fn_delete_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return throw_error(q, p1, "existence_error", "file");
	}

	remove(filename);
	free(src);
	return pl_success;
}

static USE_RESULT pl_status fn_rename_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *src1 = NULL, *src2 = NULL;
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src1 = chars_list_to_string(q, p1, p1_ctx, len);
		filename1 = src1;
	} else
		filename1 = GET_STR(p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len)
			return throw_error(q, p2, "type_error", "atom");

		src2 = chars_list_to_string(q, p2, p2_ctx, len);
		filename2 = src2;
	} else
		filename2 = GET_STR(p2);

	struct stat st = {0};

	if (stat(filename1, &st)) {
		free(src1);
		free(src2);
		return throw_error(q, p1, "existence_error", "file");
	}

	bool ok = !rename(filename1, filename2);
	free(src1);
	free(src2);
	return ok ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_time_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return throw_error(q, p1, "existence_error", "file");
	}

	free(src);
	cell tmp;
	make_float(&tmp, st.st_mtime);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_size_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,integer_or_var);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return throw_error(q, p1, "existence_error", "file");
	}

	free(src);
	cell tmp;
	make_int(&tmp, st.st_size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_exists_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(src);
		return pl_failure;
	}

	free(src);

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_make_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	struct stat st = {0};

	if (!stat(filename, &st)) {
		free(src);
		return pl_error;
	}

	free(src);
	return !mkdir(filename, 0777);
}

static USE_RESULT pl_status fn_working_directory_2(query *q)
{
	GET_FIRST_ARG(p_old,variable);
	GET_NEXT_ARG(p_new,atom_or_list_or_var);
	char tmpbuf[PATH_MAX], tmpbuf2[PATH_MAX];
	char *oldpath = getcwd(tmpbuf, sizeof(tmpbuf));
	snprintf(tmpbuf2, sizeof(tmpbuf2), "%s%s", oldpath, PATH_SEP);
	oldpath = tmpbuf2;
	cell tmp;
	may_error(make_string(&tmp, oldpath));

	if (is_atom_or_list(p_new)) {
		const char *filename;
		char *src = NULL;

		if (is_iso_list(p_new)) {
			size_t len = scan_is_chars_list(q, p_new, p_new_ctx, true);

			if (!len) {
				DECR_REF(&tmp);
				return throw_error(q, p_new, "type_error", "atom");
			}

			src = chars_list_to_string(q, p_new, p_new_ctx, len);
			filename = src;
		} else
			filename = GET_STR(p_new);

		if (chdir(filename)) {
			DECR_REF(&tmp);
			return throw_error(q, p_new, "existence_error", "path");
		}

		free(src);
	}

	pl_status ok = unify(q, p_old, p_old_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_chdir_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	const char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);
		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = GET_STR(p1);

	pl_status ok = !chdir(filename);
	free(src);
	return ok;
}

static USE_RESULT pl_status fn_edin_skip_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		printf("| ");
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == p1->val_num)
			break;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_edin_skip_2(query *q)
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
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == p1->val_num)
			break;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_edin_tab_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, "type_error", "integer");

	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	for (int i = 0; i < p1.val_num; i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_edin_tab_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, "type_error", "integer");

	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];

	for (int i = 0; i < p1.val_num; i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_edin_seen_0(query *q)
{
	int n = q->st.m->pl->current_input;
	stream *str = &g_streams[n];

	if (n <= 2)
		return pl_success;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->st.m->pl->current_input = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_edin_told_0(query *q)
{
	int n = q->st.m->pl->current_output;
	stream *str = &g_streams[n];

	if (n <= 2)
		return pl_success;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	free(str->filename);
	free(str->mode);
	free(str->name);
	memset(str, 0, sizeof(stream));
	q->st.m->pl->current_output = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_edin_seeing_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name = q->st.m->pl->current_input==0?"user":g_streams[q->st.m->pl->current_input].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return pl_success;
}

static USE_RESULT pl_status fn_edin_telling_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name =q->st.m->pl->current_output==1?"user":g_streams[q->st.m->pl->current_output].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return pl_success;
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

static USE_RESULT pl_status fn_term_hash_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1))
		return pl_success;

	char *dst = print_term_to_strbuf(q, p1, p1_ctx, 1);
	cell tmp;
	make_int(&tmp, do_jenkins_one_at_a_time_hash(dst));
	free(dst);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_hex_chars_2(query *q)
{
	GET_FIRST_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p1,atom_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "atom");

	if (is_variable(p1)) {
		char tmpbuf[256];
		snprintf(tmpbuf, sizeof(tmpbuf), "%llx", (long long)p2->val_num);
		cell tmp;
		may_error(make_string(&tmp, tmpbuf));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	const char *src = GET_STR(p1);
	int_t p1_val = strtoull(src, NULL, 16);

	if (is_variable(p2)) {
		cell tmp;
		make_int(&tmp, p1_val);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	return p1_val == p2->val_num;
}

static USE_RESULT pl_status fn_octal_chars_2(query *q)
{
	GET_FIRST_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p1,atom_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (is_variable(p1)) {
		char tmpbuf[256];
		snprintf(tmpbuf, sizeof(tmpbuf), "%llo", (long long)p2->val_num);
		cell tmp;
		may_error(make_string(&tmp, tmpbuf));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	const char *src = GET_STR(p1);
	int_t p1_val = strtoull(src, NULL, 8);

	if (is_variable(p2)) {
		cell tmp;
		make_int(&tmp, p1_val);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	return p1_val == p2->val_num;
}

static USE_RESULT pl_status fn_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_string(p1);
}

static USE_RESULT pl_status fn_getenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *value = getenv(GET_STR(p1));

	if (!value)
		return pl_failure;

	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, (char*)value));
	else
		may_error(make_cstring(&tmp, (char*)value));

	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_int);

	if (is_atom(p2)) {
		setenv(GET_STR(p1), GET_STR(p2), 1);
	} else {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), p2->val_num, 10);
		setenv(GET_STR(p1), tmpbuf, 1);
	}

	return pl_success;
}

static USE_RESULT pl_status fn_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(GET_STR(p1));
	return pl_success;
}

static USE_RESULT pl_status fn_uuid_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	uuid u;
	uuid_gen(q->st.m->pl, &u);
	char tmpbuf[128];
	uuid_to_buf(&u, tmpbuf, sizeof(tmpbuf));
	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return pl_success;
}

static USE_RESULT pl_status fn_atomic_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1) && is_variable(p2))
		return do_atom_concat_3(q);

	if (is_variable(p3)) {
		if (!is_atomic(p1))
			return throw_error(q, p1, "type_error", "atomic");

		if (!is_atomic(p2))
			return throw_error(q, p2, "type_error", "atomic");

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
			len1 = snprintf(tmpbuf1, sizeof(tmpbuf1), "%.17g", p1->val_flt);
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
			len2 = snprintf(tmpbuf2, sizeof(tmpbuf1), "%.17g", p2->val_flt);
			src2 = tmpbuf2;
		}

		STRING_INITn(tmpbuf, len1+len2);
		STRING_CAT2n(tmpbuf, src1, len1, src2, len2);
		cell tmp;
		may_error(make_cstringn(&tmp, STRING_CSTR(tmpbuf), STRING_LEN(tmpbuf)), STRING_DONE(tmpbuf));
		STRING_DONE(tmpbuf);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	if (is_variable(p1)) {
		if (strcmp(GET_STR(p3)+(LEN_STR(p3)-LEN_STR(p2)), GET_STR(p2)))
			return pl_failure;

		STRING_INIT(tmpbuf);
		STRING_CATn(tmpbuf, GET_STR(p3), LEN_STR(p3)-LEN_STR(p2));
		cell tmp;
		may_error(make_stringn(&tmp, STRING_CSTR(tmpbuf), STRING_LEN(tmpbuf)), STRING_DONE(tmpbuf));
		STRING_DONE(tmpbuf);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		return pl_success;
	}

	if (is_variable(p2)) {
		if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
			return pl_failure;

		char *dst = strdup(GET_STR(p3)+LEN_STR(p1));
		cell tmp;
		may_error(make_string(&tmp, dst), free(dst));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		DECR_REF(&tmp);
		free(dst);
		return pl_success;
	}

	if (strncmp(GET_STR(p3), GET_STR(p1), LEN_STR(p1)))
		return pl_failure;

	if (strcmp(GET_STR(p3)+LEN_STR(p1), GET_STR(p2)))
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_replace_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,variable);

	size_t srclen = LEN_STR(p1);
	size_t dstlen = srclen * LEN_STR(p3);
	const char *src = GET_STR(p1);
	const char *s1 = GET_STR(p2);
	const char *s2 = GET_STR(p3);
	size_t s1len = LEN_STR(p2);
	size_t s2len = LEN_STR(p3);
	STRING_INITn(tmpbuf, dstlen);

	while (srclen > 0) {
		if (!strncmp(src, s1, s1len)) {
			STRING_CATn(tmpbuf, s2, s2len);
			src += s1len;
			srclen -= s1len;
		} else {
			STRING_CATn(tmpbuf, src, 1);
			src++;
			srclen--;
		}
	}

	cell tmp;

	if (STRING_LEN(tmpbuf))
		may_error(make_stringn(&tmp, STRING_CSTR(tmpbuf), STRING_LEN(tmpbuf)), STRING_DONE(tmpbuf));
	else
		make_literal(&tmp, g_nil_s);

	STRING_DONE(tmpbuf);
	set_var(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	DECR_REF(&tmp);
	return pl_success;
}

static void load_properties(module *m);

static USE_RESULT pl_status fn_sys_load_properties_0(query *q)
{
	load_properties(q->st.m);
	return pl_success;
}

static void load_ops(query *q);

static USE_RESULT pl_status fn_sys_load_ops_0(query *q)
{
	load_ops(q);
	return pl_success;
}

static USE_RESULT pl_status fn_legacy_predicate_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *f = GET_STR(p1);
	cell tmp;
	bool found = false;

	if (get_builtin(q->st.m->pl, f, p1->arity, &found), found) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
		else
			return throw_error(q, p2, "domain_error", "predicate_property");
	}

	predicate *h = find_functor(q->st.m, f, p1->arity);

	if (h && !h->is_dynamic && !is_variable(p2)) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "built_in"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && h->is_multifile) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "multifile"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && h->is_dynamic) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "dynamic"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && !h->is_dynamic) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && h->is_persist) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "persist"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && h->is_public) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "public"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h && h->is_public) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "exported"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (h) {
		make_literal(&tmp, index_from_pool(q->st.m->pl, "visible"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	return pl_failure;
}

static unsigned fake_collect_vars(query *q, cell *p1, idx_t nbr_cells, cell **slots, int depth)
{
	if (depth > MAX_DEPTH)
		return 0;

	unsigned cnt = 0;

	for (idx_t i = 0; i < nbr_cells;) {
		cell *c = p1;

		if (is_structure(c)) {
			cnt += fake_collect_vars(q, c+1, c->nbr_cells-1, slots, depth+1);
		} else if (is_variable(c)) {
			assert(c->var_nbr < MAX_ARITY);

			if (!slots[c->var_nbr]) {
				slots[c->var_nbr] = c;
				cnt++;
			}
		}

		i += p1->nbr_cells;
		p1 += p1->nbr_cells;
	}

	return cnt;
}

unsigned fake_numbervars(query *q, cell *p1, idx_t p1_ctx, unsigned start)
{
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	ensure(tmp);
	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	unify(q, p1, p1_ctx, tmp, q->st.curr_frame);	// undo???
	cell *slots[MAX_ARITY] = {0};
	fake_collect_vars(q, tmp, tmp->nbr_cells, slots, 0);
	memset(q->nv_mask, 0, MAX_ARITY);
	unsigned end = q->nv_start = start;

	for (unsigned i = 0; i < MAX_ARITY; i++) {
		if (!slots[i])
			continue;

		q->nv_mask[slots[i]->var_nbr] = 1;
		end++;
	}

	return end;
}

static unsigned real_numbervars(query *q, cell *p1, idx_t p1_ctx, int *end)
{
	unsigned cnt = 0;

	if (is_variable(p1)) {
		cell *tmp = alloc_on_heap(q, 2);
		make_structure(tmp+0, g_sys_var_s, NULL, 1, 1);
		make_int(tmp+1, *end); *end = *end + 1;
		tmp->flags |= FLAG2_QUOTED;
		set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
		cnt++;
		return cnt;
	}

	if (!is_structure(p1))
		return cnt;

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = deref(q, p1, p1_ctx);

		if (is_variable(c)) {
			cell *tmp = alloc_on_heap(q, 2);
			make_structure(tmp+0, g_sys_var_s, NULL, 1, 1);
			make_int(tmp+1, *end); *end = *end + 1;
			tmp->flags |= FLAG2_QUOTED;
			set_var(q, c, q->latest_ctx, tmp, q->st.curr_frame);
			cnt++;
		} else if (is_structure(c))
			cnt += real_numbervars(q, c, q->latest_ctx, end);

		p1 += p1->nbr_cells;
	}

	q->numbervars = true;
	return cnt;
}

static USE_RESULT pl_status fn_numbervars_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int end = 0;
	real_numbervars(q, p1, p1_ctx, &end);
	return pl_success;
}

static USE_RESULT pl_status fn_numbervars_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);
	int end = q->nv_start = p2->val_num;
	unsigned cnt = real_numbervars(q, p1, p1_ctx, &end);
	cell tmp;
	make_int(&tmp, p2->val_num+cnt);
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

unsigned count_bits(const uint8_t *mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if (mask[i])
			bits++;
	}

	return bits;
}

static USE_RESULT pl_status fn_var_number_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,integer_or_var);
	unsigned pos = count_bits(q->nv_mask, p1->var_nbr);
	cell tmp;
	make_int(&tmp, q->nv_start+pos);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_char_type_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_int);
	GET_NEXT_ARG(p2,atom);
	int ch;

	if (is_atom(p1)) {
		if (LEN_STR(p1) != 1)
			return pl_failure;

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

	return pl_failure;
}

static void restore_db(module *m, FILE *fp)
{
	parser *p = create_parser(m);
	query *q = create_query(m, false);
	ensure(q);
	p->one_shot = true;
	p->fp = fp;
	m->loading = 1;

	for (;;) {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1)
			break;

		p->srcptr = p->save_line;
		parser_tokenize(p, false, false);
		term_xref(p, p->t, NULL);
		query_execute(q, p->t);
		clear_term(p->t);
	}

	m->loading = 0;
	destroy_query(q);
	destroy_parser(p);
}

void do_db_load(module *m)
{
	if (!m->use_persist)
		return;

	char filename[1024*4];
	snprintf(filename, sizeof(filename), "%s.db", m->name);
	char filename2[1024*4];
	snprintf(filename2, sizeof(filename2), "%s.TMP", m->name);
	struct stat st;

	if (!stat(filename2, &st) && !stat(filename, &st))
		remove(filename2);
	else if (!stat(filename2, &st))
		rename(filename2, filename);

	if (!stat(filename, &st)) {
		FILE *fp = fopen(filename, "rb");
		ensure(fp);
		restore_db(m, fp);
		fclose(fp);
	}

	m->fp = fopen(filename, "ab");
	ensure(m->fp);
}

static USE_RESULT pl_status fn_sys_db_load_0(query *q)
{
	do_db_load(q->st.m);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_db_save_0(query *q)
{
	if (!q->st.m->fp)
		return pl_failure;

	if (strlen(q->st.m->name) >= 1024*4-4)
		return pl_error;

	fclose(q->st.m->fp);
	char filename[1024*4];
	snprintf(filename, sizeof(filename), "%s.db", q->st.m->name);
	char filename2[1024*4];
	snprintf(filename2, sizeof(filename2), "%s.TMP", q->st.m->name);
	FILE *fp = fopen(filename2, "wb");
	may_ptr_error(fp);
	save_db(q->st.m->fp, q, 1);
	fclose(fp);
	remove(filename);
	rename(filename2, filename);
	q->st.m->fp = fopen(filename, "ab");
	may_ptr_error(q->st.m->fp);
	return pl_success;
}

static USE_RESULT pl_status fn_abolish_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);
	cell tmp = *p1;
	tmp.arity = p2->val_num;
	CLR_OP(&tmp);
	return do_abolish(q, &tmp, &tmp, true);
}

static USE_RESULT pl_status fn_sys_lt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ < p2->val_num)
		return pl_success;

	drop_choice(q);
	return pl_success;
}

static USE_RESULT pl_status fn_succ_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	if (is_integer(p1) && (p1->val_num < 0))
		return throw_error(q, p1, "domain_error", "not_less_than_zero");

	if (is_integer(p2) && (p2->val_num < 0))
		return throw_error(q, p2, "domain_error", "not_less_than_zero");

	if (is_variable(p2)) {
		cell tmp;
		make_int(&tmp, p1->val_num+1);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p1)) {
		if (p2->val_num == 0)
			return 0;

		cell tmp;
		make_int(&tmp, p2->val_num-1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	return p1->val_num == (p2->val_num - 1);
}

static USE_RESULT pl_status fn_sys_instantiated_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	return pl_success;
}

static USE_RESULT pl_status fn_sys_instantiated_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, "instantiation_error", "not_sufficiently_instantiated");

	return pl_success;
}

static USE_RESULT pl_status fn_plus_3(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p3,integer_or_var);

	if (is_variable(p1)) {
		if (!is_integer(p2))
			return throw_error(q, p2, "type_error", "integer");

		if (!is_integer(p3))
			return throw_error(q, p3, "type_error", "integer");

		cell tmp;
		make_int(&tmp, p3->val_num - p2->val_num);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (is_variable(p2)) {
		if (!is_integer(p1))
			return throw_error(q, p1, "type_error", "integer");

		if (!is_integer(p3))
			return throw_error(q, p3, "type_error", "integer");

		cell tmp;
		make_int(&tmp, p3->val_num - p1->val_num);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (is_variable(p3)) {
		if (!is_integer(p2))
			return throw_error(q, p2, "type_error", "integer");

		if (!is_integer(p1))
			return throw_error(q, p1, "type_error", "integer");

		cell tmp;
		make_int(&tmp, p1->val_num + p2->val_num);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	return p3->val_num == p1->val_num + p2->val_num;
}

static USE_RESULT pl_status fn_limit_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p2, 4);
	idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_lt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p1->val_num);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_gt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ <= p2->val_num)
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_offset_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p2, 4);
	idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_gt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p1->val_num);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_del_attrs_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->var_nbr);
	e->c.attrs = NULL;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_put_attrs_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *tmp = deep_clone_to_heap(q, p2, p2_ctx);
	may_ptr_error(tmp);
	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, "resource_error", "cyclic_term");

	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->var_nbr);
	e->c.attrs = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_get_attrs_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,variable);
	frame *g = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(g, p1->var_nbr);

	if (!e->c.attrs) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	set_var(q, p2, p2_ctx, e->c.attrs, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_ne_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (p1->val_num++ != p2->val_num)
		return pl_failure;

	drop_choice(q);
	return pl_success;
}

static USE_RESULT pl_status fn_call_nth_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p2)) {
		cell *tmp = clone_to_heap(q, true, p1, 1);
		idx_t nbr_cells = 1 + p1->nbr_cells;
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	cell *tmp = clone_to_heap(q, true, p1, 4);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_ne_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, p2->val_num);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status do_length(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer);
	unsigned nbr = p2->val_num;
	GET_RAW_ARG(2, p2_orig);
	cell tmp;
	make_int(&tmp, ++nbr);
	reset_value(q, p2_orig, p2_orig_ctx, &tmp, q->st.curr_frame);
	may_error(make_choice(q));

	if (is_variable(p1) && is_anon(p1))
		return pl_success;

	if (nbr >= MAX_VARS) {
		drop_choice(q);
		return throw_error(q, p2, "resource_error", "too_many_vars");
	}

	unsigned var_nbr = 0;

	if (nbr) {
		if (!(var_nbr = create_vars(q, nbr))) {
			drop_choice(q);
			return throw_error(q, p1, "resource_error", "too_many_vars");
		}
	}

	tmp.val_type = TYPE_VARIABLE;
	tmp.nbr_cells = 1;
	tmp.flags = FLAG2_FRESH | FLAG2_ANON;
	tmp.val_off = g_anon_s;
	tmp.var_nbr = var_nbr++;
	tmp.arity = 0;
	allocate_list(q, &tmp);

	for (unsigned i = 1; i < nbr; i++) {
		tmp.var_nbr = var_nbr++;
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_length_2(query *q)
{
	if (q->retry)
		return do_length(q);

	GET_FIRST_ARG(p1,list_or_nil_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1) && is_variable(p2)) {
		cell tmp;
		make_int(&tmp, 0);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		may_error(make_choice(q));

		if (!is_anon(p1)) {
			make_literal(&tmp, g_nil_s);
			set_var(q, p1,p1_ctx, &tmp, q->st.curr_frame);
		}

		return pl_success;
	}

	if (!is_variable(p1) && is_variable(p2)) {
		if (!is_list(p1) && !is_nil(p1))
			return pl_failure;

		unsigned cnt = 0;

		if (is_string(p1)) {
			cnt = strlen_utf8(GET_STR(p1));
		} else {
			cell *l = p1;
			LIST_HANDLER(l);

			while (is_list(l)) {
				LIST_HEAD(l);
				l = LIST_TAIL(l);
				l = deref(q, l, p1_ctx);
				p1_ctx = q->latest_ctx;
				cnt++;
			}
		}

		cell tmp;
		make_int(&tmp, cnt);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (is_integer(p2) && !is_variable(p1)) {
		if (p2->val_num < 0)
			return throw_error(q, p2, "domain_error", "not_less_than_zero");

		if (p2->val_num == 0) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		}

		int cnt = 0;

		if (is_string(p1)) {
			cnt = strlen_utf8(GET_STR(p1));
		} else {
			cell *l = p1;
			LIST_HANDLER(l);

			while (is_list(l)) {
				LIST_HEAD(l);
				l = LIST_TAIL(l);
				l = deref(q, l, p1_ctx);
				p1_ctx = q->latest_ctx;
				cnt++;
			}
		}

		return p2->val_num == cnt;
	}

	if (is_variable(p1) && is_integer(p2)) {
		if (p2->val_num < 0)
			return throw_error(q, p2, "domain_error", "not_less_than_zero");

		if (p2->val_num >= MAX_VARS)
			return throw_error(q, p2, "resource_error", "too_many_vars");

		idx_t nbr = p2->val_num;

		if (nbr == 0) {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			return pl_success;
		}

		unsigned var_nbr = 0;

		if (!(var_nbr = create_vars(q, nbr)))
			return throw_error(q, p2, "resource_error", "too_many_vars");

		cell tmp;
		tmp.val_type = TYPE_VARIABLE;
		tmp.nbr_cells = 1;
		tmp.arity = 0;
		tmp.flags = FLAG2_FRESH | FLAG2_ANON;
		tmp.val_off = g_anon_s;
		tmp.var_nbr = var_nbr++;
		allocate_list(q, &tmp);

		for (idx_t i = 1; i < nbr; i++) {
			tmp.var_nbr = var_nbr++;
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
		return pl_success;
	}

	return throw_error(q, p1, "type_error", "arg_invalid");
}

static USE_RESULT pl_status fn_memberchk_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list);
	LIST_HANDLER(p2);

	if (is_variable(p1)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		set_var(q, p1, p1_ctx, h, q->latest_ctx);
		return pl_success;
	}

	if (is_atom(p1) && is_string(p2)) {
		const char *src = GET_STR(p1);
		size_t len = LEN_STR(p1);
		size_t lench = len_char_utf8(src);

		if (lench == len)
			return memmem(GET_STR(p2), LEN_STR(p2), src, lench) ? pl_success : pl_failure;
	}

	if (is_atom(p1)) {
		const char *src = GET_STR(p1);
		size_t len = LEN_STR(p1);

		while (is_list(p2)) {
			cell *h = LIST_HEAD(p2);
			h = deref(q, h, p2_ctx);

			if (is_atom(h)) {
				size_t lenh = LEN_STR(h);

				if (lenh == len) {
					if (!memcmp(src, GET_STR(h), len))
						return pl_success;
				}
			}

			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		return pl_failure;
	}

	if (is_rational(p1)) {
		while (is_list(p2)) {
			cell *h = LIST_HEAD(p2);
			h = deref(q, h, p2_ctx);

			if (is_rational(h)) {
				if ((p1->val_num == h->val_num)
					&& (p1->val_den == h->val_den))
					return pl_success;
			}

			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		return pl_failure;
	}

	may_error(make_choice(q));
	frame *g = GET_FRAME(q->st.curr_frame);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		try_me(q, g->nbr_vars);

		if (unify(q, p1, p1_ctx, h, q->latest_ctx)) {
			drop_choice(q);
			return pl_success;
		}

		undo_me(q);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	drop_choice(q);
	return pl_failure;
}

static USE_RESULT pl_status fn_sys_put_chars_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &g_streams[n];
	GET_NEXT_ARG(p1,any);
	size_t len;

	if (is_cstring(p1)) {
		const char *src = GET_STR(p1);
		size_t len = LEN_STR(p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, false)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, "type_error", "chars");

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_current_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (!q->retry) {
		if (is_atom(p1)) {
			const char *name = GET_STR(p1);
			return find_module(q->st.m->pl, name) ? pl_success : pl_failure;
		}

		may_error(make_choice(q));
		module *m = q->save_m = find_next_module(q->st.m->pl, NULL);
		cell tmp;
		make_literal(&tmp, index_from_pool(q->st.m->pl, m->name));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	module *m = q->save_m = q->save_m->next;

	if (!m)
		return pl_failure;

	may_error(make_choice(q));
	cell tmp;
	make_literal(&tmp, index_from_pool(q->st.m->pl, m->name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_use_module_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	if (!is_atom(p1) && !is_structure(p1)) return pl_error;
	const char *name = GET_STR(p1);
	char dstbuf[1024*4];

	if (is_structure(p1) && !strcmp(name, "library")) {
		p1 = p1 + 1;
		if (!is_literal(p1)) return pl_error;
		name = GET_STR(p1);
		module *m;

		if ((m = find_module(q->st.m->pl, name)) != NULL) {
			if (!m->fp)
				do_db_load(m);

			return pl_success;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
		    || !strcmp(name, "files"))
			return pl_success;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			ensure(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			STRING_INIT(s1);
			STRING_CAT2(s1, "library/", lib->name);
			m = module_load_text(q->st.m, src, STRING_CSTR(s1));
			STRING_DONE(s1);
			free(src);

			if (m != q->st.m)
				do_db_load(m);

			return pl_success;
		}

		snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
		char *dst = dstbuf + strlen(dstbuf);
		idx_t ctx = 0;
		print_term_to_buf(q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, 0, 0);
		name = dstbuf;
	}

	char *filename = relative_to(q->st.m->filename, name);

	if (!module_load_file(q->st.m, filename)) {
		fprintf(stdout, "Error: module file not found: %s\n", filename);
		free(filename);
		return pl_failure;
	}

	free(filename);
	return pl_success;
}

static USE_RESULT pl_status fn_use_module_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	return fn_use_module_1(q);
}

static USE_RESULT pl_status fn_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (is_variable(p1)) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->st.m->pl, (q->save_m?q->save_m:q->st.m)->name));
		q->save_m = NULL;
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	const char *name = GET_STR(p1);
	module *m = find_module(q->st.m->pl, name);

	if (!m) {
		if (q->p->command)
			fprintf(stdout, "Info: created module '%s'\n", GET_STR(p1));

		m = create_module(q->st.m->pl, GET_STR(p1));
	}

	q->st.m = m;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_register_term_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	may_error(make_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_term = true;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_register_cleanup_1(query *q)
{
	if (q->retry) {
		GET_FIRST_ARG(p1,callable);
		cell *tmp = clone_to_heap(q, true, p1, 3);
		idx_t nbr_cells = 1 + p1->nbr_cells;
		make_structure(tmp+nbr_cells++, g_cut_s, fn_iso_cut_0, 0, 0);
		make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	may_error(make_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_cleanup = true;
	return pl_success;
}

void do_cleanup(query *q, cell *p1)
{
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_local_cut_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
}

static USE_RESULT pl_status fn_sys_chk_is_det_0(query *q)
{
	if (q->cp != q->save_cp) {
		choice *ch = GET_CURR_CHOICE();
		ch->chk_is_det = true;
		return pl_success;
	}

	if (q->retry)
		return pl_success;

	choice *ch = GET_CURR_CHOICE();

	for (;;) {
		if (ch->register_cleanup) {
			if (ch->did_cleanup)
				break;

			ch->did_cleanup = true;
			cell *c = ch->st.curr_cell;
			c = deref(q, c, ch->st.curr_frame);
			cell *p1 = deref(q, c+1, ch->st.curr_frame);

			//printf("*** chk_det: (");
			//print_term(q, stdout, p1, ch->st.curr_frame, 1);
			//printf(")\n");

			may_error(make_barrier(q));
			do_cleanup(q, p1);
			return pl_success;
		}

		ch--;
	}

	return pl_success;
}

pl_status fn_sys_undo_trail_0(query *q)
{
	for (idx_t i = q->undo_tp; i < q->st.tp; i++) {
		const trail *tr = q->trails + q->undo_tp + i;
		const frame *g = GET_FRAME(tr->ctx);
		slot *e = GET_SLOT(g, tr->var_nbr);
		e->save_c = e->c;
		e->c.val_type = TYPE_EMPTY;
		e->c.attrs = tr->attrs;
	}

	return pl_success;
}

pl_status fn_sys_redo_trail_0(query * q)
{
	if (!q->save_tp)
		return pl_failure;

	return pl_failure;
}

static USE_RESULT pl_status fn_iso_compare_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_atom(p1)) {
		if (strcmp(GET_STR(p1), "<")
			&& strcmp(GET_STR(p1), ">")
			&& strcmp(GET_STR(p1), "="))
			return throw_error(q, p1, "domain_error", "order");
	}

	int status = compare(q, p2, p2_ctx, p3, p3_ctx, 0);
	cell tmp;

	make_literal(&tmp,
		     (status == ERR_CYCLE_CMP || status == 0)?
		     g_eq_s:status<0?g_lt_s:g_gt_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static const struct builtins g_predicates_iso[] =
{
	{",", 2, NULL, NULL},

	{"!", 0, fn_iso_cut_0, NULL},
	{":", 2, fn_iso_invoke_2, NULL},
	{"=..", 2, fn_iso_univ_2, NULL},
	{"->", 2, fn_iso_if_then_2, NULL},
	{";", 2, fn_iso_disjunction_2, NULL},
	{"\\+", 1, fn_iso_negation_1, NULL},
	{"once", 1, fn_iso_once_1, NULL},
	{"throw", 1, fn_iso_throw_1, NULL},
	{"$catch", 3, fn_iso_catch_3, NULL},
	{"$call", 1, fn_sys_call_1, NULL},
	{"$call", 2, fn_sys_call_n, NULL},
	{"$call", 3, fn_sys_call_n, NULL},
	{"$call", 4, fn_sys_call_n, NULL},
	{"$call", 5, fn_sys_call_n, NULL},
	{"$call", 6, fn_sys_call_n, NULL},
	{"$call", 7, fn_sys_call_n, NULL},
	{"$call", 8, fn_sys_call_n, NULL},
	{"repeat", 0, fn_iso_repeat_0, NULL},
	{"true", 0, fn_iso_true_0, NULL},
	{"fail", 0, fn_iso_fail_0, NULL},
	{"false", 0, fn_iso_fail_0, NULL},
	{"halt", 0, fn_iso_halt_0, NULL},
	{"halt", 1, fn_iso_halt_1, NULL},
	{"atom", 1, fn_iso_atom_1, NULL},
	{"atomic", 1, fn_iso_atomic_1, NULL},
	{"number", 1, fn_iso_number_1, NULL},
	{"compound", 1, fn_iso_compound_1, NULL},
	{"var", 1, fn_iso_var_1, NULL},
	{"nonvar", 1, fn_iso_nonvar_1, NULL},
	{"ground", 1, fn_iso_ground_1, NULL},
	{"callable", 1, fn_iso_callable_1, NULL},
	{"char_code", 2, fn_iso_char_code_2, NULL},
	{"atom_chars", 2, fn_iso_atom_chars_2, NULL},
	{"atom_codes", 2, fn_iso_atom_codes_2, NULL},
	{"number_chars", 2, fn_iso_number_chars_2, NULL},
	{"number_codes", 2, fn_iso_number_codes_2, NULL},
	{"clause", 2, fn_iso_clause_2, NULL},
	{"length", 2, fn_iso_length_2, NULL},
	{"arg", 3, fn_iso_arg_3, NULL},
	{"functor", 3, fn_iso_functor_3, NULL},
	{"copy_term", 2, fn_iso_copy_term_2, NULL},
	{"term_variables", 2, fn_iso_term_variables_2, NULL},
	{"atom_length", 2, fn_iso_atom_length_2, NULL},
	{"atom_concat", 3, fn_iso_atom_concat_3, NULL},
	{"sub_atom", 5, fn_iso_sub_atom_5, NULL},
	{"current_rule", 1, fn_iso_current_rule_1, NULL},

	{"open", 3, fn_iso_open_3, NULL},
	{"open", 4, fn_iso_open_4, NULL},
	{"close", 1, fn_iso_close_1, NULL},
	{"close", 2, fn_iso_close_2, NULL},
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

	{"current_prolog_flag", 2, fn_iso_current_prolog_flag_2, NULL},
	{"set_prolog_flag", 2, fn_iso_set_prolog_flag_2, NULL},
	{"op", 3, fn_iso_op_3, NULL},
	{"$findall", 3, fn_sys_findall_3, NULL},
	{"$bagof", 3, fn_sys_bagof_3, NULL},
	{"current_predicate", 1, fn_iso_current_predicate_1, NULL},
	{"acyclic_term", 1, fn_iso_acyclic_term_1, NULL},
	{"compare", 3, fn_iso_compare_3, NULL},

	{"=", 2, fn_iso_unify_2, NULL},
	{"\\=", 2, fn_iso_notunify_2, NULL},

	{"current_module", 1, fn_current_module_1, NULL},
	{"use_module", 1, fn_use_module_1, NULL},
	{"use_module", 2, fn_use_module_2, NULL},
	{"module", 1, fn_module_1, NULL},
	{"consult", 1, fn_consult_1, NULL},
	{"listing", 0, fn_listing_0, NULL},
	{"listing", 1, fn_listing_1, NULL},
	{"time", 1, fn_time_1, NULL},
	{"trace", 0, fn_trace_0, NULL},

	{"$register_cleanup", 1, fn_sys_register_cleanup_1, NULL},
	{"$register_term", 1, fn_sys_register_term_1, NULL},
	{"$chk_is_det", 0, fn_sys_chk_is_det_0, NULL},

	{0}
};

static const struct builtins g_predicates_other[] =
{
	{"*->", 2, fn_if_2, NULL},
	{"if", 3, fn_if_3, NULL},

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

	{"ignore", 1, fn_ignore_1, "+callable"},
	{"memberchk", 2, fn_memberchk_2, "?term,+list"},

	{"$put_chars", 2, fn_sys_put_chars_2, "+stream,+chars"},
	{"$undo_trail", 0, fn_sys_undo_trail_0, NULL},
	{"$redo_trail", 0, fn_sys_redo_trail_0, NULL},

#if 1
	{"legacy_format", 2, fn_format_2, "+string,+list"},
	{"legacy_format", 3, fn_format_3, "+stream,+string,+list"},
#endif

	{"abolish", 2, fn_abolish_2, NULL},
	{"assert", 1, fn_iso_assertz_1, NULL},

	{"copy_term_nat", 2, fn_copy_term_nat_2, NULL},
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
	{"pid", 1, fn_pid_1, "-integer"},
	{"shell", 1, fn_shell_1, "+atom"},
	{"shell", 2, fn_shell_2, "+atom,??"},
	{"wall_time", 1, fn_wall_time_1, "-integer"},
	{"date_time", 7, fn_date_time_7, "-yyyy,-m,-d,-h,--m,-s,-ms"},
	{"between", 3, fn_between_3, "+integer,+integer,-integer"},
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
	{"is_list", 1, fn_is_list_1, "+term"},
	{"$mustbe_pairlist", 1, fn_sys_mustbe_pairlist_1, "+pair"},
	{"$mustbe_pairlist_or_var", 1, fn_sys_mustbe_pairlist_or_var_1, "?pair"},
	{"$mustbe_list", 1, fn_sys_mustbe_list_1, "+list"},
	{"$mustbe_list_or_var", 1, fn_sys_mustbe_list_or_var_1, "?list"},
	{"$mustbe_callable", 1, fn_sys_mustbe_callable_1, ":term"},
	{"$mustbe_atom", 1, fn_sys_mustbe_atom_1, "+atom"},
	{"list", 1, fn_is_list_1, "+term"},
	{"is_stream", 1, fn_is_stream_1, "+term"},
	//{"forall", 2, fn_forall_2, "+term,+term"},
	{"term_hash", 2, fn_term_hash_2, "+term,?integer"},
	{"rename_file", 2, fn_rename_file_2, "+string,+string"},
	{"directory_files", 2, fn_directory_files_2, "+pathname,-list"},
	{"delete_file", 1, fn_delete_file_1, "+string"},
	{"exists_file", 1, fn_exists_file_1, "+string"},
	{"access_file", 2, fn_access_file_2, "+string,+mode"},
	{"time_file", 2, fn_time_file_2, "+string,-real"},
	{"size_file", 2, fn_size_file_2, "+string,-integer"},
	{"exists_directory", 1, fn_exists_directory_1, "+string"},
	{"make_directory", 1, fn_make_directory_1, "+string"},
	{"working_directory", 2, fn_working_directory_2, "-string,+string"},
	{"absolute_file_name", 3, fn_absolute_file_name_3, NULL},
	{"chdir", 1, fn_chdir_1, "+string"},
	{"name", 2, fn_iso_atom_codes_2, "?string,?list"},
	{"read_term_from_chars", 2, fn_read_term_from_chars_2, "+chars,-term"},
	{"read_term_from_chars", 3, fn_read_term_from_chars_3, "+chars,+opts,+term"},
	{"read_term_from_atom", 3, fn_read_term_from_atom_3, "+chars,-term,+opts"},
	{"write_term_to_chars", 3, fn_write_term_to_chars_3, "+term,+list,?chars"},
	{"write_canonical_to_chars", 3, fn_write_canonical_to_chars_3, "+term,+list,?chars"},
	{"base64", 2, fn_base64_2, "?string,?string"},
	{"urlenc", 2, fn_urlenc_2, "?string,?string"},
	{"string_lower", 2, fn_string_lower_2, "?string,?string"},
	{"string_upper", 2, fn_string_upper_2, "?string,?string"},
	{"bread", 3, fn_bread_3, "+stream,+integer,-string"},
	{"bwrite", 2, fn_bwrite_2, "+stream,-string"},
	{"hex_chars", 2, fn_hex_chars_2, "?integer,?string"},
	{"octal_chars", 2, fn_octal_chars_2, "?integer,?string"},
	{"legacy_predicate_property", 2, fn_legacy_predicate_property_2, "+callable,?string"},
	{"$load_properties", 0, fn_sys_load_properties_0, NULL},
	{"$load_ops", 0, fn_sys_load_ops_0, NULL},
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
	{"getenv", 2, fn_getenv_2, NULL},
	{"setenv", 2, fn_setenv_2, NULL},
	{"unsetenv", 1, fn_unsetenv_1, NULL},
	{"load_files", 2, fn_consult_1, "+files"},
	{"statistics", 2, fn_statistics_2, "+string,-variable"},
	{"duplicate_term", 2, fn_iso_copy_term_2, "+string,-variable"},
	{"call_nth", 2, fn_call_nth_2, "+callable,+integer"},
	{"limit", 2, fn_limit_2, "+integer,+callable"},
	{"offset", 2, fn_offset_2, "+integer,+callable"},
	{"plus", 3, fn_plus_3, "?integer,?integer,?integer"},
	{"succ", 2, fn_succ_2, "?integer,?integer"},

	{"$put_attrs", 2, fn_sys_put_attrs_2, "+variable,+list"},
	{"$get_attrs", 2, fn_sys_get_attrs_2, "+variable,-variable"},
	{"$del_attrs", 1, fn_sys_del_attrs_1, "+variable"},

#if USE_OPENSSL
	{"sha1", 2, fn_sha1_2, "+string,?string"},
	{"sha256", 2, fn_sha256_2, "+string,?string"},
	{"sha512", 2, fn_sha512_2, "+string,?string"},
#endif

	{"fork", 0, fn_fork_0, NULL},
	{"$task", 1, fn_task_n, "+callable"},
	{"$task", 2, fn_task_n, "+callable,+term,..."},
	{"$task", 3, fn_task_n, "+callable,+term,..."},
	{"$task", 4, fn_task_n, "+callable,+term,..."},
	{"$task", 5, fn_task_n, "+callable,+term,..."},
	{"$task", 6, fn_task_n, "+callable,+term,..."},
	{"$task", 7, fn_task_n, "+callable,+term,..."},
	{"$task", 8, fn_task_n, "+callable,+term,..."},
	{"wait", 0, fn_wait_0, NULL},
	{"await", 0, fn_await_0, NULL},
	{"yield", 0, fn_yield_0, NULL},
	{"send", 1, fn_send_1, "+term"},
	{"recv", 1, fn_recv_1, "?term"},

	{"$mustbe_instantiated", 1, fn_sys_instantiated_1, "+term"},
	{"$mustbe_instantiated", 2, fn_sys_instantiated_2, "+term,+term"},

	// Used for database log

	{"$a_", 2, fn_sys_asserta_2, "+term,+ref"},
	{"$z_", 2, fn_sys_assertz_2, "+term,+ref"},
	{"$e_", 1, fn_erase_1, "+ref"},

	{"$db_load", 0, fn_sys_db_load_0, NULL},
	{"$db_save", 0, fn_sys_db_save_0, NULL},

	{0}
};

void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found)
{
	sliter *iter = sl_findkey(pl->funtab, name);
	const struct builtins *ptr;

	while (sl_nextkey(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			sl_done(iter);
			*found = true;
			return ptr->fn;
		}
	}

	*found = false;
	return NULL;
}

extern const struct builtins g_functions[];
extern const struct builtins g_contrib_funcs[];

void load_builtins(prolog *pl)
{
	for (const struct builtins *ptr = g_predicates_iso; ptr->name; ptr++) {
		sl_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_functions; ptr->name; ptr++) {
		sl_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_predicates_other; ptr->name; ptr++) {
		sl_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_contrib_funcs; ptr->name; ptr++) {
		sl_app(pl->funtab, ptr->name, ptr);
	}
}

char *format_property(char **bufptr, size_t *lenptr, char *dst, const char *name, unsigned arity, const char *type)
{
	char *tmpbuf = *bufptr;
	size_t buflen = *lenptr;

	if ((buflen-(dst-tmpbuf)) < 1024) {
		size_t offset = dst - tmpbuf;
		*bufptr = tmpbuf = realloc(tmpbuf, *lenptr=(buflen*=2));
		dst = tmpbuf + offset;
	}

	int ch = peek_char_utf8(name);

	if (!isalpha_utf8(ch) && (name[0] != '_')) {
		char namebuf[512];
		const char *src = name;
		char *dst2 = namebuf;
		size_t len = sizeof(namebuf)-1;

		while (*src && len-- > 1) {
			if (*src == '\\') {
				*dst2++ = *src;
				len--;
			}

			*dst2++ = *src++;
		}

		*dst2 = '\0';
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'('%s'", namebuf);
	} else
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s", name);


	if (arity) {
		dst += snprintf(dst, buflen-(dst-tmpbuf), "(");

		for (unsigned i = 0; i < arity; i++) {
			if (i > 0)
				dst += snprintf(dst, buflen-(dst-tmpbuf), ",");

			dst += snprintf(dst, buflen-(dst-tmpbuf), "_");
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), ")");
	}

	dst += snprintf(dst, buflen-(dst-tmpbuf), ", %s).\n", type);
	return dst;
}

static char *push_property(char **bufptr, size_t *lenptr, char *dst, const struct builtins *ptr)
{
	dst = format_property(bufptr, lenptr, dst, ptr->name, ptr->arity, "built_in");
	dst = format_property(bufptr, lenptr, dst, ptr->name, ptr->arity, "static");
	dst = format_property(bufptr, lenptr, dst, ptr->name, ptr->arity, "private");
	dst = format_property(bufptr, lenptr, dst, ptr->name, ptr->arity, "native_code");
	return dst;
}

static void load_properties(module *m)
{
	if (m->loaded_properties)
		return;

	m->loaded_properties = true;
	size_t buflen = 1024*8;
	char *tmpbuf = malloc(buflen);
	char *dst = tmpbuf;
	*dst = '\0';

	dst = format_property(&tmpbuf, &buflen, dst, ",", 2, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, ",", 2, "meta_predicate((0,0))");

	dst = format_property(&tmpbuf, &buflen, dst, ";", 2, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, ";", 2, "meta_predicate((0;0))");

	dst = format_property(&tmpbuf, &buflen, dst, "->", 2, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "->", 2, "meta_predicate((0->0))");

	dst = format_property(&tmpbuf, &buflen, dst, "*->", 2, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "*->", 2, "meta_predicate((0*->0))");

	dst = format_property(&tmpbuf, &buflen, dst, "findall", 3, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "findall", 3, "meta_predicate(findall(?,0,-))");

	dst = format_property(&tmpbuf, &buflen, dst, "bagof", 3, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "bagof", 3, "meta_predicate(bagof(?,0,-))");

	dst = format_property(&tmpbuf, &buflen, dst, "setof", 3, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "setof", 3, "meta_predicate(setof(?,0,-))");

	dst = format_property(&tmpbuf, &buflen, dst, "throw", 1, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "call", 1, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "!", 0, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "true", 0, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "fail", 0, "control_construct");
	dst = format_property(&tmpbuf, &buflen, dst, "|", 2, "meta_predicate((:|+))");
	dst = format_property(&tmpbuf, &buflen, dst, "time", 1, "meta_predicate(time(0))");
	dst = format_property(&tmpbuf, &buflen, dst, "setup_call_cleanup", 3, "meta_predicate(setup_call_cleanup(0,0,0))");
	dst = format_property(&tmpbuf, &buflen, dst, "asserta", 1, "meta_predicate(asserta(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "assertz", 1, "meta_predicate(assertz(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "retract", 1, "meta_predicate(retract(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "retractall", 1, "meta_predicate(retractall(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "current_predicate", 1, "meta_predicate(current_predicate(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "predicate_property", 1, "meta_predicate(predicate_property(:,?))");
	dst = format_property(&tmpbuf, &buflen, dst, "abolish", 1, "meta_predicate(abolish(:))");
	dst = format_property(&tmpbuf, &buflen, dst, "clause", 2, "meta_predicate(abolish(:,?))");
	dst = format_property(&tmpbuf, &buflen, dst, "catch", 3, "meta_predicate(catch(0,?,0))");
	dst = format_property(&tmpbuf, &buflen, dst, "phrase", 2, "meta_predicate(phrase(2,?))");
	dst = format_property(&tmpbuf, &buflen, dst, "phrase", 3, "meta_predicate(phrase(2,?,?))");

	for (int i = 2; i <= 7; i++) {
		char metabuf[256];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(call(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		dst = format_property(&tmpbuf, &buflen, dst, "call", i, metabuf);
	}

	for (int i = 2; i <= 7; i++) {
		char metabuf[256];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(task(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		dst = format_property(&tmpbuf, &buflen, dst, "task", i, metabuf);
	}

	for (const struct builtins *ptr = g_predicates_iso; ptr->name; ptr++) {
		sl_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		dst = push_property(&tmpbuf, &buflen, dst, ptr);
	}

	for (const struct builtins *ptr = g_functions; ptr->name; ptr++) {
		sl_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		dst = push_property(&tmpbuf, &buflen, dst, ptr);
	}

	for (const struct builtins *ptr = g_predicates_other; ptr->name; ptr++) {
		sl_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		dst = push_property(&tmpbuf, &buflen, dst, ptr);
	}

	for (const struct builtins *ptr = g_contrib_funcs; ptr->name; ptr++) {
		sl_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		dst = push_property(&tmpbuf, &buflen, dst, ptr);
	}

	parser *p = create_parser(m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	parser_tokenize(p, false, false);
	destroy_parser(p);
	free(tmpbuf);
}

static void load_ops(query *q)
{
	if (q->st.m->loaded_ops)
		return;

	cell tmp;
	make_literal(&tmp, index_from_pool(q->st.m->pl, "$current_op"));
	tmp.arity = 3;

	if (do_abolish(q, &tmp, &tmp, false) != pl_success)
		return;

	q->st.m->loaded_ops = true;
	size_t buflen = 1024*8;
	char *tmpbuf = malloc(buflen);
	char *dst = tmpbuf;
	*dst = '\0';

	for (const struct op_table *ptr = q->st.m->ops; ptr->name; ptr++) {
		char specifier[256], name[256];

		if (!ptr->specifier)
			continue;

		if (ptr->specifier == OP_FX)
			strcpy(specifier, "fx");
		else if (ptr->specifier == OP_FY)
			strcpy(specifier, "fy");
		else if (ptr->specifier == OP_YF)
			strcpy(specifier, "yf");
		else if (ptr->specifier == OP_XF)
			strcpy(specifier, "xf");
		else if (ptr->specifier == OP_YFX)
			strcpy(specifier, "yfx");
		else if (ptr->specifier == OP_XFY)
			strcpy(specifier, "xfy");
		else if (ptr->specifier == OP_XFX)
			strcpy(specifier, "xfx");

		formatted(name, sizeof(name), ptr->name, strlen(ptr->name), false);

		unsigned len = snprintf(NULL, 0, "'$current_op'(%u, %s, '%s').\n",
			ptr->priority, specifier, name);

		while ((buflen-(dst-tmpbuf)) <= len) {
			size_t offset = dst - tmpbuf;
			tmpbuf = realloc(tmpbuf, buflen*=2);
			dst = tmpbuf + offset;
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$current_op'(%u, %s, '%s').\n",
			ptr->priority, specifier, name);
	}

	for (const struct op_table *ptr = q->st.m->def_ops; ptr->name; ptr++) {
		char specifier[256], name[256];

		if (!ptr->specifier)
			continue;

		if (ptr->specifier == OP_FX)
			strcpy(specifier, "fx");
		else if (ptr->specifier == OP_FY)
			strcpy(specifier, "fy");
		else if (ptr->specifier == OP_YF)
			strcpy(specifier, "yf");
		else if (ptr->specifier == OP_XF)
			strcpy(specifier, "xf");
		else if (ptr->specifier == OP_YFX)
			strcpy(specifier, "yfx");
		else if (ptr->specifier == OP_XFY)
			strcpy(specifier, "xfy");
		else if (ptr->specifier == OP_XFX)
			strcpy(specifier, "xfx");

		formatted(name, sizeof(name), ptr->name, strlen(ptr->name), false);

		unsigned len = snprintf(NULL, 0, "'$current_op'(%u, %s, '%s').\n",
			ptr->priority, specifier, name);

		while ((buflen-(dst-tmpbuf)) <= len) {
			size_t offset = dst - tmpbuf;
			tmpbuf = realloc(tmpbuf, buflen*=2);
			dst = tmpbuf + offset;
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$current_op'(%u, %s, '%s').\n",
			ptr->priority, specifier, name);
	}

	//printf("%s", tmpbuf);

	parser *p = create_parser(q->st.m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	parser_tokenize(p, false, false);
	destroy_parser(p);
	free(tmpbuf);
}
