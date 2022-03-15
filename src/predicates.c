#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>

#ifdef _WIN32
#define USE_MMAP 0
#else
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#if USE_MMAP
#include <sys/mman.h>
#endif
#endif

#include "internal.h"
#include "network.h"
#include "base64.h"
#include "library.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "heap.h"
#include "utf8.h"
#include "history.h"

#if USE_OPENSSL
#include "openssl/sha.h"
#endif

#ifdef _WIN32
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

#ifdef _WIN32
#include <io.h>
#define PATH_SEP "\\"
#define PATH_SEP_CHAR '\\'
#define NEWLINE_MODE "dos"
#else
#define PATH_SEP "/"
#define PATH_SEP_CHAR '/'
#define NEWLINE_MODE "posix"
#endif

#define PROMPT ""

size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len)
{
	char *save = dst;

	while ((dstlen-1) && len) {
		*dst++ = *src++;
		dstlen--;
		len--;
	}

	*dst = '\0';
	return dst - save;
}

bool check_list(query *q, cell *p1, pl_idx_t p1_ctx, bool *is_partial, pl_int_t *skip_)
{
	pl_int_t skip = 0, max = 1000000000;
	pl_idx_t c_ctx = p1_ctx;
	cell tmp = {0};

	cell *c = skip_max_list(q, p1, &c_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);

	if (skip_)
		*skip_ = skip;

	if (!strcmp(GET_STR(q,c), "[]"))
		return true;

	if (is_variable(c))
		*is_partial = true;
	else
		*is_partial = false;

	return false;
}

static cell err_cell = {0};
cell *ERR_CYCLE_CELL = &err_cell;

static pl_status do_yield_0(query *q, int msecs)
{
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs > 0 ? msecs : 1;
	may_error(push_choice(q));
	return pl_failure;
}

static void set_params(query *q, pl_idx_t p1, pl_idx_t p2)
{
	choice *ch = GET_CURR_CHOICE();
	ch->v1 = p1;
	ch->v2 = p2;
}

static void get_params(query *q, pl_idx_t *p1, pl_idx_t *p2)
{
	choice *ch = GET_CURR_CHOICE();
	if (p1) *p1 = ch->v1;
	if (p2) *p2 = ch->v2;
}

void make_variable(cell *tmp, pl_idx_t off, unsigned var_nbr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->nbr_cells = 1;
	tmp->val_off = off;
	tmp->var_nbr = var_nbr;
}

void make_variable2(cell *tmp, pl_idx_t off)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->nbr_cells = 1;
	tmp->val_off = off;
}

void make_int(cell *tmp, pl_int_t v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INT;
	tmp->nbr_cells = 1;
	set_smallint(tmp, v);
}

void make_real(cell *tmp, double v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_REAL;
	tmp->nbr_cells = 1;
	set_real(tmp, v);
}

void make_structure(cell *tmp, pl_idx_t offset, void *fn, unsigned arity, pl_idx_t extra_cells)
{
	*tmp = (cell){0};
	tmp->tag = TAG_LITERAL;
	tmp->nbr_cells = 1 + extra_cells;
	if (fn) tmp->flags |= FLAG_BUILTIN;
	tmp->fn = fn;
	tmp->arity = arity;
	tmp->val_off = offset;
}

void make_end(cell *tmp)
{
	*tmp = (cell){0};
	tmp->tag = TAG_END;
	tmp->nbr_cells = 1;
}

void make_return(query *q, cell *tmp)
{
	make_end(tmp);
	cell *c = q->st.curr_cell;
	frame *f = GET_CURR_FRAME();
	tmp->val_ret = c ? c + c->nbr_cells : NULL;	// save the return instruction
	tmp->cgen = f->cgen;						// ... choice-generation
	tmp->mod_id = q->st.m->id;					// ... current-module
}

void make_literal(cell *tmp, pl_idx_t offset)
{
	*tmp = (cell){0};
	tmp->tag = TAG_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = offset;
}

static void make_smalln(cell *tmp, const char *s, size_t n)
{
	*tmp = (cell){0};
	tmp->tag = TAG_CSTR;
	tmp->nbr_cells = 1;
	memcpy(tmp->val_chr, s, n);
	tmp->val_chr[n] = '\0';
	tmp->chr_len = n;
}

char *chars_list_to_string(query *q, cell *p_chars, pl_idx_t p_chars_ctx, size_t len)
{
	char *tmp = malloc(len+1+1);
	ensure(tmp);
	char *dst = tmp;
	LIST_HANDLER(p_chars);

	while (is_list(p_chars)) {
		cell *h = LIST_HEAD(p_chars);
		h = deref(q, h, p_chars_ctx);

		if (is_integer(h)) {
			int ch = get_int(h);
			dst += put_char_utf8(dst, ch);
		} else {
			const char *p = GET_STR(q, h);
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

#if 0
static void init_queue(query *q)
{
	free(q->queue[0]);
	q->queue[0] = NULL;
	q->qp[0] = 0;
}
#endif

static pl_idx_t queue_used(const query *q) { return q->qp[0]; }
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

static void init_queuen(query *q)
{
	free(q->queue[q->st.qnbr]);
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
}

static pl_idx_t queuen_used(const query *q) { return q->qp[q->st.qnbr]; }
static cell *get_queuen(query *q) { return q->queue[q->st.qnbr]; }

static USE_RESULT cell *end_list_unsafe(query *q)
{
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}

USE_RESULT pl_status make_cstringn(cell *d, const char *s, size_t n)
{
	if (!n) {
		make_literal(d, g_empty_s);
		return pl_success;
	}

	if (n < MAX_SMALL_STRING) {
		make_smalln(d, s, n);
		return pl_success;
	}

	*d = (cell){0};
	d->tag = TAG_CSTR;
	d->nbr_cells = 1;
	SET_STR(d, s, n, 0);
	return pl_success;
}

USE_RESULT pl_status make_stringn(cell *d, const char *s, size_t n)
{
#if 0
	if (n < (MAX_SMALL_STRING-4)) { // FIXME: why the -4
		make_smalln(d, s, n);
		d->flags = FLAG_CSTR_STRING;
		d->arity = 2;
		return pl_success;
	}
#endif

	*d = (cell){0};
	d->tag = TAG_CSTR;
	d->flags = FLAG_CSTR_STRING;
	d->nbr_cells = 1;
	d->arity = 2;
	SET_STR(d, s, n, 0);
	return pl_success;
}

static USE_RESULT pl_status make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n)
{
	if (!n) {
		make_literal(d, g_empty_s);
		return pl_success;
	}

	if (is_static(orig)) {
		*d = *orig;
		d->val_str += off;
		d->str_len = n;
		return pl_success;
	}

	if (is_strbuf(orig)) {
		*d = *orig;
		d->strb_off += off;
		d->strb_len = n;
		share_cell(orig);
		return pl_success;
	}

	const char *s = GET_STR(q, orig);

	if (is_string(orig))
		return make_stringn(d, s+off, n);

	return make_cstringn(d, s+off, n);
}

static USE_RESULT pl_status fn_iso_unify_with_occurs_check_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	bool was_acyclic = is_acyclic_term(q, p1, p1_ctx) || is_acyclic_term(q, p2, p2_ctx);

	if (unify(q, p1, p1_ctx, p2, p2_ctx)) {
		GET_FIRST_ARG(p1,any);
		GET_NEXT_ARG(p2,any);

		bool is_cyclic = is_cyclic_term(q, p1, p1_ctx) || is_cyclic_term(q, p2, p2_ctx);

		if (was_acyclic && is_cyclic)
			return pl_failure;

		return pl_success;
	}

	return pl_failure;
}

static USE_RESULT pl_status fn_iso_unify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return unify(q, p1, p1_ctx, p2, p2_ctx) ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_notunify_2(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,any);
	cell tmp2;
	make_structure(&tmp2, g_unify_s, fn_iso_unify_2, 2, 0);
	cell *tmp = clone_to_heap(q, true, &tmp2, p1->nbr_cells+p2->nbr_cells+3);
	pl_idx_t nbr_cells = 1;
	tmp[nbr_cells].nbr_cells += p1->nbr_cells+p2->nbr_cells;
	nbr_cells++;
	copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
	nbr_cells += p1->nbr_cells;
	copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	nbr_cells += p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static bool parse_read_params(query *q, stream *str, cell *c, pl_idx_t c_ctx, cell **vars, pl_idx_t *vars_ctx, cell **varnames, pl_idx_t *varnames_ctx, cell **sings, pl_idx_t *sings_ctx)
{
	parser *p = str->p;

	if (!is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	cell *c1 = deref(q, c+1, c_ctx);
	pl_idx_t c1_ctx = q->latest_ctx;

	if (!CMP_SLICE2(q, c, "character_escapes")) {
		if (is_literal(c1))
			p->flag.character_escapes = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "double_quotes")) {
		if (is_literal(c1)) {
			if (!CMP_SLICE2(q, c1, "atom")) {
				p->flag.double_quote_codes = p->flag.double_quote_chars = false;
				p->flag.double_quote_atom = true;
			} else if (!CMP_SLICE2(q, c1, "chars")) {
				p->flag.double_quote_atom = p->flag.double_quote_codes = false;
				p->flag.double_quote_chars = true;
			} else if (!CMP_SLICE2(q, c1, "codes")) {
				p->flag.double_quote_atom = p->flag.double_quote_chars = false;
				p->flag.double_quote_codes = true;
			}
		}
	} else if (!CMP_SLICE2(q, c, "variables")) {
		if (is_variable(c1)) {
			if (vars) *vars = c1;
			if (vars_ctx) *vars_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_SLICE2(q, c, "variable_names")) {
		if (is_variable(c1)) {
			if (varnames) *varnames = c1;
			if (varnames_ctx) *varnames_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_SLICE2(q, c, "singletons")) {
		if (is_variable(c1)) {
			if (sings) *sings = c1;
			if (sings_ctx) *sings_ctx = c1_ctx;
		} else {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_SLICE2(q, c, "positions") && (c->arity == 2) && str->fp) {
		p->pos_start = ftello(str->fp);
	} else if (!CMP_SLICE2(q, c, "line_counts") && (c->arity == 2)) {
	} else {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	return true;
}

static pl_status do_read_term(query *q, stream *str, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, char *src)
{
	if (!str->p) {
		str->p = create_parser(q->st.m);
		str->p->flag = q->st.m->flag;
		str->p->fp = str->fp;
		str->p->no_fp = q->p->no_fp;
	} else
		reset(str->p);

	parser *p = str->p;
	p->one_shot = true;
	cell *vars = NULL, *varnames = NULL, *sings = NULL;
	pl_idx_t vars_ctx = 0, varnames_ctx = 0, sings_ctx = 0;
	cell *p21 = p2;
	pl_idx_t p21_ctx = p2_ctx;

	LIST_HANDLER(p21);

	while (is_list(p21) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p21);
		h = deref(q, h, p21_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_variable(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!parse_read_params(q, str, h, h_ctx, &vars, &vars_ctx, &varnames, &varnames_ctx, &sings, &sings_ctx))
			return pl_success;

		p21 = LIST_TAIL(p21);
		p21 = deref(q, p21, p21_ctx);
		p21_ctx = q->latest_ctx;
	}

	if (is_variable(p21))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

	if (!is_nil(p21))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!src && !p->srcptr && str->fp) {
		if (p->no_fp || getline(&p->save_line, &p->n_line, str->fp) == -1) {
			if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
				clearerr(str->fp);
				return do_yield_0(q, 1);
			}

			p->srcptr = "";
		} else
			p->srcptr = p->save_line;
	}

	if (p->srcptr) {
		char *src = (char*)eat_space(p);
		p->line_nbr_start = p->line_nbr;
		p->srcptr = src;
	}

	for (;;) {
#if 0
		if (isatty(fileno(str->fp)) && !src) {
			fprintf(str->fp, "%s", PROMPT);
			fflush(str->fp);
		}
#endif

		if (!src && (!p->srcptr || !*p->srcptr || (*p->srcptr == '\n'))) {
			if (p->srcptr && (*p->srcptr == '\n'))
				p->line_nbr++;

			if (p->no_fp || getline(&p->save_line, &p->n_line, str->fp) == -1) {
				if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
					clearerr(str->fp);
					return do_yield_0(q, 1);
				}

				p->srcptr = "";
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

				cell *p22 = p2;
				pl_idx_t p22_ctx = p2_ctx;
				LIST_HANDLER(p22);

				while (is_list(p22) && !g_tpl_interrupt) {
					cell *h = LIST_HEAD(p22);
					h = deref(q, h, p22_ctx);
					pl_idx_t h_ctx = q->latest_ctx;

					if (is_variable(h))
						return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

					if (!CMP_SLICE2(q, h, "positions") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx_t p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->pos_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, ftello(str->fp));
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					} else if (!CMP_SLICE2(q, h, "line_counts") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx_t p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->line_nbr_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, str->p->line_nbr);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					}

					p22 = LIST_TAIL(p22);
					p22 = deref(q, p22, p22_ctx);
					p22_ctx = q->latest_ctx;
				}

				cell tmp;
				make_literal(&tmp, g_eof_s);
				return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			}

			//if (!*p->save_line || (*p->save_line == '\r') || (*p->save_line == '\n'))
			//	continue;

			p->srcptr = p->save_line;
		} else if (src)
			p->srcptr = src;

		break;
	}

	frame *f = GET_CURR_FRAME();
	p->read_term = f->nbr_vars;
	p->do_read_term = true;
	tokenize(p, false, false);
	p->read_term = 0;

	if (p->error) {
		p->error = false;

		if (!p->fp || !isatty(fileno(p->fp))) {
			void *save_fp = p->fp;
			p->fp = NULL;

			while (get_token(p, false, false)
				&& p->token[0] && strcmp(p->token, "."))
				;

			p->fp = save_fp;
			p->did_getline = false;
		}

		cell tmp;
		make_literal(&tmp, g_nil_s);
		p->do_read_term = false;
		return throw_error(q, &tmp, q->st.curr_frame, "syntax_error", p->error_desc?p->error_desc:"read_term");
	}

	p->do_read_term = false;

	cell *p22 = p2;
	pl_idx_t p22_ctx = p2_ctx;
	LIST_HANDLER(p22);

	while (is_list(p22) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p22);
		h = deref(q, h, p22_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_variable(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!CMP_SLICE2(q, h, "positions") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx_t p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->pos_start);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, ftello(str->fp));
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
		} else if (!CMP_SLICE2(q, h, "line_counts") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx_t p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->line_nbr_start);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, str->p->line_nbr);
			unify(q, p, p_ctx, &tmp, q->st.curr_frame);
		}

		p22 = LIST_TAIL(p22);
		p22 = deref(q, p22, p22_ctx);
		p22_ctx = q->latest_ctx;
	}

	if (!p->cl->cidx) {
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	xref_rule(p->m, p->cl, NULL);

	if (p->nbr_vars) {
		if (!create_vars(q, p->nbr_vars))
			return throw_error(q, p1, p1_ctx, "resource_error", "stack");
	}

	q->pl->tab_idx = 0;

	if (p->nbr_vars)
		collect_vars(q, p->cl->cells, q->st.curr_frame);

	if (vars) {
		unsigned cnt = q->pl->tab_idx;
		may_ptr_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
		may_ptr_error(tmp);
		unsigned idx = 0;

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->pl->tab_idx; i++) {
				make_literal(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*2)+1;
				cell v;
				make_variable(&v, q->pl->tab3[i], q->pl->tab2[i]);
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_ptr_error(tmp);
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

		for (unsigned i = 0; i < q->pl->tab_idx; i++) {
			if (q->pl->tab5[i])
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->pl->tab_idx; i++) {
				if (q->pl->tab5[i])
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
				make_literal(&v, q->pl->tab3[i]);
				tmp[idx++] = v;
				make_variable(&v, q->pl->tab3[i], q->pl->tab2[i]);
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_ptr_error(tmp);
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
		may_ptr_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->pl->tab_idx; i++) {
			if (q->pl->tab4[i] != 1)
				continue;

			if (varnames && (q->pl->tab5[i]))
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->pl->tab_idx; i++) {
				if (q->pl->tab4[i] != 1)
					continue;

				if (varnames && (q->pl->tab5[i]))
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
				make_literal(&v, q->pl->tab3[i]);
				tmp[idx++] = v;
				make_variable(&v, q->pl->tab3[i], q->pl->tab2[i]);
				tmp[idx++] = v;
				done++;
			}

			make_literal(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			may_ptr_error(tmp);
			safe_copy_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			set_var(q, sings, sings_ctx, tmp, q->st.curr_frame);
		} else {
			cell tmp;
			make_literal(&tmp, g_nil_s);
			set_var(q, sings, sings_ctx, &tmp, q->st.curr_frame);
		}
	}

	cell *tmp = alloc_on_heap(q, p->cl->cidx-1);
	may_ptr_error(tmp);
	safe_copy_cells(tmp, p->cl->cells, p->cl->cidx-1);
	pl_status ok = unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	clear_rule(p->cl);
	return ok;
}

static USE_RESULT pl_status fn_iso_repeat_0(query *q)
{
	may_error(push_choice(q));
	return pl_success;
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
	q->halt_code = get_int(p1);
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

bool has_vars(query *q, cell *c, pl_idx_t c_ctx, unsigned depth)
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

static USE_RESULT pl_status fn_iso_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_callable(p1);
}

static USE_RESULT pl_status fn_iso_char_code_2(query *q)
{
	GET_FIRST_ARG(p1,character_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (is_variable(p2)) {
		const char *src = GET_STR(q, p1);
		size_t len = len_char_utf8(src);

		if (len != LEN_STR(q, p1))
			return throw_error(q, p1, p1_ctx, "type_error", "character");

		int ch = peek_char_utf8(src);
		cell tmp;
		make_int(&tmp, ch);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "representation_error", "character_code");

	if (is_variable(p1)) {
		char tmpbuf[256];
		int n = put_char_utf8(tmpbuf, get_int(p2));
		cell tmp;
		make_smalln(&tmp, tmpbuf, n);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *src = GET_STR(q, p1);
	size_t len = len_char_utf8(src);

	if (len != LEN_STR(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	int ch = peek_char_utf8(src);
	return ch == get_int(p2);
}

static USE_RESULT pl_status fn_iso_atom_chars_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_iso_atom(p1) && !is_variable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (is_atom(p1) && !LEN_STR(q, p1) && is_nil(p2))
		return pl_success;

	if (is_variable(p1) && is_nil(p2)) {
		cell tmp;
		make_literal(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p2) && !LEN_STR(q, p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p2) && (is_literal(p1) || (LEN_STR(q, p1) < MAX_SMALL_STRING))) {
		cell tmp;
		may_error(make_stringn(&tmp, GET_STR(q, p1), LEN_STR(q, p1)));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	if (is_variable(p2)) {
		cell tmp;
		may_error(make_stringn(&tmp, GET_STR(q, p1), LEN_STR(q, p1)));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	if (is_string(p2)) {
		cell tmp;
		may_error(make_slice(q, &tmp, p2, 0, LEN_STR(q, p2)));
		tmp.flags &= ~FLAG_CSTR_STRING;
		tmp.arity = 0;
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (!is_atom(head) && !is_variable(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (is_atom(head)) {
				const char *src = GET_STR(q, head);
				size_t len = len_char_utf8(src);

				if (len < LEN_STR(q, head))
					return throw_error(q, head, q->latest_ctx, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_string(p2) && is_variable(p1)) {
		cell tmp = *p2;
		tmp.flags &= ~FLAG_CSTR_STRING;
		tmp.arity = 0;
		set_var(q, p1, p1_ctx, p2, q->st.curr_frame);
		return pl_success;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		ASTRING(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			const char *src = GET_STR(q, head);
			ASTRING_strcatn(pr, src, len_char_utf8(src));

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		may_error(make_cstring(&tmp, ASTRING_cstr(pr)), ASTRING_free(pr));
		ASTRING_free(pr);
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *src = GET_STR(q, p1);
	size_t len = LEN_STR(q, p1);
	bool first = true;

	while (len) {
		size_t n = len_char_utf8(src);
		cell tmp2;
		make_smalln(&tmp2, src, n);
		src += n;
		len -= n;

		if (first) {
			allocate_list(q, &tmp2);
			first = false;
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
	cell *orig_p2 = p2;

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2))
		return throw_error(q, p2, p2_ctx, "syntax_error", "incomplete");

	// Verify the list

	pl_int_t cnt = 0;
	bool any_vars = false;

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2) && !g_tpl_interrupt) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (is_variable(head))
				any_vars = true;

			if (!is_atom(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (!is_atom(head) && !is_variable(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (is_atom(head)) {
				const char *src = GET_STR(q, head);
				size_t len = len_char_utf8(src);

				if (len < LEN_STR(q, head))
					return throw_error(q, head, q->latest_ctx, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
			cnt++;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");

		if (is_variable(p2))
			any_vars = true;

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_variable(p1) && any_vars)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_variable(p2) && !any_vars) {
		char *tmpbuf = malloc(cnt+1+1);
		may_ptr_error(tmpbuf);
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head)) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "type_error", "atom");
			}

			const char *src = GET_STR(q, head);
			int ch = *src;

			if (!ch)
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			*dst++ = ch;
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		*dst = '\0';

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = create_parser(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flag = q->st.m->flag;
		p->srcptr = tmpbuf;
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			free(tmpbuf);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error&&p->error_desc?p->error_desc:"number");
		}

		free(tmpbuf);
		cell tmp = p->v;
		pl_status ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	ssize_t len = print_canonical_to_buf(q, NULL, 0, p1, p1_ctx, 1, 0, 0);
	char *dst = malloc(len+10);
	may_ptr_error(dst);
	print_canonical_to_buf(q, dst, len+1, p1, p1_ctx, 1, 0, 0);
	cell tmp;
	may_error(make_string(&tmp, dst));
	free(dst);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_iso_atom_codes_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_iso_atom(p1) && !is_variable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!is_variable(p2) && is_nil(p2)) {
		cell tmp;
		make_literal(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_variable(p2) && !LEN_STR(q, p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_variable(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		ASTRING(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int_t val = get_int(head);

			if (val < 0)
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");

			char ch[10];
			int len;

			if (!val) {
				ch[0] = 0;
				len = 1;
			} else
				len = put_char_utf8(ch, val);

			ASTRING_strcatn(pr, ch, len);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		may_error(make_cstringn(&tmp, ASTRING_cstr(pr), ASTRING_strlen(pr)), ASTRING_free(pr));
		ASTRING_free(pr);
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *tmpbuf = GET_STR(q, p1);
	size_t len = LEN_STR(q, p1);
	const char *src = tmpbuf;
	cell tmp;
	len -= len_char_utf8(src);
	make_int(&tmp, get_char_utf8(&src));
	allocate_list(q, &tmp);

	while (len) {
		len -= len_char_utf8(src);
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_hex_bytes_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	// Verify the list

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_variable(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_variable(p2) && is_variable(p1)) {
		ASTRING(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int_t val = get_int(head);

			if ((val < 0) || (val > 255))
				return throw_error(q, head, q->latest_ctx, "representation_error", "byte");

			char ch[10];
			snprintf(ch, sizeof(ch), "%02X", (unsigned)val);
			ASTRING_strcat(pr, ch);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		may_error(make_string(&tmp, ASTRING_cstr(pr)), ASTRING_free(pr));
		ASTRING_free(pr);
		pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	LIST_HANDLER(p1);
	bool first = true;

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_atom(h))
			return throw_error(q, p1, p1_ctx, "type_error", "char");

		const char *src = GET_STR(q, h);
		int n = peek_char_utf8(src);;
		unsigned val = 0;

		if (isdigit(n))
			val += n - '0';
		else if ((n >= 'a') && (n <= 'f'))
			val += (n - 'a') + 10;
		else if ((n >= 'A') && (n <= 'F'))
			val += (n - 'A') + 10;
		else
			return throw_error(q, p1, p1_ctx, "representation_error", "byte");

		val <<= 4;

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (!is_list(p1))
			return throw_error(q, p1, p1_ctx, "domain_error", "hex_encoding");

		h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_atom(h))
			return throw_error(q, p1, p1_ctx, "type_error", "char");

		src = GET_STR(q, h);
		n = peek_char_utf8(src);;

		if (isdigit(n))
			val += n - '0';
		else if ((n >= 'a') && (n <= 'f'))
			val += (n - 'a') + 10;
		else if ((n >= 'A') && (n <= 'F'))
			val += (n - 'A') + 10;
		else
			return throw_error(q, p1, p1_ctx, "representation_error", "byte");

		cell tmp;
		make_int(&tmp, (int)val);

		if (first) {
			allocate_list(q, &tmp);
			first = false;
		} else
			append_list(q, &tmp);

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "hex_encoding");

	if (first) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_number_codes_2(query *q)
{
	GET_FIRST_ARG(p1,number_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);
	cell *orig_p2 = p2;

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2))
		return throw_error(q, p2, p2_ctx, "syntax_error", "incomplete");

	// Verify the list

	int cnt = 0;
	bool any_vars = false;

	if (!is_variable(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (is_variable(head))
				any_vars = true;

			if (!cnt && !is_integer(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "syntax_error", "integer");

			if (!is_integer(head) && is_variable(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_variable(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
			cnt++;
		}

		if (!is_nil(p2) && !is_variable(p2))
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");

		if (is_variable(p2))
			any_vars = true;

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_variable(p1) && any_vars)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_variable(p2) && !any_vars) {
		char *tmpbuf = malloc((cnt*6)+1+1);
		may_ptr_error(tmpbuf);
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head)) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");
			}

			int val = get_int(head);

			if (val < 0) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");
			}

			dst += put_char_utf8(dst, val);

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		*dst = '\0';

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = create_parser(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flag = q->st.m->flag;
		p->srcptr = tmpbuf;
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			free(tmpbuf);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error?p->error_desc:"number");
		}

		free(tmpbuf);
		cell tmp = p->v;
		pl_status ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	ssize_t len = print_canonical_to_buf(q, NULL, 0, p1, p1_ctx, 1, 0, 0);
	char *dst = malloc(len+10);
	may_ptr_error(dst);
	print_canonical_to_buf(q, dst, len+1, p1, p1_ctx, 1, 0, 0);
	const char *src = dst;
	cell tmp;
	make_int(&tmp, *src);
	allocate_list(q, &tmp);

	while (*++src) {
		make_int(&tmp, *src);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	may_ptr_error(l);
	free(dst);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_sub_atom_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);		// before
	GET_NEXT_ARG(p3,integer_or_var);		// len
	GET_NEXT_ARG(p4,integer_or_var);		// after
	GET_NEXT_ARG(p5,atom_or_var);
	const size_t len_p1 = LEN_STR_UTF8(p1);
	size_t before = 0, len = 0, after = 0;

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p3) && is_negative(p3))
		return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p4) && is_negative(p4))
		return throw_error(q, p4, p4_ctx, "domain_error", "not_less_than_zero");

	bool fixed = ((is_integer(p2) ? 1: 0) + (is_integer(p3) ? 1 : 0) + (is_integer(p4) ? 1 : 0)) >= 2;

	if ((!is_variable(p2) || !is_variable(p4)) && !is_variable(p5))
		fixed = true;

	if (!q->retry) {
		may_error(push_choice(q));

		if (!is_variable(p2))
			before = get_int(p2);

		if (!is_variable(p3))
			len = get_int(p3);

		if (!is_variable(p4))
			after = get_int(p4);

		if (is_variable(p2) && is_integer(p3) && is_integer(p4))
			before = len_p1 - after - len;

		if (is_variable(p3) && is_integer(p2) && is_integer(p4))
			len = len_p1 - before - after;
	} else {
		pl_idx_t v1, v2;
		get_params(q, &v1, &v2);
		before = v1;
		len = v2;
	}

	if (len > (LEN_STR_UTF8(p1)-before)) {
		before++;
		len = 0;
	}

	if (before > LEN_STR_UTF8(p1)) {
		drop_choice(q);
		return pl_failure;
	}

	for (size_t i = before; i <= len_p1; i++) {
		for (size_t j = len; j <= (len_p1-i); j++) {
			if (g_tpl_interrupt)
				break;

			set_params(q, i, j+1);
			may_error(push_choice(q));
			cell tmp;
			make_int(&tmp, i);

			if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame)) {
				undo_me(q);
				drop_choice(q);
				continue;
			}

			make_int(&tmp, j);

			if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
				undo_me(q);
				drop_choice(q);
				continue;
			}

			make_int(&tmp, len_p1-i-j);

			if (!unify(q, p4, p4_ctx, &tmp, q->st.curr_frame)) {
				undo_me(q);
				drop_choice(q);
				continue;
			}

			size_t ipos = offset_at_pos(GET_STR(q, p1), LEN_STR(q, p1), i);
			size_t jpos = offset_at_pos(GET_STR(q, p1), LEN_STR(q, p1), i+j);

			may_error(make_slice(q, &tmp, p1, ipos, jpos-ipos));

			if (is_atom(p5) && !CMP_SLICE(q, p5, GET_STR(q, &tmp), LEN_STR(q, &tmp))) {
				unshare_cell(&tmp);

				if (fixed) {
					drop_choice(q);
					drop_choice(q);
				}

				return pl_success;
			}

			if (!unify(q, p5, p5_ctx, &tmp, q->st.curr_frame)) {
				unshare_cell(&tmp);
				undo_me(q);
				drop_choice(q);
				continue;
			}

			unshare_cell(&tmp);

			if (fixed) {
				drop_choice(q);
				drop_choice(q);
			}

			return pl_success;
		}

		len = 0;
	}

	drop_choice(q);
	return pl_failure;
}

// NOTE: this just handles the mode(-,-,+) case...

static pl_status do_atom_concat_3(query *q)
{
	if (!q->retry) {
		GET_FIRST_ARG(p1,variable);
		GET_NEXT_ARG(p2,variable);
		GET_NEXT_ARG(p3,atom);
		cell tmp;
		make_literal(&tmp, g_empty_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		set_var(q, p2, p2_ctx, p3, q->st.curr_frame);

		if (LEN_STR(q, p3))
			may_error(push_choice(q));

		return pl_success;
	}

	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	const char *s2 = GET_STR(q, p2);
	size_t len = len_char_utf8(s2);
	size_t len1 = LEN_STR(q, p1);
	size_t len2 = LEN_STR(q, p2);
	bool done = false;

	if (!*(s2+len))
		done = true;

	GET_RAW_ARG(1,p1_raw);
	GET_RAW_ARG(2,p2_raw);
	unshare_cell(p1);
	unshare_cell(p2);
	cell tmp;
	may_error(make_slice(q, &tmp, p3, 0, len1+len));
	reset_var(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame, true);
	unshare_cell(&tmp);
	may_error(make_slice(q, &tmp, p2, len, len2-len));
	reset_var(q, p2_raw, p2_raw_ctx, &tmp, q->st.curr_frame, true);
	unshare_cell(&tmp);

	if (!done)
		may_error(push_choice(q));

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atom_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p3,atom_or_var);

	if (is_variable(p1) && is_variable(p2))
		return do_atom_concat_3(q);

	if (is_variable(p3)) {
		if (!is_iso_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		if (!is_iso_atom(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "atom");

		ASTRING(pr);
		ASTRING_strcatn(pr, GET_STR(q, p1), LEN_STR(q, p1));
		ASTRING_strcatn(pr, GET_STR(q, p2), LEN_STR(q, p2));
		cell tmp;
		may_error(make_cstringn(&tmp, ASTRING_cstr(pr), ASTRING_strlen(pr)), ASTRING_free(pr));
		ASTRING_free(pr);
		set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	if (is_variable(p1)) {
		size_t len2 = LEN_STR(q, p2), len3 = LEN_STR(q, p3);
		const char *s2 = GET_STR(q, p2), *s3 = GET_STR(q, p3);

		if (len2 > len3)
			return false;

		if (memcmp(s3+(len3-len2), s2, len2))
			return pl_failure;

		cell tmp;
		may_error(make_slice(q, &tmp, p3, 0, len3-len2));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	if (is_variable(p2)) {
		size_t len1 = LEN_STR(q, p1), len3 = LEN_STR(q, p3);
		const char *s1 = GET_STR(q, p1), *s3 = GET_STR(q, p3);

		if (len1 > len3)
			return false;

		if (memcmp(s3, s1, len1))
			return pl_failure;

		cell tmp;
		may_error(make_slice(q, &tmp, p3, len1, len3-len1));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	size_t len1 = LEN_STR(q, p1), len2 = LEN_STR(q, p2), len3 = LEN_STR(q, p3);
	const char *s1 = GET_STR(q, p1), *s2 = GET_STR(q, p2), *s3 = GET_STR(q, p3);

	if ((len1 + len2) != len3)
		return pl_failure;

	if (memcmp(s3, s1, len1))
		return pl_failure;

	if (memcmp(s3+len1, s2, len2))
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atom_length_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,smallint_or_var);

	if (!is_iso_atom(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	size_t len = substrlen_utf8(GET_STR(q, p1), LEN_STR(q, p1));
	cell tmp;
	make_int(&tmp, len);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static int new_stream(prolog *pl)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		if (!pl->streams[i].fp && !pl->streams[i].ignore) {
			memset(&pl->streams[i], 0, sizeof(stream));
			return i;
		}
	}

	return -1;
}

static int get_named_stream(prolog *pl, const char *name, size_t len)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &pl->streams[i];

		if (!str->fp)
			continue;

		if (str->name && (strlen(str->name) == len)
			&& !strncmp(str->name, name, len))
			return i;

		if (str->filename && (strlen(str->filename) == len)
			&& !strncmp(str->filename, name, len))
			return i;
	}

	return -1;
}

int get_stream(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_stream(q->pl, GET_STR(q, p1), LEN_STR(q, p1));

		if (n < 0)
			return -1;

		return n;
	}

	if (!(p1->flags&FLAG_INT_STREAM))
		return -1;

	if (!q->pl->streams[get_int(p1)].fp)
		return -1;

	return get_smallint(p1);
}

static bool is_closed_stream(prolog *pl, cell *p1)
{
	if (!(p1->flags&FLAG_INT_STREAM))
		return false;

	if (pl->streams[get_smallint(p1)].fp)
		return false;

	return true;
}

static USE_RESULT pl_status fn_iso_current_input_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_input);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->pl->current_input ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_current_output_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_output);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		set_var(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == q->pl->current_output ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_set_input_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	q->pl->current_input = n;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_set_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	q->pl->current_output = n;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_set_stream_position_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!str->repo)
		return throw_error(q, p1, p1_ctx, "permission_error", "reposition,stream");

	if (!is_smallint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_position");

	off_t pos = get_smallint(p1);

	if (fseeko(str->fp, pos, SEEK_SET))
		return throw_error(q, p1, p1_ctx, "domain_error", "position");

	return pl_success;
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

static void db_log(query *q, db_entry *dbe, enum log_type l)
{
	char tmpbuf[256];
	char *dst;
	q->quoted = 2;

	switch(l) {
	case LOG_ASSERTA:
		dst = print_term_to_strbuf(q, dbe->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$a_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ASSERTZ:
		dst = print_term_to_strbuf(q, dbe->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$z_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ERASE:
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$e_'('%s').\n", tmpbuf);
		break;
	}

	q->quoted = 0;
}

static pl_status do_retract(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract)
{
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_variable(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_callable(head))
		return throw_error(q, head, q->latest_ctx, "type_error", "callable");

	pl_status match;

	if (check_if_rule(p1))
		match = match_rule(q, p1, p1_ctx);
	else
		match = match_clause(q, p1, p1_ctx, is_retract);

	if ((match != pl_success) || q->did_throw)
		return match;

	db_entry *dbe = q->st.curr_clause2;
	bool last_match = !dbe->next && (is_retract == DO_RETRACT);
	stash_me(q, &dbe->cl, last_match);

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ERASE);

	add_to_dirty_list(q->st.m, dbe);
	return pl_success;
}

static void add_stream_properties(query *q, int n)
{
	stream *str = &q->pl->streams[n];
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

	char tmpbuf2[1024];
	formatted(tmpbuf2, sizeof(tmpbuf2), str->name, strlen(str->name), false);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, alias('%s')).\n", n, tmpbuf2);
	formatted(tmpbuf2, sizeof(tmpbuf2), str->filename, strlen(str->filename), false);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file_name('%s')).\n", n, tmpbuf2);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, mode(%s)).\n", n, str->mode);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, type(%s)).\n", n, str->binary ? "binary" : "text");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, line_count(%d)).\n", n, str->p ? str->p->line_nbr : 1);
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, position(%llu)).\n", n, (unsigned long long)(pos != -1 ? pos : 0));
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, reposition(%s)).\n", n, (n < 3) || str->socket ? "false" : "true");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, end_of_stream(%s)).\n", n, str->at_end_of_file ? "past" : at_end_of_file ? "at" : "not");
	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, eof_action(%s)).\n", n, str->eof_action == eof_action_eof_code ? "eof_code" : str->eof_action == eof_action_error ? "error" : str->eof_action == eof_action_reset ? "reset" : "none");

	if (!str->binary) {
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, bom(%s)).\n", n, str->bom ? "true" : "false");
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, encoding('%s')).\n", n, "UTF-8");
	}

	if (!strcmp(str->mode, "read"))
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, input).\n", n);
	else
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, output).\n", n);

	dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, newline(%s)).\n", n, NEWLINE_MODE);

	parser *p = create_parser(q->st.m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
}

#ifndef SANDBOX
static void del_stream_properties(query *q, int n)
{
	cell *tmp = alloc_on_heap(q, 3);
	make_literal(tmp+0, g_sys_stream_property_s);
	make_int(tmp+1, n);
	make_variable(tmp+2, g_anon_s, create_vars(q, 1));
	tmp->nbr_cells = 3;
	tmp->arity = 2;
	q->retry = QUERY_OK;

#if 0
	predicate *pr = find_predicate(q->st.m, tmp);

	if (!pr) {
		DISCARD_RESULT throw_error(q, tmp, "existence_error", "procedure");
		return;
	}
#endif

	while (do_retract(q, tmp, q->st.curr_frame, DO_STREAM_RETRACT)) {
		if (q->did_throw)
			return;

		q->retry = QUERY_RETRY;
		retry_choice(q);
	}

	q->retry = QUERY_OK;
}
#endif

static pl_status do_stream_property(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	cell *c = p1 + 1;
	c = deref(q, c, p1_ctx);
	pl_idx_t c_ctx = q->latest_ctx;

	if (!CMP_SLICE2(q, p1, "file_name")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->filename));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "alias")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->name));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "mode")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->mode));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "bom") && !str->binary) {
		cell tmp;
		may_error(make_cstring(&tmp, str->bom?"true":"false"));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "type")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->binary ? "binary" : "text"));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "reposition")) {
		cell tmp;
		may_error(make_cstring(&tmp, str->socket || (n <= 2) ? "false" : "true"));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "encoding") && !str->binary) {
		cell tmp;
		may_error(make_cstring(&tmp, "UTF-8"));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "newline")) {
		cell tmp;
		may_error(make_cstring(&tmp, NEWLINE_MODE));
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_SLICE2(q, p1, "input"))
		return !strcmp(str->mode, "read");

	if (!CMP_SLICE2(q, p1, "output"))
		return strcmp(str->mode, "read");

	if (!CMP_SLICE2(q, p1, "eof_action") && is_stream(pstr)) {
		cell tmp;

		if (str->eof_action == eof_action_eof_code)
			make_literal(&tmp, index_from_pool(q->pl, "eof_code"));
		else if (str->eof_action == eof_action_error)
			make_literal(&tmp, index_from_pool(q->pl, "error"));
		else if (str->eof_action == eof_action_reset)
			make_literal(&tmp, index_from_pool(q->pl, "reset"));
		else
			make_literal(&tmp, index_from_pool(q->pl, "none"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_SLICE2(q, p1, "end_of_stream") && is_stream(pstr)) {
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
			make_literal(&tmp, index_from_pool(q->pl, "past"));
		else if (at_end_of_file)
			make_literal(&tmp, index_from_pool(q->pl, "at"));
		else
			make_literal(&tmp, index_from_pool(q->pl, "not"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_SLICE2(q, p1, "position") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, ftello(str->fp));
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_SLICE2(q, p1, "line_count") && !is_variable(pstr)) {
		cell tmp;
		make_int(&tmp, str->p->line_nbr);
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	return pl_failure;
}

static void clear_streams_properties(query *q)
{
	cell tmp;
	make_literal(&tmp, g_sys_stream_property_s);
	tmp.nbr_cells = 1;
	tmp.arity = 2;

	predicate *pr = find_predicate(q->st.m, &tmp);

	if (pr) {
		for (db_entry *dbe = pr->head; dbe;) {
			db_entry *save = dbe;
			dbe = dbe->next;
			add_to_dirty_list(q->st.m, save);
		}

		pr->head = pr->tail = NULL;
		pr->cnt = 0;
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
		if (is_closed_stream(q->pl, pstr))
			return throw_error(q, pstr, q->st.curr_frame, "existence_error", "stream");
		else
			return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");
	}

	if (p1->arity > 1)
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_variable(p1) && !is_callable(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_variable(pstr) && !is_variable(p1))
		return do_stream_property(q);

	if (!q->retry) {
		clear_streams_properties(q);

		for (int i = 0; i < MAX_STREAMS; i++) {
			if (!q->pl->streams[i].fp)
				continue;

			stream *str = &q->pl->streams[i];

			if (!str->socket)
				add_stream_properties(q, i);
		}
	}

	cell *tmp = deep_copy_to_tmp(q, q->st.curr_cell, q->st.curr_frame, false, false);
	unify(q, tmp, q->st.curr_frame, q->st.curr_cell, q->st.curr_frame);
	tmp->val_off = g_sys_stream_property_s;

	if (match_clause(q, tmp, q->st.curr_frame, DO_CLAUSE) != pl_success) {
		clear_streams_properties(q);

		if (is_callable(p1) && !strstr(s_properties, GET_STR(q, p1)))
			return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

		return pl_failure;
	}

	clause *r = &q->st.curr_clause2->cl;
	GET_FIRST_ARG(pstrx,smallint);
	pstrx->flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	stash_me(q, r, false);
	return pl_success;
}

#ifndef SANDBOX
#ifndef _WIN32
static USE_RESULT pl_status fn_popen_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	const char *filename;

	if (is_atom(p1))
		filename = src = DUP_SLICE(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	}

	stream *str = &q->pl->streams[n];
	str->domain = true;
	may_ptr_error(str->filename = strdup(filename));
	may_ptr_error(str->name = strdup(filename));
	may_ptr_error(str->mode = DUP_SLICE(q, p2));
	str->eof_action = eof_action_eof_code;
	bool binary = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_structure(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);


			if (get_named_stream(q->pl, GET_STR(q, name), LEN_STR(q, name)) >= 0)
				return throw_error(q, c, q->latest_ctx, "permission_error", "open,source_sink");

			if (!CMP_SLICE2(q, c, "alias")) {
				free(str->name);
				str->name = DUP_SLICE(q, name);
			} else if (!CMP_SLICE2(q, c, "type")) {
				if (is_atom(name) && !CMP_SLICE2(q, name, "binary")) {
					str->binary = true;
					binary = true;
				} else if (is_atom(name) && !CMP_SLICE2(q, name, "text"))
					binary = false;
			} else if (!CMP_SLICE2(q, c, "eof_action")) {
				if (is_atom(name) && !CMP_SLICE2(q, name, "error")) {
					str->eof_action = eof_action_error;
				} else if (is_atom(name) && !CMP_SLICE2(q, name, "eof_code")) {
					str->eof_action = eof_action_eof_code;
				} else if (is_atom(name) && !CMP_SLICE2(q, name, "reset")) {
					str->eof_action = eof_action_reset;
				}
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_variable(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (!strcmp(str->mode, "read"))
		str->fp = popen(filename, binary?"rb":"r");
	else if (!strcmp(str->mode, "write"))
		str->fp = popen(filename, binary?"wb":"w");
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");

	free(src);

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}
#endif
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_iso_open_4(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	const char *filename;
	stream *oldstr = NULL;

	if (is_structure(p1) && (p1->arity == 1) && !CMP_SLICE2(q, p1, "stream")) {
		int oldn = get_stream(q, p1+1);

		if (oldn < 0)
			return throw_error(q, p1, p1_ctx, "type_error", "not_a_stream");

		stream *oldstr = &q->pl->streams[oldn];
		filename = oldstr->filename;
	} else if (is_atom(p1))
		filename = src = DUP_SLICE(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = src = chars_list_to_string(q, p1, p1_ctx, len);
	}

	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = strdup(filename));
	may_ptr_error(str->name = strdup(filename));
	may_ptr_error(str->mode = DUP_SLICE(q, p2));
	str->eof_action = eof_action_eof_code;
	free(src);

#if USE_MMAP
	cell *mmap_var = NULL;
	pl_idx_t mmap_ctx = 0;
#endif

	bool bom_specified = false, use_bom = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_SLICE2(q, c, "mmap")) {
#if USE_MMAP
			mmap_var = name;
			mmap_var = deref(q, mmap_var, q->latest_ctx);
			mmap_ctx = q->latest_ctx;
#endif
		} else if (!CMP_SLICE2(q, c, "encoding")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_SLICE2(q, c, "alias")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, GET_STR(q, name), LEN_STR(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			free(str->name);
			str->name = DUP_SLICE(q, name);
		} else if (!CMP_SLICE2(q, c, "type")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_SLICE2(q, name, "binary")) {
				str->binary = true;
			} else if (is_atom(name) && !CMP_SLICE2(q, name, "text"))
				str->binary = false;
			else
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_SLICE2(q, c, "bom")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			bom_specified = true;

			if (is_atom(name) && !CMP_SLICE2(q, name, "true"))
				use_bom = true;
			else if (is_atom(name) && !CMP_SLICE2(q, name, "false"))
				use_bom = false;
		} else if (!CMP_SLICE2(q, c, "reposition")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_SLICE2(q, name, "true"))
				str->repo = true;
			else if (is_atom(name) && !CMP_SLICE2(q, name, "false"))
				str->repo = false;
		} else if (!CMP_SLICE2(q, c, "eof_action")) {
			if (is_variable(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_SLICE2(q, name, "error")) {
				str->eof_action = eof_action_error;
			} else if (is_atom(name) && !CMP_SLICE2(q, name, "eof_code")) {
				str->eof_action = eof_action_eof_code;
			} else if (is_atom(name) && !CMP_SLICE2(q, name, "reset")) {
				str->eof_action = eof_action_reset;
			}
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_variable(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (oldstr) {
		int fd = fileno(oldstr->fp);

		if (!strcmp(str->mode, "read"))
			str->fp = fdopen(fd, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fdopen(fd, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fdopen(fd, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fdopen(fd, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	} else {
		if (!strcmp(str->mode, "read"))
			str->fp = fopen(str->filename, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fopen(str->filename, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fopen(str->filename, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fopen(str->filename, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	}

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		//else if ((strcmp(str->mode, "read") && (errno == EISDIR)))
		//	return throw_error(q, p1, p1_ctx, "permission_error", "open,isadir");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	size_t offset = 0;

	if (!strcmp(str->mode, "read") && !str->binary && (!bom_specified || use_bom)) {
		int ch = xgetc_utf8(net_getc, str);

		if (feof(str->fp))
			clearerr(str->fp);

		if ((unsigned)ch == 0xFEFF) {
			str->bom = true;
			offset = 3;
		} else
			fseek(str->fp, 0, SEEK_SET);
	} else if (!strcmp(str->mode, "write") && !str->binary && use_bom) {
		int ch = 0xFEFF;
		char tmpbuf[10];
		put_char_utf8(tmpbuf, ch);
		net_write(tmpbuf, strlen(tmpbuf), str);
		str->bom = true;
	}

#if USE_MMAP
	int prot = 0;

	if (!strcmp(str->mode, "read"))
		prot = PROT_READ;
	else
		prot = PROT_WRITE;

	if (mmap_var && is_variable(mmap_var)) {
		int fd = fileno(str->fp);
		struct stat st = {0};
		fstat(fd, &st);
		size_t len = st.st_size;
		void *addr = mmap(0, len, prot, MAP_PRIVATE, fd, offset);
		cell tmp = {0};
		tmp.tag = TAG_CSTR;
		tmp.flags = FLAG_CSTR_BLOB | FLAG_CSTR_STRING | FLAG_STATIC;
		tmp.nbr_cells = 1;
		tmp.arity = 2;
		tmp.val_str = addr;
		tmp.str_len = len;
		set_var(q, mmap_var, mmap_ctx, &tmp, q->st.curr_frame);
	}
#endif

	cell tmp ;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_iso_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if ((str->fp == stdin)
		|| (str->fp == stdout)
		|| (str->fp == stderr))
		return pl_success;

	if (q->pl->current_input == n)
		q->pl->current_input = 0;

	if (q->pl->current_output == n)
		q->pl->current_output = 1;

	if (q->pl->current_error == n)
		q->pl->current_error = 2;

	if (str->p)
		destroy_parser(str->p);

	if (!str->socket)
		del_stream_properties(q, n);

	net_close(str);
	free(str->mode);
	free(str->filename);
	free(str->name);
	free(str->data);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_iso_close_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,list_or_nil);
	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_structure(h)
			|| CMP_SLICE2(q, h, "force")
			|| CMP_SLICE2(q, h+1, "true"))
			return throw_error(q, h, q->latest_ctx, "domain_error", "close_option");

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (is_variable(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "close_option");

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	return fn_iso_close_1(q);
}
#endif

static USE_RESULT pl_status fn_iso_at_end_of_stream_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->ungetch && str->p) {
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
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->ungetch && str->p) {
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
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_flush_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_nl_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	fputc('\n', str->fp);
	//fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_nl_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	fputc('\n', str->fp);
	//fflush(str->fp);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_read_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static USE_RESULT pl_status fn_iso_read_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return do_read_term(q, str, p1, p1_ctx, &tmp, q->st.curr_frame, NULL);
}

static USE_RESULT pl_status fn_iso_read_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT pl_status fn_iso_read_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static USE_RESULT pl_status fn_iso_write_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_term_to_stream(q, str, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_writeq_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_writeq_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_canonical_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_canonical_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);
	return !ferror(str->fp);
}

static bool parse_write_params(query *q, cell *c, pl_idx_t c_ctx, cell **vnames, pl_idx_t *vnames_ctx)
{
	if (is_variable(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "instantiation_error", "write_option");
		return false;
	}

	if (!is_literal(c) || !is_structure(c)) {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	cell *c1 = deref(q, c+1, c_ctx);
	pl_idx_t c1_ctx = q->latest_ctx;

	if (!CMP_SLICE2(q, c, "max_depth")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (is_integer(c1) && (get_int(&c[1]) >= 1))
			q->max_depth = get_int(&c[1]);
	} else if (!CMP_SLICE2(q, c, "fullstop")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->fullstop = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "nl")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->nl = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "quoted")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->quoted = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "varnames")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->varnames = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "ignore_ops")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->ignore_ops = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "numbervars")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_literal(c1) || (CMP_SLICE2(q, c1, "true") && CMP_SLICE2(q, c1, "false"))) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->numbervars = !CMP_SLICE2(q, c1, "true");
	} else if (!CMP_SLICE2(q, c, "variable_names")) {
		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_list_or_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		cell *c1_orig = c1;
		pl_idx_t c1_orig_ctx = c1_ctx;
		LIST_HANDLER(c1);

		while (is_list(c1)) {
			cell *h = LIST_HEAD(c1);
			h = deref(q, h, c1_ctx);
			pl_idx_t h_ctx = q->latest_ctx;

			if (is_variable(h)) {
				DISCARD_RESULT throw_error(q, h, h_ctx, "instantiation_error", "write_option");
				return false;
			}

			if (!is_structure(h)) {
				DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (CMP_SLICE2(q, h, "=")) {
				DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (is_literal(h)) {
				cell *h1 = deref(q, h+1, h_ctx);

				if (is_variable(h1)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "instantiation_error", "write_option");
					return false;
				} else if (!is_atom(h1)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}

#if 0
				cell *h2 = deref(q, h+2, h_ctx);

				if (!is_variable(h2)) {
					DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}
#endif
			}

			c1 = LIST_TAIL(c1);
			c1 = deref(q, c1, c1_ctx);
			c1_ctx = q->latest_ctx;
		}

		if (is_variable(c1)) {
			DISCARD_RESULT throw_error(q, c1_orig, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_nil(c1)) {
			DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		if (vnames) *vnames = c1_orig;
		if (vnames_ctx) *vnames_ctx = c1_orig_ctx;
	} else {
		DISCARD_RESULT throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	return true;
}

static USE_RESULT pl_status fn_iso_write_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flag = q->st.m->flag;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx_t p2_orig_ctx = p2_ctx, vnames_ctx = 0;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return pl_success;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

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
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_write_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flag = q->st.m->flag;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx_t p2_orig_ctx = p2_ctx, vnames_ctx;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return pl_success;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_variable(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

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
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_char_1(query *q)
{
	GET_FIRST_ARG(p1,character);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len = len_char_utf8(GET_STR(q, p1));

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != LEN_STR(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = GET_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,character);
	size_t len = len_char_utf8(GET_STR(q, p1));

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != LEN_STR(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = GET_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_byte_1(query *q)
{
	GET_FIRST_ARG(p1,byte);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,text_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_iso_put_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,byte);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (!str->binary)
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,text_stream");

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_int(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

#define FEOF(str) feof(str->fp) && !str->ungetch

static USE_RESULT pl_status fn_iso_get_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	} else if (ch == EOF)
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr++;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_get_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}


	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_peek_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
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
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
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
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
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
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	if (FEOF(str)) {
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
	GET_NEXT_ARG(p4,integer_or_var);

	if (q->retry || is_integer(p1)) {
		if (is_negative(p1))
			return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

		if (is_bigint(p1))
			return pl_failure;

		pl_int_t arg_nbr = get_int(is_integer(p1) ? p1 : p4);

		if (q->retry) {
			if (++arg_nbr > p2->arity)
				return pl_failure;
		}

		if ((arg_nbr == 0) || (arg_nbr > p2->arity))
			return pl_failure;

		cell *c = p2 + 1;

		for (int i = 1; i <= arg_nbr; i++) {
			if (i == arg_nbr) {
				c = deref(q, c, p2_ctx);
				pl_idx_t c_ctx = q->latest_ctx;
				cell tmp;
				make_int(&tmp, arg_nbr);
				GET_RAW_ARG(4,p4_raw);
				set_var(q, p4_raw, p4_ctx, &tmp, q->st.curr_frame);

				if (!is_integer(p1))
					may_error(push_choice(q));

				unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
				return unify(q, p3, p3_ctx, c, c_ctx);
			}

			c += c->nbr_cells;
		}
	}

	return throw_error(q, p1, p1_ctx, "instantiation_error", "number");
}

static USE_RESULT pl_status fn_iso_univ_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (is_variable(p1) && is_nil(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "non_empty_list");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

#if 0
	LIST_HANDLER(p2);
	LIST_HEAD(p2);
	cell *t = LIST_TAIL(p2);
	pl_idx_t t_ctx = p2_ctx;

	if (is_variable(t) && (p2->var_nbr == t->var_nbr) && (p2_ctx == t_ctx))
		return throw_error(q, p2, p2_ctx, "type_error", "list");
#endif

	if (is_string(p1)) {
		cell tmp;
		make_literal(&tmp, g_dot_s);
		allocate_list(q, &tmp);
		LIST_HANDLER(p1);
		cell *h = LIST_HEAD(p1);
		append_list(q, h);
		cell *t = LIST_TAIL(p1);
		append_list(q, t);
		cell *l = end_list(q);
		may_ptr_error(l);
		return unify(q, p2, p2_ctx, l, p1_ctx);
	}

	if (is_variable(p2)) {
		cell *tmp = deep_copy_to_heap(q, p1, p1_ctx, false, false);
		may_ptr_error(tmp);
		if (tmp == ERR_CYCLE_CELL)
			return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

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
		cell *p22 = p2 + 1;
		p22 = deref(q, p22, p2_ctx);

		if (is_variable(p22))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "not_sufficiently_instantiated");

		cell *tmp = deep_copy_to_heap(q, p2, p2_ctx, false, false);
		may_ptr_error(tmp);
		if (tmp == ERR_CYCLE_CELL)
			return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

		unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
		p2 = tmp;
		p2_ctx = q->st.curr_frame;
		unsigned arity = 0;
		may_ptr_error(init_tmp_heap(q));
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
			return throw_error(q, p2, p2_ctx, "instantiation_error", "list");

		if (!is_nil(p2))
			return throw_error(q, save_p2, p2_ctx, "type_error", "list");

		arity--;
		cell *tmp2 = get_tmp_heap(q, 0);
		pl_idx_t nbr_cells = tmp_heap_used(q);

		if (is_cstring(tmp2)) {
			share_cell(tmp2);
			convert_to_literal(q->st.m, tmp2);
		}

		if (!is_literal(tmp2) && arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atom");

		if (tmp2->arity && arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atom");

		if (tmp2->arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atomic");

		if (arity > MAX_ARITY)
			return throw_error(q, tmp2, q->st.curr_frame, "representation_error", "max_arity");

		may_ptr_error(tmp = alloc_on_heap(q, nbr_cells));
		safe_copy_cells(tmp, tmp2, nbr_cells);
		tmp->nbr_cells = nbr_cells;
		tmp->arity = arity;
		bool found = false;

		if (is_callable(tmp)) {
			if ((tmp->match = search_predicate(q->st.m, tmp)) != NULL) {
				tmp->flags &= ~FLAG_BUILTIN;
			} else if ((tmp->fn = get_builtin(q->pl, GET_STR(q, tmp), tmp->arity, &found, NULL)), found) {
				tmp->flags |= FLAG_BUILTIN;
			}
		}

		unsigned specifier;

		if (search_op(q->st.m, GET_STR(q, tmp), &specifier, false))
			SET_OP(tmp, specifier);

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

static cell *do_term_variables(query *q, cell *p1, pl_idx_t p1_ctx)
{
	frame *f = GET_CURR_FRAME();
	q->pl->varno = f->nbr_vars;
	q->pl->tab_idx = 0;
	collect_vars(q, p1, p1_ctx);
	const unsigned cnt = q->pl->tab_idx;
	if (!init_tmp_heap(q)) return NULL;
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
			make_variable2(&v, q->pl->tab3[i]);

			if (q->pl->tab1[i] != q->st.curr_frame) {
				v.flags |= FLAG_VAR_FRESH;
				v.var_nbr = q->pl->varno++;
			} else
				v.var_nbr = q->pl->tab2[i];

			tmp[idx++] = v;
			done++;
		}

		make_literal(tmp+idx++, g_nil_s);
		tmp[0].arity = 2;
		tmp[0].nbr_cells = idx;
	} else
		make_literal(tmp, g_nil_s);

	if (cnt) {
		unsigned new_vars = q->pl->varno - f->nbr_vars;
		q->pl->varno = f->nbr_vars;

		if (new_vars) {
			if (!create_vars(q, new_vars))
				return NULL;
		}

		for (unsigned i = 0; i < cnt; i++) {
			if (q->pl->tab1[i] == q->st.curr_frame)
				continue;

			cell v, tmp2;
			make_variable(&v, g_anon_s, q->pl->varno++);
			v.flags |= FLAG_VAR_FRESH;
			make_variable(&tmp2, g_anon_s, q->pl->tab2[i]);
			tmp2.flags |= FLAG_VAR_FRESH;
			set_var(q, &v, q->st.curr_frame, &tmp2, q->pl->tab1[i]);
		}
	}

	return tmp;		// returns on tmp_heap
}

static USE_RESULT pl_status fn_iso_term_variables_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_variable(p1) && (is_atom(p1) || is_number(p1))) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *tmp = do_term_variables(q, p1, p1_ctx);

	if (!tmp)
		return throw_error(q, p1, p1_ctx, "resource_error", "out_of_memory");

	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	may_ptr_error(tmp2);
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_copy_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

#define ALLOW_CYCLES 0

#if !ALLOW_CYCLES
	bool is_partial = false;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");
#endif

	if (is_atomic(p1) && is_variable(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_variable(p2) && !has_vars(q, p1, p1_ctx, 0))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	GET_FIRST_RAW_ARG(p1_raw,any);
	cell *tmp = deep_copy_to_heap(q, p1_raw, p1_raw_ctx, false, true);

	if (!tmp || (tmp == ERR_CYCLE_CELL))
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

#if ALLOW_CYCLES
	if (!is_variable(p1)) {
		pl_idx_t tmp_ctx = q->st.curr_frame;
		pl_int_t skip = 0;
		cell tmp2;
		cell *t = skip_max_list(q, tmp, &tmp_ctx, 1000000000, &skip, &tmp2);

		if (t && is_variable(t) && !skip) {
			unify(q, p2, p2_ctx, t, q->st.curr_frame);
		}
	}
#endif

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_strip_attributes_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	LIST_HANDLER(p1);

	if (!is_variable(p1) && !is_iso_list(p1))
		return pl_success;

	while (is_list(p1)) {
		cell *c = LIST_HEAD(p1);
		c = deref(q, c, p1_ctx);

		if (is_variable(c)) {
			frame *f = GET_FRAME(q->latest_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = NULL;
		}

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return pl_success;
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
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	cell *tmp = deep_copy_to_heap(q, p1, p1_ctx, false, false);

	if (!tmp || tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_clause_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable_or_var);

	while (match_clause(q, p1, p1_ctx, DO_CLAUSE) == pl_success) {
		if (q->did_throw) return pl_success;
		clause *r = &q->st.curr_clause2->cl;
		cell *body = get_body(r->cells);
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
			stash_me(q, r, last_match);
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

static pl_status do_retractall(query *q, cell *p1, pl_idx_t p1_ctx)
{
	cell *head = deref(q, get_head(p1), p1_ctx);
	predicate *pr = search_predicate(q->st.m, head);

	if (!pr) {
		bool found = false;

		if (get_builtin(q->pl, GET_STR(q, head), head->arity, &found, NULL), found)
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

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

static pl_status do_abolish(query *q, cell *c_orig, cell *c, bool hard)
{
	predicate *pr = search_predicate(q->st.m, c);
	if (!pr) return pl_success;

	if (!pr->is_dynamic)
		return throw_error(q, c_orig, q->st.curr_frame, "permission_error", "modify,static_procedure");

	for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
		if (!q->st.m->loading && dbe->owner->is_persist && !dbe->cl.ugen_erased)
			db_log(q, dbe, LOG_ERASE);

		add_to_dirty_list(q->st.m, dbe);
	}

	m_destroy(pr->idx);
	pr->idx = NULL;

	if (hard) {
		pr->is_abolished = true;
	} else {
		pr->idx = m_create(index_cmpkey, NULL, q->st.m);
		ensure(pr->idx);
		m_allow_dups(pr->idx, false);
	}

	pr->head = pr->tail = NULL;
	pr->cnt = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (CMP_SLICE2(q, p1, "/") && CMP_SLICE2(q, p1, "//"))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, p1_ctx, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!CMP_SLICE2(q, p1, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "type_error", "integer");

	if (is_negative(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "domain_error", "not_less_than_zero");

	if (get_int(p1_arity) > MAX_ARITY)
		return throw_error(q, p1_arity, p1_ctx, "representation_error", "max_arity");

	bool found = false;

	if (get_builtin(q->pl, GET_STR(q, p1_name), get_int(p1_arity), &found, NULL), found)
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");

	cell tmp;
	tmp = *p1_name;
	tmp.arity = get_int(p1_arity);
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

static void do_term_assign_vars(parser *p)
{
	pl_idx_t nbr_cells = p->cl->cidx;
	term_assign_vars(p, 0, true);
	uint8_t vars[MAX_ARITY] = {0};

	for (pl_idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->cl->cells+i;

		if (!is_variable(c))
			continue;

		assert(c->var_nbr < MAX_ARITY);
		vars[c->var_nbr]++;
	}

	for (pl_idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->cl->cells+i;

		if (!is_variable(c))
			continue;

		unsigned var_nbr = count_non_anons(vars, c->var_nbr);

		char ch = 'A';
		ch += var_nbr % 26;
		unsigned n = var_nbr / 26;
		char tmpbuf[80];

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

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	cell *head = get_head(tmp);

	if (is_variable(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_literal(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false;

	if (get_builtin(q->pl, GET_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->nbr_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		may_ptr_error(p->cl);
		p->cl->nbr_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_literal(h))
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");

	db_entry *dbe = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	may_ptr_error(dbe);
	p->cl->cidx = 0;
	uuid_gen(q->pl, &dbe->u);

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ASSERTA);

	return pl_success;
}

static USE_RESULT pl_status fn_iso_assertz_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, p1_ctx, "resource_error", "cycle_limit");

	cell *head = get_head(tmp);

	if (is_variable(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_literal(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false, function = false;

	if (get_builtin(q->pl, GET_STR(q, head), head->arity, &found, &function), found && !function) {
		if (!GET_OP(head))
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->nbr_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		may_ptr_error(p->cl);
		p->cl->nbr_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_literal(h))
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");

	db_entry *dbe = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	may_ptr_error(dbe);
	p->cl->cidx = 0;
	uuid_gen(q->pl, &dbe->u);

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ASSERTZ);

	return pl_success;
}

static USE_RESULT pl_status fn_iso_functor_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_variable(p1)) {
		if (!is_atomic(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "atomic");

		if (!is_integer(p3))
			return throw_error(q, p3, p3_ctx, "type_error", "integer");

		if (is_negative(p3))
			return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

		if (is_gt(p3,MAX_ARITY/2))
			return throw_error(q, p3, p3_ctx, "representation_error", "max_arity");

		if (!is_atom(p2) && is_positive(p3))
			return throw_error(q, p2, p2_ctx, "type_error", "atom");

		unsigned arity = get_int(p3);
		unsigned var_nbr = 0;

		if (arity) {
			if (!(var_nbr = create_vars(q, arity)))
				return throw_error(q, p3, p3_ctx, "resource_error", "stack");

			REGET_FIRST_ARG(p1,any);
			REGET_NEXT_ARG(p2,any);
			REGET_NEXT_ARG(p3,any);
		}

		if (is_number(p2)) {
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		} else {
			cell *tmp = alloc_on_heap(q, 1+arity);
			may_ptr_error(tmp);
			*tmp = (cell){0};
			tmp[0].tag = TAG_LITERAL;
			tmp[0].arity = arity;
			tmp[0].nbr_cells = 1 + arity;

			if (is_cstring(p2)) {
				tmp[0].val_off = index_from_pool(q->pl, GET_STR(q, p2));
			} else
				tmp[0].val_off = p2->val_off;

			for (unsigned i = 1; i <= arity; i++) {
				memset(tmp+i, 0, sizeof(cell));
				tmp[i].tag = TAG_VAR;
				tmp[i].nbr_cells = 1;
				tmp[i].var_nbr = var_nbr++;
				tmp[i].val_off = g_anon_s;
				tmp[i].flags = FLAG_VAR_FRESH | FLAG_VAR_ANON;
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
		tmp.tag = TAG_LITERAL;
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

	if (!CMP_SLICE2(q, p1, "/"))
		;
	else if (!CMP_SLICE2(q, p1, "//"))
		add_two = 2;
	else
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *pf = deref(q, p1+1,p1_ctx);
	cell *pa = deref(q, p1+2, p1_ctx);

	if (!is_atom(pf))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!is_integer(pa))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	const char *functor = GET_STR(q, pf);
	unsigned arity = get_int(pa) + add_two;

	if (strchr(functor, ':')) {
		char tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
		module *m = m = find_module(q->pl, tmpbuf1);
		if (!m) return pl_failure;

		if (find_functor(m, functor, arity))
			return pl_success;

		return pl_failure;
	}

	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = index_from_pool(q->pl, functor);
	tmp.arity = arity;

	if (search_predicate(q->st.m, &tmp))
		return pl_success;

	bool found = false;

	if (get_builtin(q->pl, functor, arity, &found, NULL), found)
		return pl_success;

	return pl_failure;
}

static bool search_functor(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx)
{
	if (!q->retry)
		q->st.f_iter = m_first(q->st.m->index);

	DISCARD_RESULT push_choice(q);
	predicate *pr = NULL;

	while (m_next(q->st.f_iter, (void*)&pr)) {
		if (pr->is_abolished)
			continue;

		if (try_me(q, MAX_ARITY) != pl_success)
			return false;

		cell tmpn, tmpa;
		make_literal(&tmpn, pr->key.val_off);
		make_int(&tmpa, pr->key.arity);

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
	pl_idx_t p1_ctx, p2_ctx;

	if (p_pi->arity != 2)
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	if (CMP_SLICE2(q, p_pi, "/"))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	p1 = p_pi + 1;
	p1 = deref(q, p1, p_pi_ctx);
	p1_ctx = q->latest_ctx;

	if (!is_atom(p1) && !is_variable(p1))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	p2 = p1 + 1;
	p2 = deref(q, p2, p_pi_ctx);
	p2_ctx = q->latest_ctx;

	if ((!is_integer(p2) || is_negative(p2)) && !is_variable(p2))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	if (is_variable(p1) || is_variable(p2))
		return search_functor(q, p1, p1_ctx, p2, p2_ctx) ? pl_success : pl_failure;

	cell tmp = (cell){0};
	tmp.tag = TAG_LITERAL;
	tmp.val_off = is_literal(p1) ? p1->val_off : index_from_pool(q->pl, GET_STR(q, p1));
	tmp.arity = get_int(p2);

	return search_predicate(q->st.m, &tmp) != NULL;
}

static USE_RESULT pl_status fn_cyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_cyclic_term(q, p1, p1_ctx) ? pl_success : pl_failure;
}

static USE_RESULT pl_status fn_iso_acyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_cyclic_term(q, p1, p1_ctx) ? pl_failure : pl_success;
}

static USE_RESULT pl_status fn_iso_current_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);

	if (!CMP_SLICE2(q, p1, "double_quotes")) {
		cell tmp;

		if (q->st.m->flag.double_quote_atom)
			make_literal(&tmp, index_from_pool(q->pl, "atom"));
		else if (q->st.m->flag.double_quote_codes)
			make_literal(&tmp, index_from_pool(q->pl, "codes"));
		else if (q->st.m->flag.double_quote_chars)
			make_literal(&tmp, index_from_pool(q->pl, "chars"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "char_conversion")) {
		cell tmp;

		if (q->st.m->flag.char_conversion)
			make_literal(&tmp, g_on_s);
		else
			make_literal(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "unix")) {
		cell tmp;
		make_literal(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "dos")) {
		cell tmp;
		make_literal(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "windows")) {
		cell tmp;
		make_literal(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "occurs_check")) {
		cell tmp;

		if (q->st.m->flag.occurs_check == OCCURS_TRUE)
			make_literal(&tmp, g_on_s);
		else if (q->st.m->flag.occurs_check == OCCURS_FALSE)
			make_literal(&tmp, g_off_s);
		else
			make_literal(&tmp, index_from_pool(q->pl, "error"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "encoding")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, "UTF-8"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "strict_iso")) {
		cell tmp;

		if (!q->st.m->flag.not_strict_iso)
			make_literal(&tmp, g_on_s);
		else
			make_literal(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "debug")) {
		cell tmp;

		if (q->st.m->flag.debug)
			make_literal(&tmp, g_on_s);
		else
			make_literal(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "character_escapes")) {
		cell tmp;

		if (q->st.m->flag.character_escapes)
			make_literal(&tmp, g_true_s);
		else
			make_literal(&tmp, g_false_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "dialect")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, "trealla"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "integer_rounding_function")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, "toward_zero"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "bounded")) {
		cell tmp;
		make_literal(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "max_arity")) {
		cell tmp;
		make_int(&tmp, MAX_ARITY);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "max_integer")) {
		return false;
	} else if (!CMP_SLICE2(q, p1, "min_integer")) {
		return false;
	} else if (!CMP_SLICE2(q, p1, "cpu_count")) {
		cell tmp;
		make_int(&tmp, g_cpu_count);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "version")) {
		unsigned v1 = 0;
		sscanf(VERSION, "v%u", &v1);
		cell tmp;
		make_int(&tmp, v1);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "version_data")) {
		unsigned v1 = 0, v2 = 0, v3 = 0;
		sscanf(VERSION, "v%u.%u.%u", &v1, &v2, &v3);
		cell *tmp = alloc_on_heap(q, 5);
		may_ptr_error(tmp);
		make_literal(&tmp[0], index_from_pool(q->pl, "trealla"));
		make_int(&tmp[1], v1);
		make_int(&tmp[2], v2);
		make_int(&tmp[3], v3);
		make_literal(&tmp[4], g_nil_s);
		tmp[0].arity = 4;
		tmp[0].nbr_cells = 5;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "version_git")) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, VERSION));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "argv")) {
		if (g_avc >= g_ac) {
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
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "unknown")) {
		cell tmp;
		make_literal(&tmp,
			q->st.m->flag.unknown == UNK_ERROR ? index_from_pool(q->pl, "error") :
			q->st.m->flag.unknown == UNK_WARNING ? index_from_pool(q->pl, "warning") :
			q->st.m->flag.unknown == UNK_CHANGEABLE ? index_from_pool(q->pl, "changeable") :
			index_from_pool(q->pl, "fail"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_SLICE2(q, p1, "generate_debug_info")) {
	}

	return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
}

static USE_RESULT pl_status fn_iso_set_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_atom(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!CMP_SLICE2(q, p1, "cpu_count") && is_integer(p2)) {
		g_cpu_count = get_int(p2);
		return pl_success;
	}

	if (!is_atom(p2) && !is_integer(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "atom");

	if (!CMP_SLICE2(q, p1, "double_quotes")) {
		if (!CMP_SLICE2(q, p2, "atom")) {
			q->st.m->flag.double_quote_chars = q->st.m->flag.double_quote_codes = false;
			q->st.m->flag.double_quote_atom = true;
		} else if (!CMP_SLICE2(q, p2, "codes")) {
			q->st.m->flag.double_quote_chars = q->st.m->flag.double_quote_atom = false;
			q->st.m->flag.double_quote_codes = true;
		} else if (!CMP_SLICE2(q, p2, "chars")) {
			q->st.m->flag.double_quote_atom = q->st.m->flag.double_quote_codes = false;
			q->st.m->flag.double_quote_chars = true;
		} else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}

		q->st.m->p->flag = q->st.m->flag;
	} else if (!CMP_SLICE2(q, p1, "character_escapes")) {
		if (!CMP_SLICE2(q, p2, "true") || !CMP_SLICE2(q, p2, "on"))
			q->st.m->flag.character_escapes = true;
		else if (!CMP_SLICE2(q, p2, "false") || !CMP_SLICE2(q, p2, "off"))
			q->st.m->flag.character_escapes = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "char_conversion")) {
		if (!CMP_SLICE2(q, p2, "true") || !CMP_SLICE2(q, p2, "on"))
			q->st.m->flag.char_conversion = true;
		else if (!CMP_SLICE2(q, p2, "false") || !CMP_SLICE2(q, p2, "off"))
			q->st.m->flag.char_conversion = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "occurs_check")) {
		if (!CMP_SLICE2(q, p2, "true") || !CMP_SLICE2(q, p2, "on"))
			q->st.m->flag.occurs_check = OCCURS_TRUE;
		else if (!CMP_SLICE2(q, p2, "false") || !CMP_SLICE2(q, p2, "off"))
			q->st.m->flag.occurs_check = OCCURS_FALSE;
		else if (!CMP_SLICE2(q, p2, "error"))
			q->st.m->flag.occurs_check = OCCURS_ERROR;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "debug")) {
		if (!CMP_SLICE2(q, p2, "true") || !CMP_SLICE2(q, p2, "on"))
			q->st.m->flag.debug = true;
		else if (!CMP_SLICE2(q, p2, "false") || !CMP_SLICE2(q, p2, "off"))
			q->st.m->flag.debug = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "strict_iso")) {
		if (!CMP_SLICE2(q, p2, "true") || !CMP_SLICE2(q, p2, "on"))
			q->st.m->flag.not_strict_iso = !true;
		else if (!CMP_SLICE2(q, p2, "false") || !CMP_SLICE2(q, p2, "off"))
			q->st.m->flag.not_strict_iso = !false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "unknown")) {
		if (!CMP_SLICE2(q, p2, "fail")) {
			q->st.m->flag.unknown = UNK_FAIL;
		} else if (!CMP_SLICE2(q, p2, "error")) {
			q->st.m->flag.unknown = UNK_ERROR;
		} else if (!CMP_SLICE2(q, p2, "warning")) {
			q->st.m->flag.unknown = UNK_WARNING;
		} else if (!CMP_SLICE2(q, p2, "changeable")) {
			q->st.m->flag.unknown = UNK_CHANGEABLE;
		} else {
			cell *tmp = alloc_on_heap(q, 3);
			make_structure(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_SLICE2(q, p1, "bounded")
		|| !CMP_SLICE2(q, p1, "max_arity")
		|| !CMP_SLICE2(q, p1, "max_integer")
		|| !CMP_SLICE2(q, p1, "min_integer")
		|| !CMP_SLICE2(q, p1, "version")
		|| !CMP_SLICE2(q, p1, "version_data")
		|| !CMP_SLICE2(q, p1, "version_git")
		|| !CMP_SLICE2(q, p1, "encoding")
		|| !CMP_SLICE2(q, p1, "unix")
		|| !CMP_SLICE2(q, p1, "integer_rounding_function")
		|| !CMP_SLICE2(q, p1, "dialect")
		) {
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,flag");
	} else if (!CMP_SLICE2(q, p1, "generate_debug_info")) {
	} else {
		return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
	}

	q->flag = q->st.m->flag;
	return pl_success;
}

typedef struct { cell *c; pl_idx_t c_ctx; query *q; bool ascending; int arg; } basepair;

static int nodecmp(const void *ptr1, const void *ptr2)
{
	const basepair *cp1 = (const basepair*)ptr1;
	const basepair *cp2 = (const basepair*)ptr2;
	bool ascending = cp1->ascending;
	query *q = cp1->q;
	int arg = cp1->arg;
	cell *p1 = cp1->c, *p2 = cp2->c;
	pl_idx_t p1_ctx = cp1->c_ctx, p2_ctx = cp2->c_ctx;

	p1 = deref(q, p1, p1_ctx);
	p1_ctx = q->latest_ctx;

	p2 = deref(q, p2, p2_ctx);
	p2_ctx = q->latest_ctx;

	if ((p1->arity >= arg) && (arg > 0)) {
		p1 = p1 + 1;
		p2 = p2 + 1;

		while (--arg > 0) {
			p1 += p1->nbr_cells;
			p2 += p2->nbr_cells;
		}

		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	int ok = compare(q, p1, p1_ctx, p2, p2_ctx);

	if (ascending)
		return ok < 0 ? -1 : ok > 0 ? 1 : 0;
	else
		return ok < 0 ? 1 : ok > 0 ? -1 : 0;
}

static cell *nodesort(query *q, cell *p1, pl_idx_t p1_ctx, bool dedup, bool keysort, pl_status *status)
{
	pl_int_t max = PL_INT_MAX, skip = 0;
	pl_idx_t tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		pl_idx_t h_ctx = p1_ctx;

		if (keysort) {
			cell *tmp = deref(q, h, h_ctx);
			pl_idx_t tmp_ctx = q->latest_ctx;

			if (!is_structure(tmp) || strcmp(GET_STR(q, tmp), "-")) {
				*status = throw_error(q, tmp, tmp_ctx, "type_error", "pair");
				free(base);
				return NULL;
			}
		}

		base[idx].c = h;
		base[idx].c_ctx = h_ctx;
		base[idx].q = q;
		base[idx].ascending = true;
		base[idx].arg = keysort ? 1 : 0;
		idx++;
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	// On Linux qsort seems to produce a stable sort, although it's
	// not guaranteed. On BSD systems mergesort is supposed to be.
	// Note: bagof/setof are now using a Prolog sort/keysort that
	// is known to be stable.

#if __BSD__ || __FREEBSD__ || __APPLE__ || __MACH__ || __Darwin__ || __DragonFly__
	mergesort(base, cnt, sizeof(basepair), nodecmp);
#else
	qsort(base, cnt, sizeof(basepair), nodecmp);
#endif

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		cell tmp;

		if (is_variable(c) || is_structure(c)) {
			make_variable(&tmp, c->val_off, create_vars(q, 1));
			unify(q, c, c_ctx, &tmp, q->st.curr_frame);
			c = &tmp;
		}

		if (i == 0)
			allocate_list(q, c);
		else
			append_list(q, c);
	}

	cell *l = end_list(q);
	free(base);
	return l;
}

static USE_RESULT pl_status fn_iso_sort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	pl_status status;
	cell *l = nodesort(q, p1, p1_ctx, true, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_msort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	pl_status status;
	cell *l = nodesort(q, p1, p1_ctx, false, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_iso_keysort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_iso_list(p2)) {
		LIST_HANDLER(p2);
		cell *tmp_h = LIST_HEAD(p2);
		tmp_h = deref(q, tmp_h, p2_ctx);
		pl_idx_t tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p2);

		if (!is_variable(tmp_h) && (!is_structure(tmp_h) || strcmp(GET_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	pl_status status = 0;
	cell *l = nodesort(q, p1, p1_ctx, false, true, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static cell *nodesort4(query *q, cell *p1, pl_idx_t p1_ctx, bool dedup, bool ascending, int arg, pl_status *status)
{
	pl_int_t max = PL_INT_MAX, skip = 0;
	pl_idx_t tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		pl_idx_t h_ctx = p1_ctx;

		base[idx].c = h;
		base[idx].c_ctx = h_ctx;
		base[idx].q = q;
		base[idx].ascending = ascending;
		base[idx].arg = arg;
		idx++;
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	// On Linux qsort seems to produce a stable sort, although it's
	// not guaranteed. On BSD systems mergesort is supposed to be.
	// Note: bagof/setof are now using a Prolog sort/keysort that
	// is known to be stable.

#if __BSD__ || __FREEBSD__ || __APPLE__ || __MACH__ || __Darwin__ || __DragonFly__
	mergesort(base, cnt, sizeof(basepair), nodecmp);
#else
	qsort(base, cnt, sizeof(basepair), nodecmp);
#endif

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		cell tmp;

		if (is_variable(c) || is_structure(c)) {
			make_variable(&tmp, c->val_off, create_vars(q, 1));
			unify(q, c, c_ctx, &tmp, q->st.curr_frame);
			c = &tmp;
		}

		if (i == 0)
			allocate_list(q, c);
		else
			append_list(q, c);
	}

	cell *l = end_list(q);
	free(base);
	return l;
}

static USE_RESULT pl_status fn_sort_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_nil);
	GET_NEXT_ARG(p4,list_or_nil_or_var);
	bool is_partial = false, dedup = false, ascending = true;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_integer(p1) && is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	int arg = get_smallint(p1);
	const char *src = GET_STR(q, p2);

	if (!strcmp(src, "@<")) {
		ascending = true;
		dedup = true;
	} else if (!strcmp(src, "@=<")) {
		ascending = true;
		dedup = false;
	} else if (!strcmp(src, "@>")) {
		ascending = false;
		dedup = true;
	} else if (!strcmp(src, "@>=")) {
		ascending = false;
		dedup = false;
	} else
		return throw_error(q, p2, p2_ctx, "domain_error", "order");

	if (is_iso_list(p3) && !check_list(q, p3, p3_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p3, p3_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p3, p3_ctx, "instantiation_error", "list");

	if (is_iso_list(p4) && !check_list(q, p4, p4_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p4, p4_ctx, "type_error", "list");

	if (is_iso_list(p4)) {
		LIST_HANDLER(p4);
		cell *tmp_h = LIST_HEAD(p4);
		tmp_h = deref(q, tmp_h, p4_ctx);
		pl_idx_t tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p4);

		if (!is_variable(tmp_h) && (!is_structure(tmp_h) || strcmp(GET_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p3)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	pl_status status = 0;
	cell *l = nodesort4(q, p3, p3_ctx, dedup, ascending, arg, &status);
	if (!l) return status;
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

static cell *convert_to_list(query *q, cell *c, pl_idx_t nbr_cells)
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
	frame *f = GET_CURR_FRAME();
	unsigned new_varno = f->nbr_vars;
	cell *c = l;

	for (pl_idx_t i = 0; i < l->nbr_cells; i++, c++) {
		if (is_variable(c) && is_anon(c)) {
			c->var_nbr = new_varno++;
			c->flags = FLAG_VAR_FRESH | FLAG_VAR_ANON;
		}
	}

	if (new_varno != f->nbr_vars) {
		if (!create_vars(q, new_varno-f->nbr_vars))
			return throw_error(q, p1, p1_ctx, "resource_error", "stack");

		REGET_FIRST_ARG(p1,variable);
	}
#endif

	return unify(q, p1, p1_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_queue_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *tmp = deep_raw_copy_to_tmp(q, p1, p1_ctx);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		may_ptr_error(alloc_on_queuen(q, 0, p1));
	else
		may_ptr_error(alloc_on_queuen(q, 0, tmp));

	return pl_success;
}

static USE_RESULT pl_status fn_sys_queuen_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	cell *tmp = deep_raw_copy_to_tmp(q, p2, p2_ctx);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		may_ptr_error(alloc_on_queuen(q, get_int(p1), p2));
	else
		may_ptr_error(alloc_on_queuen(q, get_int(p1), tmp));

	return pl_success;
}

static USE_RESULT pl_status fn_iso_findall_3(query *q)
{
	GET_FIRST_ARG(xp1,any);
	GET_NEXT_ARG(xp2,callable);
	GET_NEXT_ARG(xp3,list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(xp3) && !check_list(q, xp3, xp3_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, xp3, xp3_ctx, "type_error", "list");

	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false, true);
	GET_FIRST_ARG0(p1,any,p0);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	if (p0 == ERR_CYCLE_CELL)
		return throw_error(q, q->st.curr_cell, q->st.curr_frame, "resource_error", "cyclic_term");

	if (!q->retry) {
		q->st.qnbr++;
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+2);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		may_error(push_barrier(q));
		q->st.curr_cell = tmp;
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		return pl_success;
	}

	if (!queuen_used(q)) {
		q->st.qnbr--;
		cell tmp;
		make_literal(&tmp, g_nil_s);
		pl_status ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);

		if (ok == pl_success)
			unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);

		return ok;
	}

	// Retry takes a copy

	pl_idx_t nbr_cells = queuen_used(q);
	q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
	may_ptr_error(q->tmpq[q->st.qnbr]);
	copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
	q->tmpq_size[q->st.qnbr] = nbr_cells;

	// Now grab matching solutions

	init_queuen(q);
	may_error(push_choice(q));
	nbr_cells = q->tmpq_size[q->st.qnbr];

	for (cell *c = q->tmpq[q->st.qnbr]; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		may_error(try_me(q, MAX_ARITY));

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
			may_ptr_error(tmp);
			may_ptr_error(alloc_on_queuen(q, q->st.qnbr, tmp));
		}

		undo_me(q);
	}

	// Return matching solutions

	drop_choice(q);
	trim_trail(q);
	free(q->tmpq[q->st.qnbr]);
	q->tmpq[q->st.qnbr] = NULL;
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	q->st.qnbr--;
	pl_status ok = unify(q, p3, p3_ctx, l, q->st.curr_frame);

	if (ok == pl_success)
		unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);

	return ok;
}

static pl_status do_op(query *q, cell *p3, pl_idx_t p3_ctx)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);

	if (!is_atom(p3))
		return throw_error(q, p3, p3_ctx, "type_error", "atom");

	unsigned specifier;
	unsigned pri = get_int(p1);

	if (!CMP_SLICE2(q, p2, "fx"))
		specifier = OP_FX;
	else if (!CMP_SLICE2(q, p2, "fy"))
		specifier = OP_FY;
	else if (!CMP_SLICE2(q, p2, "xf"))
		specifier = OP_XF;
	else if (!CMP_SLICE2(q, p2, "xfx"))
		specifier = OP_XFX;
	else if (!CMP_SLICE2(q, p2, "xfy"))
		specifier = OP_XFY;
	else if (!CMP_SLICE2(q, p2, "yf"))
		specifier = OP_YF;
	else if (!CMP_SLICE2(q, p2, "yfx"))
		specifier = OP_YFX;
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "operator_specifier");

	if (pri && !CMP_SLICE2(q, p3, "|") && (!IS_INFIX(specifier) || (pri < 1001)))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_SLICE2(q, p3, "[]"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_SLICE2(q, p3, "{}"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_SLICE2(q, p3, ","))
		return throw_error(q, p3, p3_ctx, "permission_error", "modify,operator");

	unsigned tmp_optype = 0;
	search_op(q->st.m, GET_STR(q, p3), &tmp_optype, false);

	if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	unsigned tmp_pri = find_op(q->st.m, GET_STR(q, p3), OP_FX);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	tmp_pri = find_op(q->st.m, GET_STR(q, p3), OP_FY);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!set_op(q->st.m, GET_STR(q, p3), specifier, pri))
		return throw_error(q, p3, p3_ctx, "resource_error", "too_many_ops");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_op_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_atom);

	if (is_negative(p1) || is_gt(p1,1200))
		return throw_error(q, p1, p1_ctx, "domain_error", "operator_priority");

	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		pl_status ok = do_op(q, h, q->latest_ctx);

		if (ok != pl_success)
			return ok;

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_variable(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "atom");

		if (is_nil(p3))
			return pl_success;
	}

	if (is_atom(p3))
		return do_op(q, p3, p3_ctx);

	return pl_success;
}

static USE_RESULT pl_status fn_erase_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	uuid u;
	uuid_from_buf(GET_STR(q, p1), &u);
	db_entry *dbe = erase_from_db(q->st.m, &u);
	may_ptr_error(dbe);

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ERASE);

	return pl_success;
}

static USE_RESULT pl_status fn_instance_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	uuid u;
	uuid_from_buf(GET_STR(q, p1), &u);
	db_entry *dbe = find_in_db(q->st.m, &u);
	may_ptr_error(dbe);
	return unify(q, p2, p2_ctx, dbe->cl.cells, q->st.curr_frame);
}

static USE_RESULT pl_status fn_clause_3(query *q)
{
	GET_FIRST_ARG(p1,callable_or_var);
	GET_NEXT_ARG(p2,callable_or_var);
	GET_NEXT_ARG(p3,atom_or_var);

	if (is_variable(p1) && is_variable(p2) && is_variable(p3))
		return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	for (;;) {
		clause *r;

		if (!is_variable(p3)) {
			uuid u;
			uuid_from_buf(GET_STR(q, p3), &u);
			db_entry *dbe = find_in_db(q->st.m, &u);

			if (!dbe || (!u.u1 && !u.u2))
				break;

			q->st.curr_clause2 = dbe;
			r = &dbe->cl;
			cell *head = get_head(r->cells);

			if (!unify(q, p1, p1_ctx, head, q->st.fp))
				break;
		} else {
			if (match_clause(q, p1, p1_ctx, DO_CLAUSE) != pl_success)
				break;

			char tmpbuf[128];
			uuid_to_buf(&q->st.curr_clause2->u, tmpbuf, sizeof(tmpbuf));
			cell tmp;
			may_error(make_cstring(&tmp, tmpbuf));
			set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
			r = &q->st.curr_clause2->cl;
		}

		cell *body = get_body(r->cells);
		pl_status ok;

		if (body)
			ok = unify(q, p2, p2_ctx, body, q->st.fp);
		else {
			cell tmp;
			make_literal(&tmp, g_true_s);
			ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		if (ok) {
			if (is_variable(p3)) {
				bool last_match = !q->st.curr_clause2->next;
				stash_me(q, r, last_match);
			}

			return pl_success;
		}

		if (!is_variable(p3))
			break;

		undo_me(q);
		drop_choice(q);
		q->retry = QUERY_RETRY;
	}

	return pl_failure;
}

static pl_status do_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_variable(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->pl, GET_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body))
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);
	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->nbr_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		may_ptr_error(p->cl);
		p->cl->nbr_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_literal(h))
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");

	db_entry *dbe = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	may_ptr_error(dbe);
	p->cl->cidx = 0;

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(q, p2), &u);
		dbe->u = u;
	} else {
		uuid_gen(q->pl, &dbe->u);
		char tmpbuf[128];
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		may_error(make_cstring(&tmp2, tmpbuf));
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ASSERTA);

	return pl_success;
}

static USE_RESULT pl_status fn_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,variable);
	return do_asserta_2(q);
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_sys_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,atom);
	return do_asserta_2(q);
}
#endif

static pl_status do_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_variable(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->pl, GET_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body))
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
	may_ptr_error(tmp);

	if (tmp == ERR_CYCLE_CELL)
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->nbr_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		may_ptr_error(p->cl);
		p->cl->nbr_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_literal(h))
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");

	db_entry *dbe = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	may_ptr_error(dbe);
	p->cl->cidx = 0;

	if (!is_variable(p2)) {
		uuid u;
		uuid_from_buf(GET_STR(q, p2), &u);
		dbe->u = u;
	} else {
		uuid_gen(q->pl, &dbe->u);
		char tmpbuf[128];
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		may_error(make_cstring(&tmp2, tmpbuf));
		set_var(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	if (!q->st.m->loading && dbe->owner->is_persist)
		db_log(q, dbe, LOG_ASSERTZ);

	return pl_success;
}

static USE_RESULT pl_status fn_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,variable);
	return do_assertz_2(q);
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_sys_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,atom);
	return do_assertz_2(q);
}
#endif

static void save_db(FILE *fp, query *q, int logging)
{
	q->listing = true;

	for (predicate *pr = q->st.m->head; pr && !g_tpl_interrupt; pr = pr->next) {
		if (pr->is_prebuilt)
			continue;

		if (logging && !pr->is_persist)
			continue;

		const char *src = GET_STR(q, &pr->key);

		if (src[0] == '$')
			continue;

		for (db_entry *dbe = pr->head; dbe && !g_tpl_interrupt; dbe = dbe->next) {
			if (dbe->cl.ugen_erased)
				continue;

			if (logging)
				fprintf(fp, "z_(");

			print_term(q, fp, dbe->cl.cells, q->st.curr_frame, 0);

			if (logging) {
				char tmpbuf[256];
				uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
				fprintf(fp, ",'%s')", tmpbuf);
			}

			fprintf(fp, ".\n");
		}
	}

	q->listing = false;
}

static USE_RESULT pl_status fn_listing_0(query *q)
{
	save_db(stdout, q, 0);
	return pl_success;
}

static void save_name(FILE *fp, query *q, pl_idx_t name, unsigned arity)
{
	module *m = q->st.curr_clause ? q->st.curr_clause->owner->m : q->st.m;

	for (predicate *pr = m->head; pr && !g_tpl_interrupt; pr = pr->next) {
		if (pr->is_prebuilt && (arity == -1U))
			continue;

		if (name != pr->key.val_off)
			continue;

		if ((arity != pr->key.arity) && (arity != -1U))
			continue;

		for (db_entry *dbe = pr->head; dbe && !g_tpl_interrupt; dbe = dbe->next) {
			if (dbe->cl.ugen_erased)
				continue;

			print_term(q, fp, dbe->cl.cells, q->st.curr_frame, 0);
			fprintf(fp, ".\n");
		}
	}
}

static USE_RESULT pl_status fn_listing_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	pl_idx_t name = p1->val_off;
	unsigned arity = -1;

	if (p1->arity) {
		if (CMP_SLICE2(q, p1, "/") && CMP_SLICE2(q, p1, "//"))
			return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, p1_ctx, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, p1_ctx, "type_error", "integer");

		name = index_from_pool(q->pl, GET_STR(q, p2));
		arity = get_int(p3);

		if (!CMP_SLICE2(q, p1, "//"))
			arity += 2;
	}

	save_name(stdout, q, name, arity);
	return pl_success;
}

const char *dump_key(const void *k, UNUSED const void *v, const void *p)
{
	query *q = (query*)p;
	cell *c = (cell*)k;
	static char tmpbuf[1024];
	print_term_to_buf(q, tmpbuf, sizeof(tmpbuf), c, q->st.curr_frame, 0, false, 0);
	return tmpbuf;
}

static USE_RESULT pl_status fn_sys_dump_keys_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	const char *name = NULL;
	unsigned arity = -1;

	if (p1->arity) {
		if (CMP_SLICE2(q, p1, "/") && CMP_SLICE2(q, p1, "//"))
			return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, p1_ctx, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, p1_ctx, "type_error", "integer");

		name = GET_STR(q, p2);
		arity = get_int(p3);

		if (!CMP_SLICE2(q, p1, "//"))
			arity += 2;
	}

	predicate *pr = find_functor(q->st.m, name, arity);

	if (!pr)
		return pl_failure;

	if (!pr->idx)
		return pl_success;

	fprintf(stderr, "\n"); sl_dump(pr->idx, dump_key, q);
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
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_elapsed_s, fn_sys_elapsed_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_statistics_0(query *q)
{
	fprintf(stdout,
		"Goals %llu, Matches %llu, Max frames %u, choices %u, trails %u, slots %u, heap: %u. Backtracks %llu, TCOs:%llu\n",
		(unsigned long long)q->tot_goals, (unsigned long long)q->tot_matches,
		q->max_frames, q->max_choices, q->max_trails, q->max_slots, q->st.hp,
		(unsigned long long)q->tot_retries, (unsigned long long)q->tot_tcos);
	return pl_success;
}

static USE_RESULT pl_status fn_statistics_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_var);

	if (!CMP_SLICE2(q, p1, "cputime") && is_variable(p2)) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->time_cpu_started;
		cell tmp;
		make_real(&tmp, elapsed/1000/1000);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!CMP_SLICE2(q, p1, "gctime") && is_variable(p2)) {
		cell tmp;
		make_real(&tmp, 0);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!CMP_SLICE2(q, p1, "runtime")) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->time_cpu_started;
		cell tmp;
		make_int(&tmp, elapsed/1000);
		allocate_list(q, &tmp);
		elapsed = now - q->time_cpu_last_started;
		q->time_cpu_last_started = now;
		make_int(&tmp, elapsed/1000);
		append_list(q, &tmp);
		make_literal(&tmp, g_nil_s);
		cell *l = end_list(q);
		may_ptr_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return pl_failure;
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_sleep_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task)
		return do_yield_0(q, get_int(p1)*1000);

	sleep((unsigned)get_int(p1));
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_delay_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,integer);

	if (q->is_task)
		return do_yield_0(q, get_int(p1));

	msleep((unsigned)get_int(p1));
	return pl_success;
}
#endif

static USE_RESULT pl_status fn_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	pl_int_t elapse = get_int(p1);

	if (elapse < 0)
		return pl_success;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return pl_success;

	pl_uint_t started = get_time_in_usec() / 1000;
	pl_uint_t end = started + elapse;

	while (((get_time_in_usec() / 1000) && !g_tpl_interrupt)  < end)
		;

	return pl_success;
}

static USE_RESULT pl_status fn_now_0(query *q)
{
	pl_int_t secs = get_time_in_usec() / 1000 / 1000;
	q->accum.tag = TAG_INT;
	set_smallint(&q->accum, secs);
	return pl_success;
}

static USE_RESULT pl_status fn_now_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	pl_int_t secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	double v = ((double)get_time_in_usec()-q->get_started) / 1000 / 1000;
	cell tmp;
	make_real(&tmp, (double)v);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_cpu_time_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	double v = ((double)cpu_time_in_usec()-q->time_cpu_started) / 1000 / 1000;
	cell tmp;
	make_real(&tmp, (double)v);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);
	GET_NEXT_ARG(p4,integer_or_var);

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (!is_integer(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	if (!q->retry) {
		if (get_int(p1) > get_int(p2))
			return pl_failure;

		if (!is_variable(p3)) {
			if (get_int(p3) > get_int(p2))
				return pl_failure;

			if (get_int(p3) < get_int(p1))
				return pl_failure;

			return pl_success;
		}

		reset_var(q, p4, q->st.curr_frame, p1, q->st.curr_frame, false);

		if (get_int(p1) != get_int(p2))
			may_error(push_choice(q));

		set_var(q, p3, p3_ctx, p1, q->st.curr_frame);
		return pl_success;
	}

	pl_int_t val = get_int(p4) + 1;
	GET_RAW_ARG(4,p4_raw);
	cell tmp;
	make_int(&tmp, val);
	reset_var(q, p4_raw, q->st.curr_frame, &tmp, q->st.curr_frame, false);

	if (val != get_int(p2))
		may_error(push_choice(q));

	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

#if 0
static USE_RESULT pl_status fn_forall_2(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);

	pl_idx_t off = heap_used(q);
	may_ptr_error(clone_to_heap(q, true, p1, 0));
	may_ptr_error(clone_to_heap(q, false, p2, 1));

	cell *tmp = get_heap(q, off);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells + p2->nbr_cells;
	make_structure(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
	may_error(push_choice(q));
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
	const char *src = GET_STR(q, p1);
	int sep = peek_char_utf8(GET_STR(q, p2));
	int pad = peek_char_utf8(GET_STR(q, p3));
	const char *start = src, *ptr;
	cell *l = NULL;
	int nbr = 1, in_list = 0;

	if (!*start) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	// FIXME: sep & pad are not a single char...

	while ((ptr = strchr_utf8(start, sep)) != NULL) {
		while ((peek_char_utf8(start) == pad) && (pad != sep))
			get_char_utf8(&start);

		if (ptr-start) {
			cell tmp;
			may_error(make_slice(q, &tmp, p1, start-src, ptr-start));

			if (nbr++ == 1)
				allocate_list(q, &tmp);
			else
				append_list(q, &tmp);

			in_list = 1;
		}

		start = ptr + 1;
	}

	if (*start) {
		while (peek_char_utf8(start) == pad)
			get_char_utf8(&start);

		cell tmp;
		may_error(make_slice(q, &tmp, p1, start-src, LEN_STR(q, p1)-(start-src)));

		if (LEN_STR(q, p1)-(start-src)) {
			if (!in_list)
				allocate_list(q, &tmp);
			else
				append_list(q, &tmp);
		}
	}

	l = end_list(q);
	may_ptr_error(l);
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_split_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_nil(p1) || !LEN_STR(q, p1)) {
		cell tmp;
		make_literal(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return pl_failure;

		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	const char *start = GET_STR(q, p1), *ptr;
	int ch = peek_char_utf8(GET_STR(q, p2));

	if ((ptr = strchr_utf8(start, ch)) != NULL) {
		cell tmp;

		if (ptr != start)
			may_error(make_stringn(&tmp, start, ptr-start));
		else
			make_literal(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
			unshare_cell(&tmp);
			return pl_failure;
		}

		unshare_cell(&tmp);
		ptr = ptr+1;

		while (isspace(*ptr))
			ptr++;

		if (*ptr)
			may_error(make_stringn(&tmp, ptr, LEN_STR(q, p1)-(ptr-start)));
		else
			make_literal(&tmp, g_nil_s);

		pl_status ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!unify(q, p3, p3_ctx, p1, p1_ctx))
		return pl_failure;

	cell tmp;
	make_literal(&tmp, g_nil_s);
	return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_savefile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	FILE *fp = fopen(filename, "wb");
	may_ptr_error(fp);
	fwrite(GET_STR(q, p2), 1, LEN_STR(q, p2), fp);
	fclose(fp);
	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_loadfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	FILE *fp = fopen(filename, "rb");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp), offset = 0;

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);
	else
		offset = 3;

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return pl_error;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	may_ptr_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	may_error(make_stringn(&tmp, s, len), free(s));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return pl_success;
}
#endif

static USE_RESULT pl_status fn_read_file_to_string_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,list_or_nil);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = src = DUP_SLICE(q, p1);

	bool bom_specified = false, use_bom = false, is_binary = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_variable(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_structure(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);

			if (!CMP_SLICE2(q, c, "type")) {
				if (is_atom(name) && !CMP_SLICE2(q, name, "binary")) {
					is_binary = true;
				} else if (is_atom(name) && !CMP_SLICE2(q, name, "text"))
					is_binary = false;
				else
					return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");
			} else if (!CMP_SLICE2(q, c, "bom")) {
				bom_specified = true;

				if (is_atom(name) && !CMP_SLICE2(q, name, "true"))
					use_bom = true;
				else if (is_atom(name) && !CMP_SLICE2(q, name, "false"))
					use_bom = false;
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_variable(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	FILE *fp = fopen(filename, is_binary?"rb":"r");
	free(src);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	size_t offset = 0;

	if (!is_binary && (!bom_specified || use_bom)) {
		int ch = getc_utf8(fp);

		if ((unsigned)ch != 0xFEFF)
			fseek(fp, 0, SEEK_SET);
		else
			offset = 3;
	}

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return pl_error;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	may_ptr_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	may_error(make_stringn(&tmp, s, len), free(s));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return pl_success;
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_getfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	FILE *fp = fopen(filename, "r");
	free(filename);

	if (!fp) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");
	}

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while ((getline(&line, &len, fp) != -1) && !g_tpl_interrupt) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
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
#endif

static USE_RESULT pl_status fn_getlines_1(query *q)
{
	GET_NEXT_ARG(p1,variable);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while ((getline(&line, &len, str->fp) != -1) && !g_tpl_interrupt) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
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

	if (!in_list) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	}

	return pl_success;
}

static USE_RESULT pl_status fn_getlines_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1, in_list = 0;

	while ((getline(&line, &len, str->fp) != -1) && !g_tpl_interrupt) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
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

	if (!in_list) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	} else {
		cell *l = end_list(q);
		may_ptr_error(l);
		set_var(q, p1, p1_ctx, l, q->st.curr_frame);
	}

	return pl_success;
}

static void parse_host(const char *src, char *hostname, char *path, unsigned *port, int *ssl, int *domain)
{
	if (!strncmp(src, "https://", 8)) {
		src += 8;
		*ssl = 1;
		*port = 443;
	} else if (!strncmp(src, "http://", 7)) {
		src += 7;
		*ssl = 0;
		*port = 80;
	} else if (!strncmp(src, "unix://", 7)) {
		src += 7;
		*domain = 1;
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
	char hostname[1024], path[1024*4];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	unsigned port = 80;
	snprintf(hostname, sizeof(hostname), "localhost");
	path[0] = '\0';
	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_SLICE2(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "keyfile")) {
				c = c + 1;

				if (is_atom(c))
					keyfile = GET_STR(q, c);
			} else if (!CMP_SLICE2(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = GET_STR(q, c);
			} else if (!CMP_SLICE2(q, c, "hostname")) {
				c = c + 1;

				if (is_atom(c))
					slicecpy(hostname, sizeof(hostname), GET_STR(q, c), LEN_STR(q, c));
			} else if (!CMP_SLICE2(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_SLICE2(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_SLICE2(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = get_int(c);
			} else if (!CMP_SLICE2(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_int(c);
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	int fd = net_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "existence_error", "server_failed");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = DUP_SLICE(q, p1));
	may_ptr_error(str->name = strdup(hostname));
	may_ptr_error(str->mode = strdup("update"));
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->fp = fdopen(fd, "r+");
	str->ssl = ssl;
	str->level = level;
	str->sslptr = NULL;

	if (str->fp == NULL) {
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
		close(fd);
	}

	if (!str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	int fd = net_accept(str);

	if (fd == -1) {
		if (q->is_task)
			return do_yield_0(q, 1);

		return pl_failure;
	}

	n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str2 = &q->pl->streams[n];
	may_ptr_error(str2->filename = strdup(str->filename));
	may_ptr_error(str2->name = strdup(str->name));
	may_ptr_error(str2->mode = strdup("update"));
	str->socket = true;
	str2->nodelay = str->nodelay;
	str2->nonblock = str->nonblock;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str2->sslptr = net_enable_ssl(fd, str->name, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			return pl_failure;
		}
	}

	if (!str->ssl)
		net_set_nonblocking(str2);

	may_error(push_choice(q));
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
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
	char hostname[1024], path[1024*4];
	char *certfile = NULL;
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	hostname[0] = path[0] = '\0';
	unsigned port = 80;
	LIST_HANDLER(p5);

	while (is_list(p5) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_SLICE2(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			} else if (!CMP_SLICE2(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = GET_STR(q, c);
			} else if (!CMP_SLICE2(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_SLICE2(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_SLICE2(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = (int)get_int(c);
			} else if (!CMP_SLICE2(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_int(c);
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = GET_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	while (is_list(p5) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_structure(c) && (c->arity == 1)) {
			if (!CMP_SLICE2(q, c, "host")) {
				c = c + 1;

				//if (is_atom(c))
				//	;//udp = !CMP_SLICE2(q, c, "true") ? 1 : 0;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	int fd = net_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "resource_error", "could_not_connect");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	may_ptr_error(str->filename = DUP_SLICE(q, p1));
	may_ptr_error(str->name = strdup(hostname));
	may_ptr_error(str->mode = strdup("update"));
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
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str->sslptr = net_enable_ssl(fd, hostname, 0, str->level, certfile);
		may_ptr_error (str->sslptr, close(fd));
	}

	if (nonblock && !str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	may_error(make_string(&tmp, hostname));
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	may_error(make_string(&tmp, path));
	set_var(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	cell tmp2;
	make_int(&tmp2, n);
	tmp2.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	set_var(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_getline_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);
		return pl_failure;
	}

	len = strlen(line);

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_getline_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield_0(q, 1);
		}

		return pl_failure;
	}

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_read_line_to_string_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield_0(q, 1);
		}

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	may_error(make_string(&tmp, line), free(line));
	free(line);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_bread_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,variable);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_integer(p1) && is_positive(p1)) {
		if (!str->data) {
			str->data = malloc(get_int(p1)+1);
			may_ptr_error(str->data);
			str->data_len = 0;
		}

		for (;;) {
			len = get_int(p1) - str->data_len;
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
				return do_yield_0(q, 1);
			}
		}

		cell tmp;
		may_error(make_stringn(&tmp, str->data, str->data_len), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return pl_success;
	}

	if (is_integer(p1)) {
		if (!str->data) {
			str->data = malloc((str->alloc_nbytes=1024)+1);
			may_ptr_error(str->data);
			str->data_len = 0;
		}

		size_t nbytes = net_read(str->data, str->alloc_nbytes, str);
		str->data[nbytes] = '\0';
		str->data = realloc(str->data, nbytes+1);
		may_ptr_error(str->data);
		cell tmp;
		may_error(make_stringn(&tmp, str->data, nbytes), free(str->data));
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return pl_success;
	}

	if (!str->data) {
		str->data = malloc((str->alloc_nbytes=1024)+1);
		may_ptr_error(str->data);
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
			may_ptr_error(str->data);
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
	unshare_cell(&tmp2);
	free(str->data);
	str->data = NULL;
	return pl_success;
}

static USE_RESULT pl_status fn_bwrite_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,atom);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	const char *src = GET_STR(q, p1);
	size_t len = LEN_STR(q, p1);

	while (len && !g_tpl_interrupt) {
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

static USE_RESULT pl_status fn_read_term_from_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	int n = 3;
	stream *str = &q->pl->streams[n];
	char *src = NULL;
	size_t len;
	bool has_var, is_partial;

	if (is_atom(p_chars) && !is_string(p_chars)) {
		if (!strcmp(GET_STR(q, p_chars), "[]")) {
			cell tmp;
			make_literal(&tmp, g_eof_s);
			return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
		} else
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	} else if (is_string(p_chars)) {
		len = LEN_STR(q, p_chars);
		src = malloc(len+1+1);		// +1 is to allow adding a '.'
		may_ptr_error(src);
		memcpy(src, GET_STR(q, p_chars), len);
		src[len] = '\0';
	} else if (!check_list(q, p_chars, p_chars_ctx, &is_partial, NULL)) {
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "list");
	} else if ((len = scan_is_chars_list2(q, p_chars, p_chars_ctx, false, &has_var, &is_partial)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else {
		if (has_var)
			return throw_error(q, p_chars, p_chars_ctx, "instantiation_error", "variable");

		return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	}

	if (!str->p) {
		str->p = create_parser(q->st.m);
		str->p->flag = q->st.m->flag;
		str->p->fp = str->fp;
	} else
		reset(str->p);

	char *save_src = src;
	str->p->srcptr = src;
	src = eat_space(str->p);

	if (!src || !*src) {
		free(save_src);
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
	}

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	q->p->no_fp = true;
	pl_status ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	q->p->no_fp = false;
	free(save_src);
	destroy_parser(str->p);
	str->p = NULL;

	if (ok != pl_success)
		return pl_failure;

	return ok;
}

static USE_RESULT pl_status fn_read_term_from_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = LEN_STR(q, p_chars);
		src = malloc(len+1+1);	// final +1 is for look-ahead
		may_ptr_error(src);
		memcpy(src, GET_STR(q, p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	q->p->no_fp = true;
	pl_status ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	q->p->no_fp = false;
	free(src);
	return ok;
}

static USE_RESULT pl_status fn_write_term_to_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_cstring(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_write_canonical_to_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_cstring(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_write_term_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_write_canonical_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx_t vnames_ctx = 0;
	q->flag = q->st.m->flag;
	LIST_HANDLER(p2);

	while (is_list(p2) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	may_error(make_string(&tmp, dst), free(dst));
	free(dst);
	pl_status ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_sys_is_partial_string_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_iso_list(p1))
		return false;

	bool has_var, is_partial;
	scan_is_chars_list2(q, p1, p1_ctx, true, &has_var, &is_partial);
	return is_partial;
}

static USE_RESULT pl_status fn_is_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	bool is_partial;
	return check_list(q, p1, p1_ctx, &is_partial, NULL);
}

static USE_RESULT pl_status fn_is_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return false;

	return is_partial;
}

static USE_RESULT pl_status fn_is_list_or_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return true;

	return is_partial;
}

static USE_RESULT pl_status fn_sys_instantiated_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_variable(p1)) {
		char *buf = print_term_to_strbuf(q, p2, p2_ctx, 1);
		pl_status ok = throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");
		free(buf);
		return ok;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_sys_mustbe_list_or_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_variable(p1) || is_nil(p1))
		return pl_success;

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (!is_iso_list(p1) || !check_list(q, p1, p1_ctx, &is_partial, NULL) || is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	return pl_success;
}

static USE_RESULT pl_status fn_sys_skip_max_list_4(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_atomic(p3) && !is_string(p3)) {
		cell tmp;
		make_int(&tmp, 0);
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return unify(q, p3, p3_ctx, p4, p4_ctx);
	}

	pl_int_t skip=0, max = is_smallint(p2) ? get_smallint(p2) : PL_INT_MAX;
	pl_idx_t c_ctx = p3_ctx;
	cell tmp = {0};
	cell *c = skip_max_list(q, p3, &c_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);

	if (!c) {
		c_ctx = p3_ctx;
		c = p3;
	}

	pl_status ok = unify(q, p4, p4_ctx, c, c_ctx);

	if (ok != pl_success)
		return ok;

	if (!is_iso_list_or_nil(c) && !(is_cstring(c) && !strcmp(GET_STR(q,c), "[]")) && !is_variable(c)) {
		make_int(&tmp, -1);
		unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	make_int(&tmp, skip);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
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
		uint64_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

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

			DISCARD_RESULT start(task);
			task = task->next;
			did_something = true;
		}

		if (!did_something)
			msleep(1);
	}

	return pl_success;
}

static USE_RESULT pl_status fn_await_0(query *q)
{
	while (!g_tpl_interrupt && q->st.m->tasks) {
		pl_uint_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

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

			DISCARD_RESULT start(task);

			if (!task->tmo_msecs && task->yielded) {
				did_something = true;
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

	may_error(push_choice(q));
	return pl_success;
}

static USE_RESULT pl_status fn_yield_0(query *q)
{
	if (q->retry)
		return pl_success;

	return do_yield_0(q, 0);
}

static USE_RESULT pl_status fn_task_n(query *q)
{
	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false, false);
	unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);

	GET_FIRST_RAW_ARG0(p1,callable,p0);
	may_ptr_error(clone_to_tmp(q, p1));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		GET_NEXT_RAW_ARG(p2,any);
		may_ptr_error(clone2_to_tmp(q, p2));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn = get_builtin(q->pl, GET_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->flags |= FLAG_BUILTIN;
	}

	cell *tmp = clone_to_heap(q, false, tmp2, 0);
	query *task = create_sub_query(q, tmp);
	task->yielded = task->spawned = true;
	push_task(q->st.m, task);
	return pl_success;
}

static USE_RESULT pl_status fn_fork_0(query *q)
{
	cell *curr_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;
	query *task = create_sub_query(q, curr_cell);
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
		return throw_error(q, p1, p1_ctx, "resource_error", "cyclic_term");

	for (pl_idx_t i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	may_ptr_error(alloc_on_queuen(dstq, 0, c));
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

static USE_RESULT pl_status fn_date_time_6(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,variable);
	GET_NEXT_ARG(p3,variable);
	GET_NEXT_ARG(p4,variable);
	GET_NEXT_ARG(p5,variable);
	GET_NEXT_ARG(p6,variable);
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
	return pl_success;
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_shell_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int status = system(GET_STR(q, p1));
	if (status == 0)
		return pl_success;
	else
		return pl_failure;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_shell_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	int status = system(GET_STR(q, p1));
	cell tmp;
	make_int(&tmp, status);
	set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}
#endif

#ifndef SANDBOX
static char *fixup(const char *srcptr)
{
	char *tmpbuf = strdup(srcptr);
	const char *src = srcptr;
	char *dst = tmpbuf;

	while (*src) {
		if ((src[0] == '.') && (src[1] == '.') && (src[2] == '/')) {
			dst -= 2;

			while ((dst != tmpbuf) && (*dst != '/'))
				dst--;

			src += 2;
			dst++;
		} else if ((src[0] == '.') && (src[1] == '/')) {
			src += 1;
		} else
			*dst++ = *src;

		src++;
	}

	*dst = '\0';
	return tmpbuf;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_absolute_file_name_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p_opts,list_or_nil);
	bool expand = false;
	char *filename = NULL;
	char *here = strdup(q->st.m->filename);
	may_ptr_error(here);
	char *ptr = here + strlen(here) - 1;

	while (*ptr && (*ptr != '/')) {
		ptr--;
		*ptr = '\0';
	}

	char *cwd = here;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	LIST_HANDLER(p_opts);

	while (is_list(p_opts) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p_opts);
		h = deref(q, h, p_opts_ctx);

		if (is_structure(h) && (h->arity == 1)) {
			if (!CMP_SLICE2(q, h, "expand")) {
				if (is_literal(h+1)) {
					if (!CMP_SLICE2(q, h+1, "true"))
						expand = true;
				}
			} else if (!CMP_SLICE2(q, h, "relative_to")) {
				if (is_atom(h+1))
					cwd = DUP_SLICE(q, h+1);
			}
		}

		p_opts = LIST_TAIL(p_opts);
		p_opts = deref(q, p_opts, p_opts_ctx);
		p_opts_ctx = q->latest_ctx;
	}

	char *tmpbuf = NULL;
	const char *s = filename;

	if (expand && (*s == '$')) {
		char envbuf[PATH_MAX];
		char *dst = envbuf;
		s++;

		while (*s && (*s != '/') && ((dst-envbuf-1) != sizeof(envbuf)))
			*dst++ = *s++;

		if (*s == '/')
			s++;

		*dst = '\0';
		char *ptr = getenv(envbuf);
		if (!ptr)
			return throw_error(q, p1, p1_ctx, "existence_error", "environment_variable");

		size_t buflen = strlen(ptr)+1+strlen(s)+1;
		tmpbuf = malloc(buflen);
		may_ptr_error(tmpbuf);
		snprintf(tmpbuf, buflen, "%s/%s", ptr, s);
		char *tmpbuf2;

		if ((tmpbuf2 = realpath(tmpbuf, NULL)) == NULL) {
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
				tmpbuf = fixup(tmp);
				may_ptr_error(tmpbuf);
				free(tmp);
			} else {
				tmpbuf = fixup(s);
				may_ptr_error(tmpbuf);
			}
		}
	}

	free(filename);

	if (cwd != here)
		free(cwd);

	free(here);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, tmpbuf), free(tmpbuf));
	else
		may_error(make_cstring(&tmp, tmpbuf), free(tmpbuf));

	free(tmpbuf);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}
#endif

#ifndef SANDBOX
static pl_status do_consult(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_SLICE(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		//unload_file(q->st.m, filename);
		free(src);

		if (!load_file(q->st.m, filename, false)) {
			free(filename);
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
		}

		free(filename);
		return pl_success;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (CMP_SLICE2(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	module *tmp_m = create_module(q->pl, GET_STR(q, mod));
	char *filename = GET_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	unload_file(q->st.m, filename);

	if (!load_file(tmp_m, filename, false)) {
		destroy_module(tmp_m);
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
static pl_status do_deconsult(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_SLICE(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		unload_file(q->st.m, filename);
		free(src);
		free(filename);
		return pl_success;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	if (CMP_SLICE2(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	module *tmp_m = create_module(q->pl, GET_STR(q, mod));
	char *filename = GET_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	unload_file(q->st.m, filename);
	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_load_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);

	if (is_atom(p1)) {
		may_error(do_consult(q, p1, p1_ctx));
		return pl_success;
	}

	LIST_HANDLER(p1);

	while (is_list(p1) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		may_error(do_consult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_unload_files_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_structure);

	if (is_atom(p1)) {
		may_error(do_deconsult(q, p1, p1_ctx));
		return pl_success;
	}

	LIST_HANDLER(p1);

	while (is_list(p1) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		may_error(do_deconsult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return pl_success;
}
#endif

static USE_RESULT pl_status fn_format_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);

	if (is_nil(p1)) {
		if (is_nil(p2))
			return pl_success;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "list");
	}

	return do_format(q, NULL, 0, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

static USE_RESULT pl_status fn_format_3(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);

	if (is_nil(p1)) {
		if (is_nil(p2))
			return pl_success;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "list");
	}

	return do_format(q, pstr, pstr_ctx, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

#if USE_OPENSSL
static USE_RESULT pl_status fn_crypto_data_hash_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p3,list_or_nil);
	bool is_sha384 = false, is_sha512 = false;
	bool is_sha256 = true;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_structure(h) && (h->arity == 1)) {
			cell *arg = h+1;
			arg = deref(q, arg, h_ctx);
			pl_idx_t arg_ctx = q->latest_ctx;

			if (!CMP_SLICE2(q, h, "algorithm")) {
				if (is_variable(arg)) {
					cell tmp;
					make_literal(&tmp, index_from_pool(q->pl, "sha256"));
					set_var(q, arg, arg_ctx, &tmp, q->st.curr_frame);
					is_sha384 = is_sha512 = false;
					is_sha256 = true;
				} else if (!CMP_SLICE2(q, arg, "sha256")) {
					is_sha384 = is_sha512 = false;
					is_sha256 = true;
				} else if (!CMP_SLICE2(q, arg, "sha384")) {
					is_sha256 = is_sha512 = false;
					is_sha384 = true;
				} else if (!CMP_SLICE2(q, arg, "sha512")) {
					is_sha384 = is_sha256 = false;
					is_sha512 = true;
				} else
					return throw_error(q, arg, arg_ctx, "domain_error", "algorithm");
			} else
				return throw_error(q, h, h_ctx, "domain_error", "hash_option");
		} else
			return throw_error(q, h, h_ctx, "domain_error", "hash_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char tmpbuf[512];
	char *dst = tmpbuf;
	*dst = '\0';
	size_t buflen = sizeof(tmpbuf);

	if (is_sha256) {
		unsigned char digest[SHA256_DIGEST_LENGTH];
		SHA256((unsigned char*)GET_STR(q, p1), LEN_STR(q, p1), digest);

		for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (is_sha384) {
		unsigned char digest[SHA384_DIGEST_LENGTH];
		SHA384((unsigned char*)GET_STR(q, p1), LEN_STR(q, p1), digest);

		for (int i = 0; i < SHA384_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (is_sha512) {
		unsigned char digest[SHA512_DIGEST_LENGTH];
		SHA512((unsigned char*)GET_STR(q, p1), LEN_STR(q, p1), digest);

		for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	}

	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}
#endif

static int do_b64encode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(q, p1);
	size_t len = LEN_STR(q, p1);
	char *dstbuf = malloc((len*3)+1);	// BASE64 can increase length x3
	ensure(dstbuf);
	b64_encode(str, len, &dstbuf, 0, 0);
	cell tmp;
	may_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static int do_b64decode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(q, p2);
	size_t len = LEN_STR(q, p2);
	char *dstbuf = malloc(len+1);
	ensure(dstbuf);
	b64_decode(str, len, &dstbuf);
	cell tmp;
	may_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_base64_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_list(p1)) && is_variable(p2))
		return do_b64encode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_b64decode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
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

static pl_status do_urlencode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,variable);
	const char *str = GET_STR(q, p1);
	size_t len = LEN_STR(q, p1);
	char *dstbuf = malloc((len*3)+1);	// URL's can increase length x3
	may_ptr_error(dstbuf);
	url_encode(str, len, dstbuf);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		may_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static pl_status do_urldecode_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,atom);
	const char *str = GET_STR(q, p2);
	size_t len = LEN_STR(q, p2);
	char *dstbuf = malloc(len+1);
	may_ptr_error(dstbuf);
	url_decode(str, dstbuf);
	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		may_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_urlenc_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_string(p1)) && is_variable(p2))
		return do_urlencode_2(q);
	else if (is_variable(p1) && (is_atom(p2) || is_string(p2)))
		return do_urldecode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
}

static USE_RESULT pl_status fn_atom_lower_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,iso_atom_or_var);
	const char *src = GET_STR(q, p1);
	size_t len = substrlen_utf8(src, LEN_STR(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	may_ptr_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towlower(ch);
		dst += put_char_bare_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	may_error(make_cstringn(&tmp, tmps, LEN_STR(q, p1)), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_atom_upper_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,iso_atom_or_var);
	const char *src = GET_STR(q, p1);
	size_t len = substrlen_utf8(src, LEN_STR(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	may_ptr_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towupper(ch);
		dst += put_char_bare_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	may_error(make_cstringn(&tmp, tmps, LEN_STR(q, p1)), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}


static USE_RESULT pl_status fn_string_lower_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *src = GET_STR(q, p1);
	size_t len = substrlen_utf8(src, LEN_STR(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	may_ptr_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towlower(ch);
		dst += put_char_bare_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	may_error(make_stringn(&tmp, tmps, LEN_STR(q, p1)), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_string_upper_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *src = GET_STR(q, p1);
	size_t len = substrlen_utf8(src, LEN_STR(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	may_ptr_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towupper(ch);
		dst += put_char_bare_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	may_error(make_stringn(&tmp, tmps, LEN_STR(q, p1)), free(tmps));
	free(tmps);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_access_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	int amode = R_OK;

	if (!CMP_SLICE2(q, p2, "read"))
		amode = R_OK;
	else if (!CMP_SLICE2(q, p2, "write"))
		amode = W_OK;
	else if (!CMP_SLICE2(q, p2, "append"))
		amode = W_OK;
	else if (!CMP_SLICE2(q, p2, "execute"))
		amode = X_OK;
	else if (!CMP_SLICE2(q, p2, "none")) {
		free(filename);
		return pl_success;
	} else {
		free(filename);
		return throw_error(q, p2, p2_ctx, "domain_error", "mode");
	}

	struct stat st = {0};
	int status = stat(filename, &st);

	if (status && (!CMP_SLICE2(q, p2, "read") || !CMP_SLICE2(q, p2, "exist") || !CMP_SLICE2(q, p2, "execute") || !CMP_SLICE2(q, p2, "none"))) {
		free(filename);
		return pl_failure;
	}

	if (status && (!CMP_SLICE2(q, p2, "write") || !CMP_SLICE2(q, p2, "append"))) {
		free(filename);
		return pl_success;
	}

	int ok = !access(filename, amode);
	free(filename);
	return ok;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_exists_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return pl_failure;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return pl_failure;

	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_directory_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
	}

	DIR *dirp = opendir(filename);

	if (!dirp) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
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
	free(filename);
	cell *l = end_list(q);
	pl_status ok = unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return ok;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_delete_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	remove(filename);
	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_rename_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_SLICE(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_SLICE(q, p2);

	struct stat st = {0};

	if (stat(filename1, &st)) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	bool ok = !rename(filename1, filename2);
	free(filename1);
	free(filename2);
	return ok ? pl_success : pl_failure;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_copy_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_SLICE(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_SLICE(q, p2);

	FILE *fp1 = fopen(filename1, "rb");

	if (!fp1) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename1);
	FILE *fp2 = fopen(filename2, "wb");

	if (!fp2) {
		fclose(fp1);
		free(filename2);
		return throw_error(q, p2, p2_ctx, "permission_error", "file");
	}

	free(filename2);
	char buffer[1024];
	size_t n;

	while ((n = fread(buffer, 1, sizeof(buffer), fp1)) > 0) {
		if (fwrite(buffer, 1, n, fp2) != n) {
			fclose(fp2);
			fclose(fp1);
			return throw_error(q, p2, p2_ctx, "system_error", "file");
		}
	}

	fclose(fp2);

	if (!feof(fp1)) {
		fclose(fp1);
		return throw_error(q, p1, p1_ctx, "system_error", "file");
	}

	fclose(fp1);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_time_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,variable);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_real(&tmp, st.st_mtime);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_size_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,integer_or_var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_int(&tmp, st.st_size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_exists_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return pl_failure;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return pl_failure;

	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_make_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	if (!stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	if (mkdir(filename, 0777))
		return throw_error(q, p1, p1_ctx, "permission_error", "file");

	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_make_directory_path_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	struct stat st = {0};

	for (char *ptr = filename+1; *ptr; ptr++) {
		if (*ptr == PATH_SEP_CHAR) {
			*ptr = '\0';

			if (stat(filename, &st)) {
				if (mkdir(filename, 0777)) {
					free(filename);
					return throw_error(q, p1, p1_ctx, "permission_error", "directory");
				}
			}

			*ptr = PATH_SEP_CHAR;
		}
	}

	if (!stat(filename, &st)) {
		free(filename);
		return pl_success;
	}

	if (mkdir(filename, 0777)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "permission_error", "directory");
	}

	free(filename);
	return pl_success;
}
#endif

#ifndef SANDBOX
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
		char *filename;

		if (is_iso_list(p_new)) {
			size_t len = scan_is_chars_list(q, p_new, p_new_ctx, true);

			if (!len) {
				unshare_cell(&tmp);
				return throw_error(q, p_new, p_new_ctx, "type_error", "atom");
			}

			filename = chars_list_to_string(q, p_new, p_new_ctx, len);
		} else
			filename = DUP_SLICE(q, p_new);

		if (chdir(filename)) {
			unshare_cell(&tmp);
			return throw_error(q, p_new, p_new_ctx, "existence_error", "path");
		}

		free(filename);
	}

	pl_status ok = unify(q, p_old, p_old_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_chdir_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);
		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_SLICE(q, p1);

	pl_status ok = !chdir(filename);
	free(filename);
	return ok;
}
#endif

static USE_RESULT pl_status fn_edin_redo_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
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

		if (ch == get_int(p1))
			break;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_edin_redo_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
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

		if (ch == get_int(p1))
			break;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_edin_tab_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_int(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_edin_tab_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_int(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_edin_seen_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

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
	q->pl->current_input = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_edin_told_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

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
	q->pl->current_output = 0;
	return pl_success;
}

static USE_RESULT pl_status fn_edin_seeing_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name = q->pl->current_input==0?"user":q->pl->streams[q->pl->current_input].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return pl_success;
}

static USE_RESULT pl_status fn_edin_telling_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	char *name =q->pl->current_output==1?"user":q->pl->streams[q->pl->current_output].name;
	cell tmp;
	may_error(make_cstring(&tmp, name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return pl_success;
}

static pl_idx_t jenkins_one_at_a_time_hash(const char *key, size_t len)
{
	pl_idx_t hash = 0;

	while (len-- > 0) {
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

	cell tmp;

	if (is_smallint(p1)) {
		char tmpbuf[256];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long)get_smallint(p1));
		make_int(&tmp, jenkins_one_at_a_time_hash(tmpbuf, strlen(tmpbuf)));
	} else if (is_atom(p1)) {
		make_int(&tmp, jenkins_one_at_a_time_hash(GET_STR(q, p1), LEN_STR(q, p1)));
	} else {
		char *tmpbuf = print_term_to_strbuf(q, p1, p1_ctx, 1);
		make_int(&tmp, jenkins_one_at_a_time_hash(tmpbuf, strlen(tmpbuf)));
		free(tmpbuf);
	}

	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_hex_chars_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");

	if (is_variable(p2)) {
		char tmpbuf[256];
		char *dst = tmpbuf;

		if (is_bigint(p1)) {
			size_t len = mp_int_string_len(&p1->val_bigint->ival, 16) -1;
			dst = malloc(len+10);
			mp_int_to_string(&p1->val_bigint->ival, 16, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%llx", (long long)get_int(p1));
		}

		cell tmp;
		may_error(make_string(&tmp, dst));
		if (is_bigint(p1)) free(dst);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	char *src = DUP_SLICE(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;

	if (!q->p)
		q->p = create_parser(q->st.m);

	read_integer(q->p, &v2, 16, s, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INT;
		tmp.val_bigint = malloc(sizeof(bigint));
		tmp.val_bigint->refcnt = 1;
		mp_int_init_copy(&tmp.val_bigint->ival, &v2);
		tmp.flags |= FLAG_MANAGED;
	} else {
		make_int(&tmp, val);
	}

	mp_int_clear(&v2);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_octal_chars_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_variable(p1) && is_variable(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");

	if (is_variable(p2)) {
		char tmpbuf[256];
		char *dst = tmpbuf;

		if (is_bigint(p1)) {
			size_t len = mp_int_string_len(&p1->val_bigint->ival, 8) -1;
			dst = malloc(len+10);
			mp_int_to_string(&p1->val_bigint->ival, 8, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%llo", (long long)get_int(p1));
		}

		cell tmp;
		may_error(make_string(&tmp, dst));
		if (is_bigint(p1)) free(dst);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return pl_success;
	}

	char *src = DUP_SLICE(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;

	if (!q->p)
		q->p = create_parser(q->st.m);

	read_integer(q->p, &v2, 16, s, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INT;
		tmp.val_bigint = malloc(sizeof(bigint));
		tmp.val_bigint->refcnt = 1;
		mp_int_init_copy(&tmp.val_bigint->ival, &v2);
		tmp.flags |= FLAG_MANAGED;
	} else {
		make_int(&tmp, val);
	}

	mp_int_clear(&v2);
	pl_status ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
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
	const char *value = getenv(GET_STR(q, p1));

	if (!value)
		return pl_failure;

	cell tmp;

	if (is_string(p1))
		may_error(make_string(&tmp, (char*)value));
	else
		may_error(make_cstring(&tmp, (char*)value));

	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_int);

	if (is_atom(p2)) {
		setenv(GET_STR(q, p1), GET_STR(q, p2), 1);
	} else if (is_integer(p2)) {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), get_int(p2), 10);
		setenv(GET_STR(q, p1), tmpbuf, 1);
	} else
		return pl_failure;

	return pl_success;
}

static USE_RESULT pl_status fn_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(GET_STR(q, p1));
	return pl_success;
}

static USE_RESULT pl_status fn_uuid_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	uuid u;
	uuid_gen(q->pl, &u);
	char tmpbuf[128];
	uuid_to_buf(&u, tmpbuf, sizeof(tmpbuf));
	cell tmp;
	may_error(make_string(&tmp, tmpbuf));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return pl_success;
}

static USE_RESULT pl_status fn_atomic_concat_3(query *q)
{
	GET_FIRST_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,any);

	const char *src1, *src2;
	size_t len1, len2;
	char tmpbuf1[256], tmpbuf2[256];

	len1 = print_term_to_buf(q, tmpbuf1, sizeof(tmpbuf1), p1, p1_ctx, 1, false, 0);
	src1 = tmpbuf1;

	len2 = print_term_to_buf(q, tmpbuf2, sizeof(tmpbuf2), p2, p2_ctx, 1, false, 0);
	src2 = tmpbuf2;

	ASTRING_alloc(pr, len1+len2);
	ASTRING_strcatn(pr, src1, len1);
	ASTRING_strcatn(pr, src2, len2);
	cell tmp;
	may_error(make_cstringn(&tmp, ASTRING_cstr(pr), ASTRING_strlen(pr)), ASTRING_free(pr));
	ASTRING_free(pr);
	pl_status ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_atomic_list_concat_3(query *q)
{
	GET_FIRST_ARG(p1,iso_list_or_nil);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,atomic_or_var);
	LIST_HANDLER(p1);
	ASTRING(pr);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (is_variable(h))
			return throw_error(q, h, q->latest_ctx, "instantiation_error", "atomic");

		if (!is_atomic(h))
			return throw_error(q, h, q->latest_ctx, "type_error", "atomic");

		char *dst = print_term_to_strbuf(q, h, q->latest_ctx, 1);
		ASTRING_strcat(pr, dst);
		free(dst);

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (is_list(p1)) {
			dst = print_term_to_strbuf(q, p2, p2_ctx, 1);
			ASTRING_strcat(pr, dst);
			free(dst);
		}
	}

	if (is_variable(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atomic_list_concat/3");

	cell tmp;
	may_error(make_cstringn(&tmp, ASTRING_cstr(pr), ASTRING_strlen(pr)), ASTRING_free(pr));
	ASTRING_free(pr);
	pl_status ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_replace_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,variable);

	size_t srclen = LEN_STR(q, p1);
	size_t dstlen = srclen * LEN_STR(q, p3);
	const char *src = GET_STR(q, p1);
	const char *s1 = GET_STR(q, p2);
	const char *s2 = GET_STR(q, p3);
	size_t s1len = LEN_STR(q, p2);
	size_t s2len = LEN_STR(q, p3);
	ASTRING_alloc(pr, dstlen);

	while (srclen > 0) {
		if (!strncmp(src, s1, s1len)) {
			ASTRING_strcatn(pr, s2, s2len);
			src += s1len;
			srclen -= s1len;
		} else {
			ASTRING_strcatn(pr, src, 1);
			src++;
			srclen--;
		}
	}

	cell tmp;

	if (ASTRING_strlen(pr))
		may_error(make_stringn(&tmp, ASTRING_cstr(pr), ASTRING_strlen(pr)), ASTRING_free(pr));
	else
		make_literal(&tmp, g_nil_s);

	ASTRING_free(pr);
	set_var(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return pl_success;
}

static void load_properties(module *m);

static USE_RESULT pl_status fn_sys_load_properties_0(query *q)
{
	load_properties(q->st.m);
	return pl_success;
}

static void load_flags(query *q);

static USE_RESULT pl_status fn_sys_load_flags_0(query *q)
{
	load_flags(q);
	return pl_success;
}

static void load_ops(query *q);

static USE_RESULT pl_status fn_sys_load_ops_0(query *q)
{
	load_ops(q);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_legacy_predicate_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var);
	cell tmp;
	bool found = false;

	if (get_builtin(q->pl, GET_STR(q, p1), p1->arity, &found, NULL), found) {
		make_literal(&tmp, index_from_pool(q->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "predicate_property");
	}

	predicate *pr = find_predicate(q->st.m, p1);

	if (pr && !pr->is_dynamic && !is_variable(p2)) {
		make_literal(&tmp, index_from_pool(q->pl, "built_in"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && pr->is_multifile) {
		make_literal(&tmp, index_from_pool(q->pl, "multifile"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && pr->is_dynamic) {
		make_literal(&tmp, index_from_pool(q->pl, "dynamic"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && !pr->is_dynamic) {
		make_literal(&tmp, index_from_pool(q->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && pr->is_persist) {
		make_literal(&tmp, index_from_pool(q->pl, "persist"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && pr->is_public) {
		make_literal(&tmp, index_from_pool(q->pl, "public"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr && pr->is_public) {
		make_literal(&tmp, index_from_pool(q->pl, "exported"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr) {
		make_literal(&tmp, index_from_pool(q->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr) {
		make_literal(&tmp, index_from_pool(q->pl, "meta_predicate"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	if (pr) {
		make_literal(&tmp, index_from_pool(q->pl, "visible"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return pl_success;
	}

	return pl_failure;
}

static unsigned do_numbervars(query *q, cell *p1, pl_idx_t p1_ctx, int *end, int depth)
{
	unsigned cnt = 0;

	if (is_variable(p1)) {
		cell *tmp = alloc_on_heap(q, 2);
		make_structure(tmp+0, g_sys_var_s, NULL, 1, 1);
		make_int(tmp+1, *end); *end = *end + 1;
		tmp->flags |= FLAG_CSTR_QUOTED;
		set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
		cnt++;
		return cnt;
	}

	if (!is_structure(p1))
		return cnt;

	if (!depth)
		q->pl->tab_idx = 0;

	unsigned arity = p1->arity;
	p1++;

	for (; arity--; p1 += p1->nbr_cells) {

		if (is_variable(p1)) {
			bool found = false;

			for (unsigned idx = 0; idx < q->pl->tab_idx; idx++) {
				if ((q->pl->tab1[idx] == p1_ctx) && (q->pl->tab2[idx] == p1->var_nbr)) {
					found = true;
					break;
				}
			}

			if (found)
				continue;

			q->pl->tab1[q->pl->tab_idx] = p1_ctx;
			q->pl->tab2[q->pl->tab_idx] = p1->var_nbr;
			q->pl->tab_idx++;
		}

		cell *c = deref(q, p1, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (is_variable(c)) {
			cell *tmp = alloc_on_heap(q, 2);
			make_structure(tmp+0, g_sys_var_s, NULL, 1, 1);
			make_int(tmp+1, *end); *end = *end + 1;
			tmp->flags |= FLAG_CSTR_QUOTED;
			set_var(q, c, c_ctx, tmp, q->st.curr_frame);
			cnt++;
		} else if (is_structure(c))
			cnt += do_numbervars(q, c, c_ctx, end, depth+1);
	}

	return cnt;
}

static USE_RESULT pl_status fn_numbervars_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int end = 0;
	q->numbervars = true;
	do_numbervars(q, p1, p1_ctx, &end, 0);
	return pl_success;
}

static USE_RESULT pl_status fn_numbervars_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);
	int end = q->nv_start = get_int(p2);
	q->numbervars = true;
	unsigned cnt = do_numbervars(q, p1, p1_ctx, &end, 0);
	cell tmp;
	make_int(&tmp, get_int(p2)+cnt);
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
		if (strlen_utf8(GET_STR(q, p1)) != 1)
			return pl_failure;

		ch = peek_char_utf8(GET_STR(q, p1));
	} else
		ch = get_int(p1);

	if (!CMP_SLICE2(q, p2, "alpha"))
		return iswalpha(ch);
	else if (!CMP_SLICE2(q, p2, "digit"))
		return iswdigit(ch);
	else if (!CMP_SLICE2(q, p2, "xdigit"))
		return iswxdigit(ch);
	else if (!CMP_SLICE2(q, p2, "whitespace"))
		return iswblank(ch) || iswspace(ch);
	else if (!CMP_SLICE2(q, p2, "white"))
		return iswblank(ch);
	else if (!CMP_SLICE2(q, p2, "space"))
		return iswspace(ch);
	else if (!CMP_SLICE2(q, p2, "lower"))
		return iswlower(ch);
	else if (!CMP_SLICE2(q, p2, "upper"))
		return iswupper(ch);
	else if (!CMP_SLICE2(q, p2, "punct"))
		return iswpunct(ch);
	else if (!CMP_SLICE2(q, p2, "cntrl"))
		return iswcntrl(ch);
	else if (!CMP_SLICE2(q, p2, "graph"))
		return iswgraph(ch);
	else if (!CMP_SLICE2(q, p2, "ascii"))
		return ch < 128;
	else if (!CMP_SLICE2(q, p2, "newline"))
		return ch == 10;
	else if (!CMP_SLICE2(q, p2, "end_of_line"))
		return (ch >= 10) && (ch <= 13);
	else if (!CMP_SLICE2(q, p2, "end_of_file"))
		return ch == -1;
	else if (!CMP_SLICE2(q, p2, "quote"))
		return (ch == '\'') || (ch == '"') || (ch == '`');
	else if (!CMP_SLICE2(q, p2, "period"))
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
		tokenize(p, false, false);
		xref_rule(p->m, p->cl, NULL);
		execute(q, p->cl->cells, p->cl->nbr_cells);
		clear_rule(p->cl);
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

#ifndef SANDBOX
static USE_RESULT pl_status fn_sys_db_load_0(query *q)
{
	do_db_load(q->st.m);
	return pl_success;
}
#endif

#ifndef SANDBOX
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
#endif

static USE_RESULT pl_status fn_abolish_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);
	cell tmp = *p1;
	tmp.arity = get_int(p2);
	CLR_OP(&tmp);
	return do_abolish(q, &tmp, &tmp, true);
}

static USE_RESULT pl_status fn_sys_lt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	pl_int_t num = get_int(p1);

	if (num < get_int(p2)) {
		set_smallint(p1, num+1);
		return pl_success;
	}

	drop_choice(q);
	trim_trail(q);
	return pl_success;
}

static USE_RESULT pl_status fn_limit_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p2, 4);
	pl_idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_lt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_int(p1));
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_gt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	pl_int_t num = get_int(p1);

	if (num <= get_int(p2)) {
		set_smallint(p1, num+1);
		return pl_failure;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_offset_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p2, 4);
	pl_idx_t nbr_cells = 1 + p2->nbr_cells;
	make_structure(tmp+nbr_cells++, g_fail_s, fn_sys_gt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_int(p1));
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_ne_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	pl_int_t num = get_int(p1);

	if (num != get_int(p2)) {
		set_smallint(p1, num+1);
		return pl_failure;
	}

	drop_choice(q);
	trim_trail(q);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_incr_2(query *q)
{
	GET_FIRST_ARG(p1, variable);
	GET_NEXT_ARG(p2, integer);
	int64_t n = get_smallint(p2);
	set_smallint(p2, n+1);
	set_var(q, p1, p1_ctx, p2, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_call_nth_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_integer(p2) && is_zero(p2))
		return pl_failure;

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_variable(p2)) {
		cell *tmp = clone_to_heap(q, true, p1, 4);
		pl_idx_t nbr_cells = 1 + p1->nbr_cells;
		make_structure(tmp+nbr_cells++, g_sys_incr_s, fn_sys_incr_2, 2, 2);
		GET_RAW_ARG(2,p2_raw);
		tmp[nbr_cells] = *p2_raw;
		tmp[nbr_cells++].nbr_cells = 1;
		make_int(tmp+nbr_cells++, 0);
		make_return(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	cell *tmp = clone_to_heap(q, true, p1, 4);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_ne_s, fn_sys_ne_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_int(p2));
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_unifiable_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	may_error(push_choice(q));
	pl_idx_t save_tp = q->st.tp;
	bool save_hook = q->in_hook;
	q->in_hook = true;

	if (!unify(q, p1, p1_ctx, p2, p2_ctx) && !q->cycle_error) {
		q->in_hook = save_hook;
		undo_me(q);
		drop_choice(q);
		return pl_failure;
	}

	q->in_hook = save_hook;
	bool first = true;

	// Go thru trail, getting the bindings...

	while (save_tp < q->st.tp) {
		const trail *tr = q->trails + save_tp;
		const frame *f = GET_FRAME(tr->ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		cell *c = deref(q, &e->c, e->ctx);

		if (is_indirect(c))
			c = c->val_ptr;

		cell *tmp = malloc(sizeof(cell)*(2+c->nbr_cells));
		may_ptr_error(tmp);
		make_structure(tmp, g_unify_s, fn_iso_unify_2, 2, 1+c->nbr_cells);
		SET_OP(tmp, OP_XFX);
		cell v;
		make_variable(&v, g_anon_s, tr->var_nbr);
		tmp[1] = v;
		safe_copy_cells(tmp+2, c, c->nbr_cells);

		if (first) {
			allocate_list(q, tmp);
			first = false;
		} else
			append_list(q, tmp);

		free(tmp);
		save_tp++;
	}

	undo_me(q);
	drop_choice(q);

	cell *l = end_list(q);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_erase_attributes_1(query *q)
{
	GET_FIRST_ARG(p1,variable);

	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	e->c.attrs = NULL;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_write_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,list_or_nil);

	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	e->c.attrs = p2;
	e->c.attrs_ctx = p2_ctx;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_read_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,variable);
	GET_NEXT_ARG(p2,variable);

	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);

	if (!e->c.attrs) {
		cell tmp;
		make_literal(&tmp, g_nil_s);
		set_var(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	set_var(q, p2, p2_ctx, e->c.attrs, e->c.attrs_ctx);
	return pl_success;
}

static USE_RESULT pl_status fn_get_unbuffered_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_get_unbuffered_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_integer(p1) && (get_int(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	if (ch == -1) {
		cell tmp;
		make_literal(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_sys_put_chars_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_cstring(p1)) {
		const char *src = GET_STR(q, p1);
		size_t len = LEN_STR(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_sys_put_chars_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	size_t len;

	if (is_cstring(p1)) {
		const char *src = GET_STR(q, p1);
		size_t len = LEN_STR(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	return !ferror(str->fp);
}

static USE_RESULT pl_status fn_kv_set_3(query *q)
{
	GET_FIRST_ARG(p1,smallint_or_atom);
	GET_NEXT_ARG(p2,smallint_or_atom);
	GET_NEXT_ARG(p3,list_or_nil);
	bool do_create = false;
	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		if (is_variable(h))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

		if (is_structure(h) && (h->arity == 1)) {
			cell *n = h + 0;
			if (!CMP_SLICE2(q, n, "create")) {
				cell *v = n + 1;
				v = deref(q, v, q->latest_ctx);

				if (is_variable(v))
					return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

				if (is_atom(v) && !CMP_SLICE2(q, v, "true"))
					do_create = true;
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char *key;

	if (is_integer(p1)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p1));
		key = strdup(tmpbuf);
	} else if (is_atom(p1))
		key = DUP_SLICE(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	may_ptr_error(key);

	if (do_create) {
		if (m_get(q->pl->keyval, key, NULL)) {
			free(key);
			return pl_failure;
		}
	}

	char *val;

	if (is_integer(p2)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p2));
		val = strdup(tmpbuf);
	} else if (is_atom(p2))
		val = DUP_SLICE(q, p2);
	else {
		free(key);
		return throw_error(q, p2, p2_ctx, "type_error", "integer");
	}

	may_ptr_error(val);
	m_set(q->pl->keyval, key, val);
	return pl_success;
}

static USE_RESULT pl_status fn_kv_get_3(query *q)
{
	GET_FIRST_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic_or_var);
	GET_NEXT_ARG(p3,list_or_nil);
	bool do_delete = false;
	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		if (is_variable(h))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

		if (is_structure(h) && (h->arity == 1)) {
			cell *n = h + 0;
			if (!CMP_SLICE2(q, n, "delete")) {
				cell *v = n + 1;
				v = deref(q, v, q->latest_ctx);

				if (is_variable(v))
					return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

				if (is_atom(v) && !CMP_SLICE2(q, v, "true"))
					do_delete = true;
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char *key;
	char tmpbuf[128];

	if (is_integer(p1)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_int(p1));
		key = tmpbuf;
	} else if (is_atom(p1))
		key = DUP_SLICE(q, p1);
	else
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	may_ptr_error(key);
	char *val = NULL;

	if (!m_get(q->pl->keyval, key, (void*)&val)) {
		if (key != tmpbuf) free(key);
		return pl_failure;
	}

	cell tmp;
	const char *src = val;
	int all_digs = 1;

	while (*src) {
		if (!isdigit(*src)) {
			all_digs = 0;
			break;
		}

		src++;
	}

	if (all_digs) {
		pl_int_t v = strtoll(val, NULL, 10);
		make_int(&tmp, v);
	} else
		may_error(make_cstring(&tmp, val));

	if (do_delete)
		m_del(q->pl->keyval, key);

	if (key != tmpbuf) free(key);
	pl_status ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static USE_RESULT pl_status fn_current_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (!q->retry) {
		if (is_atom(p1)) {
			const char *name = GET_STR(q, p1);
			return find_module(q->pl, name) ? pl_success : pl_failure;
		}

		may_error(push_choice(q));
		module *m = q->current_m = q->pl->modules;
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, m->name));
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	if (!q->current_m)
		return pl_failure;

	module *m = q->current_m = q->current_m->next;

	if (!m)
		return pl_failure;

	may_error(push_choice(q));
	cell tmp;
	make_literal(&tmp, index_from_pool(q->pl, m->name));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

#ifndef SANDBOX
static USE_RESULT pl_status fn_use_module_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	if (!is_atom(p1) && !is_structure(p1)) return pl_error;
	const char *name = GET_STR(q, p1);
	char dstbuf[1024*4];

	if (is_structure(p1) && !strcmp(name, "library")) {
		p1 = p1 + 1;
		if (!is_literal(p1)) return pl_error;
		name = GET_STR(q, p1);
		module *m;

		if ((m = find_module(q->pl, name)) != NULL) {
			if (!m->fp)
				do_db_load(m);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return pl_success;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
		    || !strcmp(name, "files"))
			return pl_success;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			may_ptr_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			ASTRING(s1);
			ASTRING_sprintf(s1, "library/%s", lib->name);
			m = load_text(q->st.m, src, ASTRING_cstr(s1));
			ASTRING_free(s1);
			free(src);

			if (m != q->st.m)
				do_db_load(m);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return pl_success;
		}

		snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
		char *dst = dstbuf + strlen(dstbuf);
		pl_idx_t ctx = 0;
		print_term_to_buf(q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, 0, 0);
		name = dstbuf;
	}

	if (true) {
		module *m;

		if ((m = find_module(q->pl, name)) != NULL) {
			if (!m->fp)
				do_db_load(m);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return pl_success;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
		    || !strcmp(name, "files"))
			return pl_success;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			may_ptr_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			ASTRING(s1);
			ASTRING_sprintf(s1, "library/%s", lib->name);
			m = load_text(q->st.m, src, ASTRING_cstr(s1));
			ASTRING_free(s1);
			free(src);

			if (m != q->st.m)
				do_db_load(m);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return pl_success;
		}
	}

	char *filename = relative_to(q->st.m->filename, name);
	module *m;

	if (!(m = load_file(q->st.m, filename, false))) {
		fprintf(stdout, "Error: module file not found: %s\n", filename);
		free(filename);
		return pl_failure;
	}

	free(filename);

	if (m != q->st.m)
		q->st.m->used[q->st.m->idx_used++] = m;

	return pl_success;
}
#endif

#ifndef SANDBOX
static USE_RESULT pl_status fn_use_module_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	return fn_use_module_1(q);
}
#endif

static USE_RESULT pl_status fn_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (is_variable(p1)) {
		cell tmp;
		make_literal(&tmp, index_from_pool(q->pl, (q->save_m?q->save_m:q->st.m)->name));
		q->save_m = NULL;
		set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	const char *name = GET_STR(q, p1);
	module *m = find_module(q->pl, name);

	if (!m) {
		if (q->p->command)
			fprintf(stdout, "Info: created module '%s'\n", name);

		m = create_module(q->pl, name);
	}

	q->st.m = m;
	return pl_success;
}

static USE_RESULT pl_status fn_using_0(query *q)
{
	module *m = q->st.m;
	fprintf(stdout, "%% %s --> [", m->name);

	for (unsigned i = 0; i < m->idx_used; i++) {
		if (i) fprintf(stdout, "%s", ",");
		fprintf(stdout, "%s", m->used[i]->name);
	}

	fprintf(stdout, "].\n");
	return pl_success;
}

static USE_RESULT pl_status fn_sys_register_term_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	may_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_term = true;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_alarm_1(query *q)
{
	GET_FIRST_ARG(p1,number);
	int time0 = 0;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	if (is_real(p1))
		time0 = get_real(p1) * 1000;
	else
		time0 = get_smallint(p1);

	if (time0 < 0)
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	struct itimerval it = {0};

	if (time0 == 0) {
		setitimer(ITIMER_REAL, &it, NULL);
		return pl_success;
	}

	int ms = time0;
	int secs = ms / 1000;
	ms -= secs * 1000;

	it.it_value.tv_sec = secs;
	it.it_value.tv_usec = ms * 1000;
	setitimer(ITIMER_REAL, &it, NULL);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_register_cleanup_1(query *q)
{
	if (q->retry) {
		GET_FIRST_ARG(p1,callable);
		cell *tmp = clone_to_heap(q, true, p1, 3);
		pl_idx_t nbr_cells = 1 + p1->nbr_cells;
		make_structure(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
		make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	may_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_cleanup = true;
	return pl_success;
}

static USE_RESULT pl_status fn_sys_get_level_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, q->cp);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_sys_choice_0(query *q)
{
	if (q->retry)
		return pl_failure;

	may_error(push_choice(q));
	return pl_success;
}

static USE_RESULT pl_status fn_iso_compare_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_atom(p1)) {
		if (CMP_SLICE2(q, p1, "<")
			&& CMP_SLICE2(q, p1, ">")
			&& CMP_SLICE2(q, p1, "="))
			return throw_error(q, p1, p1_ctx, "domain_error", "order");
	}

	int status = compare(q, p2, p2_ctx, p3, p3_ctx);
	cell tmp;

	make_literal(&tmp,
		     (status == ERR_CYCLE_CMP || status == 0)?
		     g_eq_s:status<0?g_lt_s:g_gt_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static const struct builtins g_predicates_iso[] =
{
	{",", 2, NULL, NULL, false},

	{"!", 0, fn_iso_cut_0, NULL, false},
	{":", 2, fn_iso_invoke_2, NULL, false},
	{"=..", 2, fn_iso_univ_2, NULL, false},
	{"->", 2, fn_iso_if_then_2, NULL, false},
	{";", 2, fn_iso_disjunction_2, NULL, false},
	{"\\+", 1, fn_iso_negation_1, NULL, false},
	{"$throw", 1, fn_iso_throw_1, NULL, false},
	{"$catch", 3, fn_iso_catch_3, NULL, false},
	{"$call_cleanup", 3, fn_sys_call_cleanup_3, NULL, false},

	{"call", 1, fn_iso_call_n, NULL, false},
	{"call", 2, fn_iso_call_n, NULL, false},
	{"call", 3, fn_iso_call_n, NULL, false},
	{"call", 4, fn_iso_call_n, NULL, false},
	{"call", 5, fn_iso_call_n, NULL, false},
	{"call", 6, fn_iso_call_n, NULL, false},
	{"call", 7, fn_iso_call_n, NULL, false},
	{"call", 8, fn_iso_call_n, NULL, false},

	{"repeat", 0, fn_iso_repeat_0, NULL, false},
	{"true", 0, fn_iso_true_0, NULL, false},
	{"fail", 0, fn_iso_fail_0, NULL, false},
	{"false", 0, fn_iso_fail_0, NULL, false},
	{"atom", 1, fn_iso_atom_1, NULL, false},
	{"atomic", 1, fn_iso_atomic_1, NULL, false},
	{"number", 1, fn_iso_number_1, NULL, false},
	{"compound", 1, fn_iso_compound_1, NULL, false},
	{"var", 1, fn_iso_var_1, NULL, false},
	{"nonvar", 1, fn_iso_nonvar_1, NULL, false},
	{"ground", 1, fn_iso_ground_1, NULL, false},
	{"callable", 1, fn_iso_callable_1, NULL, false},
	{"char_code", 2, fn_iso_char_code_2, NULL, false},
	{"atom_chars", 2, fn_iso_atom_chars_2, NULL, false},
	{"atom_codes", 2, fn_iso_atom_codes_2, NULL, false},
	{"number_chars", 2, fn_iso_number_chars_2, NULL, false},
	{"number_codes", 2, fn_iso_number_codes_2, NULL, false},
	{"clause", 2, fn_iso_clause_2, NULL, false},
	{"$arg", 4, fn_iso_arg_3, NULL, false},
	{"functor", 3, fn_iso_functor_3, NULL, false},
	{"copy_term", 2, fn_iso_copy_term_2, NULL, false},
	{"term_variables", 2, fn_iso_term_variables_2, NULL, false},
	{"atom_length", 2, fn_iso_atom_length_2, NULL, false},
	{"atom_concat", 3, fn_iso_atom_concat_3, NULL, false},
	{"sub_atom", 5, fn_iso_sub_atom_5, NULL, false},
	{"current_rule", 1, fn_iso_current_rule_1, NULL, false},
	{"sort", 2, fn_iso_sort_2, NULL, false},
	{"msort", 2, fn_iso_msort_2, NULL, false},
	{"keysort", 2, fn_iso_keysort_2, NULL, false},

#ifndef SANDBOX
	{"open", 4, fn_iso_open_4, NULL, false},
	{"close", 1, fn_iso_close_1, NULL, false},
	{"close", 2, fn_iso_close_2, NULL, false},
#endif

	{"halt", 0, fn_iso_halt_0, NULL, false},
	{"halt", 1, fn_iso_halt_1, NULL, false},
	{"read_term", 2, fn_iso_read_term_2, NULL, false},
	{"read_term", 3, fn_iso_read_term_3, NULL, false},
	{"read", 1, fn_iso_read_1, NULL, false},
	{"read", 2, fn_iso_read_2, NULL, false},
	{"write_canonical", 1, fn_iso_write_canonical_1, NULL, false},
	{"write_canonical", 2, fn_iso_write_canonical_2, NULL, false},
	{"write_term", 2, fn_iso_write_term_2, NULL, false},
	{"write_term", 3, fn_iso_write_term_3, NULL, false},
	{"writeq", 1, fn_iso_writeq_1, NULL, false},
	{"writeq", 2, fn_iso_writeq_2, NULL, false},
	{"write", 1, fn_iso_write_1, NULL, false},
	{"write", 2, fn_iso_write_2, NULL, false},
	{"nl", 0, fn_iso_nl_0, NULL, false},
	{"nl", 1, fn_iso_nl_1, NULL, false},
	{"at_end_of_stream", 0, fn_iso_at_end_of_stream_0, NULL, false},
	{"at_end_of_stream", 1, fn_iso_at_end_of_stream_1, NULL, false},
	{"set_stream_position", 2, fn_iso_set_stream_position_2, NULL, false},
	{"flush_output", 0, fn_iso_flush_output_0, NULL, false},
	{"flush_output", 1, fn_iso_flush_output_1, NULL, false},
	{"put_char", 1, fn_iso_put_char_1, NULL, false},
	{"put_char", 2, fn_iso_put_char_2, NULL, false},
	{"put_code", 1, fn_iso_put_code_1, NULL, false},
	{"put_code", 2, fn_iso_put_code_2, NULL, false},
	{"put_byte", 1, fn_iso_put_byte_1, NULL, false},
	{"put_byte", 2, fn_iso_put_byte_2, NULL, false},
	{"get_char", 1, fn_iso_get_char_1, NULL, false},
	{"get_char", 2, fn_iso_get_char_2, NULL, false},
	{"get_code", 1, fn_iso_get_code_1, NULL, false},
	{"get_code", 2, fn_iso_get_code_2, NULL, false},
	{"get_byte", 1, fn_iso_get_byte_1, NULL, false},
	{"get_byte", 2, fn_iso_get_byte_2, NULL, false},
	{"peek_char", 1, fn_iso_peek_char_1, NULL, false},
	{"peek_char", 2, fn_iso_peek_char_2, NULL, false},
	{"peek_code", 1, fn_iso_peek_code_1, NULL, false},
	{"peek_code", 2, fn_iso_peek_code_2, NULL, false},
	{"peek_byte", 1, fn_iso_peek_byte_1, NULL, false},
	{"peek_byte", 2, fn_iso_peek_byte_2, NULL, false},
	{"current_input", 1, fn_iso_current_input_1, NULL, false},
	{"current_output", 1, fn_iso_current_output_1, NULL, false},
	{"set_input", 1, fn_iso_set_input_1, NULL, false},
	{"set_output", 1, fn_iso_set_output_1, NULL, false},
	{"stream_property", 2, fn_iso_stream_property_2, NULL, false},

	{"abolish", 1, fn_iso_abolish_1, NULL, false},
	{"asserta", 1, fn_iso_asserta_1, NULL, false},
	{"assertz", 1, fn_iso_assertz_1, NULL, false},
	{"retract", 1, fn_iso_retract_1, NULL, false},
	{"retractall", 1, fn_iso_retractall_1, NULL, false},

	{"$legacy_current_prolog_flag", 2, fn_iso_current_prolog_flag_2, NULL, false},
	{"set_prolog_flag", 2, fn_iso_set_prolog_flag_2, NULL, false},
	{"op", 3, fn_iso_op_3, NULL, false},
	{"findall", 3, fn_iso_findall_3, NULL, false},
	{"current_predicate", 1, fn_iso_current_predicate_1, NULL, false},
	{"acyclic_term", 1, fn_iso_acyclic_term_1, NULL, false},
	{"compare", 3, fn_iso_compare_3, NULL, false},

	{"unify_with_occurs_check", 2, fn_iso_unify_with_occurs_check_2, NULL, false},
	{"=", 2, fn_iso_unify_2, NULL, false},
	{"\\=", 2, fn_iso_notunify_2, NULL, false},

	{0}
};

static const struct builtins g_predicates_other[] =
{
	{"*->", 2, fn_if_2, NULL, false},
	{"if", 3, fn_if_3, NULL, false},

	{"cyclic_term", 1, fn_cyclic_term_1, NULL, false},
	{"current_module", 1, fn_current_module_1, NULL, false},
	{"module", 1, fn_module_1, NULL, false},
	{"using", 0, fn_using_0, NULL, false},

#ifndef SANDBOX
	{"use_module", 1, fn_use_module_1, NULL, false},
	{"use_module", 2, fn_use_module_2, NULL, false},
	{"load_files", 2, fn_load_files_2, NULL, false},
	{"unload_files", 1, fn_unload_files_1, NULL, false},
	{"getfile", 2, fn_getfile_2, "+string,-list", false},
	{"loadfile", 2, fn_loadfile_2, "+string,-string", false},
	{"savefile", 2, fn_savefile_2, "+string,+string", false},
	{"rename_file", 2, fn_rename_file_2, "+string,+string", false},
	{"copy_file", 2, fn_copy_file_2, "+string,+string", false},
	{"directory_files", 2, fn_directory_files_2, "+pathname,-list", false},
	{"delete_file", 1, fn_delete_file_1, "+string", false},
	{"exists_file", 1, fn_exists_file_1, "+string", false},
	{"access_file", 2, fn_access_file_2, "+string,+mode", false},
	{"time_file", 2, fn_time_file_2, "+string,-real", false},
	{"size_file", 2, fn_size_file_2, "+string,-integer", false},
	{"exists_directory", 1, fn_exists_directory_1, "+string", false},
	{"make_directory", 1, fn_make_directory_1, "+string", false},
	{"make_directory_path", 1, fn_make_directory_path_1, "+string", false},
	{"working_directory", 2, fn_working_directory_2, "-string,+string", false},
	{"absolute_file_name", 3, fn_absolute_file_name_3, NULL, false},
	{"chdir", 1, fn_chdir_1, "+string", false},
	{"sleep", 1, fn_sleep_1, "+integer", false},
	{"delay", 1, fn_delay_1, "+integer", false},
	{"shell", 1, fn_shell_1, "+atom", false},
	{"shell", 2, fn_shell_2, "+atom,-integer", false},

#ifndef _WIN32
	{"popen", 4, fn_popen_4, "+atom,+atom,-stream,+list", false},
#endif

	// Used for database log...

	{"$a_", 2, fn_sys_asserta_2, "+term,+ref", false},
	{"$z_", 2, fn_sys_assertz_2, "+term,+ref", false},
	{"$e_", 1, fn_erase_1, "+ref", false},
	{"$db_load", 0, fn_sys_db_load_0, NULL, false},
	{"$db_save", 0, fn_sys_db_save_0, NULL, false},

#endif

	{"client", 5, fn_client_5, "+string,-string,-string,-stream,+list", false},
	{"server", 3, fn_server_3, "+string,-stream,+list", false},
	{"accept", 2, fn_accept_2, "+stream,-stream", false},
	{"bread", 3, fn_bread_3, "+stream,+integer,-string", false},
	{"bwrite", 2, fn_bwrite_2, "+stream,-string", false},

	{"listing", 0, fn_listing_0, NULL, false},
	{"listing", 1, fn_listing_1, NULL, false},
	{"time", 1, fn_time_1, NULL, false},
	{"trace", 0, fn_trace_0, NULL, false},

	{"$register_cleanup", 1, fn_sys_register_cleanup_1, NULL, false},
	{"$register_term", 1, fn_sys_register_term_1, NULL, false},
	{"$get_level", 1, fn_sys_get_level_1, "-var", false},

	// Edinburgh...

	{"seeing", 1, fn_edin_seeing_1, "-name", false},
	{"telling", 1, fn_edin_telling_1, "-name", false},
	{"seen", 0, fn_edin_seen_0, NULL, false},
	{"told", 0, fn_edin_told_0, NULL, false},
	{"redo", 1, fn_edin_redo_1, "+integer", false},
	{"redo", 2, fn_edin_redo_2, "+stream,+integer", false},
	{"tab", 1, fn_edin_tab_1, "+integer", false},
	{"tab", 2, fn_edin_tab_2, "+stream,+integer", false},

	// Miscellaneous...

	{"sort", 4, fn_sort_4, NULL, false},

	{"pid", 1, fn_pid_1, "-integer", false},
	{"get_unbuffered_code", 1, fn_get_unbuffered_code_1, "?code", false},
	{"get_unbuffered_char", 1, fn_get_unbuffered_char_1, "?char", false},
	{"$put_chars", 1, fn_sys_put_chars_1, "+chars", false},
	{"$put_chars", 2, fn_sys_put_chars_2, "+stream,+chars", false},
	{"$undo_trail", 1, fn_sys_undo_trail_1, NULL, false},
	{"$redo_trail", 0, fn_sys_redo_trail_0, NULL, false},
	{"format", 2, fn_format_2, "+string,+list", false},
	{"format", 3, fn_format_3, "+stream,+string,+list", false},
	{"abolish", 2, fn_abolish_2, NULL, false},
	{"assert", 1, fn_iso_assertz_1, NULL, false},
	{"$strip_attributes", 1, fn_sys_strip_attributes_1, "+vars", false},
	{"copy_term_nat", 2, fn_copy_term_nat_2, NULL, false},
	{"string", 1, fn_atom_1, "+term", false},
	{"atomic_concat", 3, fn_atomic_concat_3, NULL, false},
	{"atomic_list_concat", 3, fn_atomic_list_concat_3, NULL, false},
	{"replace", 4, fn_replace_4, "+orig,+from,+to,-new", false},
	{"busy", 1, fn_busy_1, "+integer", false},
	{"now", 0, fn_now_0, NULL, false},
	{"now", 1, fn_now_1, "now(-integer)", false},
	{"get_time", 1, fn_get_time_1, "-variable", false},
	{"cpu_time", 1, fn_cpu_time_1, "-variable", false},
	{"wall_time", 1, fn_wall_time_1, "-integer", false},
	{"date_time", 6, fn_date_time_6, "-yyyy,-m,-d,-h,--m,-s", false},
	{"date_time", 7, fn_date_time_7, "-yyyy,-m,-d,-h,--m,-s,-ms", false},
	{"$between", 4, fn_between_3, "+integer,+integer,-integer", false},
	{"read_line_to_string", 2, fn_read_line_to_string_2, "+stream,-string", false},
	{"read_file_to_string", 3, fn_read_file_to_string_3, "+string,-string,+options", false},
	{"getline", 1, fn_getline_1, "-string", false},
	{"getline", 2, fn_getline_2, "+stream,-string", false},
	{"getlines", 1, fn_getlines_1, "-list", false},
	{"getlines", 2, fn_getlines_2, "+stream,-list", false},
	{"split_atom", 4, fn_split_atom_4, "+string,+sep,+pad,-list", false},
	{"split_string", 4, fn_split_atom_4, "+string,+sep,+pad,-list", false},
	{"split", 4, fn_split_4, "+string,+string,?left,?right", false},
	{"$is_partial_string", 1, fn_sys_is_partial_string_1, "+string", false},
	{"is_list_or_partial_list", 1, fn_is_list_or_partial_list_1, "+term", false},
	{"is_partial_list", 1, fn_is_partial_list_1, "+term", false},
	{"is_list", 1, fn_is_list_1, "+term", false},
	{"list", 1, fn_is_list_1, "+term", false},
	{"is_stream", 1, fn_is_stream_1, "+term", false},
	//{"forall", 2, fn_forall_2, "+term,+term", false},
	{"term_hash", 2, fn_term_hash_2, "+term,?integer", false},
	{"name", 2, fn_iso_atom_codes_2, "?string,?list", false},
	{"read_term_from_atom", 3, fn_read_term_from_atom_3, "+atom,?term,+list", false},
	{"read_term_from_chars", 3, fn_read_term_from_chars_3, "+chars,?term,+list", false},
	{"write_term_to_atom", 3, fn_write_term_to_atom_3, "?atom,?term,+list", false},
	{"write_canonical_to_atom", 3, fn_write_canonical_to_chars_3, "?atom,?term,+list", false},
	{"write_term_to_chars", 3, fn_write_term_to_chars_3, "?chars,?term,+list", false},
	{"write_canonical_to_chars", 3, fn_write_canonical_to_chars_3, "?chars,?term,+list", false},
	{"base64", 3, fn_base64_3, "?string,?string,+list", false},
	{"urlenc", 3, fn_urlenc_3, "?string,?string,+list", false},
	{"atom_lower", 2, fn_atom_lower_2, "?atom,?atom", false},
	{"atom_upper", 2, fn_atom_upper_2, "?atom,?atom", false},
	{"string_lower", 2, fn_string_lower_2, "?string,?string", false},
	{"string_upper", 2, fn_string_upper_2, "?string,?string", false},
	{"hex_bytes", 2, fn_hex_bytes_2, "?string,?list", false},
	{"hex_chars", 2, fn_hex_chars_2, "?integer,?string", false},
	{"octal_chars", 2, fn_octal_chars_2, "?integer,?string", false},
	{"$legacy_predicate_property", 2, fn_sys_legacy_predicate_property_2, "+callable,?string", false},
	{"$load_properties", 0, fn_sys_load_properties_0, NULL, false},
	{"$load_flags", 0, fn_sys_load_flags_0, NULL, false},
	{"$load_ops", 0, fn_sys_load_ops_0, NULL, false},
	{"numbervars", 1, fn_numbervars_1, "+term", false},
	{"numbervars", 3, fn_numbervars_3, "+term,+start,?end", false},
	{"numbervars", 4, fn_numbervars_3, "+term,+start,?end,+list", false},
	{"var_number", 2, fn_var_number_2, "+term,?integer", false},
	{"char_type", 2, fn_char_type_2, "+char,+term", false},
	{"code_type", 2, fn_char_type_2, "+code,+term", false},
	{"uuid", 1, fn_uuid_1, "-string", false},
	{"asserta", 2, fn_asserta_2, "+term,-ref", false},
	{"assertz", 2, fn_assertz_2, "+term,-ref", false},
	{"instance", 2, fn_instance_2, "+ref,?clause", false},
	{"erase", 1, fn_erase_1, "+ref", false},
	{"clause", 3, fn_clause_3, "?head,?body,-ref", false},
	{"$queue", 1, fn_sys_queue_1, "+term", false},
	{"$list", 1, fn_sys_list_1, "-list", false},
	{"getenv", 2, fn_getenv_2, NULL, false},
	{"setenv", 2, fn_setenv_2, NULL, false},
	{"unsetenv", 1, fn_unsetenv_1, NULL, false},
	{"statistics", 0, fn_statistics_0, NULL, false},
	{"statistics", 2, fn_statistics_2, "+string,-variable", false},
	{"duplicate_term", 2, fn_iso_copy_term_2, "+term,-variable", false},
	{"call_nth", 2, fn_call_nth_2, "+callable,+integer", false},
	{"limit", 2, fn_limit_2, "+integer,+callable", false},
	{"offset", 2, fn_offset_2, "+integer,+callable", false},
	{"unifiable", 3, fn_sys_unifiable_3, NULL, false},
	{"$incr", 2, fn_sys_incr_2, "?var", false},
	{"$choice", 0, fn_sys_choice_0, NULL, false},
	{"once", 1, fn_iso_once_1, "+callable", false},
	{"ignore", 1, fn_ignore_1, "+callable", false},

	{"kv_set", 3, fn_kv_set_3, "+atomic,+value,+list", false},
	{"kv_get", 3, fn_kv_get_3, "+atomic,-value,+list", false},

	{"$alarm", 1, fn_sys_alarm_1, "+integer", false},
	{"$write_attributes", 2, fn_sys_write_attributes_2, "+variable,+list", false},
	{"$read_attributes", 2, fn_sys_read_attributes_2, "+variable,-list", false},
	{"$erase_attributes", 1, fn_sys_erase_attributes_1, "+variable", false},

	{"$dump_keys", 1, fn_sys_dump_keys_1, "+pi", false},

#if USE_OPENSSL
	{"crypto_data_hash", 3, fn_crypto_data_hash_3, "?string,?string,?list", false},
#endif

	{"task", 1, fn_task_n, "+callable", false},
	{"task", 2, fn_task_n, "+callable,+term,...", false},
	{"task", 3, fn_task_n, "+callable,+term,...", false},
	{"task", 4, fn_task_n, "+callable,+term,...", false},
	{"task", 5, fn_task_n, "+callable,+term,...", false},
	{"task", 6, fn_task_n, "+callable,+term,...", false},
	{"task", 7, fn_task_n, "+callable,+term,...", false},
	{"task", 8, fn_task_n, "+callable,+term,...", false},

	{"wait", 0, fn_wait_0, NULL, false},
	{"await", 0, fn_await_0, NULL, false},
	{"yield", 0, fn_yield_0, NULL, false},
	{"fork", 0, fn_fork_0, NULL, false},
	{"send", 1, fn_send_1, "+term", false},
	{"recv", 1, fn_recv_1, "?clause", false},

	{"$mustbe_instantiated", 2, fn_sys_instantiated_2, "+term,+term", false},
	{"$mustbe_list_or_var", 1, fn_sys_mustbe_list_or_var_1, "?list", false},

	{"$skip_max_list", 4, fn_sys_skip_max_list_4, NULL, false},

	{0}
};

void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found, bool *function)
{
	miter *iter = m_find_key(pl->funtab, name);
	const struct builtins *ptr;

	while (m_next_key(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			m_done(iter);
			if (found) *found = true;
			if (function) *function = ptr->function;
			return ptr->fn;
		}
	}

	if (found) *found = false;
	if (function) *function = false;
	return NULL;
}

extern const struct builtins g_functions[];
extern const struct builtins g_contrib_funcs[];

void load_builtins(prolog *pl)
{
	for (const struct builtins *ptr = g_predicates_iso; ptr->name; ptr++) {
		m_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_functions; ptr->name; ptr++) {
		m_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_predicates_other; ptr->name; ptr++) {
		m_app(pl->funtab, ptr->name, ptr);
	}

	for (const struct builtins *ptr = g_contrib_funcs; ptr->name; ptr++) {
		m_app(pl->funtab, ptr->name, ptr);
	}
}

void format_property(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const char *type)
{
	char *dst = tmpbuf;

	if (needs_quoting(m, name, strlen(name))) {
		char tmpbuf2[1024];
		formatted(tmpbuf2, sizeof(tmpbuf2), name, strlen(name), false);
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'('%s'", tmpbuf2);
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
}

static void load_properties(module *m)
{
	if (m->loaded_properties)
		return;

	m->loaded_properties = true;
	ASTRING_alloc(pr, 1024*64);
	char tmpbuf[1024];

	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "meta_predicate((0,0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "meta_predicate((0;0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "meta_predicate((0->0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "meta_predicate((0*->0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "if", 3, "meta_predicate(if(0,0,0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "call", 1, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call", 1, "meta_predicate(call(0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "once", 1, "meta_predicate(once(0))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "ignore", 1, "meta_predicate(ignore(0))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "catch", 3, "meta_predicate(catch(0,0,0))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 3, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 3, "meta_predicate(findall(?,0,-))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 4, "meta_predicate(findall(?,0,-,?))"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "bagof", 3, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "setof", 3, "control_construct"); ASTRING_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "throw", 1, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "!", 0, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "true", 0, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "fail", 0, "control_construct"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "|", 2, "meta_predicate((:|+))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "time", 1, "meta_predicate(time(0))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 1, "meta_predicate(asserta(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 2, "meta_predicate(asserta(:,-))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 1, "meta_predicate(assertz(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 2, "meta_predicate(assertz(:,-))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 1, "meta_predicate(retract(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 2, "meta_predicate(retract(:,?))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retractall", 1, "meta_predicate(retractall(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "current_predicate", 1, "meta_predicate(current_predicate(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "predicate_property", 1, "meta_predicate(predicate_property(:,?))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "abolish", 1, "meta_predicate(abolish(:))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 2, "meta_predicate(db_entry(:,?))"); ASTRING_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 3, "meta_predicate(db_entry(:,?,?))"); ASTRING_strcat(pr, tmpbuf);

	for (int i = 2; i <= 7; i++) {
		char metabuf[256];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(call(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "call", i, metabuf); ASTRING_strcat(pr, tmpbuf);
	}

	for (int i = 2; i <= 7; i++) {
		char metabuf[256];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(task(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "task", i, metabuf); ASTRING_strcat(pr, tmpbuf);
	}

	for (const struct builtins *ptr = g_predicates_iso; ptr->name; ptr++) {
		m_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->function) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); ASTRING_strcat(pr, tmpbuf);
 	}

	for (const struct builtins *ptr = g_functions; ptr->name; ptr++) {
		m_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->function) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); ASTRING_strcat(pr, tmpbuf);
	}

	for (const struct builtins *ptr = g_predicates_other; ptr->name; ptr++) {
		m_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->function) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); ASTRING_strcat(pr, tmpbuf);
	}

	for (const struct builtins *ptr = g_contrib_funcs; ptr->name; ptr++) {
		m_app(m->pl->funtab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->function) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); ASTRING_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); ASTRING_strcat(pr, tmpbuf);
	}

	parser *p = create_parser(m);
	p->srcptr = ASTRING_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	ASTRING_free(pr);
}

static void load_flags(query *q)
{
	cell tmp;
	make_literal(&tmp, index_from_pool(q->pl, "$current_prolog_flag"));
	tmp.arity = 2;

	if (do_abolish(q, &tmp, &tmp, false) != pl_success)
		return;

	module *m = q->st.m;
	ASTRING_alloc(pr, 1024);

	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "double_quotes", m->flag.double_quote_atom?"atom":m->flag.double_quote_chars?"chars":m->flag.double_quote_codes?"codes":"???");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "char_conversion", m->flag.char_conversion?"on":"off");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "occurs_check", m->flag.occurs_check==OCCURS_TRUE?"on":m->flag.occurs_check==OCCURS_FALSE?"off":"error");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "character_escapes", m->flag.character_escapes?"true":"false");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "strict_iso", !m->flag.not_strict_iso?"on":"off");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "debug", m->flag.debug?"on":"off");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unknown", m->flag.unknown == UNK_ERROR?"error":m->flag.unknown == UNK_WARNING?"warning":m->flag.unknown == UNK_CHANGEABLE?"changeable":"fail");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "encoding", "'UTF-8'");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unix", "true");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "dialect", "trealla");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "bounded", "false");
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "max_arity", MAX_ARITY);
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "cpu_count", g_cpu_count);
	ASTRING_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "integer_rounding_function", "toward_zero");

	parser *p = create_parser(m);
	p->srcptr = ASTRING_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	ASTRING_free(pr);
}

static void load_ops(query *q)
{
	if (q->st.m->loaded_ops)
		return;

	cell tmp;
	make_literal(&tmp, index_from_pool(q->pl, "$current_op"));
	tmp.arity = 3;

	if (do_abolish(q, &tmp, &tmp, false) != pl_success)
		return;

	q->st.m->loaded_ops = true;
	ASTRING_alloc(pr, 1024*8);
	miter *iter = m_first(q->st.m->ops);
	op_table *ptr;

	while (m_next(iter, (void**)&ptr)) {
		char specifier[80], name[256];

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
		char tmpbuf[1024];

		snprintf(tmpbuf, sizeof(tmpbuf), "'$current_op'(%u, %s, (%s)).\n",
			ptr->priority, specifier, name);
		ASTRING_strcat(pr, tmpbuf);
	}

	iter = m_first(q->st.m->defops);

	while (m_next(iter, (void**)&ptr)) {
		char specifier[80], name[256];

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
		char tmpbuf[1024];

		snprintf(tmpbuf, sizeof(tmpbuf), "'$current_op'(%u, %s, '%s').\n",
			ptr->priority, specifier, name);
		ASTRING_strcat(pr, tmpbuf);
	}

	//printf("%s", ASTRING_cstr(pr));

	parser *p = create_parser(q->st.m);
	p->srcptr = ASTRING_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	ASTRING_free(pr);
}
