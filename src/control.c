#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
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
#include <dirent.h>
#endif

#include "internal.h"
#include "network.h"
#include "base64.h"
#include "library.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "builtins.h"
#include "heap.h"
#include "utf8.h"

USE_RESULT pl_status fn_iso_true_0(__attribute__((unused)) query *q)
{
	return pl_success;
}

USE_RESULT pl_status fn_iso_fail_0(__attribute__((unused)) query *q)
{
	return pl_failure;
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
		tmp = copy_to_heap(q, false, p1, p1_ctx, 1);
		unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	} else
		tmp = clone_to_heap(q, false, p1, 1);

	idx_t nbr_cells = 0 + tmp->nbr_cells;
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

USE_RESULT pl_status fn_iso_call_n(query *q)
{
	if (q->retry)
		return pl_failure;

	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false, false);

	if (!p0 || (p0 == ERR_CYCLE_CELL))
		return throw_error(q, q->st.curr_cell, "resource_error", "too_many_vars");

	unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);

	GET_FIRST_RAW_ARG0(p1,callable,p0);
	clone_to_tmp(q, p1);
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		GET_NEXT_RAW_ARG(p2,any);
		clone2_to_tmp(q, p2);
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		share_cell(tmp2);
		convert_to_literal(q->st.m, tmp2);
	}

	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn = get_builtin(q->st.m->pl, GET_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->flags |= FLAG_BUILTIN;
	}

	unsigned specifier;

	if (search_op(q->st.m, GET_STR(q, tmp2), &specifier, false))
		SET_OP(tmp2, specifier);

	cell *tmp = clone_to_heap(q, true, tmp2, 1);
	idx_t nbr_cells = 1+tmp2->nbr_cells;
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));

	if (check_body_callable(q->st.m->p, tmp2) != NULL)
		return throw_error(q, tmp2, "type_error", "callable");

	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

USE_RESULT pl_status fn_sys_rawcall_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (check_body_callable(q->st.m->p, p1) != NULL)
		return throw_error(q, p1, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, p1, 1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

USE_RESULT pl_status fn_iso_invoke_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,callable);
	module *m = find_module(q->st.m->pl, GET_STR(q, p1));

	if (!m)
		m = create_module(q->st.m->pl, GET_STR(q, p1));

	cell *tmp = clone_to_heap(q, true, p2, 1);
	idx_t nbr_cells = 1;

	if (!is_builtin(p2) /*&& !tmp[nbr_cells].match*/)
		tmp[nbr_cells].match = find_predicate(m, p2);

	nbr_cells += p2->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->st.m = q->save_m = m;
	return pl_success;
}

USE_RESULT pl_status fn_iso_if_then_2(query *q)
{
	if (q->retry)
		return pl_failure;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_inner_cut_s, fn_sys_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

USE_RESULT pl_status fn_if_2(query *q)
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

static pl_status do_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
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
	make_structure(tmp+nbr_cells++, g_sys_inner_cut_s, fn_sys_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

static pl_status do_if_else(query *q, cell *p1, cell *p2, cell *p3)
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

USE_RESULT pl_status fn_if_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,callable);
	return do_if_else(q, p1, p2, p3);
}

USE_RESULT pl_status fn_iso_disjunction_2(query *q)
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

USE_RESULT pl_status fn_iso_negation_1(query *q)
{
	if (q->retry)
		return pl_success;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, 3);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_inner_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_structure(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

USE_RESULT pl_status fn_sys_choice_0(query *q)
{
	if (q->retry)
		return pl_failure;

	may_error(make_choice(q));
	return pl_success;
}

USE_RESULT pl_status fn_iso_once_1(query *q)
{
	if (q->retry)
		return pl_failure;

	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false, false);

	if (!p0 || (p0 == ERR_CYCLE_CELL))
		return throw_error(q, q->st.curr_cell, "resource_error", "too_many_vars");

	unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);
	GET_FIRST_RAW_ARG0(p1,callable,p0);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_inner_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

USE_RESULT pl_status fn_ignore_1(query *q)
{
	if (q->retry)
		return pl_success;

	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false, false);

	if (!p0 || (p0 == ERR_CYCLE_CELL))
		return throw_error(q, q->st.curr_cell, "resource_error", "too_many_vars");

	unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);
	GET_FIRST_RAW_ARG0(p1,callable,p0);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	idx_t nbr_cells = 1 + p1->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_inner_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	may_error(make_barrier(q));
	q->st.curr_cell = tmp;
	return pl_success;
}

USE_RESULT pl_status fn_iso_cut_0(query *q)
{
	cut_me(q, false, false);
	return pl_success;
}

USE_RESULT pl_status fn_sys_inner_cut_0(query *q)
{
	cut_me(q, true, false);
	return pl_success;
}

USE_RESULT pl_status fn_soft_cut_0(query *q)
{
	cut_me(q, true, true);
	return pl_success;
}

USE_RESULT pl_status fn_sys_block_catcher_0(query *q)
{
	if (!q->cp)
		return pl_success;

	choice *ch = GET_CURR_CHOICE();

	do {
		if (ch->catchme_retry) {
			ch->block_catcher = !ch->block_catcher;
			break;
		}

		ch--;
	}
	 while (ch != q->choices);

	if (q->retry)
		return false;

	if (ch != GET_CURR_CHOICE())
		may_error(make_choice(q));

	return pl_success;
}

USE_RESULT pl_status fn_iso_catch_3(query *q)
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
		may_error(make_catcher(q, QUERY_EXCEPTION));
		cell *tmp = clone_to_heap(q, true, p3, 1);
		may_ptr_error(tmp);
		make_call(q, tmp+1+p3->nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	if (q->retry)
		return pl_failure;

	// First time through? Try the primary goal...

	may_error(make_catcher(q, QUERY_RETRY));
	idx_t nbr_cells = p1->nbr_cells;
	cell *tmp = clone_to_heap(q, true, p1, 2);
	may_ptr_error(tmp);
	make_structure(tmp+1+nbr_cells++, g_sys_block_catcher_s, fn_sys_block_catcher_0, 0, 0);
	make_call(q, tmp+1+nbr_cells);
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

USE_RESULT pl_status fn_iso_catch2_3(query *q)
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
		may_error(make_catcher(q, QUERY_EXCEPTION));
		cell *tmp = clone_to_heap(q, true, p3, 1);
		may_ptr_error(tmp);
		make_call(q, tmp+1+p3->nbr_cells);
		q->st.curr_cell = tmp;
		return pl_success;
	}

	if (q->retry)
		return pl_failure;

	// First time through? Try the primary goal...

	may_error(make_catcher(q, QUERY_RETRY));
	idx_t nbr_cells = p1->nbr_cells;
	cell *tmp = clone_to_heap(q, true, p1, 1);
	may_ptr_error(tmp);
	make_call(q, tmp+1+nbr_cells);
	q->st.curr_cell = tmp;
	q->save_cp = q->cp;
	return pl_success;
}

USE_RESULT bool find_exception_handler(query *q, cell *e)
{
	q->exception = e;

	while (retry_choice(q)) {
		choice *ch = GET_CHOICE(q->cp);

		if (ch->block_catcher)
			continue;

		if (ch->register_cleanup && ch->did_cleanup)
			continue;

		if (!ch->catchme_retry)
			continue;

		cell *tmp = copy_to_heap(q, false, e, q->st.curr_frame, 0);
		may_ptr_error(tmp);
		cell *e2 = malloc(sizeof(cell) * tmp->nbr_cells);
		may_ptr_error(e2);
		safe_copy_cells(e2, tmp, tmp->nbr_cells);

		q->exception = e2;
		q->retry = QUERY_EXCEPTION;

		if (fn_iso_catch_3(q) != pl_success) {
			free(e2);
			q->exception = NULL;
			continue;
		}

		free(e2);
		q->exception = NULL;
		free(e);
		return true;
	}

	fprintf(stdout, "uncaught exception: ");

	if (is_cyclic_term(q, e, q->st.curr_frame)) {
		fprintf(stdout, " CYCLIC_TERM\n");
	} else {
		q->quoted = 1;
		print_term(q, stdout, e, q->st.curr_frame, 1);
		fprintf(stdout, "\n");
		q->quoted = 0;
	}

	q->st.m->pl->did_dump_vars = true;
	free(e);
	q->exception = NULL;
	q->error = true;
	return false;
}

USE_RESULT pl_status fn_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (is_cyclic_term(q, p1, p1_ctx)) {
		cell *e = malloc(sizeof(cell) * p1->nbr_cells);
		may_ptr_error(e);
		safe_copy_cells(e, p1, p1->nbr_cells);

		if (!find_exception_handler(q, e))
			return pl_failure;
	} else {
		cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false, false);
		may_ptr_error(tmp);
		cell *e = malloc(sizeof(cell) * tmp->nbr_cells);
		may_ptr_error(e);
		safe_copy_cells(e, tmp, tmp->nbr_cells);

		if (!find_exception_handler(q, e))
			return pl_failure;
	}

	return fn_iso_catch_3(q);
}

// TODO: rewrite error throwing not to do printing/parsing. Each
// type of *_error should have it's own function call...

pl_status throw_error3(query *q, cell *c, const char *err_type, const char *expected, cell *goal)
{
	if (g_tpl_interrupt)
		return pl_failure;

	q->did_throw = true;
	idx_t c_ctx = q->st.curr_frame;
	q->quoted = 1;
	ssize_t len = 0;
	bool running = !is_cyclic_term(q, c, c_ctx);

	len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false, 0);

	char *dst = malloc(len+1+1024);
	may_ptr_error(dst);
	int off = 0;

	if (q->st.m != q->st.m->pl->user_m)
		off += sprintf(dst, "%s:", q->st.m->name);

	len = print_term_to_buf(q, dst+off, len+1, c, c_ctx, running, false, 0) + off;

	size_t len2 = (len * 2) + strlen(err_type) + strlen(expected);
	if (!is_variable(c)) len2 += LEN_STR(q, goal);
	len2 += 1024;
	char *dst2 = malloc(len2+1);
	may_ptr_error(dst2);
	q->quoted = 0;

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

	if (!is_variable(c)) {
		if (needs_quoting(q->st.m, GET_STR(q, goal), LEN_STR(q, goal))) {
			snprintf(functor, sizeof(functor), "'%s'", GET_STR(q, goal));
		} else
			snprintf(functor, sizeof(functor), "%s", GET_STR(q, goal));
	}

	if (is_variable(c)) {
		err_type = "instantiation_error";
		snprintf(dst2, len2+1, "error(%s,%s).", err_type, expected);

	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "variable")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", "uninstantiation_error", dst, functor, goal->arity);

	} else if (!strcmp(err_type, "instantiation_error")) {
		snprintf(dst2, len2+1, "error(%s,(%s)/%u).", err_type, functor, goal->arity);

	} else if (!strcmp(err_type, "representation_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, goal->arity);

	} else if (!strcmp(err_type, "evaluation_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, goal->arity);

	} else if (!strcmp(err_type, "syntax_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, goal->arity);

	} else if (!strcmp(err_type, "resource_error")) {
		snprintf(dst2, len2+1, "error(%s(%s),(%s)/%u).", err_type, expected, functor, goal->arity);

	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "evaluable")) {
		snprintf(dst2, len2+1, "error(%s(%s,('%s')/%u),(%s)/%u).", err_type, expected, is_callable(c)?GET_STR(q, c):dst, c->arity, functor, goal->arity);

	} else if (!strcmp(err_type, "permission_error")
		&& is_structure(c) && slicecmp2(GET_STR(q, c), LEN_STR(q, c), "/") && is_variable(c+1)) {
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "('%s')/%u\n", GET_STR(q, c), (unsigned)c->arity);
		snprintf(dst2, len2+1, "error(%s(%s,%s),(%s)/%u).", err_type, expected, tmpbuf, functor, goal->arity);

	} else if (!strcmp(err_type, "existence_error") && !strcmp(expected, "procedure")) {
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "('%s')/%u\n", GET_STR(q, c), (unsigned)c->arity);
		snprintf(dst2, len2+1, "error(%s(%s,%s),(%s)/%u).", err_type, expected, tmpbuf, functor, goal->arity);

	} else if (!strcmp(err_type, "permission_error")) {
		snprintf(dst2, len2+1, "error(%s(%s,%s),(%s)/%u).", err_type, expected, dst, functor, goal->arity);

	} else if (IS_OP(goal)) {
		snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, GET_STR(q, goal), goal->arity);

	} else {
		if (!slicecmp2(GET_STR(q, goal), LEN_STR(q, goal), "$rawcall"))
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, "call", goal->arity);
		else if (!slicecmp2(GET_STR(q, goal), LEN_STR(q, goal), "$catch"))
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, "catch", goal->arity);
		else
			snprintf(dst2, len2+1, "error(%s(%s,(%s)),(%s)/%u).", err_type, expected, dst, functor, goal->arity);
	}

	//printf("*** %s\n", dst2);

	parser *p = create_parser(q->st.m);
	may_ptr_error(p);
	p->srcptr = dst2;
	frame *g = GET_CURR_FRAME();
	p->read_term = g->nbr_vars;
	tokenize(p, false, false);

	if (p->nbr_vars) {
		if (!create_vars(q, p->nbr_vars)) {
			destroy_parser(p);
			free(dst2);
			free(dst);
			return throw_error(q, c, "resource_error", "too_many_vars");
		}
	}

	cell *tmp = deep_copy_to_tmp(q, p->r->cells, q->st.curr_frame, false, false);
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

pl_status throw_error2(query *q, cell *c, const char *err_type, const char *expected, cell *goal)
{
	cell tmp;
	tmp = goal[1];
	tmp.arity = get_smallint(&goal[2]);
	return throw_error3(q, c, err_type, expected, &tmp);
}

pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected)
{
	return throw_error3(q, c, err_type, expected, q->st.curr_cell);
}

