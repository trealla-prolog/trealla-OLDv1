#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

USE_RESULT bool fn_iso_true_0(query *q)
{
	return true;
}

USE_RESULT bool fn_iso_fail_0(query *q)
{
	return false;
}

USE_RESULT bool fn_sys_cut_if_det_0(query *q)
{
	cut_if_det(q);
	return true;
}

void do_cleanup(query *q, cell *p1)
{
	cell *tmp = clone_to_heap(q, true, p1, 2);
	ensure(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0); // ???
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
}

static USE_RESULT bool fn_sys_cleanup_if_det_0(query *q)
{
	if (!q->cp)		// redundant
		return true;

	choice *ch = GET_CURR_CHOICE();
	frame *f = GET_CURR_FRAME();

	if (!ch->call_barrier || (ch->cgen != f->cgen))
		return true;

	drop_choice(q);
	ch = GET_CURR_CHOICE();

	if (!ch->register_cleanup)
		return true;

	if (ch->did_cleanup)
		return true;

	drop_choice(q);
	ch->did_cleanup = true;
	cell *c = deref(q, ch->st.curr_cell, ch->st.curr_frame);
	pl_idx_t c_ctx = q->latest_ctx;
	c = deref(q, c+1, c_ctx);
	do_cleanup(q, c);
	return true;
}

// module:goal

USE_RESULT bool fn_iso_invoke_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,callable);

	module *m = find_module(q->pl, C_STR(q, p1));

	if (!m)
		m = create_module(q->pl, C_STR(q, p1));

	cell *tmp = clone_to_heap(q, true, p2, 1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1;

	if (!is_builtin(p2) /*&& !tmp[nbr_cells].match*/)
		tmp[nbr_cells].match = find_predicate(m, p2);

	nbr_cells += p2->nbr_cells;
	make_return(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->st.curr_frame = p2_ctx;
	q->st.m = q->save_m = m;
	return true;
}

USE_RESULT bool fn_call_0(query *q, cell *p1)
{
	if (q->retry)
		return false;

	p1 = deref(q, p1, q->st.curr_frame);
	pl_idx_t p1_ctx = q->latest_ctx;

	if (!is_callable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	cell *tmp2;

	if ((tmp2 = check_body_callable(q->st.m->p, p1)) != NULL)
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	cell *tmp = clone_to_heap(q, false, p1, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 0 + tmp->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_cut_if_det_s, fn_sys_cut_if_det_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	q->st.curr_frame = p1_ctx;
	return true;
}

USE_RESULT bool fn_iso_call_n(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	may_heap_error(init_tmp_heap(q));
	may_heap_error(deep_clone_to_tmp(q, p1, p1_ctx));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		GET_NEXT_ARG(p2,any);
		may_heap_error(deep_clone_to_tmp(q, p2, p2_ctx));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		share_cell(tmp2);
		convert_to_literal(q->st.m, tmp2);
	}

	const char *functor = C_STR(q, tmp2);
	bool found = false;

	if ((tmp2->fn_ptr = get_builtin(q->pl, C_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->fn = tmp2->fn_ptr->fn;
		tmp2->flags |= FLAG_BUILTIN;
	}

	if (arity <= 2) {
		unsigned specifier;

		if (search_op(q->st.m, functor, &specifier, false))
			SET_OP(tmp2, specifier);
	}

	if (check_body_callable(q->st.m->p, tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_cut_if_det_s, fn_sys_cut_if_det_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool fn_iso_call_1(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	may_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	may_heap_error(tmp2);

	const char *functor = C_STR(q, tmp2);
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn_ptr = get_builtin(q->pl, C_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->fn = tmp2->fn_ptr->fn;
		tmp2->flags |= FLAG_BUILTIN;
	}
	if (check_body_callable(q->st.m->p, tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_cut_if_det_s, fn_sys_cut_if_det_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool fn_iso_once_1(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	may_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	may_heap_error(tmp2);

	const char *functor = C_STR(q, tmp2);
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn_ptr = get_builtin(q->pl, C_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->fn = tmp2->fn_ptr->fn;
		tmp2->flags |= FLAG_BUILTIN;
	}
	if (check_body_callable(q->st.m->p, tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool fn_ignore_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,callable);
	may_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	may_heap_error(tmp2);

	const char *functor = C_STR(q, tmp2);
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn_ptr = get_builtin(q->pl, C_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->fn = tmp2->fn_ptr->fn;
		tmp2->flags |= FLAG_BUILTIN;
	}
	if (check_body_callable(q->st.m->p, tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if -> then

USE_RESULT bool fn_iso_if_then_2(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if *-> then

USE_RESULT bool fn_if_2(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_soft_cut_s, fn_sys_soft_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if -> then ; else

static bool do_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
{
	if (q->retry) {
		cell *tmp = clone_to_heap(q, true, p3, 1);
		may_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p3->nbr_cells;
		make_return(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if *-> then ; else

static bool do_if_else(query *q, cell *p1, cell *p2, cell *p3)
{
	if (q->retry) {
		cell *tmp = clone_to_heap(q, true, p3, 1);
		may_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p3->nbr_cells;
		make_return(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1+p2->nbr_cells+1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_soft_cut_s, fn_sys_soft_inner_cut_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if_(if,then,else)

USE_RESULT bool fn_if_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,callable);
	return do_if_else(q, p1, p2, p3);
}

// either ; or

USE_RESULT bool fn_iso_disjunction_2(query *q)
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
		may_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_return(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, 1);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_return(q, tmp+nbr_cells);
	may_error(push_choice(q));
	q->st.curr_cell = tmp;
	return true;
}

// \+ goal

USE_RESULT bool fn_iso_negation_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, 3);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool fn_iso_cut_0(query *q)
{
	cut_me(q, false, false);
	return true;
}

USE_RESULT bool fn_sys_inner_cut_0(query *q)
{
	cut_me(q, true, false);
	return true;
}

USE_RESULT bool fn_sys_soft_inner_cut_0(query *q)
{
	cut_me(q, true, true);
	return true;
}

USE_RESULT bool fn_sys_block_catcher_1(query *q)
{
	if (!q->cp)
		return true;

	GET_FIRST_ARG(p1,integer);
	pl_idx_t cp = get_smallint(p1);
	choice *ch = GET_CHOICE(cp);

	if (!ch->catchme_retry)
		return false;

	if (q->retry) {
		ch->block_catcher = false;
		return false;
	}

	if (cut_if_det(q))
		return true;

	ch->block_catcher = true;
	may_error(push_choice(q));
	return true;
}

USE_RESULT bool fn_iso_catch_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (q->retry && q->exception) {
		cell *tmp = deep_clone_to_heap(q, q->exception, q->st.curr_frame);
		may_heap_error(tmp);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = clone_to_heap(q, true, p3, 2);
		may_heap_error(tmp);
		pl_idx_t nbr_cells = 1+p3->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_cut_if_det_s, fn_sys_cut_if_det_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		may_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_cell = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	pl_idx_t cp = q->cp;
	cell *tmp = clone_to_heap(q, true, p1, 3);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_block_catcher_s, fn_sys_block_catcher_1, 1, 1);
	make_int(tmp+nbr_cells++, cp);
	make_return(q, tmp+nbr_cells);
	may_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool fn_sys_call_cleanup_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (q->retry && q->exception) {
		cell *tmp = deep_clone_to_heap(q, q->exception, q->st.curr_frame);
		may_heap_error(tmp);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = clone_to_heap(q, true, p3, 2);
		may_heap_error(tmp);
		pl_idx_t nbr_cells = 1+p3->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_cleanup_if_det_s, fn_sys_cleanup_if_det_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		may_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_cell = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	cell *tmp = clone_to_heap(q, true, p1, 2);
	may_heap_error(tmp);
	pl_idx_t nbr_cells = 1+p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_cleanup_if_det_s, fn_sys_cleanup_if_det_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_cell = tmp;
	return true;
}

USE_RESULT bool find_exception_handler(query *q, cell *e)
{
	pl_idx_t e_ctx = q->st.curr_frame;

	while (retry_choice(q)) {
		choice *ch = GET_CHOICE(q->cp);

		if (ch->block_catcher)
			continue;

		//if (ch->did_cleanup)
		//	continue;

		if (!ch->catchme_retry)
			continue;

		may_heap_error(init_tmp_heap(q));
		cell *tmp = deep_copy_to_tmp(q, e, e_ctx, false);
		may_ptr_error(tmp);
		cell *e2 = malloc(sizeof(cell) * tmp->nbr_cells);
		may_ptr_error(e2);
		//safe_copy_cells(e2, tmp, tmp->nbr_cells);
		copy_cells(e2, tmp, tmp->nbr_cells);

		q->exception = e2;
		q->retry = QUERY_EXCEPTION;

		if (fn_iso_catch_3(q) != true) {
			free(e2);
			q->exception = NULL;
			continue;
		}

		free(e2);
		q->exception = NULL;
		return true;
	}

	if (!q->is_redo)
		fprintf(stdout, "   ");
	else
		fprintf(stdout, "  ");

	if (!is_interned(e) || strcmp(C_STR(q, e), "error"))
		fprintf(stdout, "throw(");

	if (is_cyclic_term(q, e, e_ctx)) {
		q->quoted = 1;
		print_term(q, stdout, e, e_ctx, 0);
	} else {
		q->quoted = 1;
		print_term(q, stdout, e, e_ctx, 1);
	}

	if (!is_interned(e) || strcmp(C_STR(q, e), "error"))
		fprintf(stdout, ")");

	fprintf(stdout, ".\n");
	q->quoted = 0;
	q->pl->did_dump_vars = true;
	q->exception = NULL;
	q->error = true;
	return false;
}

USE_RESULT bool fn_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	cell *e;

	if (is_cyclic_term(q, p1, p1_ctx)) {
		e = malloc(sizeof(cell) * p1->nbr_cells);
		may_ptr_error(e);
		//safe_copy_cells(e, p1, p1->nbr_cells);
		copy_cells(e, p1, p1->nbr_cells);
	} else {
		may_heap_error(init_tmp_heap(q));
		cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
		may_ptr_error(tmp);
		e = malloc(sizeof(cell) * tmp->nbr_cells);
		may_ptr_error(e);
		//safe_copy_cells(e, tmp, tmp->nbr_cells);
		copy_cells(e, tmp, tmp->nbr_cells);
	}

	if (!find_exception_handler(q, e)) {
		free(e);
		return false;
	}

	free(e);
	return fn_iso_catch_3(q);
}

bool throw_error3(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal)
{
	if (g_tpl_interrupt)
		return false;

	q->did_throw = true;
	q->quoted = 0;

	if (!strncmp(expected, "iso_", 4))
		expected += 4;

	char tmpbuf[1024];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s", expected);
	char *ptr;

	if (!strcmp(err_type, "type_error")
		&& ((ptr = strstr(tmpbuf, "_or")) != NULL))
		*ptr = '\0';

	if (!strcmp(err_type, "type_error") && !strcmp(expected, "stream"))
		err_type = "existence_error";

	expected = tmpbuf;
	char functor[1024];
	functor[0] = '\0';

	if (!strcmp(expected, "smallint"))
		expected = "integer";

	if (!is_variable(c)) {
		char *tmpbuf = DUP_STR(q, goal);
		snprintf(functor, sizeof(functor), "%s", tmpbuf);
		free(tmpbuf);
	}

	int extra = 0;
	const char *eptr = expected;

	while ((eptr = strchr(eptr, ',')) != NULL) {
		extra += 1;
		eptr++;
	}

	bool is_abolish = false;

	if (!strcmp(C_STR(q, q->st.curr_cell), "abolish"))
		is_abolish = true;

	cell *tmp;

	if (is_variable(c)) {
		err_type = "instantiation_error";
		//printf("error(%s,%s).\n", err_type, expected);
		tmp = alloc_on_heap(q, 3);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 2);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, err_type));
		make_atom(tmp+nbr_cells, index_from_pool(q->pl, expected));
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "variable")) {
		err_type = "uninstantiation_error";
		//printf("error(%s(%s),(%s)/%u).\n", err_type, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 6+(c->nbr_cells-1));
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 5+(c->nbr_cells-1));
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 1, 1+(c->nbr_cells-1));
		copy_cells(tmp+nbr_cells, c, c->nbr_cells);
		nbr_cells += c->nbr_cells;
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "evaluable")) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, C_STR(q, c), c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && is_structure(c) && CMP_STR_CSTR(q, c, "/") && is_variable(c+1)) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && (is_builtin(c) || (is_op(c) && !is_abolish))) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "instantiation_error")) {
		//printf("error(%s,(%s)/%u).\n", err_type, functor, goal->arity);
		tmp = alloc_on_heap(q, 5);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, err_type));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "existence_error") && !strcmp(expected, "procedure")) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, C_STR(q, c)));
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "representation_error")
		|| !strcmp(err_type, "evaluation_error")
		|| !strcmp(err_type, "syntax_error")
		|| !strcmp(err_type, "resource_error")) {
		//printf("error(%s(%s),(%s)/%u).\n", err_type, expected, functor, goal->arity);
		tmp = alloc_on_heap(q, 6);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 5);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 1, 1);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else {
		//printf("error(%s(%s,(%s)),(%s)/%u).\n", err_type, expected, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 7+(c->nbr_cells-1)+extra);
		may_ptr_error(tmp);
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 6+(c->nbr_cells-1)+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 2+(c->nbr_cells-1)+extra);

		if (!extra) {
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		} else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		nbr_cells += safe_copy_cells(tmp+nbr_cells, c, c->nbr_cells);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	}

	cell *e = malloc(sizeof(cell) * tmp->nbr_cells);
	may_ptr_error(e);
	safe_copy_cells(e, tmp, tmp->nbr_cells);

	if (find_exception_handler(q, e)) {
		free(e);
		return fn_iso_catch_3(q);
	}

	free(e);
	return false;
}

bool throw_error2(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal)
{
	cell tmp;
	tmp = goal[1];
	tmp.arity = get_smallint(&goal[2]);
	return throw_error3(q, c, c_ctx, err_type, expected, &tmp);
}

bool throw_error(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected)
{
	return throw_error3(q, c, c_ctx, err_type, expected, q->st.curr_cell);
}

