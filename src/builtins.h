#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>

#include "internal.h"

#define GET_RAW_ARG(n,p) \
	__attribute__((unused)) cell *p = get_raw_arg(q,n); \
	__attribute__((unused)) idx_t p##_ctx = q->latest_ctx

#define is_callable(c) (is_literal(c) || is_cstring(c))
#define is_callable_or_var(c) (is_literal(c) || is_cstring(c) || is_variable(c))
#define is_structure(c) (is_literal(c) && (c)->arity)
#define is_compound(c) (is_structure(c) || is_string(c))
#define is_number(c) (is_rational(c) || is_float(c))
#define is_atomic(c) (is_atom(c) || is_number(c))
#define is_list_or_nil(c) (is_list(c) || is_nil(c))
#define is_list_or_nil_or_var(c) (is_list_or_nil(c) || is_variable(c))
#define is_list_or_var(c) (is_list(c) || is_variable(c))
#define is_structure_or_var(c) (is_structure(c) || is_variable(c))
#define is_atom_or_var(c) (is_atom(c) || is_variable(c))
#define is_atom_or_int(c) (is_atom(c) || is_integer(c))
#define is_atom_or_structure(c) (is_atom(c) || is_structure(c))
#define is_number_or_var(c) (is_number(c) || is_variable(c))
#define is_integer_or_var(c) (is_integer(c) || is_variable(c))
#define is_integer_or_atom(c) (is_integer(c) || is_atom(c))
#define is_nonvar(c) !is_variable(c)
#define is_stream(c) (get_stream(q,c) >= 0)
#define is_stream_or_var(c) (is_stream(c) || is_variable(c))
#define is_stream_or_structure(c) (is_stream(c) || is_structure(c))
#define is_list_or_atom(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list_or_var(c) (is_atom(c) || is_iso_list(c) || is_variable(c))
#define is_in_character(c) is_atom(c)
#define is_in_character_or_var(c) (is_in_character(c) || is_variable(c))
#define is_in_byte(c) (is_integer(c) && (c->val_num >= -1) && (c->val_num < 256))
#define is_in_byte_or_var(c) (is_in_byte(c) || is_variable(c))
#define is_byte(c) (is_integer(c) && (c->val_num >= 0) && (c->val_num < 256))
#define is_any(c) 1

#define is_iso_list_or_nil(c) (is_iso_list(c) || is_nil(c))
#define is_iso_list_or_nil_or_var(c) (is_iso_list_or_nil(c) || is_variable(c))
#define is_iso_list_or_var(c) (is_iso_list(c) || is_variable(c))
#define is_iso_atom_or_var(c) (is_iso_atom(c) || is_variable(c))

inline static cell *deref_var(query *q, cell *c, idx_t c_ctx)
{
	const frame *g = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(g, c->var_nbr);

	while (is_variable(&e->c)) {
		c_ctx = e->ctx;
		c = &e->c;
		g = GET_FRAME(c_ctx);
		e = GET_SLOT(g, c->var_nbr);
	}

	if (is_empty(&e->c))
		return q->latest_ctx = c_ctx, c;

	if (is_indirect(&e->c))
		return q->latest_ctx = e->ctx, e->c.val_ptr;

	return q->latest_ctx = e->ctx, &e->c;
}

#define deref(q,c,c_ctx) !is_variable(c) ? (q->latest_ctx = c_ctx, c) : deref_var(q,c,c_ctx)

#define GET_FIRST_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_first_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->latest_ctx; \
	q->accum.val_den = 1; \
	if (!is_##val_type(p)) { return throw_error(q, p, "type_error", #val_type); }

#define GET_FIRST_RAW_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_first_raw_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##val_type(p)) { return throw_error(q, p, "type_error", #val_type); }

#define GET_NEXT_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_next_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->latest_ctx; \
	if (!is_##val_type(p)) { return throw_error(q, p, "type_error", #val_type); }

#define GET_NEXT_RAW_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_next_raw_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##val_type(p)) { return throw_error(q, p, "type_error", #val_type); }

inline static cell *get_first_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_raw_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
	return q->last_arg;
}

inline static cell *get_next_arg(query *q)
{
	q->last_arg += q->last_arg->nbr_cells;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_next_raw_arg(query *q)
{
	q->last_arg += q->last_arg->nbr_cells;
	return q->last_arg;
}

inline static cell *get_raw_arg(query *q, int n)
{
	cell *c = q->st.curr_cell + 1;

	for (int i = 1; i < n; i++)
		c += c->nbr_cells;

	return c;
}

bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);

#define unify(q,p1,p1_ctx,p2,p2_ctx) \
	unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0)

extern void make_int(cell *tmp, int_t v);
extern void make_float(cell *tmp, double v);

#define calc_(q,c) !(c->flags&FLAG_BUILTIN) ? *c : (do_calc_(q,c,c##_ctx), q->accum)

extern USE_RESULT pl_status fn_iso_add_2(query *q);
extern int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern void do_calc_(query *q, cell *c, idx_t c_ctx);

#define calc(q,c) calc_(q, c); 				\
	if (q->did_throw)						\
		return pl_success; 					\
	else if (is_variable(c))				\
		return throw_error(q, c, "instantiation_error", "number"); \
	else if (is_callable(c) && !is_builtin(c))				\
		return throw_error(q, c, "type_error", "evaluable")

