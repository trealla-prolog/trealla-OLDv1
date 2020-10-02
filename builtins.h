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
#define is_nil(c) (is_literal(c) && !(c)->arity && ((c)->val_off == g_nil_s))
#define is_structure(c) (is_literal(c) && (c)->arity)
#define is_number(c) (is_rational(c) || is_float(c))
#define is_iso_atomic(c) (is_iso_atom(c) || is_number(c))
#define is_atomic(c) (is_atom(c) || is_number(c))
#define is_list_or_nil(c) (is_list(c) || is_nil(c))
#define is_iso_list_or_nil(c) (is_iso_list(c) || is_nil(c))
#define is_list_or_nil_or_var(c) (is_list_or_nil(c) || is_variable(c))
#define is_iso_list_or_nil_or_var(c) (is_iso_list_or_nil(c) || is_variable(c))
#define is_list_or_var(c) (is_list(c) || is_variable(c))
#define is_iso_list_or_var(c) (is_iso_list(c) || is_variable(c))
#define is_structure_or_var(c) (is_structure(c) || is_variable(c))
#define is_iso_atom_or_var(c) (is_atom(c) || is_variable(c))
#define is_atom_or_var(c) (is_atom(c) || is_variable(c))
#define is_atom_or_int(c) (is_atom(c) || is_integer(c))
#define is_atom_or_catom(c) (is_atom(c) || is_cstring(c))
#define is_atom_or_structure(c) (is_atom(c) || is_structure(c))
#define is_atom_or_catom_or_structure(c) (is_atom(c) || is_cstring(c) || is_structure(c))
#define is_integer_or_var(c) (is_integer(c) || is_variable(c))
#define is_integer_or_atom(c) (is_integer(c) || is_atom(c))
#define is_nonvar(c) (!is_variable(c))
#define is_stream(c) (get_stream(q,c) >= 0)
#define is_stream_or_structure(c) (is_structure(c) || is_stream(c))
#define is_any(c) 1

inline static cell *deref(query *q, cell *c, idx_t c_ctx)
{
	frame *g = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(g, c->slot_nbr);

	while (is_variable(&e->c)) {
		g = GET_FRAME(c_ctx=e->ctx);
		e = GET_SLOT(g, (c=&e->c)->slot_nbr);
	}

	if (is_empty(&e->c))
		return (q->latest_ctx=c_ctx, c);

	if (!is_indirect(&e->c))
		return &e->c;

	return (q->latest_ctx=e->ctx, e->c.val_ptr);
}

#define deref_var(q,c,c_ctx) !is_variable(c) ? (q->latest_ctx = c_ctx, c) : deref(q,c,c_ctx)

#define GET_FIRST_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_first_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->latest_ctx; \
	if (!is_##val_type(p)) { throw_error(q, p, "type_error", #val_type); return 0; }

#define GET_FIRST_RAW_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_first_raw_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##val_type(p)) { throw_error(q, p, "type_error", #val_type); return 0; }

#define GET_NEXT_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_next_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->latest_ctx; \
	if (!is_##val_type(p)) { throw_error(q, p, "type_error", #val_type); return 0; }

#define GET_NEXT_RAW_ARG(p,val_type) \
	__attribute__((unused)) cell *p = get_next_raw_arg(q); \
	__attribute__((unused)) idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##val_type(p)) { throw_error(q, p, "type_error", #val_type); return 0; }

inline static cell *get_first_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
	return deref_var(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_raw_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
	return q->last_arg;
}

inline static cell *get_next_arg(query *q)
{
	q->last_arg += q->last_arg->nbr_cells;
	return deref_var(q, q->last_arg, q->st.curr_frame);
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

