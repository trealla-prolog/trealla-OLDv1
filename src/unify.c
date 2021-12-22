#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "internal.h"
#include "history.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "builtins.h"
#include "heap.h"
#include "utf8.h"

static bool is_cyclic_term_internal(query *q, cell *p1, pl_idx_t p1_ctx, reflist *list)
{
	if (!is_structure(p1))
		return false;

	pl_idx_t nbr_cells = p1->nbr_cells - 1;
	p1++;

	while (nbr_cells) {
		if (is_variable(p1)) {
			if (is_in_ref_list(p1, p1_ctx, list))
				return q->cycle_error = true;

			reflist nlist;
			nlist.next = list;
			nlist.var_nbr = p1->var_nbr;
			nlist.ctx = p1_ctx;

			cell *c = deref(q, p1, p1_ctx);
			pl_idx_t c_ctx = q->latest_ctx;

			if (is_cyclic_term_internal(q, c, c_ctx, &nlist)) {
				return true;
			}
		}

		nbr_cells--;
		p1++;
	}

	return false;
}

bool is_cyclic_term(query *q, cell *p1, pl_idx_t p1_ctx)
{
	q->cycle_error = false;
	return is_cyclic_term_internal(q, p1, p1_ctx, NULL);
}

// TODO : change this to make a list of vars as we go...

static void collect_vars_internal(query *q, cell *p1, pl_idx_t p1_ctx, reflist *list)
{
	pl_idx_t nbr_cells = p1->nbr_cells;

	while (nbr_cells) {
		if (is_variable(p1)) {
			if (is_in_ref_list(p1, p1_ctx, list))
				return;

			reflist nlist;
			nlist.next = list;
			nlist.var_nbr = p1->var_nbr;
			nlist.ctx = p1_ctx;
			cell *c = deref(q, p1, p1_ctx);
			pl_idx_t c_ctx = q->latest_ctx;

			if (is_structure(c))
				collect_vars_internal(q, c, c_ctx, &nlist);
		}

		cell *c = deref(q, p1, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (is_variable(c)) {
			bool found = false;

			for (unsigned idx = 0; idx < q->pl->tab_idx; idx++) {
				if ((q->pl->tab1[idx] == c_ctx) && (q->pl->tab2[idx] == c->var_nbr)) {
					q->pl->tab4[idx]++;
					found = true;
					break;
				}
			}

			if (!found) {
				q->pl->tab1[q->pl->tab_idx] = c_ctx;
				q->pl->tab2[q->pl->tab_idx] = c->var_nbr;
				q->pl->tab3[q->pl->tab_idx] = c->val_off;
				q->pl->tab4[q->pl->tab_idx] = 1;
				q->pl->tab5[q->pl->tab_idx] = is_anon(c) ? 1 : 0;
				q->pl->tab_idx++;
			}
		}

		nbr_cells--;
		p1++;
	}
}

bool collect_vars(query *q, cell *p1, pl_idx_t p1_ctx)
{
	collect_vars_internal(q, p1, p1_ctx, NULL);
	return true;
}

static bool unify_structs(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, unsigned depth)
{
	if (!depth)
		q->cycle_error = false;

	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return true;
	}

	if (p1->arity != p2->arity)
		return false;

	if (p1->val_off != p2->val_off)
		return false;

	unsigned arity = p1->arity;
	p1++; p2++;

	while (arity-- && !g_tpl_interrupt) {
		cell *c1 = deref(q, p1, p1_ctx);
		pl_idx_t c1_ctx = q->latest_ctx;
		cell *c2 = deref(q, p2, p2_ctx);
		pl_idx_t c2_ctx = q->latest_ctx;
		reflist r1 = {0}, r2 = {0};

		if (q->info1) {
			if (is_variable(p1)) {
				if (is_in_ref_list(p1, p1_ctx, q->info1->r1)) {
					c1 = p1;
					c1_ctx = p1_ctx;
				} else {
					r1.next = q->info1->r1;
					r1.var_nbr = p1->var_nbr;
					r1.ctx = p1_ctx;
					q->info1->r1 = &r1;
				}
			}
		}

		if (q->info2) {
			if (is_variable(p2)) {
				if (is_in_ref_list(p2, p2_ctx, q->info2->r2)) {
					c2 = p2;
					c2_ctx = p2_ctx;
				} else {
					r2.next = q->info2->r2;
					r2.var_nbr = p2->var_nbr;
					r2.ctx = p2_ctx;
					q->info2->r2 = &r2;
				}
			}
		}

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;

		if (q->cycle_error)
			return true;

		if (q->info1) {
			if (is_variable(p1))
				q->info1->r1 = r1.next;		// restore
		}

		if (q->info2) {
			if (is_variable(p2))
				q->info2->r2 = r2.next;		// restore
		}

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return true;
}

// This is for when one arg is a string & the other an iso-list...

static bool unify_string_to_list(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, unsigned depth)
{
	if (p1->arity != p2->arity)
		return false;

	unsigned save_depth = depth;
	LIST_HANDLER(p1);
	LIST_HANDLER(p2);

	while (is_list(p1) && is_list(p2) && !g_tpl_interrupt) {
		if (depth >= MAX_DEPTH) {
			q->cycle_error = true;
			return true;
		}

		cell *c1 = LIST_HEAD(p1);
		cell *c2 = LIST_HEAD(p2);

		c1 = deref(q, c1, p1_ctx);
		pl_idx_t c1_ctx = q->latest_ctx;
		c2 = deref(q, c2, p2_ctx);
		pl_idx_t c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1)) {
			if (q->cycle_error)
				return true;

			return false;
		}

		if (q->cycle_error)
			return true;

		c1 = LIST_TAIL(p1);
		c2 = LIST_TAIL(p2);

		p1 = deref(q, c1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, c2, p2_ctx);
		p2_ctx = q->latest_ctx;
		depth++;
	}

	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, save_depth+1);
}

static bool unify_integers(__attribute__((unused)) query *q, cell *p1, cell *p2)
{
	if (is_bigint(p1) && is_bigint(p2))
		return !mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);

	if (is_bigint(p1) && is_integer(p2))
		return !mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);

	if (is_bigint(p2) && is_integer(p1))
		return !mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (is_integer(p2))
		return (get_int(p1) == get_int(p2));

	return false;
}

static bool unify_reals(__attribute__((unused)) query *q, cell *p1, cell *p2)
{
	if (is_real(p2))
		return get_real(p1) == get_real(p2);

	return false;
}

static bool unify_literals(query *q, cell *p1, cell *p2)
{
	if (is_literal(p2))
		return p1->val_off == p2->val_off;

	if (is_cstring(p2) && (LEN_STR(q, p1) == LEN_STR(q, p2)))
		return !memcmp(GET_STR(q, p2), GET_POOL(q, p1->val_off), LEN_STR(q, p1));

	return false;
}

static bool unify_cstrings(query *q, cell *p1, cell *p2)
{
	if (is_cstring(p2) && (LEN_STR(q, p1) == LEN_STR(q, p2)))
		return !memcmp(GET_STR(q, p1), GET_STR(q, p2), LEN_STR(q, p1));

	if (is_literal(p2) && (LEN_STR(q, p1) == LEN_STR(q, p2)))
		return !memcmp(GET_STR(q, p1), GET_POOL(q, p2->val_off), LEN_STR(q, p1));

	return false;
}

struct dispatch {
	uint8_t tag;
	bool (*fn)(query*, cell*, cell*);
};

static const struct dispatch g_disp[] =
{
	{TAG_EMPTY, NULL},
	{TAG_VAR, NULL},
	{TAG_LITERAL, unify_literals},
	{TAG_CSTR, unify_cstrings},
	{TAG_INT, unify_integers},
	{TAG_REAL, unify_reals},
	{0}
};

bool unify_internal(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, unsigned depth)
{
	if (!depth)
		q->cycle_error = false;

	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return true;
	}

	if (p1_ctx == q->st.curr_frame)
		q->no_tco = true;

	if (is_variable(p1) && !is_variable(p2))
		q->has_vars = true;

	if (is_variable(p1) && is_variable(p2)) {
		if (p2_ctx > p1_ctx)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2_ctx < p1_ctx)
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		else if (p2->var_nbr != p1->var_nbr)
			set_var(q, p2, p2_ctx, p1, p1_ctx);

		return true;
	}

	if (is_variable(p1)) {
		set_var(q, p1, p1_ctx, p2, p2_ctx);
		return true;
	}

	if (is_variable(p2)) {
		set_var(q, p2, p2_ctx, p1, p1_ctx);
		return true;
	}

	q->check_unique = true;

	if (is_string(p1) && is_string(p2))
		return unify_cstrings(q, p1, p2);

	if (is_string(p1) && is_list(p2))
		return unify_string_to_list(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	if (is_string(p2) && is_list(p1))
		return unify_string_to_list(q, p2, p2_ctx, p1, p1_ctx, depth+1);

	if (p1->arity || p2->arity)
		return unify_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	return g_disp[p1->tag].fn(q, p1, p2);
}

