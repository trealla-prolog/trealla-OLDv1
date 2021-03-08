#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "builtins.h"

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.

cell *init_tmp_heap(query* q)
{
	if (!q->tmp_heap) {
		FAULTINJECT(errno = ENOMEM; return NULL);
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
		*q->tmp_heap = (cell){0};
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

cell *alloc_on_tmp(query *q, idx_t nbr_cells)
{
	idx_t new_size = q->tmphp + nbr_cells;
	if (new_size >= q->tmph_size) {
		size_t elements = alloc_grow((void**)&q->tmp_heap, sizeof(cell), new_size, new_size*2);
		if (!elements) return NULL;
		q->tmph_size = elements;
	}

	cell *c = q->tmp_heap + q->tmphp;
	q->tmphp = new_size;
	return c;
}

// The heap is used for long-life allocations and a realloc() can't be
// done as it will invalidate existing pointers. Build any compounds
// first on the tmp heap, then allocate in one go here and copy in.
// When more space is need allocate a new heap and keep them in the
// arena list. Backtracking will garbage collect and free as needed.

cell *alloc_on_heap(query *q, idx_t nbr_cells)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	if (!q->arenas) {
		if (q->h_size < nbr_cells)
			q->h_size = nbr_cells;

		arena *a = calloc(1, sizeof(arena));
		ensure(a);
		a->heap = calloc(q->h_size, sizeof(cell));
		ensure(a->heap);
		a->h_size = q->h_size;
		a->nbr = q->st.anbr++;
		q->arenas = a;
	}

	if ((q->st.hp + nbr_cells) >= q->h_size) {
		arena *a = calloc(1, sizeof(arena));
		ensure(a);
		a->next = q->arenas;

		if (q->h_size < nbr_cells) {
			q->h_size = nbr_cells;
			q->h_size += nbr_cells / 2;
		}

		a->heap = calloc(q->h_size, sizeof(cell));
		ensure(a->heap);
		a->h_size = q->h_size;
		a->nbr = q->st.anbr++;
		q->arenas = a;
		q->st.hp = 0;
	}

	cell *c = q->arenas->heap + q->st.hp;
	q->st.hp += nbr_cells;
	q->arenas->hp = q->st.hp;
	return c;
}

static cell *deep_copy2_to_tmp(query *q, cell *p1, idx_t p1_ctx, unsigned depth, bool nonlocals_only, bool copy_attrs)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	if (depth >= 64000) {
		q->cycle_error = true;
		return ERR_CYCLE_CELL;
	}

	idx_t save_idx = tmp_heap_used(q);
	p1 = deref(q, p1, p1_ctx);
	p1_ctx = q->latest_ctx;

	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1)) {
		if (!is_variable(p1))
			return tmp;

		if (nonlocals_only && (p1_ctx <= q->st.curr_frame))
			return tmp;

		frame *g = GET_FRAME(p1_ctx);
		slot *e = GET_SLOT(g, p1->var_nbr);
		idx_t slot_nbr = e - q->slots;

		for (size_t i = 0; i < q->m->pl->tab_idx; i++) {
			if (q->m->pl->tab1[i] == slot_nbr) {
				tmp->var_nbr = q->m->pl->tab2[i];
				tmp->flags = FLAG2_FRESH;

				if (is_anon(p1))
					tmp->flags |= FLAG2_ANON;

				tmp->val_off = g_nil_s;
				tmp->attrs = e->c.attrs;
				return tmp;
			}
		}

		tmp->var_nbr = q->m->pl->varno;
		tmp->flags = FLAG2_FRESH;
		tmp->val_off = g_nil_s;
		tmp->attrs = e->c.attrs;

		if (is_anon(p1))
			tmp->flags |= FLAG2_ANON;

		q->m->pl->tab1[q->m->pl->tab_idx] = slot_nbr;
		q->m->pl->tab2[q->m->pl->tab_idx] = q->m->pl->varno++;
		q->m->pl->tab_idx++;
		return tmp;
	}

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = deref(q, p1, p1_ctx);
		cell *rec = deep_copy2_to_tmp(q, c, q->latest_ctx, depth+1, nonlocals_only, copy_attrs);
		if (!rec || rec == ERR_CYCLE_CELL) return rec;
		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_copy_to_tmp(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	if (!init_tmp_heap(q))
		return NULL;

	frame *g = GET_CURR_FRAME();
	q->m->pl->varno = g->nbr_vars;
	q->m->pl->tab_idx = 0;
	q->cycle_error = false;
	cell* rec = deep_copy2_to_tmp(q, p1, p1_ctx, 0, nonlocals_only, copy_attrs);
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	int cnt = q->m->pl->varno - g->nbr_vars;

	if (cnt) {
		if (!create_vars(q, cnt)) {
			DISCARD_RESULT throw_error(q, p1, "resource_error", "too_many_vars");
			return NULL;
		}
	}

	return q->tmp_heap;
}

cell *deep_copy_to_heap(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, nonlocals_only, copy_attrs);
	if (!tmp || (tmp == ERR_CYCLE_CELL)) return tmp;
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

static cell *deep_clone2_to_tmp(query *q, cell *p1, idx_t p1_ctx, unsigned depth)
{
	FAULTINJECT(errno = ENOMEM; return NULL);
	if (depth >= 64000) {
		q->cycle_error = true;
		return ERR_CYCLE_CELL;
	}

	idx_t save_idx = tmp_heap_used(q);
	p1 = deref(q, p1, p1_ctx);
	p1_ctx = q->latest_ctx;
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1))
		return tmp;

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = deref(q, p1, p1_ctx);
		cell *rec = deep_clone2_to_tmp(q, c, q->latest_ctx, depth+1);
		if (!rec || rec == ERR_CYCLE_CELL) return rec;
		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_clone_to_tmp(query *q, cell *p1, idx_t p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	q->cycle_error = false;
	cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, 0);
	if (!rec || rec == ERR_CYCLE_CELL) return rec;
	return q->tmp_heap;
}

cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx)
{
	p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	if (!p1 || p1 == ERR_CYCLE_CELL) return p1;
	cell *tmp = alloc_on_heap(q, p1->nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

