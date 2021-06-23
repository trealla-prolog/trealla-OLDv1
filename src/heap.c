#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"
#include "builtins.h"
#include "heap.h"

size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	assert(min_elements <= max_elements);
	size_t elements = max_elements;
	void* mem;

	do {
		mem = realloc(*addr, elem_size * elements);
		if (mem) break;
		elements = min_elements + (elements-min_elements)/2;
		//message("memory pressure reduce %lu to %lu", max_elements, elements);
	}
	 while (elements > min_elements);

	if (!mem)
		return 0;

	*addr = mem;
	return elements;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.

cell *init_tmp_heap(query* q)
{
	if (!q->tmp_heap) {
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

cell *deep_copy2_to_tmp(query *q, cell *p1, idx_t p1_ctx, unsigned depth, bool nonlocals_only, bool copy_attrs)
{
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

		for (size_t i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab1[i] == slot_nbr) {
				tmp->var_nbr = q->st.m->pl->tab2[i];
				tmp->flags = FLAG2_FRESH;

				if (is_anon(p1))
					tmp->flags |= FLAG2_ANON;

				tmp->val_off = g_nil_s;
				tmp->attrs = e->c.attrs;
				return tmp;
			}
		}

		tmp->var_nbr = q->st.m->pl->varno;
		tmp->flags = FLAG2_FRESH;
		tmp->val_off = g_nil_s;
		tmp->attrs = e->c.attrs;

		if (is_anon(p1))
			tmp->flags |= FLAG2_ANON;

		q->st.m->pl->tab1[q->st.m->pl->tab_idx] = slot_nbr;
		q->st.m->pl->tab2[q->st.m->pl->tab_idx] = q->st.m->pl->varno++;
		q->st.m->pl->tab_idx++;
		return tmp;
	}

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = deref(q, p1, p1_ctx);
		cell *rec = deep_copy2_to_tmp(q, c, q->latest_ctx, depth+1, nonlocals_only, copy_attrs);
		if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_copy_to_tmp(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	if (!init_tmp_heap(q))
		return NULL;

	frame *g = GET_CURR_FRAME();
	q->st.m->pl->varno = g->nbr_vars;
	q->st.m->pl->tab_idx = 0;
	q->cycle_error = false;
	cell* rec = deep_copy2_to_tmp(q, p1, p1_ctx, 0, nonlocals_only, copy_attrs);
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	int cnt = q->st.m->pl->varno - g->nbr_vars;

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

cell *do_deep_copy_to_heap(query *q, bool prefix, cell *p1, idx_t p1_ctx, idx_t suffix, bool nonlocals_only, bool copy_attrs, idx_t *nbr_cells)
{
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, nonlocals_only, copy_attrs);
	if (!tmp || (tmp == ERR_CYCLE_CELL)) return tmp;
	cell *tmp2 = alloc_on_heap(q, (prefix?1:0)+tmp->nbr_cells+suffix);
	if (!tmp2) return NULL;

	if (prefix) {
		*nbr_cells = 1;
		// Needed for follow() to work
		*tmp2 = (cell){0};
		tmp2->tag = TAG_EMPTY;
		tmp2->nbr_cells = 1;
		tmp2->flags = FLAG_BUILTIN;
	} else
		*nbr_cells = 0;

	*nbr_cells += safe_copy_cells(tmp2+(prefix?1:0), tmp, tmp->nbr_cells);
	return tmp2;
}

static cell *deep_clone2_to_tmp(query *q, cell *p1, idx_t p1_ctx, unsigned depth)
{
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
		if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
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
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	return q->tmp_heap;
}

cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx)
{
	p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	if (!p1 || (p1 == ERR_CYCLE_CELL)) return p1;
	cell *tmp = alloc_on_heap(q, p1->nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

cell *clone2_to_tmp(query *q, cell *p1)
{
	cell *tmp = alloc_on_tmp(q, p1->nbr_cells);
	ensure(tmp);
	copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

cell *clone_to_tmp(query *q, cell *p1)
{
	if (!init_tmp_heap(q)) return NULL;
	return clone2_to_tmp(q, p1);
}

cell *clone_to_heap(query *q, bool prefix, cell *p1, idx_t suffix)
{
	idx_t nbr_cells = p1->nbr_cells;
	cell *tmp = alloc_on_heap(q, (prefix?1:0)+nbr_cells+suffix);
	ensure(tmp);

	if (prefix) {
		// Needed for follow() to work
		*tmp = (cell){0};
		tmp->tag = TAG_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	safe_copy_cells(tmp+(prefix?1:0), p1, nbr_cells);
	return tmp;
}

cell *copy_to_heap(query *q, bool prefix, cell *p1, idx_t p1_ctx, idx_t suffix)
{
	idx_t nbr_cells = p1->nbr_cells;
	cell *tmp = alloc_on_heap(q, (prefix?1:0)+nbr_cells+suffix);
	ensure(tmp);

	if (prefix) {
		// Needed for follow() to work
		*tmp = (cell){0};
		tmp->tag = TAG_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	cell *src = p1, *dst = tmp+(prefix?1:0);
	frame *g = GET_CURR_FRAME();
	q->st.m->pl->varno = g->nbr_vars;
	q->st.m->pl->tab_idx = 0;

	for (idx_t i = 0; i < nbr_cells; i++, dst++, src++) {
		*dst = *src;
		share_cell(src);

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

cell *alloc_on_queuen(query *q, int qnbr, const cell *c)
{
	if (!q->queue[qnbr]) {
		q->queue[qnbr] = calloc(q->q_size[qnbr], sizeof(cell));
		ensure(q->queue[qnbr]);
	}

	while ((q->qp[qnbr]+c->nbr_cells) >= q->q_size[qnbr]) {
		q->q_size[qnbr] += q->q_size[qnbr] / 2;
		q->queue[qnbr] = realloc(q->queue[qnbr], sizeof(cell)*q->q_size[qnbr]);
		ensure(q->queue[qnbr]);
	}

	cell *dst = q->queue[qnbr] + q->qp[qnbr];
	q->qp[qnbr] += safe_copy_cells(dst, c, c->nbr_cells);
	return dst;
}

