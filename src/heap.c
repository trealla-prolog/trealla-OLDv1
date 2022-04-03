#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "internal.h"
#include "query.h"
#include "heap.h"

size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	assert(min_elements <= max_elements);
	size_t elements = max_elements;
	void *mem;

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

cell *init_tmp_heap(query *q)
{
	if (!q->tmp_heap) {
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
		*q->tmp_heap = (cell){0};
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

cell *alloc_on_tmp(query *q, pl_idx_t nbr_cells)
{
	pl_idx_t new_size = q->tmphp + nbr_cells;

	if (new_size >= q->tmph_size) {
		size_t elements = alloc_grow((void**)&q->tmp_heap, sizeof(cell), new_size, (new_size*3)/2);
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
// page list. Backtracking will garbage collect and free as needed.

cell *alloc_on_heap(query *q, pl_idx_t nbr_cells)
{
	if (!q->pages) {
		if (q->h_size < nbr_cells)
			q->h_size = nbr_cells;

		page *a = calloc(1, sizeof(page));
		ensure(a);
		a->heap = calloc(q->h_size, sizeof(cell));
		ensure(a->heap);
		a->h_size = q->h_size;
		a->nbr = q->st.curr_page++;
		q->pages = a;
	}

	if ((q->st.hp + nbr_cells) >= q->h_size) {
		page *a = calloc(1, sizeof(page));
		ensure(a);
		a->next = q->pages;

		if (q->h_size < nbr_cells) {
			q->h_size = nbr_cells;
			q->h_size += nbr_cells / 2;
		}

		a->heap = calloc(q->h_size, sizeof(cell));
		ensure(a->heap);
		a->h_size = q->h_size;
		a->nbr = q->st.curr_page++;
		q->pages = a;
		q->st.hp = 0;
	}

	cell *c = q->pages->heap + q->st.hp;
	q->st.hp += nbr_cells;
	q->pages->hp = q->st.hp;

	if (q->st.hp > q->pages->max_hp_used)
		q->pages->max_hp_used = q->st.hp;

	return c;
}

bool is_in_ref_list(cell *c, pl_idx_t c_ctx, reflist *rlist)
{
	while (rlist && !g_tpl_interrupt) {
		if ((c->var_nbr == rlist->var_nbr)
			&& (c_ctx == rlist->ctx))
			return true;

		rlist = rlist->next;
	}

	return false;
}

static bool is_in_ref_list2(cell *c, pl_idx_t c_ctx, reflist *rlist)
{
	while (rlist && !g_tpl_interrupt) {
		if ((c == rlist->ptr)
			&& (c_ctx == rlist->ctx))
			return true;

		rlist = rlist->next;
	}

	return false;
}

static cell *deep_copy2_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, unsigned depth, bool nonlocals_only, reflist *list)
{
	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return ERR_CYCLE_CELL;
	}

	const pl_idx_t save_idx = tmp_heap_used(q);
	cell *save_p1 = p1;

	if (is_variable(p1)) {
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1)) {
		if (!is_variable(p1))
			return tmp;

		if (nonlocals_only && (p1_ctx != q->st.curr_frame))
			return tmp;

		const frame *f = GET_FRAME(p1_ctx);
		const slot *e = GET_SLOT(f, p1->var_nbr);
		const pl_idx_t slot_nbr = e - q->slots;

		for (size_t i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab1[i] == slot_nbr) {
				tmp->var_nbr = q->st.m->pl->tab2[i];
				tmp->flags = FLAG_VAR_FRESH;

				if (is_anon(p1))
					tmp->flags |= FLAG_VAR_ANON;

				tmp->val_off = p1->val_off;
				return tmp;
			}
		}

		tmp->var_nbr = q->st.m->pl->varno;
		tmp->flags = FLAG_VAR_FRESH;
		tmp->val_off = p1->val_off;
		tmp->tmp_attrs = e->c.attrs;
		tmp->tmp_ctx = e->c.attrs_ctx;

		if (is_anon(p1))
			tmp->flags |= FLAG_VAR_ANON;

		q->st.m->pl->tab1[q->st.m->pl->tab_idx] = slot_nbr;
		q->st.m->pl->tab2[q->st.m->pl->tab_idx] = q->st.m->pl->varno++;
		q->st.m->pl->tab_idx++;
		return tmp;
	}

	pl_idx_t save_p1_ctx = p1_ctx;
	bool cyclic = false;

	if (is_iso_list(p1)) {
		LIST_HANDLER(p1);

		while (is_iso_list(p1) && !g_tpl_interrupt) {
			cell *h = LIST_HEAD(p1);
			cell *c = h;
			pl_idx_t c_ctx = p1_ctx;

			if (is_variable(c)) {
				c = deref(q, c, c_ctx);
				c_ctx = q->latest_ctx;
			}

			if (is_in_ref_list2(c, c_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *h;
				tmp->var_nbr = q->st.m->pl->tab2[0];
				tmp->flags |= FLAG_VAR_FRESH;
				//tmp->attrs = NULL;
			} else {
				cell *rec = deep_copy2_to_tmp(q, c, c_ctx, depth+1, nonlocals_only, list);
				if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
			}

			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;

			if (is_in_ref_list2(p1, p1_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				tmp->tag = TAG_VAR;
				tmp->flags = 0;
				tmp->nbr_cells = 1;
				tmp->val_off = g_anon_s;
				tmp->var_nbr = q->st.m->pl->tab2[0];
				tmp->flags |= FLAG_VAR_FRESH;
				cyclic = true;
				break;
			}

			if (is_iso_list(p1)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		if (!cyclic) {
			cell *rec = deep_copy2_to_tmp(q, p1, p1_ctx, depth+1, nonlocals_only, list);
			if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
		fix_list(tmp);
	} else {
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			cell *c = p1;
			pl_idx_t c_ctx = p1_ctx;

			if (is_variable(c)) {
				c = deref(q, c, c_ctx);
				c_ctx = q->latest_ctx;
			}

			reflist nlist = {0};

			if (is_in_ref_list2(c, c_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *p1;
				tmp->var_nbr = q->st.m->pl->tab2[0];
				tmp->flags |= FLAG_VAR_FRESH;
				//tmp->attrs = NULL;
			} else {
				nlist.next = list;
				nlist.ptr = save_p1;
				nlist.ctx = save_p1_ctx;

				cell *rec = deep_copy2_to_tmp(q, c, c_ctx, depth+1, nonlocals_only, !q->lists_ok ? &nlist : NULL);
				if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
			}

			p1 += p1->nbr_cells;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	}

	return tmp;
}

cell *deep_copy_to_tmp_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *save_p1 = p1;
	pl_idx_t save_p1_ctx = p1_ctx;

	cell *c = deref(q, p1, p1_ctx);
	pl_idx_t c_ctx = q->latest_ctx;

	if (is_iso_list(c)) {
		bool is_partial;

		if (check_list(q, c, c_ctx, &is_partial, NULL))
			q->lists_ok = true;
		else
			q->lists_ok = false;
	} else
		q->lists_ok = true;

	frame *f = GET_CURR_FRAME();
	q->st.m->pl->varno = f->nbr_vars;
	q->st.m->pl->tab_idx = 0;
	q->cycle_error = false;
	int nbr_vars = f->nbr_vars;

	if (from && to) {
		f = GET_FRAME(from_ctx);
		const slot *e = GET_SLOT(f, from->var_nbr);
		const pl_idx_t from_slot_nbr = e - q->slots;

		q->st.m->pl->tab1[q->st.m->pl->tab_idx] = from_slot_nbr;
		q->st.m->pl->tab2[q->st.m->pl->tab_idx] = to->var_nbr;
		q->st.m->pl->tab_idx++;
	}

	if (is_variable(save_p1)) {
		const frame *f = GET_FRAME(p1_ctx);
		const slot *e = GET_SLOT(f, p1->var_nbr);
		const pl_idx_t slot_nbr = e - q->slots;
		q->st.m->pl->tab1[q->st.m->pl->tab_idx] = slot_nbr;
		q->st.m->pl->tab2[q->st.m->pl->tab_idx] = q->st.m->pl->varno++;
		q->st.m->pl->tab_idx++;
	}

	if (is_variable(p1)) {
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	reflist nlist = {0};
	nlist.ptr = c;
	nlist.ctx = c_ctx;

	cell *rec = deep_copy2_to_tmp(q, c, c_ctx, 0, nonlocals_only, !q->lists_ok ? &nlist : NULL);
	q->lists_ok = false;
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	int cnt = q->st.m->pl->varno - nbr_vars;

	if (cnt) {
		if (!create_vars(q, cnt)) {
			DISCARD_RESULT throw_error(q, p1, p1_ctx, "resource_error", "stack");
			return NULL;
		}
	}

	if (is_variable(save_p1)) {
		cell tmp;
		tmp = *save_p1;
		tmp.var_nbr = q->st.m->pl->tab2[0];
		unify(q, &tmp, q->st.curr_frame, rec, q->st.curr_frame);
	}

	if (!copy_attrs)
		return get_tmp_heap_start(q);

	c = rec;

	for (pl_idx_t i = 0; i < rec->nbr_cells; i++, c++) {
		if (is_variable(c) && is_fresh(c) && c->tmp_attrs) {
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = c->tmp_attrs;
			e->c.attrs_ctx = c->tmp_ctx;
		}
	}

	return get_tmp_heap_start(q);
}

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	return deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, nonlocals_only, copy_attrs, NULL, 0, NULL, 0);
}

cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, nonlocals_only, copy_attrs);
	if (!tmp || (tmp == ERR_CYCLE_CELL)) return tmp;

	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx)
{
	cell *tmp = deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, nonlocals_only, copy_attrs, from, from_ctx, to, to_ctx);
	if (!tmp || (tmp == ERR_CYCLE_CELL)) return tmp;

	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

cell *deep_raw_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	frame *f = GET_CURR_FRAME();
	q->st.m->pl->varno = f->nbr_vars;
	q->st.m->pl->tab_idx = 0;
	q->cycle_error = false;

	if (is_variable(p1)) {
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	reflist nlist = {0};
	nlist.ptr = p1;
	nlist.ctx = p1_ctx;

	cell *rec = deep_copy2_to_tmp(q, p1, p1_ctx, 0, false, &nlist);
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	return q->tmp_heap;
}

cell *deep_clone2_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, unsigned depth)
{
	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return ERR_CYCLE_CELL;
	}

	pl_idx_t save_idx = tmp_heap_used(q);
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

cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	q->cycle_error = false;
	cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, 0);
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	return q->tmp_heap;
}

cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx)
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

cell *clone_to_heap(query *q, bool prefix, cell *p1, pl_idx_t suffix)
{
	pl_idx_t nbr_cells = p1->nbr_cells;
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

cell *copy_to_heap(query *q, bool prefix, cell *p1, pl_idx_t p1_ctx, pl_idx_t suffix)
{
	pl_idx_t nbr_cells = p1->nbr_cells;
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
	frame *f = GET_CURR_FRAME();
	q->st.m->pl->varno = f->nbr_vars;
	q->st.m->pl->tab_idx = 0;

	for (pl_idx_t i = 0; i < nbr_cells; i++, dst++, src++) {
		*dst = *src;
		share_cell(src);

		if (!is_variable(src))
			continue;

		slot *e = GET_SLOT(f, src->var_nbr);
		pl_idx_t slot_nbr = e - q->slots;
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

		dst->flags = FLAG_VAR_FRESH;
	}

	if (q->st.m->pl->varno != f->nbr_vars) {
		if (!create_vars(q, q->st.m->pl->varno-f->nbr_vars)) {
			DISCARD_RESULT throw_error(q, p1, p1_ctx, "resource_error", "stack");
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

void fix_list(cell *c)
{
	pl_idx_t cnt = c->nbr_cells;

	while (is_iso_list(c)) {
		c->nbr_cells = cnt;
		c = c + 1;					// skip .
		cnt -= 1 + c->nbr_cells;
		c = c + c->nbr_cells;		// skip head
	}
}

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
	tmp->tag = TAG_LITERAL;
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
	tmp->tag = TAG_LITERAL;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}

bool search_tmp_list(query *q, cell *v)
{
	cell *tmp = get_tmp_heap(q, 0);
	pl_idx_t nbr_cells = tmp_heap_used(q);


	if (!tmp || !tmp_heap_used(q))
		return false;

	for (pl_idx_t i = 0; i < nbr_cells; i++, tmp++) {
		if (tmp->var_nbr == v->var_nbr)
			return true;
	}

	return false;
}

