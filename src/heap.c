#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

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
// arena list. Backtracking will garbage collect and free as needed.

cell *alloc_on_heap(query *q, pl_idx_t nbr_cells)
{
	if (!q->arenas) {
		if (q->h_size < nbr_cells)
			q->h_size = nbr_cells;

		arena *a = calloc(1, sizeof(arena));
		ensure(a);
		a->heap = calloc(q->h_size, sizeof(cell));
		ensure(a->heap);
		a->h_size = q->h_size;
		a->nbr = q->st.arena_nbr++;
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
		a->nbr = q->st.arena_nbr++;
		q->arenas = a;
		q->st.hp = 0;
	}

	cell *c = q->arenas->heap + q->st.hp;
	q->st.hp += nbr_cells;
	q->arenas->hp = q->st.hp;

	if (q->st.hp > q->arenas->max_hp_used)
		q->arenas->max_hp_used = q->st.hp;

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

static cell *deep_copy2_to_tmp_with_cycle_check(query *q, cell *p1, pl_idx_t p1_ctx, unsigned depth, bool nonlocals_only, reflist *list)
{
	if (depth >= MAX_DEPTH) {
		q->cycle_error = true;
		return ERR_CYCLE_CELL;
	}

	const pl_idx_t save_idx = tmp_heap_used(q);

	if (is_variable(p1)) {
		if (!is_in_ref_list(p1, p1_ctx, list)) {
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
		}
	}

	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1)) {
		if (!is_variable(p1))
			return tmp;

		if (nonlocals_only && (p1_ctx <= q->st.curr_frame))
			return tmp;

		const frame *f = GET_FRAME(p1_ctx);
		const slot *e = GET_SLOT(f, p1->var_nbr);
		const pl_idx_t slot_nbr = e - q->slots;

		for (size_t i = 0; i < q->st.m->pl->tab_idx; i++) {
			if (q->st.m->pl->tab1[i] == slot_nbr) {
				tmp->var_nbr = q->st.m->pl->tab2[i];
				tmp->flags = FLAG2_FRESH;

				if (is_anon(p1))
					tmp->flags |= FLAG2_ANON;

				tmp->val_off = p1->val_off;
				tmp->attrs = NULL;
				return tmp;
			}
		}

		tmp->var_nbr = q->st.m->pl->varno;
		tmp->flags = FLAG2_FRESH;
		tmp->val_off = p1->val_off;
		tmp->attrs = e->c.attrs;
		tmp->attrs_ctx = e->c.attrs_ctx;

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
		cell *c = p1;
		pl_idx_t c_ctx = p1_ctx;
		bool ok = false;
		reflist nlist;

		if (is_variable(c)) {
			if (!is_in_ref_list(c, c_ctx, list)) {
				nlist.next = list;
				nlist.var_nbr = c->var_nbr;
				nlist.ctx = c_ctx;
				c = deref(q, p1, p1_ctx);
				c_ctx = q->latest_ctx;
				ok = true;
			}
		}

		cell *rec = deep_copy2_to_tmp_with_cycle_check(q, c, c_ctx, depth+1, nonlocals_only, ok ? &nlist : list);
		if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs)
{
	if (!init_tmp_heap(q))
		return NULL;

	frame *f = GET_CURR_FRAME();
	q->st.m->pl->varno = f->nbr_vars;
	q->st.m->pl->tab_idx = 0;
	q->cycle_error = false;
	int nbr_vars = f->nbr_vars;
	bool ok = false;
	reflist nlist, *list = NULL;

	if (is_variable(p1)) {
		nlist.next = list;
		nlist.var_nbr = p1->var_nbr;
		nlist.ctx = p1_ctx;
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		ok = true;
	}

	cell* rec = deep_copy2_to_tmp_with_cycle_check(q, p1, p1_ctx, 0, nonlocals_only, ok ? &nlist : list);
	if (!rec || (rec == ERR_CYCLE_CELL)) return rec;
	int cnt = q->st.m->pl->varno - nbr_vars;

	if (cnt) {
		if (!create_vars(q, cnt)) {
			DISCARD_RESULT throw_error(q, p1, p1_ctx, "resource_error", "too_many_vars");
			return NULL;
		}
	}

	if (!copy_attrs)
		return q->tmp_heap;

	cell *c = rec;

	for (pl_idx_t i = 0; i < rec->nbr_cells; i++, c++) {
		if (is_variable(c) && is_fresh(c) && c->attrs) {
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = c->attrs;
			e->c.attrs_ctx = c->attrs_ctx;
		}
	}

	return q->tmp_heap;
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

static cell *deep_clone2_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, unsigned depth)
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

		dst->flags = FLAG2_FRESH;
	}

	if (q->st.m->pl->varno != f->nbr_vars) {
		if (!create_vars(q, q->st.m->pl->varno-f->nbr_vars)) {
			DISCARD_RESULT throw_error(q, p1, p1_ctx, "resource_error", "too_many_vars");
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

static cell *term_next(query *q, cell *c, pl_idx_t *c_ctx, bool *done)
{
	if (!is_list(c)) {
		*done = true;
		return c;
	}

	LIST_HANDLER(c);
	LIST_HEAD(c);
	c = LIST_TAIL(c);
	c = deref(q, c, *c_ctx);
	*c_ctx = q->latest_ctx;
	return c;
}

// This uses Brent's algorithm...

cell* detect_cycle(query *q, cell *head, pl_idx_t *head_ctx, pl_int_t max, pl_int_t *skip)
{
	if (!head)
		return NULL;

	if (!max) {
		*skip = max;
		return head;
	}

	cell* slow = head;
	pl_idx_t slow_ctx = *head_ctx, fast_ctx = *head_ctx;
	bool done = false;
	cell* fast = term_next(q, head, &fast_ctx, &done);
	int power = 1, length = 1, cnt = 0;

	while (fast && (fast != slow)) {
		if (length == power) {
			power *= 2;
			length = 0;
			slow = fast;
		}

		if ((max == ++cnt) || done){
			if (done)
				--cnt;

			*head_ctx = fast_ctx;
			*skip = cnt;
			return fast;
		}

		fast = term_next(q, fast, &fast_ctx, &done);
		++length;
	}

	if (!fast)
		return NULL;

	// length stores actual length of the loop.
	// Now set slow to the beginning
	// and fast to head+length i.e length of the cycle.

	slow = fast = head;
	int save_length = length;

	while (length > 0) {
		fast = term_next(q, fast, &fast_ctx, &done);
		--length;

		if (length == max)
			break;
	}

	while (fast != slow) {
		fast = term_next(q, fast, &fast_ctx, &done);
		slow = term_next(q, slow, &slow_ctx, &done);
	}

	*head_ctx = slow_ctx;
	*skip = save_length;
	return slow;
}
