#pragma once

USE_RESULT size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements);

USE_RESULT cell *clone2_to_tmp(query *q, cell *p1);
USE_RESULT cell *clone_to_tmp(query *q, cell *p1);
USE_RESULT cell *clone_to_heap(query *q, bool prefix, cell *p1, pl_idx_t suffix);

USE_RESULT cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);
USE_RESULT cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx);

USE_RESULT cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);
USE_RESULT cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);

USE_RESULT cell *deep_raw_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);

USE_RESULT cell *alloc_on_heap(query *q, pl_idx_t nbr_cells);
USE_RESULT cell *alloc_on_tmp(query *q, pl_idx_t nbr_cells);
USE_RESULT cell *alloc_on_queuen(query *q, int qnbr, const cell *c);

USE_RESULT cell *init_tmp_heap(query *q);

// Used for copying attributes and doesn't init tmp heap...
USE_RESULT cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx);

#define get_tmp_heap_start(q) (q)->tmp_heap
#define get_tmp_heap(q,i) ((q)->tmp_heap + (i))
#define tmp_heap_used(q) (q)->tmphp

struct heap_save {
	cell *heap;
	pl_idx_t size, hp;
};

#define push_tmp_heap(q) 								\
	struct heap_save _s;								\
	_s.heap = q->tmp_heap;								\
	_s.size = q->tmph_size;								\
	_s.hp = q->tmphp;									\
	q->tmp_heap = NULL;									\
	q->tmphp = 0;										\
	if (!init_tmp_heap(q)) return NULL;

#define pop_tmp_heap(q)									\
	free(q->tmp_heap);									\
	q->tmp_heap = _s.heap;								\
	q->tmph_size = _s.size;								\
	q->tmphp = _s.hp;

void fix_list(cell *c);
bool search_tmp_list(query *q, cell *v);

void allocate_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
USE_RESULT cell *end_list(query *q);
