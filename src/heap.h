#pragma once

USE_RESULT size_t alloc_grow(void** addr, size_t elem_size, size_t min_elements, size_t max_elements);

cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx);
cell *deep_copy_to_heap(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);

cell *deep_copy_to_tmp(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);
cell *deep_clone_to_tmp(query *q, cell *p1, idx_t p1_ctx);

cell *clone2_to_tmp(query *q, cell *p1);
cell *clone_to_tmp(query *q, cell *p1);

cell *clone_to_heap(query *q, bool prefix, cell *p1, idx_t suffix);
cell *copy_to_heap(query *q, bool prefix, cell *p1, idx_t p1_ctx, idx_t suffix);

cell *deep_clone_to_tmp(query *q, cell *p1, idx_t p1_ctx);
cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx);

cell *alloc_on_heap(query *q, idx_t nbr_cells);
cell *alloc_on_tmp(query *q, idx_t nbr_cells);
cell *alloc_on_queuen(query *q, int qnbr, const cell *c);

cell *init_tmp_heap(query* q);
inline static idx_t tmp_heap_used(const query *q) { return q->tmphp; }
inline static cell *get_tmp_heap(const query *q, idx_t i) { return q->tmp_heap + i; }

