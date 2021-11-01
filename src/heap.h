#pragma once

USE_RESULT size_t alloc_grow(void** addr, size_t elem_size, size_t min_elements, size_t max_elements);

cell *deep_clone_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, cell *p3, pl_idx_t p3_ctx, cell *p4, pl_idx_t p4_ctx);

cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx);
cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);

cell *do_deep_copy_to_heap(query *q, bool prefix, cell *p1, pl_idx_t p1_ctx, pl_idx_t suffix, bool nonlocals_only, bool copy_attrs, pl_idx_t *nbr_cells);

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);
cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);

cell *clone2_to_tmp(query *q, cell *p1);
cell *clone_to_tmp(query *q, cell *p1);

cell *clone_to_heap(query *q, bool prefix, cell *p1, pl_idx_t suffix);
cell *copy_to_heap(query *q, bool prefix, cell *p1, pl_idx_t p1_ctx, pl_idx_t suffix);

cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);
cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx);

cell *alloc_on_heap(query *q, pl_idx_t nbr_cells);
cell *alloc_on_tmp(query *q, pl_idx_t nbr_cells);
cell *alloc_on_queuen(query *q, int qnbr, const cell *c);

cell *init_tmp_heap(query* q);
inline static pl_idx_t tmp_heap_used(const query *q) { return q->tmphp; }
inline static cell *get_tmp_heap(const query *q, pl_idx_t i) { return q->tmp_heap + i; }

void fix_list(cell *c);
bool search_tmp_list(query *q, cell *v);

void allocate_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
USE_RESULT cell *end_list(query *q);
