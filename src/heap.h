#pragma once

USE_RESULT size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements);

USE_RESULT cell *clone2_to_tmp(query *q, cell *p1);
USE_RESULT cell *clone_to_tmp(query *q, cell *p1);

USE_RESULT cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);
USE_RESULT cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx);

USE_RESULT cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);
USE_RESULT cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);

USE_RESULT cell *deep_copy_to_tmp_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx);
USE_RESULT cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx);

USE_RESULT cell *clone_to_heap(query *q, bool prefix, cell *p1, pl_idx_t suffix);
USE_RESULT cell *copy_to_heap(query *q, bool prefix, cell *p1, pl_idx_t p1_ctx, pl_idx_t suffix);

USE_RESULT cell *deep_raw_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);

USE_RESULT cell *alloc_on_heap(query *q, pl_idx_t nbr_cells);
USE_RESULT cell *alloc_on_tmp(query *q, pl_idx_t nbr_cells);
USE_RESULT cell *alloc_on_queuen(query *q, int qnbr, const cell *c);

USE_RESULT cell *init_tmp_heap(query *q);

#define get_tmp_heap_start(q) (q)->tmp_heap
#define get_tmp_heap(q,i) ((q)->tmp_heap + (i))
#define tmp_heap_used(q) (q)->tmphp

void fix_list(cell *c);
bool search_tmp_list(query *q, cell *v);

void allocate_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
USE_RESULT cell *end_list(query *q);
