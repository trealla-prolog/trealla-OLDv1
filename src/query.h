#pragma once

query *create_query(module *m, bool sub_query);
query *create_sub_query(query *q, cell *curr_cell);
void destroy_query(query *q);

pl_status push_choice(query *q);
pl_status push_barrier(query *q);
pl_status push_call_barrier(query *q);
pl_status push_catcher(query *q, enum q_retry type);

void cut_me(query *q, bool inner_cut, bool soft_cut);
void set_var(query *q, const cell *c, pl_idx_t ctx, cell *v, pl_idx_t v_ctx);
void reset_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx, bool trailing);
pl_status execute(query *q, cell *cells, unsigned nbr_vars);
pl_status fn_call_0(query *q, cell *p1);
void undo_me(query *q);
pl_idx_t drop_choice(query *q);
bool retry_choice(query *q);
void term_assign_vars(parser *p, unsigned start, bool rebase);
pl_status start(query *q);
pl_status match_rule(query *q, cell *p1, pl_idx_t p1_ctx);
pl_status match_clause(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type retract);
pl_status try_me(query *q, unsigned vars);
void call_attrs(query *q, cell *attrs);
void stash_me(query *q, clause *t, bool last_match);
void trim_trail(query *q);
bool unify_internal(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
pl_status do_post_unification_hook(query *q);

bool find_exception_handler(query *q, cell *e);
pl_status throw_error(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected);
pl_status throw_error3(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);
pl_status throw_error2(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);

bool is_valid_list_up_to(query *q, cell *p1, pl_idx_t p1_ctx, bool allow_partials, int n);
bool is_valid_list(query *q, cell *p1, pl_idx_t p1_ctx, bool allow_partials);
size_t scan_is_chars_list2(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes, bool *has_var);
size_t scan_is_chars_list(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes);
char *chars_list_to_string(query *q, cell *p_chars, pl_idx_t p_chars_ctx, size_t len);

unsigned create_vars(query *q, unsigned nbr);
int compare_internal(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, unsigned depth);
void share_predicate(predicate *pr);
void unshare_predicate(query *q, predicate *pr);
cell *skip_max_list(query *q, cell *head, pl_idx_t *head_ctx, pl_int_t max, pl_int_t *skip, cell *tmp);
bool is_cyclic_term(query *q, cell *p1, pl_idx_t p1_ctx);
pl_status do_format(query *q, cell *str, pl_idx_t str_ctx, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len);
pl_status make_cstringn(cell *d, const char *s, size_t n);
pl_status make_stringn(cell *d, const char *s, size_t n);
void make_literal(cell *tmp, pl_idx_t offset);
int get_stream(query *q, cell *p1);
void make_indirect(cell *tmp, cell *c);
bool has_vars(query *q, cell *c, pl_idx_t c_ctx, unsigned depth);
void call_builtin(query *q, cell *c, pl_idx_t c_ctx);
pl_status call_userfun(query *q, cell *c, pl_idx_t c_ctx);
void do_cleanup(query *q, cell *p1);
bool cut_if_det(query *q);
bool is_in_ref_list(cell *c, pl_idx_t c_ctx, reflist *rlist);
bool collect_vars(query *q, cell *p1, pl_idx_t p1_ctx);

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth);
pl_status print_term(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
pl_status print_term_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);
char *print_term_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
void clear_write_options(query *q);

ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool unused, unsigned depth);
pl_status print_canonical(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
char *print_canonical_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
pl_status print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);

pl_status fn_sys_cut_if_det_0(query *q);
pl_status fn_iso_throw_1(query *q);
pl_status fn_sys_call_cleanup_3(query *q);
pl_status fn_iso_catch_3(query *q);
pl_status fn_sys_block_catcher_0(query *q);
pl_status fn_iso_negation_1(query *q);
pl_status fn_iso_disjunction_2(query *q);
pl_status fn_if_3(query *q);
pl_status fn_if_2(query *q);
pl_status fn_iso_if_then_2(query *q);
pl_status fn_iso_invoke_2(query *q);
pl_status fn_iso_call_n(query *q);
pl_status fn_iso_cut_0(query *q);
pl_status fn_sys_inner_cut_0(query *q);
pl_status fn_iso_fail_0(query *q);
pl_status fn_iso_true_0(query *q);
pl_status fn_iso_once_1(query *q);
pl_status fn_ignore_1(query *q);
pl_status fn_sys_undo_trail_1(query *q);
pl_status fn_sys_redo_trail_0(query * q);
pl_status fn_sys_soft_inner_cut_0(query *q);


struct reflist_ {
	reflist *next;
	pl_idx_t var_nbr, ctx;
};

struct cycle_info_ {
	reflist *r1, *r2;
};

inline static int compare(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx)
{
	cycle_info info1 = {0}, info2 = {0};
	q->info1 = &info1;
	q->info2 = &info2;
	int ok = compare_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
	q->info1 = q->info2 = NULL;
	return ok;
}

inline static bool unify(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx)
{
	cycle_info info1 = {0}, info2 = {0};
	q->info1 = &info1;
	q->info2 = &info2;
	q->save_tp = q->st.tp;
	q->run_hook = q->cycle_error = false;
	bool ok = unify_internal(q, p1, p1_ctx, p2, p2_ctx);
	q->info1 = q->info2 = NULL;
	return ok;
}

inline static pl_status make_cstring(cell *d, const char *s)
{
	return make_cstringn(d, s, strlen(s));
}

inline static pl_status make_string(cell *d, const char *s)
{
	return make_stringn(d, s, strlen(s));
}
