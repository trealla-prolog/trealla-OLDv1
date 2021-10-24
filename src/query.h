#pragma once

extern query *create_query(module *m, bool sub_query);
extern query *create_sub_query(query *q, cell *curr_cell);
extern void destroy_query(query *q);

extern USE_RESULT pl_status make_choice(query *q);
extern USE_RESULT pl_status make_barrier(query *q);
extern USE_RESULT pl_status make_call_barrier(query *q);
extern USE_RESULT pl_status make_catcher(query *q, enum q_retry type);
extern void cut_me(query *q, bool inner_cut, bool soft_cut);

extern void set_var(query *q, const cell *c, idx_t ctx, cell *v, idx_t v_ctx);
extern void reset_var(query *q, const cell *c, idx_t c_ctx, cell *v, idx_t v_ctx);
extern pl_status execute(query *q, cell *cells, unsigned nbr_vars);
extern USE_RESULT pl_status fn_call_0(query *q, cell *p1);
extern void undo_me(query *q);
extern idx_t drop_choice(query *q);
extern bool retry_choice(query *q);
extern void term_assign_vars(parser *p, unsigned start, bool rebase);
extern USE_RESULT pl_status start(query *q);
extern USE_RESULT pl_status match_rule(query *q, cell *p1, idx_t p1_ctx);
extern USE_RESULT pl_status match_clause(query *q, cell *p1, idx_t p1_ctx, enum clause_type retract);
extern unsigned create_vars(query *q, unsigned nbr);
extern void try_me(query *q, unsigned vars);
extern USE_RESULT bool find_exception_handler(query *q, cell *e);
extern USE_RESULT pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern pl_status throw_error3(query *q, cell *c, const char *err_type, const char *expected, cell *goal);
extern pl_status throw_error2(query *q, cell *c, const char *err_type, const char *expected, cell *goal);
extern void call_attrs(query *q, cell *attrs);
extern void stash_me(query *q, rule *t, bool last_match);
extern bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern pl_status do_format(query *q, cell *str, idx_t str_ctx, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx);
extern size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len);
extern USE_RESULT pl_status make_cstringn(cell *d, const char *s, size_t n);
extern USE_RESULT pl_status make_stringn(cell *d, const char *s, size_t n);
extern void make_literal(cell *tmp, idx_t offset);
extern int get_stream(query *q, cell *p1);
extern bool is_cyclic_term(query *q, cell *p1, idx_t p1_ctx);
extern void trim_trail(query *q);

extern USE_RESULT pl_status fn_sys_cut_if_det_0(query *q);
extern USE_RESULT pl_status fn_iso_throw_1(query *q);
extern USE_RESULT pl_status fn_iso_catch2_3(query *q);
extern USE_RESULT pl_status fn_iso_catch_3(query *q);
extern USE_RESULT pl_status fn_sys_block_catcher_0(query *q);
extern USE_RESULT pl_status fn_iso_negation_1(query *q);
extern USE_RESULT pl_status fn_iso_disjunction_2(query *q);
extern USE_RESULT pl_status fn_if_3(query *q);
extern USE_RESULT pl_status fn_if_2(query *q);
extern USE_RESULT pl_status fn_iso_if_then_2(query *q);
extern USE_RESULT pl_status fn_iso_invoke_2(query *q);
extern USE_RESULT pl_status fn_sys_call_1(query *q);
extern USE_RESULT pl_status fn_iso_call_n(query *q);
extern USE_RESULT pl_status fn_iso_cut_0(query *q);
extern USE_RESULT pl_status fn_sys_inner_cut_0(query *q);
extern USE_RESULT pl_status fn_iso_fail_0(query *q);
extern USE_RESULT pl_status fn_iso_true_0(query *q);
extern USE_RESULT pl_status fn_iso_once_1(query *q);
extern USE_RESULT pl_status fn_ignore_1(query *q);

extern bool is_valid_list_up_to(query *q, cell *p1, idx_t p1_ctx, bool allow_partials, int n);
extern bool is_valid_list(query *q, cell *p1, idx_t p1_ctx, bool allow_partials);
extern size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, bool allow_codes);

extern void make_indirect(cell *tmp, cell *c);
extern unsigned fake_numbervars(query *q, cell *c, idx_t c_ctx, unsigned start);
extern bool has_vars(query *q, cell *c, idx_t c_ctx, unsigned depth);
extern pl_status do_post_unification_hook(query *q);
extern pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern void call_builtin(query *q, cell *c, idx_t c_ctx);
extern pl_status call_userfun(query *q, cell *c, idx_t c_ctx);
extern void add_to_dirty_list(query *q, clause *r);
extern void do_cleanup(query *q, cell *p1);
extern void cut_if_det(query *q);

extern ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool cons, unsigned depth);
extern pl_status print_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern pl_status print_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);
extern char *print_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);
extern void clear_write_options(query *q);

extern ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool unused, unsigned depth);
extern pl_status print_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern char *print_canonical_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);
extern pl_status print_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);


inline static USE_RESULT pl_status make_cstring(cell *d, const char *s)
{
	return make_cstringn(d, s, strlen(s));
}

inline static USE_RESULT pl_status make_string(cell *d, const char *s)
{
	return make_stringn(d, s, strlen(s));
}

typedef struct ref_ ref;

