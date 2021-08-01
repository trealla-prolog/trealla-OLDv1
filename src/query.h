#pragma once

extern query *create_query(module *m, bool sub_query);
extern query *create_sub_query(query *q, cell *curr_cell);
extern void destroy_query(query *q);

extern USE_RESULT pl_status make_choice(query *q);
extern USE_RESULT pl_status make_barrier(query *q);
extern USE_RESULT pl_status make_catcher(query *q, enum q_retry type);
extern void cut_me(query *q, bool local_cut, bool soft_cut);

extern void set_var(query *q, const cell *c, idx_t ctx, cell *v, idx_t v_ctx);
extern void reset_var(query *q, const cell *c, idx_t c_ctx, cell *v, idx_t v_ctx);
extern pl_status execute(query *q, rule *t);
extern USE_RESULT pl_status fn_call_0(query *q, cell *p1);
extern void undo_me(query *q);
extern idx_t drop_choice(query *q);
extern bool retry_choice(query *q);
extern void term_assign_vars(parser *p, unsigned start, bool rebase);
extern USE_RESULT pl_status start(query *q);
extern USE_RESULT pl_status match_rule(query *q, cell *p1, idx_t p1_ctx);
extern USE_RESULT pl_status match_clause(query *q, cell *p1, idx_t p1_ctx, enum clause_type retract);
extern unsigned create_vars(query *q, unsigned nbr);
extern void try_me(const query *q, unsigned vars);
extern USE_RESULT pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern void call_attrs(query *q, cell *attrs);
extern void stash_me(query *q, rule *t, bool last_match);
extern bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern void acquire_predicate(__attribute__((unused)) query *q, predicate *pr);
extern void release_predicate(query *q, predicate *pr);

extern bool is_valid_list(query *q, cell *p1, idx_t p1_ctx, bool allow_partials);
extern size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, bool allow_codes);

extern void make_indirect(cell *tmp, cell *c);
extern unsigned fake_numbervars(query *q, cell *c, idx_t c_ctx, unsigned start);
extern bool has_vars(query *q, cell *c, idx_t c_ctx, unsigned depth);
extern pl_status do_post_unification_hook(query *q);
extern pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern void call_builtin(query *q, cell *c, idx_t c_ctx);
extern pl_status call_function(query *q, cell *c, idx_t c_ctx);
extern void add_to_dirty_list(query *q, clause *r);

extern ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool cons, unsigned depth);
extern pl_status print_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern pl_status print_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);
extern char *print_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);

extern ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool unused, unsigned depth);
extern pl_status print_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern char *print_canonical_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);
extern pl_status print_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);

