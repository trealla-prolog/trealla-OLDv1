#pragma once

extern query *create_query(module *m, bool sub_query);
extern query *create_task(query *q, cell *curr_cell);
extern void destroy_query(query *q);

extern void set_var(query *q, const cell *c, idx_t ctx, cell *v, idx_t v_ctx);
extern void reset_value(query *q, const cell *c, idx_t c_ctx, cell *v, idx_t v_ctx);
extern USE_RESULT pl_status make_choice(query *q);
extern USE_RESULT pl_status make_barrier(query *q);
extern USE_RESULT pl_status make_catcher(query *q, enum q_retry type);
extern void cut_me(query *q, bool local_cut, bool soft_cut);
extern pl_status query_execute(query *q, term *t);
extern USE_RESULT pl_status fn_call_0(query *q, cell *p1);
extern void undo_me(query *q);
extern idx_t drop_choice(query *q);
extern bool retry_choice(query *q);
extern void term_assign_vars(parser *p, unsigned start, bool rebase);
extern 	USE_RESULT pl_status query_start(query *q);
extern USE_RESULT pl_status match_rule(query *q, cell *p1, idx_t p1_ctx);
extern USE_RESULT pl_status match_clause(query *q, cell *p1, idx_t p1_ctx, enum clause_type retract);
extern unsigned create_vars(query *q, unsigned nbr);
extern void try_me(const query *q, unsigned vars);
extern USE_RESULT pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern void call_attrs(query *q, cell *attrs);
extern void allocate_list(query *q, const cell *c);
extern void append_list(query *q, const cell *c);
extern USE_RESULT cell *end_list(query *q);
extern bool is_valid_list(query *q, cell *p1, idx_t p1_ctx, bool partial_list);
extern size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, bool allow_codes);
extern void make_indirect(cell *tmp, cell *c);
extern void stash_me(query *q, term *t, bool last_match);
extern unsigned fake_numbervars(query *q, cell *c, idx_t c_ctx, unsigned start);
extern bool has_vars(query *q, cell *c, idx_t c_ctx, unsigned depth);
extern pl_status do_post_unification_hook(query *q);
extern pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
extern bool unify_internal(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth);
extern USE_RESULT pl_status fn_iso_add_2(query *q);
extern void do_calc_(query *q, cell *c, idx_t c_ctx);
extern pl_status call_function(query *q, cell *c, __attribute__((unused)) idx_t c_ctx);

extern ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool cons, unsigned depth);
extern pl_status print_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern pl_status print_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);
extern char *print_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);

extern ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool unused, unsigned depth);
extern pl_status print_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
extern char *print_canonical_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);
extern pl_status print_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);

