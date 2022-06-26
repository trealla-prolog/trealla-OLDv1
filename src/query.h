#pragma once

#include "builtins.h"

query *create_query(module *m, bool sub_query);
query *create_sub_query(query *q, cell *curr_cell);
void destroy_query(query *q);

bool push_choice(query *q);
bool push_barrier(query *q);
bool push_call_barrier(query *q);
bool push_catcher(query *q, enum q_retry type);

bool do_retract(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract);
bool do_read_term(query *q, stream *str, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, char *src);
bool do_yield_0(query *q, int msecs);

void cut_me(query *q, bool inner_cut, bool soft_cut);
void set_var(query *q, const cell *c, pl_idx_t ctx, cell *v, pl_idx_t v_ctx);
void reset_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx, bool trailing);
bool execute(query *q, cell *cells, unsigned nbr_vars);
bool fn_call_0(query *q, cell *p1);
void undo_me(query *q);
void drop_choice(query *q);
bool retry_choice(query *q);
void term_assign_vars(parser *p, unsigned start, bool rebase);
bool start(query *q);
bool match_rule(query *q, cell *p1, pl_idx_t p1_ctx);
bool match_clause(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type retract);
bool try_me(query *q, unsigned vars);
void call_attrs(query *q, cell *attrs);
void stash_me(query *q, const clause *r, bool last_match);
void trim_trail(query *q);
bool do_post_unification_hook(query *q, bool is_builtin);
bool check_redo(query *q);
void dump_vars(query *q, bool partial);
int check_interrupt(query *q);
bool more_data(query *q, db_entry *dbe);

bool find_exception_handler(query *q, cell *e);
bool throw_error(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected);
bool throw_error3(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);
bool throw_error2(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);

size_t scan_is_chars_list2(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes, bool *has_var, bool *is_partial);
size_t scan_is_chars_list(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes);
char *chars_list_to_string(query *q, cell *p_chars, pl_idx_t p_chars_ctx, size_t len);

unsigned create_vars(query *q, unsigned nbr);
void unshare_predicate(query *q, predicate *pr);
cell *skip_max_list(query *q, cell *head, pl_idx_t *head_ctx, pl_int_t max, pl_int_t *skip, cell *tmp);
bool is_cyclic_term(query *q, cell *p1, pl_idx_t p1_ctx);
bool is_acyclic_term(query *q, cell *p1, pl_idx_t p1_ctx);
bool do_format(query *q, cell *str, pl_idx_t str_ctx, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len);
int get_stream(query *q, cell *p1);
void call_builtin(query *q, cell *c, pl_idx_t c_ctx);
bool call_userfun(query *q, cell *c, pl_idx_t c_ctx);
void do_cleanup(query *q, cell *p1);
bool cut_if_det(query *q);
bool is_in_ref_list(cell *c, pl_idx_t c_ctx, reflist *rlist);
void collect_vars(query *q, cell *p1, pl_idx_t p1_ctx);
bool check_list(query *q, cell *p1, pl_idx_t p1_ctx, bool *is_partial, pl_int_t *skip);
bool parse_write_params(query *q, cell *c, pl_idx_t c_ctx, cell **vnames, pl_idx_t *vnames_ctx);
bool has_vars(query *q, cell *p1, pl_idx_t p1_ctx);
bool accum_var(query *q, const cell *c, pl_idx_t c_ctx);
int new_stream(prolog *pl);

#ifdef _WIN32
ssize_t getline(char **lineptr, size_t *n, FILE *stream);
#endif

int compare(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
bool unify(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth);
bool print_term(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
bool print_term_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);
char *print_term_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
void clear_write_options(query *q);

ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool unused, unsigned depth);
bool print_canonical(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
char *print_canonical_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);

bool fn_sys_cut_if_det_0(query *q);
bool fn_iso_throw_1(query *q);
bool fn_sys_call_cleanup_3(query *q);
bool fn_iso_catch_3(query *q);
bool fn_sys_block_catcher_0(query *q);
bool fn_iso_negation_1(query *q);
bool fn_iso_disjunction_2(query *q);
bool fn_if_3(query *q);
bool fn_if_2(query *q);
bool fn_ignore_1(query *q);

bool fn_iso_if_then_2(query *q);
bool fn_iso_invoke_2(query *q);
bool fn_iso_once_1(query *q);
bool fn_iso_call_1(query *q);
bool fn_iso_call_n(query *q);
bool fn_iso_cut_0(query *q);
bool fn_sys_inner_cut_0(query *q);
bool fn_iso_fail_0(query *q);
bool fn_iso_true_0(query *q);
bool fn_sys_undo_trail_1(query *q);
bool fn_sys_redo_trail_0(query *q);
bool fn_sys_soft_inner_cut_0(query *q);
bool fn_iso_unify_2(query *q);

inline static void share_predicate(predicate *pr)
{
	pr->ref_cnt++;
}

inline static bool can_view(const frame *f, const db_entry *dbe)
{
	if (dbe->cl.ugen_created > f->ugen)
		return false;

	if (dbe->cl.ugen_erased && (dbe->cl.ugen_erased <= f->ugen))
		return false;

	return true;
}

struct reflist_ {
	reflist *next;
	pl_idx_t ctx;

	union {
		cell *ptr;
		pl_idx_t var_nbr;
	};
};

struct cycle_info_ {
	reflist *r1, *r2;
};

#define FEOF(str) feof(str->fp) && !str->ungetch

#ifdef _WIN32
#include <io.h>
#endif

#define PROMPT ""

#define DUMP_TERM(s,c,c_ctx) {						\
	printf("*** %s ", s);							\
	q->quoted = true;								\
	print_term(q, stdout, c, c_ctx, 1);				\
	q->quoted = false;								\
	printf("\n");									\
}

#define CHECK_INTERRUPT()							\
	if (g_tpl_interrupt) {							\
		if (check_interrupt(q))						\
			break;									\
	}

inline static bool make_cstring(cell *d, const char *s)
{
	return make_cstringn(d, s, strlen(s));
}

inline static bool make_string(cell *d, const char *s)
{
	return make_stringn(d, s, strlen(s));
}

#define may_heap_error(p) if ((p) == NULL)	\
	return throw_error(q, q->st.curr_cell, q->st.curr_frame, "resource_error", "finite_memory");
