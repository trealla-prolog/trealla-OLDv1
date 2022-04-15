#pragma once

void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found, bool *function);
module *find_module(prolog *pl, const char *name);
module *find_next_module(prolog *pl, module *m);
pl_idx_t index_from_pool(prolog *pl, const char *name);
bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, unsigned arity);
void load_builtins(prolog *pl);

extern pl_idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern pl_idx_t g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
extern pl_idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
extern pl_idx_t g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern pl_idx_t g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
extern pl_idx_t g_conjunction_s, g_disjunction_s, g_at_s, g_sys_ne_s, g_sys_incr_s;
extern pl_idx_t g_dcg_s, g_throw_s, g_sys_block_catcher_s, g_sys_cut_if_det_s;
extern pl_idx_t g_sys_soft_cut_s, g_if_then_s, g_soft_cut_s, g_negation_s;
extern pl_idx_t g_error_s, g_slash_s, g_sys_cleanup_if_det_s;
extern pl_idx_t g_goal_expansion_s;
extern void convert_path(char *filename);

inline static module *find_module_id(const prolog *pl, unsigned id)
{
	module *m = pl->modmap[id];

	if (!m)
		return pl->user_m;

	return m;
}

extern void sigfn(int s);
