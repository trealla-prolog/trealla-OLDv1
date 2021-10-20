#pragma once

extern prolog *pl_create();
extern void pl_destroy(prolog*);

extern void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found, bool *function);
extern bool deconsult(prolog *pl, const char *filename);
extern module *find_module(prolog *pl, const char *name);
extern module *find_module_id(prolog *pl, unsigned id);
extern module *find_next_module(prolog *pl, module *m);
extern idx_t index_from_pool(prolog *pl, const char *name);
extern bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, unsigned arity);
extern void load_builtins(prolog *pl);

extern idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern idx_t g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
extern idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
extern idx_t g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern idx_t g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
extern idx_t g_and_s, g_or_s, g_at_s, g_sys_ne_s, g_sys_incr_s, g_sys_inner_cut_s;
extern idx_t g_dcg_s, g_throw_s, g_sys_block_catcher_s;
extern idx_t g_sys_soft_cut_s;

extern void sigfn(int s);
