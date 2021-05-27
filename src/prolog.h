#pragma once

extern void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found);
extern bool deconsult(prolog *pl, const char *filename);
extern module *find_module(prolog *pl, const char *name);
extern module *find_module_id(prolog *pl, idx_t id);
extern module *find_next_module(prolog *pl, module *m);
extern idx_t index_from_pool(prolog *pl, const char *name);
extern bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, idx_t arity);
extern void load_builtins(prolog *pl);

extern idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern idx_t g_anon_s, g_clause_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
extern idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
extern idx_t g_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern idx_t g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
