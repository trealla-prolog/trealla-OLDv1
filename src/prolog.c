#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <float.h>
#include <sys/time.h>

#include "internal.h"
#include "library.h"
#include "trealla.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"

static const size_t INITIAL_POOL_SIZE = 64000;	// bytes

stream g_streams[MAX_STREAMS] = {{0}};
idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
idx_t g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
idx_t g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
idx_t g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
idx_t g_and_s, g_or_s, g_at_s, g_sys_ne_s, g_sys_incr_s, g_sys_inner_cut_s;
idx_t g_dcg_s, g_throw_s, g_sys_block_catcher_s;

unsigned g_cpu_count = 4;
char *g_tpl_lib = NULL;
int g_ac = 0, g_avc = 1;
char **g_av = NULL, *g_argv0 = NULL;

static atomic_t int g_tpl_count = 0;

bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, idx_t arity)
{
	module *m = find_module(pl, mod);
	if (!m) return false;

	cell tmp = (cell){0};
	tmp.tag = TAG_POOL;
	tmp.val_off = index_from_pool(m->pl, name);
	if (tmp.val_off == ERR_IDX) return false;
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) return false;
	return pr->is_multifile ? true : false;
}

static idx_t add_to_pool(prolog *pl, const char *name)
{
	idx_t offset = pl->pool_offset;
	size_t len = strlen(name);

	while ((offset+len+1+1) >= pl->pool_size) {
		size_t nbytes = pl->pool_size * 2;
		char *tmp = realloc(pl->pool, nbytes);
		if (!tmp) return ERR_IDX;
		pl->pool = tmp;
		memset(pl->pool + pl->pool_size, 0, nbytes - pl->pool_size);
		pl->pool_size = nbytes;
	}

	memcpy(pl->pool + offset, name, len+1);
	pl->pool_offset += len + 1;
	const char *key = strdup(name);
	m_set(pl->symtab, key, (void*)(unsigned long)offset);
	return offset;
}

idx_t index_from_pool(prolog *pl, const char *name)
{
	const void *val;

	if (m_get(pl->symtab, name, &val))
		return (idx_t)(unsigned long)val;

	return add_to_pool(pl, name);
}

module *find_next_module(prolog *pl, module *m)
{
	if (!m)
		return pl->modules;

	return m->next;
}

module *find_module(prolog *pl, const char *name)
{
	for (module *m = pl->modules; m; m = m->next) {
		if (!strcmp(m->name, name))
			return m;
	}

	return NULL;
}

module *find_module_id(prolog *pl, unsigned id)
{
	for (module *m = pl->modules; m; m = m->next) {
		if (m->id == id)
			return m;
	}

	return pl->user_m;
}

bool deconsult(prolog *pl, const char *filename)
{
	module *m = find_module(pl, filename);
	if (!m) return false;
	destroy_module(m);
	return true;
}

bool get_halt(prolog *pl) { return pl->halt; }
bool get_status(prolog *pl) { return pl->status; }
bool get_dump_vars(prolog *pl) { return pl->did_dump_vars; }
int get_halt_code(prolog *pl) { return pl->halt_code; }

void set_trace(prolog *pl) { pl->trace = true; }
void set_quiet(prolog *pl) { pl->quiet = true; }
void set_stats(prolog *pl) { pl->stats = true; }
void set_noindex(prolog *pl) { pl->noindex = true; }
void set_ffai(prolog *pl) { pl->ffai = true; }
void set_opt(prolog *pl, int level) { pl->opt = level; }

bool pl_eval(prolog *pl, const char *s)
{
	if (!*s)
		return false;

	parser *p = create_parser(pl->curr_m);
	if (!p) return false;
	p->command = true;
	bool ok = run(p, s, true, false);
	pl->curr_m = p->m;
	destroy_parser(p);
	return ok;
}

bool pl_consult_fp(prolog *pl, FILE *fp, const char *filename)
{
	return load_fp(pl->user_m, fp, filename) != NULL;
}

bool pl_consult(prolog *pl, const char *filename)
{
	return load_file(pl->user_m, filename);
}

static void g_destroy(prolog *pl)
{
	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &g_streams[i];

		if (str->fp) {
			if ((str->fp != stdin)
				&& (str->fp != stdout)
				&& (str->fp != stderr))
				fclose(str->fp);

			free(str->filename);
			free(str->mode);
			free(str->name);
			str->filename = NULL;
			str->name = NULL;
			str->mode = NULL;
		}

		if (str->p)
			destroy_parser(str->p);

		str->p = NULL;
	}

	memset(g_streams, 0, sizeof(g_streams));

	while (pl->modules)
		destroy_module(pl->modules);

	free(g_tpl_lib);
	m_destroy(pl->funtab);
	m_destroy(pl->symtab);
	m_destroy(pl->keyval);
	free(pl->pool);
	pl->pool_offset = 0;
}

static void keyvalfree(const void *key, const void *val)
{
	free((void*)key);
	free((void*)val);
}

static bool g_init(prolog *pl)
{
	pl->pool = calloc(pl->pool_size=INITIAL_POOL_SIZE, 1);
	if (pl->pool) {
		bool error = false;

		CHECK_SENTINEL(pl->symtab = m_create((void*)strcmp, (void*)free, NULL), NULL);
		CHECK_SENTINEL(pl->keyval = m_create((void*)strcmp, (void*)keyvalfree, NULL), NULL);

		if (!error) {
			CHECK_SENTINEL(g_false_s = index_from_pool(pl, "false"), ERR_IDX);
			CHECK_SENTINEL(g_true_s = index_from_pool(pl, "true"), ERR_IDX);
			CHECK_SENTINEL(g_at_s = index_from_pool(pl, "@"), ERR_IDX);
			CHECK_SENTINEL(g_and_s = index_from_pool(pl, ","), ERR_IDX);
			CHECK_SENTINEL(g_or_s = index_from_pool(pl, ";"), ERR_IDX);
			CHECK_SENTINEL(g_dot_s = index_from_pool(pl, "."), ERR_IDX);
			CHECK_SENTINEL(g_plus_s = index_from_pool(pl, "+"), ERR_IDX);
			CHECK_SENTINEL(g_minus_s = index_from_pool(pl, "-"), ERR_IDX);
			CHECK_SENTINEL(g_pair_s = index_from_pool(pl, ":"), ERR_IDX);
			CHECK_SENTINEL(g_empty_s = index_from_pool(pl, ""), ERR_IDX);
			CHECK_SENTINEL(g_anon_s = index_from_pool(pl, "_"), ERR_IDX);
			CHECK_SENTINEL(g_dcg_s = index_from_pool(pl, "-->"), ERR_IDX);
			CHECK_SENTINEL(g_call_s = index_from_pool(pl, "call"), ERR_IDX);
			CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "braces"), ERR_IDX);
			CHECK_SENTINEL(g_unify_s = index_from_pool(pl, "="), ERR_IDX);
			CHECK_SENTINEL(g_on_s = index_from_pool(pl, "on"), ERR_IDX);
			CHECK_SENTINEL(g_off_s = index_from_pool(pl, "off"), ERR_IDX);
			CHECK_SENTINEL(g_cut_s = index_from_pool(pl, "!"), ERR_IDX);
			CHECK_SENTINEL(g_nil_s = index_from_pool(pl, "[]"), ERR_IDX);
			CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "{}"), ERR_IDX);
			CHECK_SENTINEL(g_fail_s = index_from_pool(pl, "fail"), ERR_IDX);
			CHECK_SENTINEL(g_neck_s = index_from_pool(pl, ":-"), ERR_IDX);
			CHECK_SENTINEL(g_eof_s = index_from_pool(pl, "end_of_file"), ERR_IDX);
			CHECK_SENTINEL(g_lt_s = index_from_pool(pl, "<"), ERR_IDX);
			CHECK_SENTINEL(g_gt_s = index_from_pool(pl, ">"), ERR_IDX);
			CHECK_SENTINEL(g_eq_s = index_from_pool(pl, "="), ERR_IDX);
			CHECK_SENTINEL(g_once_s = index_from_pool(pl, "once"), ERR_IDX);
			CHECK_SENTINEL(g_throw_s = index_from_pool(pl, "throw"), ERR_IDX);

			g_streams[0].fp = stdin;
			CHECK_SENTINEL(g_streams[0].filename = strdup("stdin"), NULL);
			CHECK_SENTINEL(g_streams[0].name = strdup("user_input"), NULL);
			CHECK_SENTINEL(g_streams[0].mode = strdup("read"), NULL);
			g_streams[0].eof_action = eof_action_reset;

			g_streams[1].fp = stdout;
			CHECK_SENTINEL(g_streams[1].filename = strdup("stdout"), NULL);
			CHECK_SENTINEL(g_streams[1].name = strdup("user_output"), NULL);
			CHECK_SENTINEL(g_streams[1].mode = strdup("append"), NULL);
			g_streams[1].eof_action = eof_action_reset;

			g_streams[2].fp = stderr;
			CHECK_SENTINEL(g_streams[2].filename = strdup("stderr"), NULL);
			CHECK_SENTINEL(g_streams[2].name = strdup("user_error"), NULL);
			CHECK_SENTINEL(g_streams[2].mode = strdup("append"), NULL);
			g_streams[2].eof_action = eof_action_reset;

			CHECK_SENTINEL(g_sys_elapsed_s = index_from_pool(pl, "$elapsed"), ERR_IDX);
			CHECK_SENTINEL(g_sys_queue_s = index_from_pool(pl, "$queue"), ERR_IDX);
			CHECK_SENTINEL(g_sys_var_s = index_from_pool(pl, "$VAR"), ERR_IDX);
			CHECK_SENTINEL(g_sys_stream_property_s = index_from_pool(pl, "$stream_property"), ERR_IDX);
			CHECK_SENTINEL(g_post_unify_hook_s = index_from_pool(pl, "$post_unify_hook"), ERR_IDX);
			CHECK_SENTINEL(g_sys_record_key_s = index_from_pool(pl, "$record_key"), ERR_IDX);
			CHECK_SENTINEL(g_sys_ne_s = index_from_pool(pl, "$ne"), ERR_IDX);
			CHECK_SENTINEL(g_sys_incr_s = index_from_pool(pl, "$incr"), ERR_IDX);
			CHECK_SENTINEL(g_sys_inner_cut_s = index_from_pool(pl, "$inner_cut"), ERR_IDX);
			CHECK_SENTINEL(g_sys_block_catcher_s = index_from_pool(pl, "$block_catcher"), ERR_IDX);
		}

		if (error) {
			g_destroy(pl);
			return NULL;
		}
	}
	return pl->pool ? true : false;
}

void pl_destroy(prolog *pl)
{
	if (!pl) return;

	destroy_module(pl->system_m);
	destroy_module(pl->user_m);

	if (!--g_tpl_count)
		g_destroy(pl);

	free(pl);
}

prolog *pl_create()
{
	prolog *pl = calloc(1, sizeof(prolog));

	if (!g_tpl_count++ && !g_init(pl)) {
		free(pl);
		return NULL;
	}

	if (!g_tpl_lib) {
		char *ptr = getenv("TPL_LIBRARY_PATH");

		if (ptr)
			g_tpl_lib = strdup(ptr);
	}

	if (!g_tpl_lib) {
		g_tpl_lib = realpath(g_argv0, NULL);

		if (g_tpl_lib) {
			char *src = g_tpl_lib + strlen(g_tpl_lib) - 1;

			while ((src != g_tpl_lib) && (*src != '/'))
				src--;

			*src = '\0';
			g_tpl_lib = realloc(g_tpl_lib, strlen(g_tpl_lib)+40);
			strcat(g_tpl_lib, "/library");
		} else
			g_tpl_lib = strdup("../library");
	}

	pl->funtab = m_create((void*)strcmp, NULL, NULL);

	if (pl->funtab)
		load_builtins(pl);

	//printf("Library: %s\n", g_tpl_lib);

	pl->system_m = create_module(pl, "system");

	if (!pl->system_m || pl->system_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->user_m = create_module(pl, "user");

	if (!pl->user_m || pl->user_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->curr_m = pl->user_m;
	pl->s_last = 0;
	pl->s_cnt = 0;
	pl->seed = 0;

	pl->current_input = 0;		// STDIN
	pl->current_output = 1;		// STDOUT
	pl->current_error = 2;		// STDERR

	set_multifile_in_db(pl->system_m, "term_expansion", 2);
	set_multifile_in_db(pl->system_m, "goal_expansion", 2);
	set_dynamic_in_db(pl->system_m, "term_expansion", 2);
	set_dynamic_in_db(pl->system_m, "goal_expansion", 2);

	set_multifile_in_db(pl->user_m, "term_expansion", 2);
	set_multifile_in_db(pl->user_m, "goal_expansion", 2);
	set_noindex_in_db(pl->user_m, "$predicate_property", 2);
	set_noindex_in_db(pl->user_m, "$stream_property", 2);
	set_noindex_in_db(pl->user_m, "$current_op", 3);
	set_dynamic_in_db(pl->user_m, "$record_key", 2);
	set_dynamic_in_db(pl->user_m, "$current_op", 3);
	set_dynamic_in_db(pl->user_m, "$predicate_property", 2);
	set_dynamic_in_db(pl->user_m, "$current_prolog_flag", 2);
	set_dynamic_in_db(pl->user_m, "$stream_property", 2);
	set_dynamic_in_db(pl->user_m, "term_expansion", 2);
	set_dynamic_in_db(pl->user_m, "goal_expansion", 2);
	set_dynamic_in_db(pl->user_m, "initialization", 1);
	set_dynamic_in_db(pl->user_m, ":-", 1);

	pl->user_m->prebuilt = true;

	for (library *lib = g_libs; lib->name; lib++) {
		if (!strcmp(lib->name, "builtins")) {
			char *src = malloc(*lib->len+1);
			ensure(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			ASTRING(s1);
			ASTRING_sprintf(s1, "library/%s", lib->name);
			load_text(pl->user_m, src, ASTRING_cstr(s1));
			ASTRING_free(s1);
			free(src);
			break;
		}
	}

	pl->user_m->prebuilt = false;
	return pl;
}
