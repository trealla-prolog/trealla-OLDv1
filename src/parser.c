#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <float.h>
#include <sys/time.h>

#include "internal.h"
#include "history.h"
#include "library.h"
#include "trealla.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "builtins.h"
#include "heap.h"
#include "utf8.h"

static const unsigned INITIAL_TOKEN_SIZE = 100;		// bytes

unsigned get_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	for (const op_table *ptr = m->ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier) *specifier = ptr->specifier;
			return ptr->priority;
		}
	}

	for (const op_table *ptr = m->def_ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (!strcmp(ptr->name, name)) {
			if (specifier) *specifier = ptr->specifier;
			return ptr->priority;
		}
	}

	if (hint_prefix)
		return get_op(m, name, specifier, false);

	return 0;
}

unsigned find_op(module *m, const char *name, unsigned specifier)
{
	for (const op_table *ptr = m->ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (specifier != ptr->specifier)
			continue;

		if (!strcmp(ptr->name, name))
			return ptr->priority;
	}

	for (const op_table *ptr = m->def_ops; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (specifier != ptr->specifier)
			continue;

		if (!strcmp(ptr->name, name))
			return ptr->priority;
	}

	return 0;
}

bool set_op(module *m, const char *name, unsigned specifier, unsigned priority)
{
	unsigned ot = 0, pri = 0;
	int hint = IS_PREFIX(specifier);

	if ((pri = get_op(m, name, &ot, hint)) != 0) {
		if ((ot == specifier) && priority)
			return true;
	}

	op_table *ptr = m->def_ops;

	for (; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (strcmp(ptr->name, name))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			return true;
		}

		ptr->specifier = specifier;
		ptr->priority = priority;
		m->loaded_ops = false;
		return true;
	}

	ptr = m->ops;

	for (; ptr->name; ptr++) {
		if (!ptr->specifier)
			continue;

		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (strcmp(ptr->name, name))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			m->loaded_ops = false;
			return true;
		}

		ptr->specifier = specifier;
		ptr->priority = priority;
		m->loaded_ops = false;
		return true;
	}

	if (!priority)
		return true;

	for (ptr = m->ops; ptr->specifier; ptr++)
		;

	m->user_ops = true;

	if (ptr->name)
		free(ptr->name);
	else {
		if (!m->spare_ops)
			return false;

		m->spare_ops--;
	}

	ptr->name = strdup(name);
	ptr->specifier = specifier;
	ptr->priority = priority;
	m->loaded_ops = false;
	return true;
}

static const char *get_filename(const char *path)
{
	const char *ptr = strrchr(path, '/');

	if (!ptr)
		return path;

	return ptr+1;
}

cell *list_head(cell *l, cell *tmp)
{
	if (!is_string(l))
		return l + 1;

	const char *src = is_static(l) ? l->val_str : (char*)l->val_strb->cstr + l->strb_off;
	size_t len = len_char_utf8(src);
	tmp->tag = TAG_CSTRING;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	memcpy(tmp->val_chr, src, len);
	tmp->val_chr[len] = '\0';
	return tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!l) return NULL;

	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->nbr_cells;
	}

	const char *src = is_static(l) ? l->val_str : (char*)l->val_strb->cstr + l->strb_off;
	size_t str_len = is_static(l) ? (size_t)l->str_len : (size_t)l->val_strb->len - l->strb_off;
	size_t len = len_char_utf8(src);

	if (str_len == len) {
		tmp->tag = TAG_LITERAL;
		tmp->nbr_cells = 1;
		tmp->arity = 0;
		tmp->flags = 0;
		tmp->val_off = g_nil_s;
		return tmp;
	}

	if (is_static(l)) {
		tmp->flags = FLAG_BLOB | FLAG_STATIC | FLAG_STRING;
		tmp->nbr_cells = 1;
		tmp->arity = 2;
		tmp->val_str = l->val_str + len;
		tmp->str_len = l->str_len - len;
		return tmp;
	}

	copy_cells(tmp, l, 1);
	tmp->strb_off = l->strb_off + len;
	tmp->strb_len = l->strb_len - len;
	return tmp;
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

module *find_module_id(prolog *pl, idx_t id)
{
	for (module *m = pl->modules; m; m = m->next) {
		if (m->id == id)
			return m;
	}

	return pl->user_m;
}

bool check_rule(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_clause_s) && (c->arity == 2))
		return true;

	return false;
}

cell *get_head(cell *c)
{
	if (check_rule(c))
		return c + 1;

	return c;
}

cell *get_body(cell *c)
{
	if (check_rule(c)) {
		c = c + 1;
		c += c->nbr_cells;

		if (is_end(c))
			return NULL;

		return c;
	}

	return NULL;
}

cell *get_logical_body(cell *c)
{
	cell *body = get_body(c);

	if (!body)
		return NULL;

	// A body of just 'true' is equivalent to no body at all,
	// and of course vice-versa.

	if (!body->arity && is_literal(body) && (body->val_off == g_true_s))
		return NULL;

	return body;
}

void clear_term(term *t)
{
	if (!t)
		return;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
	}

	t->cidx = 0;
}

static bool make_room(parser *p)
{
	if (p->t->cidx == p->t->nbr_cells) {
		idx_t nbr_cells = p->t->nbr_cells * 2;

		term *t = realloc(p->t, sizeof(term)+(sizeof(cell)*nbr_cells));
		if (!t) {
			p->error = true;
			return false;
		}

		p->t = t;
		p->t->nbr_cells = nbr_cells;
	}

	return true;
}

static cell *make_cell(parser *p)
{
	make_room(p);
	cell *ret = p->t->cells + p->t->cidx++;
	*ret = (cell){0};
	return ret;
}

void destroy_parser(parser *p)
{
	free(p->save_line);
	free(p->token);
	clear_term(p->t);
	free(p->t);
	free(p);
}

parser *create_parser(module *m)
{
	parser *p = calloc(1, sizeof(parser));
	ensure(p);
	p->token = calloc(p->token_size=INITIAL_TOKEN_SIZE+1, 1);
	idx_t nbr_cells = INITIAL_NBR_CELLS;
	p->t = calloc(sizeof(term)+(sizeof(cell)*nbr_cells), 1);
	p->t->nbr_cells = nbr_cells;
	p->start_term = true;
	p->line_nbr = 1;
	p->m = m;
	p->error = false;
	p->flag = m->flag;

	if (!p->token || !p->t) {
		destroy_parser(p);
		p = NULL;
	}

	return p;
}

void consultall(parser *p, cell *l)
{
	LIST_HANDLER(l);

	while (is_list(l)) {
		cell *h = LIST_HEAD(l);
		load_file(p->m, PARSER_GET_STR(h));
		l = LIST_TAIL(l);
	}
}

char *relative_to(const char *basefile, const char *relfile)
{
	char *tmpbuf = malloc(strlen(basefile) + strlen(relfile) + 256);
	char *ptr = tmpbuf;

	if (!strncmp(relfile, "../", 3)) {
		strcpy(tmpbuf, basefile);
		ptr = tmpbuf + strlen(tmpbuf) - 1;

		while ((ptr != tmpbuf) && (*ptr != '/'))
			ptr--;

		if (ptr != tmpbuf)
			*ptr++ = '/';

		*ptr = '\0';
	}

	strcpy(ptr, relfile);
	return tmpbuf;
}

static void do_op(parser *p, cell *c)
{
	cell *p1 = c + 1, *p2 = c + 2, *p3 = c + 3;

	if (!is_smallint(p1) || !is_literal(p2) || (!is_atom(p3) && !is_list(p3))) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: unknown op\n");

		p->error = true;
		return;
	}

	unsigned specifier;
	const char *spec = PARSER_GET_STR(p2);

	if (!strcmp(spec, "fx"))
		specifier = OP_FX;
	else if (!strcmp(spec, "fy"))
		specifier = OP_FY;
	else if (!strcmp(spec, "xf"))
		specifier = OP_XF;
	else if (!strcmp(spec, "yf"))
		specifier = OP_YF;
	else if (!strcmp(spec, "xfx"))
		specifier = OP_XFX;
	else if (!strcmp(spec, "xfy"))
		specifier = OP_XFY;
	else if (!strcmp(spec, "yfx"))
		specifier = OP_YFX;
	else {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: unknown op spec tag\n");
		return;
	}

	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);

		if (is_atom(h)) {
			if (!set_op(p->m, PARSER_GET_STR(h), specifier, get_smallint(p1))) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op\n");

				continue;
			}
		}

		p3 = LIST_TAIL(p3);
	}

	if (is_atom(p3) && !is_nil(p3)) {
		if (!set_op(p->m, PARSER_GET_STR(p3), specifier, get_smallint(p1))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: could not set op\n");

			return;
		}
	}
}

static bool	is_loaded(module *m, const char *filename)
{
	struct loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename))
			return true;

		ptr = ptr->next;
	}

	return false;
}

static void directives(parser *p, term *t)
{
	p->skip = false;

	if (!is_literal(t->cells))
		return;

	if (is_list(t->cells) && p->command) {
		consultall(p, t->cells);
		p->skip = true;
		return;
	}

	if (strcmp(PARSER_GET_STR(t->cells), ":-") || (t->cells->arity != 1))
		return;

	cell *c = t->cells + 1;

	if (!is_literal(c))
		return;

	const char *dirname = PARSER_GET_STR(c);

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		unsigned save_line_nbr = p->line_nbr;
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s\n", filename);

			free(filename);
			p->error = true;
			return;
		}

		p->line_nbr = save_line_nbr;
		free(filename);
		return;
	}

	if (!strcmp(dirname, "ensure_loaded") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		char *filename = relative_to(p->m->filename, name);

		if (is_loaded(p->m, filename))
			return;

		unsigned save_line_nbr = p->line_nbr;

		if (!load_file(p->m, filename)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s\n", filename);

			free(filename);
			p->error = true;
			return;
		}

		p->line_nbr = save_line_nbr;
		free(filename);
		return;
	}

	if (!strcmp(dirname, "pragma") && (c->arity == 2)) {
		cell *p2 = c + 2;
		const char *name = "";
		char tmpbuf[1024];

		if (is_variable(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: pragma name not an atom\n");

			p->error = true;
			return;
		} else
			name = PARSER_GET_STR(p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;
			return;
		}

		tmp_m = create_module(p->m->pl, name);
		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

			p->error = true;
			return;
		}

		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			LIST_HEAD(p2);
			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "module") && (c->arity == 2)) {
		cell *p2 = c + 2;
		const char *name = "";
		char tmpbuf[1024];

		if (is_variable(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module name not an atom\n");

			p->error = true;
			return;
		} else
			name = PARSER_GET_STR(p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;
			return;
		}

		p->m = create_module(p->m->pl, name);
		if (!p->m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

			p->error = true;
			return;
		}

		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			cell *head = LIST_HEAD(p2);

			if (is_structure(head)) {
				if (!strcmp(PARSER_GET_STR(head), "/")
					|| !strcmp(PARSER_GET_STR(head), "//")) {
					cell *f = head+1, *a = f+1;
					if (!is_literal(f)) return;
					if (!is_smallint(a)) return;
					cell tmp = *f;
					tmp.arity = get_smallint(a);

					if (!strcmp(PARSER_GET_STR(head), "//"))
						tmp.arity += 2;

					predicate *h = find_predicate(p->m, &tmp);
					if (!h) h = create_predicate(p->m, &tmp);
					if (!h) {
						destroy_module(p->m);
						p->m = NULL;
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: predicate creation failed\n");

						p->error = true;
						return;
					}

					h->is_public = true;
				} else if (!strcmp(PARSER_GET_STR(head), "op") && (head->arity == 3)) {
					do_op(p, head);
					// TO-DO: make public...
				}
			}

			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_structure(p1)) return;
	}

	if ((!strcmp(dirname, "use_module") || !strcmp(dirname, "autoload")) && (c->arity >= 1)) {
		if (!is_atom(p1) && !is_structure(p1)) return;
		const char *name = PARSER_GET_STR(p1);
		char dstbuf[1024*2];

		if (!strcmp(name, "library")) {
			p1 = p1 + 1;
			if (!is_literal(p1)) return;
			name = PARSER_GET_STR(p1);
			module *m;

			if ((m = find_module(p->m->pl, name)) != NULL) {
				if (!m->fp)
					do_db_load(m);

				return;
			}

			if (!strcmp(name, "between")
				|| !strcmp(name, "samsort")
				|| !strcmp(name, "terms")
				|| !strcmp(name, "types")
				|| !strcmp(name, "iso_ext")
				|| !strcmp(name, "files"))
				return;

			for (library *lib = g_libs; lib->name; lib++) {
				if (strcmp(lib->name, name))
					continue;

				STRING_alloc(src);
				STRING_strcatn(src, lib->start, *lib->len);
				STRING_alloc(s1);
				STRING_strcat2(s1, "library/", lib->name);
				m = load_text(p->m, STRING_cstr(src), STRING_cstr(s1));
				STRING_free(src);
				STRING_free(s1);

				if (m != p->m)
					do_db_load(m);

				return;
			}

			query q = (query){0};
			q.st.m = p->m;
			snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
			char *dst = dstbuf + strlen(dstbuf);
			idx_t ctx = 0;
			print_term_to_buf(&q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, false, 0);
			name = dstbuf;
		}

		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename)) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: using module file: %s\n", filename);

			//p->error = true;
			free(filename);
			return;
		}

		free(filename);
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;
		if (!is_literal(p2)) return;

		if (!strcmp(PARSER_GET_STR(p1), "double_quotes")) {
			if (!strcmp(PARSER_GET_STR(p2), "atom")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_atom = true;
			} else if (!strcmp(PARSER_GET_STR(p2), "codes")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_atom = false;
				p->m->flag.double_quote_codes = true;
			} else if (!strcmp(PARSER_GET_STR(p2), "chars")) {
				p->m->flag.double_quote_atom = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_chars = true;
			} else {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: unknown value\n");

				p->error = true;
				return;
			}
		} else if (!strcmp(PARSER_GET_STR(p1), "character_escapes")) {
			if (!strcmp(PARSER_GET_STR(p2), "true") || !strcmp(PARSER_GET_STR(p2), "on"))
				p->m->flag.character_escapes = true;
			else if (!strcmp(PARSER_GET_STR(p2), "false") || !strcmp(PARSER_GET_STR(p2), "off"))
				p->m->flag.character_escapes = false;
		} else if (!strcmp(PARSER_GET_STR(p1), "generate_debug_info")) {
		} else {
			fprintf(stdout, "Warning: unknown flag: %s\n", PARSER_GET_STR(p1));
		}

		p->flag = p->m->flag;
		return;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		do_op(p, c);
		return;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);

			if (is_literal(h) && (!strcmp(PARSER_GET_STR(h), "/") || !strcmp(PARSER_GET_STR(h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;
			if (!is_atom(c_name)) continue;
			cell *c_arity = h + 2;
			if (!is_smallint(c_arity)) continue;
			unsigned arity = get_smallint(c_arity);

			if (!strcmp(PARSER_GET_STR(h), "//"))
				arity += 2;

			//printf("*** %s => %s / %u\n", dirname, PARSER_GET_STR(c_name), arity);

			if (!strcmp(dirname, "dynamic")) {
				set_dynamic_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				set_persist_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "noindex")) {
				set_noindex_in_db(p->m, PARSER_GET_STR(c_name), arity);
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = PARSER_GET_STR(c_name);

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(p->m->pl, mod, name, arity)) {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: not multile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return;
					}
				}
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return;

	while (is_literal(p1)) {
		if (!strcmp(PARSER_GET_STR(p1), "/") && (p1->arity == 2)) {
			cell *c_name = p1 + 1;
			if (!is_atom(c_name)) return;
			cell *c_arity = p1 + 2;
			if (!is_smallint(c_arity)) return;
			unsigned arity = get_smallint(c_arity);

			if (!strcmp(PARSER_GET_STR(p1), "//"))
				arity += 2;

			if (!strcmp(dirname, "multifile")) {
				const char *src = PARSER_GET_STR(c_name);

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(p->m->pl, mod, name, arity)) {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: not multile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return;
					}
				}
			} else if (!strcmp(dirname, "discontiguous"))
				set_discontiguous_in_db(p->m, PARSER_GET_STR(c_name), arity);
			else if (!strcmp(dirname, "dynamic"))
				set_dynamic_in_db(p->m, PARSER_GET_STR(c_name), arity);
			else if (!strcmp(dirname, "persist"))
				set_persist_in_db(p->m, PARSER_GET_STR(c_name), arity);
			else if (!strcmp(dirname, "meta_predicate"))
				set_meta_predicate_in_db(p->m, c_name);

			p1 += p1->nbr_cells;
		} else if (!strcmp(dirname, "meta_predicate")) {
			set_meta_predicate_in_db(p->m, p1);
			p1 += p1->nbr_cells;
		} else if (!strcmp(PARSER_GET_STR(p1), ",") && (p1->arity == 2))
			p1 += 1;
		else
			break;
	}

	return;
}

static void xref_cell(parser *p, term *t, cell *c, predicate *parent)
{
	const char *functor = PARSER_GET_STR(c);

	unsigned specifier;
	bool hint_prefix = c->arity == 1;

	if ((c->arity == 2)
		&& !GET_OP(c)
		&& strcmp(functor, "{}")
		&& get_op(p->m, functor, &specifier, hint_prefix)) {
		SET_OP(c, specifier);
	}

	bool found = false;
	c->fn = get_builtin(p->m->pl, functor, c->arity, &found);

	if (found) {
		c->flags |= FLAG_BUILTIN;
		return;
	}

	predicate *h = search_predicate(p->m, c);

	if ((c+c->nbr_cells) >= (t->cells+t->cidx-1)) {
		c->flags |= FLAG_TAIL;

		if (parent && (h == parent)) {
			c->flags |= FLAG_TAIL_REC;
			t->tail_rec = true;
		}
	}

	if (h) {
		if ((h->m != p->m) && !h->is_public
			&& strcmp(PARSER_GET_STR(c), "dynamic")
			&& strcmp(PARSER_GET_STR(c), "module")) {
			//fprintf(stdout, "Warning: xref not a public method %s/%u\n", PARSER_GET_STR(c), c->arity);
			//p->error = true;
			return;
		}

		c->match = h;
	}
}

void term_xref(parser *p, term *t, predicate *parent)
{
	cell *c = t->cells;

	if (c->val_off == g_sys_record_key_s)
		return;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_literal(c))
			continue;

		xref_cell(p, t, c, parent);
	}
}

void xref_db(parser *p)
{
	for (predicate *h = p->m->head; h; h = h->next) {
		for (clause *r = h->head; r; r = r->next)
			term_xref(p, &r->t, h);
	}
}

static void check_first_cut(parser *p)
{
	cell *c = get_body(p->t->cells);
	int cut_only = true;

	if (!c)
		return;

	while (!is_end(c)) {
		if (!(c->flags&FLAG_BUILTIN))
			break;

		if (!strcmp(PARSER_GET_STR(c), ","))
			;
		else if (!IS_OP(c) && !strcmp(PARSER_GET_STR(c), "!")) {
			p->t->first_cut = true;
			break;
		} else {
			cut_only = false;
			break;
		}

		c += c->nbr_cells;
	}

	if (p->t->first_cut && cut_only)
		p->t->cut_only = true;
}

static idx_t get_varno(parser *p, const char *src)
{
	int anon = !strcmp(src, "_");
	size_t offset = 0;
	int i = 0;

	while (p->vartab.var_pool[offset]) {
		if (!strcmp(p->vartab.var_pool+offset, src) && !anon)
			return i;

		offset += strlen(p->vartab.var_pool+offset) + 1;
		i++;
	}

	size_t len = strlen(src);

	if ((offset+len+1) >= MAX_VAR_POOL_SIZE) {
		fprintf(stdout, "Error: variable pool exhausted\n");
		p->error = true;
		return 0;
	}

	memcpy(p->vartab.var_pool+offset, src, len+1);
	return i;
}

void term_assign_vars(parser *p, unsigned start, bool rebase)
{
	if (!p || p->error)
		return;

	p->start_term = true;
	p->nbr_vars = 0;
	memset(&p->vartab, 0, sizeof(p->vartab));
	term *t = p->t;
	t->nbr_vars = 0;
	t->first_cut = false;
	t->cut_only = false;

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_variable(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, PARSER_GET_STR(c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_ARITY) {
			fprintf(stdout, "Error: max vars per term reached\n");
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = PARSER_GET_STR(c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			c->flags |= FLAG2_FIRST_USE;
			t->nbr_vars++;
			p->nbr_vars++;
		}
	}

	for (idx_t i = 0; i < t->nbr_vars; i++) {
		if (p->consulting && !p->do_read_term && (p->vartab.var_used[i] == 1) &&
			(p->vartab.var_name[i][strlen(p->vartab.var_name[i])-1] != '_') &&
			(*p->vartab.var_name[i] != '_')) {
			if (!p->m->pl->quiet)
				fprintf(stdout, "Warning: singleton: %s, near line %u, file '%s'\n", p->vartab.var_name[i], p->line_nbr, get_filename(p->m->filename));
		}
	}

	for (idx_t i = 0; i < t->cidx; i++) {
		cell *c = t->cells + i;

		if (!is_variable(c))
			continue;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG2_ANON;
	}

	cell *c = make_cell(p);
	ensure(c);
	c->tag = TAG_END;
	c->nbr_cells = 1;
	check_first_cut(p);
	p->t->is_fact = !get_logical_body(p->t->cells);
}

static cell *insert_here(parser *p, cell *c, cell *p1)
{
	idx_t c_idx = c - p->t->cells, p1_idx = p1 - p->t->cells;
	make_room(p);

	cell *last = p->t->cells + (p->t->cidx - 1);
	idx_t cells_to_move = p->t->cidx - p1_idx;
	cell *dst = last + 1;

	while (cells_to_move--)
		*dst-- = *last--;

	p1 = p->t->cells + p1_idx;
	p1->tag = TAG_LITERAL;
	p1->flags = 0;//FLAG_BUILTIN;
	p1->fn = NULL;
	p1->val_off = g_call_s;
	p1->nbr_cells = 2;
	p1->arity = 1;

	p->t->cidx++;
	return p->t->cells + c_idx;
}

cell *check_body_callable(parser *p, cell *c)
{
	if (IS_XFX(c) || IS_XFY(c)) {
		if (!strcmp(PARSER_GET_STR(c), ",")
			|| !strcmp(PARSER_GET_STR(c), ";")
			|| !strcmp(PARSER_GET_STR(c), "->")
			|| !strcmp(PARSER_GET_STR(c), ":-")) {
			cell *lhs = c + 1;
			cell *tmp;

			if ((tmp = check_body_callable(p, lhs)) != NULL)
				return tmp;

			cell *rhs = lhs + lhs->nbr_cells;

			if ((tmp = check_body_callable(p, rhs)) != NULL)
				return tmp;
		}
	}

	return !is_callable(c) && !is_variable(c) ? c : NULL;
}

static cell *term_to_body_conversion(parser *p, cell *c)
{
	idx_t c_idx = c - p->t->cells;

	if (IS_XFX(c) || IS_XFY(c)) {
		if (!strcmp(PARSER_GET_STR(c), ",")
			|| !strcmp(PARSER_GET_STR(c), ";")
			|| !strcmp(PARSER_GET_STR(c), "->")
			|| !strcmp(PARSER_GET_STR(c), ":-")) {
			cell *lhs = c + 1;

			if (is_variable(lhs)) {
				c = insert_here(p, c, lhs);
				lhs = c + 1;
			} else
				lhs = term_to_body_conversion(p, lhs);

			cell *rhs = lhs + lhs->nbr_cells;
			c = p->t->cells + c_idx;

			if (is_variable(rhs))
				c = insert_here(p, c, rhs);
			else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + lhs->nbr_cells + rhs->nbr_cells;
		}
	}

	if (IS_FY(c)) {
			if (!strcmp(PARSER_GET_STR(c), "\\+")) {
			cell *rhs = c + 1;

			if (is_variable(rhs)) {
				c = insert_here(p, c, rhs);
				rhs = c + 1;
			} else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + rhs->nbr_cells;
		}
	}

	return p->t->cells + c_idx;
}

void term_to_body(parser *p)
{
	term_to_body_conversion(p, p->t->cells);
	p->t->cells->nbr_cells = p->t->cidx - 1;
}

static bool attach_ops(parser *p, idx_t start_idx)
{
	idx_t lowest = IDX_MAX, work_idx, end_idx = p->t->cidx - 1;
	bool do_work = false, bind_le = false;

	for (idx_t i = start_idx; i < p->t->cidx;) {
		cell *c = p->t->cells + i;

		//printf("*** OP0 %s type=%u, specifier=%u, pri=%u\n", PARSER_GET_STR(c), c->tag, GET_OP(c), c->priority);

		if ((c->nbr_cells > 1) || !is_literal(c) || !c->priority) {
			i += c->nbr_cells;
			continue;
		}

		if ((i == start_idx) && (i == end_idx)) {
			c->priority = 0;
			i++;
			continue;
		}

		if (bind_le ? c->priority <= lowest : c->priority < lowest) {
			lowest = c->priority;
			work_idx = i;
			do_work = true;
		}

		bind_le = IS_XFY(c) || IS_FY(c) ? true : false;
		i++;
	}

	if (!do_work)
		return false;

	idx_t last_idx = 0;

	for (idx_t i = start_idx; i <= end_idx;) {
		cell *c = p->t->cells + i;

		if ((c->nbr_cells > 1) || !is_literal(c) || !c->priority) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		if ((c->priority != lowest) || (i != work_idx)) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		//printf("*** OP1 %s type=%u, specifier=%u, pri=%u\n", PARSER_GET_STR(c), c->tag, GET_OP(c), c->priority);

		c->tag = TAG_LITERAL;
		c->arity = 1;

		// Prefix...

		if (IS_FX(c)) {
			cell *rhs = c + 1;

			if (IS_FX(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error = true;
				return false;
			}

			rhs += rhs->nbr_cells;

			if ((((idx_t)(rhs - p->t->cells)) < end_idx)
				&& IS_XF(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error = true;
				return false;
			}
		}

		if (IS_FX(c) || IS_FY(c)) {
			cell *rhs = c + 1;
			c->nbr_cells += rhs->nbr_cells;
			idx_t off = (idx_t)(rhs - p->t->cells);

			if (off > end_idx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: missing operand to '%s', line %u, '%s'\n", PARSER_GET_STR(c), p->line_nbr, p->save_line);

				p->error = true;
				return false;
			}

			break;
		}

		// Postfix...

		cell *rhs = c + 1;
		cell save = *c;

		if (IS_XF(rhs) && (rhs->priority == c->priority)) {
			if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

			p->error = true;
			return false;
		}

		if (IS_XF(c) || IS_YF(c)) {
			cell *lhs = p->t->cells + last_idx;
			save.nbr_cells += lhs->nbr_cells;
			idx_t cells_to_move = lhs->nbr_cells;
			lhs = c - 1;

			while (cells_to_move--)
				*c-- = *lhs--;

			*c = save;
			break;
		}

		// Infix...

		idx_t off = (idx_t)(rhs - p->t->cells);

		if (off > end_idx) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: missing operand to '%s', line %u, '%s'\n", PARSER_GET_STR(c), p->line_nbr, p->save_line);

			p->error = true;
			return false;
		}

		cell *lhs = p->t->cells + last_idx;
		save.nbr_cells += lhs->nbr_cells;
		idx_t cells_to_move = lhs->nbr_cells;
		lhs = c - 1;

		while (cells_to_move--)
			*c-- = *lhs--;

		*c = save;
		c->nbr_cells += rhs->nbr_cells;
		c->arity = 2;

		if (IS_XFX(c)) {
			cell *next = c + c->nbr_cells;
			i = next - p->t->cells;

			if ((i <= end_idx)
				&& (IS_XFX(next))
				&& (next->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error = true;
				return false;
			}
		}

		break;
	}

	return true;
}

static bool lexer_analyze(parser *p, idx_t start_idx)
{
	while (attach_ops(p, start_idx))
		;

	return !p->error;
}

void reset(parser *p)
{
	clear_term(p->t);
	p->t->cidx = 0;
	p->start_term = true;
}

static bool term_dcg_rewrite(parser *p)
{
	if (p->error || !is_literal(p->t->cells) || (p->t->cells->arity != 2))
		return false;

	if (strcmp(PARSER_GET_STR(p->t->cells), "-->"))
		return false;

	if (!find_module(p->m->pl, "dcgs")) {
		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, "dcgs"))
				continue;

			char *src = malloc(*lib->len+1);
			may_ptr_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			STRING_alloc(s1);
			STRING_strcat2(s1, "library/", lib->name);
			load_text(p->m, src, STRING_cstr(s1));
			STRING_free(s1);
			free(src);
		}
	}

	// Being conservative here (for now) and using
	// temp parser/query objects...

	query *q = create_query(p->m, false);
	ensure(q);
	char *dst = print_term_to_strbuf(q, p->t->cells, 0, -1);
	char *src = malloc(strlen(dst)+256);
	ensure(src);
	sprintf(src, "dcg_translate((%s),_TermOut).", dst);
	free(dst);

	parser *p2 = create_parser(p->m);
	ensure(p2);
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = src;
	tokenize(p2, false, false);
	term_xref(p2, p2->t, NULL);
	execute(q, p2->t);
	free(src);
	frame *g = GET_FRAME(0);
	src = NULL;

	for (unsigned i = 0; i < p2->t->nbr_vars; i++) {
		slot *e = GET_SLOT(g, i);

		if (is_empty(&e->c))
			continue;

		q->latest_ctx = e->ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
			q->latest_ctx = e->ctx;
		} else
			c = deref(q, &e->c, e->ctx);

		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		src = print_term_to_strbuf(q, c, q->latest_ctx, -1);
		strcat(src, ".");
		break;
	}

	if (!src) {
		destroy_parser(p2);
		destroy_query(q);
		p->error = true;
		return false;
	}

	reset(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);
	free(src);

	// Take the completed term...

	clear_term(p->t);
	free(p->t);
	p->t = p2->t;
	p2->t = NULL;
	p->nbr_vars = p2->nbr_vars;

	destroy_parser(p2);
	destroy_query(q);
	return true;
}

static cell *make_literal(parser *p, idx_t offset)
{
	cell *c = make_cell(p);
	c->tag = TAG_LITERAL;
	c->nbr_cells = 1;
	c->val_off = offset;
	return c;
}

static int get_octal(const char **srcptr)
{
	const char *src = *srcptr;
	int v = 0;

	while (*src == '0')
		src++;

	while ((*src >= '0') && (*src <= '7')) {
		v *= 8;
		char ch = *src++;
		v += ch - '0';
	}

	*srcptr = src;
	return v;
}

static int get_hex(const char **srcptr, int n)
{
	const char *src = *srcptr;
	int v = 0;

	while ((n > 0) && (*src == '0')) {
		src++; n--;
	}

	while ((n > 0) && (((*src >= '0') && (*src <= '9')) ||
		((*src >= 'a') && (*src <= 'f')) ||
		((*src >= 'A') && (*src <= 'F')))) {
		v *= 16;
		char ch = *src++;
		n--;

		if ((ch >= 'a') && (ch <= 'f'))
			v += 10 + (ch - 'a');
		else if ((ch >= 'A') && (ch <= 'F'))
			v += 10 + (ch - 'A');
		else
			v += ch - '0';
	}

	*srcptr = src;
	return v;
}

const char *g_escapes = "\e\a\f\b\t\v\r\n\x20\x7F\'";
const char *g_anti_escapes = "eafbtvrnsd'";

static int get_escape(const char **_src, bool *error)
{
	const char *src = *_src;
	int ch = *src++;
	const char *ptr = strchr(g_anti_escapes, ch);

	if (ptr)
		ch = g_escapes[ptr-g_anti_escapes];
	else if (isdigit(ch) || (ch == 'x') || (ch == 'u') || (ch == 'U')) {
		int unicode = 0;

		if (ch == 'U') {
			ch = get_hex(&src, 8);
			unicode = 1;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4);
			unicode = 1;
		} else if (ch == 'x')
			ch = get_hex(&src, 999);
		else {
			src--;
			ch = get_octal(&src);
		}

		if (!unicode && (*src++ != '\\')) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: syntax error, closing \\ missing\n");
			*_src = src;
			*error = true;
			return 0;
		}
	} else if ((ch != '\\') && (ch != '"') && (ch != '\'') && (ch != '\r') && (ch != '\n')) {
		*_src = src;
		*error = true;
		return 0;
	}

	*_src = src;
	return ch;
}

#define isbdigit(ch) (((ch) >= '0') && ((ch) <= '1'))
#define isodigit(ch) (((ch) >= '0') && ((ch) <= '7'))

static bool parse_number(parser *p, const char **srcptr, bool neg)
{
	set_smallint(&p->v, 0);
	p->v.flags = 0;
	const char *s = *srcptr;

	if ((*s == '.') && isdigit(s[1])) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line);

		p->error = true;
		return false;
	}

	if (!isdigit(*s))
		return false;

	if ((*s == '0') && (s[1] == '\'')) {
		s += 2;
		int v;

		if (*s == '\\') {
			s++;
			v = get_escape(&s, &p->error);
		} else if ((*s == '\'') && (s[1] == '\'')) {
			s += 2;
			v = '\'';
		} else
			v = get_char_utf8(&s);

		p->v.tag = TAG_INTEGER;
		set_smallint(&p->v, v);
		if (neg) set_smallint(&p->v, -get_smallint(&p->v));
		*srcptr = s;
		return true;
	}

	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	char *tmpptr = (char*)s;

	if ((*s == '0') && (s[1] == 'b')) {
		s += 2;

		mp_int_read_cstring(&v2, 2, s, (char**)&s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_integer = -p->v.val_integer;
			mp_int_clear(&v2);
		}

		int ch = peek_char_utf8(s);

		if (isdigit(ch) || iswalpha(ch)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing binary, line %u, '%s\n", p->line_nbr, p->save_line);

			p->error = true;
			return false;
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_BINARY;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		mp_int_read_cstring(&v2, 8, s, (char**)&s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_integer = -p->v.val_integer;
			mp_int_clear(&v2);
		}

		int ch = peek_char_utf8(s);

		if (isdigit(ch) || iswalpha(ch)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing octal, line %u, '%s'\n", p->line_nbr, p->save_line);

			p->error = true;
			return false;
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_OCTAL;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		mp_int_read_cstring(&v2, 16, s, (char**)&s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_integer = -p->v.val_integer;
			mp_int_clear(&v2);
		}

		int ch = peek_char_utf8(s);

		if (isdigit(ch) || iswalpha(ch)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing hexadecimal, line %u, '%s'\n", p->line_nbr, p->save_line);

			p->error = true;
			return false;
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_HEX;
		*srcptr = s;
		return true;
	}

	mp_int_read_cstring(&v2, 10, s, (char**)&s);

	if (s && (*s == '.') && isdigit(s[1])) {
		p->v.tag = TAG_REAL;
		double v = strtod(tmpptr, &tmpptr);
		set_real(&p->v, v);
		if (neg) p->v.val_real = -p->v.val_real;
		*srcptr = tmpptr;
		mp_int_clear(&v2);
		return true;
	}

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		p->v.val_bigint = malloc(sizeof(bigint));
		p->v.val_bigint->refcnt = 1;
		mp_int_init_copy(&p->v.val_bigint->ival, &v2);
		if (neg) p->v.val_bigint->ival.sign = MP_NEG;
		p->v.flags |= FLAG_MANAGED;
	} else {
		set_smallint(&p->v, val);
		if (neg) p->v.val_integer = -p->v.val_integer;
	}

	mp_int_clear(&v2);
	int ch = peek_char_utf8(s);

	if (iswalpha(ch)) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number, line %u, '%s'\n", p->line_nbr, p->save_line);

		p->error = true;
		return false;
	}

	p->v.tag = TAG_INTEGER;

	if ((s[-1] == '.') || iswspace(s[-1]))
		s--;

	*srcptr = s;
	ch = peek_char_utf8(*srcptr);

	if ((ch == '(') || iswalpha(ch)) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number, line %u, '%s'\n", p->line_nbr, p->save_line);

		p->error = true;
		return false;
	}

	return true;
}

static bool is_matching_pair(char **dst, char **src, int lh, int rh)
{
	char *s = *src, *d = *dst;

	if (*s != lh)
		return false;

	while (s++, iswspace(*s))
		;

	if (*s != rh)
		return false;

	s++;
	*d++ = lh;
	*d++ = rh;
	*d = '\0';
	*dst = d;
	*src = s;
	return true;
}

static bool valid_float(const char *src)
{
	if (*src == '-')
		src++;

	while (isdigit(*src))
		src++;

	if (*src != '.')
		return false;

	src++;

	if (!isdigit(*src))
		return false;

	return true;
}

static const char *eat_space(parser *p)
{
	const char *src = p->srcptr;
	bool done;

	do {
		done = true;

		while (iswspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}

		while ((*src == '%') && !p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			src++;
			done = false;
			continue;
		}

		if ((!*src || (*src == '%')) && p->fp) {
			if (*src == '%')
				p->line_nbr++;

			if (getline(&p->save_line, &p->n_line, p->fp) == -1)
				return NULL;

			p->srcptr = p->save_line;
			src = p->srcptr;
			done = false;
			continue;
		}

		do {
			if (!p->comment && (src[0] == '/') && (src[1] == '*')) {
				p->comment = true;
				src += 2;
				continue;
			}

			if (p->comment && (src[0] == '*') && (src[1] == '/')) {
				p->comment = false;
				src += 2;
				done = false;
				continue;
			}

			if (*src == '\n')
				p->line_nbr++;

			if (p->comment)
				src++;

			if (!*src && p->comment && p->fp) {
				if (getline(&p->save_line, &p->n_line, p->fp) == -1)
					return NULL;

				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->comment);
	}
	 while (!done);

	return src;
}

static bool get_token(parser *p, int last_op)
{
	if (p->error)
		return false;

	const char *src = p->srcptr;
	char *dst = p->token;
	*dst = '\0';
	bool neg = false;
	p->v.tag = TAG_LITERAL;
	p->v.flags = 0;
	p->quote_char = 0;
	p->string = p->is_quoted = p->is_variable = p->is_op = false;

	if (p->dq_consing && (*src == '"') && (src[1] == '"')) {
		src++;
	} else if (p->dq_consing && (*src == '"')) {
		*dst++ = ']';
		*dst = '\0';
		p->srcptr = (char*)++src;
		p->dq_consing = 0;
		return true;
	}

	if (p->dq_consing < 0) {
		*dst++ = ',';
		*dst = '\0';
		p->dq_consing = 1;
		return true;
	}

	if (p->dq_consing) {
		int ch = get_char_utf8(&src);

		if ((ch == '\\') && p->flag.character_escapes) {
			ch = get_escape(&src, &p->error);

			if (p->error) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d '%s'\n", p->srcptr, p->line_nbr, p->save_line);

				p->error = true;
				return false;
			}
		}

		dst += snprintf(dst, 8, "%u", ch);
		*dst = '\0';
		p->srcptr = (char*)src;
		set_smallint(&p->v, ch);
		p->v.tag = TAG_INTEGER;
		p->dq_consing = -1;
		return true;
	}

	if (!(src = eat_space(p)))
		return false;

	if (!*src) {
		p->srcptr = (char*)src;
		return false;
	}

	// -ve numbers (note there are no explicitly +ve numbers)

	if ((*src == '-') && last_op) {
		const char *save_src = src++;

		while (iswspace(*src)) {
			if (*src == '\n')
				p->line_nbr++;

			src++;
		}

		if (isdigit(*src)) {
			if (*save_src == '-')
				neg = true;
		} else
			src = save_src;
	}

	// Numbers...

	const char *tmpptr = src;

	if ((*src != '-') && parse_number(p, &src, neg)) {
		if ((size_t)(src-tmpptr) >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size = (src-tmpptr)+1);
			ensure(p->token);
			dst = p->token+offset;
		}

		strncpy(dst, tmpptr, src-tmpptr);
		dst[src-tmpptr] = '\0';

		if ((dst[0] != '0') && (dst[1] != 'x')) {
			if ((strchr(dst, '.') || strchr(dst, 'e') || strchr(dst, 'E')) && !strchr(dst, '\'')) {
				if (!valid_float(p->token)) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, float, line %u, '%s'\n", p->line_nbr, p->save_line);

					p->error = true;
					return false;
				}
			}
		}

		p->srcptr = (char*)src;
		return true;
	}

	// Quoted strings...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quote_char = *src++;
		p->is_quoted = true;

		if ((p->quote_char == '"') && p->flag.double_quote_codes) {
			*dst++ = '[';

			if ((*src == '"') && (src[1] != '"')) {
				*dst++ = ']';
				*dst = '\0';
				p->srcptr = (char*)++src;
				return true;
			}

			*dst = '\0';
			p->dq_consing = 1;
			p->quote_char = 0;
			p->srcptr = (char*)src;
			return true;
		} else if ((p->quote_char == '"') && p->flag.double_quote_chars)
			p->string = true;

		for (;;) {
			for (int ch; (ch = get_char_utf8(&src));) {

				if ((ch == p->quote_char) && (*src == ch)) {
					ch = *src++;
				} else if (ch == p->quote_char) {
					if ((ch == '"') && !*p->token && p->string) {
						dst += put_char_bare_utf8(dst, ch='[');
						dst += put_char_bare_utf8(dst, ch=']');
						p->string = false;
					}

					p->quote_char = 0;
					break;
				}

				if ((ch == '\\') && p->flag.character_escapes) {
					int ch2 = *src;
					ch = get_escape(&src, &p->error);

					if (!p->error) {
						if (ch2 == '\n') {
							//p->line_nbr++;
							continue;
						}
					} else {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d, '%s'\n", p->srcptr, p->line_nbr, p->save_line);

						p->error = true;
						return false;
					}
				}

				size_t len = (dst - p->token) + put_len_utf8(ch) + 1;

				if (len >= p->token_size) {
					size_t offset = dst - p->token;
					p->token = realloc(p->token, p->token_size*=2);
					ensure(p->token);
					dst = p->token + offset;
				}

				dst += put_char_bare_utf8(dst, ch);
			}

			*dst = '\0';

			if (p->quote_char && p->fp) {
				if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = NULL;

					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, line %d, '%s'\n", p->line_nbr, p->save_line);

					p->error = true;
					return false;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			if (get_op(p->m, p->token, NULL, false)) {
				p->is_op = true;

				if (!strcmp(p->token, ","))
					p->quote_char = -1;
			} else
				p->quote_char = -1;

			p->toklen = dst - p->token;
			p->srcptr = (char*)src;
			return true;
		}
	}

	// Atoms...

	int ch = peek_char_utf8(src);

	if (iswalpha(ch) || (ch == '_')) {
		while (iswalnum(ch) || (ch == '_')) {
			if ((src[0] == ':') && (src[1] == ':'))	// HACK
				break;

			ch = get_char_utf8(&src);

			size_t len = (dst - p->token) + put_len_utf8(ch) + 1;

			if (len >= p->token_size) {
				size_t offset = dst - p->token;
				p->token = realloc(p->token, p->token_size*=2);
				ensure(p->token);
				dst = p->token+offset;
			}

			dst += put_char_bare_utf8(dst, ch);
			ch = peek_char_utf8(src);
		}

		*dst = '\0';

		int ch_start = peek_char_utf8(p->token);

		if (iswupper(ch_start) || (ch_start == '_'))
			p->is_variable = true;
		else if (search_op(p->m, p->token, NULL, false))
			p->is_op = true;

		if (iswspace(ch)) {
			p->srcptr = (char*)src;
			src = eat_space(p);

			if (!p->is_op && (*src == '(')) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, or operator expected, line %d: %s, '%s'\n", p->line_nbr, p->token, p->save_line);

				p->error = true;
			}
		}

		p->srcptr = (char*)src;
		return true;
	}

	if (is_matching_pair(&dst, (char**)&src, '[',']') ||
		is_matching_pair(&dst, (char**)&src, '{','}')) {
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	if ((src[0] == '=') && (src[1] == '.') && (src[2] == '.')) {
		dst += sprintf(dst, "=..");
		p->srcptr = (char*)src+3;
		return (dst - p->token) != 0;
	}


	if (src[0] == '!') {
		*dst++ = *src++;
		*dst = '\0';
		p->srcptr = (char*)src;
		return (dst - p->token) != 0;
	}

	static const char *s_delims = "!(){}[]|_,;`'\"\t\r\n ";

	while (*src) {
		ch = get_char_utf8(&src);
		size_t len = (dst-p->token) + put_len_utf8(ch) + 1;

		if (len >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token+offset;
		}

		dst += put_char_utf8(dst, ch);
		*dst = '\0';

		if (strchr(s_delims, ch))
			break;

		if ((ch == '.') && iswspace(*src))
			break;

		ch = peek_char_utf8(src);

		if (strchr(s_delims, ch) || iswalnum(ch) || (ch == '_'))
			break;
	}

	p->is_op = search_op(p->m, p->token, NULL, false);
	p->srcptr = (char*)src;
	return true;
}

void fix_list(cell *c)
{
	idx_t cnt = c->nbr_cells;

	while (is_iso_list(c)) {
		c->nbr_cells = cnt;
		c = c + 1;					// skip .
		cnt -= 1 + c->nbr_cells;
		c = c + c->nbr_cells;		// skip head
	}
}

unsigned tokenize(parser *p, bool args, bool consing)
{
	idx_t begin_idx = p->t->cidx, arg_idx = p->t->cidx, save_idx = 0;
	bool last_op = true, is_func = false, was_consing = false;
	bool last_bar = false;
	unsigned arity = 1;
	p->depth++;

	while (get_token(p, last_op)) {
		if (p->error)
			break;

		//fprintf(stdout, "Debug: token '%s' quoted=%d, tag=%u, op=%d, lastop=%d, string=%d\n", p->token, p->quote_char, p->v.tag, p->is_op, last_op, p->string);

		if (!p->quote_char && !strcmp(p->token, ".")
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '|')) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (DUMP_ERRS || !p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, line %u '%s'\n", p->line_nbr, p->save_line);

				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (lexer_analyze(p, 0)) {
				if (p->t->cells->nbr_cells < (p->t->cidx-1)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: syntax error, operator expected '%s', line %u, '%s'\n", p->token, p->line_nbr, p->save_line);

					p->error = true;
				}

				term_assign_vars(p, p->read_term, false);
				term_to_body(p);

				if (p->consulting && !p->skip) {
					if (!term_dcg_rewrite(p))
						directives(p, p->t);

					if (p->already_loaded)
						break;

					cell *h = get_head(p->t->cells);

					if (is_cstring(h)) {
						idx_t off = index_from_pool(p->m->pl, PARSER_GET_STR(h));
						if (off == ERR_IDX) {
							p->error = true;
							break;
						}

						unshare_cell(h);
						h->tag = TAG_LITERAL;
						h->val_off = off;
						h->flags = 0;
					}

					if (!p->error && !assertz_to_db(p->m, p->t, 1)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: '%s', line %u\n", p->token, p->line_nbr);

						p->error = true;
					}
				}
			}

			p->end_of_term = true;
			last_op = true;

			if (p->one_shot)
				break;

			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "[")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			p->nesting_brackets++;
			tokenize(p, true, true);
			last_bar = false;

			if (p->error)
				break;

			make_literal(p, g_nil_s);
			c = p->t->cells + save_idx;
			c->nbr_cells = p->t->cidx - save_idx;
			fix_list(c);
			p->start_term = false;
			last_op = false;
			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "{")) {
			save_idx = p->t->cidx;
			cell *c = make_literal(p, g_braces_s);
			ensure(c);
			c->arity = 1;
			p->start_term = true;
			p->nesting_braces++;
			tokenize(p, false, false);
			last_bar = false;

			if (p->error)
				break;

			c = p->t->cells+save_idx;
			c->nbr_cells = p->t->cidx - save_idx;
			p->start_term = false;
			last_op = false;
			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "(")) {
			p->start_term = true;
			p->nesting_parens++;
			unsigned tmp_arity = tokenize(p, is_func, false);
			last_bar = false;

			if (p->error)
				break;

			if (is_func) {
				cell *c = p->t->cells + save_idx;
				c->arity = tmp_arity;
				c->nbr_cells = p->t->cidx - save_idx;
			}

			is_func = false;
			last_op = false;
			p->start_term = false;
			continue;
		}

		if (!p->quote_char && args && !consing && p->is_op
			&& strcmp(p->token, ",")
			) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, p->token, &specifier, last_op);

			if (!last_op && (priority > 1000)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: suggest parens around operator '%s', line %d %s\n", p->token, p->line_nbr, p->srcptr);

				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && !strcmp(p->token, ",")) {
			if ((*p->srcptr == ',') && !p->flag.double_quote_codes) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing element '%s'\n", p->save_line);

				p->error = true;
				break;
			}

			if (was_consing) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line);

				p->error = true;
				break;
			}

			cell *c = make_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			last_op = true;
			continue;
		}

		if (!p->quote_char && args && !strcmp(p->token, ",")) {
			lexer_analyze(p, arg_idx);
			arg_idx = p->t->cidx;

			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing arg '%s'\n", p->save_line);

				p->error = true;
				break;
			}

			arity++;

			if (arity > MAX_ARITY) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: max arity reached, line %d '%s'\n", p->line_nbr, p->save_line);

				p->error = true;
				break;
			}

			last_op = true;
			continue;
		}

		if (!p->is_quoted && consing && p->start_term && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line);

			p->error = true;
			break;
		}

		if (!p->is_quoted && was_consing && consing && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line);

			p->error = true;
			break;
		}

		if (!p->is_quoted && consing && !strcmp(p->token, "|")) {
			last_bar = true;
			was_consing = true;
			//consing = false;
			continue;
		}

		if (!p->is_quoted && was_consing && last_bar && !strcmp(p->token, "]")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line);

			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!strcmp(p->token, ",") || !strcmp(p->token, "]") || !strcmp(p->token, ")") || !strcmp(p->token, "}"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, start of term expected, line %d '%s'\n", p->line_nbr, p->save_line);

			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, ")")) {
			p->nesting_parens--;
			lexer_analyze(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "]")) {
			p->nesting_brackets--;
			lexer_analyze(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "}")) {
			p->nesting_braces--;
			lexer_analyze(p, begin_idx);
			return arity;
		}

		if (p->is_variable && (*p->srcptr == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, line %d '%s'\n", p->line_nbr, p->save_line);

			p->error = true;
			break;
		}

		unsigned specifier = 0;
		int priority = search_op(p->m, p->token, &specifier, last_op);

		if (!strcmp(p->token, "!") &&
			((*p->srcptr == ',') || (*p->srcptr == '.')))
			p->quote_char = 1;

		if (p->quote_char) {
			specifier = 0;
			priority = 0;
		}

		if (priority && (last_op || last_bar)
			&& !IS_POSTFIX(specifier)) {
			int nextch = *eat_space(p);

			if ((nextch == ',')
				|| (nextch == ')')
				|| (nextch == '|')
				|| (nextch == ']')
				|| (nextch == '}')
			) {
				specifier = 0;
				priority = 0;
			}
		}

		if (priority && IS_POSTFIX(specifier) && args && last_op) {
			specifier = 0;
			priority = 0;
		}

#if 0
		if (priority
			&& ((specifier == OP_XF) || (specifier == OP_YF))
			&& last_op) {
			specifier = 0;
			priority = 0;
		}
#endif

		if (priority && (specifier == OP_YFX) && last_op) {
			specifier = 0;
			priority = 0;
		}

		// Operators in canonical form..

		if (last_op && priority && (*p->srcptr == '(')) {
			p->v.tag = TAG_LITERAL;
			specifier = 0;
			priority = 0;
			p->quote_char = 0;
		}

		last_op = strcmp(p->token, ")") && priority;
		int func = is_literal(&p->v) && !specifier && (*p->srcptr == '(');

		if (func) {
			is_func = true;
			p->is_op = false;
			save_idx = p->t->cidx;
		}

		p->start_term = false;
		cell *c = make_cell(p);
		c->nbr_cells = 1;
		c->tag = p->v.tag;
		c->flags = p->v.flags;
		SET_OP(c,specifier);
		c->priority = priority;
		bool found = false;

		if (is_bigint(&p->v)) {
			c->val_bigint = p->v.val_bigint;
		} else if (is_smallint(&p->v)) {
			set_smallint(c, get_smallint(&p->v));
		} else if (p->v.tag == TAG_REAL) {
			set_real(c, get_real(&p->v));
		} else if ((!p->is_quoted || func || p->is_op || p->is_variable ||
			(get_builtin(p->m->pl, p->token, 0, &found), found) ||
			!strcmp(p->token, "[]")) && !p->string) {

			if (func && !strcmp(p->token, "."))
				c->priority = 0;

			if (p->is_variable)
				c->tag = TAG_VARIABLE;

			if (p->is_quoted)
				c->flags |= FLAG2_QUOTED;

			c->val_off = index_from_pool(p->m->pl, p->token);
			ensure(c->val_off != ERR_IDX);
		} else {
			c->tag = TAG_CSTRING;

			if ((p->toklen < MAX_SMALL_STRING) && !p->string) {
				memcpy(c->val_chr, p->token, p->toklen);
				c->val_chr[p->toklen] = '\0';
			} else {
				if (p->string) {
					c->flags |= FLAG_STRING;
					c->arity = 2;
				}

				SET_STR(c, p->token, p->toklen, 0);
			}
		}

		last_bar = false;
	}

	p->depth--;
	return !p->error;
}

bool run(parser *p, const char *pSrc, bool dump, bool is_init)
{
	if (*pSrc == '.') {
		fprintf(stdout, "Error: syntax error, unexpected end of clause\n");
		return false;
	}

	if (!is_init) {
		STRING_alloc(src);
		STRING_strcat2(src, "call((", pSrc);
		STRING_trim(src, '.');
		STRING_strcat(src, ")).");

		p->srcptr = STRING_cstr(src);
		p->line_nbr = 0;
		tokenize(p, false, false);
		STRING_free(src);
	} else {
		p->srcptr = (char*)pSrc;
		p->line_nbr = 0;
		tokenize(p, false, false);
	}

	if (!p->error && !p->end_of_term && !p->run_init) {
		fprintf(stdout, "Error: syntax error, missing operand or operator\n");
		p->error = true;
	}

	if (p->error)
		return false;

	if (p->skip) {
		p->m->pl->status = 1;
		return true;
	}

	if (!lexer_analyze(p, 0))
		return false;

	term_assign_vars(p, 0, false);

	if (!p->command)
		term_dcg_rewrite(p);

	term_xref(p, p->t, NULL);

	query *q = create_query(p->m, false);
	if (!q) return false;
	q->p = p;
	q->do_dump_vars = dump;
	q->run_init = p->run_init;
	execute(q, p->t);

	p->m->pl->halt = q->halt;
	p->m->pl->halt_code = q->halt_code;
	p->m->pl->status = q->status;

	if (!p->m->pl->quiet && !p->directive && dump && q->st.m->pl->stats) {
		fprintf(stdout,
			"Goals %llu, Matches %llu, Max frames %u, Max choices %u, Max trails: %u, Backtracks %llu, TCOs:%llu\n",
			(unsigned long long)q->tot_goals, (unsigned long long)q->tot_matches,
			q->max_frames, q->max_choices, q->max_trails,
			(unsigned long long)q->tot_retries, (unsigned long long)q->tot_tcos);
	}

	bool ok = !q->error;
	p->m = q->st.m;
	destroy_query(q);
	return ok;
}
