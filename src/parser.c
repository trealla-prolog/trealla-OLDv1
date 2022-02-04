#include <stdlib.h>
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
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "builtins.h"
#include "utf8.h"

static const unsigned INITIAL_TOKEN_SIZE = 100;		// bytes
const char *g_solo = "!(){}[]|,;`'\"";

char *slicedup(const char *s, size_t n)
{
	char *ptr = malloc(n+1);
	if (!ptr) return NULL;
	memcpy(ptr, s, n);
	ptr[n] = '\0';
	return ptr;
}

int slicecmp(const char *s1, size_t len1, const char *s2, size_t len2)
{
	size_t min_len = len1 < len2 ? len1 : len2;
	int val = memcmp(s1, s2, min_len);
	if (val) return val > 0 ? 1 : -1;
	return len1 < len2 ? -1 : len1 > len2 ? 1 : 0;
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
	tmp->tag = TAG_CSTR;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	memcpy(tmp->val_chr, src, len);
	tmp->val_chr[len] = '\0';
	tmp->chr_len = len;
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

bool check_if_rule(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_neck_s) && (c->arity == 2))
		return true;

	return false;
}

cell *get_head(cell *c)
{
	if (check_if_rule(c))
		return c + 1;

	return c;
}

cell *get_body(cell *c)
{
	if (check_if_rule(c)) {
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

void clear_rule(clause *r)
{
	if (!r)
		return;

	for (pl_idx_t i = 0; i < r->cidx; i++) {
		cell *c = r->cells + i;
		unshare_cell(c);
		*c = (cell){0};
		c->tag = TAG_EMPTY;
	}

	r->cidx = 0;
}

static bool make_room(parser *p)
{
	if (p->cl->cidx == p->cl->nbr_cells) {
		pl_idx_t nbr_cells = p->cl->nbr_cells * 2;

		clause *r = realloc(p->cl, sizeof(clause)+(sizeof(cell)*nbr_cells));
		if (!r) {
			p->error = true;
			return false;
		}

		p->cl = r;
		p->cl->nbr_cells = nbr_cells;
	}

	return true;
}

static cell *make_a_cell(parser *p)
{
	make_room(p);
	cell *ret = p->cl->cells + p->cl->cidx++;
	*ret = (cell){0};
	return ret;
}

void destroy_parser(parser *p)
{
	free(p->tmpbuf);
	free(p->save_line);
	free(p->token);
	clear_rule(p->cl);
	free(p->cl);
	free(p);
}

parser *create_parser(module *m)
{
	parser *p = calloc(1, sizeof(parser));
	ensure(p);
	p->pl = m->pl;
	p->m = m;
	p->token = calloc(p->token_size=INITIAL_TOKEN_SIZE+1, 1);
	ensure(p->token, free(p));
	pl_idx_t nbr_cells = INITIAL_NBR_CELLS;
	p->cl = calloc(sizeof(clause)+(sizeof(cell)*nbr_cells), 1);
	ensure(p->cl, (free(p->token), free(p)));
	p->cl->nbr_cells = nbr_cells;
	p->start_term = true;
	p->flag = m->flag;
	p->line_nbr = 1;
	return p;
}

static void consultall(parser *p, cell *l)
{
	LIST_HANDLER(l);

	while (is_list(l) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(l);

		if (is_iso_list(h))
			consultall(p, h);
		else {
			char *s = GET_STR(p, h);

			if (!load_file(p->m, s, false))
				fprintf(stdout, "Error: file not found: '%s'\n", s);
		}

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

	if (!is_integer(p1) || !is_literal(p2) || (!is_atom(p3) && !is_list(p3))) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: unknown op\n");

		p->error = true;
		return;
	}

	unsigned specifier;
	char *spec = DUP_SLICE(p, p2);

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
		free(spec);
		return;
	}

	free(spec);
	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);

		if (is_atom(h)) {
			char *name = DUP_SLICE(p, h);

			if (!set_op(p->m, name, specifier, get_int(p1))) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op\n");

				free(name);
				continue;
			}

			free(name);
		}

		p3 = LIST_TAIL(p3);
	}

	if (is_atom(p3) && !is_nil(p3)) {
		char *name = DUP_SLICE(p, p3);

		if (!set_op(p->m, name, specifier, get_int(p1))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: could not set op\n");

			free(name);
			return;
		}

		free(name);
	}
}

static void directives(parser *p, cell *d)
{
	p->skip = false;

	if (!is_literal(d))
		return;

	if (is_list(d) && p->command) {
		consultall(p, d);
		p->skip = true;
		return;
	}

	if (strcmp(GET_STR(p, d), ":-") || (d->arity != 1))
		return;

	cell *c = d + 1;

	if (!is_literal(c))
		return;

	const char *dirname = GET_STR(p, c);

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		unsigned save_line_nbr = p->line_nbr;
		const char *name = GET_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, true)) {
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
		unsigned save_line_nbr = p->line_nbr;
		const char *name = GET_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, false)) {
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
			name = GET_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;

			if (tmp_m != p->m)
				p->m->used[p->m->idx_used++] = tmp_m;

			return;
		}

		tmp_m = create_module(p->m->pl, name);
		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

			p->error = true;
			return;
		}

		if (tmp_m != p->m)
			p->m->used[p->m->idx_used++] = tmp_m;

		LIST_HANDLER(p2);

		while (is_iso_list(p2) && !g_tpl_interrupt) {
			LIST_HEAD(p2);
			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "attribute") && (c->arity == 1)) {
		cell *arg = c + 1;
		cell *attr = arg+1;
		const char *name = GET_STR(p, attr);

		if (strcmp(name, p->m->name))
			duplicate_module(p->m->pl, p->m, name);

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
			name = GET_STR(p, p1);

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

		p->m = tmp_m;
		LIST_HANDLER(p2);

		while (is_iso_list(p2) && !g_tpl_interrupt) {
			cell *head = LIST_HEAD(p2);

			if (is_structure(head)) {
				if (!strcmp(GET_STR(p, head), "/")
					|| !strcmp(GET_STR(p, head), "//")) {
					cell *f = head+1, *a = f+1;
					if (!is_literal(f)) return;
					if (!is_integer(a)) return;
					cell tmp = *f;
					tmp.arity = get_int(a);

					if (!strcmp(GET_STR(p, head), "//"))
						tmp.arity += 2;

					predicate *pr = find_predicate(p->m, &tmp);
					if (!pr) pr = create_predicate(p->m, &tmp);
					if (!pr) {
						destroy_module(p->m);
						p->m = NULL;
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: predicate creation failed\n");

						p->error = true;
						return;
					}

					pr->is_public = true;
				} else if (!strcmp(GET_STR(p, head), "op") && (head->arity == 3)) {
					do_op(p, head);
					// TO-DO: make public...
				}
			}

			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if ((!strcmp(dirname, "use_module") || !strcmp(dirname, "autoload") || !strcmp(dirname, "reexport")) && (c->arity >= 1)) {
		if (!is_atom(p1) && !is_structure(p1)) return;
		const char *name = GET_STR(p, p1);
		char dstbuf[1024*2];

		if (!strcmp(name, "library")) {
			p1 = p1 + 1;
			if (!is_literal(p1)) return;
			name = GET_STR(p, p1);
			module *m;

			if ((m = find_module(p->m->pl, name)) != NULL) {
				if (!m->fp)
					do_db_load(m);

				if (m != p->m)
					p->m->used[p->m->idx_used++] = m;

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

				ASTRING(src);
				ASTRING_strcatn(src, lib->start, *lib->len);
				ASTRING(s1);
				ASTRING_sprintf(s1, "library/%s", lib->name);
				m = load_text(p->m, ASTRING_cstr(src), ASTRING_cstr(s1));
				ASTRING_free(src);
				ASTRING_free(s1);

				if (m != p->m)
					do_db_load(m);

				if (m != p->m)
					p->m->used[p->m->idx_used++] = m;

				return;
			}

			query q = (query){0};
			q.pl = p->pl;
			q.st.m = p->m;
			snprintf(dstbuf, sizeof(dstbuf), "%s/", g_tpl_lib);
			char *dst = dstbuf + strlen(dstbuf);
			pl_idx_t ctx = 0;
			print_term_to_buf(&q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, false, 0);
			name = dstbuf;
		}

		char *filename = relative_to(p->m->filename, name);
		module *m;

		if (!(m = load_file(p->m, filename, false))) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: using module file: %s\n", filename);

			//p->error = true;
			free(filename);
			return;
		}

		free(filename);

		if (m != p->m)
			p->m->used[p->m->idx_used++] = m;

		return;
	}

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_structure(p1)) return;
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;

		if (!strcmp(GET_STR(p, p1), "indexing_threshold") && is_smallint(p2))
			p->m->indexing_threshold = get_smallint(p2);

		if (!is_literal(p2)) return;

		if (!strcmp(GET_STR(p, p1), "double_quotes")) {
			if (!strcmp(GET_STR(p, p2), "atom")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_atom = true;
			} else if (!strcmp(GET_STR(p, p2), "codes")) {
				p->m->flag.double_quote_chars = p->m->flag.double_quote_atom = false;
				p->m->flag.double_quote_codes = true;
			} else if (!strcmp(GET_STR(p, p2), "chars")) {
				p->m->flag.double_quote_atom = p->m->flag.double_quote_codes = false;
				p->m->flag.double_quote_chars = true;
			} else {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: unknown value\n");

				p->error = true;
				return;
			}
		} else if (!strcmp(GET_STR(p, p1), "character_escapes")) {
			if (!strcmp(GET_STR(p, p2), "true") || !strcmp(GET_STR(p, p2), "on"))
				p->m->flag.character_escapes = true;
			else if (!strcmp(GET_STR(p, p2), "false") || !strcmp(GET_STR(p, p2), "off"))
				p->m->flag.character_escapes = false;
		} else {
			//fprintf(stdout, "Warning: unknown flag: %s\n", GET_STR(p, p1));
		}

		p->flag = p->m->flag;
		return;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		do_op(p, c);
		return;
	}

	if (!strcmp(dirname, "if") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	if (!strcmp(dirname, "else") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	if (!strcmp(dirname, "endif") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	LIST_HANDLER(p1);

	while (is_list(p1) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p1);

		if (is_literal(h) && (!strcmp(GET_STR(p, h), "/") || !strcmp(GET_STR(p, h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;
			if (!is_atom(c_name)) continue;
			cell *c_arity = h + 2;
			if (!is_integer(c_arity)) continue;
			unsigned arity = get_int(c_arity);

			if (!strcmp(GET_STR(p, h), "//"))
				arity += 2;

			//printf("*** %s => %s / %u\n", dirname, GET_STR(p, c_name), arity);

			if (!strcmp(dirname, "dynamic")) {
				set_dynamic_in_db(p->m, GET_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				set_persist_in_db(p->m, GET_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, GET_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = GET_STR(p, c_name);

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
			} else {
				if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
					fprintf(stdout, "Warning: unknown directive: %s\n", dirname);
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return;

	//printf("*** %s\n", dirname);

	while (is_literal(p1) && !g_tpl_interrupt) {
		module *m = p->m;
		cell *c_id = p1;

		if (!strcmp(GET_STR(p, p1), ":") && (p1->arity == 2)) {
			cell *c_mod = p1 + 1;
			if (!is_atom(c_mod)) return;
			m = find_module(p->m->pl, GET_STR(p, c_mod));
			c_id = p1 + 2;
		}

		if (!strcmp(GET_STR(p, c_id), "/") && (p1->arity == 2)) {
			cell *c_name = c_id + 1;
			if (!is_atom(c_name)) return;
			cell *c_arity = c_id + 2;
			if (!is_integer(c_arity)) return;
			unsigned arity = get_int(c_arity);

			//printf("*** *** *** %s : %s / %u\n", m->name, GET_STR(p, c_name), arity);

			if (!strcmp(GET_STR(p, c_id), "//"))
				arity += 2;

			if (!strcmp(dirname, "multifile"))
				set_multifile_in_db(m, GET_STR(p, c_name), arity);
			else if (!strcmp(dirname, "discontiguous"))
				set_discontiguous_in_db(m, GET_STR(p, c_name), arity);
			else if (!strcmp(dirname, "dynamic"))
				set_dynamic_in_db(m, GET_STR(p, c_name), arity);
			else if (!strcmp(dirname, "persist"))
				set_persist_in_db(m, GET_STR(p, c_name), arity);

			p1 += p1->nbr_cells;
		} else if (!strcmp(dirname, "meta_predicate")) {
			set_meta_predicate_in_db(m, p1);
			p1 += p1->nbr_cells;
		} else if (!strcmp(GET_STR(p, p1), ",") && (p1->arity == 2))
			p1 += 1;
		else {
			if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
				fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

			break;
		}
	}
}

static void check_first_cut(parser *p)
{
	cell *c = get_body(p->cl->cells);
	int is_cut_only = true;

	if (!c)
		return;

	while (!is_end(c)) {
		if (!(c->flags&FLAG_BUILTIN))
			break;

		if (!strcmp(GET_STR(p, c), ","))
			;
		else if (!IS_OP(c) && !strcmp(GET_STR(p, c), "!")) {
			p->cl->is_first_cut = true;
			break;
		} else {
			is_cut_only = false;
			break;
		}

		c += c->nbr_cells;
	}

	if (p->cl->is_first_cut && is_cut_only)
		p->cl->is_cut_only = true;
}

static pl_idx_t get_varno(parser *p, const char *src)
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
	clause *r = p->cl;
	r->nbr_vars = 0;
	r->is_first_cut = false;
	r->is_cut_only = false;

	for (pl_idx_t i = 0; i < r->cidx; i++) {
		cell *c = r->cells + i;

		if (!is_variable(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, GET_STR(p, c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_ARITY) {
			fprintf(stdout, "Error: max vars reached\n");
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = GET_STR(p, c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			c->flags |= FLAG2_FIRST_USE;
			r->nbr_vars++;
			p->nbr_vars++;
		}
	}

	for (pl_idx_t i = 0; i < r->nbr_vars; i++) {
		if (p->consulting && !p->do_read_term && (p->vartab.var_used[i] == 1) &&
			(p->vartab.var_name[i][strlen(p->vartab.var_name[i])-1] != '_') &&
			(*p->vartab.var_name[i] != '_')) {
			if (!p->m->pl->quiet)
				fprintf(stdout, "Warning: singleton: %s, near line %u, file '%s'\n", p->vartab.var_name[i], p->line_nbr, get_filename(p->m->filename));
		}
	}

	for (pl_idx_t i = 0; i < r->cidx; i++) {
		cell *c = r->cells + i;

		if (!is_variable(c))
			continue;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG2_ANON;
	}

	cell *c = make_a_cell(p);
	ensure(c);
	c->tag = TAG_END;
	c->nbr_cells = 1;
	check_first_cut(p);
	p->cl->is_fact = !get_logical_body(p->cl->cells);
}

static cell *insert_here(parser *p, cell *c, cell *p1)
{
	pl_idx_t c_idx = c - p->cl->cells, p1_idx = p1 - p->cl->cells;
	make_room(p);

	cell *last = p->cl->cells + (p->cl->cidx - 1);
	pl_idx_t cells_to_move = p->cl->cidx - p1_idx;
	cell *dst = last + 1;

	while (cells_to_move--)
		*dst-- = *last--;

	p1 = p->cl->cells + p1_idx;
	p1->tag = TAG_LITERAL;
	p1->flags = FLAG_BUILTIN;
	p1->fn = fn_iso_call_n;
	p1->val_off = g_call_s;
	p1->nbr_cells = 2;
	p1->arity = 1;

	p->cl->cidx++;
	return p->cl->cells + c_idx;
}

cell *check_body_callable(parser *p, cell *c)
{
	if (IS_XFX(c) || IS_XFY(c)) {
		if (!strcmp(GET_STR(p, c), ",")
			|| !strcmp(GET_STR(p, c), ";")
			|| !strcmp(GET_STR(p, c), "->")
			|| !strcmp(GET_STR(p, c), "*->")
			|| !strcmp(GET_STR(p, c), ":-")) {
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
	pl_idx_t c_idx = c - p->cl->cells;

	if (IS_XFX(c) || IS_XFY(c)) {
		if ((c->val_off == g_conjunction_s)
			|| (c->val_off == g_disjunction_s)
			|| (c->val_off == g_if_then_s)
			|| (c->val_off == g_soft_cut_s)
			|| (c->val_off == g_neck_s)) {
			cell *lhs = c + 1;
			bool norhs = false;

			//if (c->val_off == g_soft_cut_s)
			//	norhs = true;

			if (is_variable(lhs)) {
				c = insert_here(p, c, lhs);
				lhs = c + 1;
			} else
				lhs = term_to_body_conversion(p, lhs);

			cell *rhs = lhs + lhs->nbr_cells;
			c = p->cl->cells + c_idx;

			if (is_variable(rhs) && !norhs)
				c = insert_here(p, c, rhs);
			else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + lhs->nbr_cells + rhs->nbr_cells;
		}
	}

	if (IS_FY(c)) {
		if (c->val_off == g_negation_s) {
			cell *rhs = c + 1;

			if (is_variable(rhs)) {
				c = insert_here(p, c, rhs);
				rhs = c + 1;
			} else
				rhs = term_to_body_conversion(p, rhs);

			c->nbr_cells = 1 + rhs->nbr_cells;
		}
	}

	return p->cl->cells + c_idx;
}

void term_to_body(parser *p)
{
	term_to_body_conversion(p, p->cl->cells);
	p->cl->cells->nbr_cells = p->cl->cidx - 1;
}

static bool reduce(parser *p, pl_idx_t start_idx)
{
	pl_idx_t lowest = IDX_MAX, work_idx, end_idx = p->cl->cidx - 1;
	bool do_work = false, bind_le = false;

	for (pl_idx_t i = start_idx; i < p->cl->cidx;) {
		cell *c = p->cl->cells + i;

		//printf("*** OP0 %s type=%u, specifier=%u, pri=%u, last_op=%d, is_op=%d\n", GET_STR(p, c), c->tag, GET_OP(c), c->priority, last_op, IS_OP(c));

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

	pl_idx_t last_idx = 0;

	for (pl_idx_t i = start_idx; i <= end_idx;) {
		cell *c = p->cl->cells + i;

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

		//printf("*** OP1 %s type=%u, specifier=%u, pri=%u\n", GET_STR(p, c), c->tag, GET_OP(c), c->priority);

		c->tag = TAG_LITERAL;
		c->arity = 1;

		// Prefix...

		if (IS_FX(c)) {
			cell *rhs = c + 1;

			if (IS_FX(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			rhs += rhs->nbr_cells;

			if ((((pl_idx_t)(rhs - p->cl->cells)) < end_idx)
				&& IS_XF(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (IS_FX(c) || IS_FY(c)) {
			cell *rhs = c + 1;
			c->nbr_cells += rhs->nbr_cells;
			pl_idx_t off = (pl_idx_t)(rhs - p->cl->cells);

			if (off > end_idx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: missing operand to '%s', line %u, '%s'\n", GET_STR(p, c), p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "operand_missing";
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

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		if (IS_XF(c) || IS_YF(c)) {
			cell *lhs = p->cl->cells + last_idx;
			save.nbr_cells += lhs->nbr_cells;
			pl_idx_t cells_to_move = lhs->nbr_cells;
			lhs = c - 1;

			while (cells_to_move--)
				*c-- = *lhs--;

			*c = save;
			break;
		}

		// Infix...

		pl_idx_t off = (pl_idx_t)(rhs - p->cl->cells);

		if (off > end_idx) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: missing operand to '%s', line %u, '%s'\n", GET_STR(p, c), p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "operatand_missing";
			p->error = true;
			return false;
		}

		cell *lhs = p->cl->cells + last_idx;
		save.nbr_cells += lhs->nbr_cells;
		pl_idx_t cells_to_move = lhs->nbr_cells;
		lhs = c - 1;

		while (cells_to_move--)
			*c-- = *lhs--;

		*c = save;
		c->nbr_cells += rhs->nbr_cells;
		c->arity = 2;

		if (IS_XFX(c)) {
			cell *next = c + c->nbr_cells;
			i = next - p->cl->cells;

			if ((i <= end_idx)
				&& (IS_XFX(next))
				&& (next->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: operator clash, line %u\n", p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		break;
	}

	return true;
}

static bool analyze(parser *p, pl_idx_t start_idx)
{
	while (reduce(p, start_idx))
		;

	return !p->error;
}

void reset(parser *p)
{
	clear_rule(p->cl);
	p->cl->cidx = 0;
	p->start_term = true;
	p->comment = false;
	p->error = false;
	p->last_close = false;
}

static bool dcg_expansion(parser *p)
{
	if (!p->m->pl->dcgs && !find_module(p->m->pl, "dcgs")) {
		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, "dcgs"))
				continue;

			char *src = malloc(*lib->len+1);
			may_ptr_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			ASTRING(s);
			ASTRING_sprintf(s, "library/%s", lib->name);
			module *tmp_m = load_text(p->m, src, ASTRING_cstr(s));

			if (tmp_m) {
				p->m->used[p->m->idx_used++] = tmp_m;
				p->m->pl->dcgs = tmp_m;
			}

			ASTRING_free(s);
			free(src);
			break;
		}
	}

	query *q = create_query(p->m, false);
	ensure(q);
	char *dst = print_canonical_to_strbuf(q, p->cl->cells, 0, 0);
	ASTRING(s);
	ASTRING_sprintf(s, "dcg_translate((%s),_TermOut).", dst);
	free(dst);
	parser *p2 = create_parser(p->m);
	ensure(p2);
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = ASTRING_cstr(s);
	tokenize(p2, false, false);
	ASTRING_free(s);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);
	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			break;

		q->latest_ctx = e->ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
			q->latest_ctx = e->ctx;
		} else
			c = deref(q, &e->c, e->ctx);

		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
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

	clear_rule(p->cl);
	free(p->cl);
	p->cl = p2->cl;			// Take the completed clause
	p2->cl = NULL;
	p->nbr_vars = p2->nbr_vars;

	destroy_parser(p2);
	destroy_query(q);
	return true;
}

static bool term_expansion(parser *p)
{
	if (p->error || p->internal || !is_literal(p->cl->cells))
		return false;

	if (p->cl->cells->val_off == g_dcg_s)
		return dcg_expansion(p);

	predicate *pr = find_functor(p->m, "term_expansion", 2);

	if (!pr || !pr->cnt)
		return true;

	query *q = create_query(p->m, false);
	ensure(q);
	char *dst = print_canonical_to_strbuf(q, p->cl->cells, 0, 0);
	ASTRING(s);
	ASTRING_sprintf(s, "term_expansion((%s),_TermOut).", dst);
	free(dst);
	parser *p2 = create_parser(p->m);
	ensure(p2);
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = ASTRING_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);
	ASTRING_free(s);

	if (q->retry != QUERY_OK) {
		destroy_parser(p2);
		destroy_query(q);
		return false;
	}

	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		slot *e = GET_SLOT(f, i);

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

		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
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

	clear_rule(p->cl);
	free(p->cl);
	p->cl = p2->cl;				// Take the completed clause
	p2->cl = NULL;
	p->nbr_vars = p2->nbr_vars;

	destroy_parser(p2);
	destroy_query(q);
	return true;
}

bool virtual_term(parser *p, const char *src)
{
	parser *p2 = create_parser(p->m);
	ensure(p2);
	p2->consulting = true;
	p2->srcptr = (char*)src;
	tokenize(p2, false, false);
	destroy_parser(p2);
	return true;
}

static cell *make_a_literal(parser *p, pl_idx_t offset)
{
	cell *c = make_a_cell(p);
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

static int get_hex(const char **srcptr, unsigned n, bool *error)
{
	const char *src = *srcptr;
	unsigned orig_n = n;
	int v = 0;

	while (*src == '0') {
		src++; n--;
	}

	while (((*src >= '0') && (*src <= '9')) ||
		((*src >= 'a') && (*src <= 'f')) ||
		((*src >= 'A') && (*src <= 'F'))) {
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

	if (n && ((orig_n == 4) || (orig_n == 8))) {
		*error = true;
		return 0;
	}

	*srcptr = src;
	return v;
}

const char *g_escapes = "\e\a\f\b\t\v\r\n\x20\x7F\'\\\"`";
const char *g_anti_escapes = "eafbtvrnsd'\\\"`";
#define ALLOW_UNICODE_ESCAPE true

static int get_escape(const char **_src, bool *error, bool number)
{
	const char *src = *_src;
	int ch = *src++;
	const char *ptr = strchr(g_anti_escapes, ch);

	if (ptr)
		ch = g_escapes[ptr-g_anti_escapes];
	else if ((isdigit(ch) || (ch == 'x')
#if ALLOW_UNICODE_ESCAPE
		|| (ch == 'u') || (ch == 'U')
#endif
		)
		&& !number) {
		int unicode = 0;

		if (ch == 'x')
			ch = get_hex(&src, UINT_MAX, error);
#if ALLOW_UNICODE_ESCAPE
		else if (ch == 'U') {
			ch = get_hex(&src, 8, error);
			unicode = 1;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4, error);
			unicode = 1;
		}
#endif
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

		if ((unsigned)ch > 0x10FFFF) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: syntax error, illegal character code\n");
			*_src = src;
			*error = true;
			return 0;
		}
	} else if ((((ch != '\\') && (ch != '"') && (ch != '\'') && (ch != '\r') && (ch != '\n'))
		|| number) && !isdigit(ch)) {
		*_src = --src;
		*error = true;
		return 0;
	}

	*_src = src;
	return ch;
}

#define isbdigit(ch) (((ch) >= '0') && ((ch) <= '1'))
#define isodigit(ch) (((ch) >= '0') && ((ch) <= '7'))

void read_integer(parser *p, mp_int v2, int base, const char *src,  const char **srcptr)
{
	if (!p->tmpbuf)
		p->tmpbuf = malloc(p->tmpbuf_size=256);

	char *dst = p->tmpbuf;

	 while (*src) {
		if ((base == 2) && ((*src < '0') || (*src > '1')))
			break;

		if ((base == 8) && ((*src < '0') || (*src > '7')))
			break;

		if ((base == 10) && ((*src < '0') || (*src > '9')))
			break;

		if ((base == 16) && !isxdigit(*src))
			break;

		*dst++ = *src++;

		if ((size_t)(dst - p->tmpbuf) >= (p->tmpbuf_size-1)) {
			size_t offset = dst - p->tmpbuf;
			p->tmpbuf = realloc(p->tmpbuf, p->tmpbuf_size*=2);
			dst = p->tmpbuf + offset;
		}

		while (isblank(*src) || (*src == '_')) {
			src++;
		}
	}

	*dst = '\0';

	if ((base != 16) && !isdigit(src[-1]))
		src--;
	else if ((base == 16) && !isxdigit(src[-1]))
		src--;

	mp_int_read_cstring(v2, base, (char*)p->tmpbuf, NULL);
	*srcptr = src;
}

static bool parse_number(parser *p, const char **srcptr, bool neg)
{
	set_smallint(&p->v, 0);
	p->v.flags = 0;
	const char *s = *srcptr;

	LOOP:

	if ((*s == '.') && isdigit(s[1])) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if (!isdigit(*s))
		return false;

	if ((*s == '0') && (s[1] == '\'')
		&& (!search_op(p->m, "", NULL, false) || ((s[2] == '\'') && (s[3] == '\'')))) {
		s += 2;
		int v;

		if (*s == '\\') {
			s++;

			if ((*s == '+') || (*s == '-')) {
				if (*s == '-')
					neg = true;

				s++;

				if (*s != '\'') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

					p->error_desc = "number";
					p->error = true;
					return false;
				}

				s++;
				goto LOOP;
			}

			if (!*s || iscntrl(*s)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "number";
				p->error = true;
				return false;
			}

			v = get_escape(&s, &p->error, true);
		} else if ((*s == '\'') && s[1] == '\'') {
			s++;
			v = *s++;
#if 1
		} else if ((*s == '\'') && !p->flag.not_strict_iso && search_op(p->m, "", NULL, false)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "number";
			p->error = true;
			return false;
#endif
		} else
			v = get_char_utf8(&s);

		if (p->error) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		p->v.tag = TAG_INT;
		set_smallint(&p->v, v);
		if (neg) set_smallint(&p->v, -get_int(&p->v));
		*srcptr = s;
		return true;
	}

	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	char *tmpptr = (char*)s;

	if ((*s == '0') && (s[1] == 'b')) {
		s += 2;

		read_integer(p, &v2, 2, s, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		p->v.flags |= FLAG_BINARY;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		read_integer(p, &v2, 8, s, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		p->v.flags |= FLAG_OCTAL;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		read_integer(p, &v2, 16, s, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		p->v.flags |= FLAG_HEX;
		*srcptr = s;
		return true;
	}

	read_integer(p, &v2, 10, s, &s);

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
		if (neg) p->v.val_int = -p->v.val_int;
	}

	mp_int_clear(&v2);
	int ch;
	p->v.tag = TAG_INT;

	if ((s[-1] == '.') || isspace(s[-1]))
		s--;

	*srcptr = s;
	ch = peek_char_utf8(s);

	//if ((ch == '(') || iswalpha(ch)) {
	if (ch == '(') {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	return true;
}

static bool is_matching_pair(parser *p, char **dst, char **src, int lh, int rh)
{
	char *s = *src, *d = *dst;

	if (*s != lh)
		return false;

	char *dup_src = NULL;
	int save_off = 0;
	fpos_t pos = {0};
	int save_line_nbr = p->line_nbr;
	int save_line_nbr_start = p->line_nbr_start;

	if (p->fp && p->save_line) {
		dup_src = strdup(p->save_line);
		save_off = *src - p->save_line;
		fgetpos(p->fp, &pos);
	}

	p->srcptr = ++s;
	s = eat_space(p);

	if (!s) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

		p->error_desc = "cincomplete_statement";
		p->error = true;
		free(dup_src);
		return false;
	}

	if (*s != rh) {
		if (p->did_getline) {
			fsetpos(p->fp, &pos);
			free(p->save_line);
			p->save_line = dup_src;
			p->n_line = 0;
			p->srcptr = *src = dup_src + save_off;
			p->line_nbr = save_line_nbr;
			p->line_nbr_start = save_line_nbr_start;
		} else
			free(dup_src);

		return false;
	}

	s++;
	*d++ = lh;
	*d++ = rh;
	*d = '\0';
	*dst = d;
	p->srcptr = s;
	free(dup_src);
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

char *eat_space(parser *p)
{
	p->did_getline = false;
	const char *src = p->srcptr;
	bool done;

	do {
		done = true;
		int ch = peek_char_utf8(src);

		while (iswspace(ch)) {
			if (ch == '\n')
				p->line_nbr++;

			get_char_utf8(&src);
			ch = peek_char_utf8(src);
		}

		if ((*src == '%') && !p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			src++;
			done = false;
			continue;
		}

		if ((!*src || (*src == '%')) && p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			if (*src) {
				src++;
				done = false;
				continue;
			}

			if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
				p->srcptr = NULL;
				return NULL;
			}

			p->did_getline = true;
			src = p->srcptr = p->save_line;
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
				if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = NULL;
					return NULL;
				}

				p->did_getline = true;
				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->comment);
	}
	 while (!done);

	return (char*)src;
}

static bool eat_comment(parser *p)
{
	char *src = p->srcptr;

	if (*src != '/')
		return true;

	src++;

	if (*src != '*')
		return true;

	src++;

	while (*src) {
		if ((src[0] == '*') && (src[1] == '/')) {
			src += 2;
			p->srcptr = src;
			return true;
		}

		src++;
	}

	return true;
}

bool get_token(parser *p, int last_op)
{
	if (p->error || !p->srcptr)
		return false;

	const char *src = p->srcptr;
	char *dst = p->token;
	*dst = '\0';
	bool neg = false;
	p->v.tag = TAG_LITERAL;
	p->v.flags = 0;
	p->v.nbr_cells = 1;
	p->quote_char = 0;
	p->string = p->is_quoted = p->is_variable = p->is_op = false;

	if (p->dq_consing && (*src == '"') && (src[1] == '"')) {
		src++;
	} else if (p->dq_consing && (*src == '"')) {
		*dst++ = ']';
		*dst = '\0';
		p->srcptr = (char*)++src;
		p->dq_consing = 0;
		p->toklen = dst - p->token;
		return true;
	}

	if (p->dq_consing < 0) {
		*dst++ = ',';
		*dst = '\0';
		p->dq_consing = 1;
		p->toklen = dst - p->token;
		return true;
	}

	if (p->dq_consing) {
		int ch = get_char_utf8(&src);

		if ((ch == '\\') && p->flag.character_escapes) {
			ch = get_escape(&src, &p->error, false);

			if (p->error) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d '%s'\n", p->srcptr, p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "illegal_character_escape";
				p->error = true;
				return false;
			}
		}

		dst += snprintf(dst, 8, "%u", ch);
		*dst = '\0';
		p->srcptr = (char*)src;
		set_smallint(&p->v, ch);
		p->v.tag = TAG_INT;
		p->dq_consing = -1;
		p->toklen = dst - p->token;
		return true;
	}

	src = eat_space(p);

#if 0
	if (!src && !p->end_of_term) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

		p->error_desc = "cincomplete_statement";
		p->error = true;
		return false;
	}
#endif

	if (!src)
		return false;

	if (!*src) {
		p->srcptr = (char*)src;
		return false;
	}

	// -ve numbers (note there are no explicitly +ve numbers)

	bool is_neg = false;
	const char *save_src = src;

	if ((*src == '-') && last_op) {
		is_neg = true;
		src += 1;
	}

	if ((*src == '\'') && (src[1] == '-') && (src[2] == '\'') && last_op) {
		is_neg = true;
		src += 3;
	}

	if (is_neg) {
		p->srcptr = (char*)src;
		src = eat_space(p);

		if (!src) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, incomplete statement, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "cincomplete_statement";
			p->error = true;
			return false;
		}

		if (isdigit(*src))
			neg = true;
		else
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
						fprintf(stdout, "Error: syntax error, float, line %u, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

					p->error_desc = "float";
					p->error = true;
					return false;
				}
			}
		}

		p->srcptr = (char*)src;
		p->toklen = dst - p->token;
		return eat_comment(p);
	}

	// Quoted...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quote_char = *src++;
		p->is_quoted = true;

		if ((p->quote_char == '"') && p->flag.double_quote_codes) {
			*dst++ = '[';

			if ((*src == '"') && (src[1] != '"')) {
				*dst++ = ']';
				*dst = '\0';
				p->srcptr = (char*)++src;
				p->toklen = dst - p->token;
				return true;
			}

			*dst = '\0';
			p->dq_consing = 1;
			p->quote_char = 0;
			p->srcptr = (char*)src;
			p->toklen = dst - p->token;
			return true;
		} else if ((p->quote_char == '"') && p->flag.double_quote_chars)
			p->string = true;

		if (p->string && (*src == p->quote_char) && (*src == '"')) {
			dst += put_char_bare_utf8(dst, '[');
			dst += put_char_bare_utf8(dst, ']');
			p->string = false;
			src++;
			p->srcptr = (char*)src;
			p->toklen = dst - p->token;
			return true;
		}

		for (;;) {
			for (int ch; (ch = get_char_utf8(&src));) {
				if (ch == '\n') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, line %d, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == p->quote_char) && (*src == ch)) {
					ch = *src++;
				} else if (ch == p->quote_char) {
					p->quote_char = 0;
					break;
				}

				if (ch < ' ') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, invalid quoted character, line %d\n", p->line_nbr);

					p->error_desc = "invalid_quoted_character";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == '\\') && p->flag.character_escapes) {
					int ch2 = *src;
					ch = get_escape(&src, &p->error, false);

					if (!p->error) {
						if (ch2 == '\n') {
							//p->line_nbr++;
							continue;
						}
					} else {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d, '%s'\n", p->srcptr, p->line_nbr, p->save_line?p->save_line:"");

						p->error_desc = "illegal_character_escape";
						p->error = true;
						p->srcptr = (char*)src;
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
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, line %d, '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			if (!p->string
				&&strcmp(p->token, "[")
				&& strcmp(p->token, "(")
				&& strcmp(p->token, "{")
				&& strcmp(p->token, "]")
				&& strcmp(p->token, ")")
				&& strcmp(p->token, "}"))
			{
				if (search_op(p->m, p->token, NULL, false)) {
					p->is_op = true;

					if (!strcmp(p->token, ","))
						p->quote_char = -1;
				} else
					p->quote_char = -1;
			} else
				p->quote_char = -1;

			p->srcptr = (char*)src;
			p->toklen = dst - p->token;
			return true;
		}
	}

	// Atoms...

	int ch = peek_char_utf8(src);

	if (iswalpha(ch) || (ch == '_')) {
		while (iswalnum(ch) || (ch == '_')) {
			get_char_utf8(&src);
			size_t len = (dst + put_len_utf8(ch) + 1) - p->token;

			if (len >= p->token_size) {
				size_t offset = dst - p->token;
				p->token = realloc(p->token, p->token_size*=2);
				ensure(p->token);
				dst = p->token + offset;
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

		if (iswspace(ch) && strcmp(p->token, ".")) {
			p->srcptr = (char*)src;
			src = eat_space(p);

			if (!src) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete statement, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "cincomplete_statement";
				p->error = true;
				return false;
			}

			if (!p->is_op && (*src == '(')) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator expected, line %d: %s, '%s'\n", p->line_nbr, p->token, p->save_line?p->save_line:"");

				p->error_desc = "operator_expected";
				p->error = true;
				p->srcptr = (char*)src;
				return false;
			}
		}

		p->srcptr = (char*)src;
		p->toklen = dst - p->token;
		return true;
	}

	// Symbols...

	if (is_matching_pair(p, &dst, (char**)&src, ')','(') ||
		is_matching_pair(p, &dst, (char**)&src, ']','(') ||
		is_matching_pair(p, &dst, (char**)&src, '}','(') ||
		is_matching_pair(p, &dst, (char**)&src, '}','(') ||
		is_matching_pair(p, &dst, (char**)&src, ',',')') ||
		is_matching_pair(p, &dst, (char**)&src, ',',']') ||
		is_matching_pair(p, &dst, (char**)&src, ',','}') ||
		is_matching_pair(p, &dst, (char**)&src, ',','|') ||
		is_matching_pair(p, &dst, (char**)&src, ',',',')) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, operator expected, line %d: %s, '%s'\n", p->line_nbr, p->token, p->save_line?p->save_line:"");

		p->error_desc = "operator_expected";
		p->error = true;
		p->srcptr = (char*)src;
		return false;
	}

	if (is_matching_pair(p, &dst, (char**)&src, '[',']') ||
		is_matching_pair(p, &dst, (char**)&src, '{','}') ||
		is_matching_pair(p, &dst, (char**)&src, ',',',') ||
		(!p->args && is_matching_pair(p, &dst, (char**)&src, ',',';')) ||
		(!p->args && is_matching_pair(p, &dst, (char**)&src, ';',',')) ||
		is_matching_pair(p, &dst, (char**)&src, ';',';')) {
		p->toklen = dst - p->token;
		return (dst - p->token) != 0;
	}

	if ((src[0] == '=') && (src[1] == '.') && (src[2] == '.')) {
		dst += sprintf(dst, "=..");
		p->is_op = true;
		p->srcptr = (char*)src+3;
		p->toklen = dst - p->token;
		return (dst - p->token) != 0;
	}

	while (*src) {
		ch = get_char_utf8(&src);
		size_t len = (dst + put_len_utf8(ch) + 1) - p->token;

		if (len >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			ensure(p->token);
			dst = p->token + offset;
		}

		dst += put_char_utf8(dst, ch);
		*dst = '\0';

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch))
			break;

		int ch_next = peek_char_utf8(src);

		if (ch_next == '%')
			break;

		if ((ch == '.') && iswspace(ch_next))
			break;

		ch = ch_next;

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch) || iswalnum(ch) || (ch == '_'))
			break;
	}

	p->is_op = search_op(p->m, p->token, NULL, false);
	p->srcptr = (char*)src;
	p->toklen = dst - p->token;
	return true;
}

static bool process_term(parser *p, cell *p1)
{
	directives(p, p1);

	cell *h = get_head(p1);

	if (is_cstring(h)) {
		pl_idx_t off = index_from_pool(p->m->pl, GET_STR(p, h));
		if (off == ERR_IDX) {
			p->error = true;
			return false;
		}

		unshare_cell(h);
		h->tag = TAG_LITERAL;
		h->val_off = off;
		h->flags = 0;
		h->arity = 0;
	}

	if (!p->error && !assertz_to_db(p->m, p->cl->nbr_vars, p1, 1)) {
#if 0
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: '%s', line %u\n", p->token, p->line_nbr);
#endif
		p->error = true;
		return false;
	}

	return true;
}

unsigned tokenize(parser *p, bool args, bool consing)
{
	pl_idx_t begin_idx = p->cl->cidx, arg_idx = p->cl->cidx, save_idx = 0;
	bool last_op = true, is_func = false, was_consing = false;
	bool last_bar = false, last_quoted = false, last_postfix = false;
	unsigned arity = 1;
	p->depth++;

	while (p->args = args, get_token(p, last_op)) {
		if (p->error && !p->do_read_term)
			break;

#if 0
		int ch = peek_char_utf8(p->token);
		fprintf(stderr, "Debug: token '%s' (%d) line_nbr=%d, quoted=%d, tag=%u, op=%d, lastop=%d, string=%d '%s'\n", p->token, ch, p->line_nbr, p->quote_char, p->v.tag, p->is_op, last_op, p->string, p->srcptr);
#endif

		if (!p->quote_char && !strcmp(p->token, ".")
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '|')) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (DUMP_ERRS || !p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, line %u '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (analyze(p, 0)) {
				if (p->cl->cells->nbr_cells < (p->cl->cidx-1)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: syntax error, operator expected '%s', line %u, '%s'\n", p->token, p->line_nbr, p->save_line?p->save_line:"");

					p->error_desc = "operator_expected";
					p->error = true;
				}

				term_assign_vars(p, p->read_term, false);
				term_to_body(p);

				if (p->consulting && !p->skip) {
					xref_rule(p->m, p->cl, NULL);
					term_expansion(p);
					cell *p1 = p->cl->cells;

					if (!p1->arity &&
						(!strcmp(GET_STR(p, p1), "begin_of_file") ||
							!strcmp(GET_STR(p, p1), "end_of_file")))
						p->skip = true;
				}

				if (p->consulting && !p->skip) {
					// Term expansion can return a list...

					cell *p1 = p->cl->cells;
					LIST_HANDLER(p1);

					while (is_list(p1) && !g_tpl_interrupt) {
						cell *h = LIST_HEAD(p1);

						if (!process_term(p, h))
							return false;

						if (p->already_loaded)
							return false;

						p1 = LIST_TAIL(p1);
					}

					if (!is_nil(p1) && !process_term(p, p1))
						return false;

					if (p->already_loaded)
						return false;

					p->cl->cidx = 0;
				}
			}

			p->end_of_term = true;
			last_op = true;

			if (p->one_shot)
				break;

			continue;
		}

		p->end_of_term = false;

		if (!p->quote_char && !last_op &&
			(!strcmp(p->token, "[") || !strcmp(p->token, "{"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error needs operator '%s', line %d\n", p->token, p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->last_close && !strcmp(p->token, "(")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error needs operator '%s', line %d\n", p->token, p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, "[")) {
			save_idx = p->cl->cidx;
			cell *c = make_a_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			p->nesting_brackets++;
			tokenize(p, true, true);
			last_bar = false;

			if (p->error)
				break;

			make_a_literal(p, g_nil_s);
			c = p->cl->cells + save_idx;
			c->nbr_cells = p->cl->cidx - save_idx;
			fix_list(c);
			p->start_term = p->last_close = false;
			last_op = false;
			continue;
		}

		if (!p->quote_char && !strcmp(p->token, "{")) {
			save_idx = p->cl->cidx;
			cell *c = make_a_literal(p, g_braces_s);
			ensure(c);
			c->arity = 1;
			p->start_term = true;
			p->nesting_braces++;
			tokenize(p, false, false);
			last_bar = false;

			if (p->error)
				break;

			c = p->cl->cells+save_idx;
			c->nbr_cells = p->cl->cidx - save_idx;
			p->start_term = p->last_close = false;
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
				cell *c = p->cl->cells + save_idx;
				c->arity = tmp_arity;
				c->nbr_cells = p->cl->cidx - save_idx;
			}

			is_func = last_op = false;
			p->start_term = p->last_close = false;
			continue;
		}

		if (!p->quote_char && args && !consing && p->is_op /*&& last_op*/ && strcmp(p->token, ",")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, p->token, &specifier, last_op);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parens needed around operator '%s', line %d\n", p->token, p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && p->is_op && strcmp(p->token, ",") && strcmp(p->token, "|")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, p->token, &specifier, last_op);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parens needed around operator '%s', line %d\n", p->token, p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && !strcmp(p->token, ",")) {
			if ((*p->srcptr == ',') && !p->flag.double_quote_codes) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing element '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "missing_element";
				p->error = true;
				break;
			}

			if (was_consing) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "list";
				p->error = true;
				break;
			}

			cell *c = make_a_literal(p, g_dot_s);
			c->arity = 2;
			p->start_term = last_op = true;
			p->last_close = false;
			continue;
		}

		if (!p->quote_char && args && !strcmp(p->token, ",")) {
			analyze(p, arg_idx);
			arg_idx = p->cl->cidx;

			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			arity++;

			if (arity > MAX_ARITY) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: max arity reached, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

				p->error_desc = "max_arity";
				p->error = true;
				break;
			}

			p->last_close = false;
			last_op = true;
			continue;
		}

		if (!p->is_quoted && consing && p->start_term && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && was_consing && consing && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && consing && !strcmp(p->token, "|")) {
			last_op = last_bar = was_consing = true;
			p->last_close = false;
			//consing = false;
			continue;
		}

		if (!p->is_quoted && was_consing && last_bar && !strcmp(p->token, "]")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!strcmp(p->token, ",") || !strcmp(p->token, "]") || !strcmp(p->token, ")") || !strcmp(p->token, "}"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, start of rule expected, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "start_expected";
			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, ")")) {
			p->last_close = true;
			last_op = false;
			p->nesting_parens--;
			analyze(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "]")) {
			p->last_close = true;
			last_op = false;
			p->nesting_brackets--;
			analyze(p, begin_idx);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "}")) {
			p->last_close = true;
			last_op = false;
			p->nesting_braces--;
			analyze(p, begin_idx);
			return arity;
		}

		p->last_close = false;

		if (p->is_variable && (*p->srcptr == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, variable as functor, line %d '%s'\n", p->line_nbr, p->save_line?p->save_line:"");

			p->error_desc = "variable_cannot_be_functor";
			p->error = true;
			break;
		}

		unsigned specifier = 0;
		int priority = 0;

		if (is_literal(&p->v))
			priority = search_op(p->m, p->token, &specifier, last_op);

		if (!strcmp(p->token, "!") &&
			((*p->srcptr == ')') || (*p->srcptr == ';') || (*p->srcptr == ',') || (*p->srcptr == '.')))
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

		if (priority && (specifier == OP_YFX) && last_op && !last_quoted) {
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

		int func = is_literal(&p->v) && !specifier && (*p->srcptr == '(');

		if (func) {
			is_func = true;
			p->is_op = false;
			specifier = 0;
			save_idx = p->cl->cidx;
		}

		if (!p->is_op && !is_func && last_op && last_postfix) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near '%s', operator expected '%s'\n", p->token, p->save_line?p->save_line:"");

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		//printf("*** op=%s, prefix=%d\n", p->token, IS_PREFIX(specifier));

		if ((!p->is_op || IS_PREFIX(specifier)) && !is_func && !last_op) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near '%s', operator expected '%s'\n", p->token, p->save_line?p->save_line:"");

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		last_quoted = p->is_quoted;
		last_op = strcmp(p->token, ")") && priority;
		last_postfix = last_op && IS_POSTFIX(specifier);

		p->start_term = false;
		cell *c = make_a_cell(p);
		c->nbr_cells = 1;
		c->tag = p->v.tag;
		c->flags = p->v.flags;
		SET_OP(c,specifier);
		c->priority = priority;
		bool found = false;

		if (is_bigint(&p->v)) {
			c->val_bigint = p->v.val_bigint;
		} else if (is_smallint(&p->v)) {
			set_smallint(c, get_int(&p->v));
		} else if (p->v.tag == TAG_REAL) {
			set_real(c, get_real(&p->v));
		} else if ((!p->is_quoted || func || p->is_op || p->is_variable ||
			(get_builtin(p->m->pl, p->token, 0, &found, NULL), found) ||
			!strcmp(p->token, "[]")) && !p->string) {

			if (func && !strcmp(p->token, "."))
				c->priority = 0;

			if (p->is_variable)
				c->tag = TAG_VAR;

			if (p->is_quoted)
				c->flags |= FLAG2_QUOTED;

			c->val_off = index_from_pool(p->m->pl, p->token);
			ensure(c->val_off != ERR_IDX);
		} else {
			c->tag = TAG_CSTR;

			if ((p->toklen < MAX_SMALL_STRING) && !p->string) {
				memcpy(c->val_chr, p->token, p->toklen);
				c->val_chr[p->toklen] = '\0';
				c->chr_len = p->toklen;
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

bool run(parser *p, const char *pSrc, bool dump)
{
	if (*pSrc == '.') {
		fprintf(stdout, "Error: syntax error, unexpected end of rule\n");
		return false;
	}

	ASTRING(src);
	ASTRING_sprintf(src, "'$choice',(%s", pSrc);
	ASTRING_trim_ws(src);
	ASTRING_trim(src, '.');
	ASTRING_strcat(src, ").");

	p->srcptr = ASTRING_cstr(src);
	p->line_nbr = 1;
	tokenize(p, false, false);

	ASTRING_free(src);

	if (!p->error && !p->end_of_term && !p->run_init) {
		fprintf(stdout, "Error: syntax error, missing operand or operator\n");
		p->error = true;
	}

	if (p->error) {
		p->pl->did_dump_vars = true;
		return false;
	}

	if (p->skip) {
		p->m->pl->status = true;
		return true;
	}

	if (!analyze(p, 0))
		return false;

	term_assign_vars(p, 0, false);

	if (!p->command)
		term_expansion(p);

	xref_rule(p->m, p->cl, NULL);

	query *q = create_query(p->m, false);
	if (!q) return false;
	q->p = p;
	q->do_dump_vars = dump;
	q->run_init = p->run_init;
	execute(q, p->cl->cells, p->cl->nbr_vars);

	p->m->pl->halt = q->halt;
	p->m->pl->halt_code = q->halt_code;
	p->m->pl->status = q->status;
	p->m->pl->is_redo = q->is_redo;

	if (!p->m->pl->quiet && !p->directive && dump && q->pl->stats) {
		fprintf(stdout,
			"Goals %llu. Matches %llu. Max frames %u, choices %u, trails: %u, slots %u, heap %u. Backtracks %llu. TCOs:%llu\n",
			(unsigned long long)q->tot_goals, (unsigned long long)q->tot_matches,
			q->max_frames, q->max_choices, q->max_trails, q->max_slots, q->arenas->max_hp_used,
			(unsigned long long)q->tot_retries, (unsigned long long)q->tot_tcos);
	}

	const bool ok = !q->error;
	p->m = q->st.m;
	destroy_query(q);
	return ok;
}
