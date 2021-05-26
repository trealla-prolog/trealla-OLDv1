#pragma once

extern parser *create_parser(module *m);
extern void destroy_parser(parser *p);

extern unsigned parser_tokenize(parser *p, bool args, bool consing);
extern void term_xref(parser *p, term *t, predicate *parent);
extern void parser_reset(parser *p);
extern void consultall(parser *p, cell *l);
extern void term_to_body(parser *p);
extern cell *check_body_callable(parser *p, cell *c);

// Should this be in module.h/module.c

extern module *create_module(prolog *pl, const char *name);
extern void destroy_module(module *m);

extern bool module_load_file(module *m, const char *filename);
extern bool module_save_file(module *m, const char *filename);
extern clause *asserta_to_db(module *m, term *t, bool consulting);
extern clause *assertz_to_db(module *m, term *t, bool consulting);
extern bool retract_from_db(module *m, clause *r);
extern clause *erase_from_db(module *m, uuid *ref);
extern clause *find_in_db(module *m, uuid *ref);
extern unsigned get_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
extern unsigned find_op(module *m, const char *name, unsigned specifier);
extern bool set_op(module *m, const char *name, unsigned specifier, unsigned priority);
extern predicate *find_functor(module *m, const char *name, unsigned arity);
extern predicate *find_predicate(module *m, cell *c);
extern predicate *search_predicate(module *m, cell *c);
extern unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
extern module *module_load_text(module *m, const char *src, const char *filename);
extern void do_db_load(module *m);
extern bool needs_quoting(module *m, const char *src, int srclen);

// Should this be in prolog.h/prolog.c

extern void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found);
extern bool deconsult(prolog *pl, const char *filename);
extern module *find_module(prolog *pl, const char *name);
extern module *find_module_id(prolog *pl, idx_t id);
extern module *find_next_module(prolog *pl, module *m);
extern idx_t index_from_pool(prolog *pl, const char *name);
extern void load_builtins(prolog *pl);

extern char *format_property(char **bufptr, size_t *lenptr, char *dst, const char *name, unsigned arity, const char *type);
extern void clear_term(term *t);
extern size_t sprint_int(char *dst, size_t size, int_t n, int base);
extern void fix_list(cell *c);
