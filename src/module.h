#pragma once

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
extern bool needs_quoting(module *m, const char *src, int srclen);
extern void do_db_load(module *m);

