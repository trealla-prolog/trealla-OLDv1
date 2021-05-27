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
extern predicate *create_predicate(module *m, cell *c);
extern unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
extern module *module_load_text(module *m, const char *src, const char *filename);
extern bool needs_quoting(module *m, const char *src, int srclen);
extern void do_db_load(module *m);
extern void set_noindex_in_db(module *m, const char *name, unsigned arity);
extern void set_discontiguous_in_db(module *m, const char *name, unsigned arity);
extern void set_dynamic_in_db(module *m, const char *name, unsigned arity);
extern void set_meta_predicate_in_db(module *m, cell *c);
extern void set_persist_in_db(module *m, const char *name, unsigned arity);
extern void set_multifile_in_db(module *m, const char *name, idx_t arity);
extern bool module_load_fp(module *m, FILE *fp, const char *filename);
extern void module_purge_dirty_list(module *m);
