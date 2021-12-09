#pragma once

extern module *create_module(prolog *pl, const char *name);
extern void duplicate_module(prolog *pl, module *m, const char *name);
extern void destroy_module(module *m);

extern bool save_file(module *m, const char *filename);
extern module *load_file(module *m, const char *filename);
extern module *load_fp(module *m, FILE *fp, const char *filename);
extern module *load_text(module *m, const char *src, const char *filename);

extern void convert_to_literal(module *m, cell *c);
extern clause *find_in_db(module *m, uuid *ref);
extern unsigned find_op(module *m, const char *name, unsigned specifier);
extern unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
extern bool set_op(module *m, const char *name, unsigned specifier, unsigned priority);
extern predicate *find_functor(module *m, const char *name, unsigned arity);
extern predicate *find_predicate(module *m, cell *c);
extern predicate *search_predicate(module *m, cell *c);
extern predicate *create_predicate(module *m, cell *c);
extern int index_cmpkey(const void *ptr1, const void *ptr2, const void *param);
extern bool needs_quoting(module *m, const char *src, int srclen);
extern void do_db_load(module *m);
extern int index_cmpkey_(const void *ptr1, const void *ptr2, const void *param, int depth);
extern bool unload_file(module *m, const char *filename);

extern clause *asserta_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting);
extern clause *assertz_to_db(module *m, unsigned nbr_vars, cell *p1, bool consulting);
extern bool retract_from_db(module *m, clause *cl);
extern clause *erase_from_db(module *m, uuid *ref);

extern void set_discontiguous_in_db(module *m, const char *name, unsigned arity);
extern void set_dynamic_in_db(module *m, const char *name, unsigned arity);
extern void set_meta_predicate_in_db(module *m, cell *c);
extern void set_persist_in_db(module *m, const char *name, unsigned arity);
extern void set_multifile_in_db(module *m, const char *name, pl_idx_t arity);
