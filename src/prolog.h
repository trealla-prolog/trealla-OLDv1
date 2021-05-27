#pragma once

extern void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found);
extern bool deconsult(prolog *pl, const char *filename);
extern module *find_module(prolog *pl, const char *name);
extern module *find_module_id(prolog *pl, idx_t id);
extern module *find_next_module(prolog *pl, module *m);
extern idx_t index_from_pool(prolog *pl, const char *name);
extern void load_builtins(prolog *pl);

