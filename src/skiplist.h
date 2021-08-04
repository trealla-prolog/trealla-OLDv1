#pragma once

#include <string.h>
#include <stdbool.h>

typedef struct skiplist_ skiplist;
typedef struct sliter_ sliter;

extern skiplist *sl_create(
	int (*cmpkey)(const void *k1, const void *k2, const void* p, int args),
	void (*delkey)(void *k, void *v, const void* p),
	const void *p
	);

extern void sl_allow_dups(skiplist *l, bool mode);
extern void sl_nbr_args(skiplist *l, int args);
extern bool sl_set(skiplist *l, const void *k, const void *v);
extern bool sl_app(skiplist *l, const void *k, const void *v);
extern bool sl_get(const skiplist *l, const void *k, const void **v);
extern bool sl_del(skiplist *l, const void *k);

extern void sl_iterate(
	const skiplist *l,
	int (*callback)(const void *k, const void *v, const void *p),
	const void *p
	);

extern void sl_find(
	const skiplist *l,
	const void *k,
	int (*f)(const void *k, const void *v, const void *p),
	const void *p
	);

extern sliter *sl_findkey(skiplist *l, const void *k);
extern bool sl_is_nextkey(sliter *i);
extern bool sl_nextkey(sliter *i, void **v);

extern sliter *sl_first(skiplist *l);
extern bool sl_next(sliter *i, void **v);

extern void sl_done(sliter *i);
extern size_t sl_count(const skiplist *l);

extern void sl_dump(
	const skiplist *l,
	const char *(*f)(const void* k, const void* v, const void *p),
	const void *p
	);

extern void sl_destroy(skiplist *l);
