#pragma once

#include <string.h>
#include <stdbool.h>

typedef struct skiplist_ skiplist;
typedef struct sliter_ sliter;

skiplist *sl_create(int (*compkey)(const void*, const void*, const void *p));
skiplist *sl_create1(int (*compkey)(const void*, const void*, const void *p), const void *p);
skiplist *sl_create2(int (*compkey)(const void*, const void*, const void* p), void (*delkey)(void*));
bool sl_set(skiplist *l, const void *k, const void *v);
bool sl_app(skiplist *l, const void *k, const void *v);
bool sl_get(const skiplist *l, const void *k, const void **v);
bool sl_del(skiplist *l, const void *k);
void sl_iterate(const skiplist *l, int (*callback)(const void *k, const void *v, const void *p), void *p);
void sl_find(const skiplist *l, const void *k, int (*f)(const void *k, const void *v, const void *p), void *p);
sliter *sl_findkey(skiplist *l, const void *k);
bool sl_nextkey(sliter *i, void **v);
sliter *sl_first(skiplist *l);
bool sl_next(sliter *i, void **v);
void sl_done(sliter *i);
size_t sl_count(const skiplist *l);
void sl_dump(const skiplist *l, const char *(*f)(const void* k, const void *p), void *p);
void sl_destroy(skiplist *l);
