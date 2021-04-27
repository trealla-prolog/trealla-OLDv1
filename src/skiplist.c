#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "skiplist.h"


typedef struct keyval_ keyval_t;
typedef struct slnode_ slnode_t;

struct keyval_ {
	void *key, *val;
};

#define BUCKET_SIZE 32

struct slnode_ {
	keyval_t bkt[BUCKET_SIZE];
	int nbr;
	slnode_t *forward[];
};

struct sliter_ {
	sliter *next;
	skiplist *l;
	slnode_t *p;
	const void *key;
	int idx;
	bool dead;
};

struct skiplist_ {
	slnode_t *header;
	int (*compkey)(const void*, const void*, const void *p);
	void (*delkey)(void*);
	const void *p;
	sliter *iters;
	size_t count;
	int level;
	unsigned seed;
};

#define MAX_LEVELS 32
#define MAX_LEVEL (MAX_LEVELS - 1)

inline static slnode_t *new_node_of_level(unsigned x)
{
	return malloc(sizeof(slnode_t) + ((x+1) * sizeof(slnode_t*)));
}


skiplist *sl_create2(int (*compkey)(const void*, const void*, const void *p), void(*delkey)(void*))
{
	skiplist *l = (skiplist*)calloc(1, sizeof(struct skiplist_));
	if (!l) return NULL;

	l->header = new_node_of_level(MAX_LEVELS);
	if (!l->header) {
		free(l);
		return NULL;
	}

#ifdef NDEBUG
	l->seed = (unsigned)(size_t)(l + clock());
#else
	static unsigned seed = 0xdeadbeef;
	l->seed = ++seed;
#endif

	l->level = 1;

	for (int i = 0; i < MAX_LEVELS; i++)
		l->header->forward[i] = NULL;

	l->header->nbr = 1;
	l->header->bkt[0].key = NULL;
	l->compkey = compkey;
	l->delkey = delkey;
	return l;
}

skiplist *sl_create1(int (*compkey)(const void*, const void*, const void *p), const void *p)
{
	skiplist *sl = sl_create2(compkey, NULL);

	if (sl)
		sl->p = p;

	return sl;
}

skiplist *sl_create(int (*compkey)(const void*, const void*, const void *p))
{
	return sl_create2(compkey, NULL);
}

void sl_destroy(skiplist *l)
{
	if (!l)
		return;

	slnode_t *p, *q;
	p = l->header;
	q = p->forward[0];
	free(p);
	p = q;

	while (p) {
		q = p->forward[0];

		if (l->delkey) {
			for (int j = 0; j < p->nbr; j++)
				l->delkey(p->bkt[j].key);
		}

		free(p);
		p = q;
	}

	while (l->iters) {
		sliter *iter = l->iters;
		l->iters = iter->next;
		free(iter);
	}

	free(l);
}

size_t sl_count(const skiplist *l) { return l->count; }

static int binary_search(const skiplist *l, const keyval_t n[], const void *key, int imin, int imax)
{
	while (imax >= imin) {
		int imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key, l->p) == 0)
			return imid;
		else if (l->compkey(n[imid].key, key, l->p) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	return -1;
}

// Modified binary search: return position where it is or ought to be

static int binary_search1(const skiplist *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin) {
		imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key, l->p) < 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	if (l->compkey(n[imid].key, key, l->p) < 0)
		imid++;

	return imid;
}

// Modified binary search: return position where it is or ought to be

static int binary_search2(const skiplist *l, const keyval_t n[], const void *key, int imin, int imax)
{
	int imid = 0;

	while (imax >= imin) {
		imid = (imax + imin) / 2;

		if (l->compkey(n[imid].key, key, l->p) <= 0)
			imin = imid + 1;
		else
			imax = imid - 1;
	}

	if (l->compkey(n[imid].key, key, l->p) <= 0)
		imid++;

	return imid;
}

#define frand(seedp) ((double)rand_r(seedp) / RAND_MAX)

static int random_level(unsigned *seedp)
{
	const double P = 0.5;
	int lvl = (int)(log(frand(seedp)) / log(1. - P));
	return lvl < MAX_LEVEL ? lvl : MAX_LEVEL;
}

bool sl_set(skiplist *l, const void *key, const void *val)
{
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	slnode_t stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[0].key, key, l->p) < 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, 0, p->nbr - 1);

		if (p->nbr < BUCKET_SIZE) {
			int j;

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			p->bkt[j].key = (void*)key;
			p->bkt[j].val = (void*)val;
			p->nbr++;
			l->count++;
			return true;
		}

		// Don't drop this unless you are 100% sure:

#if 1
		while ((imid < p->nbr) && (l->compkey(p->bkt[imid].key, key, l->p) == 0))
			imid++;

		if (imid <= BUCKET_SIZE) {
			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
		}
#endif
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);
	if (!q) return false;

	q->bkt[0].key = (void*)key;
	q->bkt[0].val = (void*)val;
	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	return true;
}

bool sl_app(skiplist *l, const void *key, const void *val)
{
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	slnode_t stash;
	stash.nbr = 0;
	int k;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[0].key, key, l->p) <= 0))
			p = q;

		update[k] = p;
	}

	if (p != l->header) {
		int imid = binary_search2(l, p->bkt, key, 0, p->nbr - 1);

		if (p->nbr < BUCKET_SIZE) {
			int j;

			for (j = p->nbr; j > imid; j--)
				p->bkt[j] = p->bkt[j - 1];

			p->bkt[j].key = (void*)key;
			p->bkt[j].val = (void*)val;
			p->nbr++;
			l->count++;
			return true;
		}

		// Don't drop this unless you are 100% sure:

#if 1
		while ((imid < p->nbr) && (l->compkey(p->bkt[imid].key, key, l->p) == 0))
			imid++;

		if (imid <= BUCKET_SIZE) {
			for (int j = imid; j < p->nbr; j++)
				stash.bkt[stash.nbr++] = p->bkt[j];

			p->nbr = imid;
		}
#endif
	}

	k = random_level(&l->seed);

	if (k >= l->level) {
		l->level++;
		k = l->level - 1;
		update[k] = l->header;
	}

	q = new_node_of_level(k + 1);
	if (!q) return false;

	q->bkt[0].key = (void*)key;
	q->bkt[0].val = (void*)val;
	q->nbr = 1;
	l->count++;

	if (stash.nbr) {
		for (int i = 0; i < stash.nbr; i++, q->nbr++)
			q->bkt[q->nbr] = stash.bkt[i];
	}

	for (; k >= 0; k--) {
		p = update[k];
		q->forward[k] = p->forward[k];
		p->forward[k] = q;
	}

	return true;
}

bool sl_get(const skiplist *l, const void *key, const void **val)
{
	int k;
	slnode_t *p, *q = 0;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key, l->p) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return false;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return false;

	*val = q->bkt[imid].val;
	return true;
}

bool sl_del(skiplist *l, const void *key)
{
	int k, m;
	slnode_t *update[MAX_LEVELS];
	slnode_t *p, *q;
	p = l->header;

	for (k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key, l->p) < 0))
			p = q;

		update[k] = p;
	}

	if (!(q = p->forward[0]))
		return false;

	int imid = binary_search(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return false;

	while (imid < (q->nbr - 1)) {
		q->bkt[imid] = q->bkt[imid + 1];
		imid++;
	}

	q->nbr--;
	l->count--;

	if (q->nbr)
		return true;

	m = l->level - 1;

	for (k = 0; k <= m; k++) {
		p = update[k];

		if (!p || (p->forward[k] != q))
			break;

		p->forward[k] = q->forward[k];
	}

	free(q);
	m = l->level - 1;

	while (!l->header->forward[m] && (m > 0))
		m--;

	l->level = m + 1;
	return true;
}

void sl_iterate(const skiplist *l, int (*f)(const void*, const void*, const void*), void *p1)
{
	slnode_t *p;
	p = l->header;
	p = p->forward[0];

	while (p) {
		slnode_t *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p->bkt[j].key, p->bkt[j].val, p1))
				return;
		}

		p = q;
	}
}

void sl_find(const skiplist *l, const void *key, int (*f)(const void*, const void*, const void*), void *p1)
{
	slnode_t *p, *q = 0;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key, l->p) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return;

	int imid = binary_search2(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return;

	p = q;

	for (int j = imid; j < p->nbr; j++) {
		if (!f(p->bkt[j].key, p->bkt[j].val, p1))
			return;
	}

	while (p) {
		slnode_t *q = p->forward[0];

		for (int j = 0; j < p->nbr; j++) {
			if (!f(p->bkt[j].key, p->bkt[j].val, p1))
				return;
		}

		p = q;
	}
}

sliter *sl_first(skiplist *l)
{
	sliter *iter;

	if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		iter = l->iters;
		l->iters = iter->next;
	}

	iter->key = NULL;
	iter->l = l;
	iter->p = l->header->forward[0];
	iter->idx = 0;
	iter->dead = false;
	return iter;
}

bool sl_next(sliter *iter, void **val)
{
	if (!iter || !iter->p)
		return false;

	for (;;) {
		if (iter->idx < iter->p->nbr) {
			*val = iter->p->bkt[iter->idx++].val;
			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;

		if (!iter->p)
			break;
	}

	sl_done(iter);
	return false;
}

sliter *sl_findkey(skiplist *l, const void *key)
{
	slnode_t *p, *q = 0;
	p = l->header;

	for (int k = l->level - 1; k >= 0; k--) {
		while ((q = p->forward[k]) && (l->compkey(q->bkt[q->nbr - 1].key, key, l->p) < 0))
			p = q;
	}

	if (!(q = p->forward[0]))
		return NULL;

	int imid = binary_search1(l, q->bkt, key, 0, q->nbr - 1);

	if (imid < 0)
		return NULL;

	if (l->compkey(q->bkt[imid].key, key, l->p) != 0)
		return NULL;

	sliter *iter;

	if (!l->iters) {
		iter = malloc(sizeof(sliter));
		if (!iter) return NULL;
	} else {
		iter = l->iters;
		l->iters = iter->next;
	}

	iter->key = key;
	iter->l = l;
	iter->p = q;
	iter->idx = imid;
	iter->dead = false;
	return iter;
}

bool sl_nextkey(sliter *iter, void **val)
{
	if (!iter || !iter->p)
		return false;

	for (;;) {
		if (iter->idx < iter->p->nbr) {
			if (iter->l->compkey(iter->p->bkt[iter->idx].key, iter->key, iter->l->p) != 0) {
				sl_done(iter);
				return false;
			}

			*val = iter->p->bkt[iter->idx++].val;
			return true;
		}

		iter->p = iter->p->forward[0];
		iter->idx = 0;

		if (!iter->p)
			break;
	}

	sl_done(iter);
	return false;
}

void sl_done(sliter *iter)
{
	if (!iter)
		return;

	iter->next = iter->l->iters;
	iter->l->iters = iter;
}

void sl_dump(const skiplist *l, const char *(*f)(const void*, const void*), void *p1)
{
	if (!l)
		return;

    slnode_t *p, *q;
    p = l->header;
    p = p->forward[0];

    while (p) {
		q = p->forward[0];
		printf("%6d: ", p->nbr);

		for (int j = 0; j < p->nbr; j++)
			printf("%s ", f(p->bkt[j].key, p1));

		printf("\n");
		p = q;
    }

    printf("\n");
}
