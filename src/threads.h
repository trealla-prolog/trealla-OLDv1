#pragma once

typedef struct lock_ lock;
typedef struct tpool_ tpool;
typedef int thread_function(void *);

extern lock *lock_create(void);
extern void lock_lock(lock *l);
extern void lock_unlock(lock *l);
extern void lock_destroy(lock *l);

// Run a supplied function as a one-off detached thread. The
// thread will be created then destroyed after this single use.

extern int thread_run(thread_function *f, void *data);

// Run a supplied function from a pool of threads. The thread
// will be returned to the pool for fast re-use when needed.

extern tpool *tpool_create(int threads);
extern void tpool_schedule(tpool *tp, thread_function *f, void *data);
extern void tpool_queue(tpool *tp, thread_function *f, void *data);
extern void tpool_destroy(tpool *tp);

