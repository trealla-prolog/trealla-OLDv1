#ifndef CDEBUG_H
#define CDEBUG_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#define ensure(cond, ...) if (!(cond)) abort()

#define USE_RESULT __attribute__ ((__warn_unused_result__))
#define DISCARD_RESULT (void)!

#endif
