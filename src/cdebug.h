#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#define ensure(cond, ...) if (!(cond)) abort()

#define USE_RESULT __attribute__ ((__warn_unused_result__))
#define DISCARD_RESULT (void)!

// Wrap an assignment that's expected to return anything but the given sentinel value.
// when the sentinel otherwise does some (optional) error handling action
// default action is 'error=true' to indicate an error happened

#define CHECK_SENTINEL(expr, err_sentinel, ...) CHECK_SENTINEL_((expr), err_sentinel, ## __VA_ARGS__, error=true)
#define CHECK_SENTINEL_(expr, err_sentinel, on_error, ...) do { if((expr) == err_sentinel){on_error;}} while (0)

#define may_error(expr, ...) CHECK_SENTINEL(expr, pl_error, __VA_ARGS__; return pl_error)
#define may_idx_error(expr, ...) CHECK_SENTINEL(expr, ERR_IDX, __VA_ARGS__; return pl_error)
#define may_ptr_error(expr, ...) CHECK_SENTINEL(expr, NULL, __VA_ARGS__; return pl_error)
