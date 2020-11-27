#ifndef CDEBUG_H
#define CDEBUG_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#ifdef NDEBUG
#define message(fmt, ...)
#define message_when(cond, ...)
#else
#define message(fmt, ...) fprintf(stderr, "%s:%d %s: " fmt "\n", __FILE__, __LINE__, __func__, ## __VA_ARGS__)
#define message_when(cond, ...) do { if (cond) {message(""__VA_ARGS__);}} while (0)
#endif
#define ensure(cond, ...) do { if (!(cond)) {message( #cond " failed " __VA_ARGS__); abort();}} while (0)

#if !defined(NDEBUG) && defined(FAULTINJECT_NAME)
typedef struct {
	uint64_t counter;
	bool abort;
} faultinject_t;
extern faultinject_t FAULTINJECT_NAME;
#define FAULTINJECT_ENABLED
#define FAULTINJECT(...) do {						\
		if (!--FAULTINJECT_NAME.counter){			\
			message("injecting fault: " #__VA_ARGS__);	\
			ensure(!FAULTINJECT_NAME.abort,			\
			       "aborting: " #__VA_ARGS__ );		\
			__VA_ARGS__;					\
		}} while (0)
#define FAULTINJECT_WHEN(cond, ...) do {			\
		if ((cond) &&!--FAULTINJECT_NAME.counter){	\
			ensure(!FAULTINJECT_NAME.abort,		\
			       "aborting: " #__VA_ARGS__ );	\
			__VA_ARGS__;				\
		}} while (0)
#define FAULTINJECT_ONCE(cond,...) do {			\
		static bool tried = false;		\
		if (!tried && (cond)) {			\
			tried = true;			\
			FAULTINJECT(__VA_ARGS__);	\
		}} while (0)
#else
#define FAULTINJECT(...)
#define FAULTINJECT_WHEN(...)
#define FAULTINJECT_ONCE(...)
#endif

#ifdef __GNUC__
// clang defines __GNUC__ as well
#define USE_RESULT __attribute__ ((__warn_unused_result__))
#define DISCARD_RESULT (void)!
#else
#define USE_RESULT
#define DISCARD_RESULT
#endif

#endif
