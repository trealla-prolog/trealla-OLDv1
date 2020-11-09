#ifndef CDEBUG_H
#define CDEBUG_H

#include <stdlib.h>
#include <stdint.h>


#ifdef NDEBUG
#define message(fmt, ...)
#define message_when(cond, ...)
#else
#define message(fmt, ...) fprintf(stderr, "%s:%d %s: " fmt "\n", __FILE__, __LINE__, __func__, ## __VA_ARGS__)
#define message_when(cond, ...) do { if (cond) {message(""__VA_ARGS__);}} while (0)
#endif
#define ensure(cond, ...) do { if (!(cond)) {message( #cond " failed " __VA_ARGS__); abort();}} while (0)

#if !defined(NDEBUG) && defined(FAULTINJECT_VAR)
extern uint64_t FAULTINJECT_VAR;
#define FAULTINJECT_ENABLED
#define FAULTINJECT(...) do {if (!--FAULTINJECT_VAR){ __VA_ARGS__; }} while (0)
#define FAULTINJECT_WHEN(cond, ...) do {if ((cond) &&!--FAULTINJECT_VAR){ __VA_ARGS__; }} while (0)
#define FAULTINJECT_ONCE(cond,...) do {static bool tried = false; if (!tried && (cond)) { tried = true; FAULTINJECT(__VA_ARGS__);}} while (0)
#else
#define FAULTINJECT(...)
#define FAULTINJECT_WHEN(...)
#define FAULTINJECT_ONCE(...)
#endif


#endif
