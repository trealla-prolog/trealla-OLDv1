#pragma once
#include <stdbool.h>
#include <stdio.h>

typedef struct prolog_ prolog;

typedef enum {
/*
  sketch: plan for final enums:
  - leave the compiler the freedom to put this in a signed char
  - compatible with 'bool' for non special values: pl_success == 1 and pl_failure == 0
  - make it self enumerating
    - all 'special' returns are <0
    - all errors (may need to add more in future) are <= pl_error
*/
//PLANNED:	pl_last__  = -100;  //unused for now, just for starting the enumeration
//PLANNED:	pl_...,             //insert more error codes here on demand
//PLANNED:	pl_cycle,           //cyclic term
//PLANNED:	pl_error,           //generic resource error
//PLANNED:	pl_halt    = -3,
//PLANNED:	pl_abort,
//PLANNED:	pl_yield,
	pl_halt    =  0,
	pl_abort   =  0,
	pl_yield   =  0,
	pl_cycle   =  0,
	pl_error   =  0,
	pl_failure =  0,
	pl_success =  1,
} pl_state;

prolog *pl_create();
void pl_destroy(prolog*);

bool pl_eval(prolog*, const char *expr);
bool pl_consult(prolog*, const char *filename);
bool pl_consult_fp(prolog*, FILE *fp, const char *filename);

int get_halt_code(prolog*);
bool get_halt(prolog*);
bool get_status(prolog*);
bool get_dump_vars(prolog*);

void set_trace(prolog*);
void set_quiet(prolog*);
void set_stats(prolog*);
void set_noindex(prolog*);
void set_opt(prolog*, int onoff);

extern int g_tpl_interrupt, g_ac, g_avc;
extern char **g_av, *g_argv0;
extern char *g_tpl_lib;
