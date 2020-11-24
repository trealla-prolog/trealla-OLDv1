#pragma once
#include <stdbool.h>
#include <stdio.h>

typedef struct prolog_ prolog;

typedef enum {
//PLANNED:	pl_halt    = -4,
//PLANNED:	pl_abort   = -3,
//PLANNED:	pl_yield   = -2,
//PLANNED:	pl_error   = -1,
	pl_halt    =  0,
	pl_abort   =  0,
	pl_yield   =  0,
	pl_error   =  0,
	pl_failure =  0,
	pl_success =  1,
} prolog_state;

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
