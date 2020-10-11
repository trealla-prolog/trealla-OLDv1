#pragma once

#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <sys/param.h>

#include "skiplist.h"
#include "utf8.h"

#ifndef USE_OPENSSL
#define USE_OPENSSL 0
#endif

#ifndef USE_LIBRESSL
#define USE_LIBRESSL 0
#endif

#ifndef USE_INT128
#define USE_INT128 0
#endif

#ifndef USE_INT32
#define USE_INT32 0
#endif

#if USE_INT128
typedef __int128_t int_t;
typedef __uint128_t uint_t;
#elif USE_INT32
typedef __int32_t int_t;
typedef __uint32_t uint_t;
#else
typedef __int64_t int_t;
typedef __uint64_t uint_t;
#endif

typedef uint32_t idx_t;

#define MAX_SMALL_STRING (MAX(sizeof(int_t),sizeof(void*))*2)
#define MAX_VAR_POOL_SIZE 1000
#define MAX_ARITY UCHAR_MAX
#define MAX_USER_OPS 100
#define MAX_QUEUES 16
#define MAX_STREAMS 64
#define MAX_DEPTH 1000

#define STREAM_BUFLEN 1024

#define GET_FRAME(i) (q->frames+(i))
#define GET_SLOT(g,i) ((i) < g->nbr_slots ? q->slots+g->env+(i) : q->slots+g->overflow+((i)-g->nbr_slots))

// Primary type...

#define is_literal(c) ((c)->val_type == TYPE_LITERAL)
#define is_cstring(c) ((c)->val_type == TYPE_CSTRING)
#define is_variable(c) ((c)->val_type == TYPE_VARIABLE)
#define is_empty(c) ((c)->val_type == TYPE_EMPTY)
#define is_end(c) ((c)->val_type == TYPE_END)
#define is_indirect(c) ((c)->val_type == TYPE_INDIRECT)
#define is_float(c) ((c)->val_type == TYPE_FLOAT)
#define is_rational(c) ((c)->val_type == TYPE_INTEGER)

// Derived type...

#define is_iso_atom(c) ((is_literal(c) || is_cstring(c)) && !(c)->arity)
#define is_iso_list(c) (is_literal(c) && ((c)->arity == 2) && ((c)->val_off == g_dot_s))

#define is_atom(c) ((is_literal(c) && !(c)->arity) || (is_cstring(c)))
#define is_string(c) (is_cstring(c) && (c)->flags&FLAG_STRING)
#define is_blob(c) (is_cstring(c) && ((c)->flags&FLAG_BLOB))
#define is_list(c) (is_iso_list(c) || is_string(c))
#define is_integer(c) (((c)->val_type == TYPE_INTEGER) && ((c)->val_den == 1))
#define is_const_cstring(c) (is_cstring(c) && ((c)->flags&FLAG_CONST_CSTRING))
#define is_dup_cstring(c) (is_cstring(c) && ((c)->flags&FLAG_DUP_CSTRING))
#define is_nil(c) (is_literal(c) && !(c)->arity && ((c)->val_off == g_nil_s))
#define is_quoted(c) ((c)->flags&FLAG_QUOTED)
#define is_anon(c) ((c)->flags&FLAG_ANON)

// These 2 assume literal or cstring types...

#define GET_STR(c) ((c)->val_type != TYPE_CSTRING ? (g_pool+(c)->val_off) : (c)->flags&FLAG_BLOB ? (c)->val_str : (c)->val_chr)
#define LEN_STR(c) ((c)->flags&FLAG_BLOB ? (c)->len_str : strlen(GET_STR(c)))

enum {
	TYPE_EMPTY=0,
	TYPE_VARIABLE,
	TYPE_LITERAL,
	TYPE_CSTRING,
	TYPE_INTEGER,
	TYPE_FLOAT,
	TYPE_INDIRECT,
	TYPE_END
};

enum {
	FLAG_BUILTIN=1<<0,
	FLAG_HEX=1<<1,						// used with TYPE_INTEGER
	FLAG_OCTAL=1<<2,					// used with TYPE_INTEGER
	FLAG_BINARY=1<<3,					// used with TYPE_INTEGER
	FLAG_TAIL_REC=1<<4,
	FLAG_PASS_THRU=1<<5,
	FLAG_BLOB=1<<6,
	FLAG_STRING=1<<7,

	//FLAG_SPARE1=1<<8,

	FLAG_DELETED=FLAG_HEX,				// used by bagof
	FLAG_FIRST_USE=FLAG_HEX,			// used with TYPE_VARIABLE
	FLAG_ANON=FLAG_OCTAL,				// used with TYPE_VARIABLE
	FLAG_STREAM=FLAG_TAIL_REC,			// used with TYPE_INTEGER
	FLAG_CONST_CSTRING=FLAG_HEX,		// used with TYPE_CSTRING
	FLAG_DUP_CSTRING=FLAG_OCTAL,		// used with TYPE_CSTRING
	FLAG_QUOTED=FLAG_BINARY,			// used with TYPE_CSTRING

	OP_FX=1<<9,
	OP_FY=1<<10,
	OP_XF=1<<11,
	OP_YF=1<<12,
	OP_YFX=1<<13,
	OP_XFX=1<<14,
	OP_XFY=1<<15
};

typedef struct module_ module;
typedef struct query_ query;
typedef struct rule_ rule;
typedef struct clause_ clause;
typedef struct cell_ cell;
typedef struct parser_ parser;

struct cell_ {
	uint8_t val_type;
	uint8_t arity;
	uint16_t flags;
	idx_t nbr_cells;

	// The following unions are based off 'val_type' ...

	union {
		struct {
			int_t val_num;
			int_t val_den;
		};

		struct {
			double val_flt;
		};

		struct {
			char val_chr[MAX_SMALL_STRING];
		};

		struct {
			char *val_str;
			size_t len_str;
		};

		struct {
			union {
				int (*fn)(query*);
				rule *match;
				cell *attrs;
				uint16_t precedence;
			};

			idx_t val_off;
			idx_t slot_nbr;
		};

		struct {
			cell *val_ptr;
		};
	};
};

typedef struct {
	uint64_t u1, u2;
} uuid;

typedef struct {
	idx_t nbr_cells, cidx;
	uint16_t nbr_vars;
	unsigned first_cut:1;
	unsigned cut_only:1;
	unsigned is_deleted:1;
	unsigned is_persist:1;
	cell cells[];
} term;

struct clause_ {
	rule *parent;
	clause *next;
	module *m;
	uuid u;
	term t;
};

struct rule_ {
	rule *next;
	clause *head, *tail;
	skiplist *index;
	uint32_t cnt;
	idx_t val_off;
	uint16_t arity;
	unsigned is_prebuilt:1;
	unsigned is_public:1;
	unsigned is_dynamic:1;
	unsigned is_persist:1;
	unsigned is_multifile:1;
	unsigned is_abolished:1;
};

struct builtins {
	const char *name;
	unsigned arity;
	int (*fn)(query*);
	const char *help;
};

struct op_table {
	const char *name;
	unsigned val_type;
	unsigned precedence;
};

typedef struct {
	idx_t ctx;
	idx_t slot_nbr;
} trail;

typedef struct {
	cell c;
	idx_t ctx;
} slot;

typedef struct {
	cell *curr_cell;
	module *m;
	idx_t prev_frame, env, overflow;
	uint16_t nbr_vars, nbr_slots;
	unsigned any_choices:1;
	unsigned did_cut:1;
} frame;

typedef struct {
	FILE *fp;
	char *mode, *filename, *name, *data, *src;
	void *sslptr;
	parser *p;
	char srcbuf[STREAM_BUFLEN];
	size_t data_len, alloc_nbytes;
	int ungetch, srclen;
	uint8_t level;
	unsigned did_getc:1;
	unsigned nodelay:1;
	unsigned nonblock:1;
	unsigned udp:1;
	unsigned ssl:1;
} stream;

typedef struct {
	cell *curr_cell;
	clause *curr_clause;
	sliter *iter;
	idx_t curr_frame, fp, hp, tp, sp;
	uint8_t anbr, qnbr;
} state;

typedef struct {
	state st;
	idx_t v1, v2;
	uint32_t pins;
	uint8_t nbr_vars, nbr_slots;
	unsigned local_cut:1;
	unsigned any_choices:1;
	unsigned catchme1:1;
	unsigned catchme2:1;
} choice;

typedef struct arena_ arena;

struct arena_ {
	arena *next;
	cell *heap;
	idx_t hp, h_size;
	unsigned nbr;
};

struct query_ {
	query *prev, *next, *parent;
	module *m;
	frame *frames;
	slot *slots;
	choice *choices;
	trail *trails;
	cell *last_arg, *tmpq[MAX_QUEUES], *exception;
	cell *tmp_heap, *queue[MAX_QUEUES];
	arena *arenas;
	cell accum;
	state st;
	unsigned long long tot_goals, tot_retries, tot_matches, tot_tcos;
	unsigned long long nv_mask, step, qid;
	unsigned long long time_started;
	int max_depth, tmo_msecs;
	idx_t cp, tmphp, nv_start, latest_ctx, popp;
	idx_t frames_size, slots_size, trails_size, choices_size;
	idx_t max_choices, max_frames, max_slots, max_trails;
	idx_t h_size, tmph_size, tot_heaps, tot_heapsize;
	idx_t q_size[MAX_QUEUES], tmpq_size[MAX_QUEUES], qp[MAX_QUEUES];
	uint8_t retry, halt_code, status;
	uint8_t current_input, current_output;
	int8_t quoted;
	unsigned resume:1;
	unsigned no_tco:1;
	unsigned error:1;
	unsigned trace:1;
	unsigned calc:1;
	unsigned yielded:1;
	unsigned is_task:1;
	unsigned nl:1;
	unsigned fullstop:1;
	unsigned ignore_ops:1;
	unsigned character_escapes:1;
	unsigned halt:1;
	unsigned abort:1;
	unsigned cycle_error:1;
	unsigned spawned:1;
};

struct parser_ {
	struct {
		char var_pool[MAX_VAR_POOL_SIZE];
		unsigned var_used[MAX_ARITY];
		const char *var_name[MAX_ARITY];
	} vartab;

	FILE *fp;
	module *m;
	term *t;
	char *token, *save_line, *srcptr;
	size_t token_size, n_line;
	int line_nbr, error, depth, quoted, nbr_vars;
	uint8_t val_type;
	int8_t dq_consing;
	unsigned was_quoted:1;
	unsigned string:1;
	unsigned run_init:1;
	unsigned directive:1;
	unsigned consulting:1;
	unsigned one_shot:1;
	unsigned start_term:1;
	unsigned end_of_term:1;
	unsigned comment:1;
	unsigned is_variable:1;
	unsigned is_op;
	unsigned skip:1;
	unsigned command:1;
	unsigned in_dcg:1;
	unsigned dcg_passthru:1;
};

struct module_ {
	module *next;
	query *tasks;
	char *name, *filename;
	rule *head, *tail;
	parser *p;
	FILE *fp;
	struct op_table ops[MAX_USER_OPS+1];
    const char *keywords[1000];

	struct {
		int double_quote_codes, double_quote_chars, double_quote_atom;
		int character_escapes;
		int rational_syntax_natural, prefer_rationals;
	} flag;

	int prebuilt, halt, halt_code, status, trace, quiet, dirty;
	int user_ops, opt, stats, iso_only, use_persist, loading;
	int make_public, cpu_count, dump_vars;
};

extern idx_t g_empty_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern idx_t g_anon_s, g_clause_s, g_eof_s, g_lt_s, g_false_s;
extern idx_t g_gt_s, g_eq_s, g_sys_elapsed_s, g_sys_queue_s, g_braces_s;
extern stream g_streams[MAX_STREAMS];
extern module *g_modules;
extern char *g_pool;

inline static idx_t copy_cells(cell *dst, const cell *src, idx_t nbr_cells)
{
	memcpy(dst, src, sizeof(cell)*(nbr_cells));
	return nbr_cells;
}

#define LIST_HEAD(l) list_head(l)
#define LIST_TAIL(l) list_tail(l)

cell *list_head(cell *l);
cell *list_tail(cell *l);

int is_in_pool(const char *name, idx_t *offset);
void set_var(query *q, cell *c, idx_t ctx, cell *v, idx_t v_ctx);
void reset_value(query *q, cell *c, idx_t c_ctx, cell *v, idx_t v_ctx);
int module_load_fp(module *m, FILE *fp);
int module_load_file(module *m, const char *filename);
int module_save_file(module *m, const char *filename);
int deconsult(const char *filename);
module *create_module(const char *name);
void destroy_module(module *m);
module *find_module(const char *name);
clause *asserta_to_db(module *m, term *t, int consulting);
clause *assertz_to_db(module *m, term *t, int consulting);
clause *retract_from_db(module *m, clause *r);
clause *erase_from_db(module *m, uuid *ref);
clause *find_in_db(module *m, uuid *ref);
int get_op(module *m, const char *name, unsigned *val_type, int *userop, int hint_prefix);
void write_canonical(query *q, FILE *fp, cell *c, int running, int depth);
void write_canonical_to_stream(query *q, stream *str, cell *c, int running, int depth);
size_t write_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, int running, int depth);
void write_term(query *q, FILE *fp, cell *c, int running, int cons, int depth);
void write_term_to_stream(query *q, stream *str, cell *c, int running, int cons, int depth);
size_t write_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, int running, int cons, int depth);
void make_choice(query *q);
void make_barrier(query *q);
void make_catcher(query *q, int type);
void cut_me(query *q, int local_cut);
int check_builtin(module *m, const char *name, unsigned arity);
void *get_builtin(module *m, const char *name, unsigned arity);
void query_execute(query *q, term *t);
cell *get_head(cell *c);
cell *get_body(cell *c);
rule *find_rule(module *m, cell *c);
rule *find_functor(module *m, const char *name, unsigned arity);
int call_me(query *q, cell *p1);
void undo_me(query *q);
parser *create_parser(module *m);
void destroy_parser(parser *p);
int parser_tokenize(parser *p, int args, int consing);
int parser_attach(parser *p, int start_idx);
void parser_xref(parser *p, term *t, rule *parent);
idx_t drop_choice(query *q);
int retry_choice(query *q);
void parser_assign_vars(parser *p);
query *create_query(module *m, int sub_query);
query *create_task(query *q, cell *curr_cell);
void destroy_query(query *q);
void run_query(query *q);
cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx);
cell *clone_to_heap(query *q, int prefix, cell *p1, idx_t suffix);
void make_end(cell *tmp);
int do_match(query *q, cell *curr_cell);
idx_t find_in_pool(const char *name);
void do_reduce(cell *n);
unsigned create_vars(query *q, unsigned nbr);
unsigned count_bits(unsigned long long mask, unsigned bit);
void try_me(const query *q, unsigned vars);
void load_keywords(module *m);
void throw_error(query *q, cell *c, const char *err_type, const char *expected);
unsigned long long get_time_in_usec(void);
void clear_term(term *t);
void do_db_load(module *m);
void set_dynamic_in_db(module *m, const char *name, unsigned arity);
int set_op(module *m, const char *name, unsigned val_type, unsigned precedence);
size_t sprint_int(char *dst, size_t size, int_t n, int base);
void call_attrs(query *q, cell *attrs);
void alloc_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
cell *end_list(query *q);
int scan_list(query *q, cell *l, idx_t l_ctx);
void consultall(parser *p, cell *l);
void fix_list(cell *c, idx_t cnt);
