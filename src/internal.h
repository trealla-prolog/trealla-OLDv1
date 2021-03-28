#pragma once

#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <sys/param.h>

#ifndef USE_OPENSSL
#define USE_OPENSSL 0
#endif

#ifndef USE_INT128
#define USE_INT128 0
#endif

#ifndef USE_INT32
#define USE_INT32 0
#endif

#ifndef USE_THREADS
#define USE_THREADS 0
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

#if (__STDC_VERSION__ >= 201112L) && USE_THREADS
#include <stdatomic.h>
#define atomic_t _Atomic
#else
#define atomic_t volatile
#endif

typedef uint32_t idx_t;

#include "skiplist.h"
#include "trealla.h"
#include "cdebug.h"

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
} pl_status;

// Sentinel Value
#define ERR_IDX (~(idx_t)0)
#define IDX_MAX (ERR_IDX-1)
#define ERR_CYCLE_CMP -2

#define MAX_SMALL_STRING (MAX(sizeof(int_t),sizeof(void*))*2)
#define MAX_VAR_POOL_SIZE 1000
#define MAX_ARITY UCHAR_MAX
#define MAX_OPS 250
#define MAX_QUEUES 16
#define MAX_STREAMS 1024
#define MAX_DEPTH 9000

#define STREAM_BUFLEN 1024
#define CHECK_OVERFLOW 1

#define GET_CHOICE(i) (q->choices+(i))
#define GET_CURR_CHOICE() GET_CHOICE(q->cp-1)

#define GET_FRAME(i) (q->frames+(i))
#define GET_CURR_FRAME(i) GET_FRAME(q->st.curr_frame)

#define GET_SLOT(g,i) ((i) < g->nbr_slots ? (q->slots+g->ctx+(i)) : (q->slots+g->overflow+((i)-g->nbr_slots)))

// Primary type...

#define is_empty(c) ((c)->val_type == TYPE_EMPTY)
#define is_variable(c) ((c)->val_type == TYPE_VARIABLE)
#define is_literal(c) ((c)->val_type == TYPE_LITERAL)
#define is_cstring(c) ((c)->val_type == TYPE_CSTRING)
#define is_rational(c) ((c)->val_type == TYPE_INTEGER)
#define is_bignum(c) ((c)->val_type == TYPE_BIGNUM)
#define is_float(c) ((c)->val_type == TYPE_FLOAT)
#define is_indirect(c) ((c)->val_type == TYPE_INDIRECT)
#define is_end(c) ((c)->val_type == TYPE_END)

// Derived type...

#define is_iso_atom(c) ((is_literal(c) || is_cstring(c)) && !(c)->arity)
#define is_iso_list(c) (is_literal(c) && ((c)->arity == 2) && ((c)->val_off == g_dot_s))
#define is_cons_list(c) (is_iso_list(c) && is_variable(c+2))

#define is_atom(c) ((is_literal(c) && !(c)->arity) || is_cstring(c))
#define is_string(c) (is_cstring(c) && (c)->flags & FLAG_STRING)
#define is_blob(c) (is_cstring(c) && (c)->flags & FLAG_BLOB)
#define is_list(c) (is_iso_list(c) || is_string(c))
#define is_integer(c) (is_rational(c) && ((c)->val_den == 1))
#define is_static(c) (is_blob(c) && ((c)->flags & FLAG2_STATIC))
#define is_strbuf(c) (is_blob(c) && !((c)->flags & FLAG2_STATIC))
#define is_nil(c) (is_literal(c) && !(c)->arity && ((c)->val_off == g_nil_s))
#define is_quoted(c) ((c)->flags & FLAG2_QUOTED)
#define is_fresh(c) ((c)->flags & FLAG2_FRESH)
#define is_anon(c) ((c)->flags & FLAG2_ANON)
#define is_builtin(c) ((c)->flags & FLAG_BUILTIN)
#define is_tail(c) ((c)->flags & FLAG_TAIL)
#define is_tail_recursive(c) ((c)->flags & FLAG_TAIL_REC)
#define is_key(c) ((c)->flags & FLAG_KEY)
#define is_op(c) (c->flags && 0xFF00)

typedef struct {
	size_t len;
	uint32_t refcnt;
	char cstr[];
} strbuf;

#define SET_STR(c,s,n,off) {									\
	strbuf *strb = malloc(sizeof(strbuf) + (n) + 1);			\
	may_ptr_error(strb);										\
	memcpy(strb->cstr, s, n); 									\
	strb->cstr[n] = 0;											\
	strb->len = n;												\
	strb->refcnt = 1;											\
	(c)->val_strb = strb;										\
	(c)->strb_off = off;										\
	(c)->strb_len = n;											\
	}

#define INCR_REF(c) 											\
	if (is_strbuf(c)) {											\
		(c)->val_strb->refcnt++;								\
	}

#define DECR_REF(c)												\
	if (is_strbuf(c)) {											\
		if (!(--(c)->val_strb->refcnt))	{						\
			free((c)->val_strb);								\
			(c)->val_strb = NULL;								\
		}														\
	}

#define _GET_STR(pl,c) 											\
	( !is_cstring(c) ? ((pl)->pool + (c)->val_off)				\
	: is_strbuf(c) ? ((c)->val_strb->cstr + (c)->strb_off)		\
	: is_static(c) ? (c)->val_str								\
	: ((char*)(c)->val_chr)										\
	)

#define _LEN_STR(pl,c) 											\
	( !is_cstring(c) ? strlen((pl)->pool + (c)->val_off)		\
	: is_strbuf(c) ? (c)->strb_len								\
	: is_static(c) ? (c)->str_len								\
	: strlen((c)->val_chr)										\
	)

#define GET_STR(c) _GET_STR(q->st.m->pl, c)
#define LEN_STR(c) _LEN_STR(q->st.m->pl, c)

#define PARSER_GET_STR(c) _GET_STR(p->m->pl, c)
#define PARSER_LEN_STR(c) _LEN_STR(p->m->pl, c)

#define MODULE_GET_STR(c) _GET_STR(m->pl, c)
#define MODULE_LEN_STR(c) _LEN_STR(m->pl, c)

#define QUERY_GET_POOL(off) (q->st.m->pl->pool + (off))
#define MODULE_GET_POOL(off) (m->pl->pool + (off))
#define PARSER_GET_POOL(off) (p->m->pl->pool + (off))

// Wrap an assignment that's expected to return anything but the given sentinel value.
// when the sentinel otherwise does some (optional) error handling action
// default action is 'error=true' to indicate an error happened
#define CHECK_SENTINEL(expr, err_sentinel, ...) CHECK_SENTINEL_((expr), err_sentinel, ## __VA_ARGS__, error=true)
#define CHECK_SENTINEL_(expr, err_sentinel, on_error, ...) do { if((expr) == err_sentinel){message(#expr " = " #err_sentinel); on_error;}} while (0)

#define may_error(expr, ...) CHECK_SENTINEL(expr, pl_error, __VA_ARGS__; return pl_error)
#define may_idx_error(expr, ...) CHECK_SENTINEL(expr, ERR_IDX, __VA_ARGS__; return pl_error)
#define may_ptr_error(expr, ...) CHECK_SENTINEL(expr, NULL, __VA_ARGS__; return pl_error)
#define may_cycle_error(expr, ...) CHECK_SENTINEL(expr, ERR_CYCLE_CELL, __VA_ARGS__; return pl_cycle)

// If changing the order of these: see runtime.c dispatch table

enum {
	TYPE_EMPTY=0,
	TYPE_VARIABLE=1,
	TYPE_LITERAL=2,
	TYPE_CSTRING=3,
	TYPE_INTEGER=4,
	TYPE_BIGNUM=5,
	TYPE_FLOAT=6,
	TYPE_INDIRECT=7,
	TYPE_END=8
};

enum {
	FLAG_BUILTIN=1<<0,
	FLAG_HEX=1<<1,						// used with TYPE_INTEGER
	FLAG_OCTAL=1<<2,					// used with TYPE_INTEGER
	FLAG_BINARY=1<<3,					// used with TYPE_INTEGER
	FLAG_STREAM=1<<4,					// used with TYPE_INTEGER
	FLAG_TAIL_REC=1<<5,
	FLAG_TAIL=1<<6,
	FLAG_BLOB=1<<7,						// used with TYPE_CSTRING
	FLAG_STRING=1<<8,					// used with TYPE_CSTRING
	FLAG_KEY=1<<9,						// used with keys

	FLAG_SPARE3=1<<10,
	FLAG_SPARE2=1<<11,
	FLAG_SPARE1=1<<12,

	FLAG2_PROCESSED=FLAG_KEY,			// used by bagof
	FLAG2_FIRST_USE=FLAG_HEX,			// used with TYPE_VARIABLE
	FLAG2_ANON=FLAG_OCTAL,				// used with TYPE_VARIABLE
	FLAG2_FRESH=FLAG_BINARY,			// used with TYPE_VARIABLE
	FLAG2_STATIC=FLAG_HEX,				// used with TYPE_CSTRING
	FLAG2_QUOTED=FLAG_OCTAL,			// used with TYPE_CSTRING

	FLAG_END=1<<13
};

// The OP types are stored in the high 3 bits of the flag (13-15)

#define	OP_FX 1
#define	OP_FY 2
#define	OP_XF 3
#define	OP_YF 4
#define	OP_YFX 5
#define	OP_XFX 6
#define	OP_XFY 7

#define IS_PREFIX(op) (((op) == OP_FX) || ((op) == OP_FY))
#define IS_POSTFIX(op) (((op) == OP_XF) || ((op) == OP_YF))
#define IS_INFIX(op) (((op) == OP_XFX) || ((op) == OP_XFY) || ((op) == OP_YFX))

#define CELL_PREFIX(c) IS_PREFIX(GET_OP(c))
#define CELL_POSTFIX(c) IS_POSTFIX(GET_OP(c))
#define CELL_INFIX(c) IS_INFIX(GET_OP(c))

#define IS_FX(c) (GET_OP(c) == OP_FX)
#define IS_FY(c) (GET_OP(c) == OP_FY)
#define IS_XF(c) (GET_OP(c) == OP_XF)
#define IS_YF(c) (GET_OP(c) == OP_YF)
#define IS_YFX(c) (GET_OP(c) == OP_YFX)
#define IS_XFX(c) (GET_OP(c) == OP_XFX)
#define IS_XFY(c) (GET_OP(c) == OP_XFY)

#define SET_OP(c,op) (CLR_OP(c), (c)->flags |= (((uint16_t)(op)) << 13))
#define CLR_OP(c) ((c)->flags &= ~((uint16_t)(0xF) << 13))
#define GET_OP(c) (((c)->flags >> 13) & 0xF)
#define IS_OP(c) (GET_OP(c) != 0 ? true : false)

typedef struct module_ module;
typedef struct query_ query;
typedef struct predicate_ predicate;
typedef struct clause_ clause;
typedef struct cell_ cell;
typedef struct parser_ parser;

// Using a fixed-size cell allows having arrays of cells, which is
// basically what a term is. A compound is a variable length array
// of cells, the length specified by 'nbr_cells' field in header.

struct cell_ {
	uint8_t val_type;
	uint8_t arity;
	uint16_t flags;
	idx_t nbr_cells;

	// Tagged union based off 'val_type' ...

	union {

		// A rational (and integer)...

		struct {
			int_t val_num;
			int_t val_den;
		};

		// A double...

		struct {
			double val_flt;
		};

		// A call return...

		struct {
			cell *val_ptr;
			idx_t cgen;				// choice generation
			idx_t mod_nbr;
		};

		// A small (inline) cstring (includes NULL-delimiter)...

		struct {
			char val_chr[MAX_SMALL_STRING];
		};

		// A ref-counted length-defined string...

		struct {
			strbuf *val_strb;
			uint32_t strb_off;		// slice offset
			uint32_t strb_len;		// slice length (or zero)
		};

		// A static length-defined string...

		struct {
			char *val_str;
			size_t str_len;			// slice_length
		};

		// An atom, var or predicate...

		struct {
			union {
				pl_status (*fn)(query*);
				predicate *match;
				cell *attrs;		// used in slots
				uint16_t priority;	// used in parsing operators
			};

			idx_t val_off;			// offset into pool
			idx_t var_nbr;			// used with TYPE_VAR
		};
	};
};

extern cell* ERR_CYCLE_CELL;

typedef struct {
	uint64_t u1, u2;
} uuid;

typedef struct {
	uint64_t ugen_created, ugen_erased;
	idx_t nbr_cells, cidx;
	uint16_t nbr_vars;
	bool first_cut:1;
	bool cut_only:1;
	bool is_fact:1;
	bool persist:1;
	bool tail_rec:1;
	cell cells[];
} term;

struct clause_ {
	predicate *owner;
	clause *prev, *next, *dirty;
	module *m;
	uuid u;
	term t;
};

struct predicate_ {
	predicate *next;
	clause *head, *tail;
	skiplist *index, *index_save;
	cell key;
	unsigned cnt;
	bool is_prebuilt:1;
	bool is_public:1;
	bool is_dynamic:1;
	bool is_meta_predicate:1;
	bool is_persist:1;
	bool is_multifile:1;
	bool is_discontiguous:1;
	bool is_abolished:1;
	bool is_noindex:1;
	bool check_directive:1;
};

struct builtins {
	const char *name;
	unsigned arity;
	pl_status (*fn)(query*);
	const char *help;
};

struct op_table {
	char *name;
	unsigned specifier;
	unsigned priority;
};

typedef struct {
	cell *attrs;
	idx_t ctx;
	uint16_t var_nbr;
} trail;

typedef struct {
	cell c;
	idx_t ctx;
} slot;

typedef struct {
	cell *prev_cell;
	module *m;
	uint64_t ugen;
	idx_t prev_frame, ctx, overflow, cgen;
	uint16_t nbr_vars, nbr_slots;
} frame;

enum { eof_action_eof_code, eof_action_error, eof_action_reset };

typedef struct {
	FILE *fp;
	char *mode, *filename, *name, *data, *src;
	void *sslptr;
	parser *p;
	char srcbuf[STREAM_BUFLEN];
	size_t data_len, alloc_nbytes;
	int ungetch, srclen;
	uint8_t level, eof_action;
	bool at_end_of_file:1;
	bool binary:1;
	bool did_getc:1;
	bool socket:1;
	bool nodelay:1;
	bool nonblock:1;
	bool udp:1;
	bool ssl:1;
} stream;

typedef struct {
	cell *curr_cell;
	clause *curr_clause, *curr_clause2;
	sliter *iter, *iter2;
	module *m;
	idx_t curr_frame, fp, hp, tp, sp, cgen;
	uint8_t anbr, qnbr;
} prolog_state;

typedef struct {
	prolog_state st;
	uint64_t pins, ugen;
	idx_t v1, v2, cgen, orig_cgen, overflow;
	uint16_t nbr_vars, nbr_slots;
	bool catchme_retry:1;
	bool catchme_exception:1;
	bool barrier:1;
	bool soft_cut:1;
	bool did_cleanup:1;
	bool register_cleanup:1;
	bool register_term:1;
	bool chk_is_det:1;
	bool tail_rec:1;
} choice;

typedef struct arena_ arena;

struct arena_ {
	arena *next;
	cell *heap;
	idx_t hp, h_size;
	unsigned nbr;
};

enum q_retry { QUERY_OK=0, QUERY_RETRY=1, QUERY_EXCEPTION=2 };
enum unknowns { UNK_FAIL=0, UNK_ERROR=1, UNK_WARNING=2, UNK_CHANGEABLE=3 };

typedef struct prolog_flags_ {
	short occurs_check;
	enum unknowns unknown;
	bool double_quote_codes:1;
	bool double_quote_chars:1;
	bool double_quote_atom:1;
	bool character_escapes:1;
	bool char_conversion:1;
	bool rational_syntax_natural:1;
	bool prefer_rationals:1;
	bool debug:1;
} prolog_flags;

struct query_ {
	query *prev, *next, *parent;
	module *save_m;
	parser *p;
	frame *frames;
	slot *slots;
	choice *choices;
	trail *trails;
	cell *tmp_heap, *last_arg, *exception, *variable_names;
	cell *queue[MAX_QUEUES], *tmpq[MAX_QUEUES];
	arena *arenas;
	clause *dirty_list;
	cell *save_c;
	cell accum;
	prolog_state st;
	uint64_t tot_goals, tot_retries, tot_matches, tot_tcos;
	uint64_t step, qid, time_started;
	unsigned max_depth, tmo_msecs;
	int nv_start;
	idx_t cp, tmphp, latest_ctx, popp, variable_names_ctx, save_cp;
	idx_t frames_size, slots_size, trails_size, choices_size;
	idx_t max_choices, max_frames, max_slots, max_trails, save_tp;
	idx_t h_size, tmph_size, tot_heaps, tot_heapsize, undo_tp;
	idx_t q_size[MAX_QUEUES], tmpq_size[MAX_QUEUES], qp[MAX_QUEUES];
	uint8_t nv_mask[MAX_ARITY];
	prolog_flags flag;
	enum q_retry retry;
	int8_t halt_code;
	int8_t quoted;
	bool has_attrs:1;
	bool do_dump_vars:1;
	bool status:1;
	bool resume:1;
	bool no_tco:1;
	bool error:1;
	bool did_throw:1;
	bool trace:1;
	bool calc:1;
	bool yielded:1;
	bool is_task:1;
	bool nl:1;
	bool fullstop:1;
	bool ignore_ops:1;
	bool numbervars:1;
	bool halt:1;
	bool abort:1;
	bool cycle_error:1;
	bool spawned:1;
	bool run_init:1;
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
	cell v;
	size_t token_size, n_line, toklen;
	prolog_flags flag;
	unsigned line_nbr, depth, read_term;
	unsigned nesting_parens, nesting_braces, nesting_brackets;
	int quote_char;
	unsigned nbr_vars;
	int8_t dq_consing;
	bool error;
	bool already_loaded:1;
	bool do_read_term:1;
	bool string:1;
	bool run_init:1;
	bool directive:1;
	bool consulting:1;
	bool one_shot:1;
	bool start_term:1;
	bool end_of_term:1;
	bool comment:1;
	bool is_quoted:1;
	bool is_variable:1;
	bool is_op:1;
	bool skip:1;
	bool command:1;
};

struct loaded_file {
	struct loaded_file *next;
	char filename[PATH_MAX];
};

struct module_ {
	module *next;
	prolog *pl;
	query *tasks;
	char *name, *filename;
	predicate *head, *tail;
	parser *p;
	FILE *fp;
	skiplist *index, *nbs;
	clause *dirty_list;
	struct loaded_file *loaded_files;
	struct op_table def_ops[MAX_OPS+1];
	struct op_table ops[MAX_OPS+1];
	idx_t id;
	prolog_flags flag;
	unsigned spare_ops, loaded_ops;
	bool prebuilt:1;
	bool use_persist:1;
	bool make_public:1;
	bool loaded_properties:1;
	bool loading:1;
	bool error:1;
};

struct prolog_ {
	idx_t tab1[64000];
	idx_t tab3[64000];
	idx_t tab2[64000];
	idx_t tab4[64000];
	uint8_t tab5[64000];
	module *modules;
	module *m, *curr_m;
	uint64_t s_last, s_cnt, seed;
	skiplist *symtab, *funtab;
	char *pool;
	uint64_t ugen;
	idx_t pool_offset, pool_size, tab_idx;
	unsigned varno;
	uint8_t current_input, current_output, current_error;
	int8_t halt_code, opt;
	bool halt:1;
	bool status:1;
	bool did_dump_vars:1;
	bool quiet:1;
	bool stats:1;
	bool noindex:1;
	bool iso_only:1;
	bool trace:1;
};

extern idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
extern idx_t g_anon_s, g_clause_s, g_eof_s, g_lt_s, g_false_s, g_once_s;
extern idx_t g_gt_s, g_eq_s, g_sys_elapsed_s, g_sys_queue_s, g_braces_s;
extern idx_t g_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
extern idx_t g_call_s, g_braces_s, g_plus_s, g_minus_s;
extern stream g_streams[MAX_STREAMS];
extern unsigned g_cpu_count;

inline static idx_t copy_cells(cell *dst, const cell *src, idx_t nbr_cells)
{
	memcpy(dst, src, sizeof(cell)*nbr_cells);
	return nbr_cells;
}

inline static idx_t safe_copy_cells(cell *dst, const cell *src, idx_t nbr_cells)
{
	for (idx_t i = 0; i < nbr_cells; i++, dst++, src++) {
		INCR_REF(src);
		*dst = *src;
	}

	return nbr_cells;
}

inline static void chk_cells(cell *src, idx_t nbr_cells)
{
	for (idx_t i = 0; i < nbr_cells; i++, src++) {
		DECR_REF(src);
	}
}

#define LIST_HANDLER(l) cell l##_h_tmp; cell l##_t_tmp
#define LIST_HEAD(l) list_head(l, &l##_h_tmp)
#define LIST_TAIL(l) list_tail(l, &l##_t_tmp)

cell *list_head(cell *l, cell *tmp);
cell *list_tail(cell *l, cell *tmp);

enum {DO_CLAUSE, DO_RETRACT, DO_STREAM_RETRACT, DO_RETRACTALL};

USE_RESULT size_t alloc_grow(void** addr, size_t elem_size, size_t min_elements, size_t max_elements);
void set_var(query *q, const cell *c, idx_t ctx, cell *v, idx_t v_ctx);
void reset_value(query *q, const cell *c, idx_t c_ctx, cell *v, idx_t v_ctx);
bool module_load_fp(module *m, FILE *fp);
bool module_load_file(module *m, const char *filename);
bool module_save_file(module *m, const char *filename);
bool deconsult(prolog *pl, const char *filename);
module *create_module(prolog *pl, const char *name);
void destroy_module(module *m);
module *find_module(prolog *pl, const char *name);
module *find_module_id(prolog *pl, idx_t id);
module *find_next_module(prolog *pl, module *m);
clause *asserta_to_db(module *m, term *t, bool consulting);
clause *assertz_to_db(module *m, term *t, bool consulting);
clause *erase_from_db(module *m, uuid *ref);
clause *find_in_db(module *m, uuid *ref);
unsigned get_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
unsigned get_op2(module *m, const char *name, unsigned specifier);
bool set_op(module *m, const char *name, unsigned specifier, unsigned priority);
USE_RESULT pl_status make_choice(query *q);
USE_RESULT pl_status make_barrier(query *q);
USE_RESULT pl_status make_catcher(query *q, enum q_retry type);
void cut_me(query *q, bool local_cut, bool soft_cut);
void *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found);
pl_status query_execute(query *q, term *t);
bool check_rule(const cell *c);
cell *get_head(cell *c);
cell *get_body(cell *c);
cell *get_logical_body(cell *c);
predicate *find_functor(module *m, const char *name, unsigned arity);
predicate *find_predicate(module *m, cell *c);
predicate *search_predicate(module *m, cell *c);
unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix);
USE_RESULT pl_status fn_call_0(query *q, cell *p1);
void undo_me(query *q);
parser *create_parser(module *m);
void destroy_parser(parser *p);
unsigned parser_tokenize(parser *p, bool args, bool consing);
void term_xref(parser *p, term *t, predicate *parent);
void parser_reset(parser *p);
idx_t drop_choice(query *q);
bool retry_choice(query *q);
void term_assign_vars(parser *p, unsigned start, bool rebase);
query *create_query(module *m, bool sub_query);
query *create_task(query *q, cell *curr_cell);
void destroy_query(query *q);
USE_RESULT pl_status query_start(query *q);

cell *deep_clone_to_heap(query *q, cell *p1, idx_t p1_ctx);
cell *clone_to_heap(query *q, bool prefix, cell *p1, idx_t suffix);
cell *deep_copy_to_heap(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);
cell *deep_copy_to_tmp(query *q, cell *p1, idx_t p1_ctx, bool nonlocals_only, bool copy_attrs);
cell *deep_clone_to_tmp(query *q, cell *p1, idx_t p1_ctx);

cell *alloc_on_heap(query *q, idx_t nbr_cells);
cell *alloc_on_tmp(query *q, idx_t nbr_cells);
cell *alloc_on_queuen(query *q, int qnbr, const cell *c);

cell *init_tmp_heap(query* q);
inline static idx_t tmp_heap_used(const query *q) { return q->tmphp; }
inline static cell *get_tmp_heap(const query *q, idx_t i) { return q->tmp_heap + i; }

void make_end(cell *tmp);
USE_RESULT pl_status match_rule(query *q, cell *p1, idx_t p1_ctx);
USE_RESULT pl_status match_clause(query *q, cell *p1, idx_t p1_ctx, int retract);
idx_t index_from_pool(prolog *pl, const char *name);
void do_reduce(cell *n);
unsigned create_vars(query *q, unsigned nbr);
unsigned count_bits(const uint8_t *mask, unsigned bit);
void try_me(const query *q, unsigned vars);
USE_RESULT pl_status throw_error(query *q, cell *c, const char *err_type, const char *expected);
uint64_t get_time_in_usec(void);
void clear_term(term *t);
void do_db_load(module *m);
size_t sprint_int(char *dst, size_t size, int_t n, int base);
void call_attrs(query *q, cell *attrs);
void allocate_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
USE_RESULT cell *end_list(query *q);
bool is_valid_list(query *q, cell *p1, idx_t p1_ctx, bool partial_list);
size_t scan_is_chars_list(query *q, cell *l, idx_t l_ctx, bool tolerant);
void consultall(parser *p, cell *l);
void fix_list(cell *c);
module *module_load_text(module *m, const char *src, const char *filename);
void make_indirect(cell *tmp, cell *c);
void stash_me(query *q, term *t, bool last_match);
unsigned fake_numbervars(query *q, cell *c, idx_t c_ctx, unsigned start);
char *relative_to(const char *basefile, const char *relfile);
void term_to_body(parser *p);
cell *check_body_callable(parser *p, cell *c);
void load_builtins(prolog *pl);
void add_to_dirty_list(query *q, clause *r);
char *format_property(char **bufptr, size_t *lenptr, char *dst, const char *name, unsigned arity, const char *type);
bool needs_quoting(module *m, const char *src, int srclen);
size_t formatted(char *dst, size_t dstlen, const char *src, int srclen, bool dq);
bool has_vars(query *q, cell *c, idx_t c_ctx, unsigned depth);
pl_status do_post_unification_checks(query *q);

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool cons, unsigned depth);
pl_status print_term(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
pl_status print_term_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);
char *print_term_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);

ssize_t print_canonical_to_buf(query *q, char *dst, size_t dstlen, cell *c, idx_t c_ctx, int running, bool unused, unsigned depth);
pl_status print_canonical(query *q, FILE *fp, cell *c, idx_t c_ctx, int running);
char *print_canonical_to_strbuf(query *q, cell *c, idx_t c_ctx, int running);
pl_status print_canonical_to_stream(query *q, stream *str, cell *c, idx_t c_ctx, int running);

// A string builder...

typedef struct {
	char *buf, *dst;
	size_t size;
}
 STRING;

#define STRING_INIT(pr) STRING pr_##buf;								\
	pr_##buf.size = 0;													\
	pr_##buf.buf = NULL;												\
	pr_##buf.dst = pr_##buf.buf;

#define STRING_INITn(pr,len) STRING pr_##buf; 							\
	pr_##buf.size = len;												\
	pr_##buf.buf = malloc(len+1);										\
	pr_##buf.dst = pr_##buf.buf;

#define STRING_CSTR(pr) pr_##buf.buf
#define STRING_LEN(pr) (pr_##buf.dst - pr_##buf.buf)

#define STRING_CHK(pr,len) {											\
	size_t rem = pr_##buf.size - STRING_LEN(pr);						\
	if (len >= rem) {													\
		size_t offset = STRING_LEN(pr);									\
		pr_##buf.buf = realloc(pr_##buf.buf, (pr_##buf.size += (len-rem)) + 1); \
		ensure(pr_##buf.buf);											\
		pr_##buf.dst = pr_##buf.buf + offset;							\
	}																	\
}

#define STRING_CATn(pr,s,len) {											\
	STRING_CHK(pr, len);												\
	memcpy(pr_##buf.dst, s, len+1);										\
	pr_##buf.dst += len;												\
	*pr_##buf.dst = '\0';												\
}

#define STRING_CAT2n(pr,s1,len1,s2,len2) {								\
	STRING_CATn(pr,s1,len1); 											\
	STRING_CATn(pr,s2,len2);											\
}

#define STRING_CAT(pr,s) STRING_CATn(pr,s,strlen(s))
#define STRING_CAT2(pr,s1,s2) STRING_CAT2n(pr,s1,strlen(s1),s2,strlen(s2))
#define STRING_DONE(pr) { free(pr_##buf.buf); pr_##buf.buf = NULL; }
