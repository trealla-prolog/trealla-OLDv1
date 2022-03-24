#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <fenv.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"
#include "module.h"
#include "prolog.h"
#include "heap.h"

#define SET_ACCUM() {											\
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
	q->accum.tag = TAG_INT;										\
	q->accum.flags = FLAG_MANAGED;								\
	q->accum.val_bigint = malloc(sizeof(bigint));				\
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
	q->accum.val_bigint->refcnt = 0;							\
	if (mp_int_init_copy(&q->accum.val_bigint->ival, &q->tmp_ival) == MP_MEMORY) {\
		return throw_error(q, &q->accum, q->st.curr_frame, "resource_error", "memory"); \
	} \
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
}

static void clr_accum(cell *p)
{
	if (is_bigint(p) && !p->val_bigint->refcnt) {
		mp_int_clear(&p->val_bigint->ival);
		free(p->val_bigint);
	}

	p->tag = TAG_INT;
	p->val_int = 0;
	p->flags = 0;
}

#define CLEANUP __attribute__((cleanup (clr_accum)))

#if defined(__SIZEOF_INT128__)

#define ON_OVERFLOW(op,v1,v2)									\
	__int128_t tmp = (__int128_t)v1 op v2;						\
	if ((tmp > INT64_MAX) || (tmp < INT64_MIN))

#else

#define ON_OVERFLOW(op,v1,v2)									\
	if ((v1) >= INT32_MAX ||									\
		(v1) <= INT32_MIN ||									\
		(v2) >= INT32_MAX ||									\
		(v2) <= INT32_MIN)

#endif

#define DO_OP2(op,op2,p1,p2) \
	if (is_bigint(&p1)) { \
		if (is_bigint(&p2)) { \
			mp_int_##op2(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival); \
			SET_ACCUM(); \
		} else if (is_smallint(&p2)) { \
			mp_int_##op2##_value(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival); \
			SET_ACCUM(); \
		} else if (is_real(&p2)) { \
			double d = BIGINT_TO_DOUBLE(&p1.val_bigint->ival); \
			q->accum.val_real = d op p2.val_real; \
			q->accum.tag = TAG_REAL; \
			q->accum.flags = 0; \
		} \
	} else if (is_bigint(&p2)) { \
		if (is_smallint(&p1)) { \
			mpz_t tmp; \
			mp_int_init_value(&tmp, p1.val_int); \
			mp_int_##op2(&tmp, &p2.val_bigint->ival, &q->tmp_ival); \
			mp_int_clear(&tmp); \
			SET_ACCUM(); \
		} else if (is_real(&p1)) { \
			double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival); \
			q->accum.val_real = p1.val_real op d; \
			if (isinf(q->accum.val_real)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
			q->accum.tag = TAG_REAL; \
			q->accum.flags = 0; \
		} \
	} else if (is_smallint(&p1) && is_smallint(&p2)) { \
		ON_OVERFLOW(op, p1.val_int, p2.val_int) { \
			mp_int_set_value(&q->tmp_ival, p1.val_int); \
			mp_int_##op2##_value(&q->tmp_ival, p2.val_int, &q->tmp_ival); \
			SET_ACCUM(); \
		} else { \
			q->accum.val_int = p1.val_int op p2.val_int; \
			q->accum.tag = TAG_INT; \
		} \
	} else if (is_smallint(&p1) && is_real(&p2)) { \
		q->accum.val_real = (double)p1.val_int op p2.val_real; \
		q->accum.tag = TAG_REAL; \
	} else if (is_real(&p1) && is_real(&p2)) { \
		q->accum.val_real = p1.val_real op p2.val_real; \
		q->accum.tag = TAG_REAL; \
	} else if (is_real(&p1) && is_smallint(&p2)) { \
		q->accum.val_real = p1.val_real op p2.val_int; \
		q->accum.tag = TAG_REAL; \
	} else if (is_variable(&p1) || is_variable(&p2)) { \
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated"); \
	} else { \
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable"); \
	}

#define DO_OP2int(op,op2,p1,p2) \
	if (is_bigint(&p1)) { \
		if (is_bigint(&p2)) { \
			mp_int_##op2(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival); \
			SET_ACCUM(); \
		} else if (is_smallint(&p2)) { \
			mp_int_##op2##_value(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival); \
			SET_ACCUM(); \
		} else { \
			return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable"); \
		} \
	} else if (is_bigint(&p2)) { \
		if (is_smallint(&p1)) { \
			mpz_t tmp; \
			mp_int_init_value(&tmp, p1.val_int); \
			mp_int_##op2(&tmp, &p2.val_bigint->ival, &q->tmp_ival); \
			mp_int_clear(&tmp); \
			SET_ACCUM(); \
		} else { \
			return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable"); \
		} \
	} else if (is_smallint(&p1) && is_smallint(&p2)) { \
		ON_OVERFLOW(op, p1.val_int, p2.val_int) { \
			mp_int_set_value(&q->tmp_ival, p1.val_int); \
			mp_int_##op2##_value(&q->tmp_ival, p2.val_int, &q->tmp_ival); \
			SET_ACCUM(); \
		} else { \
			q->accum.val_int = p1.val_int op p2.val_int; \
			q->accum.tag = TAG_INT; \
		} \
	} else if (is_variable(&p1) || is_variable(&p2)) { \
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated"); \
	} else { \
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable"); \
	}

static double BIGINT_TO_DOUBLE(mpz_t *v)
{
	double d;
	mp_int_to_double(v, &d);
	return d;
}

static mp_result mp_int_divx(mp_int a, mp_int b, mp_int q)
{
	return mp_int_div(a, b, q, NULL);
}

static mp_result mp_int_divx_value(mp_int a, mp_small b, mp_int q)
{
	return mp_int_div_value(a, b, q, NULL);
}

#define CHECK_CALC()							\
	clr_accum(&q->accum);						\
	errno = 0;									\
												\
	if (!q->eval) {								\
		if (q->st.m->flag.unknown == 0)			\
			return false;						\
		else									\
			return throw_error(q, q->st.curr_cell, q->st.curr_frame, "existence_error", "procedure");	\
	}

void call_builtin(query *q, cell *c, pl_idx_t c_ctx)
{
	cell *save = q->st.curr_cell;
	pl_idx_t save_ctx = q->st.curr_frame;
	bool save_calc = q->eval;
	q->st.curr_cell = c;
	q->st.curr_frame = c_ctx;
	q->eval = true;

	if (c->fn)
		c->fn(q);

	q->eval = save_calc;

	if (!q->did_throw) {
		q->st.curr_cell = save;
		q->st.curr_frame = save_ctx;
	}
}

pl_status call_userfun(query *q, cell *c, pl_idx_t c_ctx)
{
	if (q->retry)
		return pl_failure;

	if (is_string(c))
		return throw_error(q, c, c_ctx, "type_error", "evaluable");

	if (!c->match)
		c->match = search_predicate(q->st.m, c);

	if (!c->match)
		return throw_error(q, c, c_ctx, "type_error", "evaluable");

	return throw_error(q, c, c_ctx, "type_error", "evaluable");

	cell *save = q->st.curr_cell;
	pl_idx_t save_ctx = q->st.curr_frame;
	cell *tmp = clone_to_heap(q, true, c, 2);
	pl_idx_t nbr_cells = 1 + c->nbr_cells;
	make_structure(tmp+nbr_cells++, g_sys_cut_if_det_s, fn_sys_cut_if_det_0, 0, 0);
	make_return(q, tmp+nbr_cells);
	may_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	pl_status ok = start(q);
	q->error = false;

	if (!q->did_throw) {
		q->st.curr_cell = save;
		q->st.curr_frame = save_ctx;
	}

	return ok;
}

static USE_RESULT pl_status fn_return_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum = p1;
	cut_me(q, true, false);
	drop_choice(q);
	trim_trail(q);
	q->error = true;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_is_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p2 = eval(q, p2_tmp);
	p2.nbr_cells = 1;

	if (is_variable(p1) && is_number(&p2)) {
		set_var(q, p1, p1_ctx, &p2, q->st.curr_frame);
		clr_accum(&q->accum);
		return pl_success;
	}

	if (is_bigint(p1) && is_bigint(&p2))
		return !mp_int_compare(&p1->val_bigint->ival, &p2.val_bigint->ival);

	if (is_bigint(p1) && is_smallint(&p2))
		return !mp_int_compare_value(&p1->val_bigint->ival, p2.val_int);

	if (is_bigint(&p2) && is_smallint(p1))
		return !mp_int_compare_value(&p2.val_bigint->ival, p1->val_int);

	if (is_smallint(p1) && is_smallint(&p2))
		return (p1->val_int == p2.val_int);

	if (is_real(p1) && is_real(&p2))
		return p1->val_real == p2.val_real;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(q, p1), "nan"))
		return is_real(&p2)? isnan(p2.val_real) : 0;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(q, p1), "inf"))
		return is_real(&p2) ? isinf(p2.val_real) : 0;

	return pl_failure;
}

USE_RESULT pl_status fn_iso_float_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->eval) {
		CLEANUP cell p1 = eval(q, p1_tmp);

		if (is_real(&p1)) {
			q->accum.val_real = p1.val_real;
			q->accum.tag = TAG_REAL;
			return pl_success;
		}

		if (is_bigint(&p1)) {
			q->accum.val_real = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

			if (isinf(q->accum.val_real))
				return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

			q->accum.tag = TAG_REAL;
			return pl_success;
		}

		if (is_smallint(&p1)) {
			q->accum.val_real = (double)p1.val_int;
			q->accum.tag = TAG_REAL;
			return pl_success;
		}

		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer_or_float");
	}

	return is_real(p1_tmp);
}

USE_RESULT pl_status fn_iso_integer_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->eval) {
		CLEANUP cell p1 = eval(q, p1_tmp);

		if (is_real(&p1) && (p1.val_real < (double)PL_INT_MAX) && (p1.val_real > (double)PL_INT_MIN)) {
			q->accum.val_int = (pl_int_t)p1.val_real;
			q->accum.tag = TAG_INT;
			return pl_success;
		}

		if (is_real(&p1)) {
			mp_int_set_double(&q->tmp_ival, p1.val_real);
			SET_ACCUM();
			return pl_success;
		}

		if (is_integer(&p1)) {
			share_cell(&p1);
			q->accum = p1;
			return pl_success;
		}

		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer_or_float");
	}

	return is_integer(p1_tmp);
}

static USE_RESULT pl_status fn_iso_abs_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_bigint(&p1)) {
		mp_int_abs(&p1.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_smallint(&p1))
		q->accum.val_int = llabs((long long)p1.val_int);
	else if (is_real(&p1))
		q->accum.val_real = fabs(p1.val_real);
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sign_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_bigint(&p1)) {
		q->accum.val_int = mp_int_compare_zero(&p1.val_bigint->ival);
	} else if (is_smallint(&p1))
		q->accum.val_int = p1.val_int < 0 ? -1 : p1.val_int > 0  ? 1 : 0;
	else if (is_real(&p1))
		q->accum.val_real = p1.val_real < 0 ? -1 : p1.val_real > 0  ? 1 : 0;
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_positive_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum = p1;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_negative_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_bigint(&p1)) {
		mp_int_neg(&p1.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_smallint(&p1))
		q->accum.val_int = -p1.val_int;
	else if (is_real(&p1))
		q->accum.val_real = -p1.val_real;
	else if (is_variable(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_epsilon_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = DBL_EPSILON;
	q->accum.tag = TAG_REAL;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_pi_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = M_PI;
	q->accum.tag = TAG_REAL;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_e_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = M_E;
	q->accum.tag = TAG_REAL;
	return pl_success;
}

USE_RESULT pl_status fn_iso_add_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(+, add, p1, p2);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_sub_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(-, sub, p1, p2);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_mul_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(*, mul, p1, p2);
	return pl_success;
}

static USE_RESULT pl_status fn_iso_exp_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = exp(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));
		q->accum.tag = TAG_REAL;

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_smallint(&p1)) {
		q->accum.val_real = exp((double)p1.val_int);
		q->accum.tag = TAG_REAL;

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_real(&p1)) {
		q->accum.val_real = exp(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sqrt_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) < 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = sqrt(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));
		q->accum.tag = TAG_REAL;

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_smallint(&p1)) {
		if (p1.val_int < 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = sqrt((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		if (p1.val_real == -1)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = sqrt(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_log_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = log(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));
		q->accum.tag = TAG_REAL;

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_smallint(&p1)) {
		if (p1.val_int <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = log((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		if (p1.val_real <= 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = log(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_popcount_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_popcount(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_zero");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 0)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_zero");

		uint64_t n = p1.val_int;
		uint64_t count = 0;

		while (n > 0) {
			n = n & (n - 1);
			count++;
		}

		q->accum.val_int = count;
	}

	q->accum.tag = TAG_INT;
	return pl_success;
}

static USE_RESULT pl_status fn_lsb_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_lsb(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 1)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		uint64_t n = p1.val_int;
    	uint64_t lsb = 0;
    	while ((n & 1) == 0) {
    	    ++lsb;
    	    n = n >> 1;
    	}

		q->accum.val_int = lsb;
	}

	q->accum.tag = TAG_INT;
	return pl_success;
}

static USE_RESULT pl_status fn_msb_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_msb(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 1)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		uint64_t n = p1.val_int;
    	uint64_t msb = -1;
    	while (n != 0) {
    	    msb++;
    	    n = n >> 1;
    	}

		q->accum.val_int = msb;
	}

	q->accum.tag = TAG_INT;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_truncate_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_int = (pl_int_t)p1.val_real;

		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_real);
			SET_ACCUM();
		} else
			q->accum.tag = TAG_INT;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_round_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		double f = fabs(p1.val_real);

		if ((f - floor(f)) > 0.5)
			fesetround(FE_TONEAREST);
		else
			fesetround(FE_UPWARD);

		q->accum.val_int = llrint(p1.val_real);

		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_real);
			SET_ACCUM();
		} else
			q->accum.tag = TAG_INT;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_ceiling_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_int = (pl_int_t)ceil(p1.val_real);

		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_real);
			SET_ACCUM();
		} else
			q->accum.tag = TAG_INT;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_float_integer_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_real = (pl_int_t)p1.val_real;
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_float_fractional_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_real = p1.val_real - (pl_int_t)p1.val_real;
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_floor_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_int = (pl_int_t)floor(p1.val_real);

		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_real);
			SET_ACCUM();
		} else
			q->accum.tag = TAG_INT;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = sin((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = sin(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_cos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = cos((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = cos(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_tan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = tan((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = tan(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_asin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = asin((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = asin(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_acos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = acos((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = acos(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = atan((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = atan(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atan2_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int == 0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = atan2((double)p1.val_int, (double)p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		if ((p1.val_int == 0) && (p2.val_real == 0.0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = atan2((double)p1.val_int, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = atan2(p1.val_real, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = atan2(p1.val_real, (double)p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_sinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = sinh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = sinh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_cosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = cosh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = cosh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_tanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = tanh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = tanh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_asinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = asinh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = asinh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_acosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = acosh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = acosh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_atanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_real = atanh((double)p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = atanh(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_copysign_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum = p1;

		if (p2.val_int < 0)
			q->accum.val_int = -llabs((long long)p1.val_int);

		q->accum.tag = TAG_INT;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		q->accum = p1;

		if (p2.val_real < 0.0)
			q->accum.val_int = -llabs((long long)p1.val_int);

		q->accum.tag = TAG_INT;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = copysign(p1.val_real, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		q->accum.val_real = copysign(p1.val_real, p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_pow_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		if ((mp_int_compare_zero(&p1.val_bigint->ival) == 0) && (p2.val_int < 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = pow(BIGINT_TO_DOUBLE(&p1.val_bigint->ival), (double)p2.val_int);
		q->accum.tag = TAG_REAL;
		return pl_success;
	}

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int == 0) && (p2.val_int < 0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_int, (double)p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		if ((p1.val_int == 0) && (p2.val_real < 0.0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_int, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_real < 0.0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = pow(p1.val_real, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_int < 0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_real = pow(p1.val_real, p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_powi_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		if (is_negative(&p2))
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		if (mp_int_expt_full(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		if (p2.val_int < 0)
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		if (p2.val_int > (INT32_MAX/2))
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_expt(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		if (is_negative(&p2))
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);

		if (mp_int_expt_full(&tmp, &p2.val_bigint->ival, &q->tmp_ival) != MP_OK) {
			mp_int_clear(&tmp);
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");
		}

		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int != 1) && (p2.val_int < 0))
			return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");

		if (p2.val_int < 0) {
			q->accum.val_int = pow(p1.val_int, p2.val_int);
			q->accum.tag = TAG_INT;
			return pl_success;
		}

		if (p2.val_int > (INT32_MAX/2))
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_expt_value(p1.val_int, p2.val_int, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory");

		if (errno == ENOMEM)
			return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_compare_value(&q->tmp_ival, MP_SMALL_MAX) > 0) {
			SET_ACCUM();
			return pl_success;
		}

		q->accum.val_int = pow(p1.val_int, p2.val_int);
		q->accum.tag = TAG_INT;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		q->accum.val_real = pow(p1.val_int, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = pow(p1.val_real, p2.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		q->accum.val_real = pow(p1.val_real, p2.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_divide_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		q->accum.val_real = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real /= d;
		q->accum.tag = TAG_REAL;
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		q->accum.val_real = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

		if (isinf(q->accum.val_real))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real /= p2.val_int;
		q->accum.tag = TAG_REAL;
	} else if (is_bigint(&p1) && is_real(&p2)) {
		if (p2.val_real == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = BIGINT_TO_DOUBLE(&p1.val_bigint->ival) / p2.val_real;
		q->accum.tag = TAG_REAL;
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		q->accum.val_real = p1.val_int;
		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real /= d;
		q->accum.tag = TAG_REAL;
	} else if (is_bigint(&p2) && is_real(&p1)) {
		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = p1.val_real / d;
		q->accum.tag = TAG_REAL;
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = (double)p1.val_int / p2.val_int;
		q->accum.tag = TAG_REAL;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		if (p2.val_real == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = (double)p1.val_int / p2.val_real;
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if (p2.val_real == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = p1.val_real / p2.val_real;
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_real = p1.val_real / p2.val_int;
		q->accum.tag = TAG_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_divint_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (is_bigint(&p2) && mp_int_compare_zero(&p2.val_bigint->ival) == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		if (is_smallint(&p2) && get_smallint(&p2) == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		DO_OP2(/, divx, p1, p2);
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_mod_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);

		if (mp_int_compare_zero(&p2.val_bigint->ival))
			mp_int_neg(&q->tmp_ival, &q->tmp_ival);

		if (mp_int_compare_zero(&p1.val_bigint->ival))
			mp_int_neg(&q->tmp_ival, &q->tmp_ival);

		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mp_small n;
		mp_int_mod_value(&p1.val_bigint->ival, p2.val_int, &n);
		q->accum.val_int = n;
		q->accum.tag = TAG_INT;
	} else if (is_smallint(&p1) && is_bigint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mod(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_int = p1.val_int % p2.val_int;

		if (p2.val_int < 0)
			q->accum.val_int *= -1;

		if (p1.val_int < 0)
			q->accum.val_int *= -1;

		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_div_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_sub(&p1.val_bigint->ival, &q->tmp_ival, &q->tmp_ival);
		mp_int_div(&q->tmp_ival, &p2.val_bigint->ival, &q->tmp_ival, NULL);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_mod(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_sub(&p1.val_bigint->ival, &q->tmp_ival, &q->tmp_ival);
		mp_int_div(&q->tmp_ival, &tmp, &q->tmp_ival, NULL);
		SET_ACCUM();
		mp_int_clear(&tmp);
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mod(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_sub(&tmp, &q->tmp_ival, &q->tmp_ival);
		mp_int_div(&q->tmp_ival, &p2.val_bigint->ival, &q->tmp_ival, NULL);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		if (fn_iso_mod_2(q) != pl_success)
			return pl_failure;

		q->accum.val_int = (p1.val_int - q->accum.val_int) / p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_rem_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_mod(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_bigint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mod(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_int = p1.val_int % p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_max_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1)) {
		if (is_bigint(&p2)) {
			if (mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) >= 0)
				mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
			else
				mp_int_copy(&p2.val_bigint->ival, &q->tmp_ival);
		} else if (is_smallint(&p2)) {
			if (mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) >= 0)
				mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
			else {
				mp_int_set_value(&q->tmp_ival, p2.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_bigint(&p2)) {
		if (is_smallint(&p1)) {
			if (mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) >= 0)
				mp_int_copy(&p2.val_bigint->ival, &q->tmp_ival);
			else {
				mp_int_set_value(&q->tmp_ival, p1.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
 	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p1.val_int >= p2.val_int)
			q->accum = p1;
		else
			q->accum = p2;

		q->accum.tag = TAG_INT;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		double f1 = (double)p1.val_int;

		if (f1 > p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_smallint(&p2) && is_real(&p1)) {
		double f2 = (double)p2.val_int;

		if (f2 > p1.val_real)
			q->accum = p2;
		else
			q->accum = p1;
	} else if (is_real(&p1) && is_real(&p2)) {
		if (p1.val_real > p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_smallint(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_min_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1)) {
		if (is_bigint(&p2)) {
			if (mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) <= 0)
				mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
			else
				mp_int_copy(&p2.val_bigint->ival, &q->tmp_ival);
		} else if (is_smallint(&p2)) {
			if (mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) <= 0)
				mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
			else {
				mp_int_set_value(&q->tmp_ival, p2.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_bigint(&p2)) {
		if (is_smallint(&p1)) {
			if (mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) <= 0)
				mp_int_copy(&p2.val_bigint->ival, &q->tmp_ival);
			else {
				mp_int_set_value(&q->tmp_ival, p1.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p1.val_int <= p2.val_int)
			q->accum = p1;
		else
			q->accum = p2;

		q->accum.tag = TAG_INT;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		double f1 = (double)p1.val_int;

		if (f1 < p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_smallint(&p2) && is_real(&p1)) {
		double f2 = (double)p2.val_int;

		if (f2 < p1.val_real)
			q->accum = p2;
		else
			q->accum = p1;
	} else if (is_real(&p1) && is_real(&p2)) {
		if (p1.val_real < p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_smallint(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_xor_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_xor(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_xor(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_xor(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int ^ p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_or_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_or(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_or(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_or(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int | p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_and_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_and(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_and(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_and(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int & p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_shl_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
		mp_int_mul_pow2(&q->tmp_ival, p2.val_int, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int << p2.val_int;

		if ((q->accum.val_int >= 0) && (p2.val_int < 64)) {
			q->accum.tag = TAG_INT;
			return pl_success;
		}

		mp_int_init_value(&q->tmp_ival, p1.val_int);
		mp_int_mul_pow2(&q->tmp_ival, p2.val_int, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_shr_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		int n = p2.val_int;
		mp_int_copy(&p1.val_bigint->ival, &q->tmp_ival);
		mp_int_div_pow2(&q->tmp_ival, n, &q->tmp_ival, NULL);
		SET_ACCUM();
	} if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int >> p2.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_neg_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_int = ~p1.val_int;
		q->accum.tag = TAG_INT;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_seq_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res == 0 || res == ERR_CYCLE_CMP;
}

static USE_RESULT pl_status fn_iso_sne_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != 0 && res != ERR_CYCLE_CMP;
}

static USE_RESULT pl_status fn_iso_slt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != ERR_CYCLE_CMP && res < 0;
}

static USE_RESULT pl_status fn_iso_sle_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != ERR_CYCLE_CMP && res <= 0;
}

static USE_RESULT pl_status fn_iso_sgt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != ERR_CYCLE_CMP && res > 0;
}

static USE_RESULT pl_status fn_iso_sge_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != ERR_CYCLE_CMP && res >= 0;
}

static USE_RESULT pl_status fn_iso_neq_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) == 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) == 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) == 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int == p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int == p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real == p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real == p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nne_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) != 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) != 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) != 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int != p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int != p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real != p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real != p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nge_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) >= 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) >= 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) < 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int >= p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int >= p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real >= p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real >= p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_ngt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) > 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) > 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) <= 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int > p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int > p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real > p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real > p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nle_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) <= 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) <= 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) > 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int <= p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int <= p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real <= p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real <= p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nlt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) < 0;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) < 0;
	else if (is_bigint(&p2) && is_smallint(&p1))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) >= 0;
	else if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int < p2.val_int;
	else if (is_smallint(&p1) && is_real(&p2))
		return p1.val_int < p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real < p2.val_real;
	else if (is_real(&p1) && is_smallint(&p2))
		return p1.val_real < p2.val_int;

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_log_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_variable(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (! is_integer(&p1) && ! is_real(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	} else if (! is_integer(&p2) && ! is_real(&p2)){
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_smallint(&p1)) {
		if (p1.val_int == 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_int < 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		}
	} else if (is_real(&p1)) {
		if (p1.val_real == 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_real < 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		}
	}

	if (is_smallint(&p2)) {
		if (p2.val_int == 0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p2.val_int < 0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");
		}
	} else if (is_real(&p2)) {
		if (p2.val_real == 0.0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p2.val_real < 0.0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");
		}
	}

	if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_real = log(p2.val_int) / log(p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_smallint(&p1) && is_real(&p2)) {
		q->accum.val_real = log(p2.val_real) / log(p1.val_int);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_smallint(&p2)) {
		q->accum.val_real = log(p2.val_int) / log(p1.val_real);
		q->accum.tag = TAG_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = log(p2.val_real) / log(p1.val_real);
		q->accum.tag = TAG_REAL;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_log10_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_variable(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else 	if (is_smallint(&p1)) {
		if (p1.val_int == 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_int < 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		} else {
			q->accum.val_real = log10(p1.val_int);
			q->accum.tag = TAG_REAL;
		}
	} else if (is_real(&p1)) {
		if (p1.val_real == 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_real < 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		} else {
			q->accum.val_real = log10(p1.val_real);
			q->accum.tag = TAG_REAL;
		}
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return pl_success;
}

static pl_uint_t g_seed = 0;
#define random_M 0x7FFFFFFFL

static double rnd(void)
{
	g_seed = ((g_seed * 2743) + 5923) & random_M;
	return((double)g_seed / (double)random_M);
}

static USE_RESULT pl_status fn_set_seed_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	g_seed = p1->val_int;
	return pl_success;
}

static USE_RESULT pl_status fn_get_seed_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, g_seed);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_random_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);

	if (!is_smallint(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (!is_smallint(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	cell tmp;
	pl_int_t r = rnd() * ((int64_t)RAND_MAX+1);
	make_int(&tmp, get_smallint(p1) + (r % get_smallint(p2)));
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static USE_RESULT pl_status fn_random_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_real(&tmp, rnd());
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_random_integer_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_INT;
	q->accum.val_int = rnd() * ((int64_t)RAND_MAX+1);
	return pl_success;
}

static USE_RESULT pl_status fn_random_float_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_REAL;
	q->accum.val_real = rnd();
	return pl_success;
}

static USE_RESULT pl_status fn_rand_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_INT;
	q->accum.val_int = rnd() * ((int64_t)RAND_MAX+1);
	return pl_success;
}

static USE_RESULT pl_status fn_rand_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, rnd() * ((int64_t)RAND_MAX+1));
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static pl_int_t gcd(pl_int_t num, pl_int_t remainder)
{
	if (remainder == 0)
		return num;

	return gcd(remainder, num % remainder);
}

static USE_RESULT pl_status fn_gcd_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (is_bigint(&p1) && is_bigint(&p2)) {
			mp_int_gcd(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
			SET_ACCUM();
		} else if (is_bigint(&p1)) {
			mpz_t tmp;
			mp_int_init_value(&tmp, p2.val_int);
			mp_int_gcd(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
			mp_int_clear(&tmp);
			SET_ACCUM();
		} else if (is_bigint(&p2)) {
			mpz_t tmp;
			mp_int_init_value(&tmp, p1.val_int);
			mp_int_gcd(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
			mp_int_clear(&tmp);
			SET_ACCUM();
		} else {
			q->accum.val_int = gcd(p1.val_int, p2.val_int);
			q->accum.tag = TAG_INT;
		}
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return pl_success;
}

const struct builtins g_functions[] =
{
	// Predicate...

	{"=:=", 2, fn_iso_neq_2, NULL, false},
	{"=\\=", 2, fn_iso_nne_2, NULL, false},
	{">", 2, fn_iso_ngt_2, NULL, false},
	{">=", 2, fn_iso_nge_2, NULL, false},
	{"=<", 2, fn_iso_nle_2, NULL, false},
	{"<", 2, fn_iso_nlt_2, NULL, false},

	{"==", 2, fn_iso_seq_2, NULL, false},
	{"\\==", 2, fn_iso_sne_2, NULL, false},
	{"@>", 2, fn_iso_sgt_2, NULL, false},
	{"@>=", 2, fn_iso_sge_2, NULL, false},
	{"@=<", 2, fn_iso_sle_2, NULL, false},
	{"@<", 2, fn_iso_slt_2, NULL, false},

	{"is", 2, fn_iso_is_2, NULL, false},
	{"return", 1, fn_return_1, NULL, false},
	{"float", 1, fn_iso_float_1, NULL, false},
	{"integer", 1, fn_iso_integer_1, NULL, false},
	{"srandom", 1, fn_set_seed_1, "+integer", false},
	{"set_seed", 1, fn_set_seed_1, "+integer", false},
	{"get_seed", 1, fn_get_seed_1, "-integer", false},
	{"rand", 1, fn_rand_1, "?integer", false},
	{"random", 1, fn_random_1, "?integer", false},
	{"random_between", 3, fn_random_between_3, "?integer,?integer,-integer", false},

	// Functions...

	{"+", 1, fn_iso_positive_1, NULL, true},
	{"-", 1, fn_iso_negative_1, NULL, true},
	{"abs", 1, fn_iso_abs_1, NULL, true},
	{"sign", 1, fn_iso_sign_1, NULL, true},
	{"epsilon", 0, fn_iso_epsilon_0, NULL, true},
	{"pi", 0, fn_iso_pi_0, NULL, true},
	{"e", 0, fn_iso_e_0, NULL, true},
	{"+", 2, fn_iso_add_2, NULL, true},
	{"-", 2, fn_iso_sub_2, NULL, true},
	{"*", 2, fn_iso_mul_2, NULL, true},
	{"/", 2, fn_iso_divide_2, NULL, true},
	{"//", 2, fn_iso_divint_2, NULL, true},
	{"div", 2, fn_iso_div_2, NULL, true},
	{"mod", 2, fn_iso_mod_2, NULL, true},
	{"rem", 2, fn_iso_rem_2, NULL, true},
	{"max", 2, fn_iso_max_2, NULL, true},
	{"min", 2, fn_iso_min_2, NULL, true},
	{"xor", 2, fn_iso_xor_2, NULL, true},
	{"/\\", 2, fn_iso_and_2, NULL, true},
	{"\\/", 2, fn_iso_or_2, NULL, true},
	{"<<", 2, fn_iso_shl_2, NULL, true},
	{">>", 2, fn_iso_shr_2, NULL, true},
	{"\\", 1, fn_iso_neg_1, NULL, true},
	{"**", 2, fn_iso_pow_2, NULL, true},
	{"^", 2, fn_iso_powi_2, NULL, true},
	{"exp", 1, fn_iso_exp_1, NULL, true},
	{"sqrt", 1, fn_iso_sqrt_1, NULL, true},
	{"log", 1, fn_iso_log_1, NULL, true},

	{"sin", 1, fn_iso_sin_1, NULL, true},
	{"cos", 1, fn_iso_cos_1, NULL, true},
	{"tan", 1, fn_iso_tan_1, NULL, true},
	{"asin", 1, fn_iso_asin_1, NULL, true},
	{"acos", 1, fn_iso_acos_1, NULL, true},
	{"atan", 1, fn_iso_atan_1, NULL, true},

	{"sinh", 1, fn_sinh_1, NULL, true},
	{"cosh", 1, fn_cosh_1, NULL, true},
	{"tanh", 1, fn_tanh_1, NULL, true},
	{"asinh", 1, fn_asinh_1, NULL, true},
	{"acosh", 1, fn_acosh_1, NULL, true},
	{"atanh", 1, fn_atanh_1, NULL, true},

	{"popcount", 1, fn_popcount_1, NULL, true},
	{"lsb", 1, fn_lsb_1, NULL, true},
	{"msb", 1, fn_msb_1, NULL, true},
	{"atan2", 2, fn_iso_atan2_2, NULL, true},
	{"copysign", 2, fn_iso_copysign_2, NULL, true},
	{"truncate", 1, fn_iso_truncate_1, NULL, true},
	{"round", 1, fn_iso_round_1, NULL, true},
	{"ceiling", 1, fn_iso_ceiling_1, NULL, true},
	{"floor", 1, fn_iso_floor_1, NULL, true},
	{"float_integer_part", 1, fn_iso_float_integer_part_1, NULL, true},
	{"float_fractional_part", 1, fn_iso_float_fractional_part_1, NULL, true},
	{"log", 2, fn_log_2, "+number,+number", true},
	{"log10", 1, fn_log10_1, "+integer", true},
	{"random_integer", 0, fn_random_integer_0, "?integer", true},
	{"random_float", 0, fn_random_float_0, NULL, true},
	{"rand", 0, fn_rand_0, NULL, true},
	{"gcd", 2, fn_gcd_2, "?integer,?integer", true},

	{0}
};

