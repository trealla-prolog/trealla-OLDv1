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
#include "builtins.h"

#define CHECK_CALC()							\
	if (!q->calc) {								\
		if (q->st.m->flag.unknown == 0)				\
			return false;						\
		else									\
			return throw_error(q, q->st.curr_cell, "existence_error", "procedure");	\
	}

void do_calc_(query *q, cell *c, idx_t c_ctx)
{
	cell *save = q->st.curr_cell;
	idx_t save_ctx = q->st.curr_frame;
	bool save_calc = q->calc;
	q->st.curr_cell = c;
	q->st.curr_frame = c_ctx;
	q->calc = true;

	if (is_builtin(c) && c->fn)
		c->fn(q);

	q->calc = save_calc;

	if (!q->did_throw) {
		q->st.curr_cell = save;
		q->st.curr_frame = save_ctx;
	}
}

pl_status call_function(query *q, cell *c, __attribute__((unused)) idx_t c_ctx)
{
	return throw_error(q, c, "type_error", "evaluable");
}

static int_t gcd(int_t num, int_t remainder)
{
	if (remainder == 0)
		return num;

	return gcd(remainder, num % remainder);
}

void do_reduce(cell *n)
{
	int_t r = 0;

	if (n->val_den > n->val_num)
		r = gcd(n->val_den, n->val_num);
	else if (n->val_den < n->val_num)
		r = gcd(n->val_num, n->val_den);
	else
		r = gcd(n->val_num, n->val_den);

	n->val_num /= r;
	n->val_den /= r;

	if (n->val_den < 0) {
		n->val_num = -n->val_num;
		n->val_den = -n->val_den;
	}
}

#define reduce(c) if ((c)->val_den != 1) do_reduce(c)

static USE_RESULT pl_status fn_iso_is_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p2 = calc(q, p2_tmp);
	p2.nbr_cells = 1;

	if (is_variable(p1) && is_rational(&p2)) {
		reduce(&p2);
		set_var(q, p1, p1_ctx, &p2, q->st.curr_frame);
		return pl_success;
	}

	if (is_variable(p1) && is_number(&p2)) {
		set_var(q, p1, p1_ctx, &p2, q->st.curr_frame);
		return pl_success;
	}

	if (is_integer(p1) && is_integer(&p2))
		return (p1->val_num == p2.val_num);

	if (is_rational(p1) && is_rational(&p2)) {
		reduce(p1); reduce(&p2);
		return (p1->val_num == p2.val_num) && (p1->val_den == p2.val_den);
	}

	if (is_real(p1) && is_real(&p2))
		return p1->val_real == p2.val_real;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(p1), "nan"))
		return is_real(&p2)? isnan(p2.val_real) : 0;

	if (is_atom(p1) && is_number(&p2) && !strcmp(GET_STR(p1), "inf"))
		return is_real(&p2) ? isinf(p2.val_real) : 0;

	return pl_failure;
}

static USE_RESULT pl_status fn_iso_float_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if (is_real(&p1)) {
			q->accum.val_real = p1.val_real;
			q->accum.val_type = TYPE_REAL;
			return pl_success;
		}

		if (is_integer(&p1)) {
			q->accum.val_real = (double)p1.val_num;
			q->accum.val_type = TYPE_REAL;
			return pl_success;
		}

		if (is_rational(&p1)) {
			if (p1.val_num != 0) {
				if (p1.val_den == 0)
					return throw_error(q, &p1, "evaluation_error", "undefined");

				q->accum.val_real = (double)p1.val_num / p1.val_den;
			} else
				q->accum.val_real = 0.0;

			q->accum.val_type = TYPE_REAL;
			return pl_success;
		}

		return throw_error(q, &p1, "type_error", "integer_or_float");
	}

	return is_real(p1_tmp);
}

static USE_RESULT pl_status fn_iso_integer_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if (is_real(&p1)) {
			q->accum.val_num = (int_t)p1.val_real;
			q->accum.val_den = 1;
			q->accum.val_type = TYPE_RATIONAL;
			return pl_success;
		}

		if (is_rational(&p1)) {
			q->accum.val_num = p1.val_num;
			q->accum.val_den = p1.val_den;
			q->accum.val_type = TYPE_RATIONAL;
			return pl_success;
		}

		return throw_error(q, &p1, "type_error", "integer_or_float");
	}

	return is_integer(p1_tmp);
}

static USE_RESULT pl_status fn_iso_abs_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_rational(&p1))
		q->accum.val_num = llabs((long long)p1.val_num);
	else if (is_real(&p1))
		q->accum.val_real = fabs(p1.val_real);
	else
		return throw_error(q, &p1, "type_error", "number");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sign_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_rational(&p1))
		q->accum.val_num = p1.val_num < 0 ? -1 : p1.val_num > 0  ? 1 : 0;
	else if (is_real(&p1))
		q->accum.val_real = p1.val_real < 0 ? -1 : p1.val_real > 0  ? 1 : 0;
	else
		return throw_error(q, &p1, "type_error", "number");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_positive_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum = p1;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_negative_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);
	q->accum.val_type = p1.val_type;

	if (is_rational(&p1))
		q->accum.val_num = -p1.val_num;
	else if (is_real(&p1))
		q->accum.val_real = -p1.val_real;
	else if (is_variable(&p1))
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	else
		return throw_error(q, &p1, "type_error", "number");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_epsilon_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = DBL_EPSILON;
	q->accum.val_type = TYPE_REAL;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_pi_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = M_PI;
	q->accum.val_type = TYPE_REAL;
	return pl_success;
}

static USE_RESULT pl_status fn_iso_e_0(query *q)
{
	CHECK_CALC();
	q->accum.val_real = M_E;
	q->accum.val_type = TYPE_REAL;
	return pl_success;
}

USE_RESULT pl_status fn_iso_add_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num + p2.val_num;

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = (int64_t)p1.val_num + p2.val_num;

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = p1.val_num + p2.val_num;
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num += p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_integer(&p1) && is_real(&p2)) {
		q->accum.val_real = (double)p1.val_num + p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = p1.val_real + p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		q->accum.val_real = p1.val_real + p2.val_num;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sub_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num - p2.val_num;

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = (int64_t)p1.val_num - p2.val_num;

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = p1.val_num - p2.val_num;
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num -= p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_integer(&p1) && is_real(&p2)) {
		q->accum.val_real = (double)p1.val_num - p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = p1.val_real - p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		q->accum.val_real = p1.val_real - p2.val_num;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_mul_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if ((is_integer(&p1)) && is_integer(&p2)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = (__int128_t)p1.val_num * p2.val_num;

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = (int64_t)p1.val_num * p2.val_num;

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = p1.val_num * p2.val_num;
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		q->accum.val_num = p1.val_num * p2.val_den;
		q->accum.val_num *= p2.val_num * p1.val_den;
		q->accum.val_den = p1.val_den * p2.val_den;
		q->accum.val_den *= p1.val_den * p2.val_den;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_integer(&p1) && is_real(&p2)) {
		q->accum.val_real = (double)p1.val_num * p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = p1.val_real * p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		q->accum.val_real = p1.val_real * p2.val_num;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_exp_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = exp((double)p1.val_num / p1.val_den);

		if (isinf(q->accum.val_real))
			return throw_error(q, &p1, "evaluation_error", "float_overflow");

		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = exp(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sqrt_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p1.val_num < 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = sqrt((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		if (p1.val_real == -1)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = sqrt(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_log_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_num <= 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = log((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		if (p1.val_real <= 0.0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = log(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_truncate_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = p1.val_real;

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = p1.val_real;

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = (int_t)p1.val_real;
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_round_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = rint(p1.val_real);

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
			double f = fabs(p1.val_real);

			if ((f - floor(f)) > 0.5)
				fesetround(FE_TONEAREST);
			else
				fesetround(FE_UPWARD);

#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = nearbyintf(p1.val_real);

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = nearbyintf(p1.val_real);
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_ceiling_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = ceil(p1.val_real);

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = ceil(p1.val_real);

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = (int_t)ceil(p1.val_real);
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_float_integer_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_real = (int_t)p1.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_float_fractional_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
		q->accum.val_real = p1.val_real - (int_t)p1.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_floor_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_real(&p1)) {
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		__int128_t tmp = floor(p1.val_real);

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = floor(p1.val_real);

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				q->accum.val_num = (int_t)floor(p1.val_real);
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "float");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_sin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = sin((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = sin(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_cos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = cos((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = cos(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_tan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = tan((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = tan(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_asin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = asin((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = asin(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_acos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = acos((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = acos(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = atan((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = atan(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_atan2_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		if ((p1.val_num == 0) && (p2.val_num == 0))
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p2.val_den == 0)
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = atan2((double)p1.val_num / p1.val_den, (double)p2.val_num / p2.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		if ((p1.val_num == 0) && (p2.val_real == 0.0))
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = atan2((double)p1.val_num / p1.val_den, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_num == 0))
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = atan2(p1.val_real, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_rational(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_num == 0))
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p2.val_den == 0)
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = atan2(p1.val_real, (double)p2.val_num / p2.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_sinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = sinh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = sinh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_cosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = cosh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = cosh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_tanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = tanh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = tanh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "float_overflow");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_asinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = asinh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = asinh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_acosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = acosh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = acosh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_atanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_rational(&p1)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = atanh((double)p1.val_num / p1.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1)) {
		q->accum.val_real = atanh(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isinf(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_copysign_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		q->accum = p1;

		if (p2.val_num < 0)
			q->accum.val_num = -llabs((long long)p1.val_num);

		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		q->accum = p1;

		if (p2.val_real < 0.0)
			q->accum.val_num = -llabs((long long)p1.val_num);

		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = copysign(p1.val_real, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_rational(&p2)) {
		q->accum.val_real = copysign(p1.val_real, p2.val_num);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_pow_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p2.val_den == 0)
			return throw_error(q, &p2, "evaluation_error", "undefined");

		if ((p1.val_num == 0) && (p2.val_num < 0))
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_num / p1.val_den, (double)p2.val_num / p2.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if ((p1.val_num == 0) && (p2.val_real < 0.0))
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_num / p1.val_den, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_real < 0.0))
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = pow(p1.val_real, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		if ((p1.val_real == 0.0) && (p2.val_num < 0))
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = pow(p1.val_real, p2.val_num);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_powi_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

#if 0
	if (is_integer(&p1)) {
		if (p1.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");
	}
#endif

	if (is_integer(&p1) && is_integer(&p2)) {
		if ((p1.val_num != 1) && (p2.val_num < 0)) {
			return throw_error(q, &p1, "type_error", "float");
		}

#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
		double res = pow(p1.val_num,p2.val_num);

		if (res > (double)MY_INT64_MAX)
			return throw_error(q, &p1, "evaluation_error", "int_overflow");

		__int128_t tmp = pow(p1.val_num,p2.val_num);

		if ((tmp > MY_INT64_MAX) || (tmp < MY_INT64_MIN)) {
			return throw_error(q, &p1, "evaluation_error", "int_overflow");
		} else {
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
			int64_t tmp = pow(p1.val_num,p2.val_num);

			if ((tmp > MY_INT32_MAX) || (tmp < MY_INT32_MIN)) {
				return throw_error(q, &p1, "evaluation_error", "int_overflow");
			} else {
#endif
				double res = pow(p1.val_num,p2.val_num);

				if (res > (double)MY_INT64_MAX)
					return throw_error(q, &p1, "evaluation_error", "int_overflow");

				q->accum.val_num = (int_t)res;
				q->accum.val_type = TYPE_RATIONAL;
#if defined(__SIZEOF_INT128__) && !USE_INT128 && CHECK_OVERFLOW
			}
#elif defined(__SIZEOF_INT64__) && USE_INT32 && CHECK_OVERFLOW
		}
#endif
	} else if (is_rational(&p1) && is_rational(&p2)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		if (p2.val_den == 0)
			return throw_error(q, &p2, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_num / p1.val_den, (double)p2.val_num / p2.val_den);
		q->accum.val_type = TYPE_REAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		if (p1.val_den == 0)
			return throw_error(q, &p1, "evaluation_error", "undefined");

		q->accum.val_real = pow((double)p1.val_num / p1.val_den, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = pow(p1.val_real, p2.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		q->accum.val_real = pow(p1.val_real, p2.val_num);
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_divide_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_real = (double)p1.val_num / p2.val_num;
		q->accum.val_type = TYPE_REAL;
	} else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		q->accum.val_num = p1.val_num;
		q->accum.val_den = p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_integer(&p1) && is_real(&p2)) {
		if (p2.val_real == 0.0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_real = (double)p1.val_num / p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		if (p2.val_real == 0.0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_real = p1.val_real / p2.val_real;
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_real = p1.val_real / p2.val_num;
		q->accum.val_type = TYPE_REAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	if (is_real(&q->accum) && isnan(q->accum.val_real))
		return throw_error(q, &p1, "evaluation_error", "undefined");

	return pl_success;
}

static USE_RESULT pl_status fn_iso_divint_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

#if USE_INT32
		if (p1.val_num == MY_INT32_MIN)
#else
		if (p1.val_num == MY_INT64_MIN)
#endif
			return throw_error(q, &p1, "evaluation_error", "int_overflow");

		q->accum.val_num = p1.val_num / p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_div_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_num = floor((double)p1.val_num / p2.val_num);
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_mod_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_num = (long long)(p1.val_num % p2.val_num);

		if (p2.val_num < 0)
			q->accum.val_num *= -1;

		if (p1.val_num < 0)
			q->accum.val_num *= -1;

		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_rem_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (p2.val_num == 0)
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");

		q->accum.val_num = (long long)(p1.val_num % p2.val_num);
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_max_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		cell s1 = {0}, s2 = {0};
		s1.val_num = p1.val_num * p2.val_den;
		s1.val_den = p1.val_den * p2.val_den;
		s2.val_num = p2.val_num * p1.val_den;
		s2.val_den = p2.val_den * p1.val_den;
		if (s1.val_num >= s2.val_num) q->accum = s1;
		else q->accum = s2;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		double f1 = (double)p1.val_num;
		if (p1.val_den) f1 /= p1.val_den;

		if (f1 > p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_rational(&p2) && is_real(&p1)) {
		double f2 = (double)p2.val_num;
		if (p2.val_den) f2 /= p2.val_den;

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
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_rational(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_min_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		cell s1 = {0}, s2 = {0};
		s1.val_num = p1.val_num * p2.val_den;
		s1.val_den = p1.val_den * p2.val_den;
		s2.val_num = p2.val_num * p1.val_den;
		s2.val_den = p2.val_den * p1.val_den;
		if (s1.val_num <= s2.val_num) q->accum = s1;
		else q->accum = s2;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_rational(&p1) && is_real(&p2)) {
		double f1 = (double)p1.val_num;
		if (p1.val_den) f1 /= p1.val_den;

		if (f1 < p2.val_real)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_rational(&p2) && is_real(&p1)) {
		double f2 = (double)p2.val_num;
		if (p1.val_den) f2 /= p1.val_den;

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
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_rational(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_rational(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_xor_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num ^ p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_and_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num & p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_or_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num | p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_shl_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num << p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_shr_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = p1.val_num >> p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

static USE_RESULT pl_status fn_iso_neg_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_integer(&p1)) {
		q->accum.val_num = ~p1.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, "type_error", "integer");
	}

	return pl_success;
}

int compare(query *q, cell *p1, idx_t p1_ctx, cell *p2, idx_t p2_ctx, unsigned depth)
{
	if (depth == MAX_DEPTH) {
		q->cycle_error = true;
		return ERR_CYCLE_CMP;
	}

	if (is_variable(p1)) {
		if (is_variable(p2)) {
			frame *g1 = GET_FRAME(p1_ctx);
			frame *g2 = GET_FRAME(p2_ctx);
			idx_t p1_slot = GET_SLOT(g1,p1->var_nbr) - q->slots;
			idx_t p2_slot = GET_SLOT(g2,p2->var_nbr) - q->slots;
			return p1_slot < p2_slot ? -1 : p1_slot > p2_slot ? 1 : 0;
		}

		return -1;
	}

	if (is_rational(p1)) {
		if (is_rational(p2)) {
			cell tmp1 = *p1, tmp2 = *p2;
			tmp1.val_num *= tmp2.val_den;
			tmp2.val_num *= tmp1.val_den;
			return tmp1.val_num < tmp2.val_num ? -1 : tmp1.val_num > tmp2.val_num ? 1 : 0;
		}

		if (is_real(p2))
			return 1;

		if (is_variable(p2))
			return 1;

		return -1;
	}

	if (is_real(p1)) {
		if (is_real(p2))
			return p1->val_real < p2->val_real ? -1 : p1->val_real > p2->val_real ? 1 : 0;

		if (is_variable(p2))
			return 1;

		return -1;
	}

	if (is_iso_atom(p1)) {
		if (is_iso_atom(p2))
			return slicecmp(GET_STR(p1), LEN_STR(p1), GET_STR(p2), LEN_STR(p2));

		if (is_variable(p2) || is_number(p2))
			return 1;

		return -1;
	}

	assert(p1->val_type && p2->val_type);
	assert((p1->val_type != TYPE_END) && (p2->val_type != TYPE_END));

	if (p1->arity < p2->arity)
		return -1;

	if (p1->arity > p2->arity)
		return 1;

	if (is_list(p1) && is_list(p2)) {
		LIST_HANDLER(p1);
		LIST_HANDLER(p2);

		while (is_list(p1) && is_list(p2)) {
			cell *h1 = LIST_HEAD(p1);
			h1 = deref(q, h1, p1_ctx);
			idx_t tmp1_ctx = q->latest_ctx;
			cell *h2 = LIST_HEAD(p2);
			h2 = deref(q, h2, p2_ctx);
			idx_t tmp2_ctx = q->latest_ctx;

			int val = compare(q, h1, tmp1_ctx, h2, tmp2_ctx, depth+1);
			if (val) return val;

			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (is_list(p1))
			return 1;

		if (is_list(p2))
			return -1;

		int val = compare(q, p1, p1_ctx, p2, p2_ctx, depth+1);
		if (val) return val;

		return 0;
	}

	int val = slicecmp(GET_STR(p1), LEN_STR(p1), GET_STR(p2), LEN_STR(p2));
	if (val) return val>0?1:-1;

	int arity = p1->arity;
	p1 = p1 + 1;
	p2 = p2 + 1;

	while (arity--) {
		cell *h1 = deref(q, p1, p1_ctx);
		idx_t tmp1_ctx = q->latest_ctx;
		cell *h2 = deref(q, p2, p2_ctx);
		idx_t tmp2_ctx = q->latest_ctx;

		int val = compare(q, h1, tmp1_ctx, h2, tmp2_ctx, depth+1);
		if (val) return val;

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return 0;
}

static USE_RESULT pl_status fn_iso_seq_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res == 0 || res == ERR_CYCLE_CMP;
}

static USE_RESULT pl_status fn_iso_sne_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res != 0 && res != ERR_CYCLE_CMP;
}

static USE_RESULT pl_status fn_iso_slt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res != ERR_CYCLE_CMP && res < 0;
}

static USE_RESULT pl_status fn_iso_sle_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res != ERR_CYCLE_CMP && res <= 0;
}

static USE_RESULT pl_status fn_iso_sgt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res != ERR_CYCLE_CMP && res > 0;
}

static USE_RESULT pl_status fn_iso_sge_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx, 0);
	return res != ERR_CYCLE_CMP && res >= 0;
}

static USE_RESULT pl_status fn_iso_neq_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num == p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num == p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num == p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real == p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real == p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nne_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num != p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num != p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num != p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real != p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real != p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nge_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num >= p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num >= p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num >= p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real >= p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real >= p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_ngt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num > p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num > p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num > p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real > p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real > p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nle_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num <= p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num <= p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num <= p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real <= p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real <= p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_iso_nlt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2))
		return p1.val_num < p2.val_num;
	else if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		return p1.val_num < p2.val_num;
	} else if (is_integer(&p1) && is_real(&p2))
		return p1.val_num < p2.val_real;
	else if (is_real(&p1) && is_real(&p2))
		return p1.val_real < p2.val_real;
	else if (is_real(&p1) && is_integer(&p2))
		return p1.val_real < p2.val_num;

	return throw_error(q, &p1, "type_error", "evaluable");
}

static USE_RESULT pl_status fn_log_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_variable(&p2)) {
		return throw_error(q, &p2, "instantiation_error", "not_sufficiently_instantiated");
	} else if (! is_integer(&p1) && ! is_real(&p1)) {
		return throw_error(q, &p1, "type_error", "evaluable");
	} else if (! is_integer(&p2) && ! is_real(&p2)){
		return throw_error(q, &p2, "type_error", "evaluable");
	}

	if (is_integer(&p1)) {
		if (p1.val_num == 0) {
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");
		} else if (p1.val_num < 0) {
			return throw_error(q, &p1, "evaluation_error", "undefined");
		}
	} else if (is_real(&p1)) {
		if (p1.val_real == 0.0) {
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");
		} else if (p1.val_real < 0.0) {
			return throw_error(q, &p1, "evaluation_error", "undefined");
		}
	}

	if (is_integer(&p2)) {
		if (p2.val_num == 0) {
			return throw_error(q, &p2, "evaluation_error", "zero_divisor");
		} else if (p2.val_num < 0) {
			return throw_error(q, &p2, "evaluation_error", "undefined");
		}
	} else if (is_real(&p2)) {
		if (p2.val_real == 0.0) {
			return throw_error(q, &p2, "evaluation_error", "zero_divisor");
		} else if (p2.val_real < 0.0) {
			return throw_error(q, &p2, "evaluation_error", "undefined");
		}
	}

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_real = log(p2.val_num) / log(p1.val_num);
		q->accum.val_type = TYPE_REAL;
	} else if (is_integer(&p1) && is_real(&p2)) {
		q->accum.val_real = log(p2.val_real) / log(p1.val_num);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_integer(&p2)) {
		q->accum.val_real = log(p2.val_num) / log(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	} else if (is_real(&p1) && is_real(&p2)) {
		q->accum.val_real = log(p2.val_real) / log(p1.val_real);
		q->accum.val_type = TYPE_REAL;
	}

	return pl_success;
}

static USE_RESULT pl_status fn_log10_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = calc(q, p1_tmp);

	if (is_variable(&p1)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else 	if (is_integer(&p1)) {
		if (p1.val_num == 0) {
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");
		} else if (p1.val_num < 0) {
			return throw_error(q, &p1, "evaluation_error", "undefined");
		} else {
			q->accum.val_real = log10(p1.val_num);
			q->accum.val_type = TYPE_REAL;
		}
	} else if (is_real(&p1)) {
		if (p1.val_real == 0.0) {
			return throw_error(q, &p1, "evaluation_error", "zero_divisor");
		} else if (p1.val_real < 0.0) {
			return throw_error(q, &p1, "evaluation_error", "undefined");
		} else {
			q->accum.val_real = log10(p1.val_real);
			q->accum.val_type = TYPE_REAL;
		}
	} else {
		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return pl_success;
}

static uint_t g_seed = 0;
#define random_M 0x7FFFFFFFL

static double rnd(void)
{
	g_seed = ((g_seed * 2743) + 5923) & random_M;
	return((double)g_seed / (double)random_M);
}

static USE_RESULT pl_status fn_set_seed_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	g_seed = p1->val_num;
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

static USE_RESULT pl_status fn_random_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (is_variable(p1_tmp)) {
		cell tmp;
		make_real(&tmp, rnd());
		set_var(q, p1_tmp, p1_tmp_ctx, &tmp, q->st.curr_frame);
		return pl_success;
	}

	CHECK_CALC();
	cell p1 = calc(q, p1_tmp);

	if (p1.val_num < 1)
		return throw_error(q, &p1, "domain_error", "positive_integer");

	q->accum.val_type = TYPE_RATIONAL;
	q->accum.val_num = llabs((long long)((int_t)(rnd() * RAND_MAX) % p1.val_num));
	q->accum.val_den = 1;
	return pl_success;
}

static USE_RESULT pl_status fn_rand_0(query *q)
{
	q->accum.val_type = TYPE_RATIONAL;
	q->accum.val_num = (int_t)rnd() * RAND_MAX;
	q->accum.val_den = 1;
	return pl_success;
}

static USE_RESULT pl_status fn_rand_1(query *q)
{
	GET_FIRST_ARG(p1,variable);
	cell tmp;
	make_int(&tmp, rnd() * RAND_MAX);
	set_var(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	return pl_success;
}

static USE_RESULT pl_status fn_rdiv_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_rational(&p1) && is_rational(&p2)) {
		p1.val_num *= p2.val_den;
		p2.val_num *= p1.val_den;
		q->accum.val_num = p1.val_num;
		q->accum.val_den = p2.val_num;
		q->accum.val_type = TYPE_RATIONAL;
	} else
		return throw_error(q, &p1, "type_error", "integer");

	return pl_success;
}

static void do_real_to_fraction(double v, double accuracy, int_t *num, int_t *den)
{
	ensure(accuracy > 0.0 && accuracy < 1.0);

	int_t sign = v < 0 ? -1 : 1;

	if (sign == -1) {
		v = fabs(v);
	}

	double maxError = sign == 0 ? accuracy : v * accuracy;
	int_t n = floor(v);
	v -= n;

	if (v < maxError) {
		*num = n * sign;
		*den = 1;
		return;
	}

	if ((1 - maxError) < v) {
		*num = (n+1) * sign;
		*den = 1;
		return;
	}

	double z = v;
	int_t previous_denominator = 0;
	int_t denominator = 1;
	int_t numerator;

	do {
		z = 1.0 / (z - (int_t) z);
		int_t tmp = denominator;
		denominator = denominator * (int_t)z + previous_denominator;
		previous_denominator = tmp;
		numerator = v * denominator;
	}
	 while (fabs(v-(double)numerator/denominator) > maxError && (z != (int_t)z));

	*num = (n * denominator + numerator) * sign;
	*den = denominator;
	return;
}

static USE_RESULT pl_status fn_rational_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->calc) {
		cell p1 = calc(q, p1_tmp);

		if (is_rational(&p1)) {
			reduce(&p1);
			q->accum.val_num = p1.val_num;
			q->accum.val_den = p1.val_den;
			q->accum.val_type = TYPE_RATIONAL;
			return pl_success;
		}

		if (is_real(&p1)) {
			do_real_to_fraction(p1.val_real, 0.00001, &q->accum.val_num, &q->accum.val_den);
			q->accum.val_type = TYPE_RATIONAL;
			return pl_success;
		}

		return throw_error(q, &p1, "type_error", "evaluable");
	}

	return is_rational(p1_tmp);
}

static USE_RESULT pl_status fn_gcd_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	cell p1 = calc(q, p1_tmp);
	cell p2 = calc(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		q->accum.val_num = gcd(p1.val_num, p2.val_num);
		q->accum.val_type = TYPE_RATIONAL;
	} else if (is_variable(&p1) || is_variable(&p2)) {
		return throw_error(q, &p1, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, "type_error", "integer");
	}

	return pl_success;
}

const struct builtins g_functions[] =
{
	{"=:=", 2, fn_iso_neq_2, NULL},
	{"=\\=", 2, fn_iso_nne_2, NULL},
	{">", 2, fn_iso_ngt_2, NULL},
	{">=", 2, fn_iso_nge_2, NULL},
	{"=<", 2, fn_iso_nle_2, NULL},
	{"<", 2, fn_iso_nlt_2, NULL},

	{"==", 2, fn_iso_seq_2, NULL},
	{"\\==", 2, fn_iso_sne_2, NULL},
	{"@>", 2, fn_iso_sgt_2, NULL},
	{"@>=", 2, fn_iso_sge_2, NULL},
	{"@=<", 2, fn_iso_sle_2, NULL},
	{"@<", 2, fn_iso_slt_2, NULL},

	{"+", 1, fn_iso_positive_1, NULL},
	{"-", 1, fn_iso_negative_1, NULL},
	{"abs", 1, fn_iso_abs_1, NULL},
	{"sign", 1, fn_iso_sign_1, NULL},
	{"epsilon", 0, fn_iso_epsilon_0, NULL},
	{"pi", 0, fn_iso_pi_0, NULL},
	{"e", 0, fn_iso_e_0, NULL},
	{"+", 2, fn_iso_add_2, NULL},
	{"-", 2, fn_iso_sub_2, NULL},
	{"*", 2, fn_iso_mul_2, NULL},
	{"/", 2, fn_iso_divide_2, NULL},
	{"//", 2, fn_iso_divint_2, NULL},
	{"div", 2, fn_iso_div_2, NULL},
	{"mod", 2, fn_iso_mod_2, NULL},
	{"rem", 2, fn_iso_rem_2, NULL},
	{"max", 2, fn_iso_max_2, NULL},
	{"min", 2, fn_iso_min_2, NULL},
	{"xor", 2, fn_iso_xor_2, NULL},
	{"/\\", 2, fn_iso_and_2, NULL},
	{"\\/", 2, fn_iso_or_2, NULL},
	{"<<", 2, fn_iso_shl_2, NULL},
	{">>", 2, fn_iso_shr_2, NULL},
	{"\\", 1, fn_iso_neg_1, NULL},
	{"**", 2, fn_iso_pow_2, NULL},
	{"^", 2, fn_iso_powi_2, NULL},
	{"exp", 1, fn_iso_exp_1, NULL},
	{"sqrt", 1, fn_iso_sqrt_1, NULL},
	{"log", 1, fn_iso_log_1, NULL},

	{"sin", 1, fn_iso_sin_1, NULL},
	{"cos", 1, fn_iso_cos_1, NULL},
	{"tan", 1, fn_iso_tan_1, NULL},
	{"asin", 1, fn_iso_asin_1, NULL},
	{"acos", 1, fn_iso_acos_1, NULL},
	{"atan", 1, fn_iso_atan_1, NULL},

	{"sinh", 1, fn_sinh_1, NULL},
	{"cosh", 1, fn_cosh_1, NULL},
	{"tanh", 1, fn_tanh_1, NULL},
	{"asinh", 1, fn_asinh_1, NULL},
	{"acosh", 1, fn_acosh_1, NULL},
	{"atanh", 1, fn_atanh_1, NULL},

	{"atan2", 2, fn_iso_atan2_2, NULL},
	{"copysign", 2, fn_iso_copysign_2, NULL},
	{"truncate", 1, fn_iso_truncate_1, NULL},
	{"round", 1, fn_iso_round_1, NULL},
	{"ceiling", 1, fn_iso_ceiling_1, NULL},
	{"floor", 1, fn_iso_floor_1, NULL},
	{"float_integer_part", 1, fn_iso_float_integer_part_1, NULL},
	{"float_fractional_part", 1, fn_iso_float_fractional_part_1, NULL},
	{"log", 2, fn_log_2, "+number,+number"},
	{"log10", 1, fn_log10_1, "+integer"},
	{"random", 1, fn_random_1, "?integer"},
	{"rand", 1, fn_rand_1, "?integer"},
	{"rand", 0, fn_rand_0, NULL},
	{"srandom", 1, fn_set_seed_1, "+integer"},
	{"set_seed", 1, fn_set_seed_1, "+integer"},
	{"get_seed", 1, fn_get_seed_1, "-integer"},
	{"float", 1, fn_iso_float_1, NULL},
	{"gcd", 2, fn_gcd_2, "?integer,?integer"},
	{"rdiv", 2, fn_rdiv_2, "+integer,+integer"},
	{"rational", 1, fn_rational_1, "+number"},
	{"rationalize", 1, fn_rational_1, "+number"},
	{"integer", 1, fn_iso_integer_1, NULL},
	{"is", 2, fn_iso_is_2, NULL},

	{0}
};

