#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "internal.h"
#include "query.h"

#if USE_FFI
#include <dlfcn.h>
#include <ffi.h>
#endif

union result_ {
	double d;
	int64_t i;
	char *s;
	void *p;
};

#if USE_FFI
USE_RESULT pl_status fn_sys_dlopen_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,variable);
	const char *filename = C_STR(q, p1);
	int flag = get_smallint(p2);
	void *handle = dlopen(filename, !flag ? RTLD_NOW : flag);
	if (!handle) return pl_failure;
	cell tmp;
	make_uint(&tmp, (uint64_t)handle);
	tmp.flags |= FLAG_INT_HANDLE | FLAG_HANDLE_DLL;
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

USE_RESULT pl_status fn_sys_dlsym_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,variable);
	uint64_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p2);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	void *ptr = dlsym((void*)handle, symbol);
	if (!ptr) return pl_failure;
	cell tmp;
	make_uint(&tmp, (uint64_t)ptr);
	tmp.flags |= FLAG_INT_HANDLE | FLAG_INT_OCTAL;
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

USE_RESULT pl_status fn_sys_dlclose_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	uint64_t handle = get_smalluint(p1);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	return dlclose((void*)handle) ? pl_failure : pl_success;
}

static USE_RESULT pl_status do_ffi_call(query *q, void *func, cell *p2, pl_idx_t p2_ctx, cell *p3, pl_idx_t p3_ctx)
{
	ffi_cif cif;
	ffi_type *arg_types[MAX_ARITY];
	ffi_status status;
	void *arg_values[MAX_ARITY];
	LIST_HANDLER(l);
	cell *l = p2;
	pl_idx_t l_ctx = p2_ctx;
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);

		if (is_compound(h) && (h->arity == 1)) {
			const char *src = C_STR(q, h);

			if (!strcmp(src, "int64"))
				arg_types[idx++] = &ffi_type_sint64;
			else if (!strcmp(src, "fp64"))
				arg_types[idx++] = &ffi_type_double;
			else if (!strcmp(src, "cstr"))
				arg_types[idx++] = &ffi_type_pointer;
			else
				arg_types[idx++] = &ffi_type_void;
		} else {
			if (is_smallint(h))
				arg_types[idx++] = &ffi_type_sint64;
			else if (is_float(h))
				arg_types[idx++] = &ffi_type_double;
			else if (is_atom(h))
				arg_types[idx++] = &ffi_type_pointer;
			else
				arg_types[idx++] = &ffi_type_void;
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	l = p2;
	l_ctx = p2_ctx;
	idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);

		if (is_compound(h) && (h->arity == 1)) {
			const char *src = C_STR(q, h);
			cell *c = h + 1;

			if (!strcmp(src, "int64"))
				arg_values[idx++] = (void*)get_smallint(c);
			else if (!strcmp(src, "fp64"))
				arg_values[idx++] = (void*)(uint64_t)get_float(c);
			else if (!strcmp(src, "cstr"))
				arg_values[idx++] = C_STR(q, c);
			else
				arg_values[idx++] = NULL;
		} else {
			if (is_smallint(h))
				arg_values[idx++] = &h->val_int;
			else if (is_float(h))
				arg_values[idx++] = &h->val_float;
			else if (is_atom(h))
				arg_values[idx++] = C_STR(q, h);
			else
				arg_values[idx++] = NULL;
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	cell *h = p3;
	pl_idx_t h_ctx = p3_ctx;

	if (!is_structure(h) || (h->arity != 1))
		return throw_error(q, p3, p3_ctx, "domain_error", "arg");

	const char *type = C_STR(q, h);
	cell *c = h + 1;
	c = deref(q, c, h_ctx);
	pl_idx_t c_ctx = q->latest_ctx;
	ffi_type *ret_type = NULL;

	if (!strcmp(type, "int64"))
		ret_type = &ffi_type_sint64;
	else if (!strcmp(type, "fp64"))
		ret_type = &ffi_type_double;
	else if (!strcmp(type, "cstr"))
		ret_type = &ffi_type_pointer;
	else if (!strcmp(type, "string"))
		ret_type = &ffi_type_pointer;
	else
		return pl_failure;

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, idx, ret_type, arg_types) != FFI_OK)
		return pl_failure;

	union result_ result;

	ffi_call(&cif, FFI_FN(func), &result, arg_values);

	cell tmp;

	if (!strcmp(type, "int64"))
		make_int(&tmp, result.i);
	else if (!strcmp(type, "fp64"))
		make_float(&tmp, result.d);
	else if (!strcmp(type, "cstr"))
		make_cstring(&tmp, result.p);
	else if (!strcmp(type, "string"))
		make_string(&tmp, result.p);
	else
		return pl_failure;

	pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

USE_RESULT pl_status fn_sys_ffi_call_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p4,atom);
	GET_NEXT_ARG(p2,iso_list);
	GET_NEXT_ARG(p3,compound);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	uint64_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p4);
	void *func = dlsym((void*)handle, symbol);
	if (!func) return pl_failure;

	return do_ffi_call(q, func, p2, p2_ctx, p3, p3_ctx);
}

USE_RESULT pl_status fn_sys_ffi_call_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,iso_list);
	GET_NEXT_ARG(p3,compound);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_FUNC))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	void *func = (void*)get_smalluint(p1);

	return do_ffi_call(q, func, p2, p2_ctx, p3, p3_ctx);
}

USE_RESULT pl_status fn_sys_ffi_register_function_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,iso_list);
	GET_NEXT_ARG(p4,atom);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	uint64_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p2);
	void *func = dlsym((void*)handle, symbol);
	if (!func) return pl_failure;

	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	LIST_HANDLER(l);
	cell *l = p3;
	pl_idx_t l_ctx = p3_ctx;
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		const char *src = C_STR(q, h);

		if (!strcmp(src, "int64"))
			arg_types[idx++] = TAG_INT;
		else if (!strcmp(src, "fp64"))
			arg_types[idx++] = TAG_FLOAT;
		else if (!strcmp(src, "cstr"))
			arg_types[idx++] = TAG_CSTR;
		else
			arg_types[idx++] = 0;

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	const char *src = C_STR(q, p4);

	if (!strcmp(src, "int64"))
		ret_type = TAG_INT;
	else if (!strcmp(src, "fp64"))
		ret_type = TAG_FLOAT;
	else if (!strcmp(src, "cstr"))
		ret_type = TAG_CSTR;
	else
		ret_type = 0;

	register_ffi(q->pl, symbol, idx, (void*)func, arg_types, ret_type, true);
	return pl_success;
}

#define MARK_TAG(t) (((unsigned)(t) << 4) | 1)

USE_RESULT pl_status fn_sys_ffi_register_predicate_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,iso_list);
	GET_NEXT_ARG(p4,atom);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	uint64_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p2);
	void *func = dlsym((void*)handle, symbol);
	if (!func) return pl_failure;

	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	bool arg_vars[MAX_ARITY];
	LIST_HANDLER(l);
	cell *l = p3;
	pl_idx_t l_ctx = p3_ctx;
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		const char *src = C_STR(q, h);

		if (!strcmp(src, "int64"))
			arg_types[idx++] = TAG_INT;
		else if (!strcmp(src, "-") && !strcmp(C_STR(q, h+1), "int64"))
			arg_types[idx++] = MARK_TAG(TAG_INT);
		else if (!strcmp(src, "fp64"))
			arg_types[idx++] = TAG_FLOAT;
		else if (!strcmp(src, "-") && !strcmp(C_STR(q, h+1), "fp64"))
			arg_types[idx++] = MARK_TAG(TAG_FLOAT);
		else if (!strcmp(src, "cstr"))
			arg_types[idx++] = TAG_CSTR;
		else if (!strcmp(src, "-") && !strcmp(C_STR(q, h+1), "cstr"))
			arg_types[idx++] = MARK_TAG(TAG_CSTR);
		else
			arg_types[idx++] = 0;

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	const char *src = C_STR(q, p4);

	if (!strcmp(src, "int64")) {
		arg_types[idx++] = MARK_TAG(TAG_INT);
		ret_type = TAG_INT;
	} else if (!strcmp(src, "fp64")) {
		arg_types[idx++] = MARK_TAG(TAG_FLOAT);
		ret_type = TAG_FLOAT;
	} else if (!strcmp(src, "cstr")) {
		arg_types[idx++] = MARK_TAG(TAG_CSTR);
		ret_type = TAG_CSTR;
	} else {
		arg_types[idx++] = 0;
		ret_type = 0;
	}

	register_ffi(q->pl, symbol, idx, (void*)func, arg_types, ret_type, false);
	return pl_success;
}

pl_status wrapper_for_function(query *q, builtins *ptr)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1, any);
	cell *c = p1;
	pl_idx_t c_ctx = p1_ctx;

	//printf("*** wrapper %s/%u arity=%u\n", ptr->name, ptr->arity, q->st.curr_cell->arity);

	ffi_cif cif;
	ffi_type *arg_types[MAX_ARITY];
	ffi_status status;
	void *arg_values[MAX_ARITY];
	int idx = 0;

	for (unsigned i = 0; i < ptr->arity; i++, idx++) {
		//printf(" tag=%u ", c->tag);
		//DUMP_TERM("arg=", c, c_ctx);

		if (ptr->types[i] != c->tag)
			return throw_error(q, c, c_ctx, "type_error",
			ptr->types[i] == TAG_INT ? "integer" :
			ptr->types[i] == TAG_FLOAT ? "float" :
			ptr->types[i] == TAG_CSTR ? "atom" :
			ptr->types[i] == TAG_VAR ? "variable" :
			"invalid"
			);

		const char *src = C_STR(q, c);

		if (ptr->types[i] == TAG_INT)
			arg_types[idx] = &ffi_type_sint64;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_types[idx] = &ffi_type_double;
		else if (ptr->types[i] == TAG_CSTR)
			arg_types[idx] = &ffi_type_pointer;
		else
			arg_types[idx] = &ffi_type_void;

		if (ptr->types[i] == TAG_INT)
			arg_values[idx] = &c->val_int;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_values[idx] = &c->val_float;
		else if (ptr->types[i] == TAG_CSTR)
			arg_values[idx] = C_STR(q, c);
		else
			arg_values[idx] = NULL;

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	ffi_type *ret_type = NULL;

	if (ptr->ret_type == TAG_INT)
		ret_type = &ffi_type_sint64;
	else if (ptr->ret_type == TAG_FLOAT)
		ret_type = &ffi_type_double;
	else if (ptr->ret_type == TAG_CSTR)
		ret_type = &ffi_type_pointer;
	else
		return pl_failure;

	//printf("*** wrapper ret tag=%u\n", ptr->ret_type);

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, idx, ret_type, arg_types) != FFI_OK)
		return pl_failure;

	union result_ result;
	ffi_call(&cif, FFI_FN(ptr->fn), &result, arg_values);

	cell tmp;

	if (ptr->ret_type == TAG_INT)
		make_int(&tmp, result.i);
	else if (ptr->ret_type == TAG_FLOAT)
		make_float(&tmp, result.d);
	else if (ptr->ret_type == TAG_CSTR)
		make_cstring(&tmp, result.p);
	else
		return pl_failure;

	q->accum = tmp;
	return pl_success;
}

pl_status wrapper_for_predicate(query *q, builtins *ptr)
{
	GET_FIRST_ARG(p1, any);
	cell *c = p1;
	pl_idx_t c_ctx = p1_ctx;

	//printf("*** wrapper %s/%u arity=%u\n", ptr->name, ptr->arity, q->st.curr_cell->arity);

	ffi_cif cif;
	ffi_type *arg_types[MAX_ARITY];
	ffi_status status;
	void *arg_values[MAX_ARITY];
	void *s_args[MAX_ARITY];
	cell cells[MAX_ARITY];
	int idx = 0;

	for (unsigned i = 0; i < (ptr->arity-1); i++, idx++) {
		//printf(" tag=%u ", c->tag);
		//DUMP_TERM("arg=", c, c_ctx);

		if ((ptr->types[i] != c->tag) && !is_variable(c))
			return throw_error(q, c, c_ctx, "type_error",
			ptr->types[i] == TAG_INT ? "integer" :
			ptr->types[i] == TAG_FLOAT ? "float" :
			ptr->types[i] == TAG_CSTR ? "atom" :
			ptr->types[i] == TAG_VAR ? "variable" :
			"invalid"
			);

		const char *src = C_STR(q, c);

		//printf("*** wrapper tag=%X\n", ptr->types[i]);

		if (ptr->types[i] == TAG_INT)
			arg_types[idx] = &ffi_type_sint64;
		else if (ptr->types[i] == MARK_TAG(TAG_INT))
			arg_types[idx] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_types[idx] = &ffi_type_double;
		else if (ptr->types[i] == MARK_TAG(TAG_FLOAT))
			arg_types[idx] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_CSTR)
			arg_types[idx] = &ffi_type_pointer;
		else if (ptr->types[i] == MARK_TAG(TAG_CSTR))
			arg_types[idx] = &ffi_type_pointer;
		else
			arg_types[idx] = &ffi_type_void;

		cells[idx] = *c;

		if (ptr->types[i] == TAG_INT)
			arg_values[idx] = &cells[idx].val_int;
		else if (ptr->types[i] == MARK_TAG(TAG_INT)) {
			s_args[idx] = &cells[idx].val_int;
			arg_values[idx] = &cells[idx].val_int;
		} else if (ptr->types[i] == TAG_FLOAT)
			arg_values[idx] = &cells[idx].val_float;
		else if (ptr->types[i] == MARK_TAG(TAG_FLOAT)) {
			s_args[idx] = &cells[idx].val_float;
			arg_values[idx] = &s_args[idx];
		} else if (ptr->types[i] == TAG_CSTR) {
			cells[idx].val_str = C_STR(q, c);
			s_args[idx] = &cells[idx].val_str;
			arg_values[idx] = &cells[idx].val_str;
		} else if (ptr->types[i] == MARK_TAG(TAG_CSTR)) {
			cells[idx].val_str = C_STR(q, c);
			s_args[idx] = &cells[idx].val_str;
			arg_values[idx] = &s_args[idx];
		} else
			arg_values[idx] = NULL;

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	ffi_type *ret_type = NULL;

	if (ptr->ret_type == TAG_INT)
		ret_type = &ffi_type_sint64;
	else if (ptr->ret_type == TAG_FLOAT)
		ret_type = &ffi_type_double;
	else if (ptr->ret_type == TAG_CSTR)
		ret_type = &ffi_type_pointer;
	else
		return pl_failure;

	//printf("*** wrapper ret tag=%X\n", ptr->ret_type);

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, idx, ret_type, arg_types) != FFI_OK)
		return pl_failure;

	union result_ result;
	ffi_call(&cif, FFI_FN(ptr->fn), &result, arg_values);

	GET_FIRST_ARG(p11, any);
	c = p11;
	c_ctx = p11_ctx;
	idx = 0;

	for (unsigned i = 0; i < (ptr->arity-1); i++, idx++) {
		if (is_variable(c)) {
			cell tmp;

			if (ptr->types[i] == MARK_TAG(TAG_INT)) {
				make_int(&tmp, cells[idx].val_int);
				pl_status ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				if (ok != pl_success) return ok;
			} else if (ptr->types[i] == MARK_TAG(TAG_FLOAT)) {
				make_float(&tmp, cells[idx].val_float);
				pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				if (ok != pl_success) return ok;
			} else if (ptr->types[i] == MARK_TAG(TAG_CSTR)) {
				may_error(make_cstring(&tmp, cells[idx].val_str));
				pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				if (ok != pl_success) return ok;
			}
		}

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	cell tmp;

	if (ptr->ret_type == TAG_INT) {
		make_int(&tmp, result.i);
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		if (ok != pl_success) return ok;
	} else if (ptr->ret_type == TAG_FLOAT) {
		make_float(&tmp, result.d);
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		if (ok != pl_success) return ok;
	} else if (ptr->ret_type == TAG_CSTR) {
		may_error(make_cstring(&tmp, result.s));
		free(result.s);
		pl_status ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		if (ok != pl_success) return ok;
	}

	return pl_success;
}

#endif
