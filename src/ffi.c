#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "internal.h"
#include "parser.h"
#include "module.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

#if USE_FFI
#if __linux
#include <dlfcn.h>
#endif
#include <ffi.h>
#endif

union result_ {
	double d;
	int64_t i;
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
			else if (!strcmp(src, "atom"))
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
			else if (!strcmp(src, "atom"))
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
	else if (!strcmp(type, "atom"))
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
	else if (!strcmp(type, "atom"))
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
	GET_NEXT_ARG(p4,atom);
	GET_NEXT_ARG(p2,iso_list);
	GET_NEXT_ARG(p3,atom);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	uint64_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p4);
	void *func = dlsym((void*)handle, symbol);
	if (!func) return pl_failure;

	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	LIST_HANDLER(l);
	cell *l = p2;
	pl_idx_t l_ctx = p2_ctx;
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		const char *src = C_STR(q, h);

		if (!strcmp(src, "int64"))
			arg_types[idx++] = TAG_INT;
		else if (!strcmp(src, "fp64"))
			arg_types[idx++] = TAG_FLOAT;
		else if (!strcmp(src, "atom"))
			arg_types[idx++] = TAG_CSTR;
		else
			arg_types[idx++] = 0;

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	const char *src = C_STR(q, p3);

	if (!strcmp(src, "int64"))
		ret_type = TAG_INT;
	else if (!strcmp(src, "fp64"))
		ret_type = TAG_FLOAT;
	else if (!strcmp(src, "atom"))
		ret_type = TAG_CSTR;
	else
		ret_type = 0;

	register_function(q->pl, symbol, idx, (void*)func, arg_types, ret_type);
	return pl_success;
}

USE_RESULT pl_status fn_sys_ffi_register_predicate_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p4,atom);
	GET_NEXT_ARG(p2,iso_list);
	GET_NEXT_ARG(p3,atom);
	return pl_failure;
}

pl_status wrapper_function(query *q, builtins *ptr)
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
			"invalid"
			);

		const char *src = C_STR(q, c);

		if (is_smallint(c))
			arg_types[idx] = &ffi_type_sint64;
		else if (is_float(c))
			arg_types[idx] = &ffi_type_double;
		else if (is_atom(c))
			arg_types[idx] = &ffi_type_pointer;
		else
			arg_types[idx] = &ffi_type_void;

		if (is_smallint(c))
			arg_values[idx] = &c->val_int;
		else if (is_float(c))
			arg_values[idx] = &c->val_float;
		else if (is_atom(c))
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

#endif
