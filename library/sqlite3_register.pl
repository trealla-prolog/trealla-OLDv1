:- module(sqlite3_register, []).

:- initialization(
	('$dlopen'('libsqlite3.so', 0, H),
	'$register_predicate'(H, sqlite3_open, [cstr, -ptr], int64),
	'$register_predicate'(H, sqlite3_close, [ptr], int64),
	'$register_predicate'(H, sqlite3_exec, [ptr,cstr,ptr,ptr,-ptr], int64),
	'$register_predicate'(H, sqlite3_prepare_v2, [ptr,cstr,ptr,-ptr,-const_cstr], int64),
	'$register_predicate'(H, sqlite3_step, [ptr], int64),
	'$register_predicate'(H, sqlite3_finalize, [ptr], int64),
	'$register_predicate'(H, sqlite3_column_count, [ptr], int64),
	'$register_predicate'(H, sqlite3_column_name, [ptr,int64], const_cstr),
	'$register_predicate'(H, sqlite3_column_type, [ptr,int64], int64),
	'$register_predicate'(H, sqlite3_column_int64, [ptr,int64], int64),
	'$register_predicate'(H, sqlite3_column_double, [ptr,int64], fp64),
	'$register_predicate'(H, sqlite3_column_text, [ptr,int64], const_cstr),
	true
	)).
