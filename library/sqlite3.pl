:- module(sqlite,
	sqlite3_open/3,
	sqlite3_close/2,
	sqlite3_exec/6
	).

% This is experimental & under development. DO NOT USE.

:- initialization(
	('$dlopen'('libsqlite3.so', 0, H),
	'$register_predicate'(H, sqlite3_open, [cstr, -ptr], int64),
	'$register_predicate'(H, sqlite3_close, [ptr], int64),
	'$register_predicate'(H, sqlite3_exec, [ptr,cstr,ptr,ptr,-cstr], int64)
	)).

