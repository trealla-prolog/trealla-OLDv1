:- use_module(library(sqlite3)).

run :-
	test('samples/sqlite3.db', 'SELECT * FROM company').

test(Database, Query) :-
	flag('SQLITE_OK', SQLITE_OK),
	sqlite3_open(Database, Connection, Ret), Ret is SQLITE_OK,
	sqlite3_query(Connection, Query, Cols, Names),
		writeq([Cols, Names]), nl,
		fail.
test(_, _).

