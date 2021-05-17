% Testing with 16GB RAM, in-order insertion
%
% Results 1M keys:
%									 TPL	 SWI	 LVM	GPROLOG
%		main1	(database)			1.93s	1.29s	5.33s	 0.64
%		main2	(recorded)			2.14s	0.72s	-
%		main3/4	(key-val)			1.25s	-		1.88s
%
% Results 10M keys:
%									 TPL	 SWI	 LVM	GPROLOG
%		main1	(database)			21s		15s		61s		 6.2
%		main2	(recorded)			23s		7.3s	-
%		main3/4	(key-val)			13s		-		20s		
%
% Results 100M keys:
%
%									 TPL	 SWI	 LVM	GPROLOG
%		main1	(database)			fail	fail 	fail	 fail
%		main2	(recorded)			fail	1m12s	-
%		main3/4	(key-val)			2m19s	-		fail
%
% Note: Trealla key-val uses only atomic keys & values

:- dynamic(dummy/2).

main1 :-
	write('Set'), nl,
	between(1,100000000,I),
		assertz(dummy(I,I)),
		fail.

main1 :-
	write('Get'), nl,
	between(1,100000000,I),
		dummy(I,I),
		fail.

/*
main1 :-
	write('Del'), nl,
	between(1,100000000,I),
		retract(dummy(I,I)),
		fail.
*/

main1 :-
	write('Done'), nl.

main2 :-
	write('Set'), nl,
	between(1,100000000,I),
		recordz(I,I),
		fail.

main2 :-
	write('Get'), nl,
	between(1,100000000,I),
		recorded(I,I),
		fail.

/*
main2 :-
	write('Del'), nl,
	between(1,100000000,I),
		recorded(I,I,R),
		erase(R),
		fail.
*/

main2 :-
	write('Done'), nl.

main3 :-
	write('Set'), nl,
	between(1,100000000,I),
		kv_set(I,I,[]),
		fail.

main3 :-
	write('Get'), nl,
	between(1,100000000,I),
		kv_get(I,I,[]),
		fail.

/*
main3 :-
	write('Del'), nl,
	between(1,100000000,I),
		kv_get(I,I,[delete(true)]),
		fail.
*/

main3 :-
	write('Done'), nl.

main4 :-
	write('Set'), nl,
	between(1,100000000,I),
		create_key_value(I,I),
		fail.

main4 :-
	write('Get'), nl,
	between(1,100000000,I),
		read_key_value(I,I),
		fail.

/*
main4 :-
	write('Del'), nl,
	between(1,100000000,I),
		delete_key_value(I,I]),
		fail.
*/

main4 :-
	write('Done'), nl.
