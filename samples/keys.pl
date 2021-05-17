% Results 1M keys:
%
%		main1	(assertz/match)		1.93s	(SWI 1.29s)
%		main2	(recordz/recorded)	2.14s	(SWI 0.72s)
%		main3	(kv_set/kv_get)		0.14s

main1 :-
	writeln('Set'),
	between(1,1000000,I),
		assertz(key(I,I)),
		fail.

main1 :-
	writeln('Get'),
	between(1,1000000,I),
		key(I,I),
		fail.

/*
main1 :-
	writeln('Del'),
	between(1,1000000,I),
		retract(key(I,I)),
		fail.
*/

main1 :-
	writeln('Done'),
	halt.

main2 :-
	writeln('Set'),
	between(1,1000000,I),
		recordz(I,I),
		fail.

main2 :-
	writeln('Get'),
	between(1,1000000,I),
		recorded(I,I),
		fail.

/*
main2 :-
	writeln('Del'),
	between(1,1000000,I),
		recorded(I,I,R),
		erase(R),
		fail.
*/

main2 :-
	writeln('Done'),
	halt.

main3 :-
	writeln('Set'),
	between(1,1000000,I),
		kv_set(I,I,[]),
		fail.

main3 :-
	writeln('Get'),
	between(1,1000000,I),
		kv_get(I,I,[]),
		fail.

/*
main3 :-
	writeln('Del'),
	between(1,1000000,I),
		kv_get(I,I,[delete(true)]),
		fail.
*/

main3 :-
	writeln('Done'),
	halt.
