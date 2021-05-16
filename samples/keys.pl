% Results 100K keys:
%
%			main1	(assertz/match/retract)			4m30s
%			main2	(recordz/recorded/erase)
%			main3	(kv_set/kv_get/kv_del) 			0.22s

:- dynamic(key/2).

main1 :-
	writeln('Set'),
	between(1,100000,I),
		assertz(key(I,I)),
		fail.

main1 :-
	writeln('Get'),
	between(1,100000,I),
		key(I,I),
		fail.

main1 :-
	writeln('Del'),
	between(1,100000,I),
		retract(key(I,I)),
		fail.

main1 :-
	writeln('Done'),
	listing(key/2),
	halt.

main2 :-
	writeln('Set'),
	between(1,100000,I),
		recordz(I,I),
		fail.

main2 :-
	writeln('Get'),
	between(1,100000,I),
		recorded(I,I),
		fail.

main2 :-
	writeln('Del'),
	between(1,100000,I),
		recorded(I,I,R),
		erase(R),
		fail.

main2 :-
	writeln('Done'),
	halt.

main3 :-
	writeln('Set'),
	between(1,100000,I),
		kv_set(I,I,[]),
		fail.

main3 :-
	writeln('Get'),
	between(1,100000,I),
		kv_get(I,I,[]),
		fail.

main3 :-
	writeln('Del'),
	between(1,100000,I),
		kv_get(I,I,[delete(true)]),
		fail.

main3 :-
	\+ kv_get(1,1,[]),
	writeln('Done'),
	halt.
