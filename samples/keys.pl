% Results 1M keys:
%
%		main1	(assertz/match)		1.93s	(SWI 1.29s, LVM 5.33s)
%		main2	(recordz/recorded)	2.14s	(SWI 0.72s)
%		main3	(kv_set/kv_get)		1.25s
%
% Testing with 16GB RAM:
%
%  SWI and Trealla could do 10M keys for main1/main2
%  SWI and Trealla failed to do 100M keys for main1
%  SWI could do 100M keys for main2
%  Trealla could do 100M keys for main3

main1 :-
	write('Set'), nl,
	between(1,1000000,I),
		assertz(key(I,I)),
		fail.

main1 :-
	write('Get'), nl,
	between(1,1000000,I),
		key(I,I),
		fail.

/*
main1 :-
	write('Del'), nl,
	between(1,1000000,I),
		retract(key(I,I)),
		fail.
*/

main1 :-
	write('Done'), nl.

main2 :-
	write('Set'), nl,
	between(1,1000000,I),
		recordz(I,I),
		fail.

main2 :-
	write('Get'), nl,
	between(1,1000000,I),
		recorded(I,I),
		fail.

/*
main2 :-
	write('Del'), nl,
	between(1,1000000,I),
		recorded(I,I,R),
		erase(R),
		fail.
*/

main2 :-
	write('Done'), nl.

main3 :-
	write('Set'), nl,
	between(1,1000000,I),
		kv_set(I,I,[]),
		fail.

main3 :-
	write('Get'), nl,
	between(1,1000000,I),
		kv_get(I,I,[]),
		fail.

/*
main3 :-
	write('Del'), nl,
	between(1,1000000,I),
		kv_get(I,I,[delete(true)]),
		fail.
*/

main3 :-
	write('Done'), nl.
