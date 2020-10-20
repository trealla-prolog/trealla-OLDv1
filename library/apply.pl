partition([X|L], Y, [X|L1], L2) :-
	X @< Y, !,
	partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
	partition(L, Y, L1, L2).
partition([], _, [], []).

/*
qsort([X|L], R, R0) :-
	partition(L, X, L1, L2),
	qsort(L2, R1, R0),
	qsort(L1, R, [X|R1]).
qsort([], R, R).
*/

maplist(_, []).
maplist(P, [X1|X1s]) :-
	call(P, X1),
	maplist(P, X1s).

maplist(_, [], []).
maplist(P, [X1|X1s], [X2|X2s]) :-
	call(P, X1, X2),
	maplist(P, X1s, X2s).

maplist(_, [], [], []).
maplist(P, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	call(P, X1, X2, X3),
	maplist(P, X1s, X2s, X3s).

maplist(_, [], [], [], []).
maplist(P, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	call(P, X1, X2, X3, X4),
	maplist(P, X1s, X2s, X3s, X4s).

spawnlist(_, []) :- wait.
spawnlist(P, [X1|X1s]) :-
	spawn(P, X1),
	spawnlist(P, X1s).

spawnlist(_, [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s]) :-
	spawn(P, X1, X2),
	spawnlist(P, X1s, X2s).

spawnlist(_, [], [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	spawn(P, X1, X2, X3),
	spawnlist(P, X1s, X2s, X3s).

spawnlist(_, [], [], [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	spawn(P, X1, X2, X3, X4),
	spawnlist(P, X1s, X2s, X3s, X4s).

sort(L, R) :-
	length(L,N),
	sort(N, L, _, R).

sort(2, [X1, X2|L], L, R) :-
	!,
	compare(Delta, X1, X2),
	'$sort2'(Delta, X1, X2, R).
sort(1, [X|L], L, [X]) :- !.
sort(0, L, L, []) :- !.
sort(N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	sort(N1, L1, L2, R1),
	sort(N2, L2, L3, R2),
	merge(R1, R2, R).

'$sort2'(<, X1, X2, [X1, X2]).
'$sort2'(=, X1, _,  [X1]).
'$sort2'(>, X1, X2, [X2, X1]).

merge([], R, R) :- !.
merge(R, [], R) :- !.
merge([H1|T1], [H2|T2], Result) :-
	compare(Delta, H1, H2),
	!,
	merge(Delta, H1, H2, T1, T2, Result).

merge(>, H1, H2, T1, T2, [H2|R]) :-
	merge([H1|T1], T2, R).
merge(=, H1, _, T1, T2, [H1|R]) :-
	merge(T1, T2, R).
merge(<, H1, H2, T1, T2, [H1|R]) :-
	merge(T1, [H2|T2], R).

msort(L, R) :-
	length(L,N),
	sort(N, L, _, R).

msort(2, [X1, X2|L], L, R) :-
	!,
	compare(Delta, X1, X2),
	'$msort2'(Delta, X1, X2, R).
msort(1, [X|L], L, [X]) :- !.
msort(0, L, L, []) :- !.
msort(N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	sort(N1, L1, L2, R1),
	sort(N2, L2, L3, R2),
	merge(R1, R2, R).

'$msort2'(<, X1, X2, [X1, X2]).
'$msort2'(=, X1, _,  [X1]).
'$msort2'(>, X1, X2, [X2, X1]).
