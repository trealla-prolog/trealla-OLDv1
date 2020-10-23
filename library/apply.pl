partition([X|L], Y, [X|L1], L2) :-
	X @< Y, !,
	partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
	partition(L, Y, L1, L2).
partition([], _, [], []).

maplist(_, []).
maplist(Goal, [X1|X1s]) :-
	call(Goal, X1),
	maplist(Goal, X1s).

maplist(_, [], []).
maplist(Goal, [X1|X1s], [X2|X2s]) :-
	call(Goal, X1, X2),
	maplist(Goal, X1s, X2s).

maplist(_, [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	call(Goal, X1, X2, X3),
	maplist(Goal, X1s, X2s, X3s).

maplist(_, [], [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	call(Goal, X1, X2, X3, X4),
	maplist(Goal, X1s, X2s, X3s, X4s).

spawnlist(_, []), wait.
spawnlist(Goal, [X1|X1s]) :-
	spawn(Goal, X1),
	spawnlist(Goal, X1s).

spawnlist(_, [], []), wait.
spawnlist(Goal, [X1|X1s], [X2|X2s]) :-
	spawn(Goal, X1, X2),
	spawnlist(Goal, X1s, X2s).

spawnlist(_, [], [], []), wait.
spawnlist(Goal, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	spawn(Goal, X1, X2, X3),
	spawnlist(Goal, X1s, X2s, X3s).

spawnlist(_, [], [], [], []), wait.
spawnlist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	spawn(Goal, X1, X2, X3, X4),
	spawnlist(Goal, X1s, X2s, X3s, X4s).

foldl(Goal, List, V0, V) :-
	foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
	call(Goal, H, V0, V1),
	foldl_(T, Goal, V1, V).

foldl(Goal, List1, List2, V0, V) :-
	foldl_(List1, List2, Goal, V0, V).

foldl_([], [], _, V, V).
foldl_([H1|T1], [H2|T2], Goal, V0, V) :-
	call(Goal, H1, H2, V0, V1),
	foldl_(T1, T2, Goal, V1, V).

foldl(Goal, List1, List2, List3, V0, V) :-
	foldl_(List1, List2, List3, Goal, V0, V).

foldl_([], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V) :-
	  call(Goal, H1, H2, H3, V0, V1),
	  foldl_(T1, T2, T3, Goal, V1, V).

foldl(Goal, List1, List2, List3, List4, V0, V) :-
	foldl_(List1, List2, List3, List4, Goal, V0, V).

foldl_([], [], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V0, V) :-
	call(Goal, H1, H2, H3, H4, V0, V1),
	foldl_(T1, T2, T3, T4, Goal, V1, V).

