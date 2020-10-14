maplist(_, []).
maplist(P, [X1|X1s]) :-
	copy_term(P, TMP_P), P=TMP_P,
	copy_term(X1, TMP_X1), X1=TMP_X1,
	call(TMP_P, TMP_X1),
	maplist(P, X1s).

maplist(_, [], []).
maplist(P, [X1|X1s], [X2|X2s]) :-
	copy_term(P, TMP_P), P=TMP_P,
	copy_term(X1, TMP_X1), X1=TMP_X1,
	copy_term(X2, TMP_X2), X2=TMP_X2,
	call(TMP_P, TMP_X1, TMP_X2),
	maplist(P, X1s, X2s).

maplist(_, [], [], []).
maplist(P, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	copy_term(P, TMP_P), P=TMP_P,
	copy_term(X1, TMP_X1), X1=TMP_X1,
	copy_term(X2, TMP_X2), X2=TMP_X2,
	copy_term(X3, TMP_X3), X3=TMP_X3,
	call(TMP_P, TMP_X1, TMP_X2, TMP_X3),
	maplist(P, X1s, X2s, X3s).

maplist(_, [], [], [], []).
maplist(P, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	copy_term(P, TMP_P), P=TMP_P,
	copy_term(X1, TMP_X1), X1=TMP_X1,
	copy_term(X2, TMP_X2), X2=TMP_X2,
	copy_term(X3, TMP_X3), X3=TMP_X3,
	copy_term(X4, TMP_X4), X3=TMP_X4,
	call(TMP_P, TMP_X1, TMP_X2, TMP_X3, TMP_X4),
	maplist(P, X1s, X2s, X3s, X4s).

% TO-DO...

spawnlist(_, []) :- wait.
spawnlist(P, [X1|X1s]) :-
	spawn(P, X1),
	spawnlist(P, X1s).

spawnlist(_, [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s]) :-
	spawn(P, X1, X2), spawnlist(P, X1s, X2s).

spawnlist(_, [], [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	spawn(P, X1, X2, X3),
	spawnlist(P, X1s, X2s, X3s).

spawnlist(_, [], [], [], []) :- wait.
spawnlist(P, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	spawn(P, X1, X2, X3, X4),
	spawnlist(P, X1s, X2s, X3s, X4s).
