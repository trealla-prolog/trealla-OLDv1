:- pragma(apply, [once(true)]).

partition([X|L], Y, [X|L1], L2) :-
	X @< Y, !,
	partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
	partition(L, Y, L1, L2).
partition([], _, [], []).

:- meta_predicate(maplist(1, ?)).
:- meta_predicate(maplist(2, ?, ?)).
:- meta_predicate(maplist(3, ?, ?, ?)).
:- meta_predicate(maplist(4, ?, ?, ?, ?)).
:- meta_predicate(maplist(5, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(7, ?, ?, ?, ?, ?, ?, ?)).

maplist(_, X1) :- X1 = [], !.
maplist(_, []).
maplist(Goal, [X1|X1s]) :-
	call(Goal, X1),
	maplist(Goal, X1s).

maplist(_, X1, X2) :- X1 = [], X2 = [], !.
maplist(_, [], []).
maplist(Goal, [X1|X1s], [X2|X2s]) :-
	call(Goal, X1, X2),
	maplist(Goal, X1s, X2s).

maplist(_, X1, X2, X3) :- X1 = [], X2 = [], X3 = [], !.
maplist(_, [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	call(Goal, X1, X2, X3),
	maplist(Goal, X1s, X2s, X3s).

maplist(_, X1, X2, X3, X4) :- X1 = [], X2 = [], X3 = [], X4 = [], !.
maplist(_, [], [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	call(Goal, X1, X2, X3, X4),
	maplist(Goal, X1s, X2s, X3s, X4s).

maplist(_, X1, X2, X3, X4, X5) :- X1 = [], X2 = [], X3 = [], X4 = [], X5 = [], !.
maplist(_, [], [], [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s]) :-
	call(Goal, X1, X2, X3, X4, X5),
	maplist(Goal, X1s, X2s, X3s, X4s, X5s).

maplist(_, X1, X2, X3, X4, X5, X6) :- X1 = [], X2 = [], X3 = [], X4 = [], X5 = [], X6 = [], !.
maplist(_, [], [], [], [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s], [X6|X6s]) :-
	call(Goal, X1, X2, X3, X4, X5, X6),
	maplist(Goal, X1s, X2s, X3s, X4s, X5s, X6s).

maplist(_, X1, X2, X3, X4, X5, X6, X7) :- X1 = [], X2 = [], X3 = [], X4 = [], X5 = [], X6 = [], X7 = [], !.
maplist(_, [], [], [], [], [], [], []).
maplist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s], [X6|X6s], [X7|X7s]) :-
	call(Goal, X1, X2, X3, X4, X5, X6, X7),
	maplist(Goal, X1s, X2s, X3s, X4s, X5s, X6s, X7s).

:- meta_predicate(tasklist(1, ?)).
:- meta_predicate(tasklist(2, ?, ?)).
:- meta_predicate(tasklist(3, ?, ?, ?)).
:- meta_predicate(tasklist(4, ?, ?, ?, ?)).
:- meta_predicate(tasklist(5, ?, ?, ?, ?, ?)).
:- meta_predicate(tasklist(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(tasklist(7, ?, ?, ?, ?, ?, ?, ?)).

tasklist(_, []) :- wait.
tasklist(Goal, [X1|X1s]) :-
	task(Goal, X1),
	tasklist(Goal, X1s).

tasklist(_, [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s]) :-
	task(Goal, X1, X2),
	tasklist(Goal, X1s, X2s).

tasklist(_, [], [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s], [X3|X3s]) :-
	task(Goal, X1, X2, X3),
	tasklist(Goal, X1s, X2s, X3s).

tasklist(_, [], [], [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s]) :-
	task(Goal, X1, X2, X3, X4),
	tasklist(Goal, X1s, X2s, X3s, X4s).

tasklist(_, [], [], [], [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s]) :-
	task(Goal, X1, X2, X3, X4, X5),
	tasklist(Goal, X1s, X2s, X3s, X4s, X5s).

tasklist(_, [], [], [], [], [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s], [X6|X6s]) :-
	task(Goal, X1, X2, X3, X4, X5, X6),
	tasklist(Goal, X1s, X2s, X3s, X4s, X5s, X6s).

tasklist(_, [], [], [], [], [], [], []) :- wait.
tasklist(Goal, [X1|X1s], [X2|X2s], [X3|X3s], [X4|X4s], [X5|X5s], [X6|X6s], [X7|X7s]) :-
	task(Goal, X1, X2, X3, X4, X5, X6, X7),
	tasklist(Goal, X1s, X2s, X3s, X4s, X5s, X6s, X7s).

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

include(Goal, List, Included) :-
	include_(List, Goal, Included).

	include_([], _, []).
	include_([X1|Xs1], P, Included) :-
		(   call(P, X1) ->  Included = [X1|Included1]
		;   Included = Included1
		),
		include_(Xs1, P, Included1).

exclude(Goal, List, Included) :-
	exclude_(List, Goal, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
	(   call(P, X1) ->  Included = Included1
	;   Included = [X1|Included1]
	),
	exclude_(Xs1, P, Included1).
