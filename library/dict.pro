:- module(dict, [get/4, get/3, set/4, del/3, lst/2]).

get([], _, D, D) :- !.
get([N:V|_], N, V, _) :- !.
get([_|T], N, V, D) :-
	get(T, N, V, D).

get([], _, _) :- !,
	fail.
get([N:V|_], N, V) :- !.
get([_|T], N, V) :-
	get(T, N, V).

set(L, N, V, L2) :-
	del(L, N, L3),
	L2=[N:V|L3].

del([], _, []) :- !.
del([N:_|T], N, T) :- !.
del([H|T], N, [H|L]) :-
	del(T, N, L).

lst0([], L, L) :- !.
lst0([_:V|T], L1, L) :-
	lst0(T, [V|L1], L).

lst(D, L) :-
	lst0(D, [], L).
