:- module(atts, [put_atts/2, get_atts/2, attributed/1]).
:- use_module(library(dict)).

% Attributed variables, or a stealth key-value store?
% This is a start.

put_atts(V, +A) :- !,
	sys_get_atts(V, D),
	functor(A, F, _),
	dict:set(D, F, A, D2),
	sys_put_atts(V, D2).

put_atts(V, -A) :- !,
	sys_get_atts(V, D),
	functor(A, F, _),
	dict:del(D, F, D2),
	sys_put_atts(V, D2).

get_atts(V, L) :- var(L), !,
	sys_get_atts(V, D),
	dict:lst(D, L).

get_atts(V, +A) :- !,
	sys_get_atts(V, D),
	functor(A, F, _),
	dict:get(D, F, A).

get_atts(V, -A) :- !,
	sys_get_atts(V, D),
	functor(A, F, _),
	\+ dict:get(D, F, _).

attributed(V) :-
	sys_get_atts(V, D),
	D \= [].
