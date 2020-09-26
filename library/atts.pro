:- module(atts, [put_atts/2, get_atts/2]).

:- use_module(library(dict)).

put_atts(V, +(A)) :-
	sys_get_atts(V, D),
	functor(A, F, _),
	dict:set(D, F, A, D2),
	sys_put_atts(V, D2).

put_atts(V, -(A)) :-
	sys_get_atts(V, D),
	functor(A, F, _),
	dict:del(D, F, D2),
	sys_put_atts(V, D2).


