:- use_module(library(dict)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_attr(V, Module, Value) :-
	var(V),
	Access =.. [Module,Value],
	get_atts(V, [+Access]).

put_attr(V, Module, Value) :-
	var(V),
	Access =.. [Module,Value],
	put_atts(V, [+Access]).

del_attr(V, Module) :-
	var(V),
	Access =.. [Module,_],
	put_atts(V, [-Access]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_atts(V, +(A)) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	dict:set(D, F, A, D2),
	put_attrs(V, D2).

put_atts(V, -(A)) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	dict:del(D, F, D2),
	put_attrs(V, D2).

put_atts(V, A) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	dict:set(D, F, A, D2),
	put_attrs(V, D2).

get_atts(V, L) :- var(L), !,
	get_attrs(V, D),
	dict:lst(D, L).

get_atts(V, +(A)) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	dict:get(D, F, A).

get_atts(V, -(A)) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	\+ dict:get(D, F, _).

get_atts(V, A) :- !,
	get_attrs(V, D),
	functor(A, F, _),
	dict:get(D, F, A).

attributed(V) :-
	get_attrs(V, D),
	D \= [].
