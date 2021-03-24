:- module(atts,
	[op(1150, fx, attribute)]).

:- use_module(library(dict)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attributes are scoped with atts(Attribute) allowing multiple
% sets of non-conflicting attributes to be used concurrently on
% a variable.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_attr(V, Name, Value) :-
	var(V),
	Attr =.. [Name,Value],
	get_atts(V, +Attr).

put_attr(V, Name, Value) :-
	var(V),
	Attr =.. [Name,Value],
	put_atts(V, +Attr).

del_attr(V, Name) :-
	var(V),
	Attr =.. [Name,_],
	put_atts(V, -Attr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_atts(V, +(A)) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	dict:set(D, atts(F), A, D2),
	'$put_attrs'(V, D2).

put_atts(V, -(A)) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	dict:del(D, atts(F), D2),
	'$put_attrs'(V, D2).

put_atts(V, A) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	dict:set(D, atts(F), A, D2),
	'$put_attrs'(V, D2).

get_atts(V, L) :- var(L), !,
	'$get_attrs'(V, D),
	dict:match(D, atts(_), L).

get_atts(V, +(A)) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	dict:get(D, atts(F), A).

get_atts(V, -(A)) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	\+ dict:get(D, atts(F), _).

get_atts(V, A) :- !,
	'$get_attrs'(V, D),
	functor(A, F, _),
	dict:get(D, atts(F), A).

attributed(V) :-
	'$get_attrs'(V, D),
	D \= [].
