:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- meta_predicate freeze(?, 0).
:- attribute frozen/1.

freeze(V, Term) :-
	( nonvar(V) ->
		call(Term)
	;
		put_attr(V, freeze, frozen(Term))
	).

frozen(V, Term) :-
	( get_attr(V, freeze, frozen(Term)) ->
		true
	;
		Term = true
	).

verify_attributes(V, _, Goals) :-
	get_attr(V, freeze, frozen(Term)), !,
	( var(Term) ->
		Goals = []
	;
		Goals = [Term]
	).
verify_attributes(_, _, []).
