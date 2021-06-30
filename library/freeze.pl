:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- meta_predicate(freeze(?, 0)).
:- attribute frozen/1.

freeze(Var, Term) :-
	( nonvar(Var) ->
		call(Term)
	;
		put_atts(Var, frozen(Term))
	).

frozen(Var, Term) :-
	( get_atts(Var, frozen(Term)) ->
		true
	;
		Term = true
	).

verify_attributes(Var, _, Goals) :-
	get_atts(Var, frozen(Term)), !,
	( var(Term) ->
		Goals = []
	;
		Goals = [Term]
	).
verify_attributes(_, _, []).

