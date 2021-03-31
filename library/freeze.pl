:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).
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

attribute_goals(Var) -->
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze(Var, Goals)].

