:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- meta_predicate(freeze(?, 0)).
:- attribute frozen/1.

freeze(Var, Term) :-
	(	nonvar(Var)
	->	call(Term)
	;	put_atts(Var, frozen(Term))
	).

frozen(Var, Term) :-
	(	get_atts(Var, frozen(Term))
	->	true
	;	Term = true
	).

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, frozen(VarGoals)), !,
	(	var(Other)
	->	( get_atts(Other, frozen(OtherGoals))
		->	put_atts(Other, frozen((OtherGoals, VarGoals)))
		;	put_atts(Other, frozen(VarGoals))
		),
		Goals = []
	;   Goals = [VarGoals]
	).
verify_attributes(_, _, []).

attribute_goals(Var) -->
	{ get_atts(Var, frozen(Goals)), put_atts(Var, -frozen(_)) },
	[freeze(Var, Goals)].
