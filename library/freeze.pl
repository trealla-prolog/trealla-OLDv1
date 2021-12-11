:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).

:- meta_predicate(freeze(?, 0)).
:- attribute frozen/1.

freeze(Var, Goal) :-
	(	nonvar(Var)
	->	call(Goal)
	;	put_atts(Var, frozen(Goal))
	).

frozen(Var, Goal) :-
	(	get_atts(Var, frozen(Goal))
	->	true
	;	Goal = true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, frozen(VarGoals)), !,
	(	var(Other)
	->	(	(	get_atts(Other, frozen(OtherGoals))
			->	put_atts(Other, frozen((OtherGoals, VarGoals)))
			;	put_atts(Other, frozen(VarGoals))
			),
			Goals = []
		)
	;   Goals = [VarGoals]
	).
verify_attributes(_, _, []).

attribute_goals(Var) -->
	{ get_atts(Var, frozen(Goals)), put_atts(Var, -frozen(_)) },
	[freeze(Var, Goals)].
