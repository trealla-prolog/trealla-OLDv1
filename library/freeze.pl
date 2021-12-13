:- module(freeze, [freeze/2, frozen/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).

:- meta_predicate(freeze(?, 0)).
:- attribute frozen/1.

freeze(Var, Goal) :-
	(	nonvar(Var)
	->	Goal
	;	(	get_atts(Var, frozen(OldGoals))
		->	put_atts(Var, frozen((OldGoals,Goal)))
		;	put_atts(Var, frozen(Goal))
		)
	).

toconj([], In, In).
toconj([H|T], true, Out) :- !,
	Out2 = H,
	toconj(T, Out2, Out).
toconj([H|T], In, Out) :-
	Out2 = (H, In),
	toconj(T, Out2, Out).

frozen(Term, Goal) :-
	copy_term(Term, _, Gs),
	flatten(Gs, Gs2),
	toconj(Gs2, true, Goal).

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
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze(Var, Goals)].

