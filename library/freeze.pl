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
	( get_atts(Var, frozen(Term))
	->	true
	;	Term = true
	).

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, frozen(VarT)), !,
	( var(Other)
	-> ( get_atts(Other, frozen(OtherT))
		->	put_atts(Other, frozen((OtherT, VarT)))
		;	put_atts(Other, frozen(VarT))
		),
		Goals = []
	;   Goals = [VarT]
	).
verify_attributes(_, _, []).

attribute_goals(Var) -->
    { get_atts(Var, frozen(Goals)),
      put_atts(Var, -frozen(_)) },
    [freeze(Var, Goals)].
