:- module(when, [when/2]).

:- use_module(library(atts)).
:- use_module(library(dcgs)).

:- meta_predicate(when(+, 0)).
:- attribute when/1.

when(nonvar(Var), Goal) :-
	process_var_(Var, VarCond, Goal).

when(ground(Var), Goal) :-
	process_var_(Var, VarCond, Goal).

when(?=(Var1,Var2), Goal) :-
	process_var_(Var1, VarCond, Goal),
	process_var_(Var2, VarCond, Goal).

when((Var1,Var2), Goal) :-
	process_var_(Var1, VarCond, Goal),
	process_var_(Var2, VarCond, Goal).

when((Var1;Var2), Goal) :-
	process_var_(Var1, VarCond, Goal),
	process_var_(Var2, VarCond, Goal).

process_var_(Var, VarCond, Goal) :-
	(	get_atts(Var, when(OldCond-OldGoal))
		->	(	NewCond = (VarCond, OldCond),
				NewGoal = (OldGoal, Goal),
				put_atts(Var, -when(_))
			)
		;	(	NewCond = VarCond,
				NewGoal = Goal
			)
	),
    put_atts(Var, when(NewCond-NewGoal)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_attributes(Var, Other, Goals) :-
	get_atts(Var, when(VarCond-VarGoal)),
	(	var(Other)
		-> 	get_atts(Other, when(VarCond-VarGoal)),
			Goals = (
				VarCond
				-> VarGoal
				; ( VarCond == VarCond
					-> NewCond = VarCond
					; NewCond = (VarCond,VarCond)
					),
				(VarGoal == VarGoal
					-> NewGoal = VarGoal
					; NewGoal = (VarGoal,VarGoal)
					),
				put_atts(Other, -when(_)),
				put_atts(Other, when(NewCond-NewGoal))
			)
		; Goals = [(VarCond -> VarGoal ; true)]
	).

attribute_goals(Var) -->
	{ get_atts(Var, when(Goals)), put_atts(Var, -when(_)) },
	[when(Var, Goals)].
