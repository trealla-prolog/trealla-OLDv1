:- module(atts,
	[op(1150, fx, attribute)]).

'$post_unify_hook' :-
	'$undo_trail'(Vars),
	'$process_vars'(Vars, [], Goals),
	'$redo_trail',
	maplist(call, Goals),
	true.

'$process_vars'([], Goals, Goals) :- !.
'$process_vars'([Var-Val|Vars], SoFar, Goals) :-
	get_att(Var, Atts),
	'$process_var'(Var, Val, Atts, SoFar, MoreGoals),
	'$process_vars'(Vars, MoreGoals, Goals),
	true.

'$process_var'(_, _, [], Goals, Goals) :- !.
'$process_var'(Var, Val, [Att|Atts], SoFar, Goals) :-
	functor(Att, M, _),
	M:verify_attributes(Var, Val, NewGoals),
	'$append'(SoFar, NewGoals, MoreGoals),
	'$process_var'(Var, Val, Atts, MoreGoals, Goals),
	true.

