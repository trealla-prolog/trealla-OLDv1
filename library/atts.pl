:- module(atts,
	[op(1150, fx, attribute)]).
:- use_module(library(apply)).
:- use_module(library(lists)).

'$post_unify_hook' :-
	'$undo_trail'(Vars),
	'$process_vars'(Vars, [], Goals),
	'$redo_trail',
	maplist(call, Goals),
	!.

'$process_vars'([], Goals, Goals) :- !.
'$process_vars'([Var-Val|Vars], SoFar, Goals) :-
	get_atts(Var, Atts),
	'$process_var'(Var, Val, Atts, SoFar, MoreGoals),
	'$process_vars'(Vars, MoreGoals, Goals).

'$process_var'(_, _, [], Goals, Goals) :- !.
'$process_var'(Var, Val, [Att|Atts], SoFar, Goals) :-
	functor(Att, M, _),
	M:verify_attributes(Var, Val, NewGoals),
	append(SoFar, NewGoals, MoreGoals),
	'$process_var'(Var, Val, Atts, MoreGoals, Goals).

