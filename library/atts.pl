:- module(atts, [op(1150, fx, attribute)]).

:- use_module(library(apply)).
:- use_module(library(lists), [append/3]).

post_unify_hook_ :-
	'$undo_trail'(Vars),
	ignore(process_vars_(Vars, [], Goals)),  % why ignore?
	'$redo_trail',
	(	acyclic_term(Goals) ->
		maplist(call, Goals)
	; 	true).

'$post_unify_hook' :-
	post_unify_hook_ *-> '$end_hook' ; ('$end_hook', fail).

process_vars_([], Goals, Goals) :- !.
process_vars_([Var-Val|Vars], SoFar, Goals) :-
	get_atts(Var, Atts),
	process_var_(Var, Val, Atts, SoFar, MoreGoals),
	process_vars_(Vars, MoreGoals, Goals).

process_var_(_, _, [], Goals, Goals) :- !.
process_var_(Var, Val, [Att|Atts], SoFar, Goals) :-
	functor(Att, M, _),
	M:verify_attributes(Var, Val, NewGoals),
	append(SoFar, NewGoals, MoreGoals),
	process_var_(Var, Val, Atts, MoreGoals, Goals).

