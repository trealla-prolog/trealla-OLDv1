:- pragma(builtins, [once(true)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are SICStus compatible...

must_be(Term, callable, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (callable(Term) -> true ; throw(error(type_error(callable, Term), must_be/4))), !.
must_be(Term, atom, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (atom(Term) -> true ; throw(error(type_error(atom, Term), must_be/4))), !.
must_be(Term, atomic, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (atomic(Term) -> true ; throw(error(type_error(atomic, Term), must_be/4))), !.
must_be(Term, integer, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (integer(Term) -> true ; throw(error(type_error(integer, Term), must_be/4))), !.
must_be(Term, float, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (float(Term) -> true ; throw(error(type_error(float, Term), must_be/4))), !.
must_be(Term, number, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (number(Term) -> true ; throw(error(type_error(number, Term), must_be/4))), !.
must_be(Term, var, _Goal, _Arg) :- !, (var(Term) -> true ; throw(error(instantiation_error(Term), must_be/4))), !.
must_be(Term, nonvar, _Goal, _Arg) :- !, (nonvar(Term) -> true ; throw(error(uninstantiation_error(Term), must_be/4))), !.
must_be(Term, ground, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (ground(Term) -> true ; throw(error(type_error(Term, ground), must_be/4))), !.
must_be(Term, compound, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (compound(Term) -> true ; throw(error(type_error(compound, Term), must_be/4))), !.
must_be(Term, list, _Goal, _Arg) :- !, '$mustbe_instantiated'(Term), (list(Term) -> true ; throw(error(type_error(list, Term), must_be/4))), !.

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

expand_term((H --> B), Out) :- !,
	dcg_translate((H --> B), Out), !.
expand_term(In, Out) :-
	term_expansion(In, Out). !,
expand_term(T, T).

unify_with_occurs_check(X, X) :-
	acyclic_term(X).

predicate_property(P, A) :-
	nonvar(P), atom(A), !,
	must_be(P, callable, _, _),
	'$legacy_predicate_property'(P, A).
predicate_property(P, A) :-
	'$load_properties',
	(var(A) -> true ;
	 (memberchk(A, [built_in,control_construct,discontiguous,private,static,dynamic,persist,multifile,meta_predicate(_)]) ->
		true ;
		throw(error(domain_error(predicate_property, A), P))
		)
	),
	must_be(P, callable, _, _),
	'$predicate_property'(P, A).

subsumes_term(G, S) :-
	\+ \+ (
	 term_variables(S, V1),
	 G = S,
	 term_variables(V1, V2),
	 V2 == V1
	).

variant(Term1, Term2) :-
	% avoid trouble in any shared variables
	copy_term(Term1, Term1Copy),
	copy_term(Term2, Term2Copy),
	% ground and compare the term copies
	numbervars(Term1Copy, 0, N),
	numbervars(Term2Copy, 0, N),
	Term1Copy == Term2Copy.

call_cleanup(G, C) :-
	setup_call_cleanup(true, G, C).

setup_call_cleanup(S, G, C) :-
	call((S, !)),
	'$register_cleanup'((C, !)),
	catch(G, Err, ('$catch'((\+ \+ C), _, true), throw(Err))),
	'$chk_is_det'.

catch(G, E, C) :-
	'$call'('$catch'(G, E, C)).

findall(T, G, B, Tail) :-
	'$mustbe_list_or_var'(B),
	'$mustbe_list_or_var'(Tail),
	findall(T, G, B0),
	append(B0, Tail, B), !.

findall(T, G, B) :-
	copy_term('$findall'(T,G,B), G0),
	'$rawcall'(G0),
	'$findall'(T,G,B)=G0.

bagof(T, G, B) :-
	copy_term('$bagof'(T,G,B), G0),
	'$rawcall'(G0),
	'$bagof'(T,G,B)=G0.

setof(T, G, B) :-
	bagof(T, G, B0),
	sort(B0, B).

% This is to contain cuts...

call(G) :- '$call'(G).
call(G, P1) :- '$call'(G, P1).
call(G, P1, P2) :- '$call'(G, P1, P2).
call(G, P1, P2, P3) :- '$call'(G, P1, P2, P3).
call(G, P1, P2, P3, P4) :- '$call'(G, P1, P2, P3, P4).
call(G, P1, P2, P3, P4, P5) :- '$call'(G, P1, P2, P3, P4, P5).
call(G, P1, P2, P3, P4, P5, P6) :- '$call'(G, P1, P2, P3, P4, P5, P6).
call(G, P1, P2, P3, P4, P5, P6, P7) :- '$call'(G, P1, P2, P3, P4, P5, P6, P7).

task(G) :- '$task'(G).
task(G, P1) :- '$task'(G, P1).
task(G, P1, P2) :- '$task'(G, P1, P2).
task(G, P1, P2, P3) :- '$task'(G, P1, P2, P3).
task(G, P1, P2, P3, P4) :- '$task'(G, P1, P2, P3, P4).
task(G, P1, P2, P3, P4, P5) :- '$task'(G, P1, P2, P3, P4, P5).
task(G, P1, P2, P3, P4, P5, P6) :- '$task'(G, P1, P2, P3, P4, P5, P6).
task(G, P1, P2, P3, P4, P5, P6, P7) :- '$task'(G, P1, P2, P3, P4, P5, P6, P7).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

merge([], R, R) :- !.
merge(R, [], R) :- !.
merge([H1|T1], [H2|T2], Result) :-
	compare(Delta, H1, H2), !,
	merge(Delta, H1, H2, T1, T2, Result).

merge(>, H1, H2, T1, T2, [H2|R]) :-
	merge([H1|T1], T2, R).
merge(=, H1, _, T1, T2, [H1|R]) :-
	merge(T1, T2, R).
merge(<, H1, H2, T1, T2, [H1|R]) :-
	merge(T1, [H2|T2], R).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

sort(L, R) :-
	'$mustbe_list'(L),
	'$mustbe_list_or_var'(R),
	length(L,N),
	sort(N, L, _, R).

sort(2, [X1, X2|L], L, R) :- !,
	compare(Delta, X1, X2),
	'$sort2'(Delta, X1, X2, R).
sort(1, [X|L], L, [X]) :- !.
sort(0, L, L, []) :- !.
sort(N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	sort(N1, L1, L2, R1),
	sort(N2, L2, L3, R2),
	merge(R1, R2, R).

'$sort2'(<, X1, X2, [X1, X2]).
'$sort2'(=, X1, _,  [X1]).
'$sort2'(>, X1, X2, [X2, X1]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

mmerge([], R, R) :- !.
mmerge(R, [], R) :- !.
mmerge([H1|T1], [H2|T2], Result) :-
	compare(Delta, H1, H2), !,
	mmerge(Delta, H1, H2, T1, T2, Result).

mmerge(>, H1, H2, T1, T2, [H2|R]) :-
	mmerge([H1|T1], T2, R).
mmerge(=, H1, H2, T1, T2, [H1|R]) :-
	mmerge(T1, [H2|T2], R).
mmerge(<, H1, H2, T1, T2, [H1|R]) :-
	mmerge(T1, [H2|T2], R).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

samsort(L, R) :- msort(L, R).

msort(L, R) :-
	'$mustbe_list'(L),
	'$mustbe_list_or_var'(R),
	length(L,N),
	msort(N, L, _, R).

msort(2, [X1, X2|L], L, R) :- !,
	compare(Delta, X1, X2),
	'$msort2'(Delta, X1, X2, R).
msort(1, [X|L], L, [X]) :- !.
msort(0, L, L, []) :- !.
msort(N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	msort(N1, L1, L2, R1),
	msort(N2, L2, L3, R2),
	mmerge(R1, R2, R).

'$msort2'(<, X1, X2, [X1, X2]).
'$msort2'(=, X1, X2, [X1, X2]).
'$msort2'(>, X1, X2, [X2, X1]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

keymerge([], R, R) :- !.
keymerge(R, [], R) :- !.
keymerge([H1|T1], [H2|T2], Result) :-
	keycompare(Delta, H1, H2), !,
	keymerge(Delta, H1, H2, T1, T2, Result).

keymerge(>, H1, H2, T1, T2, [H2|R]) :-
	keymerge([H1|T1], T2, R).
keymerge(=, H1, H2, T1, T2, [H1|R]) :-
	keymerge(T1, [H2|T2], R).
keymerge(<, H1, H2, T1, T2, [H1|R]) :-
	keymerge(T1, [H2|T2], R).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

keycompare(Delta, (K1-_), (K2-_)) :-
	(K1 @< K2 -> Delta = '<' ;
	(K1 @> K2 -> Delta = '>' ;
	Delta = '=')).

keysort(L, R) :-
	'$mustbe_pairlist'(L),
	'$mustbe_pairlist_or_var'(R),
	length(L,N),
	keysort(N, L, _, R).

keysort(2, [X1, X2|L], L, R) :- !,
	keycompare(Delta, X1, X2),
	'$msort2'(Delta, X1, X2, R).
keysort(1, [X|L], L, [X]) :- !.
keysort(0, L, L, []) :- !.
keysort(N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	keysort(N1, L1, L2, R1),
	keysort(N2, L2, L3, R2),
	keymerge(R1, R2, R).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

phrase_from_file(P, Filename) :-
	open(Filename, read, Str, [mmap(Ms)]),
	copy_term(P, P2), P2=P,
	phrase(P2, Ms, []),
	close(Str).

phrase_from_file(P, Filename, Opts) :-
	open(Filename, read, Str, [mmap(Ms)|Opts]),
	copy_term(P, P2), P2=P,
	phrase(P2, Ms, []),
	close(Str).

phrase(GRBody, S0) :-
	phrase(GRBody, S0, []).

phrase(GRBody, S0, S) :-
	( var(GRBody) ->
		throw(error(instantiation_error, phrase/3))
	; dcg_constr(GRBody) -> phrase_(GRBody, S0, S)
	; functor(GRBody, _, _) -> call(GRBody, S0, S)
	; throw(error(type_error(callable, GRBody), phrase/3))
	).

phrase_([], S, S).
	phrase_(!, S, S).
phrase_((A, B), S0, S) :-
	phrase(A, S0, S1), phrase(B, S1, S).
phrase_((A -> B ; C), S0, S) :-
	!,
	(phrase(A, S0, S1) ->
		phrase(B, S1, S) ; phrase(C, S0, S)
	).
phrase_((A ; B), S0, S) :-
	(phrase(A, S0, S) ; phrase(B, S0, S)).
phrase_((A | B), S0, S) :-
	(phrase(A, S0, S) ; phrase(B, S0, S)).
phrase_({G}, S0, S) :-
	(call(G), S0 = S).
phrase_(call(G), S0, S) :-
	call(G, S0, S).
phrase_((A -> B), S0, S) :-
	phrase((A -> B ; fail), S0, S).
phrase_(phrase(NonTerminal), S0, S) :-
	phrase(NonTerminal, S0, S).
phrase_([T|Ts], S0, S) :-
	append([T|Ts], S, S0).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Canonical version... this is a start

phrase_to_stream(P, Stream) :-
	phrase(P, Chars, []),
	maplist(write(Stream), Chars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edinburgh...

tab(0) :- !.
tab(N) :- put_code(32), M is N-1, tab(M).
tab(_, 0) :- !.
tab(S, N) :- put_code(S, 32), M is N-1, tab(S, M).
get0(C) :- get_code(C).
get0(S, C) :- get_code(S, C).
display(T) :- write_canonical(T).
display(S, T) :- write_canonical(S, T).
put(C) :- put_code(C).
put(S,C) :- put_code(S, C).
see(F) :- open(F, read, S), set_input(S).
tell(F) :- open(F, write, S), set_output(S).
append(F) :- open(F, append, S), set_output(S).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partial_string(S, P) :- append(S, _, P).
partial_string(S, P, V) :- append(S, V, P).

forall(Cond, Action) :- \+ (Cond, \+ Action).
chars_base64(Plain, Base64,_) :- base64(Plain, Base64).
chars_urlenc(Plain, Url, _) :- urlenc(Plain, Url).

current_key(K) :- var(K), '$record_key'(K,_).
recorda(K, V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).
recordz(K, V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).
recorded(K, V) :- nonvar(K), '$record_key'(K,V).
recorda(K, V, R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V), R).
recordz(K, V, R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V), R).
recorded(K, V, R) :- nonvar(K), clause('$record_key'(K,V), _, R).

format(F) :- format(F, []).
term_to_atom(T, S) :- write_term_to_chars(S, T, []).
write_term_to_atom(S, T, Opts) :- write_term_to_chars(S, Opts, T).
read_term_from_atom(S, T, Opts) :- read_term_from_chars(S, Opts, T).
absolute_file_name(R, A) :- absolute_file_name(R, A, []).
client(U, H, P, S) :- client(U,H,P,S,[]).
server(H, S) :- server(H,S,[]).
set_random(seed(Seed)) :- set_seed(Seed).
set_random(seed(random)) :- time(Seed), set_seed(Seed).
maybe :- random(F), F < 0.5.
prolog_load_context(module, Module) :- module(Module).
open(F, M, S) :- open(F, M, S, []).
load_files(Files) :- load_files(Files,[]).
consult(Files) :- load_files(Files,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...

nb_setval(K, _) :-
	must_be(K, atom, _, _),
	user:retract('$global_key'(K, _)),
	fail.
nb_setval(K, V) :-
	must_be(K, atom, _, _),
	user:assertz('$global_key'(K, V)).

nb_getval(K, V) :-
	must_be(K, atom, _, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), nb_getval/2))),
	!.

nb_delete(K) :-
	must_be(K, atom, _, _),
	user:retract('$global_key'(K, _)),
	!.
nb_delete(_).

nb_current(K, V) :-
	user:clause('$global_key'(K, V), _).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...
% The following is not really correct.

b_setval(K, _) :-
	must_be(K, atom, _, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
b_setval(K, V) :-
	must_be(K, atom, _, _),
	user:asserta('$global_key'(K, V)).
b_setval(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_setval0(K, _) :-
	must_be(K, atom, _, _),
	\+ user:clause('$global_key'(K, _), _), asserta('$global_key'(K, 0)),
	fail.
b_setval0(K, V) :-
	must_be(K, atom, _, _),
	user:asserta('$global_key'(K, V)).
b_setval0(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_getval(K, V) :-
	must_be(K, atom, _, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), b_getval/2))),
	!.

b_delete(K) :-
	must_be(K, atom, _, _),
	user:retractall('$global_key'(K, _)),
	!.
b_delete(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compatibility with Scryer? SICStus? Needed for CLPB.

bb_b_put(K, _) :-
	must_be(K, atom, _, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
bb_b_put(K, V) :-
	must_be(K, atom, _, _),
	user:asserta('$global_key'(K, V)).
bb_b_put(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

bb_b_del(K) :-
	must_be(K, atom, _, _),
	user:retract('$global_key'(K, _)),
	!.
bb_b_del(_).

bb_put(K, _) :-
	must_be(K, atom, _, _),
	user:retract('$global_key'(K, _)),
	fail.
bb_put(K, V) :-
	must_be(K, atom, _, _),
	user:assertz('$global_key'(K, V)).

bb_get(K, V) :-
	must_be(K, atom, _, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), bb_get/2))),
	!.

bb_del(K) :-
	must_be(K, atom, _, _),
	user:retractall('$global_key'(K, _)),
	!.
bb_del(_).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

current_op(A, B, C) :- var(A), var(B), var(C),
	!, '$load_ops', '$current_op'(A, B, C).
current_op(_, _, C) :- nonvar(C), \+ atom(C),
	!, throw(error(type_error(atom,C), current_op/3)).
current_op(_, B, _) :- nonvar(B), \+ atom(B),
	!, throw(error(domain_error(operator_specifier, B), current_op/3)).
current_op(_, B, _) :- nonvar(B),
	\+ memberchk(B,[xf, yf, fx, fy, xfx, xfy, yfx]),
	!, throw(error(domain_error(operator_specifier, B), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ integer(A),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ (A >= 0),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ (A =< 1200),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, B, C) :-
	!, '$load_ops', '$current_op'(A, B, C).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note: Trealla doesn't support goal_expansion (yet) so fake it
% based on calling M:put_atts(V, AccessSpec) and active Module

put_atts(Var, Value) :-
	module(Module),
	put_attr(Var, Module, Value).

get_atts(Var, Value) :-
	module(Module),
	(var(Value) ->
		( get_att(Var, List),
			findall(F, (Template =.. [Module,F], member(Template, List)), Value)
		)
	;
		get_attr(Var, Module, Value)
	).

del_atts(Var) :-
	module(Module),
	del_attr(Var, Module).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

put_attr(Var, Module, Value) :-
	var(Var),
	Attr =.. [Module,Value],
	put_att(Var, +Attr).

get_attr(Var, Module, Value) :-
	var(Var),
	Attr =.. [Module,Value],
	get_att(Var, +Attr).

del_attr(Var, Module) :-
	var(Var),
	Attr =.. [Module,_],
	put_att(Var, -Attr).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

:- use_module(library(dict)).

put_att(Var, +Attr) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	dict:set(D, Module-(Functor/Arity), Attr, D2),
	'$put_attrs'(Var, D2).

put_att(Var, -Attr) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	dict:del(D, Module-(Functor/Arity), D2),
	'$put_attrs'(Var, D2).

put_att(Var, Attr) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	dict:set(D, Module-(Functor/Arity), Attr, D2),
	'$put_attrs'(Var, D2).

get_att(Var, L) :- var(L), !,
	'$get_attrs'(Var, D),
	dict:match(D, _, L).

get_att(Var, +(Attr)) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	dict:get(D, Module-(Functor/Arity), Attr).

get_att(Var, -Attr) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	\+ dict:get(D, Module-(Functor/Arity), _).

get_att(Var, Attr) :- !,
	'$get_attrs'(Var, D),
	Attr =.. [Module,Value],
	functor(Value,Functor,Arity),
	dict:get(D, Module-(Functor/Arity), Attr).

attributed(Var) :-
	'$get_attrs'(Var, D),
	D \= [].

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is bidirectional!

atomic_list_concat(L, Atom) :-
	atomic_list_concat(L, '', Atom).

atomic_list_concat([], _, Atom) :-
	!, Atom = ''.
atomic_list_concat(L, Sep, Atom) :-
	( atom(Sep), ground(L), is_list(L) )
	->  list_atom(L, Sep, Atom)
	;   ( atom(Sep), atom(Atom) )
	->  atom_list(Atom, Sep, L)
	;   instantiation_error(atomic_list_concat_(L, Sep, Atom)).

list_atom([Word],  _, Word).
list_atom([Word|L], Sep, Atom) :-
	list_atom(L, Sep, Right),
	atomic_concat(Sep, Right, Right1),
	atomic_concat(Word, Right1, Atom),
	!.

atom_list(Atom, Sep, [Word|L]) :-
	sub_atom(Atom, X, N, _, Sep),
	sub_atom(Atom, 0, X, _, Word),
	Z is X + N,
	sub_atom(Atom, Z, _, 0, Rest),
	!,
	atom_list(Rest, Sep, L).
atom_list(Atom, _, [Atom]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plus(X,Y,S) :- nonvar(X), nonvar(Y),
	must_be(X, integer, _, _), must_be(Y, integer, _, _), !,
	S is X + Y.
plus(X,Y,S) :- nonvar(X), var(Y), nonvar(S),
	must_be(X, integer, _, _), must_be(S, integer, _, _), !,
	Y is S - X.
plus(X,Y,S) :- var(X), nonvar(Y), nonvar(S),
	must_be(S, integer, _, _), must_be(Y, integer, _, _), !,
	X is S - Y.
plus(_,_,_) :-
	throw(error(instantiation_error, plus/3)).

succ(X,S) :- nonvar(X), Y=1, nonvar(Y),
	must_be(X, integer, _, _), must_be(Y, integer, _, _), !,
	(X >= 0 -> true ; throw(error(domain_error(not_less_than_zero, X), succ/2))),
	S is X + Y.
succ(X,S) :- var(X), Y=1, nonvar(Y), nonvar(S),
	must_be(S, integer, _, _), must_be(Y, integer, _, _), !,
	(S >= 0 -> true ; throw(error(domain_error(not_less_than_zero, S), succ/2))),
	!,
	S > 0,
	X is S - Y.
succ(_,_) :-
	throw(error(instantiation_error, succ/2)).
