:- pragma(builtins, [once]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are SICStus compatible...

must_be(Term, var, Goal, _Arg) :- !, (var(Term) -> true ; throw(error(instantiation_error(Term), Goal))), !.
must_be(Term, nonvar, Goal, _Arg) :- !, (nonvar(Term) -> true ; throw(error(uninstantiation_error(Term), Goal))), !.

must_be(Term, callable, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (callable(Term) -> true ; throw(error(type_error(callable, Term), Goal))), !.
must_be(Term, atom, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (atom(Term) -> true ; throw(error(type_error(atom, Term), Goal))), !.
must_be(Term, atomic, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (atomic(Term) -> true ; throw(error(type_error(atomic, Term), Goal))), !.
must_be(Term, integer, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (integer(Term) -> true ; throw(error(type_error(integer, Term), Goal))), !.
must_be(Term, float, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (float(Term) -> true ; throw(error(type_error(float, Term), Goal))), !.
must_be(Term, number, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (number(Term) -> true ; throw(error(type_error(number, Term), Goal))), !.
must_be(Term, ground, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (ground(Term) -> true ; throw(error(type_error(Term, ground), Goal))), !.
must_be(Term, compound, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (compound(Term) -> true ; throw(error(type_error(compound, Term), Goal))), !.
must_be(Term, list, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (is_list(Term) -> true ; throw(error(type_error(list, Term), Goal))), !.
must_be(Term, list_or_partial_list, Goal, _Arg) :- !, '$mustbe_instantiated'(Term, Goal), (is_list_or_partial_list(Term) -> true ; throw(error(type_error(list, Term), Goal))), !.

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

expand_term((H --> B), Out) :- !,
	dcg_translate((H --> B), Out), !.
expand_term(In, Out) :-
	term_expansion(In, Out).

unify_with_occurs_check(X, X) :-
	acyclic_term(X).

predicate_property(P, A) :-
	nonvar(P), atom(A), !,
	must_be(P, callable, predicate_property/2, _),
	'$legacy_predicate_property'(P, A).
predicate_property(P, A) :-
	'$load_properties',
	(	var(A)
	->	true
	; 	(	(Controls = [built_in,control_construct,discontiguous,private,static,dynamic,persist,multifile,meta_predicate(_)],
			memberchk(A, Controls))
		->	true
		;	throw(error(domain_error(predicate_property, A), P))
		)
	),
	must_be(P, callable, predicate_property/2, _),
	'$predicate_property'(P, A).

current_prolog_flag(P, A) :-
	nonvar(P), !,
	'$legacy_current_prolog_flag'(P, A).
current_prolog_flag(P, A) :-
	'$load_flags',
	'$current_prolog_flag'(P, A).

subsumes_term(G, S) :-
	\+ \+ (
	 term_variables(S, V1),
	 unify_with_occurs_check(G, S),
	 term_variables(V1, V2),
	 V2 == V1
	).

variant(Term1, Term2) :-
	% avoid trouble in any shared variables
	copy_term(Term1, Term1Copy),
	copy_term(Term2, Term2Copy),
	% ground and compare the rule copies
	numbervars(Term1Copy, 0, N),
	numbervars(Term2Copy, 0, N),
	Term1Copy == Term2Copy.

deterministic(Goal, Det) :-
	call_cleanup(Goal, Det = true),
	(	var(Det)
	->	Det = false
	;	true
	).

call_cleanup(G, C) :-
	'$register_cleanup'(ignore(C)),
	'$catch2'(('$get_level'(Before), G), Err, (catch((\+ \+ call(C)), _, true), throw(Err))),
	'$chk_is_det'(Before).

setup_call_cleanup(S, G, C) :-
	once(S),
	'$register_cleanup'(ignore(C)),
	'$catch2'(('$get_level'(Before), G), Err, (catch((\+ \+ call(C)), _, true), throw(Err))),
	'$chk_is_det'(Before).

catch(G, E, C) :-
	call('$catch'(G, E, C)).

findall(T, G, B, Tail) :-
	'$mustbe_list_or_var'(B),
	'$mustbe_list_or_var'(Tail),
	findall(T, G, B0),
	append(B0, Tail, B), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Derived from code by R.A. O'Keefe

:- meta_predicate(setof(-,0,?)).
:- meta_predicate(bagof(-,0,?)).

setof(Template, Generator, Set) :-
    ( 	var(Set)
    -> 	true
    ; 	must_be(Set, list_or_partial_list, setof/3, _)
    ),
	'$bagof'(Template, Generator, Bag),
	sort(Bag, Set).

bagof(Template, Generator, Bag) :-
    (	var(Bag)
		-> true
		; must_be(Bag, list_or_partial_list, bagof/3, _)
	),
	'$bagof'(Template, Generator, Bag).

'$bagof'(Template, Generator, Bag) :-
	'$free_variables'(Generator, Template, [], Vars, 1),
	Vars \== [],
	!,
	Key =.. [(.)|Vars],
	functor(Key, (.), N),
	findall(Key-Template,Generator,Recorded),
	'$replace_instance'(Recorded, Key, N, _, OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	'$concordant_subset'(Gamut, Key, Answer),
	Bag = Answer.
'$bagof'(Template, Generator, Bag) :-
	findall(Template, Generator, Bag0),
	Bag0 \== [],
	Bag = Bag0.

_^Goal :- Goal.

'$replace_instance'([], _, _, _, []) :- !.
'$replace_instance'([NewKey-Term|Xs], Key, NVars, Vars, [NewKey-Term|NewBag]) :-
	'$replace_key_variables'(NVars, Key, Vars, NewKey), !,
	'$replace_instance'(Xs, Key, NVars, Vars, NewBag).


%   Original R.A. O'Keefe comment:
%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

'$replace_key_variables'(0, _, _, _) :- !.
'$replace_key_variables'(N, OldKey, Vars0, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	'$replace_variables'(Arg, Vars0, Vars1),
	M is N-1,
	'$replace_key_variables'(M, OldKey, Vars1, NewKey).
'$replace_key_variables'(N, OldKey, Vars, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	'$replace_key_variables'(M, OldKey, Vars, NewKey).

'$replace_variables'(Term, [Var|Vars], Vars) :-
	var(Term), !,
	Term = Var.
'$replace_variables'(Term, Vars, Vars) :-
	atomic(Term), !.
'$replace_variables'(Term, Vars0, Vars) :-
	functor(Term, _, Arity),
	'$replace_variables_term'(Arity, Term, Vars0, Vars).

'$replace_variables_term'(0, _, Vars, Vars) :- !.
'$replace_variables_term'(N, Term, Vars0, Vars) :-
	arg(N, Term, Arg),
	'$replace_variables'(Arg, Vars0, Vars1),
	N1 is N-1,
	'$replace_variables_term'(N1, Term, Vars1, Vars).


/*
%   '$concordant_subset'([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.
*/

'$concordant_subset'([Key-Val|Rest], Clavis, Answer) :-
	'$concordant_subset'(Rest, Key, List, More),
	'$concordant_subset'(More, Key, [Val|List], Clavis, Answer).

/*
%   '$concordant_subset'(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.
*/

'$concordant_subset'([Key-Val|Rest], Clavis, List, More) :-
	subsumes_term(Key, Clavis),
	subsumes_term(Clavis, Key),
	!,
	Key = Clavis,
	List = [Val|Rest2],
	'$concordant_subset'(Rest, Clavis, Rest2, More).
'$concordant_subset'(More, _, [], More).

/*
%   '$concordant_subset'/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.
*/

'$concordant_subset'([],   Key, Subset, Key, Subset) :- !.
'$concordant_subset'(_,    Key, Subset, Key, Subset).
'$concordant_subset'(More, _,   _,   Clavis, Answer) :-
	'$concordant_subset'(More, Clavis, Answer).

% 0 disables use of '$explicit_binding', 1 enables them
% setof stuff still uses 1, that's closer to it's usual implementation
'$free_variables'(A,B,C,D) :- '$free_variables'(A,B,C,D,0).

% ---extracted from: not.pl --------------------%

%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation

%   In order to handle variables properly, we have to find all the
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
% a)  they occur in the template
% b)  they are bound by X^P, setof, or bagof
%   '$free_variables'(Generator, Template, OldList, NewList,CheckBindings=0,1)
%   finds this set, using OldList as an accumulator.

'$free_variables'(Term, Bound, VarList, [Term|VarList],_) :-
	var(Term),
	'$term_is_free_of'(Bound, Term),
	'$list_is_free_of'(VarList, Term),
	!.
'$free_variables'(Term, _, VarList, VarList,_) :-
	var(Term),
	!.
'$free_variables'(Term, Bound, OldList, NewList, 1) :-
	'$explicit_binding'(Term, Bound, NewTerm, NewBound),
	!,
	'$free_variables'(NewTerm, NewBound, OldList, NewList, 1).
'$free_variables'(Term, Bound, OldList, NewList, _) :-
	functor(Term, _, N),
	'$free_variables'(N, Term, Bound, OldList, NewList, 0).

'$free_variables'(0,    _,     _, VarList, VarList, _) :- !.
'$free_variables'(N, Term, Bound, OldList, NewList, B) :-
	arg(N, Term, Argument),
	'$free_variables'(Argument, Bound, OldList, MidList, B),
	M is N-1, !,
	'$free_variables'(M, Term, Bound, MidList, NewList, B).

%   '$explicit_binding' checks for goals known to existentially quantify
%   one or more variables.  In particular "not" is quite common.

'$explicit_binding'(\+(_),     Bound, fail, Bound ).
'$explicit_binding'(not(_),    Bound, fail, Bound ).
'$explicit_binding'(Term^Goal, Bound, Goal, Bound+Vars) :-
	term_variables(Term, Vars).
'$explicit_binding'(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var).
'$explicit_binding'(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var).

'$term_is_free_of'(Term, Var) :-
	var(Term), !,
	Term \== Var.
'$term_is_free_of'(Term, Var) :-
	functor(Term, _, N),
	'$term_is_free_of'(N, Term, Var).

'$term_is_free_of'(0, _, _) :- !.
'$term_is_free_of'(N, Term, Var) :-
	arg(N, Term, Argument),
	'$term_is_free_of'(Argument, Var),
	M is N-1, !,
	'$term_is_free_of'(M, Term, Var).

'$list_is_free_of'([], _).
'$list_is_free_of'([Head|Tail], Var) :-
	Head \== Var,
	'$list_is_free_of'(Tail, Var).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

sort(Term, _) :-
	var(Term),
	throw(error(instantiation_error, sort/2)).
sort([], []) :- !.
sort([X, Y| Xs], Ys) :- !,
	'$sort_split'([X, Y| Xs], X1s, X2s),
	sort(X1s, Y1s),
	sort(X2s, Y2s),
	'$sort_merge'(Y1s, Y2s, Ys0),
	Ys = Ys0.
sort([X], [X]) :- !.
sort(Term, _) :-
	Term \== [],
	throw(error(type_error(list,Term), sort/2)).
sort(_, Term) :-
	throw(error(type_error(list,Term), sort/2)).

'$sort_merge'([X| Xs], [Y| Ys], [X| Zs]) :-
	X == Y, !,
	'$sort_merge'(Xs, Ys, Zs).
'$sort_merge'([X| Xs], [Y| Ys], [X| Zs]) :-
	X @< Y, !,
	'$sort_merge'(Xs, [Y| Ys], Zs).
'$sort_merge'([X| Xs], [Y| Ys], [Y| Zs]) :-
	X @> Y, !,
	'$sort_merge'([X | Xs], Ys, Zs).
'$sort_merge'([], Xs, Xs) :- !.
'$sort_merge'(Xs, [], Xs).

'$sort_split'('-', _, _) :-
	throw(error(instantiation_error, sort/2)).
'$sort_split'([], [], []).
'$sort_split'([X| Xs], [X| Ys], Zs) :-
	'$sort_split'(Xs, Zs, Ys).

msort(Term, _) :-
	var(Term),
	throw(error(instantiation_error, msort/2)).
msort([], []) :- !.
msort([X, Y| Xs], Ys) :- !,
	'$sort_split'([X, Y| Xs], X1s, X2s),
	msort(X1s, Y1s),
	msort(X2s, Y2s),
	'$msort_merge'(Y1s, Y2s, Ys0),
	Ys = Ys0.
msort([X], [X]) :- !.
msort(Term, _) :-
	Term \== [],
	throw(error(type_error(list,Term), msort/2)).
msort(_, Term) :-
	throw(error(type_error(list,Term), msort/2)).

'$msort_merge'([X| Xs], [Y| Ys], [X| Zs]) :-
	X @=< Y, !,
	'$msort_merge'(Xs, [Y| Ys], Zs).
'$msort_merge'([X| Xs], [Y| Ys], [Y| Zs]) :-
	X @> Y, !,
	'$msort_merge'([X | Xs], Ys, Zs).
'$msort_merge'([], Xs, Xs) :- !.
'$msort_merge'(Xs, [], Xs).

samsort(L, R) :- msort(L, R).

keysort(List, Sorted) :-
	keysort(List, List, Sorted, []).

keysort([Term| _], _, _, _) :-
	var(Term),
	throw(error(instantiation_error, keysort/2)).
keysort([Key-X| Xs], List, Ys, YsTail) :-
	!,
	'$key_partition'(Xs, Key, List, Left, EQ, EQT, Right),
	keysort(Left, List,  Ys, [Key-X|EQ]),
	keysort(Right, List, EQT, YsTail).
keysort([], _, Ys, Ys) :-
	!.
keysort([Term| _], _, _, _) :-
	throw(error(type_error(pair,Term), keysort/2)).
keysort(Term, List, _, _) :-
	Term \== [],
	throw(error(type_error(list,List), keysort/2)).
keysort(_, _, Sorted, _) :-
	Sorted \= [_|_],
	throw(error(type_error(list,Sorted), keysort/2)).
keysort(_, _, [Term| _], _) :-
	Term \= _-_,
	throw(error(type_error(pair,Term), keysort/2)).
keysort(_, _, Sorted, _) :-
	throw(error(type_error(list,Sorted), keysort/2)).

'$key_partition'([Term| _], _, _, _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, keysort/2)).
'$key_partition'([XKey-X| Xs], YKey, List, [XKey-X| Ls], EQ, EQT, Rs) :-
	XKey @< YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([XKey-X| Xs], YKey, List, Ls, [XKey-X| EQ], EQT, Rs) :-
	XKey == YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([XKey-X| Xs], YKey, List, Ls, EQ, EQT, [XKey-X| Rs]) :-
%	XKey @> YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([], _, _, [], EQT, EQT, []) :-
	!.
'$key_partition'([Term| _], _, _, _, _, _, _) :-
	throw(error(type_error(pair,Term), keysort/2)).
'$key_partition'(_, _, List, _, _, _, _) :-
	throw(error(type_error(list,List), keysort/2)).

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
	(	var(GRBody)
	-> throw(error(instantiation_error, phrase/3))
	;	(	dcg_constr(GRBody)
		->	phrase_(GRBody, S0, S)
		;	(	functor(GRBody, _, _)
			->	call(GRBody, S0, S)
			;	throw(error(type_error(callable, GRBody), phrase/3))
			)
		)
	).

phrase_([], S, S).
	phrase_(!, S, S).
phrase_((A, B), S0, S) :-
	phrase(A, S0, S1), phrase(B, S1, S).
phrase_((A -> B ; C), S0, S) :-
	!,
	(	phrase(A, S0, S1)
	->	phrase(B, S1, S)
	;	phrase(C, S0, S)
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
file_exists(F) :- exists_file(F).
directory_exists(F) :- exists_directory(F).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

not(G) :- G, !, fail.
not(_).

forall(Cond, Action) :-
	\+ (Cond, \+ Action).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_key(K) :- var(K), '$record_key'(K,_).
recorda(K, V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).
recordz(K, V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).
recorded(K, V) :- nonvar(K), '$record_key'(K,V).
recorda(K, V, R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V), R).
recordz(K, V, R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V), R).
recorded(K, V, R) :- nonvar(K), clause('$record_key'(K,V), _, R).

format(F) :- format(F, []).
partial_string(S, P) :- append(S, _, P).
partial_string(S, P, V) :- append(S, V, P).
chars_base64(Plain, Base64,_) :- base64(Plain, Base64).
chars_urlenc(Plain, Url, _) :- urlenc(Plain, Url).
term_to_atom(T, S) :- write_term_to_chars(S, T, []).
write_term_to_atom(S, T, Opts) :- write_term_to_chars(S, Opts, T).
read_term_from_atom(S, T, Opts) :- read_term_from_chars(S, Opts, T).
absolute_file_name(R, A) :- absolute_file_name(R, A, []).
client(U, H, P, S) :- client(U,H,P,S,[]).
server(H, S) :- server(H,S,[]).
prolog_load_context(module, Module) :- module(Module).
open(F, M, S) :- open(F, M, S, []).
load_files(Files) :- load_files(Files,[]).
consult(Files) :- load_files(Files,[]).
strip_module(T,M,P) :- T=M:P -> true ; P=T, module(M).
?=(X,Y) :- \+ unifiable(X,Y,[_|_]).

member(X, [X|T]) :- T == [], !.
member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

append(A, L, L) :- A == [], !.
append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

unifiable(T1, T2, Gs) :-
	copy_term('$unifiable'(T1,T2,Gs), G0),
	'$rawcall'(G0),
	'$unifiable'(T1,T2,Gs)=G0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI compatible
%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...

nb_setval(K, _) :-
	must_be(K, atom, nb_setval/2, _),
	user:retract('$global_key'(K, _)),
	fail.
nb_setval(K, V) :-
	must_be(K, atom, nb_setval/2, _),
	user:assertz('$global_key'(K, V)).

nb_getval(K, V) :-
	must_be(K, atom, nb_getval/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), nb_getval/2))),
	!.

nb_delete(K) :-
	must_be(K, atom, nb_delete/1, _),
	user:retract('$global_key'(K, _)),
	!.
nb_delete(_).

nb_current(K, V) :-
	user:clause('$global_key'(K, V), nb_current/2).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI compatible
%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...
% The following is not really correct.

b_setval(K, _) :-
	must_be(K, atom, b_setval/2, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
b_setval(K, V) :-
	must_be(K, atom, b_setval/2, _),
	user:asserta('$global_key'(K, V)).
b_setval(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_setval0(K, _) :-
	must_be(K, atom, b_setval0/2, _),
	\+ user:clause('$global_key'(K, _), _), asserta('$global_key'(K, 0)),
	fail.
b_setval0(K, V) :-
	must_be(K, atom, b_setval0/2, _),
	user:asserta('$global_key'(K, V)).
b_setval0(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_getval(K, V) :-
	must_be(K, atom, b_getval/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), b_getval/2))),
	!.

b_delete(K) :-
	must_be(K, atom, b_delete/1, _),
	user:retractall('$global_key'(K, _)),
	!.
b_delete(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SICStus compatible

bb_b_put(K, _) :-
	must_be(K, atom, bb_b_put/2, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
bb_b_put(K, V) :-
	must_be(K, atom, bb_b_put/2, _),
	user:asserta('$global_key'(K, V)).
bb_b_put(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

bb_b_del(K) :-
	must_be(K, atom, bb_b_del/1, _),
	user:retract('$global_key'(K, _)),
	!.
bb_b_del(_).

bb_put(K, _) :-
	must_be(K, atom, bb_put/2, _),
	user:retract('$global_key'(K, _)),
	fail.
bb_put(K, V) :-
	must_be(K, atom, bb_put/2, _),
	user:assertz('$global_key'(K, V)).

bb_get(K, V) :-
	must_be(K, atom, bb_get/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), bb_get/2))),
	!.

bb_delete(K, V) :-
	must_be(K, atom, bb_delete/2, _),
	user:retract('$global_key'(K, V)),
	!.

bb_update(K, O, V) :-
	must_be(K, atom, bb_update/3, _),
	user:retract('$global_key'(K, O)),
	user:assertz('$global_key'(K, V)),
	!.

bb_del(K) :-
	must_be(K, atom, bb_del/1, _),
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
% SWI compatible

put_attr(Var, Module, Value) :-
	var(Var),
	Attr =.. [Module,Value],
	put_atts(Var, +Attr).

get_attr(Var, Module, Value) :-
	var(Var),
	Attr =.. [Module,Value],
	get_atts(Var, +Attr).

del_attr(Var, Module) :-
	var(Var),
	Attr =.. [Module,_],
	put_atts(Var, -Attr).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SICStus compatible

:- use_module(library(dict)).

put_atts(_, []) :- !.
put_atts(Var, [H|T]) :- !,
	put_atts(Var, H),
	put_atts(Var, T).

put_atts(Var, -Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	(	var(Value)
	->	Functor = Value
	; 	functor(Value, Functor, _)
	),
	dict:del(D, Module-Functor, D2),
	(	D2 = []
	->	'$erase_attributes'(Var)
	;	'$write_attributes'(Var, D2)
	).

put_atts(Var, +Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	functor(Value, Functor, _),
	dict:set(D, Module-Functor, Attr, D2),
	'$write_attributes'(Var, D2).

put_atts(Var, Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	functor(Value, Functor, _),
	dict:set(D, Module-Functor, Attr, D2),
	'$write_attributes'(Var, D2).

get_atts(Var, L) :- var(L), !,
	'$read_attributes'(Var, D),
	dict:match(D, _, L).

get_atts(Var, -Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	catch(functor(Value, Functor, _), _, true),
	\+ dict:get(D, Module-Functor, _).

get_atts(Var, +Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	catch(functor(Value, Functor, _), _, true),
	dict:get(D, Module-Functor, Attr).

get_atts(Var, Attr) :- !,
	'$read_attributes'(Var, D),
	Attr =.. [Module,Value],
	catch(functor(Value, Functor, _), _, true),
	dict:get(D, Module-Functor, Attr).

del_atts(Var) :-
	var(Var),
	'$erase_attribute'(Var).

attvar(Var) :-
	'$read_attributes'(Var, D),
	D \= [].

attributed(Var) :-
	'$read_attributes'(Var, D),
	D \= [].

term_attvars_([], VsIn, VsIn) :- !.
term_attvars_([H|T], VsIn, VsOut) :-
	(	attvar(H)
	->	term_attvars_(T, [H|VsIn], VsOut)
	;	term_attvars_(T, VsIn, VsOut)
	).

term_attvars(Term, Vs) :-
	term_variables(Term, Vs0),
	term_attvars_(Vs0, [], Vs).

collect_goals_(_, [], GsIn, GsIn) :- !.
collect_goals_(V, [H|T], GsIn, GsOut) :-
	H =.. [M,_Value],
	catch(M:attribute_goals(V, Goal, _), _, Goal = put_atts(V,+H)),
	collect_goals_(V, T, [Goal|GsIn], GsOut).

collect_goals_([], GsIn, GsIn) :- !.
collect_goals_([V|T], GsIn, GsOut) :-
	get_atts(V, Ls),
	collect_goals_(V, Ls, GsIn, GsOut2),
	collect_goals_(T, GsOut2, GsOut).

copy_term(Term, Copy, Gs) :-
	copy_term(Term, Copy),
	term_attvars(Copy, CopyVs),
	collect_goals_(CopyVs, [], Gs),
	'$strip_attributes'(CopyVs).

% Debugging...

portray_atts(Term) :-
	copy_term(Term, Copy, Gs),
	Term = Copy,
	write_term(user_output, Gs, [varnames(true)]), nl.

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is bidirectional!

atomic_list_concat(L, Atom) :-
	atomic_list_concat(L, '', Atom).

atomic_list_concat([], _, []) :- !.
atomic_list_concat(L, Sep, Atom) :-
	(	(atom(Sep), ground(L), is_list(L))
	->	list_atom(L, Sep, Atom)
	;   (	(atom(Sep), atom(Atom))
		->  atom_list(Atom, Sep, L)
		;   instantiation_error(atomic_list_concat_(L, Sep, Atom))
		)
	).

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
	must_be(X, integer, plus/3, _), must_be(Y, integer, plus/3, _), !,
	S is X + Y.
plus(X,Y,S) :- nonvar(X), var(Y), nonvar(S),
	must_be(X, integer, plus/3, _), must_be(S, integer, plus/3, _), !,
	Y is S - X.
plus(X,Y,S) :- var(X), nonvar(Y), nonvar(S),
	must_be(S, integer, plus/3, _), must_be(Y, integer, plus/3, _), !,
	X is S - Y.
plus(_,_,_) :-
	throw(error(instantiation_error, plus/3)).

succ(X,S) :- nonvar(X), Y=1, nonvar(Y),
	must_be(X, integer, succ/2, _), must_be(Y, integer, succ/2, _), !,
	(	X >= 0
	->	true
	; 	throw(error(domain_error(not_less_than_zero, X), succ/2))
	),
	S is X + Y.
succ(X,S) :- var(X), Y=1, nonvar(Y), nonvar(S),
	must_be(S, integer, succ/2, _), must_be(Y, integer, succ/2, _), !,
	(	S >= 0
	->	true
	; 	throw(error(domain_error(not_less_than_zero, S), succ/2))
	),
	!,
	S > 0,
	X is S - Y.
succ(_,_) :-
	throw(error(instantiation_error, succ/2)).
