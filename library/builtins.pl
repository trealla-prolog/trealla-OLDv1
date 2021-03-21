'$append'([], L, L).
'$append'([H|T], L, [H|R]) :- '$append'(T, L, R).

unify_with_occurs_check(X, X) :- acyclic_term(X).

predicate_property(P, A) :-
	'$mustbe_callable'(P),
	'$load_properties',
	(var(A) -> true ;
	 (memberchk(A, [built_in,control_construct,discontiguous,private,static,dynamic,persist,multifile,meta_predicate(_)]) ->
		true ;
		throw(error(domain_error(predicate_property,A),P))
		)
	),
	'$predicate_property'(P, A).

subsumes_term(G,S) :-
	\+ \+ (
	 term_variables(S, V1),
	 G = S,
	 term_variables(V1, V2),
	 V2 == V1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

call_cleanup(G,C) :-
	setup_call_cleanup(true, G, C).

setup_call_cleanup(S,G,C) :-
	copy_term('$setup_call_cleanup'(S,G,C),TMP_G),
	'$call'(TMP_G),
	'$setup_call_cleanup'(S,G,C)=TMP_G.

'$setup_call_cleanup'(S,G,C) :-
	'$call'((S,!)),
	'$register_cleanup'((C,!)),
	catch(G,Err,
	 (catch((\+ \+ C),_,true),throw(Err))
	),
	'$chk_is_det'.

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partial_string(S,P) :- '$append'(S,_,P).
partial_string(S,P,V) :- '$append'(S,V,P).

forall(Cond,Action) :- \+ (Cond, \+ Action).

chars_base64(Plain,Base64,_) :- base64(Plain,Base64).
chars_urlenc(Plain,Url,_) :- urlenc(Plain,Url).

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
	'$mustbe_instantiated'(L, R),
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

msort(L, R) :-
	'$mustbe_instantiated'(L, R),
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
	'$mustbe_instantiated'(L, R),
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

findall(T, G, B) :-
	copy_term('$findall'(T,G,B),TMP_G),
	'$call'(TMP_G),
	'$findall'(T,G,B)=TMP_G.

findall(T, G, B, Tail) :-
	copy_term('$findall'(T,G,B0),TMP_G),
	'$call'(TMP_G),
	'$findall'(T,G,B0)=TMP_G,
	'$mustbe_list_or_var'(B),
	'$mustbe_list_or_var'(Tail),
	'$append'(B0, Tail, B), !.

bagof(T,G,B) :-
	copy_term('$bagof'(T,G,B),TMP_G),
	'$call'(TMP_G),
	'$bagof'(T,G,B)=TMP_G.

setof(T,G,B) :-
	copy_term('$bagof'(T,G,_),TMP_G),
	'$call'(TMP_G),
	'$bagof'(T,G,TMP_B)=TMP_G,
	sort(TMP_B,B).

catch(G,E,C) :-
	copy_term('$catch'(G,E,C),TMP_G),
	'$call'(TMP_G),
	'$catch'(G,E,C)=TMP_G.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

call(G) :-
	copy_term('$call'(G),TMP_G),
	'$call'(TMP_G),
	'$call'(G)=TMP_G.

call(G,P1) :-
	copy_term('$call'(G,P1),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1)=TMP_G.

call(G,P1,P2) :-
	copy_term('$call'(G,P1,P2),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2)=TMP_G.

call(G,P1,P2,P3) :-
	copy_term('$call'(G,P1,P2,P3),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2,P3)=TMP_G.

call(G,P1,P2,P3,P4) :-
	copy_term('$call'(G,P1,P2,P3,P4),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2,P3,P4)=TMP_G.

call(G,P1,P2,P3,P4,P5) :-
	copy_term('$call'(G,P1,P2,P3,P4,P5),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2,P3,P4,P5)=TMP_G.
call(G,P1,P2,P3,P4,P5,P6) :-
	copy_term('$call'(G,P1,P2,P3,P4,P5,P6),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2,P3,P4,P5,P6)=TMP_G.

call(G,P1,P2,P3,P4,P5,P6,P7) :-
	copy_term('$call'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),
	'$call'(TMP_G),
	'$call'(G,P1,P2,P3,P4,P5,p6,P7)=TMP_G.

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

task(G) :-
	copy_term('$task'(G),TMP_G),
	'$call'(TMP_G),
	'$task'(G)=TMP_G.

task(G,P1) :-
	copy_term('$task'(G,P1),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1)=TMP_G.

task(G,P1,P2) :-
	copy_term('$task'(G,P1,P2),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2)=TMP_G.

task(G,P1,P2,P3) :-
	copy_term('$task'(G,P1,P2,P3),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2,P3)=TMP_G.

task(G,P1,P2,P3,P4) :-
	copy_term('$task'(G,P1,P2,P3,P4),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2,P3,P4)=TMP_G.

task(G,P1,P2,P3,P4,P5) :-
	copy_term('$task'(G,P1,P2,P3,P4,P5),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2,p3,P4,P5)=TMP_G.

task(G,P1,P2,P3,P4,P5,P6) :-
	copy_term('$task'(G,P1,P2,P3,P4,P5,P6),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2,P3,P4,P5,P6)=TMP_G.

task(G,P1,P2,P3,P4,P5,P6,P7) :-
	copy_term('$task'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),
	'$call'(TMP_G),
	'$task'(G,P1,P2,P3,P4,P5,P6,P7)=TMP_G.

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
	 '$append'([T|Ts], S, S0).

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
tab(_,0) :- !.
tab(S,N) :- put_code(S,32), M is N-1, tab(S,M).
get0(C) :- get_code(C).
get0(S,C) :- get_code(S,C).
display(T) :- write_canonical(T).
display(S,T) :- write_canonical(S,T).
put(C) :- put_code(C).
put(S,C) :- put_code(S,C).
see(F) :- open(F,read,S), set_input(S).
tell(F) :- open(F,write,S), set_output(S).
append(F) :- open(F,append,S), set_output(S).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_key(K) :- var(K), clause('$record_key'(K,_),_).
recorda(K,V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).
recordz(K,V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).
recorded(K,V) :- nonvar(K), clause('$record_key'(K,V),_).
recorda(K,V,R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V),R).
recordz(K,V,R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V),R).
recorded(K,V,R) :- nonvar(K), clause('$record_key'(K,V),_,R).

format(F) :- format(F, []).
term_to_atom(T,S) :- write_term_to_chars(S,T,[]).
write_term_to_atom(S,T,Opts) :- write_term_to_chars(S,Opts,T).
read_term_from_atom(S,T,Opts) :- read_term_from_chars(S,Opts,T).
absolute_file_name(R,A) :- absolute_file_name(R,A,[]).
client(U,H,P,S) :- client(U,H,P,S,[]).
server(H,S) :- server(H,S,[]).
set_random(seed(Seed)) :- set_seed(Seed).
set_random(seed(random)) :- time(Seed), set_seed(Seed).
maybe :- random(F), F < 0.5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Global variables. This a quick hack using assert/retract...

nb_setval(K,_) :-
	'$mustbe_atom'(K),
	retract('$global_key'(K, _)),
	fail.
nb_setval(K,V) :-
	'$mustbe_atom'(K),
	assertz('$global_key'(K, V)).

nb_getval(K,V) :-
	'$mustbe_atom'(K),
	catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), nb_setval/2))).

nb_delete(K) :-
	'$mustbe_atom'(K),
	retract('$global_key'(K, _)),
	!.
nb_delete(_).

nb_current(K, V) :-
	clause('$global_key'(K, V), _).

% The following is not really correct. Need to setup a hook
% so as to reverse the assignment to handle a cut...

b_setval(K,_) :-
	'$mustbe_atom'(K),
	\+ clause('$global_key'(K, _), _), asserta('$global_key'(K, [])),
	fail.
b_setval(K,V) :-
	'$mustbe_atom'(K),
	asserta('$global_key'(K, V)).
b_setval(K,_) :-
	retract('$global_key'(K, _)),
	!.

b_setval0(K,_) :-
	'$mustbe_atom'(K),
	\+ clause('$global_key'(K, _), _), asserta('$global_key'(K, 0)),
	fail.
b_setval0(K,V) :-
	'$mustbe_atom'(K),
	asserta('$global_key'(K, V)).
b_setval0(K,_) :-
	retract('$global_key'(K, _)),
	!.

b_getval(K,V) :-
	'$mustbe_atom'(K),
	catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), b_setval/2))),
	!.

b_delete(K) :-
	'$mustbe_atom'(K),
	retractall('$global_key'(K, _)),
	!.
b_delete(_).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

current_op(A,B,C) :- var(A), var(B), var(C),
	!, '$load_ops', '$current_op'(A, B, C).
current_op(_,_,C) :- nonvar(C), \+ atom(C),
	!, throw(error(type_error(atom,C),current_op/3)).
current_op(_,B,_) :- nonvar(B), \+ atom(B),
	!, throw(error(domain_error(operator_specifier,B),current_op/3)).
current_op(_,B,_) :- nonvar(B),
	\+ memberchk(B,[xf, yf, fx, fy, xfx, xfy, yfx]),
	!, throw(error(domain_error(operator_specifier,B),current_op/3)).
current_op(A,_,_) :- nonvar(A),
	\+ integer(A),
	!, throw(error(domain_error(operator_priority,A),current_op/3)).
current_op(A,_,_) :- nonvar(A),
	\+ (A >= 0),
	!, throw(error(domain_error(operator_priority,A),current_op/3)).
current_op(A,_,_) :- nonvar(A),
	\+ (A =< 1200),
	!, throw(error(domain_error(operator_priority,A),current_op/3)).
current_op(A,B,C) :-
	!, '$load_ops', '$current_op'(A, B, C).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
