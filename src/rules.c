make_rule(m, "format(F) :- format(F, []).");
make_rule(m, "unify_with_occurs_check(X, X) :- acyclic_term(X).");

make_rule(m,
	"'$append'([], L, L)."										\
	"'$append'([H|T], L, [H|R]) :- '$append'(T, L, R).");

make_rule(m, "predicate_property(P, A) :- "						\
	"'$mustbe_callable'(P), "									\
	"'$load_properties', "										\
	"(var(A) -> true ; "										\
	" (memberchk(A, [built_in,control_construct,discontiguous,private,static,dynamic,persist,multifile,meta_predicate(_)]) -> "							\
		"true ; "												\
		"throw(error(domain_error(predicate_property,A),P)) "	\
		")"														\
	"), "														\
	"'$predicate_property'(P, A).");

make_rule(m, "subsumes_term(G,S) :- "							\
	"\\+ \\+ ( "												\
	" term_variables(S, V1), "									\
	" G = S, "													\
	" term_variables(V1, V2), "									\
	" V2 == V1"													\
	").");

make_rule(m, "setup_call_cleanup(S,G,C) :- "					\
	"copy_term('$setup_call_cleanup'(S,G,C),TMP_G), "			\
	"'$call'(TMP_G), "											\
	"'$setup_call_cleanup'(S,G,C)=TMP_G.");

make_rule(m, "'$setup_call_cleanup'(S,G,C) :- "					\
	"'$call'((S,!)), "											\
	"'$register_cleanup'((C,!)), "								\
	"catch(G,Err, "												\
	" (catch((\\+ \\+ C),_,true),throw(Err))"					\
	"), "														\
	"'$chk_is_det'.");

make_rule(m,
	"partial_string(S,P) :- '$append'(S,_,P)."					\
	"partial_string(S,P,V) :- '$append'(S,V,P).");

make_rule(m, "forall(Cond,Action) :- \\+ (Cond, \\+ Action).");

make_rule(m, "chars_base64(Plain,Base64,_) :- base64(Plain,Base64).");
make_rule(m, "chars_urlenc(Plain,Url,_) :- urlenc(Plain,Url).");

// merge...

make_rule(m, "merge([], R, R) :- !.");
make_rule(m, "merge(R, [], R) :- !.");
make_rule(m, "merge([H1|T1], [H2|T2], Result) :- "				\
	"compare(Delta, H1, H2), !, "								\
	"merge(Delta, H1, H2, T1, T2, Result).");

make_rule(m, "merge(>, H1, H2, T1, T2, [H2|R]) :- "				\
	"merge([H1|T1], T2, R).");
make_rule(m, "merge(=, H1, _, T1, T2, [H1|R]) :- "				\
	"merge(T1, T2, R).");
make_rule(m, "merge(<, H1, H2, T1, T2, [H1|R]) :- "				\
	"merge(T1, [H2|T2], R).");

// sort...

make_rule(m, "sort(L, R) :- "									\
	"'$mustbe_instantiated'(L, R), "							\
	"'$mustbe_list'(L), "										\
	"'$mustbe_list_or_var'(R), "								\
	"length(L,N), "												\
	"sort(N, L, _, R).");

make_rule(m, "sort(2, [X1, X2|L], L, R) :- !, "					\
	"compare(Delta, X1, X2), "									\
	"'$sort2'(Delta, X1, X2, R).");
make_rule(m, "sort(1, [X|L], L, [X]) :- !.");
make_rule(m, "sort(0, L, L, []) :- !.");
make_rule(m, "sort(N, L1, L3, R) :- "							\
	"N1 is N // 2, "											\
	"plus(N1, N2, N), "											\
	"sort(N1, L1, L2, R1), "									\
	"sort(N2, L2, L3, R2), "									\
	"merge(R1, R2, R).");

make_rule(m, "'$sort2'(<, X1, X2, [X1, X2]).");
make_rule(m, "'$sort2'(=, X1, _,  [X1]).");
make_rule(m, "'$sort2'(>, X1, X2, [X2, X1]).");

// mmerge...

make_rule(m, "mmerge([], R, R) :- !.");
make_rule(m, "mmerge(R, [], R) :- !.");
make_rule(m, "mmerge([H1|T1], [H2|T2], Result) :- "				\
	"compare(Delta, H1, H2), !, "								\
	"mmerge(Delta, H1, H2, T1, T2, Result).");

make_rule(m, "mmerge(>, H1, H2, T1, T2, [H2|R]) :- "			\
	"mmerge([H1|T1], T2, R).");
make_rule(m, "mmerge(=, H1, H2, T1, T2, [H1|R]) :- "			\
	"mmerge(T1, [H2|T2], R).");
make_rule(m, "mmerge(<, H1, H2, T1, T2, [H1|R]) :- "			\
	"mmerge(T1, [H2|T2], R).");

// msort...

make_rule(m, "msort(L, R) :- "									\
	"'$mustbe_instantiated'(L, R), "							\
	"'$mustbe_list'(L), "										\
	"'$mustbe_list_or_var'(R), "								\
	"length(L,N), "												\
	"msort(N, L, _, R).");

make_rule(m, "msort(2, [X1, X2|L], L, R) :- !, "				\
	"compare(Delta, X1, X2), "									\
	"'$msort2'(Delta, X1, X2, R).");
make_rule(m, "msort(1, [X|L], L, [X]) :- !.");
make_rule(m, "msort(0, L, L, []) :- !.");
make_rule(m, "msort(N, L1, L3, R) :- "							\
	"N1 is N // 2, "											\
	"plus(N1, N2, N), "											\
	"msort(N1, L1, L2, R1), "									\
	"msort(N2, L2, L3, R2), "									\
	"mmerge(R1, R2, R).");

make_rule(m, "'$msort2'(<, X1, X2, [X1, X2]).");
make_rule(m, "'$msort2'(=, X1, X2, [X1, X2]).");
make_rule(m, "'$msort2'(>, X1, X2, [X2, X1]).");

// keymerge...

make_rule(m, "keymerge([], R, R) :- !.");
make_rule(m, "keymerge(R, [], R) :- !.");
make_rule(m, "keymerge([H1|T1], [H2|T2], Result) :- "			\
	"keycompare(Delta, H1, H2), !, "							\
	"keymerge(Delta, H1, H2, T1, T2, Result).");

make_rule(m, "keymerge(>, H1, H2, T1, T2, [H2|R]) :- "			\
	"keymerge([H1|T1], T2, R).");
make_rule(m, "keymerge(=, H1, H2, T1, T2, [H1|R]) :- "			\
	"keymerge(T1, [H2|T2], R).");
make_rule(m, "keymerge(<, H1, H2, T1, T2, [H1|R]) :- "			\
	"keymerge(T1, [H2|T2], R).");

// keysort...

make_rule(m, "keycompare(Delta, (K1-_), (K2-_)) :- "			\
	"(K1 @< K2 -> Delta = '<' ; "								\
	"(K1 @> K2 -> Delta = '>' ; "								\
	"Delta = '=')).");

make_rule(m, "keysort(L, R) :- "								\
	"'$mustbe_instantiated'(L, R), "							\
	"'$mustbe_pairlist'(L), "									\
	"'$mustbe_pairlist_or_var'(R), "							\
	"length(L,N), "												\
	"keysort(N, L, _, R).");

make_rule(m, "keysort(2, [X1, X2|L], L, R) :- !, "				\
	"keycompare(Delta, X1, X2), "								\
	"'$msort2'(Delta, X1, X2, R).");
make_rule(m, "keysort(1, [X|L], L, [X]) :- !.");
make_rule(m, "keysort(0, L, L, []) :- !.");
make_rule(m, "keysort(N, L1, L3, R) :- "						\
	"N1 is N // 2, "											\
	"plus(N1, N2, N), "											\
	"keysort(N1, L1, L2, R1), "									\
	"keysort(N2, L2, L3, R2), "									\
	"keymerge(R1, R2, R).");

make_rule(m, "findall(T, G, B) :- "								\
	"copy_term('$findall'(T,G,B),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$findall'(T,G,B)=TMP_G.");

make_rule(m, "findall(T, G, B, Tail) :- "						\
	"copy_term('$findall'(T,G,B0),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$findall'(T,G,B0)=TMP_G,"									\
	"'$mustbe_list_or_var'(B),"									\
	"'$mustbe_list_or_var'(Tail),"								\
	"'$append'(B0, Tail, B), !.");

make_rule(m, "bagof(T,G,B) :- "									\
	"copy_term('$bagof'(T,G,_),TMP_G),"							\
	"'$call'(TMP_G),"											\
	"'$bagof'(T,G,B)=TMP_G.");

make_rule(m, "setof(T,G,B) :- "									\
	"copy_term('$bagof'(T,G,_),TMP_G),"							\
	"'$call'(TMP_G),"											\
	"'$bagof'(T,G,TMP_B)=TMP_G,"								\
	"sort(TMP_B,B).");

make_rule(m, "catch(G,E,C) :- "									\
	"copy_term('$catch'(G,E,C),TMP_G),"							\
	"'$call'(TMP_G),"											\
	"'$catch'(G,E,C)=TMP_G.");

// calln...

make_rule(m, "call(G) :- "										\
	"copy_term('$call'(G),TMP_G),"								\
	"'$call'(TMP_G),"											\
	"'$call'(G)=TMP_G.");

make_rule(m, "call(G,P1) :- "									\
	"copy_term('$call'(G,P1),TMP_G),"							\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1)=TMP_G.");

make_rule(m, "call(G,P1,P2) :- "								\
	"copy_term('$call'(G,P1,P2),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2)=TMP_G.");

make_rule(m, "call(G,P1,P2,P3) :- "								\
	"copy_term('$call'(G,P1,P2,P3),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2,P3)=TMP_G.");

make_rule(m, "call(G,P1,P2,P3,P4) :- "							\
	"copy_term('$call'(G,P1,P2,P3,P4),TMP_G),"					\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2,P3,P4)=TMP_G.");

make_rule(m, "call(G,P1,P2,P3,P4,P5) :- "						\
	"copy_term('$call'(G,P1,P2,P3,P4,P5),TMP_G),"				\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2,P3,P4,P5)=TMP_G.");
make_rule(m, "call(G,P1,P2,P3,P4,P5,P6) :- "					\
	"copy_term('$call'(G,P1,P2,P3,P4,P5,P6),TMP_G),"			\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2,P3,P4,P5,P6)=TMP_G.");

make_rule(m, "call(G,P1,P2,P3,P4,P5,P6,P7) :- "					\
	"copy_term('$call'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),"			\
	"'$call'(TMP_G),"											\
	"'$call'(G,P1,P2,P3,P4,P5,p6,P7)=TMP_G.");

// taskn...

make_rule(m, "task(G) :- "										\
	"copy_term('$task'(G),TMP_G),"								\
	"'$call'(TMP_G),"											\
	"'$task'(G)=TMP_G.");

make_rule(m, "task(G,P1) :- "									\
	"copy_term('$task'(G,P1),TMP_G),"							\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1)=TMP_G.");

make_rule(m, "task(G,P1,P2) :- "								\
	"copy_term('$task'(G,P1,P2),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2)=TMP_G.");

make_rule(m, "task(G,P1,P2,P3) :- "								\
	"copy_term('$task'(G,P1,P2,P3),TMP_G),"						\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2,P3)=TMP_G.");

make_rule(m, "task(G,P1,P2,P3,P4) :- "							\
	"copy_term('$task'(G,P1,P2,P3,P4),TMP_G),"					\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2,P3,P4)=TMP_G.");

make_rule(m, "task(G,P1,P2,P3,P4,P5) :- "						\
	"copy_term('$task'(G,P1,P2,P3,P4,P5),TMP_G),"				\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2,p3,P4,P5)=TMP_G.");

make_rule(m, "task(G,P1,P2,P3,P4,P5,P6) :- "					\
	"copy_term('$task'(G,P1,P2,P3,P4,P5,P6),TMP_G),"			\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2,P3,P4,P5,P6)=TMP_G.");

make_rule(m, "task(G,P1,P2,P3,P4,P5,P6,P7) :- "					\
	"copy_term('$task'(G,P1,P2,P3,P4,P5,P6,P7),TMP_G),"			\
	"'$call'(TMP_G),"											\
	"'$task'(G,P1,P2,P3,P4,P5,P6,P7)=TMP_G.");

// phrase...

make_rule(m, "phrase_from_file(P, Filename) :- "				\
	" open(Filename, read, Str, [mmap(Ms)]),"					\
	" copy_term(P, P2), P2=P,"									\
	" phrase(P2, Ms, []),"										\
	" close(Str).");

make_rule(m, "phrase_from_file(P, Filename, Opts) :- "			\
	" open(Filename, read, Str, [mmap(Ms)|Opts])," 				\
	" copy_term(P, P2), P2=P,"									\
	" phrase(P2, Ms, []),"										\
	" close(Str).");

make_rule(m, "phrase(GRBody, S0) :-"							\
	"phrase(GRBody, S0, []).");

make_rule(m, "phrase(GRBody, S0, S) :-"							\
	" ( var(GRBody) -> "										\
	" throw(error(instantiation_error, phrase/3))"				\
	" ; dcg_constr(GRBody) -> phrase_(GRBody, S0, S)"			\
	" ; functor(GRBody, _, _) -> call(GRBody, S0, S)"			\
	" ; throw(error(type_error(callable, GRBody), phrase/3))" 	\
	" ).");

make_rule(m, "phrase_([], S, S)."								\
	"phrase_(!, S, S)."											\
	"phrase_((A, B), S0, S) :-"									\
	"  phrase(A, S0, S1), phrase(B, S1, S)."					\
	"phrase_((A -> B ; C), S0, S) :-"							\
	" !,"														\
	" (phrase(A, S0, S1) ->"									\
	"  phrase(B, S1, S) ; phrase(C, S0, S)"						\
	" )."														\
	"phrase_((A ; B), S0, S) :-"								\
	" (phrase(A, S0, S) ; phrase(B, S0, S))." 					\
	"phrase_((A | B), S0, S) :-"								\
	" (phrase(A, S0, S) ; phrase(B, S0, S))." 					\
	"phrase_({G}, S0, S) :-"									\
	" (call(G), S0 = S)."										\
	"phrase_(call(G), S0, S) :-"								\
	" call(G, S0, S)."											\
	"phrase_((A -> B), S0, S) :-"								\
	" phrase((A -> B ; fail), S0, S)."							\
	"phrase_(phrase(NonTerminal), S0, S) :-"					\
	" phrase(NonTerminal, S0, S)."								\
	"phrase_([T|Ts], S0, S) :-"									\
	" '$append'([T|Ts], S, S0).");

// Canonical version... this is a start

make_rule(m, "phrase_to_stream(P, Stream) :- "					\
	" phrase(P, Chars, []),"									\
	" maplist(write(Stream), Chars).");

// Edinburgh...

make_rule(m, "tab(0) :- !.");
make_rule(m, "tab(N) :- put_code(32), M is N-1, tab(M).");
make_rule(m, "tab(_,0) :- !.");
make_rule(m, "tab(S,N) :- put_code(S,32), M is N-1, tab(S,M).");
make_rule(m, "get0(C) :- get_code(C).");
make_rule(m, "get0(S,C) :- get_code(S,C).");
make_rule(m, "display(T) :- write_canonical(T).");
make_rule(m, "display(S,T) :- write_canonical(S,T).");
make_rule(m, "put(C) :- put_code(C).");
make_rule(m, "put(S,C) :- put_code(S,C).");
make_rule(m, "see(F) :- open(F,read,S), set_input(S).");
make_rule(m, "tell(F) :- open(F,write,S), set_output(S).");
make_rule(m, "append(F) :- open(F,append,S), set_output(S).");

// SWI or GNU

make_rule(m, "current_key(K) :- var(K), clause('$record_key'(K,_),_).");
make_rule(m, "recorda(K,V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).");
make_rule(m, "recordz(K,V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).");
make_rule(m, "recorded(K,V) :- nonvar(K), clause('$record_key'(K,V),_).");
make_rule(m, "recorda(K,V,R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V),R).");
make_rule(m, "recordz(K,V,R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V),R).");
make_rule(m, "recorded(K,V,R) :- nonvar(K), clause('$record_key'(K,V),_,R).");

make_rule(m, "term_to_atom(T,S) :- write_term_to_chars(S,T,[]).");
make_rule(m, "write_term_to_atom(S,T,Opts) :- write_term_to_chars(S,Opts,T).");
make_rule(m, "read_term_from_atom(S,T,Opts) :- read_term_from_chars(S,Opts,T).");
make_rule(m, "absolute_file_name(R,A) :- absolute_file_name(R,A,[]).");

make_rule(m, "client(U,H,P,S) :- client(U,H,P,S,[]).");
make_rule(m, "server(H,S) :- server(H,S,[]).");

make_rule(m, 																	\
	"current_op(A,B,C) :- var(A), var(B), var(C), "								\
	"	!, '$load_ops', '$current_op'(A, B, C)."								\
	"current_op(_,_,C) :- nonvar(C), \\+ atom(C), "								\
	"	!, throw(error(type_error(atom,C),current_op/3))."						\
	"current_op(_,B,_) :- nonvar(B), \\+ atom(B), "								\
	"	!, throw(error(domain_error(operator_specifier,B),current_op/3))."		\
	"current_op(_,B,_) :- nonvar(B), "											\
	"	\\+ memberchk(B,[xf, yf, fx, fy, xfx, xfy, yfx]), " 					\
	"	!, throw(error(domain_error(operator_specifier,B),current_op/3))."		\
	"current_op(A,_,_) :- nonvar(A), "											\
	"	\\+ integer(A),	"														\
	"	!, throw(error(domain_error(operator_priority,A),current_op/3))."		\
	"current_op(A,_,_) :- nonvar(A), "											\
	"	\\+ (A >= 0), "															\
	"	!, throw(error(domain_error(operator_priority,A),current_op/3))."		\
	"current_op(A,_,_) :- nonvar(A), "											\
	"	\\+ (A =< 1200), "														\
	"	!, throw(error(domain_error(operator_priority,A),current_op/3))."		\
	"current_op(A,B,C) :- "														\
	"	!, '$load_ops', '$current_op'(A, B, C).");
