:- module(lists, [
		member/2, memberchk/2,
		select/3, selectchk/3,
		append/2, append/3,
		subtract/3, union/3, intersection/3,
		nth1/3, nth0/3,
		last/2, flatten/2, same_length/2,
		sum_list/2, prod_list/2, max_list/2, min_list/2,
		toconjunction/2, numlist/3,
		length/2, reverse/2
	]).

reverse(Xs, Ys) :-
    (	nonvar(Xs)
	->	reverse_(Xs, Ys, [], Xs)
    ;	reverse_(Ys, Xs, [], Ys)
    ).

reverse_([], [], YsRev, YsRev).
reverse_([_|Xs], [Y1|Ys], YsPreludeRev, Xss) :-
    reverse_(Xs, Ys, [Y1|YsPreludeRev], Xss).

append([], []).
append([L0|Ls0], Ls) :-
    append(L0, Rest, Ls),
    append(Ls0, Rest).

append([], R, R).
append([X|L], R, [X|S]) :- append(L, R, S).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

memberchk(X, Xs) :- member(X, Xs), !.

select(X, [X|T], T).
select(X, [H|T], [H|Rest]) :- select(X, T, Rest).

selectchk(X, L, Rest) :- select(X, L, Rest), !.

subtract([], _, []) :- !.
subtract([H|T], L2, L3) :- memberchk(H, L2), !, subtract(T, L2, L3).
subtract([H|T1], L2, [H|T3]) :- subtract(T1, L2, T3).

union([], L, L).
union([H|T], Y, Z):- member(H, Y), !, union(T, Y, Z).
union([H|T], Y, [H|Z]):- union(T, Y, Z).

intersection([], _, []).
intersection([H|T], Y, [H|Z]) :- member(H, Y), !, intersection(T, Y, Z).
intersection([_|T], Y, Z) :- intersection(T, Y, Z).

nth1(N, List, Head) :-
    nonvar(N),
    must_be(N, integer, nth1/3, _),
    (N < 0 -> throw(error(domain_error(not_less_than_zero), nth1/3)) ; true),
    nth1_(N, List, Head),
    !.
nth1(N, List, Head) :-
	nth1_(N, List, Head).

nth1_(1, [Head|_], Head).
nth1_(N, [_|Tail], Elem) :-
    nonvar(N),
    N > 0,
    M is N-1,
    nth1_(M, Tail, Elem),
    !.
nth1_(N,[_|T],Item) :-
    var(N),
    nth1_(M,T,Item),
    N is M + 1.

nth0_(0, [Head|_], Head).
nth0_(N, [_|Tail], Elem) :-
    nonvar(N),
    N > 0,
    M is N-1,
    nth0_(M, Tail, Elem),
    !.
nth0_(N,[_|T],Item) :-
    var(N),
    nth0_(M,T,Item),
    N is M + 1.

nth0(N, Es0, E) :-
	nonvar(N),
    must_be(N, integer, nth0/3, _),
    (N < 0 -> throw(error(domain_error(not_less_than_zero), nth0/3)) ; true),
	'$skip_max_list'(N, N, Es0,Es),
	!,
	Es = [E|_].
nth0(N, Es, E) :-
	nth0_(N, Es, E).

last([X|Xs], Last) :- last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :- last_(Xs, X, Last).

flatten(List, FlatList) :-
    flatten_(List, [], FlatList0),
    !,
    FlatList = FlatList0.

flatten_(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.
flatten_([], Tl, Tl) :- !.
flatten_([Hd|Tl], Tail, List) :-
    !,
    flatten_(Hd, FlatHeadTail, List),
    flatten_(Tl, Tail, FlatHeadTail).
flatten_(NonList, Tl, [NonList|Tl]).

same_length([], []).
same_length([_|As], [_|Bs]) :- same_length(As, Bs).

sum_list(Xs, Sum) :-
	sum_list_(Xs, 0, Sum).

sum_list_([], Sum0, Sum) :-
	Sum = Sum0.
sum_list_([X|Xs], Sum0, Sum) :-
	Sum1 is Sum0 + X,
	sum_list_(Xs, Sum1, Sum).

prod_list(Xs, Prod) :-
	prod_list_(Xs, 1, Prod).

prod_list_([], Prod0, Prod) :-
	Prod = Prod0.
prod_list_([X|Xs], Prod0, Prod) :-
	Prod1 is Prod0 * X,
	prod_list_(Xs, Prod1, Prod).

max_list([H|T], Max) :-
	max_list_(T, H, Max).
max_list([], _) :- fail.

max_list_([], Max0, Max) :-
	Max = Max0.
max_list_([H|T], Max0, Max) :-
	Max1 is max(H, Max0),
	max_list_(T, Max1, Max).

min_list([H|T], Min) :-
	min_list_(T, H, Min).
min_list([], _) :- fail.

min_list_([], Min0, Min) :-
	Min = Min0.
min_list_([H|T], Min0, Min) :-
	Min1 is min(H, Min0),
	min_list_(T, Min1, Min).

toconjunction(List0, Goal) :-
	reverse(List0, List),
	toconjunction_(List, true, Goal).

toconjunction_([], In, In).
toconjunction_([H|T], true, Out) :- !,
	Out2 = H,
	toconjunction_(T, Out2, Out).
toconjunction_([H|T], In, Out) :-
	Out2 = (H, In),
	toconjunction_(T, Out2, Out).

numlist(L, U, Ns) :-
	must_be(L, integer, numlist/3, _),
	must_be(U, integer, numlist/3, _),
	L =< U,
	numlist_(L, U, Ns).

numlist_(U, U, List) :-
	!,
	List = [U].
numlist_(L, U, [L|Ns]) :-
	L2 is L+1,
	numlist_(L2, U, Ns).

length(Xs0, N) :-
   '$skip_max_list'(M, N, Xs0,Xs),
   !,
   (  Xs == [] -> N = M
   ;  nonvar(Xs) -> var(N), Xs = [_|_], throw(error(resource_error(finite_memory),length/2))
   ;  nonvar(N) -> R is N-M, length_rundown(Xs, R)
   ;  N == Xs -> throw(error(resource_error(finite_memory),length/2))
   ;  length_addendum(Xs, N, M)
   ).
length(_, N) :-
   integer(N), !,
   domain_error(not_less_than_zero, N, length/2).
length(_, N) :-
   type_error(integer, N, length/2).

length_addendum([], N, N).
length_addendum([_|Xs], N, M) :-
    M1 is M + 1,
    length_addendum(Xs, N, M1).

length_rundown(Xs, 0) :- !, Xs = [].
length_rundown([_|Xs], N) :-
    N1 is N-1,
    length_rundown(Xs, N1).
