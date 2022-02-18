:- module(lists, [
	member/2, select/3, selectchk/3, subtract/3, union/3,
	intersection/3, reverse/2, append/2, nth/3, nth1/3, nth0/3,
	last/2, flatten/2, append/3, same_length/2, sum_list/2,
	toconjunction/2, numlist/3, length/2
	]).

/*  Parts of this file are...

    Copyright (c)  2018-2021, Mark Thom
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

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

reverse(L1, L2) :- revzap_(L1, [], L2).

revzap_([], L, L) :- !.
revzap_([H|L], L2, L3) :- revzap_(L, [H|L2], L3).

append(ListOfLists, List) :-
	must_be(ListOfLists, list, _, _),
	append_(ListOfLists, List).

append_([], []) :- !.
append_([L|Ls], As) :-
	append(L, Ws, As),
	append_(Ls, Ws).

nth(1, [H|_], H).
nth(N, [_|T], H) :- nth(M, T, H), N is M + 1.

nth1(1, [H|_], H).
nth1(N, [_|T], H) :- nth1(M, T, H), N is M + 1.

nth0(0, [H|_], H).
nth0(N, [_|T], H) :- nth0(M, T, H), N is M + 1.

last_([], Last, Last).
last_([X|Xs], _, Last) :- last_(Xs, X, Last).

last([X|Xs], Last) :- last_(Xs, X, Last).

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
same_length([_|As], [_|Bs]) :-
        same_length(As, Bs).

sum_list(Ls, S) :-
        foldl(lists:sum_, Ls, 0, S).

sum_(L, S0, S) :- S is S0 + L.

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
	must_be(integer, L),
	must_be(integer, U),
	L =< U,
	numlist_(L, U, Ns).

numlist_(U, U, List) :-
	!,
	List = [U].
numlist_(L, U, [L|Ns]) :-
	L2 is L+1,
	numlist_(L2, U, Ns).

length(Xs0, N) :-
   '$skip_max_list'(M, N, Xs0, Xs),
   !,
   (  Xs == [] -> N = M
   ;  nonvar(Xs) -> var(N), throw(error(resource_error(finite_memory),length/2))
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
