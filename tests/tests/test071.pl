:- initialization(main).
:- use_module(library(lists)).

main :-
	member(I, [A,B,B,A]),
	writeq(I), nl, fail.
main.

