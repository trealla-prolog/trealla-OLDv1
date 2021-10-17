:- initialization(main).
:- use_module(library(lists)).

main :-
	member(I, [A,B,B,A]),
	writeln(I), fail.
main.

