:- initialization(main).

:- use_module(library(lists)).

main :-
	setof(I, member(I, [A,B,B,A]), Set), writeln(Set),
	bagof(I, member(I, [A,B,B,A]), Bag), writeln(Bag).
