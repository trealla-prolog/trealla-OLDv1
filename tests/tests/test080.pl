:- use_module(library(freeze)).


main :-
	freeze(V1, W1=2),
	freeze(V2, W2=3),
	V1 = V2,
	V1 = 1,
	writeln([V1,V2,W1,W2]).

:- initialization(main).
