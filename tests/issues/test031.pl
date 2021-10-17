:-initialization(main).

main :-
	X = f(X), X == X,
	writeln(X).
