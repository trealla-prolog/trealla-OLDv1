:- initialization(main).

main :-
	a(X) =.. [Y|Z],
	writeln(Y), writeln(Z).
