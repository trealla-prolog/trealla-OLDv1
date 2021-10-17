:-initialization(main).

main :-
	(between(1, 100000, _),
		assertz(hello(there)), false) ;
		setof(X, hello(X), Ls),
		writeln(Ls).
