:-initialization(main).

main :-
		length(_, E),
		(E is 16 -> (write(ok), nl, halt) ; true),
		X is 2^E,
		write(X), nl,
		length(Ls, X),
		fail.
