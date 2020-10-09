:-initialization(main).

main :-
		length(_, E),
		(E is 15 -> (write(ok), nl, halt) ; true),
		X is 2^E,
		write(X), nl,
		length(_Ls, X),
		fail.
