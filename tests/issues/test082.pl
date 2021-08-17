:-initialization(main).

main :-
	srandom(1000),
	random(X, 32),
	write(X), nl,
	srandom(1000),
	random(Y, 2^5),
	write(Y), nl,
	halt.
