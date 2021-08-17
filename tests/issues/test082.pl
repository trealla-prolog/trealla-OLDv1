:-initialization(main).

main :-
	srandom(1000),
	X is random(32),
	write(X), nl,
	srandom(1000),
	Y is random(2^5),
	write(Y), nl,
	halt.
