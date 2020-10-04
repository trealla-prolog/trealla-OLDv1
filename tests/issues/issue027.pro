:-initialization(main).

main :-
	[a,b] \== [a,c],
	f(X) \== f(Y),
	write(ok), nl,
	halt.
main :-
	write(nok), nl,
	halt.

