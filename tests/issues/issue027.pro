:-initialization(main).

main :-
	[a,b] \== [a,c],
	write(ok), nl,
	halt.
main :-
	write(nok), nl,
	halt.

