:-initialization(main).

main :-
	_X \== a,
	write(ok), nl,
	halt.
main :-
	write(nok), nl,
	halt.

