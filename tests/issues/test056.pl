:-initialization(main).

main :-
	number_chars(0, Cs), sha256(Cs, S),
	writeln(S).
