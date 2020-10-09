:-initialization(main).

main :-
	length(_, E), N is 2^E,
	writeln(E),
	length(Ls, N),
	maplist(=(a), Ls),
	writeln(ok),
	halt.
