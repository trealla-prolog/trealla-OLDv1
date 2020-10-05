:-initialization(main).

main :-
	append([a,b,c], "def", Ls),
	writeln(Ls),
	halt.
