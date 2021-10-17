:-initialization(main).

main :-
	setof(t, true, Ls),
	writeln(Ls),
	setof(tt, true, Ls2),
	writeln(Ls2).
