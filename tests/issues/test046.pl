:-initialization(main).

main :-
	1 =.. L1, writeln(L1),
	aa =.. L2, writeln(L2),
	[aa] =.. L3, writeln(L3),
	[aa,bb] =.. L4, writeln(L4),
	[aa,bb,cc] =.. L5, writeln(L5).
