:-initialization(main).

main :-
	a(b,c) =.. [_],
	halt.
main :-
	writeln(false),
	div([-10,0],fois([-4,0],[1,0])) =.. [F,X,Y],
	writeln(F),
	writeln(X),
	writeln(Y),
	writeln(ok).
