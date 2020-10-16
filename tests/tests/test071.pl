main :-
	member(I, [A,B,B,A]),
	writeln(I),fail.

main :-
	halt.

:- initialization(main).
