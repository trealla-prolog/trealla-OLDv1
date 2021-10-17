main :-
	limit(5, offset(5, between(1,20,I))), writeln(I), fail.
main.

:- initialization(main).
