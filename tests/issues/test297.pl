main :-
	A=A-A,
	term_variables(A, L),
	write(L), nl.

:- initialization(main).
