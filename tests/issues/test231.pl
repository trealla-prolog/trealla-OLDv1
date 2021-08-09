:- initialization(main).

main :-
	write_term(T,[quoted(true),variable_names(['N'=T])]),
	nl,
	halt.
