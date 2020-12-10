:- initialization(main).

:- dynamic(legs/2).
legs(A, 7) :- A, call(A).

main :-
	clause(legs(C,7), Body),
	Body == (call(C),call(C)),
	write(succeeded), nl,
	halt.

main :-
	write(failed), nl,
	halt.
