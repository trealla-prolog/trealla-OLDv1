:- initialization(main).

:- dynamic(insect/1).

insect(ant).
insect(bee).

main :-
	retract(insect(I)),
		write(I), nl,
		retract(insect(bee)),
		fail.

main :-
	halt.
