:-initialization(main).

main :-
	phrase([], Ls), write(Ls), nl,
	phrase([a], Ls), write(Ls), nl,
	halt.
