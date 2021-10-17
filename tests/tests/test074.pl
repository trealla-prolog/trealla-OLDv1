:- initialization(main).

test(G, Error, Context) :- catch(G, error(Error,Context), writeln(ok)).

main :-
	test(term_variables(t,[_,_|a]), E, C),
	write(E), nl,
	write(C), nl.
