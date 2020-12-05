:- initialization(main).

test(G,Error,Context) :- catch(G,error(Error,Context),writeln([Error,Context])).

main :-
	test(term_variables(t,[_,_|a]),E,C),
	writeln(E),
	writeln(C),
	halt.
