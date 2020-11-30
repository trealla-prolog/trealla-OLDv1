:- initialization(main).

a((foo(X,Y),baz(X,Y)), 1).

main :-
	findall(A-N, a(A, N), L),
	writeln(L),
	halt.
