:- initialization(main).

d(a(_), foo/1, 1).
d(a(_), bar/2, 2).
d(a(_), baz/3, 5).

main :-
	D = _,
	bagof(N, d(D, F/A, N), Ns),
	writeq(D-F/A-Ns), nl,
	fail.
main :-
	halt.
