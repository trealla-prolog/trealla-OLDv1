main :-
	L=[aa,bb,cc],L=[_|T],copy_term(T,T2),
	writeln(T2).

:- initialization(main).
