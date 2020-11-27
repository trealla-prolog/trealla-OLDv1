:- initialization(main).

p(a(_,_)).
p(b(_)).
p(c(_,_,_)).

test1 :-
	bagof(P, p(P), Ps),
	write(Ps), nl,
	halt.

test2 :-
	setof(P, p(P), Ps),
	write(Ps), nl,
	halt.

main :-
	test1, test2,
	halt.
