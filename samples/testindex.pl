:- dynamic(g/2).
:- dynamic(f/2).
:- dynamic(f/1).

test1a :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test1a :-
	write('Match using atomic 1st-arg...'), nl,
	between(1,1000000,I),
		g(I,_),
		fail.
test1a :-
	abolish(g/2),
	write('Done... '), write(1000000), write(' items'), nl, true.

test1b :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test1b :-
	write('Match using atomic 2nd-arg...'), nl,
	between(1,1000000,I),
		g(_,I),
		fail.
test1b :-
	abolish(g/2),
	write('Done... '), write(1000000), write(' items'), nl, true.

test1c :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(g(f(I),I)),
		fail.
test1c :-
	write('Match using compound 1st-arg...'), nl,
	between(1,1000000,I),
		g(f(I),_),
		fail.
test1c :-
	abolish(g/2),
	write('Done... '), write(1000000), write(' items'), nl, true.

test1d :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(g(I,f(I))),
		fail.
test1d :-
	write('Match using compound 2nd-arg...'), nl,
	between(1,1000000,I),
		g(_,f(I)),
		fail.
test1d :-
	abolish(g/2),
	write('Done... '), write(1000000), write(' items'), nl, true.

test2a :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test2a :-
	write('Iterate over set...'), nl,
	f(_),
		fail.
test2a :-
	abolish(f/1),
	write('Done... '), write(1000000), write(' items'), nl, true.

test2b :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test2b :-
	write('Use findall...'), nl,
	findall(N,f(N),L),
	length(L,Count),
	write('Done... '), write(Count), write(' items'), nl,
	true.

test3 :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(g(I,I)),
		fail.
test3 :-
	write('Iterate over 2nd-arg...'), nl,
	g(_,_),
		fail.
test3 :-
	abolish(f/1),
	write('Done... '), write(1000000), write(' items'), nl,
	true.

test4 :-
	write('Load...'), nl,
	between(1,1000000,I),
		assertz(f(I)),
		fail.
test4 :-
	write('Retract...'), nl,
	retract(f(_)),
		fail.
test4 :-
	abolish(f/1),
	write('Done... '), write(1000000), write(' items'), nl, true.

test5 :-
	write('Load...'), nl,
	between(1,10,_),
		between(1,100000,J),
			assertz(f(J)),
			fail.
test5 :-
	write('Match using once 1st-arg...'), nl,
	between(1,100000,I),
		once(f(I)),
		%write(I), nl,
		fail.
test5 :-
	abolish(f/1),
	write('Done... '), write(100000), write(' items'), nl, true.

test6 :-
	assertz(ff(3)),
	assertz(ff(2)),
	assertz(ff(1)),
	ff(X),
	write(X), nl,
	fail.
test6 :-
	abolish(ff/1),
	true.

