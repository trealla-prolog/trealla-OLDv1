:- dynamic(insect/1).

insect(ant).
insect(bee).

test :-
	findall(X, (insect(X), abolish(insect/1)), L),
	L == [ant, bee].
