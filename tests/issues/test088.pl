:- initialization(main).

:- op(600, xfy, ::).

a::b.

X::Y :-
	X = a,
	Y = b.

main :- X::Y, writeln(['X=',X,'Y=',Y]).
