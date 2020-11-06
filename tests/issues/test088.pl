:- initialization(main).

:- op(600, xfy, ::).

a::b.

X::Y :-
	X = c,
	Y = d.

main :- X::Y, writeln(['X=',X,'Y=',Y]), fail.
main :- halt.
