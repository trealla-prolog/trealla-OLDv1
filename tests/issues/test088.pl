:- initialization(main).

:- op(600, xfy, ::).
:- op(600,  fy, ::).

a::b.

X::Y :-
	X = c,
	Y = d.

main :- X::Y, writeln(['X=',X,'Y=',Y]), fail.
main.
