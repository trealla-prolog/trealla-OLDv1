:- initialization(main).

main :-
    prepare(List),
    writeln(List),
    sort(List, ListSorted),
    writeln(ListSorted),
    halt.

prepare([B,A]) :-
    A =.. [pair,2,X],
    B =.. [trio,3,Y,Z].
