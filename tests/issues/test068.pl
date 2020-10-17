:- initialization(main).

main :-
    prepare(List),
    writeln(List),
    sort(List, ListSorted),
    writeln(ListSorted),
    halt.

prepare([A,B]) :-
    A =.. [pair,2,X],
    B =.. [trio,3,Y,Z].
