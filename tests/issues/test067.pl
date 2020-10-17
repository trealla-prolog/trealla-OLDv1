:- initialization(main).

main :-
    append([A,B], [B,A], List),
    writeln(List),
    sort(List, ListSorted),
    writeln(ListSorted),
    halt.
