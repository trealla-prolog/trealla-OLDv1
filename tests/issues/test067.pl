:- initialization(main).

main :-
	prepare(List),
    writeln(List),
    sort(List, ListSorted),
    writeln(ListSorted),
    halt.

prepare(List) :-
    append([A,B], [B,A], List).
