:- initialization(main).

main :-
    maplist(col([[1,2],[3,4]]), [1,2], X),
    writeln(X),
    halt.

col(Matrix, N, Column) :-
    maplist(nth1(N), Matrix, Column).
