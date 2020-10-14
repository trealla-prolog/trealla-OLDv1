:- initialization(main).

main :-
    maplist(col([[1,2],[3,4]]), [1,2], X1),
    writeln(X1),
    maplist(col([[A,2],[3,A]]), [1,2], X2),
    writeln(X2),
    halt.

col(Matrix, N, Column) :-
    maplist(nth1(N), Matrix, Column).
