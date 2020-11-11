:- initialization(main).

main :-
    solve(-1,X),
    Y is X,
    writeln(Y),
    halt.

solve(X,Y) :-
    Y = -X+1,
    writeln(Y).
