:- dynamic(p/1).

p(a) :-
    assertz(p(b)),
    p(b).
