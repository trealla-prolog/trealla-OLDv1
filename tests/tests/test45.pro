:- initialization(main).

main :- (true -> write(ok1) ; write(nok1)), nl, fail.
main :- (false -> write(nok2) ; write(ok2)), nl, fail.
main :- halt.
