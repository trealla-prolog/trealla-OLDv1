:- initialization(main).

main :- (true -> write(ok) ; write(nok)), nl, fail.
main :- (false -> write(nok) ; write(ok)), nl, fail.
main :- halt.
