:-initialization(main).
:- use_module(library(dcgs)).

as --> [].
as --> [a], as.

main :-
	length(_,E), writeln(E),
	N is 2^E, length(Ls, N),
	phrase(as, Ls),
	false.
