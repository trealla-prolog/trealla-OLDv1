:-initialization(main).
:- use_module(library(dcgs)).

as --> [].
as --> [a], as.

main :-
	phrase(as, Ls),
	Ls = [a|_],
	writeln(Ls),
	halt.
