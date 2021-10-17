:- initialization(main).
:- use_module(library(apply)).

main :-
	length(_, E), N is 2^E,
	writeln(E),
	length(Ls, N),
	maplist(=(a), Ls),
	writeln(ok).
