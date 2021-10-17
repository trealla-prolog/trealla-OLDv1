:- initialization(main).
:- use_module(library(lists)).

main :-
	append("abc", "def", Ls),
	writeln(Ls).
