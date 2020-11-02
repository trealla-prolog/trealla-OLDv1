:-initialization(main).

main :-
	phrase(format_("~16r", [12]), Ls1), writeln(Ls1),
	phrase(format_("~8r", [12]), Ls2), writeln(Ls2),
	halt.
