phrase_from_file(P, Filename) :-
	setup_call_cleanup(
		open(Filename, read, Str, [mmap(Ls)]),
		phrase(P, Ls, []),
		close(Str)
	).
