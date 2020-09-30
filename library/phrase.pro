phrase(P, Ls) :-
	phrase(P, Ls, []).

phrase_from_file(P, Filename) :-
	phrase_from_file(P, Filename, []).

phrase_from_file(P, Filename, Opts) :-
	setup_call_cleanup(
		open(Filename, read, Str, [mmap(Ls)|Opts]),
		phrase(P, Ls, []),
		close(Str)
	).
