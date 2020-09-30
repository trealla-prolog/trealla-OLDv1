:- module(phrase, [
	phrase/2, phrase/3,
	phrase_from_file/2, phrase_from_file/3 ].

phrase(P, Ls) :-
	call(P, Ls, []).

phrase(P, Ls, Rest) :-
	call(P, Ls, Rest).

phrase_from_file(P, Filename) :-
	phrase_from_file(P, Filename, []).

phrase_from_file(P, Filename, Opts) :-
	setup_call_cleanup(
		open(Filename, read, Str, [mmap(Ls)|Opts]),
		phrase(P, Ls),
		close(Str)
	).
