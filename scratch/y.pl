:- initialization(main).

clean_file(File, Path) :-
	absolute_file_name(File, Path),
	% the file can be associated with more than one stream
	forall(
		stream_property(Stream, file_name(Path)),
		close(Stream)
	),
	% the file may exist only if some unit test failed
	(	exists_file(Path) ->
		delete_file(Path)
	;	true
	).

set_text_input(Contents) :-
	clean_file('test_input.text', Path),
	open(Path, write, WriteStream, [type(text)]),
	write_text_contents(WriteStream, Contents),
	close(WriteStream),
	open(Path, read, ReadStream, [type(text)]),
	set_input(ReadStream).

get_text_contents(Stream, Expected, Contents) :-
	atom_length(Expected, Length),
	Limit is Length + 1,
	get_chars(Stream, Chars, Limit),
	atom_chars(Contents, Chars).

check_text_input(Expected) :-
	current_input(Stream),
	get_text_contents(Stream, Expected, Contents),
	clean_text_input,
	Expected == Contents.

main :-
	set_text_input('qwerty'),
	get_char(Char),
	Char == 'q',
	check_text_input('werty'),
	halt.

