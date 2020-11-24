Trealla ProLog
==============

A compact, efficient Prolog interpreter with ISO compliant aspirations.

	Integers are 64-bit (optionally 32 or 128-bit)
	Integer overflow detection
	Rationals are a native type
	Reals are double
	Atoms are UTF-8 of unlimited length
	The default double-quoted representation is *chars* list
	Full-term just-in-time indexing
	DCG via library(dcgs)
	format_//2 via library(format)
	REPL with history
	MIT licensed

Trealla is not WAM-based. It uses tree-walking, structure-sharing and
deep-binding.


Building
========

Written in plain-old C.

	git clone https://github.com/infradig/trealla.git
	cd trealla

On Debian systems you may need to install GNU readline:

	sudo apt install libreadline-dev

Then...

	make

Other systems may vary. There are no other dependencies except OpenSSL.
To build without OpenSSL:

	make NOSSL=1

Then...

	make test

You can build without linked-in library modules:

	make NOLDLIBS=1

and this is currently necessary with OSX. The only side-effect is to
make startup a little slower as they are pre-loaded from disk.

A 'make debug' build compiles in 0.2s with *tcc* and about 3s with
*clang* and *gcc*. Should build on any Unix-like system with a C99
compiler (could do C89 with a few cosmetic tweaks). Has been tested on
Manjaro, Ubuntu, Debian, FreeBSD and Raspbian (32 & 64-bit) systems.

On *BSD* systems use gmake.


Usage
=====

	tpl [options] [files] [-- args]

where options can be:

	-O0, --noopt       - no optimization
	-l file            - consult file
	-g goal            - query goal (only used once)
	--library path     - alt to TPL_LIBRARY_PATH env variable
	-t, --trace        - trace
	-q, --quiet        - quiet mode (no banner)
	-v, --version      - version
	-h, --help         - help
	-d, --daemonize    - daemonize
	-w, --watchdog     - create watchdog
	--stats            - print stats
	--consult          - consult from STDIN
	--noindex          - don't use term indexing
	--ns               - non-stop (don't drop to REPL)

For example:

	./tpl -g test2,halt samples/sieve

Invocation without any goal presents the REPL.

The default path to the library is relative to the executable location.


What's missing, problems?
=========================

	current_predicate/1 is non-backtrackable
	current_op/3 is non-backtrackable
	database is immediate update view, fix
	modules may need more work
	environment limit is 32K vars per frame
	implement attributed variables


Acknowledgements
================

This project started in March 2020 and it would not be where it is today
without help from these people:

Special thanks to [Xin Wang](https://github.com/dram) for providing the
testing framework.

Special thanks to [Markus Triska](https://github.com/triska) for
driving the use of packed UTF-8 strings for character-lists. For the
idea of mmap()-ing files as strings. For his rigorous approach to types
and for bug-checking. Also for use of his format_//2 library.

Special thanks to [Jos De Roo](https://github.com/josd) for his testing
against some classic Prolog examples and his EYE project.

Special thanks to [Christian Thaeter](https://github.com/cehteh) for his
ongoing work with code cleanup and development ideas.

Special thanks to [Paulo Moura](https://github.com/pmoura) for his patience
and sleuthing in the ongoing quest for Trealla to run his LogTalk project.


Strings
=======

Double-quoted strings, when *set_prolog_flag(double_quotes,chars)* is set
(which is the default) are stored as packed UTF-8 byte arrays. This is
compact and efficient. Such strings emulate a list representation and
from the programmer point of view are very much indistinguishable from
lists.

A good use of such strings is *open(filename,read,Str,[mmap(Ls))*
which gives a memory-mapped view of a file as a string *Ls*. List
operations on files are now essentially zero-overhead! DCG applications
will gain greatly (*phrase_from_file/[2-3]* uses this).


GNU-Prolog & SWI-Prolog
=======================

	between/3
	forall/2
	msort/2
	merge/3
	format/[1-3]			# needs library(format)
	predicate_property/2
	numbervars/[1,3-4]
	e/0
	sleep/1
	name/2
	tab/[1,2]

	maplist/[1-4]			# autoloaded from library(apply)
	foldl/[4-7]				# autoloaded from library(apply)

	read_term_from_atom/3	# use read_term_from_chars/3 instead
	write_term_to_atom/3	# use write_term_to_chars/3 instead
	term_to_atom/2			# use write_term_to_chars/3 instead

	random/1                # random(-float) float [0.0,<1.0]
	random/1                # random(+integer) function returning integer [0,<integer]

	freeze/2
	frozen/2

	put_attrs/2
	get_attrs/2


Others
======

	read_term_from_chars/2	# read_term_from_chars(+chars,-term)
	read_term_from_chars/3	# read_term_from_chars(+chars,+opts,-term)
	write_term_to_chars/3	# write_term_to_chars(+term,+opts,-chars)
	chars_base64/3			# currently options are ignored
	chars_urlenc/3			# currently options are ignored
	hex_chars/2             # as number_chars, but in hex
	octal_chars/2           # as number_chars, but in octal

	setup_call_cleanup/3
	findall/4
	atomic_concat/3
	var_number/2
	ignore/1
	is_list/1
	is_stream/1
	term_hash/2
	writeln/1
	time/1
	inf/0
	nan/0
	\uXXXX and \UXXXXXXXX quoted character escapes
	rational/1
	rationalize/1
	rdiv/2
	char_type/2
	code_type/2
	uuid/1					# generates non-standard UUID
	load_files/2
	split_atom/4
	plus/3

	call_nth/2
	offset/2
	limit/2

	getenv/2
	setenv/2
	unsetenv/1

	delete_file/1
	exists_file/1
	rename_file/2
	time_file/2
	size_file/2
	exists_directory/1
	make_directory/1
	working_directory/2
	chdir/1
	absolute_file_name/[2,3] # expand(Bool) & relative_to(file) options

	current_key/1
	recorda/2-3
	recordz/2-3
	recorded/2-3
	instance/2
	asserta/2
	assertz/2
	clause/3
	erase/1

	string_upper/2
	string_lower/2

	log10/1                 # function returning log10 of arg
	now/0                   # function returning C-time in secs as integer
	now/1                   # now (-integer) C-time in secs as integer
	get_time/1              # get_time(-variable) C-time in secs as float
	set_seed/1              # set_seed(+integer) set random number seed
	get_seed/1              # get_seed(-integer) get random number seed
	rand/0                  # function returning integer [0,RAND_MAX]
	rand/1                  # integer(-integer) integer [0,RAND_MAX]
	delay/1                 # delay(+integer) sleep for ms
	loadfile/2              # loadfile(+filename,-string)
	savefile/2              # savefile(+filename,+string)
	getfile/2               # getfile(+filename,-strings)
	getline/1               # getline(-string)
	getline/2               # getline(+stream,-string)
	bread/3                 # bread(+stream,?len,-string)
	bwrite/2                # bwrite(+stream,+string)
	replace/4               # replace(+string,+old,+new,-string)
	split/4                 # split(+string,+sep,?left,?right)
	sha1/2                  # sha1(+plaintext,?hash)        NEEDS OPENSSL
	sha256/2                # sha256(+plaintext,?hash)      NEEDS OPENSSL
	sha512/2                # sha512(+plaintext,?hash)      NEEDS OPENSSL

	open(stream(Str),...)   # with open/4 reopen a stream
	open(F,M,S,[mmap(Ls)])  # with open/4 mmap() the file to Ls

	persist/1               # directive 'persist funct/arity'

Note: consult/1 and load_files/2 support lists of files as args. Also
support loading into modules eg. *consult(MOD:FILE-SPEC)*.


Attributed variables		##NOT WORKING YET##
====================

	:- use_module(library(atts)).

	get_attr(V, Module, Value)
	put_attr(V, Module, Value)
	del_attr(V, Module)

	put_atts(V, +(A))
	put_atts(V, -(A))
	put_atts(V, A)
	get_atts(V, L)
	get_atts(V, +(A))
	get_atts(V, -(A))
	get_atts(V, A)

	attributed(V)


A simple dictionary
===================

	:- use_module(library(dict)).

	dict:set/4              # set(+dict,+name,+value,-dict)
	dict:del/3              # del(+dict,+name,-dict)
	dict:get/3              # get(+dict,+name,-value)
	dict:get/4              # get(+dict,+name,-value,+default)
	dict:lst/2              # lst(+dict,-values)


Definite Clause Grammars
========================

	:- use_module(library(dcgs)).

DCG rules are translated automatically if this module is included.


Format
======

	:- use_module(library(format)).

	format_//2
	format[2,3]


HTTP 1.1
========

	:- use_module(library(http)).

	http_get/3
	http_post/4
	http_put/4
	http_delete/3
	http_open/3


Networking					##EXPERIMENTAL##
==========

	server/2                # server(+host,-stream)
	server/3                # server(+host,-stream,+list)
	accept/2                # accept(+stream,-stream)
	client/4                # client(+url,-host,-path,-stream)
	client/5                # client(+url,-host,-path,-stream,+list)

The options list can include *udp(bool)* (default is false),
*nodelay(bool)* (default is true), *ssl(bool)* (default is false)
and *certfile(filespec)*.

The additional server options can include *keyfile(filespec)* and
*certfile(filespec)*. If just one concatenated file is supplied, use
*keyfile(filespec)* only.

The optional schemes 'http://' (the default) and 'https://' can be
provided in the client URL.

With *bread/3* the 'len' arg can be an integer > 0 meaning return that
many bytes, = 0 meaning return what is there (if non-blocking) or a variable
meaning return all bytes until end end of file,

Network SSL reading does not support get_code/get_char/peek_code/peek_char.


Persistence					##EXPERIMENTAL##
===========

Declaring something dynamic with the *persist* directive:

	:- persist :predindicator

causes that clause to be saved to a per-module database on update
(asserta/assertz/retract). Maybe this should be an option to
*dynamic/2*?


Concurrency					##EXPERIMENTAL##
===========

Trealla is single-threaded but cooperative multitasking is available
in the form of light-weight coroutines that run until they yield control,
either explicitly or implicitly (when waiting on input or a timer)...

	fork/0                  # parent fails, child continues
	spawn/[1-n]             # concurrent form of call/1-n
	yield/0                 # voluntarily yield control
	wait/0                  # parent should wait for children to finish
	await/0                 # parent should wait for a message
	send/1                  # apend term to parent queue
	recv/1                  # pop term from queue
	spawnlist/[1-n]         # concurrent form of maplist/1-n

Note: *send/1*, *sleep/1* and *delay/1* do implied yields. As does *getline/2*,
*bread/3*, *bwrite/2* and *accept/2*.

Note: *spawn/n* acts as if defined as:

	spawn(G) :- fork, call(G).
	spawn(G,P1) :- fork, call(G,P1).
	spawn(G,P1,P2) :- fork, call(G,P1,P2).
	...

In practice *spawn* calls a special version of *fork/0* that limits
the number of such concurrent tasks (see the *cpu_count* flag, initially
and artificially set at 4). Excess tasks will be scheduled as tasks finish.

An example:

```prolog
:-use_module(library(format)).
:-use_module(library(http)).

geturl(Url) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]),
	format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

% Fetch each URL in list sequentially...

test54 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(geturl,L),
	writeln('Finished').

% Fetch each URL in list concurrently (method 1)...

test55 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(spawn(geturl),L),
	wait,
	writeln('Finished').

% Fetch each URL in list concurrently (method 2)...

test56 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	spawnlist(geturl,L),
	writeln('Finished').
```

```console
$ ./tpl samples/test -g "time(test54),halt"
Job [www.google.com] 200 ==> www.google.com done
Job [www.bing.com] 200 ==> www.bing.com done
Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
Finished
Time elapsed 0.663 secs

$ ./tpl samples/test -g "time(test55),halt"
Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
Job [www.bing.com] 200 ==> www.bing.com done
Job [www.google.com] 200 ==> www.google.com done
Finished
Time elapsed 0.331 secs

$ ./tpl samples/test -g "time(test56),halt"
Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
Job [www.bing.com] 200 ==> www.bing.com done
Job [www.google.com] 200 ==> www.google.com done
Finished
Time elapsed 0.33 secs
```


Rationals						##EXPERIMENTAL##
=========

Rationals are a native type, with integers just a special case where
the denominator happens to be 1. Rationals can be specified using
the *rdiv/2* operator:

	?- X is 1 / 7, Y is rationalize(X).
	X = 0.1428571428571428
	Y = 1 rdiv 7
	yes
	?- X is 1 rdiv 7.
	X = 1 rdiv 7.
	yes
	?- X is 1 rdiv 1.
	X = 1
	yes


Performance
===========

Elapsed time in seconds, smaller is better. Times are indicative only.
Compiled with GCC 10.1.0 on Linux.

	------------|---------|---------|-----------|-----------|---------|
	            |   tpl   |   tpl   |   swipl   |  gprolog  |   yap   |
	            |  1.1.7  | 1.2.99  |   8.2.1   |    1.45   |   6.5   |
	------------|---------|---------|-----------|-----------|---------|
	sieve       |   0.36  |   0.45  |   0.27    |   0.52    |   0.31  |
	fibonacci   |   0.59  |   0.68  |   0.30    |   0.56    |   0.79  |
	hanoi       |   1.15  |   2.04  |   0.39    |   1.18    |   0.85  |
	queens      |   1.27  |   1.62  |   0.88    |   1.40    |   1.46  |
	puzzle      |   0.35  |   0.48  |   0.17    |   0.28    |   0.22  |
	chess       |  10.7   |  14.1   |   4.9     |   4.9     |   4.9   |
	------------|---------|---------|-----------|-----------|---------|
	testindex1a |   1.35  |   1.46  |   1.31    |   0.70    |   4.94  |
	testindex1b |   1.47  |   1.61  |   1.38    |   >300    |   5.10  |
	testindex5  |   9.3   |   9.64  |  11.8     |   4.2     |  49.7   |
	------------|---------|---------|-----------|-----------|---------|

	tpl -g "time(test5),halt" samples/sieve.pl -g
	tpl -g "time(test),halt" samples/fib.pl
	tpl -g "time(hanoiq(22)),halt" samples/hanoi.pl
	tpl -g "time(testq),halt" samples/queens11.pl
	tpl -g "time(main),halt" samples/puzzle.pl
	tpl -g "time(main),halt" samples/chess.pl
	tpl -g "time(test1a),halt" samples/testindex.pl
	tpl -g "time(test1b),halt" samples/testindex.pl
	tpl -g "time(test5),halt" samples/testindex.pl

	swipl -g "time(test5),halt" samples/sieve.pl
	etc

	yap -g "time(test5),halt" -s128000 samples/sieve.pl
	etc

	export setenv LOCALSZ=256000
	export setenv GLOBALSZ=128000
	time gprolog --query-goal test5,halt --consult-file samples/sieve.pl
	etc

	time scryer-prolog -g test5,halt samples/sieve.pl
	etc

Note tpl is running slower than first reported here, as new features
have been added and not optimized.

Note swipl also has the -O option which can give improved times.

Note gprolog can also be compiled using 'gplc -o sieve samples/sieve.pl'
to achieve a significant speedup.

Times for gprolog & scryer were done using the unix *time* command and
thus include setup time, whereas the others were done with the predicate
*time(Goal)*.

Also, gprolog only seems to implement 1st argument indexing (hence very
slow *testindex1b* result) Also 2 internal stacks needed to be boosted.

Yap came from *git clone https://github.com/vscosta/yap-6.3* and needs
*cmake* installed.

Scryer came from *cargo install scryer-prolog* (it takes a long time)
and needs *m4* installed. I don't know if this is a release or debug build.

The Peirera (sic) benchmarks can be run:

	tpl -g bench_peirera,halt samples/broken/peirera.pl
	swipl -g bench_peirera,halt samples/broken/peirera.pl
