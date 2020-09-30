Trealla ProLog
==============

A compact, efficient Prolog interpreter with ISO compliant aspirations.

	Integers are 64-bit (optionally 32 or 128-bit)
	Integer overflow detection
	Rationals are a native type
	Reals are double
	Atoms are UTF-8 of unlimited length
	The default string is *chars*, using a compact UTF-8 representation
	Arity limit 255
	Limited DCG capability
	Full-term just-in-time indexing
	Peristence to per-module database
	REPL with history
	MIT licensed

Trealla is not WAM-based. It uses tree-walking, structure-sharing and
deep-binding.


Building
========

Written in plain-old C.

	git clone https://github.com/infradig/trealla.git
	cd trealla
	make
	make test

There are no dependencies except OpenSSL, which can removed by:

	make nossl

Run...

	make clean && make debug
	make test_valgrind

to do the tests under *valgrind* memory checking.

A 'make debug' build compiles in 0.2s with *tcc* and about 2s with
*clang* and *gcc*. Should build on any Unix-like system with a C99
compiler (could do C89 with a few cosmetic tweaks). Has been tested on
Manjaro, Ubuntu, FreeBSD and Raspbian (32 & 64-bit) systems. There
are no plans for a Windows port.


Usage
=====

	tpl [options] [-l file] [-g goal]

where options can be:

	-O0, --noopt       - no optimization
	-t, --trace        - trace
	-q, --quiet        - quiet mode (no banner)
	-v, --version      - version
	-h, --help         - help
	-d, --daemonize    - daemonize
	-w, --watchdog     - create watchdog
	--stats            - print stats
	--iso-only         - ISO-only mode
	--consult          - consult from STDIN

For example:

	./tpl -l samples/sieve -g test2,halt

Invocation without any goal presents the REPL.


What's missing?
===============

	there may be missing or incomplete ISO predicates
	database is immediate update view, fix
	modules need more work
	missing directives?
	aiming to run clpz.pl down the track


GNU-Prolog & SWI-Prolog
=======================

	between/3
	forall/2
	msort/2
	read_term_from_atom/3	# input term can be atom or string-list
	format/1-3
	predicate_property/2
	numbervars/1,3-4
	e/0
	sleep/1
	name/2
	maplist/1-4
	tab/1-2

	random/1                # random(-float) float [0.0,<1.0]
	random/1                # random(+integer) function returning integer [0,<integer]

	phrase/2-3


GNU-Prolog
==========

	write_term_to_atom/3


SWI-Prolog
==========

	setup_call_cleanup/3
	findall/4
	term_to_atom/2
	atom_number/2			# synonymous with string_number/2
	atomic_concat/3
	format(atom(A),...)
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
	volatile/1
	rational/1
	rationalize/1
	rdiv/2
	char_type/2
	code_type/2
	string_upper/2
	string_lower/2
	uuid/1					# generates non-standard UUID
	load_files/2
	split_string/4
	read_string/3			# synonoymous with bread/3

	getenv/2
	setenv/2
	unsetenv/1

	call_nth/2
	offset/2
	limit/2

	delete_file/1
	exists_file/1
	rename_file/2
	time_file/2
	size_file/2
	exists_directory/1
	make_directory/1
	working_directory/2
	chdir/1

	current_key/1
	recorda/2-3
	recordz/2-3
	recorded/2-3
	instance/2
	asserta/2
	assertz/2
	clause/3
	erase/1

	http_get/3				# autoloaded from library(http)
	http_post/4				# autoloaded from library(http)
	http_put/4				# autoloaded from library(http)
	http_delete/3			# autoloaded from library(http)
	http_open/3				# autoloaded from library(http)

Note: consult/1 and load_files/2 support lists of files as args. Also
support loading into modules eg. *consult(MOD:FILE-SPEC)*.

As generally, atoms can be used interchangeably with chars-lists and
codes-list (aka *strings*).

Others
======

	string_number/2         # unify (in decimal) with number
	string_hex/2            # unify (in hex) with number
	string_octal/2          # unify (in octal) with number
	log10/1                 # function returning log10 of arg
	now/0                   # function returning C-time in secs as integer
	now/1                   # now (-integer) C-time in secs as integer
	get_time/1              # get_time(-var) C-time in secs as float
	srandom/1               # seed(+integer) seed random number generator
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
	split/4                 # split(+atom,+sep,?left,?right)
	base64/2                # base64(?decoded,?encoded)
	urlenc/2                # urlenc(?decoded,?encoded)
	sha1/2                  # sha1(+plaintext,?hash)        NEEDS OPENSSL
	sha256/2                # sha256(+plaintext,?hash)      NEEDS OPENSSL
	sha512/2                # sha512(+plaintext,?hash)      NEEDS OPENSSL

	open(stream(Str),...)   # with open/4 reopen a stream
	open(F,M,S,[mmap(Ls)])  # with open/4 mmap() the file to Ls

	persist/1               # directive 'persist funct/arity'


A simple dictionary
===================

Autoloaded from library(dict)...

	dict:set/4              # set(+dict,+name,+value,-dict)
	dict:del/3              # del(+dict,+name,-dict)
	dict:get/3              # get(+dict,+name,-value)
	dict:get/4              # get(+dict,+name,-value,+default)
	dict:lst/2              # lst(+dict,-values)


Attributed variables		##NOT WORKING YET##
====================

Not built on attributed variables per se, but using the same mechanism
under the hood...

	freeze/2
	frozen/2


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
many bytes, = 0 meaning return what is there (if non-blocking) or a var
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
	spawn/1-n               # concurrent form of call/1-n
	yield/0                 # voluntarily yield control
	wait/0                  # parent should wait for children to finish
	await/0                 # parent should wait for a message
	send/1                  # apend term to parent queue
	recv/1                  # pop term from queue
	spawnlist/1-n           # concurrent form of maplist/1-n

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

	$ ./tpl -l samples/test -g "time(test54),halt"
	Job [www.google.com] 200 ==> www.google.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Finished
	Time elapsed 0.663 secs

	$ ./tpl -l samples/test -g "time(test55),halt"
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.google.com] 200 ==> www.google.com done
	Finished
	Time elapsed 0.331 secs

	$ ./tpl -l samples/test -g "time(test56),halt"
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.google.com] 200 ==> www.google.com done
	Finished
	Time elapsed 0.33 secs


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

	------------|---------|-----------|-----------|---------|----------
	            |   tpl   |   swipl   |  gprolog  |   yap   |  scryer
	            |  1.1.7  |   8.2.1   |    1.45   |   6.5   |  0.8.127
	------------|---------|-----------|-----------|---------|----------
	sieve       |   0.36  |   0.27    |   0.52    |   0.31  |   7.63
	fibonacci   |   0.59  |   0.30    |   0.56    |   0.79  |   6.74
	hanoi       |   1.15  |   0.39    |   1.18    |   0.85  |   9.9
	queens      |   1.27  |   0.88    |   1.40    |   1.46  |  10.9
	puzzle      |   0.35  |   0.17    |   0.28    |   0.22  |   3.08
	chess       |  10.7   |   4.9     |   4.9     |   4.9   |
	------------|---------|-----------|-----------|---------|----------
	testindex1a |   1.35  |   1.31    |   0.70    |   4.94  |
	testindex1b |   1.47  |   1.38    |   >300    |   5.10  |
	testindex5  |   9.3   |  11.8     |   4.2     |  49.7   |
	------------|---------|-----------|-----------|---------|----------

	tpl -l samples/sieve.pro -g "time(test5),halt"
	tpl -l samples/fib.pro -g "time(test),halt"
	tpl -l samples/hanoi.pro -g "time(hanoiq(22)),halt"
	tpl -l samples/queens11.pro -g "time(testq),halt"
	tpl -l samples/puzzle.pro -g "time(main),halt"
	tpl -l samples/chess.pro -g "time(main),halt"
	tpl -l samples/testindex.pro -g "time(test1a),halt"
	tpl -l samples/testindex.pro -g "time(test1b),halt"
	tpl -l samples/testindex.pro -g "time(test5),halt"

	swipl -l samples/sieve.pro -g "time(test5),halt"
	etc

	yap -l samples/sieve.pro -g "time(test5),halt" -s128000
	etc

	export setenv LOCALSZ=256000
	export setenv GLOBALSZ=128000
	time gprolog --consult-file samples/sieve.pro --query-goal test5,halt
	etc

	time scryer-prolog samples/sieve.pro -g test5,halt
	etc

Times for gprolog & scryer were done using the unix *time* command and
thus include setup time, whereas the others were done with the predicate
*time(Goal)*.

Also, gprolog only seems to implement 1st argument indexing (hence very
slow *testindex1b* result) Also 2 internal stacks needed to be boosted.

Yap came from *git clone https://github.com/vscosta/yap-6.3* and needs
*cmake* installed.

Scryer came from *cargo install scryer-prolog* (it takes a long time)
and needs *m4* installed. Chess needs name/2 (at least).

The Peirera (sic) benchmarks can be run:

	tpl -l samples/broken/peirera.pl -g bench_peirera,halt
	swipl -l samples/broken/peirera.pl -g bench_peirera,halt
