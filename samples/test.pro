test1 :-
	\+ \+ true,
	writeln(ok).
test1 :-
	writeln(failed).

task2 :- writeln('OK about to throw'), throw(error(p1,p2)).

test2 :-
	( catch(task2,E,(format('OK caught: ~w~n', [E]),fail)) ->
		writeln('OOPS no error') ; writeln('OK was error')
	),
	writeln('OK done').

task3 :- writeln('OK here').

test3 :-
	( catch(task3,E,(format('wrong, caught: ~w~n', [E]),fail)) ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done').

test4a :-
	( call(writeln('OK here')) ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done (3rd line)').

test4b :-
	( call(writeln, 'OK here') ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done (3rd line)').

test4c :-
	( call(writeln, 'OK here') -> writeln('OK no error') ),
	writeln('OK done (3rd line)').

test5a :- throw(error(p1,p2)).
test5b :- _ is abs(abc).

test6 :- Orig='Aa...Bb...Cc...Dd',sys_queue(Orig),string_lower(Orig,S),sys_queue(S),fail.
test6 :- sys_list(L),writeln(L).

test7 :-
	http_get('www.duckduckgo.com',Data,[status_code(Code),headers(Hdrs)]),
	write('Response='), writeln(Code),
	writeln(Hdrs),
	write(Data), nl.

test8 :-
	http_get('http://www.bing.com',Data,[status_code(Code),headers(Hdrs)]),
	write('Response='), writeln(Code),
	writeln(Hdrs),
	write(Data), nl.

test9 :-
	http_get('https://www.google.com',Data,[status_code(Code),headers(Hdrs)]),
	write('Response='), writeln(Code),
	writeln(Hdrs),
	write(Data), nl.

task10(C) :-
	repeat,
		(at_end_of_stream(C) -> (!, fail) ; true),
		getline(C,L),
		write('GOT: '), writeln(L),
		fail.
task10(_).

test10a :-
	fork,
	server(':8080',S,[]),
	accept(S,C),
		fork,
		task10(C).
test10a :-
	wait.

test10b :-
	client('localhost:8080',_,_,S,[]),
	between(1,1000000,I),
		format(S,'[~d] Hello, world~n',[I]),
		delay(100),
		fail.

task11(C) :-
	repeat,
		getline(C,L),
		write('GOT: '), writeln(L),
		fail.
task11(_).

test11a :-
	fork,
	server(':8080',S,[udp(true)]),
	task11(S).
test11a :-
	wait.

test11b :-
	client('localhost:8080',_,_,S,[udp(true)]),
	between(1,1000000,I),
		format(S,'[~d] Hello, world~n',[I]),
		delay(100),
		fail.

test12 :-
	JsonData = '[{"foo":1,"bar":2}, {"bar":3,"foo":4}]',
	read_term_from_atom(JsonData, Data, [double_quotes(atom)]),
	findall(X, (member({F1:A,F2:B},Data), (F1=foo -> X = A ; (F2=foo -> X = B))), L),
	writeln(L).

test13.
test13 :- test13.

sum14(I,I,T,T) :- !.
sum14(I,X,Tmp,T) :- NewTmp is Tmp+I, NewI is I+1, sum14(NewI,X,NewTmp,T).

test14 :-
	sum14(1,100000,0,T),
	write(T), nl.

integers(Low,High,[Low|Rest]) :-
	Low =< High,
	!,
	M is Low+1,
	integers(M,High,Rest).
integers(_,_,[]).

test15a :- integers(1, 100000, L), L=[H|_], write(H), nl.
test15b:- integers(1, 100000, L), L=[_|T], write(T), nl.
test15c:- integers(1, 1000000, L), write_term(L,[max_depth(0)]), nl.

:- dynamic(p/2).
:- dynamic(p/3).

test16 :-
	assertz(p(Z, h(Z, W), f(W))), write('ok14\n'),
	p(f(f(a)), h(f(f(a)), f(a)), f(f(a))), write('ok15\n').
test16 :- write(failed), nl.

f(a,1).
f(a,2).
f(a,3).
f(b,10).
f(b,20).
f(b,30).

test17 :-
	findall(X,f(a,X),Bag,Tail),
	write(Bag), nl,
	findall(X,f(b,X),Tail,_NewTail),
	write(Bag), nl.

test18a :- assertz(f18(123),R), assertz(f18(456)), erase(R), listing(f18).
test18b :- assertz(f18(123),_), clause(f18(_),_,_).

task50(T) :-
	between(1,inf,_),
		format('Task ... ~d',[T]), nl,
		sleep(T),
		fail.

test50 :- between(1,4,I), fork, task50(I).
test50 :- wait.

task51(T) :- Ms is random(1000), delay(Ms), send(T).

test51 :- between(1,10,I), fork, task51(I).
test51 :- wait, sys_list(L), writeln(L).

test52 :- between(1,10,_), N is random(1000), sys_queue(N), fail.
test52 :- sys_list(L), sort(L,L2),
	write_term_to_atom(S,L2,[]), writeln(S), nl,
	read_term_from_atom(S,S2,[]), write_term(S2,[]), nl.

task53(T) :- between(1,10,_), R is random(1000), delay(R), send({T,R}), fail.
task53(T) :- format('Task ~d done~n',[T]).

test53 :- between(1,4,I), fork, task53(I).
test53 :-
	forall(await, (recv(Msg), format('Got: ~w~n',[Msg]))),
	format('Finished~n').

geturl(Url) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]), !,
	format('Job [~w] ~w ==> ~w done~n',[Url,Code,Location]).

test54 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(geturl,L),
	writeln('Finished').

test55 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(spawn(geturl),L),
	wait, writeln('Finished').

task56(Url) :-
	http_open([host(Url),path('/'),method('head')],S,[status_code(Code)]),
	close(S),
	writeln(Code).

test56 :-
	L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
	maplist(task56,L),
	writeln('Finished').

test61(0).
test61(N) :- N > 0, M is N - 1, test61(M).

test62(N) :- N > 0, !, M is N - 1, test62(M).
test62(0).

test63(N) :- ((N > 0 -> M is N - 1, test63(M)) ; true ).

task64(G) :- G.

test64 :-
	task64(findall(X, pr(X, Y), S)),
	writeln(S).

pr(99, 1).
pr(98, 2).
pr(97, 3).
pr(96, 4).
pr(95, 5).
pr(94, 6).
pr(93, 7).
pr(92, 8).
pr(91, 9).
pr(90, 10).
pr(89, 11).
pr(88, 12).
pr(87, 13).
pr(86, 14).
pr(85, 15).
pr(84, 16).
pr(83, 17).
pr(82, 18).
pr(81, 19).
pr(80, 20).
pr(79, 21).
pr(78, 22).
pr(77, 23).
pr(76, 24).
pr(75, 25).
pr(74, 26).
pr(73, 27).
pr(72, 28).
pr(71, 29).
pr(70, 30).
pr(69, 31).
pr(68, 32).
pr(67, 33).
pr(66, 34).
pr(65, 35).
pr(64, 36).
pr(63, 37).
pr(62, 38).
pr(61, 39).
pr(60, 40).
pr(59, 41).
pr(58, 42).
pr(57, 43).
pr(56, 44).
pr(55, 45).
pr(54, 46).
pr(53, 47).
pr(52, 48).
pr(51, 49).
pr(50, 50).
pr(49, 51).
pr(48, 52).
pr(47, 53).
pr(46, 54).
pr(45, 55).
pr(44, 56).
pr(43, 57).
pr(42, 58).
pr(41, 59).
pr(40, 60).
pr(39, 61).
pr(38, 62).
pr(37, 63).
pr(36, 64).
pr(35, 65).
pr(34, 66).
pr(33, 67).
pr(32, 68).
pr(31, 69).
pr(30, 70).
pr(29, 71).
pr(28, 72).
pr(27, 73).
pr(26, 74).
pr(25, 75).
pr(24, 76).
pr(23, 77).
pr(22, 78).
pr(21, 79).
pr(20, 80).
pr(19, 81).
pr(18, 82).
pr(17, 83).
pr(16, 84).
pr(15, 85).
pr(14, 86).
pr(13, 87).
pr(12, 88).
pr(11, 89).
pr(10, 90).
pr(9, 91).
pr(8, 92).
pr(7, 93).
pr(6, 94).
pr(5, 95).
pr(4, 96).
pr(3, 97).
pr(2, 98).
pr(1, 99).
pr(0, 100).

