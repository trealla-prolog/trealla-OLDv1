test1(A) :- A, call(A), A.
test2(A) :- (A ; A ), A.

do_n(N, Goal, Time) :-
	get_cpu_time(T0),
	(   between(1,N,_),
	    Goal,
	    fail
	;   get_cpu_time(T1),
	    Time is (T1 - T0)/1000
	).
