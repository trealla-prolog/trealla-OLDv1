main :-
	writeq(1*(2+3)), nl,
	writeq((1*2)/(3*4)), nl,
	writeq(-(-a)), nl,
	writeq(-(1)), nl,
	writeq(-(-1)), nl,
	writeq(-(-(-a))), nl,
	writeq(-(-(1))), nl,
	writeq(-(-1- -1)), nl,
	writeq(-(-1- +1)), nl,
	writeq(-(-1- 1)), nl,
	writeq(-(-(-a))), nl.

:- initialization(main).
