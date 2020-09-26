task70(X,Y) :- write('Frozen X='),write(X),Y=456,write(', set Y='),writeln(Y).

test70 :- freeze(X, task70(X,Y)), X=123, write('Y='), writeln(Y), writeln('OK done').

task71(X) :- write('Frozen X='), writeln(X), fail.

test71 :- freeze(X, task71(X)), X=123, writeln('Ooops').
test71 :- writeln('OK done').

main :- test70, test71, halt.

:- initialization(main).
