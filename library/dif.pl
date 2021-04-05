% This is iso_dif/2 for now

:- module(dif, [dif/2]).

dif(X, Y) :-
   X \== Y,
   ( X \= Y -> true
   ; throw(error(instantiation_error,iso_dif/2))
   ).
