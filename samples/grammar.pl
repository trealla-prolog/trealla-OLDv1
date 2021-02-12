:- use_module(library(dcgs)).

% A grammar in DCG

sentence --> np, vp.
np --> det, noun.
vp --> verb, np.
vp --> verb.

noun --> [woman].
noun --> [man].
verb --> [shoots].
det --> [the].
det --> [a].

/*
    Generate all possible sentences...
*/

main :- phrase_to_stream(sentence,user_output), nl, fail.
main.
