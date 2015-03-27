:- module(remove_epsilon, [remove_epsilon/2]).

remove_epsilon(leaf(A,B,C,D,E,F,G,H), leaf(A,B,C,D,E,F,G,H)).
remove_epsilon(node(A,B,C,D,E,Ds0), node(A,B,C,D,E,Ds)) :-
	remove_epsilon1(Ds0, Ds).

remove_epsilon1([], []).
remove_epsilon1([D0|Ds0], Ds) :-
    (
        D0 = epsilon(_,_,_,_,_)
    ->
        remove_epsilon1(Ds0, Ds)
    ;
        Ds = [D|Ds1],
        remove_epsilon(D0, D),
        remove_epsilon1(Ds0, Ds1)
    ).
