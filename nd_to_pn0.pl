


atom_yield_pos(at(A), at(A,X)) -->
    [pos(A,X)].
atom_yield_pos(dia(I,A0), dia(I,A)) -->
    atom_yield_pos(A0, A).
atom_yield_pos(box(I,A0), box(I,A)) -->
    atom_yield_pos(A0, A).
atom_yield_pos(dr(I,A0,B0), dr(I,A,B)) -->
    atom_yield_neg(B0, B),
    atom_yield_pos(A0, A).
atom_yield_pos(dl(I,B0,A0), dl(I,B,A)) -->
    atom_yield_pos(A0, A),
    atom_yield_neg(B0, B).
atom_yield_pos(p(I,A0,B0), p(I,A,B)) -->
    atom_yield_pos(B0, B),
    atom_yield_pos(A0, A).


atom_yield_neg(at(A), at(A,X)) -->
    [neg(A,X)].
atom_yield_neg(dia(I,A0), dia(I,A)) -->
    atom_yield_neg(A0, A).
atom_yield_neg(box(I,A0), box(I,A)) -->
    atom_yield_neg(A0, A).
atom_yield_neg(dr(I,A0,B0), dr(I,A,B)) -->
    atom_yield_neg(A0, A),
    atom_yield_pos(B0, B).
atom_yield_neg(dl(I,B0,A0), dl(I,B,A)) -->
    atom_yield_pos(B0, B),
    atom_yield_neg(A0, A).
atom_yield_neg(p(I,A0,B0), p(I,A,B)) -->
    atom_yield_neg(A0, A),
    atom_yield_neg(B0, B).

nd_to_matching(rule(RN, _, F-_,[]), F1, L2, Lex) :-
    is_axiom(RN, F1, Lex),
    !,
    atom_yield_pos(F, F1, L1, []),
    atom_yield_neg(F, _F2, L2, []),
    match_lists(L1, L2).
nd_to_matching(rule(RN, _, _-_, [R1,R2]), F, L, Lex) :-
    is_elim(RN),
    !,
    nd_to_matching(R1, F1, L1, Lex1),
    nd_to_matching(R2, F2, L2, Lex2),
    append(Lex1, Lex2, Lex),
    match_elim(F1, F2, L1, L2, F, L).


match_elim(dr(_,A,B), B, LAB, LB, A, L) :-
    postfix_match(L, LB, LAB),
    !.
match_elim(B, dl(_,B,A), LB, LBA, A, L) :-
    prefix_match(LB, L, LBA),
    !.

is_axiom(ax, _, []).
is_axiom(lex, F, [F]).

is_elim(dr).
is_elim(dl).

prefix_match([], Ys, Ys).
prefix_match([X0|Xs], Ys, [X|Zs]) :-
    match(X0, X),
    prefix_match(Xs, Ys, Zs).

postfix_match([], B0, B) :-
    /* should this be match_lists1 ? */
    match_lists(B0, B).
postfix_match([X|Xs], Ys, [X|Zs]) :-
    postfix_match(Xs, Ys, Zs).

match_lists(Xs, Ys0) :-
    reverse(Ys0, Ys),
    match_lists1(Xs, Ys).

match_lists1([], []).
match_lists1([X|Xs], [Y|Ys]) :-
    match(X, Y),
    match_lists1(Xs, Ys).

match(pos(X,Y), neg(X,Y)).
match(neg(X,Y), pos(X,Y)).
