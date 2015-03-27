
simplify_tree(node(T, N, Role0, _L, _R, Dts0), node(T, N, Role, Dts)) :-
	simplify_role(Role0, Role),
	simplify_list(Dts0, Dts).
simplify_tree(leaf(POS, N, Role0, _L, _R, Word, _Lemma, _Atts), leaf(POS, N, Role, Word)) :-
	simplify_role(Role0, Role).
simplify_tree(epsilon(N, _, _, _, _), epsilon(N)).

simplify_list([], []).
simplify_list([T0|Ts0], [T|Ts]) :-
	simplify_tree(T0, T),
	simplify_list(Ts0, Ts).


simplify_role(--, nil) :-
	!.
simplify_role(top, nil) :-
	!.
simplify_role(R, R).