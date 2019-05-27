
% compute the set of stack items found in the currently loaded GrailLight proofs (with stack items produced by grail_light_cr_parse_actions)

set_of_stack_items(SI) :-
	setof(X, enumerate_stack_items(X), SI).

enumerate_stack_items(SI) :-
	proof(_,Proof),
	stack_item(Proof, SI).

stack_item(rule(_,_,_-_-A-B-C-D,Prems), X) :-
	( map_simplify_formula(A, X)
	; map_simplify_formula(B, X)
	; map_simplify_formula(C, X)
	; map_simplify_formula(D, X)
	; stack_item_list(Prems, X)).


stack_item_list([P|_], X) :-
	stack_item(P, X).
stack_item_list([_|Ps], X) :-
	stack_item_list(Ps, X).


map_simplify_formula([], []).
map_simplify_formula([A|As], [B|Bs]) :-
	simplify_formula(A, B),
	map_simplify_formula(As, Bs).

simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(A0)), lit(s(A))) :-
	!,
	simplify_argument(A0, A).
simplify_formula(lit(pp(A0)), lit(pp(A))) :-
	!,
	simplify_argument(A0, A).
simplify_formula(lit(np(_,_,_)), lit(np)) :-
	!.
simplify_formula(lit(X), lit(X)) :-
	!.
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(_,A0), dia(1,A)) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), box(1,A)) :-
	simplify_formula(A0, A).

simplify_argument('$VAR'(_), '$VAR'(0)) :-
	!.
simplify_argument(A, A).
