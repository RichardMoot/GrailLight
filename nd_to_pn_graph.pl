:- use_module(library(ordsets), [ord_seteq/2, ord_add_element/3]).
:- use_module(tree234, [list_to_btree/2,btree_to_list/2,btree_init/1,btree_get/3,btree_insert/4]).

% = pretty-printer for formulas

portray(lit(np(_,_,_))) :-
	!,
	write(np).
portray(lit(s(_))) :-
	!,
	write(s).
portray(lit(s(A))) :-
	var(A),
	!,
	write(s).
portray(lit(s(inf(_)))) :-
	!,
	format('s_inf', []).
portray(lit(s(A))) :-
	!,
	format('s_~w', [A]).
portray(lit(pp(_))) :-
	!,
	write(pp).
portray(lit(pp(A))) :-
	var(A),
	!,
	write(pp).
portray(lit(pp(à))) :-
	!,
	write(pp_a).
portray(lit(pp(A))) :-
	!,
	format('pp_~w', [A]).
portray(lit(A)) :-
	!,
	write(A).


portray(lit(A,I)) :-
	integer(I),
	!,
	format('~p_~w', [lit(A),I]).

%%%%

% = label_atoms(+Formula, ?Formula)

% adds a fresh variable to each atomic formula

label_atoms(lit(A), lit(A,_)).
label_atoms(dia(I,A0), dia(I, A)) :-
	label_atoms(A0, A).
label_atoms(box(I,A0), box(I, A)) :-
	label_atoms(A0, A).
label_atoms(dr(I,A0,B0), dr(I,A,B)) :-
	label_atoms(A0, A),
	label_atoms(B0, B).
label_atoms(dl(I,A0,B0), dl(I,A,B)) :-
	label_atoms(A0, A),
	label_atoms(B0, B).
label_atoms(p(I,A0,B0), p(I,A,B)) :-
	label_atoms(A0, A),
	label_atoms(B0, B).

label_all_atoms(rule(Name,Pros,Formula0-Sem,Premisses0),
		rule(Name,Pros,Formula-Sem,Premisses)) :-
	label_atoms(Formula0, Formula),
	label_all_premisses(Premisses0, Premisses).

label_all_premisses([], []).
label_all_premisses([P|Ps], [Q|Qs]) :-
	label_all_atoms(P, Q),
	label_all_premisses(Ps, Qs).

%

rule_formula(rule(_, _, Formula-_, _), Formula).
rule_name(rule(Name, _, _, _), Name).
rule_premisses(rule(_, _, _, Premisses), Premisses).

%

simplify_formula(lit(A,B), lit(A,B)).
simplify_formula(dia(_,A0), A) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), A) :-
	simplify_formula(A0, A).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).

match_formula(A0, B0) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B),
	A = B.

%

print_all_proofs :-
	proof(Num, Proof),
	nd_to_pn(Proof, Num, _),
	fail.
print_all_proofs.

nd_to_pn(Proof0, _Num, List) :-
	label_all_atoms(Proof0, Proof),
	nd_to_pn0(Proof, List0, []),
	keysort(List0, List),
	rule_formula(Proof, Goal),
	append(List, [0-Goal], Sequent),
        number_list_atoms(Sequent),
	verify_atoms(List, Goal),
	find_words(Proof0, WordList),
	print_formula_list(WordList, List, Goal, Edges, Labels),
	numbervars(Edges),
	numbervars(Labels),
	format('=== edges ===~n', []),
	print_list(Edges),
	format('=== labels ===~n', []),
	print_list(Labels),
	format('===~n', []),
	!,
	format(user_error, '.', []).
nd_to_pn(_, Num, _) :-
	write(user_error, '*'),
	write(user_error, Num),
	write(user_error, '*'),
	fail.

nd_to_pn0(rule(Name, _, Formula-Term, Premisses)) -->
	nd_to_pn1(Name, Formula, Term, Premisses).

nd_to_pn1(axiom, A, word(I), []) -->
	!,
	[I-A].
nd_to_pn1(hyp(_), _, _, []) -->
	!,
	[].
nd_to_pn1(dl, B, _, [rule(Name1,_,A0-Term1,Premisses1),
		     rule(Name2,_,dl(I,A,B)-Term2,Premisses2)]) -->
	{match_formula(A0, A)},
	!,
	nd_to_pn1(Name1, A0, Term1, Premisses1),
	nd_to_pn1(Name2, dl(I,A,B), Term2, Premisses2).
nd_to_pn1(dl1, B, _, [rule(Name1,_,A0-Term1,Premisses1),
		      rule(Name2,_,dl(I,A,B)-Term2,Premisses2)]) -->
	{match_formula(A0, A)},
	!,
	nd_to_pn1(Name1, A0, Term1, Premisses1),
	nd_to_pn1(Name2, dl(I,A,B), Term2, Premisses2).
% gapping
%nd_to_pn1(dr, B, _, [rule(Name1,_,dr(I,B,box(1,dia(1,A)))-Term1,Premisses1),
%		     rule(Name2,_,A-Term2,Premisses2)]) -->
%	!,
%	nd_to_pn1(Name1, dr(I,B,box(1,dia(1,A))), Term1, Premisses1),
%	nd_to_pn1(Name2, A, Term2, Premisses2).
nd_to_pn1(dr, B, _, [rule(Name1,_,dr(I,B,A0)-Term1,Premisses1),
		     rule(Name2,_,A-Term2,Premisses2)]) -->
	{match_formula(A0, A)},
	!,
	nd_to_pn1(Name1, dr(I,B,A0), Term1, Premisses1),
	nd_to_pn1(Name2, A, Term2, Premisses2).
nd_to_pn1(dli(K), dl(_,A,B), _, [rule(Name,_,B-Term,Premisses)]) -->
	{find_hypothesis_list(Premisses, K, A)},
	!,
	nd_to_pn1(Name, B, Term, Premisses).

nd_to_pn1(drdiaboxi(_,K), dr(0,A,B), _, [rule(Name,_,A-Term,Premisses)]) -->
	{find_hypothesis_list(Premisses, K, B)},
	!,
	nd_to_pn1(Name, A, Term, Premisses).

nd_to_pn1(dldiaboxi(I,K), dl(I,B,A), _, [rule(Name,_,A-Term,Premisses)]) -->
	{find_hypothesis_list(Premisses, K, B)},
	!,
	nd_to_pn1(Name, A, Term, Premisses).

nd_to_pn1(dli1(I,K), dl(I,A,B), _, [rule(Name,_,B-Term,Premisses)]) -->
	{find_hypothesis_list(Premisses, K, A)},
	!,
	nd_to_pn1(Name, B, Term, Premisses).

nd_to_pn1(prod_e(I), C, _, Premisses) -->
%	{trace},
	{get_formulas(Premisses, I, Name1, ProdF, Term1, Premisses1, Name2, C, Term2, Premisses2)},
	 !,
	 nd_to_pn1(Name1, ProdF, Term1, Premisses1),
	 nd_to_pn1(Name2, C, Term2, Premisses2).

nd_to_pn1(prod_i, Prod, _, [rule(Name1,_,A-Term1,Premisses1),rule(Name2,_,B-Term2,Premisses2)]) -->
	{product_formula(Prod, A, B)},
	!,
	nd_to_pn1(Name1, A, Term1, Premisses1),
	nd_to_pn1(Name2, B, Term2, Premisses2).

nd_to_pn1(Name, F, _, Prems) -->
	{format(user_error, '{Unknown rule name: ~w with formula ~p~n}~n', [Name, F]),
	 print_prems_formulas(Prems)},
	[].

product_formula(p(_,A,dia(_,box(_,B))), A, B).
product_formula(p(_,A,B), A, B).

print_prems_formulas([]) :-
	format(user_error, '===~n', []).
print_prems_formulas([rule(_,_, F-_,_)|Rest]) :-
	format(user_error, '~p~n', [F]),
	print_prems_formulas(Rest).

get_formulas([P1,P2], I, Name1, ProdF, Term1, Premisses1, Name2, CF, Term2, Premisses2) :-
	match_product(P1, P2, A, B, Name1, ProdF, Term1, Premisses1, Name2, CF, Term2, Premisses2),
	find_hypothesis_list(Premisses2, I, A),
	find_hypothesis_list(Premisses2, I, B).

match_product(rule(Name1, _, p(I,A0,B0)-Term1, Premisses1), rule(Name2, _, C-Term2, Premisses2),
	      A, B, Name1, p(I,A0,B0), Term1, Premisses1, Name2, C, Term2, Premisses2) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B),
	!.
match_product(rule(Name2, _, C-Term2, Premisses2), rule(Name1, _, p(I,A0,B0)-Term1, Premisses1),
	      A, B, Name1, p(I,A,B), Term1, Premisses1, Name2, C, Term2, Premisses2) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B),
	!.



find_hypothesis_list(List, K, A) :-
	member(Proof, List),
	find_hypothesis(Proof, K, A).
find_hypothesis(rule(hyp(K), _, A0-_, []), K, A) :-
	match_formula(A0, A),
	!.
find_hypothesis(rule(_, _, _, Premisses), K, A) :-
	find_hypothesis_list(Premisses, K, A).

% =====================================
% =       auxiliary predicates        =
% =====================================

print_list([]) :-
	nl.
print_list([X|Xs]) :-
	format('~p~n', [X]),
	print_list(Xs).


print_lists([], []).
print_lists([N-W|Ws], [N-F|Fs]) :-
	print_item(W, F),
	print_lists(Ws, Fs).


flatten_item(p(_,A,B)) -->
	!,
	flatten_item(A),
	flatten_item(B).
flatten_item(A) -->
	[A].

print_item(p(I,W1,W2), F) :-
	!,
	flatten_item(p(I,W1,W2), WList, []),
	print_items(WList, F).
print_item(W, F) :-
	format('~w|~p ', [W,F]).

print_items([], _).
print_items([W|Ws], F) :-
    (	
	 is_interpunction(W)
    ->
         format('~w|let ', [W])
    ;
         format('~w|~p ', [W,F])
    ),
         print_items(Ws, F).


is_interpunction('!').
is_interpunction('"').
is_interpunction('\'').
is_interpunction((...)).
is_interpunction('(').
is_interpunction(')').
is_interpunction('[').
is_interpunction(']').
is_interpunction('_').
is_interpunction('^').
is_interpunction(',').
is_interpunction('*').
is_interpunction('-').
is_interpunction('...').
is_interpunction('..').
is_interpunction('.').
is_interpunction('?').
is_interpunction(':').
is_interpunction(';').
is_interpunction('=').
is_interpunction('/').

number_list_atoms(List) :-
	number_list_atoms(List, 1).

number_list_atoms([], _).
number_list_atoms([_-F|Fs], N0) :-
	number_atoms(F, N0, N),
	number_list_atoms(Fs, N).



verify_atoms(Antecedent, Goal) :-
	verify_atoms_pos(Goal, [], Pos0, [], Neg0),
	verify_atoms_ant(Antecedent, Pos0, Pos, Neg0, Neg),
  ( ord_seteq(Pos, Neg) -> true ; format(user_error, '{count check failure!~n~w~n~w~n}~n', [Pos,Neg]), fail).

verify_atoms_ant([], Pos, Pos, Neg, Neg).
verify_atoms_ant([_-A|As], Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_ant(As, Pos1, Pos, Neg1, Neg).

verify_atoms_pos(lit(_,Int), Pos0, Pos, Neg, Neg) :-
	(ord_memberchk(Int, Pos0) -> format(user_error, '{multiple positive occurrences of ~w}~n', [Int]), fail ; true),
	ord_add_element(Pos0, Int, Pos).
verify_atoms_pos(dia(_,A), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_pos(A, Pos0, Pos, Neg0, Neg).
verify_atoms_pos(box(_,A), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_pos(A, Pos0, Pos, Neg0, Neg).
verify_atoms_pos(dr(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_pos(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_neg(B, Pos1, Pos, Neg1, Neg).
verify_atoms_pos(dl(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_pos(B, Pos1, Pos, Neg1, Neg).
verify_atoms_pos(p(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_pos(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_pos(B, Pos1, Pos, Neg1, Neg).

verify_atoms_neg(lit(_,Int), Pos, Pos, Neg0, Neg) :-
	(ord_memberchk(Int, Neg0) -> format(user_error, '{multiple negative occurrences of ~w}~n', [Int]), fail ; true),
	ord_add_element(Neg0, Int, Neg).
verify_atoms_neg(dia(_,A), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos, Neg0, Neg).
verify_atoms_neg(box(_,A), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos, Neg0, Neg).
verify_atoms_neg(dr(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_pos(B, Pos1, Pos, Neg1, Neg).
verify_atoms_neg(dl(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_pos(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_neg(B, Pos1, Pos, Neg1, Neg).
verify_atoms_neg(p(_,A,B), Pos0, Pos, Neg0, Neg) :-
	verify_atoms_neg(A, Pos0, Pos1, Neg0, Neg1),
	verify_atoms_neg(B, Pos1, Pos, Neg1, Neg).



number_atoms(lit(_,I), N0, N) :-
	(
	    var(I) ->
	    I = N0,
	    N is N0 + 1
	;
	    N = N0
	).
number_atoms(dia(_, A), N0, N) :-
	number_atoms(A, N0, N).
number_atoms(box(_, A), N0, N) :-
	number_atoms(A, N0, N).
number_atoms(dr(_,A,B), N0, N) :-
	number_atoms(A, N0, N1),
	number_atoms(B, N1, N).
number_atoms(dl(_,A,B), N0, N) :-
	number_atoms(A, N0, N1),
	number_atoms(B, N1, N).
number_atoms(p(_,A,B), N0, N) :-
	number_atoms(A, N0, N1),
	number_atoms(B, N1, N).

find_words(Proof, Words) :-
	find_words(Proof, Words0, []),
	keysort(Words0, Words).

find_words(rule(Name, Pros, _-Term, []), L0, L) :-
	Name = axiom,
	Term = word(I),
	!,
        L0 = [I-Pros|L].
find_words(rule(_, _, _, Premisses), L0, L) :-
	find_premiss_words(Premisses, L0, L).

find_premiss_words([], L, L).
find_premiss_words([P|Ps], L0, L) :-
	find_words(P, L0, L1),
	find_premiss_words(Ps, L1, L).

print_formula_list(Words, Formulas, Goal, Edges, Labels) :-
	btree_init(Tree0),
	print_formula_list(Words, Formulas, Edges, Edges0, Labels, Labels0, Tree0, Tree),
        print_formula_pos(X-Goal, Edges0, [], Labels0, [label(X,goal)], Tree, _).

print_formula_list([], [], Es, Es, Ls, Ls, T, T).
print_formula_list([N-W|Ws], [N-F|Fs], Es0, Es, [Label|Ls0], Ls, T0, T) :-
	Label = label(X, word(N,W)),
	print_formula_neg(X-F, Es0, Es1, Ls0, Ls1, T0, T1),
	print_formula_list(Ws, Fs, Es1, Es, Ls1, Ls, T1, T).

print_formula(Formula, Edges, Labels, Tree) :-
	btree_init(Tree0),
	print_formula_neg(_-Formula, Edges, [], Labels, [], Tree0, Tree).

print_formula_neg(X-lit(A, N), Edges, Edges, [Label|Labels], Labels, Tree0, Tree) :-
	!,
   (
	btree_get(Tree0, N, Var)
   ->
        X = Var,
        Tree = Tree0
   ;
	btree_insert(Tree0, N, X, Tree)
   ),
	Label = label(X, atom(A)).
print_formula_neg(Y-dr(I,A,B), [Edge|Edges0], Edges, [Label|Labels0], Labels, Tree0, Tree) :-
	!,
	Edge = appl(X,Y,Z),
	Label = label(X,conn(dr(I))),
	print_formula_neg(X-A, Edges0, Edges1, Labels0, Labels1, Tree0, Tree1),
	print_formula_pos(Z-B, Edges1, Edges, Labels1, Labels, Tree1, Tree).
print_formula_neg(Y-dl(I,B,A), [Edge|Edges0], Edges, [Label|Labels0], Labels, Tree0, Tree) :-
	!,
	Edge = appl(X,Y,Z),
	Label = label(X,conn(dl(I))),
	print_formula_neg(X-A, Edges0, Edges1, Labels0, Labels1, Tree0, Tree1),
	print_formula_pos(Z-B, Edges1, Edges, Labels1, Labels, Tree1, Tree).


% the negative version of this atom should already have been printed,
% so we only update the variables here.
print_formula_pos(X-lit(_, N), Edges, Edges, Labels, Labels, Tree0, Tree) :-
	!,
   (
	btree_get(Tree0, N, Var)
   ->
        X = Var,
        Tree = Tree0
   ;
	btree_insert(Tree0, N, X, Tree)
   ).
print_formula_pos(X-dr(I,A,B), [Edge|Edges0], Edges, [Label|Labels0], Labels, Tree0, Tree) :-
	!,
	Edge = lambda(X,Y,Z),
	Label = label(X,dr(I)),
	print_formula_pos(Y-A, Edges0, Edges1, Labels0, Labels1, Tree0, Tree1),
	print_formula_neg(Z-B, Edges1, Edges, Labels1, Labels, Tree1, Tree).
print_formula_pos(X-dl(I,B,A), [Edge|Edges0], Edges, [Label|Labels0], Labels, Tree0, Tree) :-
	!,
	Edge = lambda(X,Y,Z),
	Label = label(X,dr(I)),
	print_formula_pos(Y-A, Edges0, Edges1, Labels0, Labels1, Tree0, Tree1),
	print_formula_neg(Z-B, Edges1, Edges, Labels1, Labels, Tree1, Tree).
