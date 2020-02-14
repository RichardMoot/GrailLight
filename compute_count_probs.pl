:- use_module(tree234, [btree_init/1,
			btree_get_replace/5,
			btree_insert/4,
			btree_to_list/2]).
:- compile(translate_form).

btree_update_best_value(T0, Key, NewVal-NewJust, T) :-
    (
	/* check if a Val-Just pair already exists for Key */
	btree_get_replace(T0, Key, OldVal-OldJust, New, T)
    ->
        /* if so, update if NewVal is better */
	keep_best(OldVal, OldJust, NewVal, NewJust, New)
    ;
        /* otherwise, insert current value as best into map */
	btree_insert(T0, Key, NewVal-NewJust, T)
    ).

keep_best(OldVal, OldJust, NewVal, NewJust, New) :-
    (
	OldVal > NewVal
    ->
	New = OldVal-OldJust
    ;
	New = NewVal-NewJust
    ).

atomic_formulas([s,n,np,pp,cl_r,cl_y,txt]).

atom_index(s, 0).
atom_index(n, 1).
atom_index(np, 2).
atom_index(pp, 3).
atom_index(cl_r, 4).
atom_index(cl_y, 5).
atom_index(txt, 6).

atom_vector(let,  t(0,0,0,0,0,0,0)).
atom_vector(s,    t(1,0,0,0,0,0,0)).
atom_vector(n,    t(0,1,0,0,0,0,0)).
atom_vector(np,   t(0,0,1,0,0,0,0)).
atom_vector(pp,   t(0,0,0,1,0,0,0)).
atom_vector(cl_r, t(0,0,0,0,1,0,0)).
atom_vector(cl_r, t(0,0,0,0,0,1,0)).
atom_vector(txt,  t(0,0,0,0,0,0,1)).

goal_vector(t(0, 0, 0, 0, 0, 0, 0)). % let, n\n, s/s
goal_vector(t(1, 0, 0, 0, 0, 0, 0)). % s
goal_vector(t(0, 1, 0, 0, 0, 0, 0)). % n
goal_vector(t(0, 0, 1, 0, 0, 0, 0)). % np
goal_vector(t(1, 0,-1, 0, 0, 0, 0)). % np\s

test_sentence(1, [
       si('Coronavirus', nam, 'Coronavirus', [np-0.94642 ]),
       si(':', pun, ':', [dr(0,dl(0,np,s),s)-0.3901874, dr(0,dl(0,dl(0,n,n),np),np)-0.20512879, dr(0,dl(0,n,s),s)-0.13812642, dr(0,dl(0,dl(0,np,s),np),np)-0.11533434, dr(0,dl(0,np,np),np)-0.07273553 ]),
       si('cette', pro:dem, 'cette', [dr(0,np,n)-0.9999981 ]),
       si('carte', nom:fs, 'carte', [n-0.99999475 ]),
       si('interactive', adj, 'interactif', [dl(0,n,n)-0.9999831 ]),
       si('permet', ver:pres, 'permettre', [dr(0,dl(0,np,s),dl(0,np,s_inf))-0.9994475 ]),
       si('de', prp, 'de', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-0.9998908 ]),
       si('suivre', ver:infi, 'suivre', [dr(0,dl(0,np,s_inf),np)-0.9968459 ]),
       si('en', prp, 'en', [dr(0,dl(1,s,s),n)-0.9978824 ]),
       si('temps', nom:m, 'temps', [n-0.99999607 ]),
       si('réel', adj, 'réel', [dl(0,n,n)-0.99963105 ]),
       si('la', det:art, 'la', [dr(0,np,n)-0.99999523 ]),
       si('propagation', nom:fs, 'propagation', [n-0.9999999 ]),
       si('de', prp, 'de', [dr(0,dl(0,n,n),np)-0.99987876 ]),
       si('l\'', det:art, 'l\'', [dr(0,np,n)-0.99999976 ]),
       si('épidémie', nom:fs, 'épidémie', [n-0.9999964 ])
	      ]).

test_sentence(2, [
       si('Jean', nam, 'Coronavirus', [np-0.9, n-0.1 ]),
       si('aime', ver:pres, ':', [dr(0,dl(0,np,s),np)-0.8, dl(0,np,s)-0.1]),
       si('mardi', nom, 'mardi', [dl(0,s,s)-0.6,np-0.4 ])
       ]).


test1(Tree) :-
	compute_best([dr(0,dl(0,np,s),np)-0.8, dl(0,np,s)-0.1], Tree).

compute_best_counts(List) :-
	retractall(count(_,_,_,_,_)),
	compute_best_counts(List, 0).

compute_best_counts([], N) :-
	compute_closure(1, N).
compute_best_counts([si(_,_,_,List)|Rest], N0) :-
	compute_best(List, Tree),
	N is N0 + 1,
	assert_items(Tree, N0, N),
	compute_best_counts(Rest, N).

compute_closure(D0, N) :-
	extend_counts(1, D0, N),
	D is D0 + 1,
     (
	D > N
     ->
        true
     ;
        compute_closure(D, N)
     ).

extend_counts(M0, D, Max) :-
	extend_counts(M0, D),
	M is M0 + 1,
     (
	M + D > Max 
     ->
	true
     ;
	extend_counts(M, D, Max)
     ).

extend_counts(M, D) :-
	/* move one item left for all items of distance D */
	R is M + D,
	L is M - 1,
	findall(count(C,M,R,P,S),count(C,M,R,P,S),ListL),
	findall(count(C2,L,M,P2,S2),count(C2,L,M,P2,S2),List1),
	combine(ListL, L, R, List1).

combine([], _, _, _).
combine([C|Cs], L, R, List) :-
	btree_init(Tree0),
	combine1(List, C, L, R, Tree0),
	combine(Cs, L, R, List).

combine1([], _, L, R, Tree) :-
	assert_items(Tree, L, R).
combine1([count(C2,L,M,P2,[F])|Ds], count(C1,M,R,P1,Seq), L, R, Tree0) :-
	add_counts(C1, C2, C),
	P is P1 * P2,
	NewSeq = [F|Seq],
	btree_update_best_value(Tree0, C, P-NewSeq, Tree),
	combine1(Ds, count(C1,M,R,P1,Seq), L, R, Tree).
	
assert_items(Tree, L, R) :-
	btree_to_list(Tree, List),
	assert_items1(List, L, R).

assert_items1([], _, _).
assert_items1([I|Is], L, R) :-
	assert_item(I, L, R),
	assert_items1(Is, L, R).

assert_item(Count-(Weight-Formulas), Left, Right) :-
	assert(count(Count, Left, Right, Weight, Formulas)).
	
compute_best(List, Best) :-
	btree_init(Tree0),
	compute_best(List, Tree0, Best).

compute_best([], Tree, Tree).
compute_best([F0-Prob|Rest], Tree0, Tree) :-
	translate_form(F0, F1),
	count_formula(F1, C),
	btree_update_best_value(Tree0, C, Prob-[F1], Tree1),
	compute_best(Rest, Tree1, Tree).

add_counts(t(X0,X1,X2,X3,X4,X5,X6),t(Y0,Y1,Y2,Y3,Y4,Y5,Y6),t(Z0,Z1,Z2,Z3,Z4,Z5,Z6)) :-
	Z0 is X0 + Y0,
	Z1 is X1 + Y1,
	Z2 is X2 + Y2,
	Z3 is X3 + Y3,
	Z4 is X4 + Y4,
	Z5 is X5 + Y5,
	Z6 is X6 + Y6.
subtract_counts(t(X0,X1,X2,X3,X4,X5,X6),t(Y0,Y1,Y2,Y3,Y4,Y5,Y6),t(Z0,Z1,Z2,Z3,Z4,Z5,Z6)) :-
	Z0 is X0 - Y0,
	Z1 is X1 - Y1,
	Z2 is X2 - Y2,
	Z3 is X3 - Y3,
	Z4 is X4 - Y4,
	Z5 is X5 - Y5,
	Z6 is X6 - Y6.






simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(_)), s) :- !.
simplify_formula(lit(pp(_)), pp) :- !.
simplify_formula(lit(np(_,_,_)), np) :- !.
simplify_formula(lit(A), A).
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(_,A0), A) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), A) :-
	simplify_formula(A0, A).


count_formula(F, Count) :-
	simplify_formula(F, FS),
	count_formula1(FS, Count).

count_formula1(Atom, Count) :-
	atomic(Atom),
	!,
	atom_vector(Atom, Count).
count_formula1(dr(_,A,B), Count) :-
	!,
	count_formula1(A, CountA),
	count_formula1(B, CountB),
	subtract_counts(CountA, CountB, Count).
count_formula1(dl(_,B,A), Count) :-
	!,
	count_formula1(A, CountA),
	count_formula1(B, CountB),
	subtract_counts(CountA, CountB, Count).
count_formula1(p(_,A,B), Count) :-
	!,
	count_formula1(A, CountA),
	count_formula1(B, CountB),
	add_counts(CountA, CountB, Count).



