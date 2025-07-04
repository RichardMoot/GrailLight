% -*- Mode: Prolog -*-

:- module(sem_utils,  [reduce_sem/2,
		       reduce_lambda/2,
		       substitute_sem/3,
		       replace_sem/4,
		       sem_to_prolog/3,
		       get_variable_types/3,
		       sem_to_prolog_query/2,
		       sem_to_prolog_query/3,
		       check_lexicon_typing/0,
		       is_closed/1,
		       get_max_variable_number/2,
		       get_fresh_variable_number/2,
		       free_vars/2,
		       freeze/2,
		       melt/2,
		       renumbervars/1,
		       renumbervars/2,
		       subterm/2,
		       subterm_with_unify/2,
		       equivalent_semantics/2,
		       unify_semantics/2,
		       try_unify_semantics/2,
		       relabel_sem_vars/2,
		       relabel_sem_vars/4,
		       melt_bound_variables/2,
		       relabel_sem_vars/2,
		       get_drs_types/2,
		       translate_dynamics/3]).

% =

:- use_module(list_utils, [delete_all/3]).
:- use_module(ordset,     [ord_intersect/3,
			   ord_insert/3,
			   ord_delete/3,
			   ord_subset/2,
			   ord_subtract/3]).
:- use_module(tree234, [btree_get/3,
	                btree_insert/4,
			btree_remove/4,
			btree_put/4]).
:- use_module(latex,   [latex_list/2]).
:- use_module(lexicon, [macro_expand/2]).

:- dynamic user:solution_semantics_hook/2.

% no Curry-Howard semantics for the unary connectives Diamond and Box
% change this line to
%
% unary_semantics(active).
%
% if you want explicit semantics for the unary connectives.

unary_semantics(inactive).

reduce_quine(active).

reduce_eta(active).

semantic_set_type(E, _T, E).

% WARNING: though "sloppy" produces simpler structures (less duplication of DRSs), it may be subject to accidental capture
% "sloppy" bindings treats all DRS variable names as having global scope (which is likely to be incorrect, but has the
% advantage of not producing many doubled structures for sentences like "Jean et Marie aiment Pierre et Anne"

drs_binding(sloppy).

% semantic_set_type(E, T, E->T).

% reduce_solution_semantics(+InputSemantics, -OutputSemantics)
%
% allows the user to specify his own Prolog code (by means of
% user:solution_semantics_hook) to handle transformations
% and reductions of the lambda term which is the result of the
% parse.
% example uses of this functionality would include anaphora
% resolution and preposition projection.
% in case no such predicate is specified, basic beta-reduction
% is applied.

reduce_sem(SemIn, SemOut) :-
	/* lambda calculus normalization */
	reduce_lambda(SemIn, SemMid),
	reduce_quine(SemMid, SemQ),
	/* default DRS treatment (if applicable) */
	reduce_drs(SemQ, SemDRS),
	/* user-defined semantic treatment */
	reduce_user(SemDRS, SemOut).

% = DRS merge; simply appends contexts and conditions

reduce_drs(D0, D) :-
	reduce_drs1(D0, D1),
	D0 \=@= D1,
	!,
	reduce_drs(D1, D).
reduce_drs(D, D).

reduce_drs1(merge(drs(X,C),drs(Y,D)), drs(Z,F)) :-
	merge_lists(X, Y, Z),
	merge_lists(C, D, F).

% = DRS presuppositions; move the preposition to the top level

%% reduce_drs1(presup(X,Y), presupp(X,Y), Max, Max) :-
%% 	free_vars(X, FV),
%% 	bound_variables(Y, BV),
%% 	ord_intersect(BV, FV, [_|_]),
%% 	!.	     % "freeze" a presuppositions at the current level
% Quick'n'dirty solution to get complex proper names into a single DRS
reduce_drs1(merge(presup(X,Y),drs(Z,V)), presup(X,merge(Y,drs(Z,V)))).
reduce_drs1(merge(drs(Z,V),presup(X,Y)), presup(X,merge(drs(Z,V),Y))).

reduce_drs1(presup(presup(X,Y),Z), presup(merge(X,Y),Z)).
reduce_drs1(presup(P1,presup(P2,X)), presup(merge(P1,P2),X)).
reduce_drs1(merge(presup(P1,X),presup(P2,Y)), presup(merge(P1,P2),merge(X,Y))).

reduce_drs1(not(presup(P,Q)),presup(P,not(Q))).
reduce_drs1(bool(presup(P,Q),->,R), Red) :-
	free_vars(P, FV),
	bound_variables(Q, BV),
	ord_intersect(FV, BV, Int),
	/* fails if bound variables were to become free */
   (
        Int = []
   ->
        Red = presup(P,bool(Q,->,R))
   ).
reduce_drs1(bool(P,->,presup(Q,R)), Red) :-
	free_vars(Q, FV),
	bound_variables(R, BV),
	ord_intersect(FV, BV, Int),
	/* fails if bound variables were to become free */
    (
        Int = []
    ->
        Red = bool(presup(Q,P),->,R)
    ).
reduce_drs1(drs(V,L0), drs(V, [drs_label(X,merge(Q1,Q2))|L])) :-
	select(drs_label(X,Q1), L0, L1),
	select(drs_label(X,Q2), L1, L).
% TODO: add trapping conditions
reduce_drs1(drs(V,L0), presup(P,drs(V,[not(Q)|L]))) :-
	select(not(presup(P,Q)), L0, L).
% TODO: add trapping conditions
reduce_drs1(drs(V,L0), drs(V,[bool(presup(P,Q),->,R)|L])) :-
	select(bool(Q,->,presup(P,R)), L0, L),
	free_vars(P, FV),
	bound_variables(R, BV),
	ord_intersect(FV, BV, []).
reduce_drs1(drs(V,L0), presup(P,drs(V,[bool(Q,->,R)|L]))) :-
	select(bool(presup(P,Q),->,R), L0, L).
	
reduce_drs1(drs(V,L0), presup(P, drs(V,[Q|L]))) :-
	select(presup(P, Q), L0, L).
reduce_drs1(drs(V,L0), presup(P, drs(V,[drs_label(X,Q)|L]))) :-
	select(drs_label(X,presup(P, Q)), L0, L).

% recursive cases

reduce_drs1(merge(D0,D1), merge(D2,D3)) :-
	reduce_drs1(D0, D2),
	reduce_drs1(D1, D3).
reduce_drs1(presup(D0,D1), presup(D2,D3)) :-
	reduce_drs1(D0, D2),
	reduce_drs1(D1, D3).
reduce_drs1(drs(V,Cs0), drs(V,Cs)) :-
	!,
	reduce_conditions(Cs0, Cs).
reduce_drs1(DRS, DRS).

reduce_conditions([], []).
reduce_conditions([C|Cs], [D|Ds]) :-
	reduce_condition(C, D),
	reduce_conditions(Cs, Ds).

reduce_condition(bool(D0,C,D1), bool(D2,C,D3)) :-
	drs_bool(C),
	!,
	reduce_drs1(D0, D2),
	reduce_drs1(D1, D3).
reduce_condition(bool(A,B,C), bool(A,B,C)) :-
	!.
reduce_condition(not(D0), not(D)) :-
	!,
	reduce_drs1(D0, D).
reduce_condition(drs_label(L,DRS0), drs_label(L,DRS)) :-
	!,
	reduce_drs1(DRS0, DRS).
reduce_condition(appl(F0,A0), appl(F,A)) :-
	!,
	reduce_condition(F0, F),
	reduce_condition(A0, A).
reduce_condition(C, C).

drs_bool(->).
drs_bool(\/).

merge_lists([], Ls2, Ls2).
merge_lists([L1|Ls1], Ls2, [L1|Ls]) :-
	strict_removeall(L1, Ls2, Ls3),
	merge_lists(Ls1, Ls3, Ls).

strict_removeall(_,  [], []).
strict_removeall(E1, [E2|Ls0], Ls) :-
	E1 == E2,
	!,
	strict_removeall(E1, Ls0, Ls).
strict_removeall(E, [L|Ls0], [L|Ls]) :-
	strict_removeall(E, Ls0, Ls).


% = Quine's reductions

reduce_quine(Sem0, Sem) :-
	reduce_quine(active),
	!,
	reduce_quine0(Sem0, Sem).
reduce_quine(Sem, Sem).

reduce_quine0(Sem0, Sem) :-
	reduce_quine1(Sem0, Sem1),
	!,
	reduce_quine0(Sem1, Sem).
reduce_quine0(Sem, Sem).

reduce_quine1(bool(true,&,X), X).
reduce_quine1(bool(X,&,true), X).
reduce_quine1(bool(false,&,_), false).
reduce_quine1(bool(_,&,false), false).

reduce_quine1(bool(true,\/,_), true).
reduce_quine1(bool(_,\/,true), true).
reduce_quine1(bool(false,\/,X), X).
reduce_quine1(bool(X,\/,false), X).

reduce_quine1(bool(true,->,X), X).
reduce_quine1(bool(_,->,true), true).
reduce_quine1(bool(false,->,_), true).
reduce_quine1(bool(X,->,false), not(X)).

reduce_quine1(T, U, Max0, Max) :-
	T =.. [F|Ts],
	reduce_quine_list(Ts, Us, Max0, Max),
	U =.. [F|Us].

reduce_quine_list([T|Ts], [U|Ts], Max0, Max) :-
	reduce_quine1(T, U, Max0, Max).
reduce_quine_list([T|Ts], [T|Us], Max0, Max) :-
	reduce_quine_list(Ts, Us, Max0, Max).


reduce_user(SemIn, SemOut) :-
	user:solution_semantics_hook(_,_),
	!,
	user:solution_semantics_hook(SemIn, SemOut).
reduce_user(Sem, Sem).

% reduce_lambda(+LambdaTerm, -BetaEtaReducedLambdaTerm)
%
% true if BetaEtaReducedLambdaTerm is the beta-eta normal form of
% Lambda Term. Works using a simply repeat loop reducing one redex
% at each step.

reduce_lambda(Term0, Term) :-
	get_fresh_variable_number(Term0, Max),
	reduce_lambda(Term0, Term1, Max, _),
	relabel_sem_vars(Term1, Term).

reduce_lambda(Term0, Term, Max0, Max) :-
	reduce_lambda1(Term0, Term1, Max0, Max1),
	!,
	reduce_lambda(Term1, Term, Max1, Max).

reduce_lambda(Term, Term, Max, Max).

% reduce_lambda1(+Redex, -Contractum).
%
% true if Redex reduces to Contractum in a single beta or eta
% reduction.

reduce_lambda1(appl(lambda(X0,T0),Y), T, Max0, Max) :-
	alpha_conversion(lambda(X0,T0),lambda(X,T1), Max0, Max),
	replace_sem(T1, X, Y, T).
reduce_lambda1(lambda(X,appl(F,X)), F, Max, Max) :-
	reduce_eta(active),
	\+ subterm(F, X).
reduce_lambda1(pi1(pair(T,_)), T, Max, Max).
reduce_lambda1(pi2(pair(_,T)), T, Max, Max).
reduce_lambda1(pair(pi1(T),pi2(T)), T, Max, Max) :-
	reduce_eta(active).
reduce_lambda1(condia(dedia(T)), T, Max, Max).
reduce_lambda1(dedia(condia(T)), T, Max, Max).
reduce_lambda1(conbox(debox(T)), T, Max, Max).
reduce_lambda1(debox(conbox(T)), T, Max, Max).
reduce_lambda1(condia(T), T, Max, Max) :- 
	unary_semantics(inactive).
reduce_lambda1(dedia(T), T, Max, Max) :- 
	unary_semantics(inactive).
reduce_lambda1(conbox(T), T, Max, Max) :- 
	unary_semantics(inactive).
reduce_lambda1(debox(T), T, Max, Max) :- 
	unary_semantics(inactive).

% ad hoc additions 

reduce_lambda1(if_var_else(X, T1, T2), T, Max, Max) :-
    (
         var(X)
    ->
        T = T1
    ;
        T = T2
    ).
reduce_lambda1(if_unify_else(X, Y, T1, T2), T, Max, Max) :-
    (
         X \= Y
    ->
        T = T2
    ;
        T = T1
    ).
reduce_lambda1(if_equals_else(X, Y, T1, T2), T, Max, Max) :-
    (
         X \== Y
    ->
        T = T2
    ;
        T = T1
    ).

% = recursive case

reduce_lambda1(T, U, Max0, Max) :-
	T =.. [F|Ts],
	reduce_lambda_list(Ts, Us, Max0, Max),
	U =.. [F|Us].

reduce_lambda_list([T|Ts], [U|Ts], Max0, Max) :-
	reduce_lambda1(T, U, Max0, Max).
reduce_lambda_list([T|Ts], [T|Us], Max0, Max) :-
	reduce_lambda_list(Ts, Us, Max0, Max).

% subterm(+Term, +SubTerm)
%
% true if Term contains SubTerm as a subterm.

subterm(X, Y) :-
	var(X),
	!,
	X == Y.
subterm(X, Y) :-
	var(Y),
	X == Y.
subterm('$VAR'(N0), Y) :-
	nonvar(Y),
	Y = '$VAR'(N),
	!,
	N = N0.
subterm(lambda(_, X), Y) :-
	!,
	subterm(X, Y).
subterm(X, Y) :-
	functor(X, _, N),
	subterm(N, X, Y).

subterm(N0, X, Y) :-
	N0 > 0,
   (	
	arg(N0, X, A),
	subterm(A, Y)
   ->
        true
   ;
	N0 > 1,
	N is N0 - 1,
	subterm(N, X, Y)
   ).

% subterm_with_unify(+Term, +SubTerm)
%
% true if Term contains SubTerm as a subterm.

subterm_with_unify(X, _) :-
	var(X),
	!,
	fail.
subterm_with_unify(X, Y) :-
	X = Y,
	!.
subterm_with_unify(lambda(_, X), Y) :-
	!,
	subterm_with_unify(X, Y).
subterm_with_unify(X, Y) :-
	functor(X, _, N),
	subterm_with_unify(N, X, Y).

subterm_with_unify(N0, X, Y) :-
	N0 > 0,
	arg(N0, X, A),
	subterm_with_unify(A, Y),
	!.

subterm_with_unify(N0, X, Y) :-
	N0 > 1,
	N is N0 - 1,
	subterm_with_unify(N, X, Y).


% = alpha_conversion(+InTerm, -OutTerm)

alpha_conversion(Term0, Term, Max0, Max) :-
	melt_bound_variables(Term0, Term),
	numbervars(Term, Max0, Max).

% = replace_sem(+InTerm, +Term1, +Term2, -OutTerm)
%
% true if OutTerm is InTerm with all occurrences of Term1 replaced
% by occurrences of Term2.

replace_sem(X0, X, Y0, Y) :-
	X0 == X,
	!,
	Y = Y0.
replace_sem(X, _, _, X) :-
	var(X),
	!.
replace_sem(U, X, Y, V) :-
	functor(U, F, N),
	functor(V, F, N),
	replace_sem(N, U, X, Y, V).

replace_sem(0, _, _, _, _) :- 
	!.
replace_sem(N0, U, X, Y, V) :-
	N0 > 0,
	N is N0 - 1,
	arg(N0, U, A),
	replace_sem(A, X, Y, B),
	arg(N0, V, B),
	replace_sem(N, U, X, Y, V).

% = substitute_sem(+ListOfSubstitutions, +InTerm, -OutTerm).

substitute_sem(L, T0, T) :-
	max_key_list(L, 0, Max0),
	Max1 is Max0 +1,
	numbervars(T0, Max1, Max),
	substitute_sem(L, T0, T, Max, _).

substitute_sem([], T, T, N, N).
substitute_sem([X-U|Rest], T0, T, N0, N) :-
	numbervars(U, N0, N1),
	replace_sem(T0, X, U, T1),
	substitute_sem(Rest, T1, T, N1, N).

max_key_list([], M, M).
max_key_list([Term-_|Ss], M0, M) :-
	get_key(Term, K),
    (
       K > M0
    ->
       max_key_list(Ss, K, M)
    ;
       max_key_list(Ss, M0, M)
    ).

get_key('$VAR'(K), K) :-
	!.
get_key(word(K), K) :-
	!.
get_key(K0, K) :-
	integer(K0),
	!,
	K = K0.
% ensure things work correctly when substituting a term
get_key(Term, K) :-
	get_max_variable_number(Term, 0, K).

% = is_closed(+Term)
%
% true if Term is closed; uses renumbervars to work correctly
% with Prolog variable subterms and simply checks whether
% there are no free variables. Double negation is used to
% prevent instantiating Term

is_closed(Term) :-
	\+ \+ (renumbervars(Term), free_vars(Term, [])).

% = free_vars(+Term, -ListOfVariableIndices)
%
% given a Term representing a lambda term (or lambda-DRS) return
% all indices of variables occurring freely in this lambda term.

free_vars('$VAR'(N), List) :-
	!,
	List = [N].
free_vars(A, []) :-
	atomic(A),
	!.
free_vars(lambda('$VAR'(X),Y), F) :-
	!,
	free_vars(Y, F0),
	ord_delete(F0, X, F).
free_vars(quant(_,'$VAR'(X),Y), F) :-
	!,
	free_vars(Y, F0),
	ord_delete(F0, X, F).
% this is the only case which is slightly complicated:
% the variables in the DRS on the left hand side of an
% implication bind occurrences of these variables in the
% conditions on the right hand side of the implication
% as well.
free_vars(bool(drs(V0,C0),->,drs(V1,C1)), F) :-
	!,
	drs_variable_numbers(V0, VN0),
	drs_variable_numbers(V1, VN1),
	% free variables inside C1 (in FR)
	free_vars_list(C1, [], F0),
	ord_subtract(F0, VN1, F1),
	ord_subtract(F1, VN0, FR),
	% free variables inside C0 (in FL)
	free_vars_list(C0, [], F3),
	ord_subtract(F3, VN0, FL),
	ord_union(FL, FR, F).
free_vars(drs(V0,C), F) :-
	!,
	free_vars_list(C, [], F0),
	drs_variable_numbers(V0, V),
	ord_subtract(F0, V, F).
% T is compound and does not start with a "binder".
free_vars(T, F) :-
	T =.. [Fun|Args],
	free_vars_list([Fun|Args], [], F).

free_vars_list([], F, F).
free_vars_list([A|As], F0, F) :-
	free_vars(A, V),
	ord_union(F0, V, F1),
	free_vars_list(As, F1, F).

bound_variables(Var, []) :-
	var(Var),
	!.
bound_variables(presup(A,B), BVs) :-
	!,
	bound_variables(A, BVsA),
	bound_variables(B, BVsB),
	ord_union(BVsA, BVsB, BVs).
bound_variables(presupp(A,B), BVs) :-
	!,
	bound_variables(A, BVsA),
	bound_variables(B, BVsB),
	ord_union(BVsA, BVsB, BVs).
bound_variables(merge(A,B), BVs) :-
	!,
	bound_variables(A, BVsA),
	bound_variables(B, BVsB),
	ord_union(BVsA, BVsB, BVs).
bound_variables(drs(V, L), BVs) :-
	!,
   (
	drs_binding(sloppy)
   ->
	bound_variables_conditions(L, BVs)
   ;	  
        /* WARNING: this may lead to accidental capture of DRS variables */
        drs_variable_numbers(V, BVs0),
	bound_variables_conditions(L, BVs1),
	ord_union(BVs0, BVs1, BVs)
   ).
bound_variables(lambda(X, Y), BVs) :-
	 !,
    (
	var(X)
    ->
        /* already molten */
        bound_variables(Y, BVs)
    ;
	X = '$VAR'(N),
	bound_variables(Y, BVs0),
	ord_insert(BVs0, N, BVs)
    ).
bound_variables('$VAR'(_), []) :-
	!.
bound_variables(X, []) :-
	atomic(X),
	!.
bound_variables(Term, BVs) :-
	Term =.. List,
	bound_variables_list(List, BVs).

bound_variables_list([], []).
bound_variables_list([V|Vs], Bs) :-
	bound_variables(V, Bs0),
	bound_variables_list(Vs, Bs1),
	ord_union(Bs0, Bs1, Bs).

bound_variables_conditions([], []).
bound_variables_conditions([C|Cs], BVs) :-
	bound_variables_cond(C, BVs0),
	bound_variables_conditions(Cs, BVs1),
	ord_union(BVs0, BVs1, BVs).


bound_variables_cond(bool(A,->,B), BVs) :-
	!,
	bound_variables(A, BVs0),
	bound_variables(B, BVs1),
	ord_union(BVs0, BVs1, BVs).
bound_variables_cond(not(A), BVs) :-
	!,
	bound_variables(A, BVs).
bound_variables_cond(_, []).

drs_variable_numbers(L, N) :-
	drs_variable_numbers(L, [], N).
drs_variable_numbers([], R, R).
drs_variable_numbers([V|Vs], R0, R) :-
	drs_variable_numbers1(V, R0, R1),
	drs_variable_numbers(Vs, R1, R).

drs_variable_numbers1('$VAR'(N), R0, R) :-
	ord_insert(R0, N, R).
drs_variable_numbers1(event('$VAR'(N)), R0, R) :-
	ord_insert(R0, N, R).
drs_variable_numbers1(variable('$VAR'(N)), R0, R) :-
	ord_insert(R0, N, R).

% = relabel_sem_vars(T0, T)
%
% rename the variables in term T0 in such a way that all variables are in
% the range '$VAR'(0) to '$VAR'(N) for the smallest N possible.

relabel_sem_vars(T0, T) :-
	relabel_sem_vars(T0, T, 0, _, empty, _).

relabel_sem_vars(T0, T, N0, N) :-
	relabel_sem_vars(T0, T, N0, N, emtpy, _).

% relabel_sem(T0, T, M0, M)

relabel_sem_vars('$VAR'(I), '$VAR'(J), N0, N, M0, M) :-
	!,
    (
        btree_get(M0, I, J)
    ->
        M = M0,
        N = N0
    ;
        J = N0,
        N is N0 +1,
        btree_insert(M0, I, J, M)
    ).
relabel_sem_vars(Term0, Term, N0, N, M0, M) :-
	functor(Term0, F, A),
	functor(Term, F, A),
	relabel_sem_vars_args(1, A, Term0, Term, N0, N, M0, M).

relabel_sem_vars_args(A0, A, Term0, Term, N0, N, M0, M) :-
    (
	A0 > A
    ->
        N = N0,
        M = M0
    ;
        arg(A0, Term0, Arg0),
        arg(A0, Term, Arg),
        relabel_sem_vars(Arg0, Arg, N0, N1, M0, M1),
        A1 is A0 + 1,
        relabel_sem_vars_args(A1, A, Term0, Term, N1, N, M1, M)
    ).

create_tree([], Tree, Tree).
create_tree([I|Vs], Tree0, Tree) :-
	btree_put(Tree0, I, _, Tree1),
	create_tree(Vs, Tree1, Tree).

% = equivalent_semantics(+Term1, +Term2)
%
% true if Term1 and Term2 are alpha equivalent

equivalent_semantics(Term1, Term2) :-
	/* replaced bound lambda variables by Prolog variables and check for Prolog alphabetic variance */
	melt_bound_variables(Term1, TermA),
	melt_bound_variables(Term2, TermB),
	TermA =@= TermB.

% = unify_semantics(+Term1, +Term2)
%
% unifies Term1 and Term2

unify_semantics(Term1, Term2) :-
	/* replaced bound lambda variables by Prolog variables and use Prolog unification */
	melt_bound_variables(Term1, TermA),
	melt_bound_variables(Term2, TermB),
	TermA = TermB.

is_sem_var(X) :-
	var(X),
	!.
is_sem_var('$VAR'(_)).

try_unify_semantics('$VAR'(N), '$VAR'(N)) :-
	!.
try_unify_semantics(Atom0, Atom) :-
	atomic(Atom0),
	!,
        (Atom = Atom0 -> true ; true).
try_unify_semantics(lambda(X0,Y0), lambda(X,Y)) :-
	(is_sem_var(X0) -> true ; replace_sem(Y0, X0, X, Y1), try_unify_semantics(Y1, Y)).
try_unify_semantics(Term0, Term) :-
   (
	compound(Term0)
   ->
	functor(Term0, F, A),
	functor(Term, F, A),
	try_unify_semantics(1, A, Term0, Term)
    ;
        true
    ).

try_unify_semantics(A0, A, Term0, Term) :-
   (
	A0 =< A	
   ->
	arg(A, Term0, Arg0),
	arg(A, Term, Arg),
	try_unify_semantics(Arg0, Arg),
	A1 is A0 + 1,
	try_unify_semantics(A1, A, Term0, Term)
   ;
        true
   ).

% = melt_bound_variables(+Term0, -Term)
%
% true if Term is identical to Term0 but with all occurrences of
% bound variables (because of numbervars, all variables are of the form
% '$VAR'(N)') replaced by Prolog variables.

melt_bound_variables(Term0, Term) :-
	bound_variables(Term0, List),
	create_tree(List, empty, Tree),
	melt_bound_variables(Term0, Term, Tree).

melt_bound_variables(X, X, _Tree) :-
	var(X),
	!.
melt_bound_variables('$VAR'(I), Var, Tree) :-
    (
         btree_get(Tree, I, Var)
    ->
         true
    ;
         Var = '$VAR'(I)
    ).
melt_bound_variables(drs(Vars0,Conds0), drs(Vars,Conds), Tree) :-
	!,
    (
	drs_binding(sloppy)
    ->
	/* WARNING: we allow "accidental capture" of variables bound by DRS boxes here ! */
	/* this avoids duplication of referents under different names, eg. */
        /* "John and Peter love Sue" will not have two different variables both named */
        /* "Sue"; this is a pragmatic choice and care must be taken! */
	/* uncomment line below (while commenting the line "Vars0 = Vars" to */
	/* obtain correct solution */
	Vars0 = Vars
     ;		    
        melt_drs_variables(Vars0, Vars, Tree)
     ),		       
	melt_bound_variables(Conds0, Conds, Tree).
melt_bound_variables(Term0, Term, Tree) :-
	functor(Term0, F, A),
	functor(Term, F, A),
	melt_bound_variables_args(1, A, Term0, Term, Tree).

melt_bound_variables_args(A0, A, Term0, Term, Tree) :-
    (
	A0 > A
    ->
        true
    ;
        arg(A0, Term0, Arg0),
        arg(A0, Term, Arg),
        melt_bound_variables(Arg0, Arg, Tree),
        A1 is A0 + 1,
        melt_bound_variables_args(A1, A, Term0, Term, Tree)
    ).

melt_drs_variables([], [], _).
melt_drs_variables([V0|Vs0], [V|Vs], Tree) :-
	melt_drs_variable(V0, V, Tree),
	melt_drs_variables(Vs0, Vs, Tree).

melt_drs_variable('$VAR'(I), Var, Tree) :-
	!,
    (
         btree_get(Tree, I, Var)
    ->
         true
    ;
         Var = '$VAR'(I)
    ).
melt_drs_variable(variable('$VAR'(I)), Var, Tree) :-
	!,
    (
         btree_get(Tree, I, Var)
    ->
         true
    ;
         Var = variable('$VAR'(I))
    ).
melt_drs_variable(event('$VAR'(I)), Var, Tree) :-
    (
         btree_get(Tree, I, Var)
    ->
         true
    ;
         Var = '$VAR'(I)
    ).

% = get_variable_numbers(+LambdaTerm, -SetOfIntegers)
%
% true if SetOfIntegers contains all variable number which
% are the result of a call to numbervars/3 (that is to say,
% the set of all N which occur as a subterm '$VAR'(N))

get_variable_numbers(Term, Set) :-
	get_variable_numbers(Term, List, []),
	sort(List, Set).

get_variable_numbers(Var, L, L) :-
	var(Var),
	!.
get_variable_numbers('$VAR'(N), [N|L], L) :-
	!.
get_variable_numbers(Term, L0, L) :-
	functor(Term, _, A),
	get_variable_numbers_args(1, A, Term, L0, L).

get_variable_numbers_args(A0, A, Term, L0, L) :-
    (
        A0 > A
    ->
        L = L0
    ;
        arg(A0, Term, Arg),
        get_variable_numbers(Arg, L0, L1),
        A1 is A0 +1,
        get_variable_numbers_args(A1, A, Term, L1, L)
    ).

renumbervars(Term) :-
	renumbervars(Term, _).

renumbervars(Term0, MaxVar) :-
	get_fresh_variable_number(Term0, MaxVar0),
	numbervars(Term0, MaxVar0, MaxVar).


% = get_fresh_variable_number(+LambdaTerm, ?FreshVar)
%
% true if FreshVar is the largest number N which
% occurs as a subterm '$VAR'(N) of LambdaTerm
% That is to say, the call
%
%   numbervars(LambdaTerm, MaxVar, NewMaxVar)
%
% is guaranteed to be sound (in the sense that
% it does not accidentally unifies distinct
% variables.
%
% If LambdaTerm contains no occurrences of a
% subterm '$VAR'(N) then MaxVar is defined as
% 0.

get_fresh_variable_number(Term, Max) :-
	get_max_variable_number(Term, -1, Max0),
	Max is Max0 + 1.

% = get_max_variable_number(+LambdaTerm, ?MaxVar)
%
% true if MaxVar+1 is the largest number N which
% occurs as a subterm '$VAR'(N) of LambdaTerm
% That is to say, the call
%
%   numbervars(LambdaTerm, MaxVar, NewMaxVar)
%
% is guaranteed to be sound (in the sense that
% it does not accidentally unifies distinct
% variables.
%
% If LambdaTerm contains no occurrences of a
% subterm '$VAR'(N) then MaxVar is defined as
% -1 (according to the intended semantics).

get_max_variable_number(Term, Max) :-
	get_max_variable_number(Term, -1, Max).

get_max_variable_number(Var, Max, Max) :-
	var(Var),
	!.
get_max_variable_number('$VAR'(N), Max0, Max) :-
	!,
   (
	integer(N)
   ->
        Max is max(N,Max0)
   ;			 
        Max = Max0
   ).
get_max_variable_number(Term, Max0, Max) :-
	functor(Term, _, A),
	get_max_variable_number_args(1, A, Term, Max0, Max).

get_max_variable_number_args(A0, A, Term, Max0, Max) :-
    (
        A0 > A
    ->
        Max = Max0
    ;
        arg(A0, Term, Arg),
        get_max_variable_number(Arg, Max0, Max1),
        A1 is A0 +1,
        get_max_variable_number_args(A1, A, Term, Max1, Max)
    ).


get_variable_types(Sem, Formula, Tree) :-
    (
	/* skip typing checking if no atomic_type/2 declarations */
        /* are found in the grammar file */
	user:atomic_type(_, _)
    ->
        get_variable_types1(Sem, Formula, Tree)
    ;
        Tree = empty
    ).

% = skip variable typing in case the term is not well-typed

get_variable_types1(Sem, Formula, Tree) :-
	get_atomic_types(Tr),
	syntactic_to_semantic_type(Formula, goal, Type, Tr),
        check_type(Sem, Type, goal, empty, Tree),
	!.
get_variable_types1(_, _, empty).



% check_lexicon_typing.
%
% check if the lambda term semantics in the lexicon is well-typed.

check_lexicon_typing :-
    (
	/* skip typing checking if no atomic_type/2 declarations */
        /* are found in the grammar file */
	user:atomic_type(_, _)
    ->
	get_atomic_types(Tree),
	findall(lex(A,B,C), lexicon_triple(A,B,C), Lexicon),
	check_lexicon_typing(Lexicon, Tree)
    ;
	format('{Warning: no atomic_type/2 declarations found in grammar}~n{Warning: semantic type verification disabled}~n', []),
	format(log, '{Warning: no atomic_type/2 declarations found in grammar}~n{Warning: semantic type verification disabled}~n', [])
    ).

check_lexicon_typing([], _).
check_lexicon_typing([lex(Word,SynType,Term)|Ls], Tree) :-
	numbervars(Term, 0, _),
	format(log, 'lex( ~w ,~n     ~w ,~n     ~w )~n', [Word, SynType, Term]),
    (
	syntactic_to_semantic_type(SynType, Word, Type, Tree)
    ->
	format(log, 'Type: ~w~nTerm: ~w~n', [Type, Term]),
	check_lexicon_typing(Term, Type, Word)
    ;
	true
    ),
	check_lexicon_typing(Ls, Tree).

check_lexicon_typing(Term, Type, Word) :-
	check_type(Term, Type, Word, empty, _),
	!.
check_lexicon_typing(Term, Type, Word) :-
	format(log, '{Warning: typing check failed for:}~n{Word: ~w}~n{Term: ~w}~n{Type: ~w}~n', [Word,Term,Type]),
	format('{Warning: typing check failed for:}~n{Word: ~w}~n{Term: ~w}~n{Type: ~w}~n', [Word,Term,Type]).


lexicon_triple(A, B, C) :-
	user:lex(A, B0, C),
	macro_expand(B0, B).

lexicon_triple(A, B, C) :-
	user:default_semantics(A, B0, C),
	macro_expand(B0, B),
	instantiate(A, B).

lexicon_triple(A, B, C) :-
	user:default_semantics(A, _, B0, C),
	macro_expand(B0, B),
	instantiate(A, B).

instantiate('WORD'(B), B) :-
	!.
instantiate(_, _).

check_type(At, Type, Word, Tr0, Tr) :-
	atomic(At),
	!,
    (
	btree_get(Tr0, At, TA)
    ->
	Tr0 = Tr,
	verify_expected_type(At, Word, Type, TA)
    ;
	btree_insert(Tr0, At, Type, Tr)
    ).	
check_type('WORD'(_), _Type, _Word, Tr, Tr) :-
	!.
check_type('$VAR'(N), Type, Word, Tr0, Tr) :-
	!,
    (
	btree_get(Tr0, N, TN)
    ->
	Tr = Tr0,
	verify_expected_type('$VAR'(N), Word, Type, TN)
    ;
	btree_insert(Tr0, N, Type, Tr)
    ).
check_type(lambda('$VAR'(N),T), Type, Word, Tr0, Tr) :-
	!,
    (
	Type = (V->W)
    ->
	btree_insert(Tr0, N, V, Tr1),
	check_type(T, W, Word, Tr1, Tr)
    ;
	Tr = Tr0,
	format('{Typing Error (~w): expected type(~p)->type(~W) found ~W}~n', [Word,'$VAR'(N),T,[numbervars(true)],Type,[numbervars(true)]]),
	format(log, '{Typing Error (~w): expected type(~w)->type(~W) found ~W}~n', [Word,'$VAR'(N),T,[numbervars(true)],Type,[numbervars(true)]])
    ).
check_type(sub(X,_), Type, W, Tr0, Tr) :-
	!,
	check_type(X, Type, W, Tr0, Tr).
check_type(sup(X,_), Type, W, Tr0, Tr) :-
	!,
	check_type(X, Type, W, Tr0, Tr).
check_type(num(X), E, W, Tr0, Tr) :-
	!,
	e_type(E),
	check_type(X, E, W, Tr0, Tr).
check_type(complement(X), E, W, Tr0, Tr) :-
	!,
	e_type(E),
	check_type(X, E, W, Tr0, Tr).
check_type(time(X), E, W, Tr0, Tr) :-
	!,
	verify_e_type(time(X), W, E),
	s_type(S),
	check_type(X, S, W, Tr0, Tr).
check_type(count(X), E, W, Tr0, Tr) :-
	!,
	e_type(E),
	t_type(T),
	check_type(X, E->T, W, Tr0, Tr).
check_type(not(X), T, W, Tr0, Tr) :-
	!,
	check_type(X, T, W, Tr0, Tr).
check_type(debox(X), T, W, Tr0, Tr) :-
	!,
	check_type(X, T, W, Tr0, Tr).
check_type(conbox(X), T, W, Tr0, Tr) :-
	!,
	check_type(X, T, W, Tr0, Tr).
check_type(dedia(X), T, W, Tr0, Tr) :-
	!,
	check_type(X, T, W, Tr0, Tr).
check_type(condia(X), T, W, Tr0, Tr) :-
	!,
	check_type(X, T, W, Tr0, Tr).
check_type(appl(X,Y), W, Word, Tr0, Tr) :-
	!,
	check_type(X, (V->W), Word, Tr0, Tr1),
	check_type(Y, V, Word, Tr1, Tr).
check_type(quant(Q,X), T, Word, Tr0, Tr) :-
	e_type(E),
	t_type(U),
    (
        Q = iota
    ->
        verify_e_type(quant(Q,X), Word, T),
        V = (T -> U)
    ;
        V = (E -> U)
    ),
	!,
        check_type(X, V, Word, Tr0, Tr).
check_type(quant(Q,V,X), T, Word, Tr0, Tr) :-
	!,
	e_type(E),
	check_type(V, E, Word, Tr0, Tr1),  
    (
	Q = iota
    ->
	verify_e_type(quant(Q,V,X), Word, T)
    ;
        t_type(U),
        verify_expected_type(quant(Q,V,X), Word, T, U)
    ),
	check_type(X, t, Word, Tr1, Tr).
% allow polymorphic booleans
check_type(bool(X,B,Y), T, Word, Tr0, Tr) :-
	!,
	e_type(E),
	t_type(T),
	operator_type(B, E, T, U, V),
	check_type(X, U, Word, Tr0, Tr1),
	check_type(Y, V, Word, Tr1, Tr).
check_type(pair(X,Y), Type, Word, Tr0, Tr) :-
	!,
    (
	Type = U-V
    ->
	check_type(X, U, Word, Tr0, Tr1),
	check_type(Y, V, Word, Tr1, Tr)
    ;
	Tr = Tr0,
	format('{Typing Error (~w): expected type(~p)->type(~w) found ~w}~n', [Word,pair(X,Y),U-V,Type]),
	format(log, '{Typing Error (~w): expected type(~w)->type(~w) found ~w}~n', [Word,pair(X,Y),U-V,Type])
    ).
check_type(fst(X), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type-_, Word, Tr0, Tr).
check_type(snd(X), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, _-Type, Word, Tr0, Tr).
check_type(pi1(X), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type-_, Word, Tr0, Tr).
check_type(pi2(X), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, _-Type, Word, Tr0, Tr).
% special rules for alternative semantics
% = DRT
check_type(merge(X,Y), Type, Word, Tr0, Tr) :-
	!,
	t_type(T),
	verify_expected_type(merge(X,Y), Word, Type, T),
	check_type(X, T, Word, Tr0, Tr1),
	check_type(Y, T, Word, Tr1, Tr).
check_type(presup(X,Y), Type, Word, Tr0, Tr) :-
	!,
	t_type(T),
	verify_expected_type(presup(X,Y), Word, Type, T),
	check_type(X, T, Word, Tr0, Tr1),
	check_type(Y, T, Word, Tr1, Tr).
check_type(presupp(X,Y), Type, Word, Tr0, Tr) :-
	!,
	t_type(T),
	verify_expected_type(presupp(X,Y), Word, Type, T),
	check_type(X, T, Word, Tr0, Tr1),
	check_type(Y, T, Word, Tr1, Tr).
check_type(drs(V,C), Type, Word, Tr0, Tr) :-
	!,
	t_type(T),
	verify_expected_type(drs(V,C), Word, Type, T),
	check_var_list(V, Word, Tr0, Tr1),
	check_cond_list(C, Word, Tr1, Tr).
check_type(drs_label(L,D), Type, Word, Tr0, Tr) :-
	!,
	s_type(S),
	check_type(L, S, Word, Tr0, Tr1),
	check_type(D, Type, Word, Tr1, Tr).
check_type(smash(X), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type, Word, Tr0, Tr).
% = montegovian dynamics
check_type(sel(X,Y), Type, Word, Tr0, Tr) :-
	!,
	s_type(S),
	verify_e_type(sel(X,Y), Word, Type),
	check_type(Y, S, Word, Tr0, Tr).
check_type(update(X,Y), Type, Word, Tr0, Tr) :-
	!,
	e_type(E),
	s_type(S),
	verify_s_type(update(X,Y), Word, Type),
	check_type(X, E, Word, Tr0, Tr1),
	check_type(Y, S, Word, Tr1, Tr).
check_type(erase_context(X), S, Word, Tr0, Tr) :-
	!,
	s_type(S),
	check_type(X, S, Word, Tr0, Tr).
check_type(ref(_,X,_), E, Word, Tr0, Tr) :-
	!,
	e_type(E),
	check_type(X, E, Word, Tr0, Tr).
% = if_then_else hacks
check_type(if_var_else(_,X,Y), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type, Word, Tr0, Tr1),
	check_type(Y, Type, Word, Tr1, Tr).
check_type(if_unify_else(_,_,X,Y), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type, Word, Tr0, Tr1),
	check_type(Y, Type, Word, Tr1, Tr).
check_type(if_equals_else(_,_,X,Y), Type, Word, Tr0, Tr) :-
	!,
	check_type(X, Type, Word, Tr0, Tr1),
	check_type(Y, Type, Word, Tr1, Tr).

% unknown term
check_type(Term, Type, Word, Tr, Tr) :-
	format('{Typing Error (~w): unknown term ~k of type ~p}~n', [Word,Term,Type]),
	format(log, '{Typing Error (~w): unknown term ~k of type ~p}~n', [Word,Term,Type]).	

% = polymorphic
operator_type((=) , _, _, X, X).
operator_type((:=) , _, _, X, X).
operator_type(< , _, _, X, X).
operator_type(> , _, _, X, X).
operator_type(neq , _, _, X, X).
% = DRS
operator_type(overlaps , _, _, X, X).
operator_type(abuts , _, _, X, X).
% = entity and set
operator_type(in, E, T, E, Set) :-
	semantic_set_type(E, T, Set).
operator_type(not_in, E, T, E, Set) :-
	semantic_set_type(E, T, Set).
% = set and set
operator_type(intersect, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(intersection, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(setminus, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(union, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(subset, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(subseteq, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(nsubseteq, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
operator_type(subsetneq, E, T, Set, Set) :-
	semantic_set_type(E, T, Set).
% = default to boolean
operator_type(_, _, T, T, T).

verify_expected_type(Term, Word, ExpectedType, Type) :-
    (
	Type = ExpectedType
    ->
	true
    ;
	format('{Typing Error (~w): expected type(~p) = ~w, found ~w}~n', [Word,Term,ExpectedType,Type]),
	format(log, '{Typing Error (~w): expected type(~w) = ~w, found ~w}~n', [Word,Term,ExpectedType,Type])
    ).

verify_e_or_s_type(Term, Word, Type) :-
	findall(ES, (e_type(ES) ; s_type(ES)), ESs),
    (
        memberchk(Type, ESs)
    ->
        true
    ;
	format('{Typing Error (~w): expected either an entity or a state/event type for type(~p) in ~w, found ~w}~n', [Word,Term,Es,Type]),
	format('{Typing Error (~w): expected either an entity or a state/event type for type(~p) in ~w, found ~w}~n', [Word,Term,Es,Type])
    ).
	

verify_e_type(Term, Word, Type) :-
	findall(E, e_type(E), Es),
    (
	memberchk(Type, Es)
    ->
        true
    ;
	format('{Typing Error (~w): expected entity for type(~p) in ~w, found ~w}~n', [Word,Term,Es,Type]),
	format('{Typing Error (~w): expected entity for type(~p) in ~w, found ~w}~n', [Word,Term,Es,Type])
    ).

verify_s_type(Term, Word, Type) :-
	findall(S, s_type(S), Ss),
    (
	memberchk(Type, Ss)
    ->
        true
    ;
	format('{Typing Error (~w): expected state type for type(~p) in ~w, found ~w}~n', [Word,Term,Ss,Type]),
	format('{Typing Error (~w): expected state type for type(~p) in ~w, found ~w}~n', [Word,Term,Ss,Type])
    ).

verify_t_type(Term, Word, Type) :-
	findall(T, t_type(T), Ts),
    (
	memberchk(Type, Ts)
    ->
        true
    ;
	format('{Typing Error (~w): expected boolean type for type(~p) in ~w, found ~w}~n', [Word,Term,Ts,Type]),
	format('{Typing Error (~w): expected boolean type for type(~p) in ~w, found ~w}~n', [Word,Term,Ts,Type])
    ).



check_var_list([], _, Tr, Tr).
check_var_list([V|Vs], Word, Tr0, Tr) :-
	check_var(V, Word, Tr0, Tr1),
	check_var_list(Vs, Word, Tr1, Tr).

check_var(event(V), Word, Tr0, Tr) :-
	!,
	check_type(V, S, Word, Tr0, Tr),
	verify_s_type(V, Word, S).
check_var(variable(V), Word, Tr0, Tr) :-
	!,
	check_type(V, E, Word, Tr0, Tr),
	verify_e_type(V, Word, E).
check_var(set_variable(V), Word, Tr0, Tr) :-
	!,
	check_type(V, E->T, Word, Tr0, Tr),
	verify_e_type(V, Word, E),
	verify_t_type(V, Word, T).
check_var(constant(V), Word, Tr0, Tr) :-
	!,
	check_type(V, E, Word, Tr0, Tr),
	verify_e_type(V, Word, E).
check_var(V, Word, Tr0, Tr) :-
	check_type(V, E, Word, Tr0, Tr),
	verify_e_type(V, Word, E).


check_cond_list([], _, Tr, Tr).
check_cond_list([C|Cs], Word, Tr0, Tr) :-
	t_type(T),
	check_type(C, T, Word, Tr0, Tr1),
	check_cond_list(Cs, Word, Tr1, Tr).

syntactic_to_semantic_type(lit(np(_,_,_)), _W, T, Tree) :-
	!,
	btree_get(Tree, np, T).
syntactic_to_semantic_type(lit(pp(_)), _W, T, Tree) :-
	!,
	btree_get(Tree, pp, T).
syntactic_to_semantic_type(lit(s(_)), _W, T, Tree) :-
	!,
	btree_get(Tree, s, T).
syntactic_to_semantic_type(lit(A), _W, T, Tree) :-
	!,
	btree_get(Tree, A, T).
syntactic_to_semantic_type(dia(_,A), W, TA, Tree) :-
	!,
	syntactic_to_semantic_type(A, W, TA, Tree).
syntactic_to_semantic_type(box(_,A), W, TA, Tree) :-
	!,
	syntactic_to_semantic_type(A, W, TA, Tree).
syntactic_to_semantic_type(dr(_,B,A), W, (TA->TB), Tree) :-
	!,
	syntactic_to_semantic_type(A, W, TA, Tree),
	syntactic_to_semantic_type(B, W, TB, Tree).
syntactic_to_semantic_type(dl(_,A,B), W, (TA->TB), Tree) :-
	!,
	syntactic_to_semantic_type(A, W, TA, Tree),
	syntactic_to_semantic_type(B, W, TB, Tree).
syntactic_to_semantic_type(p(_,A,B), W, TA-TB, Tree) :-
	!,
	syntactic_to_semantic_type(A, W, TA, Tree),
	syntactic_to_semantic_type(B, W, TB, Tree).
syntactic_to_semantic_type(Syn, W, _, _) :-
	format('{Formula Error(~w): unknown syntactic formula ~w}~n', [W,Syn]),
	format(log, '{Formula Error(~w): unknown syntactic formula ~w}~n', [W,Syn]),
	fail.
	
e_type(E) :-
    (
	user:entity_type(_)
    ->
	user:entity_type(E)
    ;
        E = e
    ).

drs_type(Drs) :-
    (
        user:drs_type(_)
    ->
        user:drs_type(Drs)
    ;
        t_type(Drs)
    ).

t_type(Bool) :-
    (
	user:boolean_type(_)
    ->
	user:boolean_type(Bool)
	
    ;
        Bool = t
    ).

s_type(State) :-
    (
	user:state_type(_)
    ->
	user:state_type(State)
    ;
        State = s
    ).


get_atomic_types(Tree) :-
	findall(D, user:atomic_formula(D), Atoms0),
	sort(Atoms0, Atoms),
	get_atomic_types(Atoms, empty, Tree).

get_atomic_types([], T, T).
get_atomic_types([A|As], T0, T) :-
    (
	user:atomic_type(A, Type)
    ->
	btree_insert(T0, A, Type, T1)
    ;
	format('{Error: unknown semantic type for atomic type ~w}~n', [A]),
	format(log, '{Error: unknown semantic type for atomic type ~w}~n', [A]),
	T1 = T0
    ),
	get_atomic_types(As, T1, T).


get_drs_types(Sem, Tree) :-
	get_drs_types(Sem, empty, Tree).

get_drs_types('is a variable', T, T) :-
	!.
get_drs_types('$VAR'(_), T, T) :-
	!.
get_drs_types(A, T, T) :-
	atomic(A).
get_drs_types(drs(Vs,Cs), T0, T) :-
	!,
	add_drs_variables(Vs, T0, T1),
	get_conditions_types(Cs, T1, T).
get_drs_types(lambda(X,Term), T0, T) :-
	functor(X, '$VAR', 1),
	arg(1, X, N),
	integer(N),
	btree_remove(T0, N, s, T1),
	!,
	get_drs_types(Term, T1, T).
get_drs_types(Term, T0, T) :-
	Term =.. Ls,
	get_conditions_types(Ls, T0, T).


get_conditions_types([], T, T).
get_conditions_types([C|Cs], T0, T) :-
	get_drs_types(C, T0, T1),
	!,
	get_conditions_types(Cs, T1, T).

add_drs_variables([], T, T).
add_drs_variables([V|Vs], T0, T) :-
	add_drs_variable(V, T0, T1),
	add_drs_variables(Vs, T1, T).

add_drs_variable(event('$VAR'(N)), T0, T) :-
	btree_insert(T0, N, s, T),
	!.
add_drs_variable(variable('$VAR'(N)), T0, T) :-
   (	
	btree_remove(T0, N, s, T)
   ->
        true
   ;
        T = T0
   ).
add_drs_variable('$VAR'(N), T0, T) :-
   (	
	btree_remove(T0, N, s, T)
   ->
        true
   ;
        T = T0
   ).
		    
% sem_to_prolog(Goal, LambdaTerm, PrologGoal)
%

sem_to_prolog(Goal, LambdaTerm, PrologGoals) :-
    (
	user:sentence_category(Goal, Category)
    ->
	sem_to_prolog1(Category, LambdaTerm, PrologGoals)
    ;
	format('{Error: unknown sentence type ~w, no database query performed}~n', [Goal]),
	format(log, '{Error: unknown sentence type ~w, no database query performed}~n', [Goal])
    ).

% sem_to_prolog(SentenceType, +LambdaTerm, ListOfPrologClauses)
%
% convert lambda term semantics to Prolog term for use with database
% queries.

sem_to_prolog1(yes_no_question, LambdaTerm, [PrologGoal]) :-
	sem_to_prolog_query(LambdaTerm, PrologGoal, _Vars),
	query_results_yn(PrologGoal).
sem_to_prolog1(wh_question, LambdaTerm, [PrologGoal]) :-
	sem_to_prolog_query(LambdaTerm, PrologGoal, Vars),
	query_results_wh(PrologGoal, Vars).
sem_to_prolog1(assertion, LambdaTerm, PrologClauses) :-
	sem_to_fol(LambdaTerm, FOL, pos, neg, [], empty, _),
	fol_to_prolog_clause_list(FOL, PrologClauses, []),
	assert_list(PrologClauses).

sem_to_fol('$VAR'(N), X, _, _, U, T, T) :-
	!,
    (
	btree_get(T, N, Q-Var)
    ->
	handle_variable(Q, Var, [N|U], X)
    ;
	true
    ).
sem_to_fol(quant(Q,V,X0), X, Pol, NPol, U0, T0, T) :-
	!,
	V = '$VAR'(N),
    (
	universal_quantifier(Q, Pol)
    ->
	btree_put(T0, N, u-_, T1),
	U = [N|U0]
    ;
	btree_put(T0, N, e-_, T1),
	U = U0
    ),
	sem_to_fol(X0, X, Pol, NPol, U, T1, T).
sem_to_fol(bool(X0,C,Y0), Term, Pol, NPol, U, T0, T) :-
	!,
	handle_boolean(C, X0, Y0, Term, Pol, NPol, U, T0, T).
sem_to_fol(not(X0), not(X), Pol, NPol, U, T0, T) :-
	!,
	sem_to_fol(X0, X, NPol, Pol, U, T0, T).
sem_to_fol(appl(appl(appl(X0,Y0),Z0),V0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T1),
	sem_to_term(Z0, Z, Pol, NPol, U, T1, T2),
	sem_to_term(V0, V, Pol, NPol, U, T2, T),
	Term =.. [query_db, X, Y, Z, V].
sem_to_fol(appl(appl(X0,Y0),Z0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T1),
	sem_to_term(Z0, Z, Pol, NPol, U, T1, T),
	Term =.. [query_db, X, Y, Z].
sem_to_fol(appl(X0,Y0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T),
	Term =.. [query_db, X, Y].
sem_to_fol(A, A, _, _, _, T, T) :-
	atomic(A).


sem_to_atom(A, A) :-
	atomic(A).

sem_to_term('$VAR'(N), X, _, _, U, T, T) :-
	!,
    (
	btree_get(T, N, Q-Var)
    ->
	handle_variable(Q, Var, [N|U], X)
    ;
	true
    ).
sem_to_term(quant(Q,V,X0), X, Pol, NPol, U0, T0, T) :-
	!,
	V = '$VAR'(N),
    (
	universal_quantifier(Q, Pol)
    ->
	btree_put(T0, N, u-_, T1),
	U = [N|U0]
    ;
	btree_put(T0, N, e-_, T1),
	U = U0
    ),
	sem_to_term(X0, X, Pol, NPol, U, T1, T).
sem_to_term(appl(appl(appl(X0,Y0),Z0),V0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T1),
	sem_to_term(Z0, Z, Pol, NPol, U, T1, T2),
	sem_to_term(V0, V, Pol, NPol, U, T2, T),
	Term =.. [X, Y, Z, V].
sem_to_term(appl(appl(X0,Y0),Z0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T1),
	sem_to_term(Z0, Z, Pol, NPol, U, T1, T),
	Term =.. [X, Y, Z].
sem_to_term(appl(X0,Y0), Term, Pol, NPol, U, T0, T) :-
	!,
	sem_to_atom(X0, X),
	sem_to_term(Y0, Y, Pol, NPol, U, T0, T),
	Term =.. [X, Y].
sem_to_term(A, A, _, _, _, T, T) :-
	atomic(A).


handle_boolean(&, X0, Y0, and(X,Y), Pol, NPol, U, T0, T) :-
	sem_to_fol(X0, X, Pol, NPol, U, T0, T1),
	sem_to_fol(Y0, Y, Pol, NPol, U, T1, T).
handle_boolean(\/, X0, Y0, or(X,Y), Pol, NPol, U, T0, T) :-
	sem_to_fol(X0, X, Pol, NPol, U, T0, T1),
	sem_to_fol(Y0, Y, Pol, NPol, U, T1, T).
handle_boolean(->, X0, Y0, impl(X,Y), Pol, NPol, U, T0, T) :-
	sem_to_fol(X0, X, NPol, Pol, U, T0, T1),
	sem_to_fol(Y0, Y, Pol, NPol, U, T1, T).

handle_variable(e, _, N, T) :-
	T =.. [skolem|N].
handle_variable(u, V, _, V).

universal_quantifier(exists, neg).
universal_quantifier(iota, neg).
universal_quantifier((?), neg).
universal_quantifier(forall, pos).

fol_to_prolog_fact(query_db(A,B), query_db(A,B)).
fol_to_prolog_fact(query_db(A,B,C), query_db(A,B,C)).
fol_to_prolog_fact(query_db(A,B,C,D), query_db(A,B,C,D)).

fol_to_prolog_clause_list(X, L0, L) :-
	fol_to_prolog_fact(X, F),
	!,
	L0 = [F|L].
fol_to_prolog_clause_list(and(X,Y), L0, L) :-
	!,
	fol_to_prolog_clause_list(X, L0, L1),
	fol_to_prolog_clause_list(Y, L1, L).
fol_to_prolog_clause_list(impl(X,Y), [(Goal :- Body)|L], L) :-
	!,
	fol_to_prolog_body(X, Body),
	fol_to_prolog_fact(Y, Goal).
fol_to_prolog_clause_list(Term, L, L) :-
	format('{Warning: term of the form ~w ignored}~n', [Term]),
	format(log, '{Warning: term of the form ~w ignored}~n', [Term]).

fol_to_prolog_body(X0, X) :-
	fol_to_prolog_fact(X0, F),
	!,
	X = F.
fol_to_prolog_body(and(X0,Y0), (X,Y)) :-
	fol_to_prolog_body(X0, X),
	fol_to_prolog_body(Y0, Y).
fol_to_prolog_body(or(X0,Y0), (X;Y)) :-
	fol_to_prolog_body(X0, X),
	fol_to_prolog_body(Y0, Y).
fol_to_prolog_body(impl(X0,Y0), (X*->Y)) :-
	fol_to_prolog_body(X0, X),
	fol_to_prolog_body(Y0, Y).

% sem_to_prolog_query(+LambdaTerm, -PrologGoal, -Variables)
%
% true if PrologGoal is the Prolog goal (or Prolog query) with Variables
% corresponding to LambdaTerm.

sem_to_prolog_query(Term, Goal) :-
	sem_to_prolog_query(Term, Goal, _).

sem_to_prolog_query(Term, Goal, QVs) :-
	sem_to_prolog_query(Term, Goal0, empty, _, EVs, [], QVs, []),
	bind_variables(EVs, call(Goal0), Goal),
	format(sem, '\\begin{verbatimtab}~n', []),
	Q =.. [query|QVs],
	portray_clause(sem, (Q :- Goal0)),
	format(sem, '\\end{verbatimtab}~n', []).

sem_to_prolog_query('$VAR'(N), X, T0, T, Es, Es, Vs0, Vs) :-
    (
	btree_get(T0, N, X)
    ->
	T = T0,
	Vs0 = Vs
    ;
	btree_insert(T0, N, X, T),
	Vs0 = Vs
    ).
sem_to_prolog_query(A, A, T, T, Es, Es, Vs, Vs) :-
	atomic(A),
	!.
sem_to_prolog_query(appl(appl(appl(F0,Z0),Y0),X0), query_db(F,X,Y,Z), T0, T, Es0, Es, Vs0, Vs) :-
	!,
	sem_to_prolog_query(F0, F, T0, T1, Es0, Es1, Vs0, Vs1),
	sem_to_prolog_query(X0, X, T1, T2, Es1, Es2, Vs1, Vs2),
	sem_to_prolog_query(Y0, Y, T2, T3, Es2, Es3, Vs2, Vs3),
	sem_to_prolog_query(Z0, Z, T3, T, Es3, Es, Vs3, Vs).
sem_to_prolog_query(appl(appl(F0,Y0),X0), query_db(F,X,Y), T0, T, Es0, Es, Vs0, Vs) :-
	!,
	sem_to_prolog_query(F0, F, T0, T1, Es0, Es1, Vs0, Vs1),
	sem_to_prolog_query(X0, X, T1, T2, Es1, Es2, Vs1, Vs2),
	sem_to_prolog_query(Y0, Y, T2, T, Es2, Es, Vs2, Vs).
sem_to_prolog_query(appl(F0,X0), query_db(F,X), T0, T, Es0, Es, Vs0, Vs) :-
	!,
	sem_to_prolog_query(F0, F, T0, T1, Es0, Es1, Vs0, Vs1),
	sem_to_prolog_query(X0, X, T1, T, Es1, Es, Vs1, Vs).
sem_to_prolog_query(not(X0), (\+ X), T0, T, Es0, Es, Vs0, Vs) :-
	!,
	sem_to_prolog_query(X0, X, T0, T, Es0, Es, Vs0, Vs).
sem_to_prolog_query(bool(X0,C,Y0), Term, T0, T, Es0, Es, Vs0, Vs) :-
	sem_to_prolog_query(X0, X, T0, T1, Es0, Es1, Vs0, Vs1),
	sem_to_prolog_query(Y0, Y, T1, T, Es1, Es, Vs1, Vs),
	combine(C, X, Y, Term).
sem_to_prolog_query(quant(Q,V0,X0), Term, T0, T, Es0, Es, Vs0, Vs) :-
	sem_to_prolog_query(V0, V, T0, T1, Es0, Es1, Vs0, Vs1),
	sem_to_prolog_query(X0, X, T1, T, Es1, Es2, Vs1, Vs2),
	combine_quantifier(Q, V, X, Term, Es2, Es,Vs2, Vs).

combine_quantifier(forall, _Var, Term, Term, Es, Es, Vs, Vs).
combine_quantifier(exists, V, Term, Term, [V|Es], Es, Vs, Vs).
combine_quantifier(iota, V, Term, Term, [V|Es], Es, Vs, Vs).
combine_quantifier(\#, V, Term, Term, [V|Es], Es, Vs, Vs).
combine_quantifier('?', V, Term, Term, Es, Es, [V|Vs], Vs).

combine(&, X, Y, ','(X,Y)).
combine(\/, X, Y, ';'(X,Y)).
combine(->, X, Y, ';'('*->'(X,Y),true)).	

% assert_list(+ListOfFacts)
%
% assert all members of ListOfFacts.

assert_list([]).
assert_list([F|Fs]) :-
	assert_if_new(F),
	assert(Fs).

% TODO needs to be replaced with something more subtle for clauses
% which already follow from the database.
% right now, only facts are asserted if they don't already follow
% from the database (through call). 

assert_if_new(C) :-
    (
	is_clause(C)
    ->
	assert(C)
    ;
	call(C)
    ->
	true
    ;
	assert(C)
    ).

is_clause((_ :- _)).

% bind_variables(+ListOfVars, +Goal0, -Goal)
%
% bind each of the variables in ListOfVars using the X^T binding
% construction for use with setof.

bind_variables([], G, G).
bind_variables([V|Vs], G0, G) :-
	bind_variables(Vs, V^G0, G).

query_results_yn(Goal) :-
	setof(., Goal, List),
    (
	List = []
    ->
	format('Answer: No!', []),
	format(sem, 'Answer: \\textit{No!}~n', [])
    ;
	format('Answer: Yes', []),	
	format(sem, 'Answer: \\textit{Yes}~n', [])
    ).

query_results_wh(Goal, Vars) :-
	vars_to_term(Vars, Term),
	setof(Term, Goal, List),
    (
	List = []
    ->
	format('Query has no solutions!~n', []),
	format(sem, 'Query has no solutions!~n', [])
    ;
	format('Solutions:~n', []),
	portray_list(List),
	format(sem, '\\textit{Solutions:}~n', []),
	latex_list(List, sem)
    ).

vars_to_term([], '---').
vars_to_term([V|Vs], Term) :-
	vars_to_term(Vs, V, Term).

vars_to_term([], V, V).
vars_to_term([V|Vs], V0, T) :-
	vars_to_term(Vs, V0-V, T).

smash_drs([], List, Term) :-
	smash_drs1(List,Term).
smash_drs([X|Xs], List, lambda(X, Term)) :-
	smash_drs(Xs, List, Term).

smash_drs1([], true).
smash_drs1([C|Cs], Term) :-
	smash_drs2(Cs, C, Term).

smash_drs2([], Term, Term).
smash_drs2([C|Cs], C0, bool(C0,&,Term)) :-
	smash_drs2(Cs, C, Term).


% = freeze(+Term, -FrozenTerm)

freeze(Term0, Term) :-
	copy_term(Term0, Term),
	numbervars(Term, 1, _).

% = melt(+Frozen, -Term)

melt(Term, Molten) :-
	melt(Term, Molten, empty, _).

melt('$VAR'(N), Var, T0, T) :-
	integer(N),
	!,
     (
         btree_get(T0, N, Var)
     ->
         T = T0
     ;
         btree_insert(T0, N, Var, T)
     ).
melt(A0, A, T0, T) :-
	atomic(A0),
	!,
	A = A0,
	T = T0.
melt(Term0, Term, T0, T) :-
	Term0 =.. [F|List0],
	melt_list(List0, List, T0, T),	
	Term =.. [F|List].

melt_list([], [], T, T).
melt_list([A|As], [B|Bs], T0, T) :-
	melt(A, B, T0, T1),
	melt(As, Bs, T1, T).

% test_translate :-
% %	translate_dynamics(lambda(L, lambda(S, appl(L,lambda(Y,appl(appl(dans,Y),S))))), ((e->t)->t)->(t->t), OutType, Translation0, List),
% 	Paris = lambda(PsiP,lambda(EP,lambda(PhiP,appl(appl(appl(PsiP,paris),EP),lambda(EP1,appl(PhiP,update(paris,EP1))))))),
% 	Jean = lambda(PsiJ,lambda(EJ,lambda(PhiJ,appl(appl(appl(PsiJ,jean),EJ),lambda(EJ1,appl(PhiJ,update(jean,EJ1))))))),
% 	translate_dynamics(lambda(Suj,appl(Suj,lambda(XX,appl(dort,XX)))), ((e->t)->t)->t, _, Dort, _),
% 	translate_dynamics(lambda(SujA,lambda(Obj,appl(SujA,lambda(XX,appl(Obj,lambda(YY,appl(appl(aime,YY),XX))))))), ((e->t)->t)->(((e->t)->t)->t), _, Aime, _),
% 	Marie = lambda(PsiM,lambda(EM,lambda(PhiM,appl(appl(appl(PsiM,marie),EM),lambda(EM1,appl(PhiM,update(marie,EM1))))))),
% %	translate_dynamics(dans, e->(t->t), _, Dans1, _),
% 	translate_dynamics(dans, e->((e->t)->(e->t)), _, Dans1, _),
% 	translate_dynamics(dort, e->t, _, Dort1, _),
% 	translate_dynamics(aime, e->(e->t), _, Aime1, _),
% 	Dans = lambda(L,lambda(V,lambda(S,appl(S,lambda(X,appl(V,lambda(P,appl(L,lambda(Y,appl(appl(appl(Dans1,Y),P),X)))))))))),
% %	Dans = lambda(L,lambda(S,appl(L,lambda(X,appl(appl(Dans1,X),S))))),
% 	Dort = lambda(Suj,appl(Suj,lambda(XX,appl(Dort1,XX)))),
% 	Aime = lambda(AO,lambda(AS,appl(AS,lambda(XASi,appl(AO,lambda(YAOi,appl(appl(Aime1,YAOi),XASi))))))),
% %	Dort = lambda(SD,appl(SD,lambda(XD,lambda(ED,lambda(PhiD,bool(appl(sleep,XD),&,appl(PhiD,ED))))))),
% 	numbervars(Marie, 0, X1),
% 	numbervars(Paris, X1, X2),
% 	numbervars(Jean, X2, X3),
% 	numbervars(Dort, X3, X4),
% 	numbervars(Aime, X4, _),
% 	reduce_sem(Aime, AimeR),
% 	reduce_sem(Dans, DansR),
% 	reduce_sem(appl(appl(appl(Dans,Paris),appl(Aime,Marie)),Jean), Translation),
% 	format('~n~w~n~w~n~w~n', [AimeR, DansR, Translation]),
% 	new_output_file('semantics.tex', sem),
% 	latex_header(sem),
% 	write(sem, '$'),
% 	latex:latex_semantics(Translation, 1, empty, sem),
% 	write(sem, '$'),
% 	latex_tail(sem),
% 	close(sem).

translate_dynamics(Term, InType, Translation) :-
	translate_dynamics(Term, InType, _OutType, Translation, _List).

translate_dynamics(Term, InType, OutType, Translation, List) :-
	translate_type(InType, OutType),
	translate_d(Term, InType, Translation, List, []),
	numbervars(Translation, 0, _).

translate_type(e, e).
translate_type(t, s->(s->t)->t).
translate_type((A0->B0), (A->B)) :-
	translate_type(A0, A),
	translate_type(B0, B).

translate_d(Term, Type, Translation, L0, L) :-
	get_arguments_result(Type, Result, Arguments),
	translate_subtypes_d(Result, Arguments, Term, Translation, L0, L).

translate_subtypes_d(e, Arguments, Term, Translation, L0, L) :-
	translate_subtypes_d_e(Arguments, Term, Translation, L0, L).
translate_subtypes_d(t, Arguments, Term, Translation, L0, L) :-
	translate_subtypes_d_t(Arguments, Term, Translation, _, _, L0, L).

translate_subtypes_d_e([], Term, Term, L, L).
translate_subtypes_d_e([T|Ts], Term0, lambda(X0,Term), [X0-TT|L0], L) :-
	translate_type(T, TT),
	translate_r(X0, T, X, nil, lambda(_, true), L0, L1),
	translate_subtypes_d_e(Ts, appl(Term0,X), Term, L1, L).

translate_subtypes_d_t([], Term, lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))), E, Phi, [E-s,Phi-(s->t)|L], L).
translate_subtypes_d_t([T|Ts], Term0, lambda(X0,Term), E, Phi, [X0-TT|L0], L) :-
	translate_type(T, TT),
	translate_r(X0, T, X, E, Phi, L0, L1),
	translate_subtypes_d_t(Ts, appl(Term0,X), Term, E, Phi, L1, L).

translate_r(Term, Type, Translation, E, Phi, L0, L) :-
	get_arguments_result(Type, Result, Arguments),
	translate_subtypes_r(Result, Arguments, Term, Translation, E, Phi, L0, L).

translate_subtypes_r(e, Arguments, Term, Translation, _, _, L0, L) :-
	translate_subtypes_r_e(Arguments, Term, Translation, L0, L).
translate_subtypes_r(t, Arguments, Term, Translation, E, Phi, L0, L) :-
	translate_subtypes_r_t(Arguments, Term, Translation, E, Phi, L0, L).


translate_subtypes_r_e([], Term, Term, L, L).
translate_subtypes_r_e([T|Ts], Term0, lambda(X0,Term), [X0-TT|L0], L) :-
	translate_type(T, TT),
	translate_d(X0, T, X, L0, L1),
	translate_subtypes_r_e(Ts, appl(Term0,X), Term, L1, L).

translate_subtypes_r_t([], Term, appl(appl(Term,E),Phi), E, Phi, L, L).
%translate_subtypes_r_t([], Term, appl(appl(Term,E),lambda(_,true)), E, _Phi, L, L).
translate_subtypes_r_t([T|Ts], Term0, lambda(X0,Term), E, Phi, [X0-TT|L0], L) :-
	translate_type(T, TT),
	translate_d(X0, T, X, L0, L1),
	translate_subtypes_r_t(Ts, appl(Term0,X), Term, E, Phi, L1, L).


get_arguments_result(e, e, []).
get_arguments_result(t, t, []).
get_arguments_result(A->B, R, [A|As]) :-
	get_arguments_result(B, R, As).

% = drs_to_fol(+DRS, -Formula)
%
% converts a Discourse Representation Structure DRS into an equivalent
% first-order logic formula Formula.

drs_to_fol(drs(Vars,Conds), Fol) :-
	add_quantifiers(Vars, exists, Fol0, Fol),
	drs_conditions_to_fol(Conds, Fol0).

add_quantifiers([], _, F, F).
add_quantifiers([X|Xs], Q, F0, F) :-
	add_quantifier(X, Q, F0, F1),
	add_quantifiers(Xs, Q, F1, F).

add_quantifier('$VAR'(X), Q, F, quant(Q,'$VAR'(X),F)).
add_quantifier(event('$VAR'(X)), Q, F, quant(Q,'$VAR'(X),F)).
add_quantifier(variable('$VAR'(X)), Q, F, quant(Q,'$VAR'(X),F)).

drs_conditions_to_fol([], true).
drs_conditions_to_fol([C|Cs], F) :-
	drs_conditions_to_fol(Cs, C, F).

drs_conditions_to_fol([], C, F) :-
	drs_condition_to_fol(C, F).
drs_conditions_to_fol([C|Cs], C0, bool(F0,&,F)) :-
	drs_condition_to_fol(C0, F0),
	drs_conditions_to_fol(Cs, C, F).

drs_condition_to_fol(bool(drs(Vars1,Conds1),->,drs(Vars2,Conds2)), Form) :-
	!,
	add_quantifiers(Vars1, forall, bool(Form1,->,Form3), Form),
	drs_conditions_to_fol(Conds1, Form1),
	add_quantifiers(Vars2, exists, Form2, Form3),
	drs_conditions_to_fol(Conds2, Form2).
drs_condition_to_fol(bool(drs(Vars1,Conds1),\/,drs(Vars2,Conds2)), bool(Form1,\/,Form3)) :-
	!,
	add_quantifiers(Vars1, exists, Form0, Form1),
	drs_conditions_to_fol(Conds1, Form0),
	add_quantifiers(Vars2, exists, Form2, Form3),
	drs_conditions_to_fol(Conds2, Form2).
drs_condition_to_fol(not(drs(Vars,Conds)), not(Form)) :-
	!,
	drs_conditions_to_fol(Conds, Form0),
	add_quantifiers(Vars, exists, Form0, Form).
drs_condition_to_fol(Form, Form).

test_conversion(F) :-
	DRS = drs([X],[appl(orange,X),bool(drs([Y],[appl(farmer,Y)]),->,drs([Z],[appl(donkey,Z),appl(appl(beat,Z),Y)]))]),
	numbervars(DRS, 1, _),
	drs_to_fol(DRS, F0),
	reduce_sem(F0, F).
