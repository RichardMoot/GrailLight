:- use_module(sem_utils,   [replace_sem/4,get_max_variable_number/2,equivalent_semantics/2,unify_semantics/2,renumbervars/1]).
:- use_module(ordset,      [ord_dup_union/3,ord_dup_insert/3,ord_subtract/3,ord_select/3,ord_subset/2]).

quote_mode(1, 1).

% ==============================================
% =           proof transformations            =
% ==============================================

infile(aa1).
infile(aa2).
infile(ab2).
infile(ae1).
infile(af2).
infile(ag1).
infile(ag2).
infile(ah1).
infile(ah2).
infile(ai1).
infile(ai2).
infile(aj1).
infile(ak1).
infile(ak2).
infile(al1).
infile(am1).
infile(am2).
infile(an1).
infile(an2).
infile(ao1).
infile(ao2).
infile(ap1).
infile(aq2).
infile(as2).
infile(at).
infile(300).
infile(8000).
infile(annodis).
infile(monde_a).
infile(monde_b).

chart_dir('chart_proofs/').
nd_dir('nd_proofs/').


verify_all_proofs :-
	chart_dir(ChDir),
	infile(Root),
	format(user_error, '~NStarting ~w~n', [Root]),
	atom_concat(Root, '_proofs.pl', InFile0),
	atom_concat(ChDir, InFile0, InFile),
	abolish(proof/2),
	compile(InFile),
	verify_all_proofs1,
	format(user_error, '~NDone ~w~n', [Root]),
	fail.
verify_all_proofs.

verify_all_proofs1 :-
	proof(N, rule(_,_,_-Sem,_)),
	verify_semantics(N, Sem),
	fail.
verify_all_proofs1.

verify_proof(N) :-
	proof(N, rule(_,_,_-Sem,_)),
	verify_semantics(N, Sem).

verify_semantics(N, Sem) :-
	renumbervars(Sem),
	vars_words(Sem, N, FV, _Ws),
	portray_free_vars(FV, N).


vars_words('$VAR'(N), _, [N], []) :-
	!.
vars_words(word(N), _, [], [N]) :-
	!.
vars_words(lambda(V0,Y), Sent, FV, Ws) :-
	check_var(V0, X, Sent),
	!,
	vars_words(Y, Sent, FV0, Ws),
	ord_delete_all(FV0, X, FV),
	length(FV0, L0),
	length(FV, L),
	Diff is L0 - L,
	portray_diff(Diff, X, Sent).
vars_words(appl(A,B), Sent, FV, W) :-
	vars_words(A, Sent, FVA, WA),
	vars_words(B, Sent, FVB, WB),
	ord_dup_union(FVA, FVB, FV),
	ord_dup_union(WA, WB, W),
	portray_duplicates_word(WA, WB, Sent),
	portray_duplicates_var(FVA, FVB, Sent).
vars_words(pair(A,B), Sent, FV, W) :-
	vars_words(A, Sent, FVA, WA),
	vars_words(B, Sent, FVB, WB),
	ord_dup_union(FVA, FVB, FV),
	ord_dup_union(WA, WB, W),
	portray_duplicates_word(WA, WB, Sent),
	portray_duplicates_var(FVA, FVB, Sent).
vars_words(pi2(_), _, [], []).
vars_words(pi1(A), Sent, FV, Ws) :-
	vars_words(A, Sent, FV, Ws).

check_var(Var, Num, _) :-
	Var = '$VAR'(Num),
	integer(Num),
	!.
check_var(Var, Var, Sent) :-
	format('{Warning: (S~D) strange variable "~p" found}~n', [Sent,Var]).
	

portray_duplicates_word(WA0, WB0, Sent) :-
	sort(WA0, WA),
	sort(WB0, WB),
	ord_intersect(WA, WB, WC),
	portray_duplicates_word(WC, Sent).

portray_duplicates_var(WA0, WB0, Sent) :-
	sort(WA0, WA),
	sort(WB0, WB),
	ord_intersect(WA, WB, WC),
	portray_duplicates_var(WC, Sent).

portray_free_vars([], _).
portray_free_var([V|Vs], Sent) :-
	format('{Warning: (S~D) variables ~p occur multiple times}~n', [Sent,[V|Vs]]).


portray_duplicates_word([], _).
portray_duplicates_word([W|Ws], Sent) :-
	format('{Warning: (S~D) words ~p occur multiple times}~n', [Sent, [W|Ws]]).

portray_duplicates_var([], _).
portray_duplicates_var([W|Ws], Sent) :-
	format('{Warning: (S~D) words ~p occur multiple times}~n', [Sent, [W|Ws]]).


portray_diff(1, _, _) :-
	!.
portray_diff(N, X, Sent) :-
	format('{Warning: (S~D) abstraction over ~p (~D) binds ~D occurrences}~n', [Sent, '$VAR'(X), X, N]).
	
	

ord_delete_all([], _, []).
ord_delete_all([Head|Tail], Element, Rest) :-
	compare(Order, Element, Head),
	ord_delete_all(Order, Tail, Element, Head, Rest).

ord_delete_all(<, Tail, _, Head, [Head|Tail]).
ord_delete_all(=, Tail, Element, _, Rest) :-
	ord_delete_all(Tail, Element, Rest).
ord_delete_all(>, Tail, Element, Head, [Head|Rest]) :-
	ord_delete_all(Tail, Element, Rest).
