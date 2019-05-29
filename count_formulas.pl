:- compile(tlg_formulas).
:- use_module(ordset, [ord_subtract/3, ord_member/2, ord_insert/3, ord_subset/2, ord_key_insert/4, ord_key_insert_unify/4, ord_select/3, ord_delete/3]).


formulas(Formulas) :-
	setof(F, N^formula(F,N), Formulas).

atoms(dr(A,B), SetAB) :-
	!,
	atoms(A, SetA),
	atoms(B, SetB),
	ord_union(SetA, SetB, SetAB).
atoms(dl(A,B), SetAB) :-
	!,
	atoms(A, SetA),
	atoms(B, SetB),
	ord_union(SetA, SetB, SetAB).
atoms(p(A,B), SetAB) :-
	!,
	atoms(A, SetA),
	atoms(B, SetB),
	ord_union(SetA, SetB, SetAB).
atoms(dia(A), SetA) :-
	!,
	atoms(A, SetA).
atoms(box(A), SetA) :-
	!,
	atoms(A, SetA).
atoms(A, [A]).

atomic_formulas(Formulas, Atoms) :-
	atomic_formulas(Formulas, [], Atoms).

atomic_formulas([], Atoms, Atoms).
atomic_formulas([F|Fs], Atoms0, Atoms) :-
	atoms(F, FAtoms),
	ord_union(Atoms0, FAtoms, Atoms1),
	atomic_formulas(Fs, Atoms1, Atoms).

count_formulas :-
	formulas(Formulas),
	atomic_formulas(Formulas, Atoms),
	count_formulas(Formulas, Atoms).

count_formulas([], _).
count_formulas([F|Fs], Atoms) :-
	count_formula(Atoms, F),
	format(' ~p~n', [F]),
	count_formulas(Fs, Atoms).

count_formula([], _) :-
	format('', []).
count_formula([A|As], F) :-
	count_formula1(F, A, Count),
	format('~p', [Count]),
	count_formula(As, F).

count_formula1(Atom0, Atom, Count) :-
	atomic(Atom0),
	atomic(Atom),
	!,
   (
	Atom0 == Atom
   ->
	Count = 1
   ;
	Count = 0
   ).
count_formula1(dr(A,B), Atom, Count) :-
	!,
	count_formula1(A, Atom, CountA),
	count_formula1(B, Atom, CountB),
	Count is CountA - CountB.
count_formula1(dl(B,A), Atom, Count) :-
	!,
	count_formula1(A, Atom, CountA),
	count_formula1(B, Atom, CountB),
	Count is CountA - CountB.
count_formula1(p(A,B), Atom, Count) :-
	!,
	count_formula1(A, Atom, CountA),
	count_formula1(B, Atom, CountB),
	Count is CountA + CountB.
