
%:- compile(proofs).
:- use_module(ordset, [ord_insert/3,ord_union/3]).

check_proofs :-
	findall(N, proof(N, _), ProofList),
	check_proofs(ProofList, []).
check_proofs([], RNs0) :-
	msort(RNs0, RNs1),
	collapse_duplicates(RNs1, RNs),
	print_list(RNs).
check_proofs([N|Ns], RNs0) :-
	proof(N, Proof),
	check_proof(Proof, RNs0, RNs1),
	check_proofs(Ns, RNs1).


check_proof(rule(Nm, _, _, Rs), RNs0, RNs) :-
%	ord_insert(RNs0, Nm, RNs1),
	functor(Nm, F, _),
	keep_strange(F, RNs0, RNs1),
%        RNs1 = [F|RNs0],	
	check_proof_list(Rs, RNs1, RNs).


keep_strange(axiom, Rs, Rs) :-
	!.
keep_strange(let, Rs, Rs) :-
	!.
keep_strange(hyp, Rs, Rs) :-
	!.
keep_strange(dr, Rs, Rs) :-
	!.
keep_strange(dl, Rs, Rs) :-
	!.
keep_strange(dl1, Rs, Rs) :-
	!.
keep_strange(dli, Rs, Rs) :-
	!.
keep_strange(dri, Rs, Rs) :-
	!.
keep_strange(drdiaboxi, Rs, Rs) :-
	!.
keep_strange(F, Rs, [F|Rs]).

collapse_duplicates([], []).
collapse_duplicates([A|As], Bs) :-
	collapse_duplicates(As, A, 1, Bs).

collapse_duplicates([], B, N, [B-N]).
collapse_duplicates([A|As], B, N0, Cs0) :-
   (
        A = B	 
   ->
        N is N0 + 1,
        collapse_duplicates(As, B, N, Cs0)
   ;
        Cs0 = [B-N0|Cs],
        collapse_duplicates(As, A, 1, Cs)
   ).
check_proof_list([], RNs, RNs).
check_proof_list([R|Rs], RNs0, RNs) :-
	check_proof(R, RNs0, RNs1),
	check_proof_list(Rs, RNs1, RNs).

print_list([]).
print_list([A|As]) :-
	format('~p~n', [A]),
	print_list(As).
