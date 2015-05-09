
%:- compile(proofs).
:- use_module(ordset, [ord_insert/3,ord_union/3]).

nd_file(aa1_nd).
nd_file(aa2_nd).
nd_file(ab2_nd).
nd_file(ae1_nd).
nd_file(af2_nd).
nd_file(ag1_nd).
nd_file(ag2_nd).
nd_file(ah1_nd).
nd_file(ah2_nd).
nd_file(ai1_nd).
nd_file(ai2_nd).
nd_file(aj1_nd).
nd_file(ak1_nd).
nd_file(ak2_nd).
nd_file(al1_nd).
nd_file(am1_nd).
nd_file(am2_nd).
nd_file(an1_nd).
nd_file(an2_nd).
nd_file(ao1_nd).
nd_file(ao2_nd).
nd_file(ap1_nd).
nd_file(aq2_nd).
nd_file(as2_nd).
nd_file(at_nd).
nd_file('300_nd').
nd_file('8000_nd').
nd_file(annodis_nd).


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


all_strange_proofs :-
	findall(F, nd_file(F), FileList),
	all_strange_proofs(FileList, 0, 0).

all_strange_proofs([], Strange, Total) :-
	Normal is Total - Strange,
	NPct is Normal/Total,
	SPct is Strange/Total,
	format('~N**TOTAL**~nNormal : ~D (~w%)~nStrange: ~D (~w%)~nTotal  : ~D~2n', [Normal,NPct,Strange,SPct,Total]).
	
all_strange_proofs([F|Fs], Strange0, Total0) :-
	abolish(proof/2),
	compile(F),
	findall(N, proof(N, _), ProofList),
	strange_proofs(ProofList, 0, ST, 0, TT, Ss, []),
	Normal is TT - ST,
	NPct is Normal/TT,
	SPct is ST/TT,
	Total is Total0 + TT,
	Strange is Strange0 + ST,
	format('~N**~w**~nNormal : ~D (~w%)~nStrange: ~D (~w%)~nTotal  : ~D~2n', [F,Normal,NPct,ST,SPct,TT]),
	print_list(Ss),
	all_strange_proofs(Fs, Strange, Total).

strange_proofs :-
	findall(N, proof(N, _), ProofList),
	strange_proofs(ProofList, 0, ST, 0, TT, Ss, []),
	Normal is TT - ST,
	NPct is Normal/TT,
	SPct is ST/TT,
	format('~NNormal : ~D (~w%)~nStrange: ~D (~w%)~nTotal  : ~D~2n', [Normal,NPct,ST,SPct,TT]),
	print_list(Ss).

strange_proofs([], ST, ST, T, T) -->
	[].
strange_proofs([P|Ps], ST0, ST, T0, T) -->
	is_strange(P, ST0, ST1),
	{T1 is T0 + 1},
	strange_proofs(Ps, ST1, ST, T1, T).


is_strange(N, ST0, ST, L0, L) :-
	proof(N, P),
	check_proof(P, [], Ss0),
	sort(Ss0, Ss),
     (	
	Ss = []
     ->
	L = L0,
        ST = ST0
     ;
	L0 = [N-Ss|L],
	ST is ST0 + 1
     ).

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
keep_strange(dli1, Rs, Rs) :-
	!.
keep_strange(dri, Rs, Rs) :-
	!.
keep_strange(drdiaboxi, Rs, Rs) :-
	!.
keep_strange(dldiaboxi, Rs, Rs) :-
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
