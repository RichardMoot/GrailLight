
:- ['../TLGbank/chart_proofs/8000_proofs.pl'].

chart_to_const :-
	findall(I-Pros, proof(I,rule(_,Pros,_F,_Prems)), ProsList),
	chart_to_const(ProsList, LLs, LRs),
	export(LLs, '8000_left.txt'),
	export(LRs, '8000_right.txt').

chart_pros_to_const(I) :-
	proof(I, rule(_, Pros, _, _)),
	pros_to_const_l(Pros, -1, _, LL, []),
	pros_to_const_r(Pros, -1, _, RLR, []),
	reverse(RLR, LR),
	print(LL),
	nl,
	print(LR),
	compute_penalties(LL, LR, 0).

compute_penalties([], [], _).
compute_penalties([_], [_], _).
compute_penalties([W1-L1,W2-L2|Ls],[_-R1,_-R2|Rs], Pos0) :-
	X is L1 - R1, % close bracket without open bracket
	(X < 0 -> PenL is abs(X) ; PenL = 0),
	Y is L2 - R2, % open bracket without close bracket
	(Y > 0 -> PenR = Y ; PenR = 0),
	Pos1 is Pos0 + 1,
	Pos2 is Pos1 + 1,
	Pen is PenL + PenR,
	format('~w ~w [~w-~w]:(~w+~w)=~w ', [W1,W2,Pos0,Pos2,PenL,PenR,Pen]),
	compute_penalties([W2-L2|Ls], [_-R2|Rs], Pos1).

chart_to_const([], [], []).
chart_to_const([_-Pros|Rest], [LL|LLs], [LR|LRs]) :-
	pros_to_const_l(Pros, -1, _, LL, []),
	pros_to_const_r(Pros, -1, _, RLR, []),
	reverse(RLR, LR),
	chart_to_const(Rest, LLs, LRs).


pros_to_const_l(p(_,A,B), L0, L) -->
	!,
	{L1 is L0 + 1},
	pros_to_const_l(A, L1, L2),
	pros_to_const_l(B, L2, L).
pros_to_const_l(W, L, 0) -->
	{C is max(L,0)},
	[W-C].

pros_to_const_r(p(_,A,B), R0, R) -->
	!,
	{R1 is R0 + 1},
	pros_to_const_r(B, R1, R2),
	pros_to_const_r(A, R2, R).
pros_to_const_r(W, R, 0) -->
	{C is max(R,0)},
	[W-C].


export(List, File) :-
	open(File, write, Stream),
	export_array(List, Stream),
	close(Stream).

export_array([], _Stream).
export_array([L|Ls], Stream) :-
	export_list(L, Stream),
	export_array(Ls, Stream).

export_list([], _Stream).
export_list([_-I|Rest], Stream) :-
	export_list(Rest, I, Stream).

export_list([], I, Stream) :-
	print(Stream, I),
	nl(Stream).
export_list([_-J|Rest], I, Stream) :-
	format(Stream, '~p ', [I]),
	export_list(Rest, J, Stream).
	
