
proof_file('../TLGbank/chart_proofs/aa1_proofs.pl', 'aa1_parse.txt').
proof_file('../TLGbank/chart_proofs/ap1_proofs.pl', 'ap1_parse.txt').
proof_file('../TLGbank/chart_proofs/aq2_proofs.pl', 'aq2_parse.txt').
proof_file('../TLGbank/chart_proofs/as2_proofs.pl', 'as2_parse.txt').
proof_file('../TLGbank/chart_proofs/at_proofs.pl', 'at_parse.txt').
proof_file('../TLGbank/chart_proofs/300_proofs.pl', '300_parse.txt').
proof_file('../TLGbank/chart_proofs/1000_proofs.pl', '1000_parse.txt').
proof_file('../TLGbank/chart_proofs/8000_proofs.pl', '8000_parse.txt').
proof_file('../TLGbank/chart_proofs/annodis_proofs.pl', 'annodis_parse.txt').
proof_file('../TLGbank/chart_proofs/frwiki1_proofs.pl', 'frwiki1_parse.txt').
proof_file('../TLGbank/chart_proofs/frwiki2_proofs.pl', 'frwiki2_parse.txt').
proof_file('../TLGbank/chart_proofs/emea_d_proofs.pl', 'emea_d_parse.txt').
proof_file('../TLGbank/chart_proofs/europar_proofs.pl', 'europar_parse.txt').


silver_proof_file('../TLGbank/chart_proofs/aa2_proofs.pl', 'aa2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ab2_proofs.pl', 'ab2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ae1_proofs.pl', 'ae1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/af2_proofs.pl', 'af2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ag1_proofs.pl', 'ag1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ag2_proofs.pl', 'ag2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ah1_proofs.pl', 'ah1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ah2_proofs.pl', 'ah2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ai1_proofs.pl', 'ai1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ai2_proofs.pl', 'ai2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/aj1_proofs.pl', 'aj1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ak1_proofs.pl', 'ak1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ak2_proofs.pl', 'ak2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/al1_proofs.pl', 'al1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/am1_proofs.pl', 'am1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/am2_proofs.pl', 'am2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/an1_proofs.pl', 'an1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/an2_proofs.pl', 'an2_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ao1_proofs.pl', 'ao1_silver.txt').
silver_proof_file('../TLGbank/chart_proofs/ao2_proofs.pl', 'ao2_silver.txt').


portray(dr(I,A,B)) :-
	format('dr(~p,~p,~p)', [I,A,B]).
portray(dl(I,A,B)) :-
	format('dl(~p,~p,~p)', [I,A,B]).
portray(p(I,A,B)) :-
	format('p(~p,~p,~p)', [I,A,B]).
portray(dia(I,A)) :-
	format('dia(~p,~p)', [I,A]).
portray(box(I,A)) :-
	format('box(~p,~p)', [I,A]).

portray(lit(np(_,_,_))) :-
	!,
	print(np).
portray(lit(s(_))) :-
	!,
	print(s).
portray(lit(pp(_))) :-
	!,
	print(pp).
portray(lit(A)) :-
	!,
	print(A).


all_chart_proofs :-
        proof_file(InFile, OutFile),
        format('~NCreating ~w...', [OutFile]),
	flush_output,
        chart_proofs_to_parser_actions(InFile, OutFile),
        format('done', []),
	flush_output,
        fail.
all_chart_proofs.

all_silver_proofs :-
        silver_proof_file(InFile, OutFile),
        format('~NCreating ~w...', [OutFile]),
	flush_output,
        chart_proofs_to_parser_actions(InFile, OutFile),
        format('done', []),
	flush_output,
        fail.
all_silver_proofs.


chart_proofs_to_parser_actions(InFile, OutFile) :-
	abolish(proof/2),
	compile(InFile),
	tell(OutFile),
	chart_proofs_to_parser_actions1,
	told.

chart_proofs_to_parser_actions1 :-
	proof(N,_),
	chart_proof_to_parser_actions(N),
	fail.
chart_proofs_to_parser_actions1.
	
chart_proof_to_parser_actions(SentNum) :-
	chart_proof_to_parser_actions(SentNum, ParserActions),
	print_list(ParserActions).

chart_proof_to_parser_actions(SentNum, ParserActions) :-
	proof(SentNum, Proof0),
	chart_proof_to_parser_actions(Proof0, _Proof, ParserActions, []).

chart_proof_to_parser_actions(Proof0, Proof, List0, List) :-
	chart_proof_to_parser_actions1(Proof0, Proof1, ParserActions, []),
   (			      
	Proof0 \= Proof1
   ->
        List0 = [ParserActions|List1],
	chart_proof_to_parser_actions(Proof1, Proof, List1, List)
   ;
        Proof = Proof0,
        List = List0
   ).

chart_proof_to_parser_actions1(rule(axiom,Pros,Form-Sem,Premisses), rule(axiom,Pros,Form-Sem,Premisses)) -->
	!,
	[Pros-Form-'O'].
chart_proof_to_parser_actions1(rule(Name,Pros,Form,Premisses0), NewProof, ParserActions0, ParserActions) :-
	rule_premisses(Name, Premisses0, _AuxPremisses, TruePremisses),
   (	
	all_axiom_premisses(TruePremisses, Name, ParserActions0, ParserActions)
   ->
        NewProof = rule(axiom,Pros,Form,[])
   ; 
        NewProof = rule(Name,Pros,Form,Premisses),
        chart_proof_to_parser_actions_list(TruePremisses, Premisses, ParserActions0, ParserActions)
   ).


chart_proof_to_parser_actions_list([], []) -->
	[].
chart_proof_to_parser_actions_list([R0|Rs0], [R|Rs]) -->
	chart_proof_to_parser_actions1(R0, R),
	chart_proof_to_parser_actions_list(Rs0, Rs).

all_axiom_premisses([rule(axiom,Pros,Form-_,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName)},
	[Pros-Form-ActionName].
all_axiom_premisses([rule(axiom,Pros1,Form1-_,_),rule(axiom,Pros2,Form2-_,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName1)},
	{atom_concat(Name, '2', ActionName2)},
	[Pros1-Form1-ActionName1],
	[Pros2-Form2-ActionName2].
all_axiom_premisses([rule(axiom,Pros1,Form1-_,_),rule(axiom,Pros2,Form2-_,_),rule(axiom,Pros3,Form3-_,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName1)},
	{atom_concat(Name, '2', ActionName2)},
	{atom_concat(Name, '3', ActionName3)},
	[Pros1-Form1-ActionName1],
	[Pros2-Form2-ActionName2],
	[Pros3-Form3-ActionName3].


% rule_premisses(RuleName, AllPremisses, AuxPremisses, TruePremisses)
rule_premisses(e_start, [A,B], [A], [B]) :-
	!.
rule_premisses(e_start_l, [A,B], [B], [A]) :-
	!.
rule_premisses(ef_start, List0, [A], Es) :-
	A = rule(_,_,dr(0,_,dr(0,_,dia(Ind,box(Ind,dr(0,_,_)))))-_,_),
	select(A, List0, Es),
	!.
rule_premisses(ef_start_iv, List0, [A], Es) :-
	A = rule(_,_,dr(0,_,dr(0,_,dia(Ind,box(Ind,dl(0,lit(np(_,_,_)),lit(s(_)))))))-_,_),
	select(A, List0, Es),
	!.
rule_premisses(gap_i, List0, [], List) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_,_),
	B = rule(_,_,X-_,_),
	member(A, List0),
%	select(B, List0, Es),
%	!.
rule_premisses(gap_c, List0, [A], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_,_),
	select(A, List0, Es),
	!.
rule_premisses(gap_e, List0, [A], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_,_),
	select(A, List0, Es),
	!.
rule_premisses(c_l_lnr, List0, [A], Es) :-
    A = dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n))),
    select(A, List0, Es), 
    !.
rule_premisses(c_r_lnr, List0, [A], Es) :-
    A = dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n))),
    select(A, List0, Es),
    !.
rule_premisses(prod_i, [A,B,C], Ds, Es) :-
    (
	A = rule(_,_,dr(0,_,p(0,_,_))-_,_)
    ->
        Ds = [A],
        Es = [B,C]
    ;
        C = rule(_,_,dl(0,p(0,_,_),_)-_,_)
    ->
	Ds = [C],
	Es = [A,B]
    ),
    !.
% default, 
rule_premisses(_, List, [], List).

			
pros_head(dr(_,A,A), A, _, Pros, Pros) :-
    !.
pros_head(A, dl(_,A,A), Pros, _, Pros) :-
    !.
pros_head(dr(0,np,n), n, _P, Pros, Pros) :-
    !.
pros_head(dr(_,dl(_,n,n),n), n, _, Pros, Pros) :-
    !.
pros_head(dr(0,pp,n), n, _, Pros, Pros) :-
    !.
pros_head(dr(0,pp,np), np, _, Pros, Pros) :-
    !.
pros_head(dr(_,_,B), B, Pros, _, Pros) :-
    !.
pros_head(B,dl(_,B,_), _, Pros, Pros) :-
    !.


print_list([]).
print_list([X|Xs]) :-
	print_action_list(X),
	print_list(Xs).
print_action_list([]) :-
	format('~n', []).
print_action_list([X-Y-Z|Zs]) :-
	format('~w|~p|~w ', [X,Y,Z]),
	print_action_list(Zs).
