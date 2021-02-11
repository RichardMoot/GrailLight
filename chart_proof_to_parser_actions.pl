
proof_file('../TLGbank/parse_actions/aa1_parse_actions.pl', 'aa1_parse_actions.txt').
proof_file('../TLGbank/parse_actions/ap1_parse_actions.pl', 'ap1_parse_actions.txt').
proof_file('../TLGbank/parse_actions/aq2_parse_actions.pl', 'aq2_parse_actions.txt').
proof_file('../TLGbank/parse_actions/as2_parse_actions.pl', 'as2_parse_actions.txt').
proof_file('../TLGbank/parse_actions/at_parse_actions.pl', 'at_parse_actions.txt').
proof_file('../TLGbank/parse_actions/300_parse_actions.pl', '300_parse_actions.txt').
proof_file('../TLGbank/parse_actions/1000_parse_actions.pl', '1000_parse_actions.txt').
proof_file('../TLGbank/parse_actions/8000_parse_actions.pl', '8000_parse_actions.txt').
proof_file('../TLGbank/parse_actions/annodis_parse_actions.pl', 'annodis_parse_actions.txt').
proof_file('../TLGbank/parse_actions/frwiki1_parse_actions.pl', 'frwiki1_parse_actions.txt').
proof_file('../TLGbank/parse_actions/frwiki2_parse_actions.pl', 'frwiki2_parse_actions.txt').
proof_file('../TLGbank/parse_actions/emea_d_parse_actions.pl', 'emea_d_parse_actions.txt').
proof_file('../TLGbank/parse_actions/europar_parse_actions.pl', 'europar_parse_actions.txt').


silver_proof_file('../TLGbank/parse_actions/aa2_parse_actions.pl', 'aa2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ab2_parse_actions.pl', 'ab2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ae1_parse_actions.pl', 'ae1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/af2_parse_actions.pl', 'af2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ag1_parse_actions.pl', 'ag1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ag2_parse_actions.pl', 'ag2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ah1_parse_actions.pl', 'ah1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ah2_parse_actions.pl', 'ah2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ai1_parse_actions.pl', 'ai1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ai2_parse_actions.pl', 'ai2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/aj1_parse_actions.pl', 'aj1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ak1_parse_actions.pl', 'ak1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ak2_parse_actions.pl', 'ak2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/al1_parse_actions.pl', 'al1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/am1_parse_actions.pl', 'am1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/am2_parse_actions.pl', 'am2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/an1_parse_actions.pl', 'an1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/an2_parse_actions.pl', 'an2_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ao1_parse_actions.pl', 'ao1_silver.txt').
silver_proof_file('../TLGbank/parse_actions/ao2_parse_actions.pl', 'ao2_silver.txt').


% portray(dr(I,dl(J,lit(s(X)),lit(s(X))),lit(s(X)))) :-
% 	!,
% 	format('dr(~p,dl(~p,~p,~p),~p)', [I,J,lit(s(_)),lit(s(_)),lit(s(_))]).
% portray(dr(I,dl(J,lit(pp(X)),lit(pp(X))),lit(pp(X)))) :-
% 	!,
% 	format('dr(~p,dl(~p,~p,~p),~p)', [I,J,lit(pp(_)),lit(pp(_)),lit(pp(_))]).
% portray(dr(I,dl(0,lit(np(_,_,_)),lit(s(X))),dl(0,lit(np(_,_,_)),lit(s(X))))) :-
% 	!,
% 	format('dr(~p,dl(0,np,~p),dl(0,np,~p))', [I,lit(s(_)),lit(s(_))]).
% portray(dl(I,dl(0,lit(np(_,_,_)),lit(s(X))),dl(0,lit(np(_,_,_)),lit(s(X))))) :-
% 	!,
% 	format('dr(~p,dl(0,np,~p),dl(0,np,~p))', [I,lit(s(_)),lit(s(_))]).
% portray(dr(I,lit(s(X)),lit(s(X)))) :-
% 	!,
% 	format('dr(~p,~p,~p)', [I,lit(s(_)),lit(s(_))]).
% portray(dl(I,lit(s(X)),lit(s(X)))) :-
% 	!,
% 	format('dl(~p,~p,~p)', [I,lit(s(_)),lit(s(_))]).
% portray(dr(I,lit(pp(X)),lit(pp(X)))) :-
% 	!,
% 	format('dr(~p,~p,~p)', [I,lit(s(_)),lit(s(_))]).
% portray(dl(I,lit(pp(X)),lit(pp(X)))) :-
% 	!,
% 	format('dl(~p,~p,~p)', [I,lit(s(_)),lit(s(_))]).
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
portray(lit(s(SUB))) :-
	!,
	print(s),
	portray_subtype(SUB).
portray(lit(pp(SUB))) :-
	!,
	print(pp),
	portray_subtype(SUB).
portray(lit(A)) :-
	!,
	print(A).

portray_subtype(_).
% portray_subtype(V) :-
% 	var(V),
% 	!.
% portray_subtype(main) :-
% 	!.
% portray_subtype(inf(base)) :-
% 	!,
% 	write('_inf').
% portray_subtype(inf(de)) :-
% 	!,
% 	write('_deinf').
% portray_subtype(inf(par)) :-
% 	!,
% 	write('_parinf').
% portray_subtype(inf(pour)) :-
% 	!,
% 	write('_pourinf').
% portray_subtype(inf(a)) :-
% 	!,
% 	write('_ainf').
% portray_subtype(inf(à)) :-
% 	!,
% 	write('_ainf').
% portray_subtype(X) :-
% 	format('_~p', X).

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

chart_proof_to_parser_actions1(rule(axiom,Pros,Form-Sem-S1-S2-S3-S4,Premisses), rule(axiom,Pros,Form-Sem-S1-S2-S3-S4,Premisses)) -->
	!,
	[Pros-Form-S1-S2-S3-S4-'O'].
chart_proof_to_parser_actions1(rule(Name,Pros,Form,Premisses0), NewProof, ParserActions0, ParserActions) :-
	rule_premisses(Name, Premisses0, AuxPremisses, TruePremisses),
   (
	check_all_axioms(AuxPremisses),
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

check_all_axioms([]).
check_all_axioms([rule(axiom,_,_,_)|Rs]) :-
	check_all_axioms(Rs).

all_axiom_premisses([rule(axiom,Pros,Form-_-S1-S2-S3-S4,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName)},
	[Pros-Form-S1-S2-S3-S4-ActionName].
all_axiom_premisses([rule(axiom,Pros1,Form1-_-S1-S2-S3-S4,_),rule(axiom,Pros2,Form2-_-T1-T2-T3-T4,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName1)},
	{atom_concat(Name, '2', ActionName2)},
	[Pros1-Form1-S1-S2-S3-S4-ActionName1],
	[Pros2-Form2-T1-T2-T3-T4-ActionName2].
all_axiom_premisses([rule(axiom,Pros1,Form1-_-S1-S2-S3-S4,_),rule(axiom,Pros2,Form2-_-T1-T2-T3-T4,_),rule(axiom,Pros3,Form3-_-U1-U2-U3-U4,_)], Name) -->
	!,
	{atom_concat(Name, '1', ActionName1)},
	{atom_concat(Name, '2', ActionName2)},
	{atom_concat(Name, '3', ActionName3)},
	[Pros1-Form1-S1-S2-S3-S4-ActionName1],
	[Pros2-Form2-T1-T2-T3-T4-ActionName2],
	[Pros3-Form3-U1-U2-U3-U4-ActionName3].


% rule_premisses(RuleName, AllPremisses, AuxPremisses, TruePremisses)
rule_premisses(e_start, [A,B], [A], [B]) :-
	!.
rule_premisses(e_start_l, [A,B], [B], [A]) :-
	!.
rule_premisses(ef_start, List0, [A], Es) :-
	A = rule(_,_,dr(0,_,dr(0,_,dia(Ind,box(Ind,dr(0,_,_)))))-_-_-_-_-_,_),
	select(A, List0, Es),
	!.
rule_premisses(ef_start_iv, List0, [A], Es) :-
	A = rule(_,_,dr(0,_,dr(0,_,dia(Ind,box(Ind,dl(0,lit(np(_,_,_)),lit(s(_)))))))-_-_-_-_-_,_),
	select(A, List0, Es),
	!.
rule_premisses(gap_i, List0, [], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_-_-_-_-_,_),
	B = rule(_,_,X-_-_-_-_-_,_),
	member(A, List0),
	select(B, List0, Es),
	!.
rule_premisses(gap_c, List0, [A], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_-_-_-_-_,_),
	select(A, List0, Es),
	!.
rule_premisses(gap_e, List0, [A], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_-_-_-_-_,_),
	select(A, List0, Es),
	!.
rule_premisses(c_l_lnr, List0, [A], Es) :-
    A = rule(_,_,dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n)))-_-_-_-_-_,_),
    select(A, List0, Es), 
    !.
rule_premisses(c_r_lnr, List0, [A], Es) :-
    A = rule(_,_,dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n)))-_-_-_-_-_,_),
    select(A, List0, Es),
    !.
rule_premisses(prod_i, [A,B,C], Ds, Es) :-
    (
	A = rule(_,_,dr(0,_,p(0,_,_))-_-_-_-_-_,_)
    ->
        Ds = [A],
        Es = [B,C]
    ;
        C = rule(_,_,dl(0,p(0,_,_),_)-_-_-_-_-_,_)
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
print_action_list([X-Y-S1-S2-S3-S4-Z|Zs]) :-
	format('~w|~p|~p|~p|~p|~p|~w ', [X,Y,S1,S2,S3,S4,Z]),
	print_action_list(Zs).
