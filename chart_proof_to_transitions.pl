
proof_file('../TLGbank/parse_actions/aa1_parse_actions.pl', 'aa1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ap1_parse_actions.pl', 'ap1_transitions.txt').
% proof_file('../TLGbank/parse_actions/aq2_parse_actions.pl', 'aq2_transitions.txt').
% proof_file('../TLGbank/parse_actions/as2_parse_actions.pl', 'as2_transitions.txt').
% proof_file('../TLGbank/parse_actions/at_parse_actions.pl', 'at_transitions.txt').
% proof_file('../TLGbank/parse_actions/300_parse_actions.pl', '300_transitions.txt').
% proof_file('../TLGbank/parse_actions/1000_parse_actions.pl', '1000_transitions.txt').
% proof_file('../TLGbank/parse_actions/8000_parse_actions.pl', '8000_transitions.txt').
% proof_file('../TLGbank/parse_actions/annodis_parse_actions.pl', 'annodis_transitions.txt').
% proof_file('../TLGbank/parse_actions/frwiki1_parse_actions.pl', 'frwiki1_transitions.txt').
% proof_file('../TLGbank/parse_actions/frwiki2_parse_actions.pl', 'frwiki2_transitions.txt').
% proof_file('../TLGbank/parse_actions/emea_d_parse_actions.pl', 'emea_d_transitions.txt').
% proof_file('../TLGbank/parse_actions/europar_parse_actions.pl', 'europar_transitions.txt').


% proof_file('../TLGbank/parse_actions/aa2_parse_actions.pl', 'aa2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ab2_parse_actions.pl', 'ab2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ae1_parse_actions.pl', 'ae1_transitions.txt').
% proof_file('../TLGbank/parse_actions/af2_parse_actions.pl', 'af2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ag1_parse_actions.pl', 'ag1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ag2_parse_actions.pl', 'ag2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ah1_parse_actions.pl', 'ah1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ah2_parse_actions.pl', 'ah2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ai1_parse_actions.pl', 'ai1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ai2_parse_actions.pl', 'ai2_transitions.txt').
% proof_file('../TLGbank/parse_actions/aj1_parse_actions.pl', 'aj1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ak1_parse_actions.pl', 'ak1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ak2_parse_actions.pl', 'ak2_transitions.txt').
% proof_file('../TLGbank/parse_actions/al1_parse_actions.pl', 'al1_transitions.txt').
% proof_file('../TLGbank/parse_actions/am1_parse_actions.pl', 'am1_transitions.txt').
% proof_file('../TLGbank/parse_actions/am2_parse_actions.pl', 'am2_transitions.txt').
% proof_file('../TLGbank/parse_actions/an1_parse_actions.pl', 'an1_transitions.txt').
% proof_file('../TLGbank/parse_actions/an2_parse_actions.pl', 'an2_transitions.txt').
% proof_file('../TLGbank/parse_actions/ao1_parse_actions.pl', 'ao1_transitions.txt').
% proof_file('../TLGbank/parse_actions/ao2_parse_actions.pl', 'ao2_transitions.txt').


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
	abolish(transition/3),
	retractall(transition(_,_,_)),
        proof_file(InFile, _OutFile),
        format('~NTreating ~w...', [InFile]),
	flush_output,
        chart_proofs_to_transitions(InFile),
        format('done', []),
	flush_output,
        fail.
all_chart_proofs :-
	tell('transitions.pl'),
	listing(transition),
	told.


chart_proofs_to_transitions(InFile) :-
	abolish(proof/2),
	compile(InFile),
	chart_proofs_to_transitions1.

chart_proofs_to_transitions1 :-
	proof(N,_),
	chart_proof_to_transitions2(N),
	fail.
chart_proofs_to_transitions1.
	

chart_proof_to_transitions2(SentNum) :-
	proof(SentNum, Proof0),
	chart_proof_to_transitions(Proof0, _Proof).

chart_proof_to_transitions(Proof0, Proof) :-
	chart_proof_to_transitions1(Proof0, Proof1),
   (			      
	Proof0 \= Proof1
   ->
	chart_proof_to_transitions(Proof1, Proof)
   ;
        Proof = Proof0
   ).

chart_proof_to_transitions1(rule(axiom,Pros,Form-Sem-S1-S2-S3-S4,Premisses), rule(axiom,Pros,Form-Sem-S1-S2-S3-S4,Premisses)).
chart_proof_to_transitions1(rule(Name,Pros,Form,Premisses0), NewProof) :-
	rule_premisses(Name, Premisses0, AuxPremisses, TruePremisses),
   (
	check_all_axioms(AuxPremisses),
	all_axiom_premisses(TruePremisses, Name, Form)
   ->
        NewProof = rule(axiom,Pros,Form,[])
   ; 
        NewProof = rule(Name,Pros,Form,Premisses),
        chart_proof_to_transitions_list(TruePremisses, Premisses)
   ).


chart_proof_to_transitions_list([], []).
chart_proof_to_transitions_list([R0|Rs0], [R|Rs]) :-
	chart_proof_to_transitions1(R0, R),
	chart_proof_to_transitions_list(Rs0, Rs).

check_all_axioms([]).
check_all_axioms([rule(axiom,_,_,_)|Rs]) :-
	check_all_axioms(Rs).

all_axiom_premisses([rule(axiom,_Pros,Form0-_-S1-S2-S3-S4,_)], Name, CForm0-_-CS1-CS2-CS3-CS4) :-
	!,
	translate_form(Form0, Form),
	translate_form(CForm0, CForm),
	print_transition([tuple(Form,S1,S2,S3,S4)], Name, tuple(CForm,CS1,CS2,CS3,CS4)).
all_axiom_premisses([rule(axiom,_Pros1,Form1-_-S1-S2-S3-S4,_),rule(axiom,_Pros2,Form2-_-T1-T2-T3-T4,_)], Name, CForm0-_-CS1-CS2-CS3-CS4) :-
	!,
	translate_form(Form1, Form3),
	translate_form(Form2, Form4),
	translate_form(CForm0, CForm),
	print_transition([tuple(Form3,S1,S2,S3,S4),tuple(Form4,T1,T2,T3,T4)], Name, tuple(CForm,CS1,CS2,CS3,CS4)).
all_axiom_premisses([rule(axiom,_Pros1,Form1-_-S1-S2-S3-S4,_),rule(axiom,_Pros2,Form2-_-T1-T2-T3-T4,_),rule(axiom,_Pros3,Form3-_-U1-U2-U3-U4,_)], Name, CForm0-_-CS1-CS2-CS3-CS4) :-
	!,
	translate_form(Form1, Form4),
	translate_form(Form2, Form5),
	translate_form(Form3, Form6),
	translate_form(CForm0, CForm),
	print_transition([tuple(Form4,S1,S2,S3,S4),tuple(Form5,T1,T2,T3,T4),tuple(Form6,U1,U2,U3,U4)], Name, tuple(CForm,CS1,CS2,CS3,CS4)).

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
rule_premisses(gap_i, List0, [B], Es) :-
	A = rule(_,_,dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X))))-_,_),
	B = rule(_,_,X-_,_),
	member(A, List0),
	select(B, List0, Es),
	!.
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


print_transition(Premisses, Name, Conclusion) :-
	predictable(Premisses, Name, Conclusion),
	!.
print_transition(Premisses, Name, Conclusion) :-
	assert_if_new(transition(Premisses,Name,Conclusion)).

predictable([tuple(dr(_,A,B),S1,S2,S3,S4),tuple(B,T1,T2,T3,T4)], dr, tuple(A,U1,U2,U3,U4)) :-
	append(S1,T1,U1),
	append(S2,T2,U2),
	append(S3,T3,U3),
	append(S4,T4,U4),
	!.
predictable([tuple(B,S1,S2,S3,S4),tuple(dl(_,B,A),T1,T2,T3,T4)], dl, tuple(A,U1,U2,U3,U4)) :-
	append(S1,T1,U1),
	append(S2,T2,U2),
	append(S3,T3,U3),
	append(S4,T4,U4),
	!.
predictable([tuple(A,S1,S2,S3,S4),tuple(let,T1,T2,T3,T4)], let, tuple(A,U1,U2,U3,U4)) :-
	append(S1,T1,U1),
	append(S2,T2,U2),
	append(S3,T3,U3),
	append(S4,T4,U4),
	!.
predictable([tuple(let,S1,S2,S3,S4),tuple(A,T1,T2,T3,T4)], let, tuple(A,U1,U2,U3,U4)) :-
	append(S1,T1,U1),
	append(S2,T2,U2),
	append(S3,T3,U3),
	append(S4,T4,U4),
	!.


assert_if_new(Clause) :-
	call(Clause),
	!.
assert_if_new(Clause) :-
	write('.'),
	assert(Clause).

translate_form(lit(s(_)), s) :-
	!.
translate_form(lit(pp(_)), pp) :-
	!.
translate_form(lit(np(_,_,_)), np) :-
	!.
translate_form(lit(A), A).
translate_form(dl(I,A0,B0), dl(I,A,B)) :-
	translate_form(A0, A),
	translate_form(B0, B).
translate_form(dr(I,A0,B0), dr(I,A,B)) :-
	translate_form(A0, A),
	translate_form(B0, B).
translate_form(p(I,A0,B0), p(I,A,B)) :-
	translate_form(A0, A),
	translate_form(B0, B).
translate_form(dia(I,A0), dia(I,A)) :-
	translate_form(A0, A).
translate_form(box(I,A0), box(I,A)) :-
	translate_form(A0, A).
