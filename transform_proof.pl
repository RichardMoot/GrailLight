#!/Applications/SWI-Prolog.app/Contents/MacOS/swipl -q -g start -f

:- module(transform_proof, [start/0,transform_proof/2,transform_all_proofs/0,transform_all_proofs/1, latex_transform/1, latex_transform_full/1]).

:- use_module(sem_utils,   [replace_sem/4,get_fresh_variable_number/2,equivalent_semantics/2,unify_semantics/2]).
:- use_module(ordset,      [ord_dup_union/3,ord_dup_insert/3,ord_subtract/3,ord_select/3,ord_subset/2]).
:- use_module(latex,       [latex_proof/2,latex_header/2,latex_tail/1]).
:- use_module(print_proof, [print_proof/3]).

:- dynamic user:proof/2.

start :-
	current_prolog_flag(os_argv, Argv),
        append(_, [A|Av], Argv),
	file_base_name(A, 'transform_proof.pl'),
	!,
	chart_dir(ChDir),
	nd_dir(NDDir),
        tap(Av, ChDir, NDDir).


tap([], _, _) :-
	/* treat all files if no file argument specified */
	transform_all_proofs,
	halt.
tap([F|Fs], ChDir, NDDir) :-
	tap1([F|Fs], ChDir, NDDir).

tap1([], _, _) :-
	halt.
tap1([File0|Files], ChDir, NDDir) :-
   (	
	match_file(File0, File)
   ->		     
	format(user_error, '~NStarting ~w~n', [File]),
	atom_concat(File, '_proofs.pl', InFile0),
	atom_concat(ChDir, InFile0, InFile),
	atom_concat(File, '_nd.pl', OutFile0),
	atom_concat(NDDir, OutFile0, OutFile),
	abolish(user:proof/2),
	user:compile(InFile),
	transform_all_proofs(OutFile),
	format(user_error, '~NDone ~w~n', [File])
   ;
	format(user_error, '~NIgnored ~w~n', [File0])
   ),
	tap1(Files, ChDir, NDDir).
  
   
match_file(File0, File) :-
	/* coverts atoms like '8000' and '300' to Prolog integers */
	name(File0, Name),
	name(File1, Name),
	infile(File1),
	!,
	File = File1.
match_file(File0, File) :-
	name('chart_proofs/', X),
	name('_proofs.pl', W),
	name(File0, FN),
	append(X, Y, FN),
	append(V, W, Y),
	name(File, V),
	infile(File).

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
infile(frwiki1).
infile(frwiki2).
infile(monde_a).
infile(monde_b).

chart_dir('chart_proofs/').
nd_dir('nd_proofs/').

% = transform_all_proofs
%
% transform all infiles chart proofs in chart_dir into natural deduction proofs in nd_dir.

transform_all_proofs :-
	chart_dir(ChDir),
	nd_dir(NDDir),
	infile(Root),
	format(user_error, '~NStarting ~w~n', [Root]),
	atom_concat(Root, '_proofs.pl', InFile0),
	atom_concat(ChDir, InFile0, InFile),
	atom_concat(Root, '_nd.pl', OutFile0),
	atom_concat(NDDir, OutFile0, OutFile),
	abolish(proof/2),
	compile(InFile),
	transform_all_proofs(OutFile),
	format(user_error, '~NDone ~w~n', [Root]),
	fail.
transform_all_proofs.

% = transform_all_proofs(+OutFile)
%
% transform all proofs in memory (per the proof/2 predicate) into natural deduction
% files in the OutFile given as an argument.

transform_all_proofs(OutputFile) :-
	open(OutputFile, write, Stream, []),
	transform_all_proofs1(Stream).

transform_all_proofs1(Stream) :-
	user:proof(N, P0),
   (	
	transform_proof(P0, P)
   ->
	print_proof(N, P, Stream)
   ;
	format(user_error, '~N{Warning: failed to transform proof ~d!}~n', [N])
   ),
	fail.
transform_all_proofs1(Stream) :-
	close(Stream).
	
% = numbervars_proof(+Proof)
%
% uses numbervars/3 only on the different semantic components of the proof.

numbervars_proof(Proof) :-
	numbervars_proof(Proof, 0, _).

numbervars_proof(rule(_, _, _-Sem, Premisses), N0, N) :-
	/* ensure correct behaviour even when the semantics already contains occurrences of '$VAR'(N) terms */
	get_fresh_variable_number(Sem, Max0),
	Max is max(Max0, N0),
	numbervars(Sem, Max, N1),
	numbervars_proof_list(Premisses, N1, N).

numbervars_proof_list([], N, N).
numbervars_proof_list([P|Ps], N0, N) :-
	numbervars_proof(P, N0, N1),
	numbervars_proof_list(Ps, N1, N).

latex_transform(N) :-
	user:proof(N, P),
	transform_proof(P, Q),
	open('proof.tex', write, Stream),
	latex_header(Stream, 'paperwidth=400cm,textwidth=395cm'),
	latex_proof(P, Stream),
	latex_proof(Q, Stream),
	latex_tail(Stream),
	close(Stream).

latex_transform_full(N) :-
	user:proof(N, P),
	open('proof.tex', write, Stream),
	latex_header(Stream, 'paperwidth=400cm,textwidth=395cm'),
	latex_proof(P, Stream),
	latex_transform_proof_full1(P, Stream),
	latex_tail(Stream),
	close(Stream).

latex_transform_proof_full1(P, Stream) :-
	numbervars_proof(P),
	latex_transform_proof_full1(P, 0, _N, Stream).

latex_transform_proof_full1(P, N0, N, Stream) :-
	transform_proof(P, N0, N1, Q),
    (
	P = Q
     ->
        N = N1
    ;
	latex_proof(Q, Stream),
        latex_transform_proof_full1(Q, N1, N, Stream)
    ).


transform_proof(P, Q) :-
	numbervars_proof(P),
	transform_proof1(P, 0, _N, Q).

% = iterate transform_proof/4 until the proof stays the same

transform_proof1(P, N0, N, Q) :-
	transform_proof(P, N0, N1, Q1),
    (
	P = Q1
     ->
        Q = Q1,
        N = N1
    ;
        transform_proof1(Q1, N1, N, Q)
    ).

% Invariant: N0 is the index of the first unused prosodic variable (that is '$VAR'(N0) does not occur in the proof).

% = remove "let" rule altogether

transform_proof(rule(let, Pros, D-Sem, [Proof1, Proof2]), N0, N, Proof) :-
   (	
	Proof1 = rule(axiom, _, lit(let)-true, []),
	Proof2 = rule(Nm, _, D-Sem, Proofs)
   ->
        true
   ;
	Proof1 = rule(Nm, _, D-Sem, Proofs),
	Proof2 = rule(axiom, _, lit(let)-true, [])
   ->
         true
   ),
	N = N0,
	Proof = rule(Nm, Pros, D-Sem, Proofs).
			
transform_proof(rule(gap_i, GoalPros, D-Sem, [Proof3, Proof2, Proof1]), N0, N,
		rule(dr, GoalPros, D-Sem,
		     [rule(dl, p(0,ProsC2,Pros1), dr(0,Y,box(I,dia(I,dr(0,Z,V))))-appl(Term2,lambda(Var,TermX)),
			   [rule(drdiaboxi(I,N0), ProsC2, X-lambda(Var,TermX), [ProofC1]),
			    Proof1
			   ]),
		      Proof2
		     ])) :-
	N1 is N0 + 1,
	Sem = appl(appl(Term2,lambda(Var,TermX)),_Term0),
	rule_conclusion(Proof1, Pros1, ExtrForm, _),
	rule_conclusion(Proof2, Pros2, dr(0,Z,V), _),
	/* do a recursive transformation before replace_proof_bag which may otherwise erase information */
	transform_proof1(Proof3, N1, N, Proof4), 
	rule_conclusion(Proof4, _Pros3, _, _),
	ExtrForm = dl(0,X,dr(0,Y,box(I,dia(I,dr(0,Z,V))))),
	bag_of_words(Pros2, Bag),
	replace_proof_bag(Proof4, Bag, '$VAR'(N0), rule(hyp(N0), '$VAR'(N0), dr(0,Z,V)-Var, []), ProofC1),
	/* TODO: globally replace Sem by Var in all of ProofC0 */
	rule_conclusion(ProofC1, ProsC1, _, _),
	replace_pros(ProsC1, '$VAR'(N0), '$TRACE'(N0), ProsC2),
	!.

transform_proof(rule(e_end, GoalPros, D-Sem, [Proof1, Proof2]), N0, N,
		rule(dr, GoalPros, D-Sem,
		     [Proof1,
		      rule(drdiaboxi(I,N0), YZ, dr(0,C,dia(I,box(I,dr(0,A,B))))-true,
			   [Proof4])
		     ])) :-
        /* X = "et", YZ = sentence with extracted verb */
	GoalPros = p(_,X,YZ),
	ExtrForm = dr(0,D,dr(0,C,dia(I,box(I,dr(0,A,B))))),
	rule_conclusion(Proof1, X, ExtrForm, _),
	/* Pros = (prosody of) argument B */ 
	find_e_start(Proof2, ef_start, X, ExtrForm, dr(0,A,B), N0, Pros, Proof3),
	global_replace_pros(Proof3, Pros, p(0,'$VAR'(N0), Pros), N0, Proof4),
	N is N0 + 1,
	!.
transform_proof(rule(e_end, GoalPros, D-Sem, [Proof1, Proof2]), N0, N,
		rule(dr, GoalPros, D-Sem,
		     [Proof1,
		      rule(drdiaboxi(I,N0), YZ, dr(0,C,dia(I,box(I,B)))-true, [Proof4])
		     ])) :-
	GoalPros = p(_,X,YZ),
	ExtrForm = dr(0,D,dr(0,C,dia(I,box(I,B)))),
	rule_conclusion(Proof1, X, ExtrForm, _),
	find_e_start(Proof2, e_start, X, ExtrForm, B, N0, Pros, Proof3),
	global_replace_pros(Proof3, Pros, p(0,Pros,'$VAR'(N0)), N0, Proof4),
	N is N0 + 1,
	!.
transform_proof(rule(e_endd, GoalPros, dl(I0,E,D)-Sem, [Proof1,Proof2]), N0, N,
                rule(dli1(I,N1), GoalPros, dl(I,E,D)-Sem,
		     [rule(dr, p(II,X,p(1,'$VAR'(N1),YZ)), D-Sem,
			   [Proof1,
			    rule(drdiaboxi(J,N0), p(1,'$VAR'(N1),YZ), dr(0,C,dia(J,box(J,B)))-true,
				 [rule(dl, p(1,'$VAR'(N1),YZ1), C-appl(Sem1,Var),
				       [rule(hyp(N1), '$VAR'(N1), E-Var, []),
					Proof4
				       ])])])])) :-
	N1 is N0 + 1,
	N is N1 + 1,
	GoalPros = p(II,X,YZ),
	ExtrForm = dr(0,D,dr(0,C,dia(J,box(J,B)))),
	rule_conclusion(Proof1, X, ExtrForm, _),
	rule_conclusion(Proof2, YZ, dl(I0,E,C), Sem1),
	quote_mode(I0, I),
	find_e_start(Proof2, e_start, X, ExtrForm, B, N0, Pros, Proof3),
	global_replace_pros(Proof3, Pros, p(0,Pros,'$VAR'(N0)), N0, Proof4),
	rule_conclusion(Proof4, YZ1, _, _),
	!.

transform_proof(rule(e_end_l, GoalPros, D-Sem, [Proof1, Proof2]), N0, N,
		rule(dl, GoalPros, D-Sem,
		     [rule(dldiaboxi(I,N0), XY, dr(0,C,dia(I,box(I,B)))-true, [Proof4]),
		      Proof2])) :-
	GoalPros = p(_,XY,Z),
	ExtrForm = dl(0,dr(0,C,dia(I,box(I,B))),D),
	rule_conclusion(Proof2, Z, ExtrForm, _),
	find_e_start(Proof1, e_start_l, Z, ExtrForm, B, N0, Pros, Proof3),
	global_replace_pros(Proof3, Pros, p(0,Pros,'$VAR'(N0)), N0, Proof4),
	N is N0 + 1,
	!.
transform_proof(rule(gap_e, GoalPros, dr(0,X,Y)-Sem, [Proof1, Proof2]), N0, N,
		rule(drdiaboxi(0,N0), GoalPros, dr(0,X,Y)-Sem,
		     [Proof4])) :-
	ExtrForm = dl(0,dr(0,_,dia(I,box(I,dr(0,X,Y)))),_),
	rule_conclusion(Proof2, Z, ExtrForm, _),
	find_e_start(Proof1, gap_c, Z, ExtrForm, Y, N0, Pros, Proof3),
	global_replace_pros(Proof3, Pros, p(0,Pros,'$VAR'(N0)), N0, Proof4),	
	N is N0 +1,	
	!.
transform_proof(rule(wpop_vp, GoalPros, _-Sem, [Proof1]), N0, N, ProofC) :-
   (
	Sem = lambda(X,appl(SemADV,appl(_SemVP,X0))),
	X0 == X
   ;
        Sem = lambda(CL,lambda(X,appl(SemADV,appl(appl(_SemVP,CL0),X0)))),
	X0 == X,
	CL0 == CL
   ),
	find_w_start(Proof1, LPros, RPros, _AdvF, SemADV, ProofB, Proof2),
	global_replace_pros(Proof2, p(1,LPros,RPros), LPros, ProofA),
	merge_proofs(ProofA, ProofB, Wrap, Wrap, GoalPros, N0, N, ProofC),
	!.
transform_proof(rule(wpop_vp, GoalPros, dl(0,CL,dl(0,NP,S))-Sem, [Proof1]), N0, N,
		rule(dli(N3), GoalPros, dl(0,CL,dl(0,NP,S))-Sem,
		     [rule(dli(N2), p(0,'$VAR'(N3),GoalPros), dl(0,NP,S)-true, 
			  [rule(prod_e(NB), p(0,'$VAR'(N2),p(0,'$VAR'(N3),GoalPros)), S-true,
				[ProofP,
				 rule(dl1, p(0,'$VAR'(N2),p(0,'$VAR'(N3), GPP)), S-true,
				      [rule(dl, p(0,'$VAR'(N2), p(0,'$VAR'(N3), GPB)), S-true, 
					    [rule(hyp(N2), '$VAR'(N2), NP-'$VAR'(VB), []),
					     rule(dl, p(0,'$VAR'(N3), GPB), dl(0,NP,S)-true,
						  [rule(hyp(N3), '$VAR'(N3), CL-'$VAR'(V2),[]),
						   Proof3])]),
				       rule(hyp(N0), '$VAR'(NA), dl(1,S,S)-'$VAR'(VA), [])])])])])) :-
	/* VP with clitic */
	 Sem = lambda(_CL,lambda(_X,appl(Adv0,_))),
   (
	 Adv0 = pi1(SemAdv),NA=N0,NB=N1,VA=V0,VB=V1
   ;
         Adv0 = pi2(SemAdv),NA=N1,NB=N0,VA=V1,VB=V0
   ),
	get_fresh_variable_number(Sem, V0),
	V1 is V0 + 1,
	V2 is V1 + 1,			 
	N1 is N0 + 1,
	N2 is N1 + 1,
	N3 is N2 + 1,
	N is N2 + 1,								       
	find_prod_w_start(Proof1, N0, NB, VB, SemAdv, PPros, ProofP, Proof2),
	global_replace_pros(Proof2, PPros, '$VAR'(NB), Proof3),
	replace_pros(GoalPros, PPros, '$VAR'(NB), GPB),
	replace_pros(GoalPros, PPros, p(0,'$VAR'(N0),'$VAR'(N1)), GPP),
	!.
transform_proof(rule(wpop_vp, GoalPros, dl(0,NP,S)-Sem, [Proof1]), N0, N,
		rule(dli(N2), GoalPros, dl(0,NP,S)-Sem,
		     [rule(prod_e(NB), p(0,'$VAR'(N2),GoalPros), S-true,
			  [ProofP,
			   rule(dl1, p(0,'$VAR'(N2), GPP), S-true,
				[rule(dl, p(0,'$VAR'(N2), GPB), S-true, 
				      [rule(hyp(N2), '$VAR'(N2), NP-'$VAR'(VB), []),
				       Proof3]),
				 rule(hyp(N0), '$VAR'(NA), dl(1,S,S)-'$VAR'(VA), [])])])])) :-
	/* VP-scope adverb */
	 Sem = lambda(X,appl(Adv0,appl(_SemVP,X))),
   (
	 Adv0 = pi1(SemAdv),NA=N0,NB=N1,VA=V0,VB=V1
   ;
         Adv0 = pi2(SemAdv),NA=N1,NB=N0,VA=V1,VB=V0
   ),
	get_fresh_variable_number(Sem, V0),
	V1 is V0 + 1,
	N1 is N0 + 1,
	N2 is N1 + 1,
	N is N2 + 1,								       
	find_prod_w_start(Proof1, N0, NB, VB, SemAdv, PPros, ProofP, Proof2),
	global_replace_pros(Proof2, PPros, '$VAR'(NB), Proof3),
	replace_pros(GoalPros, PPros, '$VAR'(NB), GPB),
	replace_pros(GoalPros, PPros, p(0,'$VAR'(N0),'$VAR'(N1)), GPP),
	!.
transform_proof(rule(wpop, GoalPros, S-Sem, [Proof1]), N0, N,
		rule(prod_e(NB), GoalPros, S-Sem,
			  [ProofP,
			   rule(dl1, GPP, S-true, 
				 [Proof3,
				  rule(hyp(N0), '$VAR'(NA), dl(1,S,S)-'$VAR'(VA), [])])])) :-
	/* sentence-scope adverb */
	 Sem = appl(Adv0,_Sem),
   (
	 Adv0 = pi1(SemAdv),NA=N0,NB=N1,VA=V0,VB=V1
   ;
         Adv0 = pi2(SemAdv),NA=N1,NB=N0,VA=V1,VB=V0
   ),
	get_fresh_variable_number(Sem, V0),
	V1 is V0 + 1,
	N1 is N0 + 1,
	N2 is N1 + 1,
	N is N2 + 1,								       
	find_prod_w_start(Proof1, N0, NB, VB, SemAdv, PPros, ProofP, Proof2),
	global_replace_pros(Proof2, PPros, '$VAR'(NB), Proof3),
	replace_pros(GoalPros, PPros, p(0,'$VAR'(N0),'$VAR'(N1)), GPP),
	!.
transform_proof(rule(wpop_vpi, GoalPros, _, [ProofA,ProofB]), N0, N, ProofC) :-
	merge_proofs_left(ProofA, ProofB, Wrap, Wrap, GoalPros, N0, N, ProofC),
	!.
transform_proof(rule(wpop, GoalPros, _-Sem, [Proof1]), N0, N, ProofC) :-
	Sem = appl(SemADV, _),
	find_w_start(Proof1, LPros, RPros, _AdvF, SemADV, ProofB, Proof2),
	global_replace_pros(Proof2, p(1,LPros,RPros), LPros, ProofA),
	merge_proofs(ProofA, ProofB, Wrap, Wrap, GoalPros, N0, N, ProofC),
	!.
% = simplify dit_np/se_dit/a_dit combinations
transform_proof(rule(dit_np, p(0,p(0,P,p(0,Q,R)),P1), dl(I0,lit(s(ST)),lit(s(ST)))-Sem,
		     [rule(se_dit, p(0,P,p(0,Q,R)), dl(I0,lit(s(ST)),_)-_,
			   [ProofClr,
			    rule(a_dit_se, p(0,Q,R), dl(I0,lit(s(ST)),Res)-_,
				 [ProofAux,
				  ProofPPart])]),
		      ProofNP]),
 		N0, N,
 		rule(dli1(I,N0), p(0,p(0,P,p(0,Q,R)),P1), dl(I,lit(s(ST)),lit(s(ST)))-Sem,
 		     [rule(dr,   p(0,p(0,P,p(0,Q,p(I,'$VAR'(N0),R))),P1), lit(s(ST))-true,
 			   [rule(dl, p(0,P,p(0,Q,p(I,'$VAR'(N0),R))), dr(0,lit(s(ST)),lit(np(_,_,_)))-true,
				 [ProofClr,
				  rule(dr, p(0,Q,p(I,'$VAR'(N0),R)), Res-true,
				       [ProofAux,
					rule(dl, p(I,'$VAR'(N0),R), PPart-appl(_,Z),
					     [rule(hyp(N0), '$VAR'(N0), lit(s(ST))-Z, []),
					      ProofPPart])])
				  ]),
 			    ProofNP])])) :-
 	!,
 	N is N0 + 1,
 	quote_mode(I0, I),
 	ProofAux = rule(_, _, dr(0,Res,PPart)-_, _).
transform_proof(rule(dit_np, p(0,p(0,P,p(0,Q,R)),P1), dl(I0,lit(s(ST)),lit(s(ST)))-Sem,
		     [rule(se_dit, p(0,P,p(0,Q,R)), dl(I0,lit(s(ST)),_)-_,
			   [ProofClr,
			    rule(a_dit, p(0,Q,R), dl(I0,lit(s(ST)),Res)-_,
				 [ProofAux,
				  ProofPPart])]),
		      ProofNP]),
 		N0, N,
 		rule(dli1(I,N0), p(0,p(0,P,p(0,Q,R)),P1), dl(I,lit(s(ST)),lit(s(ST)))-Sem,
 		     [rule(dr,   p(0,p(0,P,p(0,Q,p(I,'$VAR'(N0),R))),P1), lit(s(ST))-true,
 			   [rule(dl, p(0,P,p(0,Q,p(I,'$VAR'(N0),R))), dr(0,lit(s(ST)),lit(np(_,_,_)))-true,
				 [ProofClr,
				  rule(dr, p(0,Q,p(I,'$VAR'(N0),R)), Res-true,
				       [ProofAux,
					rule(dl, p(I,'$VAR'(N0),R), PPart-appl(_,Z),
					     [rule(hyp(N0), '$VAR'(N0), lit(s(ST))-Z, []),
					      ProofPPart])])
				  ]),
 			    ProofNP])])) :-
 	!,
 	N is N0 + 1,
 	quote_mode(I0, I),
 	ProofAux = rule(_, _, dr(0,Res,PPart)-_, _).

%Sem2 = appl(lambda(Z,appl(_X,appl(Y,Z))),_V),
% 	Sem1 = appl(Sem2,W),
% 	Sem = lambda(W,Sem1).

% = simplify dit_np/a_dit combinations

transform_proof(rule(dit_np, p(0,p(0,P,Q),R), dl(I0,lit(s(ST)),lit(s(ST)))-Sem,
 		     [rule(a_dit, p(0,P,Q), dl(I0,lit(s(ST)),dr(0,lit(s(ST)),lit(np(A,B,C))))-_, [ProofAux,ProofPPart]), ProofNP]),
 		N0, N,
 		rule(dli1(I,N0), p(0,p(0,P,Q),R), dl(I,lit(s(ST)),lit(s(ST)))-Sem,
 		     [rule(dr, p(0,p(0,P,p(I,'$VAR'(N0),Q)),R), lit(s(ST))-Sem1,
 			   [rule(dr, p(0,P,p(I,'$VAR'(N0),Q)), dr(0,lit(s(ST)),lit(np(A,B,C)))-Sem2,
 				 [ProofAux,
 				  rule(dl, p(I,'$VAR'(N0),Q), PPart-appl(Y,Z),
 				       [rule(hyp(N0), '$VAR'(N0), lit(s(ST))-Z, []),
 					ProofPPart])]),
 			    ProofNP])])) :-
 	!,
 	N is N0 + 1,
 	quote_mode(I0, I),
 	ProofAux = rule(_, _, dr(0,_,PPart)-_, _),
 	Sem2 = appl(lambda(Z,appl(_X,appl(Y,Z))),_V),
 	Sem1 = appl(Sem2,W),
 	unify_semantics(Sem,lambda(W,Sem1)).
transform_proof(rule(a_dit, p(0,ProsL,ProsR), dl(I0,Y,X)-Sem, [Left,Right]), N0, N,
 		rule(dli1(I,N0), p(0,ProsL,ProsR), dl(I,Y,X)-Sem,
 		     [rule(dr, p(0,ProsL,p(I,'$VAR'(N0),ProsR)), X-appl(Sem1,appl(Sem2,S)),
 			   [Left,
 			    rule(dl, p(I,'$VAR'(N0),ProsR), PPart-appl(Sem2,S),
 				 [rule(hyp(N0), '$VAR'(N0), Y-S, []),
 				  Right])])])) :-
 	!,
	N is N0 + 1,
 	quote_mode(I0, I),
 	Right = rule(_, _, dl(1,Y,PPart)-_, _), 
 	Sem = lambda(S, appl(Sem1,appl(Sem2,S))).
transform_proof(rule(a_dit_se, p(0,ProsL,ProsR), dl(I0,Y,X)-Sem, [Left,Right]), N0, N,
 		rule(dli1(I,N0), p(0,ProsL,ProsR), dl(I,Y,X)-Sem,
 		     [rule(dr, p(0,ProsL,p(I,'$VAR'(N0),ProsR)), X-appl(Sem1,appl(Sem2,S)),
 			   [Left,
 			    rule(dl, p(I,'$VAR'(N0),ProsR), PPart-appl(Sem2,S),
 				 [rule(hyp(N0), '$VAR'(N0), Y-S, []),
 				  Right])])])) :-
 	!,
	N is N0 + 1,
 	quote_mode(I0, I),
 	Right = rule(_, _, dl(1,Y,PPart)-_, _), 
 	Sem = lambda(S, appl(Sem1,appl(Sem2,S))).
transform_proof(rule(dit_np, p(0,ProsL,ProsR), dl(I0,Y,X)-Sem, [Left,Right]), N0, N,
		rule(dli1(I,N0), p(0,ProsL,ProsR), dl(I,Y,X)-Sem,
		     [rule(dr, p(0,p(1,'$VAR'(N0),ProsL), ProsR), X-Sem1,
			   [rule(dl, p(1,'$VAR'(N0),ProsL), dr(0,X,lit(np(A,B,C)))-appl(PrtSem,S),
				 [rule(hyp(N0), '$VAR'(N0), Y-S, []),
				  Left]),
			   Right])])) :-
	!,
	N is N0 + 1,
	quote_mode(I0, I),
	Right = rule(_, _, lit(np(A,B,C))-_, _),
	Sem1 = appl(appl(PrtSem, S), _),
	Sem = lambda(S, Sem1).
transform_proof(rule(se_dit, p(0,ProsL,ProsR), dl(I0,Y,X)-Sem, [Left,Right]), N0, N,
		rule(dli1(I,N0), p(0,ProsL,ProsR), dl(I,Y,X)-Sem,
		     [rule(dl, p(0,ProsL,p(1,'$VAR'(N0),ProsR)), X-Sem1,
			   [Left,
			    rule(dl, p(1,'$VAR'(N0),ProsR), dl(0,lit(cl_r),X)-appl(PrtSem,S),
				 [rule(hyp(N0), '$VAR'(N0), Y-S, []),
				  Right])])])) :-
	!,
	N is N0 + 1,
	quote_mode(I0, I),
	Left = rule(_, _, lit(cl_r)-_, _),
	Sem1 = appl(appl(PrtSem, S), _),
	Sem = lambda(S, Sem1).
transform_proof(rule(e_end_l_lnr, Pros1, F1-Sem1, [LeftProofs, rule(e_end_r_lnr, Pros2, F2-Sem2, [AndProof,RightProofs])]),
		N0, N,
		rule(dl, Pros1, F1-Sem1, [Left, rule(dr, Pros2, F2-Sem2, [AndProof, Right])])) :-
	!,
	collect_left_proofs(LeftProofs, N0, N1, Left),
	collect_right_proofs(RightProofs, N1, N, Right).
transform_proof(rule(e_end_l_lnr, Pros1, F1-Sem1, [LeftProofs, AndProof]),
		N0, N,
		rule(dl, Pros1, F1-Sem1, [Left, AndProof])) :-
	!,
	collect_left_proofs(LeftProofs, N0, N, Left).

% product rules
transform_proof(rule(prod_i, p(0,Pros1,Pros2), p(0,A,B)-Sem, [Left,Mid,Right]), N, N,
		rule(prod_i, p(0,Pros1,Pros2), p(0,A,B)-Sem, [Proof1,Proof2])) :-
	/* remove auxiliary hypothesis (of the form C/(A*B) or (A*B)\C) */
        !,
  (
	Left = rule(_, Pros1, A-_, _),
	Mid = rule(_, Pros2, B-_, _)
  ->
	Proof1 = Left,
	Proof2 = Mid
  ;
	Mid = rule(_, Pros1, A-_, _),
	Right = rule(_, Pros2, B-_, _)
  ->
	Proof1 = Mid,
	Proof2 = Right
  ).
transform_proof(rule(prod_i3, p(0,p(0,Pros1,Pros2),Pros3), p(0,p(0,A,B),C)-Sem, [Left,Mid1,Mid2,Right]), N, N,
		rule(prod_i, p(0,p(0,Pros1,Pros2),Pros3), p(0,p(0,A,B),C)-Sem,
		     [rule(prod_i, p(0,Pros1,Pros2), p(0,A,B)-true, [Proof1,Proof2]),
		      Proof3])) :-
	/* remove auxiliary hypothesis (of the form C/(A*B) or (A*B)\C) */
	select(rule(NmA, Pros1, A-SA, PrmsA), [Left,Mid1,Mid2,Right], Rest0),
	select(rule(NmB, Pros2, B-SB, PrmsB), Rest0, Rest1),
	member(rule(NmC, Pros3, C-SC, PrmsC), Rest1),
        !,
	Proof1 = rule(NmA, Pros1, A-SA, PrmsA),
	Proof2 = rule(NmB, Pros2, B-SB, PrmsB),
	Proof3 = rule(NmC, Pros3, C-SC, PrmsC).

transform_proof(rule(prod_e, Pros, FS,
		     [rule(prod_c, _, _, [Proof1, Proof2])]),
		N0, N,
		rule(prod_e(N0), Pros, FS,
		     [PProof,
		      rule(dr, p(0,p(0,Pros1,'$VAR'(N0)),'$VAR'(N1)), A-appl(appl(Sem1,'$VAR'(V0)),'$VAR'(V1)),
			   [rule(dr, p(0,Pros1,'$VAR'(N0)), dr(0,A,C)-appl(Sem1,'$VAR'(V0)),
				 [Proof1,
				  AProof]),
			    rule(hyp(N0), '$VAR'(N1), C-'$VAR'(V1), [])
			   ])
		     ])) :-
	Proof1 = rule(_, Pros1, dr(0,dr(0,A,C),B)-Sem1, _),
	Proof2 = rule(_, _, p(0,B,dia(0,box(0,C)))-Sem2, _),
	/* obtain two variables which as fresh wrt Sem1 and Sem2 */
	get_fresh_variable_number(appl(Sem1,Sem2), V0),
	V1 is V0 + 1,
	N1 is N0 + 1,
	N is N1 + 1,
	prod_root(Proof2, p(0,B,dia(0,box(0,C))), PProof, rule(hyp(N0),'$VAR'(N0), B-'$VAR'(V0), []), AProof),
	!.
transform_proof(rule(prod_dr, Pros, FS, Proofs), N, N, rule(dr, Pros, FS, Proofs)) :-
	!.
transform_proof(rule(Nm, Pros, F, Ds0), N0, N, rule(Nm, Pros, F, Ds)) :-
	is_list(Ds0),
	!,
	transform_proof_list(Ds0, N0, N, Ds).

transform_proof(Strange, _, _, null) :-
  (	
	var(Strange)
  ->
	format(user_error, '~N{Error: free variable used for proof!}~n', [])
  ;
	functor(Strange, F, A),
	format(user_error, '{Error: strange proof term with functor ~w/~w: ~w}', [F,A,Strange])
  ),
        fail.

transform_proof_list([], N, N, []).
transform_proof_list([P|Ps], N0, N, [Q|Qs]) :-
	transform_proof(P, N0, N1, Q),
	transform_proof_list(Ps, N1, N, Qs).


% = product introduction rules
prod_root(rule(prod_cl, _, _, [Proof1, Proof2]), p(0,A,dia(0,box(0,C))),
	  PProof, rule(Nm, HPros, A-V, []), rule(dl, p(0,Pros1,Pros2), B-appl(Y,X), [Proof1,Result])) :-
	!,
	Proof1 = rule(_, Pros1, A-X, _),
	Proof2 = rule(_, _, p(0,dl(0,A,B),dia(0,box(0,C)))-_, _),
	prod_root(Proof2, p(0,B,dia(0,box(0,C))), PProof, rule(Nm, HPros, B-V, []), Result),
	Result = rule(_, Pros2, _-Y, _).
prod_root(rule(prod_c, _, _, [Proof1, Proof2]), p(0,A,dia(0,box(0,C))),
	  PProof, rule(Nm, HPros, A-V, []), rule(dr, p(0,Pros1,Pros2), A-appl(X,Y), [Proof1,Result])) :-
	!,
	Proof1 = rule(_, Pros1, dr(0,A,B)-X, _),
	Proof2 = rule(_, _, p(0,B,dia(0,box(0,C)))-_, _),
	prod_root(Proof2, p(0,B,dia(0,box(0,C))), PProof, rule(Nm, HPros, B-V, []), Result),
	Result = rule(_, Pros2, _-Y, _).
prod_root(PProof, _, PProof, Result, Result).

% left-node-raising constructions

collect_left_proofs(LeftProofs, N0, N, rule(dldiaboxi(0,N0), Pros, dl(0,dia(0,box(0,lit(n))),lit(n))-X, [Left])) :-
	LeftProofs = rule(_, Pros, _-X, _),
	list_left_proofs(LeftProofs, List, []),
	N is N0 + 1,
	StartL = rule(hyp(N0), '$VAR'(N0), lit(n)-_, []),
	combine_lnr_proofs(List, StartL, Left).

collect_right_proofs(RightProofs, N0, N, rule(drdiaboxi(0,N0), Pros, dl(0,dia(0,box(0,lit(n))),lit(n))-X, [Left])) :-
	RightProofs = rule(_, Pros, _-X, _),
	list_right_proofs(RightProofs, List, []),
	N is N0 +1,
	StartR = rule(hyp(N0), '$VAR'(N0), lit(n)-_, []),
	combine_lnr_proofs(List, StartR, Left). 

list_left_proofs(rule(c_l_lnr, _, _, [L1, L2, _]), Rest0, Rest) :-
	!,
	list_left_proofs(L1, Rest0, Rest1),
	list_left_proofs(L2, Rest1, Rest).
list_left_proofs(Proof, [Proof|Rest], Rest).

list_right_proofs(rule(c_r_lnr, _, _, [_, R1, R2]), Rest0, Rest) :-
	!,
	list_right_proofs(R1, Rest0, Rest1),
	list_right_proofs(R2, Rest1, Rest).
list_right_proofs(Proof, [Proof|Rest], Rest).

combine_lnr_proofs([], Proof, Proof).
combine_lnr_proofs([L|Ls], Proof0, Proof) :-
	L = rule(_, Pros2, _-X, _),
	Proof0 = rule(_, Pros1, _-Y, _),
	combine_lnr_proofs(Ls, rule(dl, p(0,Pros1,Pros2), lit(n)-appl(X,Y), [Proof0, L]), Proof).

%

merge_proofs_left(RuleA, RuleB, Wrap0, Wrap, GoalPros, N, N, rule(dl1, Wrap0, A-appl(P,M), [RuleB, RuleA])) :-
	/* REMARK: this instance of the \_1 rule is not necessarily a right daughter */
	/* If it is important to distinguish this case, change the rule name above from "dl1" to something else */
	RuleA = rule(_, _, dl(1,B,A)-P, _),
	RuleB = rule(_, _, B-M, _),
	!,
	Wrap = GoalPros.
merge_proofs_left(RuleA, RuleB, Pros0, p(0,Pros,'$VAR'(N0)), GoalPros, N0, N, rule(dri(N0), ProsA, dr(I,A,B)-lambda(X,M), [Rule])) :-
	RuleB = rule(_, ProsB, dr(I,A,B)-SemAB, _),
	!,
	N1 is N0 + 1,
	Hyp = rule(hyp(N0), '$VAR'(N0), B-X, []),
	Rule = rule(_, p(_, ProsA, _), A-M, _), 
        merge_proofs_left(RuleA, rule(dr, p(I,ProsB,'$VAR'(N0)), A-appl(SemAB,X), [RuleA, Hyp]), Pros0, Pros, GoalPros, N1, N, Rule).
merge_proofs_left(RuleA, RuleB, Pros0, p(0,'$VAR'(N0),Pros), GoalPros, N0, N, rule(dli(N0), ProsA, dl(I,B,A)-lambda(X,M), [Rule])) :-
	RuleB = rule(_, ProsB, dl(I,B,A)-SemBA, _),
	!,
	N1 is N0 + 1,
	Hyp = rule(hyp(N0), '$VAR'(N0), B-X, []),
	Rule = rule(_, p(_, _, ProsA), A-M, _),
	merge_proofs_left(RuleA, rule(dl, p(I,'$VAR'(N0),ProsB), A-appl(SemBA,X), [Hyp, RuleB]), Pros0, Pros, GoalPros, N1, N, Rule).

	
merge_proofs(RuleA, RuleB, Wrap0, Wrap, GoalPros, N, N, rule(dl1, Wrap0, A-appl(P,M), [RuleA, RuleB])) :-
	RuleA = rule(_, _ProsA, B-M, _),
	RuleB = rule(_, _ProsB, dl(1,B,A)-P, _),
	!,
	Wrap = GoalPros.
merge_proofs(RuleA, RuleB, Pros0, p(0,Pros,'$VAR'(N0)), GoalPros, N0, N, rule(dri(N0), ProsB, dr(I,A,B)-lambda(X,M), [Rule])) :-
	RuleA = rule(_, ProsA, dr(I,A,B)-SemAB, _),
	!,
	N1 is N0 + 1,
	Hyp = rule(hyp(N0), '$VAR'(N0), B-X, []),
	Rule = rule(_, p(_, ProsB, _), A-M, _), 
        merge_proofs(rule(dr, p(I,ProsA,'$VAR'(N0)), A-appl(SemAB,X), [RuleA, Hyp]), RuleB, Pros0, Pros, GoalPros, N1, N, Rule).
merge_proofs(RuleA, RuleB, Pros0, p(0,'$VAR'(N0),Pros), GoalPros, N0, N, rule(dli(N0), ProsB, dl(I,B,A)-lambda(X,M), [Rule])) :-
	RuleA = rule(_, ProsA, dl(I,B,A)-SemBA, _),
	!,
	N1 is N0 + 1,
	Hyp = rule(hyp(N0), '$VAR'(N0), B-X, []),
	Rule = rule(_, p(_, _, ProsB), A-M, _),
	merge_proofs(rule(dl, p(I,'$VAR'(N0),ProsA), A-appl(SemBA,X), [Hyp, RuleA]), RuleB, Pros0, Pros, GoalPros, N1, N, Rule).

match_pros(X, X) :-
	!.
match_pros(p(0,X,'$VAR'(_)), Pros) :-
	!,
	match_pros(X, Pros).
match_pros(p(0,'$VAR'(_),X), Pros) :-
	!,
	match_pros(X, Pros).

match_pros_i(X, X) :-
	!.
match_pros_i(p(0,I,X), Y) :-
	match_interpunction_pros(I),
	!,
	match_pros_i(X, Y).

match_interpunction_pros(I) :-
	interpunction_pros(I),
	!.
match_interpunction_pros(p(0,I0,I1)) :-
	match_interpunction_pros(I0),
	match_interpunction_pros(I1).

interpunction_pros(',').
interpunction_pros('"').
interpunction_pros(':').
interpunction_pros('(').
interpunction_pros(')').
interpunction_pros('-').
interpunction_pros(';').

%	find_prod_w_start(Proof1, NB, VB, Adv0, PPros, ProofP, Proof2),

find_prod_w_start(rule(prod_wl, Pros, _, [Prem]),
		  N0, NB, VB, Adv, Pros, Prem,
		  rule(hyp(N0), '$VAR'(NB), Y-'$VAR'(VB), [])) :-
	Prem = rule(_, 	_, p(0,dl(1,_,_),dia(0,box(0,Y)))-Sem, _),		       
	Sem =@= Adv,
	!.
find_prod_w_start(rule(prod_w, Pros, _, [Prem]),
		  N0, NB, VB, Adv, Pros, Prem,
		  rule(hyp(N0), '$VAR'(NB), Y-'$VAR'(VB), [])) :-
	Prem = rule(_, _, p(0,Y,dl(1,_,_))-Sem, _),
	Sem =@= Adv,
	!.
find_prod_w_start(rule(Nm, Pros, F, Rs0), N0, NB, VB, Adv, PPros, ProofP, rule(Nm, Pros, F, Rs)) :-
	find_prod_w_start_list(Rs0, N0, NB, VB, Adv, PPros, ProofP, Rs).

find_prod_w_start_list([P0|Ps], N0, NB, VB, Adv, PPros, ProofP, [P|Ps]) :-
	find_prod_w_start(P0, N0, NB, VB, Adv, PPros, ProofP, P).
find_prod_w_start_list([P|Ps0], N0, NB, VB, Adv, PPros, ProofP, [P|Ps]) :-
	find_prod_w_start_list(Ps0, N0, NB, VB, Adv, PPros, ProofP, Ps).

%

find_w_start(rule(wr,RPros,_,[LProof,RProof]), Left, Pros, AdvF, Sem, RProof, LProof) :-
        match_pros(RPros, p(1,Left,Pros)),
	RProof = rule(_, Pros, AdvF-Sem0, _),
	equivalent_semantics(Sem0, Sem),
	!.
find_w_start(rule(wr_a,RPros,_,[LProof,RProof]), Left, Pros, AdvF, Sem, RProof, LProof) :-
        match_pros(RPros, p(1,Left,Pros)),
	RProof = rule(_, Pros, AdvF-Sem0, _),
	equivalent_semantics(Sem0, Sem),
	!.
find_w_start(rule(Nm, P, A, Ds0), Left, Pros, AdvF, Sem, AdvProof, rule(Nm, P, A, Ds)) :-
	find_w_start_list(Ds0, Left, Pros, AdvF, Sem, AdvProof, Ds),
	!.

find_w_start_list([P0|Ps], Left, Pros, AdvF, Sem, AdvProof, [P|Ps]) :-
	find_w_start(P0, Left, Pros, AdvF, Sem, AdvProof, P).
find_w_start_list([P|Ps0], Left, Pros, AdvF, Sem, AdvProof, [P|Ps]) :-
	find_w_start_list(Ps0, Left, Pros, AdvF, Sem, AdvProof, Ps).

find_e_start(rule(gap_c, Pros, A-Sem, [Proof,rule(_, Y, EF-_,_)]),
	     gap_c, X, EF, B, N, Pros,
	     rule(dr, Pros, A-Sem, [Proof,rule(hyp(N),'$VAR'(N),B-Var0,[])])) :-
	Sem = appl(_,Var0),
	match_pros_i(X, Y),
	!.
find_e_start(rule(ef_start, Pros, A-Sem, [rule(_, Y, EF-_, _), Proof]),
	     ef_start, X, EF, dr(0,A,B), N, Pros,
	     rule(dr, Pros, A-Sem, [rule(hyp(N),'$VAR'(N),dr(0,A,B)-Var0,[]),Proof])) :-
	Sem = appl(Var0, _),
	match_pros_i(X, Y),
	!.
find_e_start(rule(e_start, Pros, A-Sem, [rule(_, Y, EF-_, _), Proof]),
	     e_start, X, EF, B, N, Pros,
	     rule(dr, Pros, A-Sem, [Proof,rule(hyp(N),'$VAR'(N),B-Var0,[])])) :-
	Sem = appl(_, Var0),
	match_pros_i(X, Y),
	!.
find_e_start(rule(e_start_l, Pros, A-Sem, [Proof,rule(_, Y, EF-_, _)]),
	     e_start_l, X, EF, B, N, Pros,
	     rule(dr, Pros, A-Sem, [Proof,rule(hyp(N),'$VAR'(N),B-Var0,[])])) :- 
	Sem = appl(_, Var0),
	match_pros_i(X, Y),
%	format(user_error, '~N!!! e_start_l !!!~n', [TermC]),
	!.
find_e_start(rule(Nm, P, A, Ds0),
	     StartName, X, EF, B, N, Pros,
	     rule(Nm, P, A, Ds)) :-
%	Term = find_e_start(rule(Nm, P, A, Ds0),StartName, X, EF, B, N, Pros, _),
%	copy_term(Term, TermC),
%	numbervars(TermC, 0, _),
%	TermC = Term,
	rule_premisses(Nm, Ds0, Ds1, Ds2, Ds),
	find_e_start_list(Ds1, StartName, X, EF, B, N, Pros, Ds2),
	!.

find_e_start_list([P0|Ps], StartName, X, EF, B, N, Pros, [P|Ps]) :-
	find_e_start(P0, StartName, X, EF, B, N, Pros, P).
find_e_start_list([P|Ps0], StartName, X, EF, B, N, Pros, [P|Ps]) :-
	find_e_start_list(Ps0, StartName, X, EF, B, N, Pros, Ps).


%! rule_premisses(+RuleName, +PremissesIn, -AccessiblePremissesIn, -AccessiblePremissesOut, -PremissesOut)
%
% 

rule_premisses(e_start_l, [P1,P2], [P1], [P3], [P3,P2]) :-
	!.
rule_premisses(e_start,   [P1,P2], [P2], [P3], [P1,P3]) :-
	!.
rule_premisses(_, Ds, Ds, Es, Es).


rule_conclusion(rule(_, A, F-S, _), A, F, S).
rule_daughters(rule(_, _, _, Ds), Ds).

% = replace_proof(+InProof, +SubProof1, +SubProof2, -OutProof)
%
% true if OutProof is a copy of InProof where all occurrences of SubProof1 have been
% replace by SubProof2

replace_proof(ProofA, ProofA, ProofB, ProofB) :-
	!.
replace_proof(rule(Nm, P, F, Ds0), ProofA, ProofB, rule(Nm, P, F, Ds)) :-
	replace_proof_list(Ds0, ProofA, ProofB, Ds).

replace_proof_list([], _, _, []).
replace_proof_list([P0|Ps0], ProofA, ProofB, [P|Ps]) :-
	replace_proof(P0, ProofA, ProofB, P),
	replace_proof_list(Ps0, ProofA, ProofB, Ps).

% =

% = special case to prevent accidental capture of hypotheses
replace_proof_bag(rule(gap_e, Pros0, X-Sem, [A,B0]), Bag, Var, Proof, rule(gap_e, Pros, X-Sem, [A,B])) :-
	!,
	replace_bag(Pros0, Pros, Bag, Var),
	replace_proof_bag(B0, Bag, Var, Proof, B).

replace_proof_bag(rule(Nm0, Pros0, X-Sem, Rs0), Bag, Var, Proof, Result) :-
	bag_of_words(Pros0, Bag0),
   (
        Bag0 = Bag
   ->
        Result = Proof
   ;
	replace_bag(Pros0, Pros, Bag, Var),
        Result = rule(Nm, Pros, X-Sem, Rs),
        replace_proof_bag_list(Rs0, Bag, Var, Proof, Rs1),
	/* collapse superfluous unary branches */
	( Rs1 = [rule(Nm, Pros, _, Rs)] -> true ; Rs = Rs1, Nm = Nm0)
   ).

replace_proof_bag_list([], _, _, _, []).
replace_proof_bag_list([A|As], Bag, Var, Proof, Results) :-
	replace_proof_bag_list(As, A, Bag, Var, Proof, Results).

replace_proof_bag_list([], A0, Bag, Var, Proof, [A]) :-
	replace_proof_bag(A0, Bag, Var, Proof, A).
replace_proof_bag_list([B0], A0, Bag0, Var, Proof, Result) :-
	A0 = rule(_, ProsA, _, _),
	B0 = rule(_, ProsB, _, _),
	bag_of_words(ProsA, BagA),
	bag_of_words(ProsB, BagB),
   (
        /* the conclusion of the left subproof contains only words from the extracted element */   
        ord_subset(BagA, Bag0),
	BagA \= Bag0   
   ->
	/* forget left subproof and reduce bag */    
        ord_subtract(Bag0, BagA, Bag),
	Result = [B],
	replace_proof_bag(B0, Bag, Var, Proof, B)
   ;			      
        ord_subset(BagB, Bag0),
        BagB \= Bag0
   ->
	ord_subtract(Bag0, BagB, Bag),
	Result = [A],
	replace_proof_bag(A0, Bag, Var, Proof, A)
   ;			      
        ord_subset(Bag0, BagA)
   ->
	B = B0,
	Result = [A,B],
	replace_proof_bag(A0, Bag0, Var, Proof, A)
   ;
        ord_subset(Bag0, BagB)
   ->	     
	A = A0,
	Result = [A,B],
	replace_proof_bag(B0, Bag0, Var, Proof, B)
   ).


% = global_replace_pros(+InProof, +Pros1, +Pros2, -OutProof)
%
% true if OutProof is a copy of InProof where all occurrences of Pros1 have been
% replace by Pros2

global_replace_pros(rule(Nm, P0, F, Ds0), A, B, rule(Nm, P, F, Ds)) :-
	replace_pros(P0, A, B, P),
	global_replace_pros_list(Ds0, A, B, Ds).

global_replace_pros_list([], _, _, []).
global_replace_pros_list([P|Ps], A, B, [Q|Qs]) :-
	global_replace_pros(P, A, B, Q),
	global_replace_pros_list(Ps, A, B, Qs).


% = global_replace_pros(+InProof, +Pros1, +Pros2, +VarNo, -OutProof)
%
% true if OutProof is a copy of InProof where all occurrences of Pros1 have been
% replace by Pros2
% 
% Unlike global_replace_pros/4, the replacement takes place only for those labels
% occurring in proof nodes which are parents of the hypothesis label '$VAR'(VarNo).

global_replace_pros(rule(Nm, P0, F, [rule(Nm1, '$VAR'(N), F1, Ds), Right]), A, B, N,
			  rule(Nm, P, F,  [rule(Nm1, '$VAR'(N), F1, Ds), Right])) :-
	/* we have arrived at the end */
	!,
	replace_pros(P0, A, B, P).
global_replace_pros(rule(Nm, P0, F, [Left, rule(Nm1, '$VAR'(N), F1, Ds)]), A, B, N,
			  rule(Nm, P, F,  [Left, rule(Nm1, '$VAR'(N), F1, Ds)])) :-
	/* we have arrived at the end */
	!,
	replace_pros(P0, A, B, P).
global_replace_pros(rule(Nm, P0, F, Ds0), A, B, N, rule(Nm, P, F, Ds)) :-
	replace_pros(P0, A, B, P),
	global_replace_pros_list(Ds0, A, B, N, Ds).

global_replace_pros_list([], _, _, _, []).
global_replace_pros_list([P|Ps], A, B, N, [Q|Qs]) :-
	global_replace_pros(P, A, B, N, Q),
	global_replace_pros_list(Ps, A, B, N, Qs).


replace_pros(A, A, B, B) :-
	!.
replace_pros(p(I,A0,B0), C, D, p(I,A,B)) :-
	!,
	replace_pros(A0, C, D, A),
	replace_pros(B0, C, D, B).
replace_pros(A, _, _, A).

%

replace_bag(Pros0, Pros, Bag, R) :-
	replace_bag(Pros0, Pros1, Bag, [], R),
	cleanup_pros(Pros1, Pros).

replace_bag(p(I,A0,B0), p(I,A,B), Bag0, Bag, R) :-
	!,
	replace_bag(A0, A, Bag0, Bag1, R),
	replace_bag(B0, B, Bag1, Bag, R).
replace_bag(A, Result, Bag0, Bag, R) :-
	ord_select(A, Bag0, Bag),
	!,
	(Bag = [] -> Result = R ; Result = '$EMPTY').
replace_bag(B, B, Bag, Bag, _R).

cleanup_pros(p(I,X0,Y0), Z) :-
	!,
	cleanup_pros(X0, X),
   (
	 X == '$EMPTY'
   ->
        cleanup_pros(Y0, Z)
   ;
        cleanup_pros(Y0, Y),
   (
        Y == '$EMPTY'
   ->
        Z = X
   ;
        Z = p(I,X,Y)
   )).
cleanup_pros(X, X).



bag_of_words(p(_,A,B), Bag) :-
	!,
	bag_of_words(A, Bag0),
	bag_of_words(B, Bag1),
	ord_dup_union(Bag0, Bag1, Bag).
bag_of_words('$VAR'(_), []) :-
	!.
bag_of_words(X, [X]).
