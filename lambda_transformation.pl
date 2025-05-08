
start(Enriched) :-
	proof(_, Proof),
	enrich_proof_term(Proof, Enriched).


enrich_proof_term(Proof, Enriched) :-
	Proof = rule(_, _, Formula-Term,_),
	enrich_term(Term, Formula, Proof, Enriched).

conclusion_formula(rule(_, _, Formula-_, _), Formula).
conclusion_term(rule(_, _, -Term,_), Term).
conclusion_formula_term(rule(_, _, Formula-Term,_), Formula, Term).
conclusion_subproofs(rule(_, _, _, SubProofs), SubProofs).

enrich_term('$VAR'(M), Formula, _, Formula-'$VAR'(M)).
enrich_term(word(M), Formula, _, Formula-word(M)).
enrich_term(lambda(X,M), Formula, Proof, Formula-lambda(X,Enriched)) :-
	conclusion_subproofs(Proof, [Proof0]),
	conclusion_formula(Proof0, Formula0),
	enrich_term(M, Formula0, Proof0, Enriched).
enrich_term(appl(M,N), Formula, Proof, Formula-appl(Enriched1,Enriched2)) :-
	match_application(Formula, Formula1, Formula2, Proof, Proof1, Proof2),
	enrich_term(M,  Formula1, Proof1, Enriched1),
	enrich_term(N, Formula2, Proof2, Enriched2).

match_application(C, A, B, Proof, ProofFun, ProofArg) :-
	conclusion_subproofs(Proof, [Proof1, Proof2]),
	conclusion_formula(Proof1, D),
	conclusion_formula(Proof2, E),
	mmcg_to_ll(C, CS),
	mmcg_to_ll(D, DS),
	mmcg_to_ll(E, Es),
	match(CS, DS, Es, D, E, A, B, Proof1, Proof2, ProofFun, ProofArg).

match(B, impl(A,B), A, C, D, C, D, ProofFun, ProofArg, ProofFun, ProofArg).
match(B, A, impl(A,B), C, D, D, C, ProofArg, ProofFun, ProofFun, ProofArg).

% adapted from grail light
% keeps "lit" wrapper around atoms

simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(_)), lit(s)) :- !.
simplify_formula(lit(pp(_)), lit(pp)) :- !.
simplify_formula(lit(np(_,_,_)), lit(np)) :- !.
simplify_formula(lit(A), lit(A)).
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(_,A0), dia(1,A)) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), box(1,A)) :-
	simplify_formula(A0, A).

% TLG to linear logic

mmcg_to_ll(lit(s(_)), atom(s)) :-
	!.
mmcg_to_ll(lit(pp(_)), atom(pp)) :-
	!.
mmcg_to_ll(lit(np(_,_,_)), atom(np)) :-
	!.
mmcg_to_ll(lit(A), atom(A)).
mmcg_to_ll(dl(_,A0,B0), impl(A,B)) :-
	mmcg_to_ll(A0, A),
	mmcg_to_ll(B0, B).
mmcg_to_ll(dr(_,B0,A0), impl(A,B)) :-
	mmcg_to_ll(A0, A),
	mmcg_to_ll(B0, B).
mmcg_to_ll(p(_,A0,B0), prod(A,B)) :-
	mmcg_to_ll(A0, A),
	mmcg_to_ll(B0, B).
mmcg_to_ll(dia(_,A0), A) :-
	mmcg_to_ll(A0, A).
mmcg_to_ll(box(_,A0), A) :-
	mmcg_to_ll(A0, A).


mmcg_proof_to_ll_proof(rule(Name0, _, Form0-Term, SubProofs0), rule(Name, Form-Term, SubProofs)) :-
	translate_name(Name0, Name),
	mmcg_to_ll(Form0, Form),
	mmcg_prooflist_to_ll_prooflist(SubProofs0, SubProofs).


translate_name(axiom, axiom).
translate_name(dl, impl_e).
translate_name(dl1, impl_e).
translate_name(dr, impl_e).
translate_name(dli(_), impl_i).
			   
% Example sentence	

% 7. On veut au contraire l' y ramener . 

proof(7, rule(dl, p(0,p(0,'On',p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))),'.'), lit(txt)-appl(word(7),appl(lambda('$VAR'(7),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(7)))),word(0))), [
   rule(dl, p(0,'On',p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(lambda('$VAR'(7),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(7)))),word(0)), [
      rule(axiom, 'On', lit(np(nom,_,_))-word(0), []),
      rule(dli(0), p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener))), dl(0,lit(np(nom,_,_)),lit(s(main)))-lambda('$VAR'(8),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8)))), [
         rule(dl1, p(0,'$VAR'(0),p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8))), [
            rule(dl, p(0,'$VAR'(0),p(0,veut,p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8)), [
               rule(hyp(0), '$VAR'(0), lit(np(nom,_,_))-'$VAR'(8), []),
               rule(dr, p(0,veut,p(0,'l\'',p(0,y,ramener))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))), [
                  rule(axiom, veut, dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(base)))))-word(1), []),
                  rule(dr, p(0,'l\'',p(0,y,ramener)), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6)))))), [
                     rule(axiom, 'l\'', dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(np(acc,_,3-s))))))-word(4), []),
                     rule(drdiaboxi(1,1), p(0,y,ramener), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(np(acc,_,3-s)))))-true, [
                        rule(dr, p(0,y,p(0,ramener,'$VAR'(1))), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6)))), [
                           rule(axiom, y, dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(pp(à))))))-word(5), []),
                           rule(drdiaboxi(1,2), p(0,ramener,'$VAR'(1)), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(pp(à)))))-true, [
                              rule(dr, p(0,p(0,ramener,'$VAR'(1)),'$VAR'(2)), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(appl(word(6),'$VAR'(5)),'$VAR'(6)), [
                                 rule(dr, p(0,ramener,'$VAR'(1)), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à)))-appl(word(6),'$VAR'(5)), [
                                    rule(axiom, ramener, dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(np(acc,_,3-s)))-word(6), []),
                                    rule(hyp(1), '$VAR'(1), lit(np(acc,_,3-s))-'$VAR'(5), [])
                                    ]),
                                 rule(hyp(2), '$VAR'(2), lit(pp(à))-'$VAR'(6), [])
                                 ])
                              ])
                           ])
                        ])
                     ])
                  ])
               ]),
            rule(dr, p(0,au,contraire), dl(1,lit(s(main)),lit(s(main)))-appl(word(2),word(3)), [
               rule(axiom, au, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(n))-word(2), []),
               rule(axiom, contraire, lit(n)-word(3), [])
               ])
            ])
         ])
      ]),
   rule(axiom, '.', dl(0,lit(s(main)),lit(txt))-word(7), [])
   ])).
