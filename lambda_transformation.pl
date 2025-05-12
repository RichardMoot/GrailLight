:- use_module(sem_utils, [relabel_sem_vars/2,relabel_sem_vars/4,replace_sem/4,get_fresh_variable_number/2]).


start :-
	proof(N, Proof),
	format(user_error, '~w', [N]), 
	enrich_proof_term(Proof, Enriched0),
	transform_term(Enriched0, Enriched),
	format(user_error, '.', [N]), 
	format('semantics(~w, ~w).~n', [N,Enriched]),
	fail.
start.

transform_term(Term0, Term) :-
	/* 1 indicates at least on subterm has changes, so continue */
	transform_term(Term0, Term1, 1),
	!,
	format(user_error, '*******', []),
	transform_term(Term1, Term).
transform_term(Term, Term).       % no changes, so keep last term.
	
transform_term(Term, Transformed, 1) :-
	transform(Term, Transformed),
	!.
transform_term(X-'$VAR'(N), X-'$VAR'(N), 0).
transform_term(X-word(N,M), X-word(N,M), 0).
transform_term(X-word(N), X-word(N), 0).   % should not appear
transform_term(X-appl(M0,N0), X-appl(M,N), Bool) :-
	transform_term(M0, M, Bool0),
	transform_term(N0, N, Bool1),
	Bool is max(Bool0,Bool1).
transform_term(X-lambda(Y,M0), X-lambda(Y,M), Bool) :-
	transform_term(M0, M, Bool).


%add_formulas_to_term(rule(axiom, _,  Form-Term, _), Form-Term).
%add_formulas_to_term(rule(hyp(_), _, Form-Term, _), Form-Term).
%add_formulas_to_term(rule(Intro, _, Form-lambda(X,Term0), 

% treat "au contraire" as a single predicate

transform(X-appl(dr(0, X, lit(n))-word(W1, au), lit(n)-word(W2, contraire)), word(W1, X-au_contraire)) :-
	W2 is W1 + 1.

% take a proof and enrich the lambda term assigned to this proof so that each node in the
% tree has the associated formula information.

enrich_proof_term(Proof, Enriched) :-
	Proof = rule(_, _, Formula-Term,_),
	enrich_term(Term, Formula, Proof, Enriched).

conclusion_formula(rule(_, _, Formula-_, _), Formula).
conclusion_term(rule(_, _, _-Term,_), Term).
conclusion_formula_term(rule(_, _, Formula-Term,_), Formula, Term).
conclusion_subproofs(rule(_, _, _, SubProofs), SubProofs).
conclusion_pros(rule(_, Pros, _, _), Pros).

enrich_term('$VAR'(M), Formula, _, Formula-'$VAR'(M)).
enrich_term(word(M), Formula, Proof, Formula-word(M,Pros)) :-
	conclusion_pros(Proof, Pros).
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

beta_reduce(X, Y) :-
	beta_reduce(X, Y, 1),
	!.
beta_reduce(X, X).

beta_reduce(X, Y, 1) :-
	beta_reduce_step(X, Y),
	!.
beta_reduce(rule(Nm,Pros,Form,Proofs0), rule(Nm,Pros,Form,Proofs), N) :-
	beta_reduce_proof_list(Proofs0, Proofs, N).

beta_reduce_proof_list([], [], 0).
beta_reduce_proof_list([P|Ps], [Q|Qs], N) :-
	beta_reduce(P, Q, N0),
	beta_reduce_proof_list(Ps, Qs, N1),
	N is max(N0, N1).


beta_reduce_step(Proof0, Proof) :-
	Proof0 = rule(_, _, _-appl(lambda(X,M), N), [Sub1,Sub2]),
	trace,
	match_proof_semantics(N, Sub1, Sub2, Fun, _Arg),
	Fun = rule(_, _, _-lambda(X,M), [Proof1]),
	!,
	replace_proof(Proof1, X, N, Proof).
%beta_reduce_step(Proof, Proof, 0).

replace_proof(rule(_, _, _-X, _), X, N, N) :-
	!.
replace_proof(rule(N, P, A, Rs0), Var, Proof, rule(N, P, A, Rs)) :-
	replace_proof_list(Rs0, Var, Proof, Rs).

replace_proof_list([], _, _, []).
replace_proof_list([X|Xs], Var, Proof, [Y|Ys]) :-
	replace_proof(X, Var, Proof, Y),
	replace_proof_list(Xs, Var, Proof, Ys).

match_proof_semantics(N, Sub1, Sub2, Sub2, Sub1) :-
	conclusion_term(Sub2, M),
	is_alpha_equivalent(M, N),
	!.
match_proof_semantics(N, Sub1, Sub2, Sub1, Sub2) :-
	conclusion_term(Sub1, M),
	is_alpha_equivalent(M, N),
	!.

is_alpha_equivalent(Term0, Term) :-
	get_fresh_variable_number(Term0, V),
	get_fresh_variable_number(Term, W),
	Max is max(V,W),
	is_alpha_equivalent(Term0, Max, _, Term).

is_alpha_equivalent('$VAR'(N), V, V, '$VAR'(N)).
is_alpha_equivalent(word(N), V, V, word(N)).
is_alpha_equivalent(appl(N0,M0), V0, V, appl(N,M)) :-
	is_alpha_equivalent(N0, V0, V1, N),
	is_alpha_equivalent(M0, V1, V, M).
is_alpha_equivalent(lambda(X,M), V0, V, lambda(Y,P)) :-
	% if Y already exists in M, replace is by a fresh variable V0
	% then replace X by Y
	replace_sem(M, Y, V0, N0),
	replace_sem(N0, X, Y, N),
	V1 is V0 + 1,
	is_alpha_equivalent(N, V1, V, P).
	

% 5. Les jeunes garçons jouent dehors et l' homme sourit à proximité
 
proof(5, rule(dl, p(0,p(0,p(0,'Les',p(0,jeunes,garçons)),p(1,jouent,dehors)),p(0,et,p(0,p(0,'l\'',homme),p(1,sourit,p(0,à,proximité))))), lit(s(main))-appl(appl(word(9),appl(appl(word(13),word(14)),appl(word(12),appl(word(10),word(11))))),appl(lambda('$VAR'(0),appl(word(5),appl(word(3),'$VAR'(0)))),appl(word(0),appl(word(1),word(2))))), [
   rule(dl, p(0,p(0,'Les',p(0,jeunes,garçons)),p(1,jouent,dehors)), lit(s(main))-appl(lambda('$VAR'(0),appl(word(5),appl(word(3),'$VAR'(0)))),appl(word(0),appl(word(1),word(2)))), [
      rule(dr, p(0,'Les',p(0,jeunes,garçons)), lit(np(nom,_,_))-appl(word(0),appl(word(1),word(2))), [
         rule(axiom, 'Les', dr(0,lit(np(nom,_,_)),lit(n))-word(0), []),
         rule(dr, p(0,jeunes,garçons), lit(n)-appl(word(1),word(2)), [
            rule(axiom, jeunes, dr(0,lit(n),lit(n))-word(1), []),
            rule(axiom, garçons, lit(n)-word(2), [])
            ])
         ]),
      rule(dli(0), p(1,jouent,dehors), dl(0,lit(np(nom,_,_)),lit(s(main)))-lambda('$VAR'(4),appl(word(5),appl(word(3),'$VAR'(4)))), [
         rule(dl1, p(0,'$VAR'(0),p(1,jouent,dehors)), lit(s(main))-appl(word(5),appl(word(3),'$VAR'(4))), [
            rule(dl, p(0,'$VAR'(0),jouent), lit(s(main))-appl(word(3),'$VAR'(4)), [
               rule(hyp(0), '$VAR'(0), lit(np(nom,_,_))-'$VAR'(4), []),
               rule(axiom, jouent, dl(0,lit(np(nom,_,_)),lit(s(main)))-word(3), [])
               ]),
            rule(axiom, dehors, dl(1,lit(s(main)),lit(s(main)))-word(5), [])
            ])
         ])
      ]),
   rule(dr, p(0,et,p(0,p(0,'l\'',homme),p(1,sourit,p(0,à,proximité)))), dl(0,lit(s(main)),lit(s(main)))-appl(word(9),appl(lambda('$VAR'(0),appl(appl(word(13),word(14)),appl(word(12),'$VAR'(0)))),appl(word(10),word(11)))), [
      rule(axiom, et, dr(0,dl(0,lit(s(main)),lit(s(main))),lit(s(main)))-word(9), []),
      rule(dl, p(0,p(0,'l\'',homme),p(1,sourit,p(0,à,proximité))), lit(s(main))-appl(lambda('$VAR'(0),appl(appl(word(13),word(14)),appl(word(12),'$VAR'(0)))),appl(word(10),word(11))), [
         rule(dr, p(0,'l\'',homme), lit(np(nom,_,_))-appl(word(10),word(11)), [
            rule(axiom, 'l\'', dr(0,lit(np(nom,_,_)),lit(n))-word(10), []),
            rule(axiom, homme, lit(n)-word(11), [])
            ]),
         rule(dli(1), p(1,sourit,p(0,à,proximité)), dl(0,lit(np(nom,_,_)),lit(s(main)))-lambda('$VAR'(7),appl(appl(word(13),word(14)),appl(word(12),'$VAR'(7)))), [
            rule(dl1, p(0,'$VAR'(1),p(1,sourit,p(0,à,proximité))), lit(s(main))-appl(appl(word(13),word(14)),appl(word(12),'$VAR'(7))), [
               rule(dl, p(0,'$VAR'(1),sourit), lit(s(main))-appl(word(12),'$VAR'(7)), [
                  rule(hyp(1), '$VAR'(1), lit(np(nom,_,_))-'$VAR'(7), []),
                  rule(axiom, sourit, dl(0,lit(np(nom,_,_)),lit(s(main)))-word(12), [])
                  ]),
               rule(dr, p(0,à,proximité), dl(1,lit(s(main)),lit(s(main)))-appl(word(13),word(14)), [
                  rule(axiom, à, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(n))-word(13), []),
                  rule(axiom, proximité, lit(n)-word(14), [])
                  ])
               ])
            ])
         ])
      ])
   ])).
 


 

% 7. On veut au contraire l' y ramener . 

%% proof(7, rule(dl, p(0,p(0,'On',p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))),'.'), lit(txt)-appl(word(7),appl(lambda('$VAR'(7),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(7)))),word(0))), [
%%    rule(dl, p(0,'On',p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(lambda('$VAR'(7),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(7)))),word(0)), [
%%       rule(axiom, 'On', lit(np(nom,_,_))-word(0), []),
%%       rule(dli(0), p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener))), dl(0,lit(np(nom,_,_)),lit(s(main)))-lambda('$VAR'(8),appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8)))), [
%%          rule(dl1, p(0,'$VAR'(0),p(0,p(1,veut,p(0,au,contraire)),p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(appl(word(2),word(3)),appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8))), [
%%             rule(dl, p(0,'$VAR'(0),p(0,veut,p(0,'l\'',p(0,y,ramener)))), lit(s(main))-appl(appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))),'$VAR'(8)), [
%%                rule(hyp(0), '$VAR'(0), lit(np(nom,_,_))-'$VAR'(8), []),
%%                rule(dr, p(0,veut,p(0,'l\'',p(0,y,ramener))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(1),appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6))))))), [
%%                   rule(axiom, veut, dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(base)))))-word(1), []),
%%                   rule(dr, p(0,'l\'',p(0,y,ramener)), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(word(4),lambda('$VAR'(5),appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6)))))), [
%%                      rule(axiom, 'l\'', dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(np(acc,_,3-s))))))-word(4), []),
%%                      rule(drdiaboxi(1,1), p(0,y,ramener), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(np(acc,_,3-s)))))-true, [
%%                         rule(dr, p(0,y,p(0,ramener,'$VAR'(1))), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(word(5),lambda('$VAR'(6),appl(appl(word(6),'$VAR'(5)),'$VAR'(6)))), [
%%                            rule(axiom, y, dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(pp(à))))))-word(5), []),
%%                            rule(drdiaboxi(1,2), p(0,ramener,'$VAR'(1)), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dia(1,box(1,lit(pp(à)))))-true, [
%%                               rule(dr, p(0,p(0,ramener,'$VAR'(1)),'$VAR'(2)), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))-appl(appl(word(6),'$VAR'(5)),'$VAR'(6)), [
%%                                  rule(dr, p(0,ramener,'$VAR'(1)), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à)))-appl(word(6),'$VAR'(5)), [
%%                                     rule(axiom, ramener, dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(np(acc,_,3-s)))-word(6), []),
%%                                     rule(hyp(1), '$VAR'(1), lit(np(acc,_,3-s))-'$VAR'(5), [])
%%                                     ]),
%%                                  rule(hyp(2), '$VAR'(2), lit(pp(à))-'$VAR'(6), [])
%%                                  ])
%%                               ])
%%                            ])
%%                         ])
%%                      ])
%%                   ])
%%                ]),
%%             rule(dr, p(0,au,contraire), dl(1,lit(s(main)),lit(s(main)))-appl(word(2),word(3)), [
%%                rule(axiom, au, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(n))-word(2), []),
%%                rule(axiom, contraire, lit(n)-word(3), [])
%%                ])
%%             ])
%%          ])
%%       ]),
%%    rule(axiom, '.', dl(0,lit(s(main)),lit(txt))-word(7), [])
%%    ])).


% 11772. Il n' y a pas de jeune enfant qui éclabousse dans l' eau
 
proof(11772, rule(dl, p(0,'Il',p(0,'n\'',p(0,y,p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau)))))), lit(s(main))-appl(appl(word(1),appl(lambda('$VAR'(0),lambda('$VAR'(1),appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(appl(word(8),word(10)),appl(word(6),word(7)))))),'$VAR'(0)),'$VAR'(1))))),word(2))),word(0)), [
   rule(axiom, 'Il', lit(np(nom,_,_))-word(0), []),
   rule(dr, p(0,'n\'',p(0,y,p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau))))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(1),appl(lambda('$VAR'(0),lambda('$VAR'(1),appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(appl(word(8),word(10)),appl(word(6),word(7)))))),'$VAR'(0)),'$VAR'(1))))),word(2))), [
      rule(axiom, 'n\'', dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(main))))-word(1), []),
      rule(dl, p(0,y,p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau)))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(lambda('$VAR'(0),lambda('$VAR'(1),appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(appl(word(8),word(10)),appl(word(6),word(7)))))),'$VAR'(0)),'$VAR'(1))))),word(2)), [
         rule(axiom, y, lit(cl_y)-word(2), []),
         rule(dli(0), p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau))), dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main))))-lambda('$VAR'(4),lambda('$VAR'(5),appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))),'$VAR'(4)),'$VAR'(5))))), [
            rule(dli(1), p(0,'$VAR'(1),p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau)))), dl(0,lit(np(nom,_,_)),lit(s(main)))-lambda('$VAR'(5),appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))),'$VAR'(4)),'$VAR'(5)))), [
               rule(dl1, p(0,'$VAR'(0),p(0,'$VAR'(1),p(1,p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))),p(0,dans,p(0,'l\'',eau))))), lit(s(main))-appl(appl(word(12),appl(word(14),word(15))),appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))),'$VAR'(4)),'$VAR'(5))), [
                  rule(dl, p(0,'$VAR'(1),p(0,'$VAR'(0),p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))))), lit(s(main))-appl(appl(appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))),'$VAR'(4)),'$VAR'(5)), [
                     rule(hyp(1), '$VAR'(1), lit(np(nom,_,_))-'$VAR'(5), []),
                     rule(dl, p(0,'$VAR'(0),p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse))))))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))),'$VAR'(4)), [
                        rule(hyp(0), '$VAR'(0), lit(cl_y)-'$VAR'(4), []),
                        rule(dr, p(0,a,p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))))), dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main))))-appl(word(3),appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))))), [
                           rule(axiom, a, dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(np(acc,_,_)))-word(3), []),
                           rule(dr, p(0,pas,p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse))))), lit(np(acc,_,_))-appl(word(4),appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7))))), [
                              rule(axiom, pas, dr(0,lit(np(acc,_,_)),lit(pp(de)))-word(4), []),
                              rule(dr, p(0,de,p(0,jeune,p(0,enfant,p(0,qui,éclabousse)))), lit(pp(de))-appl(word(5),appl(word(6),appl(appl(word(8),word(10)),word(7)))), [
                                 rule(axiom, de, dr(0,lit(pp(de)),lit(n))-word(5), []),
                                 rule(dr, p(0,jeune,p(0,enfant,p(0,qui,éclabousse))), lit(n)-appl(word(6),appl(appl(word(8),word(10)),word(7))), [
                                    rule(axiom, jeune, dr(0,lit(n),lit(n))-word(6), []),
                                    rule(dl, p(0,enfant,p(0,qui,éclabousse)), lit(n)-appl(appl(word(8),word(10)),word(7)), [
                                       rule(axiom, enfant, lit(n)-word(7), []),
                                       rule(dr, p(0,qui,éclabousse), dl(0,lit(n),lit(n))-appl(word(8),word(10)), [
                                          rule(axiom, qui, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(nom,_,_)),lit(s(main))))-word(8), []),
                                          rule(axiom, éclabousse, dl(0,lit(np(nom,_,_)),lit(s(main)))-word(10), [])
                                          ])
                                       ])
                                    ])
                                 ])
                              ])
                           ])
                        ])
                     ]),
                  rule(dr, p(0,dans,p(0,'l\'',eau)), dl(1,lit(s(main)),lit(s(main)))-appl(word(12),appl(word(14),word(15))), [
                     rule(axiom, dans, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(np(acc,_,_)))-word(12), []),
                     rule(dr, p(0,'l\'',eau), lit(np(acc,_,_))-appl(word(14),word(15)), [
                        rule(axiom, 'l\'', dr(0,lit(np(acc,_,_)),lit(n))-word(14), []),
                        rule(axiom, eau, lit(n)-word(15), [])
                        ])
                     ])
                  ])
               ])
            ])
         ])
      ])
   ])).
