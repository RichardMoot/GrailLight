
add_variables(lit(A), lit(A,_)).
add_variables(dia(I,A0), dia(I, A)) :-
    add_variables(A0, A).
add_variables(box(I,A0), box(I, A)) :-
    add_variables(A0, A).
add_variables(dr(I,A0,B0), dr(I,A,B)) :-
    add_variables(A0, A),
    add_variables(B0, B).
add_variables(dl(I,A0,B0), dl(I,A,B)) :-
    add_variables(A0, A),
    add_variables(B0, B).
add_variables(p(I,A0,B0), p(I,A,B)) :-
    add_variables(A0, A),
    add_variables(B0, B).


nd_to_matching(rule(RN, _, F0-W,[]), F, Lex) :-
    is_axiom(RN, F, W, Lex),
    !,
    add_variables(F0, F).
nd_to_matching(rule(RN, _, _-_, [R1,R2]), F, Lex) :-
    is_elim(RN),
    !,
    nd_to_matching(R1, F1, Lex1),
    nd_to_matching(R2, F2, Lex2),
    append(Lex1, Lex2, Lex),
    match_elim(F1, F2, F).
nd_to_matching(rule(RN, _, F0-_, [R]), F, Lex) :-
    add_variables(F0, F),
    is_intro(RN, Index, F, B, A),
    !,
    nd_to_matching(R, B, Lex),
    select_hyp(R, Index, A).


select_hyp(rule(hyp(I),_,A0-_,[]), I, A) :-
    add_variables(A0, A),
    !.
select_hyp(rule(_,_,_,Rs), I, A) :-
    member(R, Rs),
    select_hyp(R, I, A).
    

match_elim(dr(_,A,B), B, A).
match_elim(B, dl(_,B,A), A).

is_axiom(hyp(_), _, _, []).
is_axiom(axiom, F, W, [W-F]).

is_elim(dr).
is_elim(dl).
is_elim(dl1).

is_intro(dri(I), I, dr(_,B,A), B, A).
is_intro(dli(I), I, dl(_,A,B), B, A).



% 59. Selon la Banque centrale , cent millions de billets ont déjà été imprimés . 

proof(59, rule(dl, p(0,p(0,p(0,'Selon',p(0,la,p(0,'Banque',centrale))),p(0,p(0,p(0,p(0,',',cent),millions),p(0,de,billets)),p(0,ont,p(0,déjà,p(0,été,imprimés))))),'.'), lit(txt)-appl(word(13),appl(appl(word(0),appl(word(1),appl(word(3),word(2)))),appl(appl(word(9),appl(word(10),appl(word(11),word(12)))),appl(appl(word(7),word(8)),appl(word(5),word(6)))))), [
   rule(dr, p(0,p(0,'Selon',p(0,la,p(0,'Banque',centrale))),p(0,p(0,p(0,p(0,',',cent),millions),p(0,de,billets)),p(0,ont,p(0,déjà,p(0,été,imprimés))))), lit(s(main))-appl(appl(word(0),appl(word(1),appl(word(3),word(2)))),appl(appl(word(9),appl(word(10),appl(word(11),word(12)))),appl(appl(word(7),word(8)),appl(word(5),word(6))))), [
      rule(dr, p(0,'Selon',p(0,la,p(0,'Banque',centrale))), dr(0,lit(s(main)),lit(s(main)))-appl(word(0),appl(word(1),appl(word(3),word(2)))), [
         rule(axiom, 'Selon', dr(0,dr(0,lit(s(main)),lit(s(main))),lit(np(acc,_,_)))-word(0), []),
         rule(dr, p(0,la,p(0,'Banque',centrale)), lit(np(acc,_,_))-appl(word(1),appl(word(3),word(2))), [
            rule(axiom, la, dr(0,lit(np(acc,_,_)),lit(n))-word(1), []),
            rule(dl, p(0,'Banque',centrale), lit(n)-appl(word(3),word(2)), [
               rule(axiom, 'Banque', lit(n)-word(2), []),
               rule(axiom, centrale, dl(0,lit(n),lit(n))-word(3), [])
               ])
            ])
         ]),
      rule(dl, p(0,p(0,p(0,p(0,',',cent),millions),p(0,de,billets)),p(0,ont,p(0,déjà,p(0,été,imprimés)))), lit(s(main))-appl(appl(word(9),appl(word(10),appl(word(11),word(12)))),appl(appl(word(7),word(8)),appl(word(5),word(6)))), [
         rule(dl, p(0,p(0,p(0,',',cent),millions),p(0,de,billets)), lit(np(nom,_,_))-appl(appl(word(7),word(8)),appl(word(5),word(6))), [
            rule(dr, p(0,p(0,',',cent),millions), lit(np(_,_,_))-appl(word(5),word(6)), [
               rule(axiom, p(0,',',cent), dr(0,lit(np(_,_,_)),lit(n))-word(5), []),
               rule(axiom, millions, lit(n)-word(6), [])
               ]),
            rule(dr, p(0,de,billets), dl(0,lit(np(_,_,_)),lit(np(nom,_,_)))-appl(word(7),word(8)), [
               rule(axiom, de, dr(0,dl(0,lit(np(_,_,_)),lit(np(nom,_,_))),lit(n))-word(7), []),
               rule(axiom, billets, lit(n)-word(8), [])
               ])
            ]),
         rule(dr, p(0,ont,p(0,déjà,p(0,été,imprimés))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(9),appl(word(10),appl(word(11),word(12)))), [
            rule(axiom, ont, dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(ppart))))-word(9), []),
            rule(dr, p(0,déjà,p(0,été,imprimés)), dl(0,lit(np(nom,_,_)),lit(s(ppart)))-appl(word(10),appl(word(11),word(12))), [
               rule(axiom, déjà, dr(0,dl(0,lit(np(nom,_,_)),lit(s(ppart))),dl(0,lit(np(nom,_,_)),lit(s(ppart))))-word(10), []),
               rule(dr, p(0,été,imprimés), dl(0,lit(np(nom,_,_)),lit(s(ppart)))-appl(word(11),word(12)), [
                  rule(axiom, été, dr(0,dl(0,lit(np(nom,_,_)),lit(s(ppart))),dl(0,lit(np(_,_,_)),lit(s(pass))))-word(11), []),
                  rule(axiom, imprimés, dl(0,lit(np(_,_,_)),lit(s(pass)))-word(12), [])
                  ])
               ])
            ])
         ])
      ]),
   rule(axiom, '.', dl(0,lit(s(main)),lit(txt))-word(13), [])
   ])).

% 60. Le papier utilisé provient du Brésil alors que les pièces de monnaie sont frappées au Chili . 

proof(60, rule(dl, p(0,p(1,p(0,p(0,'Le',p(0,papier,utilisé)),p(0,provient,p(0,du,'Brésil'))),p(0,alors,p(0,que,p(1,p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)),p(0,au,'Chili'))))),'.'), lit(txt)-appl(word(16),appl(appl(word(6),appl(word(7),appl(appl(word(14),word(15)),appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9))))))),appl(appl(word(3),appl(word(4),word(5))),appl(word(0),appl(word(2),word(1)))))), [
   rule(dl, p(1,p(0,p(0,'Le',p(0,papier,utilisé)),p(0,provient,p(0,du,'Brésil'))),p(0,alors,p(0,que,p(1,p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)),p(0,au,'Chili'))))), lit(s(main))-appl(appl(word(6),appl(word(7),appl(appl(word(14),word(15)),appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9))))))),appl(appl(word(3),appl(word(4),word(5))),appl(word(0),appl(word(2),word(1))))), [
      rule(dl, p(0,p(0,'Le',p(0,papier,utilisé)),p(0,provient,p(0,du,'Brésil'))), lit(s(main))-appl(appl(word(3),appl(word(4),word(5))),appl(word(0),appl(word(2),word(1)))), [
         rule(dr, p(0,'Le',p(0,papier,utilisé)), lit(np(nom,_,_))-appl(word(0),appl(word(2),word(1))), [
            rule(axiom, 'Le', dr(0,lit(np(nom,_,_)),lit(n))-word(0), []),
            rule(dl, p(0,papier,utilisé), lit(n)-appl(word(2),word(1)), [
               rule(axiom, papier, lit(n)-word(1), []),
               rule(axiom, utilisé, dl(0,lit(n),lit(n))-word(2), [])
               ])
            ]),
         rule(dr, p(0,provient,p(0,du,'Brésil')), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(3),appl(word(4),word(5))), [
            rule(axiom, provient, dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(de)))-word(3), []),
            rule(dr, p(0,du,'Brésil'), lit(pp(de))-appl(word(4),word(5)), [
               rule(axiom, du, dr(0,lit(pp(de)),lit(n))-word(4), []),
               rule(axiom, 'Brésil', lit(n)-word(5), [])
               ])
            ])
         ]),
      rule(dr, p(0,alors,p(0,que,p(1,p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)),p(0,au,'Chili')))), dl(1,lit(s(main)),lit(s(main)))-appl(word(6),appl(word(7),appl(appl(word(14),word(15)),appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9))))))), [
         rule(axiom, alors, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(s(q)))-word(6), []),
         rule(dr, p(0,que,p(1,p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)),p(0,au,'Chili'))), lit(s(q))-appl(word(7),appl(appl(word(14),word(15)),appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9)))))), [
            rule(axiom, que, dr(0,lit(s(q)),lit(s(main)))-word(7), []),
            rule(dl, p(1,p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)),p(0,au,'Chili')), lit(s(main))-appl(appl(word(14),word(15)),appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9))))), [
               rule(dl, p(0,p(0,les,p(0,pièces,p(0,de,monnaie))),p(0,sont,frappées)), lit(s(main))-appl(appl(word(12),word(13)),appl(word(8),appl(appl(word(10),word(11)),word(9)))), [
                  rule(dr, p(0,les,p(0,pièces,p(0,de,monnaie))), lit(np(nom,_,_))-appl(word(8),appl(appl(word(10),word(11)),word(9))), [
                     rule(axiom, les, dr(0,lit(np(nom,_,_)),lit(n))-word(8), []),
                     rule(dl, p(0,pièces,p(0,de,monnaie)), lit(n)-appl(appl(word(10),word(11)),word(9)), [
                        rule(axiom, pièces, lit(n)-word(9), []),
                        rule(dr, p(0,de,monnaie), dl(0,lit(n),lit(n))-appl(word(10),word(11)), [
                           rule(axiom, de, dr(0,dl(0,lit(n),lit(n)),lit(n))-word(10), []),
                           rule(axiom, monnaie, lit(n)-word(11), [])
                           ])
                        ])
                     ]),
                  rule(dr, p(0,sont,frappées), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(12),word(13)), [
                     rule(axiom, sont, dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(pass))))-word(12), []),
                     rule(axiom, frappées, dl(0,lit(np(nom,_,_)),lit(s(pass)))-word(13), [])
                     ])
                  ]),
               rule(dr, p(0,au,'Chili'), dl(1,lit(s(main)),lit(s(main)))-appl(word(14),word(15)), [
                  rule(axiom, au, dr(0,dl(1,lit(s(main)),lit(s(main))),lit(n))-word(14), []),
                  rule(axiom, 'Chili', lit(n)-word(15), [])
                  ])
               ])
            ])
         ])
      ]),
   rule(axiom, '.', dl(0,lit(s(main)),lit(txt))-word(16), [])
   ])).

% 83. Le groupe allemand VAG ( Volkswagen ) , numéro un européen de l' automobile , a annoncé , lundi 30 décembre , un volume record de 3,13 millions de véhicules livrés dans le monde en 1991 , en hausse de 2,5 % par rapport à l' année précédente . 

proof(83, rule(dl, p(0,p(0,p(0,p(0,'Le',p(0,p(0,p(0,groupe,allemand),'VAG'),p(0,'(','Volkswagen'))),p(0,p(0,')',','),p(0,p(0,p(0,numéro,un),européen),p(0,de,p(0,'l\'',automobile))))),p(0,p(0,',',a),p(0,p(1,annoncé,p(0,p(0,',',lundi),p(0,30,décembre))),p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))))))))),'.'), lit(txt)-appl(word(48),appl(appl(word(15),lambda('$VAR'(0),appl(appl(word(18),appl(word(19),word(20))),appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(0))))),appl(appl(word(7),appl(appl(word(11),appl(word(12),word(13))),appl(word(10),appl(word(9),word(8))))),appl(word(0),appl(appl(word(4),word(5)),appl(word(3),appl(word(2),word(1)))))))), [
   rule(dl, p(0,p(0,p(0,'Le',p(0,p(0,p(0,groupe,allemand),'VAG'),p(0,'(','Volkswagen'))),p(0,p(0,')',','),p(0,p(0,p(0,numéro,un),européen),p(0,de,p(0,'l\'',automobile))))),p(0,p(0,',',a),p(0,p(1,annoncé,p(0,p(0,',',lundi),p(0,30,décembre))),p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))))))))), lit(s(main))-appl(appl(word(15),lambda('$VAR'(0),appl(appl(word(18),appl(word(19),word(20))),appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(0))))),appl(appl(word(7),appl(appl(word(11),appl(word(12),word(13))),appl(word(10),appl(word(9),word(8))))),appl(word(0),appl(appl(word(4),word(5)),appl(word(3),appl(word(2),word(1))))))), [
      rule(dl, p(0,p(0,'Le',p(0,p(0,p(0,groupe,allemand),'VAG'),p(0,'(','Volkswagen'))),p(0,p(0,')',','),p(0,p(0,p(0,numéro,un),européen),p(0,de,p(0,'l\'',automobile))))), lit(np(nom,_,_))-appl(appl(word(7),appl(appl(word(11),appl(word(12),word(13))),appl(word(10),appl(word(9),word(8))))),appl(word(0),appl(appl(word(4),word(5)),appl(word(3),appl(word(2),word(1)))))), [
         rule(dr, p(0,'Le',p(0,p(0,p(0,groupe,allemand),'VAG'),p(0,'(','Volkswagen'))), lit(np(_,_,_))-appl(word(0),appl(appl(word(4),word(5)),appl(word(3),appl(word(2),word(1))))), [
            rule(axiom, 'Le', dr(0,lit(np(_,_,_)),lit(n))-word(0), []),
            rule(dl, p(0,p(0,p(0,groupe,allemand),'VAG'),p(0,'(','Volkswagen')), lit(n)-appl(appl(word(4),word(5)),appl(word(3),appl(word(2),word(1)))), [
               rule(dl, p(0,p(0,groupe,allemand),'VAG'), lit(n)-appl(word(3),appl(word(2),word(1))), [
                  rule(dl, p(0,groupe,allemand), lit(n)-appl(word(2),word(1)), [
                     rule(axiom, groupe, lit(n)-word(1), []),
                     rule(axiom, allemand, dl(0,lit(n),lit(n))-word(2), [])
                     ]),
                  rule(axiom, 'VAG', dl(0,lit(n),lit(n))-word(3), [])
                  ]),
               rule(dr, p(0,'(','Volkswagen'), dl(0,lit(n),lit(n))-appl(word(4),word(5)), [
                  rule(axiom, '(', dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))-word(4), []),
                  rule(axiom, 'Volkswagen', lit(np(_,_,_))-word(5), [])
                  ])
               ])
            ]),
         rule(dr, p(0,p(0,')',','),p(0,p(0,p(0,numéro,un),européen),p(0,de,p(0,'l\'',automobile)))), dl(0,lit(np(_,_,_)),lit(np(nom,_,_)))-appl(word(7),appl(appl(word(11),appl(word(12),word(13))),appl(word(10),appl(word(9),word(8))))), [
            rule(axiom, p(0,')',','), dr(0,dl(0,lit(np(_,_,_)),lit(np(nom,_,_))),lit(n))-word(7), []),
            rule(dl, p(0,p(0,p(0,numéro,un),européen),p(0,de,p(0,'l\'',automobile))), lit(n)-appl(appl(word(11),appl(word(12),word(13))),appl(word(10),appl(word(9),word(8)))), [
               rule(dl, p(0,p(0,numéro,un),européen), lit(n)-appl(word(10),appl(word(9),word(8))), [
                  rule(dl, p(0,numéro,un), lit(n)-appl(word(9),word(8)), [
                     rule(axiom, numéro, lit(n)-word(8), []),
                     rule(axiom, un, dl(0,lit(n),lit(n))-word(9), [])
                     ]),
                  rule(axiom, européen, dl(0,lit(n),lit(n))-word(10), [])
                  ]),
               rule(dr, p(0,de,p(0,'l\'',automobile)), dl(0,lit(n),lit(n))-appl(word(11),appl(word(12),word(13))), [
                  rule(axiom, de, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))-word(11), []),
                  rule(dr, p(0,'l\'',automobile), lit(np(_,_,_))-appl(word(12),word(13)), [
                     rule(axiom, 'l\'', dr(0,lit(np(_,_,_)),lit(n))-word(12), []),
                     rule(axiom, automobile, lit(n)-word(13), [])
                     ])
                  ])
               ])
            ])
         ]),
      rule(dr, p(0,p(0,',',a),p(0,p(1,annoncé,p(0,p(0,',',lundi),p(0,30,décembre))),p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))))))))), dl(0,lit(np(nom,_,_)),lit(s(main)))-appl(word(15),lambda('$VAR'(0),appl(appl(word(18),appl(word(19),word(20))),appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(0))))), [
         rule(axiom, p(0,',',a), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(ppart))))-word(15), []),
         rule(dli(0), p(0,p(1,annoncé,p(0,p(0,',',lundi),p(0,30,décembre))),p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))))))), dl(0,lit(np(nom,_,_)),lit(s(ppart)))-lambda('$VAR'(1),appl(appl(word(18),appl(word(19),word(20))),appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(1)))), [
            rule(dl1, p(0,'$VAR'(0),p(0,p(1,annoncé,p(0,p(0,',',lundi),p(0,30,décembre))),p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))))))))), lit(s(ppart))-appl(appl(word(18),appl(word(19),word(20))),appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(1))), [
               rule(dl, p(0,'$VAR'(0),p(0,annoncé,p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))))))))), lit(s(ppart))-appl(appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))),'$VAR'(1)), [
                  rule(hyp(0), '$VAR'(0), lit(np(nom,_,_))-'$VAR'(1), []),
                  rule(dr, p(0,annoncé,p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))))))), dl(0,lit(np(nom,_,_)),lit(s(ppart)))-appl(word(16),appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))))), [
                     rule(axiom, annoncé, dr(0,dl(0,lit(np(nom,_,_)),lit(s(ppart))),lit(np(_,_,_)))-word(16), []),
                     rule(dr, p(0,p(0,',',un),p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))))))), lit(np(_,_,_))-appl(word(22),appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23))))), [
                        rule(axiom, p(0,',',un), dr(0,lit(np(_,_,_)),lit(n))-word(22), []),
                        rule(dl, p(0,p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))),p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))))), lit(n)-appl(appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))),appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23)))), [
                           rule(dl, p(0,p(0,volume,record),p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))))), lit(n)-appl(appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))),appl(word(24),word(23))), [
                              rule(dl, p(0,volume,record), lit(n)-appl(word(24),word(23)), [
                                 rule(axiom, volume, lit(n)-word(23), []),
                                 rule(axiom, record, dl(0,lit(n),lit(n))-word(24), [])
                                 ]),
                              rule(dr, p(0,de,p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991)))))), dl(0,lit(n),lit(n))-appl(word(25),appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27)))), [
                                 rule(axiom, de, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))-word(25), []),
                                 rule(dl, p(0,p(0,'3,13',millions),p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))))), lit(np(_,_,_))-appl(appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))),appl(word(26),word(27))), [
                                    rule(dr, p(0,'3,13',millions), lit(np(_,_,_))-appl(word(26),word(27)), [
                                       rule(axiom, '3,13', dr(0,lit(np(_,_,_)),lit(n))-word(26), []),
                                       rule(axiom, millions, lit(n)-word(27), [])
                                       ]),
                                    rule(dr, p(0,de,p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991)))), dl(0,lit(np(_,_,_)),lit(np(_,_,_)))-appl(word(28),appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29))), [
                                       rule(axiom, de, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n))-word(28), []),
                                       rule(dl, p(0,véhicules,p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991))), lit(n)-appl(appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))),word(29)), [
                                          rule(axiom, véhicules, lit(n)-word(29), []),
                                          rule(dl, p(1,p(1,livrés,p(0,dans,p(0,le,monde))),p(0,en,1991)), dl(0,lit(n),lit(n))-appl(appl(word(34),word(35)),appl(appl(word(31),appl(word(32),word(33))),word(30))), [
                                             rule(dl, p(1,livrés,p(0,dans,p(0,le,monde))), dl(0,lit(n),lit(n))-appl(appl(word(31),appl(word(32),word(33))),word(30)), [
                                                rule(axiom, livrés, dl(0,lit(n),lit(n))-word(30), []),
                                                rule(dr, p(0,dans,p(0,le,monde)), dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-appl(word(31),appl(word(32),word(33))), [
                                                   rule(axiom, dans, dr(0,dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),lit(np(_,_,_)))-word(31), []),
                                                   rule(dr, p(0,le,monde), lit(np(_,_,_))-appl(word(32),word(33)), [
                                                      rule(axiom, le, dr(0,lit(np(_,_,_)),lit(n))-word(32), []),
                                                      rule(axiom, monde, lit(n)-word(33), [])
                                                      ])
                                                   ])
                                                ]),
                                             rule(dr, p(0,en,1991), dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-appl(word(34),word(35)), [
                                                rule(axiom, en, dr(0,dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),lit(np(_,_,_)))-word(34), []),
                                                rule(axiom, 1991, lit(np(_,_,_))-word(35), [])
                                                ])
                                             ])
                                          ])
                                       ])
                                    ])
                                 ])
                              ]),
                           rule(dr, p(0,p(0,',',en),p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))))), dl(0,lit(n),lit(n))-appl(word(37),appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38))), [
                              rule(axiom, p(0,',',en), dr(0,dl(0,lit(n),lit(n)),lit(n))-word(37), []),
                              rule(dl, p(0,hausse,p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))))), lit(n)-appl(appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))),word(38)), [
                                 rule(axiom, hausse, lit(n)-word(38), []),
                                 rule(dr, p(0,de,p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))))), dl(0,lit(n),lit(n))-appl(word(39),appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41)))), [
                                    rule(axiom, de, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))-word(39), []),
                                    rule(dl, p(0,p(0,'2,5','%'),p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))))), lit(np(_,_,_))-appl(appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))),appl(word(40),word(41))), [
                                       rule(dr, p(0,'2,5','%'), lit(np(_,_,_))-appl(word(40),word(41)), [
                                          rule(axiom, '2,5', dr(0,lit(np(_,_,_)),lit(n))-word(40), []),
                                          rule(axiom, '%', lit(n)-word(41), [])
                                          ]),
                                       rule(dr, p(0,par,p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente))))), dl(0,lit(np(_,_,_)),lit(np(_,_,_)))-appl(word(42),appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46)))))), [
                                          rule(axiom, par, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n))-word(42), []),
                                          rule(dr, p(0,rapport,p(0,à,p(0,'l\'',p(0,année,précédente)))), lit(n)-appl(word(43),appl(word(44),appl(word(45),appl(word(47),word(46))))), [
                                             rule(axiom, rapport, dr(0,lit(n),lit(pp(à)))-word(43), []),
                                             rule(dr, p(0,à,p(0,'l\'',p(0,année,précédente))), lit(pp(à))-appl(word(44),appl(word(45),appl(word(47),word(46)))), [
                                                rule(axiom, à, dr(0,lit(pp(à)),lit(np(acc,_,_)))-word(44), []),
                                                rule(dr, p(0,'l\'',p(0,année,précédente)), lit(np(acc,_,_))-appl(word(45),appl(word(47),word(46))), [
                                                   rule(axiom, 'l\'', dr(0,lit(np(acc,_,_)),lit(n))-word(45), []),
                                                   rule(dl, p(0,année,précédente), lit(n)-appl(word(47),word(46)), [
                                                      rule(axiom, année, lit(n)-word(46), []),
                                                      rule(axiom, précédente, dl(0,lit(n),lit(n))-word(47), [])
                                                      ])
                                                   ])
                                                ])
                                             ])
                                          ])
                                       ])
                                    ])
                                 ])
                              ])
                           ])
                        ])
                     ])
                  ]),
               rule(dr, p(0,p(0,',',lundi),p(0,30,décembre)), dl(1,lit(s(ppart)),lit(s(ppart)))-appl(word(18),appl(word(19),word(20))), [
                  rule(axiom, p(0,',',lundi), dr(0,dl(1,lit(s(ppart)),lit(s(ppart))),lit(n))-word(18), []),
                  rule(dr, p(0,30,décembre), lit(n)-appl(word(19),word(20)), [
                     rule(axiom, 30, dr(0,lit(n),lit(n))-word(19), []),
                     rule(axiom, décembre, lit(n)-word(20), [])
                     ])
                  ])
               ])
            ])
         ])
      ]),
   rule(axiom, '.', dl(0,lit(s(main)),lit(txt))-word(48), [])
   ])).
