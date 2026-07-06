/*
rule(prod_e(I), FullPros, Y-FullSem, [
             rule(dl, _, p(0,A,dia(0,box(0,B)))-_, [
			  rule(prod_i, p(0,LPros1,LPros2)-pair(LSem1,LSem2), [
				       LProof1,
				       LProof2
			       ]),
			  rule(dr, p(0,Pet,p(0,RPros1,RPros2)), dl(0,p(0,A,B),p(0,A,dia(0,box(0,B))))-appl(SemEt,pair(RSem1,RSem1)), [
				       rule(RE, Pet, dr(0,dl(0,p(0,A,B),p(0,A,dia(0,box(0,B)))),p(0,A,B))-SemEt, EtPlist),
				       rule(prod_i, p(0,RPros1,RPros2), p(0,A,B)-pair(RSem1,RSem2), [
						    RProof1,
						    RProof2
					    ])
			       ]),
			  rule(dr, _, _, [
				       rule(dr, _, _, [
						    rule(RY, ProsY, dr(0,dr(0,Y,B),A)-SemYBA, YBAList),
						    rule(hyp(I), _, A-'$VAR'(_), [])
					    ]),
				       rule(hyp(I), _, B-'$VAR'(_), [])
			       ])
		  ])
     ]).
rule(dl, p(0,ProsY,p(0,p(0,LPros1,LPros2),p(0,Pet,p(0,RPros1,RPros2)))), Y-appl(appl(appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))),lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2))),SemYBA), [
	     rule(RY, ProsY, dr(0,dr(0,Y,B),A)-SemYBA, YBAList),
	     rule(dl, p(0,p(0,LPros1,LPros2),p(0,Pet,p(0,RPros1,RPros2))), dl(0,dr(0,dr(0,Y,B),A),Y)-appl(appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))),lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2))), [
			  rule(dli(J),p(0,LPros1,LPros2),dl(0,dr(0,dr(0,Y,B),A),Y)-lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2)), [
				       rule(dr,p(0,p(0,'$VAR'(P1),LPros1),LPros2),Y-appl(appl('$VAR'(V1),LSem1),LSem2), [
						    rule(dr, p(0,'$VAR'(P1),LPros1), dr(0,Y,B)-appl('$VAR'(V1),LSem1), [
								 rule(hyp(J), '$VAR'(P1), dr(0,dr(0,Y,B),A)-'$VAR'(V1), []),
								 LProof1
							 ]),
						    LProof2
					    ]),
				       rule(dr, p(0,Pet,p(0,RPros1,RPros2)), dl(0,dl(0,dr(0,dr(0,Y,B),A),Y),dl(0,dr(0,dr(0,Y,B),A),Y))-appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))), [				       
						    rule(RE, Pet, dr(0,dl(0,dl(0,dr(0,dr(0,Y,B),A),Y),dl(0,dr(0,dr(0,Y,B),A),Y)),dl(0,dr(0,dr(0,Y,B),A),Y))-SemEt, EtPlist),			       
						    rule(dli(K),p(0,RPros1,RPros2),dl(0,dr(0,dr(0,Y,B),A),Y)-lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2)), [
								 rule(dr,p(0,p(0,'$VAR'(P1),LPros1),LPros2),Y-appl(appl('$VAR'(V2),RSem1),RSem2), [
									      rule(dr, p(0,'$VAR'(P1),LPros1), dr(0,Y,B)-appl('$VAR'(V2),RSem1), [
											   rule(hyp(K), '$VAR'(P2), dr(0,dr(0,Y,B),A)-'$VAR'(V2), []),
											   RProof1
										   ]),
									      RProof2
								      ])
							 ])
					    ])
			       ])
		  ])
     ]).
*/

% we need FullSem for later semantic substitution
transform(rule(prod_e(I), _, Y-FullSem, [
             rule(dl, _, p(0,A,dia(0,box(0,B)))-_, [
			  rule(prod_i, p(0,LPros1,LPros2)-pair(LSem1,LSem2), [
				       LProof1,
				       LProof2
			       ]),
			  rule(dr, p(0,Pet,p(0,RPros1,RPros2)), dl(0,p(0,A,B),p(0,A,dia(0,box(0,B))))-appl(SemEt,pair(RSem1,RSem1)), [
				       rule(RE, Pet, dr(0,dl(0,p(0,A,B),p(0,A,dia(0,box(0,B)))),p(0,A,B))-SemEt, EtPlist),
				       rule(prod_i, p(0,RPros1,RPros2), p(0,A,B)-pair(RSem1,RSem2), [
						    RProof1,
						    RProof2
					    ])
			       ]),
			  rule(dr, _, _, [
				       rule(dr, _, _, [
						    rule(RY, ProsY, dr(0,dr(0,Y,B),A)-SemYBA, YBAList),
						    rule(hyp(I), _, A-'$VAR'(_), [])
					    ]),
				       rule(hyp(I), _, B-'$VAR'(_), [])
			       ])
		  ])
     ]),  
	  rule(dl, p(0,ProsY,p(0,p(0,LPros1,LPros2),p(0,Pet,p(0,RPros1,RPros2)))), Y-appl(appl(appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))),lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2))),SemYBA), [
		       rule(RY, ProsY, dr(0,dr(0,Y,B),A)-SemYBA, YBAList),
		       rule(dl, p(0,p(0,LPros1,LPros2),p(0,Pet,p(0,RPros1,RPros2))), dl(0,dr(0,dr(0,Y,B),A),Y)-appl(appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))),lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2))), [
				    rule(dli(J),p(0,LPros1,LPros2),dl(0,dr(0,dr(0,Y,B),A),Y)-lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2)), [
						 rule(dr,p(0,p(0,'$VAR'(P1),LPros1),LPros2),Y-appl(appl('$VAR'(V1),LSem1),LSem2), [
							      rule(dr, p(0,'$VAR'(P1),LPros1), dr(0,Y,B)-appl('$VAR'(V1),LSem1), [
									   rule(hyp(J), '$VAR'(P1), dr(0,dr(0,Y,B),A)-'$VAR'(V1), []),
									   LProof1
								   ]),
							      LProof2
						      ]),
						 rule(dr, p(0,Pet,p(0,RPros1,RPros2)), dl(0,dl(0,dr(0,dr(0,Y,B),A),Y),dl(0,dr(0,dr(0,Y,B),A),Y))-appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))), [				       
							      rule(RE, Pet, dr(0,dl(0,dl(0,dr(0,dr(0,Y,B),A),Y),dl(0,dr(0,dr(0,Y,B),A),Y)),dl(0,dr(0,dr(0,Y,B),A),Y))-SemEt, EtPlist),			       
							      rule(dli(K),p(0,RPros1,RPros2),dl(0,dr(0,dr(0,Y,B),A),Y)-lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2)), [
									   rule(dr,p(0,p(0,'$VAR'(P2),LPros1),LPros2),Y-appl(appl('$VAR'(V2),RSem1),RSem2), [
											rule(dr, p(0,'$VAR'(P2),LPros1), dr(0,Y,B)-appl('$VAR'(V2),RSem1), [
												     rule(hyp(K), '$VAR'(P2), dr(0,dr(0,Y,B),A)-'$VAR'(V2), []),
												     RProof1
											     ]),
											RProof2
										])
								   ])
						      ])
					 ])
			    ])
	       ]),
	  % semantic substitution
	  FullSem, appl(appl(appl(SemEt,lambda('$VAR'(V2),appl(appl('$VAR'(V2),RSem1),RSem2))),lambda('$VAR'(V1),appl(appl('$VAR'(V1),LSem1),LSem2))),SemYBA)
).
