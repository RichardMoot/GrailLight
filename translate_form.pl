
% adverbs
translate_form(dr(0,dr(0,pp,pp),n), dr(0,dr(0,lit(pp(P)),lit(pp(P))),lit(n))).
translate_form(dr(0,dr(0,pp,pp),np), dr(0,dr(0,lit(pp(P)),lit(pp(P))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,s,s),dr(0,s,s)), dr(0,dr(0,lit(s(X)),lit(s(Y))),dr(0,lit(s(X)),lit(s(Y))))).
translate_form(dr(0,dl(1,s,s),dl(1,s,s)), dr(0,dl(1,lit(s(X)),lit(s(Y))),dl(1,lit(s(X)),lit(s(Y))))).
translate_form(dr(0,dr(0,dr(0,s,s),s_q),dr(0,s,s)), dr(0,dr(0,dr(0,lit(s(X)),lit(s(Y))),lit(s(q))),dr(0,lit(s(X)),lit(s(Y))))).
translate_form(dr(0,dr(0,dl(1,s,s),s_q),dl(1,s,s)), dr(0,dr(0,dl(1,lit(s(X)),lit(s(Y))),lit(s(q))),dl(1,lit(s(X)),lit(s(Y))))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),dl(0,np,s)), dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),lit(s(q))),dl(0,lit(np(nom,A,B)),lit(s(C))))).
translate_form(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,dl(0,np,s),dl(0,np,s))),
	       dr(0,dl(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))),dl(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))))).
translate_form(dr(0,dl(1,dl(0,np,s),dl(0,np,s)),dl(1,dl(0,np,s),dl(0,np,s))),
	       dr(0,dl(1,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))),dl(1,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))))).
translate_form(dl(1,dl(0,np,s),dl(0,np,s)), dl(1,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,A,B)),lit(s(C))))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,s),dl(0,np,s))),
	       dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dl(0,lit(np(nom,D,E)),lit(s(F)))))).
translate_form(dr(0,dl(0,s,s),dl(0,s,s)), dr(0,dl(0,lit(s(X)),lit(s(Y))),dl(0,lit(s(X)),lit(s(Y))))).
translate_form(dr(0,dl(0,np,dl(0,dr(0,s,np),s)),dl(0,np,dl(0,dr(0,s,np),s))),
	       dr(0,dl(0,lit(np(A,B,C)),dl(0,dr(0,lit(s(D)),lit(np(A,B,C))),lit(s(D)))),dl(0,lit(np(A,B,C)),dl(0,dr(0,lit(s(D)),lit(np(A,B,C))),lit(s(D)))))).
translate_form(dr(0,dl(0,np,dl(0,dr(0,s,np),s)),dl(0,np,s_ppres)),
	       dr(0,dl(0,lit(np(nom,B,C)),dl(0,dr(0,lit(s(D)),lit(np(nom,B,C))),lit(s(D)))),dl(0,lit(np(nom,B,C)),lit(s(ppres))))).
% vp with infinitive subject
translate_form(dr(0,dl(0,dl(0,np,s),s),dl(0,dl(0,np,s),s)),
	       dr(0,dl(0,dl(0,lit(np(nom,A,B)),lit(s(C))),lit(s(D))),dl(0,dl(0,lit(np(nom,A,B)),lit(s(C))),lit(s(D))))).

% extraposed vp followed by subject
translate_form(dr(0,dl(0,dl(0,np,s),np),np), dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(_))),lit(np(A,B,C))),lit(np(A,B,C)))).

% "tag" expressions
translate_form(dr(0,dl(0,s,s),n), dr(0,dl(0,lit(s(A)),lit(s(A))),lit(n))).
translate_form(dr(0,dl(0,s,s),dl(0,n,n)), dr(0,dl(0,lit(s(A)),lit(s(A))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,s,s),dl(0,np,s)), dr(0,dl(0,lit(s(A)),lit(s(A))),dl(0,lit(np(nom,_,_)),lit(s(_))))).
translate_form(dr(0,dl(0,s,s),pp), dr(0,dl(0,lit(s(A)),lit(s(A))),lit(pp(_)))).
translate_form(dr(0,dr(0,s,np),s), dr(0,dr(0,lit(s(A)),lit(np(_,_,_))),lit(s(A)))).

% verbs
translate_form(dr(0,dl(0,dl(0,n,n),s),np), dr(0,dl(0,dl(0,lit(n),lit(n)),lit(s(main))),lit(np(_,_,_)))). % np not necessarily a subject
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,n,n)),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(n),lit(n))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),dl(0,n,n)),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),pp_a),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),pp_de),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),pp),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(_))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,np,s),np),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,s,np),np),dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,s_q),np),dr(0,dr(0,lit(s(main)),lit(s(q))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,dl(0,n,n)),np),dr(0,dr(0,lit(s(main)),dl(0,lit(n),lit(n))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,dl(0,np,s_ppart)),np),dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,A,B)),lit(s(ppart)))),lit(np(nom,A,B)))).
translate_form(dr(0,dr(0,s,np),dl(0,np,s_pass)),dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),dl(0,lit(np(nom,_,_)),lit(s(pass))))).
translate_form(dr(0,dr(0,s,np),dl(0,np,s_inf)),dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,s,dl(0,np,s_inf)),np),dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,dl(0,np,s_inf)),dl(0,n,n)),np), dr(0,dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),dl(0,lit(n),lit(n))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,dl(0,np,s_pass)),np),dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(pass)))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,np,s),s_q), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q)))).
translate_form(dr(0,dl(0,np,s),s_whq), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(whq)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp_de), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(à)))).
translate_form(dr(0,dl(0,np,s),np), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,np,s),pp), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(_)))).
translate_form(dr(0,dl(0,np,s),pp_a), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(à)))).
translate_form(dr(0,dl(0,np,s),pp_de), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(de)))).
translate_form(dr(0,dl(0,np,s),pp_par), dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(par)))).
translate_form(dl(0,np,s), dl(0,lit(np(nom,_,_)),lit(s(main)))).
translate_form(dr(0,s,np), dr(0,lit(s(main)),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,np),pp), dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),lit(pp(_)))).
translate_form(dr(0,dr(0,s,np),pp_a), dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),lit(pp(à)))).
translate_form(dr(0,dr(0,dr(0,s,pp_a),np),np), dr(0,dr(0,dr(0,lit(s(main)),lit(pp(à))),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,pp),np),np), dr(0,dr(0,dr(0,lit(s(main)),lit(pp(_))),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,np),pp_a),np), dr(0,dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(pp(à))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,np),pp),np), dr(0,dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(pp(_))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_inf)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_ppart)),dr(0,dl(0,lit(np(nom,B,C)),lit(s(main))),dl(0,lit(np(nom,B,C)),lit(s(ppart))))).
translate_form(dr(0,dr(0,s,np),dl(0,np,s_ppart)),dr(0,dr(0,lit(s(main)),lit(np(nom,B,C))),dl(0,lit(np(nom,B,C)),lit(s(ppart))))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_pass)),dr(0,dl(0,lit(np(nom,B,C)),lit(s(main))),dl(0,lit(np(nom,B,C)),lit(s(pass))))).
translate_form(dr(0,dr(0,s,pp),np),dr(0,dr(0,lit(s(main)),lit(pp(_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,pp_de),np),dr(0,dr(0,lit(s(main)),lit(pp(de))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,pp_a),np),dr(0,dr(0,lit(s(main)),lit(pp(à))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,pp_de),np),np), dr(0,dr(0,dr(0,lit(s(main)),lit(pp(de))),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,np,s),dl(0,n,n)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,s,np),np),dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,s,np),dl(0,n,n)), dr(0,dr(0,lit(s(main)),lit(np(_,_,_))),dl(0,lit(n),lit(n)))). % not necesarily nominative here
translate_form(dr(0,dr(0,s,np),dl(0,np,s_inf)),dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),dl(0,lit(np(_,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_a),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(à))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_par),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(par))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_de),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(_))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),pp_a),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),pp),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),dl(1,s,s)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),dl(1,lit(s(A)),lit(s(A))))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),dl(0,np,s_inf)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_ppart)),np),
	       dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(ppart)))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_y,dl(0,np,s)),s_q),np),
	       dr(0,dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(s(q))),lit(np(acc,_,_)))).
% verbs with infinitive subject
translate_form(dl(0,dl(0,np,s_inf),s), dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main)))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),np),dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),pp),dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(pp(_)))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),pp_a),dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(pp(à)))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),pp_de),dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(pp(de)))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),dl(0,np,s_inf)), dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,dl(0,np,s_inf),s)),dl(0,np,s_inf)), dr(0,dl(0,lit(cl_r),dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main)))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),dl(0,np,s_ppart)), dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(ppart))))).
translate_form(dr(0,dl(0,dl(0,np,s_inf),s),dl(0,n,n)),dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,dl(0,np,s_inf),s),pp_a),dl(0,n,n)),dr(0,dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(pp(à))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,dl(0,np,s_inf),s),pp),dl(0,n,n)),dr(0,dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(inf(_)))),lit(s(main))),lit(pp(_))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,s,dl(0,np,s_inf)),dl(0,np,s_ppart)), dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),dl(0,lit(np(nom,_,_)),lit(s(ppart))))).
% verbs with y clitic
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,np,s_inf)),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,B,C)),lit(s(main)))),dl(0,lit(np(nom,B,C)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,cl_y,dl(0,np,s_ppart))),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,B,C)),lit(s(main)))),dl(0,lit(cl_y),dl(0,lit(np(nom,B,C)),lit(s(ppart)))))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),np),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),pp),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,np,s_ainf)),np),dr(0,dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(np(nom,_,_)),lit(s(inf(à))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,np,s_inf)),np),dr(0,dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_y,s),np),np), dr(0,dr(0,dl(0,lit(cl_y),lit(s(main))),lit(np(acc,_,_))),lit(np(nom,_,_)))). % "y a-t-il"
translate_form(dr(0,dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,np,s_inf)),dl(0,n,n)), dr(0,dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,n,n)), dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(n),lit(n)))). % "il y a pire"
% verbs with reflexive clitic
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart))),
	       dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(main)))),dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(ppart)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,np,s_inf)), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(main)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_)))))).  
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,n,n)),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(n),lit(n)))).
translate_form(dl(0,cl_r,dl(0,np,s)), dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)), dl(0,cl_r,dl(0,np,s_ppart))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(main)))),dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(ppart)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp_a), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(à)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp_de), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(de)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp_par), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(par)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),s_q), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(s(q)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),s_whq), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(s(whq)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dr(0,dl(0,np,s_inf),dia(1,box(1,np)))), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(main)))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(_)))),dia(1,box(1,lit(np(acc,_,_))))))).
translate_form(dr(0,dl(0,cl_r,s),np), dr(0,dl(0,lit(cl_r),lit(s(main))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_r,s),np),np), dr(0,dr(0,dl(0,lit(cl_r),lit(s(main))),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),np), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_r,s),pp_a),np), dr(0,dr(0,dl(0,lit(cl_r),lit(s(main))),lit(pp(à))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_r,s),pp),np),dr(0,dr(0,dl(0,lit(cl_r),lit(s(main))),lit(pp(_))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,cl_r,dr(0,s,np)),dl(0,cl_r,dl(0,np,s_ppart))), dr(0,dl(0,lit(cl_r),dr(0,lit(s(main)),lit(np(nom,A,B)))),dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(ppart)))))).
translate_form(dr(0,dr(0,dl(0,cl_r,s),dl(0,np,s_inf)),np), dr(0,dr(0,dl(0,lit(cl_r),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dl(0,cl_r,dl(0,np,s)),pp_de),np), dr(0,dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,cl_r,dr(0,s,np)),dl(0,np,s_inf)), dr(0,dl(0,lit(cl_r),dr(0,lit(s(main)),lit(np(nom,_,_)))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(1,s,s)), dr(0,dl(0,cl_r,dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(1,lit(s(A)),lit(s(A))))).
translate_form(dr(0,dr(0,s,dl(0,np,s_inf)),pp_a), dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(pp(à)))). % "[a quoi] sert ..."
translate_form(dr(0,dr(0,s,dl(0,np,s_inf)),pp), dr(0,dr(0,lit(s(main)),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(pp(_)))). % "[a quoi] sert ..."
translate_form(dr(0,dr(0,dr(0,s,s_q),pp),np), dr(0,dr(0,dr(0,lit(s(main)),lit(s(q))),lit(pp(_))),lit(np(nom,_,_)))). % "est -ce pp que ..."
translate_form(dr(0,dr(0,dr(0,s,s_q),dl(0,np,s_inf)),np), dr(0,dr(0,dr(0,lit(s(main)),lit(s(q))),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dr(0,s,s_q),dl(0,n,n)),np), dr(0,dr(0,dr(0,lit(s(main)),lit(s(q))),dl(0,lit(n),lit(n))),lit(np(nom,_,_)))).
% Q: use twice or s(X) here instead of s(_) for the argument?
translate_form(dl(1,s,dr(0,s,np)), dl(1,lit(s(_)),dr(0,lit(s(main)),lit(np(nom,_,_))))).
translate_form(dl(0,cl_r,dl(1,s,dr(0,s,np))), dl(0,lit(cl_r),dl(1,lit(s(_)),dr(0,lit(s(main)),lit(np(nom,_,_)))))).
translate_form(dr(0,dl(0,cl_r,dl(1,s,s)),np), dr(0,dl(0,lit(cl_r),dl(1,lit(s(_)),lit(s(main)))),lit(np(nom,_,_)))).
%
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),dl(0,n,n)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(nom,_,_)),lit(s(inf(_))))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,np,s),np),dl(0,np,s_inf)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(np(acc,_,_)))).
% y compris
translate_form(dr(0,dl(0,cl_y,dl(1,s,s)),dl(1,s,s)),dr(0,dl(0,lit(cl_y),dl(1,lit(s(A)),lit(s(B)))),dl(1,lit(s(A)),lit(s(B))))).
translate_form(dr(0,dl(0,cl_y,dl(1,s,s)),np), dr(0,dl(0,lit(cl_y),dl(1,lit(s(A)),lit(s(A)))),lit(np(acc,_,_)))).
%
translate_form(dr(0,dl(0,cl_r,s),dr(0,dl(0,cl_r,s),dia(1,box(1,pp_a)))),dr(0,dl(0,lit(cl_r),lit(s(A))),dr(0,dl(0,lit(cl_r),lit(s(A))),dia(1,box(1,lit(pp(à))))))).
translate_form(dr(0,dl(0,cl_r,s),dr(0,dl(0,cl_r,s),dia(1,box(1,pp_de)))),dr(0,dl(0,lit(cl_r),lit(s(A))),dr(0,dl(0,lit(cl_r),lit(s(A))),dia(1,box(1,lit(pp(de))))))).
translate_form(dr(0,dl(0,cl_r,s),dl(0,cl_r,s)),dr(0,dl(0,lit(cl_r),lit(s(A))),dl(0,lit(cl_r),lit(s(A))))).
% adverbs/clitics
translate_form(dr(0,dr(0,dl(0,np,s),pp_a),dl(0,np,s_inf)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(à))),dl(0,lit(np(nom,_,_)),lit(s(inf(_)))))).
% cl_r
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dr(0,dl(0,cl_r,dl(0,np,s)),dia(1,box(1,np)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dia(1,box(1,lit(np(acc,_,_))))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dr(0,dl(0,cl_r,dl(0,np,s)),dia(1,box(1,pp_de)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dia(1,box(1,lit(pp(de))))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dr(0,dl(0,cl_r,dl(0,np,s)),dia(1,box(1,pp_a)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(C)))),dia(1,box(1,lit(pp(à))))))).
% cl_y
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,cl_y,dl(0,np,s))),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,A,B)),lit(s(C)))),dl(0,lit(cl_y),dl(0,lit(np(nom,A,B)),lit(s(C)))))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),dr(0,dl(0,cl_y,dl(0,np,s)),dia(1,box(1,np)))),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,A,B)),lit(s(C)))),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,A,B)),lit(s(C)))),dia(1,box(1,lit(np(acc,_,_))))))).
% extraction
translate_form(dr(0,dr(0,s,dr(0,s,dia(1,box(1,pp_a)))),n), dr(0,dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(à)))))),lit(n))).
translate_form(dr(0,dr(0,s,dr(0,s,dia(1,box(1,pp_a)))),np), dr(0,dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(à)))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,s,dr(0,s,dia(1,box(1,pp_de)))),n), dr(0,dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(de)))))),lit(n))).
translate_form(dr(0,dr(0,s,dr(0,s,dia(1,box(1,pp_de)))),np), dr(0,dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(de)))))),lit(np(acc,_,_)))).
translate_form(dr(0,s,dr(0,s,dia(1,box(1,pp_a)))), dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(à))))))).
translate_form(dr(0,s,dr(0,s,dia(1,box(1,pp_de)))), dr(0,lit(s(A)),dr(0,lit(s(A)),dia(1,box(1,lit(pp(de))))))).
% pied-piping
translate_form(dr(0,dl(0,dr(0,pp,np),dl(0,n,n)),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,dr(0,lit(pp(A)),lit(np(acc,_,_))),dl(0,lit(n),lit(n))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))).
translate_form(dr(0,dl(0,dr(0,pp,np),s_whq),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,dr(0,lit(pp(A)),lit(np(acc,_,_))),lit(s(whq))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))).
translate_form(dr(0,dl(0,dr(0,pp,np),dl(0,np,np)),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,dr(0,lit(pp(A)),lit(np(acc,_,_))),dl(0,lit(np(B,C,D)),lit(np(B,C,D)))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))).
translate_form(dr(0,dl(0,pp,s_q),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,lit(pp(A)),lit(s(q))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))). % "c'est avec pp que ... [pp]"
translate_form(dr(0,dr(0,dl(0,dr(0,pp,np),s_whq),dr(0,s,dia(1,box(1,pp)))),n), dr(0,dr(0,dl(0,dr(0,lit(pp(A)),lit(np(acc,_,_))),lit(s(whq))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A)))))),lit(n))).
translate_form(dr(0,dl(0,pp,dl(0,n,n)),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,lit(pp(A)),dl(0,lit(n),lit(n))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))). % "pp duquel ... [pp]"
% infinitives
translate_form(dl(0,np,s_inf), dl(0,lit(np(nom,_,_)),lit(s(inf(base))))).
translate_form(dr(0,dl(0,np,s_inf),np), dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,np,s_inf),pp_de),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(de)))).
translate_form(dr(0,dl(0,np,s_inf),pp_a),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à)))).
translate_form(dr(0,dl(0,np,s_inf),pp_par),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(par)))).
translate_form(dr(0,dl(0,np,s_inf),pp),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(_)))).
translate_form(dr(0,dl(0,np,s_inf),s_q),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(q)))).
translate_form(dr(0,dl(0,np,s_inf),s_whq),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(whq)))).
translate_form(dr(0,dl(0,np,s_inf),dl(0,n,n)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,np,s_inf),dl(0,np,s_pass)),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(pass))))).
translate_form(dr(0,dl(0,np,s_inf),dl(0,np,s_ppart)),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(ppart))))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(_))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_de),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(de))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp),pp), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(_))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_a),pp), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_a),pp_de), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),pp_de), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),pp_par), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),lit(pp(par)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),pp), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_a),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_de),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_par),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(par))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(_))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),s_q),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(q))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),s_q),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(q))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),s_q),pp_de), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(q))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),s_q),pp), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(q))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),s_whq),pp_a), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(s(whq))),lit(pp(à)))).
translate_form(dl(0,cl_r,dl(0,np,s_inf)),dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(pp(à)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_de),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(pp(de)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_par),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(pp(par)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(pp(_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),np),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),s_whq),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(s(whq)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),s_q),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(s(q)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dl(0,np,s_inf)),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(inf(base))))),dl(0,lit(np(nom,A,B)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dl(0,n,n)),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_de),np),dr(0,dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),dl(0,np,s_inf)),np), dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),dl(0,np,s_inf)), dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),lit(np(acc,_,_))),dl(0,lit(np(nom,A,B)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),dl(0,np,s_inf)),pp_a),dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_))))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),dl(0,n,n)),np), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),dl(0,lit(n),lit(n))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),np),dl(0,n,n)), dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(np(acc,_,_))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dl(0,cl_r,dl(0,np,s_ppart))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(inf(base))))),dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(ppart)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dr(0,dl(0,np,s_inf),dia(1,box(1,np)))), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(inf(base))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(_)))),dia(1,box(1,lit(np(acc,_,_))))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dr(0,dl(0,np,s_inf),dia(1,box(1,pp_a)))), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(inf(base))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(_)))),dia(1,box(1,lit(pp(à))))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s_inf)),dr(0,dl(0,np,s_inf),dia(1,box(1,pp_de)))), dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,A,B)),lit(s(inf(base))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(_)))),dia(1,box(1,lit(pp(de))))))).
translate_form(dl(1,s,dl(0,np,s_inf)),dl(1,lit(s(_)),dl(0,lit(np(nom,_,_)),lit(s(inf(base)))))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_a),dl(0,np,s_inf)),dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),lit(pp(à))),dl(0,lit(np(nom,A,B)),lit(s(inf(_)))))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),dl(0,np,s_inf)),dl(0,n,n)),dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_))))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s_inf)),np), dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dr(0,dl(0,np,s_inf),pp_par),pp_de),np),dr(0,dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(par))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dr(0,dl(0,np,s_inf),pp_a),np),pp),dr(0,dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),lit(np(acc,_,_))),lit(pp(_)))).
translate_form(dr(0,dr(0,dr(0,dl(0,np,s_inf),pp_a),dl(0,n,n)),np),dr(0,dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),dl(0,lit(n),lit(n))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dr(0,dl(0,np,s_inf),dl(0,np,s_inf)),pp_par),np),dr(0,dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_))))),lit(pp(par))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),pp_a),dl(0,n,n)),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(inf(base)))),lit(pp(à))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,np,s_inf),dl(0,np,s_inf)),pp),dr(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(inf(base)))),dl(0,lit(np(nom,A,B)),lit(s(inf(_))))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,cl_r,dl(0,np,s_inf)),s_q),np),dr(0,dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(inf(base))))),lit(s(q))),lit(np(acc,_,_)))).
translate_form(dl(0,np,dl(0,np,s_inf)),dl(0,lit(np(acc,_,_)),dl(0,lit(np(nom,_,_)),lit(s(inf(base)))))).

% clitics
translate_form(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(np(acc,_,_))))))).
translate_form(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(pp(à))))))).
translate_form(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_de)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(pp(de))))))).
translate_form(dr(0,s,dr(0,s,dia(1,box(1,np)))), dr(0,lit(s(X)),dr(0,lit(s(X)),dia(1,box(1,lit(np(acc,_,_))))))).
translate_form(dr(0,s,dr(0,s,dia(1,box(1,pp_a)))), dr(0,lit(s(X)),dr(0,lit(s(X)),dia(1,box(1,lit(pp(à))))))).
translate_form(dr(0,s,dr(0,s,dia(1,box(1,pp_de)))), dr(0,lit(s(X)),dr(0,lit(s(X)),dia(1,box(1,lit(pp(de))))))).

% preposed constituents

translate_form(dr(0,dl(0,dl(0,np,s),s),s), dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(_))),lit(s(A))),lit(s(A)))).
translate_form(dr(0,dl(0,np,s),s), dr(0,dl(0,lit(np(_,_,_)),lit(s(A))),lit(s(A)))).
translate_form(dr(0,dl(0,n,s),s), dr(0,dl(0,lit(n),lit(s(A))),lit(s(A)))).
translate_form(dr(0,dl(0,dl(0,n,n),s),s), dr(0,dl(0,dl(0,lit(n),lit(n)),lit(s(A))),lit(s(A)))).

% coordination

translate_form(dr(0,dl(0,dr(0,pp,pp),dr(0,pp,pp)),dr(0,pp,pp)), dr(0,dl(0,dr(0,lit(pp(P)),lit(pp(P))),dr(0,lit(pp(P)),lit(pp(P)))),dr(0,lit(pp(Q)),lit(pp(Q))))).
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,np,s),dl(0,np,s))),
	       dr(0,dl(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T)))),dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T))))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dl(0,lit(np(nom,_,_)),lit(s(_)))))).
translate_form(dr(0,dl(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,dl(0,np,s),dl(0,np,s))),dl(0,dl(0,np,s),dl(0,np,s))),
	       dr(0,dl(0,dl(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T)))),dl(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T))))),dl(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dl(0,lit(np(nom,_,_)),lit(s(_)))))).	       
translate_form(dr(0,dl(0,dl(1,dl(0,np,s),dl(0,np,s)),dl(1,dl(0,np,s),dl(0,np,s))),dl(1,dl(0,np,s),dl(0,np,s))),
	       dr(0,dl(0,dl(1,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T)))),dl(1,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T))))),dl(1,dl(0,lit(np(nom,_,_)),lit(s(_))),dl(0,lit(np(nom,_,_)),lit(s(_)))))).	       
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,dl(0,np,s)))),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,np,s),dia(0,box(0,dl(0,np,s))))),
      dr(0,dl(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dia(0,box(0,dl(0,lit(np(nom,X,Y)),lit(s(T)))))),dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T))))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dia(0,box(0,dl(0,lit(np(nom,_,_)),lit(s(_)))))))).
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),np),dr(0,dl(0,np,s),np)),dr(0,dl(0,np,s),np)),
	       dr(0,dl(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W))),dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W)))),
		  dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),lit(np(acc,_,_))))).
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,np))),dr(0,dl(0,np,s),np)),dr(0,dl(0,np,s),dia(0,box(0,np)))),
	       dr(0,dl(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dia(0,box(0,lit(np(acc,D,E))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),lit(np(acc,D,E)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dia(0,box(0,lit(np(acc,_,_))))))).
translate_form(dr(0,dl(0,dr(0,dr(0,dl(0,np,s),np),pp),dr(0,dr(0,dl(0,np,s),np),pp)),dr(0,dr(0,dl(0,np,s),np),pp)),dr(0,dl(0,dr(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W))),lit(pp(U))),dr(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W))),lit(pp(U)))),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),lit(np(acc,_,_))),lit(pp(_))))).
translate_form(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(S)))),dl(0,lit(np(nom,_,_)),lit(s(_))))).
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,s_q))),dr(0,dl(0,np,s),s_q)),dr(0,dl(0,np,s),dia(0,box(0,s_q)))),
	       dr(0,dl(0,dr(0,dl(0,lit(np(nom,B,C)),lit(s(D))),dia(0,box(0,lit(s(q))))),dr(0,dl(0,lit(np(nom,B,C)),lit(s(D))),lit(s(q)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dia(0,box(0,lit(s(q))))))).
translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,pp))),dr(0,dl(0,np,s),pp)),dr(0,dl(0,np,s),dia(0,box(0,pp)))),
	       dr(0,dl(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),dia(0,box(0,lit(pp(D))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(C))),lit(pp(D)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dia(0,box(0,lit(pp(_))))))).
translate_form(dr(0,dl(0,dr(0,dr(0,s,dl(0,np,s)),np),dr(0,dr(0,s,dl(0,np,s)),np)),dr(0,dr(0,s,dl(0,np,s)),np)),
	       dr(0,dl(0,dr(0,dr(0,lit(s(A)),dl(0,lit(np(nom,B,C)),lit(s(C)))),lit(np(nom,D,E))),
		       dr(0,dr(0,lit(s(A)),dl(0,lit(np(nom,B,C)),lit(s(C)))),lit(np(nom,D,E)))),dr(0,dr(0,lit(s(_)),dl(0,lit(np(nom,_,_)),lit(s(_)))),lit(np(nom,_,_))))).
translate_form(dr(0,dl(0,dr(0,s,np),dr(0,s,np)),dr(0,s,np)),
	       dr(0,dl(0,dr(0,lit(s(A)),lit(np(B,C,D))),dr(0,lit(s(A)),lit(np(B,C,D)))),dr(0,lit(s(_)),lit(np(_,_,_))))).
translate_form(dr(0,dl(0,dr(0,s,dia(0,box(0,np))),dr(0,s,np)),dr(0,s,dia(0,box(0,np)))),
	       dr(0,dl(0,dr(0,lit(s(A)),dia(0,box(0,lit(np(B,C,D))))),dr(0,lit(s(A)),lit(np(B,C,D)))),dr(0,lit(s(_)),dia(0,box(0,lit(np(_,_,_))))))).
translate_form(dr(0,dl(0,dr(0,s,dl(0,np,s)),dr(0,s,dl(0,np,s))),dr(0,s,dl(0,np,s))),
	       dr(0,dl(0,dr(0,lit(s(A)),dl(0,lit(np(nom,B,C)),lit(s(D)))),dr(0,lit(s(A)),dl(0,lit(np(nom,B,C)),lit(s(D))))),dr(0,lit(s(_)),dl(0,lit(np(nom,_,_)),lit(s(_)))))).
translate_form(dr(0,dl(0,pp,dr(0,s,dr(0,s,dia(1,box(1,pp))))),pp),
	       dr(0,dl(0,lit(pp(A)),dr(0,lit(s(B)),dr(0,lit(s(B)),dia(1,box(1,lit(pp(A))))))),lit(pp(A)))).
translate_form(dr(0,dl(0,pp,dl(1,s,s)),dr(0,s,dia(1,box(1,pp)))),
	       dr(0,dl(0,lit(pp(A)),dl(1,lit(s(B)),lit(s(B)))),dr(0,lit(s(_)),dia(1,box(1,lit(pp(A))))))).
% gapping
translate_form(dr(0,dl(0,dr(0,dr(0,s,np),dia(0,box(0,pp))),dr(0,dr(0,s,np),pp)),dr(0,dr(0,s,np),dia(0,box(0,pp)))),
	       dr(0,dl(0,dr(0,dr(0,lit(s(A)),lit(np(B,C,D))),dia(0,box(0,lit(pp(E))))),dr(0,dr(0,lit(s(A)),lit(np(B,C,D))),lit(pp(E)))),dr(0,dr(0,lit(s(_)),lit(np(_,_,_))),dia(0,box(0,lit(pp(_))))))).
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s))))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),dl(0,np,s)))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s)))))),
      dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3))))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3)))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3))))))))).
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp))))),
      dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(pp(PP)))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(pp(PP))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(pp(PP)))))))).
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),np)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),np))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),np))))),
      dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(np(acc,B,C)))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(np(acc,B,C))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),lit(np(acc,B,C)))))))).
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,n,n))))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),dl(0,n,n)))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,n,n)))))),
	    dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(n),lit(n)))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(n),lit(n))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(n),lit(n)))))))).   
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,s,np)))),dr(0,s,box(1,dia(1,dr(0,s,np))))),dr(0,s,dia(1,box(1,dr(0,s,np))))),
	       dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,lit(s(S2)),lit(np(nom,B,C)))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,lit(s(S2)),lit(np(nom,B,C))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,lit(s(S2)),lit(np(nom,B,C)))))))).
translate_form(dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s))))),
	       dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dl(0,lit(np(nom,_,_)),lit(s(S2)))))),dr(0,lit(s(S1)),box(1,dia(1,dl(0,lit(np(nom,_,_)),lit(s(S2))))))),dr(0,lit(s(S1)),dia(1,box(1,dl(0,lit(np(nom,_,_)),lit(s(S2)))))))).
	       
translate_form(lit(s),lit(s(_))).
translate_form(lit(np),lit(np(_,_,_))).
