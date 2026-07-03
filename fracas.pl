:- dynamic sent/2.

sent(1, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Italien', npp-nam, 'Italien', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(devenu, vpp-ver:pper, devenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,n,n),dr(0,n,n))-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(monde, nc-nom, monde, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(2, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,cl_y,dl(0,np,s_ppart)))-1]), si(eu, vpp-ver:pper, avoir, [dr(0,dl(0,cl_y,dl(0,np,s_ppart)),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si('Italien', nc-nam, 'Italien', [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(devenu, vpp-ver:pper, devenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,n,n),dr(0,n,n))-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(monde, nc-nom, monde, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(3, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Italien', npp-nam, 'Italien', [n-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(4, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si('Italiens', nc-nam, 'Italiens', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(5, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si('Italiens', nc-nam, 'Italiens', [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(veulent, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(6, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Italiens', nc-nam, 'Italiens', [n-1]), si(veulent, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(7, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si('Italiens', npp-nam, 'Italiens', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(8, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si('Italiens', nc-nam, 'Italiens', [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(veulent, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1])], Result).
sent(9, Result) :-
      prob_parse([ si('Chaque', det-pro:ind, 'Chaque', [dr(0,np,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si(italien, adj-adj, italien, [dl(0,n,n)-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),dl(0,n,n))-1]), si(excellent, adj-adj, excellent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(10, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(excellents, adj-adj, excellent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(11, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(veulent, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),dl(0,n,n))-1]), si(excellent, adj-adj, excellent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(12, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(vraiment, adv-adv, vraiment, [dr(0,dl(0,n,n),dl(0,n,n))-1]), si(ambitieux, adj-adj, ambitieux, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(13, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(vraiment, adv-adv, vraiment, [dr(0,dl(0,n,n),dl(0,n,n))-1]), si(ambitieux, adj-adj, ambitieux, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(14, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(très, adv-adv, très, [dr(0,dr(0,n,n),dr(0,n,n))-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(modeste, adj-adj, modeste, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(15, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(très, adv-adv, très, [dr(0,dr(0,n,n),dr(0,n,n))-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(modestes, adj-adj, modeste, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(16, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(suédois, adj-adj, suédois, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(17, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(suédois, adj-adj, suédois, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(18, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, det-prp, de, [dr(0,pp_de,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(allemands, adj-adj, allemand, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(19, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(allemands, adj-adj, allemand, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(20, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(britanniques, adj-adj, britannique, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(21, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(britanniques, adj-adj, britannique, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(22, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(23, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(italiens, adj-adj, italien, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(24, Result) :-
      prob_parse([ si('Quelques', det-pro:ind, 'Quelques', [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(chantent, v-ver:pres, chanter, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(chansons, nc-nom, chanson, [n-1]), si(populaires, adj-adj, populaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(25, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(aiment, v-ver:pres, aimer, [dr(0,dl(0,np,s),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(chansons, nc-nom, chanson, [n-1]), si(populaires, adj-adj, populaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(26, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(chantent, v-ver:pres, chanter, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(chansons, nc-nom, chanson, [n-1]), si(populaires, adj-adj, populaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(27, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(pauvres, adj-adj, pauvre, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(28, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(pauvres, adj-adj, pauvre, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(29, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(principaux, adj-adj, principal, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(excellents, adj-adj, excellent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(30, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(principaux, adj-adj, principal, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(excellents, adj-adj, excellent, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(indispensables, adj-adj, indispensable, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(31, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(principaux, adj-adj, principal, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(indispensables, adj-adj, indispensable, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(32, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(ayant, vpr-ver:ppre, avoir, [dr(0,dl(0,n,n),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rôle, nc-nom, rôle, [n-1]), si(principal, adj-adj, principal, [dl(0,n,n)-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(bon, adj-adj, bon, [dr(0,dl(0,n,n),n)-1]), si(marché, nc-nom, marché, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(33, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si(un, nc-num, un, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(ayant, vpr-ver:ppre, avoir, [dr(0,dl(0,n,n),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rôle, nc-nom, rôle, [n-1]), si(principal, adj-adj, principal, [dl(0,n,n)-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si('Pavarotti', npp-nam, 'Pavarotti', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(34, Result) :-
      prob_parse([ si('Pavarotti', npp-nam, 'Pavarotti', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ténor, nc-nom, ténor, [n-1]), si(ayant, vpr-ver:ppre, avoir, [dr(0,dl(0,n,n),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rôle, nc-nom, rôle, [n-1]), si(principal, adj-adj, principal, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(bon, adj-adj, bon, [dr(0,dl(0,n,n),n)-1]), si(marché, nc-nom, marché, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(35, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(prendront, v-ver:futu, prendre, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(part, nc-nom, part, [np-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(concert, nc-nom, concert, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(36, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(prendront, v-ver:futu, prendre, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(part, nc-nom, part, [np-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(concert, nc-nom, concert, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(37, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(plus, adv-adv, plus, [n-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(donneront, v-ver:futu, donner, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(leur, det-det:pos, son, [dr(0,np,n)-1]), si(cachet, nc-nom, cachet, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(oeuvre, nc-nom, oeuvre, [n-1]), si(caritative, adj-adj, caritatif, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(38, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(ténors, nc-nom, ténor, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(donneront, v-ver:futu, donner, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(leur, det-det:pos, son, [dr(0,np,n)-1]), si(cachet, nc-nom, cachet, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(oeuvre, nc-nom, oeuvre, [n-1]), si(caritative, adj-adj, caritatif, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(39, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Irlandais', nc-nam, 'Irlandais', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', npp-nam, 'Nobel', [dl(0,n,n)-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(littérature, nc-nom, littérature, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(40, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Irlandais', nc-nam, 'Irlandais', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', adj-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(41, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Européen', adj-nam, 'Européen', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(42, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(43, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(44, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(45, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(46, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(47, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(48, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(49, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Européen', adj-nam, 'Européen', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(50, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(51, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(52, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', adj-nam, 'Européen', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(53, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(états, nc-nom, état, [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(54, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, un, [dr(0,dl(0,n,n),n)-1]), si(états, nc-nom, état, [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(individus, nc-nom, individu, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(55, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(individu, nc-nom, individu, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(56, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(états, nc-nom, état, [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(57, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(58, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(59, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(60, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(61, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(62, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(63, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(64, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(65, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(résident, v-ver:pres, résider, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(66, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(67, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(résident, v-ver:pres, résider, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(68, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(69, Result) :-
      prob_parse([ si('Quelques', det-pro:ind, 'Quelques', [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Suède', npp-nam, 'Suède', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(70, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(71, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Suède', npp-nam, 'Suède', [n-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(72, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(quelques, det-pro:ind, quelque, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(73, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membre, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Portugal', npp-nam, 'Portugal', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(74, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(75, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Portugal', npp-nam, 'Portugal', [n-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Sud', npp-nam, 'Sud', [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(76, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(peu, adv-adv, peu, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(viennent, v-ver:pres, venir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Sud', npp-nam, 'Sud', [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(77, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(affaire, nc-nom, affaire, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si(premier, det-num, premier, [dr(0,np,n)-1]), si(plan, nc-nom, plan, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(78, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(affaire, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(79, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(80, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, det-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(81, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(82, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(83, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(84, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(85, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Irlandais', nc-nam, 'Irlandais', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', adj-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(86, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(irlandais, nc-nom, irlandais, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', npp-nam, 'Nobel', [dl(0,n,n)-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(littérature, nc-nom, littérature, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(87, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(88, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(89, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(90, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Européen', adj-nam, 'Européen', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(91, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(92, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(93, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(habiter, vinf-ver:infi, habiter, [dl(0,np,s_inf)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(94, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(habiter, vinf-ver:infi, habiter, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(95, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si('Européen', adj-nam, 'Européen', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(96, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(97, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(habiter, vinf-ver:infi, habiter, [dl(0,np,s_inf)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(98, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Européen', npp-nam, 'Européen', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(habiter, vinf-ver:infi, habiter, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(99, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('États', nc-nom, 'États', [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(100, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, un, [dr(0,dl(0,n,n),n)-1]), si('États', nc-nom, 'États', [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(101, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dl(0,np,s_inf)-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(importe, v-ver:pres, importer, [dl(0,np,s)-1]), si(où, prorel-pro:rel, où, [dl(0,dl(0,np,s),dl(1,s,s))-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(102, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('États', nc-nom, 'États', [n-1]), si(membres, nc-nom, membre, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dl(0,np,s_inf)-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(importe, v-ver:pres, importer, [dl(0,np,s)-1]), si(où, prorel-pro:rel, où, [dl(0,dl(0,np,s),dl(1,s,s))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(103, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(104, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(105, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(106, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(107, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(108, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(109, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(110, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(111, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(112, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(113, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(habitent, v-ver:pres, habiter, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(114, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(habitent, v-ver:pres, habiter, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(115, Result) :-
      prob_parse([ si('Quelques', det-pro:ind, 'Quelques', [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(116, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(117, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(originaires, adj-adj, originaire, [dr(0,dl(0,n,n),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Suède', npp-nam, 'Suède', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, adj-adj, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(118, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(quelques, det-pro:ind, quelque, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Suède', npp-nam, 'Suède', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(119, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(120, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(121, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(originaires, adj-adj, originaire, [dr(0,dl(0,n,n),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Portugal', npp-nam, 'Portugal', [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(122, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(peu, adv-adv, peu, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(originaires, adj-adj, originaire, [dr(0,dl(0,n,n),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Portugal', npp-nam, 'Portugal', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(123, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(affaires, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(124, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(affaire, nc-nom, affaire, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si(premier, det-num, premier, [dr(0,np,n)-1]), si(plan, nc-nom, plan, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(125, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, det-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(126, Result) :-
      prob_parse([ si('Un', pro-num, 'Un', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(127, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(128, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(129, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(130, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(131, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Suédois', nc-nam, 'Suédois', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', adj-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(132, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Suédois', npp-nam, 'Suédois', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si('Scandinave', nc-nam, 'Scandinave', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(133, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Scandinave', npp-nam, 'Scandinave', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', npp-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(134, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(135, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(136, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(137, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(138, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(139, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(140, Result) :-
	prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(141, Result) :-
	prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(résident, nc-nom, résident, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(142, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(143, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(principaux, adj-adj, principal, [dr(0,n,n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(144, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(principaux, adj-adj, principal, [dr(0,n,n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(habitants, nc-nom, habitant, [np-1]), si(de, p-prp, de, [dr(0,dl(0,np,np),n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(145, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(habiter, vinf-ver:infi, habiter, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(146, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si(scandinave, adj-adj, scandinave, [dl(0,n,n)-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(147, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(148, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(irlandais, adj-adj, irlandais, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(149, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(150, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(britanniques, adj-adj, britannique, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(151, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(152, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(portugais, adj-adj, portugais, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(leurs, det-det:pos, leur, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(153, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(leurs, det-det:pos, leur, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(154, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(habitent, v-ver:pres, habiter, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(155, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(156, Result) :-
      prob_parse([ si('Quelques', det-pro:ind, 'Quelques', [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(féminins, adj-adj, féminin, [dl(0,n,n)-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(157, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(quelques, det-pro:ind, quelque, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-adj, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(158, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(féminins, adj-adj, féminin, [dl(0,n,n)-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(159, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(160, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(dans, p-prp, dans, [dr(0,pp,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(affaires, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(161, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(dans, p-prp, dans, [dr(0,pp,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(affaires, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(162, Result) :-
      prob_parse([ si('Aucune', pro-pro:ind, 'Aucune', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(femmes, nc-nom, femme, [n-1]), si(commissaires, nc-nom, commissaire, [dl(0,n,n)-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(163, Result) :-
      prob_parse([ si('Une', pro-num, 'Une', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(164, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(165, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(166, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(167, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(168, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Scandinave', npp-nam, 'Scandinave', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', adj-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(169, Result) :-
      prob_parse([ si('Tout', adv-adv, 'Tout', [dr(0,np,n)-1]), si('Suédois', npp-nam, 'Suédois', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si('Scandinave', nc-nam, 'Scandinave', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(170, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si('Suédois', nc-nam, 'Suédois', [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(reçu, vpp-ver:pper, recevoir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(prix, nc-nom, prix, [n-1]), si('Nobel', adj-nam, 'Nobel', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(171, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(172, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(173, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(174, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(175, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(176, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(177, Result) :-
      prob_parse([ si('Tout', pro-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(178, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(continent, nc-nom, continent, [n-1]), si('nord-américain', adj-adj, 'nord-américain', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(179, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(habitant, nc-nom, habitant, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si('Canada', npp-nam, 'Canada', [n-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(180, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(181, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, un, [dr(0,dl(0,n,n),n)-1]), si(principaux, adj-adj, principal, [dr(0,n,n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(182, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(résidents, nc-nom, résident, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(principaux, adj-adj, principal, [dr(0,n,n)-1]), si(pays, nc-nom, pays, [n-1]), si(occidentaux, adj-adj, occidental, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(droit, nc-nom, droit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),dl(0,np,s_inf))-1]), si(vivre, vinf-ver:infi, vivre, [dr(0,dl(0,np,s_inf),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(183, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(délégué, nc-nom, délégué, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fini, vpp-ver:pper, finir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(184, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(scandinaves, adj-adj, scandinave, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fini, vpp-ver:pper, finir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(185, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fini, vpp-ver:pper, finir, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(186, Result) :-
      prob_parse([ si('Certains', det-pro:ind, 'Certains', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(irlandais, adj-adj, irlandais, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(187, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(188, Result) :-
      prob_parse([ si('Beaucoup', adv-adv, 'Beaucoup', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(britanniques, adj-adj, britannique, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(intéressants, adj-adj, intéressant, [dl(0,n,n)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(partir, vinf-ver:infi, partir, [dr(0,dl(0,np,s_inf),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(enquête, nc-nom, enquête, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(189, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(190, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(délégués, nc-nom, délégué, [n-1]), si(portugais, adj-adj, portugais, [dl(0,n,n)-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(publié, vpp-ver:pper, publier, [dr(0,dl(0,np,s_ppart),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(résultats, nc-nom, résultat, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(quotidiens, nc-nom, quotidien, [n-1]), si(nationaux, adj-adj, national, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(191, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(192, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si('Européens', nc-nam, 'Européens', [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(habitent, v-ver:pres, habiter, [dl(0,np,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(dehors, nc-nom, dehors, [dr(0,n,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si(peuvent, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(circuler, vinf-ver:infi, circuler, [dl(0,np,s_inf)-1]), si(librement, adv-adv, librement, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(193, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(194, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(quelques, det-pro:ind, quelque, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(féminins, adj-adj, féminin, [dl(0,n,n)-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-adj, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si('Scandinavie', npp-nam, 'Scandinavie', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(195, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(196, Result) :-
      prob_parse([ si('Peu', adv-adv, 'Peu', [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(membres, nc-nom, membre, [n-1]), si(féminins, adj-adj, féminin, [dl(0,n,n)-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(originaires, vpp-ver:pper, originaire, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(sud, nc-nom, sud, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('Europe', npp-nam, 'Europe', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(197, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(proviennent, v-ver:pres, provenir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(monde, nc-nom, monde, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(affaires, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(198, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-adj, commissaire, [n-1]), si(proviennent, v-ver:pres, provenir, [dr(0,dl(0,np,s),pp)-1]), si(du, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(monde, nc-nom, monde, [n-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(affaires, nc-nom, affaire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(199, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(deux, adj-num, deux, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(200, Result) :-
      prob_parse([ si('Une', pro-num, 'Une', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passe, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(201, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(202, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(moins, adv-adv, moins, [n-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(masculins, adj-adj, masculin, [dl(0,n,n)-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(203, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(204, Result) :-
      prob_parse([ si('Au', p+d-prp:det, 'Au', [dr(0,dr(0,s,s),n)-1]), si(plus, adv-adv, plus, [n-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [dr(0,n,n)-1]), si(commissaires, nc-nom, commissaire, [n-1]), si(passent, v-ver:pres, passer, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(du, p+d-prp:det, de, [dr(0,np,n)-1]), si(temps, nc-nom, temps, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(205, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(206, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(207, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(plusieurs, det-pro:ind, plusieurs, [dr(0,np,n)-1]), si(juristes, nc-nom, juriste, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(208, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(209, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ou, cc-kon, ou, [dr(0,dl(0,np,np),np)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(210, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(211, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ou, cc-kon, ou, [dr(0,dl(0,np,np),np)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(212, Result) :-
      prob_parse([ si('Si', cs-kon, 'Si', [dr(0,dr(0,s,s),s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pas, adv-adv, pas, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(213, Result) :-
      prob_parse([ si('Exactement', cs-kon, 'Exactement', [dr(0,s,s)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(juristes, nc-nom, juriste, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(comptables, nc-nom, comptable, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(214, Result) :-
      prob_parse([ si('Six', det-num, 'Six', [dr(0,np,n)-1]), si(juristes, nc-nom, juriste, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(215, Result) :-
      prob_parse([ si('Exactement', cs-kon, 'Exactement', [dr(0,s,s)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(juristes, nc-nom, juriste, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(comptables, nc-nom, comptable, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(216, Result) :-
      prob_parse([ si('Six', det-num, 'Six', [dr(0,np,n)-1]), si(comptables, nc-nom, comptable, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(217, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(218, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(219, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(220, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(221, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(ou, cc-kon, ou, [dr(0,dl(0,np,np),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(222, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(223, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(président, nc-nom, président, [n-1]), si(donne, v-ver:pres, donne, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(lecture, nc-nom, lecture, [np-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(points, nc-nom, point, [n-1]), si(inscrits, vpp-ver:pper, inscrire, [dr(0,dl(0,n,n),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(ordre, nc-nom, ordre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(jour, nc-nom, jour, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(224, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(président, nc-nom, président, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(lu, vpp-ver:pper, lire, [dr(0,dl(0,np,s_ppart),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(points, nc-nom, point, [n-1]), si(à, p-prp, à, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(ordre, nc-nom, ordre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(jour, nc-nom, jour, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(225, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(226, Result) :-
      prob_parse([ si('Chacun', pro-pro:ind, 'Chacun', [np-1]), si(à, p-prp, à, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(227, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(228, Result) :-
      prob_parse([ si('Chacun', pro-pro:ind, 'Chacun', [np-1]), si(à, p-prp, à, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(229, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(toutes, adv-adv, tout, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(230, Result) :-
      prob_parse([ si('Chacun', pro-pro:ind, 'Chacun', [np-1]), si(à, p-prp, à, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouveau, adj-adj, nouveau, [dr(0,n,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(231, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Cambridge', npp-nam, 'Cambridge', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(candidat, nc-nom, candidat, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(parti, nc-nom, parti, [n-1]), si(travailliste, adj-adj, travailliste, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(232, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(habitants, nc-nom, habitant, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Cambridge', npp-nam, 'Cambridge', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voté, vpp-ver:pper, voter, [dr(0,dl(0,np,s_ppart),pp)-1]), si(pour, p-prp, pour, [dr(0,pp,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(candidat, nc-nom, candidat, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(parti, nc-nom, parti, [n-1]), si(travailliste, adj-adj, travailliste, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(233, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(grecs, nc-nom, grec, [n-1]), si(anciens, adj-adj, ancien, [dl(0,n,n)-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(philosophes, nc-nom, philosophe, [n-1]), si(réputés, vpp-ver:pper, réputé, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(234, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(grecs, nc-nom, grec, [n-1]), si(anciens, adj-adj, ancien, [dl(0,n,n)-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(philosophes, nc-nom, philosophe, [n-1]), si(réputés, vpp-ver:pper, réputé, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(235, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(grecs, nc-nom, grec, [n-1]), si(anciens, adj-adj, ancien, [dl(0,n,n)-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(tous, adv-adv, tout, [dl(1,s,s)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(philosophes, nc-nom, philosophe, [n-1]), si(réputés, vpp-ver:pper, réputé, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(236, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(grecs, nc-nom, grec, [n-1]), si(anciens, adj-adj, ancien, [dl(0,n,n)-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(philosophes, nc-nom, philosophe, [n-1]), si(réputés, vpp-ver:pper, réputé, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(237, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(panne, nc-nom, panne, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(attribuée, vpp-ver:pper, attribuer, [dr(0,dl(0,np,s_pass),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(erreurs, nc-nom, erreur, [n-1]), si(logicielles, adj-adj, logiciel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(238, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(panne, nc-nom, panne, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(attribuée, vpp-ver:pper, attribuer, [dr(0,dl(0,np,s_pass),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(ou, cc-kon, ou, [dr(0,dl(0,dr(0,np,n),dr(0,np,n)),dr(0,np,n))-1]), si(plusieurs, det-pro:ind, plusieurs, [dr(0,np,n)-1]), si(erreurs, nc-nom, erreur, [n-1]), si(logicielles, adj-adj, logiciel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(239, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(panne, nc-nom, panne, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(attribuée, vpp-ver:pper, attribuer, [dr(0,dl(0,np,s_pass),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(erreurs, nc-nom, erreur, [n-1]), si(logicielles, adj-adj, logiciel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(240, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(bug, nc-nom, bug, [n-1]), si(numéro, nc-nom, numéro, [dr(0,dl(0,n,n),np)-1]), si('32-985', pro-num, '32-985', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(erreur, nc-nom, erreur, [n-1]), si(logicielle, adj-adj, logiciel, [dl(0,n,n)-1]), si(connue, vpp-ver:pper, connaître, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(241, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(panne, nc-nom, panne, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(attribuée, vpp-ver:pper, attribuer, [dr(0,dl(0,np,s_pass),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(bug, nc-nom, bug, [n-1]), si(numéro, nc-nom, numéro, [dl(0,n,n)-1]), si('32-985', adj-num, '32-985', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(242, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(présents, adj-adj, présent, [dr(0,dl(0,n,n),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(démonstration, nc-nom, démonstration, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(tous, adv-adv, tout, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(impressionnés, vpp-ver:pper, impressionner, [dr(0,dl(0,np,s_pass),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(performances, nc-nom, performance, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(243, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(un, pro-num, un, [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(clients, nc-nom, client, [n-1]), si(présents, adj-adj, présent, [dr(0,dl(0,n,n),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(démonstration, nc-nom, démonstration, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(244, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(impressionnée, vpp-ver:pper, impressionner, [dr(0,dl(0,np,s_pass),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(performances, nc-nom, performance, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(245, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(clients, nc-nom, client, [n-1]), si(présents, adj-adj, présent, [dr(0,dl(0,n,n),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(démonstration, nc-nom, démonstration, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(impressionné, vpp-ver:pper, impressionner, [dr(0,dl(0,np,s_pass),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(performances, nc-nom, performance, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(246, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(clients, nc-nom, client, [n-1]), si(présents, adj-adj, présent, [dr(0,dl(0,n,n),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(démonstration, nc-nom, démonstration, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(impressionnées, vpp-ver:pper, impressionner, [dr(0,dl(0,np,s_pass),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(performances, nc-nom, performance, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(système, nc-nom, système, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(247, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(diplômés, nc-nom, diplômé, [n-1]), si(universitaires, adj-adj, universitaire, [dl(0,n,n)-1]), si(font, v-ver:pres, faire, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(médiocres, adj-adj, médiocre, [dr(0,n,n)-1]), si(agents, nc-nom, agent, [n-1]), si(boursiers, adj-adj, boursier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(248, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(diplômé, nc-nom, diplômé, [n-1]), si(universitaire, adj-adj, universitaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(249, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(est, v-ver:pres, être, [dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n))-1]), si(probable, adj-adj, probable, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(fasse, vs-ver:subp, faire, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(médiocre, adj-adj, médiocre, [dr(0,n,n)-1]), si(agent, nc-nom, agent, [n-1]), si(boursier, adj-adj, boursier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(250, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(diplômés, nc-nom, diplômé, [n-1]), si(universitaires, adj-adj, universitaire, [dl(0,n,n)-1]), si(font, v-ver:pres, faire, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(médiocres, adj-adj, médiocre, [dr(0,n,n)-1]), si(agents, nc-nom, agent, [n-1]), si(boursiers, adj-adj, boursier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(251, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(diplômé, nc-nom, diplômé, [n-1]), si(universitaire, adj-adj, universitaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(252, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(fera, v-ver:futu, faire, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(médiocre, adj-adj, médiocre, [dr(0,n,n)-1]), si(agent, nc-nom, agent, [n-1]), si(boursier, adj-adj, boursier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(253, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(gestionnaires, nc-nom, gestionnaire, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('APCOM', npp-abr, 'APCOM', [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(fonction, nc-nom, fonction, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(254, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(gestionnaire, nc-nom, gestionnaire, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('APCOM', npp-abr, 'APCOM', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(255, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(fonction, nc-nom, fonction, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(256, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(cadres, nc-nom, cadre, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(voitures, nc-nom, voiture, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(fonctions, nc-nom, fonction, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(257, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(cadre, nc-nom, cadre, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(258, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(fonction, nc-nom, fonction, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(259, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(260, Result) :-
      prob_parse([ si('Aucun', pro-pro:ind, 'Aucun', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(comptables, nc-nom, comptable, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(261, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(262, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(263, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(264, Result) :-
      prob_parse([ si('Des', det-prp:det, 'Des', [dr(0,np,n)-1]), si(comptables, nc-nom, comptable, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(265, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(266, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(267, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(268, Result) :-
      prob_parse([ si('Plusieurs', det-pro:ind, 'Plusieurs', [dr(0,np,n)-1]), si(comptables, nc-nom, comptable, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(269, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(270, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(comptable, nc-nom, comptable, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(271, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(272, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(autre, adj-adj, autre, [dr(0,n,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(273, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(274, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(275, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(276, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(277, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(278, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(les, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signés, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(279, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(280, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(281, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(utilisé, vpp-ver:pper, utiliser, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(282, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(283, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(284, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(285, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(286, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(étudiant, nc-nom, étudiant, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(287, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(étudiante, nc-nom, étudiant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(288, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(289, Result) :-
      prob_parse([ si('Toute', det-pro:ind, 'Toute', [dr(0,np,n)-1]), si(étudiante, nc-nom, étudiant, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(290, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(étudiante, adj-adj, étudiant, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(291, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(292, Result) :-
      prob_parse([ si('Aucune', det-pro:ind, 'Aucune', [dr(0,np,n)-1]), si(étudiante, nc-nom, étudiant, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(293, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(étudiante, adj-adj, étudiant, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(294, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(utilisé, vpp-ver:pper, utiliser, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(295, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(assisté, vpp-ver:pper, assister, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(296, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(présidée, vpp-ver:pper, présider, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(297, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(présidé, vpp-ver:pper, présider, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(298, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remis, vpp-ver:pper, remettre, [dr(0,dr(0,dl(0,np,s_ppart),pp_a),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(299, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si(leur, clo-pro:per, son, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(également, adv-adv, également, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(remis, vpp-ver:pper, remettre, [dr(0,dr(0,dl(0,np,s_ppart),pp_a),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(facture, nc-nom, facture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(300, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,s,s)-1]), si(elle, cls-pro:per, lui, [np-1]), si(leur, clo-pro:per, son, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remis, vpp-ver:pper, remettre, [dr(0,dr(0,dl(0,np,s_ppart),pp_a),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(projet, nc-nom, projet, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(301, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remis, vpp-ver:pper, remettre, [dr(0,dr(0,dl(0,np,s_ppart),pp_a),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(facture, nc-nom, facture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(projet, nc-nom, projet, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(302, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(comité, nc-nom, comité, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(président, nc-nom, président, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(303, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(désigné, vpp-ver:pper, désigner, [dr(0,dl(0,np,s_pass),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(304, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(comité, nc-nom, comité, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(président, nc-nom, président, [n-1]), si(désigné, vpp-ver:pper, désigner, [dr(0,dl(0,n,n),pp_par)-1]), si(par, p-prp, par, [dr(0,pp_par,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(membres, nc-nom, membre, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(comité, nc-nom, comité, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(305, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(envoyé, vpp-ver:pper, envoyer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(dont, prorel-pro:rel, dont, [dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,pp_de))))-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(besoin, nc-nom, besoin, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(306, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(sur, p-prp, sur, [dr(0,pp,np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(bureau, nc-nom, bureau, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(307, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si('ITEL', npp-abr, 'ITEL', [n-1]), si(sur, p-prp, sur, [dr(0,dl(1,s,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(bureau, nc-nom, bureau, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(308, Result) :-
      prob_parse([ si('Deux', pro-num, 'Deux', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(dix, adj-num, dix, [dr(0,n,n)-1]), si(machines, nc-nom, machine, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(manquantes, adj-adj, manquante, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(309, Result) :-
      prob_parse([ si('Elles', cls-pro:per, 'Elles', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(retirées, vpp-ver:pper, retirer, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(310, Result) :-
      prob_parse([ si('Deux', det-num, 'Deux', [dr(0,np,n)-1]), si(machines, nc-nom, machine, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(retirées, vpp-ver:pper, retirer, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(311, Result) :-
      prob_parse([ si('Deux', pro-num, 'Deux', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(dix, adj-num, dix, [dr(0,n,n)-1]), si(machines, nc-nom, machine, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(manquantes, adj-adj, manquante, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(312, Result) :-
      prob_parse([ si('Elles', cls-pro:per, 'Elles', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(retirées, vpp-ver:pper, retirer, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(313, Result) :-
      prob_parse([ si('Huit', det-num, 'Huit', [dr(0,np,n)-1]), si(machines, nc-nom, machine, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(retirées, vpp-ver:pper, retirer, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(314, Result) :-
      prob_parse([ si('Deux', pro-num, 'Deux', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(dix, adj-num, dix, [dr(0,n,n)-1]), si(machines, nc-nom, machine, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(manquantes, adj-adj, manquante, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(315, Result) :-
      prob_parse([ si('Elles', cls-pro:per, 'Elles', [np-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(toutes, adv-adv, tout, [dl(1,s,s)-1]), si(là, adv-adv, là, [np-1]), si(hier, adv-adv, hier, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(316, Result) :-
      prob_parse([ si('Dix', det-num, 'Dix', [dr(0,np,n)-1]), si(machines, nc-nom, machine, [n-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),np)-1]), si(là, adv-adv, là, [np-1]), si(hier, adv-adv, hier, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(317, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(machine, nc-nom, machine, [n-1]), si(mardi, nc-nom, mardi, [dl(1,s,s)-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(machine, nc-nom, machine, [n-1]), si(mercredi, nc-nom, mercredi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(318, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si(les, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(mises, vpp-ver:pper, mettre, [dr(0,dr(0,dl(0,np,s_ppart),pp),np)-1]), si(dans, p-prp, dans, [dr(0,pp,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(hall, nc-nom, hall, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(319, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(mis, vpp-ver:pper, mettre, [dr(0,dr(0,dl(0,np,s_ppart),pp),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(machines, nc-nom, machine, [n-1]), si(dans, p-prp, dans, [dr(0,pp,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(hall, nc-nom, hall, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(320, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(collègues, nc-nom, collègue, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allés, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(321, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détesté, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(322, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(collègues, nc-nom, collègue, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détesté, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(323, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(collègues, nc-nom, collègue, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allés, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(324, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détestée, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(325, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détesté, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(326, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(collègues, nc-nom, collègue, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allés, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(327, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détestée, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(328, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(détesté, vpp-ver:pper, détester, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(329, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(département, nc-nom, département, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(ligne, nc-nom, ligne, [n-1]), si(dédiée, vpp-ver:pper, dédier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(330, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si(les, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(louent, v-ver:pres, louer, [dr(0,dl(0,np,s),np)-1]), si(chez, p-prp, chez, [dr(0,dl(1,s,s),np)-1]), si('BT', npp-abr, 'BT', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(331, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(département, nc-nom, département, [n-1]), si(loue, v-ver:pres, louer, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(ligne, nc-nom, ligne, [n-1]), si(chez, p-prp, chez, [dr(0,dl(1,s,s),np)-1]), si('BT', npp-abr, 'BT', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(332, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(département, nc-nom, département, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(ligné, nc-nom, ligné, [n-1]), si(dédiée, vpp-ver:pper, dédier, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(333, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(service, nc-nom, service, [n-1]), si(commercial, adj-adj, commercial, [dl(0,n,n)-1]), si(la, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(loue, v-ver:pres, louer, [dr(0,dl(0,np,s),np)-1]), si(chez, p-prp, chez, [dr(0,dl(1,s,s),np)-1]), si('BT', npp-abr, 'BT', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(334, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(service, nc-nom, service, [n-1]), si(commercial, adj-adj, commercial, [dl(0,n,n)-1]), si(loue, v-ver:pres, louer, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(ligne, nc-nom, ligne, [n-1]), si(chez, p-prp, chez, [dr(0,dl(0,n,n),np)-1]), si('BT', npp-abr, 'BT', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(335, Result) :-
      prob_parse([ si('GFI', npp-abr, 'GFI', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(plusieurs, det-pro:ind, plusieurs, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(336, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(les, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(entretient, v-ver:pres, entretenir, [dr(0,dl(0,np,s),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(337, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(entretient, v-ver:pres, entretenir, [dr(0,dl(0,np,s),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si(que, prorel-pro:rel, que, [dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,np))))-1]), si('GFI', npp-abr, 'GFI', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(338, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(client, nc-nom, client, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(celui, pro-pro:dem, celui, [np-1]), si('-ci', adv-adv, '-ci', [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(339, Result) :-
      prob_parse([ si('MFI', npp-abr, 'MFI', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(client, nc-nom, client, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(exactement, adv-adv, exactement, [dr(0,np,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(340, Result) :-
      prob_parse([ si('MFI', npp-abr, 'MFI', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(341, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(client, nc-nom, client, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(celui, pro-pro:dem, celui, [np-1]), si('-ci', adv-adv, '-ci', [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(342, Result) :-
      prob_parse([ si('MFI', npp-abr, 'MFI', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(client, nc-nom, client, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(plusieurs, det-pro:ind, plusieurs, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(343, Result) :-
      prob_parse([ si('MFI', npp-abr, 'MFI', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(344, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(cadre, nc-nom, cadre, [n-1]), si(exécutif, adj-adj, exécutif, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si(portable, adj-adj, portable, [dl(0,n,n)-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(amené, vpp-ver:pper, amener, [dr(0,dl(0,np,s_ppart),np)-1]), si(pour, p-prp, pour, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si(prendre, vinf-ver:infi, prendre, [dr(0,dl(0,np,s_inf),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(notes, nc-nom, note, [n-1]), si(lors, adv-adv, lors, [dr(0,dl(1,s,s),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(345, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(cadre, nc-nom, cadre, [n-1]), si(exécutive, adj-adj, exécutif, [dl(0,n,n)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si(portables, adj-adj, portable, [dl(0,n,n)-1]), si(différents, adj-adj, différent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(346, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(amené, vpp-ver:pper, amener, [dr(0,dl(0,np,s_ppart),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si(portables, adj-adj, portable, [dl(0,n,n)-1]), si(lors, adv-adv, lors, [dr(0,dl(1,s,s),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(347, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(100, det-num, 100, [dr(0,np,n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(348, Result) :-
      prob_parse([ si('ICM', npp-abr, 'ICM', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(une, nc-num, un, [n-1]), si(de, p-prp, de, [dr(0,dl(0,np,np),np)-1]), si(ces, det-pro:dem, ce, [dr(0,np,n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(150, det-num, 150, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(349, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(aucun, pro-pro:ind, aucun, [np-1]), si(de, p-prp, de, [dr(0,dl(0,np,np),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(350, Result) :-
      prob_parse([ si('Chacune', pro-pro:ind, 'Chacune', [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(99, adj-num, 99, [dr(0,n,n)-1]), si(autres, adj-adj, autre, [dr(0,n,n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(351, Result) :-
      prob_parse([ si('Elles', cls-pro:per, 'Elles', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(contrats, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(ceux, pro-pro:dem, celui, [np-1]), si('-ci', adv-adv, '-ci', [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(352, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(plupart, nc-nom, plupart, [dr(0,n,pp_de)-1]), si(des, p+d-prp:det, de, [dr(0,pp_de,n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(possèdent, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(maintenance, nc-nom, maintenance, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(celui, pro-pro:dem, celui, [np-1]), si('-ci', adv-adv, '-ci', [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(353, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,n,n),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(page, nc-nom, page, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(couverture, nc-nom, couverture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(354, Result) :-
      prob_parse([ si('R-95-103', npp-nam, 'R-95-103', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(355, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(page, nc-nom, page, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(couverture, nc-nom, couverture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(356, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(page, nc-nom, page, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(couverture, nc-nom, couverture, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('R-95-103', npp-num, 'R-95-103', [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(357, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(chef, nc-nom, chef, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(est, v-ver:pres, être, [dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart)))-1]), si(accordé, vpp-ver:pper, accorder, [dr(0,dl(0,cl_r,dl(0,np,s_ppart)),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(forte, adj-adj, fort, [dr(0,n,n)-1]), si(augmentation, nc-nom, augmentation, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(358, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(chef, nc-nom, chef, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(accordé, vpp-ver:pper, accorder, [dr(0,dl(0,np,s_ppart),np)-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(0,box(0,np))),dr(0,s,np)),dr(0,s,dia(0,box(0,np))))-1]), si(il, cls-pro:per, il, [np-1]), si(lui, clo-pro:per, luire, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(accordé, vpp-ver:pper, accorder, [dr(0,dr(0,dl(0,np,s_pass),pp_a),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(augmentation, nc-nom, augmentation, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(359, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(était, v-ver:impf, être, [dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart)))-1]), si(blessé, vpp-ver:pper, blesser, [dl(0,cl_r,dl(0,np,s_ppart))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(360, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(blessé, vpp-ver:pper, blesser, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(361, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(était, v-ver:impf, être, [dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart)))-1]), si(blessé, vpp-ver:pper, blesser, [dl(0,cl_r,dl(0,np,s_ppart))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(362, Result) :-
      prob_parse([ si('Quelqu\'un', pro-pro:rel, 'Quelqu\'un', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(blessé, vpp-ver:pper, blesser, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(363, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(364, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(365, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(366, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(367, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(368, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(quatre, det-num, quatre, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(369, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(quatre, det-num, quatre, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(370, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(quatre, det-num, quatre, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(371, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(372, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(quatre, det-num, quatre, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(373, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(quatre, det-num, quatre, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(374, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,np,np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(à, p-prp, à, [dr(0,dl(0,np,np),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(375, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(376, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(377, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(va, v-ver:pres, aller, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(le, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(faire, vinf-ver:infi, faire, [dr(0,dl(0,np,s_inf),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(378, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(parlera, v-ver:futu, parler, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(379, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(lundi, nc-nom, lundi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(380, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pas, adv-adv, pas, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(381, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(lundi, nc-nom, lundi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(382, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si('-t-il', cls-pro:per, '-t-il', [dl(1,s,s)-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('?', ponct-pun, '?', [dl(0,s,txt)-1])], Result).
sent(383, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(384, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(385, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(386, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(étudiants, nc-nom, étudiant, [n-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(387, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(étudiants, nc-nom, étudiant, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(388, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(389, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(390, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Berlin', npp-nam, 'Berlin', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(391, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Berlin', npp-nam, 'Berlin', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(392, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Berlin', npp-nam, 'Berlin', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(393, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Berlin', npp-nam, 'Berlin', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(394, Result) :-
	prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(va, v-ver:pres, aller, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(étudiants, nc-nom, étudiant, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(395, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(étudiants, nc-nom, étudiant, [n-1]), si(vont, v-ver:pres, aller, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(396, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(397, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, p-prp, en, [dr(0,dl(0,np,np),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(398, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(allé, vpp-ver:pper, aller, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(train, nc-nom, train, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(399, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(400, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(401, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(402, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(403, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(404, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(que, prorel-pro:rel, que, [dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,np))))-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possèdent, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(405, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(406, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(bleue, nc-nom, bleu, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(407, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(bleue, adj-adj, bleu, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(408, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(409, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(bleue, nc-nom, bleu, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(410, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(411, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(412, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(rapide, nc-adj, rapide, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(413, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(414, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(415, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(rapide, nc-adj, rapide, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(416, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(417, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(418, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(rapide, nc-adj, rapide, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(419, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(420, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(421, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(lente, nc-adj, lent, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(422, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(rouge, adj-adj, rouge, [dl(0,n,n)-1]), si(et, cc-kon, et, [dr(0,dl(0,dl(0,n,n),dl(0,n,n)),dl(0,n,n))-1]), si(lente, adj-adj, lent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(423, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,n,n)),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(papier, nc-nom, papier, [n-1]), si(accepté, vpp-ver:pper, accepter, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(424, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(sait, v-ver:pres, savoir, [dr(0,dl(0,np,s),s_whq)-1]), si(pas, adv-adv, pas, [dl(1,s,s)-1]), si(pourquoi, advwh-adv, pourquoi, [s_whq-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(425, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(sait, v-ver:pres, savoir, [dr(0,dl(0,np,s),s_whq)-1]), si(pourquoi, advwh-adv, pourquoi, [dr(0,s_whq,s)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,n,n)),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(papier, nc-nom, papier, [n-1]), si(accepté, vpp-ver:pper, accepter, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(426, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(427, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,dr(0,s,s),dr(0,s,s))-1]), si(à, p-prp, à, [dr(0,dr(0,s,s),np)-1]), si('Suzanne', npp-nam, 'Suzanne', [np-1]), si('.', ponct-pun, '.', [dl(0,dr(0,s,s),txt)-1])], Result).
sent(428, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Suzanne', npp-nam, 'Suzanne', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(429, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(430, Result) :-
      prob_parse([ si('Vendredi', nc-nom, 'Vendredi', [n-1]), si('.', ponct-pun, '.', [dl(0,n,txt)-1])], Result).
sent(431, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(vendredi, nc-nom, vendredi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(432, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(jeudi, nc-nom, jeudi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(433, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,dr(0,s,s),dr(0,s,s))-1]), si(vendredi, nc-nom, vendredi, [dr(0,s,s)-1]), si('.', ponct-pun, '.', [dl(0,dr(0,s,s),txt)-1])], Result).
sent(434, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(vendredi, nc-nom, vendredi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(435, Result) :-
      prob_parse([ si('Vingt', det-num, 'Vingt', [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(au, p+d-prp:det, au, [dr(0,dl(1,s,s),n)-1]), si(service, nc-nom, service, [n-1]), si(commercial, adj-adj, commercial, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(436, Result) :-
      prob_parse([ si('Mais', cc-kon, 'Mais', [dr(0,np,np)-1]), si(seulement, adv-adv, seulement, [dr(0,np,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(437, Result) :-
      prob_parse([ si('Deux', det-num, 'Deux', [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(au, p+d-prp:det, au, [dr(0,dl(1,s,s),n)-1]), si(service, nc-nom, service, [n-1]), si(commercial, adj-adj, commercial, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(438, Result) :-
      prob_parse([ si('Cinq', det-num, 'Cinq', [dr(0,np,n)-1]), si(hommes, nc-nom, homme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(439, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,np,np)-1]), si('quarante-cinq', det-num, 'quarante-cinq', [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(440, Result) :-
      prob_parse([ si('Quarante-cinq', det-num, 'Quarante-cinq', [dr(0,np,n)-1]), si(femmes, nc-nom, femme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(441, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(442, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(ait, vs-ver:subp, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(443, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(444, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(ait, vs-ver:subp, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(trouvé, vpp-ver:pper, trouver, [dr(0,dl(0,np,s_ppart),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(445, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(savoir, vinf-ver:infi, savoir, [dr(0,dl(0,np,s_inf),s_whq)-1]), si(combien, advwh-adv, combien, [dr(0,dr(0,s_whq,dl(0,np,s)),pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,n)-1]), si(hommes, nc-nom, homme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(446, Result) :-
      prob_parse([ si('Et', cc-kon, 'Et', [dr(0,dr(0,s,s),pp)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(femmes, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,dr(0,s,s),txt)-1])], Result).
sent(447, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(savoir, vinf-ver:infi, savoir, [dr(0,dl(0,np,s_inf),s_whq)-1]), si(combien, advwh-adv, combien, [dr(0,dr(0,s_whq,dl(0,np,s)),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(femmes, nc-nom, femme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(448, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(savoir, vinf-ver:infi, savoir, [dr(0,dl(0,np,s_inf),s_whq)-1]), si(combien, advwh-adv, combien, [dr(0,dr(0,s_whq,dl(0,np,s)),pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,n)-1]), si(hommes, nc-nom, homme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si(',', ponct-pun, ',', [let-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si(lesquels, prorel-pro:rel, lequel, [dr(0,s_whq,dl(0,np,s))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(449, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(veut, v-ver:pres, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(savoir, vinf-ver:infi, savoir, [dr(0,dl(0,np,s_inf),s_whq)-1]), si(quels, detwh-pro:rel, quel, [dr(0,dr(0,s_whq,dl(0,np,s)),n)-1]), si(hommes, nc-nom, homme, [n-1]), si(travaillent, v-ver:pres, travailler, [dl(0,np,s)-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(partiel, adj-adj, partiel, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(450, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(ceux, pro-pro:dem, celui, [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,dr(0,pp,np),dl(0,np,np)),dr(0,s,dia(1,box(1,pp))))-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(451, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(452, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(453, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(ceux, pro-pro:dem, celui, [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,dr(0,pp,np),dl(0,np,np)),dr(0,s,dia(1,box(1,pp))))-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(454, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(455, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(456, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(457, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(458, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(459, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(460, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(461, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(462, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Pierre', npp-nam, 'Pierre', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(463, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dit, vpp-ver:pper, dire, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Pierre', npp-nam, 'Pierre', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(464, Result) :-
      prob_parse([ si('Si', cs-kon, 'Si', [dr(0,s,s)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(alors, adv-adv, alors, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(465, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(466, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(467, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(voulait, v-ver:impf, vouloir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(acheter, vinf-ver:infi, acheter, [dr(0,dl(0,np,s_inf),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(468, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(acheté, vpp-ver:pper, acheter, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(469, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(devait, v-ver:impf, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(acheter, vinf-ver:infi, acheter, [dr(0,dl(0,np,s_inf),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(470, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(acheté, vpp-ver:pper, acheter, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(voiture, nc-nom, voiture, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(471, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(472, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(473, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(474, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(475, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(476, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(477, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(478, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(479, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(480, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(481, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(482, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(483, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(également, adv-adv, également, [dl(0,np,np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(484, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(prétendu, vpp-ver:pper, prétendre, [dr(0,dl(0,np,s_ppart),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(chiffré, vpp-ver:pper, chiffrer, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(proposition, nc-nom, proposition, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(485, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(homme, nc-nom, homme, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(486, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(487, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(488, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(homme, nc-nom, homme, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(489, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dl(0,np,s)))),dr(0,s,box(1,dia(1,dl(0,np,s))))),dr(0,s,dia(1,box(1,dl(0,np,s)))))-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(aussi, adv-adv, aussi, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(490, Result) :-
      prob_parse([ si('Marie', npp-nam, 'Marie', [np-1]), si(représente, v-ver:pres, représenter, [dr(0,dl(0,np,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Jean', npp-nam, 'Jean', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(491, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dr(0,dl(0,np,s_ppart),s_q),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(ils, cls-pro:per, il, [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(492, Result) :-
      prob_parse([ si('S\'', cs-kon, 'S\'', [dr(0,dr(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(ensemble, adv-adv, ensemble, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(493, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dr(0,dl(0,np,s_ppart),s_q),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(ils, cls-pro:per, il, [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(494, Result) :-
      prob_parse([ si('S\'', cs-kon, 'S\'', [dr(0,dr(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,s,s)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(495, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dr(0,dl(0,np,s_ppart),s_q),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(ils, cls-pro:per, il, [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(496, Result) :-
      prob_parse([ si('S\'', cs-kon, 'S\'', [dr(0,dr(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,s,s)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(497, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dr(0,dl(0,np,s_ppart),s_q),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(ils, cls-pro:per, il, [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(498, Result) :-
      prob_parse([ si('S\'', cs-kon, 'S\'', [dr(0,dr(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,s,s)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(499, Result) :-
      prob_parse([ si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dr(0,dl(0,np,s_ppart),s_q),pp_a)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(ils, cls-pro:per, il, [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),pp))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),pp)))))-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(500, Result) :-
      prob_parse([ si('S\'', cs-kon, 'S\'', [dr(0,dr(0,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(patron, nc-nom, patron, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Franck', npp-nam, 'Franck', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a)-1]), si(ensemble, adv-adv, ensemble, [dl(1,dl(0,np,s),dl(0,np,s))-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(suggéré, vpp-ver:pper, suggérer, [dr(0,dl(0,np,s_pass),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Charles', npp-nam, 'Charles', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(femme, nc-nom, femme, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('Alain', npp-nam, 'Alain', [np-1]), si(devraient, v-ver:cond, devoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(se, clr-pro:per, se, [cl_r-1]), si(rendre, vinf-ver:infi, rendre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp)-1]), si(ensemble, adv-adv, ensemble, [dl(1,s,s)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(501, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(avocat, nc-nom, avocat, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(vérificateur, nc-nom, vérificateur, [n-1]), si(aux, p+d-prp:det, à, [dr(0,dl(0,n,n),n)-1]), si(comptes, nc-nom, compte, [n-1]), si(également, adv-adv, également, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(502, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,np,np),n)-1]), si(fait, nc-nom, fait, [n-1]), si(',', ponct-pun, ',', [let-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(avocat, nc-nom, avocat, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,np,txt)-1])], Result).
sent(503, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),dl(0,cl_y,dl(0,np,s_ppart)))-1]), si(eu, vpp-ver:pper, avoir, [dr(0,dl(0,cl_y,dl(0,np,s_ppart)),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(vérificateur, nc-nom, vérificateur, [n-1]), si(aux, p+d-prp:det, à, [dr(0,dl(0,n,n),n)-1]), si(comptes, nc-nom, compte, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(504, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(véritable, adj-adj, véritable, [dr(0,n,n)-1]), si(diamant, nc-nom, diamant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(505, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(possède, v-ver:pres, posséder, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(diamant, nc-nom, diamant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(506, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ancien, adj-adj, ancien, [dr(0,n,n)-1]), si(étudiant, nc-nom, étudiant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(507, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(étudiant, nc-nom, étudiant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(508, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [let-1]), si(est, v-ver:pres, être, [np-1]), si(un, det-det:art, un, [dr(0,dl(0,np,s),np)-1]), si(remarquable, adj-adj, remarquable, [dr(0,np,n)-1]), si(ancien, adj-adj, ancien, [dr(0,n,n)-1]), si(étudiant, nc-nom, étudiant, [dr(0,n,n)-1]), si('.', ponct-pun, '.', [n-1])], Result).
sent(509, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(remarquable, adj-adj, remarquable, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(510, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ancien, adj-adj, ancien, [dr(0,n,n)-1]), si(remarquable, adj-adj, remarquable, [dr(0,n,n)-1]), si(étudiant, nc-nom, étudiant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(511, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(remarquable, adj-adj, remarquable, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(512, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ancien, adj-adj, ancien, [dr(0,n,n)-1]), si(remarquable, adj-adj, remarquable, [dr(0,n,n)-1]), si(étudiant, nc-nom, étudiant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(513, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(étudiant, adj-adj, étudiant, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(514, Result) :-
      prob_parse([ si('Tout', adv-pro:ind, 'Tout', [dr(0,np,n)-1]), si(mammifère, nc-nom, mammifère, [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(515, Result) :-
      prob_parse([ si('Tout', det-pro:ind, 'Tout', [dr(0,np,n)-1]), si(mammifère, nc-nom, mammifère, [n-1]), si(quadrupède, adj-adj, quadrupède, [dl(0,n,n)-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(animal, nc-nom, animal, [n-1]), si(quadrupède, adj-adj, quadrupède, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(516, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(animal, nc-nom, animal, [n-1]), si(quadrupède, adj-adj, quadrupède, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(517, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(quadrupède, adj-adj, quadrupède, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(518, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(519, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(520, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(521, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(522, Result) :-
      prob_parse([ si('Fido', npp-nam, 'Fido', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(pas, adv-adv, pas, [dl(1,s,s)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(523, Result) :-
      prob_parse([ si('Fido', npp-nam, 'Fido', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(524, Result) :-
      prob_parse([ si('Fido', npp-nam, 'Fido', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(pas, adv-adv, pas, [dl(1,s,s)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(525, Result) :-
      prob_parse([ si('Fido', npp-nam, 'Fido', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(526, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(527, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(528, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(petit, adj-adj, petit, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Dumbo', npp-nam, 'Dumbo', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(529, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(530, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(531, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(grand, adj-adj, grand, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Dumbo', npp-nam, 'Dumbo', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(532, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(souris, nc-nom, souris, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(petits, adj-adj, petit, [dr(0,n,n)-1]), si(animaux, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(533, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(grande, adj-adj, grand, [dr(0,n,n)-1]), si(souris, nc-nom, souris, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(534, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(grand, adj-adj, grand, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(535, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(éléphants, nc-nom, éléphant, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(animaux, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(536, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(éléphant, nc-nom, éléphant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(537, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(animal, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(538, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(souris, nc-nom, souris, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, p-prp, de, [dr(0,np,n)-1]), si(petits, adj-adj, petit, [dr(0,n,n)-1]), si(animaux, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(539, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(éléphants, nc-nom, éléphant, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(grands, adj-adj, grand, [dr(0,n,n)-1]), si(animaux, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(540, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(grande, adj-adj, grand, [dr(0,n,n)-1]), si(souris, nc-nom, souris, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(541, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(petit, adj-adj, petit, [dr(0,n,n)-1]), si(éléphant, nc-nom, éléphant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(542, Result) :-
      prob_parse([ si('Dumbo', npp-nam, 'Dumbo', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(grand, adj-adj, grand, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Mickey', npp-nam, 'Mickey', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(543, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(souris, nc-nom, souris, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(de, p-prp, de, [dr(0,np,n)-1]), si(petits, adj-adj, petit, [dr(0,n,n)-1]), si(animaux, nc-nom, animal, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(544, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(grande, adj-adj, grand, [dr(0,n,n)-1]), si(souris, nc-nom, souris, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(545, Result) :-
      prob_parse([ si('Mickey', npp-nam, 'Mickey', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(petit, adj-adj, petit, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(546, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(547, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(548, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si(en, p-prp, en, [dr(0,dl(0,n,n),n)-1]), si(surpoids, nc-nom, surpoids, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si(en, p-prp, en, [dr(0,dl(0,n,n),n)-1]), si(surpoids, nc-nom, surpoids, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(549, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(550, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(551, Result) :-
      prob_parse([ si('Toutes', adv-adv, 'Toutes', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(autorités, nc-nom, autorité, [n-1]), si(judiciaires, adj-adj, judiciaire, [dl(0,n,n)-1]), si(compétentes, adj-adj, compétent, [dl(0,n,n)-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(professeurs, nc-nom, professeur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(droit, nc-nom, droit, [n-1]), si(compétents, adj-adj, compétent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(552, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(politicien, nc-nom, politicien, [n-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(gros, adj-adj, gros, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(553, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(gros, adj-adj, gros, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(554, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(politicien, nc-nom, politicien, [n-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(astucieux, adj-adj, astucieux, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(555, Result) :-
      prob_parse([ si('Jean', npp-nam, 'Jean', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(astucieux, adj-adj, astucieux, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('Guillaume', npp-nam, 'Guillaume', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(556, Result) :-
      prob_parse([ si('Kim', npp-nam, 'Kim', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(astucieuse, adj-adj, astucieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(557, Result) :-
      prob_parse([ si('Kim', npp-nam, 'Kim', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(astucieuse, adj-adj, astucieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(558, Result) :-
      prob_parse([ si('Kim', npp-nam, 'Kim', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(politicienne, nc-nom, politicien, [n-1]), si(astucieuse, adj-adj, astucieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(559, Result) :-
      prob_parse([ si('Kim', npp-nam, 'Kim', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(astucieuse, adj-adj, astucieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(560, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(561, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(562, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(563, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(564, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(565, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(566, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(567, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(568, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(569, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(lent, adj-adj, lent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(570, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(571, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(572, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(573, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(574, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(575, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(576, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(577, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(578, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(579, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(580, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(lent, adj-adj, lent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(581, Result) :-
      prob_parse([ si('L\'', det-det:art, 'L\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(582, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(583, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(584, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(585, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(lent, adj-adj, lent, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-XZ', npp-abr, 'ITEL-XZ', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(586, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(587, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(588, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(589, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(590, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(591, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(592, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(au, p+d-prp:det, au, [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(onze, det-num, onze, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(593, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(594, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(595, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(596, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(597, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(598, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(599, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(au, p+d-prp:det, au, [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(onze, det-num, onze, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(600, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(avec, p-prp, avec, [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(601, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(avec, p-prp, avec, [dr(0,dl(1,s,s),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(602, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(seul, adj-adj, seul, [dr(0,n,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(avec, p-prp, avec, [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(603, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(commande, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(604, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,dr(0,np,np),n)-1]), si(fois, nc-nom, fois, [n-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(605, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(606, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(vingt, det-num, vingt, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(607, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(608, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(609, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(610, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(611, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,np,s_q),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(612, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si(dix, det-num, dix, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(613, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(remporté, vpp-ver:pper, remporter, [dr(0,dl(0,np,s_ppart),np)-1]), si(au, p+d-prp:det, au, [dr(0,dr(0,np,np),n)-1]), si(moins, adv-adv, moins, [n-1]), si(onze, det-num, onze, [dr(0,np,n)-1]), si(commandes, nc-nom, commande, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(614, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(500, adj-num, 500, [dr(0,n,n)-1]), si('MIPS', npp-abr, 'MIPS', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(615, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('ITEL-ZX', npp-abr, 'ITEL-ZX', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(lent, adj-adj, lent, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(500, adj-num, 500, [dr(0,n,n)-1]), si('MIPS', npp-abr, 'MIPS', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(616, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(617, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vendu, vpp-ver:pper, vendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(3000, det-num, 3000, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si(plus, adv-adv, plus, [dr(0,np,s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(618, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vendu, vpp-ver:pper, vendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(exactement, adv-adv, exactement, [dr(0,np,np)-1]), si(2500, det-num, 2500, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(619, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vendu, vpp-ver:pper, vendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(5500, det-num, 5500, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(620, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,n,s_q),n)-1]), si(gros, adj-adj, gros, [dr(0,n,n)-1]), si(client, nc-nom, client, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(621, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(client, nc-nom, client, [n-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(gros, adj-adj, gros, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si(ne, adv-adv, ne, [dr(0,s,s)-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,s,dr(0,s,dia(1,box(1,np))))-1]), si(est, v-ver:pres, être, [dr(0,dr(0,s,np),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(622, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,n,s_q),n)-1]), si(gros, adj-adj, gros, [dr(0,n,n)-1]), si(client, nc-nom, client, [n-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(623, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(plus, adv-adv, plus, [dr(0,dr(0,n,n),dr(0,n,n))-1]), si(gros, adj-adj, gros, [dr(0,n,n)-1]), si(client, nc-nom, client, [n-1]), si(que, cs-kon, que, [dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,np))))-1]), si('n\'', adv-adv, 'n\'', [dr(0,s,s)-1]), si(a, v-ver:pres, avoir, [dr(0,dr(0,s,np),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(624, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(tous, adv-adv, tout, [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(625, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('ITEL-ZX', npp-abr, 'ITEL-ZX', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(626, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(627, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,np)-1]), si(un, pro-num, un, [np-1]), si(des, p+d-prp:det, de, [dr(0,dl(0,np,np),n)-1]), si(ordinateurs, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(628, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('ITEL-ZX', npp-abr, 'ITEL-ZX', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(629, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(630, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-nam, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('n\'', adv-adv, 'n\'', [np-1]), si(importe, v-ver:pres, importer, [dr(0,dl(0,np,s),np)-1]), si(quel, detwh-pro:rel, quel, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(631, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('ITEL-ZX', npp-abr, 'ITEL-ZX', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(ordinateur, nc-nom, ordinateur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(632, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(633, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZY', npp-nam, 'ITEL-ZY', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(634, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(635, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si(ou, cc-kon, ou, [dr(0,dl(0,np,np),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZY', npp-nam, 'ITEL-ZY', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(636, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si('PC-6082', npp-abr, 'PC-6082', [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(plus, adv-adv, plus, [dr(0,dr(0,dl(0,n,n),s_q),dl(0,n,n))-1]), si(rapide, adj-adj, rapide, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si('ITEL-ZX', npp-nam, 'ITEL-ZX', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(637, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(à, p-prp, à, [dr(0,dl(0,n,n),np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(638, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),np)-1]), si(actuellement, adv-adv, actuellement, [dl(1,s,s)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(639, Result) :-
      prob_parse([ si('Depuis', p-prp, 'Depuis', [dr(0,dr(0,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(installée, vpp-ver:pper, installer, [dr(0,dl(0,np,s_pass),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(640, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(641, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(642, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(développe, v-ver:pres, développer, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouvel, adj-adj, nouveau, [dr(0,n,n)-1]), si(éditeur, nc-nom, éditeur, [n-1]), si(depuis, p-prp, depuis, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(643, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(644, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(développé, vpp-ver:pper, développer, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(nouvel, adj-adj, nouveau, [dr(0,n,n)-1]), si(éditeur, nc-nom, éditeur, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(645, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(grossi, vpp-ver:pper, grossir, [dl(0,np,s_ppart)-1]), si(depuis, p-prp, depuis, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(646, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(647, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(grossi, vpp-ver:pper, grossir, [dl(0,np,s_ppart)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(648, Result) :-
      prob_parse([ si('Depuis', p-prp, 'Depuis', [dr(0,dr(0,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(649, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(650, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(651, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si(depuis, p-prp, depuis, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(652, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(653, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(654, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si(depuis, p-prp, depuis, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(655, Result) :-
      prob_parse([ si('Nous', cls-pro:per, 'Nous', [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(maintenant, adv-adv, maintenant, [dl(1,s,s)-1]), si(en, p-prp, en, [dr(0,pp,np)-1]), si(1996, pro-num, 1996, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(656, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(subi, vpp-ver:pper, subir, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(pertes, nc-nom, perte, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(657, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),n)-1]), si(mars, nc-nom, mars, [n-1]), si(1993, adj-num, 1993, [dl(0,n,n)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fondé, vpp-ver:pper, fonder, [dr(0,dl(0,np,s_ppart),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(658, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(existait, v-ver:impf, exister, [dl(0,np,s)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(659, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(conférence, nc-nom, conférence, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(débuté, vpp-ver:pper, débuter, [dl(0,np,s_ppart)-1]), si(le, det-det:art, le, [dr(0,dl(1,s,s),n)-1]), si(4, adj-num, 4, [dr(0,n,n)-1]), si('Juillet', nc-nom, 'Juillet', [n-1]), si(1994, adj-num, 1994, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(660, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(duré, vpp-ver:pper, durer, [dr(0,dl(0,np,s_ppart),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(jours, nc-nom, jour, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(661, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,np,n)-1]), si(conférence, nc-nom, conférence, [n-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(terminée, vpp-ver:pper, terminer, [dl(0,np,s_pass)-1]), si(le, det-det:art, le, [dr(0,dl(1,s,s),n)-1]), si(8, adj-num, 8, [dr(0,n,n)-1]), si(juillet, nc-nom, juillet, [n-1]), si(1994, adj-num, 1994, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(662, Result) :-
      prob_parse([ si('Hier', adv-adv, 'Hier', [dr(0,s,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(663, Result) :-
      prob_parse([ si('Aujourd\'hui', adv-adv, 'Aujourd\'hui', [dr(0,s,s)-1]), si(on, cls-pro:per, on, [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(samedi, nc-nom, samedi, [dr(0,n,n)-1]), si(14, adj-num, 14, [dr(0,n,n)-1]), si(juillet, nc-nom, juillet, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(664, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(un, det-det:art, un, [dr(0,dl(1,s,s),n)-1]), si(vendredi, nc-nom, vendredi, [n-1]), si(13, adj-num, 13, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(665, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-nom, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(666, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(667, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(668, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(669, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(670, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(671, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(fut, v-ver:simp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(672, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(673, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(674, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(675, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(676, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(677, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(678, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(679, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(680, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(681, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(682, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(683, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(684, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(685, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parti, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, vpp-ver:pper, partir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(686, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(687, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(688, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(ait, vs-ver:subp, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(689, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(690, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(691, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(692, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(693, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(modifié, vpp-ver:pper, modifier, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(eut, v-ver:simp, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fait, vpp-ver:pper, faire, [dr(0,dl(0,np,s_ppart),np)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(694, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(nageait, v-ver:impf, nager, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(695, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(nageait, v-ver:impf, nager, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(696, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(nageait, v-ver:impf, nager, [dl(0,np,s)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(nageât, v-ver:impf, nager, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(697, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(nageait, v-ver:impf, nager, [dl(0,np,s)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(nageait, v-ver:impf, nager, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(698, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(699, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(700, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(701, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(ait, vs-ver:subp, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(nagé, vpp-ver:pper, nager, [dl(0,np,s_ppart)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(rive, nc-nom, rive, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(702, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(703, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(704, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(705, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(fût, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(706, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(707, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(708, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(fût, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(709, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présent, adj-adj, présent, [dl(0,n,n)-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(présente, adj-adj, présent, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(710, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(711, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(712, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(fût, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(713, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(714, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(715, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(716, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(après, p-prp, après, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(717, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(fût, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(718, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(avant, p-prp, avant, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si(de, p-prp, de, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(perdre, vinf-ver:infi, perdre, [dr(0,dl(0,np,s_inf),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si('sang-froid', nc-nom, 'sang-froid', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(719, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si('sang-froid', nc-nom, 'sang-froid', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(720, Result) :-
      prob_parse([ si('Quand', cs-kon, 'Quand', [dr(0,dr(0,s,s),s)-1]), si(ils, cls-pro:per, il, [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(ouvert, vpp-ver:pper, ouvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si('M25', nc-abr, 'M25', [n-1]), si(',', ponct-pun, ',', [let-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(circulation, nc-nom, circulation, [n-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(augmenté, vpp-ver:pper, augmenter, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(721, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(habitait, v-ver:impf, habiter, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(722, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(723, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(premier, adj-num, premier, [dr(0,n,n)-1]), si(roman, nc-nom, roman, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(724, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(premier, adj-num, premier, [dr(0,n,n)-1]), si(roman, nc-nom, roman, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(725, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(roman, nc-nom, roman, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(726, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(727, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(roman, nc-nom, roman, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(728, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(roman, nc-nom, roman, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(729, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(dirigeait, v-ver:impf, diriger, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(730, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(la, clo-pro:per, le, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(dirigeait, v-ver:impf, diriger, [dr(0,dl(0,np,s),np)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(731, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(732, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si('l\'', clo-pro:per, 'l\'', [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découverte, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(733, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1991, pro-num, 1991, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(734, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(735, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(736, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(commencé, vpp-ver:pper, commencer, [dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(8, det-num, 8, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(matin, nc-nom, matin, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(737, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fini, vpp-ver:pper, finir, [dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(11, det-num, 11, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(matin, nc-nom, matin, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(738, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(739, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(740, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(741, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(742, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(743, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(744, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(745, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(746, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(747, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(découvrir, vinf-ver:infi, découvrir, [dr(0,dl(0,np,s_inf),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(748, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(749, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(750, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(beaucoup, adv-adv, beaucoup, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,n)-1]), si(nouvelles, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèces, nc-nom, espèce, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(751, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(découvrir, vinf-ver:infi, découvrir, [dr(0,dl(0,np,s_inf),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(nouvelles, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèces, nc-nom, espèce, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(752, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(dirigeait, v-ver:impf, diriger, [dr(0,dl(0,np,s),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(753, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(diriger, vinf-ver:infi, diriger, [dr(0,dl(0,np,s_inf),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(754, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(dirigeait, v-ver:impf, diriger, [dr(0,dl(0,np,s),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(755, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(passé, vpp-ver:pper, passer, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(diriger, vinf-ver:infi, diriger, [dr(0,dl(0,np,s_inf),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(756, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(dirigeait, v-ver:impf, diriger, [dr(0,dl(0,np,s),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(757, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dirigé, vpp-ver:pper, diriger, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(758, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(possédait, v-ver:impf, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaîne, nc-nom, chaîne, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(759, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(possédé, vpp-ver:pper, posséder, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaîne, nc-nom, chaîne, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(760, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(possédait, v-ver:impf, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaîne, nc-nom, chaîne, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(761, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(possédé, vpp-ver:pper, posséder, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaîne, nc-nom, chaîne, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(762, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(possédait, v-ver:impf, posséder, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaîne, nc-nom, chaîne, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(763, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(possédé, vpp-ver:pper, posséder, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(chaine, nc-nom, chaine, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(entreprises, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(764, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(765, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(an, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(766, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(767, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(an, nc-nom, an, [n-1]), si(exactement, adv-adv, exactement, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(768, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(769, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(habité, vpp-ver:pper, habiter, [dr(0,dl(0,np,s_ppart),np)-1]), si('Birmingham', npp-nam, 'Birmingham', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(770, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dirigé, vpp-ver:pper, diriger, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(771, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dirigé, vpp-ver:pper, diriger, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(an, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(772, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dirigé, vpp-ver:pper, diriger, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(773, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(dirigé, vpp-ver:pper, diriger, [dr(0,dl(0,np,s_ppart),np)-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(propre, adj-adj, propre, [dr(0,n,n)-1]), si(entreprise, nc-nom, entreprise, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(774, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(775, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(776, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(heures, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(777, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(778, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(nouvelle, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèce, nc-nom, espèce, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(779, Result) :-
      prob_parse([ si('None', et-nom, 'None', [np-1])], Result).
sent(780, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(nouvelles, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèces, nc-nom, espèce, [n-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(781, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),np)-1]), si(de, det-prp, de, [dr(0,np,n)-1]), si(nouvelles, adj-adj, nouveau, [dr(0,n,n)-1]), si(espèces, nc-nom, espèce, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(782, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(1994, pro-num, 1994, [np-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(envoyé, vpp-ver:pper, envoyer, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(avancement, nc-nom, avancement, [n-1]), si(tous, adv-adv, tout, [dr(0,dl(1,s,s),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(mois, nc-nom, mois, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(783, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(envoyé, vpp-ver:pper, envoyer, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(avancement, nc-nom, avancement, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(juillet, nc-nom, juillet, [n-1]), si(1994, adj-num, 1994, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(784, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(représentant, nc-nom, représentant, [n-1]), si(toutes, adv-adv, tout, [dr(0,dl(1,s,s),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(semaines, nc-nom, semaine, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(785, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(représentant, nc-nom, représentant, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,dr(0,pp,np),dl(0,n,n)),dr(0,s,dia(1,box(1,pp))))-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(écrit, vpp-ver:pper, écrire, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(toutes, adv-adv, tout, [dr(0,dl(1,s,s),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(semaines, nc-nom, semaine, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(786, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(partie, nc-nom, partir, [dr(0,dl(0,np,s_ppart),pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,n,n),n)-1]), si(quart, nc-nom, quart, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(787, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(taxi, nc-nom, taxi, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(gare, nc-nom, gare, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(attrapé, vpp-ver:pper, attraper, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(premier, adj-num, premier, [dr(0,n,n)-1]), si(train, nc-nom, train, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,n,n),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si('Luxembourg', npp-nam, 'Luxembourg', [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(788, Result) :-
      prob_parse([ si('None', et-nom, 'None', [np-1])], Result).
sent(789, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(perdu, vpp-ver:pper, perdre, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(fichiers, nc-nom, fichier, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(790, Result) :-
      prob_parse([ si('Ils', cls-pro:per, 'Ils', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(détruits, vpp-ver:pper, détruire, [dl(0,np,s_pass)-1]), si(quand, cs-kon, quand, [dr(0,dl(1,s,s),s)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(disque, nc-nom, disque, [n-1]), si(dur, adj-adj, dur, [dl(0,n,n)-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(planté, vpp-ver:pper, planter, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(791, Result) :-
      prob_parse([ si('None', et-nam, 'None', [np-1])], Result).
sent(792, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, à, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si(a, v-ver:pres, à, [dr(0,dl(1,s,s),np)-1]), si(cinq, det-num, cinq, [dr(0,np,n)-1]), si(heure, nc-nom, heure, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,n,n),n)-1]), si(quart, nc-nom, quart, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(793, Result) :-
      prob_parse([ si('Ensuite', adv-adv, 'Ensuite', [dr(0,s,s)-1]), si(',', ponct-pun, ',', [let-1]), si(elle, cls-pro:per, lui, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(taxi, nc-nom, taxi, [n-1]), si(pour, p-prp, pour, [dr(0,dl(1,s,s),dl(0,np,s_inf))-1]), si(aller, vinf-ver:infi, aller, [dr(0,dl(0,np,s_inf),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(gare, nc-nom, gare, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(794, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(maison, nc-nom, maison, [n-1]), si(avant, p-prp, avant, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si(de, p-prp, de, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(prendre, vinf-ver:infi, prendre, [dr(0,dl(0,np,s_inf),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(taxi, nc-nom, taxi, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si(aller, vinf-ver:infi, aller, [dr(0,dl(0,np,s_inf),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(gare, nc-nom, gare, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(795, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(rend, v-ver:pres, rendre, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(toujours, adv-adv, toujours, [dl(1,s,s)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(retard, nc-nom, retard, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(796, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si(',', ponct-pun, ',', [let-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(rendu, vpp-ver:pper, rendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(797, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(rendu, vpp-ver:pper, rendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(0,n,n),n)-1]), si(retard, nc-nom, retard, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(798, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(rend, v-ver:pres, rendre, [dr(0,dr(0,dl(0,np,s),pp),np)-1]), si(jamais, adv-adv, jamais, [dl(1,s,s)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(retard, nc-nom, retard, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(799, Result) :-
      prob_parse([ si('En', p-prp, 'En', [dr(0,dr(0,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si(',', ponct-pun, ',', [let-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(rendu, vpp-ver:pper, rendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(800, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(rendu, vpp-ver:pper, rendre, [dr(0,dl(0,np,s_ppart),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(rapports, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(retard, nc-nom, retard, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(801, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(arrivée, vpp-ver:pper, arriver, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(le, det-det:art, le, [dr(0,dl(1,s,s),n)-1]), si(5, adj-num, 5, [dr(0,n,n)-1]), si(mai, nc-nom, mai, [n-1]), si(1995, adj-num, 1995, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(802, Result) :-
      prob_parse([ si('Aujourd\'hui', adv-adv, 'Aujourd\'hui', [dr(0,s,s)-1]), si(',', ponct-pun, ',', [let-1]), si(nous, cls-pro:per, nous, [np-1]), si(sommes, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(15, adj-num, 15, [dr(0,n,n)-1]), si(mai, nc-nom, mai, [n-1]), si(1995, adj-num, 1995, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(803, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),pp)-1]), si(toujours, adv-adv, toujours, [dl(1,s,s)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1])], Result).
sent(804, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Paris', npp-nam, 'Paris', [np-1]), si(le, det-det:art, le, [dr(0,dl(1,s,s),n)-1]), si(7, adj-num, 7, [dr(0,n,n)-1]), si(mai, nc-nom, mai, [n-1]), si(1995, adj-num, 1995, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(805, Result) :-
      prob_parse([ si('Quand', cs-kon, 'Quand', [dr(0,dr(0,s,s),s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(arrivée, vpp-ver:pper, arriver, [dr(0,dl(0,np,s_ppart),pp_a)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Katmandou', npp-nam, 'Katmandou', [np-1]), si(',', ponct-pun, ',', [let-1]), si(elle, cls-pro:per, lui, [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voyagé, vpp-ver:pper, voyager, [dl(0,np,s_ppart)-1]), si(durant, p-prp, durer, [dr(0,dl(1,s,s),np)-1]), si(trois, det-num, trois, [dr(0,np,n)-1]), si(jours, nc-nom, jour, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(806, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(voyagé, vpp-ver:pper, voyager, [dl(0,np,s_ppart)-1]), si(la, det-det:art, le, [dr(0,dl(1,s,s),n)-1]), si(veille, nc-nom, veille, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(arrivée, nc-nom, arrivée, [n-1]), si(à, p-prp, à, [dr(0,dl(0,n,n),np)-1]), si('Katmandou', npp-nam, 'Katmandou', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(807, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(diplôme, nc-nom, diplôme, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(mars, nc-nom, mars, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(toujours, adv-adv, toujours, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(employé, vpp-ver:pper, employer, [dl(0,np,s_pass)-1]), si(depuis, adv-adv, depuis, [dl(1,s,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(808, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),pp)-1]), si(sans, p-prp, sans, [dr(0,pp,n)-1]), si(emploi, nc-nom, emploi, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(passé, nc-nom, passé, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(809, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),pp)-1]), si(sans, p-prp, sans, [dr(0,pp,n)-1]), si(emploi, nc-nom, emploi, [n-1]), si(un, det-det:art, un, [dr(0,dl(1,s,s),n)-1]), si(temps, nc-nom, temps, [n-1]), si(avant, p-prp, avant, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(obtenir, vinf-ver:infi, obtenir, [dr(0,dl(0,np,s_inf),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(diplôme, nc-nom, diplôme, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(810, Result) :-
      prob_parse([ si('Tous', adv-adv, 'Tous', [dr(0,np,np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(lu, vpp-ver:pper, lire, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(811, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(pas, adv-adv, pas, [dr(0,np,np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(lu, vpp-ver:pper, lire, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),n)-1]), si(même, adj-adj, même, [dr(0,n,n)-1]), si(temps, nc-nom, temps, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(812, Result) :-
      prob_parse([ si('Aucun', det-pro:ind, 'Aucun', [dr(0,np,n)-1]), si(représentant, nc-nom, représentant, [n-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(moins, adv-adv, moins, [dr(0,np,pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si('demi-journée', nc-nom, 'demi-journée', [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(lire, vinf-ver:infi, lire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(813, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si(16, det-num, 16, [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(814, Result) :-
      prob_parse([ si('Les', det-det:art, 'Les', [dr(0,np,n)-1]), si(représentants, nc-nom, représentant, [n-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(pris, vpp-ver:pper, prendre, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si('d\'', p-prp, 'd\'', [dr(0,pp_de,np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(semaine, nc-nom, semaine, [n-1]), si(pour, p-prp, pour, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(lire, vinf-ver:infi, lire, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(rapport, nc-nom, rapport, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(815, Result) :-
      prob_parse([ si('Pendant', p-prp, 'Pendant', [dr(0,dr(0,s,s),s_q)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(mettait, v-ver:impf, mettre, [dr(0,dr(0,dl(0,np,s),np),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,n)-1]), si(jour, nc-nom, jour, [n-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(programme, nc-nom, programme, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(entrée, vpp-ver:pper, entrer, [dl(0,np,s_ppart)-1]), si(et, cc-kon, et, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s))-1]), si(lui, clo-pro:per, luire, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a))))-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(parlé, vpp-ver:pper, parler, [dr(0,dr(0,dl(0,np,s_ppart),pp_de),pp_a)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(conseil, nc-nom, conseil, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),n)-1]), si(administration, nc-nom, administration, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(816, Result) :-
      prob_parse([ si('Elle', cls-pro:per, 'Elle', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(fini, vpp-ver:pper, finir, [dl(0,np,s_ppart)-1]), si(avant, p-prp, avant, [dr(0,dl(1,s,s),np)-1]), si(lui, pro-pro:per, luire, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(817, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(récit, nc-nom, récit, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Marie', npp-nam, 'Marie', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(duré, vpp-ver:pper, durer, [dl(0,np,s_ppart)-1]), si(aussi, adv-adv, aussi, [dr(0,dr(0,dl(1,s,s),s_q),dl(1,s,s))-1]), si(longtemps, adv-adv, longtemps, [dl(1,s,s)-1]), si(que, cs-kon, que, [dr(0,s_q,np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(mise, nc-nom, mise, [n-1]), si(à, p-prp, à, [dr(0,dl(0,n,n),n)-1]), si(jour, nc-nom, jour, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(programme, nc-nom, programme, [n-1]), si(par, p-prp, par, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(818, Result) :-
      prob_parse([ si('Avant', p-prp, 'Avant', [dr(0,dr(0,s,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(achète, v-ver:subp, acheter, [dr(0,dl(0,np,s),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(actuels, adj-adj, actuel, [dr(0,n,n)-1]), si(bureaux, nc-nom, bureau, [n-1]), si(',', ponct-pun, ',', [let-1]), si(ils, cls-pro:per, il, [np-1]), si(avaient, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(payé, vpp-ver:pper, payer, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(frais, nc-nom, frais, [n-1]), si(hypothécaires, adj-adj, hypothécaire, [dl(0,n,n)-1]), si(sur, p-prp, sur, [dr(0,dl(1,s,s),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(précédents, nc-nom, précédent, [n-1]), si(pendant, p-prp, pendant, [dr(0,dl(1,s,s),np)-1]), si(8, det-num, 8, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(819, Result) :-
      prob_parse([ si('Depuis', p-prp, 'Depuis', [dr(0,dr(0,s,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(acheté, vpp-ver:pper, acheter, [dr(0,dl(0,np,s_ppart),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(actuels, adj-adj, actuel, [dr(0,n,n)-1]), si(bureaux, nc-nom, bureau, [n-1]), si(',', ponct-pun, ',', [let-1]), si(ils, cls-pro:per, il, [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(payé, vpp-ver:pper, payer, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(intérêts, nc-nom, intérêt, [n-1]), si(hypothécaires, adj-adj, hypothécaire, [dl(0,n,n)-1]), si(depuis, p-prp, depuis, [dr(0,dl(1,s,s),np)-1]), si(plus, adv-adv, plus, [dr(0,np,pp_de)-1]), si(de, p-prp, de, [dr(0,pp_de,np)-1]), si(10, det-num, 10, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(820, Result) :-
      prob_parse([ si('APCOM', npp-abr, 'APCOM', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(payé, vpp-ver:pper, payer, [dr(0,dl(0,np,s_ppart),np)-1]), si(des, det-prp:det, un, [dr(0,np,n)-1]), si(frais, nc-nom, frais, [n-1]), si(hypothécaires, adj-adj, hypothécaire, [dl(0,n,n)-1]), si(pour, p-prp, pour, [dr(0,dl(1,s,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(total, nc-nom, total, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si(15, det-num, 15, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(ou, cc-kon, ou, [dr(0,dl(0,np,np),np)-1]), si(plus, adv-adv, plus, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(821, Result) :-
      prob_parse([ si('Lorsque', cs-kon, 'Lorsque', [dr(0,dr(0,s,s),s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(son, det-det:pos, son, [dr(0,np,n)-1]), si(emploi, nc-nom, emploi, [n-1]), si(à, p-prp, à, [dr(0,dl(1,s,s),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si('CIA', npp-abr, 'CIA', [n-1]), si(',', ponct-pun, ',', [let-1]), si(il, cls-pro:per, il, [np-1]), si(savait, v-ver:impf, savoir, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(il, cls-pro:per, il, [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(serait, v-ver:cond, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(jamais, adv-adv, jamais, [dl(1,s,s)-1]), si(autorisé, vpp-ver:pper, autoriser, [dr(0,dl(0,np,s_pass),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(mémoires, nc-nom, mémoire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(822, Result) :-
	prob_parse([ si('C\'', cls-pro:dem, 'C\'', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(fait, nc-nom, fait, [n-1]), si(que, cs-kon, que, [dr(0,dl(0,n,n),s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si('n\'', adv-adv, 'n\'', [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(pas, adv-adv, pas, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,dl(0,np,s)))),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,np,s),dia(0,box(0,dl(0,np,s)))))-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(sera, v-ver:futu, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(jamais, adv-adv, jamais, [dl(1,s,s)-1]), si(autorisé, vpp-ver:pper, autoriser, [dr(0,dl(0,np,s_pass),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(écrire, vinf-ver:infi, écrire, [dr(0,dl(0,np,s_inf),np)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(mémoires, nc-nom, mémoire, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(823, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),pp)-1]), si(deux, det-num, deux, [dr(0,dr(0,pp,pp),n)-1]), si(fois, nc-nom, fois, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Florence', npp-nam, 'Florence', [np-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(passé, nc-nom, passé, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(824, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(ira, v-ver:pres, aller, [dr(0,dl(0,np,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Florence', npp-nam, 'Florence', [np-1]), si(deux, det-num, deux, [dr(0,dr(0,dl(1,s,s),dl(1,s,s)),n)-1]), si(fois, nc-nom, fois, [n-1]), si(dans, p-prp, dans, [dr(0,dl(1,s,s),np)-1]), si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1]), si(année, nc-nom, année, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(vient, v-ver:pres, venir, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(825, Result) :-
      prob_parse([ si('Dans', p-prp, 'Dans', [dr(0,dr(0,s,s),np)-1]), si(deux, det-num, deux, [dr(0,np,n)-1]), si(ans, nc-nom, an, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(aura, v-ver:futu, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),pp)-1]), si(au, p+d-prp:det, au, [dr(0,dr(0,dl(1,s,s),dl(1,s,s)),n)-1]), si(moins, adv-adv, moins, [n-1]), si(4, det-num, 4, [dr(0,dl(1,s,s),n)-1]), si(fois, nc-nom, fois, [n-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si('Florence', npp-nam, 'Florence', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(826, Result) :-
      prob_parse([ si('La', det-det:art, 'La', [dr(0,dr(0,s,s),n)-1]), si(semaine, nc-nom, semaine, [n-1]), si(dernière, adj-adj, dernier, [dl(0,n,n)-1]), si(',', ponct-pun, ',', [let-1]), si(je, cls-pro:per, je, [np-1]), si(savais, v-ver:impf, savoir, [dr(0,dl(0,np,s),s_q)-1]), si(déjà, adv-adv, déjà, [dl(1,s,s)-1]), si(',', ponct-pun, ',', [let-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(un, det-det:art, un, [dr(0,dr(0,s,s),n)-1]), si(mois, nc-nom, mois, [n-1]), si(après, adv-adv, après, [dl(0,n,n)-1]), si(',', ponct-pun, ',', [let-1]), si(quand, cs-kon, quand, [dr(0,dr(0,s,s),s)-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(aura, v-ver:futu, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(découvert, vpp-ver:pper, découvrir, [dr(0,dl(0,np,s_ppart),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(elle, cls-pro:per, lui, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(dupée, vpp-ver:pper, duper, [dl(0,np,s_pass)-1]), si(',', ponct-pun, ',', [let-1]), si(elle, cls-pro:per, lui, [np-1]), si(sera, v-ver:futu, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(furieuse, adj-adj, furieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(827, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(est, v-ver:pres, être, [dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n))-1]), si(vrai, adj-adj, vrai, [dl(0,n,n)-1]), si(que, cs-kon, que, [dr(0,s_q,s)-1]), si(',', ponct-pun, ',', [let-1]), si(dans, p-prp, dans, [dr(0,dr(0,s,s),np)-1]), si(quelques, det-pro:ind, quelque, [dr(0,np,n)-1]), si(semaines, nc-nom, semaine, [n-1]), si(',', ponct-pun, ',', [let-1]), si('Dupont', npp-nam, 'Dupont', [np-1]), si(découvrira, v-ver:pres, découvrir, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(elle, cls-pro:per, lui, [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(été, vpp-ver:pper, être, [dr(0,dl(0,np,s_ppart),dl(0,np,s_pass))-1]), si(trompée, vpp-ver:pper, tromper, [dl(0,np,s_pass)-1]), si(et, cc-kon, et, [dr(0,dl(0,s,s),s)-1]), si(elle, cls-pro:per, lui, [np-1]), si(sera, v-ver:futu, être, [dr(0,dl(0,np,s),dl(0,n,n))-1]), si(furieuse, adj-adj, furieux, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(828, Result) :-
      prob_parse([ si('Personne', pro-pro:ind, 'Personne', [np-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,np,np),dl(0,np,s))-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(joue, v-ver:pres, joue, [dl(0,np,s)-1]), si(sérieusement, adv-adv, sérieusement, [dl(1,s,s)-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(arrête, v-ver:pres, arrêter, [dl(0,cl_r,dl(0,np,s))-1]), si(avant, p-prp, avant, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),dl(0,np,s_pass))-1]), si(ruiné, vpp-ver:pper, ruiner, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(829, Result) :-
      prob_parse([ si('Personne', pro-pro:ind, 'Personne', [np-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(peut, v-ver:pres, pouvoir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(jouer, vinf-ver:infi, jouer, [dl(0,np,s_inf)-1]), si(quand, cs-kon, quand, [dr(0,dl(1,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(ruiné, vpp-ver:pper, ruiner, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(830, Result) :-
      prob_parse([ si('Quelqu\'un', pro-pro:rel, 'Quelqu\'un', [np-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,np,np),dl(0,np,s))-1]), si(commence, v-ver:pres, commencer, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(jouer, vinf-ver:infi, jouer, [dl(0,np,s_inf)-1]), si(sérieusement, adv-adv, sérieusement, [dl(1,s,s)-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(arrête, v-ver:pres, arrêter, [dl(0,cl_r,dl(0,np,s))-1]), si(que, adv-adv, que, [dr(0,dl(1,s,s),dl(1,s,s))-1]), si(quand, cs-kon, quand, [dr(0,dl(1,s,s),s)-1]), si(il, cls-pro:per, il, [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(ruiné, vpp-ver:pper, ruiner, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(831, Result) :-
      prob_parse([ si('Personne', pro-pro:ind, 'Personne', [np-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,np,np),dl(0,np,s))-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(commence, v-ver:pres, commencer, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(jouer, vinf-ver:infi, jouer, [dl(0,np,s_inf)-1]), si(sérieusement, adv-adv, sérieusement, [dl(1,s,s)-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(arrête, v-ver:pres, arrêter, [dl(0,cl_r,dl(0,np,s))-1]), si(avant, p-prp, avant, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(être, vinf-ver:infi, être, [dr(0,dl(0,np,s_inf),dl(0,np,s_pass))-1]), si(ruiné, vpp-ver:pper, ruiner, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(832, Result) :-
      prob_parse([ si('Quelqu\'un', pro-pro:rel, 'Quelqu\'un', [np-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,np,np),dl(0,np,s))-1]), si(commence, v-ver:pres, commencer, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(jouer, vinf-ver:infi, jouer, [dl(0,np,s_inf)-1]), si(sérieusement, adv-adv, sérieusement, [dl(1,s,s)-1]), si(continue, v-ver:pres, continuer, [dl(0,np,s)-1]), si('jusqu\'', p-prp, 'jusqu\'', [dr(0,dl(1,s,s),pp)-1]), si(à, p-prp, à, [dr(0,pp_a,np)-1]), si(ce, pro-pro:dem, ce, [np-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,dl(0,np,np),s)-1]), si(il, cls-pro:per, il, [np-1]), si(soit, vs-ver:subp, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(ruiné, vpp-ver:pper, ruiner, [dl(0,np,s_pass)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(833, Result) :-
      prob_parse([ si('Aucune', det-pro:ind, 'Aucune', [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si(qui, prorel-pro:rel, qui, [dr(0,dl(0,n,n),dl(0,np,s))-1]), si(dort, v-ver:pres, dormir, [dl(0,np,s)-1]), si(ne, adv-adv, ne, [dr(0,dl(0,np,s),dl(0,np,s))-1]), si(sait, v-ver:pres, savoir, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(elle, cls-pro:per, lui, [np-1]), si(dort, v-ver:pres, dormir, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(834, Result) :-
      prob_parse([ si('Mais', cc-kon, 'Mais', [dr(0,s,s)-1]), si(certaines, det-pro:ind, certain, [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(savent, v-ver:pres, savoir, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si(elles, cls-pro:per, lui, [np-1]), si(étaient, v-ver:impf, être, [dr(0,dl(0,np,s),dl(0,np,s_pass))-1]), si(endormies, vpp-ver:pper, endormir, [dl(0,np,s_pass)-1]), si(après, p-prp, après, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf))-1]), si(avoir, vinf-ver:infi, avoir, [dr(0,dl(0,np,s_inf),dl(0,np,s_ppart))-1]), si(dormi, vpp-ver:pper, dormir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(835, Result) :-
      prob_parse([ si('Certaines', det-pro:ind, 'Certaines', [dr(0,np,n)-1]), si(personnes, nc-nom, personne, [n-1]), si(découvrent, v-ver:pres, découvrir, [dr(0,dl(0,np,s),dl(0,np,s_inf))-1]), si(avoir, vinf-ver:infi, avoir, [dr(0,dl(0,np,s_inf),dl(0,np,s_ppart))-1]), si(dormi, vpp-ver:pper, dormir, [dl(0,np,s_ppart)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(836, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(construit, vpp-ver:pper, construire, [dr(0,dl(0,np,s_ppart),np)-1]), si('MTALK', npp-abr, 'MTALK', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(837, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('MTALK', npp-abr, 'MTALK', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(838, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(construisait, v-ver:impf, construire, [dr(0,dl(0,np,s),np)-1]), si('MTALK', npp-abr, 'MTALK', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(839, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(terminé, vpp-ver:pper, terminer, [dr(0,dl(0,np,s_ppart),np)-1]), si('MTALK', npp-abr, 'MTALK', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(840, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(841, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(842, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(était, v-ver:impf, être, [dr(0,dl(0,np,s),pp)-1]), si(en, p-prp, en, [dr(0,pp,n)-1]), si(train, nc-nom, train, [dr(0,n,dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(obtenir, vinf-ver:infi, obtenir, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(843, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(un, det-det:art, un, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1993, pro-num, 1993, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(844, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(détenait, v-ver:impf, détenir, [dr(0,dl(0,np,s),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(de, p-prp, de, [dr(0,dl(1,s,s),np)-1]), si(1988, pro-num, 1988, [np-1]), si(à, p-prp, à, [dr(0,dl(0,np,np),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(845, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(détenait, v-ver:impf, détenir, [dr(0,dl(0,np,s),np)-1]), si('APCOM', npp-abr, 'APCOM', [np-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1990, pro-num, 1990, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(846, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(847, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(848, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(ont, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(849, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(quitté, vpp-ver:pper, quitter, [dr(0,dl(0,np,s_ppart),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(réunion, nc-nom, réunion, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(850, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(',', ponct-pun, ',', [dr(0,dl(0,np,np),np)-1]), si('Dubois', npp-nam, 'Dubois', [np-1]), si(et, cc-kon, et, [dr(0,dl(0,np,np),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(se, clr-pro:per, se, [cl_r-1]), si(sont, v-ver:pres, être, [dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart)))-1]), si(rencontrés, vpp-ver:pper, rencontrer, [dl(0,cl_r,dl(0,np,s_ppart))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(851, Result) :-
      prob_parse([ si('Un', det-det:art, 'Un', [dr(0,np,n)-1]), si(groupe, nc-nom, groupe, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),n)-1]), si(personnes, nc-nom, personne, [n-1]), si('s\'', clr-pro:per, 's\'', [cl_r-1]), si(est, v-ver:pres, être, [dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart)))-1]), si(rencontré, vpp-ver:pper, rencontrer, [dl(0,cl_r,dl(0,np,s_ppart))-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(852, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(savait, v-ver:impf, savoir, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(853, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(854, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(croyait, v-ver:impf, croire, [dr(0,dl(0,np,s),s_q)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(avait, v-ver:impf, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(855, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(856, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(réussi, vpp-ver:pper, réussir, [dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))-1]), si(à, p-prp, à, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(obtenir, vinf-ver:infi, obtenir, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(857, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(858, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(essayé, vpp-ver:pper, essayer, [dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-1]), si(obtenir, vinf-ver:infi, obtenir, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(859, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(860, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(est, v-ver:pres, être, [dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n))-1]), si(vrai, adj-adj, vrai, [dl(0,n,n)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(861, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(862, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(est, v-ver:pres, être, [dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n))-1]), si(faux, adj-adj, faux, [dl(0,n,n)-1]), si('qu\'', cs-kon, 'qu\'', [dr(0,s_q,s)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(863, Result) :-
      prob_parse([ si('ITEL', npp-abr, 'ITEL', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(obtenu, vpp-ver:pper, obtenir, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(en, p-prp, en, [dr(0,dl(1,s,s),np)-1]), si(1992, pro-num, 1992, [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(864, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(865, Result) :-
      prob_parse([ si('Si', cs-kon, 'Si', [dr(0,dr(0,s,s),s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(',', ponct-pun, ',', [let-1]), si(alors, adv-adv, alors, [dr(0,s,s)-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(mains, nc-nom, main, [n-1]), si(tremblaient, v-ver:impf, trembler, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(866, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(mains, nc-nom, main, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(trembler, vinf-ver:infi, trembler, [dl(0,np,s_inf)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(867, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(868, Result) :-
      prob_parse([ si('Quand', cs-kon, 'Quand', [dr(0,dr(0,s,s),s)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(',', ponct-pun, ',', [let-1]), si(ses, det-det:pos, son, [dr(0,np,n)-1]), si(mains, nc-nom, main, [n-1]), si(tremblaient, v-ver:impf, trembler, [dl(0,np,s)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(869, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(les, det-det:art, le, [dr(0,np,n)-1]), si(mains, nc-nom, main, [n-1]), si(de, p-prp, de, [dr(0,dl(0,n,n),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(trembler, vinf-ver:infi, trembler, [dl(0,np,s_inf)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(870, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(871, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(signé, vpp-ver:pper, signer, [dr(0,dl(0,np,s_ppart),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(872, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(873, Result) :-
      prob_parse([ si('Durand', npp-nam, 'Durand', [np-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(directeur, nc-nom, directeur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(874, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(directeur, nc-nom, directeur, [n-1]), si('d\'', p-prp, 'd\'', [dr(0,dl(0,n,n),np)-1]), si('ITEL', npp-abr, 'ITEL', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(875, Result) :-
      prob_parse([ si('Hélène', npp-nam, 'Hélène', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(directeur, nc-nom, directeur, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(département, nc-nom, département, [n-1]), si(répondre, vinf-ver:infi, répondre, [dr(0,dl(0,np,s_inf),pp)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(téléphone, nc-nom, téléphone, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(876, Result) :-
      prob_parse([ si('Le', det-det:art, 'Le', [dr(0,np,n)-1]), si(directeur, nc-nom, directeur, [n-1]), si(du, p+d-prp:det, de, [dr(0,dl(0,n,n),n)-1]), si(département, nc-nom, département, [n-1]), si(est, v-ver:pres, être, [dr(0,dl(0,np,s),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(personne, nc-nom, personne, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(877, Result) :-
      prob_parse([ si('Il', cls-pro:per, 'Il', [np-1]), si(y, clo-pro:per, y, [cl_y-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,cl_y,dl(0,np,s)),np)-1]), si('quelqu\'un', pro-pro:ind, 'quelqu\'un', [np-1]), si('qu\'', prorel-pro:rel, 'qu\'', [dr(0,dl(0,np,np),dr(0,s,dia(1,box(1,np))))-1]), si('Hélène', npp-nam, 'Hélène', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),np),dl(0,np,s_inf))-1]), si(répondre, vinf-ver:infi, répondre, [dr(0,dl(0,np,s_inf),pp)-1]), si(au, p+d-prp:det, au, [dr(0,pp_a,n)-1]), si(téléphone, nc-nom, téléphone, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(878, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(et, cc-kon, et, [dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s))))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),dl(0,np,s)))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s))))))-1]), si(sa, det-det:pos, son, [dr(0,np,n)-1]), si(secrétaire, nc-nom, secrétaire, [n-1]), si(en, clo-pro:per, en, [dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_de))))-1]), si(faire, vinf-ver:infi, faire, [dr(0,dr(0,dl(0,np,s_inf),pp),np)-1]), si(une, det-det:art, un, [dr(0,np,n)-1]), si(copie, nc-nom, copie, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(879, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
sent(880, Result) :-
      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(ou, cc-kon, ou, [dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s))-1]), si(rayer, vinf-ver:infi, rayer, [dr(0,dl(0,np,s_inf),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(clause, nc-nom, clause, [n-1]), si(cruciale, adj-adj, crucial, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).
%sent(882, Result) :-
%      prob_parse([ si('Dupont', npp-nam, 'Dupont', [np-1]), si(a, v-ver:pres, avoir, [dr(0,dl(0,np,s),dl(0,np,s_ppart))-1]), si(vu, vpp-ver:pper, voir, [dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-1]), si(soit, cc-kon, être, [dr(0,np,np)-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(signer, vinf-ver:infi, signer, [dr(0,dl(0,np,s_inf),np)-1]), si(le, det-det:art, le, [dr(0,np,n)-1]), si(contrat, nc-nom, contrat, [n-1]), si(',', ponct-pun, ',', [let-1]), si(soit, cc-kon, être, [dr(0,dl(0,p(0,np,dl(0,np,s)),p(0,np,dia(0,box(0,dl(0,np,s))))),p(0,np,dl(0,np,s)))-1]), si('Durand', npp-nam, 'Durand', [np-1]), si(rayer, vinf-ver:infi, rayer, [dr(0,dl(0,np,s_inf),np)-1]), si(la, det-det:art, le, [dr(0,np,n)-1]), si(clause, nc-nom, clause, [n-1]), si(principale, adj-adj, principal, [dl(0,n,n)-1]), si('.', ponct-pun, '.', [dl(0,s,txt)-1])], Result).

:- dynamic crosses/4.

crosses(1, 1, 3, 2).
crosses(1, 3, 5, 2).
crosses(1, 4, 6, 1).
crosses(1, 6, 8, 1).
crosses(1, 7, 9, 1).
crosses(1, 9, 11, 2).
crosses(1, 0, 3, 1).
crosses(1, 1, 4, 1).
crosses(1, 2, 5, 1).
crosses(1, 3, 6, 3).
crosses(1, 6, 9, 2).
crosses(1, 8, 11, 1).
crosses(1, 1, 5, 2).
crosses(1, 2, 6, 2).
crosses(1, 3, 7, 2).
crosses(1, 5, 9, 1).
crosses(1, 6, 10, 1).
crosses(1, 7, 11, 1).
crosses(1, 0, 5, 1).
crosses(1, 1, 6, 3).
crosses(1, 2, 7, 1).
crosses(1, 3, 8, 2).
crosses(1, 4, 9, 1).
crosses(1, 6, 11, 2).
crosses(1, 0, 6, 2).
crosses(1, 1, 7, 2).
crosses(1, 2, 8, 1).
crosses(1, 3, 9, 3).
crosses(1, 5, 11, 1).
crosses(1, 0, 7, 1).
crosses(1, 1, 8, 2).
crosses(1, 2, 9, 2).
crosses(1, 3, 10, 1).
crosses(1, 0, 8, 1).
crosses(1, 1, 9, 3).
crosses(1, 3, 11, 1).
crosses(1, 0, 9, 2).
crosses(1, 1, 10, 1).
crosses(1, 1, 11, 1).
crosses(2, 3, 5, 2).
crosses(2, 5, 7, 1).
crosses(2, 6, 8, 1).
crosses(2, 8, 10, 2).
crosses(2, 9, 11, 1).
crosses(2, 11, 13, 1).
crosses(2, 12, 14, 1).
crosses(2, 14, 16, 4).
crosses(2, 2, 5, 2).
crosses(2, 3, 6, 2).
crosses(2, 4, 7, 1).
crosses(2, 5, 8, 2).
crosses(2, 7, 10, 1).
crosses(2, 8, 11, 3).
crosses(2, 11, 14, 2).
crosses(2, 13, 16, 3).
crosses(2, 1, 5, 2).
crosses(2, 2, 6, 2).
crosses(2, 3, 7, 3).
crosses(2, 4, 8, 2).
crosses(2, 5, 9, 1).
crosses(2, 6, 10, 1).
crosses(2, 7, 11, 2).
crosses(2, 8, 12, 2).
crosses(2, 10, 14, 1).
crosses(2, 11, 15, 1).
crosses(2, 12, 16, 3).
crosses(2, 0, 5, 1).
crosses(2, 1, 6, 2).
crosses(2, 2, 7, 3).
crosses(2, 3, 8, 4).
crosses(2, 4, 9, 1).
crosses(2, 5, 10, 2).
crosses(2, 6, 11, 2).
crosses(2, 7, 12, 1).
crosses(2, 8, 13, 2).
crosses(2, 9, 14, 1).
crosses(2, 11, 16, 4).
crosses(2, 0, 6, 1).
crosses(2, 1, 7, 3).
crosses(2, 2, 8, 4).
crosses(2, 3, 9, 3).
crosses(2, 4, 10, 2).
crosses(2, 5, 11, 3).
crosses(2, 6, 12, 1).
crosses(2, 7, 13, 1).
crosses(2, 8, 14, 3).
crosses(2, 10, 16, 3).
crosses(2, 0, 7, 2).
crosses(2, 1, 8, 4).
crosses(2, 2, 9, 3).
crosses(2, 3, 10, 4).
crosses(2, 4, 11, 3).
crosses(2, 5, 12, 2).
crosses(2, 6, 13, 1).
crosses(2, 7, 14, 2).
crosses(2, 8, 15, 1).
crosses(2, 9, 16, 2).
crosses(2, 0, 8, 3).
crosses(2, 1, 9, 3).
crosses(2, 2, 10, 4).
crosses(2, 3, 11, 5).
crosses(2, 4, 12, 2).
crosses(2, 5, 13, 2).
crosses(2, 6, 14, 2).
crosses(2, 8, 16, 3).
crosses(2, 0, 9, 2).
crosses(2, 1, 10, 4).
crosses(2, 2, 11, 5).
crosses(2, 3, 12, 4).
crosses(2, 4, 13, 2).
crosses(2, 5, 14, 3).
crosses(2, 7, 16, 2).
crosses(2, 0, 10, 3).
crosses(2, 1, 11, 5).
crosses(2, 2, 12, 4).
crosses(2, 3, 13, 4).
crosses(2, 4, 14, 3).
crosses(2, 6, 16, 1).
crosses(2, 0, 11, 4).
crosses(2, 1, 12, 4).
crosses(2, 2, 13, 4).
crosses(2, 3, 14, 5).
crosses(2, 5, 16, 1).
crosses(2, 0, 12, 3).
crosses(2, 1, 13, 4).
crosses(2, 2, 14, 5).
crosses(2, 3, 15, 1).
crosses(2, 0, 13, 3).
crosses(2, 1, 14, 5).
crosses(2, 2, 15, 1).
crosses(2, 3, 16, 1).
crosses(2, 0, 14, 4).
crosses(2, 1, 15, 1).
crosses(2, 2, 16, 1).
crosses(2, 1, 16, 1).
crosses(3, 1, 3, 1).
crosses(3, 2, 4, 1).
crosses(3, 3, 5, 1).
crosses(3, 6, 8, 2).
crosses(3, 1, 4, 2).
crosses(3, 2, 5, 2).
crosses(3, 3, 6, 1).
crosses(3, 5, 8, 2).
crosses(3, 0, 4, 1).
crosses(3, 1, 5, 3).
crosses(3, 2, 6, 2).
crosses(3, 4, 8, 1).
crosses(3, 0, 5, 2).
crosses(3, 1, 6, 3).
crosses(3, 0, 6, 2).
crosses(3, 1, 7, 1).
crosses(3, 1, 8, 1).
crosses(4, 1, 3, 1).
crosses(4, 2, 4, 1).
crosses(4, 5, 7, 1).
crosses(4, 1, 4, 2).
crosses(4, 2, 5, 1).
crosses(4, 4, 7, 1).
crosses(4, 0, 4, 1).
crosses(4, 1, 5, 2).
crosses(4, 0, 5, 1).
crosses(4, 1, 6, 1).
crosses(4, 1, 7, 1).
crosses(5, 2, 4, 2).
crosses(5, 4, 6, 1).
crosses(5, 6, 8, 1).
crosses(5, 7, 9, 1).
crosses(5, 10, 12, 4).
crosses(5, 1, 4, 2).
crosses(5, 2, 5, 2).
crosses(5, 3, 6, 1).
crosses(5, 4, 7, 1).
crosses(5, 5, 8, 1).
crosses(5, 6, 9, 2).
crosses(5, 7, 10, 1).
crosses(5, 9, 12, 4).
crosses(5, 0, 4, 1).
crosses(5, 1, 5, 2).
crosses(5, 2, 6, 3).
crosses(5, 3, 7, 1).
crosses(5, 4, 8, 2).
crosses(5, 5, 9, 2).
crosses(5, 6, 10, 2).
crosses(5, 8, 12, 3).
crosses(5, 0, 5, 1).
crosses(5, 1, 6, 3).
crosses(5, 2, 7, 3).
crosses(5, 3, 8, 2).
crosses(5, 4, 9, 3).
crosses(5, 5, 10, 2).
crosses(5, 7, 12, 2).
crosses(5, 0, 6, 2).
crosses(5, 1, 7, 3).
crosses(5, 2, 8, 4).
crosses(5, 3, 9, 3).
crosses(5, 4, 10, 3).
crosses(5, 6, 12, 2).
crosses(5, 0, 7, 2).
crosses(5, 1, 8, 4).
crosses(5, 2, 9, 5).
crosses(5, 3, 10, 3).
crosses(5, 5, 12, 1).
crosses(5, 0, 8, 3).
crosses(5, 1, 9, 5).
crosses(5, 2, 10, 5).
crosses(5, 4, 12, 1).
crosses(5, 0, 9, 4).
crosses(5, 1, 10, 5).
crosses(5, 2, 11, 1).
crosses(5, 0, 10, 4).
crosses(5, 1, 11, 1).
crosses(5, 2, 12, 1).
crosses(5, 1, 12, 1).
crosses(6, 2, 4, 1).
crosses(6, 3, 5, 1).
crosses(6, 4, 6, 1).
crosses(6, 7, 9, 2).
crosses(6, 1, 4, 1).
crosses(6, 2, 5, 2).
crosses(6, 3, 6, 2).
crosses(6, 4, 7, 1).
crosses(6, 6, 9, 2).
crosses(6, 1, 5, 2).
crosses(6, 2, 6, 3).
crosses(6, 3, 7, 2).
crosses(6, 5, 9, 1).
crosses(6, 0, 5, 1).
crosses(6, 1, 6, 3).
crosses(6, 2, 7, 3).
crosses(6, 0, 6, 2).
crosses(6, 1, 7, 3).
crosses(6, 2, 8, 1).
crosses(6, 0, 7, 2).
crosses(6, 1, 8, 1).
crosses(6, 2, 9, 1).
crosses(6, 1, 9, 1).
crosses(7, 1, 3, 1).
crosses(7, 2, 4, 1).
crosses(7, 5, 7, 1).
crosses(7, 1, 4, 2).
crosses(7, 2, 5, 1).
crosses(7, 4, 7, 1).
crosses(7, 0, 4, 1).
crosses(7, 1, 5, 2).
crosses(7, 0, 5, 1).
crosses(7, 1, 6, 1).
crosses(7, 1, 7, 1).
crosses(8, 2, 4, 2).
crosses(8, 4, 6, 1).
crosses(8, 6, 8, 1).
crosses(8, 7, 9, 1).
crosses(8, 1, 4, 2).
crosses(8, 2, 5, 2).
crosses(8, 3, 6, 1).
crosses(8, 4, 7, 1).
crosses(8, 5, 8, 1).
crosses(8, 6, 9, 2).
crosses(8, 7, 10, 1).
crosses(8, 0, 4, 1).
crosses(8, 1, 5, 2).
crosses(8, 2, 6, 3).
crosses(8, 3, 7, 1).
crosses(8, 4, 8, 2).
crosses(8, 5, 9, 2).
crosses(8, 6, 10, 2).
crosses(8, 0, 5, 1).
crosses(8, 1, 6, 3).
crosses(8, 2, 7, 3).
crosses(8, 3, 8, 2).
crosses(8, 4, 9, 3).
crosses(8, 5, 10, 2).
crosses(8, 0, 6, 2).
crosses(8, 1, 7, 3).
crosses(8, 2, 8, 4).
crosses(8, 3, 9, 3).
crosses(8, 4, 10, 3).
crosses(8, 0, 7, 2).
crosses(8, 1, 8, 4).
crosses(8, 2, 9, 5).
crosses(8, 3, 10, 3).
crosses(8, 0, 8, 3).
crosses(8, 1, 9, 5).
crosses(8, 2, 10, 5).
crosses(8, 0, 9, 4).
crosses(8, 1, 10, 5).
crosses(8, 2, 11, 1).
crosses(8, 0, 10, 4).
crosses(8, 1, 11, 1).
crosses(9, 2, 4, 1).
crosses(9, 3, 5, 2).
crosses(9, 5, 7, 2).
crosses(9, 1, 4, 1).
crosses(9, 2, 5, 3).
crosses(9, 1, 5, 3).
crosses(9, 2, 6, 1).
crosses(9, 0, 5, 2).
crosses(9, 1, 6, 1).
crosses(9, 2, 7, 1).
crosses(9, 1, 7, 1).
crosses(10, 2, 4, 1).
crosses(10, 1, 4, 1).
crosses(10, 2, 5, 1).
crosses(10, 1, 5, 1).
crosses(10, 2, 6, 1).
crosses(10, 1, 6, 1).
crosses(11, 2, 4, 2).
crosses(11, 5, 7, 1).
crosses(11, 7, 9, 2).
crosses(11, 9, 11, 4).
crosses(11, 1, 4, 2).
crosses(11, 2, 5, 2).
crosses(11, 4, 7, 1).
crosses(11, 5, 8, 1).
crosses(11, 6, 9, 2).
crosses(11, 8, 11, 2).
crosses(11, 0, 4, 1).
crosses(11, 1, 5, 2).
crosses(11, 2, 6, 2).
crosses(11, 3, 7, 1).
crosses(11, 4, 8, 1).
crosses(11, 5, 9, 3).
crosses(11, 7, 11, 2).
crosses(11, 0, 5, 1).
crosses(11, 1, 6, 2).
crosses(11, 2, 7, 3).
crosses(11, 3, 8, 1).
crosses(11, 4, 9, 3).
crosses(11, 6, 11, 1).
crosses(11, 0, 6, 1).
crosses(11, 1, 7, 3).
crosses(11, 2, 8, 3).
crosses(11, 3, 9, 3).
crosses(11, 5, 11, 1).
crosses(11, 0, 7, 2).
crosses(11, 1, 8, 3).
crosses(11, 2, 9, 5).
crosses(11, 4, 11, 1).
crosses(11, 0, 8, 2).
crosses(11, 1, 9, 5).
crosses(11, 2, 10, 1).
crosses(11, 0, 9, 4).
crosses(11, 1, 10, 1).
crosses(11, 2, 11, 1).
crosses(11, 1, 11, 1).
crosses(12, 1, 3, 1).
crosses(12, 3, 5, 2).
crosses(12, 0, 3, 1).
crosses(12, 2, 5, 1).
crosses(12, 3, 6, 2).
crosses(12, 1, 5, 1).
crosses(12, 2, 6, 1).
crosses(12, 3, 7, 2).
crosses(12, 1, 6, 1).
crosses(12, 2, 7, 1).
crosses(12, 1, 7, 1).
crosses(13, 2, 4, 2).
crosses(13, 4, 6, 1).
crosses(13, 6, 8, 2).
crosses(13, 9, 11, 2).
crosses(13, 1, 4, 2).
crosses(13, 2, 5, 2).
crosses(13, 3, 6, 1).
crosses(13, 5, 8, 1).
crosses(13, 6, 9, 2).
crosses(13, 8, 11, 2).
crosses(13, 0, 4, 1).
crosses(13, 1, 5, 2).
crosses(13, 2, 6, 3).
crosses(13, 4, 8, 1).
crosses(13, 5, 9, 1).
crosses(13, 6, 10, 1).
crosses(13, 7, 11, 1).
crosses(13, 0, 5, 1).
crosses(13, 1, 6, 3).
crosses(13, 2, 7, 2).
crosses(13, 3, 8, 1).
crosses(13, 4, 9, 1).
crosses(13, 6, 11, 2).
crosses(13, 0, 6, 2).
crosses(13, 1, 7, 2).
crosses(13, 2, 8, 3).
crosses(13, 3, 9, 1).
crosses(13, 5, 11, 1).
crosses(13, 0, 7, 1).
crosses(13, 1, 8, 3).
crosses(13, 2, 9, 3).
crosses(13, 4, 11, 1).
crosses(13, 0, 8, 2).
crosses(13, 1, 9, 3).
crosses(13, 2, 10, 1).
crosses(13, 0, 9, 2).
crosses(13, 1, 10, 1).
crosses(13, 2, 11, 1).
crosses(13, 1, 11, 1).
crosses(14, 0, 2, 1).
crosses(14, 2, 4, 1).
crosses(14, 3, 5, 2).
crosses(14, 5, 7, 1).
crosses(14, 2, 5, 3).
crosses(14, 3, 6, 1).
crosses(14, 5, 8, 1).
crosses(14, 1, 5, 2).
crosses(14, 2, 6, 2).
crosses(14, 3, 7, 1).
crosses(14, 0, 5, 1).
crosses(14, 1, 6, 1).
crosses(14, 2, 7, 2).
crosses(14, 3, 8, 1).
crosses(14, 1, 7, 1).
crosses(14, 2, 8, 2).
crosses(14, 1, 8, 1).
crosses(15, 2, 4, 2).
crosses(15, 3, 5, 1).
crosses(15, 5, 7, 1).
crosses(15, 6, 8, 2).
crosses(15, 9, 11, 1).
crosses(15, 1, 4, 2).
crosses(15, 2, 5, 3).
crosses(15, 5, 8, 3).
crosses(15, 6, 9, 2).
crosses(15, 8, 11, 1).
crosses(15, 0, 4, 1).
crosses(15, 1, 5, 3).
crosses(15, 2, 6, 2).
crosses(15, 4, 8, 2).
crosses(15, 5, 9, 3).
crosses(15, 6, 10, 1).
crosses(15, 0, 5, 2).
crosses(15, 1, 6, 2).
crosses(15, 2, 7, 1).
crosses(15, 3, 8, 1).
crosses(15, 4, 9, 2).
crosses(15, 5, 10, 2).
crosses(15, 6, 11, 1).
crosses(15, 0, 6, 1).
crosses(15, 1, 7, 1).
crosses(15, 2, 8, 2).
crosses(15, 3, 9, 1).
crosses(15, 4, 10, 1).
crosses(15, 5, 11, 2).
crosses(15, 1, 8, 2).
crosses(15, 2, 9, 2).
crosses(15, 4, 11, 1).
crosses(15, 0, 8, 1).
crosses(15, 1, 9, 2).
crosses(15, 2, 10, 1).
crosses(15, 0, 9, 1).
crosses(15, 1, 10, 1).
crosses(15, 2, 11, 1).
crosses(15, 1, 11, 1).
crosses(16, 2, 4, 1).
crosses(16, 1, 4, 1).
crosses(16, 2, 5, 1).
crosses(16, 1, 5, 1).
crosses(16, 2, 6, 1).
crosses(16, 1, 6, 1).
crosses(17, 2, 4, 2).
crosses(17, 5, 7, 1).
crosses(17, 8, 10, 2).
crosses(17, 1, 4, 2).
crosses(17, 2, 5, 2).
crosses(17, 4, 7, 1).
crosses(17, 5, 8, 1).
crosses(17, 7, 10, 2).
crosses(17, 0, 4, 1).
crosses(17, 1, 5, 2).
crosses(17, 2, 6, 2).
crosses(17, 3, 7, 1).
crosses(17, 4, 8, 1).
crosses(17, 6, 10, 1).
crosses(17, 0, 5, 1).
crosses(17, 1, 6, 2).
crosses(17, 2, 7, 3).
crosses(17, 3, 8, 1).
crosses(17, 5, 10, 1).
crosses(17, 0, 6, 1).
crosses(17, 1, 7, 3).
crosses(17, 2, 8, 3).
crosses(17, 4, 10, 1).
crosses(17, 0, 7, 2).
crosses(17, 1, 8, 3).
crosses(17, 2, 9, 1).
crosses(17, 0, 8, 2).
crosses(17, 1, 9, 1).
crosses(17, 2, 10, 1).
crosses(17, 1, 10, 1).
crosses(18, 0, 2, 1).
crosses(18, 1, 3, 1).
crosses(18, 3, 5, 3).
crosses(18, 0, 3, 2).
crosses(18, 2, 5, 2).
crosses(18, 3, 6, 3).
crosses(18, 1, 5, 1).
crosses(18, 2, 6, 2).
crosses(18, 3, 7, 3).
crosses(18, 1, 6, 1).
crosses(18, 2, 7, 2).
crosses(18, 1, 7, 1).
crosses(19, 2, 4, 2).
crosses(19, 5, 7, 1).
crosses(19, 8, 10, 2).
crosses(19, 1, 4, 2).
crosses(19, 2, 5, 2).
crosses(19, 4, 7, 1).
crosses(19, 5, 8, 1).
crosses(19, 7, 10, 2).
crosses(19, 0, 4, 1).
crosses(19, 1, 5, 2).
crosses(19, 2, 6, 2).
crosses(19, 3, 7, 1).
crosses(19, 4, 8, 1).
crosses(19, 6, 10, 1).
crosses(19, 0, 5, 1).
crosses(19, 1, 6, 2).
crosses(19, 2, 7, 3).
crosses(19, 3, 8, 1).
crosses(19, 5, 10, 1).
crosses(19, 0, 6, 1).
crosses(19, 1, 7, 3).
crosses(19, 2, 8, 3).
crosses(19, 4, 10, 1).
crosses(19, 0, 7, 2).
crosses(19, 1, 8, 3).
crosses(19, 2, 9, 1).
crosses(19, 0, 8, 2).
crosses(19, 1, 9, 1).
crosses(19, 2, 10, 1).
crosses(19, 1, 10, 1).
crosses(20, 2, 4, 1).
crosses(20, 1, 4, 1).
crosses(20, 2, 5, 1).
crosses(20, 1, 5, 1).
crosses(20, 2, 6, 1).
crosses(20, 1, 6, 1).
crosses(21, 2, 4, 2).
crosses(21, 5, 7, 1).
crosses(21, 8, 10, 2).
crosses(21, 1, 4, 2).
crosses(21, 2, 5, 2).
crosses(21, 4, 7, 1).
crosses(21, 5, 8, 1).
crosses(21, 7, 10, 2).
crosses(21, 0, 4, 1).
crosses(21, 1, 5, 2).
crosses(21, 2, 6, 2).
crosses(21, 3, 7, 1).
crosses(21, 4, 8, 1).
crosses(21, 6, 10, 1).
crosses(21, 0, 5, 1).
crosses(21, 1, 6, 2).
crosses(21, 2, 7, 3).
crosses(21, 3, 8, 1).
crosses(21, 5, 10, 1).
crosses(21, 0, 6, 1).
crosses(21, 1, 7, 3).
crosses(21, 2, 8, 3).
crosses(21, 4, 10, 1).
crosses(21, 0, 7, 2).
crosses(21, 1, 8, 3).
crosses(21, 2, 9, 1).
crosses(21, 0, 8, 2).
crosses(21, 1, 9, 1).
crosses(21, 2, 10, 1).
crosses(21, 1, 10, 1).
crosses(22, 1, 3, 1).
crosses(22, 2, 4, 1).
crosses(22, 4, 6, 3).
crosses(22, 0, 3, 1).
crosses(22, 1, 4, 2).
crosses(22, 3, 6, 2).
crosses(22, 4, 7, 3).
crosses(22, 0, 4, 2).
crosses(22, 2, 6, 1).
crosses(22, 3, 7, 2).
crosses(22, 4, 8, 3).
crosses(22, 1, 6, 1).
crosses(22, 2, 7, 1).
crosses(22, 3, 8, 2).
crosses(22, 1, 7, 1).
crosses(22, 2, 8, 1).
crosses(22, 1, 8, 1).
crosses(23, 2, 4, 2).
crosses(23, 5, 7, 1).
crosses(23, 8, 10, 2).
crosses(23, 1, 4, 2).
crosses(23, 2, 5, 2).
crosses(23, 4, 7, 1).
crosses(23, 5, 8, 1).
crosses(23, 7, 10, 2).
crosses(23, 0, 4, 1).
crosses(23, 1, 5, 2).
crosses(23, 2, 6, 2).
crosses(23, 3, 7, 1).
crosses(23, 4, 8, 1).
crosses(23, 6, 10, 1).
crosses(23, 0, 5, 1).
crosses(23, 1, 6, 2).
crosses(23, 2, 7, 3).
crosses(23, 3, 8, 1).
crosses(23, 5, 10, 1).
crosses(23, 0, 6, 1).
crosses(23, 1, 7, 3).
crosses(23, 2, 8, 3).
crosses(23, 4, 10, 1).
crosses(23, 0, 7, 2).
crosses(23, 1, 8, 3).
crosses(23, 2, 9, 1).
crosses(23, 0, 8, 2).
crosses(23, 1, 9, 1).
crosses(23, 2, 10, 1).
crosses(23, 1, 10, 1).
crosses(24, 2, 4, 1).
crosses(24, 3, 5, 1).
crosses(24, 6, 8, 1).
crosses(24, 1, 4, 1).
crosses(24, 2, 5, 2).
crosses(24, 3, 6, 1).
crosses(24, 5, 8, 1).
crosses(24, 1, 5, 2).
crosses(24, 2, 6, 2).
crosses(24, 0, 5, 1).
crosses(24, 1, 6, 2).
crosses(24, 2, 7, 1).
crosses(24, 0, 6, 1).
crosses(24, 1, 7, 1).
crosses(24, 2, 8, 1).
crosses(24, 1, 8, 1).
crosses(25, 2, 4, 1).
crosses(25, 3, 5, 1).
crosses(25, 6, 8, 1).
crosses(25, 1, 4, 1).
crosses(25, 2, 5, 2).
crosses(25, 3, 6, 1).
crosses(25, 5, 8, 1).
crosses(25, 1, 5, 2).
crosses(25, 2, 6, 2).
crosses(25, 0, 5, 1).
crosses(25, 1, 6, 2).
crosses(25, 2, 7, 1).
crosses(25, 0, 6, 1).
crosses(25, 1, 7, 1).
crosses(25, 2, 8, 1).
crosses(25, 1, 8, 1).
crosses(26, 2, 4, 2).
crosses(26, 5, 7, 1).
crosses(26, 7, 9, 1).
crosses(26, 10, 12, 3).
crosses(26, 1, 4, 2).
crosses(26, 2, 5, 2).
crosses(26, 4, 7, 1).
crosses(26, 5, 8, 1).
crosses(26, 6, 9, 1).
crosses(26, 7, 10, 1).
crosses(26, 9, 12, 3).
crosses(26, 0, 4, 1).
crosses(26, 1, 5, 2).
crosses(26, 2, 6, 2).
crosses(26, 3, 7, 1).
crosses(26, 4, 8, 1).
crosses(26, 5, 9, 2).
crosses(26, 6, 10, 1).
crosses(26, 8, 12, 2).
crosses(26, 0, 5, 1).
crosses(26, 1, 6, 2).
crosses(26, 2, 7, 3).
crosses(26, 3, 8, 1).
crosses(26, 4, 9, 2).
crosses(26, 5, 10, 2).
crosses(26, 7, 12, 2).
crosses(26, 0, 6, 1).
crosses(26, 1, 7, 3).
crosses(26, 2, 8, 3).
crosses(26, 3, 9, 2).
crosses(26, 4, 10, 2).
crosses(26, 6, 12, 1).
crosses(26, 0, 7, 2).
crosses(26, 1, 8, 3).
crosses(26, 2, 9, 4).
crosses(26, 3, 10, 2).
crosses(26, 5, 12, 1).
crosses(26, 0, 8, 2).
crosses(26, 1, 9, 4).
crosses(26, 2, 10, 4).
crosses(26, 4, 12, 1).
crosses(26, 0, 9, 3).
crosses(26, 1, 10, 4).
crosses(26, 2, 11, 1).
crosses(26, 0, 10, 3).
crosses(26, 1, 11, 1).
crosses(26, 2, 12, 1).
crosses(26, 1, 12, 1).
crosses(27, 0, 2, 1).
crosses(27, 1, 3, 1).
crosses(27, 3, 5, 3).
crosses(27, 0, 3, 2).
crosses(27, 2, 5, 2).
crosses(27, 3, 6, 3).
crosses(27, 1, 5, 1).
crosses(27, 2, 6, 2).
crosses(27, 3, 7, 3).
crosses(27, 1, 6, 1).
crosses(27, 2, 7, 2).
crosses(27, 1, 7, 1).
crosses(28, 2, 4, 2).
crosses(28, 5, 7, 1).
crosses(28, 8, 10, 2).
crosses(28, 1, 4, 2).
crosses(28, 2, 5, 2).
crosses(28, 4, 7, 1).
crosses(28, 5, 8, 1).
crosses(28, 7, 10, 2).
crosses(28, 0, 4, 1).
crosses(28, 1, 5, 2).
crosses(28, 2, 6, 2).
crosses(28, 3, 7, 1).
crosses(28, 4, 8, 1).
crosses(28, 6, 10, 1).
crosses(28, 0, 5, 1).
crosses(28, 1, 6, 2).
crosses(28, 2, 7, 3).
crosses(28, 3, 8, 1).
crosses(28, 5, 10, 1).
crosses(28, 0, 6, 1).
crosses(28, 1, 7, 3).
crosses(28, 2, 8, 3).
crosses(28, 4, 10, 1).
crosses(28, 0, 7, 2).
crosses(28, 1, 8, 3).
crosses(28, 2, 9, 1).
crosses(28, 0, 8, 2).
crosses(28, 1, 9, 1).
crosses(28, 2, 10, 1).
crosses(28, 1, 10, 1).
crosses(29, 3, 5, 1).
crosses(29, 2, 5, 1).
crosses(29, 3, 6, 1).
crosses(29, 1, 5, 1).
crosses(29, 2, 6, 1).
crosses(29, 3, 7, 1).
crosses(29, 1, 6, 1).
crosses(29, 2, 7, 1).
crosses(29, 1, 7, 1).
crosses(30, 2, 4, 1).
crosses(30, 5, 7, 2).
crosses(30, 1, 4, 1).
crosses(30, 2, 5, 1).
crosses(30, 4, 7, 2).
crosses(30, 5, 8, 2).
crosses(30, 0, 4, 1).
crosses(30, 1, 5, 1).
crosses(30, 3, 7, 1).
crosses(30, 4, 8, 2).
crosses(30, 5, 9, 2).
crosses(30, 0, 5, 1).
crosses(30, 2, 7, 1).
crosses(30, 3, 8, 1).
crosses(30, 4, 9, 2).
crosses(30, 1, 7, 1).
crosses(30, 2, 8, 1).
crosses(30, 3, 9, 1).
crosses(30, 1, 8, 1).
crosses(30, 2, 9, 1).
crosses(30, 1, 9, 1).
crosses(31, 3, 5, 1).
crosses(31, 2, 5, 1).
crosses(31, 3, 6, 1).
crosses(31, 1, 5, 1).
crosses(31, 2, 6, 1).
crosses(31, 3, 7, 1).
crosses(31, 1, 6, 1).
crosses(31, 2, 7, 1).
crosses(31, 1, 7, 1).
crosses(32, 0, 2, 1).
crosses(32, 1, 3, 1).
crosses(32, 3, 5, 1).
crosses(32, 4, 6, 1).
crosses(32, 7, 9, 6).
crosses(32, 9, 11, 2).
crosses(32, 11, 13, 1).
crosses(32, 0, 3, 2).
crosses(32, 1, 4, 1).
crosses(32, 2, 5, 1).
crosses(32, 3, 6, 2).
crosses(32, 4, 7, 1).
crosses(32, 6, 9, 6).
crosses(32, 7, 10, 5).
crosses(32, 8, 11, 1).
crosses(32, 9, 12, 1).
crosses(32, 0, 4, 2).
crosses(32, 1, 5, 2).
crosses(32, 2, 6, 2).
crosses(32, 3, 7, 2).
crosses(32, 5, 9, 5).
crosses(32, 6, 10, 5).
crosses(32, 7, 11, 6).
crosses(32, 9, 13, 1).
crosses(32, 0, 5, 3).
crosses(32, 1, 6, 3).
crosses(32, 2, 7, 2).
crosses(32, 4, 9, 4).
crosses(32, 5, 10, 4).
crosses(32, 6, 11, 6).
crosses(32, 7, 12, 5).
crosses(32, 0, 6, 4).
crosses(32, 1, 7, 3).
crosses(32, 3, 9, 4).
crosses(32, 4, 10, 3).
crosses(32, 5, 11, 5).
crosses(32, 6, 12, 5).
crosses(32, 7, 13, 5).
crosses(32, 0, 7, 4).
crosses(32, 2, 9, 3).
crosses(32, 3, 10, 3).
crosses(32, 4, 11, 4).
crosses(32, 5, 12, 4).
crosses(32, 6, 13, 5).
crosses(32, 1, 9, 2).
crosses(32, 2, 10, 2).
crosses(32, 3, 11, 4).
crosses(32, 4, 12, 3).
crosses(32, 5, 13, 4).
crosses(32, 0, 9, 1).
crosses(32, 1, 10, 1).
crosses(32, 2, 11, 3).
crosses(32, 3, 12, 3).
crosses(32, 4, 13, 3).
crosses(32, 1, 11, 2).
crosses(32, 2, 12, 2).
crosses(32, 3, 13, 3).
crosses(32, 0, 11, 1).
crosses(32, 1, 12, 1).
crosses(32, 2, 13, 2).
crosses(32, 1, 13, 1).
crosses(33, 1, 3, 1).
crosses(33, 2, 4, 1).
crosses(33, 3, 5, 1).
crosses(33, 4, 6, 1).
crosses(33, 7, 9, 6).
crosses(33, 9, 11, 1).
crosses(33, 0, 3, 1).
crosses(33, 1, 4, 2).
crosses(33, 2, 5, 2).
crosses(33, 3, 6, 2).
crosses(33, 4, 7, 1).
crosses(33, 6, 9, 6).
crosses(33, 7, 10, 5).
crosses(33, 0, 4, 2).
crosses(33, 1, 5, 3).
crosses(33, 2, 6, 3).
crosses(33, 3, 7, 2).
crosses(33, 5, 9, 5).
crosses(33, 6, 10, 5).
crosses(33, 7, 11, 5).
crosses(33, 0, 5, 3).
crosses(33, 1, 6, 4).
crosses(33, 2, 7, 3).
crosses(33, 4, 9, 4).
crosses(33, 5, 10, 4).
crosses(33, 6, 11, 5).
crosses(33, 0, 6, 4).
crosses(33, 1, 7, 4).
crosses(33, 3, 9, 3).
crosses(33, 4, 10, 3).
crosses(33, 5, 11, 4).
crosses(33, 0, 7, 4).
crosses(33, 2, 9, 2).
crosses(33, 3, 10, 2).
crosses(33, 4, 11, 3).
crosses(33, 1, 9, 2).
crosses(33, 2, 10, 1).
crosses(33, 3, 11, 2).
crosses(33, 0, 9, 1).
crosses(33, 1, 10, 1).
crosses(33, 2, 11, 1).
crosses(33, 1, 11, 1).
crosses(34, 1, 3, 1).
crosses(34, 2, 4, 1).
crosses(34, 3, 5, 1).
crosses(34, 4, 6, 1).
crosses(34, 7, 9, 4).
crosses(34, 9, 11, 1).
crosses(34, 11, 13, 3).
crosses(34, 0, 3, 1).
crosses(34, 1, 4, 2).
crosses(34, 2, 5, 2).
crosses(34, 3, 6, 2).
crosses(34, 4, 7, 1).
crosses(34, 6, 9, 4).
crosses(34, 7, 10, 4).
crosses(34, 8, 11, 1).
crosses(34, 10, 13, 2).
crosses(34, 0, 4, 2).
crosses(34, 1, 5, 3).
crosses(34, 2, 6, 3).
crosses(34, 3, 7, 2).
crosses(34, 5, 9, 3).
crosses(34, 6, 10, 4).
crosses(34, 7, 11, 5).
crosses(34, 9, 13, 2).
crosses(34, 0, 5, 3).
crosses(34, 1, 6, 4).
crosses(34, 2, 7, 3).
crosses(34, 4, 9, 2).
crosses(34, 5, 10, 3).
crosses(34, 6, 11, 5).
crosses(34, 7, 12, 3).
crosses(34, 8, 13, 1).
crosses(34, 0, 6, 4).
crosses(34, 1, 7, 4).
crosses(34, 3, 9, 1).
crosses(34, 4, 10, 2).
crosses(34, 5, 11, 4).
crosses(34, 6, 12, 3).
crosses(34, 7, 13, 4).
crosses(34, 0, 7, 4).
crosses(34, 1, 8, 1).
crosses(34, 2, 9, 1).
crosses(34, 3, 10, 1).
crosses(34, 4, 11, 3).
crosses(34, 5, 12, 2).
crosses(34, 6, 13, 4).
crosses(34, 0, 8, 1).
crosses(34, 1, 9, 2).
crosses(34, 2, 10, 1).
crosses(34, 3, 11, 2).
crosses(34, 4, 12, 1).
crosses(34, 5, 13, 3).
crosses(34, 0, 9, 2).
crosses(34, 1, 10, 2).
crosses(34, 2, 11, 2).
crosses(34, 4, 13, 2).
crosses(34, 0, 10, 2).
crosses(34, 1, 11, 3).
crosses(34, 3, 13, 1).
crosses(34, 0, 11, 3).
crosses(35, 3, 5, 2).
crosses(35, 5, 7, 2).
crosses(35, 7, 9, 1).
crosses(35, 2, 5, 2).
crosses(35, 3, 6, 1).
crosses(35, 4, 7, 1).
crosses(35, 5, 8, 1).
crosses(35, 1, 5, 2).
crosses(35, 2, 6, 1).
crosses(35, 3, 7, 2).
crosses(35, 5, 9, 1).
crosses(35, 0, 5, 1).
crosses(35, 1, 6, 1).
crosses(35, 2, 7, 2).
crosses(35, 3, 8, 1).
crosses(35, 1, 7, 2).
crosses(35, 2, 8, 1).
crosses(35, 3, 9, 1).
crosses(35, 0, 7, 1).
crosses(35, 1, 8, 1).
crosses(35, 2, 9, 1).
crosses(35, 1, 9, 1).
crosses(36, 2, 4, 2).
crosses(36, 4, 6, 1).
crosses(36, 5, 7, 1).
crosses(36, 7, 9, 2).
crosses(36, 9, 11, 3).
crosses(36, 1, 4, 2).
crosses(36, 2, 5, 2).
crosses(36, 3, 6, 1).
crosses(36, 4, 7, 2).
crosses(36, 6, 9, 1).
crosses(36, 7, 10, 1).
crosses(36, 8, 11, 2).
crosses(36, 0, 4, 1).
crosses(36, 1, 5, 2).
crosses(36, 2, 6, 3).
crosses(36, 3, 7, 2).
crosses(36, 4, 8, 1).
crosses(36, 5, 9, 1).
crosses(36, 7, 11, 3).
crosses(36, 0, 5, 1).
crosses(36, 1, 6, 3).
crosses(36, 2, 7, 4).
crosses(36, 3, 8, 1).
crosses(36, 4, 9, 2).
crosses(36, 6, 11, 2).
crosses(36, 0, 6, 2).
crosses(36, 1, 7, 4).
crosses(36, 2, 8, 3).
crosses(36, 3, 9, 2).
crosses(36, 5, 11, 1).
crosses(36, 0, 7, 3).
crosses(36, 1, 8, 3).
crosses(36, 2, 9, 4).
crosses(36, 4, 11, 1).
crosses(36, 0, 8, 2).
crosses(36, 1, 9, 4).
crosses(36, 2, 10, 1).
crosses(36, 0, 9, 3).
crosses(36, 1, 10, 1).
crosses(36, 2, 11, 1).
crosses(36, 1, 11, 1).
crosses(37, 1, 3, 1).
crosses(37, 3, 5, 1).
crosses(37, 4, 6, 1).
crosses(37, 6, 8, 2).
crosses(37, 7, 9, 1).
crosses(37, 10, 12, 2).
crosses(37, 0, 3, 1).
crosses(37, 3, 6, 2).
crosses(37, 5, 8, 1).
crosses(37, 6, 9, 3).
crosses(37, 7, 10, 1).
crosses(37, 9, 12, 2).
crosses(37, 2, 6, 1).
crosses(37, 3, 7, 1).
crosses(37, 4, 8, 1).
crosses(37, 5, 9, 2).
crosses(37, 6, 10, 3).
crosses(37, 8, 12, 1).
crosses(37, 1, 6, 1).
crosses(37, 3, 8, 2).
crosses(37, 4, 9, 2).
crosses(37, 5, 10, 2).
crosses(37, 6, 11, 1).
crosses(37, 0, 6, 1).
crosses(37, 2, 8, 1).
crosses(37, 3, 9, 3).
crosses(37, 4, 10, 2).
crosses(37, 6, 12, 1).
crosses(37, 1, 8, 1).
crosses(37, 2, 9, 2).
crosses(37, 3, 10, 3).
crosses(37, 0, 8, 1).
crosses(37, 1, 9, 2).
crosses(37, 2, 10, 2).
crosses(37, 3, 11, 1).
crosses(37, 0, 9, 2).
crosses(37, 1, 10, 2).
crosses(37, 3, 12, 1).
crosses(37, 0, 10, 2).
crosses(38, 2, 4, 2).
crosses(38, 4, 6, 1).
crosses(38, 6, 8, 1).
crosses(38, 8, 10, 2).
crosses(38, 9, 11, 1).
crosses(38, 12, 14, 4).
crosses(38, 1, 4, 2).
crosses(38, 2, 5, 2).
crosses(38, 3, 6, 1).
crosses(38, 4, 7, 1).
crosses(38, 5, 8, 1).
crosses(38, 7, 10, 1).
crosses(38, 8, 11, 3).
crosses(38, 9, 12, 1).
crosses(38, 11, 14, 4).
crosses(38, 0, 4, 1).
crosses(38, 1, 5, 2).
crosses(38, 2, 6, 3).
crosses(38, 3, 7, 1).
crosses(38, 4, 8, 2).
crosses(38, 6, 10, 1).
crosses(38, 7, 11, 2).
crosses(38, 8, 12, 3).
crosses(38, 10, 14, 3).
crosses(38, 0, 5, 1).
crosses(38, 1, 6, 3).
crosses(38, 2, 7, 3).
crosses(38, 3, 8, 2).
crosses(38, 4, 9, 1).
crosses(38, 5, 10, 1).
crosses(38, 6, 11, 2).
crosses(38, 7, 12, 2).
crosses(38, 8, 13, 1).
crosses(38, 9, 14, 2).
crosses(38, 0, 6, 2).
crosses(38, 1, 7, 3).
crosses(38, 2, 8, 4).
crosses(38, 3, 9, 1).
crosses(38, 4, 10, 2).
crosses(38, 5, 11, 2).
crosses(38, 6, 12, 2).
crosses(38, 8, 14, 3).
crosses(38, 0, 7, 2).
crosses(38, 1, 8, 4).
crosses(38, 2, 9, 3).
crosses(38, 3, 10, 2).
crosses(38, 4, 11, 3).
crosses(38, 5, 12, 2).
crosses(38, 7, 14, 2).
crosses(38, 0, 8, 3).
crosses(38, 1, 9, 3).
crosses(38, 2, 10, 4).
crosses(38, 3, 11, 3).
crosses(38, 4, 12, 3).
crosses(38, 6, 14, 2).
crosses(38, 0, 9, 2).
crosses(38, 1, 10, 4).
crosses(38, 2, 11, 5).
crosses(38, 3, 12, 3).
crosses(38, 5, 14, 1).
crosses(38, 0, 10, 3).
crosses(38, 1, 11, 5).
crosses(38, 2, 12, 5).
crosses(38, 4, 14, 1).
crosses(38, 0, 11, 4).
crosses(38, 1, 12, 5).
crosses(38, 2, 13, 1).
crosses(38, 0, 12, 4).
crosses(38, 1, 13, 1).
crosses(38, 2, 14, 1).
crosses(38, 1, 14, 1).
crosses(39, 1, 3, 2).
crosses(39, 3, 5, 2).
crosses(39, 6, 8, 1).
crosses(39, 8, 10, 2).
crosses(39, 0, 3, 1).
crosses(39, 1, 4, 1).
crosses(39, 2, 5, 1).
crosses(39, 3, 6, 2).
crosses(39, 5, 8, 1).
crosses(39, 7, 10, 1).
crosses(39, 1, 5, 2).
crosses(39, 2, 6, 1).
crosses(39, 3, 7, 2).
crosses(39, 4, 8, 1).
crosses(39, 6, 10, 1).
crosses(39, 0, 5, 1).
crosses(39, 1, 6, 2).
crosses(39, 2, 7, 1).
crosses(39, 3, 8, 3).
crosses(39, 5, 10, 1).
crosses(39, 0, 6, 1).
crosses(39, 1, 7, 2).
crosses(39, 2, 8, 2).
crosses(39, 3, 9, 1).
crosses(39, 0, 7, 1).
crosses(39, 1, 8, 3).
crosses(39, 3, 10, 1).
crosses(39, 0, 8, 2).
crosses(39, 1, 9, 1).
crosses(39, 1, 10, 1).
crosses(40, 1, 3, 2).
crosses(40, 3, 5, 2).
crosses(40, 6, 8, 1).
crosses(40, 0, 3, 1).
crosses(40, 1, 4, 1).
crosses(40, 2, 5, 1).
crosses(40, 3, 6, 2).
crosses(40, 5, 8, 1).
crosses(40, 1, 5, 2).
crosses(40, 2, 6, 1).
crosses(40, 3, 7, 1).
crosses(40, 0, 5, 1).
crosses(40, 1, 6, 2).
crosses(40, 3, 8, 1).
crosses(40, 0, 6, 1).
crosses(40, 1, 7, 1).
crosses(40, 1, 8, 1).
crosses(41, 1, 3, 1).
crosses(41, 2, 4, 1).
crosses(41, 4, 6, 1).
crosses(41, 6, 8, 1).
crosses(41, 8, 10, 3).
crosses(41, 1, 4, 2).
crosses(41, 2, 5, 1).
crosses(41, 3, 6, 1).
crosses(41, 4, 7, 1).
crosses(41, 5, 8, 1).
crosses(41, 7, 10, 2).
crosses(41, 0, 4, 1).
crosses(41, 1, 5, 2).
crosses(41, 2, 6, 2).
crosses(41, 3, 7, 1).
crosses(41, 4, 8, 2).
crosses(41, 6, 10, 2).
crosses(41, 0, 5, 1).
crosses(41, 1, 6, 3).
crosses(41, 2, 7, 2).
crosses(41, 3, 8, 2).
crosses(41, 5, 10, 1).
crosses(41, 0, 6, 2).
crosses(41, 1, 7, 3).
crosses(41, 2, 8, 3).
crosses(41, 4, 10, 1).
crosses(41, 0, 7, 2).
crosses(41, 1, 8, 4).
crosses(41, 0, 8, 3).
crosses(41, 1, 9, 1).
crosses(41, 1, 10, 1).
crosses(42, 1, 3, 1).
crosses(42, 2, 4, 1).
crosses(42, 4, 6, 1).
crosses(42, 1, 4, 2).
crosses(42, 0, 4, 1).
crosses(42, 1, 5, 1).
crosses(42, 1, 6, 1).
crosses(43, 1, 3, 1).
crosses(43, 3, 5, 1).
crosses(43, 5, 7, 1).
crosses(43, 7, 9, 1).
crosses(43, 9, 11, 5).
crosses(43, 10, 12, 1).
crosses(43, 12, 14, 1).
crosses(43, 14, 16, 2).
crosses(43, 0, 3, 1).
crosses(43, 1, 4, 1).
crosses(43, 2, 5, 1).
crosses(43, 3, 6, 1).
crosses(43, 4, 7, 1).
crosses(43, 5, 8, 1).
crosses(43, 6, 9, 1).
crosses(43, 8, 11, 4).
crosses(43, 9, 12, 6).
crosses(43, 10, 13, 1).
crosses(43, 11, 14, 1).
crosses(43, 13, 16, 1).
crosses(43, 0, 4, 1).
crosses(43, 1, 5, 2).
crosses(43, 2, 6, 1).
crosses(43, 3, 7, 2).
crosses(43, 4, 8, 1).
crosses(43, 5, 9, 2).
crosses(43, 7, 11, 4).
crosses(43, 8, 12, 5).
crosses(43, 9, 13, 6).
crosses(43, 10, 14, 2).
crosses(43, 12, 16, 1).
crosses(43, 0, 5, 2).
crosses(43, 1, 6, 2).
crosses(43, 2, 7, 2).
crosses(43, 3, 8, 2).
crosses(43, 4, 9, 2).
crosses(43, 6, 11, 3).
crosses(43, 7, 12, 5).
crosses(43, 8, 13, 5).
crosses(43, 9, 14, 7).
crosses(43, 0, 6, 2).
crosses(43, 1, 7, 3).
crosses(43, 2, 8, 2).
crosses(43, 3, 9, 3).
crosses(43, 5, 11, 3).
crosses(43, 6, 12, 4).
crosses(43, 7, 13, 5).
crosses(43, 8, 14, 6).
crosses(43, 9, 15, 5).
crosses(43, 0, 7, 3).
crosses(43, 1, 8, 3).
crosses(43, 2, 9, 3).
crosses(43, 4, 11, 2).
crosses(43, 5, 12, 4).
crosses(43, 6, 13, 4).
crosses(43, 7, 14, 6).
crosses(43, 8, 15, 4).
crosses(43, 9, 16, 5).
crosses(43, 0, 8, 3).
crosses(43, 1, 9, 4).
crosses(43, 3, 11, 2).
crosses(43, 4, 12, 3).
crosses(43, 5, 13, 4).
crosses(43, 6, 14, 5).
crosses(43, 7, 15, 4).
crosses(43, 8, 16, 4).
crosses(43, 0, 9, 4).
crosses(43, 2, 11, 1).
crosses(43, 3, 12, 3).
crosses(43, 4, 13, 3).
crosses(43, 5, 14, 5).
crosses(43, 6, 15, 3).
crosses(43, 7, 16, 4).
crosses(43, 1, 11, 1).
crosses(43, 2, 12, 2).
crosses(43, 3, 13, 3).
crosses(43, 4, 14, 4).
crosses(43, 5, 15, 3).
crosses(43, 6, 16, 3).
crosses(43, 1, 12, 2).
crosses(43, 2, 13, 2).
crosses(43, 3, 14, 4).
crosses(43, 4, 15, 2).
crosses(43, 5, 16, 3).
crosses(43, 0, 12, 1).
crosses(43, 1, 13, 2).
crosses(43, 2, 14, 3).
crosses(43, 3, 15, 2).
crosses(43, 4, 16, 2).
crosses(43, 0, 13, 1).
crosses(43, 1, 14, 3).
crosses(43, 2, 15, 1).
crosses(43, 3, 16, 2).
crosses(43, 0, 14, 2).
crosses(43, 1, 15, 1).
crosses(43, 2, 16, 1).
crosses(43, 1, 16, 1).
crosses(44, 1, 3, 1).
crosses(44, 2, 4, 1).
crosses(44, 4, 6, 1).
crosses(44, 6, 8, 2).
crosses(44, 1, 4, 2).
crosses(44, 2, 5, 1).
crosses(44, 3, 6, 1).
crosses(44, 5, 8, 1).
crosses(44, 0, 4, 1).
crosses(44, 1, 5, 2).
crosses(44, 2, 6, 2).
crosses(44, 4, 8, 1).
crosses(44, 0, 5, 1).
crosses(44, 1, 6, 3).
crosses(44, 0, 6, 2).
crosses(44, 1, 7, 1).
crosses(44, 1, 8, 1).
crosses(45, 2, 4, 1).
crosses(45, 3, 5, 1).
crosses(45, 5, 7, 1).
crosses(45, 7, 9, 1).
crosses(45, 9, 11, 3).
crosses(45, 1, 4, 1).
crosses(45, 2, 5, 2).
crosses(45, 3, 6, 1).
crosses(45, 4, 7, 1).
crosses(45, 5, 8, 1).
crosses(45, 6, 9, 1).
crosses(45, 8, 11, 2).
crosses(45, 1, 5, 2).
crosses(45, 2, 6, 2).
crosses(45, 3, 7, 2).
crosses(45, 4, 8, 1).
crosses(45, 5, 9, 2).
crosses(45, 7, 11, 2).
crosses(45, 0, 5, 1).
crosses(45, 1, 6, 2).
crosses(45, 2, 7, 3).
crosses(45, 3, 8, 2).
crosses(45, 4, 9, 2).
crosses(45, 6, 11, 1).
crosses(45, 0, 6, 1).
crosses(45, 1, 7, 3).
crosses(45, 2, 8, 3).
crosses(45, 3, 9, 3).
crosses(45, 5, 11, 1).
crosses(45, 0, 7, 2).
crosses(45, 1, 8, 3).
crosses(45, 2, 9, 4).
crosses(45, 0, 8, 2).
crosses(45, 1, 9, 4).
crosses(45, 2, 10, 1).
crosses(45, 0, 9, 3).
crosses(45, 1, 10, 1).
crosses(45, 2, 11, 1).
crosses(45, 1, 11, 1).
crosses(46, 1, 3, 1).
crosses(46, 2, 4, 1).
crosses(46, 4, 6, 1).
crosses(46, 1, 4, 2).
crosses(46, 0, 4, 1).
crosses(46, 1, 5, 1).
crosses(46, 1, 6, 1).
crosses(47, 1, 3, 1).
crosses(47, 3, 5, 1).
crosses(47, 5, 7, 1).
crosses(47, 7, 9, 1).
crosses(47, 9, 11, 5).
crosses(47, 10, 12, 1).
crosses(47, 12, 14, 1).
crosses(47, 14, 16, 2).
crosses(47, 0, 3, 1).
crosses(47, 1, 4, 1).
crosses(47, 2, 5, 1).
crosses(47, 3, 6, 1).
crosses(47, 4, 7, 1).
crosses(47, 5, 8, 1).
crosses(47, 6, 9, 1).
crosses(47, 8, 11, 4).
crosses(47, 9, 12, 6).
crosses(47, 10, 13, 1).
crosses(47, 11, 14, 1).
crosses(47, 13, 16, 1).
crosses(47, 0, 4, 1).
crosses(47, 1, 5, 2).
crosses(47, 2, 6, 1).
crosses(47, 3, 7, 2).
crosses(47, 4, 8, 1).
crosses(47, 5, 9, 2).
crosses(47, 7, 11, 4).
crosses(47, 8, 12, 5).
crosses(47, 9, 13, 6).
crosses(47, 10, 14, 2).
crosses(47, 12, 16, 1).
crosses(47, 0, 5, 2).
crosses(47, 1, 6, 2).
crosses(47, 2, 7, 2).
crosses(47, 3, 8, 2).
crosses(47, 4, 9, 2).
crosses(47, 6, 11, 3).
crosses(47, 7, 12, 5).
crosses(47, 8, 13, 5).
crosses(47, 9, 14, 7).
crosses(47, 0, 6, 2).
crosses(47, 1, 7, 3).
crosses(47, 2, 8, 2).
crosses(47, 3, 9, 3).
crosses(47, 5, 11, 3).
crosses(47, 6, 12, 4).
crosses(47, 7, 13, 5).
crosses(47, 8, 14, 6).
crosses(47, 9, 15, 5).
crosses(47, 0, 7, 3).
crosses(47, 1, 8, 3).
crosses(47, 2, 9, 3).
crosses(47, 4, 11, 2).
crosses(47, 5, 12, 4).
crosses(47, 6, 13, 4).
crosses(47, 7, 14, 6).
crosses(47, 8, 15, 4).
crosses(47, 9, 16, 5).
crosses(47, 0, 8, 3).
crosses(47, 1, 9, 4).
crosses(47, 3, 11, 2).
crosses(47, 4, 12, 3).
crosses(47, 5, 13, 4).
crosses(47, 6, 14, 5).
crosses(47, 7, 15, 4).
crosses(47, 8, 16, 4).
crosses(47, 0, 9, 4).
crosses(47, 2, 11, 1).
crosses(47, 3, 12, 3).
crosses(47, 4, 13, 3).
crosses(47, 5, 14, 5).
crosses(47, 6, 15, 3).
crosses(47, 7, 16, 4).
crosses(47, 1, 11, 1).
crosses(47, 2, 12, 2).
crosses(47, 3, 13, 3).
crosses(47, 4, 14, 4).
crosses(47, 5, 15, 3).
crosses(47, 6, 16, 3).
crosses(47, 1, 12, 2).
crosses(47, 2, 13, 2).
crosses(47, 3, 14, 4).
crosses(47, 4, 15, 2).
crosses(47, 5, 16, 3).
crosses(47, 0, 12, 1).
crosses(47, 1, 13, 2).
crosses(47, 2, 14, 3).
crosses(47, 3, 15, 2).
crosses(47, 4, 16, 2).
crosses(47, 0, 13, 1).
crosses(47, 1, 14, 3).
crosses(47, 2, 15, 1).
crosses(47, 3, 16, 2).
crosses(47, 0, 14, 2).
crosses(47, 1, 15, 1).
crosses(47, 2, 16, 1).
crosses(47, 1, 16, 1).
crosses(48, 2, 4, 1).
crosses(48, 3, 5, 1).
crosses(48, 5, 7, 1).
crosses(48, 7, 9, 2).
crosses(48, 1, 4, 1).
crosses(48, 2, 5, 2).
crosses(48, 3, 6, 1).
crosses(48, 4, 7, 1).
crosses(48, 6, 9, 1).
crosses(48, 1, 5, 2).
crosses(48, 2, 6, 2).
crosses(48, 3, 7, 2).
crosses(48, 5, 9, 1).
crosses(48, 0, 5, 1).
crosses(48, 1, 6, 2).
crosses(48, 2, 7, 3).
crosses(48, 0, 6, 1).
crosses(48, 1, 7, 3).
crosses(48, 2, 8, 1).
crosses(48, 0, 7, 2).
crosses(48, 1, 8, 1).
crosses(48, 2, 9, 1).
crosses(48, 1, 9, 1).
crosses(49, 1, 3, 1).
crosses(49, 2, 4, 1).
crosses(49, 4, 6, 1).
crosses(49, 6, 8, 1).
crosses(49, 8, 10, 3).
crosses(49, 1, 4, 2).
crosses(49, 2, 5, 1).
crosses(49, 3, 6, 1).
crosses(49, 4, 7, 1).
crosses(49, 5, 8, 1).
crosses(49, 7, 10, 2).
crosses(49, 0, 4, 1).
crosses(49, 1, 5, 2).
crosses(49, 2, 6, 2).
crosses(49, 3, 7, 1).
crosses(49, 4, 8, 2).
crosses(49, 6, 10, 2).
crosses(49, 0, 5, 1).
crosses(49, 1, 6, 3).
crosses(49, 2, 7, 2).
crosses(49, 3, 8, 2).
crosses(49, 5, 10, 1).
crosses(49, 0, 6, 2).
crosses(49, 1, 7, 3).
crosses(49, 2, 8, 3).
crosses(49, 4, 10, 1).
crosses(49, 0, 7, 2).
crosses(49, 1, 8, 4).
crosses(49, 0, 8, 3).
crosses(49, 1, 9, 1).
crosses(49, 1, 10, 1).
crosses(50, 1, 3, 1).
crosses(50, 2, 4, 1).
crosses(50, 4, 6, 1).
crosses(50, 1, 4, 2).
crosses(50, 0, 4, 1).
crosses(50, 1, 5, 1).
crosses(50, 1, 6, 1).
crosses(51, 1, 3, 1).
crosses(51, 3, 5, 1).
crosses(51, 5, 7, 1).
crosses(51, 7, 9, 1).
crosses(51, 9, 11, 5).
crosses(51, 10, 12, 1).
crosses(51, 12, 14, 1).
crosses(51, 14, 16, 2).
crosses(51, 0, 3, 1).
crosses(51, 1, 4, 1).
crosses(51, 2, 5, 1).
crosses(51, 3, 6, 1).
crosses(51, 4, 7, 1).
crosses(51, 5, 8, 1).
crosses(51, 6, 9, 1).
crosses(51, 8, 11, 4).
crosses(51, 9, 12, 6).
crosses(51, 10, 13, 1).
crosses(51, 11, 14, 1).
crosses(51, 13, 16, 1).
crosses(51, 0, 4, 1).
crosses(51, 1, 5, 2).
crosses(51, 2, 6, 1).
crosses(51, 3, 7, 2).
crosses(51, 4, 8, 1).
crosses(51, 5, 9, 2).
crosses(51, 7, 11, 4).
crosses(51, 8, 12, 5).
crosses(51, 9, 13, 6).
crosses(51, 10, 14, 2).
crosses(51, 12, 16, 1).
crosses(51, 0, 5, 2).
crosses(51, 1, 6, 2).
crosses(51, 2, 7, 2).
crosses(51, 3, 8, 2).
crosses(51, 4, 9, 2).
crosses(51, 6, 11, 3).
crosses(51, 7, 12, 5).
crosses(51, 8, 13, 5).
crosses(51, 9, 14, 7).
crosses(51, 0, 6, 2).
crosses(51, 1, 7, 3).
crosses(51, 2, 8, 2).
crosses(51, 3, 9, 3).
crosses(51, 5, 11, 3).
crosses(51, 6, 12, 4).
crosses(51, 7, 13, 5).
crosses(51, 8, 14, 6).
crosses(51, 9, 15, 5).
crosses(51, 0, 7, 3).
crosses(51, 1, 8, 3).
crosses(51, 2, 9, 3).
crosses(51, 4, 11, 2).
crosses(51, 5, 12, 4).
crosses(51, 6, 13, 4).
crosses(51, 7, 14, 6).
crosses(51, 8, 15, 4).
crosses(51, 9, 16, 5).
crosses(51, 0, 8, 3).
crosses(51, 1, 9, 4).
crosses(51, 3, 11, 2).
crosses(51, 4, 12, 3).
crosses(51, 5, 13, 4).
crosses(51, 6, 14, 5).
crosses(51, 7, 15, 4).
crosses(51, 8, 16, 4).
crosses(51, 0, 9, 4).
crosses(51, 2, 11, 1).
crosses(51, 3, 12, 3).
crosses(51, 4, 13, 3).
crosses(51, 5, 14, 5).
crosses(51, 6, 15, 3).
crosses(51, 7, 16, 4).
crosses(51, 1, 11, 1).
crosses(51, 2, 12, 2).
crosses(51, 3, 13, 3).
crosses(51, 4, 14, 4).
crosses(51, 5, 15, 3).
crosses(51, 6, 16, 3).
crosses(51, 1, 12, 2).
crosses(51, 2, 13, 2).
crosses(51, 3, 14, 4).
crosses(51, 4, 15, 2).
crosses(51, 5, 16, 3).
crosses(51, 0, 12, 1).
crosses(51, 1, 13, 2).
crosses(51, 2, 14, 3).
crosses(51, 3, 15, 2).
crosses(51, 4, 16, 2).
crosses(51, 0, 13, 1).
crosses(51, 1, 14, 3).
crosses(51, 2, 15, 1).
crosses(51, 3, 16, 2).
crosses(51, 0, 14, 2).
crosses(51, 1, 15, 1).
crosses(51, 2, 16, 1).
crosses(51, 1, 16, 1).
crosses(52, 1, 3, 1).
crosses(52, 2, 4, 1).
crosses(52, 4, 6, 1).
crosses(52, 6, 8, 2).
crosses(52, 1, 4, 2).
crosses(52, 2, 5, 1).
crosses(52, 3, 6, 1).
crosses(52, 5, 8, 1).
crosses(52, 0, 4, 1).
crosses(52, 1, 5, 2).
crosses(52, 2, 6, 2).
crosses(52, 4, 8, 1).
crosses(52, 0, 5, 1).
crosses(52, 1, 6, 3).
crosses(52, 0, 6, 2).
crosses(52, 1, 7, 1).
crosses(52, 1, 8, 1).
crosses(53, 1, 3, 1).
crosses(53, 2, 4, 1).
crosses(53, 4, 6, 3).
crosses(53, 5, 7, 1).
crosses(53, 7, 9, 1).
crosses(53, 9, 11, 1).
crosses(53, 11, 13, 3).
crosses(53, 0, 3, 1).
crosses(53, 1, 4, 2).
crosses(53, 3, 6, 2).
crosses(53, 4, 7, 4).
crosses(53, 5, 8, 1).
crosses(53, 6, 9, 1).
crosses(53, 7, 10, 1).
crosses(53, 8, 11, 1).
crosses(53, 10, 13, 2).
crosses(53, 0, 4, 2).
crosses(53, 2, 6, 1).
crosses(53, 3, 7, 3).
crosses(53, 4, 8, 4).
crosses(53, 5, 9, 2).
crosses(53, 6, 10, 1).
crosses(53, 7, 11, 2).
crosses(53, 9, 13, 2).
crosses(53, 1, 6, 1).
crosses(53, 2, 7, 2).
crosses(53, 3, 8, 3).
crosses(53, 4, 9, 5).
crosses(53, 5, 10, 2).
crosses(53, 6, 11, 2).
crosses(53, 8, 13, 1).
crosses(53, 1, 7, 2).
crosses(53, 2, 8, 2).
crosses(53, 3, 9, 4).
crosses(53, 4, 10, 5).
crosses(53, 5, 11, 3).
crosses(53, 7, 13, 1).
crosses(53, 0, 7, 1).
crosses(53, 1, 8, 2).
crosses(53, 2, 9, 3).
crosses(53, 3, 10, 4).
crosses(53, 4, 11, 6).
crosses(53, 0, 8, 1).
crosses(53, 1, 9, 3).
crosses(53, 2, 10, 3).
crosses(53, 3, 11, 5).
crosses(53, 4, 12, 3).
crosses(53, 0, 9, 2).
crosses(53, 1, 10, 3).
crosses(53, 2, 11, 4).
crosses(53, 3, 12, 2).
crosses(53, 4, 13, 3).
crosses(53, 0, 10, 2).
crosses(53, 1, 11, 4).
crosses(53, 2, 12, 1).
crosses(53, 3, 13, 2).
crosses(53, 0, 11, 3).
crosses(53, 1, 12, 1).
crosses(53, 2, 13, 1).
crosses(53, 1, 13, 1).
crosses(54, 2, 4, 1).
crosses(54, 3, 5, 1).
crosses(54, 5, 7, 3).
crosses(54, 6, 8, 1).
crosses(54, 8, 10, 1).
crosses(54, 1, 4, 1).
crosses(54, 2, 5, 2).
crosses(54, 4, 7, 2).
crosses(54, 5, 8, 4).
crosses(54, 0, 4, 1).
crosses(54, 1, 5, 2).
crosses(54, 3, 7, 1).
crosses(54, 4, 8, 3).
crosses(54, 5, 9, 3).
crosses(54, 0, 5, 2).
crosses(54, 2, 7, 1).
crosses(54, 3, 8, 2).
crosses(54, 4, 9, 2).
crosses(54, 5, 10, 3).
crosses(54, 1, 7, 1).
crosses(54, 2, 8, 2).
crosses(54, 3, 9, 1).
crosses(54, 4, 10, 2).
crosses(54, 1, 8, 2).
crosses(54, 2, 9, 1).
crosses(54, 3, 10, 1).
crosses(54, 0, 8, 1).
crosses(54, 1, 9, 1).
crosses(54, 2, 10, 1).
crosses(54, 1, 10, 1).
crosses(55, 1, 3, 1).
crosses(55, 3, 5, 1).
crosses(55, 5, 7, 1).
crosses(55, 7, 9, 1).
crosses(55, 9, 11, 5).
crosses(55, 10, 12, 1).
crosses(55, 12, 14, 1).
crosses(55, 14, 16, 2).
crosses(55, 0, 3, 1).
crosses(55, 1, 4, 1).
crosses(55, 2, 5, 1).
crosses(55, 3, 6, 1).
crosses(55, 4, 7, 1).
crosses(55, 5, 8, 1).
crosses(55, 6, 9, 1).
crosses(55, 8, 11, 4).
crosses(55, 9, 12, 6).
crosses(55, 10, 13, 1).
crosses(55, 11, 14, 1).
crosses(55, 13, 16, 1).
crosses(55, 0, 4, 1).
crosses(55, 1, 5, 2).
crosses(55, 2, 6, 1).
crosses(55, 3, 7, 2).
crosses(55, 4, 8, 1).
crosses(55, 5, 9, 2).
crosses(55, 7, 11, 4).
crosses(55, 8, 12, 5).
crosses(55, 9, 13, 6).
crosses(55, 10, 14, 2).
crosses(55, 12, 16, 1).
crosses(55, 0, 5, 2).
crosses(55, 1, 6, 2).
crosses(55, 2, 7, 2).
crosses(55, 3, 8, 2).
crosses(55, 4, 9, 2).
crosses(55, 6, 11, 3).
crosses(55, 7, 12, 5).
crosses(55, 8, 13, 5).
crosses(55, 9, 14, 7).
crosses(55, 0, 6, 2).
crosses(55, 1, 7, 3).
crosses(55, 2, 8, 2).
crosses(55, 3, 9, 3).
crosses(55, 5, 11, 3).
crosses(55, 6, 12, 4).
crosses(55, 7, 13, 5).
crosses(55, 8, 14, 6).
crosses(55, 9, 15, 5).
crosses(55, 0, 7, 3).
crosses(55, 1, 8, 3).
crosses(55, 2, 9, 3).
crosses(55, 4, 11, 2).
crosses(55, 5, 12, 4).
crosses(55, 6, 13, 4).
crosses(55, 7, 14, 6).
crosses(55, 8, 15, 4).
crosses(55, 9, 16, 5).
crosses(55, 0, 8, 3).
crosses(55, 1, 9, 4).
crosses(55, 3, 11, 2).
crosses(55, 4, 12, 3).
crosses(55, 5, 13, 4).
crosses(55, 6, 14, 5).
crosses(55, 7, 15, 4).
crosses(55, 8, 16, 4).
crosses(55, 0, 9, 4).
crosses(55, 2, 11, 1).
crosses(55, 3, 12, 3).
crosses(55, 4, 13, 3).
crosses(55, 5, 14, 5).
crosses(55, 6, 15, 3).
crosses(55, 7, 16, 4).
crosses(55, 1, 11, 1).
crosses(55, 2, 12, 2).
crosses(55, 3, 13, 3).
crosses(55, 4, 14, 4).
crosses(55, 5, 15, 3).
crosses(55, 6, 16, 3).
crosses(55, 1, 12, 2).
crosses(55, 2, 13, 2).
crosses(55, 3, 14, 4).
crosses(55, 4, 15, 2).
crosses(55, 5, 16, 3).
crosses(55, 0, 12, 1).
crosses(55, 1, 13, 2).
crosses(55, 2, 14, 3).
crosses(55, 3, 15, 2).
crosses(55, 4, 16, 2).
crosses(55, 0, 13, 1).
crosses(55, 1, 14, 3).
crosses(55, 2, 15, 1).
crosses(55, 3, 16, 2).
crosses(55, 0, 14, 2).
crosses(55, 1, 15, 1).
crosses(55, 2, 16, 1).
crosses(55, 1, 16, 1).
crosses(56, 1, 3, 1).
crosses(56, 2, 4, 1).
crosses(56, 4, 6, 3).
crosses(56, 5, 7, 1).
crosses(56, 7, 9, 1).
crosses(56, 9, 11, 2).
crosses(56, 0, 3, 1).
crosses(56, 1, 4, 2).
crosses(56, 3, 6, 2).
crosses(56, 4, 7, 4).
crosses(56, 5, 8, 1).
crosses(56, 6, 9, 1).
crosses(56, 8, 11, 1).
crosses(56, 0, 4, 2).
crosses(56, 2, 6, 1).
crosses(56, 3, 7, 3).
crosses(56, 4, 8, 4).
crosses(56, 5, 9, 2).
crosses(56, 7, 11, 1).
crosses(56, 1, 6, 1).
crosses(56, 2, 7, 2).
crosses(56, 3, 8, 3).
crosses(56, 4, 9, 5).
crosses(56, 1, 7, 2).
crosses(56, 2, 8, 2).
crosses(56, 3, 9, 4).
crosses(56, 4, 10, 3).
crosses(56, 0, 7, 1).
crosses(56, 1, 8, 2).
crosses(56, 2, 9, 3).
crosses(56, 3, 10, 2).
crosses(56, 4, 11, 3).
crosses(56, 0, 8, 1).
crosses(56, 1, 9, 3).
crosses(56, 2, 10, 1).
crosses(56, 3, 11, 2).
crosses(56, 0, 9, 2).
crosses(56, 1, 10, 1).
crosses(56, 2, 11, 1).
crosses(56, 1, 11, 1).
crosses(57, 1, 3, 2).
crosses(57, 4, 6, 2).
crosses(57, 6, 8, 1).
crosses(57, 8, 10, 2).
crosses(57, 0, 3, 1).
crosses(57, 1, 4, 2).
crosses(57, 3, 6, 2).
crosses(57, 4, 7, 2).
crosses(57, 5, 8, 1).
crosses(57, 7, 10, 1).
crosses(57, 0, 4, 1).
crosses(57, 1, 5, 1).
crosses(57, 2, 6, 1).
crosses(57, 3, 7, 2).
crosses(57, 4, 8, 3).
crosses(57, 6, 10, 1).
crosses(57, 1, 6, 2).
crosses(57, 2, 7, 1).
crosses(57, 3, 8, 3).
crosses(57, 4, 9, 1).
crosses(57, 0, 6, 1).
crosses(57, 1, 7, 2).
crosses(57, 2, 8, 2).
crosses(57, 3, 9, 1).
crosses(57, 4, 10, 1).
crosses(57, 0, 7, 1).
crosses(57, 1, 8, 3).
crosses(57, 3, 10, 1).
crosses(57, 0, 8, 2).
crosses(57, 1, 9, 1).
crosses(57, 1, 10, 1).
crosses(58, 1, 3, 2).
crosses(58, 4, 6, 2).
crosses(58, 6, 8, 1).
crosses(58, 0, 3, 1).
crosses(58, 1, 4, 2).
crosses(58, 3, 6, 2).
crosses(58, 4, 7, 1).
crosses(58, 0, 4, 1).
crosses(58, 1, 5, 1).
crosses(58, 2, 6, 1).
crosses(58, 3, 7, 1).
crosses(58, 4, 8, 1).
crosses(58, 1, 6, 2).
crosses(58, 3, 8, 1).
crosses(58, 0, 6, 1).
crosses(58, 1, 7, 1).
crosses(58, 1, 8, 1).
crosses(59, 1, 3, 2).
crosses(59, 3, 5, 2).
crosses(59, 5, 7, 1).
crosses(59, 7, 9, 2).
crosses(59, 0, 3, 1).
crosses(59, 1, 4, 1).
crosses(59, 2, 5, 1).
crosses(59, 3, 6, 2).
crosses(59, 4, 7, 1).
crosses(59, 6, 9, 1).
crosses(59, 1, 5, 2).
crosses(59, 2, 6, 1).
crosses(59, 3, 7, 3).
crosses(59, 5, 9, 1).
crosses(59, 0, 5, 1).
crosses(59, 1, 6, 2).
crosses(59, 2, 7, 2).
crosses(59, 3, 8, 1).
crosses(59, 0, 6, 1).
crosses(59, 1, 7, 3).
crosses(59, 3, 9, 1).
crosses(59, 0, 7, 2).
crosses(59, 1, 8, 1).
crosses(59, 1, 9, 1).
crosses(60, 1, 3, 2).
crosses(60, 3, 5, 2).
crosses(60, 5, 7, 1).
crosses(60, 0, 3, 1).
crosses(60, 1, 4, 1).
crosses(60, 2, 5, 1).
crosses(60, 3, 6, 1).
crosses(60, 1, 5, 2).
crosses(60, 3, 7, 1).
crosses(60, 0, 5, 1).
crosses(60, 1, 6, 1).
crosses(60, 1, 7, 1).
crosses(61, 2, 4, 2).
crosses(61, 4, 6, 2).
crosses(61, 7, 9, 2).
crosses(61, 10, 12, 1).
crosses(61, 12, 14, 2).
crosses(61, 1, 4, 2).
crosses(61, 2, 5, 1).
crosses(61, 3, 6, 1).
crosses(61, 4, 7, 2).
crosses(61, 6, 9, 2).
crosses(61, 7, 10, 2).
crosses(61, 9, 12, 1).
crosses(61, 11, 14, 1).
crosses(61, 0, 4, 1).
crosses(61, 1, 5, 1).
crosses(61, 2, 6, 2).
crosses(61, 3, 7, 1).
crosses(61, 4, 8, 1).
crosses(61, 5, 9, 1).
crosses(61, 6, 10, 2).
crosses(61, 7, 11, 2).
crosses(61, 8, 12, 1).
crosses(61, 10, 14, 1).
crosses(61, 1, 6, 2).
crosses(61, 2, 7, 2).
crosses(61, 4, 9, 2).
crosses(61, 5, 10, 1).
crosses(61, 6, 11, 2).
crosses(61, 7, 12, 3).
crosses(61, 9, 14, 1).
crosses(61, 0, 6, 1).
crosses(61, 1, 7, 2).
crosses(61, 2, 8, 1).
crosses(61, 3, 9, 1).
crosses(61, 4, 10, 2).
crosses(61, 5, 11, 1).
crosses(61, 6, 12, 3).
crosses(61, 7, 13, 1).
crosses(61, 0, 7, 1).
crosses(61, 1, 8, 1).
crosses(61, 2, 9, 2).
crosses(61, 3, 10, 1).
crosses(61, 4, 11, 2).
crosses(61, 5, 12, 2).
crosses(61, 6, 13, 1).
crosses(61, 7, 14, 1).
crosses(61, 1, 9, 2).
crosses(61, 2, 10, 2).
crosses(61, 3, 11, 1).
crosses(61, 4, 12, 3).
crosses(61, 6, 14, 1).
crosses(61, 0, 9, 1).
crosses(61, 1, 10, 2).
crosses(61, 2, 11, 2).
crosses(61, 3, 12, 2).
crosses(61, 4, 13, 1).
crosses(61, 0, 10, 1).
crosses(61, 1, 11, 2).
crosses(61, 2, 12, 3).
crosses(61, 4, 14, 1).
crosses(61, 0, 11, 1).
crosses(61, 1, 12, 3).
crosses(61, 2, 13, 1).
crosses(61, 0, 12, 2).
crosses(61, 1, 13, 1).
crosses(61, 2, 14, 1).
crosses(61, 1, 14, 1).
crosses(62, 2, 4, 2).
crosses(62, 4, 6, 2).
crosses(62, 6, 8, 2).
crosses(62, 9, 11, 1).
crosses(62, 11, 13, 2).
crosses(62, 1, 4, 2).
crosses(62, 2, 5, 1).
crosses(62, 3, 6, 1).
crosses(62, 4, 7, 1).
crosses(62, 5, 8, 1).
crosses(62, 6, 9, 2).
crosses(62, 8, 11, 1).
crosses(62, 10, 13, 1).
crosses(62, 0, 4, 1).
crosses(62, 1, 5, 1).
crosses(62, 2, 6, 2).
crosses(62, 4, 8, 2).
crosses(62, 5, 9, 1).
crosses(62, 6, 10, 2).
crosses(62, 7, 11, 1).
crosses(62, 9, 13, 1).
crosses(62, 1, 6, 2).
crosses(62, 2, 7, 1).
crosses(62, 3, 8, 1).
crosses(62, 4, 9, 2).
crosses(62, 5, 10, 1).
crosses(62, 6, 11, 3).
crosses(62, 8, 13, 1).
crosses(62, 0, 6, 1).
crosses(62, 1, 7, 1).
crosses(62, 2, 8, 2).
crosses(62, 3, 9, 1).
crosses(62, 4, 10, 2).
crosses(62, 5, 11, 2).
crosses(62, 6, 12, 1).
crosses(62, 1, 8, 2).
crosses(62, 2, 9, 2).
crosses(62, 3, 10, 1).
crosses(62, 4, 11, 3).
crosses(62, 6, 13, 1).
crosses(62, 0, 8, 1).
crosses(62, 1, 9, 2).
crosses(62, 2, 10, 2).
crosses(62, 3, 11, 2).
crosses(62, 4, 12, 1).
crosses(62, 0, 9, 1).
crosses(62, 1, 10, 2).
crosses(62, 2, 11, 3).
crosses(62, 4, 13, 1).
crosses(62, 0, 10, 1).
crosses(62, 1, 11, 3).
crosses(62, 2, 12, 1).
crosses(62, 0, 11, 2).
crosses(62, 1, 12, 1).
crosses(62, 2, 13, 1).
crosses(62, 1, 13, 1).
crosses(63, 1, 3, 2).
crosses(63, 3, 5, 2).
crosses(63, 5, 7, 2).
crosses(63, 6, 8, 1).
crosses(63, 10, 12, 2).
crosses(63, 0, 3, 1).
crosses(63, 1, 4, 1).
crosses(63, 2, 5, 1).
crosses(63, 3, 6, 1).
crosses(63, 4, 7, 1).
crosses(63, 5, 8, 3).
crosses(63, 6, 9, 1).
crosses(63, 9, 12, 2).
crosses(63, 1, 5, 2).
crosses(63, 3, 7, 2).
crosses(63, 4, 8, 2).
crosses(63, 5, 9, 3).
crosses(63, 6, 10, 1).
crosses(63, 8, 12, 2).
crosses(63, 0, 5, 1).
crosses(63, 1, 6, 1).
crosses(63, 2, 7, 1).
crosses(63, 3, 8, 3).
crosses(63, 4, 9, 2).
crosses(63, 5, 10, 3).
crosses(63, 7, 12, 1).
crosses(63, 1, 7, 2).
crosses(63, 2, 8, 2).
crosses(63, 3, 9, 3).
crosses(63, 4, 10, 2).
crosses(63, 5, 11, 1).
crosses(63, 0, 7, 1).
crosses(63, 1, 8, 3).
crosses(63, 2, 9, 2).
crosses(63, 3, 10, 3).
crosses(63, 5, 12, 1).
crosses(63, 0, 8, 2).
crosses(63, 1, 9, 3).
crosses(63, 2, 10, 2).
crosses(63, 3, 11, 1).
crosses(63, 0, 9, 2).
crosses(63, 1, 10, 3).
crosses(63, 3, 12, 1).
crosses(63, 0, 10, 2).
crosses(63, 1, 11, 1).
crosses(63, 1, 12, 1).
crosses(64, 1, 3, 2).
crosses(64, 3, 5, 2).
crosses(64, 5, 7, 1).
crosses(64, 0, 3, 1).
crosses(64, 1, 4, 1).
crosses(64, 2, 5, 1).
crosses(64, 3, 6, 1).
crosses(64, 1, 5, 2).
crosses(64, 3, 7, 1).
crosses(64, 0, 5, 1).
crosses(64, 1, 6, 1).
crosses(64, 1, 7, 1).
crosses(65, 1, 3, 1).
crosses(65, 3, 5, 2).
crosses(65, 4, 6, 1).
crosses(65, 6, 8, 1).
crosses(65, 0, 3, 1).
crosses(65, 2, 5, 1).
crosses(65, 3, 6, 3).
crosses(65, 1, 5, 1).
crosses(65, 2, 6, 2).
crosses(65, 3, 7, 2).
crosses(65, 1, 6, 2).
crosses(65, 2, 7, 1).
crosses(65, 3, 8, 2).
crosses(65, 0, 6, 1).
crosses(65, 1, 7, 1).
crosses(65, 2, 8, 1).
crosses(65, 1, 8, 1).
crosses(66, 2, 4, 1).
crosses(66, 3, 5, 1).
crosses(66, 5, 7, 1).
crosses(66, 1, 4, 1).
crosses(66, 2, 5, 2).
crosses(66, 1, 5, 2).
crosses(66, 2, 6, 1).
crosses(66, 0, 5, 1).
crosses(66, 1, 6, 1).
crosses(66, 2, 7, 1).
crosses(66, 1, 7, 1).
crosses(67, 2, 4, 1).
crosses(67, 4, 6, 1).
crosses(67, 6, 8, 3).
crosses(67, 7, 9, 1).
crosses(67, 9, 11, 1).
crosses(67, 11, 13, 2).
crosses(67, 1, 4, 1).
crosses(67, 2, 5, 1).
crosses(67, 3, 6, 1).
crosses(67, 5, 8, 2).
crosses(67, 6, 9, 4).
crosses(67, 7, 10, 1).
crosses(67, 8, 11, 1).
crosses(67, 10, 13, 1).
crosses(67, 0, 4, 1).
crosses(67, 1, 5, 1).
crosses(67, 2, 6, 2).
crosses(67, 4, 8, 2).
crosses(67, 5, 9, 3).
crosses(67, 6, 10, 4).
crosses(67, 7, 11, 2).
crosses(67, 9, 13, 1).
crosses(67, 0, 5, 1).
crosses(67, 1, 6, 2).
crosses(67, 3, 8, 1).
crosses(67, 4, 9, 3).
crosses(67, 5, 10, 3).
crosses(67, 6, 11, 5).
crosses(67, 0, 6, 2).
crosses(67, 2, 8, 1).
crosses(67, 3, 9, 2).
crosses(67, 4, 10, 3).
crosses(67, 5, 11, 4).
crosses(67, 6, 12, 3).
crosses(67, 1, 8, 1).
crosses(67, 2, 9, 2).
crosses(67, 3, 10, 2).
crosses(67, 4, 11, 4).
crosses(67, 5, 12, 2).
crosses(67, 6, 13, 3).
crosses(67, 1, 9, 2).
crosses(67, 2, 10, 2).
crosses(67, 3, 11, 3).
crosses(67, 4, 12, 2).
crosses(67, 5, 13, 2).
crosses(67, 0, 9, 1).
crosses(67, 1, 10, 2).
crosses(67, 2, 11, 3).
crosses(67, 3, 12, 1).
crosses(67, 4, 13, 2).
crosses(67, 0, 10, 1).
crosses(67, 1, 11, 3).
crosses(67, 2, 12, 1).
crosses(67, 3, 13, 1).
crosses(67, 0, 11, 2).
crosses(67, 1, 12, 1).
crosses(67, 2, 13, 1).
crosses(67, 1, 13, 1).
crosses(68, 1, 3, 1).
crosses(68, 3, 5, 2).
crosses(68, 4, 6, 1).
crosses(68, 6, 8, 1).
crosses(68, 8, 10, 2).
crosses(68, 0, 3, 1).
crosses(68, 2, 5, 1).
crosses(68, 3, 6, 3).
crosses(68, 4, 7, 1).
crosses(68, 5, 8, 1).
crosses(68, 7, 10, 1).
crosses(68, 1, 5, 1).
crosses(68, 2, 6, 2).
crosses(68, 3, 7, 3).
crosses(68, 4, 8, 2).
crosses(68, 6, 10, 1).
crosses(68, 1, 6, 2).
crosses(68, 2, 7, 2).
crosses(68, 3, 8, 4).
crosses(68, 0, 6, 1).
crosses(68, 1, 7, 2).
crosses(68, 2, 8, 3).
crosses(68, 3, 9, 2).
crosses(68, 0, 7, 1).
crosses(68, 1, 8, 3).
crosses(68, 2, 9, 1).
crosses(68, 3, 10, 2).
crosses(68, 0, 8, 2).
crosses(68, 1, 9, 1).
crosses(68, 2, 10, 1).
crosses(68, 1, 10, 1).
crosses(69, 1, 3, 1).
crosses(69, 3, 5, 2).
crosses(69, 4, 6, 1).
crosses(69, 6, 8, 1).
crosses(69, 0, 3, 1).
crosses(69, 2, 5, 1).
crosses(69, 3, 6, 3).
crosses(69, 1, 5, 1).
crosses(69, 2, 6, 2).
crosses(69, 3, 7, 2).
crosses(69, 1, 6, 2).
crosses(69, 2, 7, 1).
crosses(69, 3, 8, 2).
crosses(69, 0, 6, 1).
crosses(69, 1, 7, 1).
crosses(69, 2, 8, 1).
crosses(69, 1, 8, 1).
crosses(70, 2, 4, 1).
crosses(70, 4, 6, 2).
crosses(70, 5, 7, 1).
crosses(70, 7, 9, 1).
crosses(70, 1, 4, 1).
crosses(70, 3, 6, 1).
crosses(70, 4, 7, 3).
crosses(70, 0, 4, 1).
crosses(70, 2, 6, 1).
crosses(70, 3, 7, 2).
crosses(70, 4, 8, 2).
crosses(70, 1, 6, 1).
crosses(70, 2, 7, 2).
crosses(70, 3, 8, 1).
crosses(70, 4, 9, 2).
crosses(70, 1, 7, 2).
crosses(70, 2, 8, 1).
crosses(70, 3, 9, 1).
crosses(70, 0, 7, 1).
crosses(70, 1, 8, 1).
crosses(70, 2, 9, 1).
crosses(70, 1, 9, 1).
crosses(71, 2, 4, 1).
crosses(71, 4, 6, 1).
crosses(71, 6, 8, 3).
crosses(71, 7, 9, 1).
crosses(71, 9, 11, 1).
crosses(71, 1, 4, 1).
crosses(71, 2, 5, 1).
crosses(71, 3, 6, 1).
crosses(71, 5, 8, 2).
crosses(71, 6, 9, 4).
crosses(71, 0, 4, 1).
crosses(71, 1, 5, 1).
crosses(71, 2, 6, 2).
crosses(71, 4, 8, 2).
crosses(71, 5, 9, 3).
crosses(71, 6, 10, 3).
crosses(71, 0, 5, 1).
crosses(71, 1, 6, 2).
crosses(71, 3, 8, 1).
crosses(71, 4, 9, 3).
crosses(71, 5, 10, 2).
crosses(71, 6, 11, 3).
crosses(71, 0, 6, 2).
crosses(71, 2, 8, 1).
crosses(71, 3, 9, 2).
crosses(71, 4, 10, 2).
crosses(71, 5, 11, 2).
crosses(71, 1, 8, 1).
crosses(71, 2, 9, 2).
crosses(71, 3, 10, 1).
crosses(71, 4, 11, 2).
crosses(71, 1, 9, 2).
crosses(71, 2, 10, 1).
crosses(71, 3, 11, 1).
crosses(71, 0, 9, 1).
crosses(71, 1, 10, 1).
crosses(71, 2, 11, 1).
crosses(71, 1, 11, 1).
crosses(72, 1, 3, 1).
crosses(72, 3, 5, 1).
crosses(72, 5, 7, 2).
crosses(72, 6, 8, 1).
crosses(72, 8, 10, 1).
crosses(72, 0, 3, 1).
crosses(72, 1, 4, 1).
crosses(72, 2, 5, 1).
crosses(72, 4, 7, 1).
crosses(72, 5, 8, 3).
crosses(72, 0, 4, 1).
crosses(72, 1, 5, 2).
crosses(72, 3, 7, 1).
crosses(72, 4, 8, 2).
crosses(72, 5, 9, 2).
crosses(72, 0, 5, 2).
crosses(72, 3, 8, 2).
crosses(72, 4, 9, 1).
crosses(72, 5, 10, 2).
crosses(72, 2, 8, 1).
crosses(72, 3, 9, 1).
crosses(72, 4, 10, 1).
crosses(72, 1, 8, 1).
crosses(72, 3, 10, 1).
crosses(72, 0, 8, 1).
crosses(73, 0, 2, 1).
crosses(73, 1, 3, 1).
crosses(73, 2, 4, 1).
crosses(73, 4, 6, 4).
crosses(73, 5, 7, 1).
crosses(73, 7, 9, 1).
crosses(73, 0, 3, 2).
crosses(73, 1, 4, 2).
crosses(73, 3, 6, 3).
crosses(73, 4, 7, 5).
crosses(73, 0, 4, 3).
crosses(73, 2, 6, 2).
crosses(73, 3, 7, 4).
crosses(73, 4, 8, 4).
crosses(73, 1, 6, 1).
crosses(73, 2, 7, 3).
crosses(73, 3, 8, 3).
crosses(73, 4, 9, 4).
crosses(73, 1, 7, 2).
crosses(73, 2, 8, 2).
crosses(73, 3, 9, 3).
crosses(73, 0, 7, 1).
crosses(73, 1, 8, 1).
crosses(73, 2, 9, 2).
crosses(73, 1, 9, 1).
crosses(74, 2, 4, 1).
crosses(74, 4, 6, 2).
crosses(74, 5, 7, 1).
crosses(74, 7, 9, 1).
crosses(74, 1, 4, 1).
crosses(74, 3, 6, 1).
crosses(74, 4, 7, 3).
crosses(74, 0, 4, 1).
crosses(74, 2, 6, 1).
crosses(74, 3, 7, 2).
crosses(74, 4, 8, 2).
crosses(74, 1, 6, 1).
crosses(74, 2, 7, 2).
crosses(74, 3, 8, 1).
crosses(74, 4, 9, 2).
crosses(74, 1, 7, 2).
crosses(74, 2, 8, 1).
crosses(74, 3, 9, 1).
crosses(74, 0, 7, 1).
crosses(74, 1, 8, 1).
crosses(74, 2, 9, 1).
crosses(74, 1, 9, 1).
crosses(75, 2, 4, 1).
crosses(75, 4, 6, 1).
crosses(75, 6, 8, 3).
crosses(75, 7, 9, 1).
crosses(75, 9, 11, 2).
crosses(75, 10, 12, 1).
crosses(75, 12, 14, 2).
crosses(75, 1, 4, 1).
crosses(75, 2, 5, 1).
crosses(75, 3, 6, 1).
crosses(75, 5, 8, 2).
crosses(75, 6, 9, 4).
crosses(75, 8, 11, 1).
crosses(75, 9, 12, 3).
crosses(75, 11, 14, 1).
crosses(75, 0, 4, 1).
crosses(75, 1, 5, 1).
crosses(75, 2, 6, 2).
crosses(75, 4, 8, 2).
crosses(75, 5, 9, 3).
crosses(75, 6, 10, 3).
crosses(75, 7, 11, 1).
crosses(75, 8, 12, 2).
crosses(75, 9, 13, 1).
crosses(75, 0, 5, 1).
crosses(75, 1, 6, 2).
crosses(75, 3, 8, 1).
crosses(75, 4, 9, 3).
crosses(75, 5, 10, 2).
crosses(75, 6, 11, 4).
crosses(75, 7, 12, 2).
crosses(75, 9, 14, 1).
crosses(75, 0, 6, 2).
crosses(75, 2, 8, 1).
crosses(75, 3, 9, 2).
crosses(75, 4, 10, 2).
crosses(75, 5, 11, 3).
crosses(75, 6, 12, 5).
crosses(75, 1, 8, 1).
crosses(75, 2, 9, 2).
crosses(75, 3, 10, 1).
crosses(75, 4, 11, 3).
crosses(75, 5, 12, 4).
crosses(75, 6, 13, 3).
crosses(75, 1, 9, 2).
crosses(75, 2, 10, 1).
crosses(75, 3, 11, 2).
crosses(75, 4, 12, 4).
crosses(75, 5, 13, 2).
crosses(75, 6, 14, 3).
crosses(75, 0, 9, 1).
crosses(75, 1, 10, 1).
crosses(75, 2, 11, 2).
crosses(75, 3, 12, 3).
crosses(75, 4, 13, 2).
crosses(75, 5, 14, 2).
crosses(75, 1, 11, 2).
crosses(75, 2, 12, 3).
crosses(75, 3, 13, 1).
crosses(75, 4, 14, 2).
crosses(75, 0, 11, 1).
crosses(75, 1, 12, 3).
crosses(75, 2, 13, 1).
crosses(75, 3, 14, 1).
crosses(75, 0, 12, 2).
crosses(75, 1, 13, 1).
crosses(75, 2, 14, 1).
crosses(75, 1, 14, 1).
crosses(76, 2, 4, 2).
crosses(76, 3, 5, 1).
crosses(76, 4, 6, 1).
crosses(76, 5, 7, 1).
crosses(76, 6, 8, 1).
crosses(76, 7, 9, 1).
crosses(76, 9, 11, 1).
crosses(76, 11, 13, 8).
crosses(76, 12, 14, 1).
crosses(76, 14, 16, 2).
crosses(76, 1, 4, 2).
crosses(76, 2, 5, 3).
crosses(76, 3, 6, 2).
crosses(76, 4, 7, 2).
crosses(76, 5, 8, 2).
crosses(76, 6, 9, 2).
crosses(76, 7, 10, 1).
crosses(76, 8, 11, 1).
crosses(76, 10, 13, 7).
crosses(76, 11, 14, 9).
crosses(76, 13, 16, 1).
crosses(76, 0, 4, 1).
crosses(76, 1, 5, 3).
crosses(76, 2, 6, 4).
crosses(76, 3, 7, 3).
crosses(76, 4, 8, 3).
crosses(76, 5, 9, 3).
crosses(76, 6, 10, 2).
crosses(76, 7, 11, 2).
crosses(76, 9, 13, 7).
crosses(76, 10, 14, 8).
crosses(76, 11, 15, 7).
crosses(76, 0, 5, 2).
crosses(76, 1, 6, 4).
crosses(76, 2, 7, 5).
crosses(76, 3, 8, 4).
crosses(76, 4, 9, 4).
crosses(76, 5, 10, 3).
crosses(76, 6, 11, 3).
crosses(76, 8, 13, 6).
crosses(76, 9, 14, 8).
crosses(76, 10, 15, 6).
crosses(76, 11, 16, 7).
crosses(76, 0, 6, 3).
crosses(76, 1, 7, 5).
crosses(76, 2, 8, 6).
crosses(76, 3, 9, 5).
crosses(76, 4, 10, 4).
crosses(76, 5, 11, 4).
crosses(76, 7, 13, 5).
crosses(76, 8, 14, 7).
crosses(76, 9, 15, 6).
crosses(76, 10, 16, 6).
crosses(76, 0, 7, 4).
crosses(76, 1, 8, 6).
crosses(76, 2, 9, 7).
crosses(76, 3, 10, 5).
crosses(76, 4, 11, 5).
crosses(76, 6, 13, 4).
crosses(76, 7, 14, 6).
crosses(76, 8, 15, 5).
crosses(76, 9, 16, 6).
crosses(76, 0, 8, 5).
crosses(76, 1, 9, 7).
crosses(76, 2, 10, 7).
crosses(76, 3, 11, 6).
crosses(76, 5, 13, 3).
crosses(76, 6, 14, 5).
crosses(76, 7, 15, 4).
crosses(76, 8, 16, 5).
crosses(76, 0, 9, 6).
crosses(76, 1, 10, 7).
crosses(76, 2, 11, 8).
crosses(76, 4, 13, 2).
crosses(76, 5, 14, 4).
crosses(76, 6, 15, 3).
crosses(76, 7, 16, 4).
crosses(76, 0, 10, 6).
crosses(76, 1, 11, 8).
crosses(76, 2, 12, 1).
crosses(76, 3, 13, 1).
crosses(76, 4, 14, 3).
crosses(76, 5, 15, 2).
crosses(76, 6, 16, 3).
crosses(76, 0, 11, 7).
crosses(76, 1, 12, 1).
crosses(76, 2, 13, 2).
crosses(76, 3, 14, 2).
crosses(76, 4, 15, 1).
crosses(76, 5, 16, 2).
crosses(76, 1, 13, 2).
crosses(76, 2, 14, 3).
crosses(76, 4, 16, 1).
crosses(76, 0, 13, 1).
crosses(76, 1, 14, 3).
crosses(76, 2, 15, 1).
crosses(76, 0, 14, 2).
crosses(76, 1, 15, 1).
crosses(76, 2, 16, 1).
crosses(76, 1, 16, 1).
crosses(77, 2, 4, 2).
crosses(77, 4, 6, 2).
crosses(77, 6, 8, 1).
crosses(77, 8, 10, 2).
crosses(77, 9, 11, 1).
crosses(77, 11, 13, 3).
crosses(77, 1, 4, 2).
crosses(77, 2, 5, 1).
crosses(77, 3, 6, 1).
crosses(77, 4, 7, 2).
crosses(77, 5, 8, 1).
crosses(77, 7, 10, 1).
crosses(77, 8, 11, 3).
crosses(77, 10, 13, 2).
crosses(77, 0, 4, 1).
crosses(77, 1, 5, 1).
crosses(77, 2, 6, 2).
crosses(77, 3, 7, 1).
crosses(77, 4, 8, 3).
crosses(77, 6, 10, 1).
crosses(77, 7, 11, 2).
crosses(77, 8, 12, 1).
crosses(77, 9, 13, 1).
crosses(77, 1, 6, 2).
crosses(77, 2, 7, 2).
crosses(77, 3, 8, 2).
crosses(77, 4, 9, 2).
crosses(77, 5, 10, 1).
crosses(77, 6, 11, 2).
crosses(77, 8, 13, 2).
crosses(77, 0, 6, 1).
crosses(77, 1, 7, 2).
crosses(77, 2, 8, 3).
crosses(77, 3, 9, 1).
crosses(77, 4, 10, 3).
crosses(77, 5, 11, 2).
crosses(77, 7, 13, 1).
crosses(77, 0, 7, 1).
crosses(77, 1, 8, 3).
crosses(77, 2, 9, 2).
crosses(77, 3, 10, 2).
crosses(77, 4, 11, 4).
crosses(77, 6, 13, 1).
crosses(77, 0, 8, 2).
crosses(77, 1, 9, 2).
crosses(77, 2, 10, 3).
crosses(77, 3, 11, 3).
crosses(77, 4, 12, 1).
crosses(77, 0, 9, 1).
crosses(77, 1, 10, 3).
crosses(77, 2, 11, 4).
crosses(77, 4, 13, 1).
crosses(77, 0, 10, 2).
crosses(77, 1, 11, 4).
crosses(77, 2, 12, 1).
crosses(77, 0, 11, 3).
crosses(77, 1, 12, 1).
crosses(77, 2, 13, 1).
crosses(77, 1, 13, 1).
crosses(78, 2, 4, 2).
crosses(78, 4, 6, 2).
crosses(78, 8, 10, 1).
crosses(78, 1, 4, 2).
crosses(78, 2, 5, 1).
crosses(78, 3, 6, 1).
crosses(78, 4, 7, 2).
crosses(78, 7, 10, 1).
crosses(78, 0, 4, 1).
crosses(78, 1, 5, 1).
crosses(78, 2, 6, 2).
crosses(78, 3, 7, 1).
crosses(78, 4, 8, 2).
crosses(78, 6, 10, 1).
crosses(78, 1, 6, 2).
crosses(78, 2, 7, 2).
crosses(78, 3, 8, 1).
crosses(78, 4, 9, 1).
crosses(78, 0, 6, 1).
crosses(78, 1, 7, 2).
crosses(78, 2, 8, 2).
crosses(78, 4, 10, 1).
crosses(78, 0, 7, 1).
crosses(78, 1, 8, 2).
crosses(78, 2, 9, 1).
crosses(78, 0, 8, 1).
crosses(78, 1, 9, 1).
crosses(78, 2, 10, 1).
crosses(78, 1, 10, 1).
crosses(79, 0, 2, 1).
crosses(79, 1, 3, 1).
crosses(79, 3, 5, 4).
crosses(79, 5, 7, 2).
crosses(79, 8, 10, 2).
crosses(79, 9, 11, 1).
crosses(79, 11, 13, 2).
crosses(79, 0, 3, 2).
crosses(79, 2, 5, 3).
crosses(79, 3, 6, 3).
crosses(79, 4, 7, 1).
crosses(79, 5, 8, 2).
crosses(79, 7, 10, 2).
crosses(79, 8, 11, 3).
crosses(79, 10, 13, 1).
crosses(79, 1, 5, 2).
crosses(79, 2, 6, 2).
crosses(79, 3, 7, 4).
crosses(79, 4, 8, 1).
crosses(79, 5, 9, 1).
crosses(79, 6, 10, 1).
crosses(79, 7, 11, 3).
crosses(79, 8, 12, 1).
crosses(79, 0, 5, 1).
crosses(79, 1, 6, 1).
crosses(79, 2, 7, 3).
crosses(79, 3, 8, 4).
crosses(79, 5, 10, 2).
crosses(79, 6, 11, 2).
crosses(79, 7, 12, 1).
crosses(79, 8, 13, 1).
crosses(79, 1, 7, 2).
crosses(79, 2, 8, 3).
crosses(79, 3, 9, 3).
crosses(79, 4, 10, 1).
crosses(79, 5, 11, 3).
crosses(79, 7, 13, 1).
crosses(79, 0, 7, 1).
crosses(79, 1, 8, 2).
crosses(79, 2, 9, 2).
crosses(79, 3, 10, 4).
crosses(79, 4, 11, 2).
crosses(79, 5, 12, 1).
crosses(79, 0, 8, 1).
crosses(79, 1, 9, 1).
crosses(79, 2, 10, 3).
crosses(79, 3, 11, 5).
crosses(79, 5, 13, 1).
crosses(79, 1, 10, 2).
crosses(79, 2, 11, 4).
crosses(79, 3, 12, 3).
crosses(79, 0, 10, 1).
crosses(79, 1, 11, 3).
crosses(79, 2, 12, 2).
crosses(79, 3, 13, 3).
crosses(79, 0, 11, 2).
crosses(79, 1, 12, 1).
crosses(79, 2, 13, 2).
crosses(79, 1, 13, 1).
crosses(80, 0, 2, 1).
crosses(80, 1, 3, 1).
crosses(80, 3, 5, 4).
crosses(80, 7, 9, 2).
crosses(80, 8, 10, 1).
crosses(80, 10, 12, 2).
crosses(80, 0, 3, 2).
crosses(80, 2, 5, 3).
crosses(80, 3, 6, 4).
crosses(80, 6, 9, 2).
crosses(80, 7, 10, 3).
crosses(80, 9, 12, 1).
crosses(80, 1, 5, 2).
crosses(80, 2, 6, 3).
crosses(80, 3, 7, 4).
crosses(80, 5, 9, 2).
crosses(80, 6, 10, 3).
crosses(80, 7, 11, 1).
crosses(80, 0, 5, 1).
crosses(80, 1, 6, 2).
crosses(80, 2, 7, 3).
crosses(80, 3, 8, 3).
crosses(80, 4, 9, 1).
crosses(80, 5, 10, 3).
crosses(80, 6, 11, 1).
crosses(80, 7, 12, 1).
crosses(80, 0, 6, 1).
crosses(80, 1, 7, 2).
crosses(80, 2, 8, 2).
crosses(80, 3, 9, 4).
crosses(80, 4, 10, 2).
crosses(80, 5, 11, 1).
crosses(80, 6, 12, 1).
crosses(80, 0, 7, 1).
crosses(80, 1, 8, 1).
crosses(80, 2, 9, 3).
crosses(80, 3, 10, 5).
crosses(80, 5, 12, 1).
crosses(80, 1, 9, 2).
crosses(80, 2, 10, 4).
crosses(80, 3, 11, 3).
crosses(80, 0, 9, 1).
crosses(80, 1, 10, 3).
crosses(80, 2, 11, 2).
crosses(80, 3, 12, 3).
crosses(80, 0, 10, 2).
crosses(80, 1, 11, 1).
crosses(80, 2, 12, 2).
crosses(80, 1, 12, 1).
crosses(81, 1, 3, 1).
crosses(81, 3, 5, 1).
crosses(81, 4, 6, 1).
crosses(81, 7, 9, 2).
crosses(81, 8, 10, 1).
crosses(81, 10, 12, 2).
crosses(81, 0, 3, 1).
crosses(81, 3, 6, 2).
crosses(81, 4, 7, 1).
crosses(81, 6, 9, 2).
crosses(81, 7, 10, 3).
crosses(81, 9, 12, 1).
crosses(81, 2, 6, 1).
crosses(81, 3, 7, 2).
crosses(81, 5, 9, 1).
crosses(81, 6, 10, 3).
crosses(81, 7, 11, 1).
crosses(81, 1, 6, 1).
crosses(81, 2, 7, 1).
crosses(81, 3, 8, 1).
crosses(81, 4, 9, 1).
crosses(81, 5, 10, 2).
crosses(81, 6, 11, 1).
crosses(81, 7, 12, 1).
crosses(81, 0, 6, 1).
crosses(81, 1, 7, 1).
crosses(81, 3, 9, 2).
crosses(81, 4, 10, 2).
crosses(81, 6, 12, 1).
crosses(81, 0, 7, 1).
crosses(81, 2, 9, 1).
crosses(81, 3, 10, 3).
crosses(81, 1, 9, 1).
crosses(81, 2, 10, 2).
crosses(81, 3, 11, 1).
crosses(81, 0, 9, 1).
crosses(81, 1, 10, 2).
crosses(81, 3, 12, 1).
crosses(81, 0, 10, 2).
crosses(82, 1, 3, 1).
crosses(82, 3, 5, 1).
crosses(82, 6, 8, 1).
crosses(82, 7, 9, 1).
crosses(82, 9, 11, 2).
crosses(82, 0, 3, 1).
crosses(82, 3, 6, 1).
crosses(82, 5, 8, 1).
crosses(82, 6, 9, 2).
crosses(82, 8, 11, 1).
crosses(82, 3, 7, 1).
crosses(82, 4, 8, 1).
crosses(82, 5, 9, 2).
crosses(82, 3, 8, 2).
crosses(82, 4, 9, 2).
crosses(82, 2, 8, 1).
crosses(82, 3, 9, 3).
crosses(82, 1, 8, 1).
crosses(82, 2, 9, 2).
crosses(82, 3, 10, 1).
crosses(82, 0, 8, 1).
crosses(82, 1, 9, 2).
crosses(82, 3, 11, 1).
crosses(82, 0, 9, 2).
crosses(83, 3, 5, 1).
crosses(83, 4, 6, 1).
crosses(83, 7, 9, 2).
crosses(83, 8, 10, 1).
crosses(83, 10, 12, 2).
crosses(83, 2, 5, 1).
crosses(83, 3, 6, 2).
crosses(83, 4, 7, 1).
crosses(83, 6, 9, 2).
crosses(83, 7, 10, 3).
crosses(83, 9, 12, 1).
crosses(83, 1, 5, 1).
crosses(83, 2, 6, 2).
crosses(83, 3, 7, 2).
crosses(83, 5, 9, 1).
crosses(83, 6, 10, 3).
crosses(83, 7, 11, 1).
crosses(83, 1, 6, 2).
crosses(83, 2, 7, 2).
crosses(83, 3, 8, 1).
crosses(83, 4, 9, 1).
crosses(83, 5, 10, 2).
crosses(83, 6, 11, 1).
crosses(83, 7, 12, 1).
crosses(83, 0, 6, 1).
crosses(83, 1, 7, 2).
crosses(83, 2, 8, 1).
crosses(83, 3, 9, 2).
crosses(83, 4, 10, 2).
crosses(83, 6, 12, 1).
crosses(83, 0, 7, 1).
crosses(83, 1, 8, 1).
crosses(83, 2, 9, 2).
crosses(83, 3, 10, 3).
crosses(83, 1, 9, 2).
crosses(83, 2, 10, 3).
crosses(83, 3, 11, 1).
crosses(83, 0, 9, 1).
crosses(83, 1, 10, 3).
crosses(83, 2, 11, 1).
crosses(83, 3, 12, 1).
crosses(83, 0, 10, 2).
crosses(83, 1, 11, 1).
crosses(83, 2, 12, 1).
crosses(83, 1, 12, 1).
crosses(84, 3, 5, 1).
crosses(84, 6, 8, 1).
crosses(84, 7, 9, 1).
crosses(84, 9, 11, 2).
crosses(84, 2, 5, 1).
crosses(84, 3, 6, 1).
crosses(84, 5, 8, 1).
crosses(84, 6, 9, 2).
crosses(84, 8, 11, 1).
crosses(84, 1, 5, 1).
crosses(84, 2, 6, 1).
crosses(84, 3, 7, 1).
crosses(84, 4, 8, 1).
crosses(84, 5, 9, 2).
crosses(84, 1, 6, 1).
crosses(84, 2, 7, 1).
crosses(84, 3, 8, 2).
crosses(84, 4, 9, 2).
crosses(84, 1, 7, 1).
crosses(84, 2, 8, 2).
crosses(84, 3, 9, 3).
crosses(84, 1, 8, 2).
crosses(84, 2, 9, 3).
crosses(84, 3, 10, 1).
crosses(84, 0, 8, 1).
crosses(84, 1, 9, 3).
crosses(84, 2, 10, 1).
crosses(84, 3, 11, 1).
crosses(84, 0, 9, 2).
crosses(84, 1, 10, 1).
crosses(84, 2, 11, 1).
crosses(84, 1, 11, 1).
crosses(85, 1, 3, 2).
crosses(85, 3, 5, 2).
crosses(85, 6, 8, 1).
crosses(85, 0, 3, 1).
crosses(85, 1, 4, 1).
crosses(85, 2, 5, 1).
crosses(85, 3, 6, 2).
crosses(85, 5, 8, 1).
crosses(85, 1, 5, 2).
crosses(85, 2, 6, 1).
crosses(85, 3, 7, 1).
crosses(85, 0, 5, 1).
crosses(85, 1, 6, 2).
crosses(85, 3, 8, 1).
crosses(85, 0, 6, 1).
crosses(85, 1, 7, 1).
crosses(85, 1, 8, 1).
crosses(86, 1, 3, 2).
crosses(86, 3, 5, 2).
crosses(86, 6, 8, 1).
crosses(86, 8, 10, 2).
crosses(86, 0, 3, 1).
crosses(86, 1, 4, 1).
crosses(86, 2, 5, 1).
crosses(86, 3, 6, 2).
crosses(86, 5, 8, 1).
crosses(86, 7, 10, 1).
crosses(86, 1, 5, 2).
crosses(86, 2, 6, 1).
crosses(86, 3, 7, 2).
crosses(86, 4, 8, 1).
crosses(86, 6, 10, 1).
crosses(86, 0, 5, 1).
crosses(86, 1, 6, 2).
crosses(86, 2, 7, 1).
crosses(86, 3, 8, 3).
crosses(86, 5, 10, 1).
crosses(86, 0, 6, 1).
crosses(86, 1, 7, 2).
crosses(86, 2, 8, 2).
crosses(86, 3, 9, 1).
crosses(86, 0, 7, 1).
crosses(86, 1, 8, 3).
crosses(86, 3, 10, 1).
crosses(86, 0, 8, 2).
crosses(86, 1, 9, 1).
crosses(86, 1, 10, 1).
crosses(87, 1, 3, 1).
crosses(87, 2, 4, 1).
crosses(87, 4, 6, 1).
crosses(87, 6, 8, 2).
crosses(87, 1, 4, 2).
crosses(87, 2, 5, 1).
crosses(87, 3, 6, 1).
crosses(87, 5, 8, 1).
crosses(87, 0, 4, 1).
crosses(87, 1, 5, 2).
crosses(87, 2, 6, 2).
crosses(87, 4, 8, 1).
crosses(87, 0, 5, 1).
crosses(87, 1, 6, 3).
crosses(87, 0, 6, 2).
crosses(87, 1, 7, 1).
crosses(87, 1, 8, 1).
crosses(88, 1, 3, 1).
crosses(88, 2, 4, 1).
crosses(88, 4, 6, 1).
crosses(88, 1, 4, 2).
crosses(88, 0, 4, 1).
crosses(88, 1, 5, 1).
crosses(88, 1, 6, 1).
crosses(89, 1, 3, 1).
crosses(89, 3, 5, 1).
crosses(89, 5, 7, 1).
crosses(89, 7, 9, 1).
crosses(89, 9, 11, 5).
crosses(89, 10, 12, 1).
crosses(89, 12, 14, 1).
crosses(89, 14, 16, 2).
crosses(89, 0, 3, 1).
crosses(89, 1, 4, 1).
crosses(89, 2, 5, 1).
crosses(89, 3, 6, 1).
crosses(89, 4, 7, 1).
crosses(89, 5, 8, 1).
crosses(89, 6, 9, 1).
crosses(89, 8, 11, 4).
crosses(89, 9, 12, 6).
crosses(89, 10, 13, 1).
crosses(89, 11, 14, 1).
crosses(89, 13, 16, 1).
crosses(89, 0, 4, 1).
crosses(89, 1, 5, 2).
crosses(89, 2, 6, 1).
crosses(89, 3, 7, 2).
crosses(89, 4, 8, 1).
crosses(89, 5, 9, 2).
crosses(89, 7, 11, 4).
crosses(89, 8, 12, 5).
crosses(89, 9, 13, 6).
crosses(89, 10, 14, 2).
crosses(89, 12, 16, 1).
crosses(89, 0, 5, 2).
crosses(89, 1, 6, 2).
crosses(89, 2, 7, 2).
crosses(89, 3, 8, 2).
crosses(89, 4, 9, 2).
crosses(89, 6, 11, 3).
crosses(89, 7, 12, 5).
crosses(89, 8, 13, 5).
crosses(89, 9, 14, 7).
crosses(89, 0, 6, 2).
crosses(89, 1, 7, 3).
crosses(89, 2, 8, 2).
crosses(89, 3, 9, 3).
crosses(89, 5, 11, 3).
crosses(89, 6, 12, 4).
crosses(89, 7, 13, 5).
crosses(89, 8, 14, 6).
crosses(89, 9, 15, 5).
crosses(89, 0, 7, 3).
crosses(89, 1, 8, 3).
crosses(89, 2, 9, 3).
crosses(89, 4, 11, 2).
crosses(89, 5, 12, 4).
crosses(89, 6, 13, 4).
crosses(89, 7, 14, 6).
crosses(89, 8, 15, 4).
crosses(89, 9, 16, 5).
crosses(89, 0, 8, 3).
crosses(89, 1, 9, 4).
crosses(89, 3, 11, 2).
crosses(89, 4, 12, 3).
crosses(89, 5, 13, 4).
crosses(89, 6, 14, 5).
crosses(89, 7, 15, 4).
crosses(89, 8, 16, 4).
crosses(89, 0, 9, 4).
crosses(89, 2, 11, 1).
crosses(89, 3, 12, 3).
crosses(89, 4, 13, 3).
crosses(89, 5, 14, 5).
crosses(89, 6, 15, 3).
crosses(89, 7, 16, 4).
crosses(89, 1, 11, 1).
crosses(89, 2, 12, 2).
crosses(89, 3, 13, 3).
crosses(89, 4, 14, 4).
crosses(89, 5, 15, 3).
crosses(89, 6, 16, 3).
crosses(89, 1, 12, 2).
crosses(89, 2, 13, 2).
crosses(89, 3, 14, 4).
crosses(89, 4, 15, 2).
crosses(89, 5, 16, 3).
crosses(89, 0, 12, 1).
crosses(89, 1, 13, 2).
crosses(89, 2, 14, 3).
crosses(89, 3, 15, 2).
crosses(89, 4, 16, 2).
crosses(89, 0, 13, 1).
crosses(89, 1, 14, 3).
crosses(89, 2, 15, 1).
crosses(89, 3, 16, 2).
crosses(89, 0, 14, 2).
crosses(89, 1, 15, 1).
crosses(89, 2, 16, 1).
crosses(89, 1, 16, 1).
crosses(90, 1, 3, 1).
crosses(90, 2, 4, 1).
crosses(90, 4, 6, 1).
crosses(90, 6, 8, 1).
crosses(90, 8, 10, 3).
crosses(90, 1, 4, 2).
crosses(90, 2, 5, 1).
crosses(90, 3, 6, 1).
crosses(90, 4, 7, 1).
crosses(90, 5, 8, 1).
crosses(90, 7, 10, 2).
crosses(90, 0, 4, 1).
crosses(90, 1, 5, 2).
crosses(90, 2, 6, 2).
crosses(90, 3, 7, 1).
crosses(90, 4, 8, 2).
crosses(90, 6, 10, 2).
crosses(90, 0, 5, 1).
crosses(90, 1, 6, 3).
crosses(90, 2, 7, 2).
crosses(90, 3, 8, 2).
crosses(90, 5, 10, 1).
crosses(90, 0, 6, 2).
crosses(90, 1, 7, 3).
crosses(90, 2, 8, 3).
crosses(90, 4, 10, 1).
crosses(90, 0, 7, 2).
crosses(90, 1, 8, 4).
crosses(90, 0, 8, 3).
crosses(90, 1, 9, 1).
crosses(90, 1, 10, 1).
crosses(91, 2, 4, 1).
crosses(91, 3, 5, 1).
crosses(91, 5, 7, 1).
crosses(91, 7, 9, 2).
crosses(91, 1, 4, 1).
crosses(91, 2, 5, 2).
crosses(91, 3, 6, 1).
crosses(91, 4, 7, 1).
crosses(91, 6, 9, 1).
crosses(91, 1, 5, 2).
crosses(91, 2, 6, 2).
crosses(91, 3, 7, 2).
crosses(91, 5, 9, 1).
crosses(91, 0, 5, 1).
crosses(91, 1, 6, 2).
crosses(91, 2, 7, 3).
crosses(91, 0, 6, 1).
crosses(91, 1, 7, 3).
crosses(91, 2, 8, 1).
crosses(91, 0, 7, 2).
crosses(91, 1, 8, 1).
crosses(91, 2, 9, 1).
crosses(91, 1, 9, 1).
crosses(92, 1, 3, 1).
crosses(92, 2, 4, 1).
crosses(92, 4, 6, 1).
crosses(92, 1, 4, 2).
crosses(92, 0, 4, 1).
crosses(92, 1, 5, 1).
crosses(92, 1, 6, 1).
crosses(93, 1, 3, 1).
crosses(93, 3, 5, 1).
crosses(93, 5, 7, 1).
crosses(93, 7, 9, 1).
crosses(93, 9, 11, 5).
crosses(93, 10, 12, 1).
crosses(93, 12, 14, 1).
crosses(93, 14, 16, 2).
crosses(93, 0, 3, 1).
crosses(93, 1, 4, 1).
crosses(93, 2, 5, 1).
crosses(93, 3, 6, 1).
crosses(93, 4, 7, 1).
crosses(93, 5, 8, 1).
crosses(93, 6, 9, 1).
crosses(93, 8, 11, 4).
crosses(93, 9, 12, 6).
crosses(93, 10, 13, 1).
crosses(93, 11, 14, 1).
crosses(93, 13, 16, 1).
crosses(93, 0, 4, 1).
crosses(93, 1, 5, 2).
crosses(93, 2, 6, 1).
crosses(93, 3, 7, 2).
crosses(93, 4, 8, 1).
crosses(93, 5, 9, 2).
crosses(93, 7, 11, 4).
crosses(93, 8, 12, 5).
crosses(93, 9, 13, 6).
crosses(93, 10, 14, 2).
crosses(93, 12, 16, 1).
crosses(93, 0, 5, 2).
crosses(93, 1, 6, 2).
crosses(93, 2, 7, 2).
crosses(93, 3, 8, 2).
crosses(93, 4, 9, 2).
crosses(93, 6, 11, 3).
crosses(93, 7, 12, 5).
crosses(93, 8, 13, 5).
crosses(93, 9, 14, 7).
crosses(93, 0, 6, 2).
crosses(93, 1, 7, 3).
crosses(93, 2, 8, 2).
crosses(93, 3, 9, 3).
crosses(93, 5, 11, 3).
crosses(93, 6, 12, 4).
crosses(93, 7, 13, 5).
crosses(93, 8, 14, 6).
crosses(93, 9, 15, 5).
crosses(93, 0, 7, 3).
crosses(93, 1, 8, 3).
crosses(93, 2, 9, 3).
crosses(93, 4, 11, 2).
crosses(93, 5, 12, 4).
crosses(93, 6, 13, 4).
crosses(93, 7, 14, 6).
crosses(93, 8, 15, 4).
crosses(93, 9, 16, 5).
crosses(93, 0, 8, 3).
crosses(93, 1, 9, 4).
crosses(93, 3, 11, 2).
crosses(93, 4, 12, 3).
crosses(93, 5, 13, 4).
crosses(93, 6, 14, 5).
crosses(93, 7, 15, 4).
crosses(93, 8, 16, 4).
crosses(93, 0, 9, 4).
crosses(93, 2, 11, 1).
crosses(93, 3, 12, 3).
crosses(93, 4, 13, 3).
crosses(93, 5, 14, 5).
crosses(93, 6, 15, 3).
crosses(93, 7, 16, 4).
crosses(93, 1, 11, 1).
crosses(93, 2, 12, 2).
crosses(93, 3, 13, 3).
crosses(93, 4, 14, 4).
crosses(93, 5, 15, 3).
crosses(93, 6, 16, 3).
crosses(93, 1, 12, 2).
crosses(93, 2, 13, 2).
crosses(93, 3, 14, 4).
crosses(93, 4, 15, 2).
crosses(93, 5, 16, 3).
crosses(93, 0, 12, 1).
crosses(93, 1, 13, 2).
crosses(93, 2, 14, 3).
crosses(93, 3, 15, 2).
crosses(93, 4, 16, 2).
crosses(93, 0, 13, 1).
crosses(93, 1, 14, 3).
crosses(93, 2, 15, 1).
crosses(93, 3, 16, 2).
crosses(93, 0, 14, 2).
crosses(93, 1, 15, 1).
crosses(93, 2, 16, 1).
crosses(93, 1, 16, 1).
crosses(94, 2, 4, 1).
crosses(94, 3, 5, 1).
crosses(94, 5, 7, 1).
crosses(94, 7, 9, 1).
crosses(94, 9, 11, 3).
crosses(94, 1, 4, 1).
crosses(94, 2, 5, 2).
crosses(94, 3, 6, 1).
crosses(94, 4, 7, 1).
crosses(94, 5, 8, 1).
crosses(94, 6, 9, 1).
crosses(94, 8, 11, 2).
crosses(94, 1, 5, 2).
crosses(94, 2, 6, 2).
crosses(94, 3, 7, 2).
crosses(94, 4, 8, 1).
crosses(94, 5, 9, 2).
crosses(94, 7, 11, 2).
crosses(94, 0, 5, 1).
crosses(94, 1, 6, 2).
crosses(94, 2, 7, 3).
crosses(94, 3, 8, 2).
crosses(94, 4, 9, 2).
crosses(94, 6, 11, 1).
crosses(94, 0, 6, 1).
crosses(94, 1, 7, 3).
crosses(94, 2, 8, 3).
crosses(94, 3, 9, 3).
crosses(94, 5, 11, 1).
crosses(94, 0, 7, 2).
crosses(94, 1, 8, 3).
crosses(94, 2, 9, 4).
crosses(94, 0, 8, 2).
crosses(94, 1, 9, 4).
crosses(94, 2, 10, 1).
crosses(94, 0, 9, 3).
crosses(94, 1, 10, 1).
crosses(94, 2, 11, 1).
crosses(94, 1, 11, 1).
crosses(95, 1, 3, 1).
crosses(95, 2, 4, 1).
crosses(95, 4, 6, 1).
crosses(95, 6, 8, 2).
crosses(95, 1, 4, 2).
crosses(95, 2, 5, 1).
crosses(95, 3, 6, 1).
crosses(95, 5, 8, 1).
crosses(95, 0, 4, 1).
crosses(95, 1, 5, 2).
crosses(95, 2, 6, 2).
crosses(95, 4, 8, 1).
crosses(95, 0, 5, 1).
crosses(95, 1, 6, 3).
crosses(95, 0, 6, 2).
crosses(95, 1, 7, 1).
crosses(95, 1, 8, 1).
crosses(96, 1, 3, 1).
crosses(96, 2, 4, 1).
crosses(96, 4, 6, 1).
crosses(96, 1, 4, 2).
crosses(96, 0, 4, 1).
crosses(96, 1, 5, 1).
crosses(96, 1, 6, 1).
crosses(97, 1, 3, 1).
crosses(97, 3, 5, 1).
crosses(97, 5, 7, 1).
crosses(97, 7, 9, 1).
crosses(97, 9, 11, 5).
crosses(97, 10, 12, 1).
crosses(97, 12, 14, 1).
crosses(97, 14, 16, 2).
crosses(97, 0, 3, 1).
crosses(97, 1, 4, 1).
crosses(97, 2, 5, 1).
crosses(97, 3, 6, 1).
crosses(97, 4, 7, 1).
crosses(97, 5, 8, 1).
crosses(97, 6, 9, 1).
crosses(97, 8, 11, 4).
crosses(97, 9, 12, 6).
crosses(97, 10, 13, 1).
crosses(97, 11, 14, 1).
crosses(97, 13, 16, 1).
crosses(97, 0, 4, 1).
crosses(97, 1, 5, 2).
crosses(97, 2, 6, 1).
crosses(97, 3, 7, 2).
crosses(97, 4, 8, 1).
crosses(97, 5, 9, 2).
crosses(97, 7, 11, 4).
crosses(97, 8, 12, 5).
crosses(97, 9, 13, 6).
crosses(97, 10, 14, 2).
crosses(97, 12, 16, 1).
crosses(97, 0, 5, 2).
crosses(97, 1, 6, 2).
crosses(97, 2, 7, 2).
crosses(97, 3, 8, 2).
crosses(97, 4, 9, 2).
crosses(97, 6, 11, 3).
crosses(97, 7, 12, 5).
crosses(97, 8, 13, 5).
crosses(97, 9, 14, 7).
crosses(97, 0, 6, 2).
crosses(97, 1, 7, 3).
crosses(97, 2, 8, 2).
crosses(97, 3, 9, 3).
crosses(97, 5, 11, 3).
crosses(97, 6, 12, 4).
crosses(97, 7, 13, 5).
crosses(97, 8, 14, 6).
crosses(97, 9, 15, 5).
crosses(97, 0, 7, 3).
crosses(97, 1, 8, 3).
crosses(97, 2, 9, 3).
crosses(97, 4, 11, 2).
crosses(97, 5, 12, 4).
crosses(97, 6, 13, 4).
crosses(97, 7, 14, 6).
crosses(97, 8, 15, 4).
crosses(97, 9, 16, 5).
crosses(97, 0, 8, 3).
crosses(97, 1, 9, 4).
crosses(97, 3, 11, 2).
crosses(97, 4, 12, 3).
crosses(97, 5, 13, 4).
crosses(97, 6, 14, 5).
crosses(97, 7, 15, 4).
crosses(97, 8, 16, 4).
crosses(97, 0, 9, 4).
crosses(97, 2, 11, 1).
crosses(97, 3, 12, 3).
crosses(97, 4, 13, 3).
crosses(97, 5, 14, 5).
crosses(97, 6, 15, 3).
crosses(97, 7, 16, 4).
crosses(97, 1, 11, 1).
crosses(97, 2, 12, 2).
crosses(97, 3, 13, 3).
crosses(97, 4, 14, 4).
crosses(97, 5, 15, 3).
crosses(97, 6, 16, 3).
crosses(97, 1, 12, 2).
crosses(97, 2, 13, 2).
crosses(97, 3, 14, 4).
crosses(97, 4, 15, 2).
crosses(97, 5, 16, 3).
crosses(97, 0, 12, 1).
crosses(97, 1, 13, 2).
crosses(97, 2, 14, 3).
crosses(97, 3, 15, 2).
crosses(97, 4, 16, 2).
crosses(97, 0, 13, 1).
crosses(97, 1, 14, 3).
crosses(97, 2, 15, 1).
crosses(97, 3, 16, 2).
crosses(97, 0, 14, 2).
crosses(97, 1, 15, 1).
crosses(97, 2, 16, 1).
crosses(97, 1, 16, 1).
crosses(98, 1, 3, 1).
crosses(98, 2, 4, 1).
crosses(98, 4, 6, 1).
crosses(98, 6, 8, 1).
crosses(98, 8, 10, 3).
crosses(98, 1, 4, 2).
crosses(98, 2, 5, 1).
crosses(98, 3, 6, 1).
crosses(98, 4, 7, 1).
crosses(98, 5, 8, 1).
crosses(98, 7, 10, 2).
crosses(98, 0, 4, 1).
crosses(98, 1, 5, 2).
crosses(98, 2, 6, 2).
crosses(98, 3, 7, 1).
crosses(98, 4, 8, 2).
crosses(98, 6, 10, 2).
crosses(98, 0, 5, 1).
crosses(98, 1, 6, 3).
crosses(98, 2, 7, 2).
crosses(98, 3, 8, 2).
crosses(98, 5, 10, 1).
crosses(98, 0, 6, 2).
crosses(98, 1, 7, 3).
crosses(98, 2, 8, 3).
crosses(98, 4, 10, 1).
crosses(98, 0, 7, 2).
crosses(98, 1, 8, 4).
crosses(98, 0, 8, 3).
crosses(98, 1, 9, 1).
crosses(98, 1, 10, 1).
crosses(99, 1, 3, 1).
crosses(99, 2, 4, 1).
crosses(99, 4, 6, 3).
crosses(99, 5, 7, 1).
crosses(99, 7, 9, 1).
crosses(99, 9, 11, 2).
crosses(99, 0, 3, 1).
crosses(99, 1, 4, 2).
crosses(99, 3, 6, 2).
crosses(99, 4, 7, 4).
crosses(99, 5, 8, 1).
crosses(99, 6, 9, 1).
crosses(99, 8, 11, 1).
crosses(99, 0, 4, 2).
crosses(99, 2, 6, 1).
crosses(99, 3, 7, 3).
crosses(99, 4, 8, 4).
crosses(99, 5, 9, 2).
crosses(99, 7, 11, 1).
crosses(99, 1, 6, 1).
crosses(99, 2, 7, 2).
crosses(99, 3, 8, 3).
crosses(99, 4, 9, 5).
crosses(99, 1, 7, 2).
crosses(99, 2, 8, 2).
crosses(99, 3, 9, 4).
crosses(99, 4, 10, 3).
crosses(99, 0, 7, 1).
crosses(99, 1, 8, 2).
crosses(99, 2, 9, 3).
crosses(99, 3, 10, 2).
crosses(99, 4, 11, 3).
crosses(99, 0, 8, 1).
crosses(99, 1, 9, 3).
crosses(99, 2, 10, 1).
crosses(99, 3, 11, 2).
crosses(99, 0, 9, 2).
crosses(99, 1, 10, 1).
crosses(99, 2, 11, 1).
crosses(99, 1, 11, 1).
crosses(100, 2, 4, 1).
crosses(100, 3, 5, 1).
crosses(100, 5, 7, 3).
crosses(100, 6, 8, 1).
crosses(100, 8, 10, 1).
crosses(100, 1, 4, 1).
crosses(100, 2, 5, 2).
crosses(100, 4, 7, 2).
crosses(100, 5, 8, 4).
crosses(100, 0, 4, 1).
crosses(100, 1, 5, 2).
crosses(100, 3, 7, 1).
crosses(100, 4, 8, 3).
crosses(100, 5, 9, 3).
crosses(100, 0, 5, 2).
crosses(100, 2, 7, 1).
crosses(100, 3, 8, 2).
crosses(100, 4, 9, 2).
crosses(100, 5, 10, 3).
crosses(100, 1, 7, 1).
crosses(100, 2, 8, 2).
crosses(100, 3, 9, 1).
crosses(100, 4, 10, 2).
crosses(100, 1, 8, 2).
crosses(100, 2, 9, 1).
crosses(100, 3, 10, 1).
crosses(100, 0, 8, 1).
crosses(100, 1, 9, 1).
crosses(100, 2, 10, 1).
crosses(100, 1, 10, 1).
crosses(101, 1, 3, 1).
crosses(101, 3, 5, 1).
crosses(101, 5, 7, 1).
crosses(101, 7, 9, 5).
crosses(101, 9, 11, 1).
crosses(101, 10, 12, 1).
crosses(101, 12, 14, 1).
crosses(101, 13, 15, 1).
crosses(101, 15, 17, 1).
crosses(101, 17, 19, 2).
crosses(101, 0, 3, 1).
crosses(101, 1, 4, 1).
crosses(101, 2, 5, 1).
crosses(101, 3, 6, 1).
crosses(101, 4, 7, 1).
crosses(101, 6, 9, 4).
crosses(101, 7, 10, 4).
crosses(101, 9, 12, 2).
crosses(101, 12, 15, 2).
crosses(101, 13, 16, 1).
crosses(101, 14, 17, 1).
crosses(101, 16, 19, 1).
crosses(101, 0, 4, 1).
crosses(101, 1, 5, 2).
crosses(101, 2, 6, 1).
crosses(101, 3, 7, 2).
crosses(101, 5, 9, 4).
crosses(101, 6, 10, 3).
crosses(101, 7, 11, 4).
crosses(101, 8, 12, 1).
crosses(101, 9, 13, 1).
crosses(101, 11, 15, 1).
crosses(101, 12, 16, 2).
crosses(101, 13, 17, 2).
crosses(101, 15, 19, 1).
crosses(101, 0, 5, 2).
crosses(101, 1, 6, 2).
crosses(101, 2, 7, 2).
crosses(101, 4, 9, 3).
crosses(101, 5, 10, 3).
crosses(101, 6, 11, 3).
crosses(101, 7, 12, 5).
crosses(101, 9, 14, 1).
crosses(101, 10, 15, 1).
crosses(101, 11, 16, 1).
crosses(101, 12, 17, 3).
crosses(101, 0, 6, 2).
crosses(101, 1, 7, 3).
crosses(101, 3, 9, 3).
crosses(101, 4, 10, 2).
crosses(101, 5, 11, 3).
crosses(101, 6, 12, 4).
crosses(101, 7, 13, 4).
crosses(101, 9, 15, 2).
crosses(101, 10, 16, 1).
crosses(101, 11, 17, 2).
crosses(101, 12, 18, 1).
crosses(101, 0, 7, 3).
crosses(101, 2, 9, 2).
crosses(101, 3, 10, 2).
crosses(101, 4, 11, 2).
crosses(101, 5, 12, 4).
crosses(101, 6, 13, 3).
crosses(101, 7, 14, 4).
crosses(101, 8, 15, 1).
crosses(101, 9, 16, 2).
crosses(101, 10, 17, 2).
crosses(101, 12, 19, 1).
crosses(101, 1, 9, 2).
crosses(101, 2, 10, 1).
crosses(101, 3, 11, 2).
crosses(101, 4, 12, 3).
crosses(101, 5, 13, 3).
crosses(101, 6, 14, 3).
crosses(101, 7, 15, 5).
crosses(101, 8, 16, 1).
crosses(101, 9, 17, 3).
crosses(101, 0, 9, 1).
crosses(101, 1, 10, 1).
crosses(101, 2, 11, 1).
crosses(101, 3, 12, 3).
crosses(101, 4, 13, 2).
crosses(101, 5, 14, 3).
crosses(101, 6, 15, 4).
crosses(101, 7, 16, 5).
crosses(101, 8, 17, 2).
crosses(101, 9, 18, 1).
crosses(101, 1, 11, 1).
crosses(101, 2, 12, 2).
crosses(101, 3, 13, 2).
crosses(101, 4, 14, 2).
crosses(101, 5, 15, 4).
crosses(101, 6, 16, 4).
crosses(101, 7, 17, 6).
crosses(101, 9, 19, 1).
crosses(101, 1, 12, 2).
crosses(101, 2, 13, 1).
crosses(101, 3, 14, 2).
crosses(101, 4, 15, 3).
crosses(101, 5, 16, 4).
crosses(101, 6, 17, 5).
crosses(101, 7, 18, 4).
crosses(101, 0, 12, 1).
crosses(101, 1, 13, 1).
crosses(101, 2, 14, 1).
crosses(101, 3, 15, 3).
crosses(101, 4, 16, 3).
crosses(101, 5, 17, 5).
crosses(101, 6, 18, 3).
crosses(101, 7, 19, 4).
crosses(101, 1, 14, 1).
crosses(101, 2, 15, 2).
crosses(101, 3, 16, 3).
crosses(101, 4, 17, 4).
crosses(101, 5, 18, 3).
crosses(101, 6, 19, 3).
crosses(101, 1, 15, 2).
crosses(101, 2, 16, 2).
crosses(101, 3, 17, 4).
crosses(101, 4, 18, 2).
crosses(101, 5, 19, 3).
crosses(101, 0, 15, 1).
crosses(101, 1, 16, 2).
crosses(101, 2, 17, 3).
crosses(101, 3, 18, 2).
crosses(101, 4, 19, 2).
crosses(101, 0, 16, 1).
crosses(101, 1, 17, 3).
crosses(101, 2, 18, 1).
crosses(101, 3, 19, 2).
crosses(101, 0, 17, 2).
crosses(101, 1, 18, 1).
crosses(101, 2, 19, 1).
crosses(101, 1, 19, 1).
crosses(102, 1, 3, 1).
crosses(102, 2, 4, 1).
crosses(102, 4, 6, 3).
crosses(102, 5, 7, 1).
crosses(102, 7, 9, 1).
crosses(102, 12, 14, 2).
crosses(102, 0, 3, 1).
crosses(102, 1, 4, 2).
crosses(102, 3, 6, 2).
crosses(102, 4, 7, 4).
crosses(102, 5, 8, 1).
crosses(102, 6, 9, 1).
crosses(102, 7, 10, 1).
crosses(102, 11, 14, 2).
crosses(102, 0, 4, 2).
crosses(102, 2, 6, 1).
crosses(102, 3, 7, 3).
crosses(102, 4, 8, 4).
crosses(102, 5, 9, 2).
crosses(102, 6, 10, 1).
crosses(102, 7, 11, 1).
crosses(102, 10, 14, 2).
crosses(102, 1, 6, 1).
crosses(102, 2, 7, 2).
crosses(102, 3, 8, 3).
crosses(102, 4, 9, 5).
crosses(102, 5, 10, 2).
crosses(102, 6, 11, 1).
crosses(102, 7, 12, 1).
crosses(102, 9, 14, 2).
crosses(102, 1, 7, 2).
crosses(102, 2, 8, 2).
crosses(102, 3, 9, 4).
crosses(102, 4, 10, 5).
crosses(102, 5, 11, 2).
crosses(102, 6, 12, 1).
crosses(102, 8, 14, 1).
crosses(102, 0, 7, 1).
crosses(102, 1, 8, 2).
crosses(102, 2, 9, 3).
crosses(102, 3, 10, 4).
crosses(102, 4, 11, 5).
crosses(102, 5, 12, 2).
crosses(102, 7, 14, 1).
crosses(102, 0, 8, 1).
crosses(102, 1, 9, 3).
crosses(102, 2, 10, 3).
crosses(102, 3, 11, 4).
crosses(102, 4, 12, 5).
crosses(102, 0, 9, 2).
crosses(102, 1, 10, 3).
crosses(102, 2, 11, 3).
crosses(102, 3, 12, 4).
crosses(102, 4, 13, 3).
crosses(102, 0, 10, 2).
crosses(102, 1, 11, 3).
crosses(102, 2, 12, 3).
crosses(102, 3, 13, 2).
crosses(102, 4, 14, 3).
crosses(102, 0, 11, 2).
crosses(102, 1, 12, 3).
crosses(102, 2, 13, 1).
crosses(102, 3, 14, 2).
crosses(102, 0, 12, 2).
crosses(102, 1, 13, 1).
crosses(102, 2, 14, 1).
crosses(102, 1, 14, 1).
crosses(103, 1, 3, 2).
crosses(103, 4, 6, 2).
crosses(103, 6, 8, 1).
crosses(103, 0, 3, 1).
crosses(103, 1, 4, 2).
crosses(103, 3, 6, 2).
crosses(103, 4, 7, 1).
crosses(103, 0, 4, 1).
crosses(103, 1, 5, 1).
crosses(103, 2, 6, 1).
crosses(103, 3, 7, 1).
crosses(103, 4, 8, 1).
crosses(103, 1, 6, 2).
crosses(103, 3, 8, 1).
crosses(103, 0, 6, 1).
crosses(103, 1, 7, 1).
crosses(103, 1, 8, 1).
crosses(104, 1, 3, 2).
crosses(104, 3, 5, 2).
crosses(104, 5, 7, 1).
crosses(104, 7, 9, 2).
crosses(104, 0, 3, 1).
crosses(104, 1, 4, 1).
crosses(104, 2, 5, 1).
crosses(104, 3, 6, 2).
crosses(104, 4, 7, 1).
crosses(104, 6, 9, 1).
crosses(104, 1, 5, 2).
crosses(104, 2, 6, 1).
crosses(104, 3, 7, 3).
crosses(104, 5, 9, 1).
crosses(104, 0, 5, 1).
crosses(104, 1, 6, 2).
crosses(104, 2, 7, 2).
crosses(104, 3, 8, 1).
crosses(104, 0, 6, 1).
crosses(104, 1, 7, 3).
crosses(104, 3, 9, 1).
crosses(104, 0, 7, 2).
crosses(104, 1, 8, 1).
crosses(104, 1, 9, 1).
crosses(105, 1, 3, 2).
crosses(105, 3, 5, 2).
crosses(105, 5, 7, 1).
crosses(105, 0, 3, 1).
crosses(105, 1, 4, 1).
crosses(105, 2, 5, 1).
crosses(105, 3, 6, 1).
crosses(105, 1, 5, 2).
crosses(105, 3, 7, 1).
crosses(105, 0, 5, 1).
crosses(105, 1, 6, 1).
crosses(105, 1, 7, 1).
crosses(106, 1, 3, 2).
crosses(106, 3, 5, 2).
crosses(106, 5, 7, 1).
crosses(106, 7, 9, 2).
crosses(106, 0, 3, 1).
crosses(106, 1, 4, 1).
crosses(106, 2, 5, 1).
crosses(106, 3, 6, 2).
crosses(106, 4, 7, 1).
crosses(106, 6, 9, 1).
crosses(106, 1, 5, 2).
crosses(106, 2, 6, 1).
crosses(106, 3, 7, 3).
crosses(106, 5, 9, 1).
crosses(106, 0, 5, 1).
crosses(106, 1, 6, 2).
crosses(106, 2, 7, 2).
crosses(106, 3, 8, 1).
crosses(106, 0, 6, 1).
crosses(106, 1, 7, 3).
crosses(106, 3, 9, 1).
crosses(106, 0, 7, 2).
crosses(106, 1, 8, 1).
crosses(106, 1, 9, 1).
crosses(107, 2, 4, 2).
crosses(107, 4, 6, 2).
crosses(107, 6, 8, 2).
crosses(107, 9, 11, 1).
crosses(107, 11, 13, 2).
crosses(107, 1, 4, 2).
crosses(107, 2, 5, 1).
crosses(107, 3, 6, 1).
crosses(107, 4, 7, 1).
crosses(107, 5, 8, 1).
crosses(107, 6, 9, 2).
crosses(107, 8, 11, 1).
crosses(107, 10, 13, 1).
crosses(107, 0, 4, 1).
crosses(107, 1, 5, 1).
crosses(107, 2, 6, 2).
crosses(107, 4, 8, 2).
crosses(107, 5, 9, 1).
crosses(107, 6, 10, 2).
crosses(107, 7, 11, 1).
crosses(107, 9, 13, 1).
crosses(107, 1, 6, 2).
crosses(107, 2, 7, 1).
crosses(107, 3, 8, 1).
crosses(107, 4, 9, 2).
crosses(107, 5, 10, 1).
crosses(107, 6, 11, 3).
crosses(107, 8, 13, 1).
crosses(107, 0, 6, 1).
crosses(107, 1, 7, 1).
crosses(107, 2, 8, 2).
crosses(107, 3, 9, 1).
crosses(107, 4, 10, 2).
crosses(107, 5, 11, 2).
crosses(107, 6, 12, 1).
crosses(107, 1, 8, 2).
crosses(107, 2, 9, 2).
crosses(107, 3, 10, 1).
crosses(107, 4, 11, 3).
crosses(107, 6, 13, 1).
crosses(107, 0, 8, 1).
crosses(107, 1, 9, 2).
crosses(107, 2, 10, 2).
crosses(107, 3, 11, 2).
crosses(107, 4, 12, 1).
crosses(107, 0, 9, 1).
crosses(107, 1, 10, 2).
crosses(107, 2, 11, 3).
crosses(107, 4, 13, 1).
crosses(107, 0, 10, 1).
crosses(107, 1, 11, 3).
crosses(107, 2, 12, 1).
crosses(107, 0, 11, 2).
crosses(107, 1, 12, 1).
crosses(107, 2, 13, 1).
crosses(107, 1, 13, 1).
crosses(108, 2, 4, 2).
crosses(108, 4, 6, 2).
crosses(108, 7, 9, 2).
crosses(108, 10, 12, 1).
crosses(108, 12, 14, 2).
crosses(108, 1, 4, 2).
crosses(108, 2, 5, 1).
crosses(108, 3, 6, 1).
crosses(108, 4, 7, 2).
crosses(108, 6, 9, 2).
crosses(108, 7, 10, 2).
crosses(108, 9, 12, 1).
crosses(108, 11, 14, 1).
crosses(108, 0, 4, 1).
crosses(108, 1, 5, 1).
crosses(108, 2, 6, 2).
crosses(108, 3, 7, 1).
crosses(108, 4, 8, 1).
crosses(108, 5, 9, 1).
crosses(108, 6, 10, 2).
crosses(108, 7, 11, 2).
crosses(108, 8, 12, 1).
crosses(108, 10, 14, 1).
crosses(108, 1, 6, 2).
crosses(108, 2, 7, 2).
crosses(108, 4, 9, 2).
crosses(108, 5, 10, 1).
crosses(108, 6, 11, 2).
crosses(108, 7, 12, 3).
crosses(108, 9, 14, 1).
crosses(108, 0, 6, 1).
crosses(108, 1, 7, 2).
crosses(108, 2, 8, 1).
crosses(108, 3, 9, 1).
crosses(108, 4, 10, 2).
crosses(108, 5, 11, 1).
crosses(108, 6, 12, 3).
crosses(108, 7, 13, 1).
crosses(108, 0, 7, 1).
crosses(108, 1, 8, 1).
crosses(108, 2, 9, 2).
crosses(108, 3, 10, 1).
crosses(108, 4, 11, 2).
crosses(108, 5, 12, 2).
crosses(108, 6, 13, 1).
crosses(108, 7, 14, 1).
crosses(108, 1, 9, 2).
crosses(108, 2, 10, 2).
crosses(108, 3, 11, 1).
crosses(108, 4, 12, 3).
crosses(108, 6, 14, 1).
crosses(108, 0, 9, 1).
crosses(108, 1, 10, 2).
crosses(108, 2, 11, 2).
crosses(108, 3, 12, 2).
crosses(108, 4, 13, 1).
crosses(108, 0, 10, 1).
crosses(108, 1, 11, 2).
crosses(108, 2, 12, 3).
crosses(108, 4, 14, 1).
crosses(108, 0, 11, 1).
crosses(108, 1, 12, 3).
crosses(108, 2, 13, 1).
crosses(108, 0, 12, 2).
crosses(108, 1, 13, 1).
crosses(108, 2, 14, 1).
crosses(108, 1, 14, 1).
crosses(109, 1, 3, 2).
crosses(109, 3, 5, 2).
crosses(109, 5, 7, 1).
crosses(109, 0, 3, 1).
crosses(109, 1, 4, 1).
crosses(109, 2, 5, 1).
crosses(109, 3, 6, 1).
crosses(109, 1, 5, 2).
crosses(109, 3, 7, 1).
crosses(109, 0, 5, 1).
crosses(109, 1, 6, 1).
crosses(109, 1, 7, 1).
crosses(110, 1, 3, 2).
crosses(110, 3, 5, 2).
crosses(110, 5, 7, 2).
crosses(110, 6, 8, 1).
crosses(110, 10, 12, 2).
crosses(110, 0, 3, 1).
crosses(110, 1, 4, 1).
crosses(110, 2, 5, 1).
crosses(110, 3, 6, 1).
crosses(110, 4, 7, 1).
crosses(110, 5, 8, 3).
crosses(110, 6, 9, 1).
crosses(110, 9, 12, 2).
crosses(110, 1, 5, 2).
crosses(110, 3, 7, 2).
crosses(110, 4, 8, 2).
crosses(110, 5, 9, 3).
crosses(110, 6, 10, 1).
crosses(110, 8, 12, 2).
crosses(110, 0, 5, 1).
crosses(110, 1, 6, 1).
crosses(110, 2, 7, 1).
crosses(110, 3, 8, 3).
crosses(110, 4, 9, 2).
crosses(110, 5, 10, 3).
crosses(110, 7, 12, 1).
crosses(110, 1, 7, 2).
crosses(110, 2, 8, 2).
crosses(110, 3, 9, 3).
crosses(110, 4, 10, 2).
crosses(110, 5, 11, 1).
crosses(110, 0, 7, 1).
crosses(110, 1, 8, 3).
crosses(110, 2, 9, 2).
crosses(110, 3, 10, 3).
crosses(110, 5, 12, 1).
crosses(110, 0, 8, 2).
crosses(110, 1, 9, 3).
crosses(110, 2, 10, 2).
crosses(110, 3, 11, 1).
crosses(110, 0, 9, 2).
crosses(110, 1, 10, 3).
crosses(110, 3, 12, 1).
crosses(110, 0, 10, 2).
crosses(110, 1, 11, 1).
crosses(110, 1, 12, 1).
crosses(111, 1, 3, 1).
crosses(111, 3, 5, 2).
crosses(111, 4, 6, 1).
crosses(111, 6, 8, 1).
crosses(111, 8, 10, 2).
crosses(111, 0, 3, 1).
crosses(111, 2, 5, 1).
crosses(111, 3, 6, 3).
crosses(111, 4, 7, 1).
crosses(111, 5, 8, 1).
crosses(111, 7, 10, 1).
crosses(111, 1, 5, 1).
crosses(111, 2, 6, 2).
crosses(111, 3, 7, 3).
crosses(111, 4, 8, 2).
crosses(111, 6, 10, 1).
crosses(111, 1, 6, 2).
crosses(111, 2, 7, 2).
crosses(111, 3, 8, 4).
crosses(111, 0, 6, 1).
crosses(111, 1, 7, 2).
crosses(111, 2, 8, 3).
crosses(111, 3, 9, 2).
crosses(111, 0, 7, 1).
crosses(111, 1, 8, 3).
crosses(111, 2, 9, 1).
crosses(111, 3, 10, 2).
crosses(111, 0, 8, 2).
crosses(111, 1, 9, 1).
crosses(111, 2, 10, 1).
crosses(111, 1, 10, 1).
crosses(112, 2, 4, 1).
crosses(112, 3, 5, 1).
crosses(112, 5, 7, 1).
crosses(112, 1, 4, 1).
crosses(112, 2, 5, 2).
crosses(112, 1, 5, 2).
crosses(112, 2, 6, 1).
crosses(112, 0, 5, 1).
crosses(112, 1, 6, 1).
crosses(112, 2, 7, 1).
crosses(112, 1, 7, 1).
crosses(113, 2, 4, 1).
crosses(113, 4, 6, 1).
crosses(113, 6, 8, 3).
crosses(113, 7, 9, 1).
crosses(113, 9, 11, 1).
crosses(113, 11, 13, 2).
crosses(113, 1, 4, 1).
crosses(113, 2, 5, 1).
crosses(113, 3, 6, 1).
crosses(113, 5, 8, 2).
crosses(113, 6, 9, 4).
crosses(113, 7, 10, 1).
crosses(113, 8, 11, 1).
crosses(113, 10, 13, 1).
crosses(113, 0, 4, 1).
crosses(113, 1, 5, 1).
crosses(113, 2, 6, 2).
crosses(113, 4, 8, 2).
crosses(113, 5, 9, 3).
crosses(113, 6, 10, 4).
crosses(113, 7, 11, 2).
crosses(113, 9, 13, 1).
crosses(113, 0, 5, 1).
crosses(113, 1, 6, 2).
crosses(113, 3, 8, 1).
crosses(113, 4, 9, 3).
crosses(113, 5, 10, 3).
crosses(113, 6, 11, 5).
crosses(113, 0, 6, 2).
crosses(113, 2, 8, 1).
crosses(113, 3, 9, 2).
crosses(113, 4, 10, 3).
crosses(113, 5, 11, 4).
crosses(113, 6, 12, 3).
crosses(113, 1, 8, 1).
crosses(113, 2, 9, 2).
crosses(113, 3, 10, 2).
crosses(113, 4, 11, 4).
crosses(113, 5, 12, 2).
crosses(113, 6, 13, 3).
crosses(113, 1, 9, 2).
crosses(113, 2, 10, 2).
crosses(113, 3, 11, 3).
crosses(113, 4, 12, 2).
crosses(113, 5, 13, 2).
crosses(113, 0, 9, 1).
crosses(113, 1, 10, 2).
crosses(113, 2, 11, 3).
crosses(113, 3, 12, 1).
crosses(113, 4, 13, 2).
crosses(113, 0, 10, 1).
crosses(113, 1, 11, 3).
crosses(113, 2, 12, 1).
crosses(113, 3, 13, 1).
crosses(113, 0, 11, 2).
crosses(113, 1, 12, 1).
crosses(113, 2, 13, 1).
crosses(113, 1, 13, 1).
crosses(114, 1, 3, 1).
crosses(114, 3, 5, 2).
crosses(114, 4, 6, 1).
crosses(114, 6, 8, 1).
crosses(114, 0, 3, 1).
crosses(114, 2, 5, 1).
crosses(114, 3, 6, 3).
crosses(114, 1, 5, 1).
crosses(114, 2, 6, 2).
crosses(114, 3, 7, 2).
crosses(114, 1, 6, 2).
crosses(114, 2, 7, 1).
crosses(114, 3, 8, 2).
crosses(114, 0, 6, 1).
crosses(114, 1, 7, 1).
crosses(114, 2, 8, 1).
crosses(114, 1, 8, 1).
crosses(115, 1, 3, 1).
crosses(115, 3, 5, 2).
crosses(115, 4, 6, 1).
crosses(115, 5, 7, 1).
crosses(115, 7, 9, 2).
crosses(115, 0, 3, 1).
crosses(115, 2, 5, 1).
crosses(115, 3, 6, 3).
crosses(115, 4, 7, 2).
crosses(115, 6, 9, 1).
crosses(115, 1, 5, 1).
crosses(115, 2, 6, 2).
crosses(115, 3, 7, 4).
crosses(115, 1, 6, 2).
crosses(115, 2, 7, 3).
crosses(115, 3, 8, 2).
crosses(115, 0, 6, 1).
crosses(115, 1, 7, 3).
crosses(115, 2, 8, 1).
crosses(115, 3, 9, 2).
crosses(115, 0, 7, 2).
crosses(115, 1, 8, 1).
crosses(115, 2, 9, 1).
crosses(115, 1, 9, 1).
crosses(116, 2, 4, 1).
crosses(116, 4, 6, 2).
crosses(116, 5, 7, 1).
crosses(116, 7, 9, 1).
crosses(116, 1, 4, 1).
crosses(116, 3, 6, 1).
crosses(116, 4, 7, 3).
crosses(116, 0, 4, 1).
crosses(116, 2, 6, 1).
crosses(116, 3, 7, 2).
crosses(116, 4, 8, 2).
crosses(116, 1, 6, 1).
crosses(116, 2, 7, 2).
crosses(116, 3, 8, 1).
crosses(116, 4, 9, 2).
crosses(116, 1, 7, 2).
crosses(116, 2, 8, 1).
crosses(116, 3, 9, 1).
crosses(116, 0, 7, 1).
crosses(116, 1, 8, 1).
crosses(116, 2, 9, 1).
crosses(116, 1, 9, 1).
crosses(117, 2, 4, 1).
crosses(117, 3, 5, 1).
crosses(117, 5, 7, 3).
crosses(117, 6, 8, 1).
crosses(117, 7, 9, 1).
crosses(117, 9, 11, 2).
crosses(117, 1, 4, 1).
crosses(117, 2, 5, 2).
crosses(117, 4, 7, 2).
crosses(117, 5, 8, 4).
crosses(117, 6, 9, 2).
crosses(117, 8, 11, 1).
crosses(117, 0, 4, 1).
crosses(117, 1, 5, 2).
crosses(117, 3, 7, 1).
crosses(117, 4, 8, 3).
crosses(117, 5, 9, 5).
crosses(117, 0, 5, 2).
crosses(117, 2, 7, 1).
crosses(117, 3, 8, 2).
crosses(117, 4, 9, 4).
crosses(117, 5, 10, 3).
crosses(117, 1, 7, 1).
crosses(117, 2, 8, 2).
crosses(117, 3, 9, 3).
crosses(117, 4, 10, 2).
crosses(117, 5, 11, 3).
crosses(117, 1, 8, 2).
crosses(117, 2, 9, 3).
crosses(117, 3, 10, 1).
crosses(117, 4, 11, 2).
crosses(117, 0, 8, 1).
crosses(117, 1, 9, 3).
crosses(117, 2, 10, 1).
crosses(117, 3, 11, 1).
crosses(117, 0, 9, 2).
crosses(117, 1, 10, 1).
crosses(117, 2, 11, 1).
crosses(117, 1, 11, 1).
crosses(118, 1, 3, 1).
crosses(118, 3, 5, 1).
crosses(118, 5, 7, 2).
crosses(118, 6, 8, 1).
crosses(118, 7, 9, 1).
crosses(118, 9, 11, 2).
crosses(118, 0, 3, 1).
crosses(118, 1, 4, 1).
crosses(118, 2, 5, 1).
crosses(118, 4, 7, 1).
crosses(118, 5, 8, 3).
crosses(118, 6, 9, 2).
crosses(118, 8, 11, 1).
crosses(118, 0, 4, 1).
crosses(118, 1, 5, 2).
crosses(118, 3, 7, 1).
crosses(118, 4, 8, 2).
crosses(118, 5, 9, 4).
crosses(118, 0, 5, 2).
crosses(118, 3, 8, 2).
crosses(118, 4, 9, 3).
crosses(118, 5, 10, 2).
crosses(118, 2, 8, 1).
crosses(118, 3, 9, 3).
crosses(118, 4, 10, 1).
crosses(118, 5, 11, 2).
crosses(118, 1, 8, 1).
crosses(118, 2, 9, 2).
crosses(118, 3, 10, 1).
crosses(118, 4, 11, 1).
crosses(118, 0, 8, 1).
crosses(118, 1, 9, 2).
crosses(118, 3, 11, 1).
crosses(118, 0, 9, 2).
crosses(119, 0, 2, 1).
crosses(119, 1, 3, 1).
crosses(119, 2, 4, 1).
crosses(119, 4, 6, 4).
crosses(119, 5, 7, 1).
crosses(119, 6, 8, 1).
crosses(119, 7, 9, 1).
crosses(119, 8, 10, 1).
crosses(119, 9, 11, 1).
crosses(119, 11, 13, 5).
crosses(119, 0, 3, 2).
crosses(119, 1, 4, 2).
crosses(119, 3, 6, 3).
crosses(119, 4, 7, 5).
crosses(119, 5, 8, 2).
crosses(119, 6, 9, 2).
crosses(119, 7, 10, 2).
crosses(119, 8, 11, 2).
crosses(119, 10, 13, 4).
crosses(119, 0, 4, 3).
crosses(119, 2, 6, 2).
crosses(119, 3, 7, 4).
crosses(119, 4, 8, 6).
crosses(119, 5, 9, 3).
crosses(119, 6, 10, 3).
crosses(119, 7, 11, 3).
crosses(119, 9, 13, 3).
crosses(119, 1, 6, 1).
crosses(119, 2, 7, 3).
crosses(119, 3, 8, 5).
crosses(119, 4, 9, 7).
crosses(119, 5, 10, 4).
crosses(119, 6, 11, 4).
crosses(119, 8, 13, 2).
crosses(119, 1, 7, 2).
crosses(119, 2, 8, 4).
crosses(119, 3, 9, 6).
crosses(119, 4, 10, 8).
crosses(119, 5, 11, 5).
crosses(119, 7, 13, 1).
crosses(119, 0, 7, 1).
crosses(119, 1, 8, 3).
crosses(119, 2, 9, 5).
crosses(119, 3, 10, 7).
crosses(119, 4, 11, 9).
crosses(119, 0, 8, 2).
crosses(119, 1, 9, 4).
crosses(119, 2, 10, 6).
crosses(119, 3, 11, 8).
crosses(119, 4, 12, 4).
crosses(119, 0, 9, 3).
crosses(119, 1, 10, 5).
crosses(119, 2, 11, 7).
crosses(119, 3, 12, 3).
crosses(119, 4, 13, 4).
crosses(119, 0, 10, 4).
crosses(119, 1, 11, 6).
crosses(119, 2, 12, 2).
crosses(119, 3, 13, 3).
crosses(119, 0, 11, 5).
crosses(119, 1, 12, 1).
crosses(119, 2, 13, 2).
crosses(119, 1, 13, 1).
crosses(120, 2, 4, 1).
crosses(120, 4, 6, 2).
crosses(120, 5, 7, 1).
crosses(120, 7, 9, 1).
crosses(120, 1, 4, 1).
crosses(120, 3, 6, 1).
crosses(120, 4, 7, 3).
crosses(120, 0, 4, 1).
crosses(120, 2, 6, 1).
crosses(120, 3, 7, 2).
crosses(120, 4, 8, 2).
crosses(120, 1, 6, 1).
crosses(120, 2, 7, 2).
crosses(120, 3, 8, 1).
crosses(120, 4, 9, 2).
crosses(120, 1, 7, 2).
crosses(120, 2, 8, 1).
crosses(120, 3, 9, 1).
crosses(120, 0, 7, 1).
crosses(120, 1, 8, 1).
crosses(120, 2, 9, 1).
crosses(120, 1, 9, 1).
crosses(121, 2, 4, 1).
crosses(121, 3, 5, 1).
crosses(121, 5, 7, 3).
crosses(121, 6, 8, 1).
crosses(121, 7, 9, 1).
crosses(121, 8, 10, 1).
crosses(121, 9, 11, 1).
crosses(121, 10, 12, 1).
crosses(121, 12, 14, 5).
crosses(121, 1, 4, 1).
crosses(121, 2, 5, 2).
crosses(121, 4, 7, 2).
crosses(121, 5, 8, 4).
crosses(121, 6, 9, 2).
crosses(121, 7, 10, 2).
crosses(121, 8, 11, 2).
crosses(121, 9, 12, 2).
crosses(121, 11, 14, 4).
crosses(121, 0, 4, 1).
crosses(121, 1, 5, 2).
crosses(121, 3, 7, 1).
crosses(121, 4, 8, 3).
crosses(121, 5, 9, 5).
crosses(121, 6, 10, 3).
crosses(121, 7, 11, 3).
crosses(121, 8, 12, 3).
crosses(121, 10, 14, 3).
crosses(121, 0, 5, 2).
crosses(121, 2, 7, 1).
crosses(121, 3, 8, 2).
crosses(121, 4, 9, 4).
crosses(121, 5, 10, 6).
crosses(121, 6, 11, 4).
crosses(121, 7, 12, 4).
crosses(121, 9, 14, 2).
crosses(121, 1, 7, 1).
crosses(121, 2, 8, 2).
crosses(121, 3, 9, 3).
crosses(121, 4, 10, 5).
crosses(121, 5, 11, 7).
crosses(121, 6, 12, 5).
crosses(121, 8, 14, 1).
crosses(121, 1, 8, 2).
crosses(121, 2, 9, 3).
crosses(121, 3, 10, 4).
crosses(121, 4, 11, 6).
crosses(121, 5, 12, 8).
crosses(121, 0, 8, 1).
crosses(121, 1, 9, 3).
crosses(121, 2, 10, 4).
crosses(121, 3, 11, 5).
crosses(121, 4, 12, 7).
crosses(121, 5, 13, 3).
crosses(121, 0, 9, 2).
crosses(121, 1, 10, 4).
crosses(121, 2, 11, 5).
crosses(121, 3, 12, 6).
crosses(121, 4, 13, 2).
crosses(121, 5, 14, 3).
crosses(121, 0, 10, 3).
crosses(121, 1, 11, 5).
crosses(121, 2, 12, 6).
crosses(121, 3, 13, 1).
crosses(121, 4, 14, 2).
crosses(121, 0, 11, 4).
crosses(121, 1, 12, 6).
crosses(121, 2, 13, 1).
crosses(121, 3, 14, 1).
crosses(121, 0, 12, 5).
crosses(121, 1, 13, 1).
crosses(121, 2, 14, 1).
crosses(121, 1, 14, 1).
crosses(122, 2, 4, 2).
crosses(122, 3, 5, 1).
crosses(122, 4, 6, 1).
crosses(122, 5, 7, 1).
crosses(122, 6, 8, 1).
crosses(122, 8, 10, 1).
crosses(122, 10, 12, 6).
crosses(122, 1, 4, 2).
crosses(122, 2, 5, 3).
crosses(122, 3, 6, 2).
crosses(122, 4, 7, 2).
crosses(122, 5, 8, 2).
crosses(122, 6, 9, 1).
crosses(122, 7, 10, 1).
crosses(122, 9, 12, 5).
crosses(122, 0, 4, 1).
crosses(122, 1, 5, 3).
crosses(122, 2, 6, 4).
crosses(122, 3, 7, 3).
crosses(122, 4, 8, 3).
crosses(122, 5, 9, 2).
crosses(122, 6, 10, 2).
crosses(122, 8, 12, 5).
crosses(122, 0, 5, 2).
crosses(122, 1, 6, 4).
crosses(122, 2, 7, 5).
crosses(122, 3, 8, 4).
crosses(122, 4, 9, 3).
crosses(122, 5, 10, 3).
crosses(122, 7, 12, 4).
crosses(122, 0, 6, 3).
crosses(122, 1, 7, 5).
crosses(122, 2, 8, 6).
crosses(122, 3, 9, 4).
crosses(122, 4, 10, 4).
crosses(122, 6, 12, 3).
crosses(122, 0, 7, 4).
crosses(122, 1, 8, 6).
crosses(122, 2, 9, 6).
crosses(122, 3, 10, 5).
crosses(122, 5, 12, 2).
crosses(122, 0, 8, 5).
crosses(122, 1, 9, 6).
crosses(122, 2, 10, 7).
crosses(122, 4, 12, 1).
crosses(122, 0, 9, 5).
crosses(122, 1, 10, 7).
crosses(122, 2, 11, 1).
crosses(122, 0, 10, 6).
crosses(122, 1, 11, 1).
crosses(122, 2, 12, 1).
crosses(122, 1, 12, 1).
crosses(123, 2, 4, 1).
crosses(123, 3, 5, 1).
crosses(123, 7, 9, 1).
crosses(123, 1, 4, 1).
crosses(123, 2, 5, 2).
crosses(123, 3, 6, 1).
crosses(123, 6, 9, 1).
crosses(123, 1, 5, 2).
crosses(123, 2, 6, 2).
crosses(123, 3, 7, 1).
crosses(123, 5, 9, 1).
crosses(123, 0, 5, 1).
crosses(123, 1, 6, 2).
crosses(123, 2, 7, 2).
crosses(123, 0, 6, 1).
crosses(123, 1, 7, 2).
crosses(123, 2, 8, 1).
crosses(123, 0, 7, 1).
crosses(123, 1, 8, 1).
crosses(123, 2, 9, 1).
crosses(123, 1, 9, 1).
crosses(124, 2, 4, 1).
crosses(124, 3, 5, 1).
crosses(124, 7, 9, 1).
crosses(124, 8, 10, 1).
crosses(124, 10, 12, 3).
crosses(124, 1, 4, 1).
crosses(124, 2, 5, 2).
crosses(124, 3, 6, 1).
crosses(124, 6, 9, 1).
crosses(124, 7, 10, 2).
crosses(124, 9, 12, 2).
crosses(124, 1, 5, 2).
crosses(124, 2, 6, 2).
crosses(124, 3, 7, 1).
crosses(124, 5, 9, 1).
crosses(124, 6, 10, 2).
crosses(124, 8, 12, 1).
crosses(124, 0, 5, 1).
crosses(124, 1, 6, 2).
crosses(124, 2, 7, 2).
crosses(124, 3, 8, 1).
crosses(124, 4, 9, 1).
crosses(124, 5, 10, 2).
crosses(124, 7, 12, 1).
crosses(124, 0, 6, 1).
crosses(124, 1, 7, 2).
crosses(124, 2, 8, 2).
crosses(124, 3, 9, 2).
crosses(124, 4, 10, 2).
crosses(124, 6, 12, 1).
crosses(124, 0, 7, 1).
crosses(124, 1, 8, 2).
crosses(124, 2, 9, 3).
crosses(124, 3, 10, 3).
crosses(124, 5, 12, 1).
crosses(124, 0, 8, 1).
crosses(124, 1, 9, 3).
crosses(124, 2, 10, 4).
crosses(124, 0, 9, 2).
crosses(124, 1, 10, 4).
crosses(124, 2, 11, 1).
crosses(124, 0, 10, 3).
crosses(124, 1, 11, 1).
crosses(124, 2, 12, 1).
crosses(124, 1, 12, 1).
crosses(125, 0, 2, 1).
crosses(125, 1, 3, 1).
crosses(125, 3, 5, 4).
crosses(125, 7, 9, 2).
crosses(125, 8, 10, 1).
crosses(125, 10, 12, 2).
crosses(125, 0, 3, 2).
crosses(125, 2, 5, 3).
crosses(125, 3, 6, 4).
crosses(125, 6, 9, 2).
crosses(125, 7, 10, 3).
crosses(125, 9, 12, 1).
crosses(125, 1, 5, 2).
crosses(125, 2, 6, 3).
crosses(125, 3, 7, 4).
crosses(125, 5, 9, 2).
crosses(125, 6, 10, 3).
crosses(125, 7, 11, 1).
crosses(125, 0, 5, 1).
crosses(125, 1, 6, 2).
crosses(125, 2, 7, 3).
crosses(125, 3, 8, 3).
crosses(125, 4, 9, 1).
crosses(125, 5, 10, 3).
crosses(125, 6, 11, 1).
crosses(125, 7, 12, 1).
crosses(125, 0, 6, 1).
crosses(125, 1, 7, 2).
crosses(125, 2, 8, 2).
crosses(125, 3, 9, 4).
crosses(125, 4, 10, 2).
crosses(125, 5, 11, 1).
crosses(125, 6, 12, 1).
crosses(125, 0, 7, 1).
crosses(125, 1, 8, 1).
crosses(125, 2, 9, 3).
crosses(125, 3, 10, 5).
crosses(125, 5, 12, 1).
crosses(125, 1, 9, 2).
crosses(125, 2, 10, 4).
crosses(125, 3, 11, 3).
crosses(125, 0, 9, 1).
crosses(125, 1, 10, 3).
crosses(125, 2, 11, 2).
crosses(125, 3, 12, 3).
crosses(125, 0, 10, 2).
crosses(125, 1, 11, 1).
crosses(125, 2, 12, 2).
crosses(125, 1, 12, 1).
crosses(126, 0, 2, 1).
crosses(126, 1, 3, 1).
crosses(126, 3, 5, 3).
crosses(126, 4, 6, 1).
crosses(126, 7, 9, 2).
crosses(126, 8, 10, 1).
crosses(126, 10, 12, 2).
crosses(126, 0, 3, 2).
crosses(126, 2, 5, 2).
crosses(126, 3, 6, 4).
crosses(126, 4, 7, 1).
crosses(126, 6, 9, 2).
crosses(126, 7, 10, 3).
crosses(126, 9, 12, 1).
crosses(126, 1, 5, 1).
crosses(126, 2, 6, 3).
crosses(126, 3, 7, 4).
crosses(126, 5, 9, 1).
crosses(126, 6, 10, 3).
crosses(126, 7, 11, 1).
crosses(126, 1, 6, 2).
crosses(126, 2, 7, 3).
crosses(126, 3, 8, 3).
crosses(126, 4, 9, 1).
crosses(126, 5, 10, 2).
crosses(126, 6, 11, 1).
crosses(126, 7, 12, 1).
crosses(126, 0, 6, 1).
crosses(126, 1, 7, 2).
crosses(126, 2, 8, 2).
crosses(126, 3, 9, 4).
crosses(126, 4, 10, 2).
crosses(126, 6, 12, 1).
crosses(126, 0, 7, 1).
crosses(126, 1, 8, 1).
crosses(126, 2, 9, 3).
crosses(126, 3, 10, 5).
crosses(126, 1, 9, 2).
crosses(126, 2, 10, 4).
crosses(126, 3, 11, 3).
crosses(126, 0, 9, 1).
crosses(126, 1, 10, 3).
crosses(126, 2, 11, 2).
crosses(126, 3, 12, 3).
crosses(126, 0, 10, 2).
crosses(126, 1, 11, 1).
crosses(126, 2, 12, 2).
crosses(126, 1, 12, 1).
crosses(127, 1, 3, 1).
crosses(127, 3, 5, 1).
crosses(127, 6, 8, 1).
crosses(127, 7, 9, 1).
crosses(127, 9, 11, 2).
crosses(127, 0, 3, 1).
crosses(127, 3, 6, 1).
crosses(127, 5, 8, 1).
crosses(127, 6, 9, 2).
crosses(127, 8, 11, 1).
crosses(127, 3, 7, 1).
crosses(127, 4, 8, 1).
crosses(127, 5, 9, 2).
crosses(127, 3, 8, 2).
crosses(127, 4, 9, 2).
crosses(127, 2, 8, 1).
crosses(127, 3, 9, 3).
crosses(127, 1, 8, 1).
crosses(127, 2, 9, 2).
crosses(127, 3, 10, 1).
crosses(127, 0, 8, 1).
crosses(127, 1, 9, 2).
crosses(127, 3, 11, 1).
crosses(127, 0, 9, 2).
crosses(128, 1, 3, 1).
crosses(128, 3, 5, 1).
crosses(128, 4, 6, 1).
crosses(128, 7, 9, 2).
crosses(128, 8, 10, 1).
crosses(128, 10, 12, 2).
crosses(128, 0, 3, 1).
crosses(128, 3, 6, 2).
crosses(128, 4, 7, 1).
crosses(128, 6, 9, 2).
crosses(128, 7, 10, 3).
crosses(128, 9, 12, 1).
crosses(128, 2, 6, 1).
crosses(128, 3, 7, 2).
crosses(128, 5, 9, 1).
crosses(128, 6, 10, 3).
crosses(128, 7, 11, 1).
crosses(128, 1, 6, 1).
crosses(128, 2, 7, 1).
crosses(128, 3, 8, 1).
crosses(128, 4, 9, 1).
crosses(128, 5, 10, 2).
crosses(128, 6, 11, 1).
crosses(128, 7, 12, 1).
crosses(128, 0, 6, 1).
crosses(128, 1, 7, 1).
crosses(128, 3, 9, 2).
crosses(128, 4, 10, 2).
crosses(128, 6, 12, 1).
crosses(128, 0, 7, 1).
crosses(128, 2, 9, 1).
crosses(128, 3, 10, 3).
crosses(128, 1, 9, 1).
crosses(128, 2, 10, 2).
crosses(128, 3, 11, 1).
crosses(128, 0, 9, 1).
crosses(128, 1, 10, 2).
crosses(128, 3, 12, 1).
crosses(128, 0, 10, 2).
crosses(129, 3, 5, 1).
crosses(129, 6, 8, 1).
crosses(129, 7, 9, 1).
crosses(129, 9, 11, 2).
crosses(129, 2, 5, 1).
crosses(129, 3, 6, 1).
crosses(129, 5, 8, 1).
crosses(129, 6, 9, 2).
crosses(129, 8, 11, 1).
crosses(129, 1, 5, 1).
crosses(129, 2, 6, 1).
crosses(129, 3, 7, 1).
crosses(129, 4, 8, 1).
crosses(129, 5, 9, 2).
crosses(129, 1, 6, 1).
crosses(129, 2, 7, 1).
crosses(129, 3, 8, 2).
crosses(129, 4, 9, 2).
crosses(129, 1, 7, 1).
crosses(129, 2, 8, 2).
crosses(129, 3, 9, 3).
crosses(129, 1, 8, 2).
crosses(129, 2, 9, 3).
crosses(129, 3, 10, 1).
crosses(129, 0, 8, 1).
crosses(129, 1, 9, 3).
crosses(129, 2, 10, 1).
crosses(129, 3, 11, 1).
crosses(129, 0, 9, 2).
crosses(129, 1, 10, 1).
crosses(129, 2, 11, 1).
crosses(129, 1, 11, 1).
crosses(130, 3, 5, 1).
crosses(130, 4, 6, 1).
crosses(130, 7, 9, 2).
crosses(130, 8, 10, 1).
crosses(130, 10, 12, 2).
crosses(130, 2, 5, 1).
crosses(130, 3, 6, 2).
crosses(130, 4, 7, 1).
crosses(130, 6, 9, 2).
crosses(130, 7, 10, 3).
crosses(130, 9, 12, 1).
crosses(130, 1, 5, 1).
crosses(130, 2, 6, 2).
crosses(130, 3, 7, 2).
crosses(130, 5, 9, 1).
crosses(130, 6, 10, 3).
crosses(130, 7, 11, 1).
crosses(130, 1, 6, 2).
crosses(130, 2, 7, 2).
crosses(130, 3, 8, 1).
crosses(130, 4, 9, 1).
crosses(130, 5, 10, 2).
crosses(130, 6, 11, 1).
crosses(130, 7, 12, 1).
crosses(130, 0, 6, 1).
crosses(130, 1, 7, 2).
crosses(130, 2, 8, 1).
crosses(130, 3, 9, 2).
crosses(130, 4, 10, 2).
crosses(130, 6, 12, 1).
crosses(130, 0, 7, 1).
crosses(130, 1, 8, 1).
crosses(130, 2, 9, 2).
crosses(130, 3, 10, 3).
crosses(130, 1, 9, 2).
crosses(130, 2, 10, 3).
crosses(130, 3, 11, 1).
crosses(130, 0, 9, 1).
crosses(130, 1, 10, 3).
crosses(130, 2, 11, 1).
crosses(130, 3, 12, 1).
crosses(130, 0, 10, 2).
crosses(130, 1, 11, 1).
crosses(130, 2, 12, 1).
crosses(130, 1, 12, 1).
crosses(131, 1, 3, 2).
crosses(131, 3, 5, 2).
crosses(131, 6, 8, 1).
crosses(131, 0, 3, 1).
crosses(131, 1, 4, 1).
crosses(131, 2, 5, 1).
crosses(131, 3, 6, 2).
crosses(131, 5, 8, 1).
crosses(131, 1, 5, 2).
crosses(131, 2, 6, 1).
crosses(131, 3, 7, 1).
crosses(131, 0, 5, 1).
crosses(131, 1, 6, 2).
crosses(131, 3, 8, 1).
crosses(131, 0, 6, 1).
crosses(131, 1, 7, 1).
crosses(131, 1, 8, 1).
crosses(132, 1, 3, 1).
crosses(132, 2, 4, 1).
crosses(132, 4, 6, 1).
crosses(132, 1, 4, 2).
crosses(132, 0, 4, 1).
crosses(132, 1, 5, 1).
crosses(132, 1, 6, 1).
crosses(133, 1, 3, 2).
crosses(133, 3, 5, 2).
crosses(133, 6, 8, 1).
crosses(133, 0, 3, 1).
crosses(133, 1, 4, 1).
crosses(133, 2, 5, 1).
crosses(133, 3, 6, 2).
crosses(133, 5, 8, 1).
crosses(133, 1, 5, 2).
crosses(133, 2, 6, 1).
crosses(133, 3, 7, 1).
crosses(133, 0, 5, 1).
crosses(133, 1, 6, 2).
crosses(133, 3, 8, 1).
crosses(133, 0, 6, 1).
crosses(133, 1, 7, 1).
crosses(133, 1, 8, 1).
crosses(134, 1, 3, 1).
crosses(134, 3, 5, 2).
crosses(134, 4, 6, 1).
crosses(134, 6, 8, 1).
crosses(134, 8, 10, 2).
crosses(134, 0, 3, 1).
crosses(134, 2, 5, 1).
crosses(134, 3, 6, 3).
crosses(134, 4, 7, 1).
crosses(134, 5, 8, 1).
crosses(134, 7, 10, 1).
crosses(134, 1, 5, 1).
crosses(134, 2, 6, 2).
crosses(134, 3, 7, 3).
crosses(134, 4, 8, 2).
crosses(134, 6, 10, 1).
crosses(134, 1, 6, 2).
crosses(134, 2, 7, 2).
crosses(134, 3, 8, 4).
crosses(134, 0, 6, 1).
crosses(134, 1, 7, 2).
crosses(134, 2, 8, 3).
crosses(134, 3, 9, 2).
crosses(134, 0, 7, 1).
crosses(134, 1, 8, 3).
crosses(134, 2, 9, 1).
crosses(134, 3, 10, 2).
crosses(134, 0, 8, 2).
crosses(134, 1, 9, 1).
crosses(134, 2, 10, 1).
crosses(134, 1, 10, 1).
crosses(135, 1, 3, 1).
crosses(135, 3, 5, 2).
crosses(135, 4, 6, 1).
crosses(135, 6, 8, 1).
crosses(135, 7, 9, 1).
crosses(135, 9, 11, 3).
crosses(135, 0, 3, 1).
crosses(135, 2, 5, 1).
crosses(135, 3, 6, 3).
crosses(135, 4, 7, 1).
crosses(135, 5, 8, 1).
crosses(135, 6, 9, 2).
crosses(135, 8, 11, 2).
crosses(135, 1, 5, 1).
crosses(135, 2, 6, 2).
crosses(135, 3, 7, 3).
crosses(135, 4, 8, 2).
crosses(135, 5, 9, 2).
crosses(135, 7, 11, 1).
crosses(135, 1, 6, 2).
crosses(135, 2, 7, 2).
crosses(135, 3, 8, 4).
crosses(135, 4, 9, 3).
crosses(135, 6, 11, 1).
crosses(135, 0, 6, 1).
crosses(135, 1, 7, 2).
crosses(135, 2, 8, 3).
crosses(135, 3, 9, 5).
crosses(135, 0, 7, 1).
crosses(135, 1, 8, 3).
crosses(135, 2, 9, 4).
crosses(135, 3, 10, 2).
crosses(135, 0, 8, 2).
crosses(135, 1, 9, 4).
crosses(135, 2, 10, 1).
crosses(135, 3, 11, 2).
crosses(135, 0, 9, 3).
crosses(135, 1, 10, 1).
crosses(135, 2, 11, 1).
crosses(135, 1, 11, 1).
crosses(136, 1, 3, 1).
crosses(136, 3, 5, 3).
crosses(136, 5, 7, 2).
crosses(136, 7, 9, 1).
crosses(136, 9, 11, 2).
crosses(136, 0, 3, 1).
crosses(136, 2, 5, 2).
crosses(136, 3, 6, 2).
crosses(136, 4, 7, 1).
crosses(136, 5, 8, 2).
crosses(136, 6, 9, 1).
crosses(136, 8, 11, 1).
crosses(136, 1, 5, 2).
crosses(136, 2, 6, 1).
crosses(136, 3, 7, 3).
crosses(136, 4, 8, 1).
crosses(136, 5, 9, 3).
crosses(136, 7, 11, 1).
crosses(136, 0, 5, 1).
crosses(136, 1, 6, 1).
crosses(136, 2, 7, 2).
crosses(136, 3, 8, 3).
crosses(136, 4, 9, 2).
crosses(136, 5, 10, 1).
crosses(136, 1, 7, 2).
crosses(136, 2, 8, 2).
crosses(136, 3, 9, 4).
crosses(136, 5, 11, 1).
crosses(136, 0, 7, 1).
crosses(136, 1, 8, 2).
crosses(136, 2, 9, 3).
crosses(136, 3, 10, 2).
crosses(136, 0, 8, 1).
crosses(136, 1, 9, 3).
crosses(136, 2, 10, 1).
crosses(136, 3, 11, 2).
crosses(136, 0, 9, 2).
crosses(136, 1, 10, 1).
crosses(136, 2, 11, 1).
crosses(136, 1, 11, 1).
crosses(137, 2, 4, 1).
crosses(137, 4, 6, 2).
crosses(137, 5, 7, 1).
crosses(137, 7, 9, 1).
crosses(137, 9, 11, 2).
crosses(137, 1, 4, 1).
crosses(137, 3, 6, 1).
crosses(137, 4, 7, 3).
crosses(137, 5, 8, 1).
crosses(137, 6, 9, 1).
crosses(137, 8, 11, 1).
crosses(137, 0, 4, 1).
crosses(137, 2, 6, 1).
crosses(137, 3, 7, 2).
crosses(137, 4, 8, 3).
crosses(137, 5, 9, 2).
crosses(137, 7, 11, 1).
crosses(137, 1, 6, 1).
crosses(137, 2, 7, 2).
crosses(137, 3, 8, 2).
crosses(137, 4, 9, 4).
crosses(137, 1, 7, 2).
crosses(137, 2, 8, 2).
crosses(137, 3, 9, 3).
crosses(137, 4, 10, 2).
crosses(137, 0, 7, 1).
crosses(137, 1, 8, 2).
crosses(137, 2, 9, 3).
crosses(137, 3, 10, 1).
crosses(137, 4, 11, 2).
crosses(137, 0, 8, 1).
crosses(137, 1, 9, 3).
crosses(137, 2, 10, 1).
crosses(137, 3, 11, 1).
crosses(137, 0, 9, 2).
crosses(137, 1, 10, 1).
crosses(137, 2, 11, 1).
crosses(137, 1, 11, 1).
crosses(138, 1, 3, 1).
crosses(138, 3, 5, 2).
crosses(138, 4, 6, 1).
crosses(138, 6, 8, 1).
crosses(138, 7, 9, 1).
crosses(138, 9, 11, 3).
crosses(138, 0, 3, 1).
crosses(138, 2, 5, 1).
crosses(138, 3, 6, 3).
crosses(138, 4, 7, 1).
crosses(138, 5, 8, 1).
crosses(138, 6, 9, 2).
crosses(138, 8, 11, 2).
crosses(138, 1, 5, 1).
crosses(138, 2, 6, 2).
crosses(138, 3, 7, 3).
crosses(138, 4, 8, 2).
crosses(138, 5, 9, 2).
crosses(138, 7, 11, 1).
crosses(138, 1, 6, 2).
crosses(138, 2, 7, 2).
crosses(138, 3, 8, 4).
crosses(138, 4, 9, 3).
crosses(138, 6, 11, 1).
crosses(138, 0, 6, 1).
crosses(138, 1, 7, 2).
crosses(138, 2, 8, 3).
crosses(138, 3, 9, 5).
crosses(138, 0, 7, 1).
crosses(138, 1, 8, 3).
crosses(138, 2, 9, 4).
crosses(138, 3, 10, 2).
crosses(138, 0, 8, 2).
crosses(138, 1, 9, 4).
crosses(138, 2, 10, 1).
crosses(138, 3, 11, 2).
crosses(138, 0, 9, 3).
crosses(138, 1, 10, 1).
crosses(138, 2, 11, 1).
crosses(138, 1, 11, 1).
crosses(139, 2, 4, 1).
crosses(139, 4, 6, 3).
crosses(139, 6, 8, 2).
crosses(139, 8, 10, 1).
crosses(139, 10, 12, 2).
crosses(139, 1, 4, 1).
crosses(139, 3, 6, 2).
crosses(139, 4, 7, 2).
crosses(139, 5, 8, 1).
crosses(139, 6, 9, 2).
crosses(139, 7, 10, 1).
crosses(139, 9, 12, 1).
crosses(139, 0, 4, 1).
crosses(139, 2, 6, 2).
crosses(139, 3, 7, 1).
crosses(139, 4, 8, 3).
crosses(139, 5, 9, 1).
crosses(139, 6, 10, 3).
crosses(139, 8, 12, 1).
crosses(139, 1, 6, 2).
crosses(139, 2, 7, 1).
crosses(139, 3, 8, 2).
crosses(139, 4, 9, 3).
crosses(139, 5, 10, 2).
crosses(139, 6, 11, 1).
crosses(139, 0, 6, 1).
crosses(139, 1, 7, 1).
crosses(139, 2, 8, 2).
crosses(139, 3, 9, 2).
crosses(139, 4, 10, 4).
crosses(139, 6, 12, 1).
crosses(139, 1, 8, 2).
crosses(139, 2, 9, 2).
crosses(139, 3, 10, 3).
crosses(139, 4, 11, 2).
crosses(139, 0, 8, 1).
crosses(139, 1, 9, 2).
crosses(139, 2, 10, 3).
crosses(139, 3, 11, 1).
crosses(139, 4, 12, 2).
crosses(139, 0, 9, 1).
crosses(139, 1, 10, 3).
crosses(139, 2, 11, 1).
crosses(139, 3, 12, 1).
crosses(139, 0, 10, 2).
crosses(139, 1, 11, 1).
crosses(139, 2, 12, 1).
crosses(139, 1, 12, 1).
crosses(140, 1, 3, 1).
crosses(140, 3, 5, 2).
crosses(140, 4, 6, 1).
crosses(140, 6, 8, 1).
crosses(140, 8, 10, 2).
crosses(140, 0, 3, 1).
crosses(140, 2, 5, 1).
crosses(140, 3, 6, 3).
crosses(140, 4, 7, 1).
crosses(140, 5, 8, 1).
crosses(140, 7, 10, 1).
crosses(140, 1, 5, 1).
crosses(140, 2, 6, 2).
crosses(140, 3, 7, 3).
crosses(140, 4, 8, 2).
crosses(140, 6, 10, 1).
crosses(140, 1, 6, 2).
crosses(140, 2, 7, 2).
crosses(140, 3, 8, 4).
crosses(140, 0, 6, 1).
crosses(140, 1, 7, 2).
crosses(140, 2, 8, 3).
crosses(140, 3, 9, 2).
crosses(140, 0, 7, 1).
crosses(140, 1, 8, 3).
crosses(140, 2, 9, 1).
crosses(140, 3, 10, 2).
crosses(140, 0, 8, 2).
crosses(140, 1, 9, 1).
crosses(140, 2, 10, 1).
crosses(140, 1, 10, 1).
crosses(141, 1, 3, 1).
crosses(141, 3, 5, 2).
crosses(141, 4, 6, 1).
crosses(141, 6, 8, 1).
crosses(141, 7, 9, 1).
crosses(141, 9, 11, 3).
crosses(141, 0, 3, 1).
crosses(141, 2, 5, 1).
crosses(141, 3, 6, 3).
crosses(141, 4, 7, 1).
crosses(141, 5, 8, 1).
crosses(141, 6, 9, 2).
crosses(141, 8, 11, 2).
crosses(141, 1, 5, 1).
crosses(141, 2, 6, 2).
crosses(141, 3, 7, 3).
crosses(141, 4, 8, 2).
crosses(141, 5, 9, 2).
crosses(141, 7, 11, 1).
crosses(141, 1, 6, 2).
crosses(141, 2, 7, 2).
crosses(141, 3, 8, 4).
crosses(141, 4, 9, 3).
crosses(141, 6, 11, 1).
crosses(141, 0, 6, 1).
crosses(141, 1, 7, 2).
crosses(141, 2, 8, 3).
crosses(141, 3, 9, 5).
crosses(141, 0, 7, 1).
crosses(141, 1, 8, 3).
crosses(141, 2, 9, 4).
crosses(141, 3, 10, 2).
crosses(141, 0, 8, 2).
crosses(141, 1, 9, 4).
crosses(141, 2, 10, 1).
crosses(141, 3, 11, 2).
crosses(141, 0, 9, 3).
crosses(141, 1, 10, 1).
crosses(141, 2, 11, 1).
crosses(141, 1, 11, 1).
crosses(142, 1, 3, 1).
crosses(142, 3, 5, 3).
crosses(142, 5, 7, 2).
crosses(142, 7, 9, 1).
crosses(142, 9, 11, 2).
crosses(142, 0, 3, 1).
crosses(142, 2, 5, 2).
crosses(142, 3, 6, 2).
crosses(142, 4, 7, 1).
crosses(142, 5, 8, 2).
crosses(142, 6, 9, 1).
crosses(142, 8, 11, 1).
crosses(142, 1, 5, 2).
crosses(142, 2, 6, 1).
crosses(142, 3, 7, 3).
crosses(142, 4, 8, 1).
crosses(142, 5, 9, 3).
crosses(142, 7, 11, 1).
crosses(142, 0, 5, 1).
crosses(142, 1, 6, 1).
crosses(142, 2, 7, 2).
crosses(142, 3, 8, 3).
crosses(142, 4, 9, 2).
crosses(142, 5, 10, 1).
crosses(142, 1, 7, 2).
crosses(142, 2, 8, 2).
crosses(142, 3, 9, 4).
crosses(142, 5, 11, 1).
crosses(142, 0, 7, 1).
crosses(142, 1, 8, 2).
crosses(142, 2, 9, 3).
crosses(142, 3, 10, 2).
crosses(142, 0, 8, 1).
crosses(142, 1, 9, 3).
crosses(142, 2, 10, 1).
crosses(142, 3, 11, 2).
crosses(142, 0, 9, 2).
crosses(142, 1, 10, 1).
crosses(142, 2, 11, 1).
crosses(142, 1, 11, 1).
crosses(143, 1, 3, 1).
crosses(143, 2, 4, 1).
crosses(143, 5, 7, 3).
crosses(143, 6, 8, 1).
crosses(143, 8, 10, 1).
crosses(143, 10, 12, 2).
crosses(143, 0, 3, 1).
crosses(143, 1, 4, 2).
crosses(143, 2, 5, 1).
crosses(143, 4, 7, 3).
crosses(143, 5, 8, 4).
crosses(143, 6, 9, 1).
crosses(143, 7, 10, 1).
crosses(143, 9, 12, 1).
crosses(143, 0, 4, 2).
crosses(143, 1, 5, 2).
crosses(143, 3, 7, 2).
crosses(143, 4, 8, 4).
crosses(143, 5, 9, 4).
crosses(143, 6, 10, 2).
crosses(143, 8, 12, 1).
crosses(143, 0, 5, 2).
crosses(143, 2, 7, 1).
crosses(143, 3, 8, 3).
crosses(143, 4, 9, 4).
crosses(143, 5, 10, 5).
crosses(143, 1, 7, 1).
crosses(143, 2, 8, 2).
crosses(143, 3, 9, 3).
crosses(143, 4, 10, 5).
crosses(143, 5, 11, 3).
crosses(143, 1, 8, 2).
crosses(143, 2, 9, 2).
crosses(143, 3, 10, 4).
crosses(143, 4, 11, 3).
crosses(143, 5, 12, 3).
crosses(143, 0, 8, 1).
crosses(143, 1, 9, 2).
crosses(143, 2, 10, 3).
crosses(143, 3, 11, 2).
crosses(143, 4, 12, 3).
crosses(143, 0, 9, 1).
crosses(143, 1, 10, 3).
crosses(143, 2, 11, 1).
crosses(143, 3, 12, 2).
crosses(143, 0, 10, 2).
crosses(143, 1, 11, 1).
crosses(143, 2, 12, 1).
crosses(143, 1, 12, 1).
crosses(144, 2, 4, 1).
crosses(144, 3, 5, 1).
crosses(144, 6, 8, 3).
crosses(144, 7, 9, 1).
crosses(144, 8, 10, 1).
crosses(144, 9, 11, 1).
crosses(144, 11, 13, 3).
crosses(144, 1, 4, 1).
crosses(144, 2, 5, 2).
crosses(144, 3, 6, 1).
crosses(144, 5, 8, 3).
crosses(144, 6, 9, 4).
crosses(144, 7, 10, 2).
crosses(144, 8, 11, 2).
crosses(144, 10, 13, 2).
crosses(144, 0, 4, 1).
crosses(144, 1, 5, 2).
crosses(144, 2, 6, 2).
crosses(144, 4, 8, 2).
crosses(144, 5, 9, 4).
crosses(144, 6, 10, 5).
crosses(144, 7, 11, 3).
crosses(144, 9, 13, 1).
crosses(144, 0, 5, 2).
crosses(144, 1, 6, 2).
crosses(144, 3, 8, 1).
crosses(144, 4, 9, 3).
crosses(144, 5, 10, 5).
crosses(144, 6, 11, 6).
crosses(144, 0, 6, 2).
crosses(144, 2, 8, 1).
crosses(144, 3, 9, 2).
crosses(144, 4, 10, 4).
crosses(144, 5, 11, 6).
crosses(144, 6, 12, 3).
crosses(144, 1, 8, 1).
crosses(144, 2, 9, 2).
crosses(144, 3, 10, 3).
crosses(144, 4, 11, 5).
crosses(144, 5, 12, 3).
crosses(144, 6, 13, 3).
crosses(144, 1, 9, 2).
crosses(144, 2, 10, 3).
crosses(144, 3, 11, 4).
crosses(144, 4, 12, 2).
crosses(144, 5, 13, 3).
crosses(144, 0, 9, 1).
crosses(144, 1, 10, 3).
crosses(144, 2, 11, 4).
crosses(144, 3, 12, 1).
crosses(144, 4, 13, 2).
crosses(144, 0, 10, 2).
crosses(144, 1, 11, 4).
crosses(144, 2, 12, 1).
crosses(144, 3, 13, 1).
crosses(144, 0, 11, 3).
crosses(144, 1, 12, 1).
crosses(144, 2, 13, 1).
crosses(144, 1, 13, 1).
crosses(145, 1, 3, 1).
crosses(145, 2, 4, 1).
crosses(145, 4, 6, 3).
crosses(145, 5, 7, 1).
crosses(145, 7, 9, 1).
crosses(145, 9, 11, 1).
crosses(145, 11, 13, 3).
crosses(145, 0, 3, 1).
crosses(145, 1, 4, 2).
crosses(145, 3, 6, 2).
crosses(145, 4, 7, 4).
crosses(145, 5, 8, 1).
crosses(145, 6, 9, 1).
crosses(145, 7, 10, 1).
crosses(145, 8, 11, 1).
crosses(145, 10, 13, 2).
crosses(145, 0, 4, 2).
crosses(145, 2, 6, 1).
crosses(145, 3, 7, 3).
crosses(145, 4, 8, 4).
crosses(145, 5, 9, 2).
crosses(145, 6, 10, 1).
crosses(145, 7, 11, 2).
crosses(145, 9, 13, 2).
crosses(145, 1, 6, 1).
crosses(145, 2, 7, 2).
crosses(145, 3, 8, 3).
crosses(145, 4, 9, 5).
crosses(145, 5, 10, 2).
crosses(145, 6, 11, 2).
crosses(145, 8, 13, 1).
crosses(145, 1, 7, 2).
crosses(145, 2, 8, 2).
crosses(145, 3, 9, 4).
crosses(145, 4, 10, 5).
crosses(145, 5, 11, 3).
crosses(145, 7, 13, 1).
crosses(145, 0, 7, 1).
crosses(145, 1, 8, 2).
crosses(145, 2, 9, 3).
crosses(145, 3, 10, 4).
crosses(145, 4, 11, 6).
crosses(145, 0, 8, 1).
crosses(145, 1, 9, 3).
crosses(145, 2, 10, 3).
crosses(145, 3, 11, 5).
crosses(145, 4, 12, 3).
crosses(145, 0, 9, 2).
crosses(145, 1, 10, 3).
crosses(145, 2, 11, 4).
crosses(145, 3, 12, 2).
crosses(145, 4, 13, 3).
crosses(145, 0, 10, 2).
crosses(145, 1, 11, 4).
crosses(145, 2, 12, 1).
crosses(145, 3, 13, 2).
crosses(145, 0, 11, 3).
crosses(145, 1, 12, 1).
crosses(145, 2, 13, 1).
crosses(145, 1, 13, 1).
crosses(146, 2, 4, 2).
crosses(146, 5, 7, 2).
crosses(146, 7, 9, 1).
crosses(146, 9, 11, 2).
crosses(146, 1, 4, 2).
crosses(146, 2, 5, 2).
crosses(146, 4, 7, 2).
crosses(146, 5, 8, 2).
crosses(146, 6, 9, 1).
crosses(146, 8, 11, 1).
crosses(146, 0, 4, 1).
crosses(146, 1, 5, 2).
crosses(146, 2, 6, 1).
crosses(146, 3, 7, 1).
crosses(146, 4, 8, 2).
crosses(146, 5, 9, 3).
crosses(146, 7, 11, 1).
crosses(146, 0, 5, 1).
crosses(146, 1, 6, 1).
crosses(146, 2, 7, 2).
crosses(146, 3, 8, 1).
crosses(146, 4, 9, 3).
crosses(146, 5, 10, 1).
crosses(146, 1, 7, 2).
crosses(146, 2, 8, 2).
crosses(146, 3, 9, 2).
crosses(146, 4, 10, 1).
crosses(146, 5, 11, 1).
crosses(146, 0, 7, 1).
crosses(146, 1, 8, 2).
crosses(146, 2, 9, 3).
crosses(146, 4, 11, 1).
crosses(146, 0, 8, 1).
crosses(146, 1, 9, 3).
crosses(146, 2, 10, 1).
crosses(146, 0, 9, 2).
crosses(146, 1, 10, 1).
crosses(146, 2, 11, 1).
crosses(146, 1, 11, 1).
crosses(147, 1, 3, 2).
crosses(147, 3, 5, 2).
crosses(147, 5, 7, 1).
crosses(147, 7, 9, 2).
crosses(147, 0, 3, 1).
crosses(147, 1, 4, 1).
crosses(147, 2, 5, 1).
crosses(147, 3, 6, 2).
crosses(147, 4, 7, 1).
crosses(147, 6, 9, 1).
crosses(147, 1, 5, 2).
crosses(147, 2, 6, 1).
crosses(147, 3, 7, 3).
crosses(147, 5, 9, 1).
crosses(147, 0, 5, 1).
crosses(147, 1, 6, 2).
crosses(147, 2, 7, 2).
crosses(147, 3, 8, 1).
crosses(147, 0, 6, 1).
crosses(147, 1, 7, 3).
crosses(147, 3, 9, 1).
crosses(147, 0, 7, 2).
crosses(147, 1, 8, 1).
crosses(147, 1, 9, 1).
crosses(148, 2, 4, 2).
crosses(148, 4, 6, 2).
crosses(148, 6, 8, 1).
crosses(148, 8, 10, 2).
crosses(148, 1, 4, 2).
crosses(148, 2, 5, 1).
crosses(148, 3, 6, 1).
crosses(148, 4, 7, 2).
crosses(148, 5, 8, 1).
crosses(148, 7, 10, 1).
crosses(148, 0, 4, 1).
crosses(148, 1, 5, 1).
crosses(148, 2, 6, 2).
crosses(148, 3, 7, 1).
crosses(148, 4, 8, 3).
crosses(148, 6, 10, 1).
crosses(148, 1, 6, 2).
crosses(148, 2, 7, 2).
crosses(148, 3, 8, 2).
crosses(148, 4, 9, 1).
crosses(148, 0, 6, 1).
crosses(148, 1, 7, 2).
crosses(148, 2, 8, 3).
crosses(148, 4, 10, 1).
crosses(148, 0, 7, 1).
crosses(148, 1, 8, 3).
crosses(148, 2, 9, 1).
crosses(148, 0, 8, 2).
crosses(148, 1, 9, 1).
crosses(148, 2, 10, 1).
crosses(148, 1, 10, 1).
crosses(149, 1, 3, 2).
crosses(149, 3, 5, 2).
crosses(149, 5, 7, 1).
crosses(149, 7, 9, 2).
crosses(149, 0, 3, 1).
crosses(149, 1, 4, 1).
crosses(149, 2, 5, 1).
crosses(149, 3, 6, 2).
crosses(149, 4, 7, 1).
crosses(149, 6, 9, 1).
crosses(149, 1, 5, 2).
crosses(149, 2, 6, 1).
crosses(149, 3, 7, 3).
crosses(149, 5, 9, 1).
crosses(149, 0, 5, 1).
crosses(149, 1, 6, 2).
crosses(149, 2, 7, 2).
crosses(149, 3, 8, 1).
crosses(149, 0, 6, 1).
crosses(149, 1, 7, 3).
crosses(149, 3, 9, 1).
crosses(149, 0, 7, 2).
crosses(149, 1, 8, 1).
crosses(149, 1, 9, 1).
crosses(150, 0, 2, 1).
crosses(150, 1, 3, 1).
crosses(150, 3, 5, 4).
crosses(150, 5, 7, 2).
crosses(150, 8, 10, 2).
crosses(150, 11, 13, 1).
crosses(150, 13, 15, 2).
crosses(150, 0, 3, 2).
crosses(150, 2, 5, 3).
crosses(150, 3, 6, 3).
crosses(150, 4, 7, 1).
crosses(150, 5, 8, 2).
crosses(150, 7, 10, 2).
crosses(150, 8, 11, 2).
crosses(150, 10, 13, 1).
crosses(150, 12, 15, 1).
crosses(150, 1, 5, 2).
crosses(150, 2, 6, 2).
crosses(150, 3, 7, 4).
crosses(150, 4, 8, 1).
crosses(150, 5, 9, 1).
crosses(150, 6, 10, 1).
crosses(150, 7, 11, 2).
crosses(150, 8, 12, 2).
crosses(150, 9, 13, 1).
crosses(150, 11, 15, 1).
crosses(150, 0, 5, 1).
crosses(150, 1, 6, 1).
crosses(150, 2, 7, 3).
crosses(150, 3, 8, 4).
crosses(150, 5, 10, 2).
crosses(150, 6, 11, 1).
crosses(150, 7, 12, 2).
crosses(150, 8, 13, 3).
crosses(150, 10, 15, 1).
crosses(150, 1, 7, 2).
crosses(150, 2, 8, 3).
crosses(150, 3, 9, 3).
crosses(150, 4, 10, 1).
crosses(150, 5, 11, 2).
crosses(150, 6, 12, 1).
crosses(150, 7, 13, 3).
crosses(150, 8, 14, 1).
crosses(150, 0, 7, 1).
crosses(150, 1, 8, 2).
crosses(150, 2, 9, 2).
crosses(150, 3, 10, 4).
crosses(150, 4, 11, 1).
crosses(150, 5, 12, 2).
crosses(150, 6, 13, 2).
crosses(150, 7, 14, 1).
crosses(150, 8, 15, 1).
crosses(150, 0, 8, 1).
crosses(150, 1, 9, 1).
crosses(150, 2, 10, 3).
crosses(150, 3, 11, 4).
crosses(150, 4, 12, 1).
crosses(150, 5, 13, 3).
crosses(150, 7, 15, 1).
crosses(150, 1, 10, 2).
crosses(150, 2, 11, 3).
crosses(150, 3, 12, 4).
crosses(150, 4, 13, 2).
crosses(150, 5, 14, 1).
crosses(150, 0, 10, 1).
crosses(150, 1, 11, 2).
crosses(150, 2, 12, 3).
crosses(150, 3, 13, 5).
crosses(150, 5, 15, 1).
crosses(150, 0, 11, 1).
crosses(150, 1, 12, 2).
crosses(150, 2, 13, 4).
crosses(150, 3, 14, 3).
crosses(150, 0, 12, 1).
crosses(150, 1, 13, 3).
crosses(150, 2, 14, 2).
crosses(150, 3, 15, 3).
crosses(150, 0, 13, 2).
crosses(150, 1, 14, 1).
crosses(150, 2, 15, 2).
crosses(150, 1, 15, 1).
crosses(151, 2, 4, 2).
crosses(151, 4, 6, 2).
crosses(151, 7, 9, 2).
crosses(151, 10, 12, 1).
crosses(151, 12, 14, 2).
crosses(151, 1, 4, 2).
crosses(151, 2, 5, 1).
crosses(151, 3, 6, 1).
crosses(151, 4, 7, 2).
crosses(151, 6, 9, 2).
crosses(151, 7, 10, 2).
crosses(151, 9, 12, 1).
crosses(151, 11, 14, 1).
crosses(151, 0, 4, 1).
crosses(151, 1, 5, 1).
crosses(151, 2, 6, 2).
crosses(151, 3, 7, 1).
crosses(151, 4, 8, 1).
crosses(151, 5, 9, 1).
crosses(151, 6, 10, 2).
crosses(151, 7, 11, 2).
crosses(151, 8, 12, 1).
crosses(151, 10, 14, 1).
crosses(151, 1, 6, 2).
crosses(151, 2, 7, 2).
crosses(151, 4, 9, 2).
crosses(151, 5, 10, 1).
crosses(151, 6, 11, 2).
crosses(151, 7, 12, 3).
crosses(151, 9, 14, 1).
crosses(151, 0, 6, 1).
crosses(151, 1, 7, 2).
crosses(151, 2, 8, 1).
crosses(151, 3, 9, 1).
crosses(151, 4, 10, 2).
crosses(151, 5, 11, 1).
crosses(151, 6, 12, 3).
crosses(151, 7, 13, 1).
crosses(151, 0, 7, 1).
crosses(151, 1, 8, 1).
crosses(151, 2, 9, 2).
crosses(151, 3, 10, 1).
crosses(151, 4, 11, 2).
crosses(151, 5, 12, 2).
crosses(151, 6, 13, 1).
crosses(151, 7, 14, 1).
crosses(151, 1, 9, 2).
crosses(151, 2, 10, 2).
crosses(151, 3, 11, 1).
crosses(151, 4, 12, 3).
crosses(151, 6, 14, 1).
crosses(151, 0, 9, 1).
crosses(151, 1, 10, 2).
crosses(151, 2, 11, 2).
crosses(151, 3, 12, 2).
crosses(151, 4, 13, 1).
crosses(151, 0, 10, 1).
crosses(151, 1, 11, 2).
crosses(151, 2, 12, 3).
crosses(151, 4, 14, 1).
crosses(151, 0, 11, 1).
crosses(151, 1, 12, 3).
crosses(151, 2, 13, 1).
crosses(151, 0, 12, 2).
crosses(151, 1, 13, 1).
crosses(151, 2, 14, 1).
crosses(151, 1, 14, 1).
crosses(152, 2, 4, 2).
crosses(152, 4, 6, 2).
crosses(152, 6, 8, 2).
crosses(152, 7, 9, 1).
crosses(152, 11, 13, 2).
crosses(152, 1, 4, 2).
crosses(152, 2, 5, 1).
crosses(152, 3, 6, 1).
crosses(152, 4, 7, 1).
crosses(152, 5, 8, 1).
crosses(152, 6, 9, 3).
crosses(152, 7, 10, 1).
crosses(152, 10, 13, 2).
crosses(152, 0, 4, 1).
crosses(152, 1, 5, 1).
crosses(152, 2, 6, 2).
crosses(152, 4, 8, 2).
crosses(152, 5, 9, 2).
crosses(152, 6, 10, 3).
crosses(152, 7, 11, 1).
crosses(152, 9, 13, 2).
crosses(152, 1, 6, 2).
crosses(152, 2, 7, 1).
crosses(152, 3, 8, 1).
crosses(152, 4, 9, 3).
crosses(152, 5, 10, 2).
crosses(152, 6, 11, 3).
crosses(152, 8, 13, 1).
crosses(152, 0, 6, 1).
crosses(152, 1, 7, 1).
crosses(152, 2, 8, 2).
crosses(152, 3, 9, 2).
crosses(152, 4, 10, 3).
crosses(152, 5, 11, 2).
crosses(152, 6, 12, 1).
crosses(152, 1, 8, 2).
crosses(152, 2, 9, 3).
crosses(152, 3, 10, 2).
crosses(152, 4, 11, 3).
crosses(152, 6, 13, 1).
crosses(152, 0, 8, 1).
crosses(152, 1, 9, 3).
crosses(152, 2, 10, 3).
crosses(152, 3, 11, 2).
crosses(152, 4, 12, 1).
crosses(152, 0, 9, 2).
crosses(152, 1, 10, 3).
crosses(152, 2, 11, 3).
crosses(152, 4, 13, 1).
crosses(152, 0, 10, 2).
crosses(152, 1, 11, 3).
crosses(152, 2, 12, 1).
crosses(152, 0, 11, 2).
crosses(152, 1, 12, 1).
crosses(152, 2, 13, 1).
crosses(152, 1, 13, 1).
crosses(153, 1, 3, 2).
crosses(153, 3, 5, 2).
crosses(153, 5, 7, 2).
crosses(153, 6, 8, 1).
crosses(153, 10, 12, 2).
crosses(153, 0, 3, 1).
crosses(153, 1, 4, 1).
crosses(153, 2, 5, 1).
crosses(153, 3, 6, 1).
crosses(153, 4, 7, 1).
crosses(153, 5, 8, 3).
crosses(153, 6, 9, 1).
crosses(153, 9, 12, 2).
crosses(153, 1, 5, 2).
crosses(153, 3, 7, 2).
crosses(153, 4, 8, 2).
crosses(153, 5, 9, 3).
crosses(153, 6, 10, 1).
crosses(153, 8, 12, 2).
crosses(153, 0, 5, 1).
crosses(153, 1, 6, 1).
crosses(153, 2, 7, 1).
crosses(153, 3, 8, 3).
crosses(153, 4, 9, 2).
crosses(153, 5, 10, 3).
crosses(153, 7, 12, 1).
crosses(153, 1, 7, 2).
crosses(153, 2, 8, 2).
crosses(153, 3, 9, 3).
crosses(153, 4, 10, 2).
crosses(153, 5, 11, 1).
crosses(153, 0, 7, 1).
crosses(153, 1, 8, 3).
crosses(153, 2, 9, 2).
crosses(153, 3, 10, 3).
crosses(153, 5, 12, 1).
crosses(153, 0, 8, 2).
crosses(153, 1, 9, 3).
crosses(153, 2, 10, 2).
crosses(153, 3, 11, 1).
crosses(153, 0, 9, 2).
crosses(153, 1, 10, 3).
crosses(153, 3, 12, 1).
crosses(153, 0, 10, 2).
crosses(153, 1, 11, 1).
crosses(153, 1, 12, 1).
crosses(154, 1, 3, 1).
crosses(154, 2, 4, 1).
crosses(154, 3, 5, 1).
crosses(154, 5, 7, 1).
crosses(154, 7, 9, 5).
crosses(154, 8, 10, 1).
crosses(154, 10, 12, 1).
crosses(154, 12, 14, 2).
crosses(154, 0, 3, 1).
crosses(154, 1, 4, 2).
crosses(154, 2, 5, 2).
crosses(154, 3, 6, 1).
crosses(154, 4, 7, 1).
crosses(154, 6, 9, 4).
crosses(154, 7, 10, 6).
crosses(154, 8, 11, 1).
crosses(154, 9, 12, 1).
crosses(154, 11, 14, 1).
crosses(154, 0, 4, 2).
crosses(154, 1, 5, 3).
crosses(154, 2, 6, 2).
crosses(154, 3, 7, 2).
crosses(154, 5, 9, 4).
crosses(154, 6, 10, 5).
crosses(154, 7, 11, 6).
crosses(154, 8, 12, 2).
crosses(154, 10, 14, 1).
crosses(154, 0, 5, 3).
crosses(154, 1, 6, 3).
crosses(154, 2, 7, 3).
crosses(154, 4, 9, 3).
crosses(154, 5, 10, 5).
crosses(154, 6, 11, 5).
crosses(154, 7, 12, 7).
crosses(154, 0, 6, 3).
crosses(154, 1, 7, 4).
crosses(154, 3, 9, 2).
crosses(154, 4, 10, 4).
crosses(154, 5, 11, 5).
crosses(154, 6, 12, 6).
crosses(154, 7, 13, 5).
crosses(154, 0, 7, 4).
crosses(154, 2, 9, 1).
crosses(154, 3, 10, 3).
crosses(154, 4, 11, 4).
crosses(154, 5, 12, 6).
crosses(154, 6, 13, 4).
crosses(154, 7, 14, 5).
crosses(154, 1, 9, 1).
crosses(154, 2, 10, 2).
crosses(154, 3, 11, 3).
crosses(154, 4, 12, 5).
crosses(154, 5, 13, 4).
crosses(154, 6, 14, 4).
crosses(154, 1, 10, 2).
crosses(154, 2, 11, 2).
crosses(154, 3, 12, 4).
crosses(154, 4, 13, 3).
crosses(154, 5, 14, 4).
crosses(154, 0, 10, 1).
crosses(154, 1, 11, 2).
crosses(154, 2, 12, 3).
crosses(154, 3, 13, 2).
crosses(154, 4, 14, 3).
crosses(154, 0, 11, 1).
crosses(154, 1, 12, 3).
crosses(154, 2, 13, 1).
crosses(154, 3, 14, 2).
crosses(154, 0, 12, 2).
crosses(154, 1, 13, 1).
crosses(154, 2, 14, 1).
crosses(154, 1, 14, 1).
crosses(155, 1, 3, 1).
crosses(155, 3, 5, 2).
crosses(155, 4, 6, 1).
crosses(155, 6, 8, 1).
crosses(155, 8, 10, 2).
crosses(155, 0, 3, 1).
crosses(155, 2, 5, 1).
crosses(155, 3, 6, 3).
crosses(155, 4, 7, 1).
crosses(155, 5, 8, 1).
crosses(155, 7, 10, 1).
crosses(155, 1, 5, 1).
crosses(155, 2, 6, 2).
crosses(155, 3, 7, 3).
crosses(155, 4, 8, 2).
crosses(155, 6, 10, 1).
crosses(155, 1, 6, 2).
crosses(155, 2, 7, 2).
crosses(155, 3, 8, 4).
crosses(155, 0, 6, 1).
crosses(155, 1, 7, 2).
crosses(155, 2, 8, 3).
crosses(155, 3, 9, 2).
crosses(155, 0, 7, 1).
crosses(155, 1, 8, 3).
crosses(155, 2, 9, 1).
crosses(155, 3, 10, 2).
crosses(155, 0, 8, 2).
crosses(155, 1, 9, 1).
crosses(155, 2, 10, 1).
crosses(155, 1, 10, 1).
crosses(156, 2, 4, 1).
crosses(156, 4, 6, 2).
crosses(156, 5, 7, 1).
crosses(156, 6, 8, 1).
crosses(156, 8, 10, 2).
crosses(156, 1, 4, 1).
crosses(156, 3, 6, 1).
crosses(156, 4, 7, 3).
crosses(156, 5, 8, 2).
crosses(156, 7, 10, 1).
crosses(156, 0, 4, 1).
crosses(156, 2, 6, 1).
crosses(156, 3, 7, 2).
crosses(156, 4, 8, 4).
crosses(156, 1, 6, 1).
crosses(156, 2, 7, 2).
crosses(156, 3, 8, 3).
crosses(156, 4, 9, 2).
crosses(156, 1, 7, 2).
crosses(156, 2, 8, 3).
crosses(156, 3, 9, 1).
crosses(156, 4, 10, 2).
crosses(156, 0, 7, 1).
crosses(156, 1, 8, 3).
crosses(156, 2, 9, 1).
crosses(156, 3, 10, 1).
crosses(156, 0, 8, 2).
crosses(156, 1, 9, 1).
crosses(156, 2, 10, 1).
crosses(156, 1, 10, 1).
crosses(157, 1, 3, 1).
crosses(157, 3, 5, 1).
crosses(157, 5, 7, 2).
crosses(157, 6, 8, 1).
crosses(157, 7, 9, 1).
crosses(157, 9, 11, 2).
crosses(157, 0, 3, 1).
crosses(157, 1, 4, 1).
crosses(157, 2, 5, 1).
crosses(157, 4, 7, 1).
crosses(157, 5, 8, 3).
crosses(157, 6, 9, 2).
crosses(157, 8, 11, 1).
crosses(157, 0, 4, 1).
crosses(157, 1, 5, 2).
crosses(157, 3, 7, 1).
crosses(157, 4, 8, 2).
crosses(157, 5, 9, 4).
crosses(157, 0, 5, 2).
crosses(157, 3, 8, 2).
crosses(157, 4, 9, 3).
crosses(157, 5, 10, 2).
crosses(157, 2, 8, 1).
crosses(157, 3, 9, 3).
crosses(157, 4, 10, 1).
crosses(157, 5, 11, 2).
crosses(157, 1, 8, 1).
crosses(157, 2, 9, 2).
crosses(157, 3, 10, 1).
crosses(157, 4, 11, 1).
crosses(157, 0, 8, 1).
crosses(157, 1, 9, 2).
crosses(157, 3, 11, 1).
crosses(157, 0, 9, 2).
crosses(158, 0, 2, 1).
crosses(158, 1, 3, 1).
crosses(158, 3, 5, 1).
crosses(158, 5, 7, 4).
crosses(158, 6, 8, 1).
crosses(158, 7, 9, 1).
crosses(158, 8, 10, 1).
crosses(158, 9, 11, 1).
crosses(158, 10, 12, 1).
crosses(158, 12, 14, 5).
crosses(158, 0, 3, 2).
crosses(158, 1, 4, 1).
crosses(158, 2, 5, 1).
crosses(158, 4, 7, 3).
crosses(158, 5, 8, 5).
crosses(158, 6, 9, 2).
crosses(158, 7, 10, 2).
crosses(158, 8, 11, 2).
crosses(158, 9, 12, 2).
crosses(158, 11, 14, 4).
crosses(158, 0, 4, 2).
crosses(158, 1, 5, 2).
crosses(158, 3, 7, 3).
crosses(158, 4, 8, 4).
crosses(158, 5, 9, 6).
crosses(158, 6, 10, 3).
crosses(158, 7, 11, 3).
crosses(158, 8, 12, 3).
crosses(158, 10, 14, 3).
crosses(158, 0, 5, 3).
crosses(158, 2, 7, 2).
crosses(158, 3, 8, 4).
crosses(158, 4, 9, 5).
crosses(158, 5, 10, 7).
crosses(158, 6, 11, 4).
crosses(158, 7, 12, 4).
crosses(158, 9, 14, 2).
crosses(158, 1, 7, 1).
crosses(158, 2, 8, 3).
crosses(158, 3, 9, 5).
crosses(158, 4, 10, 6).
crosses(158, 5, 11, 8).
crosses(158, 6, 12, 5).
crosses(158, 8, 14, 1).
crosses(158, 1, 8, 2).
crosses(158, 2, 9, 4).
crosses(158, 3, 10, 6).
crosses(158, 4, 11, 7).
crosses(158, 5, 12, 9).
crosses(158, 0, 8, 1).
crosses(158, 1, 9, 3).
crosses(158, 2, 10, 5).
crosses(158, 3, 11, 7).
crosses(158, 4, 12, 8).
crosses(158, 5, 13, 4).
crosses(158, 0, 9, 2).
crosses(158, 1, 10, 4).
crosses(158, 2, 11, 6).
crosses(158, 3, 12, 8).
crosses(158, 4, 13, 3).
crosses(158, 5, 14, 4).
crosses(158, 0, 10, 3).
crosses(158, 1, 11, 5).
crosses(158, 2, 12, 7).
crosses(158, 3, 13, 3).
crosses(158, 4, 14, 3).
crosses(158, 0, 11, 4).
crosses(158, 1, 12, 6).
crosses(158, 2, 13, 2).
crosses(158, 3, 14, 3).
crosses(158, 0, 12, 5).
crosses(158, 1, 13, 1).
crosses(158, 2, 14, 2).
crosses(158, 1, 14, 1).
crosses(159, 0, 2, 1).
crosses(159, 1, 3, 1).
crosses(159, 2, 4, 1).
crosses(159, 4, 6, 4).
crosses(159, 5, 7, 1).
crosses(159, 6, 8, 1).
crosses(159, 7, 9, 1).
crosses(159, 8, 10, 1).
crosses(159, 9, 11, 1).
crosses(159, 11, 13, 5).
crosses(159, 0, 3, 2).
crosses(159, 1, 4, 2).
crosses(159, 3, 6, 3).
crosses(159, 4, 7, 5).
crosses(159, 5, 8, 2).
crosses(159, 6, 9, 2).
crosses(159, 7, 10, 2).
crosses(159, 8, 11, 2).
crosses(159, 10, 13, 4).
crosses(159, 0, 4, 3).
crosses(159, 2, 6, 2).
crosses(159, 3, 7, 4).
crosses(159, 4, 8, 6).
crosses(159, 5, 9, 3).
crosses(159, 6, 10, 3).
crosses(159, 7, 11, 3).
crosses(159, 9, 13, 3).
crosses(159, 1, 6, 1).
crosses(159, 2, 7, 3).
crosses(159, 3, 8, 5).
crosses(159, 4, 9, 7).
crosses(159, 5, 10, 4).
crosses(159, 6, 11, 4).
crosses(159, 8, 13, 2).
crosses(159, 1, 7, 2).
crosses(159, 2, 8, 4).
crosses(159, 3, 9, 6).
crosses(159, 4, 10, 8).
crosses(159, 5, 11, 5).
crosses(159, 7, 13, 1).
crosses(159, 0, 7, 1).
crosses(159, 1, 8, 3).
crosses(159, 2, 9, 5).
crosses(159, 3, 10, 7).
crosses(159, 4, 11, 9).
crosses(159, 0, 8, 2).
crosses(159, 1, 9, 4).
crosses(159, 2, 10, 6).
crosses(159, 3, 11, 8).
crosses(159, 4, 12, 4).
crosses(159, 0, 9, 3).
crosses(159, 1, 10, 5).
crosses(159, 2, 11, 7).
crosses(159, 3, 12, 3).
crosses(159, 4, 13, 4).
crosses(159, 0, 10, 4).
crosses(159, 1, 11, 6).
crosses(159, 2, 12, 2).
crosses(159, 3, 13, 3).
crosses(159, 0, 11, 5).
crosses(159, 1, 12, 1).
crosses(159, 2, 13, 2).
crosses(159, 1, 13, 1).
crosses(160, 3, 5, 1).
crosses(160, 4, 6, 1).
crosses(160, 5, 7, 1).
crosses(160, 7, 9, 2).
crosses(160, 2, 5, 1).
crosses(160, 3, 6, 2).
crosses(160, 4, 7, 2).
crosses(160, 6, 9, 1).
crosses(160, 1, 5, 1).
crosses(160, 2, 6, 2).
crosses(160, 3, 7, 3).
crosses(160, 1, 6, 2).
crosses(160, 2, 7, 3).
crosses(160, 3, 8, 1).
crosses(160, 0, 6, 1).
crosses(160, 1, 7, 3).
crosses(160, 2, 8, 1).
crosses(160, 3, 9, 1).
crosses(160, 0, 7, 2).
crosses(160, 1, 8, 1).
crosses(160, 2, 9, 1).
crosses(160, 1, 9, 1).
crosses(161, 2, 4, 1).
crosses(161, 3, 5, 1).
crosses(161, 4, 6, 1).
crosses(161, 6, 8, 2).
crosses(161, 1, 4, 1).
crosses(161, 2, 5, 2).
crosses(161, 3, 6, 2).
crosses(161, 5, 8, 1).
crosses(161, 1, 5, 2).
crosses(161, 2, 6, 3).
crosses(161, 0, 5, 1).
crosses(161, 1, 6, 3).
crosses(161, 2, 7, 1).
crosses(161, 0, 6, 2).
crosses(161, 1, 7, 1).
crosses(161, 2, 8, 1).
crosses(161, 1, 8, 1).
crosses(162, 0, 2, 1).
crosses(162, 1, 3, 1).
crosses(162, 4, 6, 4).
crosses(162, 6, 8, 2).
crosses(162, 9, 11, 2).
crosses(162, 10, 12, 1).
crosses(162, 12, 14, 2).
crosses(162, 0, 3, 2).
crosses(162, 1, 4, 1).
crosses(162, 3, 6, 4).
crosses(162, 4, 7, 3).
crosses(162, 5, 8, 1).
crosses(162, 6, 9, 2).
crosses(162, 8, 11, 2).
crosses(162, 9, 12, 3).
crosses(162, 11, 14, 1).
crosses(162, 0, 4, 2).
crosses(162, 2, 6, 3).
crosses(162, 3, 7, 3).
crosses(162, 4, 8, 4).
crosses(162, 5, 9, 1).
crosses(162, 6, 10, 1).
crosses(162, 7, 11, 1).
crosses(162, 8, 12, 3).
crosses(162, 9, 13, 1).
crosses(162, 1, 6, 2).
crosses(162, 2, 7, 2).
crosses(162, 3, 8, 4).
crosses(162, 4, 9, 4).
crosses(162, 6, 11, 2).
crosses(162, 7, 12, 2).
crosses(162, 8, 13, 1).
crosses(162, 9, 14, 1).
crosses(162, 0, 6, 1).
crosses(162, 1, 7, 1).
crosses(162, 2, 8, 3).
crosses(162, 3, 9, 4).
crosses(162, 4, 10, 3).
crosses(162, 5, 11, 1).
crosses(162, 6, 12, 3).
crosses(162, 8, 14, 1).
crosses(162, 1, 8, 2).
crosses(162, 2, 9, 3).
crosses(162, 3, 10, 3).
crosses(162, 4, 11, 4).
crosses(162, 5, 12, 2).
crosses(162, 6, 13, 1).
crosses(162, 0, 8, 1).
crosses(162, 1, 9, 2).
crosses(162, 2, 10, 2).
crosses(162, 3, 11, 4).
crosses(162, 4, 12, 5).
crosses(162, 6, 14, 1).
crosses(162, 0, 9, 1).
crosses(162, 1, 10, 1).
crosses(162, 2, 11, 3).
crosses(162, 3, 12, 5).
crosses(162, 4, 13, 3).
crosses(162, 1, 11, 2).
crosses(162, 2, 12, 4).
crosses(162, 3, 13, 3).
crosses(162, 4, 14, 3).
crosses(162, 0, 11, 1).
crosses(162, 1, 12, 3).
crosses(162, 2, 13, 2).
crosses(162, 3, 14, 3).
crosses(162, 0, 12, 2).
crosses(162, 1, 13, 1).
crosses(162, 2, 14, 2).
crosses(162, 1, 14, 1).
crosses(163, 2, 4, 1).
crosses(163, 3, 5, 1).
crosses(163, 6, 8, 2).
crosses(163, 7, 9, 1).
crosses(163, 9, 11, 2).
crosses(163, 1, 4, 1).
crosses(163, 2, 5, 2).
crosses(163, 3, 6, 1).
crosses(163, 5, 8, 2).
crosses(163, 6, 9, 3).
crosses(163, 8, 11, 1).
crosses(163, 1, 5, 2).
crosses(163, 2, 6, 2).
crosses(163, 4, 8, 1).
crosses(163, 5, 9, 3).
crosses(163, 6, 10, 1).
crosses(163, 0, 5, 1).
crosses(163, 1, 6, 2).
crosses(163, 2, 7, 1).
crosses(163, 3, 8, 1).
crosses(163, 4, 9, 2).
crosses(163, 5, 10, 1).
crosses(163, 6, 11, 1).
crosses(163, 0, 6, 1).
crosses(163, 1, 7, 1).
crosses(163, 2, 8, 2).
crosses(163, 3, 9, 2).
crosses(163, 5, 11, 1).
crosses(163, 1, 8, 2).
crosses(163, 2, 9, 3).
crosses(163, 0, 8, 1).
crosses(163, 1, 9, 3).
crosses(163, 2, 10, 1).
crosses(163, 0, 9, 2).
crosses(163, 1, 10, 1).
crosses(163, 2, 11, 1).
crosses(163, 1, 11, 1).
crosses(164, 1, 3, 1).
crosses(164, 4, 6, 1).
crosses(164, 7, 9, 1).
crosses(164, 8, 10, 1).
crosses(164, 10, 12, 2).
crosses(164, 0, 3, 1).
crosses(164, 1, 4, 1).
crosses(164, 3, 6, 1).
crosses(164, 4, 7, 1).
crosses(164, 6, 9, 1).
crosses(164, 7, 10, 2).
crosses(164, 9, 12, 1).
crosses(164, 0, 4, 1).
crosses(164, 3, 7, 1).
crosses(164, 4, 8, 1).
crosses(164, 5, 9, 1).
crosses(164, 6, 10, 2).
crosses(164, 3, 8, 1).
crosses(164, 4, 9, 2).
crosses(164, 5, 10, 2).
crosses(164, 3, 9, 2).
crosses(164, 4, 10, 3).
crosses(164, 2, 9, 1).
crosses(164, 3, 10, 3).
crosses(164, 4, 11, 1).
crosses(164, 1, 9, 1).
crosses(164, 2, 10, 2).
crosses(164, 3, 11, 1).
crosses(164, 4, 12, 1).
crosses(164, 0, 9, 1).
crosses(164, 1, 10, 2).
crosses(164, 3, 12, 1).
crosses(164, 0, 10, 2).
crosses(165, 1, 3, 1).
crosses(165, 3, 5, 1).
crosses(165, 6, 8, 1).
crosses(165, 7, 9, 1).
crosses(165, 9, 11, 2).
crosses(165, 0, 3, 1).
crosses(165, 3, 6, 1).
crosses(165, 5, 8, 1).
crosses(165, 6, 9, 2).
crosses(165, 8, 11, 1).
crosses(165, 3, 7, 1).
crosses(165, 4, 8, 1).
crosses(165, 5, 9, 2).
crosses(165, 3, 8, 2).
crosses(165, 4, 9, 2).
crosses(165, 2, 8, 1).
crosses(165, 3, 9, 3).
crosses(165, 1, 8, 1).
crosses(165, 2, 9, 2).
crosses(165, 3, 10, 1).
crosses(165, 0, 8, 1).
crosses(165, 1, 9, 2).
crosses(165, 3, 11, 1).
crosses(165, 0, 9, 2).
crosses(166, 1, 3, 1).
crosses(166, 4, 6, 1).
crosses(166, 7, 9, 1).
crosses(166, 8, 10, 1).
crosses(166, 10, 12, 2).
crosses(166, 0, 3, 1).
crosses(166, 1, 4, 1).
crosses(166, 3, 6, 1).
crosses(166, 4, 7, 1).
crosses(166, 6, 9, 1).
crosses(166, 7, 10, 2).
crosses(166, 9, 12, 1).
crosses(166, 0, 4, 1).
crosses(166, 3, 7, 1).
crosses(166, 4, 8, 1).
crosses(166, 5, 9, 1).
crosses(166, 6, 10, 2).
crosses(166, 3, 8, 1).
crosses(166, 4, 9, 2).
crosses(166, 5, 10, 2).
crosses(166, 3, 9, 2).
crosses(166, 4, 10, 3).
crosses(166, 2, 9, 1).
crosses(166, 3, 10, 3).
crosses(166, 4, 11, 1).
crosses(166, 1, 9, 1).
crosses(166, 2, 10, 2).
crosses(166, 3, 11, 1).
crosses(166, 4, 12, 1).
crosses(166, 0, 9, 1).
crosses(166, 1, 10, 2).
crosses(166, 3, 12, 1).
crosses(166, 0, 10, 2).
crosses(167, 3, 5, 1).
crosses(167, 6, 8, 1).
crosses(167, 7, 9, 1).
crosses(167, 9, 11, 2).
crosses(167, 2, 5, 1).
crosses(167, 3, 6, 1).
crosses(167, 5, 8, 1).
crosses(167, 6, 9, 2).
crosses(167, 8, 11, 1).
crosses(167, 1, 5, 1).
crosses(167, 2, 6, 1).
crosses(167, 3, 7, 1).
crosses(167, 4, 8, 1).
crosses(167, 5, 9, 2).
crosses(167, 1, 6, 1).
crosses(167, 2, 7, 1).
crosses(167, 3, 8, 2).
crosses(167, 4, 9, 2).
crosses(167, 1, 7, 1).
crosses(167, 2, 8, 2).
crosses(167, 3, 9, 3).
crosses(167, 1, 8, 2).
crosses(167, 2, 9, 3).
crosses(167, 3, 10, 1).
crosses(167, 0, 8, 1).
crosses(167, 1, 9, 3).
crosses(167, 2, 10, 1).
crosses(167, 3, 11, 1).
crosses(167, 0, 9, 2).
crosses(167, 1, 10, 1).
crosses(167, 2, 11, 1).
crosses(167, 1, 11, 1).
crosses(168, 1, 3, 2).
crosses(168, 3, 5, 2).
crosses(168, 6, 8, 1).
crosses(168, 0, 3, 1).
crosses(168, 1, 4, 1).
crosses(168, 2, 5, 1).
crosses(168, 3, 6, 2).
crosses(168, 5, 8, 1).
crosses(168, 1, 5, 2).
crosses(168, 2, 6, 1).
crosses(168, 3, 7, 1).
crosses(168, 0, 5, 1).
crosses(168, 1, 6, 2).
crosses(168, 3, 8, 1).
crosses(168, 0, 6, 1).
crosses(168, 1, 7, 1).
crosses(168, 1, 8, 1).
crosses(169, 1, 3, 1).
crosses(169, 2, 4, 1).
crosses(169, 4, 6, 1).
crosses(169, 1, 4, 2).
crosses(169, 0, 4, 1).
crosses(169, 1, 5, 1).
crosses(169, 1, 6, 1).
crosses(170, 1, 3, 2).
crosses(170, 3, 5, 2).
crosses(170, 6, 8, 1).
crosses(170, 0, 3, 1).
crosses(170, 1, 4, 1).
crosses(170, 2, 5, 1).
crosses(170, 3, 6, 2).
crosses(170, 5, 8, 1).
crosses(170, 1, 5, 2).
crosses(170, 2, 6, 1).
crosses(170, 3, 7, 1).
crosses(170, 0, 5, 1).
crosses(170, 1, 6, 2).
crosses(170, 3, 8, 1).
crosses(170, 0, 6, 1).
crosses(170, 1, 7, 1).
crosses(170, 1, 8, 1).
crosses(171, 1, 3, 1).
crosses(171, 3, 5, 3).
crosses(171, 5, 7, 2).
crosses(171, 7, 9, 1).
crosses(171, 9, 11, 2).
crosses(171, 0, 3, 1).
crosses(171, 2, 5, 2).
crosses(171, 3, 6, 2).
crosses(171, 4, 7, 1).
crosses(171, 5, 8, 2).
crosses(171, 6, 9, 1).
crosses(171, 8, 11, 1).
crosses(171, 1, 5, 2).
crosses(171, 2, 6, 1).
crosses(171, 3, 7, 3).
crosses(171, 4, 8, 1).
crosses(171, 5, 9, 3).
crosses(171, 7, 11, 1).
crosses(171, 0, 5, 1).
crosses(171, 1, 6, 1).
crosses(171, 2, 7, 2).
crosses(171, 3, 8, 3).
crosses(171, 4, 9, 2).
crosses(171, 5, 10, 1).
crosses(171, 1, 7, 2).
crosses(171, 2, 8, 2).
crosses(171, 3, 9, 4).
crosses(171, 5, 11, 1).
crosses(171, 0, 7, 1).
crosses(171, 1, 8, 2).
crosses(171, 2, 9, 3).
crosses(171, 3, 10, 2).
crosses(171, 0, 8, 1).
crosses(171, 1, 9, 3).
crosses(171, 2, 10, 1).
crosses(171, 3, 11, 2).
crosses(171, 0, 9, 2).
crosses(171, 1, 10, 1).
crosses(171, 2, 11, 1).
crosses(171, 1, 11, 1).
crosses(172, 1, 3, 1).
crosses(172, 3, 5, 2).
crosses(172, 4, 6, 1).
crosses(172, 6, 8, 1).
crosses(172, 7, 9, 1).
crosses(172, 9, 11, 3).
crosses(172, 0, 3, 1).
crosses(172, 2, 5, 1).
crosses(172, 3, 6, 3).
crosses(172, 4, 7, 1).
crosses(172, 5, 8, 1).
crosses(172, 6, 9, 2).
crosses(172, 8, 11, 2).
crosses(172, 1, 5, 1).
crosses(172, 2, 6, 2).
crosses(172, 3, 7, 3).
crosses(172, 4, 8, 2).
crosses(172, 5, 9, 2).
crosses(172, 7, 11, 1).
crosses(172, 1, 6, 2).
crosses(172, 2, 7, 2).
crosses(172, 3, 8, 4).
crosses(172, 4, 9, 3).
crosses(172, 6, 11, 1).
crosses(172, 0, 6, 1).
crosses(172, 1, 7, 2).
crosses(172, 2, 8, 3).
crosses(172, 3, 9, 5).
crosses(172, 0, 7, 1).
crosses(172, 1, 8, 3).
crosses(172, 2, 9, 4).
crosses(172, 3, 10, 2).
crosses(172, 0, 8, 2).
crosses(172, 1, 9, 4).
crosses(172, 2, 10, 1).
crosses(172, 3, 11, 2).
crosses(172, 0, 9, 3).
crosses(172, 1, 10, 1).
crosses(172, 2, 11, 1).
crosses(172, 1, 11, 1).
crosses(173, 1, 3, 1).
crosses(173, 3, 5, 2).
crosses(173, 4, 6, 1).
crosses(173, 6, 8, 1).
crosses(173, 8, 10, 2).
crosses(173, 0, 3, 1).
crosses(173, 2, 5, 1).
crosses(173, 3, 6, 3).
crosses(173, 4, 7, 1).
crosses(173, 5, 8, 1).
crosses(173, 7, 10, 1).
crosses(173, 1, 5, 1).
crosses(173, 2, 6, 2).
crosses(173, 3, 7, 3).
crosses(173, 4, 8, 2).
crosses(173, 6, 10, 1).
crosses(173, 1, 6, 2).
crosses(173, 2, 7, 2).
crosses(173, 3, 8, 4).
crosses(173, 0, 6, 1).
crosses(173, 1, 7, 2).
crosses(173, 2, 8, 3).
crosses(173, 3, 9, 2).
crosses(173, 0, 7, 1).
crosses(173, 1, 8, 3).
crosses(173, 2, 9, 1).
crosses(173, 3, 10, 2).
crosses(173, 0, 8, 2).
crosses(173, 1, 9, 1).
crosses(173, 2, 10, 1).
crosses(173, 1, 10, 1).
crosses(174, 2, 4, 1).
crosses(174, 4, 6, 3).
crosses(174, 6, 8, 2).
crosses(174, 8, 10, 1).
crosses(174, 10, 12, 2).
crosses(174, 1, 4, 1).
crosses(174, 3, 6, 2).
crosses(174, 4, 7, 2).
crosses(174, 5, 8, 1).
crosses(174, 6, 9, 2).
crosses(174, 7, 10, 1).
crosses(174, 9, 12, 1).
crosses(174, 0, 4, 1).
crosses(174, 2, 6, 2).
crosses(174, 3, 7, 1).
crosses(174, 4, 8, 3).
crosses(174, 5, 9, 1).
crosses(174, 6, 10, 3).
crosses(174, 8, 12, 1).
crosses(174, 1, 6, 2).
crosses(174, 2, 7, 1).
crosses(174, 3, 8, 2).
crosses(174, 4, 9, 3).
crosses(174, 5, 10, 2).
crosses(174, 6, 11, 1).
crosses(174, 0, 6, 1).
crosses(174, 1, 7, 1).
crosses(174, 2, 8, 2).
crosses(174, 3, 9, 2).
crosses(174, 4, 10, 4).
crosses(174, 6, 12, 1).
crosses(174, 1, 8, 2).
crosses(174, 2, 9, 2).
crosses(174, 3, 10, 3).
crosses(174, 4, 11, 2).
crosses(174, 0, 8, 1).
crosses(174, 1, 9, 2).
crosses(174, 2, 10, 3).
crosses(174, 3, 11, 1).
crosses(174, 4, 12, 2).
crosses(174, 0, 9, 1).
crosses(174, 1, 10, 3).
crosses(174, 2, 11, 1).
crosses(174, 3, 12, 1).
crosses(174, 0, 10, 2).
crosses(174, 1, 11, 1).
crosses(174, 2, 12, 1).
crosses(174, 1, 12, 1).
crosses(175, 1, 3, 1).
crosses(175, 3, 5, 2).
crosses(175, 4, 6, 1).
crosses(175, 6, 8, 1).
crosses(175, 7, 9, 1).
crosses(175, 9, 11, 3).
crosses(175, 0, 3, 1).
crosses(175, 2, 5, 1).
crosses(175, 3, 6, 3).
crosses(175, 4, 7, 1).
crosses(175, 5, 8, 1).
crosses(175, 6, 9, 2).
crosses(175, 8, 11, 2).
crosses(175, 1, 5, 1).
crosses(175, 2, 6, 2).
crosses(175, 3, 7, 3).
crosses(175, 4, 8, 2).
crosses(175, 5, 9, 2).
crosses(175, 7, 11, 1).
crosses(175, 1, 6, 2).
crosses(175, 2, 7, 2).
crosses(175, 3, 8, 4).
crosses(175, 4, 9, 3).
crosses(175, 6, 11, 1).
crosses(175, 0, 6, 1).
crosses(175, 1, 7, 2).
crosses(175, 2, 8, 3).
crosses(175, 3, 9, 5).
crosses(175, 0, 7, 1).
crosses(175, 1, 8, 3).
crosses(175, 2, 9, 4).
crosses(175, 3, 10, 2).
crosses(175, 0, 8, 2).
crosses(175, 1, 9, 4).
crosses(175, 2, 10, 1).
crosses(175, 3, 11, 2).
crosses(175, 0, 9, 3).
crosses(175, 1, 10, 1).
crosses(175, 2, 11, 1).
crosses(175, 1, 11, 1).
crosses(176, 2, 4, 1).
crosses(176, 4, 6, 2).
crosses(176, 5, 7, 1).
crosses(176, 7, 9, 1).
crosses(176, 9, 11, 2).
crosses(176, 1, 4, 1).
crosses(176, 3, 6, 1).
crosses(176, 4, 7, 3).
crosses(176, 5, 8, 1).
crosses(176, 6, 9, 1).
crosses(176, 8, 11, 1).
crosses(176, 0, 4, 1).
crosses(176, 2, 6, 1).
crosses(176, 3, 7, 2).
crosses(176, 4, 8, 3).
crosses(176, 5, 9, 2).
crosses(176, 7, 11, 1).
crosses(176, 1, 6, 1).
crosses(176, 2, 7, 2).
crosses(176, 3, 8, 2).
crosses(176, 4, 9, 4).
crosses(176, 1, 7, 2).
crosses(176, 2, 8, 2).
crosses(176, 3, 9, 3).
crosses(176, 4, 10, 2).
crosses(176, 0, 7, 1).
crosses(176, 1, 8, 2).
crosses(176, 2, 9, 3).
crosses(176, 3, 10, 1).
crosses(176, 4, 11, 2).
crosses(176, 0, 8, 1).
crosses(176, 1, 9, 3).
crosses(176, 2, 10, 1).
crosses(176, 3, 11, 1).
crosses(176, 0, 9, 2).
crosses(176, 1, 10, 1).
crosses(176, 2, 11, 1).
crosses(176, 1, 11, 1).
crosses(177, 1, 3, 1).
crosses(177, 3, 5, 3).
crosses(177, 5, 7, 2).
crosses(177, 7, 9, 1).
crosses(177, 9, 11, 2).
crosses(177, 0, 3, 1).
crosses(177, 2, 5, 2).
crosses(177, 3, 6, 2).
crosses(177, 4, 7, 1).
crosses(177, 5, 8, 2).
crosses(177, 6, 9, 1).
crosses(177, 8, 11, 1).
crosses(177, 1, 5, 2).
crosses(177, 2, 6, 1).
crosses(177, 3, 7, 3).
crosses(177, 4, 8, 1).
crosses(177, 5, 9, 3).
crosses(177, 7, 11, 1).
crosses(177, 0, 5, 1).
crosses(177, 1, 6, 1).
crosses(177, 2, 7, 2).
crosses(177, 3, 8, 3).
crosses(177, 4, 9, 2).
crosses(177, 5, 10, 1).
crosses(177, 1, 7, 2).
crosses(177, 2, 8, 2).
crosses(177, 3, 9, 4).
crosses(177, 5, 11, 1).
crosses(177, 0, 7, 1).
crosses(177, 1, 8, 2).
crosses(177, 2, 9, 3).
crosses(177, 3, 10, 2).
crosses(177, 0, 8, 1).
crosses(177, 1, 9, 3).
crosses(177, 2, 10, 1).
crosses(177, 3, 11, 2).
crosses(177, 0, 9, 2).
crosses(177, 1, 10, 1).
crosses(177, 2, 11, 1).
crosses(177, 1, 11, 1).
crosses(178, 1, 3, 1).
crosses(178, 3, 5, 2).
crosses(178, 4, 6, 1).
crosses(178, 6, 8, 1).
crosses(178, 7, 9, 1).
crosses(178, 9, 11, 3).
crosses(178, 0, 3, 1).
crosses(178, 2, 5, 1).
crosses(178, 3, 6, 3).
crosses(178, 4, 7, 1).
crosses(178, 5, 8, 1).
crosses(178, 6, 9, 2).
crosses(178, 8, 11, 2).
crosses(178, 1, 5, 1).
crosses(178, 2, 6, 2).
crosses(178, 3, 7, 3).
crosses(178, 4, 8, 2).
crosses(178, 5, 9, 2).
crosses(178, 7, 11, 1).
crosses(178, 1, 6, 2).
crosses(178, 2, 7, 2).
crosses(178, 3, 8, 4).
crosses(178, 4, 9, 3).
crosses(178, 6, 11, 1).
crosses(178, 0, 6, 1).
crosses(178, 1, 7, 2).
crosses(178, 2, 8, 3).
crosses(178, 3, 9, 5).
crosses(178, 0, 7, 1).
crosses(178, 1, 8, 3).
crosses(178, 2, 9, 4).
crosses(178, 3, 10, 2).
crosses(178, 0, 8, 2).
crosses(178, 1, 9, 4).
crosses(178, 2, 10, 1).
crosses(178, 3, 11, 2).
crosses(178, 0, 9, 3).
crosses(178, 1, 10, 1).
crosses(178, 2, 11, 1).
crosses(178, 1, 11, 1).
crosses(179, 1, 3, 1).
crosses(179, 3, 5, 2).
crosses(179, 4, 6, 1).
crosses(179, 6, 8, 1).
crosses(179, 8, 10, 2).
crosses(179, 0, 3, 1).
crosses(179, 2, 5, 1).
crosses(179, 3, 6, 3).
crosses(179, 4, 7, 1).
crosses(179, 5, 8, 1).
crosses(179, 7, 10, 1).
crosses(179, 1, 5, 1).
crosses(179, 2, 6, 2).
crosses(179, 3, 7, 3).
crosses(179, 4, 8, 2).
crosses(179, 6, 10, 1).
crosses(179, 1, 6, 2).
crosses(179, 2, 7, 2).
crosses(179, 3, 8, 4).
crosses(179, 0, 6, 1).
crosses(179, 1, 7, 2).
crosses(179, 2, 8, 3).
crosses(179, 3, 9, 2).
crosses(179, 0, 7, 1).
crosses(179, 1, 8, 3).
crosses(179, 2, 9, 1).
crosses(179, 3, 10, 2).
crosses(179, 0, 8, 2).
crosses(179, 1, 9, 1).
crosses(179, 2, 10, 1).
crosses(179, 1, 10, 1).
crosses(180, 1, 3, 1).
crosses(180, 2, 4, 1).
crosses(180, 4, 6, 3).
crosses(180, 5, 7, 1).
crosses(180, 7, 9, 1).
crosses(180, 9, 11, 2).
crosses(180, 0, 3, 1).
crosses(180, 1, 4, 2).
crosses(180, 3, 6, 2).
crosses(180, 4, 7, 4).
crosses(180, 5, 8, 1).
crosses(180, 6, 9, 1).
crosses(180, 8, 11, 1).
crosses(180, 0, 4, 2).
crosses(180, 2, 6, 1).
crosses(180, 3, 7, 3).
crosses(180, 4, 8, 4).
crosses(180, 5, 9, 2).
crosses(180, 7, 11, 1).
crosses(180, 1, 6, 1).
crosses(180, 2, 7, 2).
crosses(180, 3, 8, 3).
crosses(180, 4, 9, 5).
crosses(180, 1, 7, 2).
crosses(180, 2, 8, 2).
crosses(180, 3, 9, 4).
crosses(180, 4, 10, 3).
crosses(180, 0, 7, 1).
crosses(180, 1, 8, 2).
crosses(180, 2, 9, 3).
crosses(180, 3, 10, 2).
crosses(180, 4, 11, 3).
crosses(180, 0, 8, 1).
crosses(180, 1, 9, 3).
crosses(180, 2, 10, 1).
crosses(180, 3, 11, 2).
crosses(180, 0, 9, 2).
crosses(180, 1, 10, 1).
crosses(180, 2, 11, 1).
crosses(180, 1, 11, 1).
crosses(181, 2, 4, 1).
crosses(181, 3, 5, 1).
crosses(181, 6, 8, 3).
crosses(181, 7, 9, 1).
crosses(181, 9, 11, 2).
crosses(181, 12, 14, 1).
crosses(181, 1, 4, 1).
crosses(181, 2, 5, 2).
crosses(181, 3, 6, 1).
crosses(181, 5, 8, 3).
crosses(181, 6, 9, 4).
crosses(181, 8, 11, 1).
crosses(181, 9, 12, 2).
crosses(181, 11, 14, 1).
crosses(181, 0, 4, 1).
crosses(181, 1, 5, 2).
crosses(181, 2, 6, 2).
crosses(181, 4, 8, 2).
crosses(181, 5, 9, 4).
crosses(181, 6, 10, 3).
crosses(181, 7, 11, 1).
crosses(181, 8, 12, 1).
crosses(181, 9, 13, 1).
crosses(181, 0, 5, 2).
crosses(181, 1, 6, 2).
crosses(181, 3, 8, 1).
crosses(181, 4, 9, 3).
crosses(181, 5, 10, 3).
crosses(181, 6, 11, 4).
crosses(181, 7, 12, 1).
crosses(181, 9, 14, 1).
crosses(181, 0, 6, 2).
crosses(181, 2, 8, 1).
crosses(181, 3, 9, 2).
crosses(181, 4, 10, 2).
crosses(181, 5, 11, 4).
crosses(181, 6, 12, 4).
crosses(181, 1, 8, 1).
crosses(181, 2, 9, 2).
crosses(181, 3, 10, 1).
crosses(181, 4, 11, 3).
crosses(181, 5, 12, 4).
crosses(181, 6, 13, 3).
crosses(181, 1, 9, 2).
crosses(181, 2, 10, 1).
crosses(181, 3, 11, 2).
crosses(181, 4, 12, 3).
crosses(181, 5, 13, 3).
crosses(181, 6, 14, 3).
crosses(181, 0, 9, 1).
crosses(181, 1, 10, 1).
crosses(181, 2, 11, 2).
crosses(181, 3, 12, 2).
crosses(181, 4, 13, 2).
crosses(181, 5, 14, 3).
crosses(181, 1, 11, 2).
crosses(181, 2, 12, 2).
crosses(181, 3, 13, 1).
crosses(181, 4, 14, 2).
crosses(181, 0, 11, 1).
crosses(181, 1, 12, 2).
crosses(181, 2, 13, 1).
crosses(181, 3, 14, 1).
crosses(181, 0, 12, 1).
crosses(181, 1, 13, 1).
crosses(181, 2, 14, 1).
crosses(181, 1, 14, 1).
crosses(182, 1, 3, 1).
crosses(182, 2, 4, 1).
crosses(182, 5, 7, 3).
crosses(182, 6, 8, 1).
crosses(182, 8, 10, 1).
crosses(182, 10, 12, 1).
crosses(182, 12, 14, 3).
crosses(182, 0, 3, 1).
crosses(182, 1, 4, 2).
crosses(182, 2, 5, 1).
crosses(182, 4, 7, 3).
crosses(182, 5, 8, 4).
crosses(182, 6, 9, 1).
crosses(182, 7, 10, 1).
crosses(182, 8, 11, 1).
crosses(182, 9, 12, 1).
crosses(182, 11, 14, 2).
crosses(182, 0, 4, 2).
crosses(182, 1, 5, 2).
crosses(182, 3, 7, 2).
crosses(182, 4, 8, 4).
crosses(182, 5, 9, 4).
crosses(182, 6, 10, 2).
crosses(182, 7, 11, 1).
crosses(182, 8, 12, 2).
crosses(182, 10, 14, 2).
crosses(182, 0, 5, 2).
crosses(182, 2, 7, 1).
crosses(182, 3, 8, 3).
crosses(182, 4, 9, 4).
crosses(182, 5, 10, 5).
crosses(182, 6, 11, 2).
crosses(182, 7, 12, 2).
crosses(182, 9, 14, 1).
crosses(182, 1, 7, 1).
crosses(182, 2, 8, 2).
crosses(182, 3, 9, 3).
crosses(182, 4, 10, 5).
crosses(182, 5, 11, 5).
crosses(182, 6, 12, 3).
crosses(182, 8, 14, 1).
crosses(182, 1, 8, 2).
crosses(182, 2, 9, 2).
crosses(182, 3, 10, 4).
crosses(182, 4, 11, 5).
crosses(182, 5, 12, 6).
crosses(182, 0, 8, 1).
crosses(182, 1, 9, 2).
crosses(182, 2, 10, 3).
crosses(182, 3, 11, 4).
crosses(182, 4, 12, 6).
crosses(182, 5, 13, 3).
crosses(182, 0, 9, 1).
crosses(182, 1, 10, 3).
crosses(182, 2, 11, 3).
crosses(182, 3, 12, 5).
crosses(182, 4, 13, 3).
crosses(182, 5, 14, 3).
crosses(182, 0, 10, 2).
crosses(182, 1, 11, 3).
crosses(182, 2, 12, 4).
crosses(182, 3, 13, 2).
crosses(182, 4, 14, 3).
crosses(182, 0, 11, 2).
crosses(182, 1, 12, 4).
crosses(182, 2, 13, 1).
crosses(182, 3, 14, 2).
crosses(182, 0, 12, 3).
crosses(182, 1, 13, 1).
crosses(182, 2, 14, 1).
crosses(182, 1, 14, 1).
crosses(183, 1, 3, 2).
crosses(183, 4, 6, 2).
crosses(183, 6, 8, 1).
crosses(183, 8, 10, 2).
crosses(183, 0, 3, 1).
crosses(183, 1, 4, 2).
crosses(183, 3, 6, 2).
crosses(183, 4, 7, 2).
crosses(183, 5, 8, 1).
crosses(183, 7, 10, 1).
crosses(183, 0, 4, 1).
crosses(183, 1, 5, 1).
crosses(183, 2, 6, 1).
crosses(183, 3, 7, 2).
crosses(183, 4, 8, 3).
crosses(183, 6, 10, 1).
crosses(183, 1, 6, 2).
crosses(183, 2, 7, 1).
crosses(183, 3, 8, 3).
crosses(183, 4, 9, 1).
crosses(183, 0, 6, 1).
crosses(183, 1, 7, 2).
crosses(183, 2, 8, 2).
crosses(183, 3, 9, 1).
crosses(183, 4, 10, 1).
crosses(183, 0, 7, 1).
crosses(183, 1, 8, 3).
crosses(183, 3, 10, 1).
crosses(183, 0, 8, 2).
crosses(183, 1, 9, 1).
crosses(183, 1, 10, 1).
crosses(184, 2, 4, 2).
crosses(184, 4, 6, 2).
crosses(184, 6, 8, 1).
crosses(184, 8, 10, 2).
crosses(184, 1, 4, 2).
crosses(184, 2, 5, 1).
crosses(184, 3, 6, 1).
crosses(184, 4, 7, 2).
crosses(184, 5, 8, 1).
crosses(184, 7, 10, 1).
crosses(184, 0, 4, 1).
crosses(184, 1, 5, 1).
crosses(184, 2, 6, 2).
crosses(184, 3, 7, 1).
crosses(184, 4, 8, 3).
crosses(184, 6, 10, 1).
crosses(184, 1, 6, 2).
crosses(184, 2, 7, 2).
crosses(184, 3, 8, 2).
crosses(184, 4, 9, 1).
crosses(184, 0, 6, 1).
crosses(184, 1, 7, 2).
crosses(184, 2, 8, 3).
crosses(184, 4, 10, 1).
crosses(184, 0, 7, 1).
crosses(184, 1, 8, 3).
crosses(184, 2, 9, 1).
crosses(184, 0, 8, 2).
crosses(184, 1, 9, 1).
crosses(184, 2, 10, 1).
crosses(184, 1, 10, 1).
crosses(185, 1, 3, 2).
crosses(185, 3, 5, 2).
crosses(185, 5, 7, 1).
crosses(185, 7, 9, 2).
crosses(185, 0, 3, 1).
crosses(185, 1, 4, 1).
crosses(185, 2, 5, 1).
crosses(185, 3, 6, 2).
crosses(185, 4, 7, 1).
crosses(185, 6, 9, 1).
crosses(185, 1, 5, 2).
crosses(185, 2, 6, 1).
crosses(185, 3, 7, 3).
crosses(185, 5, 9, 1).
crosses(185, 0, 5, 1).
crosses(185, 1, 6, 2).
crosses(185, 2, 7, 2).
crosses(185, 3, 8, 1).
crosses(185, 0, 6, 1).
crosses(185, 1, 7, 3).
crosses(185, 3, 9, 1).
crosses(185, 0, 7, 2).
crosses(185, 1, 8, 1).
crosses(185, 1, 9, 1).
crosses(186, 2, 4, 2).
crosses(186, 4, 6, 2).
crosses(186, 6, 8, 1).
crosses(186, 8, 10, 2).
crosses(186, 1, 4, 2).
crosses(186, 2, 5, 1).
crosses(186, 3, 6, 1).
crosses(186, 4, 7, 2).
crosses(186, 5, 8, 1).
crosses(186, 7, 10, 1).
crosses(186, 0, 4, 1).
crosses(186, 1, 5, 1).
crosses(186, 2, 6, 2).
crosses(186, 3, 7, 1).
crosses(186, 4, 8, 3).
crosses(186, 6, 10, 1).
crosses(186, 1, 6, 2).
crosses(186, 2, 7, 2).
crosses(186, 3, 8, 2).
crosses(186, 4, 9, 1).
crosses(186, 0, 6, 1).
crosses(186, 1, 7, 2).
crosses(186, 2, 8, 3).
crosses(186, 4, 10, 1).
crosses(186, 0, 7, 1).
crosses(186, 1, 8, 3).
crosses(186, 2, 9, 1).
crosses(186, 0, 8, 2).
crosses(186, 1, 9, 1).
crosses(186, 2, 10, 1).
crosses(186, 1, 10, 1).
crosses(187, 2, 4, 2).
crosses(187, 4, 6, 2).
crosses(187, 7, 9, 2).
crosses(187, 10, 12, 1).
crosses(187, 12, 14, 2).
crosses(187, 1, 4, 2).
crosses(187, 2, 5, 1).
crosses(187, 3, 6, 1).
crosses(187, 4, 7, 2).
crosses(187, 6, 9, 2).
crosses(187, 7, 10, 2).
crosses(187, 9, 12, 1).
crosses(187, 11, 14, 1).
crosses(187, 0, 4, 1).
crosses(187, 1, 5, 1).
crosses(187, 2, 6, 2).
crosses(187, 3, 7, 1).
crosses(187, 4, 8, 1).
crosses(187, 5, 9, 1).
crosses(187, 6, 10, 2).
crosses(187, 7, 11, 2).
crosses(187, 8, 12, 1).
crosses(187, 10, 14, 1).
crosses(187, 1, 6, 2).
crosses(187, 2, 7, 2).
crosses(187, 4, 9, 2).
crosses(187, 5, 10, 1).
crosses(187, 6, 11, 2).
crosses(187, 7, 12, 3).
crosses(187, 9, 14, 1).
crosses(187, 0, 6, 1).
crosses(187, 1, 7, 2).
crosses(187, 2, 8, 1).
crosses(187, 3, 9, 1).
crosses(187, 4, 10, 2).
crosses(187, 5, 11, 1).
crosses(187, 6, 12, 3).
crosses(187, 7, 13, 1).
crosses(187, 0, 7, 1).
crosses(187, 1, 8, 1).
crosses(187, 2, 9, 2).
crosses(187, 3, 10, 1).
crosses(187, 4, 11, 2).
crosses(187, 5, 12, 2).
crosses(187, 6, 13, 1).
crosses(187, 7, 14, 1).
crosses(187, 1, 9, 2).
crosses(187, 2, 10, 2).
crosses(187, 3, 11, 1).
crosses(187, 4, 12, 3).
crosses(187, 6, 14, 1).
crosses(187, 0, 9, 1).
crosses(187, 1, 10, 2).
crosses(187, 2, 11, 2).
crosses(187, 3, 12, 2).
crosses(187, 4, 13, 1).
crosses(187, 0, 10, 1).
crosses(187, 1, 11, 2).
crosses(187, 2, 12, 3).
crosses(187, 4, 14, 1).
crosses(187, 0, 11, 1).
crosses(187, 1, 12, 3).
crosses(187, 2, 13, 1).
crosses(187, 0, 12, 2).
crosses(187, 1, 13, 1).
crosses(187, 2, 14, 1).
crosses(187, 1, 14, 1).
crosses(188, 0, 2, 1).
crosses(188, 1, 3, 1).
crosses(188, 3, 5, 4).
crosses(188, 5, 7, 2).
crosses(188, 8, 10, 2).
crosses(188, 11, 13, 1).
crosses(188, 13, 15, 2).
crosses(188, 0, 3, 2).
crosses(188, 2, 5, 3).
crosses(188, 3, 6, 3).
crosses(188, 4, 7, 1).
crosses(188, 5, 8, 2).
crosses(188, 7, 10, 2).
crosses(188, 8, 11, 2).
crosses(188, 10, 13, 1).
crosses(188, 12, 15, 1).
crosses(188, 1, 5, 2).
crosses(188, 2, 6, 2).
crosses(188, 3, 7, 4).
crosses(188, 4, 8, 1).
crosses(188, 5, 9, 1).
crosses(188, 6, 10, 1).
crosses(188, 7, 11, 2).
crosses(188, 8, 12, 2).
crosses(188, 9, 13, 1).
crosses(188, 11, 15, 1).
crosses(188, 0, 5, 1).
crosses(188, 1, 6, 1).
crosses(188, 2, 7, 3).
crosses(188, 3, 8, 4).
crosses(188, 5, 10, 2).
crosses(188, 6, 11, 1).
crosses(188, 7, 12, 2).
crosses(188, 8, 13, 3).
crosses(188, 10, 15, 1).
crosses(188, 1, 7, 2).
crosses(188, 2, 8, 3).
crosses(188, 3, 9, 3).
crosses(188, 4, 10, 1).
crosses(188, 5, 11, 2).
crosses(188, 6, 12, 1).
crosses(188, 7, 13, 3).
crosses(188, 8, 14, 1).
crosses(188, 0, 7, 1).
crosses(188, 1, 8, 2).
crosses(188, 2, 9, 2).
crosses(188, 3, 10, 4).
crosses(188, 4, 11, 1).
crosses(188, 5, 12, 2).
crosses(188, 6, 13, 2).
crosses(188, 7, 14, 1).
crosses(188, 8, 15, 1).
crosses(188, 0, 8, 1).
crosses(188, 1, 9, 1).
crosses(188, 2, 10, 3).
crosses(188, 3, 11, 4).
crosses(188, 4, 12, 1).
crosses(188, 5, 13, 3).
crosses(188, 7, 15, 1).
crosses(188, 1, 10, 2).
crosses(188, 2, 11, 3).
crosses(188, 3, 12, 4).
crosses(188, 4, 13, 2).
crosses(188, 5, 14, 1).
crosses(188, 0, 10, 1).
crosses(188, 1, 11, 2).
crosses(188, 2, 12, 3).
crosses(188, 3, 13, 5).
crosses(188, 5, 15, 1).
crosses(188, 0, 11, 1).
crosses(188, 1, 12, 2).
crosses(188, 2, 13, 4).
crosses(188, 3, 14, 3).
crosses(188, 0, 12, 1).
crosses(188, 1, 13, 3).
crosses(188, 2, 14, 2).
crosses(188, 3, 15, 3).
crosses(188, 0, 13, 2).
crosses(188, 1, 14, 1).
crosses(188, 2, 15, 2).
crosses(188, 1, 15, 1).
crosses(189, 1, 3, 2).
crosses(189, 3, 5, 2).
crosses(189, 5, 7, 2).
crosses(189, 6, 8, 1).
crosses(189, 10, 12, 2).
crosses(189, 0, 3, 1).
crosses(189, 1, 4, 1).
crosses(189, 2, 5, 1).
crosses(189, 3, 6, 1).
crosses(189, 4, 7, 1).
crosses(189, 5, 8, 3).
crosses(189, 6, 9, 1).
crosses(189, 9, 12, 2).
crosses(189, 1, 5, 2).
crosses(189, 3, 7, 2).
crosses(189, 4, 8, 2).
crosses(189, 5, 9, 3).
crosses(189, 6, 10, 1).
crosses(189, 8, 12, 2).
crosses(189, 0, 5, 1).
crosses(189, 1, 6, 1).
crosses(189, 2, 7, 1).
crosses(189, 3, 8, 3).
crosses(189, 4, 9, 2).
crosses(189, 5, 10, 3).
crosses(189, 7, 12, 1).
crosses(189, 1, 7, 2).
crosses(189, 2, 8, 2).
crosses(189, 3, 9, 3).
crosses(189, 4, 10, 2).
crosses(189, 5, 11, 1).
crosses(189, 0, 7, 1).
crosses(189, 1, 8, 3).
crosses(189, 2, 9, 2).
crosses(189, 3, 10, 3).
crosses(189, 5, 12, 1).
crosses(189, 0, 8, 2).
crosses(189, 1, 9, 3).
crosses(189, 2, 10, 2).
crosses(189, 3, 11, 1).
crosses(189, 0, 9, 2).
crosses(189, 1, 10, 3).
crosses(189, 3, 12, 1).
crosses(189, 0, 10, 2).
crosses(189, 1, 11, 1).
crosses(189, 1, 12, 1).
crosses(190, 2, 4, 2).
crosses(190, 4, 6, 2).
crosses(190, 6, 8, 2).
crosses(190, 7, 9, 1).
crosses(190, 11, 13, 2).
crosses(190, 1, 4, 2).
crosses(190, 2, 5, 1).
crosses(190, 3, 6, 1).
crosses(190, 4, 7, 1).
crosses(190, 5, 8, 1).
crosses(190, 6, 9, 3).
crosses(190, 7, 10, 1).
crosses(190, 10, 13, 2).
crosses(190, 0, 4, 1).
crosses(190, 1, 5, 1).
crosses(190, 2, 6, 2).
crosses(190, 4, 8, 2).
crosses(190, 5, 9, 2).
crosses(190, 6, 10, 3).
crosses(190, 7, 11, 1).
crosses(190, 9, 13, 2).
crosses(190, 1, 6, 2).
crosses(190, 2, 7, 1).
crosses(190, 3, 8, 1).
crosses(190, 4, 9, 3).
crosses(190, 5, 10, 2).
crosses(190, 6, 11, 3).
crosses(190, 8, 13, 1).
crosses(190, 0, 6, 1).
crosses(190, 1, 7, 1).
crosses(190, 2, 8, 2).
crosses(190, 3, 9, 2).
crosses(190, 4, 10, 3).
crosses(190, 5, 11, 2).
crosses(190, 6, 12, 1).
crosses(190, 1, 8, 2).
crosses(190, 2, 9, 3).
crosses(190, 3, 10, 2).
crosses(190, 4, 11, 3).
crosses(190, 6, 13, 1).
crosses(190, 0, 8, 1).
crosses(190, 1, 9, 3).
crosses(190, 2, 10, 3).
crosses(190, 3, 11, 2).
crosses(190, 4, 12, 1).
crosses(190, 0, 9, 2).
crosses(190, 1, 10, 3).
crosses(190, 2, 11, 3).
crosses(190, 4, 13, 1).
crosses(190, 0, 10, 2).
crosses(190, 1, 11, 3).
crosses(190, 2, 12, 1).
crosses(190, 0, 11, 2).
crosses(190, 1, 12, 1).
crosses(190, 2, 13, 1).
crosses(190, 1, 13, 1).
crosses(191, 1, 3, 1).
crosses(191, 3, 5, 2).
crosses(191, 4, 6, 1).
crosses(191, 6, 8, 1).
crosses(191, 8, 10, 2).
crosses(191, 0, 3, 1).
crosses(191, 2, 5, 1).
crosses(191, 3, 6, 3).
crosses(191, 4, 7, 1).
crosses(191, 5, 8, 1).
crosses(191, 7, 10, 1).
crosses(191, 1, 5, 1).
crosses(191, 2, 6, 2).
crosses(191, 3, 7, 3).
crosses(191, 4, 8, 2).
crosses(191, 6, 10, 1).
crosses(191, 1, 6, 2).
crosses(191, 2, 7, 2).
crosses(191, 3, 8, 4).
crosses(191, 0, 6, 1).
crosses(191, 1, 7, 2).
crosses(191, 2, 8, 3).
crosses(191, 3, 9, 2).
crosses(191, 0, 7, 1).
crosses(191, 1, 8, 3).
crosses(191, 2, 9, 1).
crosses(191, 3, 10, 2).
crosses(191, 0, 8, 2).
crosses(191, 1, 9, 1).
crosses(191, 2, 10, 1).
crosses(191, 1, 10, 1).
crosses(192, 1, 3, 1).
crosses(192, 2, 4, 1).
crosses(192, 3, 5, 1).
crosses(192, 5, 7, 1).
crosses(192, 8, 10, 1).
crosses(192, 10, 12, 6).
crosses(192, 11, 13, 1).
crosses(192, 13, 15, 1).
crosses(192, 15, 17, 2).
crosses(192, 0, 3, 1).
crosses(192, 1, 4, 2).
crosses(192, 2, 5, 2).
crosses(192, 3, 6, 1).
crosses(192, 4, 7, 1).
crosses(192, 5, 8, 1).
crosses(192, 7, 10, 1).
crosses(192, 9, 12, 5).
crosses(192, 10, 13, 7).
crosses(192, 11, 14, 1).
crosses(192, 12, 15, 1).
crosses(192, 14, 17, 1).
crosses(192, 0, 4, 2).
crosses(192, 1, 5, 3).
crosses(192, 2, 6, 2).
crosses(192, 3, 7, 2).
crosses(192, 4, 8, 1).
crosses(192, 5, 9, 1).
crosses(192, 6, 10, 1).
crosses(192, 8, 12, 5).
crosses(192, 9, 13, 6).
crosses(192, 10, 14, 7).
crosses(192, 11, 15, 2).
crosses(192, 13, 17, 1).
crosses(192, 0, 5, 3).
crosses(192, 1, 6, 3).
crosses(192, 2, 7, 3).
crosses(192, 3, 8, 2).
crosses(192, 4, 9, 1).
crosses(192, 5, 10, 2).
crosses(192, 7, 12, 5).
crosses(192, 8, 13, 6).
crosses(192, 9, 14, 6).
crosses(192, 10, 15, 8).
crosses(192, 0, 6, 3).
crosses(192, 1, 7, 4).
crosses(192, 2, 8, 3).
crosses(192, 3, 9, 2).
crosses(192, 4, 10, 2).
crosses(192, 6, 12, 4).
crosses(192, 7, 13, 6).
crosses(192, 8, 14, 6).
crosses(192, 9, 15, 7).
crosses(192, 10, 16, 6).
crosses(192, 0, 7, 4).
crosses(192, 1, 8, 4).
crosses(192, 2, 9, 3).
crosses(192, 3, 10, 3).
crosses(192, 5, 12, 4).
crosses(192, 6, 13, 5).
crosses(192, 7, 14, 6).
crosses(192, 8, 15, 7).
crosses(192, 9, 16, 5).
crosses(192, 10, 17, 6).
crosses(192, 0, 8, 4).
crosses(192, 1, 9, 4).
crosses(192, 2, 10, 4).
crosses(192, 4, 12, 3).
crosses(192, 5, 13, 5).
crosses(192, 6, 14, 5).
crosses(192, 7, 15, 7).
crosses(192, 8, 16, 5).
crosses(192, 9, 17, 5).
crosses(192, 0, 9, 4).
crosses(192, 1, 10, 5).
crosses(192, 3, 12, 2).
crosses(192, 4, 13, 4).
crosses(192, 5, 14, 5).
crosses(192, 6, 15, 6).
crosses(192, 7, 16, 5).
crosses(192, 8, 17, 5).
crosses(192, 0, 10, 5).
crosses(192, 2, 12, 1).
crosses(192, 3, 13, 3).
crosses(192, 4, 14, 4).
crosses(192, 5, 15, 6).
crosses(192, 6, 16, 4).
crosses(192, 7, 17, 5).
crosses(192, 1, 12, 1).
crosses(192, 2, 13, 2).
crosses(192, 3, 14, 3).
crosses(192, 4, 15, 5).
crosses(192, 5, 16, 4).
crosses(192, 6, 17, 4).
crosses(192, 1, 13, 2).
crosses(192, 2, 14, 2).
crosses(192, 3, 15, 4).
crosses(192, 4, 16, 3).
crosses(192, 5, 17, 4).
crosses(192, 0, 13, 1).
crosses(192, 1, 14, 2).
crosses(192, 2, 15, 3).
crosses(192, 3, 16, 2).
crosses(192, 4, 17, 3).
crosses(192, 0, 14, 1).
crosses(192, 1, 15, 3).
crosses(192, 2, 16, 1).
crosses(192, 3, 17, 2).
crosses(192, 0, 15, 2).
crosses(192, 1, 16, 1).
crosses(192, 2, 17, 1).
crosses(192, 1, 17, 1).
crosses(193, 0, 2, 1).
crosses(193, 1, 3, 1).
crosses(193, 2, 4, 1).
crosses(193, 4, 6, 4).
crosses(193, 5, 7, 1).
crosses(193, 6, 8, 1).
crosses(193, 8, 10, 2).
crosses(193, 0, 3, 2).
crosses(193, 1, 4, 2).
crosses(193, 3, 6, 3).
crosses(193, 4, 7, 5).
crosses(193, 5, 8, 2).
crosses(193, 7, 10, 1).
crosses(193, 0, 4, 3).
crosses(193, 2, 6, 2).
crosses(193, 3, 7, 4).
crosses(193, 4, 8, 6).
crosses(193, 1, 6, 1).
crosses(193, 2, 7, 3).
crosses(193, 3, 8, 5).
crosses(193, 4, 9, 4).
crosses(193, 1, 7, 2).
crosses(193, 2, 8, 4).
crosses(193, 3, 9, 3).
crosses(193, 4, 10, 4).
crosses(193, 0, 7, 1).
crosses(193, 1, 8, 3).
crosses(193, 2, 9, 2).
crosses(193, 3, 10, 3).
crosses(193, 0, 8, 2).
crosses(193, 1, 9, 1).
crosses(193, 2, 10, 2).
crosses(193, 1, 10, 1).
crosses(194, 1, 3, 1).
crosses(194, 4, 6, 1).
crosses(194, 6, 8, 2).
crosses(194, 7, 9, 1).
crosses(194, 8, 10, 1).
crosses(194, 10, 12, 2).
crosses(194, 0, 3, 1).
crosses(194, 1, 4, 1).
crosses(194, 3, 6, 1).
crosses(194, 5, 8, 1).
crosses(194, 6, 9, 3).
crosses(194, 7, 10, 2).
crosses(194, 9, 12, 1).
crosses(194, 0, 4, 1).
crosses(194, 1, 5, 1).
crosses(194, 2, 6, 1).
crosses(194, 4, 8, 1).
crosses(194, 5, 9, 2).
crosses(194, 6, 10, 4).
crosses(194, 0, 5, 1).
crosses(194, 1, 6, 2).
crosses(194, 3, 8, 1).
crosses(194, 4, 9, 2).
crosses(194, 5, 10, 3).
crosses(194, 6, 11, 2).
crosses(194, 0, 6, 2).
crosses(194, 3, 9, 2).
crosses(194, 4, 10, 3).
crosses(194, 5, 11, 1).
crosses(194, 6, 12, 2).
crosses(194, 2, 9, 1).
crosses(194, 3, 10, 3).
crosses(194, 4, 11, 1).
crosses(194, 5, 12, 1).
crosses(194, 1, 9, 1).
crosses(194, 2, 10, 2).
crosses(194, 3, 11, 1).
crosses(194, 4, 12, 1).
crosses(194, 0, 9, 1).
crosses(194, 1, 10, 2).
crosses(194, 3, 12, 1).
crosses(194, 0, 10, 2).
crosses(195, 0, 2, 1).
crosses(195, 1, 3, 1).
crosses(195, 2, 4, 1).
crosses(195, 4, 6, 4).
crosses(195, 5, 7, 1).
crosses(195, 6, 8, 1).
crosses(195, 7, 9, 1).
crosses(195, 8, 10, 1).
crosses(195, 9, 11, 1).
crosses(195, 11, 13, 5).
crosses(195, 0, 3, 2).
crosses(195, 1, 4, 2).
crosses(195, 3, 6, 3).
crosses(195, 4, 7, 5).
crosses(195, 5, 8, 2).
crosses(195, 6, 9, 2).
crosses(195, 7, 10, 2).
crosses(195, 8, 11, 2).
crosses(195, 10, 13, 4).
crosses(195, 0, 4, 3).
crosses(195, 2, 6, 2).
crosses(195, 3, 7, 4).
crosses(195, 4, 8, 6).
crosses(195, 5, 9, 3).
crosses(195, 6, 10, 3).
crosses(195, 7, 11, 3).
crosses(195, 9, 13, 3).
crosses(195, 1, 6, 1).
crosses(195, 2, 7, 3).
crosses(195, 3, 8, 5).
crosses(195, 4, 9, 7).
crosses(195, 5, 10, 4).
crosses(195, 6, 11, 4).
crosses(195, 8, 13, 2).
crosses(195, 1, 7, 2).
crosses(195, 2, 8, 4).
crosses(195, 3, 9, 6).
crosses(195, 4, 10, 8).
crosses(195, 5, 11, 5).
crosses(195, 7, 13, 1).
crosses(195, 0, 7, 1).
crosses(195, 1, 8, 3).
crosses(195, 2, 9, 5).
crosses(195, 3, 10, 7).
crosses(195, 4, 11, 9).
crosses(195, 0, 8, 2).
crosses(195, 1, 9, 4).
crosses(195, 2, 10, 6).
crosses(195, 3, 11, 8).
crosses(195, 4, 12, 4).
crosses(195, 0, 9, 3).
crosses(195, 1, 10, 5).
crosses(195, 2, 11, 7).
crosses(195, 3, 12, 3).
crosses(195, 4, 13, 4).
crosses(195, 0, 10, 4).
crosses(195, 1, 11, 6).
crosses(195, 2, 12, 2).
crosses(195, 3, 13, 3).
crosses(195, 0, 11, 5).
crosses(195, 1, 12, 1).
crosses(195, 2, 13, 2).
crosses(195, 1, 13, 1).
crosses(196, 0, 2, 1).
crosses(196, 1, 3, 1).
crosses(196, 3, 5, 1).
crosses(196, 5, 7, 4).
crosses(196, 6, 8, 1).
crosses(196, 7, 9, 1).
crosses(196, 8, 10, 1).
crosses(196, 9, 11, 1).
crosses(196, 10, 12, 1).
crosses(196, 12, 14, 5).
crosses(196, 0, 3, 2).
crosses(196, 1, 4, 1).
crosses(196, 2, 5, 1).
crosses(196, 4, 7, 3).
crosses(196, 5, 8, 5).
crosses(196, 6, 9, 2).
crosses(196, 7, 10, 2).
crosses(196, 8, 11, 2).
crosses(196, 9, 12, 2).
crosses(196, 11, 14, 4).
crosses(196, 0, 4, 2).
crosses(196, 1, 5, 2).
crosses(196, 3, 7, 3).
crosses(196, 4, 8, 4).
crosses(196, 5, 9, 6).
crosses(196, 6, 10, 3).
crosses(196, 7, 11, 3).
crosses(196, 8, 12, 3).
crosses(196, 10, 14, 3).
crosses(196, 0, 5, 3).
crosses(196, 2, 7, 2).
crosses(196, 3, 8, 4).
crosses(196, 4, 9, 5).
crosses(196, 5, 10, 7).
crosses(196, 6, 11, 4).
crosses(196, 7, 12, 4).
crosses(196, 9, 14, 2).
crosses(196, 1, 7, 1).
crosses(196, 2, 8, 3).
crosses(196, 3, 9, 5).
crosses(196, 4, 10, 6).
crosses(196, 5, 11, 8).
crosses(196, 6, 12, 5).
crosses(196, 8, 14, 1).
crosses(196, 1, 8, 2).
crosses(196, 2, 9, 4).
crosses(196, 3, 10, 6).
crosses(196, 4, 11, 7).
crosses(196, 5, 12, 9).
crosses(196, 0, 8, 1).
crosses(196, 1, 9, 3).
crosses(196, 2, 10, 5).
crosses(196, 3, 11, 7).
crosses(196, 4, 12, 8).
crosses(196, 5, 13, 4).
crosses(196, 0, 9, 2).
crosses(196, 1, 10, 4).
crosses(196, 2, 11, 6).
crosses(196, 3, 12, 8).
crosses(196, 4, 13, 3).
crosses(196, 5, 14, 4).
crosses(196, 0, 10, 3).
crosses(196, 1, 11, 5).
crosses(196, 2, 12, 7).
crosses(196, 3, 13, 3).
crosses(196, 4, 14, 3).
crosses(196, 0, 11, 4).
crosses(196, 1, 12, 6).
crosses(196, 2, 13, 2).
crosses(196, 3, 14, 3).
crosses(196, 0, 12, 5).
crosses(196, 1, 13, 1).
crosses(196, 2, 14, 2).
crosses(196, 1, 14, 1).
crosses(197, 2, 4, 1).
crosses(197, 3, 5, 1).
crosses(197, 4, 6, 1).
crosses(197, 5, 7, 1).
crosses(197, 7, 9, 3).
crosses(197, 1, 4, 1).
crosses(197, 2, 5, 2).
crosses(197, 3, 6, 2).
crosses(197, 4, 7, 2).
crosses(197, 6, 9, 2).
crosses(197, 1, 5, 2).
crosses(197, 2, 6, 3).
crosses(197, 3, 7, 3).
crosses(197, 5, 9, 1).
crosses(197, 0, 5, 1).
crosses(197, 1, 6, 3).
crosses(197, 2, 7, 4).
crosses(197, 0, 6, 2).
crosses(197, 1, 7, 4).
crosses(197, 2, 8, 1).
crosses(197, 0, 7, 3).
crosses(197, 1, 8, 1).
crosses(197, 2, 9, 1).
crosses(197, 1, 9, 1).
crosses(198, 3, 5, 1).
crosses(198, 4, 6, 1).
crosses(198, 5, 7, 1).
crosses(198, 6, 8, 1).
crosses(198, 8, 10, 3).
crosses(198, 2, 5, 1).
crosses(198, 3, 6, 2).
crosses(198, 4, 7, 2).
crosses(198, 5, 8, 2).
crosses(198, 7, 10, 2).
crosses(198, 1, 5, 1).
crosses(198, 2, 6, 2).
crosses(198, 3, 7, 3).
crosses(198, 4, 8, 3).
crosses(198, 6, 10, 1).
crosses(198, 1, 6, 2).
crosses(198, 2, 7, 3).
crosses(198, 3, 8, 4).
crosses(198, 0, 6, 1).
crosses(198, 1, 7, 3).
crosses(198, 2, 8, 4).
crosses(198, 3, 9, 1).
crosses(198, 0, 7, 2).
crosses(198, 1, 8, 4).
crosses(198, 2, 9, 1).
crosses(198, 3, 10, 1).
crosses(198, 0, 8, 3).
crosses(198, 1, 9, 1).
crosses(198, 2, 10, 1).
crosses(198, 1, 10, 1).
crosses(199, 0, 2, 1).
crosses(199, 1, 3, 1).
crosses(199, 3, 5, 4).
crosses(199, 5, 7, 2).
crosses(199, 8, 10, 2).
crosses(199, 9, 11, 1).
crosses(199, 11, 13, 2).
crosses(199, 0, 3, 2).
crosses(199, 2, 5, 3).
crosses(199, 3, 6, 3).
crosses(199, 4, 7, 1).
crosses(199, 5, 8, 2).
crosses(199, 7, 10, 2).
crosses(199, 8, 11, 3).
crosses(199, 10, 13, 1).
crosses(199, 1, 5, 2).
crosses(199, 2, 6, 2).
crosses(199, 3, 7, 4).
crosses(199, 4, 8, 1).
crosses(199, 5, 9, 1).
crosses(199, 6, 10, 1).
crosses(199, 7, 11, 3).
crosses(199, 8, 12, 1).
crosses(199, 0, 5, 1).
crosses(199, 1, 6, 1).
crosses(199, 2, 7, 3).
crosses(199, 3, 8, 4).
crosses(199, 5, 10, 2).
crosses(199, 6, 11, 2).
crosses(199, 7, 12, 1).
crosses(199, 8, 13, 1).
crosses(199, 1, 7, 2).
crosses(199, 2, 8, 3).
crosses(199, 3, 9, 3).
crosses(199, 4, 10, 1).
crosses(199, 5, 11, 3).
crosses(199, 7, 13, 1).
crosses(199, 0, 7, 1).
crosses(199, 1, 8, 2).
crosses(199, 2, 9, 2).
crosses(199, 3, 10, 4).
crosses(199, 4, 11, 2).
crosses(199, 5, 12, 1).
crosses(199, 0, 8, 1).
crosses(199, 1, 9, 1).
crosses(199, 2, 10, 3).
crosses(199, 3, 11, 5).
crosses(199, 5, 13, 1).
crosses(199, 1, 10, 2).
crosses(199, 2, 11, 4).
crosses(199, 3, 12, 3).
crosses(199, 0, 10, 1).
crosses(199, 1, 11, 3).
crosses(199, 2, 12, 2).
crosses(199, 3, 13, 3).
crosses(199, 0, 11, 2).
crosses(199, 1, 12, 1).
crosses(199, 2, 13, 2).
crosses(199, 1, 13, 1).
crosses(200, 3, 5, 1).
crosses(200, 4, 6, 1).
crosses(200, 7, 9, 2).
crosses(200, 8, 10, 1).
crosses(200, 10, 12, 2).
crosses(200, 2, 5, 1).
crosses(200, 3, 6, 2).
crosses(200, 4, 7, 1).
crosses(200, 6, 9, 2).
crosses(200, 7, 10, 3).
crosses(200, 9, 12, 1).
crosses(200, 1, 5, 1).
crosses(200, 2, 6, 2).
crosses(200, 3, 7, 2).
crosses(200, 5, 9, 1).
crosses(200, 6, 10, 3).
crosses(200, 7, 11, 1).
crosses(200, 1, 6, 2).
crosses(200, 2, 7, 2).
crosses(200, 3, 8, 1).
crosses(200, 4, 9, 1).
crosses(200, 5, 10, 2).
crosses(200, 6, 11, 1).
crosses(200, 7, 12, 1).
crosses(200, 0, 6, 1).
crosses(200, 1, 7, 2).
crosses(200, 2, 8, 1).
crosses(200, 3, 9, 2).
crosses(200, 4, 10, 2).
crosses(200, 6, 12, 1).
crosses(200, 0, 7, 1).
crosses(200, 1, 8, 1).
crosses(200, 2, 9, 2).
crosses(200, 3, 10, 3).
crosses(200, 1, 9, 2).
crosses(200, 2, 10, 3).
crosses(200, 3, 11, 1).
crosses(200, 0, 9, 1).
crosses(200, 1, 10, 3).
crosses(200, 2, 11, 1).
crosses(200, 3, 12, 1).
crosses(200, 0, 10, 2).
crosses(200, 1, 11, 1).
crosses(200, 2, 12, 1).
crosses(200, 1, 12, 1).
crosses(201, 1, 3, 1).
crosses(201, 3, 5, 1).
crosses(201, 6, 8, 1).
crosses(201, 7, 9, 1).
crosses(201, 9, 11, 2).
crosses(201, 0, 3, 1).
crosses(201, 3, 6, 1).
crosses(201, 5, 8, 1).
crosses(201, 6, 9, 2).
crosses(201, 8, 11, 1).
crosses(201, 3, 7, 1).
crosses(201, 4, 8, 1).
crosses(201, 5, 9, 2).
crosses(201, 3, 8, 2).
crosses(201, 4, 9, 2).
crosses(201, 2, 8, 1).
crosses(201, 3, 9, 3).
crosses(201, 1, 8, 1).
crosses(201, 2, 9, 2).
crosses(201, 3, 10, 1).
crosses(201, 0, 8, 1).
crosses(201, 1, 9, 2).
crosses(201, 3, 11, 1).
crosses(201, 0, 9, 2).
crosses(202, 1, 3, 1).
crosses(202, 4, 6, 1).
crosses(202, 7, 9, 1).
crosses(202, 8, 10, 1).
crosses(202, 10, 12, 2).
crosses(202, 0, 3, 1).
crosses(202, 1, 4, 1).
crosses(202, 3, 6, 1).
crosses(202, 4, 7, 1).
crosses(202, 6, 9, 1).
crosses(202, 7, 10, 2).
crosses(202, 9, 12, 1).
crosses(202, 0, 4, 1).
crosses(202, 3, 7, 1).
crosses(202, 4, 8, 1).
crosses(202, 5, 9, 1).
crosses(202, 6, 10, 2).
crosses(202, 3, 8, 1).
crosses(202, 4, 9, 2).
crosses(202, 5, 10, 2).
crosses(202, 3, 9, 2).
crosses(202, 4, 10, 3).
crosses(202, 2, 9, 1).
crosses(202, 3, 10, 3).
crosses(202, 4, 11, 1).
crosses(202, 1, 9, 1).
crosses(202, 2, 10, 2).
crosses(202, 3, 11, 1).
crosses(202, 4, 12, 1).
crosses(202, 0, 9, 1).
crosses(202, 1, 10, 2).
crosses(202, 3, 12, 1).
crosses(202, 0, 10, 2).
crosses(203, 3, 5, 1).
crosses(203, 6, 8, 1).
crosses(203, 7, 9, 1).
crosses(203, 9, 11, 2).
crosses(203, 2, 5, 1).
crosses(203, 3, 6, 1).
crosses(203, 5, 8, 1).
crosses(203, 6, 9, 2).
crosses(203, 8, 11, 1).
crosses(203, 1, 5, 1).
crosses(203, 2, 6, 1).
crosses(203, 3, 7, 1).
crosses(203, 4, 8, 1).
crosses(203, 5, 9, 2).
crosses(203, 1, 6, 1).
crosses(203, 2, 7, 1).
crosses(203, 3, 8, 2).
crosses(203, 4, 9, 2).
crosses(203, 1, 7, 1).
crosses(203, 2, 8, 2).
crosses(203, 3, 9, 3).
crosses(203, 1, 8, 2).
crosses(203, 2, 9, 3).
crosses(203, 3, 10, 1).
crosses(203, 0, 8, 1).
crosses(203, 1, 9, 3).
crosses(203, 2, 10, 1).
crosses(203, 3, 11, 1).
crosses(203, 0, 9, 2).
crosses(203, 1, 10, 1).
crosses(203, 2, 11, 1).
crosses(203, 1, 11, 1).
crosses(204, 1, 3, 1).
crosses(204, 4, 6, 1).
crosses(204, 7, 9, 1).
crosses(204, 8, 10, 1).
crosses(204, 10, 12, 2).
crosses(204, 0, 3, 1).
crosses(204, 1, 4, 1).
crosses(204, 3, 6, 1).
crosses(204, 4, 7, 1).
crosses(204, 6, 9, 1).
crosses(204, 7, 10, 2).
crosses(204, 9, 12, 1).
crosses(204, 0, 4, 1).
crosses(204, 3, 7, 1).
crosses(204, 4, 8, 1).
crosses(204, 5, 9, 1).
crosses(204, 6, 10, 2).
crosses(204, 3, 8, 1).
crosses(204, 4, 9, 2).
crosses(204, 5, 10, 2).
crosses(204, 3, 9, 2).
crosses(204, 4, 10, 3).
crosses(204, 2, 9, 1).
crosses(204, 3, 10, 3).
crosses(204, 4, 11, 1).
crosses(204, 1, 9, 1).
crosses(204, 2, 10, 2).
crosses(204, 3, 11, 1).
crosses(204, 4, 12, 1).
crosses(204, 0, 9, 1).
crosses(204, 1, 10, 2).
crosses(204, 3, 12, 1).
crosses(204, 0, 10, 2).
crosses(205, 4, 6, 2).
crosses(205, 6, 8, 2).
crosses(205, 8, 10, 1).
crosses(205, 3, 6, 2).
crosses(205, 4, 7, 1).
crosses(205, 5, 8, 1).
crosses(205, 6, 9, 1).
crosses(205, 2, 6, 2).
crosses(205, 3, 7, 1).
crosses(205, 4, 8, 2).
crosses(205, 6, 10, 1).
crosses(205, 1, 6, 2).
crosses(205, 2, 7, 1).
crosses(205, 3, 8, 2).
crosses(205, 4, 9, 1).
crosses(205, 0, 6, 1).
crosses(205, 1, 7, 1).
crosses(205, 2, 8, 2).
crosses(205, 3, 9, 1).
crosses(205, 4, 10, 1).
crosses(205, 1, 8, 2).
crosses(205, 2, 9, 1).
crosses(205, 3, 10, 1).
crosses(205, 0, 8, 1).
crosses(205, 1, 9, 1).
crosses(205, 2, 10, 1).
crosses(205, 1, 10, 1).
crosses(206, 0, 2, 1).
crosses(206, 2, 4, 2).
crosses(206, 4, 6, 1).
crosses(206, 1, 4, 1).
crosses(206, 2, 5, 1).
crosses(206, 0, 4, 1).
crosses(206, 2, 6, 1).
crosses(207, 3, 5, 1).
crosses(207, 5, 7, 3).
crosses(207, 7, 9, 2).
crosses(207, 9, 11, 1).
crosses(207, 2, 5, 1).
crosses(207, 4, 7, 2).
crosses(207, 5, 8, 2).
crosses(207, 6, 9, 1).
crosses(207, 7, 10, 1).
crosses(207, 1, 5, 1).
crosses(207, 3, 7, 2).
crosses(207, 4, 8, 1).
crosses(207, 5, 9, 3).
crosses(207, 7, 11, 1).
crosses(207, 0, 5, 1).
crosses(207, 2, 7, 2).
crosses(207, 3, 8, 1).
crosses(207, 4, 9, 2).
crosses(207, 5, 10, 2).
crosses(207, 1, 7, 2).
crosses(207, 2, 8, 1).
crosses(207, 3, 9, 2).
crosses(207, 4, 10, 1).
crosses(207, 5, 11, 2).
crosses(207, 0, 7, 1).
crosses(207, 1, 8, 1).
crosses(207, 2, 9, 2).
crosses(207, 3, 10, 1).
crosses(207, 4, 11, 1).
crosses(207, 1, 9, 2).
crosses(207, 2, 10, 1).
crosses(207, 3, 11, 1).
crosses(207, 0, 9, 1).
crosses(207, 1, 10, 1).
crosses(207, 2, 11, 1).
crosses(207, 1, 11, 1).
crosses(208, 0, 2, 1).
crosses(208, 2, 4, 2).
crosses(208, 4, 6, 1).
crosses(208, 1, 4, 1).
crosses(208, 2, 5, 1).
crosses(208, 0, 4, 1).
crosses(208, 2, 6, 1).
crosses(209, 4, 6, 2).
crosses(209, 6, 8, 2).
crosses(209, 8, 10, 1).
crosses(209, 3, 6, 2).
crosses(209, 4, 7, 1).
crosses(209, 5, 8, 1).
crosses(209, 6, 9, 1).
crosses(209, 2, 6, 2).
crosses(209, 3, 7, 1).
crosses(209, 4, 8, 2).
crosses(209, 6, 10, 1).
crosses(209, 1, 6, 2).
crosses(209, 2, 7, 1).
crosses(209, 3, 8, 2).
crosses(209, 4, 9, 1).
crosses(209, 0, 6, 1).
crosses(209, 1, 7, 1).
crosses(209, 2, 8, 2).
crosses(209, 3, 9, 1).
crosses(209, 4, 10, 1).
crosses(209, 1, 8, 2).
crosses(209, 2, 9, 1).
crosses(209, 3, 10, 1).
crosses(209, 0, 8, 1).
crosses(209, 1, 9, 1).
crosses(209, 2, 10, 1).
crosses(209, 1, 10, 1).
crosses(210, 0, 2, 1).
crosses(210, 2, 4, 2).
crosses(210, 4, 6, 1).
crosses(210, 1, 4, 1).
crosses(210, 2, 5, 1).
crosses(210, 0, 4, 1).
crosses(210, 2, 6, 1).
crosses(211, 4, 6, 2).
crosses(211, 6, 8, 2).
crosses(211, 8, 10, 1).
crosses(211, 3, 6, 2).
crosses(211, 4, 7, 1).
crosses(211, 5, 8, 1).
crosses(211, 6, 9, 1).
crosses(211, 2, 6, 2).
crosses(211, 3, 7, 1).
crosses(211, 4, 8, 2).
crosses(211, 6, 10, 1).
crosses(211, 1, 6, 2).
crosses(211, 2, 7, 1).
crosses(211, 3, 8, 2).
crosses(211, 4, 9, 1).
crosses(211, 0, 6, 1).
crosses(211, 1, 7, 1).
crosses(211, 2, 8, 2).
crosses(211, 3, 9, 1).
crosses(211, 4, 10, 1).
crosses(211, 1, 8, 2).
crosses(211, 2, 9, 1).
crosses(211, 3, 10, 1).
crosses(211, 0, 8, 1).
crosses(211, 1, 9, 1).
crosses(211, 2, 10, 1).
crosses(211, 1, 10, 1).
crosses(212, 0, 2, 1).
crosses(212, 3, 5, 2).
crosses(212, 7, 9, 2).
crosses(212, 9, 11, 2).
crosses(212, 11, 13, 1).
crosses(212, 13, 15, 2).
crosses(212, 15, 17, 1).
crosses(212, 0, 3, 1).
crosses(212, 2, 5, 2).
crosses(212, 3, 6, 2).
crosses(212, 6, 9, 2).
crosses(212, 7, 10, 1).
crosses(212, 8, 11, 1).
crosses(212, 9, 12, 2).
crosses(212, 10, 13, 1).
crosses(212, 12, 15, 1).
crosses(212, 13, 16, 1).
crosses(212, 1, 5, 1).
crosses(212, 2, 6, 2).
crosses(212, 3, 7, 2).
crosses(212, 5, 9, 2).
crosses(212, 6, 10, 1).
crosses(212, 7, 11, 2).
crosses(212, 8, 12, 1).
crosses(212, 9, 13, 3).
crosses(212, 11, 15, 1).
crosses(212, 13, 17, 1).
crosses(212, 0, 5, 1).
crosses(212, 1, 6, 1).
crosses(212, 2, 7, 2).
crosses(212, 3, 8, 1).
crosses(212, 4, 9, 1).
crosses(212, 5, 10, 1).
crosses(212, 6, 11, 2).
crosses(212, 7, 12, 2).
crosses(212, 8, 13, 2).
crosses(212, 9, 14, 2).
crosses(212, 10, 15, 1).
crosses(212, 0, 6, 1).
crosses(212, 1, 7, 1).
crosses(212, 2, 8, 1).
crosses(212, 3, 9, 2).
crosses(212, 5, 11, 2).
crosses(212, 6, 12, 2).
crosses(212, 7, 13, 3).
crosses(212, 8, 14, 1).
crosses(212, 9, 15, 3).
crosses(212, 0, 7, 1).
crosses(212, 2, 9, 2).
crosses(212, 3, 10, 1).
crosses(212, 4, 11, 1).
crosses(212, 5, 12, 2).
crosses(212, 6, 13, 3).
crosses(212, 7, 14, 2).
crosses(212, 8, 15, 2).
crosses(212, 9, 16, 2).
crosses(212, 1, 9, 1).
crosses(212, 2, 10, 1).
crosses(212, 3, 11, 2).
crosses(212, 4, 12, 1).
crosses(212, 5, 13, 3).
crosses(212, 6, 14, 2).
crosses(212, 7, 15, 3).
crosses(212, 8, 16, 1).
crosses(212, 9, 17, 2).
crosses(212, 0, 9, 1).
crosses(212, 2, 11, 2).
crosses(212, 3, 12, 2).
crosses(212, 4, 13, 2).
crosses(212, 5, 14, 2).
crosses(212, 6, 15, 3).
crosses(212, 7, 16, 2).
crosses(212, 8, 17, 1).
crosses(212, 1, 11, 1).
crosses(212, 2, 12, 2).
crosses(212, 3, 13, 3).
crosses(212, 4, 14, 1).
crosses(212, 5, 15, 3).
crosses(212, 6, 16, 2).
crosses(212, 7, 17, 2).
crosses(212, 1, 12, 1).
crosses(212, 2, 13, 3).
crosses(212, 3, 14, 2).
crosses(212, 4, 15, 2).
crosses(212, 5, 16, 2).
crosses(212, 6, 17, 2).
crosses(212, 1, 13, 2).
crosses(212, 2, 14, 2).
crosses(212, 3, 15, 3).
crosses(212, 4, 16, 1).
crosses(212, 5, 17, 2).
crosses(212, 0, 13, 1).
crosses(212, 1, 14, 1).
crosses(212, 2, 15, 3).
crosses(212, 3, 16, 2).
crosses(212, 4, 17, 1).
crosses(212, 1, 15, 2).
crosses(212, 2, 16, 2).
crosses(212, 3, 17, 2).
crosses(212, 0, 15, 1).
crosses(212, 1, 16, 1).
crosses(212, 2, 17, 2).
crosses(212, 1, 17, 1).
crosses(213, 0, 2, 1).
crosses(213, 3, 5, 1).
crosses(213, 5, 7, 3).
crosses(213, 7, 9, 2).
crosses(213, 9, 11, 1).
crosses(213, 0, 3, 1).
crosses(213, 2, 5, 1).
crosses(213, 4, 7, 2).
crosses(213, 5, 8, 2).
crosses(213, 6, 9, 1).
crosses(213, 7, 10, 1).
crosses(213, 0, 4, 1).
crosses(213, 1, 5, 1).
crosses(213, 3, 7, 2).
crosses(213, 4, 8, 1).
crosses(213, 5, 9, 3).
crosses(213, 7, 11, 1).
crosses(213, 0, 5, 2).
crosses(213, 2, 7, 2).
crosses(213, 3, 8, 1).
crosses(213, 4, 9, 2).
crosses(213, 5, 10, 2).
crosses(213, 1, 7, 1).
crosses(213, 2, 8, 1).
crosses(213, 3, 9, 2).
crosses(213, 4, 10, 1).
crosses(213, 5, 11, 2).
crosses(213, 0, 7, 1).
crosses(213, 2, 9, 2).
crosses(213, 3, 10, 1).
crosses(213, 4, 11, 1).
crosses(213, 1, 9, 1).
crosses(213, 2, 10, 1).
crosses(213, 3, 11, 1).
crosses(213, 0, 9, 1).
crosses(213, 2, 11, 1).
crosses(214, 1, 3, 2).
crosses(214, 3, 5, 2).
crosses(214, 5, 7, 1).
crosses(214, 0, 3, 1).
crosses(214, 1, 4, 1).
crosses(214, 2, 5, 1).
crosses(214, 3, 6, 1).
crosses(214, 1, 5, 2).
crosses(214, 3, 7, 1).
crosses(214, 0, 5, 1).
crosses(214, 1, 6, 1).
crosses(214, 1, 7, 1).
crosses(215, 0, 2, 1).
crosses(215, 3, 5, 1).
crosses(215, 5, 7, 3).
crosses(215, 7, 9, 2).
crosses(215, 9, 11, 1).
crosses(215, 0, 3, 1).
crosses(215, 2, 5, 1).
crosses(215, 4, 7, 2).
crosses(215, 5, 8, 2).
crosses(215, 6, 9, 1).
crosses(215, 7, 10, 1).
crosses(215, 0, 4, 1).
crosses(215, 1, 5, 1).
crosses(215, 3, 7, 2).
crosses(215, 4, 8, 1).
crosses(215, 5, 9, 3).
crosses(215, 7, 11, 1).
crosses(215, 0, 5, 2).
crosses(215, 2, 7, 2).
crosses(215, 3, 8, 1).
crosses(215, 4, 9, 2).
crosses(215, 5, 10, 2).
crosses(215, 1, 7, 1).
crosses(215, 2, 8, 1).
crosses(215, 3, 9, 2).
crosses(215, 4, 10, 1).
crosses(215, 5, 11, 2).
crosses(215, 0, 7, 1).
crosses(215, 2, 9, 2).
crosses(215, 3, 10, 1).
crosses(215, 4, 11, 1).
crosses(215, 1, 9, 1).
crosses(215, 2, 10, 1).
crosses(215, 3, 11, 1).
crosses(215, 0, 9, 1).
crosses(215, 2, 11, 1).
crosses(216, 1, 3, 2).
crosses(216, 3, 5, 2).
crosses(216, 5, 7, 1).
crosses(216, 0, 3, 1).
crosses(216, 1, 4, 1).
crosses(216, 2, 5, 1).
crosses(216, 3, 6, 1).
crosses(216, 1, 5, 2).
crosses(216, 3, 7, 1).
crosses(216, 0, 5, 1).
crosses(216, 1, 6, 1).
crosses(216, 1, 7, 1).
crosses(217, 3, 5, 1).
crosses(217, 5, 7, 2).
crosses(217, 6, 8, 1).
crosses(217, 7, 9, 1).
crosses(217, 9, 11, 2).
crosses(217, 2, 5, 1).
crosses(217, 4, 7, 1).
crosses(217, 5, 8, 3).
crosses(217, 6, 9, 2).
crosses(217, 8, 11, 1).
crosses(217, 1, 5, 1).
crosses(217, 3, 7, 1).
crosses(217, 4, 8, 2).
crosses(217, 5, 9, 4).
crosses(217, 0, 5, 1).
crosses(217, 2, 7, 1).
crosses(217, 3, 8, 2).
crosses(217, 4, 9, 3).
crosses(217, 5, 10, 2).
crosses(217, 1, 7, 1).
crosses(217, 2, 8, 2).
crosses(217, 3, 9, 3).
crosses(217, 4, 10, 1).
crosses(217, 5, 11, 2).
crosses(217, 1, 8, 2).
crosses(217, 2, 9, 3).
crosses(217, 3, 10, 1).
crosses(217, 4, 11, 1).
crosses(217, 0, 8, 1).
crosses(217, 1, 9, 3).
crosses(217, 2, 10, 1).
crosses(217, 3, 11, 1).
crosses(217, 0, 9, 2).
crosses(217, 1, 10, 1).
crosses(217, 2, 11, 1).
crosses(217, 1, 11, 1).
crosses(218, 2, 4, 1).
crosses(218, 3, 5, 1).
crosses(218, 4, 6, 1).
crosses(218, 6, 8, 2).
crosses(218, 1, 4, 1).
crosses(218, 2, 5, 2).
crosses(218, 3, 6, 2).
crosses(218, 5, 8, 1).
crosses(218, 1, 5, 2).
crosses(218, 2, 6, 3).
crosses(218, 0, 5, 1).
crosses(218, 1, 6, 3).
crosses(218, 2, 7, 1).
crosses(218, 0, 6, 2).
crosses(218, 1, 7, 1).
crosses(218, 2, 8, 1).
crosses(218, 1, 8, 1).
crosses(219, 3, 5, 1).
crosses(219, 5, 7, 2).
crosses(219, 6, 8, 1).
crosses(219, 7, 9, 1).
crosses(219, 9, 11, 2).
crosses(219, 2, 5, 1).
crosses(219, 4, 7, 1).
crosses(219, 5, 8, 3).
crosses(219, 6, 9, 2).
crosses(219, 8, 11, 1).
crosses(219, 1, 5, 1).
crosses(219, 3, 7, 1).
crosses(219, 4, 8, 2).
crosses(219, 5, 9, 4).
crosses(219, 0, 5, 1).
crosses(219, 2, 7, 1).
crosses(219, 3, 8, 2).
crosses(219, 4, 9, 3).
crosses(219, 5, 10, 2).
crosses(219, 1, 7, 1).
crosses(219, 2, 8, 2).
crosses(219, 3, 9, 3).
crosses(219, 4, 10, 1).
crosses(219, 5, 11, 2).
crosses(219, 1, 8, 2).
crosses(219, 2, 9, 3).
crosses(219, 3, 10, 1).
crosses(219, 4, 11, 1).
crosses(219, 0, 8, 1).
crosses(219, 1, 9, 3).
crosses(219, 2, 10, 1).
crosses(219, 3, 11, 1).
crosses(219, 0, 9, 2).
crosses(219, 1, 10, 1).
crosses(219, 2, 11, 1).
crosses(219, 1, 11, 1).
crosses(220, 2, 4, 1).
crosses(220, 3, 5, 1).
crosses(220, 4, 6, 1).
crosses(220, 6, 8, 2).
crosses(220, 1, 4, 1).
crosses(220, 2, 5, 2).
crosses(220, 3, 6, 2).
crosses(220, 5, 8, 1).
crosses(220, 1, 5, 2).
crosses(220, 2, 6, 3).
crosses(220, 0, 5, 1).
crosses(220, 1, 6, 3).
crosses(220, 2, 7, 1).
crosses(220, 0, 6, 2).
crosses(220, 1, 7, 1).
crosses(220, 2, 8, 1).
crosses(220, 1, 8, 1).
crosses(221, 3, 5, 1).
crosses(221, 5, 7, 2).
crosses(221, 6, 8, 1).
crosses(221, 7, 9, 1).
crosses(221, 9, 11, 2).
crosses(221, 2, 5, 1).
crosses(221, 4, 7, 1).
crosses(221, 5, 8, 3).
crosses(221, 6, 9, 2).
crosses(221, 8, 11, 1).
crosses(221, 1, 5, 1).
crosses(221, 3, 7, 1).
crosses(221, 4, 8, 2).
crosses(221, 5, 9, 4).
crosses(221, 0, 5, 1).
crosses(221, 2, 7, 1).
crosses(221, 3, 8, 2).
crosses(221, 4, 9, 3).
crosses(221, 5, 10, 2).
crosses(221, 1, 7, 1).
crosses(221, 2, 8, 2).
crosses(221, 3, 9, 3).
crosses(221, 4, 10, 1).
crosses(221, 5, 11, 2).
crosses(221, 1, 8, 2).
crosses(221, 2, 9, 3).
crosses(221, 3, 10, 1).
crosses(221, 4, 11, 1).
crosses(221, 0, 8, 1).
crosses(221, 1, 9, 3).
crosses(221, 2, 10, 1).
crosses(221, 3, 11, 1).
crosses(221, 0, 9, 2).
crosses(221, 1, 10, 1).
crosses(221, 2, 11, 1).
crosses(221, 1, 11, 1).
crosses(222, 3, 5, 1).
crosses(222, 6, 8, 2).
crosses(222, 7, 9, 1).
crosses(222, 8, 10, 1).
crosses(222, 10, 12, 2).
crosses(222, 2, 5, 1).
crosses(222, 3, 6, 1).
crosses(222, 5, 8, 2).
crosses(222, 6, 9, 3).
crosses(222, 7, 10, 2).
crosses(222, 9, 12, 1).
crosses(222, 1, 5, 1).
crosses(222, 2, 6, 1).
crosses(222, 4, 8, 1).
crosses(222, 5, 9, 3).
crosses(222, 6, 10, 4).
crosses(222, 0, 5, 1).
crosses(222, 1, 6, 1).
crosses(222, 3, 8, 1).
crosses(222, 4, 9, 2).
crosses(222, 5, 10, 4).
crosses(222, 6, 11, 2).
crosses(222, 0, 6, 1).
crosses(222, 2, 8, 1).
crosses(222, 3, 9, 2).
crosses(222, 4, 10, 3).
crosses(222, 5, 11, 2).
crosses(222, 6, 12, 2).
crosses(222, 1, 8, 1).
crosses(222, 2, 9, 2).
crosses(222, 3, 10, 3).
crosses(222, 4, 11, 1).
crosses(222, 5, 12, 2).
crosses(222, 1, 9, 2).
crosses(222, 2, 10, 3).
crosses(222, 3, 11, 1).
crosses(222, 4, 12, 1).
crosses(222, 0, 9, 1).
crosses(222, 1, 10, 3).
crosses(222, 2, 11, 1).
crosses(222, 3, 12, 1).
crosses(222, 0, 10, 2).
crosses(222, 1, 11, 1).
crosses(222, 2, 12, 1).
crosses(222, 1, 12, 1).
crosses(223, 1, 3, 1).
crosses(223, 2, 4, 1).
crosses(223, 3, 5, 1).
crosses(223, 4, 6, 1).
crosses(223, 5, 7, 1).
crosses(223, 6, 8, 1).
crosses(223, 11, 13, 5).
crosses(223, 1, 4, 2).
crosses(223, 2, 5, 2).
crosses(223, 3, 6, 2).
crosses(223, 4, 7, 2).
crosses(223, 5, 8, 2).
crosses(223, 6, 9, 1).
crosses(223, 10, 13, 5).
crosses(223, 0, 4, 1).
crosses(223, 1, 5, 3).
crosses(223, 2, 6, 3).
crosses(223, 3, 7, 3).
crosses(223, 4, 8, 3).
crosses(223, 5, 9, 2).
crosses(223, 6, 10, 1).
crosses(223, 9, 13, 5).
crosses(223, 0, 5, 2).
crosses(223, 1, 6, 4).
crosses(223, 2, 7, 4).
crosses(223, 3, 8, 4).
crosses(223, 4, 9, 3).
crosses(223, 5, 10, 2).
crosses(223, 6, 11, 1).
crosses(223, 8, 13, 5).
crosses(223, 0, 6, 3).
crosses(223, 1, 7, 5).
crosses(223, 2, 8, 5).
crosses(223, 3, 9, 4).
crosses(223, 4, 10, 3).
crosses(223, 5, 11, 2).
crosses(223, 7, 13, 4).
crosses(223, 0, 7, 4).
crosses(223, 1, 8, 6).
crosses(223, 2, 9, 5).
crosses(223, 3, 10, 4).
crosses(223, 4, 11, 3).
crosses(223, 6, 13, 3).
crosses(223, 0, 8, 5).
crosses(223, 1, 9, 6).
crosses(223, 2, 10, 5).
crosses(223, 3, 11, 4).
crosses(223, 5, 13, 2).
crosses(223, 0, 9, 5).
crosses(223, 1, 10, 6).
crosses(223, 2, 11, 5).
crosses(223, 4, 13, 1).
crosses(223, 0, 10, 5).
crosses(223, 1, 11, 6).
crosses(223, 0, 11, 5).
crosses(223, 1, 12, 1).
crosses(223, 1, 13, 1).
crosses(224, 1, 3, 2).
crosses(224, 3, 5, 2).
crosses(224, 6, 8, 2).
crosses(224, 11, 13, 1).
crosses(224, 0, 3, 1).
crosses(224, 1, 4, 1).
crosses(224, 2, 5, 1).
crosses(224, 3, 6, 2).
crosses(224, 5, 8, 2).
crosses(224, 6, 9, 2).
crosses(224, 10, 13, 1).
crosses(224, 1, 5, 2).
crosses(224, 2, 6, 1).
crosses(224, 3, 7, 1).
crosses(224, 4, 8, 1).
crosses(224, 5, 9, 2).
crosses(224, 6, 10, 2).
crosses(224, 9, 13, 1).
crosses(224, 0, 5, 1).
crosses(224, 1, 6, 2).
crosses(224, 3, 8, 2).
crosses(224, 4, 9, 1).
crosses(224, 5, 10, 2).
crosses(224, 6, 11, 2).
crosses(224, 8, 13, 1).
crosses(224, 0, 6, 1).
crosses(224, 1, 7, 1).
crosses(224, 2, 8, 1).
crosses(224, 3, 9, 2).
crosses(224, 4, 10, 1).
crosses(224, 5, 11, 2).
crosses(224, 6, 12, 1).
crosses(224, 1, 8, 2).
crosses(224, 2, 9, 1).
crosses(224, 3, 10, 2).
crosses(224, 4, 11, 1).
crosses(224, 5, 12, 1).
crosses(224, 6, 13, 1).
crosses(224, 0, 8, 1).
crosses(224, 1, 9, 2).
crosses(224, 2, 10, 1).
crosses(224, 3, 11, 2).
crosses(224, 5, 13, 1).
crosses(224, 0, 9, 1).
crosses(224, 1, 10, 2).
crosses(224, 2, 11, 1).
crosses(224, 3, 12, 1).
crosses(224, 0, 10, 1).
crosses(224, 1, 11, 2).
crosses(224, 3, 13, 1).
crosses(224, 0, 11, 1).
crosses(224, 1, 12, 1).
crosses(224, 1, 13, 1).
crosses(225, 1, 3, 1).
crosses(225, 2, 4, 1).
crosses(225, 4, 6, 2).
crosses(225, 5, 7, 1).
crosses(225, 7, 9, 5).
crosses(225, 9, 11, 2).
crosses(225, 10, 12, 1).
crosses(225, 13, 15, 2).
crosses(225, 0, 3, 1).
crosses(225, 1, 4, 2).
crosses(225, 3, 6, 1).
crosses(225, 4, 7, 3).
crosses(225, 6, 9, 4).
crosses(225, 7, 10, 4).
crosses(225, 8, 11, 1).
crosses(225, 9, 12, 3).
crosses(225, 10, 13, 1).
crosses(225, 12, 15, 2).
crosses(225, 0, 4, 2).
crosses(225, 1, 5, 1).
crosses(225, 2, 6, 1).
crosses(225, 3, 7, 2).
crosses(225, 4, 8, 1).
crosses(225, 5, 9, 3).
crosses(225, 6, 10, 3).
crosses(225, 7, 11, 5).
crosses(225, 8, 12, 2).
crosses(225, 9, 13, 3).
crosses(225, 11, 15, 1).
crosses(225, 0, 5, 1).
crosses(225, 1, 6, 2).
crosses(225, 2, 7, 2).
crosses(225, 4, 9, 4).
crosses(225, 5, 10, 2).
crosses(225, 6, 11, 4).
crosses(225, 7, 12, 6).
crosses(225, 8, 13, 2).
crosses(225, 9, 14, 1).
crosses(225, 0, 6, 2).
crosses(225, 1, 7, 3).
crosses(225, 3, 9, 3).
crosses(225, 4, 10, 3).
crosses(225, 5, 11, 3).
crosses(225, 6, 12, 5).
crosses(225, 7, 13, 6).
crosses(225, 9, 15, 1).
crosses(225, 0, 7, 3).
crosses(225, 2, 9, 2).
crosses(225, 3, 10, 2).
crosses(225, 4, 11, 4).
crosses(225, 5, 12, 4).
crosses(225, 6, 13, 5).
crosses(225, 7, 14, 4).
crosses(225, 1, 9, 2).
crosses(225, 2, 10, 1).
crosses(225, 3, 11, 3).
crosses(225, 4, 12, 5).
crosses(225, 5, 13, 4).
crosses(225, 6, 14, 3).
crosses(225, 7, 15, 4).
crosses(225, 0, 9, 1).
crosses(225, 1, 10, 1).
crosses(225, 2, 11, 2).
crosses(225, 3, 12, 4).
crosses(225, 4, 13, 5).
crosses(225, 5, 14, 2).
crosses(225, 6, 15, 3).
crosses(225, 1, 11, 2).
crosses(225, 2, 12, 3).
crosses(225, 3, 13, 4).
crosses(225, 4, 14, 3).
crosses(225, 5, 15, 2).
crosses(225, 0, 11, 1).
crosses(225, 1, 12, 3).
crosses(225, 2, 13, 3).
crosses(225, 3, 14, 2).
crosses(225, 4, 15, 3).
crosses(225, 0, 12, 2).
crosses(225, 1, 13, 3).
crosses(225, 2, 14, 1).
crosses(225, 3, 15, 2).
crosses(225, 0, 13, 2).
crosses(225, 1, 14, 1).
crosses(225, 2, 15, 1).
crosses(225, 1, 15, 1).
crosses(226, 0, 2, 1).
crosses(226, 1, 3, 1).
crosses(226, 3, 5, 4).
crosses(226, 5, 7, 2).
crosses(226, 6, 8, 1).
crosses(226, 9, 11, 2).
crosses(226, 0, 3, 2).
crosses(226, 2, 5, 3).
crosses(226, 3, 6, 3).
crosses(226, 4, 7, 1).
crosses(226, 5, 8, 3).
crosses(226, 6, 9, 1).
crosses(226, 8, 11, 2).
crosses(226, 1, 5, 2).
crosses(226, 2, 6, 2).
crosses(226, 3, 7, 4).
crosses(226, 4, 8, 2).
crosses(226, 5, 9, 3).
crosses(226, 7, 11, 1).
crosses(226, 0, 5, 1).
crosses(226, 1, 6, 1).
crosses(226, 2, 7, 3).
crosses(226, 3, 8, 5).
crosses(226, 4, 9, 2).
crosses(226, 5, 10, 1).
crosses(226, 1, 7, 2).
crosses(226, 2, 8, 4).
crosses(226, 3, 9, 5).
crosses(226, 5, 11, 1).
crosses(226, 0, 7, 1).
crosses(226, 1, 8, 3).
crosses(226, 2, 9, 4).
crosses(226, 3, 10, 3).
crosses(226, 0, 8, 2).
crosses(226, 1, 9, 3).
crosses(226, 2, 10, 2).
crosses(226, 3, 11, 3).
crosses(226, 0, 9, 2).
crosses(226, 1, 10, 1).
crosses(226, 2, 11, 2).
crosses(226, 1, 11, 1).
crosses(227, 2, 4, 1).
crosses(227, 3, 5, 1).
crosses(227, 5, 7, 2).
crosses(227, 6, 8, 1).
crosses(227, 8, 10, 5).
crosses(227, 10, 12, 2).
crosses(227, 11, 13, 1).
crosses(227, 14, 16, 2).
crosses(227, 1, 4, 1).
crosses(227, 2, 5, 2).
crosses(227, 4, 7, 1).
crosses(227, 5, 8, 3).
crosses(227, 7, 10, 4).
crosses(227, 8, 11, 4).
crosses(227, 9, 12, 1).
crosses(227, 10, 13, 3).
crosses(227, 11, 14, 1).
crosses(227, 13, 16, 2).
crosses(227, 0, 4, 1).
crosses(227, 1, 5, 2).
crosses(227, 2, 6, 1).
crosses(227, 3, 7, 1).
crosses(227, 4, 8, 2).
crosses(227, 5, 9, 1).
crosses(227, 6, 10, 3).
crosses(227, 7, 11, 3).
crosses(227, 8, 12, 5).
crosses(227, 9, 13, 2).
crosses(227, 10, 14, 3).
crosses(227, 12, 16, 1).
crosses(227, 0, 5, 2).
crosses(227, 1, 6, 1).
crosses(227, 2, 7, 2).
crosses(227, 3, 8, 2).
crosses(227, 5, 10, 4).
crosses(227, 6, 11, 2).
crosses(227, 7, 12, 4).
crosses(227, 8, 13, 6).
crosses(227, 9, 14, 2).
crosses(227, 10, 15, 1).
crosses(227, 0, 6, 1).
crosses(227, 1, 7, 2).
crosses(227, 2, 8, 3).
crosses(227, 4, 10, 3).
crosses(227, 5, 11, 3).
crosses(227, 6, 12, 3).
crosses(227, 7, 13, 5).
crosses(227, 8, 14, 6).
crosses(227, 10, 16, 1).
crosses(227, 0, 7, 2).
crosses(227, 1, 8, 3).
crosses(227, 3, 10, 2).
crosses(227, 4, 11, 2).
crosses(227, 5, 12, 4).
crosses(227, 6, 13, 4).
crosses(227, 7, 14, 5).
crosses(227, 8, 15, 4).
crosses(227, 0, 8, 3).
crosses(227, 2, 10, 2).
crosses(227, 3, 11, 1).
crosses(227, 4, 12, 3).
crosses(227, 5, 13, 5).
crosses(227, 6, 14, 4).
crosses(227, 7, 15, 3).
crosses(227, 8, 16, 4).
crosses(227, 1, 10, 2).
crosses(227, 2, 11, 1).
crosses(227, 3, 12, 2).
crosses(227, 4, 13, 4).
crosses(227, 5, 14, 5).
crosses(227, 6, 15, 2).
crosses(227, 7, 16, 3).
crosses(227, 0, 10, 1).
crosses(227, 1, 11, 1).
crosses(227, 2, 12, 2).
crosses(227, 3, 13, 3).
crosses(227, 4, 14, 4).
crosses(227, 5, 15, 3).
crosses(227, 6, 16, 2).
crosses(227, 1, 12, 2).
crosses(227, 2, 13, 3).
crosses(227, 3, 14, 3).
crosses(227, 4, 15, 2).
crosses(227, 5, 16, 3).
crosses(227, 0, 12, 1).
crosses(227, 1, 13, 3).
crosses(227, 2, 14, 3).
crosses(227, 3, 15, 1).
crosses(227, 4, 16, 2).
crosses(227, 0, 13, 2).
crosses(227, 1, 14, 3).
crosses(227, 2, 15, 1).
crosses(227, 3, 16, 1).
crosses(227, 0, 14, 2).
crosses(227, 1, 15, 1).
crosses(227, 2, 16, 1).
crosses(227, 1, 16, 1).
crosses(228, 0, 2, 1).
crosses(228, 1, 3, 1).
crosses(228, 3, 5, 4).
crosses(228, 5, 7, 2).
crosses(228, 6, 8, 1).
crosses(228, 9, 11, 2).
crosses(228, 0, 3, 2).
crosses(228, 2, 5, 3).
crosses(228, 3, 6, 3).
crosses(228, 4, 7, 1).
crosses(228, 5, 8, 3).
crosses(228, 6, 9, 1).
crosses(228, 8, 11, 2).
crosses(228, 1, 5, 2).
crosses(228, 2, 6, 2).
crosses(228, 3, 7, 4).
crosses(228, 4, 8, 2).
crosses(228, 5, 9, 3).
crosses(228, 7, 11, 1).
crosses(228, 0, 5, 1).
crosses(228, 1, 6, 1).
crosses(228, 2, 7, 3).
crosses(228, 3, 8, 5).
crosses(228, 4, 9, 2).
crosses(228, 5, 10, 1).
crosses(228, 1, 7, 2).
crosses(228, 2, 8, 4).
crosses(228, 3, 9, 5).
crosses(228, 5, 11, 1).
crosses(228, 0, 7, 1).
crosses(228, 1, 8, 3).
crosses(228, 2, 9, 4).
crosses(228, 3, 10, 3).
crosses(228, 0, 8, 2).
crosses(228, 1, 9, 3).
crosses(228, 2, 10, 2).
crosses(228, 3, 11, 3).
crosses(228, 0, 9, 2).
crosses(228, 1, 10, 1).
crosses(228, 2, 11, 2).
crosses(228, 1, 11, 1).
crosses(229, 1, 3, 1).
crosses(229, 2, 4, 1).
crosses(229, 4, 6, 2).
crosses(229, 5, 7, 1).
crosses(229, 7, 9, 5).
crosses(229, 10, 12, 2).
crosses(229, 11, 13, 1).
crosses(229, 14, 16, 2).
crosses(229, 0, 3, 1).
crosses(229, 1, 4, 2).
crosses(229, 3, 6, 1).
crosses(229, 4, 7, 3).
crosses(229, 6, 9, 4).
crosses(229, 7, 10, 5).
crosses(229, 9, 12, 2).
crosses(229, 10, 13, 3).
crosses(229, 11, 14, 1).
crosses(229, 13, 16, 2).
crosses(229, 0, 4, 2).
crosses(229, 1, 5, 1).
crosses(229, 2, 6, 1).
crosses(229, 3, 7, 2).
crosses(229, 4, 8, 1).
crosses(229, 5, 9, 3).
crosses(229, 6, 10, 4).
crosses(229, 7, 11, 4).
crosses(229, 8, 12, 1).
crosses(229, 9, 13, 3).
crosses(229, 10, 14, 3).
crosses(229, 12, 16, 1).
crosses(229, 0, 5, 1).
crosses(229, 1, 6, 2).
crosses(229, 2, 7, 2).
crosses(229, 4, 9, 4).
crosses(229, 5, 10, 3).
crosses(229, 6, 11, 3).
crosses(229, 7, 12, 5).
crosses(229, 8, 13, 2).
crosses(229, 9, 14, 3).
crosses(229, 10, 15, 1).
crosses(229, 0, 6, 2).
crosses(229, 1, 7, 3).
crosses(229, 3, 9, 3).
crosses(229, 4, 10, 4).
crosses(229, 5, 11, 2).
crosses(229, 6, 12, 4).
crosses(229, 7, 13, 6).
crosses(229, 8, 14, 2).
crosses(229, 9, 15, 1).
crosses(229, 10, 16, 1).
crosses(229, 0, 7, 3).
crosses(229, 2, 9, 2).
crosses(229, 3, 10, 3).
crosses(229, 4, 11, 3).
crosses(229, 5, 12, 3).
crosses(229, 6, 13, 5).
crosses(229, 7, 14, 6).
crosses(229, 9, 16, 1).
crosses(229, 1, 9, 2).
crosses(229, 2, 10, 2).
crosses(229, 3, 11, 2).
crosses(229, 4, 12, 4).
crosses(229, 5, 13, 4).
crosses(229, 6, 14, 5).
crosses(229, 7, 15, 4).
crosses(229, 0, 9, 1).
crosses(229, 1, 10, 2).
crosses(229, 2, 11, 1).
crosses(229, 3, 12, 3).
crosses(229, 4, 13, 5).
crosses(229, 5, 14, 4).
crosses(229, 6, 15, 3).
crosses(229, 7, 16, 4).
crosses(229, 0, 10, 1).
crosses(229, 1, 11, 1).
crosses(229, 2, 12, 2).
crosses(229, 3, 13, 4).
crosses(229, 4, 14, 5).
crosses(229, 5, 15, 2).
crosses(229, 6, 16, 3).
crosses(229, 1, 12, 2).
crosses(229, 2, 13, 3).
crosses(229, 3, 14, 4).
crosses(229, 4, 15, 3).
crosses(229, 5, 16, 2).
crosses(229, 0, 12, 1).
crosses(229, 1, 13, 3).
crosses(229, 2, 14, 3).
crosses(229, 3, 15, 2).
crosses(229, 4, 16, 3).
crosses(229, 0, 13, 2).
crosses(229, 1, 14, 3).
crosses(229, 2, 15, 1).
crosses(229, 3, 16, 2).
crosses(229, 0, 14, 2).
crosses(229, 1, 15, 1).
crosses(229, 2, 16, 1).
crosses(229, 1, 16, 1).
crosses(230, 0, 2, 1).
crosses(230, 1, 3, 1).
crosses(230, 3, 5, 4).
crosses(230, 5, 7, 2).
crosses(230, 6, 8, 1).
crosses(230, 9, 11, 2).
crosses(230, 0, 3, 2).
crosses(230, 2, 5, 3).
crosses(230, 3, 6, 3).
crosses(230, 4, 7, 1).
crosses(230, 5, 8, 3).
crosses(230, 6, 9, 1).
crosses(230, 8, 11, 2).
crosses(230, 1, 5, 2).
crosses(230, 2, 6, 2).
crosses(230, 3, 7, 4).
crosses(230, 4, 8, 2).
crosses(230, 5, 9, 3).
crosses(230, 7, 11, 1).
crosses(230, 0, 5, 1).
crosses(230, 1, 6, 1).
crosses(230, 2, 7, 3).
crosses(230, 3, 8, 5).
crosses(230, 4, 9, 2).
crosses(230, 5, 10, 1).
crosses(230, 1, 7, 2).
crosses(230, 2, 8, 4).
crosses(230, 3, 9, 5).
crosses(230, 5, 11, 1).
crosses(230, 0, 7, 1).
crosses(230, 1, 8, 3).
crosses(230, 2, 9, 4).
crosses(230, 3, 10, 3).
crosses(230, 0, 8, 2).
crosses(230, 1, 9, 3).
crosses(230, 2, 10, 2).
crosses(230, 3, 11, 3).
crosses(230, 0, 9, 2).
crosses(230, 1, 10, 1).
crosses(230, 2, 11, 2).
crosses(230, 1, 11, 1).
crosses(231, 1, 3, 1).
crosses(231, 3, 5, 3).
crosses(231, 5, 7, 2).
crosses(231, 6, 8, 1).
crosses(231, 8, 10, 1).
crosses(231, 9, 11, 1).
crosses(231, 11, 13, 4).
crosses(231, 0, 3, 1).
crosses(231, 2, 5, 2).
crosses(231, 3, 6, 2).
crosses(231, 4, 7, 1).
crosses(231, 5, 8, 3).
crosses(231, 6, 9, 1).
crosses(231, 7, 10, 1).
crosses(231, 8, 11, 2).
crosses(231, 10, 13, 3).
crosses(231, 1, 5, 2).
crosses(231, 2, 6, 1).
crosses(231, 3, 7, 3).
crosses(231, 4, 8, 2).
crosses(231, 5, 9, 3).
crosses(231, 6, 10, 2).
crosses(231, 7, 11, 2).
crosses(231, 9, 13, 2).
crosses(231, 0, 5, 1).
crosses(231, 1, 6, 1).
crosses(231, 2, 7, 2).
crosses(231, 3, 8, 4).
crosses(231, 4, 9, 2).
crosses(231, 5, 10, 4).
crosses(231, 6, 11, 3).
crosses(231, 8, 13, 2).
crosses(231, 1, 7, 2).
crosses(231, 2, 8, 3).
crosses(231, 3, 9, 4).
crosses(231, 4, 10, 3).
crosses(231, 5, 11, 5).
crosses(231, 7, 13, 1).
crosses(231, 0, 7, 1).
crosses(231, 1, 8, 3).
crosses(231, 2, 9, 3).
crosses(231, 3, 10, 5).
crosses(231, 4, 11, 4).
crosses(231, 5, 12, 1).
crosses(231, 0, 8, 2).
crosses(231, 1, 9, 3).
crosses(231, 2, 10, 4).
crosses(231, 3, 11, 6).
crosses(231, 5, 13, 1).
crosses(231, 0, 9, 2).
crosses(231, 1, 10, 4).
crosses(231, 2, 11, 5).
crosses(231, 3, 12, 2).
crosses(231, 0, 10, 3).
crosses(231, 1, 11, 5).
crosses(231, 2, 12, 1).
crosses(231, 3, 13, 2).
crosses(231, 0, 11, 4).
crosses(231, 1, 12, 1).
crosses(231, 2, 13, 1).
crosses(231, 1, 13, 1).
crosses(232, 2, 4, 1).
crosses(232, 4, 6, 3).
crosses(232, 6, 8, 2).
crosses(232, 7, 9, 1).
crosses(232, 9, 11, 1).
crosses(232, 10, 12, 1).
crosses(232, 12, 14, 4).
crosses(232, 1, 4, 1).
crosses(232, 3, 6, 2).
crosses(232, 4, 7, 2).
crosses(232, 5, 8, 1).
crosses(232, 6, 9, 3).
crosses(232, 7, 10, 1).
crosses(232, 8, 11, 1).
crosses(232, 9, 12, 2).
crosses(232, 11, 14, 3).
crosses(232, 0, 4, 1).
crosses(232, 2, 6, 2).
crosses(232, 3, 7, 1).
crosses(232, 4, 8, 3).
crosses(232, 5, 9, 2).
crosses(232, 6, 10, 3).
crosses(232, 7, 11, 2).
crosses(232, 8, 12, 2).
crosses(232, 10, 14, 2).
crosses(232, 1, 6, 2).
crosses(232, 2, 7, 1).
crosses(232, 3, 8, 2).
crosses(232, 4, 9, 4).
crosses(232, 5, 10, 2).
crosses(232, 6, 11, 4).
crosses(232, 7, 12, 3).
crosses(232, 9, 14, 2).
crosses(232, 0, 6, 1).
crosses(232, 1, 7, 1).
crosses(232, 2, 8, 2).
crosses(232, 3, 9, 3).
crosses(232, 4, 10, 4).
crosses(232, 5, 11, 3).
crosses(232, 6, 12, 5).
crosses(232, 8, 14, 1).
crosses(232, 1, 8, 2).
crosses(232, 2, 9, 3).
crosses(232, 3, 10, 3).
crosses(232, 4, 11, 5).
crosses(232, 5, 12, 4).
crosses(232, 6, 13, 1).
crosses(232, 0, 8, 1).
crosses(232, 1, 9, 3).
crosses(232, 2, 10, 3).
crosses(232, 3, 11, 4).
crosses(232, 4, 12, 6).
crosses(232, 6, 14, 1).
crosses(232, 0, 9, 2).
crosses(232, 1, 10, 3).
crosses(232, 2, 11, 4).
crosses(232, 3, 12, 5).
crosses(232, 4, 13, 2).
crosses(232, 0, 10, 2).
crosses(232, 1, 11, 4).
crosses(232, 2, 12, 5).
crosses(232, 3, 13, 1).
crosses(232, 4, 14, 2).
crosses(232, 0, 11, 3).
crosses(232, 1, 12, 5).
crosses(232, 2, 13, 1).
crosses(232, 3, 14, 1).
crosses(232, 0, 12, 4).
crosses(232, 1, 13, 1).
crosses(232, 2, 14, 1).
crosses(232, 1, 14, 1).
crosses(233, 2, 4, 1).
crosses(233, 3, 5, 1).
crosses(233, 6, 8, 1).
crosses(233, 1, 4, 1).
crosses(233, 2, 5, 2).
crosses(233, 3, 6, 1).
crosses(233, 5, 8, 1).
crosses(233, 1, 5, 2).
crosses(233, 2, 6, 2).
crosses(233, 0, 5, 1).
crosses(233, 1, 6, 2).
crosses(233, 2, 7, 1).
crosses(233, 0, 6, 1).
crosses(233, 1, 7, 1).
crosses(233, 2, 8, 1).
crosses(233, 1, 8, 1).
crosses(234, 3, 5, 1).
crosses(234, 4, 6, 1).
crosses(234, 7, 9, 1).
crosses(234, 2, 5, 1).
crosses(234, 3, 6, 2).
crosses(234, 4, 7, 1).
crosses(234, 6, 9, 1).
crosses(234, 1, 5, 1).
crosses(234, 2, 6, 2).
crosses(234, 3, 7, 2).
crosses(234, 1, 6, 2).
crosses(234, 2, 7, 2).
crosses(234, 3, 8, 1).
crosses(234, 0, 6, 1).
crosses(234, 1, 7, 2).
crosses(234, 2, 8, 1).
crosses(234, 3, 9, 1).
crosses(234, 0, 7, 1).
crosses(234, 1, 8, 1).
crosses(234, 2, 9, 1).
crosses(234, 1, 9, 1).
crosses(235, 2, 4, 1).
crosses(235, 3, 5, 1).
crosses(235, 7, 9, 1).
crosses(235, 1, 4, 1).
crosses(235, 2, 5, 2).
crosses(235, 3, 6, 1).
crosses(235, 6, 9, 1).
crosses(235, 1, 5, 2).
crosses(235, 2, 6, 2).
crosses(235, 3, 7, 1).
crosses(235, 5, 9, 1).
crosses(235, 0, 5, 1).
crosses(235, 1, 6, 2).
crosses(235, 2, 7, 2).
crosses(235, 0, 6, 1).
crosses(235, 1, 7, 2).
crosses(235, 2, 8, 1).
crosses(235, 0, 7, 1).
crosses(235, 1, 8, 1).
crosses(235, 2, 9, 1).
crosses(235, 1, 9, 1).
crosses(236, 3, 5, 1).
crosses(236, 4, 6, 1).
crosses(236, 7, 9, 1).
crosses(236, 2, 5, 1).
crosses(236, 3, 6, 2).
crosses(236, 4, 7, 1).
crosses(236, 6, 9, 1).
crosses(236, 1, 5, 1).
crosses(236, 2, 6, 2).
crosses(236, 3, 7, 2).
crosses(236, 1, 6, 2).
crosses(236, 2, 7, 2).
crosses(236, 3, 8, 1).
crosses(236, 0, 6, 1).
crosses(236, 1, 7, 2).
crosses(236, 2, 8, 1).
crosses(236, 3, 9, 1).
crosses(236, 0, 7, 1).
crosses(236, 1, 8, 1).
crosses(236, 2, 9, 1).
crosses(236, 1, 9, 1).
crosses(237, 1, 3, 1).
crosses(237, 3, 5, 3).
crosses(237, 6, 8, 2).
crosses(237, 7, 9, 1).
crosses(237, 10, 12, 2).
crosses(237, 0, 3, 1).
crosses(237, 2, 5, 2).
crosses(237, 3, 6, 3).
crosses(237, 5, 8, 2).
crosses(237, 6, 9, 3).
crosses(237, 7, 10, 1).
crosses(237, 9, 12, 2).
crosses(237, 1, 5, 2).
crosses(237, 2, 6, 2).
crosses(237, 3, 7, 2).
crosses(237, 4, 8, 1).
crosses(237, 5, 9, 3).
crosses(237, 6, 10, 3).
crosses(237, 8, 12, 1).
crosses(237, 0, 5, 1).
crosses(237, 1, 6, 2).
crosses(237, 2, 7, 1).
crosses(237, 3, 8, 3).
crosses(237, 4, 9, 2).
crosses(237, 5, 10, 3).
crosses(237, 6, 11, 1).
crosses(237, 0, 6, 1).
crosses(237, 1, 7, 1).
crosses(237, 2, 8, 2).
crosses(237, 3, 9, 4).
crosses(237, 4, 10, 2).
crosses(237, 5, 11, 1).
crosses(237, 6, 12, 1).
crosses(237, 1, 8, 2).
crosses(237, 2, 9, 3).
crosses(237, 3, 10, 4).
crosses(237, 5, 12, 1).
crosses(237, 0, 8, 1).
crosses(237, 1, 9, 3).
crosses(237, 2, 10, 3).
crosses(237, 3, 11, 2).
crosses(237, 0, 9, 2).
crosses(237, 1, 10, 3).
crosses(237, 2, 11, 1).
crosses(237, 3, 12, 2).
crosses(237, 0, 10, 2).
crosses(237, 1, 11, 1).
crosses(237, 2, 12, 1).
crosses(237, 1, 12, 1).
crosses(238, 1, 3, 1).
crosses(238, 3, 5, 3).
crosses(238, 6, 8, 2).
crosses(238, 7, 9, 1).
crosses(238, 12, 14, 2).
crosses(238, 0, 3, 1).
crosses(238, 2, 5, 2).
crosses(238, 3, 6, 3).
crosses(238, 5, 8, 2).
crosses(238, 6, 9, 3).
crosses(238, 7, 10, 1).
crosses(238, 11, 14, 2).
crosses(238, 1, 5, 2).
crosses(238, 2, 6, 2).
crosses(238, 3, 7, 2).
crosses(238, 4, 8, 1).
crosses(238, 5, 9, 3).
crosses(238, 6, 10, 3).
crosses(238, 7, 11, 1).
crosses(238, 10, 14, 2).
crosses(238, 0, 5, 1).
crosses(238, 1, 6, 2).
crosses(238, 2, 7, 1).
crosses(238, 3, 8, 3).
crosses(238, 4, 9, 2).
crosses(238, 5, 10, 3).
crosses(238, 6, 11, 3).
crosses(238, 7, 12, 1).
crosses(238, 9, 14, 2).
crosses(238, 0, 6, 1).
crosses(238, 1, 7, 1).
crosses(238, 2, 8, 2).
crosses(238, 3, 9, 4).
crosses(238, 4, 10, 2).
crosses(238, 5, 11, 3).
crosses(238, 6, 12, 3).
crosses(238, 8, 14, 1).
crosses(238, 1, 8, 2).
crosses(238, 2, 9, 3).
crosses(238, 3, 10, 4).
crosses(238, 4, 11, 2).
crosses(238, 5, 12, 3).
crosses(238, 6, 13, 1).
crosses(238, 0, 8, 1).
crosses(238, 1, 9, 3).
crosses(238, 2, 10, 3).
crosses(238, 3, 11, 4).
crosses(238, 4, 12, 2).
crosses(238, 5, 13, 1).
crosses(238, 6, 14, 1).
crosses(238, 0, 9, 2).
crosses(238, 1, 10, 3).
crosses(238, 2, 11, 3).
crosses(238, 3, 12, 4).
crosses(238, 5, 14, 1).
crosses(238, 0, 10, 2).
crosses(238, 1, 11, 3).
crosses(238, 2, 12, 3).
crosses(238, 3, 13, 2).
crosses(238, 0, 11, 2).
crosses(238, 1, 12, 3).
crosses(238, 2, 13, 1).
crosses(238, 3, 14, 2).
crosses(238, 0, 12, 2).
crosses(238, 1, 13, 1).
crosses(238, 2, 14, 1).
crosses(238, 1, 14, 1).
crosses(239, 1, 3, 1).
crosses(239, 3, 5, 3).
crosses(239, 6, 8, 2).
crosses(239, 7, 9, 1).
crosses(239, 10, 12, 2).
crosses(239, 0, 3, 1).
crosses(239, 2, 5, 2).
crosses(239, 3, 6, 3).
crosses(239, 5, 8, 2).
crosses(239, 6, 9, 3).
crosses(239, 7, 10, 1).
crosses(239, 9, 12, 2).
crosses(239, 1, 5, 2).
crosses(239, 2, 6, 2).
crosses(239, 3, 7, 2).
crosses(239, 4, 8, 1).
crosses(239, 5, 9, 3).
crosses(239, 6, 10, 3).
crosses(239, 8, 12, 1).
crosses(239, 0, 5, 1).
crosses(239, 1, 6, 2).
crosses(239, 2, 7, 1).
crosses(239, 3, 8, 3).
crosses(239, 4, 9, 2).
crosses(239, 5, 10, 3).
crosses(239, 6, 11, 1).
crosses(239, 0, 6, 1).
crosses(239, 1, 7, 1).
crosses(239, 2, 8, 2).
crosses(239, 3, 9, 4).
crosses(239, 4, 10, 2).
crosses(239, 5, 11, 1).
crosses(239, 6, 12, 1).
crosses(239, 1, 8, 2).
crosses(239, 2, 9, 3).
crosses(239, 3, 10, 4).
crosses(239, 5, 12, 1).
crosses(239, 0, 8, 1).
crosses(239, 1, 9, 3).
crosses(239, 2, 10, 3).
crosses(239, 3, 11, 2).
crosses(239, 0, 9, 2).
crosses(239, 1, 10, 3).
crosses(239, 2, 11, 1).
crosses(239, 3, 12, 2).
crosses(239, 0, 10, 2).
crosses(239, 1, 11, 1).
crosses(239, 2, 12, 1).
crosses(239, 1, 12, 1).
crosses(240, 3, 5, 1).
crosses(240, 4, 6, 1).
crosses(240, 6, 8, 1).
crosses(240, 8, 10, 2).
crosses(240, 2, 5, 1).
crosses(240, 3, 6, 2).
crosses(240, 4, 7, 1).
crosses(240, 5, 8, 1).
crosses(240, 7, 10, 1).
crosses(240, 1, 5, 1).
crosses(240, 2, 6, 2).
crosses(240, 3, 7, 2).
crosses(240, 4, 8, 2).
crosses(240, 6, 10, 1).
crosses(240, 1, 6, 2).
crosses(240, 2, 7, 2).
crosses(240, 3, 8, 3).
crosses(240, 0, 6, 1).
crosses(240, 1, 7, 2).
crosses(240, 2, 8, 3).
crosses(240, 3, 9, 1).
crosses(240, 0, 7, 1).
crosses(240, 1, 8, 3).
crosses(240, 2, 9, 1).
crosses(240, 3, 10, 1).
crosses(240, 0, 8, 2).
crosses(240, 1, 9, 1).
crosses(240, 2, 10, 1).
crosses(240, 1, 10, 1).
crosses(241, 1, 3, 1).
crosses(241, 3, 5, 3).
crosses(241, 6, 8, 2).
crosses(241, 7, 9, 1).
crosses(241, 10, 12, 2).
crosses(241, 0, 3, 1).
crosses(241, 2, 5, 2).
crosses(241, 3, 6, 3).
crosses(241, 5, 8, 2).
crosses(241, 6, 9, 3).
crosses(241, 7, 10, 1).
crosses(241, 9, 12, 2).
crosses(241, 1, 5, 2).
crosses(241, 2, 6, 2).
crosses(241, 3, 7, 2).
crosses(241, 4, 8, 1).
crosses(241, 5, 9, 3).
crosses(241, 6, 10, 3).
crosses(241, 8, 12, 1).
crosses(241, 0, 5, 1).
crosses(241, 1, 6, 2).
crosses(241, 2, 7, 1).
crosses(241, 3, 8, 3).
crosses(241, 4, 9, 2).
crosses(241, 5, 10, 3).
crosses(241, 6, 11, 1).
crosses(241, 0, 6, 1).
crosses(241, 1, 7, 1).
crosses(241, 2, 8, 2).
crosses(241, 3, 9, 4).
crosses(241, 4, 10, 2).
crosses(241, 5, 11, 1).
crosses(241, 6, 12, 1).
crosses(241, 1, 8, 2).
crosses(241, 2, 9, 3).
crosses(241, 3, 10, 4).
crosses(241, 5, 12, 1).
crosses(241, 0, 8, 1).
crosses(241, 1, 9, 3).
crosses(241, 2, 10, 3).
crosses(241, 3, 11, 2).
crosses(241, 0, 9, 2).
crosses(241, 1, 10, 3).
crosses(241, 2, 11, 1).
crosses(241, 3, 12, 2).
crosses(241, 0, 10, 2).
crosses(241, 1, 11, 1).
crosses(241, 2, 12, 1).
crosses(241, 1, 12, 1).
crosses(242, 1, 3, 1).
crosses(242, 2, 4, 1).
crosses(242, 3, 5, 1).
crosses(242, 5, 7, 5).
crosses(242, 9, 11, 2).
crosses(242, 10, 12, 1).
crosses(242, 12, 14, 1).
crosses(242, 14, 16, 3).
crosses(242, 0, 3, 1).
crosses(242, 1, 4, 2).
crosses(242, 2, 5, 2).
crosses(242, 4, 7, 4).
crosses(242, 5, 8, 5).
crosses(242, 8, 11, 2).
crosses(242, 9, 12, 3).
crosses(242, 10, 13, 1).
crosses(242, 11, 14, 1).
crosses(242, 13, 16, 2).
crosses(242, 0, 4, 2).
crosses(242, 1, 5, 3).
crosses(242, 3, 7, 3).
crosses(242, 4, 8, 4).
crosses(242, 5, 9, 5).
crosses(242, 7, 11, 2).
crosses(242, 8, 12, 3).
crosses(242, 9, 13, 3).
crosses(242, 10, 14, 2).
crosses(242, 12, 16, 2).
crosses(242, 0, 5, 3).
crosses(242, 2, 7, 2).
crosses(242, 3, 8, 3).
crosses(242, 4, 9, 4).
crosses(242, 5, 10, 4).
crosses(242, 6, 11, 1).
crosses(242, 7, 12, 3).
crosses(242, 8, 13, 3).
crosses(242, 9, 14, 4).
crosses(242, 11, 16, 1).
crosses(242, 1, 7, 2).
crosses(242, 2, 8, 2).
crosses(242, 3, 9, 3).
crosses(242, 4, 10, 3).
crosses(242, 5, 11, 5).
crosses(242, 6, 12, 2).
crosses(242, 7, 13, 3).
crosses(242, 8, 14, 4).
crosses(242, 9, 15, 1).
crosses(242, 0, 7, 1).
crosses(242, 1, 8, 2).
crosses(242, 2, 9, 2).
crosses(242, 3, 10, 2).
crosses(242, 4, 11, 4).
crosses(242, 5, 12, 6).
crosses(242, 6, 13, 2).
crosses(242, 7, 14, 4).
crosses(242, 8, 15, 1).
crosses(242, 9, 16, 1).
crosses(242, 0, 8, 1).
crosses(242, 1, 9, 2).
crosses(242, 2, 10, 1).
crosses(242, 3, 11, 3).
crosses(242, 4, 12, 5).
crosses(242, 5, 13, 6).
crosses(242, 6, 14, 3).
crosses(242, 7, 15, 1).
crosses(242, 8, 16, 1).
crosses(242, 0, 9, 1).
crosses(242, 1, 10, 1).
crosses(242, 2, 11, 2).
crosses(242, 3, 12, 4).
crosses(242, 4, 13, 5).
crosses(242, 5, 14, 7).
crosses(242, 7, 16, 1).
crosses(242, 1, 11, 2).
crosses(242, 2, 12, 3).
crosses(242, 3, 13, 4).
crosses(242, 4, 14, 6).
crosses(242, 5, 15, 4).
crosses(242, 0, 11, 1).
crosses(242, 1, 12, 3).
crosses(242, 2, 13, 3).
crosses(242, 3, 14, 5).
crosses(242, 4, 15, 3).
crosses(242, 5, 16, 4).
crosses(242, 0, 12, 2).
crosses(242, 1, 13, 3).
crosses(242, 2, 14, 4).
crosses(242, 3, 15, 2).
crosses(242, 4, 16, 3).
crosses(242, 0, 13, 2).
crosses(242, 1, 14, 4).
crosses(242, 2, 15, 1).
crosses(242, 3, 16, 2).
crosses(242, 0, 14, 3).
crosses(242, 1, 15, 1).
crosses(242, 2, 16, 1).
crosses(242, 1, 16, 1).
crosses(243, 1, 3, 1).
crosses(243, 2, 4, 1).
crosses(243, 3, 5, 1).
crosses(243, 4, 6, 1).
crosses(243, 5, 7, 1).
crosses(243, 6, 8, 1).
crosses(243, 8, 10, 6).
crosses(243, 0, 3, 1).
crosses(243, 1, 4, 2).
crosses(243, 2, 5, 2).
crosses(243, 3, 6, 2).
crosses(243, 4, 7, 2).
crosses(243, 5, 8, 2).
crosses(243, 7, 10, 5).
crosses(243, 0, 4, 2).
crosses(243, 1, 5, 3).
crosses(243, 2, 6, 3).
crosses(243, 3, 7, 3).
crosses(243, 4, 8, 3).
crosses(243, 6, 10, 4).
crosses(243, 0, 5, 3).
crosses(243, 1, 6, 4).
crosses(243, 2, 7, 4).
crosses(243, 3, 8, 4).
crosses(243, 5, 10, 3).
crosses(243, 0, 6, 4).
crosses(243, 1, 7, 5).
crosses(243, 2, 8, 5).
crosses(243, 4, 10, 2).
crosses(243, 0, 7, 5).
crosses(243, 1, 8, 6).
crosses(243, 3, 10, 1).
crosses(243, 0, 8, 6).
crosses(244, 0, 2, 1).
crosses(244, 3, 5, 2).
crosses(244, 4, 6, 1).
crosses(244, 6, 8, 1).
crosses(244, 8, 10, 3).
crosses(244, 0, 3, 1).
crosses(244, 2, 5, 2).
crosses(244, 3, 6, 3).
crosses(244, 4, 7, 1).
crosses(244, 5, 8, 1).
crosses(244, 7, 10, 2).
crosses(244, 1, 5, 1).
crosses(244, 2, 6, 3).
crosses(244, 3, 7, 3).
crosses(244, 4, 8, 2).
crosses(244, 6, 10, 2).
crosses(244, 0, 5, 1).
crosses(244, 1, 6, 2).
crosses(244, 2, 7, 3).
crosses(244, 3, 8, 4).
crosses(244, 5, 10, 1).
crosses(244, 0, 6, 2).
crosses(244, 1, 7, 2).
crosses(244, 2, 8, 4).
crosses(244, 3, 9, 1).
crosses(244, 0, 7, 2).
crosses(244, 1, 8, 3).
crosses(244, 2, 9, 1).
crosses(244, 3, 10, 1).
crosses(244, 0, 8, 3).
crosses(244, 2, 10, 1).
crosses(245, 1, 3, 1).
crosses(245, 2, 4, 1).
crosses(245, 3, 5, 1).
crosses(245, 5, 7, 5).
crosses(245, 8, 10, 2).
crosses(245, 9, 11, 1).
crosses(245, 11, 13, 1).
crosses(245, 13, 15, 3).
crosses(245, 0, 3, 1).
crosses(245, 1, 4, 2).
crosses(245, 2, 5, 2).
crosses(245, 4, 7, 4).
crosses(245, 5, 8, 5).
crosses(245, 7, 10, 2).
crosses(245, 8, 11, 3).
crosses(245, 9, 12, 1).
crosses(245, 10, 13, 1).
crosses(245, 12, 15, 2).
crosses(245, 0, 4, 2).
crosses(245, 1, 5, 3).
crosses(245, 3, 7, 3).
crosses(245, 4, 8, 4).
crosses(245, 5, 9, 4).
crosses(245, 6, 10, 1).
crosses(245, 7, 11, 3).
crosses(245, 8, 12, 3).
crosses(245, 9, 13, 2).
crosses(245, 11, 15, 2).
crosses(245, 0, 5, 3).
crosses(245, 2, 7, 2).
crosses(245, 3, 8, 3).
crosses(245, 4, 9, 3).
crosses(245, 5, 10, 5).
crosses(245, 6, 11, 2).
crosses(245, 7, 12, 3).
crosses(245, 8, 13, 4).
crosses(245, 10, 15, 1).
crosses(245, 1, 7, 2).
crosses(245, 2, 8, 2).
crosses(245, 3, 9, 2).
crosses(245, 4, 10, 4).
crosses(245, 5, 11, 6).
crosses(245, 6, 12, 2).
crosses(245, 7, 13, 4).
crosses(245, 8, 14, 1).
crosses(245, 0, 7, 1).
crosses(245, 1, 8, 2).
crosses(245, 2, 9, 1).
crosses(245, 3, 10, 3).
crosses(245, 4, 11, 5).
crosses(245, 5, 12, 6).
crosses(245, 6, 13, 3).
crosses(245, 7, 14, 1).
crosses(245, 8, 15, 1).
crosses(245, 0, 8, 1).
crosses(245, 1, 9, 1).
crosses(245, 2, 10, 2).
crosses(245, 3, 11, 4).
crosses(245, 4, 12, 5).
crosses(245, 5, 13, 7).
crosses(245, 7, 15, 1).
crosses(245, 1, 10, 2).
crosses(245, 2, 11, 3).
crosses(245, 3, 12, 4).
crosses(245, 4, 13, 6).
crosses(245, 5, 14, 4).
crosses(245, 0, 10, 1).
crosses(245, 1, 11, 3).
crosses(245, 2, 12, 3).
crosses(245, 3, 13, 5).
crosses(245, 4, 14, 3).
crosses(245, 5, 15, 4).
crosses(245, 0, 11, 2).
crosses(245, 1, 12, 3).
crosses(245, 2, 13, 4).
crosses(245, 3, 14, 2).
crosses(245, 4, 15, 3).
crosses(245, 0, 12, 2).
crosses(245, 1, 13, 4).
crosses(245, 2, 14, 1).
crosses(245, 3, 15, 2).
crosses(245, 0, 13, 3).
crosses(245, 1, 14, 1).
crosses(245, 2, 15, 1).
crosses(245, 1, 15, 1).
crosses(246, 1, 3, 1).
crosses(246, 2, 4, 1).
crosses(246, 3, 5, 1).
crosses(246, 4, 6, 1).
crosses(246, 5, 7, 1).
crosses(246, 7, 9, 7).
crosses(246, 10, 12, 2).
crosses(246, 11, 13, 1).
crosses(246, 13, 15, 1).
crosses(246, 15, 17, 3).
crosses(246, 0, 3, 1).
crosses(246, 1, 4, 2).
crosses(246, 2, 5, 2).
crosses(246, 3, 6, 2).
crosses(246, 4, 7, 2).
crosses(246, 6, 9, 6).
crosses(246, 7, 10, 7).
crosses(246, 9, 12, 2).
crosses(246, 10, 13, 3).
crosses(246, 11, 14, 1).
crosses(246, 12, 15, 1).
crosses(246, 14, 17, 2).
crosses(246, 0, 4, 2).
crosses(246, 1, 5, 3).
crosses(246, 2, 6, 3).
crosses(246, 3, 7, 3).
crosses(246, 5, 9, 5).
crosses(246, 6, 10, 6).
crosses(246, 7, 11, 6).
crosses(246, 8, 12, 1).
crosses(246, 9, 13, 3).
crosses(246, 10, 14, 3).
crosses(246, 11, 15, 2).
crosses(246, 13, 17, 2).
crosses(246, 0, 5, 3).
crosses(246, 1, 6, 4).
crosses(246, 2, 7, 4).
crosses(246, 4, 9, 4).
crosses(246, 5, 10, 5).
crosses(246, 6, 11, 5).
crosses(246, 7, 12, 7).
crosses(246, 8, 13, 2).
crosses(246, 9, 14, 3).
crosses(246, 10, 15, 4).
crosses(246, 12, 17, 1).
crosses(246, 0, 6, 4).
crosses(246, 1, 7, 5).
crosses(246, 3, 9, 3).
crosses(246, 4, 10, 4).
crosses(246, 5, 11, 4).
crosses(246, 6, 12, 6).
crosses(246, 7, 13, 8).
crosses(246, 8, 14, 2).
crosses(246, 9, 15, 4).
crosses(246, 10, 16, 1).
crosses(246, 0, 7, 5).
crosses(246, 2, 9, 2).
crosses(246, 3, 10, 3).
crosses(246, 4, 11, 3).
crosses(246, 5, 12, 5).
crosses(246, 6, 13, 7).
crosses(246, 7, 14, 8).
crosses(246, 8, 15, 3).
crosses(246, 9, 16, 1).
crosses(246, 10, 17, 1).
crosses(246, 1, 9, 2).
crosses(246, 2, 10, 2).
crosses(246, 3, 11, 2).
crosses(246, 4, 12, 4).
crosses(246, 5, 13, 6).
crosses(246, 6, 14, 7).
crosses(246, 7, 15, 9).
crosses(246, 9, 17, 1).
crosses(246, 0, 9, 1).
crosses(246, 1, 10, 2).
crosses(246, 2, 11, 1).
crosses(246, 3, 12, 3).
crosses(246, 4, 13, 5).
crosses(246, 5, 14, 6).
crosses(246, 6, 15, 8).
crosses(246, 7, 16, 6).
crosses(246, 0, 10, 1).
crosses(246, 1, 11, 1).
crosses(246, 2, 12, 2).
crosses(246, 3, 13, 4).
crosses(246, 4, 14, 5).
crosses(246, 5, 15, 7).
crosses(246, 6, 16, 5).
crosses(246, 7, 17, 6).
crosses(246, 1, 12, 2).
crosses(246, 2, 13, 3).
crosses(246, 3, 14, 4).
crosses(246, 4, 15, 6).
crosses(246, 5, 16, 4).
crosses(246, 6, 17, 5).
crosses(246, 0, 12, 1).
crosses(246, 1, 13, 3).
crosses(246, 2, 14, 3).
crosses(246, 3, 15, 5).
crosses(246, 4, 16, 3).
crosses(246, 5, 17, 4).
crosses(246, 0, 13, 2).
crosses(246, 1, 14, 3).
crosses(246, 2, 15, 4).
crosses(246, 3, 16, 2).
crosses(246, 4, 17, 3).
crosses(246, 0, 14, 2).
crosses(246, 1, 15, 4).
crosses(246, 2, 16, 1).
crosses(246, 3, 17, 2).
crosses(246, 0, 15, 3).
crosses(246, 1, 16, 1).
crosses(246, 2, 17, 1).
crosses(246, 1, 17, 1).
crosses(247, 2, 4, 1).
crosses(247, 3, 5, 1).
crosses(247, 7, 9, 1).
crosses(247, 1, 4, 1).
crosses(247, 2, 5, 2).
crosses(247, 3, 6, 1).
crosses(247, 6, 9, 1).
crosses(247, 1, 5, 2).
crosses(247, 2, 6, 2).
crosses(247, 3, 7, 1).
crosses(247, 5, 9, 1).
crosses(247, 0, 5, 1).
crosses(247, 1, 6, 2).
crosses(247, 2, 7, 2).
crosses(247, 0, 6, 1).
crosses(247, 1, 7, 2).
crosses(247, 2, 8, 1).
crosses(247, 0, 7, 1).
crosses(247, 1, 8, 1).
crosses(247, 2, 9, 1).
crosses(247, 1, 9, 1).
crosses(248, 1, 3, 1).
crosses(248, 4, 6, 1).
crosses(248, 0, 3, 1).
crosses(248, 1, 4, 1).
crosses(248, 3, 6, 1).
crosses(248, 0, 4, 1).
crosses(249, 1, 3, 1).
crosses(249, 2, 4, 1).
crosses(249, 5, 7, 1).
crosses(249, 9, 11, 2).
crosses(249, 1, 4, 2).
crosses(249, 2, 5, 1).
crosses(249, 4, 7, 1).
crosses(249, 5, 8, 1).
crosses(249, 8, 11, 2).
crosses(249, 0, 4, 1).
crosses(249, 1, 5, 2).
crosses(249, 2, 6, 1).
crosses(249, 3, 7, 1).
crosses(249, 4, 8, 1).
crosses(249, 5, 9, 1).
crosses(249, 7, 11, 2).
crosses(249, 0, 5, 1).
crosses(249, 1, 6, 2).
crosses(249, 2, 7, 2).
crosses(249, 3, 8, 1).
crosses(249, 4, 9, 1).
crosses(249, 6, 11, 1).
crosses(249, 0, 6, 1).
crosses(249, 1, 7, 3).
crosses(249, 2, 8, 2).
crosses(249, 3, 9, 1).
crosses(249, 5, 11, 1).
crosses(249, 0, 7, 2).
crosses(249, 1, 8, 3).
crosses(249, 2, 9, 2).
crosses(249, 4, 11, 1).
crosses(249, 0, 8, 2).
crosses(249, 1, 9, 3).
crosses(249, 0, 9, 2).
crosses(249, 1, 10, 1).
crosses(249, 1, 11, 1).
crosses(250, 2, 4, 1).
crosses(250, 3, 5, 1).
crosses(250, 7, 9, 1).
crosses(250, 1, 4, 1).
crosses(250, 2, 5, 2).
crosses(250, 3, 6, 1).
crosses(250, 6, 9, 1).
crosses(250, 1, 5, 2).
crosses(250, 2, 6, 2).
crosses(250, 3, 7, 1).
crosses(250, 5, 9, 1).
crosses(250, 0, 5, 1).
crosses(250, 1, 6, 2).
crosses(250, 2, 7, 2).
crosses(250, 0, 6, 1).
crosses(250, 1, 7, 2).
crosses(250, 2, 8, 1).
crosses(250, 0, 7, 1).
crosses(250, 1, 8, 1).
crosses(250, 2, 9, 1).
crosses(250, 1, 9, 1).
crosses(251, 1, 3, 1).
crosses(251, 4, 6, 1).
crosses(251, 0, 3, 1).
crosses(251, 1, 4, 1).
crosses(251, 3, 6, 1).
crosses(251, 0, 4, 1).
crosses(252, 1, 3, 1).
crosses(252, 5, 7, 1).
crosses(252, 0, 3, 1).
crosses(252, 1, 4, 1).
crosses(252, 4, 7, 1).
crosses(252, 0, 4, 1).
crosses(252, 1, 5, 1).
crosses(252, 3, 7, 1).
crosses(252, 0, 5, 1).
crosses(253, 2, 4, 1).
crosses(253, 3, 5, 1).
crosses(253, 5, 7, 3).
crosses(253, 6, 8, 1).
crosses(253, 8, 10, 1).
crosses(253, 10, 12, 2).
crosses(253, 1, 4, 1).
crosses(253, 2, 5, 2).
crosses(253, 4, 7, 2).
crosses(253, 5, 8, 4).
crosses(253, 6, 9, 1).
crosses(253, 7, 10, 1).
crosses(253, 9, 12, 1).
crosses(253, 0, 4, 1).
crosses(253, 1, 5, 2).
crosses(253, 3, 7, 1).
crosses(253, 4, 8, 3).
crosses(253, 5, 9, 4).
crosses(253, 6, 10, 2).
crosses(253, 8, 12, 1).
crosses(253, 0, 5, 2).
crosses(253, 2, 7, 1).
crosses(253, 3, 8, 2).
crosses(253, 4, 9, 3).
crosses(253, 5, 10, 5).
crosses(253, 1, 7, 1).
crosses(253, 2, 8, 2).
crosses(253, 3, 9, 2).
crosses(253, 4, 10, 4).
crosses(253, 5, 11, 3).
crosses(253, 1, 8, 2).
crosses(253, 2, 9, 2).
crosses(253, 3, 10, 3).
crosses(253, 4, 11, 2).
crosses(253, 5, 12, 3).
crosses(253, 0, 8, 1).
crosses(253, 1, 9, 2).
crosses(253, 2, 10, 3).
crosses(253, 3, 11, 1).
crosses(253, 4, 12, 2).
crosses(253, 0, 9, 1).
crosses(253, 1, 10, 3).
crosses(253, 2, 11, 1).
crosses(253, 3, 12, 1).
crosses(253, 0, 10, 2).
crosses(253, 1, 11, 1).
crosses(253, 2, 12, 1).
crosses(253, 1, 12, 1).
crosses(254, 1, 3, 1).
crosses(254, 3, 5, 1).
crosses(254, 4, 6, 1).
crosses(254, 6, 8, 3).
crosses(254, 0, 3, 1).
crosses(254, 1, 4, 1).
crosses(254, 2, 5, 1).
crosses(254, 3, 6, 2).
crosses(254, 5, 8, 2).
crosses(254, 0, 4, 1).
crosses(254, 1, 5, 2).
crosses(254, 2, 6, 2).
crosses(254, 4, 8, 1).
crosses(254, 0, 5, 2).
crosses(254, 1, 6, 3).
crosses(254, 3, 8, 1).
crosses(254, 0, 6, 3).
crosses(255, 1, 3, 1).
crosses(255, 3, 5, 1).
crosses(255, 5, 7, 2).
crosses(255, 0, 3, 1).
crosses(255, 1, 4, 1).
crosses(255, 2, 5, 1).
crosses(255, 4, 7, 1).
crosses(255, 0, 4, 1).
crosses(255, 1, 5, 2).
crosses(255, 3, 7, 1).
crosses(255, 0, 5, 2).
crosses(256, 2, 4, 1).
crosses(256, 4, 6, 2).
crosses(256, 5, 7, 1).
crosses(256, 9, 11, 1).
crosses(256, 1, 4, 1).
crosses(256, 3, 6, 1).
crosses(256, 4, 7, 3).
crosses(256, 5, 8, 1).
crosses(256, 8, 11, 1).
crosses(256, 0, 4, 1).
crosses(256, 2, 6, 1).
crosses(256, 3, 7, 2).
crosses(256, 4, 8, 3).
crosses(256, 5, 9, 1).
crosses(256, 7, 11, 1).
crosses(256, 1, 6, 1).
crosses(256, 2, 7, 2).
crosses(256, 3, 8, 2).
crosses(256, 4, 9, 3).
crosses(256, 1, 7, 2).
crosses(256, 2, 8, 2).
crosses(256, 3, 9, 2).
crosses(256, 4, 10, 2).
crosses(256, 0, 7, 1).
crosses(256, 1, 8, 2).
crosses(256, 2, 9, 2).
crosses(256, 3, 10, 1).
crosses(256, 4, 11, 2).
crosses(256, 0, 8, 1).
crosses(256, 1, 9, 2).
crosses(256, 2, 10, 1).
crosses(256, 3, 11, 1).
crosses(256, 0, 9, 1).
crosses(256, 1, 10, 1).
crosses(256, 2, 11, 1).
crosses(256, 1, 11, 1).
crosses(257, 1, 3, 1).
crosses(257, 3, 5, 1).
crosses(257, 5, 7, 2).
crosses(257, 0, 3, 1).
crosses(257, 1, 4, 1).
crosses(257, 2, 5, 1).
crosses(257, 4, 7, 1).
crosses(257, 0, 4, 1).
crosses(257, 1, 5, 2).
crosses(257, 3, 7, 1).
crosses(257, 0, 5, 2).
crosses(258, 1, 3, 1).
crosses(258, 2, 4, 1).
crosses(258, 3, 5, 1).
crosses(258, 5, 7, 1).
crosses(258, 7, 9, 4).
crosses(258, 0, 3, 1).
crosses(258, 1, 4, 2).
crosses(258, 2, 5, 2).
crosses(258, 3, 6, 1).
crosses(258, 4, 7, 1).
crosses(258, 6, 9, 3).
crosses(258, 0, 4, 2).
crosses(258, 1, 5, 3).
crosses(258, 2, 6, 2).
crosses(258, 3, 7, 2).
crosses(258, 5, 9, 3).
crosses(258, 0, 5, 3).
crosses(258, 1, 6, 3).
crosses(258, 2, 7, 3).
crosses(258, 4, 9, 2).
crosses(258, 0, 6, 3).
crosses(258, 1, 7, 4).
crosses(258, 3, 9, 1).
crosses(258, 0, 7, 4).
crosses(259, 2, 4, 2).
crosses(259, 4, 6, 2).
crosses(259, 5, 7, 1).
crosses(259, 7, 9, 2).
crosses(259, 1, 4, 2).
crosses(259, 2, 5, 1).
crosses(259, 3, 6, 1).
crosses(259, 4, 7, 3).
crosses(259, 6, 9, 1).
crosses(259, 0, 4, 1).
crosses(259, 1, 5, 1).
crosses(259, 2, 6, 2).
crosses(259, 3, 7, 2).
crosses(259, 4, 8, 1).
crosses(259, 1, 6, 2).
crosses(259, 2, 7, 3).
crosses(259, 4, 9, 1).
crosses(259, 0, 6, 1).
crosses(259, 1, 7, 3).
crosses(259, 2, 8, 1).
crosses(259, 0, 7, 2).
crosses(259, 1, 8, 1).
crosses(259, 2, 9, 1).
crosses(259, 1, 9, 1).
crosses(260, 0, 2, 1).
crosses(260, 2, 4, 3).
crosses(260, 5, 7, 2).
crosses(260, 6, 8, 1).
crosses(260, 8, 10, 2).
crosses(260, 1, 4, 2).
crosses(260, 2, 5, 3).
crosses(260, 4, 7, 2).
crosses(260, 5, 8, 3).
crosses(260, 7, 10, 1).
crosses(260, 0, 4, 1).
crosses(260, 1, 5, 2).
crosses(260, 2, 6, 2).
crosses(260, 3, 7, 1).
crosses(260, 4, 8, 3).
crosses(260, 5, 9, 1).
crosses(260, 0, 5, 1).
crosses(260, 1, 6, 1).
crosses(260, 2, 7, 3).
crosses(260, 3, 8, 2).
crosses(260, 4, 9, 1).
crosses(260, 5, 10, 1).
crosses(260, 1, 7, 2).
crosses(260, 2, 8, 4).
crosses(260, 4, 10, 1).
crosses(260, 0, 7, 1).
crosses(260, 1, 8, 3).
crosses(260, 2, 9, 2).
crosses(260, 0, 8, 2).
crosses(260, 1, 9, 1).
crosses(260, 2, 10, 2).
crosses(260, 1, 10, 1).
crosses(261, 2, 4, 2).
crosses(261, 4, 6, 2).
crosses(261, 5, 7, 1).
crosses(261, 7, 9, 2).
crosses(261, 1, 4, 2).
crosses(261, 2, 5, 1).
crosses(261, 3, 6, 1).
crosses(261, 4, 7, 3).
crosses(261, 6, 9, 1).
crosses(261, 0, 4, 1).
crosses(261, 1, 5, 1).
crosses(261, 2, 6, 2).
crosses(261, 3, 7, 2).
crosses(261, 4, 8, 1).
crosses(261, 1, 6, 2).
crosses(261, 2, 7, 3).
crosses(261, 4, 9, 1).
crosses(261, 0, 6, 1).
crosses(261, 1, 7, 3).
crosses(261, 2, 8, 1).
crosses(261, 0, 7, 2).
crosses(261, 1, 8, 1).
crosses(261, 2, 9, 1).
crosses(261, 1, 9, 1).
crosses(262, 1, 3, 2).
crosses(262, 4, 6, 2).
crosses(262, 5, 7, 1).
crosses(262, 7, 9, 2).
crosses(262, 0, 3, 1).
crosses(262, 1, 4, 2).
crosses(262, 3, 6, 2).
crosses(262, 4, 7, 3).
crosses(262, 6, 9, 1).
crosses(262, 0, 4, 1).
crosses(262, 1, 5, 1).
crosses(262, 2, 6, 1).
crosses(262, 3, 7, 3).
crosses(262, 4, 8, 1).
crosses(262, 1, 6, 2).
crosses(262, 2, 7, 2).
crosses(262, 3, 8, 1).
crosses(262, 4, 9, 1).
crosses(262, 0, 6, 1).
crosses(262, 1, 7, 3).
crosses(262, 3, 9, 1).
crosses(262, 0, 7, 2).
crosses(262, 1, 8, 1).
crosses(262, 1, 9, 1).
crosses(263, 2, 4, 2).
crosses(263, 4, 6, 2).
crosses(263, 5, 7, 1).
crosses(263, 7, 9, 2).
crosses(263, 1, 4, 2).
crosses(263, 2, 5, 1).
crosses(263, 3, 6, 1).
crosses(263, 4, 7, 3).
crosses(263, 6, 9, 1).
crosses(263, 0, 4, 1).
crosses(263, 1, 5, 1).
crosses(263, 2, 6, 2).
crosses(263, 3, 7, 2).
crosses(263, 4, 8, 1).
crosses(263, 1, 6, 2).
crosses(263, 2, 7, 3).
crosses(263, 4, 9, 1).
crosses(263, 0, 6, 1).
crosses(263, 1, 7, 3).
crosses(263, 2, 8, 1).
crosses(263, 0, 7, 2).
crosses(263, 1, 8, 1).
crosses(263, 2, 9, 1).
crosses(263, 1, 9, 1).
crosses(264, 1, 3, 2).
crosses(264, 3, 5, 2).
crosses(264, 4, 6, 1).
crosses(264, 6, 8, 2).
crosses(264, 0, 3, 1).
crosses(264, 1, 4, 1).
crosses(264, 2, 5, 1).
crosses(264, 3, 6, 3).
crosses(264, 5, 8, 1).
crosses(264, 1, 5, 2).
crosses(264, 2, 6, 2).
crosses(264, 3, 7, 1).
crosses(264, 0, 5, 1).
crosses(264, 1, 6, 3).
crosses(264, 3, 8, 1).
crosses(264, 0, 6, 2).
crosses(264, 1, 7, 1).
crosses(264, 1, 8, 1).
crosses(265, 2, 4, 2).
crosses(265, 4, 6, 2).
crosses(265, 5, 7, 1).
crosses(265, 7, 9, 2).
crosses(265, 1, 4, 2).
crosses(265, 2, 5, 1).
crosses(265, 3, 6, 1).
crosses(265, 4, 7, 3).
crosses(265, 6, 9, 1).
crosses(265, 0, 4, 1).
crosses(265, 1, 5, 1).
crosses(265, 2, 6, 2).
crosses(265, 3, 7, 2).
crosses(265, 4, 8, 1).
crosses(265, 1, 6, 2).
crosses(265, 2, 7, 3).
crosses(265, 4, 9, 1).
crosses(265, 0, 6, 1).
crosses(265, 1, 7, 3).
crosses(265, 2, 8, 1).
crosses(265, 0, 7, 2).
crosses(265, 1, 8, 1).
crosses(265, 2, 9, 1).
crosses(265, 1, 9, 1).
crosses(266, 1, 3, 2).
crosses(266, 3, 5, 2).
crosses(266, 4, 6, 1).
crosses(266, 6, 8, 2).
crosses(266, 0, 3, 1).
crosses(266, 1, 4, 1).
crosses(266, 2, 5, 1).
crosses(266, 3, 6, 3).
crosses(266, 5, 8, 1).
crosses(266, 1, 5, 2).
crosses(266, 2, 6, 2).
crosses(266, 3, 7, 1).
crosses(266, 0, 5, 1).
crosses(266, 1, 6, 3).
crosses(266, 3, 8, 1).
crosses(266, 0, 6, 2).
crosses(266, 1, 7, 1).
crosses(266, 1, 8, 1).
crosses(267, 2, 4, 2).
crosses(267, 4, 6, 2).
crosses(267, 5, 7, 1).
crosses(267, 7, 9, 2).
crosses(267, 1, 4, 2).
crosses(267, 2, 5, 1).
crosses(267, 3, 6, 1).
crosses(267, 4, 7, 3).
crosses(267, 6, 9, 1).
crosses(267, 0, 4, 1).
crosses(267, 1, 5, 1).
crosses(267, 2, 6, 2).
crosses(267, 3, 7, 2).
crosses(267, 4, 8, 1).
crosses(267, 1, 6, 2).
crosses(267, 2, 7, 3).
crosses(267, 4, 9, 1).
crosses(267, 0, 6, 1).
crosses(267, 1, 7, 3).
crosses(267, 2, 8, 1).
crosses(267, 0, 7, 2).
crosses(267, 1, 8, 1).
crosses(267, 2, 9, 1).
crosses(267, 1, 9, 1).
crosses(268, 1, 3, 2).
crosses(268, 3, 5, 2).
crosses(268, 4, 6, 1).
crosses(268, 6, 8, 2).
crosses(268, 0, 3, 1).
crosses(268, 1, 4, 1).
crosses(268, 2, 5, 1).
crosses(268, 3, 6, 3).
crosses(268, 5, 8, 1).
crosses(268, 1, 5, 2).
crosses(268, 2, 6, 2).
crosses(268, 3, 7, 1).
crosses(268, 0, 5, 1).
crosses(268, 1, 6, 3).
crosses(268, 3, 8, 1).
crosses(268, 0, 6, 2).
crosses(268, 1, 7, 1).
crosses(268, 1, 8, 1).
crosses(269, 2, 4, 2).
crosses(269, 4, 6, 2).
crosses(269, 5, 7, 1).
crosses(269, 7, 9, 2).
crosses(269, 1, 4, 2).
crosses(269, 2, 5, 1).
crosses(269, 3, 6, 1).
crosses(269, 4, 7, 3).
crosses(269, 6, 9, 1).
crosses(269, 0, 4, 1).
crosses(269, 1, 5, 1).
crosses(269, 2, 6, 2).
crosses(269, 3, 7, 2).
crosses(269, 4, 8, 1).
crosses(269, 1, 6, 2).
crosses(269, 2, 7, 3).
crosses(269, 4, 9, 1).
crosses(269, 0, 6, 1).
crosses(269, 1, 7, 3).
crosses(269, 2, 8, 1).
crosses(269, 0, 7, 2).
crosses(269, 1, 8, 1).
crosses(269, 2, 9, 1).
crosses(269, 1, 9, 1).
crosses(270, 1, 3, 2).
crosses(270, 3, 5, 2).
crosses(270, 4, 6, 1).
crosses(270, 6, 8, 2).
crosses(270, 0, 3, 1).
crosses(270, 1, 4, 1).
crosses(270, 2, 5, 1).
crosses(270, 3, 6, 3).
crosses(270, 5, 8, 1).
crosses(270, 1, 5, 2).
crosses(270, 2, 6, 2).
crosses(270, 3, 7, 1).
crosses(270, 0, 5, 1).
crosses(270, 1, 6, 3).
crosses(270, 3, 8, 1).
crosses(270, 0, 6, 2).
crosses(270, 1, 7, 1).
crosses(270, 1, 8, 1).
crosses(271, 0, 2, 1).
crosses(271, 2, 4, 2).
crosses(271, 4, 6, 1).
crosses(271, 1, 4, 1).
crosses(271, 2, 5, 1).
crosses(271, 0, 4, 1).
crosses(271, 2, 6, 1).
crosses(272, 0, 2, 1).
crosses(272, 2, 4, 2).
crosses(272, 5, 7, 1).
crosses(272, 1, 4, 1).
crosses(272, 2, 5, 2).
crosses(272, 4, 7, 1).
crosses(272, 0, 4, 1).
crosses(272, 1, 5, 1).
crosses(272, 2, 6, 1).
crosses(272, 0, 5, 1).
crosses(272, 2, 7, 1).
crosses(273, 2, 4, 2).
crosses(273, 4, 6, 2).
crosses(273, 6, 8, 1).
crosses(273, 1, 4, 2).
crosses(273, 2, 5, 1).
crosses(273, 3, 6, 1).
crosses(273, 4, 7, 1).
crosses(273, 0, 4, 1).
crosses(273, 1, 5, 1).
crosses(273, 2, 6, 2).
crosses(273, 4, 8, 1).
crosses(273, 1, 6, 2).
crosses(273, 2, 7, 1).
crosses(273, 0, 6, 1).
crosses(273, 1, 7, 1).
crosses(273, 2, 8, 1).
crosses(273, 1, 8, 1).
crosses(274, 0, 2, 1).
crosses(274, 2, 4, 2).
crosses(274, 4, 6, 1).
crosses(274, 1, 4, 1).
crosses(274, 2, 5, 1).
crosses(274, 0, 4, 1).
crosses(274, 2, 6, 1).
crosses(275, 0, 2, 1).
crosses(275, 2, 4, 2).
crosses(275, 4, 6, 1).
crosses(275, 1, 4, 1).
crosses(275, 2, 5, 1).
crosses(275, 0, 4, 1).
crosses(275, 2, 6, 1).
crosses(276, 2, 4, 2).
crosses(276, 4, 6, 2).
crosses(276, 6, 8, 1).
crosses(276, 1, 4, 2).
crosses(276, 2, 5, 1).
crosses(276, 3, 6, 1).
crosses(276, 4, 7, 1).
crosses(276, 0, 4, 1).
crosses(276, 1, 5, 1).
crosses(276, 2, 6, 2).
crosses(276, 4, 8, 1).
crosses(276, 1, 6, 2).
crosses(276, 2, 7, 1).
crosses(276, 0, 6, 1).
crosses(276, 1, 7, 1).
crosses(276, 2, 8, 1).
crosses(276, 1, 8, 1).
crosses(277, 0, 2, 1).
crosses(277, 2, 4, 2).
crosses(277, 4, 6, 1).
crosses(277, 1, 4, 1).
crosses(277, 2, 5, 1).
crosses(277, 0, 4, 1).
crosses(277, 2, 6, 1).
crosses(278, 0, 2, 1).
crosses(278, 3, 5, 1).
crosses(278, 0, 3, 1).
crosses(278, 2, 5, 1).
crosses(278, 3, 6, 1).
crosses(278, 2, 6, 1).
crosses(279, 2, 4, 2).
crosses(279, 4, 6, 2).
crosses(279, 6, 8, 1).
crosses(279, 1, 4, 2).
crosses(279, 2, 5, 1).
crosses(279, 3, 6, 1).
crosses(279, 4, 7, 1).
crosses(279, 0, 4, 1).
crosses(279, 1, 5, 1).
crosses(279, 2, 6, 2).
crosses(279, 4, 8, 1).
crosses(279, 1, 6, 2).
crosses(279, 2, 7, 1).
crosses(279, 0, 6, 1).
crosses(279, 1, 7, 1).
crosses(279, 2, 8, 1).
crosses(279, 1, 8, 1).
crosses(280, 0, 2, 1).
crosses(280, 2, 4, 2).
crosses(280, 4, 6, 1).
crosses(280, 1, 4, 1).
crosses(280, 2, 5, 1).
crosses(280, 0, 4, 1).
crosses(280, 2, 6, 1).
crosses(281, 1, 3, 1).
crosses(281, 3, 5, 3).
crosses(281, 6, 8, 1).
crosses(281, 0, 3, 1).
crosses(281, 2, 5, 2).
crosses(281, 3, 6, 3).
crosses(281, 5, 8, 1).
crosses(281, 1, 5, 2).
crosses(281, 2, 6, 2).
crosses(281, 3, 7, 2).
crosses(281, 0, 5, 1).
crosses(281, 1, 6, 2).
crosses(281, 2, 7, 1).
crosses(281, 3, 8, 2).
crosses(281, 0, 6, 1).
crosses(281, 1, 7, 1).
crosses(281, 2, 8, 1).
crosses(281, 1, 8, 1).
crosses(282, 0, 2, 1).
crosses(282, 2, 4, 2).
crosses(282, 4, 6, 1).
crosses(282, 1, 4, 1).
crosses(282, 2, 5, 1).
crosses(282, 0, 4, 1).
crosses(282, 2, 6, 1).
crosses(283, 1, 3, 1).
crosses(283, 3, 5, 1).
crosses(283, 0, 3, 1).
crosses(284, 0, 2, 1).
crosses(284, 2, 4, 2).
crosses(284, 4, 6, 1).
crosses(284, 1, 4, 1).
crosses(284, 2, 5, 1).
crosses(284, 0, 4, 1).
crosses(284, 2, 6, 1).
crosses(285, 1, 3, 1).
crosses(285, 3, 5, 1).
crosses(285, 0, 3, 1).
crosses(286, 1, 3, 2).
crosses(286, 3, 5, 2).
crosses(286, 5, 7, 1).
crosses(286, 0, 3, 1).
crosses(286, 1, 4, 1).
crosses(286, 2, 5, 1).
crosses(286, 3, 6, 1).
crosses(286, 1, 5, 2).
crosses(286, 3, 7, 1).
crosses(286, 0, 5, 1).
crosses(286, 1, 6, 1).
crosses(286, 1, 7, 1).
crosses(287, 1, 3, 1).
crosses(287, 3, 5, 1).
crosses(287, 0, 3, 1).
crosses(288, 0, 2, 1).
crosses(288, 2, 4, 2).
crosses(288, 4, 6, 1).
crosses(288, 1, 4, 1).
crosses(288, 2, 5, 1).
crosses(288, 0, 4, 1).
crosses(288, 2, 6, 1).
crosses(289, 1, 3, 2).
crosses(289, 3, 5, 2).
crosses(289, 5, 7, 1).
crosses(289, 0, 3, 1).
crosses(289, 1, 4, 1).
crosses(289, 2, 5, 1).
crosses(289, 3, 6, 1).
crosses(289, 1, 5, 2).
crosses(289, 3, 7, 1).
crosses(289, 0, 5, 1).
crosses(289, 1, 6, 1).
crosses(289, 1, 7, 1).
crosses(291, 1, 3, 1).
crosses(291, 3, 5, 1).
crosses(291, 0, 3, 1).
crosses(292, 1, 3, 2).
crosses(292, 4, 6, 2).
crosses(292, 6, 8, 1).
crosses(292, 0, 3, 1).
crosses(292, 1, 4, 2).
crosses(292, 3, 6, 2).
crosses(292, 4, 7, 1).
crosses(292, 0, 4, 1).
crosses(292, 1, 5, 1).
crosses(292, 2, 6, 1).
crosses(292, 3, 7, 1).
crosses(292, 4, 8, 1).
crosses(292, 1, 6, 2).
crosses(292, 3, 8, 1).
crosses(292, 0, 6, 1).
crosses(292, 1, 7, 1).
crosses(292, 1, 8, 1).
crosses(294, 0, 2, 1).
crosses(294, 2, 4, 2).
crosses(294, 4, 6, 1).
crosses(294, 1, 4, 1).
crosses(294, 2, 5, 1).
crosses(294, 0, 4, 1).
crosses(294, 2, 6, 1).
crosses(295, 0, 2, 1).
crosses(295, 2, 4, 2).
crosses(295, 3, 5, 1).
crosses(295, 5, 7, 2).
crosses(295, 1, 4, 1).
crosses(295, 2, 5, 3).
crosses(295, 4, 7, 1).
crosses(295, 0, 4, 1).
crosses(295, 1, 5, 2).
crosses(295, 2, 6, 1).
crosses(295, 0, 5, 2).
crosses(295, 2, 7, 1).
crosses(296, 3, 5, 1).
crosses(296, 2, 5, 1).
crosses(296, 1, 5, 1).
crosses(297, 0, 2, 1).
crosses(297, 2, 4, 2).
crosses(297, 4, 6, 1).
crosses(297, 1, 4, 1).
crosses(297, 2, 5, 1).
crosses(297, 0, 4, 1).
crosses(297, 2, 6, 1).
crosses(298, 0, 2, 1).
crosses(298, 2, 4, 2).
crosses(298, 4, 6, 1).
crosses(298, 6, 8, 2).
crosses(298, 1, 4, 1).
crosses(298, 2, 5, 2).
crosses(298, 3, 6, 1).
crosses(298, 5, 8, 1).
crosses(298, 0, 4, 1).
crosses(298, 1, 5, 1).
crosses(298, 2, 6, 3).
crosses(298, 4, 8, 1).
crosses(298, 0, 5, 1).
crosses(298, 1, 6, 2).
crosses(298, 2, 7, 1).
crosses(298, 0, 6, 2).
crosses(298, 2, 8, 1).
crosses(299, 4, 6, 2).
crosses(299, 6, 8, 1).
crosses(299, 3, 6, 2).
crosses(299, 4, 7, 1).
crosses(299, 2, 6, 2).
crosses(299, 3, 7, 1).
crosses(299, 4, 8, 1).
crosses(299, 1, 6, 2).
crosses(299, 2, 7, 1).
crosses(299, 3, 8, 1).
crosses(299, 0, 6, 1).
crosses(299, 1, 7, 1).
crosses(299, 2, 8, 1).
crosses(299, 1, 8, 1).
crosses(300, 0, 2, 1).
crosses(300, 4, 6, 2).
crosses(300, 6, 8, 1).
crosses(300, 8, 10, 2).
crosses(300, 0, 3, 1).
crosses(300, 3, 6, 2).
crosses(300, 4, 7, 2).
crosses(300, 5, 8, 1).
crosses(300, 7, 10, 1).
crosses(300, 0, 4, 1).
crosses(300, 2, 6, 2).
crosses(300, 3, 7, 2).
crosses(300, 4, 8, 3).
crosses(300, 6, 10, 1).
crosses(300, 1, 6, 1).
crosses(300, 2, 7, 2).
crosses(300, 3, 8, 3).
crosses(300, 4, 9, 1).
crosses(300, 0, 6, 1).
crosses(300, 1, 7, 1).
crosses(300, 2, 8, 3).
crosses(300, 3, 9, 1).
crosses(300, 4, 10, 1).
crosses(300, 0, 7, 1).
crosses(300, 1, 8, 2).
crosses(300, 2, 9, 1).
crosses(300, 3, 10, 1).
crosses(300, 0, 8, 2).
crosses(300, 2, 10, 1).
crosses(301, 0, 2, 1).
crosses(301, 2, 4, 2).
crosses(301, 5, 7, 1).
crosses(301, 7, 9, 1).
crosses(301, 8, 10, 1).
crosses(301, 10, 12, 1).
crosses(301, 11, 13, 1).
crosses(301, 12, 14, 1).
crosses(301, 14, 16, 5).
crosses(301, 1, 4, 1).
crosses(301, 2, 5, 2).
crosses(301, 4, 7, 1).
crosses(301, 7, 10, 2).
crosses(301, 8, 11, 1).
crosses(301, 9, 12, 1).
crosses(301, 10, 13, 2).
crosses(301, 11, 14, 2).
crosses(301, 13, 16, 4).
crosses(301, 0, 4, 1).
crosses(301, 1, 5, 1).
crosses(301, 2, 6, 2).
crosses(301, 3, 7, 1).
crosses(301, 6, 10, 1).
crosses(301, 7, 11, 2).
crosses(301, 8, 12, 2).
crosses(301, 9, 13, 2).
crosses(301, 10, 14, 3).
crosses(301, 12, 16, 3).
crosses(301, 0, 5, 1).
crosses(301, 1, 6, 1).
crosses(301, 2, 7, 3).
crosses(301, 5, 10, 1).
crosses(301, 6, 11, 1).
crosses(301, 7, 12, 3).
crosses(301, 8, 13, 3).
crosses(301, 9, 14, 3).
crosses(301, 11, 16, 2).
crosses(301, 0, 6, 1).
crosses(301, 1, 7, 2).
crosses(301, 2, 8, 2).
crosses(301, 4, 10, 1).
crosses(301, 5, 11, 1).
crosses(301, 6, 12, 2).
crosses(301, 7, 13, 4).
crosses(301, 8, 14, 4).
crosses(301, 10, 16, 2).
crosses(301, 0, 7, 2).
crosses(301, 1, 8, 1).
crosses(301, 2, 9, 2).
crosses(301, 3, 10, 1).
crosses(301, 4, 11, 1).
crosses(301, 5, 12, 2).
crosses(301, 6, 13, 3).
crosses(301, 7, 14, 5).
crosses(301, 9, 16, 1).
crosses(301, 0, 8, 1).
crosses(301, 1, 9, 1).
crosses(301, 2, 10, 3).
crosses(301, 3, 11, 1).
crosses(301, 4, 12, 2).
crosses(301, 5, 13, 3).
crosses(301, 6, 14, 4).
crosses(301, 7, 15, 1).
crosses(301, 8, 16, 1).
crosses(301, 0, 9, 1).
crosses(301, 1, 10, 2).
crosses(301, 2, 11, 3).
crosses(301, 3, 12, 2).
crosses(301, 4, 13, 3).
crosses(301, 5, 14, 4).
crosses(301, 7, 16, 2).
crosses(301, 0, 10, 2).
crosses(301, 1, 11, 2).
crosses(301, 2, 12, 4).
crosses(301, 3, 13, 3).
crosses(301, 4, 14, 4).
crosses(301, 6, 16, 1).
crosses(301, 0, 11, 2).
crosses(301, 1, 12, 3).
crosses(301, 2, 13, 5).
crosses(301, 3, 14, 4).
crosses(301, 5, 16, 1).
crosses(301, 0, 12, 3).
crosses(301, 1, 13, 4).
crosses(301, 2, 14, 6).
crosses(301, 4, 16, 1).
crosses(301, 0, 13, 4).
crosses(301, 1, 14, 5).
crosses(301, 2, 15, 1).
crosses(301, 0, 14, 5).
crosses(301, 2, 16, 1).
crosses(302, 1, 3, 1).
crosses(302, 2, 4, 1).
crosses(302, 4, 6, 1).
crosses(302, 1, 4, 2).
crosses(302, 0, 4, 1).
crosses(302, 1, 5, 1).
crosses(302, 1, 6, 1).
crosses(303, 2, 4, 2).
crosses(303, 3, 5, 1).
crosses(303, 5, 7, 2).
crosses(303, 1, 4, 2).
crosses(303, 2, 5, 3).
crosses(303, 4, 7, 1).
crosses(303, 0, 4, 1).
crosses(303, 1, 5, 3).
crosses(303, 2, 6, 1).
crosses(303, 0, 5, 2).
crosses(303, 1, 6, 1).
crosses(303, 2, 7, 1).
crosses(303, 1, 7, 1).
crosses(304, 1, 3, 1).
crosses(304, 2, 4, 1).
crosses(304, 4, 6, 1).
crosses(304, 5, 7, 1).
crosses(304, 6, 8, 1).
crosses(304, 8, 10, 1).
crosses(304, 10, 12, 5).
crosses(304, 1, 4, 2).
crosses(304, 2, 5, 1).
crosses(304, 3, 6, 1).
crosses(304, 4, 7, 2).
crosses(304, 5, 8, 2).
crosses(304, 6, 9, 1).
crosses(304, 7, 10, 1).
crosses(304, 9, 12, 4).
crosses(304, 0, 4, 1).
crosses(304, 1, 5, 2).
crosses(304, 2, 6, 2).
crosses(304, 3, 7, 2).
crosses(304, 4, 8, 3).
crosses(304, 5, 9, 2).
crosses(304, 6, 10, 2).
crosses(304, 8, 12, 4).
crosses(304, 0, 5, 1).
crosses(304, 1, 6, 3).
crosses(304, 2, 7, 3).
crosses(304, 3, 8, 3).
crosses(304, 4, 9, 3).
crosses(304, 5, 10, 3).
crosses(304, 7, 12, 3).
crosses(304, 0, 6, 2).
crosses(304, 1, 7, 4).
crosses(304, 2, 8, 4).
crosses(304, 3, 9, 3).
crosses(304, 4, 10, 4).
crosses(304, 6, 12, 2).
crosses(304, 0, 7, 3).
crosses(304, 1, 8, 5).
crosses(304, 2, 9, 4).
crosses(304, 3, 10, 4).
crosses(304, 5, 12, 1).
crosses(304, 0, 8, 4).
crosses(304, 1, 9, 5).
crosses(304, 2, 10, 5).
crosses(304, 4, 12, 1).
crosses(304, 0, 9, 4).
crosses(304, 1, 10, 6).
crosses(304, 0, 10, 5).
crosses(304, 1, 11, 1).
crosses(304, 1, 12, 1).
crosses(305, 0, 2, 1).
crosses(305, 2, 4, 2).
crosses(305, 4, 6, 1).
crosses(305, 5, 7, 1).
crosses(305, 6, 8, 1).
crosses(305, 8, 10, 1).
crosses(305, 10, 12, 5).
crosses(305, 1, 4, 1).
crosses(305, 2, 5, 2).
crosses(305, 3, 6, 1).
crosses(305, 4, 7, 2).
crosses(305, 5, 8, 2).
crosses(305, 6, 9, 1).
crosses(305, 7, 10, 1).
crosses(305, 9, 12, 4).
crosses(305, 0, 4, 1).
crosses(305, 1, 5, 1).
crosses(305, 2, 6, 3).
crosses(305, 3, 7, 2).
crosses(305, 4, 8, 3).
crosses(305, 5, 9, 2).
crosses(305, 6, 10, 2).
crosses(305, 8, 12, 4).
crosses(305, 0, 5, 1).
crosses(305, 1, 6, 2).
crosses(305, 2, 7, 4).
crosses(305, 3, 8, 3).
crosses(305, 4, 9, 3).
crosses(305, 5, 10, 3).
crosses(305, 7, 12, 3).
crosses(305, 0, 6, 2).
crosses(305, 1, 7, 3).
crosses(305, 2, 8, 5).
crosses(305, 3, 9, 3).
crosses(305, 4, 10, 4).
crosses(305, 6, 12, 2).
crosses(305, 0, 7, 3).
crosses(305, 1, 8, 4).
crosses(305, 2, 9, 5).
crosses(305, 3, 10, 4).
crosses(305, 5, 12, 1).
crosses(305, 0, 8, 4).
crosses(305, 1, 9, 4).
crosses(305, 2, 10, 6).
crosses(305, 4, 12, 1).
crosses(305, 0, 9, 4).
crosses(305, 1, 10, 5).
crosses(305, 2, 11, 1).
crosses(305, 0, 10, 5).
crosses(305, 2, 12, 1).
crosses(306, 1, 3, 2).
crosses(306, 2, 4, 1).
crosses(306, 4, 6, 2).
crosses(306, 0, 3, 1).
crosses(306, 1, 4, 3).
crosses(306, 3, 6, 1).
crosses(306, 0, 4, 2).
crosses(306, 1, 5, 1).
crosses(306, 1, 6, 1).
crosses(307, 2, 4, 2).
crosses(307, 4, 6, 1).
crosses(307, 6, 8, 3).
crosses(307, 7, 9, 1).
crosses(307, 9, 11, 1).
crosses(307, 11, 13, 3).
crosses(307, 1, 4, 2).
crosses(307, 2, 5, 2).
crosses(307, 3, 6, 1).
crosses(307, 5, 8, 2).
crosses(307, 6, 9, 4).
crosses(307, 7, 10, 1).
crosses(307, 8, 11, 1).
crosses(307, 10, 13, 2).
crosses(307, 0, 4, 1).
crosses(307, 1, 5, 2).
crosses(307, 2, 6, 3).
crosses(307, 4, 8, 2).
crosses(307, 5, 9, 3).
crosses(307, 6, 10, 4).
crosses(307, 7, 11, 2).
crosses(307, 9, 13, 2).
crosses(307, 0, 5, 1).
crosses(307, 1, 6, 3).
crosses(307, 2, 7, 1).
crosses(307, 3, 8, 1).
crosses(307, 4, 9, 3).
crosses(307, 5, 10, 3).
crosses(307, 6, 11, 5).
crosses(307, 8, 13, 1).
crosses(307, 0, 6, 2).
crosses(307, 1, 7, 1).
crosses(307, 2, 8, 2).
crosses(307, 3, 9, 2).
crosses(307, 4, 10, 3).
crosses(307, 5, 11, 4).
crosses(307, 6, 12, 2).
crosses(307, 1, 8, 2).
crosses(307, 2, 9, 3).
crosses(307, 3, 10, 2).
crosses(307, 4, 11, 4).
crosses(307, 5, 12, 1).
crosses(307, 6, 13, 2).
crosses(307, 0, 8, 1).
crosses(307, 1, 9, 3).
crosses(307, 2, 10, 3).
crosses(307, 3, 11, 3).
crosses(307, 4, 12, 1).
crosses(307, 5, 13, 1).
crosses(307, 0, 9, 2).
crosses(307, 1, 10, 3).
crosses(307, 2, 11, 4).
crosses(307, 4, 13, 1).
crosses(307, 0, 10, 2).
crosses(307, 1, 11, 4).
crosses(307, 2, 12, 1).
crosses(307, 0, 11, 3).
crosses(307, 1, 12, 1).
crosses(307, 2, 13, 1).
crosses(307, 1, 13, 1).
crosses(308, 0, 2, 1).
crosses(308, 1, 3, 1).
crosses(308, 3, 5, 4).
crosses(308, 5, 7, 1).
crosses(308, 0, 3, 2).
crosses(308, 2, 5, 3).
crosses(308, 3, 6, 3).
crosses(308, 1, 5, 2).
crosses(308, 2, 6, 2).
crosses(308, 3, 7, 3).
crosses(308, 0, 5, 1).
crosses(308, 1, 6, 1).
crosses(308, 2, 7, 2).
crosses(308, 1, 7, 1).
crosses(309, 3, 5, 1).
crosses(309, 2, 5, 1).
crosses(309, 1, 5, 1).
crosses(310, 1, 3, 2).
crosses(310, 4, 6, 1).
crosses(310, 0, 3, 1).
crosses(310, 1, 4, 2).
crosses(310, 3, 6, 1).
crosses(310, 0, 4, 1).
crosses(310, 1, 5, 1).
crosses(310, 1, 6, 1).
crosses(311, 0, 2, 1).
crosses(311, 1, 3, 1).
crosses(311, 3, 5, 4).
crosses(311, 5, 7, 1).
crosses(311, 0, 3, 2).
crosses(311, 2, 5, 3).
crosses(311, 3, 6, 3).
crosses(311, 1, 5, 2).
crosses(311, 2, 6, 2).
crosses(311, 3, 7, 3).
crosses(311, 0, 5, 1).
crosses(311, 1, 6, 1).
crosses(311, 2, 7, 2).
crosses(311, 1, 7, 1).
crosses(312, 3, 5, 1).
crosses(312, 2, 5, 1).
crosses(312, 1, 5, 1).
crosses(313, 1, 3, 2).
crosses(313, 4, 6, 1).
crosses(313, 0, 3, 1).
crosses(313, 1, 4, 2).
crosses(313, 3, 6, 1).
crosses(313, 0, 4, 1).
crosses(313, 1, 5, 1).
crosses(313, 1, 6, 1).
crosses(314, 0, 2, 1).
crosses(314, 1, 3, 1).
crosses(314, 3, 5, 4).
crosses(314, 5, 7, 1).
crosses(314, 0, 3, 2).
crosses(314, 2, 5, 3).
crosses(314, 3, 6, 3).
crosses(314, 1, 5, 2).
crosses(314, 2, 6, 2).
crosses(314, 3, 7, 3).
crosses(314, 0, 5, 1).
crosses(314, 1, 6, 1).
crosses(314, 2, 7, 2).
crosses(314, 1, 7, 1).
crosses(315, 1, 3, 1).
crosses(315, 1, 4, 1).
crosses(315, 1, 5, 1).
crosses(315, 1, 6, 1).
crosses(316, 1, 3, 1).
crosses(316, 1, 4, 1).
crosses(316, 1, 5, 1).
crosses(316, 1, 6, 1).
crosses(317, 0, 2, 1).
crosses(317, 2, 4, 2).
crosses(317, 4, 6, 2).
crosses(317, 7, 9, 2).
crosses(317, 9, 11, 2).
crosses(317, 12, 14, 1).
crosses(317, 1, 4, 1).
crosses(317, 2, 5, 1).
crosses(317, 3, 6, 1).
crosses(317, 4, 7, 2).
crosses(317, 6, 9, 2).
crosses(317, 7, 10, 1).
crosses(317, 8, 11, 1).
crosses(317, 9, 12, 2).
crosses(317, 11, 14, 1).
crosses(317, 0, 4, 1).
crosses(317, 2, 6, 2).
crosses(317, 3, 7, 1).
crosses(317, 4, 8, 1).
crosses(317, 5, 9, 1).
crosses(317, 6, 10, 1).
crosses(317, 7, 11, 2).
crosses(317, 8, 12, 1).
crosses(317, 9, 13, 1).
crosses(317, 1, 6, 1).
crosses(317, 2, 7, 2).
crosses(317, 4, 9, 2).
crosses(317, 6, 11, 2).
crosses(317, 7, 12, 2).
crosses(317, 9, 14, 1).
crosses(317, 0, 6, 1).
crosses(317, 1, 7, 1).
crosses(317, 2, 8, 1).
crosses(317, 3, 9, 1).
crosses(317, 4, 10, 1).
crosses(317, 5, 11, 1).
crosses(317, 6, 12, 2).
crosses(317, 7, 13, 1).
crosses(317, 0, 7, 1).
crosses(317, 2, 9, 2).
crosses(317, 4, 11, 2).
crosses(317, 5, 12, 1).
crosses(317, 6, 13, 1).
crosses(317, 7, 14, 1).
crosses(317, 1, 9, 1).
crosses(317, 2, 10, 1).
crosses(317, 3, 11, 1).
crosses(317, 4, 12, 2).
crosses(317, 6, 14, 1).
crosses(317, 0, 9, 1).
crosses(317, 2, 11, 2).
crosses(317, 3, 12, 1).
crosses(317, 4, 13, 1).
crosses(317, 1, 11, 1).
crosses(317, 2, 12, 2).
crosses(317, 4, 14, 1).
crosses(317, 0, 11, 1).
crosses(317, 1, 12, 1).
crosses(317, 2, 13, 1).
crosses(317, 0, 12, 1).
crosses(317, 2, 14, 1).
crosses(318, 3, 5, 2).
crosses(318, 4, 6, 1).
crosses(318, 6, 8, 2).
crosses(318, 2, 5, 2).
crosses(318, 3, 6, 3).
crosses(318, 5, 8, 1).
crosses(318, 1, 5, 2).
crosses(318, 2, 6, 3).
crosses(318, 3, 7, 1).
crosses(318, 0, 5, 1).
crosses(318, 1, 6, 3).
crosses(318, 2, 7, 1).
crosses(318, 3, 8, 1).
crosses(318, 0, 6, 2).
crosses(318, 1, 7, 1).
crosses(318, 2, 8, 1).
crosses(318, 1, 8, 1).
crosses(319, 2, 4, 2).
crosses(319, 4, 6, 2).
crosses(319, 6, 8, 2).
crosses(319, 7, 9, 1).
crosses(319, 9, 11, 2).
crosses(319, 1, 4, 2).
crosses(319, 2, 5, 1).
crosses(319, 3, 6, 1).
crosses(319, 4, 7, 1).
crosses(319, 5, 8, 1).
crosses(319, 6, 9, 3).
crosses(319, 8, 11, 1).
crosses(319, 0, 4, 1).
crosses(319, 1, 5, 1).
crosses(319, 2, 6, 2).
crosses(319, 4, 8, 2).
crosses(319, 5, 9, 2).
crosses(319, 6, 10, 1).
crosses(319, 1, 6, 2).
crosses(319, 2, 7, 1).
crosses(319, 3, 8, 1).
crosses(319, 4, 9, 3).
crosses(319, 6, 11, 1).
crosses(319, 0, 6, 1).
crosses(319, 1, 7, 1).
crosses(319, 2, 8, 2).
crosses(319, 3, 9, 2).
crosses(319, 4, 10, 1).
crosses(319, 1, 8, 2).
crosses(319, 2, 9, 3).
crosses(319, 4, 11, 1).
crosses(319, 0, 8, 1).
crosses(319, 1, 9, 3).
crosses(319, 2, 10, 1).
crosses(319, 0, 9, 2).
crosses(319, 1, 10, 1).
crosses(319, 2, 11, 1).
crosses(319, 1, 11, 1).
crosses(320, 1, 3, 1).
crosses(320, 3, 5, 3).
crosses(320, 5, 7, 2).
crosses(320, 6, 8, 1).
crosses(320, 8, 10, 2).
crosses(320, 0, 3, 1).
crosses(320, 2, 5, 2).
crosses(320, 3, 6, 2).
crosses(320, 4, 7, 1).
crosses(320, 5, 8, 3).
crosses(320, 7, 10, 1).
crosses(320, 1, 5, 2).
crosses(320, 2, 6, 1).
crosses(320, 3, 7, 3).
crosses(320, 4, 8, 2).
crosses(320, 5, 9, 1).
crosses(320, 0, 5, 1).
crosses(320, 1, 6, 1).
crosses(320, 2, 7, 2).
crosses(320, 3, 8, 4).
crosses(320, 5, 10, 1).
crosses(320, 1, 7, 2).
crosses(320, 2, 8, 3).
crosses(320, 3, 9, 2).
crosses(320, 0, 7, 1).
crosses(320, 1, 8, 3).
crosses(320, 2, 9, 1).
crosses(320, 3, 10, 2).
crosses(320, 0, 8, 2).
crosses(320, 1, 9, 1).
crosses(320, 2, 10, 1).
crosses(320, 1, 10, 1).
crosses(321, 3, 5, 1).
crosses(321, 2, 5, 1).
crosses(321, 1, 5, 1).
crosses(322, 1, 3, 1).
crosses(322, 3, 5, 3).
crosses(322, 5, 7, 2).
crosses(322, 7, 9, 1).
crosses(322, 0, 3, 1).
crosses(322, 2, 5, 2).
crosses(322, 3, 6, 2).
crosses(322, 4, 7, 1).
crosses(322, 5, 8, 1).
crosses(322, 1, 5, 2).
crosses(322, 2, 6, 1).
crosses(322, 3, 7, 3).
crosses(322, 5, 9, 1).
crosses(322, 0, 5, 1).
crosses(322, 1, 6, 1).
crosses(322, 2, 7, 2).
crosses(322, 3, 8, 2).
crosses(322, 1, 7, 2).
crosses(322, 2, 8, 1).
crosses(322, 3, 9, 2).
crosses(322, 0, 7, 1).
crosses(322, 1, 8, 1).
crosses(322, 2, 9, 1).
crosses(322, 1, 9, 1).
crosses(323, 1, 3, 1).
crosses(323, 3, 5, 3).
crosses(323, 5, 7, 2).
crosses(323, 6, 8, 1).
crosses(323, 8, 10, 2).
crosses(323, 0, 3, 1).
crosses(323, 2, 5, 2).
crosses(323, 3, 6, 2).
crosses(323, 4, 7, 1).
crosses(323, 5, 8, 3).
crosses(323, 7, 10, 1).
crosses(323, 1, 5, 2).
crosses(323, 2, 6, 1).
crosses(323, 3, 7, 3).
crosses(323, 4, 8, 2).
crosses(323, 5, 9, 1).
crosses(323, 0, 5, 1).
crosses(323, 1, 6, 1).
crosses(323, 2, 7, 2).
crosses(323, 3, 8, 4).
crosses(323, 5, 10, 1).
crosses(323, 1, 7, 2).
crosses(323, 2, 8, 3).
crosses(323, 3, 9, 2).
crosses(323, 0, 7, 1).
crosses(323, 1, 8, 3).
crosses(323, 2, 9, 1).
crosses(323, 3, 10, 2).
crosses(323, 0, 8, 2).
crosses(323, 1, 9, 1).
crosses(323, 2, 10, 1).
crosses(323, 1, 10, 1).
crosses(324, 3, 5, 1).
crosses(324, 2, 5, 1).
crosses(324, 1, 5, 1).
crosses(325, 0, 2, 1).
crosses(325, 2, 4, 2).
crosses(325, 4, 6, 1).
crosses(325, 1, 4, 1).
crosses(325, 2, 5, 1).
crosses(325, 0, 4, 1).
crosses(325, 2, 6, 1).
crosses(326, 1, 3, 1).
crosses(326, 3, 5, 3).
crosses(326, 5, 7, 2).
crosses(326, 6, 8, 1).
crosses(326, 8, 10, 2).
crosses(326, 0, 3, 1).
crosses(326, 2, 5, 2).
crosses(326, 3, 6, 2).
crosses(326, 4, 7, 1).
crosses(326, 5, 8, 3).
crosses(326, 7, 10, 1).
crosses(326, 1, 5, 2).
crosses(326, 2, 6, 1).
crosses(326, 3, 7, 3).
crosses(326, 4, 8, 2).
crosses(326, 5, 9, 1).
crosses(326, 0, 5, 1).
crosses(326, 1, 6, 1).
crosses(326, 2, 7, 2).
crosses(326, 3, 8, 4).
crosses(326, 5, 10, 1).
crosses(326, 1, 7, 2).
crosses(326, 2, 8, 3).
crosses(326, 3, 9, 2).
crosses(326, 0, 7, 1).
crosses(326, 1, 8, 3).
crosses(326, 2, 9, 1).
crosses(326, 3, 10, 2).
crosses(326, 0, 8, 2).
crosses(326, 1, 9, 1).
crosses(326, 2, 10, 1).
crosses(326, 1, 10, 1).
crosses(327, 3, 5, 1).
crosses(327, 2, 5, 1).
crosses(327, 1, 5, 1).
crosses(328, 0, 2, 1).
crosses(328, 2, 4, 2).
crosses(328, 4, 6, 1).
crosses(328, 1, 4, 1).
crosses(328, 2, 5, 1).
crosses(328, 0, 4, 1).
crosses(328, 2, 6, 1).
crosses(329, 1, 3, 1).
crosses(329, 2, 4, 1).
crosses(329, 5, 7, 1).
crosses(329, 1, 4, 2).
crosses(329, 2, 5, 1).
crosses(329, 4, 7, 1).
crosses(329, 0, 4, 1).
crosses(329, 1, 5, 2).
crosses(329, 0, 5, 1).
crosses(329, 1, 6, 1).
crosses(329, 1, 7, 1).
crosses(330, 2, 4, 2).
crosses(330, 4, 6, 1).
crosses(330, 1, 4, 2).
crosses(330, 2, 5, 1).
crosses(330, 0, 4, 1).
crosses(330, 1, 5, 1).
crosses(330, 2, 6, 1).
crosses(330, 1, 6, 1).
crosses(331, 1, 3, 1).
crosses(331, 2, 4, 1).
crosses(331, 4, 6, 2).
crosses(331, 6, 8, 1).
crosses(331, 1, 4, 2).
crosses(331, 3, 6, 1).
crosses(331, 4, 7, 1).
crosses(331, 0, 4, 1).
crosses(331, 1, 5, 1).
crosses(331, 2, 6, 1).
crosses(331, 4, 8, 1).
crosses(331, 1, 6, 2).
crosses(331, 0, 6, 1).
crosses(331, 1, 7, 1).
crosses(331, 1, 8, 1).
crosses(332, 1, 3, 1).
crosses(332, 2, 4, 1).
crosses(332, 5, 7, 1).
crosses(332, 1, 4, 2).
crosses(332, 2, 5, 1).
crosses(332, 4, 7, 1).
crosses(332, 0, 4, 1).
crosses(332, 1, 5, 2).
crosses(332, 0, 5, 1).
crosses(332, 1, 6, 1).
crosses(332, 1, 7, 1).
crosses(333, 2, 4, 2).
crosses(333, 4, 6, 2).
crosses(333, 6, 8, 1).
crosses(333, 1, 4, 2).
crosses(333, 2, 5, 1).
crosses(333, 3, 6, 1).
crosses(333, 4, 7, 1).
crosses(333, 0, 4, 1).
crosses(333, 1, 5, 1).
crosses(333, 2, 6, 2).
crosses(333, 4, 8, 1).
crosses(333, 1, 6, 2).
crosses(333, 2, 7, 1).
crosses(333, 0, 6, 1).
crosses(333, 1, 7, 1).
crosses(333, 2, 8, 1).
crosses(333, 1, 8, 1).
crosses(334, 2, 4, 1).
crosses(334, 3, 5, 1).
crosses(334, 5, 7, 2).
crosses(334, 7, 9, 1).
crosses(334, 1, 4, 1).
crosses(334, 2, 5, 2).
crosses(334, 4, 7, 1).
crosses(334, 5, 8, 1).
crosses(334, 1, 5, 2).
crosses(334, 2, 6, 1).
crosses(334, 3, 7, 1).
crosses(334, 5, 9, 1).
crosses(334, 0, 5, 1).
crosses(334, 1, 6, 1).
crosses(334, 2, 7, 2).
crosses(334, 1, 7, 2).
crosses(334, 2, 8, 1).
crosses(334, 0, 7, 1).
crosses(334, 1, 8, 1).
crosses(334, 2, 9, 1).
crosses(334, 1, 9, 1).
crosses(335, 1, 3, 1).
crosses(335, 3, 5, 1).
crosses(335, 0, 3, 1).
crosses(336, 0, 2, 1).
crosses(336, 2, 4, 1).
crosses(337, 1, 3, 1).
crosses(337, 4, 6, 1).
crosses(337, 7, 9, 2).
crosses(337, 0, 3, 1).
crosses(337, 1, 4, 1).
crosses(337, 3, 6, 1).
crosses(337, 4, 7, 1).
crosses(337, 6, 9, 2).
crosses(337, 0, 4, 1).
crosses(337, 1, 5, 1).
crosses(337, 2, 6, 1).
crosses(337, 3, 7, 1).
crosses(337, 5, 9, 1).
crosses(337, 0, 5, 1).
crosses(337, 1, 6, 2).
crosses(337, 2, 7, 1).
crosses(337, 4, 9, 1).
crosses(337, 0, 6, 2).
crosses(337, 1, 7, 2).
crosses(337, 3, 9, 1).
crosses(337, 0, 7, 2).
crosses(338, 1, 3, 1).
crosses(338, 3, 5, 1).
crosses(338, 5, 7, 3).
crosses(338, 6, 8, 1).
crosses(338, 8, 10, 1).
crosses(338, 10, 12, 3).
crosses(338, 12, 14, 1).
crosses(338, 0, 3, 1).
crosses(338, 1, 4, 1).
crosses(338, 2, 5, 1).
crosses(338, 4, 7, 2).
crosses(338, 5, 8, 4).
crosses(338, 6, 9, 1).
crosses(338, 7, 10, 1).
crosses(338, 9, 12, 2).
crosses(338, 10, 13, 2).
crosses(338, 12, 15, 1).
crosses(338, 0, 4, 1).
crosses(338, 1, 5, 2).
crosses(338, 3, 7, 2).
crosses(338, 4, 8, 3).
crosses(338, 5, 9, 4).
crosses(338, 6, 10, 2).
crosses(338, 8, 12, 2).
crosses(338, 9, 13, 1).
crosses(338, 10, 14, 2).
crosses(338, 0, 5, 2).
crosses(338, 2, 7, 1).
crosses(338, 3, 8, 3).
crosses(338, 4, 9, 3).
crosses(338, 5, 10, 5).
crosses(338, 7, 12, 1).
crosses(338, 8, 13, 1).
crosses(338, 9, 14, 1).
crosses(338, 10, 15, 2).
crosses(338, 1, 7, 1).
crosses(338, 2, 8, 2).
crosses(338, 3, 9, 3).
crosses(338, 4, 10, 4).
crosses(338, 5, 11, 3).
crosses(338, 6, 12, 1).
crosses(338, 8, 14, 1).
crosses(338, 9, 15, 1).
crosses(338, 1, 8, 2).
crosses(338, 2, 9, 2).
crosses(338, 3, 10, 4).
crosses(338, 4, 11, 2).
crosses(338, 5, 12, 4).
crosses(338, 8, 15, 1).
crosses(338, 0, 8, 1).
crosses(338, 1, 9, 2).
crosses(338, 2, 10, 3).
crosses(338, 3, 11, 2).
crosses(338, 4, 12, 3).
crosses(338, 5, 13, 3).
crosses(338, 0, 9, 1).
crosses(338, 1, 10, 3).
crosses(338, 2, 11, 1).
crosses(338, 3, 12, 3).
crosses(338, 4, 13, 2).
crosses(338, 5, 14, 3).
crosses(338, 0, 10, 2).
crosses(338, 1, 11, 1).
crosses(338, 2, 12, 2).
crosses(338, 3, 13, 2).
crosses(338, 4, 14, 2).
crosses(338, 5, 15, 3).
crosses(338, 1, 12, 2).
crosses(338, 2, 13, 1).
crosses(338, 3, 14, 2).
crosses(338, 4, 15, 2).
crosses(338, 0, 12, 1).
crosses(338, 1, 13, 1).
crosses(338, 2, 14, 1).
crosses(338, 3, 15, 2).
crosses(338, 1, 14, 1).
crosses(338, 2, 15, 1).
crosses(338, 1, 15, 1).
crosses(339, 1, 3, 1).
crosses(339, 3, 5, 1).
crosses(339, 6, 8, 1).
crosses(339, 8, 10, 3).
crosses(339, 0, 3, 1).
crosses(339, 1, 4, 1).
crosses(339, 2, 5, 1).
crosses(339, 3, 6, 1).
crosses(339, 5, 8, 1).
crosses(339, 7, 10, 2).
crosses(339, 0, 4, 1).
crosses(339, 1, 5, 2).
crosses(339, 2, 6, 1).
crosses(339, 3, 7, 1).
crosses(339, 4, 8, 1).
crosses(339, 6, 10, 2).
crosses(339, 0, 5, 2).
crosses(339, 1, 6, 2).
crosses(339, 2, 7, 1).
crosses(339, 3, 8, 2).
crosses(339, 5, 10, 2).
crosses(339, 0, 6, 2).
crosses(339, 1, 7, 2).
crosses(339, 2, 8, 2).
crosses(339, 4, 10, 1).
crosses(339, 0, 7, 2).
crosses(339, 1, 8, 3).
crosses(339, 3, 10, 1).
crosses(339, 0, 8, 3).
crosses(340, 1, 3, 1).
crosses(340, 3, 5, 1).
crosses(340, 5, 7, 3).
crosses(340, 6, 8, 1).
crosses(340, 9, 11, 2).
crosses(340, 0, 3, 1).
crosses(340, 1, 4, 1).
crosses(340, 2, 5, 1).
crosses(340, 4, 7, 2).
crosses(340, 5, 8, 4).
crosses(340, 6, 9, 1).
crosses(340, 8, 11, 2).
crosses(340, 0, 4, 1).
crosses(340, 1, 5, 2).
crosses(340, 3, 7, 2).
crosses(340, 4, 8, 3).
crosses(340, 5, 9, 4).
crosses(340, 7, 11, 1).
crosses(340, 0, 5, 2).
crosses(340, 2, 7, 1).
crosses(340, 3, 8, 3).
crosses(340, 4, 9, 3).
crosses(340, 5, 10, 2).
crosses(340, 1, 7, 1).
crosses(340, 2, 8, 2).
crosses(340, 3, 9, 3).
crosses(340, 4, 10, 1).
crosses(340, 5, 11, 2).
crosses(340, 0, 7, 1).
crosses(340, 1, 8, 2).
crosses(340, 2, 9, 2).
crosses(340, 3, 10, 1).
crosses(340, 4, 11, 1).
crosses(340, 0, 8, 2).
crosses(340, 1, 9, 2).
crosses(340, 3, 11, 1).
crosses(340, 0, 9, 2).
crosses(341, 1, 3, 1).
crosses(341, 3, 5, 1).
crosses(341, 5, 7, 3).
crosses(341, 6, 8, 1).
crosses(341, 8, 10, 1).
crosses(341, 10, 12, 3).
crosses(341, 12, 14, 1).
crosses(341, 0, 3, 1).
crosses(341, 1, 4, 1).
crosses(341, 2, 5, 1).
crosses(341, 4, 7, 2).
crosses(341, 5, 8, 4).
crosses(341, 6, 9, 1).
crosses(341, 7, 10, 1).
crosses(341, 9, 12, 2).
crosses(341, 10, 13, 2).
crosses(341, 12, 15, 1).
crosses(341, 0, 4, 1).
crosses(341, 1, 5, 2).
crosses(341, 3, 7, 2).
crosses(341, 4, 8, 3).
crosses(341, 5, 9, 4).
crosses(341, 6, 10, 2).
crosses(341, 8, 12, 2).
crosses(341, 9, 13, 1).
crosses(341, 10, 14, 2).
crosses(341, 0, 5, 2).
crosses(341, 2, 7, 1).
crosses(341, 3, 8, 3).
crosses(341, 4, 9, 3).
crosses(341, 5, 10, 5).
crosses(341, 7, 12, 1).
crosses(341, 8, 13, 1).
crosses(341, 9, 14, 1).
crosses(341, 10, 15, 2).
crosses(341, 1, 7, 1).
crosses(341, 2, 8, 2).
crosses(341, 3, 9, 3).
crosses(341, 4, 10, 4).
crosses(341, 5, 11, 3).
crosses(341, 6, 12, 1).
crosses(341, 8, 14, 1).
crosses(341, 9, 15, 1).
crosses(341, 1, 8, 2).
crosses(341, 2, 9, 2).
crosses(341, 3, 10, 4).
crosses(341, 4, 11, 2).
crosses(341, 5, 12, 4).
crosses(341, 8, 15, 1).
crosses(341, 0, 8, 1).
crosses(341, 1, 9, 2).
crosses(341, 2, 10, 3).
crosses(341, 3, 11, 2).
crosses(341, 4, 12, 3).
crosses(341, 5, 13, 3).
crosses(341, 0, 9, 1).
crosses(341, 1, 10, 3).
crosses(341, 2, 11, 1).
crosses(341, 3, 12, 3).
crosses(341, 4, 13, 2).
crosses(341, 5, 14, 3).
crosses(341, 0, 10, 2).
crosses(341, 1, 11, 1).
crosses(341, 2, 12, 2).
crosses(341, 3, 13, 2).
crosses(341, 4, 14, 2).
crosses(341, 5, 15, 3).
crosses(341, 1, 12, 2).
crosses(341, 2, 13, 1).
crosses(341, 3, 14, 2).
crosses(341, 4, 15, 2).
crosses(341, 0, 12, 1).
crosses(341, 1, 13, 1).
crosses(341, 2, 14, 1).
crosses(341, 3, 15, 2).
crosses(341, 1, 14, 1).
crosses(341, 2, 15, 1).
crosses(341, 1, 15, 1).
crosses(342, 1, 3, 1).
crosses(342, 3, 5, 1).
crosses(342, 5, 7, 1).
crosses(342, 7, 9, 3).
crosses(342, 0, 3, 1).
crosses(342, 1, 4, 1).
crosses(342, 2, 5, 1).
crosses(342, 3, 6, 1).
crosses(342, 4, 7, 1).
crosses(342, 6, 9, 2).
crosses(342, 0, 4, 1).
crosses(342, 1, 5, 2).
crosses(342, 2, 6, 1).
crosses(342, 3, 7, 2).
crosses(342, 5, 9, 2).
crosses(342, 0, 5, 2).
crosses(342, 1, 6, 2).
crosses(342, 2, 7, 2).
crosses(342, 4, 9, 1).
crosses(342, 0, 6, 2).
crosses(342, 1, 7, 3).
crosses(342, 3, 9, 1).
crosses(342, 0, 7, 3).
crosses(343, 1, 3, 1).
crosses(343, 3, 5, 1).
crosses(343, 5, 7, 3).
crosses(343, 6, 8, 1).
crosses(343, 9, 11, 2).
crosses(343, 0, 3, 1).
crosses(343, 1, 4, 1).
crosses(343, 2, 5, 1).
crosses(343, 4, 7, 2).
crosses(343, 5, 8, 4).
crosses(343, 6, 9, 1).
crosses(343, 8, 11, 2).
crosses(343, 0, 4, 1).
crosses(343, 1, 5, 2).
crosses(343, 3, 7, 2).
crosses(343, 4, 8, 3).
crosses(343, 5, 9, 4).
crosses(343, 7, 11, 1).
crosses(343, 0, 5, 2).
crosses(343, 2, 7, 1).
crosses(343, 3, 8, 3).
crosses(343, 4, 9, 3).
crosses(343, 5, 10, 2).
crosses(343, 1, 7, 1).
crosses(343, 2, 8, 2).
crosses(343, 3, 9, 3).
crosses(343, 4, 10, 1).
crosses(343, 5, 11, 2).
crosses(343, 0, 7, 1).
crosses(343, 1, 8, 2).
crosses(343, 2, 9, 2).
crosses(343, 3, 10, 1).
crosses(343, 4, 11, 1).
crosses(343, 0, 8, 2).
crosses(343, 1, 9, 2).
crosses(343, 3, 11, 1).
crosses(343, 0, 9, 2).
crosses(344, 2, 4, 1).
crosses(344, 4, 6, 1).
crosses(344, 7, 9, 4).
crosses(344, 10, 12, 2).
crosses(344, 12, 14, 1).
crosses(344, 14, 16, 2).
crosses(344, 16, 18, 1).
crosses(344, 18, 20, 3).
crosses(344, 1, 4, 1).
crosses(344, 2, 5, 1).
crosses(344, 3, 6, 1).
crosses(344, 4, 7, 1).
crosses(344, 6, 9, 4).
crosses(344, 7, 10, 4).
crosses(344, 9, 12, 2).
crosses(344, 10, 13, 2).
crosses(344, 11, 14, 1).
crosses(344, 13, 16, 1).
crosses(344, 14, 17, 2).
crosses(344, 15, 18, 1).
crosses(344, 17, 20, 2).
crosses(344, 0, 4, 1).
crosses(344, 1, 5, 1).
crosses(344, 2, 6, 2).
crosses(344, 3, 7, 1).
crosses(344, 5, 9, 3).
crosses(344, 6, 10, 4).
crosses(344, 7, 11, 3).
crosses(344, 8, 12, 1).
crosses(344, 9, 13, 2).
crosses(344, 10, 14, 3).
crosses(344, 12, 16, 1).
crosses(344, 13, 17, 1).
crosses(344, 14, 18, 3).
crosses(344, 16, 20, 2).
crosses(344, 0, 5, 1).
crosses(344, 1, 6, 2).
crosses(344, 2, 7, 2).
crosses(344, 4, 9, 3).
crosses(344, 5, 10, 3).
crosses(344, 6, 11, 3).
crosses(344, 7, 12, 4).
crosses(344, 8, 13, 1).
crosses(344, 9, 14, 3).
crosses(344, 10, 15, 2).
crosses(344, 11, 16, 1).
crosses(344, 12, 17, 1).
crosses(344, 13, 18, 2).
crosses(344, 14, 19, 1).
crosses(344, 15, 20, 1).
crosses(344, 0, 6, 2).
crosses(344, 1, 7, 2).
crosses(344, 3, 9, 2).
crosses(344, 4, 10, 3).
crosses(344, 5, 11, 2).
crosses(344, 6, 12, 4).
crosses(344, 7, 13, 4).
crosses(344, 8, 14, 2).
crosses(344, 9, 15, 2).
crosses(344, 10, 16, 3).
crosses(344, 11, 17, 1).
crosses(344, 12, 18, 2).
crosses(344, 14, 20, 2).
crosses(344, 0, 7, 2).
crosses(344, 2, 9, 2).
crosses(344, 3, 10, 2).
crosses(344, 4, 11, 2).
crosses(344, 5, 12, 3).
crosses(344, 6, 13, 4).
crosses(344, 7, 14, 5).
crosses(344, 8, 15, 1).
crosses(344, 9, 16, 3).
crosses(344, 10, 17, 3).
crosses(344, 11, 18, 2).
crosses(344, 13, 20, 1).
crosses(344, 1, 9, 2).
crosses(344, 2, 10, 2).
crosses(344, 3, 11, 1).
crosses(344, 4, 12, 3).
crosses(344, 5, 13, 3).
crosses(344, 6, 14, 5).
crosses(344, 7, 15, 4).
crosses(344, 8, 16, 2).
crosses(344, 9, 17, 3).
crosses(344, 10, 18, 4).
crosses(344, 12, 20, 1).
crosses(344, 0, 9, 1).
crosses(344, 1, 10, 2).
crosses(344, 2, 11, 1).
crosses(344, 3, 12, 2).
crosses(344, 4, 13, 3).
crosses(344, 5, 14, 4).
crosses(344, 6, 15, 4).
crosses(344, 7, 16, 5).
crosses(344, 8, 17, 2).
crosses(344, 9, 18, 4).
crosses(344, 10, 19, 1).
crosses(344, 0, 10, 1).
crosses(344, 1, 11, 1).
crosses(344, 2, 12, 2).
crosses(344, 3, 13, 2).
crosses(344, 4, 14, 4).
crosses(344, 5, 15, 3).
crosses(344, 6, 16, 5).
crosses(344, 7, 17, 5).
crosses(344, 8, 18, 3).
crosses(344, 9, 19, 1).
crosses(344, 10, 20, 1).
crosses(344, 1, 12, 2).
crosses(344, 2, 13, 2).
crosses(344, 3, 14, 3).
crosses(344, 4, 15, 3).
crosses(344, 5, 16, 4).
crosses(344, 6, 17, 5).
crosses(344, 7, 18, 6).
crosses(344, 9, 20, 1).
crosses(344, 0, 12, 1).
crosses(344, 1, 13, 2).
crosses(344, 2, 14, 3).
crosses(344, 3, 15, 2).
crosses(344, 4, 16, 4).
crosses(344, 5, 17, 4).
crosses(344, 6, 18, 6).
crosses(344, 7, 19, 3).
crosses(344, 0, 13, 1).
crosses(344, 1, 14, 3).
crosses(344, 2, 15, 2).
crosses(344, 3, 16, 3).
crosses(344, 4, 17, 4).
crosses(344, 5, 18, 5).
crosses(344, 6, 19, 3).
crosses(344, 7, 20, 3).
crosses(344, 0, 14, 2).
crosses(344, 1, 15, 2).
crosses(344, 2, 16, 3).
crosses(344, 3, 17, 3).
crosses(344, 4, 18, 5).
crosses(344, 5, 19, 2).
crosses(344, 6, 20, 3).
crosses(344, 0, 15, 1).
crosses(344, 1, 16, 3).
crosses(344, 2, 17, 3).
crosses(344, 3, 18, 4).
crosses(344, 4, 19, 2).
crosses(344, 5, 20, 2).
crosses(344, 0, 16, 2).
crosses(344, 1, 17, 3).
crosses(344, 2, 18, 4).
crosses(344, 3, 19, 1).
crosses(344, 4, 20, 2).
crosses(344, 0, 17, 2).
crosses(344, 1, 18, 4).
crosses(344, 2, 19, 1).
crosses(344, 3, 20, 1).
crosses(344, 0, 18, 3).
crosses(344, 1, 19, 1).
crosses(344, 2, 20, 1).
crosses(344, 1, 20, 1).
crosses(345, 1, 3, 1).
crosses(345, 4, 6, 1).
crosses(345, 6, 8, 1).
crosses(345, 10, 12, 3).
crosses(345, 0, 3, 1).
crosses(345, 1, 4, 1).
crosses(345, 3, 6, 1).
crosses(345, 4, 7, 1).
crosses(345, 5, 8, 1).
crosses(345, 6, 9, 1).
crosses(345, 9, 12, 3).
crosses(345, 0, 4, 1).
crosses(345, 1, 5, 1).
crosses(345, 2, 6, 1).
crosses(345, 3, 7, 1).
crosses(345, 4, 8, 2).
crosses(345, 5, 9, 1).
crosses(345, 6, 10, 1).
crosses(345, 8, 12, 3).
crosses(345, 0, 5, 1).
crosses(345, 1, 6, 2).
crosses(345, 2, 7, 1).
crosses(345, 3, 8, 2).
crosses(345, 4, 9, 2).
crosses(345, 5, 10, 1).
crosses(345, 7, 12, 2).
crosses(345, 0, 6, 2).
crosses(345, 1, 7, 2).
crosses(345, 2, 8, 2).
crosses(345, 3, 9, 2).
crosses(345, 4, 10, 2).
crosses(345, 6, 12, 2).
crosses(345, 0, 7, 2).
crosses(345, 1, 8, 3).
crosses(345, 2, 9, 2).
crosses(345, 3, 10, 2).
crosses(345, 5, 12, 1).
crosses(345, 0, 8, 3).
crosses(345, 1, 9, 3).
crosses(345, 2, 10, 2).
crosses(345, 4, 12, 1).
crosses(345, 0, 9, 3).
crosses(345, 1, 10, 3).
crosses(345, 3, 12, 1).
crosses(345, 0, 10, 3).
crosses(346, 0, 2, 1).
crosses(346, 2, 4, 2).
crosses(346, 5, 7, 2).
crosses(346, 7, 9, 1).
crosses(346, 9, 11, 2).
crosses(346, 1, 4, 1).
crosses(346, 2, 5, 2).
crosses(346, 4, 7, 2).
crosses(346, 5, 8, 2).
crosses(346, 6, 9, 1).
crosses(346, 8, 11, 1).
crosses(346, 0, 4, 1).
crosses(346, 1, 5, 1).
crosses(346, 2, 6, 1).
crosses(346, 3, 7, 1).
crosses(346, 4, 8, 2).
crosses(346, 5, 9, 3).
crosses(346, 7, 11, 1).
crosses(346, 0, 5, 1).
crosses(346, 2, 7, 2).
crosses(346, 3, 8, 1).
crosses(346, 4, 9, 3).
crosses(346, 5, 10, 1).
crosses(346, 1, 7, 1).
crosses(346, 2, 8, 2).
crosses(346, 3, 9, 2).
crosses(346, 4, 10, 1).
crosses(346, 5, 11, 1).
crosses(346, 0, 7, 1).
crosses(346, 1, 8, 1).
crosses(346, 2, 9, 3).
crosses(346, 4, 11, 1).
crosses(346, 0, 8, 1).
crosses(346, 1, 9, 2).
crosses(346, 2, 10, 1).
crosses(346, 0, 9, 2).
crosses(346, 2, 11, 1).
crosses(347, 2, 4, 2).
crosses(347, 4, 6, 1).
crosses(347, 1, 4, 2).
crosses(347, 2, 5, 1).
crosses(347, 0, 4, 1).
crosses(347, 1, 5, 1).
crosses(347, 2, 6, 1).
crosses(347, 1, 6, 1).
crosses(348, 1, 3, 1).
crosses(348, 3, 5, 1).
crosses(348, 4, 6, 1).
crosses(348, 6, 8, 3).
crosses(348, 8, 10, 1).
crosses(348, 10, 12, 1).
crosses(348, 0, 3, 1).
crosses(348, 1, 4, 1).
crosses(348, 2, 5, 1).
crosses(348, 3, 6, 2).
crosses(348, 5, 8, 2).
crosses(348, 6, 9, 3).
crosses(348, 7, 10, 1).
crosses(348, 0, 4, 1).
crosses(348, 1, 5, 2).
crosses(348, 2, 6, 2).
crosses(348, 4, 8, 1).
crosses(348, 5, 9, 2).
crosses(348, 6, 10, 4).
crosses(348, 0, 5, 2).
crosses(348, 1, 6, 3).
crosses(348, 3, 8, 1).
crosses(348, 4, 9, 1).
crosses(348, 5, 10, 3).
crosses(348, 6, 11, 3).
crosses(348, 0, 6, 3).
crosses(348, 3, 9, 1).
crosses(348, 4, 10, 2).
crosses(348, 5, 11, 2).
crosses(348, 6, 12, 3).
crosses(348, 3, 10, 2).
crosses(348, 4, 11, 1).
crosses(348, 5, 12, 2).
crosses(348, 2, 10, 1).
crosses(348, 3, 11, 1).
crosses(348, 4, 12, 1).
crosses(348, 1, 10, 1).
crosses(348, 3, 12, 1).
crosses(348, 0, 10, 1).
crosses(349, 2, 4, 2).
crosses(349, 4, 6, 1).
crosses(349, 6, 8, 3).
crosses(349, 7, 9, 1).
crosses(349, 8, 10, 1).
crosses(349, 9, 11, 1).
crosses(349, 11, 13, 4).
crosses(349, 1, 4, 2).
crosses(349, 2, 5, 2).
crosses(349, 3, 6, 1).
crosses(349, 5, 8, 2).
crosses(349, 6, 9, 4).
crosses(349, 7, 10, 2).
crosses(349, 8, 11, 2).
crosses(349, 10, 13, 3).
crosses(349, 0, 4, 1).
crosses(349, 1, 5, 2).
crosses(349, 2, 6, 3).
crosses(349, 4, 8, 2).
crosses(349, 5, 9, 3).
crosses(349, 6, 10, 5).
crosses(349, 7, 11, 3).
crosses(349, 9, 13, 2).
crosses(349, 0, 5, 1).
crosses(349, 1, 6, 3).
crosses(349, 2, 7, 1).
crosses(349, 3, 8, 1).
crosses(349, 4, 9, 3).
crosses(349, 5, 10, 4).
crosses(349, 6, 11, 6).
crosses(349, 8, 13, 1).
crosses(349, 0, 6, 2).
crosses(349, 1, 7, 1).
crosses(349, 2, 8, 2).
crosses(349, 3, 9, 2).
crosses(349, 4, 10, 4).
crosses(349, 5, 11, 5).
crosses(349, 6, 12, 2).
crosses(349, 1, 8, 2).
crosses(349, 2, 9, 3).
crosses(349, 3, 10, 3).
crosses(349, 4, 11, 5).
crosses(349, 5, 12, 1).
crosses(349, 6, 13, 2).
crosses(349, 0, 8, 1).
crosses(349, 1, 9, 3).
crosses(349, 2, 10, 4).
crosses(349, 3, 11, 4).
crosses(349, 4, 12, 1).
crosses(349, 5, 13, 1).
crosses(349, 0, 9, 2).
crosses(349, 1, 10, 4).
crosses(349, 2, 11, 5).
crosses(349, 4, 13, 1).
crosses(349, 0, 10, 3).
crosses(349, 1, 11, 5).
crosses(349, 2, 12, 1).
crosses(349, 0, 11, 4).
crosses(349, 1, 12, 1).
crosses(349, 2, 13, 1).
crosses(349, 1, 13, 1).
crosses(350, 4, 6, 1).
crosses(350, 5, 7, 1).
crosses(350, 7, 9, 1).
crosses(350, 3, 6, 1).
crosses(350, 4, 7, 2).
crosses(350, 2, 6, 1).
crosses(350, 3, 7, 2).
crosses(350, 4, 8, 1).
crosses(350, 1, 6, 1).
crosses(350, 2, 7, 2).
crosses(350, 3, 8, 1).
crosses(350, 4, 9, 1).
crosses(350, 1, 7, 2).
crosses(350, 2, 8, 1).
crosses(350, 3, 9, 1).
crosses(350, 0, 7, 1).
crosses(350, 1, 8, 1).
crosses(350, 2, 9, 1).
crosses(350, 1, 9, 1).
crosses(351, 1, 3, 2).
crosses(351, 3, 5, 1).
crosses(351, 5, 7, 3).
crosses(351, 6, 8, 1).
crosses(351, 8, 10, 2).
crosses(351, 0, 3, 1).
crosses(351, 1, 4, 2).
crosses(351, 2, 5, 1).
crosses(351, 4, 7, 2).
crosses(351, 5, 8, 4).
crosses(351, 7, 10, 1).
crosses(351, 0, 4, 1).
crosses(351, 1, 5, 3).
crosses(351, 3, 7, 2).
crosses(351, 4, 8, 3).
crosses(351, 5, 9, 2).
crosses(351, 0, 5, 2).
crosses(351, 1, 6, 1).
crosses(351, 2, 7, 1).
crosses(351, 3, 8, 3).
crosses(351, 4, 9, 1).
crosses(351, 5, 10, 2).
crosses(351, 1, 7, 2).
crosses(351, 2, 8, 2).
crosses(351, 3, 9, 1).
crosses(351, 4, 10, 1).
crosses(351, 0, 7, 1).
crosses(351, 1, 8, 3).
crosses(351, 3, 10, 1).
crosses(351, 0, 8, 2).
crosses(351, 1, 9, 1).
crosses(351, 1, 10, 1).
crosses(352, 1, 3, 1).
crosses(352, 2, 4, 1).
crosses(352, 3, 5, 1).
crosses(352, 5, 7, 1).
crosses(352, 7, 9, 5).
crosses(352, 8, 10, 1).
crosses(352, 10, 12, 1).
crosses(352, 12, 14, 3).
crosses(352, 14, 16, 1).
crosses(352, 0, 3, 1).
crosses(352, 1, 4, 2).
crosses(352, 2, 5, 2).
crosses(352, 3, 6, 1).
crosses(352, 4, 7, 1).
crosses(352, 6, 9, 4).
crosses(352, 7, 10, 6).
crosses(352, 8, 11, 1).
crosses(352, 9, 12, 1).
crosses(352, 11, 14, 2).
crosses(352, 12, 15, 2).
crosses(352, 14, 17, 1).
crosses(352, 0, 4, 2).
crosses(352, 1, 5, 3).
crosses(352, 2, 6, 2).
crosses(352, 3, 7, 2).
crosses(352, 5, 9, 4).
crosses(352, 6, 10, 5).
crosses(352, 7, 11, 6).
crosses(352, 8, 12, 2).
crosses(352, 10, 14, 2).
crosses(352, 11, 15, 1).
crosses(352, 12, 16, 2).
crosses(352, 0, 5, 3).
crosses(352, 1, 6, 3).
crosses(352, 2, 7, 3).
crosses(352, 4, 9, 3).
crosses(352, 5, 10, 5).
crosses(352, 6, 11, 5).
crosses(352, 7, 12, 7).
crosses(352, 9, 14, 1).
crosses(352, 10, 15, 1).
crosses(352, 11, 16, 1).
crosses(352, 12, 17, 2).
crosses(352, 0, 6, 3).
crosses(352, 1, 7, 4).
crosses(352, 3, 9, 2).
crosses(352, 4, 10, 4).
crosses(352, 5, 11, 5).
crosses(352, 6, 12, 6).
crosses(352, 7, 13, 5).
crosses(352, 8, 14, 1).
crosses(352, 10, 16, 1).
crosses(352, 11, 17, 1).
crosses(352, 0, 7, 4).
crosses(352, 2, 9, 1).
crosses(352, 3, 10, 3).
crosses(352, 4, 11, 4).
crosses(352, 5, 12, 6).
crosses(352, 6, 13, 4).
crosses(352, 7, 14, 6).
crosses(352, 10, 17, 1).
crosses(352, 1, 9, 1).
crosses(352, 2, 10, 2).
crosses(352, 3, 11, 3).
crosses(352, 4, 12, 5).
crosses(352, 5, 13, 4).
crosses(352, 6, 14, 5).
crosses(352, 7, 15, 5).
crosses(352, 1, 10, 2).
crosses(352, 2, 11, 2).
crosses(352, 3, 12, 4).
crosses(352, 4, 13, 3).
crosses(352, 5, 14, 5).
crosses(352, 6, 15, 4).
crosses(352, 7, 16, 5).
crosses(352, 0, 10, 1).
crosses(352, 1, 11, 2).
crosses(352, 2, 12, 3).
crosses(352, 3, 13, 2).
crosses(352, 4, 14, 4).
crosses(352, 5, 15, 4).
crosses(352, 6, 16, 4).
crosses(352, 7, 17, 5).
crosses(352, 0, 11, 1).
crosses(352, 1, 12, 3).
crosses(352, 2, 13, 1).
crosses(352, 3, 14, 3).
crosses(352, 4, 15, 3).
crosses(352, 5, 16, 4).
crosses(352, 6, 17, 4).
crosses(352, 0, 12, 2).
crosses(352, 1, 13, 1).
crosses(352, 2, 14, 2).
crosses(352, 3, 15, 2).
crosses(352, 4, 16, 3).
crosses(352, 5, 17, 4).
crosses(352, 1, 14, 2).
crosses(352, 2, 15, 1).
crosses(352, 3, 16, 2).
crosses(352, 4, 17, 3).
crosses(352, 0, 14, 1).
crosses(352, 1, 15, 1).
crosses(352, 2, 16, 1).
crosses(352, 3, 17, 2).
crosses(352, 1, 16, 1).
crosses(352, 2, 17, 1).
crosses(352, 1, 17, 1).
crosses(353, 1, 3, 1).
crosses(353, 2, 4, 1).
crosses(353, 4, 6, 1).
crosses(353, 6, 8, 2).
crosses(353, 1, 4, 2).
crosses(353, 2, 5, 1).
crosses(353, 3, 6, 1).
crosses(353, 5, 8, 1).
crosses(353, 0, 4, 1).
crosses(353, 1, 5, 2).
crosses(353, 2, 6, 2).
crosses(353, 4, 8, 1).
crosses(353, 0, 5, 1).
crosses(353, 1, 6, 3).
crosses(353, 0, 6, 2).
crosses(353, 1, 7, 1).
crosses(353, 1, 8, 1).
crosses(354, 1, 3, 1).
crosses(354, 3, 5, 1).
crosses(354, 0, 3, 1).
crosses(355, 0, 2, 1).
crosses(355, 2, 4, 2).
crosses(355, 4, 6, 1).
crosses(355, 6, 8, 2).
crosses(355, 1, 4, 1).
crosses(355, 2, 5, 2).
crosses(355, 3, 6, 1).
crosses(355, 5, 8, 1).
crosses(355, 0, 4, 1).
crosses(355, 1, 5, 1).
crosses(355, 2, 6, 3).
crosses(355, 4, 8, 1).
crosses(355, 0, 5, 1).
crosses(355, 1, 6, 2).
crosses(355, 2, 7, 1).
crosses(355, 0, 6, 2).
crosses(355, 2, 8, 1).
crosses(356, 0, 2, 1).
crosses(356, 2, 4, 2).
crosses(356, 4, 6, 1).
crosses(356, 6, 8, 2).
crosses(356, 7, 9, 1).
crosses(356, 9, 11, 3).
crosses(356, 1, 4, 1).
crosses(356, 2, 5, 2).
crosses(356, 3, 6, 1).
crosses(356, 5, 8, 1).
crosses(356, 6, 9, 3).
crosses(356, 8, 11, 2).
crosses(356, 0, 4, 1).
crosses(356, 1, 5, 1).
crosses(356, 2, 6, 3).
crosses(356, 4, 8, 1).
crosses(356, 5, 9, 2).
crosses(356, 6, 10, 1).
crosses(356, 7, 11, 1).
crosses(356, 0, 5, 1).
crosses(356, 1, 6, 2).
crosses(356, 2, 7, 2).
crosses(356, 3, 8, 1).
crosses(356, 4, 9, 2).
crosses(356, 6, 11, 2).
crosses(356, 0, 6, 2).
crosses(356, 1, 7, 1).
crosses(356, 2, 8, 3).
crosses(356, 3, 9, 2).
crosses(356, 5, 11, 1).
crosses(356, 0, 7, 1).
crosses(356, 1, 8, 2).
crosses(356, 2, 9, 4).
crosses(356, 4, 11, 1).
crosses(356, 0, 8, 2).
crosses(356, 1, 9, 3).
crosses(356, 2, 10, 1).
crosses(356, 0, 9, 3).
crosses(356, 2, 11, 1).
crosses(357, 3, 5, 2).
crosses(357, 6, 8, 2).
crosses(357, 9, 11, 1).
crosses(357, 2, 5, 2).
crosses(357, 3, 6, 2).
crosses(357, 5, 8, 2).
crosses(357, 6, 9, 2).
crosses(357, 8, 11, 1).
crosses(357, 1, 5, 2).
crosses(357, 2, 6, 2).
crosses(357, 3, 7, 1).
crosses(357, 4, 8, 1).
crosses(357, 5, 9, 2).
crosses(357, 6, 10, 1).
crosses(357, 0, 5, 1).
crosses(357, 1, 6, 2).
crosses(357, 2, 7, 1).
crosses(357, 3, 8, 2).
crosses(357, 4, 9, 1).
crosses(357, 5, 10, 1).
crosses(357, 6, 11, 1).
crosses(357, 0, 6, 1).
crosses(357, 1, 7, 1).
crosses(357, 2, 8, 2).
crosses(357, 3, 9, 2).
crosses(357, 5, 11, 1).
crosses(357, 1, 8, 2).
crosses(357, 2, 9, 2).
crosses(357, 3, 10, 1).
crosses(357, 0, 8, 1).
crosses(357, 1, 9, 2).
crosses(357, 2, 10, 1).
crosses(357, 3, 11, 1).
crosses(357, 0, 9, 1).
crosses(357, 1, 10, 1).
crosses(357, 2, 11, 1).
crosses(357, 1, 11, 1).
crosses(358, 3, 5, 2).
crosses(358, 5, 7, 1).
crosses(358, 6, 8, 1).
crosses(358, 11, 13, 2).
crosses(358, 13, 15, 1).
crosses(358, 2, 5, 2).
crosses(358, 3, 6, 1).
crosses(358, 5, 8, 2).
crosses(358, 6, 9, 1).
crosses(358, 10, 13, 2).
crosses(358, 11, 14, 1).
crosses(358, 1, 5, 2).
crosses(358, 2, 6, 1).
crosses(358, 3, 7, 1).
crosses(358, 4, 8, 1).
crosses(358, 5, 9, 2).
crosses(358, 6, 10, 1).
crosses(358, 9, 13, 2).
crosses(358, 10, 14, 1).
crosses(358, 11, 15, 1).
crosses(358, 0, 5, 1).
crosses(358, 1, 6, 1).
crosses(358, 2, 7, 1).
crosses(358, 3, 8, 2).
crosses(358, 4, 9, 1).
crosses(358, 5, 10, 2).
crosses(358, 6, 11, 1).
crosses(358, 8, 13, 2).
crosses(358, 9, 14, 1).
crosses(358, 10, 15, 1).
crosses(358, 1, 7, 1).
crosses(358, 2, 8, 2).
crosses(358, 3, 9, 2).
crosses(358, 4, 10, 1).
crosses(358, 5, 11, 2).
crosses(358, 7, 13, 1).
crosses(358, 8, 14, 1).
crosses(358, 9, 15, 1).
crosses(358, 1, 8, 2).
crosses(358, 2, 9, 2).
crosses(358, 3, 10, 2).
crosses(358, 4, 11, 1).
crosses(358, 5, 12, 1).
crosses(358, 6, 13, 1).
crosses(358, 8, 15, 1).
crosses(358, 0, 8, 1).
crosses(358, 1, 9, 2).
crosses(358, 2, 10, 2).
crosses(358, 3, 11, 2).
crosses(358, 5, 13, 2).
crosses(358, 0, 9, 1).
crosses(358, 1, 10, 2).
crosses(358, 2, 11, 2).
crosses(358, 3, 12, 1).
crosses(358, 4, 13, 1).
crosses(358, 5, 14, 1).
crosses(358, 0, 10, 1).
crosses(358, 1, 11, 2).
crosses(358, 2, 12, 1).
crosses(358, 3, 13, 2).
crosses(358, 5, 15, 1).
crosses(358, 0, 11, 1).
crosses(358, 1, 12, 1).
crosses(358, 2, 13, 2).
crosses(358, 3, 14, 1).
crosses(358, 1, 13, 2).
crosses(358, 2, 14, 1).
crosses(358, 3, 15, 1).
crosses(358, 0, 13, 1).
crosses(358, 1, 14, 1).
crosses(358, 2, 15, 1).
crosses(358, 1, 15, 1).
crosses(359, 0, 2, 1).
crosses(359, 2, 4, 2).
crosses(359, 4, 6, 1).
crosses(359, 7, 9, 2).
crosses(359, 1, 4, 1).
crosses(359, 2, 5, 2).
crosses(359, 3, 6, 1).
crosses(359, 4, 7, 1).
crosses(359, 6, 9, 2).
crosses(359, 0, 4, 1).
crosses(359, 1, 5, 1).
crosses(359, 2, 6, 3).
crosses(359, 3, 7, 1).
crosses(359, 5, 9, 1).
crosses(359, 0, 5, 1).
crosses(359, 1, 6, 2).
crosses(359, 2, 7, 3).
crosses(359, 4, 9, 1).
crosses(359, 0, 6, 2).
crosses(359, 1, 7, 2).
crosses(359, 2, 8, 1).
crosses(359, 0, 7, 2).
crosses(359, 2, 9, 1).
crosses(360, 0, 2, 1).
crosses(360, 2, 4, 2).
crosses(360, 4, 6, 1).
crosses(360, 7, 9, 2).
crosses(360, 1, 4, 1).
crosses(360, 2, 5, 2).
crosses(360, 3, 6, 1).
crosses(360, 4, 7, 1).
crosses(360, 6, 9, 2).
crosses(360, 0, 4, 1).
crosses(360, 1, 5, 1).
crosses(360, 2, 6, 3).
crosses(360, 3, 7, 1).
crosses(360, 5, 9, 1).
crosses(360, 0, 5, 1).
crosses(360, 1, 6, 2).
crosses(360, 2, 7, 3).
crosses(360, 4, 9, 1).
crosses(360, 0, 6, 2).
crosses(360, 1, 7, 2).
crosses(360, 2, 8, 1).
crosses(360, 0, 7, 2).
crosses(360, 2, 9, 1).
crosses(361, 0, 2, 1).
crosses(361, 2, 4, 2).
crosses(361, 4, 6, 1).
crosses(361, 7, 9, 2).
crosses(361, 1, 4, 1).
crosses(361, 2, 5, 2).
crosses(361, 3, 6, 1).
crosses(361, 4, 7, 1).
crosses(361, 6, 9, 2).
crosses(361, 0, 4, 1).
crosses(361, 1, 5, 1).
crosses(361, 2, 6, 3).
crosses(361, 3, 7, 1).
crosses(361, 5, 9, 1).
crosses(361, 0, 5, 1).
crosses(361, 1, 6, 2).
crosses(361, 2, 7, 3).
crosses(361, 4, 9, 1).
crosses(361, 0, 6, 2).
crosses(361, 1, 7, 2).
crosses(361, 2, 8, 1).
crosses(361, 0, 7, 2).
crosses(361, 2, 9, 1).
crosses(362, 0, 2, 1).
crosses(362, 2, 4, 2).
crosses(362, 4, 6, 1).
crosses(362, 7, 9, 2).
crosses(362, 1, 4, 1).
crosses(362, 2, 5, 2).
crosses(362, 3, 6, 1).
crosses(362, 4, 7, 1).
crosses(362, 6, 9, 2).
crosses(362, 0, 4, 1).
crosses(362, 1, 5, 1).
crosses(362, 2, 6, 3).
crosses(362, 3, 7, 1).
crosses(362, 5, 9, 1).
crosses(362, 0, 5, 1).
crosses(362, 1, 6, 2).
crosses(362, 2, 7, 3).
crosses(362, 4, 9, 1).
crosses(362, 0, 6, 2).
crosses(362, 1, 7, 2).
crosses(362, 2, 8, 1).
crosses(362, 0, 7, 2).
crosses(362, 2, 9, 1).
crosses(363, 0, 2, 1).
crosses(363, 2, 4, 2).
crosses(363, 4, 6, 1).
crosses(363, 1, 4, 1).
crosses(363, 2, 5, 1).
crosses(363, 0, 4, 1).
crosses(363, 2, 6, 1).
crosses(365, 0, 2, 1).
crosses(365, 2, 4, 2).
crosses(365, 4, 6, 1).
crosses(365, 1, 4, 1).
crosses(365, 2, 5, 1).
crosses(365, 0, 4, 1).
crosses(365, 2, 6, 1).
crosses(366, 0, 2, 1).
crosses(366, 2, 4, 2).
crosses(366, 4, 6, 1).
crosses(366, 1, 4, 1).
crosses(366, 2, 5, 1).
crosses(366, 0, 4, 1).
crosses(366, 2, 6, 1).
crosses(368, 0, 2, 1).
crosses(368, 2, 4, 2).
crosses(368, 4, 6, 2).
crosses(368, 5, 7, 1).
crosses(368, 7, 9, 2).
crosses(368, 1, 4, 1).
crosses(368, 2, 5, 1).
crosses(368, 3, 6, 1).
crosses(368, 4, 7, 3).
crosses(368, 6, 9, 1).
crosses(368, 0, 4, 1).
crosses(368, 2, 6, 2).
crosses(368, 3, 7, 2).
crosses(368, 4, 8, 1).
crosses(368, 1, 6, 1).
crosses(368, 2, 7, 3).
crosses(368, 4, 9, 1).
crosses(368, 0, 6, 1).
crosses(368, 1, 7, 2).
crosses(368, 2, 8, 1).
crosses(368, 0, 7, 2).
crosses(368, 2, 9, 1).
crosses(369, 0, 2, 1).
crosses(369, 2, 4, 2).
crosses(369, 4, 6, 2).
crosses(369, 5, 7, 1).
crosses(369, 7, 9, 2).
crosses(369, 1, 4, 1).
crosses(369, 2, 5, 1).
crosses(369, 3, 6, 1).
crosses(369, 4, 7, 3).
crosses(369, 6, 9, 1).
crosses(369, 0, 4, 1).
crosses(369, 2, 6, 2).
crosses(369, 3, 7, 2).
crosses(369, 4, 8, 1).
crosses(369, 1, 6, 1).
crosses(369, 2, 7, 3).
crosses(369, 4, 9, 1).
crosses(369, 0, 6, 1).
crosses(369, 1, 7, 2).
crosses(369, 2, 8, 1).
crosses(369, 0, 7, 2).
crosses(369, 2, 9, 1).
crosses(370, 0, 2, 1).
crosses(370, 2, 4, 2).
crosses(370, 4, 6, 2).
crosses(370, 5, 7, 1).
crosses(370, 7, 9, 2).
crosses(370, 1, 4, 1).
crosses(370, 2, 5, 1).
crosses(370, 3, 6, 1).
crosses(370, 4, 7, 3).
crosses(370, 6, 9, 1).
crosses(370, 0, 4, 1).
crosses(370, 2, 6, 2).
crosses(370, 3, 7, 2).
crosses(370, 4, 8, 1).
crosses(370, 1, 6, 1).
crosses(370, 2, 7, 3).
crosses(370, 4, 9, 1).
crosses(370, 0, 6, 1).
crosses(370, 1, 7, 2).
crosses(370, 2, 8, 1).
crosses(370, 0, 7, 2).
crosses(370, 2, 9, 1).
crosses(372, 0, 2, 1).
crosses(372, 2, 4, 2).
crosses(372, 4, 6, 2).
crosses(372, 5, 7, 1).
crosses(372, 7, 9, 2).
crosses(372, 1, 4, 1).
crosses(372, 2, 5, 1).
crosses(372, 3, 6, 1).
crosses(372, 4, 7, 3).
crosses(372, 6, 9, 1).
crosses(372, 0, 4, 1).
crosses(372, 2, 6, 2).
crosses(372, 3, 7, 2).
crosses(372, 4, 8, 1).
crosses(372, 1, 6, 1).
crosses(372, 2, 7, 3).
crosses(372, 4, 9, 1).
crosses(372, 0, 6, 1).
crosses(372, 1, 7, 2).
crosses(372, 2, 8, 1).
crosses(372, 0, 7, 2).
crosses(372, 2, 9, 1).
crosses(373, 0, 2, 1).
crosses(373, 2, 4, 2).
crosses(373, 4, 6, 2).
crosses(373, 5, 7, 1).
crosses(373, 7, 9, 2).
crosses(373, 1, 4, 1).
crosses(373, 2, 5, 1).
crosses(373, 3, 6, 1).
crosses(373, 4, 7, 3).
crosses(373, 6, 9, 1).
crosses(373, 0, 4, 1).
crosses(373, 2, 6, 2).
crosses(373, 3, 7, 2).
crosses(373, 4, 8, 1).
crosses(373, 1, 6, 1).
crosses(373, 2, 7, 3).
crosses(373, 4, 9, 1).
crosses(373, 0, 6, 1).
crosses(373, 1, 7, 2).
crosses(373, 2, 8, 1).
crosses(373, 0, 7, 2).
crosses(373, 2, 9, 1).
crosses(374, 1, 3, 1).
crosses(374, 2, 4, 1).
crosses(374, 4, 6, 2).
crosses(374, 0, 3, 1).
crosses(374, 1, 4, 2).
crosses(374, 3, 6, 1).
crosses(374, 0, 4, 2).
crosses(375, 0, 2, 1).
crosses(375, 2, 4, 2).
crosses(375, 4, 6, 2).
crosses(375, 5, 7, 1).
crosses(375, 7, 9, 2).
crosses(375, 1, 4, 1).
crosses(375, 2, 5, 1).
crosses(375, 3, 6, 1).
crosses(375, 4, 7, 3).
crosses(375, 6, 9, 1).
crosses(375, 0, 4, 1).
crosses(375, 2, 6, 2).
crosses(375, 3, 7, 2).
crosses(375, 4, 8, 1).
crosses(375, 1, 6, 1).
crosses(375, 2, 7, 3).
crosses(375, 4, 9, 1).
crosses(375, 0, 6, 1).
crosses(375, 1, 7, 2).
crosses(375, 2, 8, 1).
crosses(375, 0, 7, 2).
crosses(375, 2, 9, 1).
crosses(376, 0, 2, 1).
crosses(376, 2, 4, 2).
crosses(376, 4, 6, 1).
crosses(376, 1, 4, 1).
crosses(376, 2, 5, 1).
crosses(376, 0, 4, 1).
crosses(376, 2, 6, 1).
crosses(377, 1, 3, 2).
crosses(377, 3, 5, 2).
crosses(377, 0, 3, 2).
crosses(378, 1, 3, 1).
crosses(378, 3, 5, 1).
crosses(378, 0, 3, 1).
crosses(379, 0, 2, 1).
crosses(379, 2, 4, 2).
crosses(379, 3, 5, 1).
crosses(379, 5, 7, 2).
crosses(379, 1, 4, 1).
crosses(379, 2, 5, 3).
crosses(379, 4, 7, 1).
crosses(379, 0, 4, 1).
crosses(379, 1, 5, 2).
crosses(379, 2, 6, 1).
crosses(379, 0, 5, 2).
crosses(379, 2, 7, 1).
crosses(380, 0, 2, 1).
crosses(380, 5, 7, 1).
crosses(380, 0, 3, 1).
crosses(380, 4, 7, 1).
crosses(380, 0, 4, 1).
crosses(380, 3, 7, 1).
crosses(380, 0, 5, 1).
crosses(380, 2, 7, 1).
crosses(381, 0, 2, 1).
crosses(381, 2, 4, 2).
crosses(381, 3, 5, 1).
crosses(381, 5, 7, 2).
crosses(381, 1, 4, 1).
crosses(381, 2, 5, 3).
crosses(381, 4, 7, 1).
crosses(381, 0, 4, 1).
crosses(381, 1, 5, 2).
crosses(381, 2, 6, 1).
crosses(381, 0, 5, 2).
crosses(381, 2, 7, 1).
crosses(382, 0, 2, 1).
crosses(382, 3, 5, 2).
crosses(382, 5, 7, 1).
crosses(382, 0, 3, 1).
crosses(382, 2, 5, 2).
crosses(382, 3, 6, 1).
crosses(382, 1, 5, 1).
crosses(382, 2, 6, 1).
crosses(382, 3, 7, 1).
crosses(382, 0, 5, 1).
crosses(382, 2, 7, 1).
crosses(383, 0, 2, 1).
crosses(383, 3, 5, 1).
crosses(383, 0, 3, 1).
crosses(383, 2, 5, 1).
crosses(384, 0, 2, 1).
crosses(384, 2, 4, 2).
crosses(384, 4, 6, 1).
crosses(384, 1, 4, 1).
crosses(384, 2, 5, 1).
crosses(384, 0, 4, 1).
crosses(384, 2, 6, 1).
crosses(385, 0, 2, 1).
crosses(385, 2, 4, 2).
crosses(385, 4, 6, 1).
crosses(385, 1, 4, 1).
crosses(385, 2, 5, 1).
crosses(385, 0, 4, 1).
crosses(385, 2, 6, 1).
crosses(386, 1, 3, 2).
crosses(386, 4, 6, 1).
crosses(386, 0, 3, 1).
crosses(386, 1, 4, 2).
crosses(386, 3, 6, 1).
crosses(386, 4, 7, 1).
crosses(386, 0, 4, 1).
crosses(386, 1, 5, 1).
crosses(386, 3, 7, 1).
crosses(386, 1, 6, 1).
crosses(386, 1, 7, 1).
crosses(387, 1, 3, 2).
crosses(387, 3, 5, 2).
crosses(387, 5, 7, 1).
crosses(387, 0, 3, 1).
crosses(387, 1, 4, 1).
crosses(387, 2, 5, 1).
crosses(387, 3, 6, 1).
crosses(387, 1, 5, 2).
crosses(387, 3, 7, 1).
crosses(387, 0, 5, 1).
crosses(387, 1, 6, 1).
crosses(387, 1, 7, 1).
crosses(388, 0, 2, 1).
crosses(388, 2, 4, 2).
crosses(388, 4, 6, 2).
crosses(388, 5, 7, 1).
crosses(388, 10, 12, 2).
crosses(388, 1, 4, 1).
crosses(388, 2, 5, 1).
crosses(388, 3, 6, 1).
crosses(388, 4, 7, 3).
crosses(388, 5, 8, 1).
crosses(388, 9, 12, 2).
crosses(388, 0, 4, 1).
crosses(388, 2, 6, 2).
crosses(388, 3, 7, 2).
crosses(388, 4, 8, 3).
crosses(388, 5, 9, 1).
crosses(388, 8, 12, 2).
crosses(388, 1, 6, 1).
crosses(388, 2, 7, 3).
crosses(388, 3, 8, 2).
crosses(388, 4, 9, 3).
crosses(388, 5, 10, 1).
crosses(388, 7, 12, 2).
crosses(388, 0, 6, 1).
crosses(388, 1, 7, 2).
crosses(388, 2, 8, 3).
crosses(388, 3, 9, 2).
crosses(388, 4, 10, 3).
crosses(388, 6, 12, 1).
crosses(388, 0, 7, 2).
crosses(388, 1, 8, 2).
crosses(388, 2, 9, 3).
crosses(388, 3, 10, 2).
crosses(388, 4, 11, 1).
crosses(388, 0, 8, 2).
crosses(388, 1, 9, 2).
crosses(388, 2, 10, 3).
crosses(388, 4, 12, 1).
crosses(388, 0, 9, 2).
crosses(388, 1, 10, 2).
crosses(388, 2, 11, 1).
crosses(388, 0, 10, 2).
crosses(388, 2, 12, 1).
crosses(389, 0, 2, 1).
crosses(389, 2, 4, 2).
crosses(389, 3, 5, 1).
crosses(389, 6, 8, 2).
crosses(389, 1, 4, 1).
crosses(389, 2, 5, 3).
crosses(389, 3, 6, 1).
crosses(389, 5, 8, 2).
crosses(389, 0, 4, 1).
crosses(389, 1, 5, 2).
crosses(389, 2, 6, 3).
crosses(389, 4, 8, 1).
crosses(389, 0, 5, 2).
crosses(389, 1, 6, 2).
crosses(389, 2, 7, 1).
crosses(389, 0, 6, 2).
crosses(389, 2, 8, 1).
crosses(390, 0, 2, 1).
crosses(390, 2, 4, 2).
crosses(390, 4, 6, 2).
crosses(390, 5, 7, 1).
crosses(390, 8, 10, 3).
crosses(390, 12, 14, 1).
crosses(390, 1, 4, 1).
crosses(390, 2, 5, 1).
crosses(390, 3, 6, 1).
crosses(390, 4, 7, 3).
crosses(390, 5, 8, 1).
crosses(390, 7, 10, 3).
crosses(390, 8, 11, 3).
crosses(390, 11, 14, 1).
crosses(390, 0, 4, 1).
crosses(390, 2, 6, 2).
crosses(390, 3, 7, 2).
crosses(390, 4, 8, 3).
crosses(390, 6, 10, 2).
crosses(390, 7, 11, 3).
crosses(390, 8, 12, 3).
crosses(390, 10, 14, 1).
crosses(390, 1, 6, 1).
crosses(390, 2, 7, 3).
crosses(390, 3, 8, 2).
crosses(390, 4, 9, 1).
crosses(390, 5, 10, 1).
crosses(390, 6, 11, 2).
crosses(390, 7, 12, 3).
crosses(390, 8, 13, 2).
crosses(390, 0, 6, 1).
crosses(390, 1, 7, 2).
crosses(390, 2, 8, 3).
crosses(390, 4, 10, 2).
crosses(390, 5, 11, 1).
crosses(390, 6, 12, 2).
crosses(390, 7, 13, 2).
crosses(390, 8, 14, 2).
crosses(390, 0, 7, 2).
crosses(390, 1, 8, 2).
crosses(390, 2, 9, 1).
crosses(390, 3, 10, 1).
crosses(390, 4, 11, 2).
crosses(390, 5, 12, 1).
crosses(390, 6, 13, 1).
crosses(390, 7, 14, 2).
crosses(390, 0, 8, 2).
crosses(390, 2, 10, 2).
crosses(390, 3, 11, 1).
crosses(390, 4, 12, 2).
crosses(390, 6, 14, 1).
crosses(390, 1, 10, 1).
crosses(390, 2, 11, 2).
crosses(390, 3, 12, 1).
crosses(390, 4, 13, 1).
crosses(390, 0, 10, 1).
crosses(390, 1, 11, 1).
crosses(390, 2, 12, 2).
crosses(390, 4, 14, 1).
crosses(390, 0, 11, 1).
crosses(390, 1, 12, 1).
crosses(390, 2, 13, 1).
crosses(390, 0, 12, 1).
crosses(390, 2, 14, 1).
crosses(391, 0, 2, 1).
crosses(391, 2, 4, 2).
crosses(391, 3, 5, 1).
crosses(391, 6, 8, 2).
crosses(391, 1, 4, 1).
crosses(391, 2, 5, 3).
crosses(391, 3, 6, 1).
crosses(391, 5, 8, 2).
crosses(391, 0, 4, 1).
crosses(391, 1, 5, 2).
crosses(391, 2, 6, 3).
crosses(391, 4, 8, 1).
crosses(391, 0, 5, 2).
crosses(391, 1, 6, 2).
crosses(391, 2, 7, 1).
crosses(391, 0, 6, 2).
crosses(391, 2, 8, 1).
crosses(392, 0, 2, 1).
crosses(392, 2, 4, 2).
crosses(392, 4, 6, 2).
crosses(392, 5, 7, 1).
crosses(392, 8, 10, 1).
crosses(392, 10, 12, 3).
crosses(392, 1, 4, 1).
crosses(392, 2, 5, 1).
crosses(392, 3, 6, 1).
crosses(392, 4, 7, 3).
crosses(392, 5, 8, 1).
crosses(392, 7, 10, 1).
crosses(392, 9, 12, 2).
crosses(392, 0, 4, 1).
crosses(392, 2, 6, 2).
crosses(392, 3, 7, 2).
crosses(392, 4, 8, 3).
crosses(392, 5, 9, 1).
crosses(392, 6, 10, 1).
crosses(392, 8, 12, 2).
crosses(392, 1, 6, 1).
crosses(392, 2, 7, 3).
crosses(392, 3, 8, 2).
crosses(392, 4, 9, 3).
crosses(392, 5, 10, 2).
crosses(392, 7, 12, 2).
crosses(392, 0, 6, 1).
crosses(392, 1, 7, 2).
crosses(392, 2, 8, 3).
crosses(392, 3, 9, 2).
crosses(392, 4, 10, 4).
crosses(392, 6, 12, 1).
crosses(392, 0, 7, 2).
crosses(392, 1, 8, 2).
crosses(392, 2, 9, 3).
crosses(392, 3, 10, 3).
crosses(392, 4, 11, 1).
crosses(392, 0, 8, 2).
crosses(392, 1, 9, 2).
crosses(392, 2, 10, 4).
crosses(392, 4, 12, 1).
crosses(392, 0, 9, 2).
crosses(392, 1, 10, 3).
crosses(392, 2, 11, 1).
crosses(392, 0, 10, 3).
crosses(392, 2, 12, 1).
crosses(393, 0, 2, 1).
crosses(393, 2, 4, 2).
crosses(393, 4, 6, 2).
crosses(393, 6, 8, 1).
crosses(393, 1, 4, 1).
crosses(393, 2, 5, 1).
crosses(393, 3, 6, 1).
crosses(393, 4, 7, 1).
crosses(393, 0, 4, 1).
crosses(393, 2, 6, 2).
crosses(393, 4, 8, 1).
crosses(393, 1, 6, 1).
crosses(393, 2, 7, 1).
crosses(393, 0, 6, 1).
crosses(393, 2, 8, 1).
crosses(394, 1, 3, 1).
crosses(394, 3, 5, 2).
crosses(394, 5, 7, 1).
crosses(394, 6, 8, 1).
crosses(394, 10, 12, 1).
crosses(394, 0, 3, 1).
crosses(394, 2, 5, 1).
crosses(394, 3, 6, 1).
crosses(394, 5, 8, 2).
crosses(394, 6, 9, 1).
crosses(394, 9, 12, 1).
crosses(394, 1, 5, 1).
crosses(394, 3, 7, 1).
crosses(394, 4, 8, 1).
crosses(394, 5, 9, 2).
crosses(394, 6, 10, 1).
crosses(394, 8, 12, 1).
crosses(394, 0, 5, 1).
crosses(394, 3, 8, 2).
crosses(394, 4, 9, 1).
crosses(394, 5, 10, 2).
crosses(394, 2, 8, 1).
crosses(394, 3, 9, 2).
crosses(394, 4, 10, 1).
crosses(394, 5, 11, 1).
crosses(394, 1, 8, 1).
crosses(394, 2, 9, 1).
crosses(394, 3, 10, 2).
crosses(394, 5, 12, 1).
crosses(394, 0, 8, 1).
crosses(394, 1, 9, 1).
crosses(394, 2, 10, 1).
crosses(394, 3, 11, 1).
crosses(394, 0, 9, 1).
crosses(394, 1, 10, 1).
crosses(394, 3, 12, 1).
crosses(394, 0, 10, 1).
crosses(395, 1, 3, 1).
crosses(395, 2, 4, 1).
crosses(395, 4, 6, 2).
crosses(395, 6, 8, 1).
crosses(395, 1, 4, 2).
crosses(395, 3, 6, 1).
crosses(395, 4, 7, 1).
crosses(395, 0, 4, 1).
crosses(395, 1, 5, 1).
crosses(395, 2, 6, 1).
crosses(395, 4, 8, 1).
crosses(395, 1, 6, 2).
crosses(395, 0, 6, 1).
crosses(395, 1, 7, 1).
crosses(395, 1, 8, 1).
crosses(396, 0, 2, 1).
crosses(396, 2, 4, 2).
crosses(396, 4, 6, 2).
crosses(396, 6, 8, 1).
crosses(396, 1, 4, 1).
crosses(396, 2, 5, 1).
crosses(396, 3, 6, 1).
crosses(396, 4, 7, 1).
crosses(396, 0, 4, 1).
crosses(396, 2, 6, 2).
crosses(396, 4, 8, 1).
crosses(396, 1, 6, 1).
crosses(396, 2, 7, 1).
crosses(396, 0, 6, 1).
crosses(396, 2, 8, 1).
crosses(397, 2, 4, 1).
crosses(397, 1, 4, 1).
crosses(398, 0, 2, 1).
crosses(398, 2, 4, 2).
crosses(398, 3, 5, 1).
crosses(398, 6, 8, 2).
crosses(398, 1, 4, 1).
crosses(398, 2, 5, 3).
crosses(398, 3, 6, 1).
crosses(398, 5, 8, 2).
crosses(398, 0, 4, 1).
crosses(398, 1, 5, 2).
crosses(398, 2, 6, 3).
crosses(398, 4, 8, 1).
crosses(398, 0, 5, 2).
crosses(398, 1, 6, 2).
crosses(398, 2, 7, 1).
crosses(398, 0, 6, 2).
crosses(398, 2, 8, 1).
crosses(399, 1, 3, 1).
crosses(399, 3, 5, 1).
crosses(399, 0, 3, 1).
crosses(400, 0, 2, 1).
crosses(400, 2, 4, 2).
crosses(400, 4, 6, 1).
crosses(400, 1, 4, 1).
crosses(400, 2, 5, 1).
crosses(400, 0, 4, 1).
crosses(400, 2, 6, 1).
crosses(401, 1, 3, 1).
crosses(401, 3, 5, 1).
crosses(401, 0, 3, 1).
crosses(402, 1, 3, 1).
crosses(402, 3, 5, 1).
crosses(402, 0, 3, 1).
crosses(403, 0, 2, 1).
crosses(403, 2, 4, 2).
crosses(403, 4, 6, 1).
crosses(403, 1, 4, 1).
crosses(403, 2, 5, 1).
crosses(403, 0, 4, 1).
crosses(403, 2, 6, 1).
crosses(404, 2, 4, 2).
crosses(404, 4, 6, 1).
crosses(404, 5, 7, 1).
crosses(404, 8, 10, 1).
crosses(404, 9, 11, 2).
crosses(404, 1, 4, 2).
crosses(404, 2, 5, 2).
crosses(404, 3, 6, 1).
crosses(404, 4, 7, 2).
crosses(404, 5, 8, 1).
crosses(404, 7, 10, 1).
crosses(404, 8, 11, 3).
crosses(404, 0, 4, 1).
crosses(404, 1, 5, 2).
crosses(404, 2, 6, 3).
crosses(404, 3, 7, 2).
crosses(404, 4, 8, 2).
crosses(404, 7, 11, 3).
crosses(404, 0, 5, 1).
crosses(404, 1, 6, 3).
crosses(404, 2, 7, 4).
crosses(404, 3, 8, 2).
crosses(404, 4, 9, 1).
crosses(404, 6, 11, 2).
crosses(404, 0, 6, 2).
crosses(404, 1, 7, 4).
crosses(404, 2, 8, 4).
crosses(404, 3, 9, 1).
crosses(404, 5, 11, 1).
crosses(404, 0, 7, 3).
crosses(404, 1, 8, 4).
crosses(404, 2, 9, 3).
crosses(404, 4, 11, 1).
crosses(404, 0, 8, 3).
crosses(404, 1, 9, 3).
crosses(404, 2, 10, 1).
crosses(404, 0, 9, 2).
crosses(404, 1, 10, 1).
crosses(404, 2, 11, 1).
crosses(404, 1, 11, 1).
crosses(405, 1, 3, 1).
crosses(405, 4, 6, 1).
crosses(405, 0, 3, 1).
crosses(405, 1, 4, 1).
crosses(405, 3, 6, 1).
crosses(405, 0, 4, 1).
crosses(406, 0, 2, 1).
crosses(406, 2, 4, 2).
crosses(406, 4, 6, 1).
crosses(406, 1, 4, 1).
crosses(406, 2, 5, 1).
crosses(406, 0, 4, 1).
crosses(406, 2, 6, 1).
crosses(407, 1, 3, 1).
crosses(407, 4, 6, 1).
crosses(407, 0, 3, 1).
crosses(407, 1, 4, 1).
crosses(407, 3, 6, 1).
crosses(407, 0, 4, 1).
crosses(408, 1, 3, 1).
crosses(408, 4, 6, 1).
crosses(408, 0, 3, 1).
crosses(408, 1, 4, 1).
crosses(408, 3, 6, 1).
crosses(408, 0, 4, 1).
crosses(409, 0, 2, 1).
crosses(409, 2, 4, 2).
crosses(409, 4, 6, 1).
crosses(409, 1, 4, 1).
crosses(409, 2, 5, 1).
crosses(409, 0, 4, 1).
crosses(409, 2, 6, 1).
crosses(410, 1, 3, 1).
crosses(410, 4, 6, 1).
crosses(410, 0, 3, 1).
crosses(410, 1, 4, 1).
crosses(410, 3, 6, 1).
crosses(410, 0, 4, 1).
crosses(411, 1, 3, 1).
crosses(411, 4, 6, 1).
crosses(411, 0, 3, 1).
crosses(411, 1, 4, 1).
crosses(411, 3, 6, 1).
crosses(411, 0, 4, 1).
crosses(412, 0, 2, 1).
crosses(412, 2, 4, 2).
crosses(412, 4, 6, 1).
crosses(412, 1, 4, 1).
crosses(412, 2, 5, 1).
crosses(412, 0, 4, 1).
crosses(412, 2, 6, 1).
crosses(413, 1, 3, 1).
crosses(413, 4, 6, 1).
crosses(413, 0, 3, 1).
crosses(413, 1, 4, 1).
crosses(413, 3, 6, 1).
crosses(413, 0, 4, 1).
crosses(414, 1, 3, 1).
crosses(414, 4, 6, 1).
crosses(414, 0, 3, 1).
crosses(414, 1, 4, 1).
crosses(414, 3, 6, 1).
crosses(414, 0, 4, 1).
crosses(415, 0, 2, 1).
crosses(415, 2, 4, 2).
crosses(415, 4, 6, 1).
crosses(415, 1, 4, 1).
crosses(415, 2, 5, 1).
crosses(415, 0, 4, 1).
crosses(415, 2, 6, 1).
crosses(416, 1, 3, 1).
crosses(416, 5, 7, 1).
crosses(416, 0, 3, 1).
crosses(416, 1, 4, 1).
crosses(416, 4, 7, 1).
crosses(416, 0, 4, 1).
crosses(416, 1, 5, 1).
crosses(416, 3, 7, 1).
crosses(416, 0, 5, 1).
crosses(417, 1, 3, 1).
crosses(417, 4, 6, 1).
crosses(417, 0, 3, 1).
crosses(417, 1, 4, 1).
crosses(417, 3, 6, 1).
crosses(417, 0, 4, 1).
crosses(418, 0, 2, 1).
crosses(418, 2, 4, 2).
crosses(418, 4, 6, 1).
crosses(418, 1, 4, 1).
crosses(418, 2, 5, 1).
crosses(418, 0, 4, 1).
crosses(418, 2, 6, 1).
crosses(419, 1, 3, 1).
crosses(419, 5, 7, 1).
crosses(419, 0, 3, 1).
crosses(419, 1, 4, 1).
crosses(419, 4, 7, 1).
crosses(419, 0, 4, 1).
crosses(419, 1, 5, 1).
crosses(419, 3, 7, 1).
crosses(419, 0, 5, 1).
crosses(420, 1, 3, 1).
crosses(420, 5, 7, 1).
crosses(420, 0, 3, 1).
crosses(420, 1, 4, 1).
crosses(420, 4, 7, 1).
crosses(420, 0, 4, 1).
crosses(420, 1, 5, 1).
crosses(420, 3, 7, 1).
crosses(420, 0, 5, 1).
crosses(421, 0, 2, 1).
crosses(421, 2, 4, 2).
crosses(421, 4, 6, 1).
crosses(421, 1, 4, 1).
crosses(421, 2, 5, 1).
crosses(421, 0, 4, 1).
crosses(421, 2, 6, 1).
crosses(422, 1, 3, 1).
crosses(422, 3, 5, 1).
crosses(422, 6, 8, 2).
crosses(422, 0, 3, 1).
crosses(422, 1, 4, 1).
crosses(422, 2, 5, 1).
crosses(422, 3, 6, 1).
crosses(422, 5, 8, 2).
crosses(422, 0, 4, 1).
crosses(422, 1, 5, 2).
crosses(422, 2, 6, 1).
crosses(422, 4, 8, 1).
crosses(422, 0, 5, 2).
crosses(422, 1, 6, 2).
crosses(422, 3, 8, 1).
crosses(422, 0, 6, 2).
crosses(423, 0, 2, 1).
crosses(423, 2, 4, 2).
crosses(423, 5, 7, 1).
crosses(423, 1, 4, 1).
crosses(423, 2, 5, 2).
crosses(423, 4, 7, 1).
crosses(423, 0, 4, 1).
crosses(423, 1, 5, 1).
crosses(423, 2, 6, 1).
crosses(423, 0, 5, 1).
crosses(423, 2, 7, 1).
crosses(424, 0, 2, 1).
crosses(424, 2, 4, 1).
crosses(424, 2, 5, 1).
crosses(424, 2, 6, 1).
crosses(425, 1, 3, 1).
crosses(425, 3, 5, 1).
crosses(425, 5, 7, 2).
crosses(425, 8, 10, 2).
crosses(425, 0, 3, 1).
crosses(425, 1, 4, 1).
crosses(425, 2, 5, 1).
crosses(425, 4, 7, 1).
crosses(425, 5, 8, 2).
crosses(425, 7, 10, 2).
crosses(425, 0, 4, 1).
crosses(425, 1, 5, 2).
crosses(425, 3, 7, 1).
crosses(425, 4, 8, 1).
crosses(425, 5, 9, 1).
crosses(425, 6, 10, 1).
crosses(425, 0, 5, 2).
crosses(425, 1, 6, 1).
crosses(425, 2, 7, 1).
crosses(425, 3, 8, 1).
crosses(425, 5, 10, 2).
crosses(425, 0, 6, 1).
crosses(425, 1, 7, 2).
crosses(425, 2, 8, 1).
crosses(425, 4, 10, 1).
crosses(425, 0, 7, 2).
crosses(425, 1, 8, 2).
crosses(425, 3, 10, 1).
crosses(425, 0, 8, 2).
crosses(426, 0, 2, 1).
crosses(426, 2, 4, 2).
crosses(426, 4, 6, 1).
crosses(426, 1, 4, 1).
crosses(426, 2, 5, 1).
crosses(426, 0, 4, 1).
crosses(426, 2, 6, 1).
crosses(427, 0, 2, 1).
crosses(427, 2, 4, 1).
crosses(428, 0, 2, 1).
crosses(428, 2, 4, 2).
crosses(428, 4, 6, 1).
crosses(428, 1, 4, 1).
crosses(428, 2, 5, 1).
crosses(428, 0, 4, 1).
crosses(428, 2, 6, 1).
crosses(429, 0, 2, 1).
crosses(429, 2, 4, 2).
crosses(429, 4, 6, 1).
crosses(429, 1, 4, 1).
crosses(429, 2, 5, 1).
crosses(429, 0, 4, 1).
crosses(429, 2, 6, 1).
crosses(431, 0, 2, 1).
crosses(431, 2, 4, 2).
crosses(431, 3, 5, 1).
crosses(431, 5, 7, 2).
crosses(431, 1, 4, 1).
crosses(431, 2, 5, 3).
crosses(431, 4, 7, 1).
crosses(431, 0, 4, 1).
crosses(431, 1, 5, 2).
crosses(431, 2, 6, 1).
crosses(431, 0, 5, 2).
crosses(431, 2, 7, 1).
crosses(432, 0, 2, 1).
crosses(432, 2, 4, 2).
crosses(432, 3, 5, 1).
crosses(432, 5, 7, 2).
crosses(432, 1, 4, 1).
crosses(432, 2, 5, 3).
crosses(432, 4, 7, 1).
crosses(432, 0, 4, 1).
crosses(432, 1, 5, 2).
crosses(432, 2, 6, 1).
crosses(432, 0, 5, 2).
crosses(432, 2, 7, 1).
crosses(434, 0, 2, 1).
crosses(434, 2, 4, 2).
crosses(434, 3, 5, 1).
crosses(434, 5, 7, 2).
crosses(434, 1, 4, 1).
crosses(434, 2, 5, 3).
crosses(434, 4, 7, 1).
crosses(434, 0, 4, 1).
crosses(434, 1, 5, 2).
crosses(434, 2, 6, 1).
crosses(434, 0, 5, 2).
crosses(434, 2, 7, 1).
crosses(435, 1, 3, 1).
crosses(435, 2, 4, 1).
crosses(435, 3, 5, 1).
crosses(435, 5, 7, 2).
crosses(435, 1, 4, 2).
crosses(435, 2, 5, 2).
crosses(435, 4, 7, 1).
crosses(435, 0, 4, 1).
crosses(435, 1, 5, 3).
crosses(435, 0, 5, 2).
crosses(435, 1, 6, 1).
crosses(435, 1, 7, 1).
crosses(436, 1, 3, 1).
crosses(436, 3, 5, 1).
crosses(436, 0, 3, 1).
crosses(437, 1, 3, 1).
crosses(437, 2, 4, 1).
crosses(437, 3, 5, 1).
crosses(437, 5, 7, 2).
crosses(437, 1, 4, 2).
crosses(437, 2, 5, 2).
crosses(437, 4, 7, 1).
crosses(437, 0, 4, 1).
crosses(437, 1, 5, 3).
crosses(437, 0, 5, 2).
crosses(437, 1, 6, 1).
crosses(437, 1, 7, 1).
crosses(438, 1, 3, 1).
crosses(438, 2, 4, 1).
crosses(438, 3, 5, 1).
crosses(438, 5, 7, 2).
crosses(438, 1, 4, 2).
crosses(438, 2, 5, 2).
crosses(438, 4, 7, 1).
crosses(438, 0, 4, 1).
crosses(438, 1, 5, 3).
crosses(438, 0, 5, 2).
crosses(438, 1, 6, 1).
crosses(438, 1, 7, 1).
crosses(439, 0, 2, 1).
crosses(439, 2, 4, 1).
crosses(440, 1, 3, 1).
crosses(440, 2, 4, 1).
crosses(440, 3, 5, 1).
crosses(440, 5, 7, 2).
crosses(440, 1, 4, 2).
crosses(440, 2, 5, 2).
crosses(440, 4, 7, 1).
crosses(440, 0, 4, 1).
crosses(440, 1, 5, 3).
crosses(440, 0, 5, 2).
crosses(440, 1, 6, 1).
crosses(440, 1, 7, 1).
crosses(441, 0, 2, 1).
crosses(441, 2, 4, 2).
crosses(441, 5, 7, 1).
crosses(441, 1, 4, 1).
crosses(441, 2, 5, 2).
crosses(441, 4, 7, 1).
crosses(441, 0, 4, 1).
crosses(441, 1, 5, 1).
crosses(441, 2, 6, 1).
crosses(441, 0, 5, 1).
crosses(441, 2, 7, 1).
crosses(442, 0, 2, 1).
crosses(442, 2, 4, 1).
crosses(442, 3, 5, 1).
crosses(442, 6, 8, 1).
crosses(442, 9, 11, 1).
crosses(442, 10, 12, 1).
crosses(442, 2, 5, 2).
crosses(442, 3, 6, 1).
crosses(442, 5, 8, 1).
crosses(442, 6, 9, 1).
crosses(442, 8, 11, 1).
crosses(442, 9, 12, 2).
crosses(442, 1, 5, 1).
crosses(442, 2, 6, 2).
crosses(442, 3, 7, 1).
crosses(442, 4, 8, 1).
crosses(442, 5, 9, 1).
crosses(442, 8, 12, 2).
crosses(442, 0, 5, 1).
crosses(442, 1, 6, 1).
crosses(442, 2, 7, 2).
crosses(442, 3, 8, 2).
crosses(442, 4, 9, 1).
crosses(442, 7, 12, 1).
crosses(442, 0, 6, 1).
crosses(442, 1, 7, 1).
crosses(442, 2, 8, 3).
crosses(442, 3, 9, 2).
crosses(442, 6, 12, 1).
crosses(442, 0, 7, 1).
crosses(442, 1, 8, 2).
crosses(442, 2, 9, 3).
crosses(442, 3, 10, 1).
crosses(442, 5, 12, 1).
crosses(442, 0, 8, 2).
crosses(442, 1, 9, 2).
crosses(442, 2, 10, 2).
crosses(442, 0, 9, 2).
crosses(442, 1, 10, 1).
crosses(442, 2, 11, 1).
crosses(442, 0, 10, 1).
crosses(442, 2, 12, 1).
crosses(443, 0, 2, 1).
crosses(443, 2, 4, 2).
crosses(443, 5, 7, 1).
crosses(443, 1, 4, 1).
crosses(443, 2, 5, 2).
crosses(443, 4, 7, 1).
crosses(443, 0, 4, 1).
crosses(443, 1, 5, 1).
crosses(443, 2, 6, 1).
crosses(443, 0, 5, 1).
crosses(443, 2, 7, 1).
crosses(444, 0, 2, 1).
crosses(444, 2, 4, 1).
crosses(444, 3, 5, 1).
crosses(444, 6, 8, 1).
crosses(444, 9, 11, 1).
crosses(444, 10, 12, 1).
crosses(444, 2, 5, 2).
crosses(444, 3, 6, 1).
crosses(444, 5, 8, 1).
crosses(444, 6, 9, 1).
crosses(444, 8, 11, 1).
crosses(444, 9, 12, 2).
crosses(444, 1, 5, 1).
crosses(444, 2, 6, 2).
crosses(444, 3, 7, 1).
crosses(444, 4, 8, 1).
crosses(444, 5, 9, 1).
crosses(444, 8, 12, 2).
crosses(444, 0, 5, 1).
crosses(444, 1, 6, 1).
crosses(444, 2, 7, 2).
crosses(444, 3, 8, 2).
crosses(444, 4, 9, 1).
crosses(444, 7, 12, 1).
crosses(444, 0, 6, 1).
crosses(444, 1, 7, 1).
crosses(444, 2, 8, 3).
crosses(444, 3, 9, 2).
crosses(444, 6, 12, 1).
crosses(444, 0, 7, 1).
crosses(444, 1, 8, 2).
crosses(444, 2, 9, 3).
crosses(444, 3, 10, 1).
crosses(444, 5, 12, 1).
crosses(444, 0, 8, 2).
crosses(444, 1, 9, 2).
crosses(444, 2, 10, 2).
crosses(444, 0, 9, 2).
crosses(444, 1, 10, 1).
crosses(444, 2, 11, 1).
crosses(444, 0, 10, 1).
crosses(444, 2, 12, 1).
crosses(445, 1, 3, 1).
crosses(445, 2, 4, 1).
crosses(445, 3, 5, 1).
crosses(445, 5, 7, 1).
crosses(445, 6, 8, 1).
crosses(445, 7, 9, 1).
crosses(445, 9, 11, 4).
crosses(445, 0, 3, 1).
crosses(445, 1, 4, 2).
crosses(445, 2, 5, 2).
crosses(445, 5, 8, 2).
crosses(445, 6, 9, 2).
crosses(445, 8, 11, 3).
crosses(445, 0, 4, 2).
crosses(445, 1, 5, 3).
crosses(445, 2, 6, 1).
crosses(445, 4, 8, 1).
crosses(445, 5, 9, 3).
crosses(445, 7, 11, 2).
crosses(445, 0, 5, 3).
crosses(445, 1, 6, 2).
crosses(445, 2, 7, 1).
crosses(445, 3, 8, 1).
crosses(445, 4, 9, 2).
crosses(445, 5, 10, 1).
crosses(445, 6, 11, 2).
crosses(445, 0, 6, 2).
crosses(445, 1, 7, 2).
crosses(445, 2, 8, 2).
crosses(445, 3, 9, 2).
crosses(445, 5, 11, 3).
crosses(445, 0, 7, 2).
crosses(445, 1, 8, 3).
crosses(445, 2, 9, 3).
crosses(445, 4, 11, 2).
crosses(445, 0, 8, 3).
crosses(445, 1, 9, 4).
crosses(445, 3, 11, 1).
crosses(445, 0, 9, 4).
crosses(446, 0, 2, 1).
crosses(446, 2, 4, 1).
crosses(447, 1, 3, 1).
crosses(447, 2, 4, 1).
crosses(447, 3, 5, 1).
crosses(447, 5, 7, 1).
crosses(447, 6, 8, 1).
crosses(447, 7, 9, 1).
crosses(447, 9, 11, 4).
crosses(447, 0, 3, 1).
crosses(447, 1, 4, 2).
crosses(447, 2, 5, 2).
crosses(447, 5, 8, 2).
crosses(447, 6, 9, 2).
crosses(447, 8, 11, 3).
crosses(447, 0, 4, 2).
crosses(447, 1, 5, 3).
crosses(447, 2, 6, 1).
crosses(447, 4, 8, 1).
crosses(447, 5, 9, 3).
crosses(447, 7, 11, 2).
crosses(447, 0, 5, 3).
crosses(447, 1, 6, 2).
crosses(447, 2, 7, 1).
crosses(447, 3, 8, 1).
crosses(447, 4, 9, 2).
crosses(447, 5, 10, 1).
crosses(447, 6, 11, 2).
crosses(447, 0, 6, 2).
crosses(447, 1, 7, 2).
crosses(447, 2, 8, 2).
crosses(447, 3, 9, 2).
crosses(447, 5, 11, 3).
crosses(447, 0, 7, 2).
crosses(447, 1, 8, 3).
crosses(447, 2, 9, 3).
crosses(447, 4, 11, 2).
crosses(447, 0, 8, 3).
crosses(447, 1, 9, 4).
crosses(447, 3, 11, 1).
crosses(447, 0, 9, 4).
crosses(448, 1, 3, 1).
crosses(448, 2, 4, 1).
crosses(448, 3, 5, 1).
crosses(448, 5, 7, 1).
crosses(448, 6, 8, 1).
crosses(448, 7, 9, 1).
crosses(448, 9, 11, 4).
crosses(448, 0, 3, 1).
crosses(448, 1, 4, 2).
crosses(448, 2, 5, 2).
crosses(448, 5, 8, 2).
crosses(448, 6, 9, 2).
crosses(448, 8, 11, 3).
crosses(448, 9, 12, 4).
crosses(448, 0, 4, 2).
crosses(448, 1, 5, 3).
crosses(448, 2, 6, 1).
crosses(448, 4, 8, 1).
crosses(448, 5, 9, 3).
crosses(448, 7, 11, 2).
crosses(448, 8, 12, 3).
crosses(448, 9, 13, 4).
crosses(448, 0, 5, 3).
crosses(448, 1, 6, 2).
crosses(448, 2, 7, 1).
crosses(448, 3, 8, 1).
crosses(448, 4, 9, 2).
crosses(448, 5, 10, 1).
crosses(448, 6, 11, 2).
crosses(448, 7, 12, 2).
crosses(448, 8, 13, 3).
crosses(448, 9, 14, 4).
crosses(448, 0, 6, 2).
crosses(448, 1, 7, 2).
crosses(448, 2, 8, 2).
crosses(448, 3, 9, 2).
crosses(448, 5, 11, 3).
crosses(448, 6, 12, 2).
crosses(448, 7, 13, 2).
crosses(448, 8, 14, 3).
crosses(448, 0, 7, 2).
crosses(448, 1, 8, 3).
crosses(448, 2, 9, 3).
crosses(448, 4, 11, 2).
crosses(448, 5, 12, 3).
crosses(448, 6, 13, 2).
crosses(448, 7, 14, 2).
crosses(448, 0, 8, 3).
crosses(448, 1, 9, 4).
crosses(448, 3, 11, 1).
crosses(448, 4, 12, 2).
crosses(448, 5, 13, 3).
crosses(448, 6, 14, 2).
crosses(448, 0, 9, 4).
crosses(448, 3, 12, 1).
crosses(448, 4, 13, 2).
crosses(448, 5, 14, 3).
crosses(448, 3, 13, 1).
crosses(448, 4, 14, 2).
crosses(448, 3, 14, 1).
crosses(449, 1, 3, 1).
crosses(449, 2, 4, 1).
crosses(449, 4, 6, 1).
crosses(449, 5, 7, 1).
crosses(449, 6, 8, 1).
crosses(449, 8, 10, 3).
crosses(449, 0, 3, 1).
crosses(449, 1, 4, 2).
crosses(449, 4, 7, 2).
crosses(449, 5, 8, 2).
crosses(449, 7, 10, 2).
crosses(449, 0, 4, 2).
crosses(449, 1, 5, 1).
crosses(449, 3, 7, 1).
crosses(449, 4, 8, 3).
crosses(449, 6, 10, 1).
crosses(449, 0, 5, 1).
crosses(449, 1, 6, 1).
crosses(449, 2, 7, 1).
crosses(449, 3, 8, 2).
crosses(449, 4, 9, 1).
crosses(449, 5, 10, 1).
crosses(449, 0, 6, 1).
crosses(449, 1, 7, 2).
crosses(449, 2, 8, 2).
crosses(449, 4, 10, 2).
crosses(449, 0, 7, 2).
crosses(449, 1, 8, 3).
crosses(449, 3, 10, 1).
crosses(449, 0, 8, 3).
crosses(450, 0, 2, 1).
crosses(450, 2, 4, 2).
crosses(450, 4, 6, 1).
crosses(450, 5, 7, 2).
crosses(450, 7, 9, 1).
crosses(450, 8, 10, 1).
crosses(450, 10, 12, 4).
crosses(450, 1, 4, 1).
crosses(450, 2, 5, 2).
crosses(450, 3, 6, 1).
crosses(450, 4, 7, 3).
crosses(450, 5, 8, 1).
crosses(450, 7, 10, 2).
crosses(450, 9, 12, 3).
crosses(450, 0, 4, 1).
crosses(450, 1, 5, 1).
crosses(450, 2, 6, 3).
crosses(450, 3, 7, 3).
crosses(450, 4, 8, 2).
crosses(450, 5, 9, 1).
crosses(450, 6, 10, 1).
crosses(450, 7, 11, 1).
crosses(450, 8, 12, 3).
crosses(450, 0, 5, 1).
crosses(450, 1, 6, 2).
crosses(450, 2, 7, 5).
crosses(450, 3, 8, 2).
crosses(450, 4, 9, 2).
crosses(450, 5, 10, 2).
crosses(450, 7, 12, 4).
crosses(450, 0, 6, 2).
crosses(450, 1, 7, 4).
crosses(450, 2, 8, 4).
crosses(450, 3, 9, 2).
crosses(450, 4, 10, 3).
crosses(450, 6, 12, 2).
crosses(450, 0, 7, 4).
crosses(450, 1, 8, 3).
crosses(450, 2, 9, 4).
crosses(450, 3, 10, 3).
crosses(450, 5, 12, 1).
crosses(450, 0, 8, 3).
crosses(450, 1, 9, 3).
crosses(450, 2, 10, 5).
crosses(450, 4, 12, 1).
crosses(450, 0, 9, 3).
crosses(450, 1, 10, 4).
crosses(450, 2, 11, 1).
crosses(450, 0, 10, 4).
crosses(450, 2, 12, 1).
crosses(451, 0, 2, 1).
crosses(451, 2, 4, 2).
crosses(451, 4, 6, 1).
crosses(451, 1, 4, 1).
crosses(451, 2, 5, 1).
crosses(451, 0, 4, 1).
crosses(451, 2, 6, 1).
crosses(452, 0, 2, 1).
crosses(452, 2, 4, 2).
crosses(452, 4, 6, 1).
crosses(452, 1, 4, 1).
crosses(452, 2, 5, 1).
crosses(452, 0, 4, 1).
crosses(452, 2, 6, 1).
crosses(453, 0, 2, 1).
crosses(453, 2, 4, 2).
crosses(453, 4, 6, 1).
crosses(453, 5, 7, 2).
crosses(453, 7, 9, 1).
crosses(453, 8, 10, 1).
crosses(453, 10, 12, 4).
crosses(453, 1, 4, 1).
crosses(453, 2, 5, 2).
crosses(453, 3, 6, 1).
crosses(453, 4, 7, 3).
crosses(453, 5, 8, 1).
crosses(453, 7, 10, 2).
crosses(453, 9, 12, 3).
crosses(453, 0, 4, 1).
crosses(453, 1, 5, 1).
crosses(453, 2, 6, 3).
crosses(453, 3, 7, 3).
crosses(453, 4, 8, 2).
crosses(453, 5, 9, 1).
crosses(453, 6, 10, 1).
crosses(453, 7, 11, 1).
crosses(453, 8, 12, 3).
crosses(453, 0, 5, 1).
crosses(453, 1, 6, 2).
crosses(453, 2, 7, 5).
crosses(453, 3, 8, 2).
crosses(453, 4, 9, 2).
crosses(453, 5, 10, 2).
crosses(453, 7, 12, 4).
crosses(453, 0, 6, 2).
crosses(453, 1, 7, 4).
crosses(453, 2, 8, 4).
crosses(453, 3, 9, 2).
crosses(453, 4, 10, 3).
crosses(453, 6, 12, 2).
crosses(453, 0, 7, 4).
crosses(453, 1, 8, 3).
crosses(453, 2, 9, 4).
crosses(453, 3, 10, 3).
crosses(453, 5, 12, 1).
crosses(453, 0, 8, 3).
crosses(453, 1, 9, 3).
crosses(453, 2, 10, 5).
crosses(453, 4, 12, 1).
crosses(453, 0, 9, 3).
crosses(453, 1, 10, 4).
crosses(453, 2, 11, 1).
crosses(453, 0, 10, 4).
crosses(453, 2, 12, 1).
crosses(454, 0, 2, 1).
crosses(454, 2, 4, 2).
crosses(454, 4, 6, 1).
crosses(454, 1, 4, 1).
crosses(454, 2, 5, 1).
crosses(454, 0, 4, 1).
crosses(454, 2, 6, 1).
crosses(455, 0, 2, 1).
crosses(455, 2, 4, 2).
crosses(455, 4, 6, 1).
crosses(455, 1, 4, 1).
crosses(455, 2, 5, 1).
crosses(455, 0, 4, 1).
crosses(455, 2, 6, 1).
crosses(456, 0, 2, 1).
crosses(456, 2, 4, 2).
crosses(456, 4, 6, 1).
crosses(456, 6, 8, 2).
crosses(456, 11, 13, 2).
crosses(456, 1, 4, 1).
crosses(456, 2, 5, 2).
crosses(456, 3, 6, 1).
crosses(456, 5, 8, 1).
crosses(456, 6, 9, 2).
crosses(456, 10, 13, 2).
crosses(456, 0, 4, 1).
crosses(456, 1, 5, 1).
crosses(456, 2, 6, 3).
crosses(456, 4, 8, 1).
crosses(456, 5, 9, 1).
crosses(456, 6, 10, 2).
crosses(456, 9, 13, 2).
crosses(456, 0, 5, 1).
crosses(456, 1, 6, 2).
crosses(456, 2, 7, 2).
crosses(456, 3, 8, 1).
crosses(456, 4, 9, 1).
crosses(456, 5, 10, 1).
crosses(456, 6, 11, 2).
crosses(456, 8, 13, 2).
crosses(456, 0, 6, 2).
crosses(456, 1, 7, 1).
crosses(456, 2, 8, 3).
crosses(456, 3, 9, 1).
crosses(456, 4, 10, 1).
crosses(456, 5, 11, 1).
crosses(456, 6, 12, 1).
crosses(456, 7, 13, 1).
crosses(456, 0, 7, 1).
crosses(456, 1, 8, 2).
crosses(456, 2, 9, 3).
crosses(456, 3, 10, 1).
crosses(456, 4, 11, 1).
crosses(456, 6, 13, 2).
crosses(456, 0, 8, 2).
crosses(456, 1, 9, 2).
crosses(456, 2, 10, 3).
crosses(456, 3, 11, 1).
crosses(456, 5, 13, 1).
crosses(456, 0, 9, 2).
crosses(456, 1, 10, 2).
crosses(456, 2, 11, 3).
crosses(456, 4, 13, 1).
crosses(456, 0, 10, 2).
crosses(456, 1, 11, 2).
crosses(456, 2, 12, 1).
crosses(456, 0, 11, 2).
crosses(456, 2, 13, 1).
crosses(457, 0, 2, 1).
crosses(457, 2, 4, 2).
crosses(457, 4, 6, 1).
crosses(457, 6, 8, 2).
crosses(457, 8, 10, 2).
crosses(457, 1, 4, 1).
crosses(457, 2, 5, 2).
crosses(457, 3, 6, 1).
crosses(457, 5, 8, 1).
crosses(457, 6, 9, 1).
crosses(457, 7, 10, 1).
crosses(457, 0, 4, 1).
crosses(457, 1, 5, 1).
crosses(457, 2, 6, 3).
crosses(457, 4, 8, 1).
crosses(457, 6, 10, 2).
crosses(457, 0, 5, 1).
crosses(457, 1, 6, 2).
crosses(457, 2, 7, 2).
crosses(457, 3, 8, 1).
crosses(457, 5, 10, 1).
crosses(457, 0, 6, 2).
crosses(457, 1, 7, 1).
crosses(457, 2, 8, 3).
crosses(457, 4, 10, 1).
crosses(457, 0, 7, 1).
crosses(457, 1, 8, 2).
crosses(457, 2, 9, 1).
crosses(457, 0, 8, 2).
crosses(457, 2, 10, 1).
crosses(458, 0, 2, 1).
crosses(458, 2, 4, 2).
crosses(458, 4, 6, 1).
crosses(458, 6, 8, 2).
crosses(458, 11, 13, 2).
crosses(458, 1, 4, 1).
crosses(458, 2, 5, 2).
crosses(458, 3, 6, 1).
crosses(458, 5, 8, 1).
crosses(458, 6, 9, 2).
crosses(458, 10, 13, 2).
crosses(458, 0, 4, 1).
crosses(458, 1, 5, 1).
crosses(458, 2, 6, 3).
crosses(458, 4, 8, 1).
crosses(458, 5, 9, 1).
crosses(458, 6, 10, 2).
crosses(458, 9, 13, 2).
crosses(458, 0, 5, 1).
crosses(458, 1, 6, 2).
crosses(458, 2, 7, 2).
crosses(458, 3, 8, 1).
crosses(458, 4, 9, 1).
crosses(458, 5, 10, 1).
crosses(458, 6, 11, 2).
crosses(458, 8, 13, 2).
crosses(458, 0, 6, 2).
crosses(458, 1, 7, 1).
crosses(458, 2, 8, 3).
crosses(458, 3, 9, 1).
crosses(458, 4, 10, 1).
crosses(458, 5, 11, 1).
crosses(458, 6, 12, 1).
crosses(458, 7, 13, 1).
crosses(458, 0, 7, 1).
crosses(458, 1, 8, 2).
crosses(458, 2, 9, 3).
crosses(458, 3, 10, 1).
crosses(458, 4, 11, 1).
crosses(458, 6, 13, 2).
crosses(458, 0, 8, 2).
crosses(458, 1, 9, 2).
crosses(458, 2, 10, 3).
crosses(458, 3, 11, 1).
crosses(458, 5, 13, 1).
crosses(458, 0, 9, 2).
crosses(458, 1, 10, 2).
crosses(458, 2, 11, 3).
crosses(458, 4, 13, 1).
crosses(458, 0, 10, 2).
crosses(458, 1, 11, 2).
crosses(458, 2, 12, 1).
crosses(458, 0, 11, 2).
crosses(458, 2, 13, 1).
crosses(459, 0, 2, 1).
crosses(459, 2, 4, 2).
crosses(459, 4, 6, 1).
crosses(459, 6, 8, 2).
crosses(459, 8, 10, 2).
crosses(459, 1, 4, 1).
crosses(459, 2, 5, 2).
crosses(459, 3, 6, 1).
crosses(459, 5, 8, 1).
crosses(459, 6, 9, 1).
crosses(459, 7, 10, 1).
crosses(459, 0, 4, 1).
crosses(459, 1, 5, 1).
crosses(459, 2, 6, 3).
crosses(459, 4, 8, 1).
crosses(459, 6, 10, 2).
crosses(459, 0, 5, 1).
crosses(459, 1, 6, 2).
crosses(459, 2, 7, 2).
crosses(459, 3, 8, 1).
crosses(459, 5, 10, 1).
crosses(459, 0, 6, 2).
crosses(459, 1, 7, 1).
crosses(459, 2, 8, 3).
crosses(459, 4, 10, 1).
crosses(459, 0, 7, 1).
crosses(459, 1, 8, 2).
crosses(459, 2, 9, 1).
crosses(459, 0, 8, 2).
crosses(459, 2, 10, 1).
crosses(460, 0, 2, 1).
crosses(460, 2, 4, 2).
crosses(460, 4, 6, 1).
crosses(460, 6, 8, 2).
crosses(460, 8, 10, 1).
crosses(460, 9, 11, 1).
crosses(460, 12, 14, 2).
crosses(460, 1, 4, 1).
crosses(460, 2, 5, 2).
crosses(460, 3, 6, 1).
crosses(460, 5, 8, 1).
crosses(460, 6, 9, 1).
crosses(460, 8, 11, 2).
crosses(460, 9, 12, 1).
crosses(460, 11, 14, 2).
crosses(460, 0, 4, 1).
crosses(460, 1, 5, 1).
crosses(460, 2, 6, 3).
crosses(460, 4, 8, 1).
crosses(460, 6, 10, 1).
crosses(460, 7, 11, 1).
crosses(460, 8, 12, 2).
crosses(460, 10, 14, 1).
crosses(460, 0, 5, 1).
crosses(460, 1, 6, 2).
crosses(460, 2, 7, 2).
crosses(460, 3, 8, 1).
crosses(460, 6, 11, 2).
crosses(460, 7, 12, 1).
crosses(460, 8, 13, 1).
crosses(460, 9, 14, 1).
crosses(460, 0, 6, 2).
crosses(460, 1, 7, 1).
crosses(460, 2, 8, 3).
crosses(460, 5, 11, 1).
crosses(460, 6, 12, 2).
crosses(460, 8, 14, 2).
crosses(460, 0, 7, 1).
crosses(460, 1, 8, 2).
crosses(460, 2, 9, 2).
crosses(460, 4, 11, 1).
crosses(460, 5, 12, 1).
crosses(460, 6, 13, 1).
crosses(460, 7, 14, 1).
crosses(460, 0, 8, 2).
crosses(460, 1, 9, 1).
crosses(460, 2, 10, 2).
crosses(460, 3, 11, 1).
crosses(460, 4, 12, 1).
crosses(460, 6, 14, 2).
crosses(460, 0, 9, 1).
crosses(460, 1, 10, 1).
crosses(460, 2, 11, 3).
crosses(460, 3, 12, 1).
crosses(460, 5, 14, 1).
crosses(460, 0, 10, 1).
crosses(460, 1, 11, 2).
crosses(460, 2, 12, 3).
crosses(460, 4, 14, 1).
crosses(460, 0, 11, 2).
crosses(460, 1, 12, 2).
crosses(460, 2, 13, 1).
crosses(460, 0, 12, 2).
crosses(460, 2, 14, 1).
crosses(461, 0, 2, 1).
crosses(461, 2, 4, 2).
crosses(461, 4, 6, 1).
crosses(461, 6, 8, 2).
crosses(461, 8, 10, 2).
crosses(461, 1, 4, 1).
crosses(461, 2, 5, 2).
crosses(461, 3, 6, 1).
crosses(461, 5, 8, 1).
crosses(461, 6, 9, 1).
crosses(461, 7, 10, 1).
crosses(461, 0, 4, 1).
crosses(461, 1, 5, 1).
crosses(461, 2, 6, 3).
crosses(461, 4, 8, 1).
crosses(461, 6, 10, 2).
crosses(461, 0, 5, 1).
crosses(461, 1, 6, 2).
crosses(461, 2, 7, 2).
crosses(461, 3, 8, 1).
crosses(461, 5, 10, 1).
crosses(461, 0, 6, 2).
crosses(461, 1, 7, 1).
crosses(461, 2, 8, 3).
crosses(461, 4, 10, 1).
crosses(461, 0, 7, 1).
crosses(461, 1, 8, 2).
crosses(461, 2, 9, 1).
crosses(461, 0, 8, 2).
crosses(461, 2, 10, 1).
crosses(462, 0, 2, 1).
crosses(462, 2, 4, 2).
crosses(462, 4, 6, 1).
crosses(462, 6, 8, 1).
crosses(462, 8, 10, 2).
crosses(462, 10, 12, 1).
crosses(462, 1, 4, 1).
crosses(462, 2, 5, 1).
crosses(462, 4, 7, 1).
crosses(462, 5, 8, 1).
crosses(462, 7, 10, 1).
crosses(462, 8, 11, 1).
crosses(462, 10, 13, 1).
crosses(462, 0, 4, 1).
crosses(462, 2, 6, 1).
crosses(462, 4, 8, 2).
crosses(462, 6, 10, 1).
crosses(462, 8, 12, 1).
crosses(462, 2, 7, 1).
crosses(462, 3, 8, 1).
crosses(462, 4, 9, 1).
crosses(462, 5, 10, 1).
crosses(462, 8, 13, 1).
crosses(462, 2, 8, 2).
crosses(462, 4, 10, 2).
crosses(462, 1, 8, 1).
crosses(462, 2, 9, 1).
crosses(462, 3, 10, 1).
crosses(462, 4, 11, 1).
crosses(462, 0, 8, 1).
crosses(462, 2, 10, 2).
crosses(462, 4, 12, 1).
crosses(462, 1, 10, 1).
crosses(462, 2, 11, 1).
crosses(462, 4, 13, 1).
crosses(462, 0, 10, 1).
crosses(462, 2, 12, 1).
crosses(462, 2, 13, 1).
crosses(463, 0, 2, 1).
crosses(463, 2, 4, 2).
crosses(463, 4, 6, 1).
crosses(463, 6, 8, 2).
crosses(463, 8, 10, 2).
crosses(463, 1, 4, 1).
crosses(463, 2, 5, 2).
crosses(463, 3, 6, 1).
crosses(463, 5, 8, 1).
crosses(463, 6, 9, 1).
crosses(463, 7, 10, 1).
crosses(463, 0, 4, 1).
crosses(463, 1, 5, 1).
crosses(463, 2, 6, 3).
crosses(463, 4, 8, 1).
crosses(463, 6, 10, 2).
crosses(463, 0, 5, 1).
crosses(463, 1, 6, 2).
crosses(463, 2, 7, 2).
crosses(463, 3, 8, 1).
crosses(463, 5, 10, 1).
crosses(463, 0, 6, 2).
crosses(463, 1, 7, 1).
crosses(463, 2, 8, 3).
crosses(463, 4, 10, 1).
crosses(463, 0, 7, 1).
crosses(463, 1, 8, 2).
crosses(463, 2, 9, 1).
crosses(463, 0, 8, 2).
crosses(463, 2, 10, 1).
crosses(464, 1, 3, 1).
crosses(464, 3, 5, 2).
crosses(464, 5, 7, 1).
crosses(464, 7, 9, 3).
crosses(464, 0, 3, 1).
crosses(464, 2, 5, 1).
crosses(464, 3, 6, 2).
crosses(464, 4, 7, 1).
crosses(464, 6, 9, 2).
crosses(464, 7, 10, 3).
crosses(464, 1, 5, 1).
crosses(464, 2, 6, 1).
crosses(464, 3, 7, 3).
crosses(464, 5, 9, 2).
crosses(464, 6, 10, 2).
crosses(464, 0, 5, 1).
crosses(464, 1, 6, 1).
crosses(464, 2, 7, 2).
crosses(464, 3, 8, 1).
crosses(464, 4, 9, 1).
crosses(464, 5, 10, 2).
crosses(464, 0, 6, 1).
crosses(464, 1, 7, 2).
crosses(464, 3, 9, 2).
crosses(464, 4, 10, 1).
crosses(464, 0, 7, 2).
crosses(464, 2, 9, 1).
crosses(464, 3, 10, 2).
crosses(464, 1, 9, 1).
crosses(464, 2, 10, 1).
crosses(464, 1, 10, 1).
crosses(465, 0, 2, 1).
crosses(465, 2, 4, 2).
crosses(465, 4, 6, 1).
crosses(465, 1, 4, 1).
crosses(465, 2, 5, 1).
crosses(465, 0, 4, 1).
crosses(465, 2, 6, 1).
crosses(466, 0, 2, 1).
crosses(466, 2, 4, 2).
crosses(466, 4, 6, 1).
crosses(466, 1, 4, 1).
crosses(466, 2, 5, 1).
crosses(466, 0, 4, 1).
crosses(466, 2, 6, 1).
crosses(467, 1, 3, 1).
crosses(467, 2, 4, 1).
crosses(467, 4, 6, 2).
crosses(467, 5, 7, 2).
crosses(467, 9, 11, 2).
crosses(467, 0, 3, 1).
crosses(467, 1, 4, 2).
crosses(467, 3, 6, 1).
crosses(467, 4, 7, 4).
crosses(467, 5, 8, 2).
crosses(467, 8, 11, 2).
crosses(467, 0, 4, 2).
crosses(467, 3, 7, 3).
crosses(467, 4, 8, 4).
crosses(467, 5, 9, 2).
crosses(467, 7, 11, 2).
crosses(467, 2, 7, 2).
crosses(467, 3, 8, 3).
crosses(467, 4, 9, 4).
crosses(467, 1, 7, 2).
crosses(467, 2, 8, 2).
crosses(467, 3, 9, 3).
crosses(467, 4, 10, 2).
crosses(467, 0, 7, 2).
crosses(467, 1, 8, 2).
crosses(467, 2, 9, 2).
crosses(467, 3, 10, 1).
crosses(467, 4, 11, 2).
crosses(467, 0, 8, 2).
crosses(467, 1, 9, 2).
crosses(467, 3, 11, 1).
crosses(467, 0, 9, 2).
crosses(468, 0, 2, 1).
crosses(468, 2, 4, 2).
crosses(468, 4, 6, 1).
crosses(468, 1, 4, 1).
crosses(468, 2, 5, 1).
crosses(468, 0, 4, 1).
crosses(468, 2, 6, 1).
crosses(469, 1, 3, 1).
crosses(469, 2, 4, 1).
crosses(469, 7, 9, 2).
crosses(469, 0, 3, 1).
crosses(469, 1, 4, 2).
crosses(469, 2, 5, 1).
crosses(469, 6, 9, 2).
crosses(469, 0, 4, 2).
crosses(469, 1, 5, 2).
crosses(469, 2, 6, 1).
crosses(469, 5, 9, 2).
crosses(469, 0, 5, 2).
crosses(469, 1, 6, 2).
crosses(469, 2, 7, 1).
crosses(469, 4, 9, 2).
crosses(469, 0, 6, 2).
crosses(469, 1, 7, 2).
crosses(469, 3, 9, 1).
crosses(469, 0, 7, 2).
crosses(470, 0, 2, 1).
crosses(470, 2, 4, 2).
crosses(470, 4, 6, 1).
crosses(470, 1, 4, 1).
crosses(470, 2, 5, 1).
crosses(470, 0, 4, 1).
crosses(470, 2, 6, 1).
crosses(471, 1, 3, 1).
crosses(471, 4, 6, 1).
crosses(471, 6, 8, 2).
crosses(471, 0, 3, 1).
crosses(471, 1, 4, 1).
crosses(471, 3, 6, 1).
crosses(471, 5, 8, 1).
crosses(471, 0, 4, 1).
crosses(471, 1, 5, 1).
crosses(471, 2, 6, 1).
crosses(471, 4, 8, 1).
crosses(471, 0, 5, 1).
crosses(471, 1, 6, 2).
crosses(471, 3, 8, 1).
crosses(471, 0, 6, 2).
crosses(472, 1, 3, 1).
crosses(472, 3, 5, 1).
crosses(472, 5, 7, 2).
crosses(472, 0, 3, 1).
crosses(472, 1, 4, 1).
crosses(472, 2, 5, 1).
crosses(472, 4, 7, 1).
crosses(472, 0, 4, 1).
crosses(472, 1, 5, 2).
crosses(472, 3, 7, 1).
crosses(472, 0, 5, 2).
crosses(473, 1, 3, 1).
crosses(473, 4, 6, 1).
crosses(473, 6, 8, 2).
crosses(473, 0, 3, 1).
crosses(473, 1, 4, 1).
crosses(473, 3, 6, 1).
crosses(473, 5, 8, 1).
crosses(473, 0, 4, 1).
crosses(473, 1, 5, 1).
crosses(473, 2, 6, 1).
crosses(473, 4, 8, 1).
crosses(473, 0, 5, 1).
crosses(473, 1, 6, 2).
crosses(473, 3, 8, 1).
crosses(473, 0, 6, 2).
crosses(474, 1, 3, 1).
crosses(474, 3, 5, 1).
crosses(474, 5, 7, 2).
crosses(474, 0, 3, 1).
crosses(474, 1, 4, 1).
crosses(474, 2, 5, 1).
crosses(474, 4, 7, 1).
crosses(474, 0, 4, 1).
crosses(474, 1, 5, 2).
crosses(474, 3, 7, 1).
crosses(474, 0, 5, 2).
crosses(475, 1, 3, 1).
crosses(475, 4, 6, 1).
crosses(475, 6, 8, 2).
crosses(475, 0, 3, 1).
crosses(475, 1, 4, 1).
crosses(475, 3, 6, 1).
crosses(475, 5, 8, 1).
crosses(475, 0, 4, 1).
crosses(475, 1, 5, 1).
crosses(475, 2, 6, 1).
crosses(475, 4, 8, 1).
crosses(475, 0, 5, 1).
crosses(475, 1, 6, 2).
crosses(475, 3, 8, 1).
crosses(475, 0, 6, 2).
crosses(476, 1, 3, 1).
crosses(476, 3, 5, 1).
crosses(476, 5, 7, 2).
crosses(476, 0, 3, 1).
crosses(476, 1, 4, 1).
crosses(476, 2, 5, 1).
crosses(476, 4, 7, 1).
crosses(476, 0, 4, 1).
crosses(476, 1, 5, 2).
crosses(476, 3, 7, 1).
crosses(476, 0, 5, 2).
crosses(477, 0, 2, 1).
crosses(477, 2, 4, 2).
crosses(477, 3, 5, 1).
crosses(477, 6, 8, 2).
crosses(477, 9, 11, 1).
crosses(477, 11, 13, 3).
crosses(477, 1, 4, 1).
crosses(477, 2, 5, 3).
crosses(477, 3, 6, 1).
crosses(477, 5, 8, 2).
crosses(477, 6, 9, 2).
crosses(477, 8, 11, 1).
crosses(477, 10, 13, 2).
crosses(477, 0, 4, 1).
crosses(477, 1, 5, 2).
crosses(477, 2, 6, 3).
crosses(477, 4, 8, 1).
crosses(477, 5, 9, 2).
crosses(477, 6, 10, 2).
crosses(477, 7, 11, 1).
crosses(477, 9, 13, 2).
crosses(477, 0, 5, 2).
crosses(477, 1, 6, 2).
crosses(477, 2, 7, 2).
crosses(477, 3, 8, 1).
crosses(477, 4, 9, 1).
crosses(477, 5, 10, 2).
crosses(477, 6, 11, 3).
crosses(477, 8, 13, 2).
crosses(477, 0, 6, 2).
crosses(477, 1, 7, 1).
crosses(477, 2, 8, 3).
crosses(477, 3, 9, 1).
crosses(477, 4, 10, 1).
crosses(477, 5, 11, 3).
crosses(477, 6, 12, 1).
crosses(477, 7, 13, 1).
crosses(477, 0, 7, 1).
crosses(477, 1, 8, 2).
crosses(477, 2, 9, 3).
crosses(477, 3, 10, 1).
crosses(477, 4, 11, 2).
crosses(477, 5, 12, 1).
crosses(477, 6, 13, 2).
crosses(477, 0, 8, 2).
crosses(477, 1, 9, 2).
crosses(477, 2, 10, 3).
crosses(477, 3, 11, 2).
crosses(477, 5, 13, 2).
crosses(477, 0, 9, 2).
crosses(477, 1, 10, 2).
crosses(477, 2, 11, 4).
crosses(477, 4, 13, 1).
crosses(477, 0, 10, 2).
crosses(477, 1, 11, 3).
crosses(477, 2, 12, 1).
crosses(477, 0, 11, 3).
crosses(477, 2, 13, 1).
crosses(478, 0, 2, 1).
crosses(478, 2, 4, 2).
crosses(478, 3, 5, 1).
crosses(478, 6, 8, 2).
crosses(478, 9, 11, 2).
crosses(478, 1, 4, 1).
crosses(478, 2, 5, 3).
crosses(478, 3, 6, 1).
crosses(478, 5, 8, 2).
crosses(478, 6, 9, 2).
crosses(478, 8, 11, 2).
crosses(478, 0, 4, 1).
crosses(478, 1, 5, 2).
crosses(478, 2, 6, 3).
crosses(478, 4, 8, 1).
crosses(478, 5, 9, 2).
crosses(478, 6, 10, 1).
crosses(478, 7, 11, 1).
crosses(478, 0, 5, 2).
crosses(478, 1, 6, 2).
crosses(478, 2, 7, 2).
crosses(478, 3, 8, 1).
crosses(478, 4, 9, 1).
crosses(478, 5, 10, 1).
crosses(478, 6, 11, 2).
crosses(478, 0, 6, 2).
crosses(478, 1, 7, 1).
crosses(478, 2, 8, 3).
crosses(478, 3, 9, 1).
crosses(478, 5, 11, 2).
crosses(478, 0, 7, 1).
crosses(478, 1, 8, 2).
crosses(478, 2, 9, 3).
crosses(478, 4, 11, 1).
crosses(478, 0, 8, 2).
crosses(478, 1, 9, 2).
crosses(478, 2, 10, 1).
crosses(478, 0, 9, 2).
crosses(478, 2, 11, 1).
crosses(479, 0, 2, 1).
crosses(479, 2, 4, 2).
crosses(479, 3, 5, 1).
crosses(479, 6, 8, 2).
crosses(479, 9, 11, 1).
crosses(479, 11, 13, 3).
crosses(479, 1, 4, 1).
crosses(479, 2, 5, 3).
crosses(479, 3, 6, 1).
crosses(479, 5, 8, 2).
crosses(479, 6, 9, 2).
crosses(479, 8, 11, 1).
crosses(479, 10, 13, 2).
crosses(479, 0, 4, 1).
crosses(479, 1, 5, 2).
crosses(479, 2, 6, 3).
crosses(479, 4, 8, 1).
crosses(479, 5, 9, 2).
crosses(479, 6, 10, 2).
crosses(479, 7, 11, 1).
crosses(479, 9, 13, 2).
crosses(479, 0, 5, 2).
crosses(479, 1, 6, 2).
crosses(479, 2, 7, 2).
crosses(479, 3, 8, 1).
crosses(479, 4, 9, 1).
crosses(479, 5, 10, 2).
crosses(479, 6, 11, 3).
crosses(479, 8, 13, 2).
crosses(479, 0, 6, 2).
crosses(479, 1, 7, 1).
crosses(479, 2, 8, 3).
crosses(479, 3, 9, 1).
crosses(479, 4, 10, 1).
crosses(479, 5, 11, 3).
crosses(479, 6, 12, 1).
crosses(479, 7, 13, 1).
crosses(479, 0, 7, 1).
crosses(479, 1, 8, 2).
crosses(479, 2, 9, 3).
crosses(479, 3, 10, 1).
crosses(479, 4, 11, 2).
crosses(479, 5, 12, 1).
crosses(479, 6, 13, 2).
crosses(479, 0, 8, 2).
crosses(479, 1, 9, 2).
crosses(479, 2, 10, 3).
crosses(479, 3, 11, 2).
crosses(479, 5, 13, 2).
crosses(479, 0, 9, 2).
crosses(479, 1, 10, 2).
crosses(479, 2, 11, 4).
crosses(479, 4, 13, 1).
crosses(479, 0, 10, 2).
crosses(479, 1, 11, 3).
crosses(479, 2, 12, 1).
crosses(479, 0, 11, 3).
crosses(479, 2, 13, 1).
crosses(480, 0, 2, 1).
crosses(480, 2, 4, 2).
crosses(480, 3, 5, 1).
crosses(480, 6, 8, 2).
crosses(480, 8, 10, 1).
crosses(480, 10, 12, 3).
crosses(480, 1, 4, 1).
crosses(480, 2, 5, 3).
crosses(480, 3, 6, 1).
crosses(480, 5, 8, 2).
crosses(480, 6, 9, 2).
crosses(480, 7, 10, 1).
crosses(480, 9, 12, 2).
crosses(480, 0, 4, 1).
crosses(480, 1, 5, 2).
crosses(480, 2, 6, 3).
crosses(480, 4, 8, 1).
crosses(480, 5, 9, 2).
crosses(480, 6, 10, 3).
crosses(480, 8, 12, 2).
crosses(480, 0, 5, 2).
crosses(480, 1, 6, 2).
crosses(480, 2, 7, 2).
crosses(480, 3, 8, 1).
crosses(480, 4, 9, 1).
crosses(480, 5, 10, 3).
crosses(480, 6, 11, 1).
crosses(480, 7, 12, 1).
crosses(480, 0, 6, 2).
crosses(480, 1, 7, 1).
crosses(480, 2, 8, 3).
crosses(480, 3, 9, 1).
crosses(480, 4, 10, 2).
crosses(480, 5, 11, 1).
crosses(480, 6, 12, 2).
crosses(480, 0, 7, 1).
crosses(480, 1, 8, 2).
crosses(480, 2, 9, 3).
crosses(480, 3, 10, 2).
crosses(480, 5, 12, 2).
crosses(480, 0, 8, 2).
crosses(480, 1, 9, 2).
crosses(480, 2, 10, 4).
crosses(480, 4, 12, 1).
crosses(480, 0, 9, 2).
crosses(480, 1, 10, 3).
crosses(480, 2, 11, 1).
crosses(480, 0, 10, 3).
crosses(480, 2, 12, 1).
crosses(481, 0, 2, 1).
crosses(481, 2, 4, 2).
crosses(481, 3, 5, 1).
crosses(481, 6, 8, 2).
crosses(481, 9, 11, 1).
crosses(481, 11, 13, 3).
crosses(481, 1, 4, 1).
crosses(481, 2, 5, 3).
crosses(481, 3, 6, 1).
crosses(481, 5, 8, 2).
crosses(481, 6, 9, 2).
crosses(481, 8, 11, 1).
crosses(481, 10, 13, 2).
crosses(481, 0, 4, 1).
crosses(481, 1, 5, 2).
crosses(481, 2, 6, 3).
crosses(481, 4, 8, 1).
crosses(481, 5, 9, 2).
crosses(481, 6, 10, 2).
crosses(481, 7, 11, 1).
crosses(481, 9, 13, 2).
crosses(481, 0, 5, 2).
crosses(481, 1, 6, 2).
crosses(481, 2, 7, 2).
crosses(481, 3, 8, 1).
crosses(481, 4, 9, 1).
crosses(481, 5, 10, 2).
crosses(481, 6, 11, 3).
crosses(481, 8, 13, 2).
crosses(481, 0, 6, 2).
crosses(481, 1, 7, 1).
crosses(481, 2, 8, 3).
crosses(481, 3, 9, 1).
crosses(481, 4, 10, 1).
crosses(481, 5, 11, 3).
crosses(481, 6, 12, 1).
crosses(481, 7, 13, 1).
crosses(481, 0, 7, 1).
crosses(481, 1, 8, 2).
crosses(481, 2, 9, 3).
crosses(481, 3, 10, 1).
crosses(481, 4, 11, 2).
crosses(481, 5, 12, 1).
crosses(481, 6, 13, 2).
crosses(481, 0, 8, 2).
crosses(481, 1, 9, 2).
crosses(481, 2, 10, 3).
crosses(481, 3, 11, 2).
crosses(481, 5, 13, 2).
crosses(481, 0, 9, 2).
crosses(481, 1, 10, 2).
crosses(481, 2, 11, 4).
crosses(481, 4, 13, 1).
crosses(481, 0, 10, 2).
crosses(481, 1, 11, 3).
crosses(481, 2, 12, 1).
crosses(481, 0, 11, 3).
crosses(481, 2, 13, 1).
crosses(482, 0, 2, 1).
crosses(482, 2, 4, 2).
crosses(482, 4, 6, 1).
crosses(482, 6, 8, 2).
crosses(482, 8, 10, 1).
crosses(482, 10, 12, 3).
crosses(482, 1, 4, 1).
crosses(482, 2, 5, 2).
crosses(482, 3, 6, 1).
crosses(482, 5, 8, 1).
crosses(482, 6, 9, 2).
crosses(482, 7, 10, 1).
crosses(482, 9, 12, 2).
crosses(482, 0, 4, 1).
crosses(482, 1, 5, 1).
crosses(482, 2, 6, 3).
crosses(482, 4, 8, 1).
crosses(482, 5, 9, 1).
crosses(482, 6, 10, 3).
crosses(482, 8, 12, 2).
crosses(482, 0, 5, 1).
crosses(482, 1, 6, 2).
crosses(482, 2, 7, 2).
crosses(482, 3, 8, 1).
crosses(482, 4, 9, 1).
crosses(482, 5, 10, 2).
crosses(482, 6, 11, 1).
crosses(482, 7, 12, 1).
crosses(482, 0, 6, 2).
crosses(482, 1, 7, 1).
crosses(482, 2, 8, 3).
crosses(482, 3, 9, 1).
crosses(482, 4, 10, 2).
crosses(482, 6, 12, 2).
crosses(482, 0, 7, 1).
crosses(482, 1, 8, 2).
crosses(482, 2, 9, 3).
crosses(482, 3, 10, 2).
crosses(482, 5, 12, 1).
crosses(482, 0, 8, 2).
crosses(482, 1, 9, 2).
crosses(482, 2, 10, 4).
crosses(482, 4, 12, 1).
crosses(482, 0, 9, 2).
crosses(482, 1, 10, 3).
crosses(482, 2, 11, 1).
crosses(482, 0, 10, 3).
crosses(482, 2, 12, 1).
crosses(483, 0, 2, 1).
crosses(483, 2, 4, 2).
crosses(483, 3, 5, 1).
crosses(483, 6, 8, 2).
crosses(483, 9, 11, 1).
crosses(483, 11, 13, 3).
crosses(483, 1, 4, 1).
crosses(483, 2, 5, 3).
crosses(483, 3, 6, 1).
crosses(483, 5, 8, 2).
crosses(483, 6, 9, 2).
crosses(483, 8, 11, 1).
crosses(483, 10, 13, 2).
crosses(483, 0, 4, 1).
crosses(483, 1, 5, 2).
crosses(483, 2, 6, 3).
crosses(483, 4, 8, 1).
crosses(483, 5, 9, 2).
crosses(483, 6, 10, 2).
crosses(483, 7, 11, 1).
crosses(483, 9, 13, 2).
crosses(483, 0, 5, 2).
crosses(483, 1, 6, 2).
crosses(483, 2, 7, 2).
crosses(483, 3, 8, 1).
crosses(483, 4, 9, 1).
crosses(483, 5, 10, 2).
crosses(483, 6, 11, 3).
crosses(483, 8, 13, 2).
crosses(483, 0, 6, 2).
crosses(483, 1, 7, 1).
crosses(483, 2, 8, 3).
crosses(483, 3, 9, 1).
crosses(483, 4, 10, 1).
crosses(483, 5, 11, 3).
crosses(483, 6, 12, 1).
crosses(483, 7, 13, 1).
crosses(483, 0, 7, 1).
crosses(483, 1, 8, 2).
crosses(483, 2, 9, 3).
crosses(483, 3, 10, 1).
crosses(483, 4, 11, 2).
crosses(483, 5, 12, 1).
crosses(483, 6, 13, 2).
crosses(483, 0, 8, 2).
crosses(483, 1, 9, 2).
crosses(483, 2, 10, 3).
crosses(483, 3, 11, 2).
crosses(483, 5, 13, 2).
crosses(483, 0, 9, 2).
crosses(483, 1, 10, 2).
crosses(483, 2, 11, 4).
crosses(483, 4, 13, 1).
crosses(483, 0, 10, 2).
crosses(483, 1, 11, 3).
crosses(483, 2, 12, 1).
crosses(483, 0, 11, 3).
crosses(483, 2, 13, 1).
crosses(484, 0, 2, 1).
crosses(484, 2, 4, 2).
crosses(484, 4, 6, 1).
crosses(484, 6, 8, 2).
crosses(484, 8, 10, 1).
crosses(484, 10, 12, 3).
crosses(484, 1, 4, 1).
crosses(484, 2, 5, 2).
crosses(484, 3, 6, 1).
crosses(484, 5, 8, 1).
crosses(484, 6, 9, 2).
crosses(484, 7, 10, 1).
crosses(484, 9, 12, 2).
crosses(484, 0, 4, 1).
crosses(484, 1, 5, 1).
crosses(484, 2, 6, 3).
crosses(484, 4, 8, 1).
crosses(484, 5, 9, 1).
crosses(484, 6, 10, 3).
crosses(484, 8, 12, 2).
crosses(484, 0, 5, 1).
crosses(484, 1, 6, 2).
crosses(484, 2, 7, 2).
crosses(484, 3, 8, 1).
crosses(484, 4, 9, 1).
crosses(484, 5, 10, 2).
crosses(484, 6, 11, 1).
crosses(484, 7, 12, 1).
crosses(484, 0, 6, 2).
crosses(484, 1, 7, 1).
crosses(484, 2, 8, 3).
crosses(484, 3, 9, 1).
crosses(484, 4, 10, 2).
crosses(484, 6, 12, 2).
crosses(484, 0, 7, 1).
crosses(484, 1, 8, 2).
crosses(484, 2, 9, 3).
crosses(484, 3, 10, 2).
crosses(484, 5, 12, 1).
crosses(484, 0, 8, 2).
crosses(484, 1, 9, 2).
crosses(484, 2, 10, 4).
crosses(484, 4, 12, 1).
crosses(484, 0, 9, 2).
crosses(484, 1, 10, 3).
crosses(484, 2, 11, 1).
crosses(484, 0, 10, 3).
crosses(484, 2, 12, 1).
crosses(485, 1, 3, 1).
crosses(485, 3, 5, 1).
crosses(485, 6, 8, 1).
crosses(485, 8, 10, 1).
crosses(485, 0, 3, 1).
crosses(485, 3, 6, 1).
crosses(485, 5, 8, 1).
crosses(485, 3, 7, 1).
crosses(485, 4, 8, 1).
crosses(485, 3, 8, 2).
crosses(485, 2, 8, 1).
crosses(485, 3, 9, 1).
crosses(485, 1, 8, 1).
crosses(485, 3, 10, 1).
crosses(485, 0, 8, 1).
crosses(486, 1, 3, 1).
crosses(486, 6, 8, 1).
crosses(486, 0, 3, 1).
crosses(486, 1, 4, 1).
crosses(486, 5, 8, 1).
crosses(486, 0, 4, 1).
crosses(486, 1, 5, 1).
crosses(486, 4, 8, 1).
crosses(486, 0, 5, 1).
crosses(486, 1, 6, 1).
crosses(486, 3, 8, 1).
crosses(486, 0, 6, 1).
crosses(487, 1, 3, 1).
crosses(487, 4, 6, 1).
crosses(487, 0, 3, 1).
crosses(487, 1, 4, 1).
crosses(487, 3, 6, 1).
crosses(487, 0, 4, 1).
crosses(488, 1, 3, 1).
crosses(488, 3, 5, 1).
crosses(488, 6, 8, 1).
crosses(488, 8, 10, 1).
crosses(488, 0, 3, 1).
crosses(488, 3, 6, 1).
crosses(488, 5, 8, 1).
crosses(488, 3, 7, 1).
crosses(488, 4, 8, 1).
crosses(488, 3, 8, 2).
crosses(488, 2, 8, 1).
crosses(488, 3, 9, 1).
crosses(488, 1, 8, 1).
crosses(488, 3, 10, 1).
crosses(488, 0, 8, 1).
crosses(489, 1, 3, 1).
crosses(489, 6, 8, 1).
crosses(489, 0, 3, 1).
crosses(489, 1, 4, 1).
crosses(489, 5, 8, 1).
crosses(489, 0, 4, 1).
crosses(489, 1, 5, 1).
crosses(489, 4, 8, 1).
crosses(489, 0, 5, 1).
crosses(489, 1, 6, 1).
crosses(489, 3, 8, 1).
crosses(489, 0, 6, 1).
crosses(490, 1, 3, 1).
crosses(490, 3, 5, 1).
crosses(490, 5, 7, 2).
crosses(490, 0, 3, 1).
crosses(490, 1, 4, 1).
crosses(490, 2, 5, 1).
crosses(490, 4, 7, 1).
crosses(490, 0, 4, 1).
crosses(490, 1, 5, 2).
crosses(490, 3, 7, 1).
crosses(490, 0, 5, 2).

