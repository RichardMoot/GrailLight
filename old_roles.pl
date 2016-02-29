% = roles for verbs taking a single argument

% = roles for verbs taking two arguments
% if it is the subject which moves along the path and the movement is not necessarily agentive, it has the theme role.
role_lexicon(aller, theme, destination) :-
	!.
role_lexicon(aller_à, theme, destination) :-
	!.
role_lexicon(venir_de, theme, source) :-
	!.
role_lexicon(arriver_à, theme, destination) :-
	!.
role_lexicon(parvenir_à, theme, destination) :-
	!.
role_lexicon(revenir_à, theme, destination) :-
	!.
role_lexicon(venir_à, theme, destination) :-
	!.
role_lexicon(arriver, theme, destination) :-
	!.
role_lexicon(partir_de, theme, source) :-
	!.
role_lexicon(partir, theme, source) :-
	!.
role_lexicon(passer, agent, durée) :-
	!.
% object = theme
role_lexicon(affirmer, agent, theme) :-
	!.
role_lexicon(assurer, agent, theme) :-
	!.
role_lexicon(connaître, agent, theme) :-
	!.
role_lexicon(contrôler, agent, theme) :-
	!.
role_lexicon(critiquer, agent, theme) :-
	!.
role_lexicon(croire, agent, theme) :-
	!.
role_lexicon(déduire, agent, theme) :-
	!.
role_lexicon(définir, agent, theme) :-
	!.
role_lexicon(démontrer, agent, theme) :-
	!.
role_lexicon(déplorer, agent, theme) :-
	!.
role_lexicon(desirer, agent, theme) :-
	!.
role_lexicon(dire, agent, theme) :-
	!.
role_lexicon(entendre, agent, theme) :-
	!.
role_lexicon(entreprendre, agent, theme) :-
	!.
role_lexicon(être, agent, theme) :-
	!.
role_lexicon(expliquer, agent, theme) :-
	!.
role_lexicon(faire, agent, theme) :-
	!.
role_lexicon(fasciner, agent, theme) :-
	!.
role_lexicon(filmer, agent, theme) :-
	!.
role_lexicon(honorer, agent, theme) :-
	!.
role_lexicon(imaginer, agent, theme) :-
	!.
role_lexicon(indiquer, agent, theme) :-
	!.
role_lexicon(interdire, agent, theme) :-
	!.
role_lexicon(monopoliser, agent, theme) :-
	!.
role_lexicon(nier, agent, theme) :-
	!.
role_lexicon(penser, agent, theme) :-
	!.
role_lexicon(pouvoir, agent, theme) :-
	!.
role_lexicon(pratiquer, agent, theme) :-
	!.
role_lexicon(prouver, agent, theme) :-
	!.
role_lexicon(reconnaître, agent, theme) :-
	!.
role_lexicon(redouter, agent, theme) :-
	!.
role_lexicon(refuser, agent, theme) :-
	!.
role_lexicon(regarder, agent, theme) :-
	!.
role_lexicon(regretter, agent, theme) :-
	!.
role_lexicon(réglementer, agent, theme) :-
	!.
role_lexicon(résoudre, agent, theme) :-
	!.
role_lexicon(redouter, agent, theme) :-
	!.
role_lexicon(savoir, agent, theme) :-
	!.
role_lexicon(souhaiter, agent, theme) :-
	!.
role_lexicon(suggérer, agent, theme) :-
	!.
role_lexicon(longer, agent, lieu) :-
	!.
role_lexicon(_, agent, patient).

% = roles for verbs taking three arguments

role_lexicon(amener_à, agent, patient, destination) :-
	!.
role_lexicon(donner_à, agent, patient, destination) :-
	!.
role_lexicon(offrir_à, agent, patient, destination) :-
	!.
role_lexicon(rendre_à, agent, patient, destination) :-
	!.
role_lexicon(demander_à, agent, patient, source) :-
	!.
role_lexicon(prendre_de, agent, patient, source) :-
	!.
role_lexicon(prendre_à, agent, patient, source) :-
	!.
role_lexicon(mettre, agent, patient, destination) :-
	!.
role_lexicon(_, agent, patient, theme).

role_lexicon_np_adj(_, agent, theme).
role_lexicon_np_np_adj(_, agent, patient, theme).


role_lexicon_np_loc(entrer, agent, destination) :-
	!.
role_lexicon_np_loc(aller, agent, destination) :-
	!.
role_lexicon_np_loc(être, agent, lieu) :-
	!.
role_lexicon_np_loc(figurer, agent, lieu) :-
	!.
role_lexicon_np_loc(habiter, agent, lieu) :-
	!.
role_lexicon_np_loc(résider, agent, lieu) :-
	!.
role_lexicon_np_loc(_, agent, lieu).

role_lexicon_np_np_loc(mettre, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(placer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(envoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(renvoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(_, agent, patient, lieu).

role_lexicon_np_loc(entrer, agent, destination) :-
	!.
role_lexicon_np_loc(aller, agent, destination) :-
	!.
role_lexicon_np_loc(être, agent, lieu) :-
	!.
role_lexicon_np_loc(figurer, agent, lieu) :-
	!.
role_lexicon_np_loc(habiter, agent, lieu) :-
	!.
role_lexicon_np_loc(résider, agent, lieu) :-
	!.
role_lexicon_np_loc(_, agent, lieu).

role_lexicon_np_np_loc(mettre, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(placer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(envoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(renvoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(_, agent, patient, lieu).
