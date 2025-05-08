:- use_module(library(sgml)).

start :-
	sentence_to_xml([tuple(the,det,det,dr(0,np,n)),tuple(student,noun,noun,n),tuple(slept,v,v,dl(0,np,s))], s, 1, XML), 
	xml_write(user_output, XML, []).

translate_formula(s, at(s)) :-
	!.
translate_formula(np, at(np)) :-
	!.
translate_formula(n, at(n)) :-
	!.
translate_formula(cl_r, at(cl_r)) :-
	!.
translate_formula(s_inf, at(s, inf)) :-
	!.
translate_formula(s_main, at(s, main)) :-
	!.
translate_formula(s_ppart, at(s, ppart)) :-
	!.
translate_formula(s_ppres, at(s, ppres)) :-
	!.
translate_formula(s_q, at(s, q)) :-
	!.
translate_formula(s_whq, at(s, whq)) :-
	!.
translate_formula(pp, at(pp)) :-
	!.
translate_formula(pp_a, at(pp, a)) :-
	!.
translate_formula(pp_de, at(pp, de)) :-
	!.
translate_formula(pp_par, at(pp, par)) :-
	!.
translate_formula(dr(I,A0,dia(J,box(J,B0))), dr(Lab, A, B)) :-
	!,
	atomic_list_concat([dr, I, diabox, J], '_', Lab),
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(p(I,A0,dia(J,box(J,B0))), p(Lab, A, B)) :-
	!,
	atomic_list_concat([I, diabox, J], '_', Lab),
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(dr(I,A0,box(J,dia(J,B0))), dr(Lab, A, B)) :-
	!,
	atomic_list_concat([I, boxdia, J], '_', Lab),
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(dl(I,dia(J,box(J,A0)),B0), dr(Lab, A, B)) :-
	!,
	atomic_list_concat([I, diabox, J], '_', Lab),
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(dr(I,A0,B0), dr(I,A,B)) :-
	!,
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(dl(I,A0,B0), dl(I,A,B)) :-
	!,
	translate_formula(A0, A),
	translate_formula(B0, B).
translate_formula(p(I,A0,B0), p(I,A,B)) :-
	!,
	translate_formula(A0, A),
	translate_formula(B0, B).


sentence_to_xml(Entries, Goal0, Id,
		element(sentence,
			[id=Id],
			[element(lexical_entries, [], EntriesXML),
			 element(goal_formula, [], [GoalXML])])) :-
	sentence_to_xml(Entries, 1, _, 1, M, EntriesXML),
	translate_formula(Goal0, Goal),
	formula_to_xml(Goal, M, _, GoalXML).

sentence_to_xml([], N, N, M, M, []).
sentence_to_xml([Entry|Entries], N0, N, M0, M, [XMLEntry|XMLEntries]) :-
	entry_to_xml(Entry, N0, N1, M0, M1, XMLEntry),
	sentence_to_xml(Entries, N1, N, M1, M, XMLEntries).

entry_to_xml(tuple(Word, Pos1, Pos2, Formula0), N0, N, M0, M,
	     element(entry, [id=N0],
		     [element(word, [], [Word]),
		      element(pos1, [], [Pos1]),
		      element(pos2, [], [Pos2]),
		      element(formula, [], [XMLFormula])])) :-
	!,
	N is N0 + 1,
	translate_formula(Formula0, Formula),
	formula_to_xml(Formula, M0, M, XMLFormula).

formula_to_xml(at(A), N0, N, element(atom, [id=N0], [A])) :-
	N is N0 + 1.
formula_to_xml(at(A, F), N0, N, element(atom, [id=N0, feature=F], [A])) :-
	N is N0 + 1.
formula_to_xml(dr(I,A,B), N0, N, element(binary,
				  [label=XML_Label],
				  [AXML,
				   BXML])) :-
	!,
	label(dr, I, XML_Label),
	formula_to_xml(A, N0, N1, AXML),
	formula_to_xml(B, N1, N,  BXML).
formula_to_xml(dl(I,A,B), N0, N, element(binary,
				  [label=XML_Label],
				  [AXML,
				   BXML])) :-
	!,
	label(dl, I, XML_Label), 
	formula_to_xml(A, N0, N1, AXML),
	formula_to_xml(B, N1, N,  BXML).
formula_to_xml(p(I,A,B), N0, N, element(binary,
				  [label=XML_Label],
				  [AXML,
				   BXML])) :-
	!,
	label(p, I, XML_Label), 
	formula_to_xml(A, N0, N1, AXML),
	formula_to_xml(B, N1, N,  BXML).
	      
label(Connective, Index, Label) :-
	atomic_list_concat([Connective,Index], '_', Label).
