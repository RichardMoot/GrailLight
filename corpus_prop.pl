:- op(400, xfy, \).

% given a lemmatized parse file (such as the parse files of the TLGbank), output a
% tab-separated table to user_output with, for each sentence in the corpus, 1) the
% number of words, 2) the number of atomic subformulas, and 3) the number of
% connectives.

export_corpus_degrees :-
	findall(N, clause(sent(N,_),_), List),
	export_corpus_degrees(List).

export_corpus_degrees([]).
export_corpus_degrees([N|Ns]) :-
	clause(sent(N,Sem), prob_parse(L,Sem)),
	export_sentence_degrees(L),
	export_corpus_degrees(Ns).

export_sentence_degrees(L) :-
	export_sentence_degrees(L, 0, 0, 0).

export_sentence_degrees([], W, D, C) :-
	format('~D\t~D\t~D~n', [W,D,C]).
export_sentence_degrees([si(_,_,_,L)|Rest], W0, D0, C0) :-
	count_item_stats(L, D0, D, C0, C),
	W is W0 + 1,
	export_sentence_degrees(Rest, W, D, C).

count_item_stats([], D, D, C, C).
count_item_stats([F-_|Rest], D0, D, C0, C) :-
	count_formula_stats(F, D0, D1, C0, C1),
	count_item_stats(Rest, D1, D, C1, C).

count_formula_stats(dia(_,F), D0, D, C0, C) :-
	!,
	C1 is C0 + 1,
	count_formula_stats(F, D0, D, C1, C).
count_formula_stats(box(_,F), D0, D, C0, C) :-
	!,
	C1 is C0 + 1,
	count_formula_stats(F, D0, D, C1, C).
count_formula_stats(dr(_,F1,F2), D0, D, C0, C) :-
	!,
	C1 is C0 + 1,
	count_formula_stats(F1, D0, D1, C1, C2),
	count_formula_stats(F2, D1, D, C2, C).
count_formula_stats(dl(_,F1,F2), D0, D, C0, C) :-
	!,
	C1 is C0 + 1,
	count_formula_stats(F1, D0, D1, C1, C2),
	count_formula_stats(F2, D1, D, C2, C).
count_formula_stats(p(_,F1,F2), D0, D, C0, C) :-
	!,
	C1 is C0 + 1,
	count_formula_stats(F1, D0, D1, C1, C2),
	count_formula_stats(F2, D1, D, C2, C).
count_formula_stats(_, D0, D, C, C) :-
	D is D0 + 1.
