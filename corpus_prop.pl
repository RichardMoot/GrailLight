:- op(400, xfy, \).

export_corpus_degrees :-
	findall(N, clause(sent(N,_),_), List),
	export_corpus_degrees(List).

export_corpus_degrees([]).
export_corpus_degrees([N|Ns]) :-
	clause(sent(N,Sem), prob_parse(L,Sem)),
	export_sentence_degrees(L),
	export_corpus_degrees(Ns).

