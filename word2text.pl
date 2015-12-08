% transform words file (as produced as output by m2const) into a text file (as used as input for taggers)

export_words :-
	setof(Sent, A^B^C^word(Sent, A, B, C), List),
	export_words(List).

export_words([]).
export_words([S|Ss]) :-
	export_sentence_words(S),
	export_words(Ss).

export_sentence_words(Sent) :-
	findall(W, word(Sent, W, _, _), Words),
	write_words(Words).

write_words([]) :-
	nl.
write_words([W|Ws]) :-
	write_words(Ws, W).

write_words([], W) :-
	write(W),
	nl.
write_words([W|Ws], W0) :-
	write(W0),
	write(' '),
	write_words(Ws, W).
