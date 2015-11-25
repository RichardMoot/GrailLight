
:- ensure_loaded(lexicon).
:- ensure_loaded(big_french_drt).
:- ensure_loaded(ml).

% based on the file "ml.pl" output all words for which no semantics is found in big_french_drt 

start :-
	open('missing_formula_raw.txt', write, _Stream1, [alias(form)]), 
	open('missing_word_raw.txt', write, _Stream2, [alias(word)]), 
	clause(sent(_,Y), prob_parse(List,Y)),
	lookup_list(List),
	fail.
start :-
	close(form),
	close(word),
	shell('sort missing_formula_raw.txt | uniq -c | sort -nr > missing_formula.txt'),
	shell('sort missing_word_raw.txt | uniq -c | sort -nr > missing_word.txt').


lookup_list([]).
lookup_list([si(Word, Pos, Lemma, [Form0-_|_])|Rest]) :-
	get_pos_tt(Pos, PosTT),
	macro_expand(Form0, Form),
	get_item_semantics(Word, PosTT, Lemma, Form, Semantics),
	numbervars(Form),
	(Semantics = Word -> format(form, '~W~n', [Form,[numbervars(true)]]), format(word, '~w-~W~n', [Word,Form,[numbervars(true)]]) ; true),
	lookup_list(Rest).


get_pos_tt(Pos, PosTT) :-
      (
         Pos = _Melt0-Pos0:Sub
      ->
         PosTT = Pos0:Sub
      ;
         Pos = _Melt1-PosTT
      ->
         true
      ;
         PosTT = Pos
      ).
