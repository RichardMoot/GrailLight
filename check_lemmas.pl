:- ensure_loaded(lefff).

% given a superpos file, checks for strange word-pos-lemma combinations

check_lemmas :-
	clause(sent(_,_), prob_parse(List, _)),
	check_lemmas_list(List),
	fail.
check_lemmas.

check_lemmas_list([]).
check_lemmas_list([I|Is]) :-
	check_item_lemma(I),
	check_lemmas_list(Is).

check_item_lemma(si(Word, Pos0,  Lemma, _)) :-
	get_pos(Pos0, Pos),
	atom_string(Word, WS),
	string_lower(WS, WLS),
	atom_string(LWord, WLS),
	(
	   (lefff(Word, Pos, Lemma, _, _, _, _, _, _) ;  lefff(LWord, Pos, Lemma, _, _, _, _, _, _))
	->
	   true
	;
	format('~w ~w ~w~n', [Word,  Pos, Lemma])
	).


get_pos(_-Pos, Pos).
get_pos(_-Pos0:Pos, Pos0:Pos).
