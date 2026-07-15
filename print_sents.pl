
% script for translating superpos files back to the different input formats

print_formula(best_only).
print_pos(yes).
print_word(yes).

%%%%

print_sents :-
	clause(sent(_,_), prob_parse(List, _)),
	print_list(List),
	fail.
print_sents.

print_list([]) :- nl.
print_list([I|Is]) :-
	print_item(I),
	print_list(Is).

print_item(si(Word, Pos, _,  Forms)) :-
	format('~w|~p', [Word, Pos]),
	print_forms(Forms).


print_forms(Forms) :-
	print_formula(best_only),
	!,
	Forms = [Best-_|_],
	format('|~p ', [Best]).
	

print_forms(Forms) :-
	print_formula(all),
	print_all_forms(Forms).

print_all_forms([]) :-
	format(' ', []).
print_all_forms([Form-Prob|Rest]) :-
	format('|~p|~p', [Form, Prob]),
	print_all_forms(Rest).


%%%%

print_wfp :-
	clause(sent(_,_), prob_parse(List, _)),
	print_wfp_list(List),
	fail.
print_wfp.

print_wfp_list([]).
print_wfp_list([I|Is]) :-
	print_item_wfp(I),
	print_wfp_list(Is).

print_item_wfp(si(Word, Pos, _Lemma, [Form-_|_])) :-
	format('~w|~w|~w~n', [Word, Form, Pos]).

%%%

print_word_pos :-
	clause(sent(_,_), prob_parse(List, _)),
	print_word_pos_list(List),
	fail.
print_word_pos.

print_word_pos_list([]) :- nl.
print_word_pos_list([I|Is]) :-
	print_item_word_pos(I),
	print_word_pos_list(Is).

print_item_word_pos(si(Word, Pos,  _, _)) :-
	format('~w|~w ', [Word, Pos]).

%%%%%

print_words :-
	clause(sent(_,_), prob_parse(List, _)),
	print_word_list(List),
	fail.
print_words.

print_word_list([]) :- nl.
print_word_list([I|Is]) :-
	print_item_word(I),
	print_word_list(Is).

print_item_word(si(Word, _,  _, _)) :-
	format('~w ', [Word]).


