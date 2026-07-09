
% script for translating superpos files back to the different input formats

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

print_forms([]) :-
	format(' ', []).
print_forms([Form-Prob|Rest]) :-
	format('|~p|~p', [Form, Prob]),
	print_forms(Rest).



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
