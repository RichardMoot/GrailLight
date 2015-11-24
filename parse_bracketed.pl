:- ensure_loaded(m2const).

% = read_trees(+FileName)
%
% read trees from FileName in Stanford parser output such as the following
%
% (NP (Det the) (ADJ interesting) (N idea))
%
% and converts them into GrailLight crosses/4 declarations.

read_trees(FileName) :-
	see(FileName),
	read_all_trees(0),
	seen.

read_all_trees(_) :-
	read_spaces,
	peek_char(end_of_file),
	!.
read_all_trees(N0) :-
	read_tree(Tree),
	portray_clause(Tree),
	/* compute constituents */
	abolish(constituent/4),
	retractall(constituent(_,_,_,_)),
	N is N0 + 1,
	tree_length(Tree, N, 0, Length),
	listing(constituent/4),
	/* compute crosses declarations */
	abolish(crosses/4),
	retractall(crosses(_,_,_,_)),
	compute_penalties1(N, Length),
	retractall(crosses(_,_,_,0)),
	listing(crosses/4),
	/* just in case, clean up choice points */
	!,
	read_all_trees(N).

tree_length(word(_), _, N0, N) :-
	N is N0 + 1.
tree_length(tree(Label, List), Sent, N0, N) :-
	tree_length_list(List, Sent, N0, N),
	/* assert only non-trivial constituents */
   (
	N > N0 + 1
   ->
        assert(constituent(Sent, Label, N0, N))
   ;
        true
   ).

tree_length_list([], _, N, N).
tree_length_list([T|Ts], Sent, N0, N) :-
	tree_length(T, Sent, N0, N1),
	tree_length_list(Ts, Sent, N1, N).


portray_tree(Tree) :-
	format('~N'),
	portray_tree(Tree, 0).

portray_tree(word(Word), _Tab) :-
	format('word(~p)', [Word]).
portray_tree(tree(Label, Daughters), Tab0) :-
	format('tree(~p, [', [Label]),
	Tab is Tab0 + 3,
	portray_tree_list(Daughters, Tab).


portray_tree_list([], _) :-
	format('])', []).
portray_tree_list([T|Ts], Tab) :-
	format('~N'),
	tab(Tab),
	portray_tree_list1(Ts, T, Tab).

portray_tree_list1([], T, Tab) :-
	portray_tree(T, Tab),
	format('~N', []),
	tab(Tab),
	format('])', []).
portray_tree_list1([T|Ts], T0, Tab) :-
	portray_tree(T0, Tab),
	format(',~n', []),
	tab(Tab),
	portray_tree_list1(Ts, T, Tab).

read_tree(Tree) :-
	get_char(C),
	read_tree(C, Tree).

read_tree('(', tree(Label, Ds)) :-
	read_label(Label),
	!,
	read_spaces,
	read_daughters(Ds).
read_tree(C, W) :-
	read_word(C, W),
	!.

read_daughters(Ds) :-
	get_char(C),
	read_daughters(C, Ds).

read_daughters(')', []) :-
	!.
read_daughters(C, [W|Ws]) :-
	read_tree(C, W),
	read_spaces,
	!,
	read_daughters(Ws).

read_label(Label) :-
	get_char(C),
	read_label(C, LabelL, []),
	atom_chars(Label, LabelL).

read_label(C) -->
	{char_type(C, upper)},
	!,
	{char_type(CL, to_lower(C))},
	[CL],
	{get_char(C2)},
	read_label(C2).
read_label(C) -->
	{char_type(C, lower)},
	!,
	[C],
	{get_char(C2)},
	read_label(C2).

read_label(' ') -->
	[].
read_label('\n') -->
	[].
read_label(end_of_file) -->
	[].


read_word(Label) :-
	get_char(C),
	read_word(C, Label).

read_word(C, Label) :-
	read_word(C, LabelL, []),
	atom_chars(Label0, LabelL),
	atomic_list_concat([A|As], '_', Label0),
	return_word(As, A, Label).

return_word([], A, word(A)).
return_word([A|As], A0, tree(w, [word(A0)|Words])) :-
	return_word_list([A|As], Words).

return_word_list([], []).
return_word_list([A|As0], [word(A)|As]) :-
	return_word_list(As0, As).


read_word(')') -->
	!,
	[].
read_word(' ') -->
	!,
	[].
read_word('\n') -->
	!,
	[].
read_word(end_of_file) -->
	!,
	[].
read_word(C, W0, W) :-
	char_type(C, graph),
	!,
	W0 = [C|W1],
  (
	peek_char(')')
  ->
	 W1 = W
  ;

	get_char(C2),
	read_word(C2, W1, W)
   ).

read_spaces :-
	peek_char(C),
	char_type(C, space),
	!,
	get_char(C),
	read_spaces.
read_spaces.
