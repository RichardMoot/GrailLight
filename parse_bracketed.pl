
read_line :-
	read_tree(X).

read_tree(Tree) :-
	get_char(C),
	read_tree(C, Tree).

read_tree('(', Tree) :-
	!,
	read_label(Label),
	read_daughters(Ds),
	get_char(')'),
	Tree =.. [Label|Ds].
read_tree(C, W) :-
	!,
	read_word(W).

read_daughters(Ds) :-
	get_char(C),
	read_daughters(C, Ds).

read_daughters(C, W) 

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

read_label(' ') -->
	[].
read_label('\n') -->
	[].
read_label(end_of_file) -->
	[].


read_word(Label) :-
	get_char(C),
	read_word(C, LabelL, []),
	atom_chars(Label, LabelL).

read_word(C) -->
	{char_type(C, graph)},
	!,
	[C],
	{get_char(C2)},
	read_word(C2).

read_word(' ') -->
	[].
read_word('\n') -->
	[].
read_word(end_of_file) -->
	[].