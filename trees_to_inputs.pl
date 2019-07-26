#!/Applications/SWI-Prolog.app/Contents/MacOS/swipl -q -g start -f

:- use_module(print_proof, [print_proof/3,print_proof/4]).

:- dynamic user:proof/2.
infile(aa1).
infile(ap1).
infile(aq2).
infile(as2).
infile(at).
infile(300).
infile(1000).
infile(8000).
infile(annodis).
infile(frwiki1).
infile(frwiki2).
infile(europar).
infile(emea_d).

silver_infile(aa2).
silver_infile(ab2).
silver_infile(ae1).
silver_infile(af2).
silver_infile(ag1).
silver_infile(ag2).
silver_infile(ah1).
silver_infile(ah2).
silver_infile(ai1).
silver_infile(ai2).
silver_infile(aj1).
silver_infile(ak1).
silver_infile(ak2).
silver_infile(al1).
silver_infile(am1).
silver_infile(am2).
silver_infile(an1).
silver_infile(an2).
silver_infile(ao1).
silver_infile(ao2).
silver_infile(monde_a).
silver_infile(monde_b).

mode(gold).

proof_file(File) :-
	mode(gold),
	infile(File).
proof_file(File) :-
	mode(silver),
	silver_infile(File).

matches_dir('/Users/moot/checkout/GrailLight/').

start :-
	current_prolog_flag(os_argv, Argv),
        append(_, [A|Av], Argv),
	file_base_name(A, 'trees_to_inputs.pl'),
	!,
	matches_dir(MDir),
        t2i(Av, MDir).

t2i([], _) :-
	/* treat all files if no file argument specified */
	trees_to_inputs,
	halt.
t2i([F|Fs], NDDir) :-
	t2i1([F|Fs], NDDir).

t2i1([], _) :-
	halt.
t2i1([File0|Files], NDDir) :-
   (	
	match_file(File0, File)
   ->		     
	format(user_error, '~NStarting ~w~n', [File]),
	atom_concat(File, '_matches.pl', File0),
	atom_concat(NDDir, File0, File),
	abolish(user:proof/2),
	user:compile(File),
	trees_to_inputs(File),
	format(user_error, '~NDone ~w~n', [File])
   ;
	format(user_error, '~NIgnored ~w~n', [File0])
   ),
	t2i1(Files, NDDir).

trees_to_inputs :-
	matches_dir(NDDir),
	proof_file(Root),
	format(user_error, '~NStarting ~w~n', [Root]),
	atom_concat(Root, '_matches.pl', InFile0),
	atom_concat(NDDir, InFile0, InFile),
	atom_concat(Root, '_trees.txt', OutFile),
	abolish(proof/2),
	compile(InFile),
	trees_to_inputs(OutFile),
	format(user_error, '~NDone ~w~n', [Root]),
	fail.
trees_to_inputs.


trees_to_inputs(OutputFile) :-
	open(OutputFile, write, Stream, []),
	trees_to_inputs1(Stream).

trees_to_inputs1(Stream) :-
	user:proof(N, Proof),
	output_proof(N, Proof, Stream),
	fail.
trees_to_inputs1(Stream) :-
	close(Stream).

output_proof(N, Proof, Stream) :-
	get_tree(N, TreeIndex),
	get_leaves(Proof, Leaves, []),
	print_result(TreeIndex, Stream),
	print_leaves(Leaves, Stream).

get_tree(T-_, TreeIndex) :-
	name(T, [116|Rest]),
	name(TreeIndex, Rest).

get_leaves(rule(_,Word,Form-word(_),[])) -->
	!,
	[Word-Form].
get_leaves(rule(_,_,_,Daughters)) -->
	get_daughter_leaves(Daughters).

get_daughter_leaves([]) -->
	[].
get_daughter_leaves([D|Ds]) -->
	get_leaves(D),
	get_daughter_leaves(Ds).

print_result(R, Stream) :-
	format(Stream, '~p ', [R]).

print_leaves([L|Ls], Stream) :-
	print_leaves(Ls, L, Stream).

print_leaves([], L, Stream) :-
	print_leaf(L, Stream),
	nl(Stream).
print_leaves([L|Ls], L0, Stream) :-
	print_leaf(L0, Stream),
	write(Stream, ' '),
	print_leaves(Ls, L, Stream).

print_leaf(W-F, Stream) :-
	print_word(W, Stream),
	write(Stream,'|'),
	print(Stream, F).
print_word('d\'', Stream) :-
	!,
	write(Stream, "d'").
print_word('D\'', Stream) :-
	!,
	write(Stream, "D'").
print_word('l\'', Stream) :-
	!,
	write(Stream, "l'").
print_word('L\'', Stream) :-
	!,
	write(Stream, "L'").
print_word('j\'', Stream) :-
	!,
	write(Stream, "j'").
print_word('J\'', Stream) :-
	!,
	write(Stream, "J'").
print_word(p(_,A,B), Stream) :-
	is_interpunction(A),
	!,
	print_word(B, Stream).
print_word(p(_,A,B), Stream) :-
	is_interpunction(B),
	!,
	print_word(A, Stream).
%print_word(p(I,A,B), Stream) :-
%	trace,
%	print_word(p(I,A,B), Stream).
print_word(A, Stream) :-
	write(Stream, A).

is_interpunction(p(_,A,B)) :-
	is_interpunction(A),
	is_interpunction(B),
	!.
is_interpunction(A) :-
	is_interpunction1(A).

is_interpunction1(',').
is_interpunction1('.').
is_interpunction1('!').
is_interpunction1('?').
is_interpunction1('-').
is_interpunction1(':').
is_interpunction1(';').
is_interpunction1('"').
is_interpunction1('\'').
is_interpunction1('(').
is_interpunction1(')').
is_interpunction1('[').
is_interpunction1(']').
is_interpunction1('*').
is_interpunction1('/').
is_interpunction1('...').
is_interpunction1('(...)').
