
:- module(m2const, [start/0,
		    start/1,
		    update_all/0,
		    export_all/0,
		    export/1,
		    export_text/1,
		    verify_sentences/0,
		    compute_penalties/0,
		    compute_penalties1/2]).

% constituent calculations

% start/0   computes word/4 and constituent/4 declarations from all files declared by xml_files/1
%
% export(File) all-in-one predicate combining *head.pl and the current XML files into a single *.pl file
%
% compute_penalties/0   computes crosses/4 declarations from constituent/4 declarations for all sentences
%
% cp(List)            as compute_penalties for all sentences in List
%
% update_crosses(Sent, Left, Right, Diff)    update given crosses/4 information by adding Diff to all
%                                            constituents crossing Left-Right
%
% update_crosses(Sent, Left, Right)          equivalent to update_crosses(Sent, Left, Right, 1).
%
% verify_sentences/0   verify if the words used in sent/2 declarations correspond to the given word/4
%                      declarations
%
% verify_lemmas/0

verbose(false).

xml_files(File) :-
	xml_files(File, _).

xml_files('flmf7aa1ep.cat.xml', aa1).
xml_files('flmf7aa2ep.cat.xml', aa2).
xml_files('flmf7ab2ep.xml', ab2).
xml_files('flmf7ae1ep.cat.xml', ae1).
xml_files('flmf7af2ep.cat.xml', af2).
xml_files('flmf7ag1exp.cat.xml', ag1).
xml_files('flmf7ag2ep.cat.xml', ag2).
xml_files('flmf7ah1ep.aa.xml', ah1).
xml_files('flmf7ah2ep.aa.xml', ah2).
xml_files('flmf7ai1exp.cat.xml', ai1).
xml_files('flmf7ai2ep.aa.cat.xml', ai2).
xml_files('flmf7aj1ep.indent.xml', aj1).
xml_files('flmf7ak1ep.indent.xml', ak1).
xml_files('flmf7ak2ep.xd.cat.xml', ak2).
xml_files('flmf7al1ep.cat.xml', al1).
xml_files('flmf7am1ep.xd.cat.xml', am1).
xml_files('flmf7am2ep.xd.cat.xml', am2).
xml_files('flmf7an1ep.xml', an1).
xml_files('flmf7an2co.af.cat.xml', an2).
xml_files('flmf7ao1ep.xml', ao1).
xml_files('flmf7ao2ep.xml', ao2).
xml_files('flmf7ap1ep.af.cat.xml', ap1).
xml_files('flmf7aq2ep.xd.cat.xml', aq2).
xml_files('flmf7as2ep.af.cat.xml', as2).
xml_files('flmf7atep.cat.xml', at).

xml_files('flmf300_13000ep.cat.xml', '300').
xml_files('flmf3_01000_01499ep.aa.xml','1000').
%xml_files('flmf3_03500_03999ep.aa.xml','3500').
xml_files('flmf3_08000_08499ep.xd.cat.xml', '8000').
%xml_files('flmf3_08500_08999ep.aa.xml','8500').
%xml_files('flmf3_09000_09499ep.aa.xml','9000').
%xml_files('flmf3_10000_10499ep.aa.xml','10000').
%xml_files('flmf3_11000_11499ep.aa.xml','11000').
%xml_files('flmf3_12000_12499ep.aa.xml','12000').
%xml_files('flmf3_12500_12999co.aa.xml','12500').

xml_files('annodis.er.xml', annodis).
xml_files('frwiki1.xml', frwiki1).
xml_files('frwiki2.xml', frwiki2).
xml_files('Europar.550.xml', europar).
xml_files('emea-fr-dev.xml', emea_d).
% xml_files('emea-fr-test.xml', emea_t).

% NOTE: all exported predicates are declared as being in module "user", since otherwise predicates like listing/1
% will prefix the module "m2const" leading to incorrect beheviour.

:- dynamic user:word/4, user:lemma/4, user:constituent/4, user:crosses/4, sent/2, current_file/1.

% create word/4 and constituent/4 declarations for the XML files declared by xml_file/1 (above).

start :-
	user:abolish(word/4),
	user:retractall(lemma(_,_,_,_)),
	user:retractall(word(_,_,_,_)),
	user:retractall(constituent(_,_,_,_)),
	findall(F, xml_files(F), Files),
	start(Files, 0, _).

start([], N, N).
start([F|Fs], N0, N) :-
	format(user_error, '~NXML File: ~w~n', [F]),
	load_structure(F, L0, [dialect(xml), space(default)]),
	delete_all_spaces(L0, L),
	xml_to_const(L, N0, N1, 0, _),
	nl(user_error),
	start(Fs, N1, N).

start(XMLFile) :-
	abolish(word/4),
	user:retractall(lemma(_,_,_,_)),
	user:retractall(word(_,_,_,_)),
	user:retractall(constituent(_,_,_,_)),
	retractall(current_file(_)),
	assert(current_file(XMLFile)),
	load_structure(XMLFile, L0, [dialect(xml), space(default)]),
	delete_all_spaces(L0, L),
	xml_to_const(L, 0, _, 0, _).

update_all :-
	user:abolish(word/4),
	user:retractall(lemma(_,_,_,_)),
	user:retractall(word(_,_,_,_)),
	user:retractall(constituent(_,_,_,_)),
	findall(F, xml_files(F), Files0),
	update_files(Files0, Files, Warn),
	nl(user_error),
	export_all(Files),
	format('~n= Update done!=~2n', []),
	format('Exported: ~@', [print_flat_list(Files)]),
	format('Warning : ~@', [print_flat_list(Warn)]).

% = update_files(+AllFiles, -UpdatedFiles, -Changed)
%
% 

update_files([], [], []).
update_files([XMLFile|Fs0], Fs, Ws) :-
	xml_files(XMLFile, FileRoot),
	atom_concat(FileRoot, 'head.pl', HeadFile),
	check_exists(HeadFile),
	atom_concat('head/', HeadFile, HHeadFile),
	check_exists(HHeadFile),
	atom_concat(FileRoot, '.pl', TargetFile),
	time_file(HeadFile, Time),
	time_file(HHeadFile, Time1),
        time_file(TargetFile, Time2),
	time_file(XMLFile, TimeX),
	atomic_list_concat(['diff ', HHeadFile, ' ', HeadFile, '> /dev/null'], Cmd),
  (
        /* check if corresponding file extracted from treebank is newer and different */
	/* warn if it is */
	Time1 > Time,
	\+ shell(Cmd)  
  ->
        format(user_error, '{Warning: file "~p" has been updated}~n', [HHeadFile]),
        Ws = [FileRoot|Ws1]
  ;
        Ws = Ws1
  ),	   
  (
	/* schedule file only if either the xml or the head file has changed */  
        Time2 > max(Time,TimeX)
  ->
        format(user_error, '(~p) ', [FileRoot]),
        Fs = Fs1
  ;
        format(user_error, '~p ', [FileRoot]),
        Fs = [FileRoot|Fs1]
  ),
        update_files(Fs0, Fs1, Ws1).

print_flat_list([]) :-
	format('none!~n', []).
print_flat_list([X|Xs]) :-
	print_flat_list(Xs, X).
print_flat_list([], X) :-
	format('~p~n', [X]).
print_flat_list([X|Xs], Y) :-
	format('~p, ', [Y]),
	print_flat_list(Xs, X).

export_all :-
	findall(FileRoot, xml_files(_, FileRoot), List),
	export_all(List).

export_all([]).
export_all([F|Fs]) :-
	export(F),
	export_all(Fs).

% = export(FileRoot)
%
% Given a file *head.pl and the currently loaded XML file, create a file *.pl (with intermediate
% files *const.pl, *word.pl and *crosses.pl, this last one will be deleted, because of its size)
% (* is replaced by FileRoot).

export(FileRoot) :-
	xml_files(XMLFile, FileRoot),
	atom_concat(FileRoot, 'head.pl', HeadFile),
	check_exists(HeadFile),
	atom_concat(FileRoot, '.pl', TargetFile),
	atom_concat(FileRoot, 'const.pl', ConstFile),
	delete_if_exists(ConstFile),
	atom_concat(FileRoot, 'word.pl', WordFile),
	delete_if_exists(WordFile),
	atom_concat(FileRoot, 'crosses.pl', CrossFile),
	delete_if_exists(CrossFile),
	format(user_error, '~NXML File: ~w~n', [XMLFile]),
	start(XMLFile),
	nl(user_error),
	tell(WordFile),
	user:listing(word(_,_,_,_)),
	told,
	abolish(sent/2),
	[HeadFile],
	rebracket_constituents,
	tell(ConstFile),
	user:listing(constituent(_,_,_,_)),
	told,
	compute_penalties,
	format(user_error, '~2nSaving penalties...', []),
	flush_output(user_error),
	tell(CrossFile),
	user:listing(crosses(_,_,_,_)),
	told,
	format(user_error, 'done!~n', []),
	format(user_error, 'Exporting...', []),
	flush_output(user_error),
	atomic_list_concat([cat,HeadFile,WordFile,CrossFile,'>',TargetFile], ' ', Cmd),
	format('~N~w~n', [Cmd]),
	process_create(path(sh), ['-c',Cmd], []),
	delete_file(CrossFile),
	format(user_error, 'done!~n', []).

% = export_text(+FileRoot)
%
% export the XML annotation file denoted by FileRoot as a standard text file (with .txt extension),
% using the same mechanisms as the extraction procedure.

export_text(FileRoot) :-
	xml_files(XMLFile, FileRoot),
	atom_concat(FileRoot, '.txt', TextFile),
	delete_if_exists(TextFile),
	user:abolish(word/4),
	user:retractall(lemma(_,_,_,_)),
	user:retractall(word(_,_,_,_)),
	user:retractall(constituent(_,_,_,_)),
	start([XMLFile], 0, N),
	nl(user_error),
	tell(TextFile),
	export_text_words(0, N),
	told.

export_text_words(N, N) :-
	!.
export_text_words(N0, N) :-
	N0 < N,
	N1 is N0 + 1,
	findall(W, word(N1, W, _, _), WL),
	print_word_list(WL),
	!,
	export_text_words(N1, N).

print_word_list([]) :-
	nl.
print_word_list([W|Ws]) :-
	format(' ~W', [W,[quoted(false)]]),
	print_word_list(Ws).

check_exists(File) :-
   (
	exists_file(File)
   ->	
        true
   ;
        format(user_error, '~NFile ~w not found, aborting!~n', [File]),
        fail
   ).
   
delete_if_exists(File) :-
   (
	exists_file(File)
   ->
        delete_file(File)
   ;
	true
   ).
   
delete_all_spaces(Es0, Es) :-
	filter_list(Es0, Es1),
	delete_all_spaces1(Es1, Es).

delete_all_spaces1([], []).
delete_all_spaces1([E0|Es0], [E|Es]) :-
	delete_element_spaces(E0, E),
	delete_all_spaces1(Es0, Es).

delete_element_spaces(element(A,B,Cs0), element(A,B,Cs)) :-
	!,
	filter_list(Cs0, Cs1),
	delete_all_spaces1(Cs1, Cs).
delete_element_spaces(E, E).


handle_word_list([], _, N, N).
handle_word_list([W|Ws], S, N0, N) :-
	handle_word(W, S, N0, N1),
	handle_word_list(Ws, S, N1, N).


handle_word('C.L.', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'C.',
	Word2 = 'L.',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('H.D.', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'H.',
	Word2 = 'D.',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('B.F.', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'B.',
	Word2 = 'F.',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('l\'isloise', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'l\'',
	Word2 = isloise,
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('jusqu\'au', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'jusqu\'',
	Word2 = au,
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('l\'on', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'l\'',
	Word2 = on,
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word('T.bond', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'T',
	Word2 = bond,
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('Pan.Am', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = 'Pan',
	Word2 = 'Am',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('de la', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = de,
	Word2 = la,
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('jusque-là', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = jusque,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
handle_word('de l\'', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word1 = de,
	Word2 = 'l\'',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word('R.R.Donnelley', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N2 is N1 + 1,
	N is N2 + 1,
	Word1 = 'R.',
	Word2 = 'R.',
	Word3 = 'Donnelley',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N2]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word3, N2, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N2)),
	user:assert(word(S, Word3, N2, N)).

handle_word('L.C.Waïkiki', S, N0, N) :-
	!,
	N1 is N0 + 1,
	N2 is N1 + 1,
	N is N2 + 1,
	Word1 = 'L.',
	Word2 = 'C.',
	Word3 = 'Waïkiki',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N2]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word3, N2, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N2)),
	user:assert(word(S, Word3, N2, N)).


handle_word(W, S, N0, N) :-
	current_file('flmf7aa1ep.cat.xml'),
	atomic_list_concat([Word1,ci], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-ci',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	current_file('flmf7am1ep.xd.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	current_file('flmf3_08000_08499ep.xd.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	current_file('flmf7atep.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	current_file('flmf7ak2ep.xd.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	current_file('flmf7ai2ep.aa.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).


handle_word(W, S, N0, N) :-
	current_file('flmf7ag1exp.cat.xml'),
	atomic_list_concat([Word1,là], '-', W),
	Word1 \= '',
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	Word2 = '-là',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).

handle_word(W, S, N0, N) :-
	W \= '&',
	atomic_list_concat([Word1,Word3], '&', W),
	!,
	N1 is N0 + 1,
	N2 is N1 + 1,
	N is N2 + 1,
	Word2 = '&',
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N2]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word3, N2, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N2)),
	user:assert(word(S, Word3, N2, N)).

handle_word(W, S, N0, N) :-
	atom_chars(W, List),
	append(Prefix, ['°','C'], List),
	Prefix \= [],
	!,
	N1 is N0 + 1,
	N is N1 + 1,
	atom_chars(Word1, Prefix),
	atom_chars(Word2, ['°','C']),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word1, N0, N1]),
	format1('word(~w, ~k, ~w, ~w).~n', [S, Word2, N1, N]),
	user:assert(word(S, Word1, N0, N1)),
	user:assert(word(S, Word2, N1, N)).
	
handle_word(W, S, N0, N) :-
	N is N0 + 1,
	format1('word(~w, ~k, ~w, ~w).~n', [S, W, N0, N]),
	user:assert(word(S, W, N0, N)).

% = xml_to_const(+ElementList, +ParentElement)
%
%

xml_to_const([], N, N, M, M).
xml_to_const([E|Es], N0, N, M0, M) :-
	element_to_const(E, N0, N1, M0, M1),
	xml_to_const(Es, N1, N, M1, M).

get_number(Int, _, S) :-
	integer(Int),
	!,
	S = Int.
get_number(_-Int, _, S) :-
	integer(Int),
	!,
	S = Int.
get_number(Atom, _, S) :-
	atomic_list_concat(List, -, Atom),
	last(List, Last),
	integer(Last),
	!,
	S = Last.
get_number(_, S0, S) :-
	S is S0 + 1.

element_to_const(element(Nm,As,Cs), N0, N, M0, M) :-
	!,
	element_to_const(Nm, As, Cs, N0, N, M0, M).

element_to_const('SENT', As, Cs, S0, S, _, M) :-
	!,
	write(user_error, '.'),
    (
        member(nb=Num,As)
    ->
        get_number(Num, S0, S1)
    ;
	S1 is S0 +1
    ),
	xml_to_const(Cs, S1, S, 0, M).

element_to_const(w, As, Cs, S, S, M0, M) :-
	!,
	get_lemma(As, Lemma),
	collect_words(Cs, Ws0, []),
	simplify_words(Ws0, Ws),
    (   Ws \= []
    ->
	smart_concat_atoms(Ws, Word),
	atomic_list_concat(L, '_', Word),
	handle_word_list(L, S, M0, M),
    (
	M > M0 + 1
    ->
        format1('constituent(~w, w, ~w, ~w).~n', [S, M0, M]),
        user:assert(constituent(S, w, M0, M))
    ;
        user:assert(lemma(S, Lemma, M0, M))
    )
    ;
	M is M0
    ).
element_to_const(Cat, _As, Cs, S0, S, N0, N) :-
	!,
	xml_to_const(Cs, S0, S, N0, N),
    (
	N > N0 + 1
    ->
        format1('constituent(~w, ~k, ~w, ~w).~n', [S0, Cat, N0, N]),
        user:assert(constituent(S0, Cat, N0, N))
     ;
        true
    ).

% simplify_words(+ListOfWords, -SimplifiedListOfWords)
%
% specify various rewrites of the different multi-words/complex words, ie. these rules
% are applied to a list of "subwords" of a "w" constituent.

simplify_words(List0, List) :-
	append(Prefix, ['Gmb.H'], List0),
	append(Prefix, ['GmbH'], List),
	!.
simplify_words(List0, List) :-
	append(Prefix, ['Gmb.','H'], List0),
	append(Prefix, ['GmbH'], List),
	!.

simplify_words(['Puma','A.G.Rudolf', 'Dassler', 'Sport'],
	       ['Puma','A.', 'G.', 'Rudolf', 'Dassler', 'Sport']) :-
	!.
simplify_words(['R','19'], ['R','19']) :-
	current_file('flmf7aj1ep.indent.xml'),
	!.
simplify_words(['R','19'], ['R.19']) :-
	!.
simplify_words(['presqu\'','île'], ['presqu\'île']) :-
	!.
simplify_words([X,Y,'aujourd\'',hui], [X,Y,'aujourd\'hui']) :-
	!.
simplify_words([X,'aujourd\'',hui], [X,'aujourd\'hui']) :-
	!.
simplify_words([X,Y,'d\'',oeuvre], [X,Y,'d\'oeuvre']) :-
	!.
simplify_words(['Côte',-,'d\'',ivoire], ['Côte-d\'ivoire']) :-
	!.
simplify_words(['Côtes',-,'d\'','Armor'], ['Côtes-d\'Armor']) :-
	!.
simplify_words(['côte',-,'d\'',ivoire], ['côte-d\'ivoire']) :-
	!.
simplify_words(['Monde',-,'l\'','économie'], ['Monde-l\'économie']) :-
	!.
simplify_words([X,s], [Y]) :-
	atom_chars(X, List),
	last(List, '\''),
	!,
	atom_concat(X, s, Y).
simplify_words([no,'man\'',s,land], [no,'man\'s',land]) :-
	!.
simplify_words(['Rubik\'',s,cube], ['Rubik\'s',cube]) :-
	!.
simplify_words(['Who\'',s,next], ['Who\'s',next]) :-
	!.
simplify_words([X,-,ci], [Atom]) :-
	!,
	atomic_list_concat([X,-,ci], Atom).
simplify_words([pub,-,info], ['pub-info']) :-
	!.
% simplify_words([ville,-,campagne], ['ville-campagne']) :-
% 	!.
simplify_words([demi,-,X], ['demi-', X]) :-
	!.
simplify_words([mi,-,X], ['mi-', X]) :-
	!.
simplify_words(['FR', '3'], ['FR3']) :-
	!.
simplify_words(['Demak\'', up], ['Demak\'up']) :-
	!.
simplify_words(['FR', '3', X], ['FR3', X]) :-
	!.
simplify_words(['Antenne', 2, -, 'FR', 3], ['Antenne2-FR3']) :-
	!.
simplify_words(['Antenne', '2', -, 'FR', '3'], ['Antenne2-FR3']) :-
	!.
simplify_words([X], [Y,h,Z]) :-
	atomic_list_concat([Y0,Z0], h, X),
	atom_number(Y0, Y),
	atom_number(Z0, Z),
	!.
% simplify_words(['TF', 1], ['TF1']) :-
% 	!.
% simplify_words(['TF', '1'], ['TF1']) :-
% 	!.
simplify_words(Ws, Ws).


collect_words([]) -->
	[].
collect_words([W|Ws]) -->
	!,
	collect_word(W),
	collect_words(Ws).

collect_word(element(w,_,Ws)) -->
	!,
	collect_words(Ws).
collect_word(W) -->
	[W].

smart_concat_atoms(['arrière-', 'petits-enfants'], 'arrière-petits-enfants') :-
	!.
smart_concat_atoms(['aujourd\'',hui], 'aujourd\'hui') :-
	!.
smart_concat_atoms(['quelqu\'',un], 'quelqu\'un') :-
	!.
smart_concat_atoms(['Aujourd\'',hui], 'Aujourd\'hui') :-
	!.
smart_concat_atoms(Cs, Atom) :-
    (
        append(As, [-|Bs], Cs)
     ->
        smart_concat_atoms(As, AAtom),
        smart_concat_atoms(Bs, BAtom),
        concat_atom([AAtom,-,BAtom], Atom)
    ;
	append(As, [','|Bs], Cs)
    ->
        smart_concat_atoms(As, AAtom),
        smart_concat_atoms(Bs, BAtom),
        concat_atom([AAtom,',',BAtom], Atom)
     ;
        all_digits(Cs)
    ->
        concat_atom(Cs, '.', Atom)
    ;
	concat_atom(Cs, '_', Atom)
     ).
     

get_lemma(List, Lemma) :-
	member(lemma=Lemma, List),
	!.
get_lemma(_, '???').

all_digits([]).
all_digits([D|Ds]) :-
	integer(D),
	!,
	all_digits(Ds).
all_digits([D|Ds]) :-
	/* atom_number/2 fails silently if D is not a number */
	atom_number(D, _),
	all_digits(Ds).

% = filter_list(+ListA, ?ListB)
%
% true if ListA and ListB have the same elements except for the ' ' which
% are removed from ListB.

filter_list([], []).
filter_list([X|Xs], Ys0) :-
    (
	X == ' '
    ->
	filter_list(Xs, Ys0)
    ;
	Ys0 = [X|Ys],
	filter_list(Xs, Ys)
    ).

% = member1(?Element, +List)
%
% true if List contains Element. Like the library predicate member/2, but
% succeeds at most once. Unlike member_chk/2 unification is used instead of
% strict identity.

member1(X, [X|_]) :-
	!.
member1(X, [_|Ys]) :-
	member1(X, Ys).

cp(N0, N) :-
        compute_penalties1(N0),
    (
        N0 >= N
    ->
        true
    ;
        N1 is N0 + 1,
        cp(N1, N)
    ).

cp(List) :-
	user:abolish(crosses/4),
	user:retractall(crosses(_,_,_,_)),
	compute_penalties(List),
	user:retractall(crosses(_,_,_,0)).

% = compute_penalties
%
% compute penalties (as crosses/4 declarations) for all sentences

compute_penalties :-
	user:abolish(crosses/4),
	user:retractall(crosses(_,_,_,_)),
	/* recover list of sentence numbers */
	user:setof(X, A^B^C^constituent(X,A,B,C), Sentences),
	compute_penalties(Sentences),
	/* erase zero entries */
	user:retractall(crosses(_,_,_,0)).

% = compute_let(+SentNo, -Let)
%
% true if Let is the list containing all right edge numbers of words assigned the formula "let"

compute_let(SentNo, Let) :-
	clause(sent(SentNo,Sem),prob_parse(List0,Sem)),
	!,
	compute_let(List0, SentNo, 0, Let).
% do nothing when no clauses have been found
compute_let(_, []).

get_word(Sent, Word, L, R) :-
	word(Sent, Word, L, R),
	!.
get_word(_, 'NULL', _, _).


compute_let([], _, _, []).
compute_let([si(Word, _, _, [Formula-_])|Rest], SentNo, N0, Let0) :-
	N is N0 + 1,
	get_word(SentNo, Word0, N0, N),
   (
	/* do not distinguish number atoms '9' from integers */
	atom_codes(Word0, Codes0),
	atom_codes(Word, Codes),
	Codes = Codes0
   ->
	true
   ;
        format(user_error, '[~d] ~d-~d "~w" "~w"~n', [SentNo,N0,N,Word0,Word])
   ),
   (
        Formula = let
   ->
        Let0 = [N|Let1]
   ;
        Let1 = Let0
   ),
        compute_let(Rest, SentNo, N, Let1).

% =
% TODO: add some additional rules, for example:
% - when a "VPinf" constituent has "de" as first word, add a new "VPinf" constituent without "de"
% Recommended when release is stable, followed by verification

rebracket_constituents :-
	user:setof(X, A^B^C^constituent(X,A,B,C), Sentences),
	rebracket_constituents_list(Sentences).

rebracket_constituents_list([]).
rebracket_constituents_list([S|Ss]) :-
	rebracket_constituents(S),
	rebracket_constituents_list(Ss).

rebracket_constituents(S) :-
	compute_let(S, Let),
	reverse(Let, LetR),
	rebracket_constituents_left(LetR, S),
	rebracket_constituents_right(Let, S).

rebracket_constituents_right([], _).
rebracket_constituents_right([LR|Ls], S) :-
	LR1 is LR - 1,
	user:findall(t(Cat,R), constituent(S, Cat, LR, R), ListTR), % touching interpunction symbol on right edge 
	user:retractall(constituent(S, Cat, LR, _)),
	assert_all_right(ListTR, S, LR1),
	rebracket_constituents_right(Ls, S).

	
rebracket_constituents_left([], _).
rebracket_constituents_left([LR|Ls], S) :-
	LR1 is LR - 1,
	user:findall(t(Cat,L), constituent(S, Cat, L, LR), ListTL), % touching interpunction symbol on left edge
	user:retractall(constituent(S, Cat, _, LR)),
	assert_all_left(ListTL, S, LR1),
	rebracket_constituents_left(Ls, S).

assert_all_left([], _, _).
assert_all_left([t(Cat,L)|Rest], S, LR) :-
	user:assert(constituent(S, Cat, L, LR)),
	assert_all_left(Rest, S, LR).

assert_all_right([], _, _).
assert_all_right([t(Cat,R)|Rest], S, LR) :-
	user:assert(constituent(S, Cat, LR, R)),
	assert_all_right(Rest, S, LR).




compute_penalties([]).
compute_penalties([S|Ss]) :-
	format(user_error, '~n~w', [S]),
	compute_penalties1(S),
	compute_penalties(Ss).

compute_penalties1(S) :-
	compute_length(S, 0, Max),
	compute_penalties1(S, Max).

compute_penalties1(S, Max) :-
	initialize(0, Max, S),
	write(user_error, ':'),
	cross_comp(2, Max, S).
initialize(N0, N, S) :-
	NR is N0 + 2,
    (
        NR > N
    ->
        true
    ;
        findall(L,(user:constituent(S,_,L,R),aux_crosses(L,R,N0,NR)), Cs),
        length(Cs, Cr),
        user:assert(crosses(S, N0, NR, Cr)),
        N1 is N0 + 1,
        initialize(N1, N, S)
    ).


assert_crosses(S, NL, NR) :-
        findall(L-R,(user:constituent(S,_,L,R),aux_crosses(L,R,NL,NR)), Cs),
        length(Cs, Cr),
        user:assert(crosses(S, NL, NR, Cr)).
	
% = aux_crosses(+L, +R, +NL, +NR)
% 
%
% L -- N0 -- R -- NR
%
% xxxxxxxxxxxx
%      ooooooooooooo

aux_crosses(L, R, N0, NR) :-
	L < N0,
	N0 < R,
	R < NR.
% N0 -- L -- NR -- R
%
% ooooooooooooo
%       xxxxxxxxxxxx

aux_crosses(L, R, N0, NR) :-
	N0 < L,
	L < NR,
	NR < R.

cross_comp(D0, Max, S) :-
    (
        D0 >= Max
    ->
        true
    ;
	write(user_error, '.'),
        D is D0 + 1,
        compute_crosses(D, 0, Max, S),
        cross_comp(D, Max, S)
    ).

compute_crosses(D, N0, N, S) :-
	NR is N0 + D,
    (
        NR > N
    ->
        true
    ;
        assert_crosses(S, N0, NR),
        N1 is N0 +1,
        compute_crosses(D, N1, N, S)
     ). 

crosses1(S, L, R, C) :-
	user:crosses(S, L, R, C),
	!.
crosses1(_, _, _, 0).

compute_length(S, 0, Max) :-
	user:findall(R, word(S,_,_,R), Rights),
	compute_lengths1(Rights, 0, Max).

compute_lengths1([], M, M).
compute_lengths1([R|Rs], M0, M) :-
    (
        R > M0
    ->
        M1 = R
    ;
        M1 = M0
    ),
    compute_lengths1(Rs, M1, M).

% TODO: needs to be updated to handle implicit zeros correctly!

% = update_crosses(+Sentence, +Left, +Right)
%
% update the asserted crosses/4 predicate for Sentence, adding a
% constituent from position Left to Right.

update_crosses(Sent, X, Y) :-
	update_crosses(Sent, X, Y, 1).

% = update_crosses(+Sentence, +Left, +Right, +Plus)
%
% update the asserted crosses/4 predicate for Sentence, adding a
% constituent from position Left to Right, increasing the value
% of all constituent crossing this positions by Plus (eg. we
% can "erase" a pair of brackets with Plus = -1 or make "strong"
% brackets with Plus >= 2.

update_crosses(Sent, X, Y, Plus) :-
%	compute_length(Sent, 0, Length),
	user:findall(crosses(Sent,V,W,Z), crosses(Sent,V,W,Z), List0),
	update_crosses(List0, X, Y, Plus, List),
	user:retractall(crosses(Sent,_,_,_)),
	user:assert_crosses(List),
	user:listing(crosses(Sent,_,_,_)).

update_crosses([], _, _, _, []).
update_crosses([crosses(Sent, V, W, Cross0)|Rest0], X, Y, Plus, [crosses(Sent, V, W, Cross)|Rest]) :-
   ((	
	/* X < V < Y < W */
	X < V,
	V < Y,
	Y < W
   ;
	/* V < X < Y < W */
	V < W,
	X < W,
	W < Y
   )
   ->	
        Cross is Cross0 + Plus
   ;
       Cross = Cross0
   ),
       update_crosses(Rest0, X, Y, Plus, Rest).

assert_crosses([]).
assert_crosses([crosses(A,B,C,D)|Cs]) :-
   (
	D =:= 0
   ->
	true
   ;
	user:assert(crosses(A,B,C,D))
   ),
	assert_crosses(Cs).


assert_list([]).
assert_list([C|Cs]) :-
	user:assert(C),
	assert_list(Cs).


delete_position(Sent, Pos) :-
	user:findall(crosses(Sent,V,W,Z), crosses(Sent,V,W,Z), List0),
	delete_position(List0, Pos, List),
	user:retractall(crosses(Sent,_,_,_)),
	assert_list(List),
	user:listing(crosses(Sent,_,_,_)).


verify_sentences :-
	verify_sentences(log).
verify_sentences(Log) :-
    ( exists_file(Log) -> delete_file(Log) ; true),
	tell(log),
	findall(Num, clause(sent(Num,_),_), List),
	verify_sentences_list(List),
	told,
	format(user_error, '~NDone!~nLog output to file ~w~n', [Log]).

verify_sentences_list([]).
verify_sentences_list([N|Ns]) :-
	verify_sentence(N),
	verify_sentences_list(Ns).

verify_sentence(Num) :-
	clause(sent(Num,_), prob_parse(List,_)),
	verify_sentence(List, 0, Num, '.').

verify_sentence([], N0, Num, Status) :-
   (	
	word(Num, Word2, N0, N)
     ->
	format('~N!!!~d: Untreated word ~w~n', [Num,Word2]),
	verify_sentence([], N, Num, '*')
   ;
	write(user_error, Status)
   ).
verify_sentence([si(Word1,_,_,_)|Rest], N0, Num, Status0) :-
   (	
	word(Num, Word2, N0, N)
     ->
   (
        Word1 = Word2
   ->
        Status = Status0
   ;
        atom_number(Word2, Word1)	 
   ->
	Status = Status0
   ;
        Status = '*',
	format('~N~d: Word mismatch ~w-~w~n', [Num,Word1,Word2])
   );	Status = '*',
	format('~N~d: Unmatched word ~w~n', [Num,Word1]),
	N is N0 + 1),
        verify_sentence(Rest, N, Num, Status).


verify_lemmas :-
	verify_lemmas(log).
verify_lemmas(Log) :-
    ( exists_file(Log) -> delete_file(Log) ; true),
	tell(log),
	findall(Num, clause(sent(Num,_),_), List),
	verify_lemmas_list(List),
	told,
	format(user_error, '~NDone!~nLog output to file ~w~n', [Log]).

verify_lemmas_list([]).
verify_lemmas_list([N|Ns]) :-
	verify_lemma(N),
	verify_lemmas_list(Ns).

verify_lemma(Num) :-
	clause(sent(Num,_), prob_parse(List,_)),
	verify_lemma(List, 0, Num, '.').

verify_lemma([], _, _, Status) :-
	write(user_error, Status).
verify_lemma([si(_,_,Lemma1,_)|Rest], N0, Num, Status0) :-
   (	
	lemma(Num, Lemma2, N0, N)
     ->
   (
        same_lemma(Lemma1, Lemma2)
   ->
        Status = Status0
   ;
        atom_number(Lemma2, Lemma1)	 
   ->
	Status = Status0
   ;
        Status = '*',
	format('~N~d: Lemma mismatch ~w-~w~n', [Num,Lemma1,Lemma2])
   );	Status = '.',
	format('~N~d: Unmatched lemma ~w~n', [Num,Lemma1]),
	N is N0 + 1),
        verify_lemma(Rest, N, Num, Status).

same_lemma(Lemma, Lemma) :-
	!.
same_lemma('l\'', le) :-
	!.
same_lemma(la, le) :-
	!.
same_lemma(les, le) :-
	!.
same_lemma(des, de) :-
	!.
same_lemma(du, de) :-
	!.
same_lemma(notre, mon) :-
	!.
same_lemma(leur, son) :-
	!.
same_lemma('France', 'FRANCE') :-
	!.


format1(X, Y) :-
	verbose(true),
	!,
	format(X, Y).
format1(_, _).
