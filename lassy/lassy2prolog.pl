% Convert the Lassy corpus to Prolog files.

% node(Category, Index, Relation, Left, Right, DaughterList)
% leaf(POStag, Index, Relation, Left, Right, Word, Lemma, FeatureList)
% epsilon(AntecedentIndex, Index, Relation, Left, Right)

% Category is the syntactic category of the node
% Index is an integer identifying the node uniquely
% Relation is the syntactive function -- su(bject) h(ea)d, etc. -- of the node in its parent category
% Left is the integer corresponding to the left position of the constituent
% Right is the integer corresponding to the right position of the constituent
% DaughterList is a list of nodes
% FeatureList is a list of features (each of the form Feature-Value, eg. getal-ev)
% AntecedentIndex is the node index of the antecedent of the empty node

treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.10.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.10.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.12.s.3.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.12.s.4.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.13.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.13.s.3.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.3.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.5.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.6.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.8.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.16.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.17.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.17.s.3.xml'). % "hoe" relative
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.18.s.3.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.19.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.20.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.21.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.22.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.23.s.5.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.25.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.26.s.1.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.26.s.2.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-6930/wiki-6930.p.2.s.14.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-6930/wiki-6930.p.2.s.6.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-6930/wiki-6930.p.2.s.7.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-6930/wiki-6930.p.2.s.8.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-7543/wiki-7543.p.6.s.3.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-832/wiki-832.p.10.s.5.xml').
treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/wiki-832/wiki-832.p.14.s.8.xml').
% complex coordination, includes interpunction
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.1.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.10.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.10.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.11.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.12.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.13.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.15.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.15.s.5.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.16.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.16.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.17.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.18.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.18.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.18.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.19.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.19.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.20.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.20.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.20.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.21.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.22.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.22.s.3.xml').
% word order
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.11.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.11.s.3.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.11.s.5.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.12.s.2.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.14.s.7.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.15.s.1.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.15.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.18.s.5.xml').
% extraction
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.11.s.5.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.15.s.2.xml'). % waar ... mee
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.16.s.4.xml').
%treebank_file('/Users/moot/Corpus/lassy-r19749/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.16.s.5.xml').

verbose(yes).
:- dynamic index/2.

l2pl :-
	retractall(tree(_,_,_)),
	retractall('$SENT'(_)),
	assert('$SENT'(1)),
	treebank_file(File),
	l2pl(File, [[_-Tree,Sent]]),
	'$SENT'(N0),
	write_sent(N0, Sent),
	assert(tree(N0, Tree, Sent)),
%	simplify_tree(Tree, STree),
%	yield(Tree, List, []),
%	format('   ~q~n', [STree]),
%	format('   ~w~n', [List]),
	N is N0 + 1,
	retractall('$SENT'(_)),
	assert('$SENT'(N)),
	fail.
l2pl.


l2pl(Pattern) :-
	retractall(tree(_,_,_)),
	retractall('$SENT'(_)),
	assert('$SENT'(1)),
	expand_file_name(Pattern, Files),
	member(File, Files),
	l2pl(File, [[_-Tree,Sent]]),
	'$SENT'(N0),
	write_sent(N0, Sent),
	assert(tree(N0, Tree, Sent)),
%	simplify_tree(Tree, STree),
%	yield(Tree, List, []),
%	format('   ~q~n', [STree]),
%	format('   ~w~n', [List]),
	N is N0 + 1,
	retractall('$SENT'(_)),
	assert('$SENT'(N)),
	fail.
l2pl(_).

l2pl(File, Tree) :-
	retractall(index/2),
	load_xml(File, XML, [space(remove)]),
	retractall(index(_,_)),
	assert_indices_list(XML),
	translate_xml(XML, Tree).

write_sent(N, sentence(X)) :-
	format('~w. ~w~n', [N,X]).


assert_indices(element(sentence, _, _)) :-
	!.
assert_indices(element(_Tag, Atts0, Dts)) :-
   (
	select(index=Idx0, Atts0, Atts1),
        select(id=Id0, Atts1, Atts),
	( Dts \= [] ; member(word=_,Atts))
   ->
        make_number(Id0, Id),
        make_number(Idx0, Idx),
        assert(index(Idx, Id))
   ;
        true
   ),
	assert_indices_list(Dts).

assert_indices_list([]).
assert_indices_list([X|Xs]) :-
	assert_indices(X),
	assert_indices_list(Xs).

make_number(X0, X) :-
	integer(X0),
	!,
	X = X0.
make_number(A, X) :-
	atom(A),
	!,
	atom_number(A, X).

translate_xml([], []).
translate_xml([X|Xs], [T|Ts]) :-
	translate_element(X, T),
	translate_xml(Xs, Ts).

translate_element(element(Tag, Atts, Dts), Term) :-
	translate_element(Tag, Atts, Dts, Term).

translate_element(alpino_ds, _, Dts, Term) :-
	translate_xml(Dts, Term).
translate_element(sentence, _, [Sent], sentence(Sent)).
translate_element(node, Atts, Dts, Term) :-
	translate_atts(Atts, Term, SortedDtsList),
	translate_xml(Dts, DtsList),
	keysort(DtsList, KeySortedDtsList),
	strip_keys(KeySortedDtsList, SortedDtsList).


translate_atts(Atts0, R-Term, Dts) :-
%	 retractall(current_index(_)),
     (
         select(cat=Cat, Atts0, Atts)
     ->
         functor(Term, node, 6),
         arg(1, Term, Cat),
         arg(5, Term, R),
         arg(6, Term, Dts),
         translate_other_cat(Atts, Term)
     ;
         select(pt=POS, Atts0, Atts1)
     ->
         functor(Term, leaf, 8),
         arg(1, Term, POS),
         arg(5, Term, R),
         arg(8, Term, OAts),
         translate_other_pos(Atts1, Term, [], OAts),
         Dts = []
     ;
         select(index=Idx0, Atts0, Atts)
     ->
         functor(Term, epsilon, 5),
         arg(5, Term, R),
         atom_number(Idx0, Idx),
         index(Idx, Index),
         arg(1, Term, Index),
         translate_other_cat(Atts, Term)
     ;
         format('~NWarning: don\'t know how to treat attributes~n~p~n', [Atts0]),
         Term = nil
      
     ).

translate_other_cat([], _).
translate_other_cat([A|As], Term) :-
	translate_other_cat1(A, Term),
	translate_other_cat(As, Term).

translate_other_cat1(A=B, Term) :-
	translate_other_cat2(A, B, Term).

translate_other_cat2(id, _, _) :-
 	!.
% 	atom_number(IdA, Id),
% 	retractall(current_id(_)),
% 	assert(current_id(Id)),
%    (
%         current_index(Index)
%    ->
%         retractall(index(_,_)),
%         assert(index(Index, Id))
%    ;
%         true
%    ),
% 	arg(2, Term, Id).
translate_other_cat2(index, _, _) :-
 	!.
% 	atom_number(IdA, Id),
%    (
%         current_id(ID)
%    ->
%         retractall(index(_,_)),
%         assert(index(Id, ID))
%    ;
% 	true
%    ),  
%	assert(current_index(Id)).
translate_other_cat2(rel, Rel, Term) :-
	!,
	arg(3, Term, Rel).
translate_other_cat2(begin, LA, Term) :-
	!,
	atom_number(LA, L),
	arg(4, Term, L).
translate_other_cat2(end, RA, Term) :-
	!,
	atom_number(RA, R),
	arg(5, Term, R).
translate_other_cat2(Ignored, Value, _) :-
	verbose_output('~NIgnored: ~w=~w~n', [Ignored, Value]).

translate_other_pos([], _, OAs, OAs).
translate_other_pos([A|As], Term, OAs0, OAs) :-
	translate_other_pos1(A, Term, OAs0, OAs1),
	translate_other_pos(As, Term, OAs1, OAs).

translate_other_pos1(A=B, Term, OAs0, OAs) :-
	translate_other_pos2(A, B, OAs0, OAs, Term).

translate_other_pos2(id, IdA, As, As, Term) :-
	!,
	atom_number(IdA, Id),
	arg(2, Term, Id).
translate_other_pos2(rel, Rel, As, As, Term) :-
	!,
	arg(3, Term, Rel).
translate_other_pos2(begin, LA, As, As, Term) :-
	!,
	atom_number(LA, L),
	arg(4, Term, L).
translate_other_pos2(end, RA, As, As, Term) :-
	!,
	atom_number(RA, R),
	arg(5, Term, R).
translate_other_pos2(word, Word, As, As, Term) :-
	!,
	arg(6, Term, Word).
translate_other_pos2(lemma, Lemma, As, As, Term) :-
	!,
	arg(7, Term, Lemma).
translate_other_pos2(index, _, As, As, _Term) :-
	!.
%	atom_number(Index0, Index),
%	arg(2, Term, Id),
%	index(Index, Id).
% = the following attributes are ignored
translate_other_pos2(postag, _, As, As, _) :-
	!.
translate_other_pos2(root, _, As, As, _) :-
	!.
translate_other_pos2(Rel, Val, As, [Rel-Val|As], _Term).


strip_keys([], []).
strip_keys([_-V|KVs], [V|Vs]) :-
	strip_keys(KVs, Vs).

verbose_output(Str, Args) :-
    (
        verbose(yes)
    ->
        format(Str, Args)
    ;
        true
    ).


pretty_print_term(Term) :-
	pretty_print_term(Term, 0).
pretty_print_term(epsilon(A,B,C,D,E), _) :-
	print(epsilon(A,B,C,D,E)).
pretty_print_term(leaf(A,B,C,D,E,F,G,H), _) :-
	print(leaf(A,B,C,D,E,F,G,H)).
pretty_print_term(node(A,B,C,D,E,F), Tab0) :-
	format('node(~p,~p,~p,~p,~p,', [A,B,C,D,E]),
	Tab is Tab0 + 3,
	pretty_print_term(F, Tab),
	print(')').
pretty_print_term(A-B, Tab0) :-
	!,
	print(A),
	print(-),
	Tab is Tab0 + 3,
	pretty_print_term(B, Tab).
pretty_print_term([A|Bs], Tab0) :-
	!,
	print('['),
	Tab is Tab0 + 3,
	pretty_print_args(Bs, A, Tab),
	nl,
	tab(Tab0),
	print(']').
pretty_print_term(Term, Tab0) :-
	Term =.. [Fun|Args],
	print(Fun),
   (
        Args = [A|As]
   ->
	print('('),
	Tab is Tab0 + 4,
	pretty_print_args(As, A, Tab),
        nl,
        tab(Tab0),
	print(')')
   ;
        true
   ).

pretty_print_args([], A, Tab) :-
	nl,
	tab(Tab),
	pretty_print_term(A, Tab).
pretty_print_args([A|As], A0, Tab) :-
	nl,
	tab(Tab),
	pretty_print_term(A0, Tab),
	print(','),
	pretty_print_args(As, A, Tab).
