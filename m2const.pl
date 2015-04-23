:- dynamic constituent/4.


 xml_files('flmf7aa1ep.cat.xml').
% xml_files('flmf7aa2ep.cat.xml').
% xml_files('flmf7ab2ep.xml').
% xml_files('flmf7ae1ep.cat.xml').
% xml_files('flmf7af2ep.cat.xml').
% xml_files('flmf7ag1exp.cat.xml').
% xml_files('flmf7ag2ep.cat.xml').
% xml_files('flmf7ah1ep.aa.xml').
% xml_files('flmf7ah2ep.aa.xml').
% xml_files('flmf7ai1exp.cat.xml').
% xml_files('flmf7ai2ep.aa.cat.xml').
% xml_files('flmf7aj1ep.indent.xml').
% xml_files('flmf7ak1ep.indent.xml').
% xml_files('flmf7ak2ep.xd.cat.xml').
% xml_files('flmf7al1ep.cat.xml').
% xml_files('flmf7am1ep.xd.cat.xml').
% xml_files('flmf7am2ep.xd.cat.xml').
% xml_files('flmf7an1ep.xml').
% xml_files('flmf7an2co.af.cat.xml').
% xml_files('flmf7ao1ep.xml').
% xml_files('flmf7ao1ep.xml').
% xml_files('flmf7ap1ep.af.cat.xml').
% xml_files('flmf7aq2ep.xd.cat.xml').
% xml_files('flmf7as2ep.af.cat.xml').
% xml_files('flmf7atep.cat.xml').

%xml_files('annodis.er.xml').

start :-
	findall(F, xml_files(F), Files),
	start(Files, 0, _).

start([], N, N).
start([F|Fs], N0, N) :-
	load_structure(F, L0, [dialect(xml), space(default)]),
	delete_all_spaces(L0, L),
	xml_to_const(L, N0, N1, 0, _),
	start(Fs, N1, N).

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
	N1 is N0 + 1,
	format('word(~w, ~k, ~w, ~w).~n', [S, W, N0, N1]),
	handle_word_list(Ws, S, N1, N).

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
    (
        member(nb=Num,As)
    ->
        get_number(Num, S0, S1)
    ;
	S1 is S0 +1
    ),
	xml_to_const(Cs, S1, S, 0, M).

element_to_const(w, _As, Cs, S, S, M0, M) :-
	!,
	collect_words(Cs, Ws, []),
    (   Ws \= []
    ->
	smart_concat_atoms(Ws, Word),
	atomic_list_concat(L, '_', Word),
	handle_word_list(L, S, M0, M),
    (
	M > M0 + 1
    ->
        format('constituent(~w, w, ~w, ~w).~n', [S, M0, M])
    ;
        true
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
        format('constituent(~w, ~k, ~w, ~w).~n', [S0, Cat, N0, N])
    ;
        true
    ).

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
	retractall(crosses(_,_,_,_)),
	compute_penalties(List),
	retractall(crosses(_,_,_,0)).


compute_all :-
	retractall(crosses(_,_,_,_)),
	setof(X, A^B^C^constituent(X,A,B,C), Sentences),
	add_constituents(Sentences),
	compute_penalties(Sentences).

compute_penalties :-
	retractall(crosses(_,_,_,_)),
	setof(X, A^B^C^constituent(X,A,B,C), Sentences),
	compute_penalties(Sentences).

add_constituents :-
	setof(X, A^B^C^constituent(X,A,B,C), Sentences),
	add_constituents(Sentences),

add_constituents([]).
add_constituents([S|Ss]) :-
	add_constituents1(S),
	add_constituents(Ss).

add_constituents1(S) :-
	findall(t(Cat,L,R), constituent(S, Cat, L, R), Triples),
	add_constituents2(Triples, S).

add_constituents2([], _).
add_constituents2([t(Cat, L, R)|Rest], S) :-
	add_constituents2(S, Cat, L, R),
	add_constituents2(Rest, S).

% check if there is an interpunction symbol to the left of the current constituent,
% if so add a new constituent which includes this interpunction symbol (and do so
% recursively, adding multiple interpunction symbols if necessary).

add_constituents2(S, Cat, L, R) :-
   (
	word(S, W, L0, L),
        is_interpunction(W)
   ->
        format('constituent(~w, ~w, ~w, ~w).~n', [S,Cat,L0,R]),
        assert(constituent(S, Cat, L0, R)),
        add_constituents2(S, Cat, L0, R)
   ;
        true
   ).

is_interpunction('...').
is_interpunction(',').
is_interpunction(':').
is_interpunction(';').
is_interpunction('(').
is_interpunction(')').
is_interpunction('"').
is_interpunction('[').
is_interpunction(']').

compute_penalties([]).
compute_penalties([S|Ss]) :-
	format('~n~w', [S]),
	compute_penalties1(S),
	compute_penalties(Ss).

compute_penalties1(S) :-
	compute_length(S, 0, Max),
	compute_penalties1(S, Max).

compute_penalties1(S, Max) :-
	initialize(0, Max, S),
	write(':'),
	cross_comp(2, Max, S).

initialize(N0, N, S) :-
	NR is N0 + 2,
    (
        NR > N
    ->
        true
    ;
        findall(L,(constituent(S,_,L,R),aux_crosses(L,R,N0,NR)), Cs),
        length(Cs, Cr),
        assert(crosses(S, N0, NR, Cr)),
        N1 is N0 +1,
        initialize(N1, N, S)
    ).

% L -- N0 -- R -- NR

aux_crosses(L, R, N0, NR) :-
	L < N0,
	N0 < R,
	R < NR.
% N0 -- L -- NR -- R
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
	write('.'),
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
        NR0 is NR - 1,
        crosses(S, N0, NR0, Cr0),
        /* crossed at length D-1, but no longer crosses at length D */
        findall(., (constituent(S,_,L,NR),L > N0), List0),
        /* didn't cross at length D-1, but crosses at length D */
        findall(., constituent(S,_,NR0,_), List1),
        length(List0, C0),
        length(List1, C1),
        Cr is (Cr0 + C1) - C0,
        assert(crosses(S, N0, NR, Cr)),
        N1 is N0 +1,
        compute_crosses(D, N1, N, S)
     ). 

crosses1(S, L, R, C) :-
	crosses(S, L, R, C),
	!.
crosses1(_, _, _, 0).

compute_length(S, 0, Max) :-
	findall(R, constituent(S,_,_,R), Rights),
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


update_crosses(Sent, X, Y, Plus) :-
	crosses(Sent, V, W, Cross0),
   (	
	/* X < V < Y < W */
	X < V,
	V < Y,
	Y < W
   ;
	/* V < X < Y < W */
	V < W,
	X < W,
	W < Y
   ),
        Cross is Cross0 + Plus,
        portray_clause((crosses(Sent, V, W, Cross) :- true)),
        fail.
   
update_crosses(_, _, _, _).