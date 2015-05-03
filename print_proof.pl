% ==============================================
% =            output Prolog proof             =
% ==============================================

:- module(print_proof, [print_proof/3,xml_proof/3]).

print_proof(Index, Proof, Stream) :-
	numbervars(Proof, 0, _),
	print_title(Proof, Index, Stream),
	format(Stream, 'proof(~w, ', [Index]),
	print_proof1(Proof, 0, Stream),
	format(Stream, ').~2n', []).

print_proof1(rule(RName,Pros0,Sem,Ds), T0, Stream) :-
%	reduce_pros(Pros0, Pros),
	Pros = Pros0,
    (
	Ds = []
    ->
        write_term(Stream, rule(RName, Pros, Sem, Ds), [numbervars(true),quoted(true)])
    ;
        Ds = [D|Ds0], 
        T is T0 + 3,
        format(Stream, 'rule(~W, ~W ,~W , [~n', [RName,[numbervars(true),quoted(true)],Pros,[numbervars(true),quoted(true)],Sem,[numbervars(true),quoted(true)]]),
        tab(Stream, T),
        print_proof_list(Ds0, D, T, Stream),
        format(Stream, '])', [])
    ).

print_proof_list([], D, T, Stream) :-
	print_proof1(D, T, Stream),
	nl(Stream),
	tab(Stream, T).
print_proof_list([D|Ds], D0, T, Stream) :-
	print_proof1(D0, T, Stream),
	write(Stream, ','),
	nl(Stream),
	tab(Stream, T),
	print_proof_list(Ds, D, T, Stream).

print_title(rule(_, Pros, _, _), Sent, Stream) :-
	format(Stream, '% ~w. ', [Sent]),
	print_pros(Pros, Stream),
	nl(Stream),
	nl(Stream).
print_pros(Atom, Stream) :-
	atomic(Atom),
	!,
	format(Stream, '~w ', [Atom]).
print_pros(L-R, Stream) :-
	format(Stream, '~w-~w', [L,R]).
print_pros(hyp(_,_,Pros), Stream) :-
	format(Stream, '~w ', [Pros]).
print_pros(leaf(_,_,Pros,_,_), Stream) :-
	format(Stream, '~w ', [Pros]).
print_pros(p(_,_,_,L,R), Stream) :-
	print_pros(L, Stream),
	print_pros(R, Stream).
print_pros(p(_,L,R), Stream) :-
	print_pros(L, Stream),
	print_pros(R, Stream).

reduce_pros(Atom, Atom) :-
	atomic(Atom),
	!.
reduce_pros('$VAR'(N), Atom) :-
	atomic_concat(p, N, Atom).
reduce_pros(L-R, L-R).
reduce_pros(p(I,L,R), p(I,L,R)).
reduce_pros(hyp(_,_,Pros), Pros).
reduce_pros(leaf(_,_,Pros,_,_), Pros).
reduce_pros(p(I,_,_,L0,R0), p(I,L,R)) :-
	reduce_pros(L0, L),
	reduce_pros(R0, R).



% ==============================================
% =              output XML proof              =
% ==============================================

xml_proof(Index, Proof, Stream) :-
	format(Stream, '<?xml version="1.0" encoding="UTF-8"?>~n', []),
	numbervars(Proof, 0, _),
	Proof = rule(_, Pros, _, _),
	format(Stream, '<!-- ~w. ', [Index]),
	print_pros_xml(Pros, Stream),
	format(Stream, '>~n', []),
	xml_proof1(Proof, 0, Stream).

xml_proof1(rule(A,B0,C,Ds), T0, Stream) :-
	reduce_pros(B0, B),
	nl(Stream),
	tab(Stream, T0),
	format(Stream, '<rule name="~w" pros="~q" formula="~w">', [A,B,C]),
	T is T0 + 3,
	xml_proof_list(Ds, T, Stream),
	nl(Stream),
	tab(Stream, T0),
	format(Stream, '</rule>', []).

xml_proof_list([], _, _).
xml_proof_list([P|Ps], T, Stream) :-
	xml_proof1(P, T, Stream),
%	nl(Stream),
%	tab(Stream, T),
	xml_proof_list(Ps, T, Stream).

print_pros_xml(Atom, Stream) :-
	atomic(Atom),
	!,
	print_pros_xml1(Atom, Stream).
print_pros_xml('$VAR'(N), Stream) :-
	format(Stream, 'p~w', [N]).
print_pros_xml(L-R, Stream) :-
	format(Stream, '~w-~w', [L,R]).
print_pros_xml(leaf(_,_,Pros,_,_), Stream) :-
	print_pros_xml1(Pros, Stream).
print_pros_xml(p(_,_,_,L,R), Stream) :-
	print_pros_xml(L, Stream),
	print_pros_xml(R, Stream).
print_pros_xml(p(_,L,R), Stream) :-
	print_pros_xml(L, Stream),
	print_pros_xml(R, Stream).

print_pros_xml1(Pros0, Stream) :-
	name(Pros0, Codes0),
	xml_codes(Codes0, Codes),
	name(Pros, Codes),
	write(Stream, Pros).

xml_codes([], []).
xml_codes([C|Cs], Ds0) :-
     (
         C = 38
     ->
         /* &amp; */
         Ds0 = [38,97,109,112,59|Ds]
     ;
         C = 39
     ->
         /* &apos; */
         Ds0 = [38,97,112,111,115,59|Ds]
     ;
         C = 34
     ->
         /* &quot; */
         Ds0 = [38,113,117,111,116,59|Ds]
     ;
         /* default */
         Ds0 = [C|Ds]
     ),
         xml_codes(Cs, Ds).
