% ==============================================
% =            output Prolog proof             =
% ==============================================

:- module(print_proof, [print_proof/3,print_proof/4,xml_proof/3]).

:- use_module(sem_utils, [get_fresh_variable_number/2]).

:- dynamic ignore_variables/1.

ignore_variables(false).

print_proof(Index, Proof, Stream, Bool) :-
	retractall(ignore_variables(_)),
	assert(ignore_variables(Bool)),
	print_proof(Index, Proof, Stream).

print_proof(Index, Proof, Stream) :-
	get_fresh_variable_number(Proof, Max),
	numbervars(Proof, Max, _, [singletons(true)]),
	print_title(Proof, Index, Stream),
	format(Stream, 'proof(~w, ', [Index]),
	print_proof1(Proof, 0, Stream),
	format(Stream, ').~2n', []).

print_proof1(rule(RName,Pros,FormulaSem0,Ds), T0, Stream) :-
	update_sem(FormulaSem0, FormulaSem),
    (
	Ds = []
    ->
        format(Stream, 'rule(~W, ~W, ~@, [])', [RName,[numbervars(true),quoted(true)],Pros,[quoted(true)],print_formula_sem(FormulaSem,Stream)])
    ;
        Ds = [D|Ds0], 
        T is T0 + 3,
        format(Stream, 'rule(~W, ~W, ~@, [~n', [RName,[numbervars(true),quoted(true)],Pros,[quoted(true)],print_formula_sem(FormulaSem,Stream)]),
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
print_pros(t(List), Stream) :-
	print_pros_list(List, Stream).

print_pros_list([], _).
print_pros_list([P|Ps], Stream) :-
	print_pros(P, Stream),
	print_pros_list(Ps, Stream).

update_sem(lit(let)-_, lit(let)-true) :-
	/* special case for let to avoid singleton variable warnings */
	!.
update_sem(FS, FS).
%update_sem(Formula-Sem, Formula-Sem) :-
%	get_max_variable_number(Sem, Max),
%	numbervars(Sem, Max, _).	

print_formula_sem(tuple(Formula,Sem,Stacks), Stream) :-
	print_formula_sem_stacks(Formula, Sem, Stacks, Stream).
print_formula_sem(Formula-Sem, Stream) :-
	print_formula_sem(Formula, Sem, Stream).

print_formula_sem_stacks(Formula, Sem, Stacks, Stream) :-
	print_formula(Formula, Stream),
	format(Stream, '-~W', [Sem, [quoted(true)]]),
	print_stacks(Stacks, Stream).

print_stacks(stacks(S1,S2,S3,S4), Stream) :-
	print_stack(S1, 1, Stream),
	print_stack(S2, 2, Stream),
	print_stack(S3, 3, Stream),
	print_stack(S4, 4, Stream).

print_stack(Stack, SN, Stream) :-
	write(Stream, '-['),
	print_stack0(Stack, SN, Stream).

print_stack0([], _, Stream) :-
	write(Stream, ']').
print_stack0([S|Ss], SN, Stream) :-
	print_stack1(Ss, S, SN, Stream).

print_stack1([], S, SN, Stream) :-
	format(Stream, '~@]', [print_stack_item1(S,SN,Stream)]).
print_stack1([S|Ss], S0, SN, Stream) :-
	format(Stream, '~@,', [print_stack_item1(S0,SN,Stream)]),
	print_stack1(Ss, S, SN, Stream).

print_stack_item1(_-T, SN, Stream) :-
	print_stack_item1(T, SN, Stream).
print_stack_item1(t(I1,I2,I3,I4), SN, Stream) :-
	print_stack_item2(SN, I1, I2, I3, I4, Stream).
print_stack_item1(t(I1,I2,I3,I4,_), SN, Stream) :-
	print_stack_item2(SN, I1, I2, I3, I4, Stream).


print_stack_item2(1, _, _, F, _, Stream) :-
	print_formula_nv(F, Stream).
print_stack_item2(2, _, _, F, _, Stream) :-
	print_formula_nv(F, Stream).
print_stack_item2(3, F, _, _, _, Stream) :-
	print_formula_nv(F, Stream).
print_stack_item2(4, F, _, _, _, Stream) :-
	print_formula_nv(F, Stream).


print_formula_nv(F0, Stream) :-
	simplify_formula(F0, F),
	print_formula(F, Stream).


simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(A0)), lit(s(A))) :-
	!,
	simplify_argument(A0, A).
simplify_formula(lit(pp(A0)), lit(pp(A))) :-
	!,
	simplify_argument(A0, A).
simplify_formula(lit(np(_,_,_)), lit(np)) :-
	!.
simplify_formula(lit(X), lit(X)) :-
	!.
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(_,A0), dia(1,A)) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), box(1,A)) :-
	simplify_formula(A0, A).

simplify_argument('$VAR'(_), '$VAR'(0)) :-
	!.
simplify_argument(A, A).

print_formula_sem(Formula, Sem, Stream) :-
	print_formula(Formula, Stream),
	format(Stream, '-~W', [Sem, [quoted(true)]]).

print_formula(lit(A), Stream) :-
	!,
	format(Stream, 'lit(~@)', [print_formula1(A,Stream)]).
print_formula(dr(I,A,B), Stream) :-
	!,
	format(Stream, 'dr(~w,~@,~@)', [I,print_formula(A,Stream),print_formula(B,Stream)]).
print_formula(dl(I,A,B), Stream) :-
	!,
	format(Stream, 'dl(~w,~@,~@)', [I,print_formula(A,Stream),print_formula(B,Stream)]).
print_formula(p(I,A,B), Stream) :-
	!,
	format(Stream, 'p(~w,~@,~@)', [I,print_formula(A,Stream),print_formula(B,Stream)]).
print_formula(dia(I,A), Stream) :-
	!,
	format(Stream, 'dia(~w,~@)', [I,print_formula(A,Stream)]).
print_formula(box(I,A), Stream) :-
	!,
	format(Stream, 'box(~w,~@)', [I,print_formula(A,Stream)]).
print_formula(Term, Stream) :-
	functor(Term, F, A),
	format(user_error, '{Warning: unknown term ~w with functor ~w/~w}', [Term,F,A]),
	write(Stream, Term).

print_formula1(np(A,B,C), Stream) :-
	format(Stream, 'np(~@,~@,~@)', [print_case_item(A,Stream),print_case_item(B,Stream),print_case_item(C,Stream)]).
print_formula1(s(A), Stream) :-
	nonvar(A),
	A = inf(B),
	!,
	format(Stream, 's(inf(~@))', [print_item(B,Stream)]).
print_formula1(s(A), Stream) :-
	!,
	format(Stream, 's(~@)', [print_item(A,Stream)]).
print_formula1(pp(A), Stream) :-
	!,
	format(Stream, 'pp(~@)', [print_item(A,Stream)]).
print_formula1(n, Stream) :-
	!,
	write(Stream, n).
print_formula1(np, Stream) :-
	!,
	write(Stream, np).
print_formula1(cl_r, Stream) :-
	!,
	write(Stream, cl_r).
print_formula1(cl_y, Stream) :-
	!,
	write(Stream, cl_y).
print_formula1(txt, Stream) :-
	!,
	write(Stream, txt).
print_formula1(let, Stream) :-
	!,
	write(Stream, let).
print_formula1(Strange, Stream) :-
	format(user_error, '{Warning: unknown atom: ~w}~n', [Strange]),
	write(Stream, Strange).

print_item(V, Stream) :-
	var(V),
	!,
	(ignore_variables(true) -> write(Stream, '_') ; print(Stream, V)).
print_item('$VAR'(N), Stream) :-
	integer(N),
	!,
	(ignore_variables(true) -> write(Stream, '_') ; print(Stream, '$VAR'(N))).
print_item('$VAR'(_), Stream) :-
	!,
	write(Stream, '_').
print_item(Atom, Stream) :-
	format(Stream, '~W', [Atom,[quoted(true)]]).


print_case_item('$VAR'(_), Stream) :-
	!,
	write(Stream, '_').
print_case_item(Atom, Stream) :-
	format(Stream, '~W', [Atom,[quoted(true)]]).


% ==============================================
% =              output XML proof              =
% ==============================================

xml_proof(Index, Proof, Stream) :-
	format(Stream, '<?xml version="1.0" encoding="UTF-8"?>~n', []),
	numbervars(Proof, 0, _),
	Proof = rule(_, Pros, _, _),
	format(Stream, '<!-- ~w. ', [Index]),
	print_pros_xml(Pros, Stream),
	format(Stream, ' -->~n', []),
	xml_proof1(Proof, 0, Stream).

xml_proof1(rule(A,B0,C0,Ds), T0, Stream) :-
        reduce_pros(B0, B),
        xml_formula_sem(C0, Form, Sem),
	nl(Stream),
	tab(Stream, T0),
	format(Stream, '<rule name="~w" pros="~q" formula="~w" sem="~w">', [A,B,Form, Sem]),
	T is T0 + 3,
	xml_proof_list(Ds, T, Stream),
	nl(Stream),
	tab(Stream, T0),
	format(Stream, '</rule>', []).


xml_formula_sem(Form-Sem, Form, Sem) :-
        !.
xml_formula_sem(Form, Form, '').

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
        format(Stream, ' ', []),
	print_pros_xml(R, Stream).
print_pros_xml(p(_,L,R), Stream) :-
	print_pros_xml(L, Stream),
        format(Stream, ' ', []),
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
