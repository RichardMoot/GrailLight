:- op(400, xfy, \).

max_degree(Max) :-
	findall(F, formula(F, _), List),
	max_degree(List, 0, Max).

max_degree([], Max, Max).
max_degree([F|Fs], Max0, Max) :-
	degree(F, Deg),
	Max1 is max(Max0,Deg),
	max_degree(Fs, Max1, Max).

degree(dr(A,B), D) :-
	!,
	degree(A, D0),
	degree(B, D1),
	D is D0 + D1.
degree(dl(A,B), D) :-
	!,
	degree(A, D0),
	degree(B, D1),
	D is D0 + D1.
degree(_, 1).

show_degree(Min) :-
	findall(F-N, formula(F, N), List),
	show_degree(List, Min, 0).

show_degree([], _, Total) :-
	format('===~nTotal: ~D~n', [Total]).
show_degree([F-N|Fs], Min, Total0) :-
	degree(F, D),
	(D >= Min -> format('~p-~D [~D]~n', [F,N,D]),Total=Total0+N ; Total=Total0),
	show_degree(Fs, Min, Total).

max_right_aas(Max) :-
	findall(F, formula(F,_), List),
	max_right_aas(List, 0, Max).

max_right_aas([], Max, Max).
max_right_aas([F|Fs], Max0, Max) :-
	right_aas(F, RA),
	Max1 is max(Max0, RA),
	max_right_aas(Fs, Max1, Max).

right_aas(dr(A,B), Max) :-
	!,
	right_arguments(B, MaxB),
	right_aas(A, Max0),
	Max is max(Max0, MaxB).
right_aas(dl(B,A), Max) :-
	!,
	right_arguments(B, MaxB),
	right_aas(A, Max0),
	Max is max(Max0, MaxB).
right_aas(_, 0).


max_left_aas(Max) :-
	findall(F, formula(F,_), List),
	max_left_aas(List, 0, Max).

max_left_aas([], Max, Max).
max_left_aas([F|Fs], Max0, Max) :-
	left_aas(F, RA),
	Max1 is max(Max0, RA),
	max_left_aas(Fs, Max1, Max).

left_aas(dr(A,B), Max) :-
	!,
	left_arguments(B, MaxB),
	left_aas(A, Max0),
	Max is max(Max0, MaxB).
left_aas(dl(B,A), Max) :-
	!,
	left_arguments(B, MaxB),
	left_aas(A, Max0),
	Max is max(Max0, MaxB).
left_aas(_, 0).

find_left_aas(LA) :-
	formula(F, N),
	left_aas(F, LA0),
	LA0 >= LA,
	format('~w-~@-~w [~w]~n', [F, print_ccgform(F), N, LA0]),
	fail.
find_left_aas(_).

find_right_aas(RA) :-
	formula(F, N),
	right_aas(F, RA0),
	RA0 >= RA,
	format('~w-~w [~w]~n', [F, N, RA0]),
	fail.
find_right_aas(_).


max_right_arguments(Max) :-
	findall(F, formula(F,_), List),
	max_right_arguments(List, 0, Max).


max_right_arguments([], Max, Max).
max_right_arguments([F|Fs], Max0, Max) :-
	right_arguments(F, RA),
	Max1 is max(Max0, RA),
	max_right_arguments(Fs, Max1, Max).


right_arguments(dr(A,_), Max) :-
	!,
	right_arguments(A, Max0),
	Max is Max0 + 1.
right_arguments(dl(_,A), Max) :-
	!,
	right_arguments(A, Max).
right_arguments(_, 0).



find_left_arguments(LA) :-
	formula(F, N),
	left_arguments(F, LA0),
	LA0 >= LA,
	format('~w-~@-~w [~w]~n', [F, print_ccgform(F), N, LA0]),
	fail.
find_left_arguments(_).

find_right_arguments(RA) :-
	formula(F, N),
	right_arguments(F, RA0),
	RA0 >= RA,
	format('~w-~w [~w]~n', [F, N, RA0]),
	fail.
find_right_arguments(_).


max_left_arguments(Max) :-
	findall(F, formula(F,_), List),
	max_left_arguments(List, 0, Max).


max_left_arguments([], Max, Max).
max_left_arguments([F|Fs], Max0, Max) :-
	left_arguments(F, RA),
	Max1 is max(Max0, RA),
	max_left_arguments(Fs, Max1, Max).

left_arguments(dl(_,A), Max) :-
	!,
	left_arguments(A, Max0),
	Max is Max0 + 1.
left_arguments(dr(A,_), Max) :-
	!,
	left_arguments(A, Max).
left_arguments(_, 0).


print_ccgform(dr(A,B)) :-
	!,
	format('(~@/~@)', [print_ccgform(A),print_ccgform(B)]).
print_ccgform(dl(B,A)) :-
	!,
	format('(~@\\~@)', [print_ccgform(A),print_ccgform(B)]).
print_ccgform(A) :-
	print_ccgatom(A).

print_ccgatom(Term0) :-
	functor(Term0, F0, A),
	atom_to_upper(F0, F),
	functor(Term, F, A),
	copy_arg(A, Term0, Term),
	format('~w', [Term]).

%atom_to_upper(A, A).

atom_to_upper(Atom0, Atom) :-
 	atom_chars(Atom0, Chars0),
 	list_to_upper(Chars0, Chars),
 	atom_chars(Atom, Chars).

list_to_upper([], []).
list_to_upper([A0|As0], [A|As]) :-
	upcase_atom(A0, A),
	list_to_upper(As0, As).

copy_arg(0, _, _).
copy_arg(1, Term0, Term) :-
	arg(1, Term0, Arg),
	arg(1, Term, Arg).


%

undergeneration :-
	setof(F-N, A^(formula(F,N),argument(F,A),undergenerates(A)), Formulas),
	portray_formulas(Formulas, 0).

portray_formulas([], Total) :-
	format('===~nTotal: ~D~n', [Total]).
portray_formulas([F-N|Fs], T0) :-
	format('~p [~w]~n', [F,N]),
	T is T0 + N, 	
	portray_formulas(Fs, T).


undergenerates(dl(dl(_,_),_)).
undergenerates(dr(_,dr(_,_))).


argument(dr(_, A), A).
argument(dl(A, _), A).
argument(dr(F, _), A) :-
	argument(F, A).
argument(dl(_, F), A) :-
	argument(F, A).


%

simplify_formulas :-
	formula(F0, N),
	simplify_formula(F0, F),
	portray_clause(formula(F, N)),
	fail.
simplify_formulas.

simplify_formula(dl(_,A0,B0), dl(A,B)) :-
	!,
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(_,A0,B0), dr(A,B)) :-
	!,
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(_,A0,B0), p(A,B)) :-
	!,
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(_, A0), A) :-
	!,
	simplify_formula(A0, A).
simplify_formula(box(_, A0), A) :-
	!,
	simplify_formula(A0, A).
simplify_formula(A, A).


	
