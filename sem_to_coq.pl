
sem_to_coq(A) :-
	atomic(A),
	!,
	print(A).
sem_to_coq(variable(X)) :-
	!,
	print_var(X, 'Entity').
sem_to_coq(event(X)) :-
	!,
	print_var(X, 'Event').
sem_to_coq(X) :-
	functor(X,'$VAR',_),
	!,
	print(X).

sem_to_coq(bool(drs(V1,C1),->,drs(V2,C2))) :-
	!,
	format('~@ ( ~@ -> ~@ ( ~@))', [print_universal(V1),print_and(C1),print_existential(V2),print_and(C2)]).
sem_to_coq(drs(V,C)) :-
	!,  
	format('~@ ( ~@)', [print_existential(V),print_and(C)]).
sem_to_coq(appl(A,B)) :-
	format('(~@ ~@)', [sem_to_coq(A),sem_to_coq(B)]).

sem_to_coq(not(A)) :-
	!,
	format('~~(~@)', [sem_to_coq(A)]).
sem_to_coq(bool(A,->,B)) :-
	!,
	format('(~@ -> ~@)', [sem_to_coq(A),sem_to_coq(B)]).
sem_to_coq(bool(A,&,B)) :-
	!,
	format('(~@ /\\ ~@)', [sem_to_coq(A),sem_to_coq(B)]).
sem_to_coq(bool(A,\/,B)) :-
	!,
	format('(~@ \\/ ~@)', [sem_to_coq(A),sem_to_coq(B)]).
sem_to_coq(quant(forall,X,A)) :-
	!,
	format('forall ~@, (~@)', [print_var(X),sem_to_coq(A)]).
sem_to_coq(quant(exists,X,A)) :-
	!,
	format('exists ~@, (~@)', [print_var(X),sem_to_coq(A)]).

print_and([]) :-
	write('True').
print_and([A|As]) :-
	print_and(As, A).

print_and([], A) :-
	sem_to_coq(A).
print_and([B|Bs], A) :-
	format('~@ /\\ ~@', [sem_to_coq(A), sem_to_coq(Bs, B)]).

print_universal([]).
print_universal([X|Xs]) :-
	format('forall ~@, ~@', [print_var(X), print_universal(Xs)]).

print_existential([]).
print_existential([X|Xs]) :-
	format('exists ~@, ~@', [print_var(X), print_existential(Xs)]).



print_var(variable(X)) :-
	!,
	print_var(X, 'Entity').
print_var(event(X)) :-
	!,
	print_var(X, 'Event').
print_var(X) :-
	print_var(X, 'Entity').

print_var(X, Type) :-
	X = '$VAR'(_),
	format('~p:~w ', [X, Type]).
