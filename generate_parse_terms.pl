% -*- Mode: Prolog -*-

:- use_module(sem_utils, [relabel_sem_vars/2]).

:- compile('/Users/moot/checkout/TLGbank/nd_proofs/300_nd.pl').

start :-
    proof(N, rule(_,_,_-Term0,_)),
    format('% = ~p~n', [N]),
    relabel_sem_vars(Term0, Term),
    generate_parse_terms([Term], [Term]).

% compute all 1-step reductions
% remove those already seen
% recursively compute 1-step reductions for all new sets of terms.

generate_parse_terms(Terms0) :-
    %    format('~w~n', [Term])
    findall(N-A, decompose_terms(Terms0, N, A), List),
    print(List).


decompose_terms(Terms0, NewTerms, AllTerms) :-
    select(Term, Terms0, Terms1),
    decompose_term(Term, NewTerms),
    append(NewTerms, Terms1, Terms3),
    sort(Terms3, AllTerms).

decompose_term(appl(A0,B0), [A,B]) :-
    relabel_sem_vars(A0, A),
    relabel_sem_vars(B0, B).
decompose_term(A0, [A]) :-
    decompose_lambda(A0, A1),
    A0 \== A1,   % shouldn't be necessary
    relabel_sem_vars(A1, A).


decompose_lambda(lambda(X,M0), M) :-
    decompose_lambda1(M0, X, M).
decompose_lambda(lambda(X,M0), lambda(X,M)) :-
    decompose_lambda(M0, M).
decompose_lambda(appl(M0, N), appl(M,N)) :-
    decompose_lambda(M0, M).
decompose_lambda(appl(M,N0), appl(M,N)) :-
    decompose_lambda(N0, N).

decompose_lambda1(appl(X,M), X, M) :-
    !.
decompose_lambda1(appl(M,X), X, M) :-
    !.
decompose_lambda1('$VAR'(N), '$VAR'(N)).
decompose_lambda1(word(N), word(N)).
decompose_lambda1(appl(M0,N0), appl(M,N)) :-
    decompose_lambda1(M0, M),
    decompose_lambda1(N0, N).
decompose_lambda1(lambda(X,M0), lambda(X,M)) :-
    decompose_lambda1(M0, M).

    
