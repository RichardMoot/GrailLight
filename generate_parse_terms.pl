% -*- Mode: Prolog -*-

:- use_module(sem_utils, [relabel_sem_vars/2,relabel_sem_vars/4,replace_sem/4,get_fresh_variable_number/2]).
:- use_module(ordset, [ord_union/3, ord_subtract/3, ord_insert/3]).

:- compile('/Users/moot/checkout/TLGbank/nd_proofs/300_nd.pl').

start :-
	tell('term_log.txt'),
%	Sents = [4],
%	member(N, Sents),
	proof(N, rule(_,_,_-Term0,_)),
	rightmost_word(Term0, Right),
	Right < 16,
	format('% = ~p (~w words)~n', [N, Right]),
	%	proof(3, rule(_,_,_-Term0,_)),
	format(user_error, '~n***~w  (~w words)***', [N, Right]),
	relabel_sem_vars(Term0, Term),
	generate_parse_terms([[Term]], [[Term]], AllTerms),
%	add_leftmost(AllTerms, LTerms),
%	sort(1, @>, LTerms, Sorted),
	length(AllTerms, LenAllTerms),
	format(user_error, '~n***~w parse items for ~w words', [LenAllTerms, Right]),
	print_list(AllTerms),
	fail.
start :-
	told.


generate_parse_terms(Term0, AllTerms) :-
	relabel_sem_vars(Term0, Term),
	generate_parse_terms([[Term]], [[Term]], AllTerms).

test(AllTerms) :-
	generate_parse_terms(appl(word(1),lambda('$VAR'(0),(appl(word(2),'$VAR'(0))))), AllTerms).
test(Example, All, Valid) :-
	Term = appl(word(1),lambda('$VAR'(0),(appl(word(2),'$VAR'(0))))),
	generate_parse_terms(Term, AllTerms0),
	sort(AllTerms0, AllTerms),
	print_list(AllTerms),
	member(Example, AllTerms),
%	AllTerms = [_,Example|_], % take the second term list
	term_list_to_graph_list(Example, GraphList),
	compute_correct_actions(Term, GraphList, All, Valid).

% compute all 1-step reductions
% remove those already seen
% recursively compute 1-step reductions for all new sets of terms.


generate_parse_terms([], Visited, Visited).
generate_parse_terms([New|News0], Visited0, Visited) :-
	%    format('~w~n', [Term])
	findall(D, decompose_terms(New, D), List),
	sort(List, OrdSet),
	ord_union(OrdSet, Visited0, Visited1),
	ord_subtract(OrdSet, Visited0, News1),
	ord_union(News0, News1, News),
	length(News, Len),
	length(Visited1, VLen),
	format(user_error, '[~w-~w]', [Len,VLen]),
	generate_parse_terms(News, Visited1, Visited).

decompose_terms(Terms0, DTerms) :-
	select(Term, Terms0, Terms1),
	decompose_term(Term, NewTerm),
	add_new(NewTerm, Terms1, Terms2),
	sort(Terms2, DTerms).

add_new(Term1-Term2, Terms, [Term1, Term2|Terms]) :-
	!.
add_new(Term, Terms, [Term|Terms]).

decompose_term(appl(A0,B0), A-B) :-
	relabel_sem_vars(A0, A),
	relabel_sem_vars(B0, B).
decompose_term(A0, A) :-
	decompose_lambda(A0, A1),
%	A0 \== A1,   % shouldn't be necessary
	relabel_sem_vars(A1, A).

decompose_lambda(lambda(X,M0), M) :-
	decompose_lambda1(M0, X, M),
	M \== M0.
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
decompose_lambda1('$VAR'(N), _, '$VAR'(N)).
decompose_lambda1(word(N), _, word(N)).
decompose_lambda1(appl(M0,N0), X, appl(M,N)) :-
	decompose_lambda1(M0, X, M),
	decompose_lambda1(N0, X, N).
decompose_lambda1(lambda(X,M0), Y, lambda(X,M)) :-
	decompose_lambda1(M0, Y, M).

% =

lambda_subterm(lambda(X,Term0), LR, Term0, Sister) :-
	lambda_subterm1(Term0, X, LR, Sister).
lambda_subterm(lambda(_,Term0), LR, Root, Sister) :-
	lambda_subterm(Term0, LR, Root, Sister).
lambda_subterm(appl(Term0,_), LR, Root, Sister) :-
	lambda_subterm(Term0, LR, Root, Sister).
lambda_subterm(appl(_,Term0), LR, Root, Sister) :-
	lambda_subterm(Term0, LR, Root, Sister).

lambda_subterm1(appl(X,Sister), X, l, Sister) :-
	!.
lambda_subterm1(appl(Sister,X), X, r, Sister) :-
	!.
lambda_subterm1(appl(Term,_), Var, LR, Sister) :-
	lambda_subterm1(Term, Var, LR, Sister).
lambda_subterm1(appl(_,Term), Var, LR, Sister) :-
	lambda_subterm1(Term, Var, LR, Sister).
lambda_subterm1(lambda(_,Term), Var, LR, Sister) :-
	lambda_subterm1(Term, Var, LR, Sister).

term_list_to_graph_list(TermList, GraphList) :-
	term_list_to_graph_list(TermList, 1, GraphList).

term_list_to_graph_list([], _, []).
term_list_to_graph_list([T|Ts], N0, [Vs-Es|Graphs]) :-
	term_to_graph(T, N0, N, Vs, Es),
	term_list_to_graph_list(Ts, N, Graphs).

%

term_yield(Term, Yield) :-
	term_yield(Term, Yield, []).

term_yield('$VAR'(_), Ys, Ys).
term_yield(word(N), [word(N)|Ys], Ys).
term_yield(lambda(_,M), Ys0, Ys) :-
	term_yield(M, Ys0, Ys).
term_yield(appl(M,N), Ys0, Ys) :-
	term_yield(M, Ys0, Ys1),
	term_yield(N, Ys1, Ys).

% = term_to_graph(+Term, -Graph)
%
% take a lambda term Term and convert it into a hypergraph Graph (in the form Vertices-Edges)

term_to_graph(Term, Vertices-Edges) :-
	term_to_graph(Term, 0, _, Vertices, Edges).

term_to_graph(Term, Vertices, Edges) :-
	term_to_graph(Term, 0, _, Vertices, Edges).
	
term_to_graph(Term, N0, N, Vertices, Edges) :-
	dfs_number(Term, N0, N, Vars, [], Term0),
	replace_vars(Term0, Vars, Term1),
	term_to_graph(Term1, Vertices, [], Edges, []).

term_to_graph(N-'$VAR'(M), [N-'$VAR'(M)|Vs], Vs, Es, Es).
term_to_graph(N-word(M), [N-word(M)|Vs], Vs, Es, Es).
term_to_graph(N1-appl(N2-M,N3-P), [N1-appl|Vs0], Vs, [hyperedge(appl,N1,N2,N3)|Es0], Es) :-
	term_to_graph(N2-M, Vs0, Vs1, Es0, Es1),
	term_to_graph(N3-P, Vs1, Vs, Es1, Es).
term_to_graph(N1-lambda(N3-_,N2-M), [N1-lambda|Vs0], Vs, [hyperedge(lambda,N1,N2,N3)|Es0], Es) :-
	term_to_graph(N2-M, Vs0, Vs, Es0, Es).
	
replace_vars(N-'$VAR'(M), _, N-'$VAR'(M)).
replace_vars(N-word(M), _, N-word(M)).
replace_vars(N-appl(M0,P0), Vars, N-appl(M,P)) :-
	replace_vars(M0, Vars, M),
	replace_vars(P0, Vars, P).
replace_vars(N0-lambda(X,P0), Vars, N0-lambda(N-X,P)) :-
	member(N-X, Vars),
	!,
	replace_vars(P0, Vars, P).

graph_list_to_term_list([], []).
graph_list_to_term_list([G|Gs], [T|Ts]) :-
	graph_to_term(G, T),
	graph_list_to_term_list(Gs, Ts).

% = graph_to_term

graph_to_term(Vs-Es, Term) :-
	graph_to_term(Vs, Es, Term).

graph_to_term(Vs, Es, Term) :-
	find_root(Es, Vs, N, L),
	graph_to_term(L, N, Vs, Es, Term0),
	relabel_sem_vars(Term0, Term).

graph_to_term('$VAR'(_), N, _, _, '$VAR'(N)).
graph_to_term(word(M), _, _, _, word(M)).
graph_to_term(lambda, N, Vs, Es, lambda('$VAR'(Var),P)) :-
	member(hyperedge(lambda,N,N2,Var), Es),
	member(Var-'$VAR'(_), Vs),
	member(N2-Lab, Vs),
	!,
	graph_to_term(Lab, N2, Vs, Es, P).
graph_to_term(appl, N, Vs, Es, appl(P,Q)) :-
	member(hyperedge(appl,N,N1,N2), Es),
	member(N1-L1, Vs),
	member(N2-L2, Vs),
	!,
	graph_to_term(L1, N1, Vs, Es, P),
	graph_to_term(L2, N2, Vs, Es, Q).

find_root([], [N-L], N, L).
find_root([E|Es], Vs0, N, L) :-
	delete_daughters(E, Vs0, Vs1),
	find_root(Es, Vs1, N, L).

find_roots([], Vs, Vs).
find_roots([E|Es], Vs0, Vs) :-
	delete_daughters(E, Vs0, Vs1),
	find_roots(Es, Vs1, Vs).



compute_yields(Vertices-Edges, RootYieldPairs) :-
	find_roots(Edges, Vertices, Roots),
	compute_yields(Roots, Vertices, Edges, RootYieldPairs).

compute_yields(Roots, Vertices, Edges, RootYieldPairs) :-
	compute_yields(Roots, Vertices, _, Edges, [], RootYieldPairs).

compute_yields([], Vs, Vs, Es, Es, []).
compute_yields([R-L|Rs], Vs0, Vs, Es0, Es, [R-Yield|Ys]) :-
	compute_yield(L, R, Vs0, Vs1, Es0, Es1, Yield, []),
	compute_yields(Rs, Vs1, Vs, Es1, Es, Ys).

compute_yield('$VAR'(_), _, Vs, Vs, Es, Es, Ys, Ys).
compute_yield(word(M), _, Vs, Vs, Es, Es, [M|Ys], Ys).
compute_yield(lambda, R, Vs0, Vs, Es0, Es, Ys0, Ys) :-
	select(hyperedge(lambda,R,D1,_), Es0, Es1),
	select(D1-L1, Vs0, Vs1),
	!,
	compute_yield(L1, D1, Vs1, Vs, Es1, Es, Ys0, Ys).
compute_yield(appl, R, Vs0, Vs, Es0, Es, Ys0, Ys) :-
	select(hyperedge(appl,R,D1,D2), Es0, Es1),
	select(D1-L1, Vs0, Vs1),
	select(D2-L2, Vs1, Vs2),
	!,
	compute_yield(L1, D1, Vs2, Vs3, Es1, Es2, Ys0, Ys1),
	compute_yield(L2, D2, Vs3, Vs, Es2, Es, Ys1, Ys).

delete_daughters(hyperedge(L,_,A,B), Vs0, Vs) :-
	delete_daughters1(L, A, B, Vs0, Vs).

delete_daughters1(lambda, A, _, Vs0, Vs) :-
	select(A-_, Vs0, Vs),
	!.
delete_daughters1(appl, A, B, Vs0, Vs) :-
	select(A-_, Vs0, Vs1),
	select(B-_, Vs1, Vs),
	!.


% number terms nodes per DFS pre-order numbering.
% N0 is the first unused number
% differences list Vs keeps track of node numbers of bound variables

dfs_number('$VAR'(M), N0, N, [N0-'$VAR'(M)|Vs], Vs, N0-'$VAR'(M)) :-
	N is N0 + 1.
dfs_number(word(M), N0, N, Vs, Vs, N0-word(M)) :-
	N is N0 + 1.
dfs_number(appl(M0,P0), N0, N, Vs0, Vs, N0-appl(M,P)) :-
	N1 is N0 + 1,
	dfs_number(M0, N1, N2, Vs0, Vs1, M),
	dfs_number(P0, N2, N, Vs1, Vs, P).
dfs_number(lambda(X,P0), N0, N, Vs0, Vs, N0-lambda(X,P)) :-
	N1 is N0 + 1,
	dfs_number(P0, N1, N, Vs0, Vs, P).

% = compute_actions(+ListOfGraphs, -ListOfActions)
%
% Take ListOfGraphs and compute all possible parse actions in ListOfActions.
% possible actions are
%
% - end (possible if only a single graph remains)
% - appl (for any two root nodes)
% - lambda (for a node and one of its descendants, plus a choice of left/right)

compute_actions(Gs, Actions) :-
        compute_applications(Gs, Aps),
	compute_abstractions(Gs, Abs, Aps),
	compute_can_stop(Gs, Actions, Abs).

compute_correct_actions(CorrectTerm, Gs, AllActions, ValidActions) :-
	compute_actions(Gs, AllActions),
	correct_actions(AllActions, CorrectTerm, Gs, ValidActions, []).


correct_actions([], _, _, Vs, Vs).
correct_actions([A|As], CorrectTerm, Gs, Vs0, Vs) :-
	is_correct_action(A, CorrectTerm, Gs, Vs0, Vs1),
	correct_actions(As, CorrectTerm, Gs, Vs1, Vs).

is_correct_action(end(N), CorrectTerm, [G], [end(N)|Vs], Vs) :-
	graph_to_term(G, Term),
	is_alpha_equivalent(Term, CorrectTerm),
	!.
is_correct_action(appl(N,M), CorrectTerm, Gs0, [appl(N,M)|As], As) :-
	/* we again assume the root node is the first list member of each graph */
%	trace,
	G1 = [N-_|_]-_,
	G2 = [M-_|_]-_,
	select(G1, Gs0, Gs1),
	member(G2, Gs1),
	graph_to_term(G1, Term1),
	graph_to_term(G2, Term2),
	check_is_subterm(appl(Term1,Term2), CorrectTerm),
	!.
is_correct_action(lambda(LR, V1, V2), CorrectTerm, Gs, [lambda(LR,V1,V2)|As], As) :-
        member(G, Gs),
	G = Vs-Es,
	member(V1-L1, Vs),
	member(V2-L2, Vs),
	graph_to_term(L1, V1, Vs, Es, TermA),
	graph_to_term(L2, V2, Vs, Es, TermB),
	select_term(TermA, TermB, TermC, TermCC),
	get_fresh_variable_number(TermA, FX),
	add_variable(LR, '$VAR'(FX), TermB, TermCC),
	Result = lambda('$VAR'(FX), TermC),
	check_is_subterm(Result, CorrectTerm),
	!.	
% if none of the previous cases succeeded, then the action must be incorrect
is_correct_action(_, _, _, As, As).


select_term(X, X, V, V).
select_term(appl(M0,N), X, appl(M,N), V) :-
	select_term(M0, X, M, V).
select_term(appl(M,N0), X, appl(M,N), V) :-
	select_term(N0, X, N, V).
select_term(lambda(Y,M0), X, lambda(Y,M), V) :-
	/* NB: we explicitly allow variable capture here, i.e. X can contain Y */
	select_term(M0, X, M, V).

add_variable(l, X, Term, appl(X, Term)).
add_variable(r, X, Term, appl(Term, X)).

compute_root_nodes([], []).
compute_root_nodes([G|Gs], [R|Rs]) :-
	root_node(G, R),
	compute_root_nodes(Gs, Rs).


% possible applications connect any two root nodes (with the first being the functor, and the
% second the argument)

compute_applications(Gs, Applications) :-
	compute_root_nodes(Gs, Rs),
	findall(appl(R1,R2), (select(R1, Rs, Rs0), member(R2, Rs0)), Applications).

compute_abstractions(Gs, Abs) :-
	compute_abstractions(Gs, Abs, []).

compute_abstractions([], Abs, Abs).
compute_abstractions([G|Gs], Abs0, Abs) :-
	compute_abstractions1(G, Abs0, Abs1),
	compute_abstractions(Gs, Abs1, Abs).

% possible abstractions connect any vertex in the graph to one of its descendants

compute_abstractions1(Vs-Es, Abs0, Abs) :-
	findall(V1-V2, (member(V1-_, Vs),
			compute_descendants(V1, Es, Ds),
			member(V2, Ds)), Pairs),
	add_pairs(Pairs, Abs0, Abs).

%add_pairs(

compute_descendants(Vs, Es, Ds) :-
	compute_descendants(Vs, Es, _, Ds, []).

compute_descendants(V, Es0, Es, [V|Ds0], Ds) :-
	select(hyperedge(Type,V,D1,D2), Es0, Es1),
	!,
	compute_descendants1(Type, D1, D2, Es1, Es, Ds0, Ds).
compute_descendants(V, Es, Es, [V|Ds], Ds).	

compute_descendants1(lambda, D1, _, Es0, Es, Ds0, Ds) :-
	compute_descendants(D1, Es0, Es, Ds0, Ds).
compute_descendants1(appl, D1, D2, Es0, Es, Ds0, Ds) :-
	compute_descendants(D1, Es0, Es1, Ds0, Ds1),
	compute_descendants(D2, Es1, Es, Ds1, Ds).

add_pairs([], Abs, Abs).
add_pairs([V1-V2|Ps], [lambda(r,V1,V2),lambda(l,V1,V2)|Abs0], Abs) :-
	add_pairs(Ps, Abs0, Abs).

compute_can_stop([G], [end(N)|As], As) :-
	!,
	root_node(G, N).
compute_can_stop(_, As, As).

% returns the root node of a set of vertices; by convention this is
% the first vertex.

root_node([N-_|_]-_, N).

%

action_subterm(end(_), Graph, Term) :-
	graph_to_term(Graph, Term).

% apply_acction(+Action, +InGraph, -OutGraph)
%
% takes an action description Action and an input graph InGraph (in Vs-Es form) and produces
% the graph resulting from the application of the given Action as OutGraph.

apply_action(end(_), Graph, Graph).
apply_action(appl(N,M), Graph0, Graph) :-
	application_action(Graph0, N, M, Graph).
apply_action(lambda(LR,N,M), Graph0, Graph) :-
	abstraction_action(Graph0, LR, N, M, Graph).

application_action(Vs0-Es, N, M, Vs-[hyperedge(appl,P,N,M)|Es]) :-
	add_fresh_vertices(Vs0, 0, _, [P-appl], Vs).
abstraction_action(Vs0-Es0, LR, N, M, Vs-[hyperedge(lambda,P,AR,R),hyperedge(appl,Q,Fun,Arg)|Es]) :-
	add_fresh_vertices(Vs0, 0, _, [P-lambda,Q-appl,R-'$VAR'(_)], Vs),
	appl_order(LR, M, R, Fun, Arg),
	renumber(N, M, Q, AR),
	renumber_vertices(Es0, N, P, Es1),
	renumber_vertices(Es1, M, Q, Es).


appl_order(l, Sister, Var, Var, Sister).
appl_order(r, Sister, Var, Sister, Var).

renumber_vertices([], _, _, []).
renumber_vertices([hyperedge(L,A,B0,C0)|Es0], X, Y, [hyperedge(L,A,B,C)|Es]) :-
	renumber1(L, X, Y, B0, B, C0, C),
	renumber_vertices(Es0, X, Y, Es).

renumber1(appl, X, Y, B0, B, C0, C) :-
	renumber(B0, X, Y, B),
	renumber(C0, X, Y, C).
renumber1(lambda, X, Y, B0, B, C, C) :-
	renumber(B0, X, Y, B).

renumber(V, X, Y, W) :-
   (
	V = X
   ->
	W = Y
   ;
	V = W
   ).
			   
add_fresh_vertices1([], N, N, []).
add_fresh_vertices1([N1-F|Fs], N0, N, [N1-F|Vs]) :-
	N1 is N0 + 1,
	add_fresh_vertices1(Fs, N1, N, Vs).

add_fresh_vertices([], N0, N, Fs, Vs) :-
	add_fresh_vertices1(Fs, N0, N, Vs).
add_fresh_vertices([N1-X|Vs], N0, N, Fs, [N1-X|Ws]) :-
	N2 is max(N0, N1),
	add_fresh_vertices(Vs, N2, N, Fs, Ws).

%term_to_graph('$VAR'(N), 
    
print_list([]) :-
	nl.
print_list([A|As]) :-
	print_length(A),
	print(A),
	nl,
	print_list(As).

add_leftmost([], []).
add_leftmost([L0|Ls0], [L|Ls]) :-
	add_leftmost1(L0, L),
	add_leftmost(Ls0, Ls).

% take a list of terms List and add the leftmost word label to each of tem
% OrdList is an accumulator, containing all the terms in increasing order
% of their leftmost label.

add_leftmost1(L0, L) :-
	add_leftmost1(L0, [], L).

add_leftmost1([], Ls, Ls).
add_leftmost1([T|Ts], Ls0, Ls) :-
	leftmost_word(T, LT),
	ord_insert(Ls0, LT-T, Ls1),
	add_leftmost1(Ts, Ls1, Ls).

leftmost_word(T, Left) :-
	leftmost_word(T, 999999999, Left).

leftmost_word(word(N0), Left0, Left) :-
   (
	N0 < Left0
   ->
	Left = N0
   ;
	Left = Left0
   ).
leftmost_word('$VAR'(_), Left, Left).
leftmost_word(appl(M,N), Left0, Left) :-
	leftmost_word(M, Left0, Left1),
	leftmost_word(N, Left1, Left).
leftmost_word(lambda(_,M), Left0, Left) :-
	leftmost_word(M, Left0, Left).

rightmost_word(T, Right) :-
	rightmost_word(T, -1, Right).
rightmost_word(word(N0), Right0, Right) :-
   (
	N0 > Right0
   ->
        Right = N0
   ;
        Right = Right0
   ).
rightmost_word('$VAR'(_), Right, Right).
rightmost_word(appl(M,N), Right0, Right) :-
	rightmost_word(M, Right0, Right1),
	rightmost_word(N, Right1, Right).
rightmost_word(lambda(_,M), Right0, Right) :-
	rightmost_word(M, Right0, Right).

print_length(L) :-
	is_list(L),
	!,
	length(L, N),
	format('~w-', [N]).
print_length(_) :-
	true.


subterm(X, X).
subterm(appl(M,_), X) :-
	subterm(M, X).
subterm(appl(_,N), X) :-
	subterm(N, X).
subterm(lambda(_,M), X) :-
	subterm(M, X).

check_is_subterm(M, N) :-
	is_subterm(M, N),
	!.

% is_subterm(X, Y) is true if X is a subterm of Y

is_subterm(X, Y) :-
	get_fresh_variable_number(X, V),
	get_fresh_variable_number(Y, W),
	Max is max(V,W),
	is_subterm(X, Max, Y).

is_subterm(X, V, Y) :-
	is_equivalent(X, V, _, Y).
is_subterm(P, V, appl(M,_)) :-
	is_subterm(P, V, M).
is_subterm(P, V, appl(_,N)) :-
	is_subterm(P, V, N).
is_subterm(P, V, lambda(_,M)) :-
	is_subterm(P, V, M).
%is_subterm(lambda(X,M), lambda(Y,Q)) :-
%	replace_sem(P, Y, X, Q),
%	is_subterm(M, P).
%is_subterm(lambda(X,M), P) :-
%	remove_variable(M, X, N),
%	is_subterm(N, P).

is_equivalent('$VAR'(N), V, V, '$VAR'(N)).
is_equivalent(word(N), V, V, word(N)).
is_equivalent(appl(N0,M0), V0, V, appl(N,M)) :-
	is_equivalent(N0, V0, V1, N),
	is_equivalent(M0, V1, V, M).
is_equivalent(lambda(X,M0), V0, V, lambda(Y,P0)) :-
	% if Y already exists in M, replace is by a fresh variable V0
	% then replace X by Y
	% as written, fails when X = Y
	replace_sem(M0, X, '$VAR'(V0), M),
	replace_sem(P0, Y, '$VAR'(V0), P),
	V1 is V0 + 1,
	is_equivalent(M, V1, V, P).
% this case allows specifically for equivalence module lambda expensions
is_equivalent(P, V0, V, lambda(X,M)) :-
	remove_variable(M, X, N),
	is_equivalent(P, V0, V, N).


% 

is_alpha_equivalent(Term0, Term) :-
	get_fresh_variable_number(Term0, V),
	get_fresh_variable_number(Term, W),
	Max is max(V,W),
	is_alpha_equivalent(Term0, Max, _, Term).

is_alpha_equivalent('$VAR'(N), V, V, '$VAR'(N)).
is_alpha_equivalent(word(N), V, V, word(N)).
is_alpha_equivalent(appl(N0,M0), V0, V, appl(N,M)) :-
	is_alpha_equivalent(N0, V0, V1, N),
	is_alpha_equivalent(M0, V1, V, M).
is_alpha_equivalent(lambda(X,M0), V0, V, lambda(Y,N0)) :-
	% if Y already exists in M, replace is by a fresh variable V0
	% then replace X by Y
	replace_sem(M0, X, '$VAR'(V0), M),
	replace_sem(N0, Y, '$VAR'(V0), N),
	V1 is V0 + 1,
	is_alpha_equivalent(M, V1, V, N).

remove_variable('$VAR'(N), _, '$VAR'(N)).
remove_variable(word(N), _, word(N)).
remove_variable(appl(M,X), X, M) :-
	!.
remove_variable(appl(X,M), X, M) :-
	!.
remove_variable(lambda(Y,M), X, lambda(Y, N)) :-
	% check for bound variable; should never be necessary
   (
	X == Y
   ->
	N = M
   ;
	remove_variable(M, X, N)
   ).
remove_variable(appl(M0,N0), X, appl(M,N)) :-
	remove_variable(M0, X, M),
	remove_variable(N0, X, N).

