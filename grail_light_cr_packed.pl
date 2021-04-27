#!/Applications/SWI-Prolog.app/Contents/MacOS//swipl -q -t main -f

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

:- use_module(transform_proof, [transform_proof/2]).
:- use_module(lexicon, [macro_expand/2,get_item_semantics/5]).
:- use_module(heap, [empty_heap/1,add_to_heap/4,get_from_heap/4]).
:- use_module(prob_lex, [list_atom_term/2,list_atom_term/3,remove_brackets/2]).
:- use_module(sem_utils, [substitute_sem/3,reduce_sem/2,replace_sem/4,melt_bound_variables/2,subterm/2,subterm_with_unify/2,renumbervars/1,try_unify_semantics/2,is_closed/1]).
:- use_module(latex, [latex_proof/2,latex_header/1,latex_header/2,latex_tail/1,latex_semantics/3]).
:- use_module(options, [create_options/0,get_option/2,option_true/1]).
:- use_module(print_proof, [print_proof/3,xml_proof/3]).
:- use_module(ordset, [ord_subtract/3, ord_member/2, ord_insert/3, ord_subset/2, ord_key_insert/4, ord_select/3, ord_delete/3]).
:- use_module(list_utils, [strip_keys/2,insert_nth0/4]).
:- use_module(library(pce)).
:- use_module(library(pce_util)).

:- dynamic sentence_length/1, total_formulas/1, unparsed/3, parsed/2.
:- dynamic word/3, word/4, word/5, stored/6, max_queue_size/1, justify/2.
:- dynamic crosses/3, crosses/4, constituent/3, constituent/4, current_sentence/1.
:- dynamic translate_form/2, translate_sem/2, state/2.
:- dynamic active_rule/1, vp_left/1, let_right/1, '$PROOFAXIOMS'/1.

:- compile('big_french_drt.pl').

:- dynamic '$SOLUTION'/1.
:- dynamic verbose/0, interactive/0.
:- dynamic key_index/2.

:- create_options.

quote_mode(1, 1).

display_unreduced_semantics(no).

default_depth_limit(10000).
%default_depth_limit(25000).
%output_proofs(nd).
output_proofs(chart).

% = function combining the weight of items given a rule application.

%LP% = combine two log-probabilities using sum
%LPcombine_probability(Prob0, Prob1, _J, _K, _R, Prob) :-
%LP	Prob is Prob0 + Prob1.
%LP
%LP% = take log of initial probability
%LPcompute_weight(Prob, Weight) :-
%LP	Weight is log(Prob).

% = count the crossing links (subtract the crossing links since we are maximizing)
combine_probability(Prob0, Prob1, J, K, _R, Prob) :-
	crosses(J, K, Cross),
    (
        constituent(_, J, K)
    ->
        Const = 2
    ;
        Const = 0
    ),
	Prob is Prob0 + Prob1 - Cross + Const.
%	Prob is Prob0 + Prob1 + Const.

% = initialize to zero for crossing links
compute_weight(_, 0).


% = Chart parse library.
%
% This library contains code adapted from the following paper:
%
% Stuart M. Shieber, Yves Schabes and Fernando C. N. Pereira (1995)
% `Principles and Implementation of Deductive Parsing', Journal of
% Logic Programming 24(1-2):3-36
%
% though with some notable additions and modifications.

% Chart items are of the form
%
%   item(Formula, Left, Right, Data)
%
% where Formula is a multimodal formula, Left and Right are
% string positions representing the leftmost and the
% rightmost part of the string which was recognized as
% being of type Formula.
%
% Item data is a structure containing the following information.
%
%   data(Pros, Sem, Prob, H, A, B, C, D)
%
% where Pros is the prosodic structure (representing the
% words of the string in tree form), Sem is the semantics
% of the Formula, Prob is its probabiblity, H is the head
% word. In addition, there are four stacks (two for
% infixation, two for extraction). The first is for
% parenthetical infixation, the second for the (more
% constrained) adverbs and verb-modifying pps. The
% extraction stacks are for left extraction (fairly
% rare) and right extraction.

% = main
%
% handles the command line invocation of this script; all command line arguments are treated
% as file names to be compiled and parsed (using chart_parse_all).

main :-
	current_prolog_flag(os_argv, Argv),
        append(_, [A|Av], Argv),
	file_base_name(A, 'grail_light_cr.pl'),
	!,
        main(Av).


main([]).
main([from,A0,to,B0,F|Fs]) :-
	!,
	get_integer(A0, A),
	get_integer(B0, B),
	compile(F),
	chart_parse_from_to(A,B),
	main(Fs).
main([from,A0,F|Fs]) :-
	!,
	get_integer(A0, A),
	compile(F),
	chart_parse_all(A),
	main(Fs).
main([to,B0,F|Fs]) :-
	!,
	get_integer(B0, B),
	compile(F),
	chart_parse_until(B),
	main(Fs).
main([F|Fs]) :-
	compile(F),
	chart_parse_all,
	main(Fs).

get_integer(N0, N) :-
	integer(N0),
	!,
	N = N0.
get_integer(N0, N) :-
	atom(N0),
	!,
	atom_number(N0, N).

% = parse all sentences.

chart_parse_all :-
	default_depth_limit(DLimit),
	chart_parse_all(1, max, DLimit).

% = parse all sentences with a given depth limit DL

chart_parse_all_dl(DL) :-
	chart_parse_all(1, max, DL).

% = parse all sentences starting with SentNo

chart_parse_all(SentNo) :-
	default_depth_limit(DLimit),
	chart_parse_all(SentNo, max, DLimit).

% = parse all sentences until Max

chart_parse_until(Max) :-
	default_depth_limit(DLimit),
	chart_parse_all(1, Max, DLimit).

chart_parse_until(Max, DLimit) :-
	chart_parse_all(1, Max, DLimit).

chart_parse_from_to(Start, End) :-
	default_depth_limit(DLimit),
	chart_parse_all(Start, End, DLimit).	

chart_parse_from_to(Start, End, DLimit) :-
	chart_parse_all(Start, End, DLimit).	

chart_parse_all(SentNo, Max, DL) :-
	retractall(unparsed(_,_,_)),
	new_output_file(grail_log, log),
	new_output_file(unparsed, unparsed),
	new_output_file(parse_logs, plog),
	new_output_file('proof.tex', proof),
	new_output_file('proofs.pl', pl_proof),
	set_global_counter('$CHART_CURRENT', 0),
	set_global_counter('$CHART_FAIL', 0),
	set_global_counter('$CHART_LIMIT', 0),
	set_global_counter('$CHART_ALL', 0),
	print_grail_semantics_header,
	chart_parse_all0(SentNo, Max, DL),
	/* cleanup after failure-driven loop has parsed all sentences */
	'$CHART_ALL'(ALL),
   (
        ALL > 0
   ->
	'$CHART_FAIL'(FAIL),
	'$CHART_LIMIT'(LIMIT),
	retractall('$CHART_ALL'(_)),
	retractall('$CHART_FAIL'(_)),
	retractall('$CHART_LIMIT'(_)),
	Success is (ALL - FAIL) - LIMIT,
	SPercentage is (100*Success)/ALL,
	FPercentage is (100*FAIL)/ALL,
	LPercentage is (100*LIMIT)/ALL,
	format('~nFinished parsing~n~w total sentences~n~w sentences succeeded (~w %)~n~w sentences failed (~w %)~n~w resource limits (~w %)~n', [ALL,Success,SPercentage,FAIL,FPercentage,LIMIT,LPercentage])
   ;
        format('No sentences~n', [])
   ),
	print_grail_semantics_tail,
	close(unparsed),
	close(proof),
	close(pl_proof),
	close(plog),
	close(log).

chart_parse_all0(N, Max, DL) :-
	clause(user:sent(N0,_),_),
    (
        /* fail for sentence numbers smaller than N or bigger than Max */
        N0 >= N, N0 @=< Max
    ->
        increase_global_counter('$CHART_ALL'),
        set_global_counter('$CHART_CURRENT', N0)
    ),
	format('~nStarting: ~w~n', [N0]),
	update_crossing(N0),
	statistics(process_cputime, CPU0),
	statistics(inferences, Inferences0),
	parse_with_depth_limit(N0, Result, DL, DepthLimit),
    (
        DepthLimit = depth_limit_exceeded
     ->
	try_recover_chart_semantics(N0, DL, Inferences0, CPU0)
    ;
        Result = fail
    ->
        print_statistics('F', N0, DepthLimit, Inferences0, CPU0),
        sentence_length(Length),
	assert('unparsed'(N0,Length,fail)),
        portray_clause(unparsed, unparsed(N0,Length,fail)), 
        increase_global_counter('$CHART_FAIL'),
	format('~nFailure: ~w (~w)~n', [N0,DepthLimit])
     ;
        print_statistics('S', N0, DepthLimit, Inferences0, CPU0),
	print_grail_semantics(Result),
	assert(parsed(N0, Result)),
	format('~nSuccess: ~w (~w)~n', [N0,DepthLimit])
     ),
        fail.
chart_parse_all0(_, _, _).

% = parse_with_depth_limit(+SentNo, -Result, +MaxDepth, -FinalDepth)
%
% parse SentNo (with semantics Result) given depth limit MaxDepth, on exit
% FinalDepth will be the maximum depth used; in case the depth limit is
% exceeded FinalDepth will be depth_limit_exceeded.

parse_with_depth_limit(N, Result, DL, DepthLimit) :-
	call_with_depth_limit(parse_sentence(N, Result), DL, DepthLimit0),
   (
        Result = fail,
        DepthLimit0 >= DL
   ->
        DepthLimit = depth_limit_exceeded
   ;
        DepthLimit = DepthLimit0
   ).

% = parse_id(+SentNo, -Result, +MaxDepth, -FinalDepth)
%
% as parse_with_depth_limit/4, but performs iterative deepening: if the
% depth limit is exceeded (while the search space is not yet exhausted)
% then the sentence is retried with a higher depth limit (1000 higher).

parse_id(N, Result, DL0, DepthLimit) :-
	update_crossing(N),
 	parse_with_depth_limit(N, Result0, DL0, DepthLimit0),
    (
        DepthLimit0 = depth_limit_exceeded
    ->
        DL is DL0 + 1000,
        format('~nFailed~nNew Depth Limit: ~w~n', [DL]),
        parse_id(N, Result, DL, DepthLimit)
    ;
        Result = Result0,
        DepthLimit = DepthLimit0
    ).

% = parse_with_time_limit(+SentNo, -Result, +Time)
%
% parses SentNo (with semantics Result) given the time limit Time, in case the time limit
% is exceeded, Result with be depth_limit_exceeded (for reasons of uniformity, no distinction
% is made between an exceeded depth or time limit).

parse_with_time_limit(N, Result, T) :-
	catch(call_with_time_limit(T, parse_sentence(N, Result)), _, Result=depth_limit_exceeded).

% = parse_sentence(+SentNo, -Semantics)
%
% computes the first Semantics corresponding to sentence SentNo, returning Semantics=fail in case the
% parse is unsuccessful.
%
% In combination with call_with_depth_limit, this will return Semantics=fail in case the depth limit
% is exceeded; "fail" with values of less than the depth limit give an indication of the depth of
% the search space.

parse_sentence(N, Result) :-
    (
	user:sent(N, Result)
    ->
        true
    ;
        Result = fail
    ).

sentence(S, Sem) :-
	update_crossing(S),
	user:sent(S, Sem).

% = update_crossing(+SentNo)
%
% asserts basic facts in preparation for parsing SentNo; this includes, for each pair of string
% positions, the number of constituent boundaries it crosses.

update_crossing(S) :-
	retractall(current_sentence(_)),
	assert(current_sentence(S)),
	retractall(crosses(_,_,_)),
	retractall(constituent(_,_,_)),
	% set three-argument crosses/constituent to current sentence
	assert((crosses(X,Y,Z) :- crosses(S,X,Y,Z), !)),
	assert((constituent(Y,Z) :- constituent(S,_,Y,Z), !)),
	% default to no crosses
	assertz(crosses(_,_,0)),
	retractall(word(_,_,_)),
	retractall(vp_left(_)),
	retractall(let_right(_)),
	assert((word(A,B,C) :- word(S,A,B,C))).

% = print_statistics
%
% output parse statistics to the log file

print_statistics(State, N0, DepthLimit, Inferences0, CPU0) :-
	   statistics(inferences, Inferences1),
	   statistics(process_cputime, CPU1),
	   CPU is CPU1 - CPU0,
	   Inferences is Inferences1 - Inferences0,
	   sentence_length(Length),
	   total_formulas(TotalForms),
	   robust_max_queue_size(MaxQ),
	   format(plog, '~w\t~w\t~w\t~w\t~w\t~2f\t~w\t~w\t', [N0, State, Length, TotalForms, DepthLimit, CPU, Inferences, MaxQ]),
	   output_rule_statistics.


% = statistics_header
%
% add header with column names to the parse logs file

statistics_header :-
	check_plog_stream,
	rule_counts_init(Counts),
	format(plog, 'Sent\tState\tLength\tForms\tDepth\tCPU\tInfs\tQueue', []),
	print_header(Counts).

% print the rule names

print_header([]) :-
	nl(plog).
print_header([R-_|Rest]) :-
	format(plog, '~w\t', [R]),
	print_header(Rest).

% = try_recover_chart_semantics
%
% once the DepthLimit has been exceeded, check if the chart contains a solution
% and if so, recover its semantics (and count the parse as a success); if not
% the parse is added as a failure (for the current depth_limit) and noted as such.

try_recover_chart_semantics(N0, DL, Inferences, CPU) :-
	final_item(Goal, Index, Sem),
	item_in_chart(Goal, Index),
	compute_proof(Index),
	!,
       	increase_global_counter('$SOLUTION'),
	assert(parsed(N0, Sem)),
	print_grail_semantics(Sem),
	format('~nSuccess: ~w (MAX)~n', [N0]),
        print_statistics('S', N0, DL, Inferences, CPU).
try_recover_chart_semantics(N0, DL, Inferences, CPU) :-
	increase_global_counter('$CHART_LIMIT'),
	sentence_length(Length),
	assert('unparsed'(N0,Length,depth_limit)),
        portray_clause(unparsed, unparsed(N0,Length,depth_limit)),
	format('~nDepth limit exceeded: ~w~n', [N0]),
        print_statistics('L', N0, DL, Inferences, CPU).

% = startsymbol(+Start, +Semantics)
%
% true if Start is a valid category for spanning the entire chart.
% Semantics is the semantic term for existantial closure and other "final" operations to produce proper DRSs

startsymbol(lit(txt), lambda(X,X)).
startsymbol(lit(s), lambda(S,merge(drs([event(E)],[]),appl(S,E)))).
startsymbol(lit(s(_)), lambda(S,merge(drs([event(E)],[]),appl(S,E)))).
startsymbol(lit(np(_,_,_)), lambda(P,appl(P,lambda(_V,drs([],[]))))).
startsymbol(lit(n), lambda(N,merge(drs([variable(X)],[]),appl(N,X)))).
startsymbol(dl(0,lit(np(_,_,_)),lit(s(_))), lambda(VP,merge(drs([event(E),variable(X)],[appl(generic,X)]),appl(appl(VP,lambda(P,appl(P,X))),E)))).
startsymbol(dl(0,lit(n),lit(n)), lambda(ADJ,merge(drs([variable(X)],[]),appl(appl(ADJ,lambda(_,drs([],[]))),X)))).
startsymbol(dr(0,lit(s),lit(s)), lambda(ADV,merge(drs([event(E)],[bool(E,=,'event?')]),appl(appl(ADV,lambda(_,drs([],[]))),E)))).
startsymbol(dr(0,lit(s(_)),lit(s(_))), lambda(ADV,merge(drs([event(E)],[bool(E,=,'event?')]),appl(appl(ADV,lambda(_,drs([],[]))),E)))).
startsymbol(lit(let), lambda(_,drs([],[]))).


chart_semantics(SemInfo0, Semantics0, Semantics) :-
    (
        '$PROOFAXIOMS'(PFs),
        update_seminfo(SemInfo0, PFs, SemInfo)
    ->
        true
    ;
        /* proceed normally if no match is found */
        SemInfo0 = SemInfo
    ),
	compute_semantics(SemInfo, Subst),
        substitute_sem(Subst, Semantics0, Semantics).	

% = update_seminfo(+InitialEntries, +ProofAxioms, -MergedEntries)
%
% try to unify the initial lexical formulas with the axioms of the proof; this may further instantiate some
% underspecified lexical formulas.

update_seminfo([], [], []).
update_seminfo([IN-t(W,PosTT,Lemma,F)|Rest], [W0-F0|WFs], Update0) :-
   (
        W0 = W,
        F = F0
   ->
        Update0 = [IN-t(W,PosTT,Lemma,F)|Update],
	update_seminfo(Rest, WFs, Update)
   ;
        /* ignore axioms which don't match the given word-formula pair */
        Update = Update0,
        update_seminfo([IN-t(W,PosTT,Lemma,F)|Rest], WFs, Update)
   ).

compute_semantics([], []).
compute_semantics([IN-t(W,PosTT,Lemma,F)|Rest0], [IN-Sem|Rest]) :-
	get_item_semantics(W, PosTT, Lemma, F, Sem),
	compute_semantics(Rest0, Rest).

print_grail_semantics_header :-
	open_semantics_files,
	get_option(paper_size, PaperSize),
	latex_header(sem, PaperSize).

print_grail_semantics(Sem) :-
	renumbervars(Sem),
	reduce_sem(Sem, RSem),
	format('~nSemantics   : ~p~n', [Sem]),
	format('Reduced Sem : ~p~n', [RSem]),
	format(log, '~n% = Semantics~2n ~W~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(log, '% = Reduced Semantics~2n~W~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '~n% = Semantics~2n ~W.~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '% = Reduced Semantics~2n~W.~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem, '~n\\begin{multline}~n', []),
   (
        display_unreduced_semantics(yes)
   ->
	latex_semantics(Sem, Formula, sem),
	format(sem, '\\rightarrow_{\\beta}\\\\ ', [])
   ;
        true
   ),
	latex_semantics(RSem, Formula, sem),
	format(sem, '~n\\end{multline}~2n', []).

print_grail_semantics_tail :-
	latex_tail(sem),
	close(sem),
	pdflatex_semantics.

% = prob_parse(+ListOfAxioms, -Result)

prob_parse(List, Result) :-
	check_log_stream,
	init_chart,
	empty_heap(Heap),
	list_to_chart(List, 0, Heap, Chart, [], 0, _V, SemInfo, []),
   (
        interactive
   ->
        interactive_parse(Chart, Result0)
   ;
	chart_parse(Chart, Result0)
   ),
	chart_semantics(SemInfo, Result0, Result).
   
verify_word(W, N0, N) :-
	word(W1, N0, N),
	!,
    (
        W = W1
    ->
        true
    ;
        format('Alignment error: ~w-~w (~w-~w)~n', [W, W1, N0, N]),
        format(log, 'Alignment error: ~w-~w (~w-~w)~n', [W, W1, N0, N])
    ).
verify_word(_, _ , _).

lemma_sequence([], N, N).
lemma_sequence([L|Ls], N0, N) :-
	word(_, _, L, N0, N1),
	lemma_sequence(Ls, N1, N).


% = list_to_chart
%
% convert a list of (weighted) lexical entries to an agenda.
% add the best item of each word to the agenda, while constructing a heap with
% all of the alternatives.
%
% The result is an agenda with the best items for each word from left to right
% followed by the alternatives in order of decreasing weight.

list_to_chart([], N, H, As0, As, V, V, S, S) :-
	retractall(sentence_length(_)),
	assert(sentence_length(N)),
	add_heap_to_chart(H, As0, As).
% skip final punctuation if its formula is "boring"
%LPlist_to_chart([si(_, PUN, _, FP)], N, H, As0, As, V, V, S, S) :-
%LP       is_punct(PUN),
%LP       boring(FP) ,
%LP       retractall(sentence_length(_)),
%LP       assert(sentence_length(N)),
%LP       !,
%LP       add_heap_to_chart(H, As0, As).
list_to_chart([ex_si(_,_,_,_)|_], _N0, _H0, _As0, _As, _V0, _V, _S0, _S) :-
	format('~N{Error: unlemmatized sentence!}~n', []),
	fail.
list_to_chart([si(W,Pos,Lemma,FPs)|Ws], N0, H0, As0, As, V0, V, S0, S) :-
	N1 is N0 + 1,
	assert(word(W, Pos, Lemma, N0, N1)),
	assert_if_verb(Pos, N0),
	assert_if_let(Pos, N1),
	append_item_and_update_heap(FPs, W, Pos, Lemma, N0, N1, V0, V1, S0, S1, H0, H, As0, As1),
	list_to_chart(Ws, N1, H, As1, As, V1, V, S1, S).

% = add_heap_to_chart
%
% add the alternative items of the agenda by decreasing weight.

add_heap_to_chart(H0) -->
	{get_from_heap(H0, _Key, Datum, H)},
	!,
	[Datum],
	add_heap_to_chart(H).
add_heap_to_chart(_) -->
	[].


% = append_item_and_update_heap
%
% adds best item to the list and all alternatives to the heap.

append_item_and_update_heap([], _, _, _, _, _, _, S, S, H, H) -->
	[].
append_item_and_update_heap([F0-P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) -->
	!,
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	 IN1 is IN0 + 1},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H0, H)}.
append_item_and_update_heap([F0,P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) -->
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	 IN1 is IN0 + 1},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H0, H)}.

% = update_heap
%
% adds items to the heap

update_heap([], _, _, _, _, _, _, IN, IN, S, S, H, H).
update_heap([F0-P|FPs], PMax, W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) :-
	!,
	create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	IN1 is IN0 + 1,
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H1, H).
update_heap([F0,P|FPs], PMax, W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) :-
	create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	IN1 is IN0 + 1,
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H1, H).

% = create_item(+Formula, +Probability, +Word, +POStag, +Lemma, +Left, +Right, +ItemNo, ListIn, ListOut, -Item)
%
% construct Item based on all available information

create_item(F0, P, W, Pos, Lemma, N0, N1, IN, [word(IN)-t(W,PosTT,Lemma,F)|Ss], Ss, item(F, N0, N1, Data)) :-
	macro_expand(F0, F1),
	get_pos_tt(Pos, PosTT),
	enrich_formula(Lemma, PosTT, F1),
	correct_formula(PosTT, F1, F),
	create_data(W, F, Lemma, word(IN), P, N0, N1, Data).

% = get_semantics(+ChartItem, ?Semantics)
%
% true if ChartItem has meaning Semantics

get_semantics(item(_, _, _, Data), Sem) :-
	get_data_semantics(Data, Sem).

get_data_semantics(data(_, Sem, _, _, _, _, _, _), Sem).

% = get_weight(+ChartItem, ?Weight)
%
% true if ChartItem has Weight

get_weight(item(_, _, _, Data), Weight) :-
	get_data_weight(Data, Weight).

get_data_weight(data(_, _, Weight, _, _, _, _, _), Weight).

% = robust_max_queue_size(+QueueSize)
%
% computes the size of the queue; normally this is stored as a dynamic predicate max_queue_size/1.
% However, if it is not, this predicate will scan the chart to find the chart item with the
% highest value.

robust_max_queue_size(MaxQ) :-
    (
         max_queue_size(MaxQ)
    ->
         true
    ;
         robust_max_queue_size1(1000, 1000, MaxQ)
    ).


robust_max_queue_size1(Current, Step, MaxQ) :-
    (
         stored(Current, _, _, _, _, _)
    ->
         New is Current + Step,
         robust_max_queue_size1(New, Step, MaxQ)
    ;
         NewStep is Step/10,
         New is Current - Step + NewStep,
         robust_max_queue_size2(NewStep, New, MaxQ)
    ).


robust_max_queue_size2(1, Current, MaxQ) :-
	!,
    (
	stored(Current, _, _, _, _, _)
    ->
        New is Current + 1,
        robust_max_queue_size2(1, New, MaxQ)
    ;
        MaxQ is Current - 1
    ).
robust_max_queue_size2(Step, Current, MaxQ) :-
	robust_max_queue_size1(Current, Step, MaxQ).

% = is_punct(+POS)
%
% true if POS tag is an interpunction symbol

is_punct(ponct).
is_punct(pun).
is_punct(ponct-pun).

% = boring(+Entry)
%
% true if all solutions formulas assigned to a word are "boring"

boring([]).
boring([Item-_|FPs]) :-
	boring_item(Item),
	!,
	boring(FPs).
boring([Item,_|FPs]) :-
	boring_item(Item),
	boring(FPs).

boring_item(let).
boring_item(dl(0,_,txt)).
boring_item(dl(0,_,lit(txt))).

% = enrich_formula(+Word, +POStag, ?Formula)
%
% given a Word-POStag pair, tries to unify Formula with a formula containing more detailed information;
% for example, Word with formula dr(0,pp(_),np) is instantiated to dr(0,pp(Word),np)

enrich_formula(par, _, dr(0,dl(0,lit(np(_,_,_)),lit(s(inf(par)))),dl(0,lit(np(_,_,_)),lit(s(inf(base)))))) :-
	!.
enrich_formula(par, _, dr(0,lit(pp(par)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(par, _, dr(0,lit(pp(par)),lit(n))) :-
	!.
enrich_formula(pour, _, dr(0,lit(pp(pour)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(pour, _, dr(0,lit(pp(pour)),lit(n))) :-
	!.
enrich_formula(contre, _, dr(0,lit(pp(contre)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(contre, _, dr(0,lit(pp(contre)),lit(n))) :-
	!.
enrich_formula(sous, _, dr(0,lit(pp(sous)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(sous, _, dr(0,lit(pp(sous)),lit(n))) :-
	!.
enrich_formula(sur, _, dr(0,lit(pp(sur)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(sur, _, dr(0,lit(pp(sur)),lit(n))) :-
	!.
enrich_formula(en, _, dr(0,lit(pp(en)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(en, _, dr(0,lit(pp(en)),lit(n))) :-
	!.
enrich_formula(de, _, dr(0,lit(pp(de)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(de, _, dr(0,lit(pp(de)),lit(n))) :-
	!.
enrich_formula(avant, _, dr(0,lit(pp(avant)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(avant, _, dr(0,lit(pp(avant)),lit(n))) :-
	!.
enrich_formula(après, _, dr(0,lit(pp(après)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(après, _, dr(0,lit(pp(après)),lit(n))) :-
	!.
enrich_formula(L, _, dr(0,lit(pp(L)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(L, _, dr(0,lit(pp(L)),lit(n))) :-
	!.
enrich_formula(_, _, _).

% = correct_formula(+TTPostag, +Formula, -CorrectedFormula)
%
% sometimes, the correct formula translation is decided in part by the POS-tag (eg. for an imperative, the argument np is an
% accusative instead of a nominative); this predicate corrects the formula based on the POS-tag information; defaults to
% leaving the formula as is

correct_formula(pro:rel, dl(0, dr(0, dl(0, lit(np(A,B,C)), lit(s(main))), lit(np(D,E,F))), lit(s(whq))),
		         dl(0, dr(0, dl(0, lit(np(A,B,C)), lit(s(_))), lit(np(D,E,F))), lit(s(whq)))) :-
	!.
correct_formula(adv, dr(0, dl(0, lit(np(nom, A, B)), lit(s(main))), lit(s(q))), dr(0, dl(0,lit(np(nom,A,B)), lit(s(_))), lit(s(q)))) :-
	!.
correct_formula(ver:impe, dr(0, lit(s(S)),lit(np(nom,A,B))), dr(0, lit(s(S)), lit(np(acc,A,B)))) :-
	!.
correct_formula(ver:impe, dr(0, dr(0, lit(s(S)),lit(np(nom,A,B))), lit(pp(P))), dr(0, dr(0, lit(s(S)), lit(np(acc,A,B))), lit(pp(P)))) :-
	!.
correct_formula(ver:impe, dr(0, dr(0, lit(s(S)), lit(pp(P))),lit(np(nom,A,B))), dr(0, dr(0, lit(s(S)), lit(pp(P))), lit(np(acc,A,B)))) :-
	!.
correct_formula(ver:impe, dr(0, dr(0, lit(s(S)), lit(s(Q))), lit(np(nom,A,B))), dr(0, dr(0, lit(s(S)), lit(s(Q))), lit(np(acc,A,B)))) :-
	!.
correct_formula(_, F, F).


get_pos_tt(Pos, PosTT) :-
      (
         Pos = _Melt0-Pos0:Sub
      ->
         PosTT = Pos0:Sub
      ;
         Pos = _Melt1-PosTT
      ->
         true
      ;
         PosTT = Pos
      ).

% = 

chart_parse(Axioms, Sem) :-
	init_agenda(Axioms, Agenda),
	active_rules(Axioms),
    (
        /* succeed for empty sentences (eg. interpunction only) */
        Agenda = queue(0,0)
    ->
        Sem = drs([],[])
    ;
	exhaust(Agenda),
	check_solution(Sem)
    ).

% = active_rules(+Axioms)
%
% check all formulas used in the axioms and activate only the chart inferences
% which are required by these rules (eg. the gapping rules and products rules
% are useful only when there is a gap formula or a product formula in the
% axioms.
%
% TODO: it is worth thinking about a smarter version of this type of
% rule restriction by constraining the rules which can apply given a
% certain span of the active formulas.

active_rules(Axioms) :-
	all_rules_triggers(Axioms, List, []),
	sort(List, Set),
	retractall(active_rule(_)),
	% "dr" and "dl" are always active
	assert(active_rule(dr)),
	assert(active_rule(dl)),
	assert_active_rules(Set).

all_rules_triggers([]) -->
	[].
all_rules_triggers([item(F,_,_,_)|As]) -->
	rules_trigger(F),
	all_rules_triggers(As).

assert_active_rules([]).
assert_active_rules([R|Rs]) :-
	assert(active_rule(R)),
	assert_active_rules(Rs).

all_active_rule_statistics :-
	clause(sent(_Number, Sem), prob_parse(List, Sem)),
	all_active_rule_statistics(List),
	fail.
all_active_rule_statistics.

all_active_rule_statistics(List) :-	
	init_chart,
	empty_heap(Heap),
	list_to_chart(List, 0, Heap, Axioms, [], 0, _V, _SemInfo, []),
	all_active_rule_statistics1(Axioms).

% TODO: complete!

all_active_rule_statistics1([]).
all_active_rule_statistics1([item(F,_,_,_)|As]) :-
	rules_trigger(F, L, []),
	sort(L, Set),
	do_something_with(Set),
	all_active_rule_statistics1(As).

% = rules_trigger(+Formula, -ListOfInferences)
%
% given a formula, provide a list (with possible repetitions) of all special chart inference which we (may) need
% to apply (besides dl and dr) in order to complete the chart.

rules_trigger(lit(let)) -->
	!,
	[let],
	[wr].
rules_trigger(dl(0, lit(cl_r), dl(I, lit(s(_)), dr(0, lit(s(_)), lit(np(_,_,_)))))) -->
	{I > 0},
	[wr],
	[wr_a],
	[wpop],
	[e_endd],
	[dit_np],
	[se_dit].
rules_trigger(dl(I,_,dl(0,lit(np(_,_,_)),lit(s(_))))) -->
	{I > 0},
	!,
	[wr],
	[wr_a],
	[wpop],
	[a_dit],
	[e_endd],
	[dit_np],
	[se_dit].
rules_trigger(dl(I,_,dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(_)))))) -->
	{I > 0},
	!,
	[wr],
	[wr_a],
	[wpop],
	[e_endd],
	[a_dit],
	[a_dit_se],
	[dit_np],
	[se_dit].
rules_trigger(dl(I,_,_)) -->
	{I > 0},
	!,
	[wr],
	[wr_a],
	[wpop],
	[wpop_vp],
	[wpop_vpi].
rules_trigger(p(0,dl(I,_,_),_)) -->
	{I > 0},
	!,
	[prod_dr],
	[prod_i],
	[prod_e],
	[prod_wl].
rules_trigger(p(0,dl(0,_,_),_)) -->
	!,
	[prod_dr],
	[prod_i],
	[prod_e],
	[prod_c],
	[prod_cl].
rules_trigger(p(0,p(0,_,_),dl(I,_,_))) -->
	{I > 0},
	!,
	[prod_dr],
	[prod_i3],
	[prod_i],
	[prod_w],
	[prod_e],
	[prod_c],
	[prod_cl],
	[prod_wl].
rules_trigger(p(0,_,dl(I,_,_))) -->
	{I > 0},
	!,
	[prod_dr],
	[prod_i],
	[prod_e],
	[prod_w],
	[prod_c],
	[prod_cl].
rules_trigger(p(0,p(0,_,_),_)) -->
	!,
	[prod_dr],
	[prod_i3],
	[prod_i],
	[prod_e],
	[prod_c],
	[prod_cl],
	[prod_wl].
rules_trigger(p(0,_,_)) -->
	!,
	[prod_dr],
	[prod_i],
	[prod_e],
	[prod_c],
	[prod_cl],
	[prod_wl].
rules_trigger(box(_,dia(_,dl(0,lit(np(_,_,_)),lit(s(_)))))) -->
	!,
	[ef_start_iv],
	[gap_i].
rules_trigger(box(_,dia(_,dr(0,_,_)))) -->
	!,
	[ef_start],
	[gap_i],
	[gap_c],
	[gap_e].
rules_trigger(box(_,dia(_,_))) -->
	!,
	[ef_start],
	[gap_i].
rules_trigger(dr(0,A,dr(0,B,dia(_,box(_,_))))) -->
	!,
	[e_start],
	[e_end],
	[e_endd],
	rules_trigger(A),
	rules_trigger(B).
rules_trigger(dl(0,dr(0,A,dia(0,box(0,_))),B)) -->
	!,
	[e_start_l],
	[e_end_l],
	rules_trigger(A),
	rules_trigger(B).
rules_trigger(dr(0,A,dl(0,dia(0,box(0,lit(n))),lit(n)))) -->
	!,
	[e_end_r_lnr],
	[c_r_lnr],
	rules_trigger(A).
rules_trigger(dl(0,dl(0,dia(0,box(0,lit(n))),lit(n)),A)) -->
	!,
	[e_end_l_lnr],
	[c_l_lnr],
	rules_trigger(A).
% = recursive cases
rules_trigger(dr(_,A,B)) -->
	!,
	rules_trigger(A),
	rules_trigger(B).
rules_trigger(dl(_,A,B)) -->
	!,
	rules_trigger(A),
	rules_trigger(B).
rules_trigger(dia(_,A)) -->
	!,
	rules_trigger(A).
rules_trigger(box(_,A)) -->
	!,
	rules_trigger(A).
rules_trigger(_) -->
	!,
	[].
	
check_solution(Sem) :-
	final_item(Goal, Index, Sem),
	item_in_chart(Goal, Index),
	compute_proof(Index),
       	increase_global_counter('$SOLUTION').

check_solution(Index, Sem) :-
	final_item(Goal, Index, Sem),
	item_in_chart(Goal, Index),
	compute_proof(Index),
       	increase_global_counter('$SOLUTION').

init_agenda(Axioms, Agenda) :-
	empty_agenda(Empty),
	add_axioms_to_agenda(Axioms, Empty, Agenda),
	Agenda = queue(_, TotalForms),
	retractall(total_formulas(_)),
	assert(total_formulas(TotalForms)).

exhaust(queue(Front, Back)) :-
	Front >= Back,
	!,
	retractall(max_queue_size(_)),
	assert(max_queue_size(Front)).
exhaust(Agenda0) :-
	pop_agenda(Agenda0, Index, Agenda1),
	write(':'),
	add_consequences_to_agenda(Index, Agenda1, Agenda),
	exhaust(Agenda).

add_consequences_to_agenda(Index, Agenda0, Agenda) :-
	find_all_consequences(Index, Consequences),
	add_items_to_agenda(Consequences, Agenda0, Agenda).

find_all_consequences(Index, Consequences) :-
	findall(Weight-(Consequence-Justification),
		consequence(Index, Consequence, Justification, Weight),
		Consequences0),
	keysort(Consequences0, Consequences).

consequence(Index, Consequent, Justification, Weight) :-
	index_to_item(Index, Trigger),
	matching_rule(Trigger, Nth, RuleName, Others, Consequent, SideConds),
	items_in_chart(Others, Index, Indices0),
	hold(SideConds),
	insert_nth0(Nth, Index, Indices0, Indices),
	Justification =.. [RuleName|Indices],
	get_weight(Consequent, Weight).

items_in_chart([], _MaxIndex, []).
items_in_chart([Antecedent|Antecedents], MaxIndex, [Index|Indices]) :-
	item_in_chart(Antecedent, MaxIndex, Index),
	items_in_chart(Antecedents, MaxIndex, Indices).

% = variant for interactive parser

find_all_consequences1(Index, Consequences, Active) :-
	findall(Weight-(Consequence-Justification),
		consequence1(Index, Consequence, Active, Justification, Weight),
		Consequences0),
	keysort(Consequences0, Consequences).

consequence1(Index, Consequent, Active, Justification, Weight) :-
	index_to_item(Index, Trigger),
	matching_rule(Trigger, Nth, RuleName, Others, Consequent, SideConds),
	items_in_chart(Others, Indices0),
	/* items should still be active to allow a rule to trigger */
	/* exceptions are the auxiliary hypotheses of the gap rules */
   (
        RuleName = gap_i
   ->
        true
   ;
        RuleName = gap_c
   ->
        true
   ;
        RuleName = gap_e
   ->
        true
   ;
	ord_subset(Indices0, Active)
   ),
	hold(SideConds),
	insert_nth0(Nth, Index, Indices0, Indices),
	Justification =.. [RuleName|Indices],
	get_weight(Consequent, Weight).

items_in_chart([], []).
items_in_chart([Antecedent|Antecedents], [Index|Indices]) :-
	item_in_chart(Antecedent, Index),
	items_in_chart(Antecedents, Indices).


hold([]).
hold([Cond|Conds]) :-
	call(Cond),
	hold(Conds).


matching_rule(Trigger, Nth, RuleName, Others, Consequent, SideConds) :-
	active_rule(RuleName),
	inference(RuleName, Antecedent, Consequent, SideConds),
	nth0(Nth, Antecedent, Trigger, Others).


item_stored(Item, Index) :-
	Item = item(Formula, I, J, Data),
	calculate_key_index(Formula, Key, Index),
	stored(Index, Key, I, J, Formula, Data).

% = calculate_key_index(+Formula, +Index, -Key)
%
% true if Key is a unique hashkey for Formula. Since
% hashkeys are only defined for ground terms, this will
% return a variable (matching all keys) if Formula is not
% ground.

calculate_key_index(Formula0, Key, Index) :-
	simplify_formula(Formula0, Formula),
	ground(Formula),
	!,
        simplified_formula_to_key(Formula, Key),
        key_index(Key, Index).
calculate_key_index(_, _, _).


similar_item(Item, item(Formula, I, J, Data), IndexofSimilar) :-
	Item = item(Formula, I, J, _),
	simplify_formula(Formula, SForm),
	simplified_formula_to_key(SForm, Key),
	key_index(Key, IndexofSimilar),
	stored(IndexofSimilar, Key, I, J, Formula, Data).

subsumed_item(Item, Front, Back, Justif) :-
	similar_item(Item, OtherItem, IndexofSimilar),
	subsumes_item(OtherItem, Item, IndexofSimilar, Front, Back, Justif).

subsumes_item(item(F0, I0, J0, Data0), item(F, I, J, Data1), IndexofSimilar, _Front, Back, Justif) :-
	subsumes_chk(F0, F),
	subsumes_chk(I0, I),
	subsumes_chk(J0, J),
	subsumes_data(Data0, Data1, O),
	keep_maximum_item(O, IndexofSimilar, F0, F, I0, I, J0, J, Data0, Data1, Back, Justif).


subsumes_data(data(_,Sem0,Prob0,_,A0,B0,C0,D0),data(_,Sem,Prob,_,A,B,C,D), O) :-
	subsumes_list(A0, A),
	subsumes_list(B0, B),
	subsumes_chk(C0, C),
	subsumes_chk(D0, D),
	subsumes_chk(Sem0, Sem),
	compare(O, Prob0, Prob).

subsumes_list([], []).
subsumes_list([t(L,R,F0,_)|As], [t(L,R,F,_)|Bs]) :-
	subsumes_chk(F0, F),
	subsumes_list(As, Bs).

% If the old value is *identical* to the new one, but the weight is lower,
% then erase the old value and fail the subsumption test. Otherwise, succeed.

keep_maximum_item(<, IndexofSimilar, F0, F, I0, I, J0, J, Data, BetterData, Back, _Justif) :-
	F0 == F,
	I0 == I,
	J0 == J,
	!,
	Data = data(Pros0,Sem0,Prob0,_,_,_,_,_),
	BetterData = data(Pros1,Sem1,Prob,_,_,_,_,_),
	/* delete the lower weight item */
	retract(stored(IndexofSimilar, _H, I, J, F, Data)),
	retract(justification(IndexofSimilar, _)),
	/* add back pointer from deleted item to new, better item */
	assert(justification(IndexofSimilar, id(Back))),
	/* comment out retracts above and remove comments below to obtain a "packed chart" */
%	assertz(stored(IndexofSimilar, H, I, J, F, BetterData)),
%	assert(justification(IndexofSimilar, Justif)),
   (
        verbose
    ->
        \+ \+ (	    
	numbervars(Sem0, 0, _),
	reduce_sem(Sem0, RSem0),
	numbervars(Sem1, 0, _),
	reduce_sem(Sem1, RSem1),
        format('~NDELETED (~w < ~w): ~w~nDATA       :~p ~p~nBETTER DATA:~p ~p~n', [Prob, Prob0, IndexofSimilar,Pros0,RSem0,Pros1,RSem1]))
   ;
        true
   ),
        fail.
keep_maximum_item(=, _, _, _, _, _, _, _, _, _, _, _).
keep_maximum_item(>, _, _, _, _, _, _, _, _, _, _, _).

init_chart :-
	reset_global_counters,
	retractall(max_queue_size(_)),
	retractall(justification(_,_)),
	retractall(vp_left(_)),
	retractall(let_right(_)),
	retractall(word(_,_,_,_,_)),
	retractall(stored(_,_,_,_,_,_)),
	retractall(key_index(_,_)),
	write('-'),
	flush_output.

item_in_chart(Item, RefIndex, ItemIndex) :-
	item_stored(Item, ItemIndex),
    (
        ItemIndex =< RefIndex
    ->
        true
    ;
        !,
        fail
    ).

item_in_chart(Item, ItemIndex) :-
	item_stored(Item, ItemIndex).

item_in_chart(Item) :-
	item_stored(Item, _).

empty_agenda(queue(0,0)).

pop_agenda(queue(Front,Back), Front, queue(NewFront, Back)) :-
	Front < Back,
	NewFront is Front + 1.

% = update_data
%
% last-minute changes to the Data fields just before storage.

update_data(data(Pros0, Sem, Prob, H, As, Bs, Cs, Ds), I, J, Justification, data(Pros, Sem, Prob, H, As, Bs, Cs, Ds)) :-
	simplify_pros(Pros0, Justification, I, J, Pros),
	renumbervars(Sem).

% no simplification
% simplify_pros(Pros, _, _, _, Pros).

% = expand_data
%
% as update_data above, but reconstructs the Pros value based on its sim

expand_data(data(Pros0, Sem, Prob, H, As, Bs, Cs, Ds), Justification, data(Pros, Sem, Prob, H, As, Bs, Cs, Ds)) :-
	reconstruct_pros(Pros0, Justification, Pros).

% simplifies the prosody information present in the chart entries: instead of
% storing the complete term, which can get quite big, we only give the positions
% of the two immediate substrings; this means extra work when reconstructing
% the proof tree, but uses considerably less memory.
%
% TODO: it would be useful to adopt the same strategy for the semantic terms
% at some point, especially when using packed charts.

simplify_pros(p(0,p(0,Pros1,Pros2),Pros3), Justification, I, L, p(0,Result,K-L)) :-
	Justification =.. [_|List0],
	select(Index1, List0, List),
	get_stored(Index1, _, _, I, J, _, data(Pros1,_,_,_,_,_,_,_)),
	member(Index2, List),
	get_stored(Index2, _, _, J, L, _, data(ProsR,_,_,_,_,_,_,_)),
	!,
   (
        ProsR = p(0,Pros2,Pros3)
   ->
	pros_right(Pros2, K),
	pros_left(Pros3, K),
	pros_mid(ProsR, K),
        Result = p(0,I-J,J-K)
    ;
        Result = I-K
   ).
simplify_pros(p(Ind,ProsL,ProsR), Justification, I, K, p(Ind,I-J,J-K)) :-
	Justification =.. [_|List0],
	select(Index1, List0, List),
	get_stored(Index1, _, _, J, K, _, data(ProsR,_,_,_,_,_,_,_)),
	member(Index2, List),
	get_stored(Index2, _, _, I, J, _, data(ProsL,_,_,_,_,_,_,_)),
	!.
simplify_pros(_, _, I, K, I-K).

% = pros_left(+Prosodics, ?LeftEdge)
%
% true if LeftEdge is the leftmost position of Prosodics

pros_left(I-_, I).
pros_left(p(_,L,_), I) :-
	pros_left(L, I).


% = pros_right(+Prosodics, ?RightEdge)
%
% true if RightEdge is the rightmost position of Prosodics

pros_right(_-J, J).
pros_right(p(_,_,R), J) :-
	pros_right(R, J).

% = pros_mid(+Prosodics, ?Mid)
%
% true if, for a prosodic term of the form p(I,Left,Right), Mid
% is the point where Left and Right join; it is computed here by
% taking the right edge of Left, but could equivalently be
% computed by taking the left edge of Right.

pros_mid(p(_,L,_), M) :-
	pros_right(L, M).


% = reconstruct_pros(+Pros, +Justification, -FullPros)
%
% recover the full prosodic tree label using the derivation trace justification/2 and
% the words of the input sentence.
% Does nothing for unreduced prosody.
%
% NOTE: the current strategy is rather wasteful: it recomputes the subtrees instead of
% computing all useful subtrees in the chart just a single time.

reconstruct_pros(I-J, [], Pros) :-
	word(Pros, _, _, I, J),
	!.
reconstruct_pros(I-K, [Index0], Pros) :-
	!,
	get_stored(Index0, Index, _, I, K, _, data(ProsD,_,_,_,_,_,_,_)),
	justification(Index, Just),
	Just =.. [_|Args],
	reconstruct_pros(ProsD, Args, Pros).	
reconstruct_pros(I-K, [A|As], Pros) :-
	!,
	select(Index0, [A|As], Bs),
	get_stored(Index0, Index, _, J, K, _, data(ProsD,_,_,_,_,_,_,_)),
	justification(Index, Just),
	Just =.. [_|Args],
	reconstruct_pros(ProsD, Args, Pros1),
  (
        I = J
  ->
        /* the stored item spans the complete string */
        Pros = Pros1
  ;
        /* special case for prod_i3; the stored item spans only a postfix */
        /* we therefore compute the missing prefix from the other premisses */
        Pros = p(0,Pros0,Pros1),
        reconstruct_pros(I-J, Bs, Pros0)
  ).
        
reconstruct_pros(p(Ind,I-J,J-K), Args0, p(Ind,ProsL,ProsR)) :-
	!,
	select(Index1, Args0, Args),
	get_stored(Index1, Index11, _, I, J, _, data(ProsL0,_,_,_,_,_,_,_)),
	member(Index2, Args),
	get_stored(Index2, Index22, _, J, K, _, data(ProsR0,_,_,_,_,_,_,_)),
	justification(Index11, Just1),
	Just1 =.. [_|Args1],
	justification(Index22, Just2),
	Just2 =.. [_|Args2],
	reconstruct_pros(ProsL0, Args1, ProsL),
	reconstruct_pros(ProsR0, Args2, ProsR).
reconstruct_pros(Pros, _, Pros) :-
	format(user_error, '~N{Warning: failed to reconstruct prosodics for "~w"}~n', [Pros]).

% = get_stored(+Index, ?TrueIndex, ?Key, ?L, ?R, ?Form, ?Data)
%
% as stored/6, but follows any back pointers from Index (these indicate an item which
% has been superseded) to TrueIndex.
% This predicate only works correctly when Index is instantiated (ie. it cannot be used
% to enumerate the stored/6 database by leaving Index a variable).

get_stored(Index0, Index, Key, L, R, Form, Data) :-
	/* back pointer found, follow it */
	justification(Index0, id(Index1)),
	!,
	get_stored(Index1, Index, Key, L, R, Form, Data).
get_stored(Index, Index, Key, L, R, Form, Data) :-
	stored(Index, Key, L, R, Form, Data).

% = coherent_item(+Left, +Right, +Data)
%
% checks if the two extraction stacks are coherent with respect to the string
% position of the candidate item: it checks whether the formulas licensing
% extraction whose extracted elements have 

coherent_item(I, J, data(_, _, _, _, _, _, Cs, Ds)) :-
	coherent_item_right(Ds, I),
	coherent_item_left(Cs, J).

% I---item---J  [R-_|_]
% Requirement: R =< I

coherent_item_right([], _).
coherent_item_right([J0-_|_], J) :-
	J0 =< J.

% [L-_|_]  I---item---J ... Licensor
% Requirement: L =< J

coherent_item_left([], _).
coherent_item_left([I0-_|_], I) :-
	I0 =< I.


add_item_to_agenda(Item0, Justification, queue(Front,Back), queue(Front, NewBack)) :-
	Item0 = item(F, I, J, Data0),
        update_data(Data0, I, J, Justification, Data),
	Item = item(F, I, J, Data),
	write('.'),
    (   coherent_item(I, J, Data),
        \+ subsumed_item(Item, Front, Back, Justification)
    ->
        simplify_formula(F, SF),
        simplified_formula_to_key(SF, Key),
        assertz(stored(Back, Key, I, J, F, Data)),
        assert(key_index(Key, Back)),
        assertz(justification(Back, Justification)),
        NewBack is Back + 1
    ;
        NewBack = Back
    ).

add_items_to_agenda([], Agenda, Agenda).
add_items_to_agenda([_-(Item-Justification)|Items], Agenda0, Agenda) :-
	!,
	add_item_to_agenda(Item, Justification, Agenda0, Agenda1),
	add_items_to_agenda(Items, Agenda1, Agenda).

add_axioms_to_agenda([], Agenda, Agenda).
add_axioms_to_agenda([Item|Items], Agenda0, Agenda) :-
	add_item_to_agenda(Item, axiom, Agenda0, Agenda1),
	add_axioms_to_agenda(Items, Agenda1, Agenda).

add_axioms_to_chart([], N, N, []).
add_axioms_to_chart([Item|Items], N0, N, [N0|Ls]) :-
	add_item_to_agenda(Item, axiom, queue(N0,N0), _),
	N1 is N0 + 1,
	add_axioms_to_chart(Items, N1, N, Ls).


index_to_item(Index, item(F, I, J, Sem)) :-
	get_stored(Index, _, _, I, J, F, Sem).

final_item(item(Start,0,Length,D), Best, BestSem) :-
	sentence_length(Length),
	has_empty_stack(D),
	findall(W-(Index-appl(SemI,Sem)),(startsymbol(Start, SemI),item_in_chart(item(Start,0,Length,D), Index),get_data_weight(D,W),get_data_semantics(D,Sem)), Solutions),
	get_best_solution(Solutions, Best, BestSem).


get_best_solution([W0-(Best0-BestSem0)|Rest], Best, BestSem) :-
	get_best_solution(Rest, W0, Best0, Best, BestSem0, BestSem).

get_best_solution([], _, Best, Best, BestSem, BestSem).
get_best_solution([W1-(Best1-BestSem1)|Rest], W0, Best0, Best, BestSem0, BestSem) :-
   (
	W1 > W0
   ->
	get_best_solution(Rest, W1, Best1, Best, BestSem1, BestSem)
   ;			    
	get_best_solution(Rest, W0, Best0, Best, BestSem0, BestSem)		     
   ).
			 
simplified_formula_to_key(SimplifiedFormula, Key) :-
	term_hash(SimplifiedFormula, Key).

% =

output_rule_statistics :-
	rule_counts_init(Counts0),
	findall(X, (justification(_,T),functor(T,X,_)), List),
	msort(List, MList),
	update_counts(MList, Counts0, Counts),
	print_counts(Counts).

update_counts([], Counts, Counts).
update_counts([A|As], [R-N0|Rs], Counts0) :-
    (
	A = R
    ->
        N is N0 + 1,
        update_counts(As, [R-N|Rs], Counts0)
    ;
        Counts0 = [R-N0|Counts],
        update_counts([A|As], Rs, Counts)
    ).

print_counts([C|Cs]) :-
	print_counts(Cs, C).

print_counts([], _-C) :-
	format(plog, '~w~n', [C]).
print_counts([C|Cs], _-C0) :-
	format(plog, '~w\t', [C0]),
	print_counts(Cs, C).

% ==============================================
% =               compute proofs               =
% ==============================================

% reconstruct proofs from chart output

% = compute_proof

compute_proof(Index) :-
	 output_proofs(Type),
     (
	 proof_type(Type, Pred)
     ->
         compute_proof(Index, Pred)
     ;
         /* default to not producing a proof */
         true
     ).

% each proof type defines a proof transformation to be used on the
% chart proof; the "chart" option outputs the chart proof directly.

proof_type(chart, =).
proof_type(nd, transform_proof).

compute_proof(Index, Pred) :-
	check_proof_stream,
	check_prolog_proof_stream,
	check_xml_stream,
	check_plog_stream,
	current_sentence(Sent),
	compute_chart_proof(Index, Proof),
	!,
        proof_axioms(Proof, Axioms, []),
	retractall('$PROOFAXIOMS'(_)),
	assert('$PROOFAXIOMS'(Axioms)),
	call(Pred, Proof, TrueProof),
	print_proof(Sent, TrueProof, pl_proof),
	xml_proof(Sent, TrueProof, xml),
	format(proof, '~n\\begin{multline}~n', []),
	latex_proof(TrueProof, proof),
	format(proof, '~n\\end{multline}~2n', []).

compute_proof(Index, _) :-
    (
	current_predicate('$CHART_CURRENT'/1)
    ->
        '$CHART_CURRENT'(CUR)
    ;
        CUR = 1
    ),
	format(proof, '~n% FAILED to compute proof for index ~w (~w)~n', [CUR,Index]),
	format(log, '~nFAILED to compute proof for index ~w (~w)~n', [CUR,Index]),
	format('~nFAILED to compute proof for index ~w (~w)~n', [CUR,Index]),
	fail.	

compute_chart_proof(Index0, rule(Rule,Pros,Formula-Sem,Prems)) :-
	get_stored(Index0, Index, Idf, _L, _R, Formula, data(Pros0,Sem,_,_,_,_,_,_)),
	justification(Index, Just),
	Just =.. [Rule|Args0],
	sort_args(Args0, Args),
	reconstruct_pros(Pros0, Args, Pros),
	compute_chart_proof_list(Args0, List, KeyList),
	keysort(KeyList, PList),
	strip_keys(PList, Prems),
   (
        List = []
   ->
        true
   ;
	unify_rule(Rule, Index, Idf, Args0, Formula, List)
   ).

% = proof_axioms(+Proof, -AxiomsDL)
%
% TODO: remove auxiliary rule premisses, which are now counted double!

proof_axioms(rule(axiom,Pros,Formula-_,[])) -->
	!,
	[Pros-Formula].
proof_axioms(rule(_,_,_,List)) -->
	list_axioms(List).

list_axioms([]) -->
	[].
list_axioms([P|Ps]) -->
	proof_axioms(P),
	list_axioms(Ps).

unify_rule(RuleName, Index, Idf, Just, Formula, List) :-
	inference(RuleName, Antecedent, item(Formula,L,R,Data), _SideConds),
	get_stored(Index, _, Idf, L, R, Formula, Data),
	match_antecedent(Just, Antecedent, List),
	!.

match_antecedent([], [], []).
match_antecedent([I|Is], [item(Formula,L,R,Data0)|As], [rule(_Rule,_Pros,Formula-Sem,_)|Rs]) :-
	get_stored(I, _, _, L, R, Formula, Data0),
	Data0 = data(_, Sem, _, _, _, _, _, _),
	match_antecedent(Is, As, Rs).

compute_chart_proof_list([], [], []).
compute_chart_proof_list([I|Is], [P|Ps], [K-P|KPs]) :-
	get_stored(I, _, _, K, _, _, _),
	compute_chart_proof(I, P),
	compute_chart_proof_list(Is, Ps, KPs).

sort_args(As, Ss) :-
	add_keys(As, Ks0),
	keysort(Ks0, Ks),
	strip_keys(Ks, Ss).

add_keys([], []).
add_keys([I|Is], [K-I|Ks]) :-
	get_stored(I, _, _, K, _, _, _),
	add_keys(Is, Ks).

% ==============================================
% =              chart inferences              =
% ==============================================

% = application rules

inference(dr, [item(dr(M,X,Y), I, J, Data1), item(Y, J, K, Data2)],
	       item(X, I, K, Data),
               [functor_head_r(X,Y,H),application_r(M, I, K, H, Data1, Data2, Data),check_wrap(Y, J, K, Data2)]).

inference(dl, [item(Y, I, J, Data1), item(dl(M,Y,X), J, K, Data2)],
	       item(X, I, K, Data),
               [functor_head_l(Y,X,H), check_islands(Y,Data2), application_l(M, I, K, H, Data2, Data1, Data)]).


% = rules for skipping interpunction symbols

inference(let, [item(X, I, J, Data1),item(lit(let), J, K, Data2)],
	        item(X, I, K, Data),
	       [X\=lit(let),sentence_length(K),combine_let_l(I,K,Data1,Data2,Data)]).
inference(let, [item(lit(let), I, J, Data2),item(X, J, K, Data1)],
	        item(X, I, K, Data),
	        [K is J + 1,combine_let_r(I,K,Data2,Data1,Data)]).  % prevent "attachment ambiguity"

% = wrapping rules

% push the item on the stack

% TODO: cependant (and some other adverbs) are always treated as "parenthetical"

inference(wr, [item(lit(let), I, J, Data1), item(dl(1,V,V), J, K, Data2)],
	       item(lit(let), I, K, Data),
              [wrap(dl(1,V,V), I, J, K, Data1, Data2, Data)]).

inference(wr_a, [item(X, I, J, Data1),item(dl(1,V,V), J, K, Data2)],
	         item(X, I, K, Data),
                [wrappable(X, V, I, J), wrap_arg(dl(1,V,V), I, J, K, Data1, Data2, Data)]).

inference(wpop, [item(X, I0, J0, Data0)],
	         item(Y, I, J, Data),
	        [pop(dl(1,X,Y), I0, J0, I, J, Data0, Data)]).

% special rule to allow an s-modifier to modify a vp

inference(wpop_vp, [item(dl(0,lit(np(A,B,C)),lit(s(S))), I0, J0, Data0)],
	            item(dl(0,lit(np(A,B,C)),lit(s(S))), I, J, Data),
	           [pop_vp(I0, J0, I, J, Data0, Data)]).
inference(wpop_vp, [item(dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(S)))), I0, J0, Data0)],
	            item(dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(S)))), I, J, Data),
	           [pop_vpc(I0, J0, I, J, Data0, Data)]).
inference(wpop_vp, [item(dl(0,lit(cl_y),dl(0,lit(np(A,B,C)),lit(s(S)))), I0, J0, Data0)],
	            item(dl(0,lit(cl_y),dl(0,lit(np(A,B,C)),lit(s(S)))), I, J, Data),
	           [pop_vpc(I0, J0, I, J, Data0, Data)]).
inference(wpop_vp, [item(dl(1,lit(s(S)),lit(s(S))), I0, J0, Data0)],
	            item(dl(1,lit(s(S)),lit(s(S))), I, J, Data),
	           [vp_left(I0),pop_vp_strict(I0, J0, I, J, Data0, Data)]).

inference(wpop_vpi, [item(dl(0,lit(np(A,B,C)),lit(s(S))), J, K, Data0),
		     item(dl(1,lit(s(S)),lit(s(S))), I, J, Data1)],
	             item(dl(0,lit(np(A,B,C)),lit(s(S))), I, K, Data),
	            [adv_vp(I, J, K, Data1, Data0, Data)]).
inference(wpop_vpi, [item(dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(S)))), J, K, Data0),
		     item(dl(1,lit(s(S)),lit(s(S))), I, J, Data1)],
	             item(dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(S)))), I, K, Data),
	            [adv_vpc(I, J, K, Data1, Data0, Data)]).
inference(wpop_vpi, [item(dl(1,lit(s(S2)),dl(0,lit(np(A,B,C)),lit(s(S)))), J, K, Data0),
		     item(dl(1,lit(s(S)),lit(s(S))), I, J, Data1)],
	             item(dl(1,lit(s(S2)),dl(0,lit(np(A,B,C)),lit(s(S)))), I, K, Data),
	            [adv_vpc(I, J, K, Data1, Data0, Data)]).

% = special rules for reported speech of the form "SENT, a dit NP"

inference(a_dit, [item(dr(0,X,dl(0,lit(np(A,B,C)),lit(s(INFL)))), I, J, Data1),
		  item(dl(Ind,Y,dl(0,lit(np(A,B,C)),lit(s(INFL)))), J, K, Data2)],
	          item(dl(Ind,Y,X), I, K, Data),
	         [a_dit(I, K, Data1, Data2, Data)]).
inference(a_dit_se, [item(dr(0,X,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(INFL))))), I, J, Data1),
		     item(dl(1,Y,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(INFL))))), J, K, Data2)],
	             item(dl(1,Y,X), I, K, Data),
	            [a_dit(I, K, Data1, Data2, Data)]).
inference(dit_np, [item(dl(1,Y,dr(0,lit(s(S)),lit(np(A,B,C)))), I, J, Data1),
	 	   item(lit(np(A,B,C)), J, K, Data2)],
	           item(dl(1,Y,lit(s(S))), I, K, Data),
	          [dit_np(I, K, Data1, Data2, Data)]).
inference(se_dit, [item(dl(1,Y,dl(0,lit(cl_r),X)), J, K, Data2),
		   item(lit(cl_r), I, J, Data1)],
	           item(dl(1,Y,X), I, K, Data),
	          [dit_np(I, K, Data1, Data2, Data)]).

% = right-extraction rules

% = argument extraction

inference(e_start, [item(dr(0,_,dr(0,_,dia(Ind,box(Ind,Y)))),K0,K,_),
		    item(dr(0,X,Y), I, J, Data0)],
		    item(X, I, J, Data),
		   [K=<I,no_island_violation(Ind,X,Y),start_extraction(Y, J, K0, K, Data0, Data)]).
inference(e_end, [item(dr(0,X,dr(0,Y,dia(Ind,box(Ind,Z)))), I, J, Data0),
		  item(Y, J, K, Data1)],
	          item(X, I, K, Data),
	         [check_extraction(Ind,K0,K),end_extraction(Z, I, J, K0, K, Data0, Data1, Data)]).

inference(e_endd, [item(dr(0,X,dr(0,Y,dia(Ind,box(Ind,Z)))), I, J, Data0),
		  item(dl(1,V,Y), J, K, Data1)],
	          item(dl(1,V,X), I, K, Data),
	         [check_extraction(Ind,K0,K),end_extraction(Z, I, J, K0, K, Data0, Data1, Data)]).


inference(e_start_l, [item(dl(0,dr(0,_,dia(0,box(0,Y))),_),K,L,data(_,_,Prob0,_,[],[],[],[])),
		      item(dr(0,X,Y), I, J, Data0)],
		      item(X, I, J, Data),
		     [J=<K,no_island_violation(0,X,Y),start_extraction_l(Y, J, K, L, Prob0, Data0, Data)]).
inference(e_end_l, [item(dl(0,dr(0,Y,dia(0,box(0,Z))),X), J, K, Data0),
		    item(Y, I, J, Data1)],
	            item(X, I, K, Data),
	           [end_extraction_l(Z, J, J, I, K, Data0, Data1, Data)]).


% = left-node raising (only for sequences of adjectives, quite rare)

inference(e_end_l_lnr, [item(dl(0,dl(0,dia(0,box(0,lit(n))),lit(n)),Z), J, K, Data1),
			item(dl(0,lit(n),lit(n)), I, J, Data0)],
	                item(Z, I, K, Data),
	               [application_l(0,I,K,f,Data1,Data0,Data)]).

inference(e_end_r_lnr, [item(dr(0,Z,dl(0,dia(0,box(0,lit(n))),lit(n))), I, J, Data0),
			item(dl(0,lit(n),lit(n)), J, K, Data1)],
	                item(Z, I, K, Data),
	               [application_r(0,I,K,f,Data0,Data1,Data)]).

inference(c_l_lnr, [item(dl(0,dl(0,dia(0,box(0,lit(n))),lit(n)),_), K, _, data(_,_,Prob0,_,_,_,_,_)),
		    item(dl(0,lit(n),lit(n)), J, K, data(Pros1,Sem1,Prob1,H1,SetA1,SetB1,SetC1,SetD1)),
		    item(dl(0,lit(n),lit(n)), I, J, data(Pros2,Sem2,Prob2,_H2,SetA2,SetB2,SetC2,SetD2))],
	            item(dl(0,lit(n),lit(n)), I, K, data(Pros, lambda(X,appl(Sem1,appl(Sem2,X))), Prob, H1, SetA, SetB, SetC, SetD)),
	           [Pros=p(0,Pros2,Pros1),
		    combine_sets(SetA1, SetB1, SetC1, SetD1, SetA2, SetB2, SetC2, SetD2, SetA, SetB, SetC, SetD),
		    combine_probability(Prob1, Prob2, I,K ,c_l_lnr, Prob3),
		    Prob is Prob0 + Prob3]).

inference(c_r_lnr, [item(dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n))), _, I, data(_,_,Prob0,_,_,_,_,_)),
		    item(dl(0,lit(n),lit(n)), J, K, data(Pros1,Sem1,Prob1,H1,SetA1,SetB1,SetC1,SetD1)),
		    item(dl(0,lit(n),lit(n)), I, J, data(Pros2,Sem2,Prob2,_H2,SetA2,SetB2,SetC2,SetD2))],
	            item(dl(0,lit(n),lit(n)), I, K, data(Pros, lambda(X,appl(Sem2,appl(Sem1,X))), Prob, H1, SetA, SetB, SetC, SetD)),
	           [Pros=p(0,Pros2,Pros1),
		    combine_sets(SetA1, SetB1, SetC1, SetD1, SetA2, SetB2, SetC2, SetD2, SetA, SetB, SetC, SetD),
		    combine_probability(Prob1, Prob2, I, K, c_r_lnr, Prob3),
		    Prob is Prob0 + Prob3]).

% = product rules

inference(prod_e, [item(p(0,dr(0,X,Y),dia(0,box(0,Y))), I, J, data(Pros0,Sem0,Prob,H,SetA,SetB,SetC,SetD))],
	           item(X, I, J, data(Pros,appl(pi1(Sem0),pi2(Sem0)),Prob,H,SetA,SetB,SetC,SetD)),
	          [Pros=Pros0]).
inference(prod_i, [item(X, I, J,   data(Pros0,Sem0,Prob0,H0,[],[],[],[])),
		   item(Y, J, K,   data(Pros1,Sem1,Prob1,_H1,[],[],[],[])),
		   item(F, I0, J0, data(_    ,_   ,Prob2,_,_,_,_,_))],
	           item(p(0,X,Y), I, K, data(Pros,pair(Sem0,Sem1), Prob, H0, [], [], [], [])),
	          [Pros=p(0,Pros0,Pros1),
		   prod_formula(F,p(0,X,Y), I0, J0, I, K),
		   combine_probability(Prob0, Prob1, I, K, prod_i, Prob3),
		   Prob is Prob2 + Prob3]).
inference(prod_i3, [item(X, I, J, data(Pros0,Sem0,Prob0,H0,SetA0,SetB0,SetC0,SetD0)),
		    item(Y, J, K, data(Pros1,Sem1,Prob1,_H1,SetA1,SetB1,SetC1,SetD1)),
		    item(dl(1,lit(s(S)),lit(s(S))), K, L, data(Pros2,Sem2,Prob2,_H2,SetA2,SetB2,SetC2,SetD2)),
		    item(dl(0,p(0,p(0,X,Y),dl(1,lit(s(S)),lit(s(S)))),_), L, _, _)],
	            item(p(0,p(0,X,Y),dl(1,lit(s(S)),lit(s(S)))), I, L, data(Pros,pair(pair(Sem0,Sem1),Sem2), Prob, H0, SetA, SetB, SetC, SetD)),
	           [Pros=p(0,p(0,Pros0,Pros1),Pros2),
		    combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA3, SetB3, SetC3, SetD3),
		    combine_sets(SetA2, SetB2, SetC2, SetD2, SetA3, SetB3, SetC3, SetD3, SetA, SetB, SetC, SetD),
		    combine_probability(Prob0, Prob1, I, K, prod_i, Prob3),
		    combine_probability(Prob2, Prob3, I, L, prod_i, Prob)]).
inference(prod_w, [item(p(0,X,dl(1,Y,Z)), I, J, data(Pros0,Sem,Prob,H,SetA,SetB,SetC,SetD))],
	           item(X, I, J, data(Pros,pi1(Sem),Prob,H,SetA,[t(I,J,dl(1,Y,Z),pi2(Sem))|SetB],SetC,SetD)),
		  [Pros=Pros0]).
inference(prod_wl, [item(p(0,dl(1,Y,Z),dia(0,box(0,X))), I, J, data(Pros0,Sem,Prob,H,SetA,SetB,SetC,SetD))],
	           item(X, I, J, data(Pros,pi2(Sem),Prob,H,SetA,[t(I,J,dl(1,Y,Z),pi1(Sem))|SetB],SetC,SetD)),
		  [Pros=Pros0]).
inference(prod_c, [item(dr(0,X,Y),              I, J, data(Pros1,Sem1,Prob0,H1,SetA0,SetB0,SetC0,SetD0)),
		   item(p(0,Y,dia(0,box(0,Z))), J, K, data(Pros2,Sem2,Prob1,_H2,SetA1,SetB1,SetC1,SetD1))],
	           item(p(0,X,dia(0,box(0,Z))), I, K, data(Pros,pair(appl(Sem1,pi1(Sem2))),Prob,H1,SetA,SetB,SetC,SetD)),
	          [Pros=p(0,Pros1,Pros2),
		   combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD),
		   combine_probability(Prob0, Prob1, I, K, prod_c, Prob)]).
inference(prod_cl, [item(X, I, J, data(Pros1,Sem1,Prob0,_H0,SetA0,SetB0,SetC0,SetD0)),
		    item(p(0,dl(0,X,Y),dia(0,box(0,Z))), J, K, data(Pros2,Sem2,Prob1,H1,SetA1,SetB1,SetC1,SetD1))],
	            item(p(0,Y,dia(0,box(0,Z))), I, K, data(Pros,pair(appl(Sem1,pi1(Sem2))),Prob,H1,SetA,SetB,SetC,SetD)),
	           [Pros=p(0,Pros1,Pros2),
		    combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD),
		    combine_probability(Prob0, Prob1, I, K, prod_cl, Prob)]).

inference(prod_dr, [item(dr(0,X,p(0,Y,Z)), I, J, Data1),
		    item(p(0,Y,dia(0,box(0,Z))), J, K, Data2)],
	            item(X, I, K, Data),
                   [application_r(0, I, K, f, Data1, Data2, Data)]).

% = gapping

% = functor extraction (used for gapping)
inference(ef_start, [item(dr(0,_,dr(0,_,dia(Ind,box(Ind,dr(0,X,Y))))), K0, K, _),
		     item(Y, I, J, Data0)],
		     item(X, I, J, Data),
		    [K=<I,start_extraction_inv(dr(0,X,Y), J, K0, K, Data0, Data)]).
% = intransitive verb gap, very rare (1 occurence in treebank)
% (formulated this way to avoid n\n traces selecting explicit arguments)
inference(ef_start_iv, [item(dr(0,_,dr(0,_,dia(Ind,box(Ind,dl(0,lit(np(A,B,C)),lit(s(S))))))),K0,K,_),
			item(lit(np(A,B,C)), I, J, Data0)],
		        item(lit(s(S)), I, J, Data),
		       [K=<I,start_extraction_inv(dl(0,lit(np(A,B,C)),lit(s(S))), J, K0, K, Data0, Data)]).
% base case:
inference(gap_i, [item(dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X)))), K0, K, Data0),
		  item(X, I, J, Data1),
		  item(lit(s(S)), I0, K0, Data2)],
	          item(lit(s(S)),I0, K, Data),
	         [I0=<I,J=<K0,combine_gap(I0,K,Data1,Data2,Data0,Data)]).
% complex gap:
% (start extraction from licensor at position 0)
inference(gap_c, [item(dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,dr(0,X,Y)))),dr(0,lit(s(S)),box(Ind,dia(Ind,dr(0,X,Y))))), K, L, data(_,_,Prob0,_,[],[],[],[])),
		  item(dr(0,Z,Y), I, J, Data0)],
	          item(Z, I, J, Data),
	  [J=<K,start_extraction_l0(Y, J, K, L, Prob0, Data0, Data)]).
% require empty stacks for gap_e to avoid strange scopings
inference(gap_e, [item(dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,dr(0,X,Y)))),dr(0,lit(s(S)),box(Ind,dia(Ind,dr(0,X,Y))))), K, L, data(_,_,Prob0,_,[],[],[],[])),
		  item(X, I, J, data(Pros0,Sem,Prob1,H,[],[],SetCs0,[]))],
	          item(dr(0,X,Y), I, J, data(Pros,lambda('$VAR'(K),Sem),Prob,H,[],[],[],[])),
	         [J=<K,Pros0=Pros,select(0-t(Y,J,K,L,'$VAR'(K)), SetCs0, []), Prob is Prob0 + Prob1]).

% ==============================================
% =            auxiliary predicates            =
% ==============================================

% These predicates verify side conditions on the different inference rules

combine_gap(I, J, data(_    , Term0, Prob0, _ , _  , _  , _  , _  ),   % extracted functor
	          data(Pros1, Term1, Prob1, _ , [], [], [], []),       % result sentence
	          data(Pros2, Term2, Prob2, H2, As, Bs, Cs, Ds),       % gapping "licensor"
	          data(p(0,Pros1,Pros2),appl(appl(Term2,lambda(X,TermX)),Term0), Prob, H2, As, Bs, Cs, Ds)) :-
	/* in the result semantics Term1, replace functor semantics Term0 by a fresh variable X */
	/* require Term0 to be closed to prevent accidental capture */
	is_closed(Term0),
	update_semantics(Term1, Term0, X, TermX),
	!,
	combine_probability(Prob1, Prob2, I, J, gap_i, Prob3),
	Prob is Prob0 + Prob3.


update_semantics(Term1, Term0, X, TermX) :-
	/* simple case: replace Term0 by X */
	replace_sem(Term1, Term0, X, TermX),
	subterm(TermX, X).
update_semantics(Term1, Term0, X, TermX) :-
	/* gap_e/gap_c combination */
	Term1 = Term10,
	melt_bound_variables(Term0, Term00),
	Term00 = lambda(Var, TermV),
	var(Var),
  (	
        subterm_with_unify(Term10, TermV)
  ->
        try_unify_semantics(Term00, Term0),
	replace_sem(Term1, TermV, appl(X,Var), TermX)
  ).

% = is_clitic(+Formula)
%
% true if Formula is a clitic

is_clitic(cl_r).
is_clitic(cl_y).

% = is_clitic(+Word, +POStag)
%
% true if Word-POStag is a reflexive clitic (only se/me for now)

is_clitic('s\'', pro:per).
is_clitic(se, pro:per).
is_clitic('s\'', clr-pro:per).
is_clitic(se, clr-pro:per).
is_clitic(me, clo-pro:per).
is_clitic(me, pro:per).

% = 

parenthetical(J) :-
	let_right(J).

interpunction(ponct-pun:cit).
interpunction(ponct-pun).
interpunction(ponct).
interpunction(pun).
interpunction(pun:cit).

assert_if_verb(Pos, Index) :-
    (
	is_verb(Pos)
    ->
        assert(vp_left(Index)),
        move_vp_left(Index)
    ;
        true
    ).

move_vp_left(Index) :-
	Prev is Index - 1,
    (
        let_right(Index)
    ->
        assert(vp_left(Prev)),
        move_vp_left(Prev)
    ;
	word(Word, Pos, _, Prev, Index),
        is_clitic(Word, Pos)
    ->
        assert(vp_left(Prev)),
        move_vp_left(Prev)
    ;
        true
    ).

assert_if_let(Pos, Index) :-
    (
        is_let(Pos)
    ->
        assert(let_right(Index))
    ;
        true
    ).


is_verb(V-ver:_) :-
	is_verb(V).
is_verb(ver:_).
is_verb(v).
is_verb(vpp).
is_verb(vpr).
is_verb(vinf).
is_verb(vs).
is_verb(vimp).

is_let(ponct-pun:cit).
is_let(ponct-pun).
is_let(pun:cit).
is_let(pun).
is_let(ponct).

% = prod_formula
%
% formula licensing prod_i rule

prod_formula(dr(0,_,p(0,X,Y)), p(0,X,Y), _, J, J, _).
prod_formula(dl(0,p(0,X,Y),_), p(0,X,Y), I, _, _, I).


% = integrated adverbs can only wrap among other arguments
% (note: past participles are excluded here).

% don't wrap if application is possible
wrappable(lit(s(_)), _, _, _) :-
	!,
	fail.
% don't argument wrap an integrated adverb
wrappable(_, _, _, J) :-
	I is J - 1,
	word(_, Pos, _, I, J),
	interpunction(Pos),
	!,
	fail.
wrappable(F, G, I, _) :-
	vp_left(I),
	!,
	wrappable(F, G).
wrappable(F, G, _, _) :-
	other_wrappable(F, G).


other_wrappable(dl(0,lit(np(_,_,_)),lit(s(S))), lit(s(S))) :-
	S \== main,
	S \== pass.
other_wrappable(dl(0,lit(n),lit(n)), dl(0,lit(n),lit(n))).
other_wrappable(dr(0,X,lit(np(_,_,_))), Y) :-
	other_wrappable(X, Y).
other_wrappable(dr(0,X,lit(pp(_))), Y) :-
	other_wrappable(X, Y).
other_wrappable(dr(0,X,lit(s(q))), Y) :-
	other_wrappable(X, Y).
other_wrappable(dr(0,X,dl(0,lit(np(_,_,_)),lit(s(_)))), Y) :-
	other_wrappable(X, Y).
%other_wrappable(dr(0,X,dl(0,n,n)), Y) :-
%	other_wrappable(X, Y).

wrappable(dl(0,lit(n),lit(n)), dl(0,lit(n),lit(n))).
wrappable(dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lit(pp(_))), dl(0,lit(np(_,_,_)),lit(np(_,_,_)))).
wrappable(dl(0,lit(np(_,_,_)),lit(s(_))), lit(s(_))) :- !.
wrappable(dl(0,dl(0,lit(n),lit(n)),lit(s(_))), lit(s(_))) :- !.
wrappable(dl(0,dl(0,lit(np(_,_,_)),lit(s(S))),lit(s(_))), lit(s(_))) :-
	nonvar(S),
	functor(S, inf, 1),
	!.
wrappable(dl(0,lit(np(_,_,_)),lit(s(_))), dl(0,lit(np(_,_,_)),lit(s(_)))) :- !.
wrappable(lit(s(_)), lit(s(_))) :- !.
wrappable(dl(0,lit(CL),X), Y) :-
    (
         CL == cl_y
    ;
         CL == cl_r
    ),
	 !,
	 wrappable(X, Y).
wrappable(dl(1,lit(s(_)),dr(0,lit(s(_)),lit(np(_,_,_)))), lit(s(_))).
wrappable(dl(1,lit(s(_)),X), Y) :-
	wrappable(X, Y).
wrappable(dr(0,X,lit(np(_,_,_))), Y) :-
	!,
	wrappable(X, Y).
wrappable(dr(0,X,dl(0,lit(n),lit(n))), Y) :-
	!,
	wrappable(X, Y).
wrappable(dr(0,X,lit(pp(_))), Y) :-
	!,
	wrappable(X, Y).
wrappable(dr(0,X,lit(s(S))), Y) :-
    (
        S == q
    ;
        S == whq
    ),
	!,
	wrappable(X, Y).
wrappable(dr(0,X,dl(0,lit(np(_,_,_)),lit(s(inf(_))))), Y) :-
	!,
	wrappable(X, Y).

% = no_island_violation(+Mode, +Formula, +Gap)
%
% true if Formula/Gap is a valid point for extraction of Gap

no_island_violation(1, Formula, Gap) :-
	island_violation(Formula, Gap),
	!,
	fail.
no_island_violation(_, _, _).

% = island_violation(+Formula, +Gap)
%
% true if Gap cannot be extracted from Formula/Gap

% this is right for adjectivally used prepositional phrases, but maybe not for eg. passives: VERIFY!
island_violation(dl(0,lit(n),lit(n)), lit(np(_,_,_))).
island_violation(dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lit(np(_,_,_))).
island_violation(lit(np(_,_,_)), lit(np(_,_,_))).
island_violation(lit(pp(_)), lit(np(_,_,_))).
island_violation(dl(1,lit(s(_)),lit(s(_))), lit(np(_,_,_))).
% "il y a"
island_violation(dl(0,lit(cl_y),dl(0,lit(np(_,_,_)),dl(1,lit(s(_)),lit(s(_))))), lit(np(_,_,_))).
island_violation(dr(0,lit(s(_)),lit(s(_))), lit(np(_,_,_))).

check_extraction(0, K, K).
check_extraction(1, _, _).

% np island constraint

check_islands(dl(0,lit(np(_,_,_)),lit(s(S))), Data) :-
        ( S = main ; S = ppart ; S = ppres ),
	!,
	Data = data(_, _, _, _, As, [], _, _),
	check_islands1(As).
check_islands(lit(n), Data) :-
	!,
	Data = data(_, _, _, _, As, Bs, _, _),
	check_islands1(As),
	check_islands1(Bs).
check_islands(lit(np(_,_,_)), Data) :-
	!,
	Data = data(_, _, _, _, _, [], _, _).
check_islands(lit(pp(_)), Data) :-
	!,
	/* no extraction of np's out of a pp */
	Data = data(_, _, _, _, _, _, [], []).
check_islands(_, _).

check_islands1([]).
check_islands1([t(_, _, F, _)|As]) :-
    (
        F = dl(1, dl(0, lit(n), lit(n)), dl(0, lit(n), lit(n)))
    ->
	fail
    ;
        F = dl(1, dl(0, lit(np(_,_,_)), lit(np(_,_,_))), dl(0, lit(np(_,_,_)), lit(np(_,_,_))))
    ->
        fail
    ;
        check_islands1(As)
    ).


check_wrap(dl(0,lit(n),lit(n)), _, _, data(_,_,_,_,As,[],_,_)) :-
	!,
	check_islands1(As).
check_wrap(lit(s(S)), _, _, data(_,_,_,_,As,Bs,_,_)) :-
	!,
    (
	S == q
    ->
	As = [],
	Bs = []
    ;
        Bs = []
    ).
%check_wrap(dl(0,lit(np(_,_,_)),lit(s(S))), _, _, data(_,_,_,_,_,Bs,_,_)) :-
%	S == main,
%	!,
%	Bs = [].
%check_wrap(dl(0,lit(np(_,_,_)),lit(s(S))), _, _, data(_,_,_,_,As,Bs,_,_)) :-
%	S == ppart,
%	!,
%	As = [],
%	Bs = [].
check_wrap(dl(0,lit(np(_,_,_)),lit(s(_))), _, _, data(_,_,_,_,As,Bs,_,_)) :-
	!,
	As = [],
	Bs = [].
check_wrap(dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(_)))), _, _, data(_,_,_,_,As,Bs,_,_)) :-
%	S == ppart,
	!,
	As = [],
	Bs = [].
%check_wrap(dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(S)))), _, _, data(_,_,_,_,As,Bs,_,_)) :-
%	S == pass,
%	!,
%	As = [],
%	Bs = [].
check_wrap(_, _, _, _).


functor_head_r(X, Y, H) :-
     (
         X == Y
     ->
         H = a
     ;
         /* determiners are not heads */
         X = lit(np(_,_,_)),
         Y = lit(n)
     ->
         H = a
     ;
         H = f
     ).
% TODO: change so that "a dit np" etc. are not treated as modifiers here
functor_head_l(X, Y, H) :-
     (
         X == Y
     ->
         H = a
     ;
         Y = lit(txt)
     ->
         H = a
     ;
         H = f
     ).

has_empty_stack(data(_, _, _, _, [], [], [], [])).


% = combine_sets

combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD) :-
	join_sets(SetA0, SetA1, SetA),
	join_sets(SetB0, SetB1, SetB),
	append(SetA, SetB, SetAB),
	check_coherence(SetAB),
	ord_key_union_var(SetC0, SetC1, SetC),
	ord_key_union_i(SetD0, SetD1, SetD).

join_sets(SetA1, SetA2, SetA) :-
	append(SetA1, SetA2, List),
	sort(List, SetA),
	check_coherence(SetA, 0).

check_coherence(SetA0) :-
	sort(SetA0, SetA),
	check_coherence(SetA, 0).

check_coherence([], _).
check_coherence([t(J,K,_,_)|As], I) :-
	I =< J,
	check_coherence(As, K).

combine_let_l(J, K, data(Pros0, Sem, Prob1, H, SetA0, SetB0, SetC0, SetD0),
	            data(Pros1   , _  , Prob2, _, SetA1, SetB1, SetC1, SetD1),
	            data(p(0,Pros0,Pros1), Sem, Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob1, Prob2, J, K, let, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).
combine_let_r(J, K, data(Pros0, _, Prob1, _, SetA0, SetB0, SetC0, SetD0),
	            data(Pros1   , Sem  , Prob2, H, SetA1, SetB1, SetC1, SetD1),
	            data(p(0,Pros0,Pros1), Sem, Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob1, Prob2, J, K, let, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

application_l(M, J, K, Head, data(Pros1, Sem1, Prob1, H1, SetA0, SetB0, SetC0, SetD0),
	         data(Pros2, Sem2, Prob2, H2, SetA1, SetB1, SetC1, SetD1),
	         data(p(M,Pros2,Pros1), appl(Sem1,Sem2), Prob, H, SetA, SetB, SetC, SetD)) :-
   (
        Head = f
   ->
	H = H1
   ;
	H = H2
   ),
	combine_probability(Prob1, Prob2, J, K, appl_l(Pros1,Pros2), Prob),
	combine_sets(SetA1, SetB1, SetC1, SetD1, SetA0, SetB0, SetC0, SetD0, SetA, SetB, SetC, SetD).
application_r(M, J, K, Head, data(Pros1, Sem1, Prob1, H1, SetA0, SetB0, SetC0, SetD0),
	                     data(Pros2, Sem2, Prob2, H2, SetA1, SetB1, SetC1, SetD1),
	                     data(p(M,Pros1,Pros2), appl(Sem1,Sem2), Prob, H, SetA, SetB, SetC, SetD)) :-
   (
        Head = f
   ->
	H = H1
   ;
	H = H2
   ),
	combine_probability(Prob1, Prob2, J, K, appl_r(Pros1,Pros2), Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

a_dit(I, K, data(Pros1, Sem1, Prob1, H, SetA0, SetB0, SetC0, SetD0),
            data(Pros2, Sem2, Prob2, _, SetA1, SetB1, SetC1, SetD1),
            data(p(0,Pros1,Pros2), lambda(S, appl(Sem1,appl(Sem2,S))), Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob1, Prob2, I, K, a_dit, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

dit_np(I, K, data(Pros1, Sem1, Prob1, H, SetA0, SetB0, SetC0, SetD0),
             data(Pros2, Sem2, Prob2, _, SetA1, SetB1, SetC1, SetD1),
             data(p(0,Pros1,Pros2), lambda(S, appl(appl(Sem1, S), Sem2)), Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob1, Prob2, I, K, dit_np, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

% = wrapping
%
% incidental adverbs have wide scope and more permissive positions; they are pushed onto stack A

wrap(dl(1,V,W), I, J, K, data(Pros1, Sem, Prob1, H0, SetA0, SetB0, SetC0, SetD0),
                      data(Pros2, Sem2, Prob2, _H1, SetA1, SetB1, SetC1, SetD1),
                      data(p(1,Pros1,Pros2), Sem, Prob, H0, [t(J,K,dl(1,V,W),Sem2)|SetA], SetB, SetC, SetD)) :-
	combine_probability(Prob1, Prob2, I, K, wrap, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

% integrated adverbs have local and narrow scope (wrt. the incidental adverbs); they can occur more or less
% freely among the verb arguments but not elsewhere and are pused onto stack B

wrap_arg(dl(1,V,W), I, J, K, data(Pros1, Sem, Prob1, H0, SetA0, SetB0, SetC0, SetD0),
                      data(Pros2, Sem2, Prob2, _H1, SetA1, SetB1, SetC1, SetD1),
                      data(p(1,Pros1,Pros2), Sem, Prob, H0, SetA, [t(J,K,dl(1,V,W),Sem2)|SetB], SetC, SetD)) :-
	combine_probability(Prob1, Prob2, I, K, wrap_arg, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

% only wrap from the incidental adverbs stack when there are no integrated adverbs (with net result that
% incidental adverbs outscope integrated adverbs, as they should)
pop(X, I1, J1, I, J, data(Pros, Sem, Prob0, H, [t(I0,J0,X,Sem0)|SetA], [], SetC, SetD), data(Pros, appl(Sem0,Sem), Prob, H, SetA, [], SetC, SetD)) :-
	/* verify the wrapped constituent is a substing of the current string */
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop, I1, J1, I0, J0),
	!.
pop(X, I1, J1, I, J, data(Pros, Sem, Prob0, H, SetA, [t(I0,J0,X,Sem0)|SetB], SetC, SetD), data(Pros, appl(Sem0,Sem), Prob, H, SetA, SetB, SetC, SetD)) :-
	/* verify the wrapped constituent is a substring of the current string */
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop, I1, J1, I0, J0),
	!.

pop_vp(I1, J1, I, J, data(Pros, SemVP, Prob0, H, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),SemADV)|SetA], [], SetC, SetD), data(Pros, lambda(X,appl(SemADV,appl(SemVP,X))), Prob, H, SetA, [], SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vp, I1, J1, I0, J0),
	!.
pop_vp(I1, J1, I, J, data(Pros, Sem, Prob0, H, SetA, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),Sem0)|SetB], SetC, SetD), data(Pros, lambda(X,appl(Sem0,appl(Sem,X))), Prob, H, SetA, SetB, SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vp, I1, J1, I0, J0),
	!.

pop_vp_strict(I1, J1, I, J, data(Pros, SemVP, Prob0, H, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),SemADV)|SetA], [], SetC, SetD), data(Pros, lambda(X,appl(SemADV,appl(SemVP,X))), Prob, H, SetA, [], SetC, SetD)) :-
	verify_wrap_strict(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vp, I1, J1, I0, J0),
	!.
pop_vp_strict(I1, J1, I, J, data(Pros, Sem, Prob0, H, SetA, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),SemADV)|SetB], SetC, SetD), data(Pros, lambda(X,appl(SemADV,appl(Sem,X))), Prob, H, SetA, SetB, SetC, SetD)) :-
	verify_wrap_strict(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vp, I1, J1, I0, J0),
	!.

% variant of pop_vp with additional clitic
pop_vpc(I1, J1, I, J, data(Pros, SemVP, Prob0, H, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),SemADV)|SetA], [], SetC, SetD), data(Pros, lambda(CL,lambda(X,appl(SemADV,appl(appl(SemVP,CL),X)))), Prob, H, SetA, [], SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vpc, I1, J1, I0, J0),
	!.
pop_vpc(I1, J1, I, J, data(Pros, SemVP, Prob0, H, [t(I0,J0,dl(1,dl(0,lit(np(A,B,C)),lit(s(S))),dl(0,lit(np(A,B,C)),lit(s(S)))),SemADV)|SetA], [], SetC, SetD), data(Pros, lambda(CL,appl(SemADV,appl(SemVP,CL))), Prob, H, SetA, [], SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vpc, I1, J1, I0, J0),
	!.
pop_vpc(I1, J1, I, J, data(Pros, SemVP, Prob0, H, SetA, [t(I0,J0,dl(1,lit(s(S)),lit(s(S))),SemADV)|SetB], SetC, SetD), data(Pros, lambda(CL,lambda(X,appl(SemADV,appl(appl(SemVP,CL),X)))), Prob, H, SetA, SetB, SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vpc, I1, J1, I0, J0),
	!.
pop_vpc(I1, J1, I, J, data(Pros, SemVP, Prob0, H, SetA, [t(I0,J0,dl(1,dl(0,lit(np(A,B,C)),lit(s(S))),dl(0,lit(np(A,B,C)),lit(s(S)))),SemADV)|SetB], SetC, SetD), data(Pros, lambda(CL,appl(SemADV,appl(SemVP,CL))), Prob, H, SetA, SetB, SetC, SetD)) :-
	verify_wrap(I1, I0, J0, J1, I, J),
	combine_probability_pop(Prob0, Prob, pop_vpc, I1, J1, I0, J0),
	!.

% = combine_probability_pop(+InWeight, -OutWeight, +RuleName, +ParentLeft, +ParentRight, +AdverbLeft, +AdverbRight)
%
% a special version of combine_probability which computes the penalty of an adverb (spanning positions
% AdverbLeft-AdverbRight) taking scope within an ancestor (spanning ParentLeft-ParentRight).
% we assign as weight the worst of the number of brackets crossing between ParentLeft-AdverbLeft and
% AdverbRight-ParentRight. Does nothing when we are dealing with probabilities.

combine_probability_pop(Prob0, Prob, Rule, PL, PR, AL0, AR0) :-
	AL is AL0 + 1,
	AR is AR0 - 1,
	combine_probability(Prob0, 0, PL, AL, Rule, ProbL),
	combine_probability(Prob0, 0, AR, PR, Rule, ProbR),
	/* we are dealing with negative weights (log probs or crossing branches) so we keep the minimum */
	/* I'm not sure whether the sum would be a good alternative; the maximum is not a good idea since */
	/* it does not penalize certain scopes which are higher than they should be at all */ 
	Prob is min(ProbL, ProbR).


adv_vp(I, J, K, data(Pros0, Sem0, Prob0, _, SetA0, SetB0, SetC0, SetD0),
             data(Pros1, Sem1, Prob1, H, SetA1, SetB1, SetC1, SetD1),
             data(p(0,Pros0, Pros1), lambda(X,appl(Sem0,appl(Sem1,X))), Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob0, Prob1, I, K, wrap_vpi, Prob2),
	J1 is J - 1,
	I1 is I + 1,
	combine_probability(Prob2, 0, I1, K, wrap_vpi, ProbL),
	combine_probability(Prob2, 0, J1, J, wrap_vpi, ProbR),
	Prob is min(ProbL, ProbR),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).
% variant of adv_vp with additional clitic
adv_vpc(I, J, K, data(Pros0, Sem0, Prob0, _, SetA0, SetB0, SetC0, SetD0),
              data(Pros1, Sem1, Prob1, H, SetA1, SetB1, SetC1, SetD1),
              data(p(0,Pros0, Pros1), lambda(CL,lambda(X,appl(Sem0,appl(appl(Sem1,CL),X)))), Prob, H, SetA, SetB, SetC, SetD)) :-
	combine_probability(Prob0, Prob1, I, K, wrap_vpi, Prob2),
	J1 is J - 1,
	I1 is I + 1,
	combine_probability(Prob2, 0, I1, K, wrap_vpi, ProbL),
	combine_probability(Prob2, 0, J1, J, wrap_vpi, ProbR),
	Prob is min(ProbL, ProbR),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD1, SetA, SetB, SetC, SetD).

% I = I1 --- I0 --- J0 --- J1 = J

verify_wrap(I, I0, J0, J, I, J) :-
	I0 >= I,
	J0 =< J,
	!.

% I < I1 --- I0 --- J0 --- J1 < J

verify_wrap_strict(I, I0, J0, J, I, J) :-
	I0 > I,
	J0 =< J,
	!.

% = extraction

% = start_extraction(+ExtractedFormula, RightEdgeOfFormula, LeftEdgeOfIntroduction, RightEdgeOfIntroduction, Data1, Data2)
%
% ExtractedFormula       : formula extracted
% RightEdgeOfFormula     : string position where the formula has been inserted
% LeftEdgeOfIntroduction : string position where the higher-order formula authorizing the introduction starts
% RightEdgeOfIntroduction: string position where the higher-order formula authorizing the introduction ends
%
% SetD has entries of the form IntroRightEdge-r(Formula,FormRightEdge,SemVar)
% with meaning Formula-SemVar has been used at string position J-J (FormRightEdge)

start_extraction(Y, J, K0, K, data(Pros, Sem, Prob, H, SetA, SetB, SetC, SetD0), data(Pros, appl(Sem,'$VAR'(K)), Prob, H, SetA, SetB, SetC, SetD)) :-
	ord_key_insert_i(SetD0, K, t(Y,J,K0,'$VAR'(K)), SetD).

start_extraction_inv(Y, J, K0, K, data(Pros, Sem, Prob, H, SetA, SetB, SetC, SetD0), data(Pros, appl('$VAR'(K),Sem), Prob, H, SetA, SetB, SetC, SetD)) :-
	ord_key_insert_i(SetD0, K, t(Y,J,K0,'$VAR'(K)), SetD).


% = start_extraction(+ExtractedFormula, RightEdgeOfFormula, LeftEdgeOfIntroduction, Data1, Data2)
%
% ExtractedFormula: formula extracted
% RightEdgeOfFormula: string position where the formula has been inserted
% LeftEdgeOfIntroduction: string position where the higher-order formula authorizing the introduction ends
%
% SetC has entries of the form IntroRightEdge-r(Formula,FormRightEdge,SemVar)
% with meaning Formula-SemVar has been used at string position J-J (FormRightEdge)

start_extraction_l0(Y, J, K, L, Prob0, data(Pros, Sem, Prob1, H, SetA, SetB, SetC0, SetD), data(Pros, appl(Sem,'$VAR'(K)), Prob, H, SetA, SetB, SetC, SetD)) :-
	ord_key_insert_var(SetC0, 0, t(Y,J,K,L,'$VAR'(K)), SetC),
	Prob is Prob0 + Prob1.

start_extraction_l(Y, J, K, L, Prob0, data(Pros, Sem, Prob1, H, SetA, SetB, SetC0, SetD), data(Pros, appl(Sem,'$VAR'(K)), Prob, H, SetA, SetB, SetC, SetD)) :-
	ord_key_insert_var(SetC0, K, t(Y,J,K,L,'$VAR'(K)), SetC),
	Prob is Prob0 + Prob1.

end_extraction_l(Y, J, K, _L, M, data(Pros0, Sem0, Prob0, _, [], [], [], []),
                          data(Pros1, Sem1, Prob1, H, SetA, SetB, SetC1, SetD),
	                  data(p(0,Pros1,Pros0), appl(Sem0,lambda(X,Sem1)), Prob, H, SetA, SetB, SetC, SetD)) :-
	select(K-t(Y,J,K,M,X), SetC1, SetC),
	subterm(Sem1, X),
	!,
	combine_probability(Prob0, Prob1, K, M, e_end_l, Prob).


end_extraction(Y, I, J, K0, K, data(Pros0, Sem0, Prob0, H, SetA0, SetB0, SetC0, SetD0),
                  data(Pros1, Sem1, Prob1, _, SetA1, SetB1, SetC1, SetD1),
	          data(p(0,Pros0,Pros1), appl(Sem0,lambda(X,Sem1)), Prob, H, SetA, SetB, SetC, SetD)) :-
	select(J-t(Y,K0,I,X), SetD1, SetD2),
	subterm(Sem1, X),
	!,
	combine_probability(Prob0, Prob1, I, K, e_end, Prob),
	combine_sets(SetA0, SetB0, SetC0, SetD0, SetA1, SetB1, SetC1, SetD2, SetA, SetB, SetC, SetD).

print_stacks(data(_,_,_,_,[],[],[],[])) :-
	!.
print_stacks(data(_,_,_,_,As,Bs,Cs,Ds)) :-
	format('{~p ~p ~p ~p}', [As,Bs,Cs,Ds]).

create_data(Pros, _Form, Lemma, Sem, Prob, _I, _J, data(Pros, Sem, Weight, Lemma, [], [], [], [])) :-
	compute_weight(Prob, Weight).


simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(_)), s) :- !.
simplify_formula(lit(pp(_)), pp) :- !.
simplify_formula(lit(np(_,_,_)), np) :- !.
simplify_formula(lit(A), A).
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


% ==============================================
% =           ordered set predicates           =
% ==============================================

% variants of predicates from ordset.pl

% = ord_key_insert_i(+OrdSet, +Key, +Data, -OrdSet)
%
% as ord_insert/3, but assumes the elements of OrdSet are Key-Value
% pairs; uses *inverse* ordering                                     RM
% Fails if Key already exists in OrdSet

ord_key_insert_i([], Key, Data, [Key-Data]).
ord_key_insert_i([Key-Data|Tail], Key0, Data0, Rest) :-
	compare(Order, Key0, Key),
	ord_key_insert_i(Order, Tail, Key0, Data0, Key, Data, Rest).

ord_key_insert_i(>, Tail, Key0, Data0, Key, Data, [Key0-Data0,Key-Data|Tail]).
ord_key_insert_i(<, Tail, Key0, Data0, Key, Data, [Key-Data|Rest]) :-
	ord_key_insert_i(Tail, Key0, Data0, Rest).


% = ord_key_insert_var(+OrdSet, +Key, +Data, -OrdSet)
%
% variant of ord_key_insert which fails if Key already exists in
% OrdSet (to prevent multiply licensed extractions).


ord_key_insert_var([], Key, Data, [Key-Data]).
ord_key_insert_var([Key-Data|Tail], Key0, Data0, Rest) :-
	compare(Order, Key0, Key),
	ord_key_insert_var(Order, Tail, Key0, Data0, Key, Data, Rest).

ord_key_insert_var(<, Tail, Key0, Data0, Key, Data, [Key0-Data0,Key-Data|Tail]).
ord_key_insert_var(>, Tail, Key0, Data0, Key, Data, [Key-Data|Rest]) :-
	ord_key_insert_var(Tail, Key0, Data0, Rest).



% = ord_key_union(+Map1, +Map2, ?Map3)
%
% as ord_union/3, but for ordered sets of Key-Value pairs, where Value
% is itself an ordered set. If Map1 and Map2 contain the same Key,
% Map3 will contain the ord_union of the two values.                RM

ord_key_union_i([], Set2, Set2).
ord_key_union_i([H1-V1|T1], Set2, Union) :-
	ord_key_union_2_i(Set2, H1, V1, T1, Union).

ord_key_union_2_i([], H1, V1, T1, [H1-V1|T1]).
ord_key_union_2_i([H2-V2|T2], H1, V1, T1, Union) :-
	compare(Order, H1, H2),
	ord_key_union_3_i(Order, H1, V1, T1, H2, V2, T2, Union).

ord_key_union_3_i(>, H1, V1, T1, H2, V2, T2, [H1-V1|Union]) :-
	ord_key_union_2_i(T1, H2, V2, T2, Union).
% NOTE: commenting out the equality restricts lexical entries to have only
% a single extraction each, since the gapping analysis will require special
% treatment in any case, this simple solution seems justified
%ord_key_union_3_i(=, H1, V1, T1, H2, V2, T2, [H1-V1,H2-V2|Union]) :-
%	V1 \== V2,
%	ord_key_union_i(T1, T2, Union).
ord_key_union_3_i(<, H1, V1, T1, H2, V2, T2, [H2-V2|Union]) :-
	ord_key_union_2_i(T2, H1, V1, T1, Union).


% = ord_key_union_var(+Map1, +Map2, ?Map3)
%
% variant of ord_key_union from the library ordsets which fails in case
% the two sets contain an identical element (as the inverse ord_key_union_i
% predicate above.

ord_key_union_var([], Set2, Set2).
ord_key_union_var([H1-V1|T1], Set2, Union) :-
	ord_key_union_var_2(Set2, H1, V1, T1, Union).

ord_key_union_var_2([], H1, V1, T1, [H1-V1|T1]).
ord_key_union_var_2([H2-V2|T2], H1, V1, T1, Union) :-
	compare(Order, H1, H2),
	ord_key_union_var_3(Order, H1, V1, T1, H2, V2, T2, Union).

ord_key_union_var_3(<, H1, V1, T1, H2, V2, T2, [H1-V1|Union]) :-
	ord_key_union_var_2(T1, H2, V2, T2, Union).
ord_key_union_var_3(>, H1, V1, T1, H2, V2, T2, [H2-V2|Union]) :-
	ord_key_union_var_2(T2, H1, V1, T1, Union).

% = streams

check_log_stream :-
    (
	is_stream(log)
    ->
        true
    ;
        new_output_file(grail_log, log)
    ).

check_proof_stream :-
    (
	is_stream(proof)
    ->
        true
    ;
        new_output_file('proof.tex', proof)
    ).

check_prolog_proof_stream :-
   (
       is_stream(pl_proof)
   ->
       true
   ;
       new_output_file('proofs.pl', pl_proof)
   ).

check_xml_stream :-
   (
       is_stream(xml)
   ->
       true
   ;
       new_output_file('proofs.xml', xml)
   ).
check_plog_stream :-
   (
        is_stream(plog)
   ->
        true
   ;
        new_output_file(rule_logs, plog)
   ).

% = new_output_file(+File, +Alias)
%
% opens File to be accessed by Alias
% An old file of the same name, if it exists, is deleted.
% If opening the file raises an exception (because of permissions)
% a null stream is opened instead.

new_output_file(File, Alias) :-
   (
	is_stream(Alias)
   ->
	close(Alias)
    ;
        true
   ),
        delete_file_if_exists(File),
	catch(open(File, update, _, [alias(Alias),buffer(line)]),
	      _, 
	      (open_null_stream(Null),set_stream(Null,alias(Alias)))).
delete_file_if_exists(File) :-
    (
        exists_file(File),
	access_file(File, write)
    ->
	delete_file(File)
    ;
	true
    ).

reset_global_counters :-
	retractall('$SOLUTION'(_)),
	assert('$SOLUTION'(0)),
	statistics(cputime, CPU),
	assert('$START_CPU'(CPU)),
	statistics(inferences, INF),
	assert('$START_INF'(INF)).

increase_global_counter(Functor) :-
	functor(Term, Functor, 1),
	call(Functor, Num0),
	Num is Num0 + 1,
	retractall(Term),
	arg(1, Term, Num),
	assert(Term).

increase_global_counter(Functor, Num) :-
	functor(Term, Functor, 1),
	call(Functor, Num0),
	Num is Num0 + 1,
	retractall(Term),
	arg(1, Term, Num),
	assert(Term).

set_global_counter(Functor, Num) :-
	functor(Term, Functor, 1),
	retractall(Term),
	arg(1, Term, Num),
	assert(Term).


pdflatex_semantics :-
    (
	option_true(latex)
    ->
	shell_warn('pdflatex semantics.tex >> texlog &')
    ;
	true
    ).

open_semantics_files :-
	new_output_file('semantics.tex', sem),
	new_output_file('previous_semantics.pl', previous_sem),
	open('semantics.pl', append, _, [alias(sem_pl),buffer(line)]).

atomic_formula(D) :-
	lex(_, D0, _),
	macro_expand(D0, D1),
	atomic_formula1(D1, D).

atomic_formula1(lit(A), D) :-
    (
        compound(A)
    ->
        functor(A, D, _)
    ;
        D = A
    ).
atomic_formula1(dl(_,A,B), D) :-
    (
	atomic_formula1(A, D)
    ;
	atomic_formula1(B, D)
    ).
atomic_formula1(dr(_,A,B), D) :-
    (
	atomic_formula1(A, D)
    ;
	atomic_formula1(B, D)
    ).
atomic_formula1(p(_,A,B), D) :-
    (
	atomic_formula1(A, D)
    ;
	atomic_formula1(B, D)
    ).
atomic_formula1(dia(_,A), D) :-
	atomic_formula1(A, D).
atomic_formula1(box(_,A), D) :-
	atomic_formula1(A, D).

shell_warn(Cmd) :-
	shell(Cmd, Exit),
    (
        Exit = 0
    ->
        true
    ;
        format(user_error, '{Warning: Shell command failed "~w"}~n', [Cmd])
    ).


% ==============================================
% =               pretty-printing              =
% ==============================================

user:portray(lit(np(A,_,_))) :-
	!,
    (
        A == nom
    ->
        write(np_nom)
    ;
       A == acc
    ->
       write(np_acc)
    ;
       write(np)
    ).
user:portray(lit(s(A))) :-
	var(A),
	!,
	write(s).
user:portray(lit(s(A))) :-
	!,
	format('s_~w', [A]).

user:portray(lit(A)) :-
	!,
	write(A).
user:portray(box(0,A)) :-
	!,
	format('[]~p', [A]).
user:portray(dia(0,A)) :-
	!,
	format('<>~p', [A]).
user:portray(box(I,A)) :-
	!,
	format('[]~w ~p', [I,A]).
user:portray(dia(I,A)) :-
	!,
	format('<>~w ~p', [I,A]).

user:portray(dr(0,A,B)) :-
	!,
	format('(~p/~p)', [A,B]).
user:portray(dl(0,A,B)) :-
	!,
	format('(~p\\~p)', [A,B]).
user:portray(p(0,A,B)) :-
	!,
	format('(~p o ~p)', [A,B]).
user:portray(p(1,A,B)) :-
	!,
	format('(~p * ~p)', [A,B]).
user:portray(dr(I,A,B)) :-
	!,
	!,
	format('(~p /~w ~p)', [A,I,B]).
user:portray(dl(I,A,B)) :-
	!,
	format('(~p \\~w ~p)', [A,I,B]).
user:portray(p(I,A,B)) :-
	!,
	format('(~p o~w ~p)', [A,I,B]).
user:portray(t(I,J,F,_V)) :-
	integer(I),
	integer(J),
	!,
	format('~w-~w ~p', [I,J,F]).

% ==============================================
% =             interactive parser             =
% ==============================================

% interactive chart parser, requires communication (in utf-8!) 
% between SWI Prolog and TclTk

grail_gui :-
	repeat,
	/* start main loop, tell Tk we are in the INIT state, that is done processing the
	   previous command and waiting for new input */
	format('INIT~nREPEAT~n', []),
	flush_output,
	read(Input),
	format('TREATING INPUT: ~w~n', [Input]),
	start(Input),
	Input = end_of_file,
	!.

extended_active(Active) :-
	state(S, L),
	extended_active(S, L, Active).

start(end_of_file) :-
	!,
	halt.
start(trace) :-
	!,
	trace.
start(undo(Input)) :-
	!,
	state(choose_active, [Active0, N0]),
	active(Input, Item),
	format('UNDO: ~w~n', [Item]),
	flush_output,
	justification(Item, Justification),
	Justification =.. [_Rulename,I|Indices0],
	sort([I|Indices0], Indices),
	format('JUST: ~w~n', [Indices]),
	flush_output,
	ord_select(Item, Active0, Active1),
	ord_union(Active1, Indices, Active),
	retractall(stored(Item, _, _, _, _, _)),
	format('~nCHOOSE ACTIVE~n', []),
	write_active(Active),
	flush_output,
	update_state(Active, N0).
start(active(Input)) :-
	!,
	state_active(Active0, N0),
	active(Input, Item),
	handle(Item, Active0, Active, N0, N),
	update_state(Active, N).
start(choose(Input)) :-
	!,
	state(choose_rule, [[C|Cs],_,A0,N0]),
	integer(Input),
	nth0(Input, [C|Cs], _-(Item-Just)),
	apply_rule(Item, Just, A0, A, N0, N),
	format('~nCHOOSE ACTIVE~n', []),
	write_active(A),
	flush_output,
	update_state(A, N).
start(export) :-
	!,
	export_stored.
start(load(File)) :-
	!,
	format('COMPILING~n', []),
	flush_output,
	compile(File),
	format('DONE~n', []),
	findall(Number, clause(sent(Number, _), _), List0),
	sort(List0, List),
	format('SENTENCES~n', []),
	write_list(List),
	flush_output,
	List = [First|_],
	interactive_parse_init(First).
	
start(parse(Number)) :-
	!,
	interactive_parse_init(Number).
start(X) :-
	format('STRANGE INPUT: ~w~n', [X]).


state_active(Active, N) :-
	state(choose_active, [Active, N]),
	!.
% This case is relevant is case the user cancels the rule selection
state_active(Active, N) :-
	state(choose_rule, [_,A,Active0,N]),
	ord_insert(Active0, A, Active).

interactive_parse_init(Number) :-
	clause(sent(Number, Sem), prob_parse(List, Sem)),
	!,
	format('START INITIALIZATION ~w~n', [List]),
	flush_output,
	update_crossing(Number),
	retractall(active_rule(_)),
	assert(active_rule(_)),
	check_log_stream,
	init_chart,
	empty_heap(Heap),
	format('HEAP ~w~n', [Heap]),
	flush_output,
	list_to_chart(List, 0, Heap, As, [], 0, _V, _SemInfo, []),
	format('START AXIOMS ~w~n', [As]),
	flush_output,
	add_axioms_to_chart(As, 0, N, Active),
	format('START PARSE ~w~n', [Number]),
	flush_output,
	interactive_parse_tk(Active, N).
interactive_parse_init(Number) :-
	format('No clause found for sent(~k, Sem)~n', [Number]).

% = interactive_parse_tk(+ActiveItems, +MaxChart)

interactive_parse_tk(Active, N) :-
	/* send choices to Tk, then wait for Tk user input */
	format('~nCHOOSE ACTIVE~n', []),
	write_active(Active),
	flush_output,
	update_state(Active, N).

% = handle(+Item, +ActiveIn, -ActiveOut, +NewIn, -NewOut)
%
% Find all consequences of Item (in the context of active items
% ActiveIn) and update the chart accordingly.
%
% Do nothing if no consequences are found, ask for user input
% if multiple consequences are found and if only a single
% consequence exists (nearly always possible with careful
% selection of active items) apply it immediately.

handle(X, A0, A, N0, N) :-
	ord_member(X, A0),
	!,
	find_all_consequences1(X, Cs0, A0),
    (
        Cs0 = [C|Cs1]
    ->
        handle_consequences(Cs1, C, X, A0, A, N0, N)
    ;
        format('REDO~n', []),
        flush_output,
        fail
    ).

% = handle_consequence(+OtherConsequences, +FirstConsequence, Agenda, New)
%
% 

handle_consequences([], C, _, A0, A, N0, N) :-
	C = _-(Item-Just),
	apply_rule(Item, Just, A0, A, N0, N),
	!,
	format('~nCHOOSE ACTIVE~n', []),
	write_active(A),
	flush_output.
handle_consequences(Cs, C, X, A0, _A, N0, _N) :-
	format('CHOOSE RULE~n', []),
	write_choice([C|Cs]),
	flush_output,
	retractall(state(_,_)),
	assert(state(choose_rule,[[C|Cs],X,A0,N0])),
	fail.

% = update_state(+ActiveFormulas, +ChartNew)
%
% Updates the permanent state with the current ordered set of ActiveFormulas
% and the first unused chart position ChartNew
%
% If a solution is found, compute the corresponding proof and output this proof
% to the Prolog, LaTeX and XML files.

update_state(Active, N) :-
	retractall(state(_,_)),
   (
        Active = [I],
    	check_solution(I, Sem)
   ->
	format('~nSOLUTION~n~w~n', [Sem])
   ;
        true
   ),
	assert(state(choose_active, [Active, N])).       


% = interactive_parse(+ActiveFormulas, -Semantics)

interactive_parse([], drs([],[])).
interactive_parse([A|As], Sem) :-
	add_axioms_to_chart([A|As], 0, N, Active),
	interactive_parse1(Active, N),
	check_solution(Sem).

interactive_parse1(Active0, N0) :-
    (
        Active0 = [_]
    ->
        true
    ;
        nl,
        portray_active(Active0),
	interactive_rule(Active0, Active, N0, N),
	interactive_parse1(Active, N)
    ).

portray_active([]).
portray_active([A|As]) :-
	stored(A,_B,C,D,E,data(Pros,_,W,_,L1,L2,L3,L4)),
	!,
    (
        L1 = [], L2 = [], L3 = [], L4 = []
    ->
     	format('~t~w~3| ~t~w~3+-~w~t~3+~t~p~3+ ~p~t~36+ ~p~n', [A,C,D,W,E,Pros])
    ;
	format('~t~w~3| ~t~w~3+-~w~t~3+~t~p~3+ ~p~t~36+ ~p ~p ~p ~p ~p~n', [A,C,D,W,E,Pros,L1,L2,L3,L4])
     ),
	portray_active(As).

portray_chart(N) :-
	findall(stored(A,B,C,D,E,F), stored(A,B,C,D,E,F), List),
	portray_chart(List, 0, N).

interactive_rule(Active0, Active, N0, N) :-
	read_potential_trigger(Active0, Consequences),
	length(Consequences, L),
   (
        L = 1
   ->
        /* single rule application possible */
        Consequences = [_-(Item-Just)]
   ;
        /* multiple rules possible, ask user */
	portray_numbered_list(Consequences, 0),
	read_integer(Cnsq, L),
	nth0(Cnsq, Consequences, _-(Item-Just))
   ),
	apply_rule(Item, Just, Active0, Active, N0, N).

apply_rule(Item, Just, Active0, Active, N0, N) :-
	Just =.. [R|Args0],
	sort(Args0,Args),
	subtract_premisses(R, Args, Active0, Active1),
	ord_insert(Active1, N0, Active),
	N is N0 + 1,
	add_item_to_agenda(Item, Just, queue(N0,N0), _).

subtract_premisses(e_start_l, [A,B], Active0, Active) :-
	!,
    (
	stored(A, _, _, _, dl(0,dr(0,_,dia(Ind,box(Ind,_))),_), _)
    ->
        ord_delete(Active0, B, Active)
    ;
        ord_delete(Active0, A, Active)
    ).
subtract_premisses(e_start, [A,B], Active0, Active) :-
	!,
    (
	stored(A, _, _, _, dr(0,_,dr(0,_,dia(Ind,box(Ind,_)))), _)
    ->
        ord_delete(Active0, B, Active)
    ;
        ord_delete(Active0, A, Active)
    ).
subtract_premisses(ef_start, [A,B], Active0, Active) :-
	!,
    (
	stored(A, _, _, _, dr(0,_,dr(0,_,dia(Ind,box(Ind,_)))), _)
    ->
        ord_delete(Active0, B, Active)
    ;
        ord_delete(Active0, A, Active)
    ).
subtract_premisses(ef_start_iv, [A,B], Active0, Active) :-
	!,
    (
	stored(A, _, _, _, dr(0,_,dr(0,_,dia(Ind,box(Ind,_)))), _)
    ->
        ord_delete(Active0, B, Active)
    ;
        ord_delete(Active0, A, Active)
    ).
subtract_premisses(gap_i, [A,B,C], Active0, Active) :-
        select(D, [A,B,C], R0),
        stored(D, _, _, _, dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,X))),dr(0,lit(s(S)),box(Ind,dia(Ind,X)))), _),
        select(E, R0, [F]),
        stored(E, _, _, _, X, _),
	!,
	ord_delete(Active0, D, Active1),
	ord_delete(Active1, F, Active).
subtract_premisses(gap_c, [A,B], Active0, Active) :-
	select(D, [A,B], [E]),
	stored(D, _, _, _, dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,dr(0,X,Y)))),dr(0,lit(s(S)),box(Ind,dia(Ind,dr(0,X,Y))))), _),
	!,
	ord_delete(Active0, E, Active).
subtract_premisses(gap_e, [A,B], Active0, Active) :-
	select(D, [A,B], [E]),
	stored(D, _, _, _, dl(0,dr(0,lit(s(S)),dia(Ind,box(Ind,dr(0,X,Y)))),dr(0,lit(s(S)),box(Ind,dia(Ind,dr(0,X,Y))))), _),
	stored(E, _, _, _, X, _),
	!,
	ord_delete(Active0, E, Active).
subtract_premisses(c_r_lnr, [A,B,C], Active0, Active) :-
	select(D, [A,B,C], [E,F]),
	stored(D, _, _, _, dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n))), _),
	!,
	ord_delete(Active0, E, Active1),
	ord_delete(Active1, F, Active).
subtract_premisses(c_l_lnr, [A,B,C], Active0, Active) :-
	select(D, [A,B,C], [E,F]),
	stored(D, _, _, _, dr(0,_,dl(0,dia(0,box(0,lit(n))),lit(n))), _),
	!,
	ord_delete(Active0, E, Active1),
	ord_delete(Active1, F, Active).

subtract_premisses(_, Ps, As0, As) :-
	ord_subtract(As0, Ps, As).

portray_numbered_list([], _).
portray_numbered_list([A|As], N0) :-
	format('~w. ~p~n', [N0,A]),
	N is N0 + 1,
	portray_numbered_list(As, N).

read_potential_trigger(Active, Consequences) :-
	read_integer_from_list(Input0, Active0, _Active1),
	find_all_consequences1(Input0, Consequences0, Active0),
    (
        Consequences0 = []
    ->
        format('No rule application possible, select another trigger!~n', []),
        read_potential_trigger(Active, Consequences)
    ;
        Consequences = Consequences0
    ).
	
read_integer_from_list(Input, List0, List) :-
	robust_read(Input0),
    (
        integer(Input0),
        select(Input0, List0, List)
    ->
        Input = Input0
    ;
        Input0 = a
    ->
        format('Aborted!~n', []),
        fail
    ;
        Input0 = abort
    ->
        format('Aborted!~n', []),
        fail
    ;
        format('Input an integer from ~w (or type "a." to abort)~n', [List0]),
        read_integer_from_list(Input, List0, List)
    ).
    
read_integer(Input, N) :-
	robust_read(Input0),
    (
	integer(Input0),
        Input0 >= 0,
        Input0 =< N
    ->
        Input = Input0
    ;
        write('Input an integer between 0 and ~w', [N]),
        read_integer(Input, N)
    ).

robust_read(Input) :-
	catch(read(Input), E, print_message(error, E)),
    (
        var(Input)
    ->
        robust_read(Input)
    ;
        true
    ).


write_list([]) :-
	format('LIST END~n', []),
	flush_output.
write_list([C|Cs]) :-
	format('~w~n', [C]),
	write_list(Cs).

write_choice([]) :-
	format('LIST END~n', []),
	flush_output.	
write_choice([_-(_-Just)|Cs]) :-
	Just =.. [Rule,P|Prems],
	format('~w ', [Rule]),
	write_prems(Prems,P),
	write_choice(Cs).

write_prems([], P) :-
    (
	active(C, P)
    ->
	format('~w~n', [C])
    ;
        stored(P, _, L, R, F, data(Pros,_,_,_,_,_,_,_)),
        format('aux(~w,~w,~w,~w,~w)~n', [P,L,R,F,Pros])
    ).
write_prems([P|Ps], P0) :-
    (
	active(C, P0)
    ->
	format('~w ', [C])
    ;
        stored(P, _, L, R, F, data(Pros,_,_,_,_,_,_,_)),
        format('aux(~w,~w,~w,~w,~w) ', [P,L,R,F,Pros])
    ),
	write_prems(Ps, P).

write_active(L0) :-
	retractall(active(_,_)),
	order_active(L0, [], L),
	write_active1(L, 0).

order_active([], S, S).
order_active([C|Cs], S0, S) :-
	stored(C, _, L, R, F0, data(Pros0, Sem, W, _, L1, L2, L3, L4)),
	tcl_pros(C, Pros0, Pros),
	tcl_form(F0, F),
	tcl_stacks(L1, L2, L3, L4, Stacks),
	ord_key_insert(S0, L, t(C,R,F,Pros,Sem,W,Stacks), S1),
	order_active(Cs, S1, S).

write_active1([], _) :-
	format('LIST END~n', []),
	flush_output.
write_active1([L-t(C,R,F,Pros,Sem,W,Stacks)|Cs], N0) :-
	format('~w ~w ~w ~w ~w ~w ~w ~w~n', [C, L, R, F, Pros, Sem, W, Stacks]),
	assert(active(N0,C)),
	N is N0 + 1,
	write_active1(Cs, N).

tcl_stacks([], [], [], [], ' ') :-
	!.
tcl_stacks([t(_I,_J,F0,_Sem)], [], [], [], F) :-
	!,
	tcl_form(F0, F).
tcl_stacks([], [t(_I,_J,F0,_Sem)], [], [], F) :-
	!,
	tcl_form(F0, F).
tcl_stacks([], [], [_-t(F0,_,_)], [], F) :-
	!,
	tcl_form(F0, F).
tcl_stacks([], [], [], [_-t(F0,_,_)], F) :-
	!,
	tcl_form(F0, F).
tcl_stacks(As, Bs, Cs, Ds, As-Bs-Cs-Ds).

tcl_form(F0, F) :-
	macro_expand(F0, F1),
	tcl_form1(F1, F).

tcl_form1(lit(np(A,_,_)), N) :-
	!,
     (
        A == nom
    ->
        N = np_nom
    ;
        A == acc
    ->
        N = np_acc
     ;
        N = np
     ).
tcl_form1(lit(s(T)), S) :-
	!,
    (
        T == main
    ->
        S = s_main       
    ;
        T == pass
    ->
        S = s_pass       
    ;
        T == ppart
    ->
        S = s_ppart       
    ;
        T == ppres
    ->
        S = s_ppres       
    ;
        T == q
    ->
        S = s_q
    ;
        T == whq
    ->
        S = s_whq
    ;
        T = inf(_I)
    ->
        S = s_inf
    ;
        T == ainf
    ->
        S = s_ainf
    ;
        T == deinf
    ->
        S = s_deinf
    ;
        S = s
    ).
tcl_form1(lit(pp_a), PP) :-
	!,
	PP = pp_à.
tcl_form1(pp_a, PP) :-
	!,
	PP = pp_à.
tcl_form1(lit(pp_de), PP) :-
	!,
	PP = pp_de.
tcl_form1(lit(pp_par), PP) :-
	!,
	PP = pp_par.
tcl_form1(lit(pp(P)), PP) :-
	!,
    (
        var(P)
    ->
        PP = pp
    ;
        P = a
    ->
        PP = pp_à
    ;
        atom(P)
    ->
        atomic_list_concat([pp,P], '_', PP)
    ;
       PP = pp
    ).
tcl_form1(lit(F), F).
tcl_form1(dr(I,A0,B0), dr(I,A,B)) :-
	tcl_form1(A0, A),
	tcl_form1(B0, B).
tcl_form1(dl(I,A0,B0), dl(I,A,B)) :-
	tcl_form1(A0, A),
	tcl_form1(B0, B).
tcl_form1(p(I,A0,B0), p(I,A,B)) :-
	tcl_form1(A0, A),
	tcl_form1(B0, B).
tcl_form1(dia(I,A0), dia(I,A)) :-
	tcl_form1(A0, A).
tcl_form1(box(I,A0), box(I,A)) :-
	tcl_form1(A0, A).

tcl_pros(C, P0, P) :-
	justification(C, Js),
	Js =.. [_|As],
	reconstruct_pros(P0, As, P1),
	tcl_pros(P1, P).

tcl_pros('"', '*QUOTE*') :-
	!.
tcl_pros(',', '*COMMA*') :-
	!.
tcl_pros('(', '*LPAR*') :-
	!.
tcl_pros(')', '*RPAR*') :-
	!.
tcl_pros(p(I,A0,B0), p(I,A,B)) :-
	!,
	tcl_pros(A0, A),
	tcl_pros(B0, B).
tcl_pros(Pros0, Pros) :-
	name(Pros0, Codes0),
	replace_commas(Codes0, Codes),
	safe_name(Pros, Codes).


safe_name(Pros, Codes) :-
	name(Pros0, Codes),
	handle_numbers(Pros0, Pros, Codes).

handle_numbers(Pros0, Pros, _Codes) :-
	/* default name/2 treatment of integers */
	integer(Pros0),
	!,
	Pros = Pros0.
handle_numbers(Pros0, Pros, Codes) :-
	/* take care we don't "round off" numbers like '100.000'  to '100.0' ! */
	number(Pros0),
	!,
	atom_chars(Pros, Codes).
handle_numbers(Pros, Pros, _Codes).


replace_commas([], []).
replace_commas([C|Cs], Ds0) :-
    (
        C = 44
    ->
        Ds0 = [42,67,79,77,77,65,42|Ds]
    ;
        Ds0 = [C|Ds]
    ),
        replace_commas(Cs, Ds).

flatten_pros(p(_,A,B)) -->
	!,
	flatten_pros(A),
	[' '],
	flatten_pros(B).
flatten_pros(dia(_,A)) -->
	!,
	flatten_pros(A).
flatten_pros(A) -->
	[A].


% ==============================================
% =              output the chart              =
% ==============================================

% = print_chart(+SentNo)
%
% output the chart contexts to a GraphViz file

print_chart(SentNo) :-
	concat_atom(['chart',SentNo,'.dot'], FileName),
	open(FileName, write, Stream),
	print_chart1(Stream),
	close(Stream).
print_chart1(Stream) :-
	format(Stream, 'digraph "chart" {~n', []),
	stored(_, _, I, J, F, _),
	format(Stream, '~w -> ~w [label="~p"]~n', [I,J,F]),
	fail.
print_chart1(Stream) :-
	format(Stream, '}~n', []).


% = export_raw
%
% exports chart contents to the file 'stored.txt'

export_raw :-
	shell('rm stored.txt', _),
	tell('stored.txt'),
	listing(stored),
	listing(justification),
	told.

% = export_stored
%
% as export_raw, but reconstructs the full prosodic label for all chart items.

export_stored :-
	shell('rm stored.txt', _),
	tell('stored.txt'),
	export_stored1,
	listing(justification),
	told.

export_stored1 :-
	stored(Index, Key, L, R, F, Data0),
	justification(Index, Just),
	Just =.. [_|Args],
	expand_data(Data0, Args, Data),
	portray_clause(stored(Index, Key, L, R, F, Data)),
	fail.
export_stored1.


% ==============================================
% =               initialization               =
% ==============================================

% automatic initialization of rule counts

compute_rule_counts_init :-
	retractall(rule_counts_init(_)),
	findall(Name-0, inference(Name, _, _, _), List),
	sort([axiom-0,id-1|List], Set),
	assert(rule_counts_init(Set)).
	
:- compute_rule_counts_init.
