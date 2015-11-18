% change "old" TLGbank format (with free variables) to "new" TLGbank format, what words occur in the lambda terms like word(N) and other variables occur as '$VAR'(N).

:- use_module(ordset, [ord_member/2, ord_insert/3]).
:- use_module(print_proof, [print_proof/3]).

% = convert_all(+FileName)

% convert all sentences in FileName to the new format, if FileName does not exist in the local
% directory, create the new version locally with the same name, otherwise output to the
% current output.

convert_all(FileName) :-
	open(FileName, read, Stream),
	file_base_name(FileName, BaseName),
    (	
	exists_file(BaseName)
    ->
	WStream = current_output    
    ;
        open(BaseName, write, WStream)
    ),
	read_term(Stream, Proof0, [variable_names(VarNameList)]),
        convert_all(Proof0, VarNameList, Stream, WStream).

% = convert_all(+ReadTerm, +ReadVariables, +InStream, +OutStream)

% continue reading Terms until end_of_file, converting all Terms
% and outputting them to OutStream

convert_all(end_of_file, _, Stream, WStream) :-
	close(Stream),
	close(WStream).
convert_all(proof(SentNo, ProofRules0), VarNameList, Stream, WStream) :-
	/* replace variables by source '$VAR'(N) terms */
	denumbervars(ProofRules0, VarNameList),
	/* convert unbound '$VAR'(N) (less than sentence length) to word(N) */
	count_words(ProofRules0, NumWords),
	replace_input_words(ProofRules0, ProofRules, NumWords),
        print_proof(SentNo, ProofRules, WStream),
	read_term(Stream, Proof1, [variable_names(VarNameList1)]),
        convert_all(Proof1, VarNameList1, Stream, WStream).

% = convert_single(+FileName)
%
% a version of convert_all/1 which presupposes FileName contains only a single term (it ignores
% everything beyond that).

convert_single(FileName) :-
	open(FileName, read, Stream),
	file_base_name(FileName, BaseName),
	read_term(Stream, Proof0, [variable_names(VarNameList)]),
	close(Stream),
	Proof0 = proof(SentNo, ProofRules0),
	denumbervars(Proof0, VarNameList),
	count_words(ProofRules0, NumWords),
	replace_input_words(ProofRules0, ProofRules, NumWords),
   (	
	exists_file(BaseName)
   ->
        print_proof(SentNo, ProofRules, current_output)
    ;
        open(BaseName, write, WStream),
        print_proof(SentNo, ProofRules, WStream),
	close(WStream)
    ).		   

% replace_input_words(+Proof

replace_input_words(rule(Nm, Pros, Form-Sem0, Rules0), rule(Nm, Pros, Form-Sem, Rules), NumWords) :-
	replace_sem_words(Sem0, Sem, NumWords, []),
	replace_input_words_list(Rules0, Rules, NumWords).

replace_input_words_list([], [], _).
replace_input_words_list([P0|Ps0], [P|Ps], NumWords) :-
	replace_input_words(P0, P, NumWords),
	replace_input_words_list(Ps0, Ps, NumWords).

% = replace_sem_words(+InSem, -OutSem, +NumWords, +BoundVariables)
%
% replace all subterms '$VAR'(N) of InSem by word(N) in OutSem, provided that
%
% 1. N is smaller than the NumWords (it corresponds to a word in the sentence)
% 2. N is not bound

replace_sem_words(X, Y, _, _) :-
	var(X),
	!,
	Y = X.
replace_sem_words(true, true, _, _).
replace_sem_words('$VAR'(N), Var, NumWords, Bound) :-
   (
        ord_member(N, Bound)
   ->
        Var = '$VAR'(N)
   ;		       
        N < NumWords
   ->
        Var = word(N)
   ;
       /* unbound variable */
        Var = '$VAR'(N)
   ).
replace_sem_words(appl(A0, B0), appl(A, B), NumWords, Bound) :-
	replace_sem_words(A0, A, NumWords, Bound),
	replace_sem_words(B0, B, NumWords, Bound).
replace_sem_words(lambda(X, A0), lambda('$VAR'(N), A), NumWords, Bound0) :-
    (
	X = '$VAR'(N)
    ->
	true
    ;
	format('~N{Error: bound lexical variable $VAR(~w)}~n', [N]),fail
    ),
	ord_insert(Bound0, N, Bound),
	replace_sem_words(A0, A, NumWords, Bound).
replace_sem_words(pair(A0, B0), pair(A, B), NumWords, Bound) :-
	replace_sem_words(A0, A, NumWords, Bound),
	replace_sem_words(B0, B, NumWords, Bound).
replace_sem_words(pi1(A0), pi1(A), NumWords, Bound) :-
	replace_sem_words(A0, A, NumWords, Bound).
replace_sem_words(pi2(A0), pi2(A), NumWords, Bound) :-
	replace_sem_words(A0, A, NumWords, Bound).

% = count_words(+ProsLabel, ?NumWords)
%
% true if NumWords is the number of leaves of ProsLabel.

count_words(rule(_, Label, _, _), NumWords) :-
	count_words_label(Label, 0, NumWords).

count_words_label(V, N0, N) :-
	var(V),
	!,
	N is N0 + 1.
count_words_label(p(_,A,B), N0, N) :-
	!,
	count_words_label(A, N0, N1),
	count_words_label(B, N1, N).
count_words_label(_, N0, N) :-
	N is N0 + 1.

% = denumbervars(?Term, +VarNamesList)
%
% instantiates the variables in Term which appear named 'A' and 'B2' in VarNamesList
% to the appropriate numbervars output, eg. '$VAR'(0) and '$VAR'(53) for the previous
% examples.

denumbervars(Term, List) :-
   (	
	var(Term)
   ->
	find_var(List, Term)
   ;
        atomic(Term)
   ->
        true
   ;
        denumbervars_term(Term, List)
   ).

denumbervars_term(Term, List) :-
	functor(Term, _F, A),
	denumbervars_args(0, A, Term, List).

denumbervars_args(A0, A, Term, List) :-
   (	
	   A0 < A
   ->
	   A1 is A0 + 1,
	   arg(A1, Term, Arg),
	   denumbervars(Arg, List),
	   denumbervars_args(A1, A, Term, List)
   ;
           true 
   ).

find_var([X=Y|Rest], Z) :-
   (
	Z == Y
   ->
	denumvar(X, Z)
   ;
        find_var(Rest, Z)
   ).
find_var([], Z) :-
	format(user_error, '~N{Warning: unknown variable ~w}~n', [Z]).

% = "inverse" the pretty-printer for terms of the form '$VAR'(N)

denumvar(X, '$VAR'(N)) :-
	name(X, [Let|Numbers]),
	N0 is Let - 65,
   (	
	Numbers = []
   ->
        N1 = 0
   ;
        number_codes(N1, Numbers)
   ),
	N is (N1*26)+N0.
