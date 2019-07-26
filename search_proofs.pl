#!/Applications/SWI-Prolog.app/Contents/MacOS/swipl -q -g start -f

:- use_module(print_proof, [print_proof/3,print_proof/4]).

:- dynamic user:proof/2.
infile(aa1).
infile(ap1).
infile(aq2).
infile(as2).
infile(at).
infile(300).
infile(1000).
infile(8000).
infile(annodis).
infile(frwiki1).
infile(frwiki2).
infile(europar).
infile(emea_d).

silver_infile(aa2).
silver_infile(ab2).
silver_infile(ae1).
silver_infile(af2).
silver_infile(ag1).
silver_infile(ag2).
silver_infile(ah1).
silver_infile(ah2).
silver_infile(ai1).
silver_infile(ai2).
silver_infile(aj1).
silver_infile(ak1).
silver_infile(ak2).
silver_infile(al1).
silver_infile(am1).
silver_infile(am2).
silver_infile(an1).
silver_infile(an2).
silver_infile(ao1).
silver_infile(ao2).
silver_infile(monde_a).
silver_infile(monde_b).

mode(gold).

proof_file(File) :-
	mode(gold),
	infile(File).
proof_file(File) :-
	mode(silver),
	silver_infile(File).

nd_dir('/Users/moot/checkout/TLGbank/nd_proofs/').


% patterns of 5-node trees
% t1 = a(b(c(de)))
pattern(t1, rule(_,_,_,[
			 rule(_,_,_-word(_),[]),
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])])])])])).
% t2 = a(b((cd)e))
pattern(t2, rule(_,_,_,[
			 rule(_,_,_-word(_),[]),
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])]),
						   rule(_,_,_-word(_),[])])])])).
% t3 = (ab)(c(de))
pattern(t3, rule(_,_,_,[
			 rule(_,_,_,[rule(_,_,_-word(_),[]),
				     rule(_,_,_-word(_),[])]),
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])])])
			 ])).
% t4 = a((bc)(de))
pattern(t4, rule(_,_,_,[
			 rule(_,_,_-word(_),[]),
			 rule(_,_,_,[
				      rule(_,_,_,[rule(_,_,_-word(_),[]),
						  rule(_,_,_-word(_),[])]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])])
			 ])])).
% t5 = a((b(cd))e)
pattern(t5, rule(_,_,_,[
			 rule(_,_,_-word(_),[]),
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])])]),
				      rule(_,_,_-word(_),[])])])).
% t6 = a(((bc)d)e)
pattern(t6, rule(_,_,_,[
			 rule(_,_,_-word(_),[]),
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])]),
						   rule(_,_,_-word(_),[])]),
				      rule(_,_,_-word(_),[])])])).
% t7 = (ab)((cd)e)
pattern(t7, rule(_,_,_,[
			 rule(_,_,_,[rule(_,_,_-word(_),[]),
				     rule(_,_,_-word(_),[])]),
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])]),
				      rule(_,_,_-word(_),[])])
			 ])).
% t8 = (a(bc))(de)
pattern(t8, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])])]),
			 rule(_,_,_,[rule(_,_,_-word(_),[]),
				     rule(_,_,_-word(_),[])])

			 ])).
% t9 = (a(b(cd)))e
pattern(t9, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])])])]),
			 rule(_,_,_-word(_),[])
		 ])).
% t10 = (a((bc)d))e
pattern(t10, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_-word(_),[]),
				      rule(_,_,_,[
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])]),
						   rule(_,_,_-word(_),[])])]),
			 rule(_,_,_-word(_),[])
		 ])).

% t11 = ((ab)(cd))e
pattern(t11, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_,[rule(_,_,_-word(_),[]),
						  rule(_,_,_-word(_),[])]),
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])])
			      ]),
			 rule(_,_,_-word(_),[])
		  ])).
% t12 = ((ab)c)(de)
pattern(t12, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_-word(_),[])]),
				      rule(_,_,_-word(_),[])]),
			 rule(_,_,_,[rule(_,_,_-word(_),[]),
				     rule(_,_,_-word(_),[])])
			 ])).
% t13 = ((a(bc))d)e
pattern(t13, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_-word(_),[]),
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])])]),
				      rule(_,_,_-word(_),[])]),
			 rule(_,_,_-word(_),[])
		 ])).

% t14 = (((ab)c)d)e
pattern(t14, rule(_,_,_,[
			 rule(_,_,_,[
				      rule(_,_,_,[
						   rule(_,_,_,[
								rule(_,_,_-word(_),[]),
								rule(_,_,_-word(_),[])]),
						   rule(_,_,_-word(_),[])]),
				      rule(_,_,_-word(_),[])]),
			 rule(_,_,_-word(_),[])
		  ])).

%% pattern(1, rule(dl,_,lit(n)-_,[
%% 			rule(axiom,_,lit(n)-word(_),[]),
%% 			rule(dr,_,dl(0,lit(n),lit(n))-_,[
%% 				     rule(axiom,_,dr(0,dl(0,lit(n),lit(n)),lit(n))-word(_),[]),
%% 				     rule(dl,_,lit(n)-_,[
%% 						  rule(axiom,_,lit(n)-word(_),[]),
%% 						  rule(axiom,_,dl(0,lit(n),lit(n))-word(_),[])])])])).
%% pattern(2, rule(dl,_,lit(n)-_,[
%% 			rule(dl,_,lit(n)-_,[
%% 				     rule(axiom,_,lit(n)-word(_),[]),
%% 				     rule(dr,_,dl(0,lit(n),lit(n))-_,[
%% 						  rule(axiom,_,dr(0,dl(0,lit(n),lit(n)),lit(n))-word(_),[]),
%% 						  rule(axiom,_,lit(n)-word(_),[])])]),
%% 			rule(axiom,_,dl(0,lit(n),lit(n))-word(_),[])])).
%% % two readings for n (n\n)/np np/n n n\n (more generally A (A\A)/B B/A A A\A)
%% pattern(3, rule(dl,_,A-_,[
%% 			rule(axiom,_,A-word(_),[]),
%% 			rule(dr,_,dl(_,A,A)-_,[
%% 				     rule(axiom,_,dr(_,dl(_,A,A),B)-word(_),[]),
%% 				     rule(dr,_,lit(np(_,_,_))-_,[
%% 						  rule(axiom,_,dr(_,B,A)-word(_),[]),
%% 						  rule(dl,_,A-_,[
%% 							       rule(axiom,_,A-word(_),[]),
%% 							       rule(axiom,_,dl(_,A,A)-word(_),[])])])])])).

%% pattern(4, rule(dl,_,A-_,[
%% 			rule(dl,_,A-_,[
%% 				     rule(axiom,_,A-word(_),[]),
%% 				     rule(dr,_,dl(_,A,A)-_,[
%% 						  rule(axiom,_,dr(_,dl(_,A,A),B)-word(_),[]),
%% 						  rule(dr,_,lit(np(_,_,_))-_,[
%% 							       rule(axiom,_,dr(_,B,A)-word(_),[]),
%% 							       rule(axiom,_,A-word(_),[])
%% 						       ])])]),
%% 			rule(axiom,_,dl(_,A,A)-word(_),[])])).

start :-
	current_prolog_flag(os_argv, Argv),
        append(_, [A|Av], Argv),
	file_base_name(A, 'search_proofs.pl'),
	!,
	nd_dir(NDDir),
        sap(Av, NDDir).


sap([], _) :-
	/* treat all files if no file argument specified */
	search_all_proofs,
	halt.
sap([F|Fs], NDDir) :-
	sap1([F|Fs], NDDir).

sap1([], _) :-
	halt.
sap1([File0|Files], NDDir) :-
   (	
	match_file(File0, File)
   ->		     
	format(user_error, '~NStarting ~w~n', [File]),
	atom_concat(File, '_nd.pl', File0),
	atom_concat(NDDir, File0, File),
	abolish(user:proof/2),
	user:compile(File),
	search_all_proofs(File),
	format(user_error, '~NDone ~w~n', [File])
   ;
	format(user_error, '~NIgnored ~w~n', [File0])
   ),
	sap1(Files, NDDir).


%
% search all infiles chart proofs in chart_dir into natural deduction proofs in nd_dir.

search_all_proofs :-
	nd_dir(NDDir),
	proof_file(Root),
	format(user_error, '~NStarting ~w~n', [Root]),
	atom_concat(Root, '_nd.pl', InFile0),
	atom_concat(NDDir, InFile0, InFile),
	atom_concat(Root, '_matches.pl', OutFile),
	abolish(proof/2),
	compile(InFile),
	search_all_proofs(OutFile),
	format(user_error, '~NDone ~w~n', [Root]),
	fail.
search_all_proofs.

% = search_all_proofs(+OutFile)
%
% search all proofs in memory (per the proof/2 predicate) into natural deduction
% files in the OutFile given as an argument.

search_all_proofs(OutputFile) :-
	open(OutputFile, write, Stream, []),
	search_all_proofs1(Stream).

search_all_proofs1(Stream) :-
	user:proof(N, Proof),
	pattern(Id, Pattern),
	search_proof(Proof, N, Id, Pattern, Stream),
	fail.
search_all_proofs1(Stream) :-
	close(Stream).


search_proof(Proof, N, Id, Pattern, Stream) :-
	Pattern = Proof,
	format(user_error, '(~p)', [Id]),
	print_proof(Id-N, Pattern, Stream, true).
	
% recursive case
search_proof(rule(_Nm, _Pros, _F, Ds), N, Id, Pattern, Stream) :-
	search_proof_list(Ds, N, Id, Pattern, Stream).

search_proof_list([], _, _, _, _).
search_proof_list([P|Ps], N, Id, Pattern, Stream) :-
	search_proof(P, N, Id, Pattern, Stream),
	search_proof_list(Ps, N, Id, Pattern, Stream).


proof_pros(rule(_,Pros,_,_), Pros).
   
match_file(File0, File) :-
	/* coverts atoms like '8000' and '300' to Prolog integers */
	name(File0, Name),
	name(File1, Name),
	proof_file(File1),
	!,
	File = File1.
match_file(File0, File) :-
	name('nd_proofs/', X),
	name('_nd.pl', W),
	name(File0, FN),
	append(X, Y, FN),
	append(V, W, Y),
	name(File, V),
	proof_file(File).
