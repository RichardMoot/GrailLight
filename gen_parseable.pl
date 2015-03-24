
% unparsed (without pl extention, add it yourself) is a file automatically produced by Grail Light.
:- compile(unparsed).
% specify the file used for the parse_all call of Grail Light here
:- compile(ml_vpmod).

start :-
	/* use your own filename here */
	tell('ml_linearone.pl'),
	clause(sent(N, Sem), Body),
	( unparsed(N, _, _) -> true ; portray_clause((sent(N,Sem):-Body))),
	fail.
start :-
	told.
