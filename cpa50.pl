:- compile('~/checkout/Grail/source/g3').
:- compile('~/checkout/Grail/grammars/big_french_drt').
:- retractall(sequence_semantics(_,_,_,_,_,_)).

start1 :-
	compile(apdi50),
	chart_parse_all,
	rename_file(unparsed,unparsed50).
