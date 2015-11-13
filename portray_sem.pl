
portray(appl(X,Y)) :-
	!,
	format('(~p @ ~p)', [X,Y]).
portray(lambda(X,Y)) :-
	!,
	format('(~p^~p)', [X,Y]).
portray(word(N)) :-
	!,
	format('w_~p', [N]).
