% -*- Mode: Prolog -*-
% ============================================================
% big_french_drt.pl
% ============================================================
% !grail 3.1.1

:- encoding(utf8).
:- ensure_loaded(translate_form).

% these variables have little effect for the moment, since a lot
% of the neo-Davidsonian DRT semantics is hard-coded.

event_semantics(classic).
discourse_semantics(drt).

tense_aspect(verkuyl).


invisible_mode(0).

atomic_type(n, e->t).
atomic_type(np, (e->t)->t).
atomic_type(pp, (e->t)->t).
atomic_type(s, s->t).
atomic_type(s(_), s->t).
atomic_type(cs, s->t).
atomic_type(txt, t).


special_string(".", '.').
special_string(";", ';').
special_string(":", ':').
special_string(",", ',').
special_string("!", '!').
special_string("?", '?').

raising_verb(seem).
raising_verb(appear).

weather_verb(rain).
weather_verb(snow).


macro(np, lit(np(_,_,_))).
macro(s, lit(s(_))).

% =====================================
% =        Tense information          =
% =====================================

% pos_time(+VerbForm, -EventList)
% compatibility only, subsumed by pos_time/4, which allows
% the introduction of additional (event) variables in the
% main DRS.

pos_time(VForm, EList) :-
	pos_time(VForm, _, _, EList),
	format(user_error, 'POS_TIME ~w ~w~n', [VForm,EList]).

pos_time(VForm, EVs0, EVs, EList) :-
	tense_aspect(TA),
	!,
	pos_time(TA, VForm, EVs0, EVs, EList).
pos_time(no, _, EVs, EVs, _-[]) :-
	!.
pos_time(none, _, EVs, EVs, _-[]) :-
	!.
pos_time(drs, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_drs(TENSE, EVs0, EVs, EList).
pos_time(verkuyl, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_verkuyl(TENSE, EVs0, EVs, EList).
pos_time(yes, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_verkuyl(TENSE, EVs0, EVs, EList).
pos_time(_, _, EVs, EVs, _-[]).

% the tense operators "present" and "past" are of type (v->t)->(v->t)

semantics_tense_pres(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(time(I),overlaps,now)]))))).
semantics_tense_past(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(time(I),<,now)]))))).

% the "aspectual" operators are of type (v->t)->(v->t)
semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,<,J)])))).
%semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(sub(I,a),<,J)])))).
semantics_aspect_post_imm(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,abuts,J)])))).
semantics_aspect_perf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(K,<,I)])))).
semantics_aspect_impf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(I,subseteq,K)])))).
semantics_aspect_anch(lambda(Phi, lambda(J, drs([K],[appl(Phi,K),bool(K,=,'event?'),bool(K,<,J)])))).


pos_time_drs(pres, E-[bool(E,overlaps,now),bool(E,overlaps,ref_time)]) :-
	!.
pos_time_drs(impf, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(ref_time,overlaps,S1),bool(ref_time,overlaps,appl(time,E))]) :-
	!.
pos_time_drs(simp, EVs, EVs, E-[bool(appl(time,E),<,now),bool(ref_time,<,now),bool(ref_time,overlaps,appl(time,E))]) :-
	!.
pos_time_drs(cond, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(S1,<,E),bool(ref_time,overlaps,appl(time,E))]) :-
	!.
pos_time_drs(futu, EVs, EVs, E-[bool(now,<,appl(time,E)),bool(ref_time,overlaps,now)]) :-
	!.
pos_time_drs(_, EVs, EVs, _-[]).

pos_time_verkuyl(pres, EVs, EVs, E-[bool(appl(time,E),overlaps,now)]) :-
	!.
pos_time_verkuyl(impf, EVs, EVs, E-[drs([event(X)],[bool(appl(time,X),subseteq,appl(time,E)),bool(appl(time,X),<,now)])]) :-
	!.
pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(appl(time,X),<,appl(time,E)),bool(appl(time,X),overlaps,now)])]) :-
	!.
%pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(sub(appl(time,X),a),<,appl(time,E)),bool(appl(time,X),overlaps,now)])]) :-
%	!.
pos_time_verkuyl(cond, EVs, EVs, E-[drs([event(X)],[bool(appl(time,X),<,appl(time,E)),bool(appl(time,X),<,now)])]) :-
	!.
pos_time_verkuyl(simp, EVs, EVs, E-[drs([event(X)],[bool(X,=,'event?'),bool(appl(time,X),<,appl(time,E)),bool(appl(time,E),<,now)])]) :-
	!.
pos_time_verkuyl(_, EVs, EVs, _-[]).

% =====================================
% =           Role lexicon            =
% =====================================


singleton_sets(_, []).

%get_roles(Verb, List, Roles) :-
%	get_roles1(Verb, List, Roles),
%	!.
get_roles(Verb, [A|As], Roles) :-
	default_roles(As, A, Verb, Roles).

default_roles([], np, _, [agent]).
default_roles([np], np, _, [agent,patient]).
default_roles([np], cl_r, _, [agent,patient]).
default_roles([np,np], np, _, [agent,patient,theme]).
default_roles([np,inf(_)], np, _, [agent,patient,theme]).
default_roles([pp(_)], np, _, [agent,theme]).
default_roles([np,pp(_)], np, _, [agent,theme,patient]).
default_roles([pp(_),np], np, _, [agent,patient,theme]).

% = add roles

add_roles(L, P, E, R0, R) :-
	event_semantics(prolog),
	!,
	add_roles_predicate(L, P, E, R0, R).

add_roles(L, P, E, R0, R) :-
	event_semantics(classic),
	!,
	R0 = [appl(T,E)|R],
	add_roles_classic(L, P, T).

add_roles(L, P, E, R0, R) :-
	event_semantics(neo),
	!,
	add_roles_neo(L, P, E, R0, R).

add_roles_neo(L, P, E) -->
	[appl(P,E)],
	add_roles_neo_args(L, E).

add_roles_neo_args([], _) -->
	[].
add_roles_neo_args([R-V|Rest], E) -->
	[appl(appl(R,V),E)],
	add_roles_neo_args(Rest, E).

add_roles_classic([], P, P).
add_roles_classic([_-A|Rest], P, appl(T,A)) :-
	add_roles_classic(Rest, P, T).

		  
add_roles_predicate(L0, P, E) -->
	{strip_keys(L0, L),
	T =.. [P,E|L]},
	[T].

% ====================
% = semantic recipes =
% ====================

semantics(dot, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_semantics(ES, Dis, Sem).

semantics(dot_np, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_np_semantics(ES, Dis, Sem).

dot_semantics(none, _, lambda(X,X)).
dot_semantics(classic, Dis, Sem) :-
	dot_semantics1(Dis, Sem).
dot_semantics(neo, Dis, Sem) :-
	dot_semantics1(Dis, Sem).

dot_semantics1(drt, lambda(P, merge(drs([event(E)],[]),appl(P, E)))). 

dot_np_semantics(none, _, lambda(X,X)).
dot_np_semantics(classic, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).
dot_np_semantics(neo, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).

dot_np_semantics1(drt, lambda(P,appl(P,lambda(_V,drs([],[]))))).

noun_semantics(drt, Word, lambda(V,drs([],[appl(Word,V)]))).

% default semantics

default_semantics('*FAIL*', '*FAIL*', '*FAIL*').

default_semantics(Word, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).


default_semantics(W, nom, lit(n), lambda(X,drs([],[appl(W,X)]))).


default_semantics(Word, nam, lit(np(_,_,_)), lambda(P,presup(drs([variable(X)],[appl(appl(named,Word),X)]),appl(P,X)))).

lex(the, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,presup(merge(drs([variable(X)],L),appl(P,X)),appl(Q,X))))) :-
	singleton_sets(X, L).
lex(a, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,merge(drs([variable(X)],[]),merge(appl(P,X),appl(Q,X)))))).

lex(she, lit(np(nom,_,3-s)), lambda(P,merge(drs([variable(X)],[bool(X,=,'feminine?')]),appl(P,X)))).
lex(it, lit(np(_,_,3-s)), lambda(P,merge(drs([variable(X)],[bool(X,=,'non-human?')]),appl(P,X)))).
lex(them, lit(np(acc,il,3-p)), lambda(P,merge(drs([variable(X)],[bool(num(X),>,1),bool(X,=,'?')]),appl(P,X)))).

lex(and, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,lambda(X,appl(P,X))),appl(NP1,lambda(Y,appl(P,Y)))))))).

lex(then, dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(time,E),<,appl(time,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex('If', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(merge(drs([event(E)],[]),appl(P,E)),->,merge(drs([event(F)],[]),appl(Q,F)))]))))).
lex(if,   dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(merge(drs([event(E)],[]),appl(P,E)),->,merge(drs([event(F)],[]),appl(Q,F)))]))))).


% = interpunction

lex('.', dl(0,dr(0,s,s),txt), lambda(SMOD,appl(appl(SMOD,lambda(_,drs([event(E)],[bool(E,=,'event?')]))),E))).
lex('.', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('?', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('...', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('.', dl(0,lit(np(_,_,_)),lit(txt)), Sem) :-
	semantics(dot_np, Sem).
lex('.', dl(0,lit(n),lit(txt)), lambda(N,merge(drs([variable(X)],[]),appl(N,X)))).


sent(1, A) :-
	prob_parse(
		   [ si('Oliver', npp-nam, 'Oliver', [np-1]),
		     si(stole,
			v-ver:impf,
			steal,
			[dr(0, dl(0, np, s), np)-1]),
		     si(the, det-det:art, les, [dr(0, np, n)-1]),
		     si('books\\_on\\_the\\_shelf', nc-nom, 'books\\_on\\_the\\_shelf', [n-1]),
		     si(then, c-prp, then, [dr(0, dl(0, dl(0, np, s), dl(0, np, s)), dl(0, np, s))-1]),
		     si(read,
			v-ver:impf,
			read,
			[dr(0, dl(0, np, s), np)-1]),
		     si(them, pro-per, them, [np-1]),
		     si('.', ponct-pun, '.', [dl(0, s, txt)-1])
		   ],
		   A).		     

sent(2, A) :-
	prob_parse(
	    [ si('If', cc-kon, 'Oliver', [dr(0, dr(0, s, s), s)-1]),

	     si('Emma', npp-nam, 'Emma', [np-1]),
		     si(finds,
			v-ver:pres,
			find,
			[dr(0, dl(0, np, s), np)-1]),
 		     si(a, det-det:art, a, [dr(0, np, n)-1]),
		     si('book', nc-nom, 'book', [n-1]),
  		     si(she, pro-per, she, [np-1]),
 		     si(leaves,
			v-ver:pres,
			leave,
			[dr(0, dl(0, np, s), np)-1]),
 		     si(it, pro-per, it, [np-1]),
 		     si('.', ponct-pun, '.', [dl(0, s, txt)-1])
		   ],
		   A).		     

sent(3, A) :-
    	prob_parse(
    [                si('Bob', npp-nam, 'Bob', [np-1]),
                     si(and, cc-crd, and, [dr(0,dl(0,np,np),np)-1]),
                     si('Susan', npp-nam, 'Susan', [np-1]),
		     si(own,
			v-ver:pres,
			own,
			[dr(0, dl(0, np, s), np)-1]),
		     si(a, det-det:art, a, [dr(0, np, n)-1]),
		     si('donkey', nc-nom, 'donkey', [n-1]),
 		     si('.', ponct-pun, '.', [dl(0, s, txt)-1])
		   ],
		   A).		     

