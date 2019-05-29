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
semantics_tense_pres(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(temps(I),overlaps,maintenant)]))))).
semantics_tense_past(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(temps(I),<,maintenant)]))))).

% the "aspectual" operators are of type (v->t)->(v->t)
semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,<,J)])))).
%semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(sub(I,a),<,J)])))).
semantics_aspect_post_imm(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,abuts,J)])))).
semantics_aspect_perf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(K,<,I)])))).
semantics_aspect_impf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(I,subseteq,K)])))).
semantics_aspect_anch(lambda(Phi, lambda(J, drs([K],[appl(Phi,K),bool(K,=,'event?'),bool(K,<,J)])))).


pos_time_drs(pres, E-[bool(E,overlaps,maintenant),bool(E,overlaps,ref_time)]) :-
	!.
pos_time_drs(impf, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(ref_time,overlaps,S1),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(simp, EVs, EVs, E-[bool(appl(temps,E),<,maintenant),bool(ref_time,<,maintenant),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(cond, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(S1,<,E),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(futu, EVs, EVs, E-[bool(maintenant,<,appl(temps,E)),bool(ref_time,overlaps,maintenant)]) :-
	!.
pos_time_drs(_, EVs, EVs, _-[]).

pos_time_verkuyl(pres, EVs, EVs, E-[bool(appl(temps,E),overlaps,maintenant)]) :-
	!.
pos_time_verkuyl(impf, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),subseteq,appl(temps,E)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,X),overlaps,maintenant)])]) :-
	!.
%pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(sub(appl(temps,X),a),<,appl(temps,E)),bool(appl(temps,X),overlaps,maintenant)])]) :-
%	!.
pos_time_verkuyl(cond, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
pos_time_verkuyl(simp, EVs, EVs, E-[drs([event(X)],[bool(X,=,'event?'),bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,E),<,maintenant)])]) :-
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


default_semantics('*FAIL*', '*FAIL*', '*FAIL*').

default_semantics(Word, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).


default_semantics(W, nom, lit(n), lambda(X,drs([],[appl(W,X)]))).

default_semantics(Word, nam, lit(np(_,_,_)), lambda(P,presup(drs([variable(X)],[appl(appl(nommé,Word),X)]),appl(P,X)))).

lex(the, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,presup(merge(drs([variable(X)],L),appl(P,X)),appl(Q,X))))) :-
	singleton_sets(X, L).
lex(them, lit(np(nom,il,3-p)), lambda(P,merge(drs([variable(X)],[bool(num(X),>,1),bool(X,=,'?')]),appl(P,X)))).
lex(then, dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).

sent(1, A) :-
	prob_parse(
		   [ si('Oliver', det-det:art, 'Oliver', [np-1]),
		     si(stole,
			v-ver:impf,
			steal,
			[dr(0, dl(0, np, s), np)-1]),
		     si(the, det-det:art, les, [dr(0, np, n)-1]),
		     si('books\\_on\\_the\\_shelf', nc-nom, 'books\\_on\\_the\\_shelf', [n-1]),
		     si(then, c-prp, du, [dr(0, dl(0, dl(0, np, s), dl(0, np, s)), dl(0, np, s))-1]),
		     si(read,
			v-ver:impf,
			read,
			[dr(0, dl(0, np, s), np)-1]),
		     si(them, pro-per, them, [np-1]),
		     si('.', ponct-pun, '.', [dl(0, s, txt)-1])
		   ],
		   A).		     
