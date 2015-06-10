
:- use_module(latex, [latex_proof/2]).
:- use_module(remove_epsilon, [remove_epsilon/2]).
:- use_module(merge_mwu, [merge_mwu/2]).
:- use_module(print_lexicon, [print_lexicon/1]).
:- use_module(trees, [tree/3]).

invisible_mode(0).

extract :-
	open('proof.tex', write, _, [alias(proof)]),
	extract_all,
	close(proof),
	shell('pdflatex latex_proofs.tex > /dev/null', Res),
   (
        Res = 0
   ->
        Msg = done
   ;
        Msg = failed
   ),
	format('~NLaTeX ~w', [Msg]).

extract_all :-
	tree(Num, Tree0, _),
	remove_epsilon(Tree0, Tree1),
	merge_mwu(Tree1, Tree),
	extract(Tree, txt, Proof),
	numbervars(Proof, 0, N),
   (
        N > 0
   ->
        format('~N{Warning: sentence ~w has ~w free Prolog variables in proof}', [Num,N])
   ;
        true
   ),
        format(proof, '~w. ', [Num]),
	latex_proof(Proof, proof),
	print_lexicon(Proof),
	format(proof, '~2n\\bigskip~2n', []),
	write('.'),
	fail.
extract_all.

extract(N, Proof) :-
	tree(N, Tree0, _),
	remove_epsilon(Tree0, Tree),
	extract(Tree, txt, Proof).

extract(leaf(_,_,_,_,_,W,_,_), F, rule(ax, W, F, [])).
% skip unary branches
extract(node(_,_,_,_,_,[D]), F, Proof) :-
	!,
    (
        F = txt
    ->
        /* don't use txt for sentences without interpunction */
	argument_formula(D, --, F0)
    ;
        F0 = F
    ),
	extract(D, F0, Proof).
extract(node(P,_,_,_,_,Ds), F, Proof) :-
	select_head(Ds, P, H, Ls, Rs),
	list_formulas(Ls, P, l, LForms, LMods),
	list_formulas(Rs, P, r, RForms, RMods),
	reverse(LForms, LF),
	reverse(LMods, LM),
	reverse(RForms, RF),
	reverse(RMods, RM),
	extract_list(Ls, LForms, LProofs),
	extract_list(Rs, RForms, RProofs),
	combine_formulas(LF, LM, RF, RM, F, HF),
	extract(H, HF, HProof),
	LP = LProofs,
	RP = RProofs,
%	reverse(LProofs, LP),
%	reverse(RProofs, RP),
	combine_proofs(RP, LP, HProof, Proof).

extract_list([], [], []).
extract_list([T|Ts], [F|Fs], [P|Ps]) :-
	extract(T, F, P),
	extract_list(Ts, Fs, Ps).

select_head(Ds, Par, H, Ls, Rs) :-
    (
	select_head(Ds, Par, H, [], Ls, Rs)
    ->
        true
    ;
	list_pos_role(Ds, PR),
        format('{Warning: no head found for parent ~w amongst ~w}', [Par,PR]),
        fail
    ).


select_head([D|Ds], Par, D, Ls, Ls, Ds) :-
	node_role(D, R),
	node_pos(D, P),
	is_head(Par, R, P),
	!.
select_head([D|Ds], Par, H, Ls0, Ls, Rs) :-
	select_head(Ds, Par, H, [D|Ls0], Ls, Rs).


list_pos_role([], []).
list_pos_role([D|Ds], [P-R|PRs]) :-
	node_role(D, R),
	node_pos(D, P),
	list_pos_role(Ds, PRs).

list_formulas([], _, _, [], []).
list_formulas([A|As], P, LR, [F|Fs], [M|Ms]) :-
    (
	is_modifier(A, P)
    ->
        M = 1,
        modifier_formula(LR, F)
    ;
        M = 0,
	argument_formula(A, P, F)
    ),
    list_formulas(As, P, LR, Fs, Ms).

modifier_formula(l, dr(0,A,A)).
modifier_formula(r, dl(0,A,A)).

% = argument_formula(TreeNode, ParentPOStag, Formula)
%
% true if Formula corresponds TreeNode (when occurring as an argument)

argument_formula(Node, P, F) :-
        node_pos(Node, Pos),
 	node_role(Node, Role),
    (
        Role = svp,
        node_lemma(Node, Lemma)
    ->
        F = Lemma
    ;
	pos_formula(Pos, P, F)
    ).

% = pos_formula(POStag, ParentPOStag, Formula)
%
% true if Formula corresponds to POStag (when occurring as an argument)
% given that the parent has ParentPOStag.

pos_formula(adj, _, dr(0,np,np)) :-
	!.
pos_formula(ap, _, dr(0,np,np)) :-
	!.
pos_formula(du, _, s) :-
	!.
pos_formula(smain, _, s) :-
	!.
pos_formula(vnw, _, np) :-
	!.
pos_formula(n, _, np) :-
	!.
pos_formula(spec, _, np) :-
	!.

pos_formula(X, _, X).

% = node_role(TreeNode, NodeRole)
% recovers the NodeRole argument of a TreeNode)

node_role(node(_,_,R,_,_,_), R).
node_role(leaf(_,_,R,_,_,_,_,_), R).

% = node_pos(TreeNode, POStag)
% recovers the POStag/syntactic category of TreeNode

node_pos(node(P,_,_,_,_,_), P).
node_pos(leaf(P,_,_,_,_,_,_,_), P).

% = node_daughters(TreeNode, Daughters)
% recovers the list of Daughters of a Tree Node

node_daughters(node(_,_,_,_,_,Ds), Ds).
node_daughters(leaf(_,_,_,_,_,_,_,_), []).

node_lemma(leaf(_,_,_,_,_,_,Lemma,_), Lemma).

% is_modifier(TreeNode, ParentCategory)
% true if TreeNode is a modifier of ParentCategory

is_modifier(Node, P) :-
	node_pos(Node, Pos),
	node_role(Node, Role),
	is_modifier(P, Role, Pos).

% is_modifier(Category, Role, ParentCategory)
% true if Category is a modifier when it occurs as a a daughter of
% ParentCategory with the specified Role.

is_modifier(np, _, _) :-
	!.
is_modifier(mwu, _, _) :-
	!.
is_modifier(_, mod, _) :-
	!.
is_modifier(_, app, _).

% is_head(ParentCategory, Role, Category)
% true if Category is the head of ParentCategory when it occurs with
% the given Role.

is_head(top, --, let) :-
	!.
is_head(_, hd, _) :-
	!.
is_head(rel, rhd, _) :-
	!.
is_head(_, mwp, _) :-
	!.
is_head(conj, crd, _) :-
	!.
is_head(cp, cmp, _) :-
	!.
is_head(ti, cmp, _) :-
	!.
is_head(du, dlink, _) :-
	!.
is_head(whsub, whd, _) :-
	!.



combine_proofs([], LPs, Pr0, Pr) :-
	combine_proofs(LPs, Pr0, Pr).
combine_proofs([RP|RPs], LPs, Pr0, Pr) :-
   (
	proof_conclusion(Pr0, WL, dr(0,F,FA)),
        (var(F) -> F \== FA ; true),
	proof_conclusion(RP, WR, FA)
   ->
        true
   ;
        proof_conclusion(Pr0, WL, F),
        proof_conclusion(RP, WR, dl(0,F,F))
   ),
	combine_proofs(RPs, LPs, rule(dl, p(0,WL,WR), F, [Pr0,RP]), Pr).

combine_proofs([], Pr, Pr).
combine_proofs([LP|LPs], Pr0, Pr) :-
   (
	proof_conclusion(Pr0, WR, dl(0,FA,F)),
        (var(F) -> F \== FA ; true),
	proof_conclusion(LP, WL, FA)
   ->
        true
   ;
        proof_conclusion(Pr0, WR, F),
        proof_conclusion(LP, WL, dr(0,F,F))
   ),
	combine_proofs(LPs, rule(dl, p(0,WL,WR), F, [LP,Pr0]), Pr).

combine_formulas([], [], RFs, RMs, FH0, FH) :-
	combine_formulas(RFs, RMs, FH0, FH).
combine_formulas([L|Ls], [LM|LMs], RFs, RMs, FH0, FH) :-
	combine_l(LM, L, FH0, FH1),
	combine_formulas(Ls, LMs, RFs, RMs, FH1, FH).

combine_formulas([], [], FH, FH).
combine_formulas([R|Rs], [RM|RMs], FH0, FH) :-
	combine_r(RM, R, FH0, FH1),
	combine_formulas(Rs, RMs, FH1, FH).


combine_l(0, L, FH, dl(0,L,FH)).
combine_l(1, _, FH, FH).

combine_r(0, R, FH, dr(0,FH,R)).
combine_r(1, _, FH, FH).

proof_conclusion(rule(_,W,F,_), W, F).
