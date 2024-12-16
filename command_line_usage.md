# Command line usage of GrailLight with an external part-of-speech tagger and supertagger

## Tokenization

Given an input file `raw.txt` which can contain several paragraphs, the tokenization script splits sentences and words.

```
tokenize.tcl raw.txt > input.txt
```

You can use other tokenization scripts, but make sure the tokenization is that same as the one used for your supertagger.

As an example, we take the following input text.

```
Aussi ont-ils décidé d'eux-mêmes, mercredi, de se soumettre à l'arbitrage définitif des autorités monétaires.
```

## Tagging

Use use your external taggers to convert your input file into a supertag file, ensure it returns its results in the format of the Clark and Curran taggers.
The expected format for each word is `word|POS|N|Formula1|Prob1 ... Formulan|ProbN` where `word` is the word, `POS` the assigned part-of-speech tag (using the Treetagger tagset), `N` is the number of formulas give
by the supertagger, followed by `N` occurrences of a formula and its probability.

This gives the following output for our example

```
Aussi|ADV-ADV|1|dr(0,s,s)|0.9999778 ont|V-VER:pres|2|dr(0,dr(0,s,dl(0,np,s_ppart)),np)|0.9980058|dr(0,dr(0,s,np),dl(0,np,s_ppart))|0.0018172036 -ils|CLS-PRO:PER|1|np|0.99999213 décidé|VPP-VER:pper|9|dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))|0.95984834|dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)|0.012183049|dr(0,dl(0,np,s_ppart),pp_de)|0.0071665817|dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),pp_a)|0.005813557|dr(0,dr(0,dl(0,np,s_ppart),pp_de),np)|0.0021970696|dr(0,dl(0,cl_r,dl(0,np,s_inf)),dl(0,np,s_inf))|0.0011594007|dr(0,dl(0,np,s),dl(0,np,s_inf))|0.0011494745|dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,np,s_inf))|0.0010469758|dr(0,dl(0,cl_r,dl(0,np,s_ppart)),dl(0,np,s_inf))|0.0010047258 d'|P-PRP|2|dr(0,pp_de,np)|0.9892832|dr(0,pp_de,n)|0.010377718 eux-mêmes|PRO-PRO:PER|2|np|0.99766695|n|0.002082536 ,|PONCT-PUN|1|let|0.99999964 mercredi|NC-NOM|1|dl(1,s,s)|0.99971145 ,|PONCT-PUN|1|let|0.99999976 de|P-PRP|1|dr(0,dl(0,np,s_inf),dl(0,np,s_inf))|0.9999877 se|CLR-PRO:PER|1|cl_r|1.0 soumettre|VINF-VER:infi|1|dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a)|0.99848455 à|P-PRP|1|dr(0,pp_a,np)|0.99999905 l'|DET-DET:ART|1|dr(0,np,n)|1.0 arbitrage|NC-NOM|1|n|1.0 définitif|ADJ-ADJ|1|dl(0,n,n)|1.0 des|P+D-PRP:det|1|dr(0,dl(0,n,n),n)|0.9999982 autorités|NC-NOM|1|n|1.0 monétaires|ADJ-ADJ|1|dl(0,n,n)|1.0 .|PONCT-PUN|1|dl(0,s,txt)|0.9996948
```

This output uses the `MElt-TT` format for part-of-speech tags. Only the treetagger tags are used later.

## Prolog conversion

The script `supertag2pl` converts the supertagger output into (unlemmatized) Prolog clauses for the parser. 
Clauses contain a list of items of the form `ex_si(Word, Pos, Word, List)` where `List` is a list of `Formula-Probability` pairs.

```
supertag2pl superpos.txt > superpos_nolem.pl
```

For our example, this script gives the following ouput.

```
sent(20, Result) :-
      prob_parse([ ex_si('Aussi', adv-adv, 'Aussi', [dr(0,s,s)-0.9999778]),
                   ex_si(ont, v-ver:pres, ont, [dr(0,dr(0,s,dl(0,np,s_ppart)),np)-0.9980058,
	                                        dr(0,dr(0,s,np),dl(0,np,s_ppart))-0.0018172036]),
                   ex_si('-ils', cls-pro:per, '-ils', [np-0.99999213]),
                   ex_si(décidé, vpp-ver:pper, décidé, [dr(0,dl(0,np,s_ppart),dl(0,np,s_inf))-0.95984834,
                                                        dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),np)-0.012183049,
                                                        dr(0,dl(0,np,s_ppart),pp_de)-0.0071665817,
                                                        dr(0,dr(0,dl(0,np,s_ppart),dl(0,np,s_inf)),pp_a)-0.005813557,
                                                        dr(0,dr(0,dl(0,np,s_ppart),pp_de),np)-0.0021970696,
                                                        dr(0,dl(0,cl_r,dl(0,np,s_inf)),dl(0,np,s_inf))-0.0011594007,
                                                        dr(0,dl(0,np,s),dl(0,np,s_inf))-0.0011494745,
                                                        dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,np,s_inf))-0.0010469758,
                                                        dr(0,dl(0,cl_r,dl(0,np,s_ppart)),dl(0,np,s_inf))-0.0010047258]),
                   ex_si('d\'', p-prp, 'd\'', [dr(0,pp_de,np)-0.9892832,
                                               dr(0,pp_de,n)-0.010377718]),
                   ex_si('eux-mêmes', pro-pro:per, 'eux-mêmes', [np-0.99766695, n-0.002082536]),
                   ex_si(',', ponct-pun, ',', [let-0.99999964]),
                   ex_si(mercredi, nc-nom, mercredi, [dl(1,s,s)-0.99971145]),
                   ex_si(',', ponct-pun, ',', [let-0.99999976]),
                   ex_si(de, p-prp, de, [dr(0,dl(0,np,s_inf),dl(0,np,s_inf))-0.9999877]),
                   ex_si(se, clr-pro:per, se, [cl_r-1.0]),
                   ex_si(soumettre, vinf-ver:infi, soumettre, [dr(0,dl(0,cl_r,dl(0,np,s_inf)),pp_a)-0.99848455]),
                   ex_si(à , p-prp, à, [dr(0,pp_a,np)-0.99999905]),
                   ex_si('l\'', det-det:art, 'l\'', [dr(0,np,n)-1.0]),
                   ex_si(arbitrage, nc-nom, arbitrage, [n-1.0]),
                   ex_si(définitif, adj-adj, définitif, [dl(0,n,n)-1.0]),
                   ex_si(des, p+d-prp:det, des, [dr(0,dl(0,n,n),n)-0.9999982]),
                   ex_si(autorités, nc-nom, autorité, [n-1.0]),
                   ex_si(monétaires, adj-adj, monétaires, [dl(0,n,n)-1.0]),
                   ex_si('.', ponct-pun, '.', [dl(0,s,txt)-0.9996948])], Result).
```

## Lemmatization

It it preferable to use an external lemmatizer. Otherwise, the script `lefff.pl` provides a basic functionality, looking up the word-POStag combination in the Lefff database. This is very slow.
The lemmatizer replaces the list entries of the form `ex_si(Word, Pos, Word, List)` by entries `si(Word, Pos, Lemma, List)`.
The `lefff.pl` script takes a file `NAME_nolem.pl` and returns a lemmatized file `NAME.pl`.

```
lefff.pl superpos_nolem.pl
```

The output of the lemmatizer (in the file `superpos.pl`) is shown below.

```
sent(20, A) :-
	prob_parse(
		   [ si('Aussi', adv-adv, aussi, [dr(0, s, s)-0.9999778]),
		     si(ont,
			v-ver:pres,
			avoir,			
			[ dr(0, dr(0, s, dl(0, np, s_ppart)), np)-0.9980058,
			  dr(0, dr(0, s, np), dl(0, np, s_ppart))-0.0018172036
			]),
		     si('-ils', cls-pro:per, ils, [np-0.99999213]),
		     si(décidé,
			vpp-ver:pper,
			décider,			
			[ dr(0, dl(0, np, s_ppart), dl(0, np, s_inf))-0.95984834,
			  dr(0, dr(0, dl(0, np, s_ppart), dl(0, np, s_inf)), np)-0.012183049,
			  dr(0, dl(0, np, s_ppart), pp_de)-0.0071665817,
			  dr(0, dr(0, dl(0, np, s_ppart), dl(0, np, s_inf)), pp_a)-0.005813557,
			  dr(0, dr(0, dl(0, np, s_ppart), pp_de), np)-0.0021970696,
			  dr(0, dl(0, cl_r, dl(0, np, s_inf)), dl(0, np, s_inf))-0.0011594007,
			  dr(0, dl(0, np, s), dl(0, np, s_inf))-0.0011494745,
			  dr(0, dl(0, cl_r, dl(0, np, s)), dl(0, np, s_inf))-0.0010469758,
			  dr(0, dl(0, cl_r, dl(0, np, s_ppart)), dl(0, np, s_inf))-0.0010047258
			]),
		     si('d\'',
			p-prp,
			de,
			[dr(0, pp_de, np)-0.9892832, dr(0, pp_de, n)-0.010377718]),
		     si('eux-mêmes',
			pro-pro:per,
			'eux-mêmes',
			[np-0.99766695, n-0.002082536]),
		     si(',', ponct-pun, ',', [let-0.99999964]),
		     si(mercredi, nc-nom, mercredi, [dl(1, s, s)-0.99971145]),
		     si(',', ponct-pun, ',', [let-0.99999976]),
		     si(de,
			p-prp,
			de,
			[dr(0, dl(0, np, s_inf), dl(0, np, s_inf))-0.9999877]),
		     si(se, clr-pro:per, se, [cl_r-1.0]),
		     si(soumettre,
			vinf-ver:infi,
			soumettre,
			[dr(0, dl(0, cl_r, dl(0, np, s_inf)), pp_a)-0.99848455]),
		     si(à, p-prp, à, [dr(0, pp_a, np)-0.99999905]),
		     si('l\'', det-det:art, 'l\'', [dr(0, np, n)-1.0]),
		     si(arbitrage, nc-nom, arbitrage, [n-1.0]),
		     si(définitif, adj-adj, définitif, [dl(0, n, n)-1.0]),
		     si(des, p+d-prp:det, des, [dr(0, dl(0, n, n), n)-0.9999982]),
		     si(autorités, nc-nom, autorité, [n-1.0]),
		     si(monétaires, adj-adj, monétaire, [dl(0, n, n)-1.0]),
		     si('.', ponct-pun, '.', [dl(0, s, txt)-0.9996948])
		   ],
		   A).
```

## Parsing

Finally, the parser is run as follows.

```
grail_light_nd superpos.pl
```

This command tries to parse all sentence and outputs the results in a number of files:
- `semantics.pl` contains the Prolog ouput giving the DRT semantics, both in unreduced and reduced versions.
- `proofs.pl` contains the Prolog proofs (chart proofs, but they can be converted to natural deduction)

The proofs in `proofs.pl` are of the following form

```
proof(N, rule(RuleName, Structure, Formula-LambdaTerm, ListOfPremisses)).
```

where `N` is the sentence number, `RuleName` the name of the last rule of the proof, `Structure` the computed structure (a tree over the words in the sentence), `Formula` is the goal formula, and `LambdaTerm` the lambda term corresponding to the proof, `ListOfPremisses` is the list of proofs (all of the form `rule(...)` allowing us to derive the conclusion).
