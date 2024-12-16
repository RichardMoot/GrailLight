# Command line usage of GrailLight with an external part-of-speech tagger and supertagger

## Tokenization

Given an input file `raw.txt` which can contain several paragraphs, the tokenization script splits sentences and words.

```
tokenize.tcl raw.txt > input.txt
```

You can use other tokenization scripts, but make sure the tokenization is that same as the one used for your supertagger.

## Tagging

Use use your external taggers to convert your input file into a supertag file, ensure it returns its results in the format of the Clark and Curran taggers.
The expected format for each word is `word|POS|N|Formula1|Prob1 ... Formulan|ProbN` where `word` is the word, `POS` the assigned part-of-speech tag (using the Treetagger tagset), `N` is the number of formulas give
by the supertagger, followed by `N` occurrences of a formula and its probability.

## Prolog conversion

The script `supertag2pl` converts the supertagger output into (unlemmatized) Prolog clauses for the parser. 
Clauses contain a list of items of the form `ex_si(Word, Pos, Word, List)` where `List` is a list of `Formula-Probability` pairs.

```
supertag2pl superpos.txt > superpos_nolem.pl
```

## Lemmatization

It it preferable to use an external lemmatizer. Otherwise, the script `lefff.pl` provides a basic functionality, looking up the word-POStag combination in the Lefff database. This is very slow.
The lemmatizer replaces the list entries of the form `ex_si(Word, Pos, Word, List)` by entries `si(Word, Pos, Lemma, List)`.
The `lefff.pl` script takes a file `NAME_nolem.pl` and returns a lemmatized file `NAME.pl`.

```
lefff.pl superpos_nolem.pl
```

## Parsing

```
grail_light_nd superpos.pl
```

This command tries to parse all sentence and outputs the results in a number of files:
- `semantics.pl` contains the Prolog semantic ouput.
