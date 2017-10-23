# GrailLight

:copyright: 2015-2017 [CNRS](http://www.cnrs.fr)

:copyright: 2015-2017 Richard Moot (@RichardMoot)

Grail Light is a light-weight chart parser for multimodal type-logical
grammars, written in SWI Prolog. It is accompanied by a set of auxiliary tools for interaction with the
supertagger, interactive parsing, corpus extraction, etc. Together with the [part-of-speech tag and supertag models](https://github.com/RichardMoot/models), Grail Light
can function as a wide-coverage parser for French, assigning Discourse Representation Structures as the meaning of parsed sentences. Grail Light produces natural
deduction proofs like those found in the [TLGbank](https://github.com/RichardMoot/TLGbank) and uses the same conventions for formulas, rules and proofs.

# Quick Start

Clone the repository, enter the Grail Light directory, start SWI
Prolog then type.

`[grail_light_cr,annodis].`

This will load Grail Light as well as the sentences from the Annodis
corpus.

To parse a sentence, type

`sentence(2, Semantics).`

This will parse the sentence and unify _Semantics_ with the meaning
computed for this sentence.

# LaTeX Output

A successful parse will produce a LaTeX representation both of the
proof found and of the meaning. To see the LaTeX output, type the
following (in a shell terminal).

`pdflatex latex_proofs.tex`

# Going Further

The file [`readme.pdf`](https://github.com/RichardMoot/GrailLight/blob/master/readme.pdf) contains more detailed instructions whereas the
file [`chart_parser.pdf`](https://github.com/RichardMoot/GrailLight/blob/master/chart_parser.pdf) describes the strategy used by the chart parser.

# Acknowledgments

This chart parser has been developed starting from source code
originally developed by Shieber e.a. (1995), though with a set of
inference rules and a number of improvements which I specifically tailored for multimodal type-logical grammars.

The Prolog file `lefff.pl` has been automatically extracted from the
[Lefff lexicon](http://alpage.inria.fr/~sagot/lefff.html) (Sagot, 2010).

The file `annodis.pl` has been extracted from the
[Sequoia corpus](http://deep-sequoia.inria.fr) (Candito and Seddah,
2012) and is part of the [TLGbank](http:richardmoot.github.io/TLGbank)
(Moot, 2015).

# References

Marie Candito and Djam&eacute; Seddah (2012) _Le corpus Sequoia : annotation
syntaxique et exploitation pour l’adaptation d’analyseur par pont
lexical_, Proceedings TALN'2012, Grenoble, France.

Richard Moot (2015) _A Type-Logical Treebank for French_, Journal of
Language Modelling **3(1)**, pp. 229-265.

Richard Moot (2017) _The Grail Theorem Prover: Type Theory for Syntax and Semantics_. In Stergios Chatzikyriakidis and Zhaohui Luo (eds.) Modern Perspectives in
Type-Theoretical Semantics, pp. 247-277.

Beno&icirc;t Sagot (2010) _The_ Lefff, _a freely available and large-coverage morphological and syntactic lexicon for French_. In Proceedings of the 7th international conference on Language Resources and Evaluation (LREC 2010), Istanbul, Turkey

Stuart M. Shieber, Yves Schabes and Fernando C. N. Pereira (1995)
 _Principles and Implementation of Deductive Parsing_, Journal of
 Logic Programming **24(1-2)**, pp. 3-36.

