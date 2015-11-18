# GrailLight

:copyright: 2015 [CNRS](http://www.cnrs.fr)

:copyright: 2015 Richard Moot (@RichardMoot)

Grail Light is a light-weight chart parser for multimodal type-logical
grammars, written in SWI Prolog.

It is accompanied by a set of auxiliary tools for interaction with the
supertagger, interactive parsing, corpus extraction, etc.

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
