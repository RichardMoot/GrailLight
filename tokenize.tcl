#!/usr/bin/tclsh

package provide tokenize 1.0

proc tokenize {sentence} {

    set prev $sentence
    set sentence [tokenize1 $sentence]
    while {![string equal $sentence $prev]} {
	set prev $sentence
	set sentence [tokenize1 $sentence]
    }
    # ellipsis points
    regsub -all { \. \. \.} $sentence { ...} sentence
    regsub -all { \. \.} $sentence { .} sentence
    # split multiple sentences
    regsub -all { \.\.\.[[:space:]]?([[:upper:]])} $sentence { ...
\1} sentence
    regsub -all {([[:alnum:]])\.\.\.([[:upper:]])} $sentence {\1 ...
\2} sentence
    regsub -all { etc\. ([[:upper:]])} $sentence { etc.
\1} sentence
    regsub -all { \" \. \" } $sentence { " .
"} sentence
    regsub -all { \. \" ([[:upper:]])} $sentence { .
" \1} sentence
    regsub -all { \. \" ([[:graph:]])} $sentence { . "
\1} sentence
    regsub -all { \. ([^ \"])} $sentence { .
\1} sentence 
    regsub -all { \.\.\. \" ([[:upper:]])} $sentence { ...
" \1} sentence
    regsub -all { \.\.\. ([^ \"])} $sentence { ...
\1} sentence 
    regsub -all { \! \" ([[:graph:]])} $sentence { ! "
\1} sentence
    regsub -all { \! ([^ \"])} $sentence { .
\1} sentence 
    regsub -all { \? \" ([[:graph:]])} $sentence { ? "
\1} sentence
    regsub -all { \? ([^ \"])} $sentence { ?
\1} sentence 
    # sentence-final interpunction
#    if {![string is punct [string rang $sentence end end]]} {
#	set sentence "$sentence ."
#    }
    regsub -all {
(C')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(D')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(J')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(L')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(M')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(N')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(S')([[:graph:]])} $sentence {
\1 \2} sentence
    regsub -all {
(T')([[:graph:]])} $sentence {
\1 \2} sentence

    return $sentence
}

proc tokenize_words {sentence} {
    set prev $sentence
    set sentence [tokenize1 $sentence]
    while {![string equal $sentence $prev]} {
	set prev $sentence
	puts stderr $sentence
	set sentence [tokenize1 $sentence]
    }
    return $sentence
}

proc tokenize1 {sentence} {

    regsub -all {[[:space:]]+} $sentence " " sentence
    #
    regsub -all {œ} $sentence {oe} sentence
    regsub -all {“} $sentence {"} sentence
    regsub -all {”} $sentence {"} sentence
    regsub -all {«} $sentence {"} sentence
    regsub -all {»} $sentence {"} sentence
    regsub -all {’} $sentence {'} sentence
    regsub -all {…} $sentence {...} sentence
    regsub -all {–} $sentence {-} sentence
    regsub -all {–} $sentence {-} sentence
    regsub -all {—} $sentence {-} sentence
    regsub -all {n°([0-9]+)} $sentence {n° \1} sentence
    #
    regsub -all { vis à vis } $sentence { vis-à-vis } sentence
    # sentence-final interpunction
    regsub -all {
+} $sentence {
} sentence 
    # ellipsis points
    regsub -all {\.\.\.[ \.]+} $sentence {...} sentence
    regsub -all { \. \. \. } $sentence { ... } sentence
    regsub -all { \.{2,} \.$} $sentence { ...} sentence
    regsub {([[:graph:]])\.\.\.$} $sentence {\1 ...} sentence
    regsub {([[:alnum:]])\.$} $sentence {\1 .} sentence
    regsub {([[:graph:]])\!$} $sentence {\1 .} sentence
    regsub {([[:graph:]])\?$} $sentence {\1 ?} sentence
    regsub {([[:graph:]])\:$} $sentence {\1 :} sentence
    regsub {([[:graph:]])\;$} $sentence {\1 ;} sentence
    regsub {([[:graph:]])\,$} $sentence {\1 ,} sentence
    regsub {([[:graph:]])\"$} $sentence {\1 "} sentence
    # french suffixes
    regsub -all {([[:graph:]])-je[[:space:]]} $sentence {\1 -je } sentence
    regsub -all {([[:graph:]])-tu[[:space:]]} $sentence {\1 -tu } sentence
    regsub -all {([[:graph:]])-il[[:space:]]} $sentence {\1 -il } sentence
    regsub -all {([[:graph:]])-on[[:space:]]} $sentence {\1 -on } sentence
    regsub -all {([[:graph:]])-elle[[:space:]]} $sentence {\1 -elle } sentence
    regsub -all {([[:graph:]])-la[[:space:]]} $sentence {\1 -la } sentence
    regsub -all {([[:graph:]])-le[[:space:]]} $sentence {\1 -le } sentence
    regsub -all {([[:graph:]])-ci[[:space:]]} $sentence {\1 -ci } sentence
    regsub -all {([[:graph:]])-ce[[:space:]]} $sentence {\1 -ce } sentence
    regsub -all {([[:graph:]])-nous[[:space:]]} $sentence {\1 -nous } sentence
    regsub -all {([[:graph:]])-vous[[:space:]]} $sentence {\1 -vous } sentence
    regsub -all {([[:graph:]])-ils[[:space:]]} $sentence {\1 -ils } sentence
    regsub -all {([[:graph:]])-elles[[:space:]]} $sentence {\1 -elles } sentence
    regsub -all {([[:graph:]])-là[[:space:]]} $sentence {\1 -là } sentence
    regsub -all {([[:graph:]])-y[[:space:]]} $sentence {\1 -y } sentence
    regsub -all {([[:graph:]])-en[[:space:]]} $sentence {\1 -en } sentence
    # -t 
    regsub -all {([[:graph:]])-t -il[[:space:]]} $sentence {\1 -t-il } sentence
    regsub -all {([[:graph:]])-t -elle[[:space:]]} $sentence {\1 -t-elle } sentence
    regsub -all {([[:graph:]])-t -on[[:space:]]} $sentence {\1 -t-on } sentence
    regsub -all {([[:graph:]])-t -ils[[:space:]]} $sentence {\1 -t-ils } sentence
    regsub -all {([[:graph:]])-t -elles[[:space:]]} $sentence {\1 -t-elles } sentence
    regsub -all {([[:graph:]])-t -y[[:space:]]} $sentence {\1 -t-y } sentence
    regsub -all {([[:graph:]])-t -en[[:space:]]} $sentence {\1 -t-en } sentence
    regsub -all {[[:space:]]-t -il[[:space:]]} $sentence { -t-il } sentence
    regsub -all {[[:space:]]-t -elle[[:space:]]} $sentence { -t-elle } sentence
    regsub -all {[[:space:]]-t -on[[:space:]]} $sentence { -t-on } sentence
    regsub -all {[[:space:]]-t -ils[[:space:]]} $sentence { -t-ils } sentence
    regsub -all {[[:space:]]-t -elles[[:space:]]} $sentence { -t-elles } sentence
    regsub -all {[[:space:]]-t -y[[:space:]]} $sentence { -t-y } sentence
    regsub -all {[[:space:]]-t -en[[:space:]]} $sentence { -t-en } sentence
    # interpunction
    regsub -all {([[:alnum:]])\. } $sentence {\1 . } sentence
    regsub -all {([[:graph:]])\, } $sentence {\1 , } sentence
    regsub -all {([[:graph:]])\: } $sentence {\1 : } sentence
    regsub -all {([[:graph:]])\; } $sentence {\1 ; } sentence
    regsub -all {([[:graph:]])\- } $sentence {\1 - } sentence
    regsub -all {([[:graph:]])\"([[:graph:]])} $sentence {\1 " \2} sentence
    regsub -all {^\"([[:graph:]])} $sentence {" \1} sentence
    regsub -all {([[:space:]])\"([[:graph:]])} $sentence {\1" \2} sentence
    regsub -all {([[:graph:]])\"([[:space:]])} $sentence {\1 "\2} sentence
    regsub -all {([[:space:]])\'([[:graph:]])} $sentence {\1' \2} sentence
    # "
    regsub -all {([[:graph:]])/([[:graph:]])} $sentence {\1 / \2} sentence
    regsub -all { /([[:graph:]])} $sentence { / \1} sentence
    regsub -all {([[:graph:]])/ } $sentence {\1 / } sentence
    regsub -all {([[:graph:]])\(([[:graph:]])} $sentence {\1 ( \2} sentence
    regsub -all {([[:space:]])\(([[:graph:]])} $sentence {\1( \2} sentence
    regsub -all {([[:graph:]])\(([[:space:]])} $sentence {\1 (\2} sentence
    regsub -all {([[:graph:]])\)([[:graph:]])} $sentence {\1 ) \2} sentence
    regsub -all {([[:space:]])\)([[:graph:]])} $sentence {\1) \2} sentence
    regsub -all {([[:graph:]])\)([[:space:]])} $sentence {\1 )\2} sentence
    regsub -all {([[:graph:]])\&([[:graph:]])} $sentence {\1 \& \2} sentence
    regsub -all {([[:space:]])\&([[:graph:]])} $sentence {\1\& \2} sentence
    regsub -all {([[:graph:]])\&([[:space:]])} $sentence {\1 |&\2} sentence
    regsub -all { '([[:graph:]]+)' } $sentence {' \1 '} sentence
    regsub -all {([[:graph:]])\! } $sentence {\1 ! } sentence
    regsub -all {([[:graph:]])\? } $sentence {\1 ? } sentence
    # percentage sign
    regsub -all {([[:graph:]])\%([[:space:]])} $sentence {\1 %\2} sentence
    # french prefixes
    regsub -all {([[:space:]]c')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]d')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]j')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]jusqu')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]qu')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]l')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]m')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]n')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]s')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {([[:space:]]t')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^C')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^D')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^J')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^Jusqu')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^L')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^M')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^N')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^Qu')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^S')([[:graph:]])} $sentence {\1 \2} sentence
    regsub -all {(^T')([[:graph:]])} $sentence {\1 \2} sentence
    # real numbers with superfluous spaces
	regsub -all {\m(\d+) \, (\d+)\M} $sentence { \1,\2 } sentence
    # numbers greater than 1.000 with space instead of "."
        regsub -all {\m(\d+)\s+(\d{3})\M} $sentence {\1.\2} sentence
        regsub -all {\.(\d{3})\s+(\d{3})\M} $sentence {.\1.\2} sentence
    # ajourd'hui
	regsub -all {aujourd' hui} $sentence {aujourd'hui} sentence
    # ellipsis
        regsub -all { \( \.\.\. \) } $sentence { (...) } sentence
    # period belonging to a word (ie. abbreviations)

	# initials
	regsub -all { ([[:upper:]]) \. } $sentence { \1. } sentence
	regsub -all {^([[:upper:]]) \. } $sentence {\1. } sentence
	regsub -all { ([[:upper:]]) \. -([[:upper:]]) \. } $sentence { \1.-\2. } sentence
	regsub -all {^([[:upper:]]) \. -([[:upper:]]) \. } $sentence {\1.-\2. } sentence
    return $sentence
}

#     proc is_abbreviation {string} {
	
# 	global abbrev

# 	return [info exists $abbrev($string)]
#     }

# abbrev("B. D.")
# abbrev("B.D.")
# abbrev("B. D")
# abbrev("B.D")
# abbrev("C. V.")
# abbrev("C.V.")
# abbrev("C. V")
# abbrev("C.V")
#     regsub -all {^B \. D \. } $sentence {BD } sentence
#     regsub -all { B \. D } $sentence { BD } sentence
#     regsub -all {^B \. D } $sentence {BD } sentence
# 	regsub -all { C \. V \. } $sentence { CV } sentence
# 	regsub -all {^C \. V \. } $sentence {CV } sentence
# 	regsub -all { C \. V } $sentence { CV } sentence
# 	regsub -all {^C \. V } $sentence {CV } sentence
#     regsub -all {^C\.N\.R\.S \. } $sentence {CNRS } sentence
#     regsub -all { C\.N\.R\.S \. } $sentence { CNRS } sentence
#     regsub -all {^C\.N\.R\.S } $sentence {CNRS } sentence
#     regsub -all { C\.N\.R\.S } $sentence { CNRS } sentence
# 	regsub -all {^C \. N \. R \. S \. } $sentence {CNRS } sentence
# 	regsub -all { C \. N \. R \. S \. } $sentence { CNRS } sentence
# 	regsub -all {^C \. N \.R \.S } $sentence {CNRS } sentence
# 	regsub -all { C \. N \. R \. S } $sentence { CNRS } sentence
#     regsub -all { Ch \. } $sentence { Ch. } sentence
#     regsub -all {^Ch \. } $sentence {^Ch. } sentence
# 	regsub -all {^J \. O } $sentence {JO } sentence
# 	regsub -all { J \. O \. } $sentence { JO } sentence
# 	regsub -all {^J \. O \. } $sentence {JO } sentence
# 	regsub -all { J \. O } $sentence { JO } sentence
#     regsub -all { MM \. } $sentence { MM. } sentence
#     regsub -all {^MM \. } $sentence {MM. } sentence
#     regsub -all { Mr \. } $sentence { Mr. } sentence
#     regsub -all {^Mr \. } $sentence {Mr. } sentence
#     regsub -all { M \. } $sentence { M. } sentence
#     regsub -all {^M \. } $sentence {M. } sentence
#     regsub -all { Me \. } $sentence { Me. } sentence
#     regsub -all {^Me \. } $sentence {Me. } sentence
# 	regsub -all {^N \. B } $sentence {NB } sentence
# 	regsub -all { N \. B \. } $sentence { NB } sentence
# 	regsub -all {^N \. B \. } $sentence {NB } sentence
# 	regsub -all { N \. B } $sentence { NB } sentence
#     regsub -all {^O \. K } $sentence {OK } sentence
#     regsub -all { O \. K \. } $sentence { OK } sentence
#     regsub -all {^O \. K \. } $sentence {OK } sentence
#     regsub -all { O \. K } $sentence { OK } sentence
# 	regsub -all {^O \. N \. U \. } $sentence {ONU } sentence
# 	regsub -all { O \. N \. U \. } $sentence { ONU } sentence
# 	regsub -all {^O \. N \. U } $sentence {ONU } sentence
# 	regsub -all { O \. N \. U } $sentence { ONU } sentence
#     regsub -all { P \. S \. } $sentence { PS } sentence
#     regsub -all {^P \. S \. } $sentence {PS } sentence
#     regsub -all {^P \. S } $sentence {PS } sentence
#     regsub -all { P \. S } $sentence { PS } sentence
# 	regsub -all { Q \. G \. } $sentence { QG } sentence
# 	regsub -all {^Q \. G \. } $sentence {QG } sentence
# 	regsub -all {^Q \. G } $sentence {QG } sentence
# 	regsub -all { Q \. G } $sentence { QG } sentence
#     regsub -all { R \. P \. } $sentence { RP } sentence
#     regsub -all {^R \. P \. } $sentence {RP } sentence
#     regsub -all { R \. P } $sentence { RP } sentence
#     regsub -all {^R \. P } $sentence {RP } sentence
# 	regsub -all { S \. A \. } $sentence { SA } sentence
# 	regsub -all {^S \. A \. } $sentence {SA } sentence
# 	regsub -all { S \. A } $sentence { SA } sentence
# 	regsub -all {^S \. A } $sentence {SA } sentence
# 	regsub -all { S\.A \. } $sentence { SA } sentence
# 	regsub -all {^S\.A \. } $sentence {SA } sentence
# 	regsub -all { S\.A } $sentence { SA } sentence
# 	regsub -all {^S\.A } $sentence {SA } sentence
#     regsub -all {^S \. A \. R \. L \. } $sentence {SARL } sentence
#     regsub -all { S \. A \. R \. L \. } $sentence { SARL } sentence
#     regsub -all {^S \. A \. R \. L } $sentence {SARL } sentence
#     regsub -all { S \. A \. R \. L } $sentence { SARL } sentence
#     regsub -all {^S\.A\.R\.L \. } $sentence {SARL } sentence
#     regsub -all { S\.A\.R\.L \. } $sentence { SARL } sentence
#     regsub -all {^S\.A\.R\.L } $sentence {SARL } sentence
#     regsub -all { S\.A\.R\.L } $sentence { SARL } sentence
# 	regsub -all { S \. F \. } $sentence { SF } sentence
#         regsub -all {^S \. F \. } $sentence {SF } sentence
# 	regsub -all { S \. F } $sentence { SF } sentence
# 	regsub -all {^S \. F } $sentence {SF } sentence
# 	regsub -all { S\.F \. } $sentence { SF } sentence
# 	regsub -all {^S\.F \. } $sentence {SF } sentence
# 	regsub -all { S\.F } $sentence { SF } sentence
# 	regsub -all {^S\.F } $sentence {SF } sentence
# 	regsub -all {^S \. O \. S \. } $sentence {SOS } sentence
# 	regsub -all { S \. O \. S \. } $sentence { SOS } sentence
# 	regsub -all {^S \. O \. S } $sentence {SOS } sentence
# 	regsub -all { S \. O \. S } $sentence { SOS } sentence
# 	regsub -all { St \. } $sentence { St. } sentence
# 	regsub -all {^St \. } $sentence {St. } sentence
# 	regsub -all { Tel \. } $sentence { Tel. } sentence
# 	regsub -all {^Tel \. } $sentence {Tel. } sentence
# 	regsub -all { Tél \. } $sentence { Tel. } sentence
# 	regsub -all {^Tél \. } $sentence {Tel. } sentence
# 	regsub -all { Th \. } $sentence { Th. } sentence
# 	regsub -all {^Th \. } $sentence {Th. } sentence
# 	regsub -all { U \. E \. } $sentence { UE } sentence
# 	regsub -all {^U \. E \. } $sentence {UE } sentence
# 	regsub -all { U \. E } $sentence { UE } sentence
# 	regsub -all {^U \. E } $sentence {UE } sentence
# 	regsub -all {^U \. R \. S \. S \. } $sentence {URSS } sentence
# 	regsub -all { U \. R \. S \. S \. } $sentence { URSS } sentence
# 	regsub -all {^U \. R \. S \. S } $sentence {URSS } sentence
# 	regsub -all { U \. R \. S \. S } $sentence { URSS } sentence
# 	regsub -all {^U \. S \. A \. } $sentence {USA } sentence
# 	regsub -all { U \. S \. A \. } $sentence { USA } sentence
# 	regsub -all {^U \. S \. A } $sentence {USA } sentence
# 	regsub -all { U \. S \. A } $sentence { USA } sentence
# 	regsub -all { adj \. } $sentence { adj. } sentence
# 	regsub -all {^Adj \. } $sentence {Adj. } sentence
# 	regsub -all { adv \. } $sentence { adv. } sentence
# 	regsub -all {^Adv \. } $sentence {Adv. } sentence
# 	regsub -all { apr \. } $sentence { après } sentence
# 	regsub -all {^Apr \. } $sentence {Après } sentence
# 	regsub -all { art \. } $sentence { article } sentence
# 	regsub -all {^Art \. } $sentence {Article } sentence
# 	regsub -all { av \. } $sentence { avant } sentence
# 	regsub -all {^Av \. } $sentence {Avant } sentence
# 	regsub -all { bibliogr \. } $sentence { bibliographie } sentence
# 	regsub -all {^Bibliogr \. } $sentence {Bibliographie } sentence
# 	regsub -all { boul \. } $sentence { boulevard } sentence
# 	regsub -all {^Boul \. } $sentence {Boulevard } sentence
# 	regsub -all { bull \. } $sentence { bulletin } sentence
# 	regsub -all {^Bull \. } $sentence {Bulletin } sentence
# 	regsub -all { cap \. } $sentence { capitale } sentence
# 	regsub -all {^Cap \. } $sentence {Capitale } sentence
# 	regsub -all { cf \. } $sentence { cf. } sentence
# 	regsub -all {^Cf \. } $sentence {Cf. } sentence
# 	regsub -all { ch \. } $sentence { ch. } sentence
# 	regsub -all {^Ch \. } $sentence {Ch. } sentence
# 	regsub -all { chap \. } $sentence { chap. } sentence
# 	regsub -all {^Chap \. } $sentence {Chap. } sentence
# 	regsub -all { coll \. } $sentence { coll. } sentence
# 	regsub -all {^Coll \. } $sentence {Coll. } sentence
# 	regsub -all { collec \. } $sentence { collec. } sentence
# 	regsub -all {^Collec \. } $sentence {Collec. } sentence
# 	regsub -all { conf \. } $sentence { conf. } sentence
# 	regsub -all {^Conf \. } $sentence {Conf. } sentence
# 	regsub -all { corp \. } $sentence { corp. } sentence
# 	regsub -all {^Corp \. } $sentence {Corp. } sentence
# 	regsub -all { dept \. } $sentence { dept. } sentence
# 	regsub -all {^Dept \. } $sentence {Dept. } sentence
# 	regsub -all { dép \. } $sentence { dép. } sentence
# 	regsub -all {^Dép \. } $sentence {Dép. } sentence
# 	regsub -all { dir \. } $sentence { dir. } sentence
# 	regsub -all {^Dir \. } $sentence {Dir. } sentence
# 	regsub -all { e \. a \. } $sentence { e.a. } sentence
# 	regsub -all {^e \. a \. } $sentence {e.a. } sentence
# 	regsub -all {^e \. a } $sentence {e.a. } sentence
# 	regsub -all { e \. a } $sentence { e.a. } sentence
# 	regsub -all { e \. g \. } $sentence { e.g. } sentence
# 	regsub -all {^e \. g \. } $sentence {e.g. } sentence
# 	regsub -all {^e \. g } $sentence {e.g. } sentence
# 	regsub -all { e \. g } $sentence { e.g. } sentence
# 	regsub -all { et al \. } $sentence { et al. } sentence
# 	regsub -all { etc \. } $sentence { etc. } sentence
# 	regsub -all { ex \. } $sentence { exemple } sentence
# 	regsub -all {^Ex \. } $sentence {Exemple } sentence
# 	regsub -all { fasc \. } $sentence { fasc. } sentence
# 	regsub -all {^Fasc \. } $sentence {Fasc. } sentence
# 	regsub -all { fig \. } $sentence { fig. } sentence
# 	regsub -all {^Fig \. } $sentence {Fig. } sentence
# 	regsub -all { hab \. } $sentence { habitant } sentence
# 	regsub -all {^Hab \. } $sentence {Habitant } sentence
# 	regsub -all { i \. e \. } $sentence { i.e. } sentence
# 	regsub -all {^i \. e \. } $sentence {i.e. } sentence
# 	regsub -all {^i \. e } $sentence {i.e. } sentence
# 	regsub -all { i \. e } $sentence { i.e. } sentence
# 	regsub -all { i\.e \. } $sentence { i.e. } sentence
# 	regsub -all {^i\.e \. } $sentence {i.e. } sentence
# 	regsub -all {^i\.e } $sentence {i.e. } sentence
# 	regsub -all { i\.e } $sentence { i.e. } sentence
# 	regsub -all { ibid \. } $sentence { ibid. } sentence
# 	regsub -all {^Ibid \. } $sentence {Ibid. } sentence
# 	regsub -all { id \. } $sentence { id. } sentence
# 	regsub -all {^Id \. } $sentence {Id. } sentence
# 	regsub -all { ill \. } $sentence { ill. } sentence
# 	regsub -all {^Ill \. } $sentence {Ill. } sentence
# 	regsub -all { inf \. } $sentence { inf. } sentence
# 	regsub -all {^Inf \. } $sentence {Inf. } sentence
# 	regsub -all { intr \. } $sentence { introduction } sentence
# 	regsub -all {^Intr \. } $sentence {Introduction } sentence
# 	regsub -all { introd \. } $sentence { introduction } sentence
# 	regsub -all {^Introd \. } $sentence {Introduction } sentence
# 	regsub -all { ital \. } $sentence { italique } sentence
# 	regsub -all {^Ital \. } $sentence {Italique } sentence
# 	regsub -all { loc \. cit \. } $sentence { loc. cit. } sentence
# 	regsub -all {^Loc \. cit \. } $sentence {Loc. cit. } sentence
# 	regsub -all { ms \. } $sentence { ms. } sentence
# 	regsub -all {^Ms \. } $sentence {Ms. } sentence
# 	regsub -all { obs \. } $sentence { obs. } sentence
# 	regsub -all {^Obs \. } $sentence {Obs. } sentence
# 	regsub -all { op \. cit \. } $sentence { op. cit. } sentence
# 	regsub -all {^Op \. cit \. } $sentence {Op. cit. } sentence
# 	regsub -all { p \. } $sentence { p. } sentence
# 	regsub -all {^P \. } $sentence {P. } sentence
# 	regsub -all { paragr \. } $sentence { paragr. } sentence
# 	regsub -all {^Paragr \. } $sentence {Paragr. } sentence
# 	regsub -all { pl \. } $sentence { pl. } sentence
# 	regsub -all {^Pl \. } $sentence {Pl. } sentence
# 	regsub -all { pp \. } $sentence { pp. } sentence
# 	regsub -all {^Pp \. } $sentence {Pp. } sentence
# 	regsub -all { préc \. } $sentence { préc. } sentence
# 	regsub -all {^Préc \. } $sentence {Préc. } sentence
# 	regsub -all {^q \. e \. d \. } $sentence {q.e.d. } sentence
# 	regsub -all { q \. e \. d \. } $sentence { q.e.d. } sentence
# 	regsub -all {^q \. e \. d } $sentence {q.e.d. } sentence
# 	regsub -all { q \. e \. d } $sentence { q.e.d. } sentence
# 	regsub -all {^q\.e\.d \. } $sentence {q.e.d. } sentence
# 	regsub -all { q\.e\.d \. } $sentence { q.e.d. } sentence
# 	regsub -all {^q\.e\.d } $sentence {q.e.d. } sentence
# 	regsub -all { q\.e\.d } $sentence { q.e.d. } sentence
# 	regsub -all { réf \. } $sentence { réf. } sentence
# 	regsub -all {^Réf \. } $sentence {Réf. } sentence
# 	regsub -all { rééd \. } $sentence { rééd. } sentence
# 	regsub -all {^Rééd \. } $sentence {Rééd. } sentence
# 	regsub -all { s \. f \. } $sentence { SF } sentence
# 	regsub -all {^s \. f \. } $sentence {SF } sentence
# 	regsub -all { s \. f } $sentence { SF } sentence
# 	regsub -all {^s \. f } $sentence {SF } sentence
# 	regsub -all { sp \. } $sentence { sp. } sentence
# 	regsub -all {^Sp \. } $sentence {Sp. } sentence
# 	regsub -all { spp \. } $sentence { spp. } sentence
# 	regsub -all {^Spp \. } $sentence {Spp. } sentence
# 	regsub -all { sq \. } $sentence { sq. } sentence
# 	regsub -all {^Sq \. } $sentence {Sq. } sentence
# 	regsub -all { sqq \. } $sentence { sqq. } sentence
# 	regsub -all {^Sqq \. } $sentence {Sqq. } sentence
# 	regsub -all { suiv \. } $sentence { suiv. } sentence
# 	regsub -all {^Suiv \. } $sentence {Suiv. } sentence
# 	regsub -all { sup \. } $sentence { sup. } sentence
# 	regsub -all {^Sup \. } $sentence {Sup. } sentence
# 	regsub -all { t \. } $sentence { t. } sentence
# 	regsub -all { tel \. } $sentence { tel. } sentence
# 	regsub -all {^Tel \. } $sentence {Tel. } sentence
# 	regsub -all { tél \. } $sentence { tél. } sentence
# 	regsub -all {^Tél \. } $sentence {Tél. } sentence
# 	regsub -all { trad \. } $sentence { trad. } sentence
# 	regsub -all {^Trad \. } $sentence {Trad. } sentence
# 	regsub -all { v \. } $sentence { v. } sentence
# 	regsub -all { vs \. } $sentence { vs } sentence
# 	regsub -all { zool \. } $sentence { zool. } sentence

    foreach f $argv {

	set fs [open $f r]
	while {[gets $fs line] >= 0} {
	    set out [tokenize $line]
	    if {$out != ""} {
		puts $out
	    }
	}
	close $fs
    }

