#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

set auto_path [linsert $auto_path 0 /Users/moot/checkout/monde/]

package require Tk
package require Mk4tcl
package require tokenize

set sent_values {}

set skip 90
set xwidth [expr $skip/9]

set diamond "\u25c7"
# set diamond "<>"
set diamond0 "\u25c7\u2080"
set diamond1 "\u25c7\u2081"
set box "\u25a1"
# set box "[]"
set box0 "\u25a1\u2080"
set box1 "\u25a1\u2081"
set bs1 "\\\u2081"
set bullet "\u2022"
# set bullet "*"
set mode0 ""
# set mode0 "\u2080"
set spc "\u2006"
# set spc " "

set debug off
set debugstring ""
# set debugstring "fyx allbrackets"
# set debugstring "spy prob_parse"
# set debugstring "noregin nozeroheap"

set cvs_prefix        /Users/moot/checkout
set lefff_prefix      /Users/moot/checkout
set monde_prefix      /Users/moot/checkout/monde
set grail_prefix      /Users/moot/checkout/GrailLight
set model_prefix      /Volumes/richard.moot
set config_prefix     /Volumes/richard.moot
set grammar_prefix    "$cvs_prefix/Grail/grammars"
set tagger_prefix     /Users/moot/Corpus/WSJ/candc-1.00/bin
set pos_model(dutch)  "$model_prefix/best_dutch_pos_reduced"
set st_model(dutch)   "$model_prefix/best_dutch_model"
set pos_model(dutchx)  "$model_prefix/best_dutch_pos_reduced"
set st_model(dutchx)   "$model_prefix/detailed_dutch_model"
# Best model on Cloud
# set st_model(french)  "$model_prefix/best_french_tt"
# set pos_model(french) "$model_prefix/french_pos_tt"
# Last local model
set pos_model(french) "$monde_prefix/all_pos_tt"
set pos_model(frenchx) "$monde_prefix/extra_pos_tt"
set st_model(french)  "$monde_prefix/french_tt"
set st_model(frenchx)  "$monde_prefix/french_tt"
#set grail_prefix      /Users/moot/grailexec/bin
set pos_cmd           "$tagger_prefix/mpos"
set st_cmd            "$tagger_prefix/msuper"
set grail_cmd         "$grail_prefix/g3"
set tmp_dir           "/Users/moot/Library/Supertagger"
set semantics         drt
set postagset         tt
set grail_exec        "grail_light_nd.pl"
set bootstrap_parser_cmd "/Users/moot/Programs/stanford-parser-full-2015-04-20/lexparser-french.sh"
set bootstrap_parser_length 30

set bw 1

# code snippet from robeadam / thommey
# found at 
# http://tclhelp.net/unb/169

proc round {number {digits 0}} {

   if { $digits} {
      return [expr {round(pow(10,$digits)*$number)/pow(10,$digits)}]
   } else {
      return [ int [expr {round(pow(10,$digits)*$number)/pow(10,$digits)}]]
   }
}

proc set_default_colors {bw} {

    global st_conf_color st_other_color pos_conf_color pos_other_color

    if {$bw} {

	    set st_conf_color     gray20
	    set st_other_color    gray80
	    
	    set pos_conf_color    gray40
	    set pos_other_color   gray80

	} else {
	    set st_conf_color     blue
	    set st_other_color    "#a6e4ff"

	    set pos_other_color  "#c60207"
	    set pos_conf_color   "#8d23f7"
	}
}

set_default_colors $bw

proc st_dialog {} {

    tk_dialog .about "About the supertagger/parser" "Interface script to the Clark & Curran taggers and the Grail parser" "" 0 "Ok"

}

proc latex_proofs {} {

    global tmp_dir

    set cur_dir [pwd]
    cd $tmp_dir
    exec /Library/TeX/texbin/pdflatex latex_proofs.tex
    catch { exec open latex_proofs.pdf }
    cd $cur_dir

}


proc update_models {} {

    global postagset model_prefix lang

    set pos_model($lang) "$model_prefix/$lang\_pos_$postagset"
    set st_model($lang)  "$model_prefix/best_$lang\_$postagset"

}

proc update_menus {} {

    global lang postagset grail_parse

    if {[string equal $lang "french"]} {
	.mb.options.pos entryconfigure 0 -state normal
	.mb.options.pos entryconfigure 1 -state normal
	.mb.options.pos entryconfigure 2 -state normal
	.mb.options.pos entryconfigure 3 -state normal
	.mb.options.pos entryconfigure 4 -state disabled
	.mb.options.pos entryconfigure 5 -state disabled
	if {[string equal $postagset "detailed"] || [string equal $postagset "reduced"]} {
	    set postagset tt
	    update_models
	}
    } elseif {[string equal $lang "dutch"]} {
	.mb.options.pos entryconfigure 0 -state disabled
	.mb.options.pos entryconfigure 1 -state disabled
	.mb.options.pos entryconfigure 2 -state disabled
	.mb.options.pos entryconfigure 3 -state disabled
	.mb.options.pos entryconfigure 4 -state normal
	.mb.options.pos entryconfigure 5 -state normal
	if {![string equal $postagset "detailed"] && ![string equal $postagset "reduced"]} {
	    set postagset detailed
	    update_models
	}
    }

    if {[string equal $grail_parse "chart_pos_lemma"]} {
	.mb.options.par entryconfigure 0 -state disabled
	.mb.options.par entryconfigure 1 -state disabled
	.mb.options.link entryconfigure 0 -state disabled
	.mb.options.link entryconfigure 1 -state disabled
	.mb.options.link entryconfigure 2 -state disabled
	.mb.options.link entryconfigure 3 -state disabled
	.mb.options.debug entryconfigure 0 -state disabled
	.mb.options.debug entryconfigure 1 -state disabled
    } else {
	.mb.options.par entryconfigure 0 -state normal
	.mb.options.par entryconfigure 1 -state normal
	.mb.options.link entryconfigure 0 -state normal
	.mb.options.link entryconfigure 1 -state normal
	.mb.options.link entryconfigure 2 -state normal
	.mb.options.link entryconfigure 3 -state normal
	.mb.options.debug entryconfigure 0 -state normal
	.mb.options.debug entryconfigure 1 -state normal
    }
}

set grail_parse "chart_pos_lemma"


set log [open "$tmp_dir/log.txt" w]
puts $log [clock format [clock seconds] -format "%a %d %b %Y"]
close $log

if {![file exists $tmp_dir]} {
    file mkdir $tmp_dir
}

mk::file open lefff $lefff_prefix/lefff.db -readonly
set morph [mk::view layout lefff.morph "wordpos lemma features"]

set beta 0.1
set algo fwdbwd
set link xpce
set par auto
set lang french

# French POS labels

# treetagger

set vertex(ABR) abbreviation
set vertex(ADJ) adjective
set vertex(ADV) adverb
set vertex(DET:ART) article
set vertex(DET:POS) [list possessive pronoun]
set vertex(INT) interjection
set vertex(KON) conjunction
set vertex(NAM) [list proper name]
set vertex(NOM) noun
set vertex(NUM) numeral
set vertex(PRO) pronoun
set vertex(PRO:DEM) [list demonstrative pronoun]
set vertex(PRO:IND) [list indefinite pronoun]
set vertex(PRO:PER) [list personal pronoun]
set vertex(PRO:POS) [list possessive pronoun]
set vertex(PRO:REL) [list relative pronoun]
set vertex(PRP) preposition
set vertex(PRP:det) [list preposition plus article]
set vertex(PUN) punctuation
set vertex(PUN:cit) [list punctuation citation]
set vertex(PUN:CIT) [list punctuation citation]
set vertex(SENT) [list sentence tag]
set vertex(SYM) symbol
set vertex(VER:cond) [list verb conditional]
set vertex(VER:futu) [list verb futur]
set vertex(VER:impe) [list verb imperative]
set vertex(VER:impf) [list verb imperfect]
set vertex(VER:infi) [list verb infinitive]
set vertex(VER:pres) [list verb present]
set vertex(VER:pper) [list verb past participle]
set vertex(VER:ppre) [list verb present participle]
set vertex(VER:simp) [list verb simple past]
set vertex(VER:subi) [list verb subjunctive imperfect]
set vertex(VER:subp) [list verb subjunctive present]
set vertex(VER:COND) [list verb conditional]
set vertex(VER:FUTU) [list verb futur]
set vertex(VER:IMPE) [list verb imperative]
set vertex(VER:IMPF) [list verb imperfect]
set vertex(VER:INFI) [list verb infinitive]
set vertex(VER:PRES) [list verb present]
set vertex(VER:PPER) [list verb past participle]
set vertex(VER:PPRE) [list verb present participle]
set vertex(VER:SIMP) [list verb simple past]
set vertex(VER:SUBI) [list verb subjunctive imperfect]
set vertex(VER:SUBP) [list verb subjunctive present]

set vertex(NC) [list common noun]
set vertex(NPP) [list proper noun]
set vertex(ADJ) [list adjective]
set vertex(ADV) [list adverb]
set vertex(NPP) [list proper noun]
set vertex(CC) [list coordination]
set vertex(CS) [list subordinate coordination]
set vertex(V) [list verb]
set vertex(VS) [list subjunctive verb]
set vertex(VPP) [list past participle]
set vertex(VPR) [list present participle]
set vertex(VINF) [list infinitive]
set vertex(VIMP) [list imperative]

# Dutch POS labels

set vertex(--) <dummy>
set vertex(UNKNOWN) <automatisch>
set vertex(WW1) WW(pv,ev)
set vertex(WW2) WW(pv,mv)
set vertex(WW3) WW(pv,met-t)
set vertex(WW4) WW(inf,vrij)
set vertex(WW5) WW(inf,prenom)
set vertex(WW6) WW(inf,nom)
set vertex(WW7) WW(vd,vrij)
set vertex(WW8) WW(vd,prenom)
set vertex(WW9) WW(vd,nom)
set vertex(WW10) WW(od,vrij)
set vertex(WW11) WW(od,prenom)
set vertex(WW12) WW(od,nom)
set vertex(WW13) WW(dial)
set vertex(LET) LET
set vertex(TW1) TW(hoofd)
set vertex(TW2) TW(rang)
set vertex(VNW1) VNW(pers,pron,nomin)
set vertex(VNW2) VNW(pers,pron,obl)
set vertex(VNW3) VNW(pers,pron,stan)
set vertex(VNW4) VNW(pers,pron,gen)
set vertex(VNW5) VNW(pers,pron,dial)
set vertex(VNW6) VNW(pr,pron)
set vertex(VNW7) VNW(refl,pron)
set vertex(VNW8) VNW(recip,pron,obl)
set vertex(VNW9) VNW(recip,pron,gen)
set vertex(VNW10) VNW(recip,pron,dial)
set vertex(VNW11) VNW(bez,det)
set vertex(VNW12) VNW(vrag,pron)
set vertex(VNW13) VNW(betr,pron)
set vertex(VNW14) VNW(vb,pron)
set vertex(VNW15) VNW(vb,adv-pron)
set vertex(VNW16) VNW(excl,pron)
set vertex(VNW17) VNW(vb,det)
set vertex(VNW18) VNW(excl,det)
set vertex(VNW19) VNW(aanw,pron)
set vertex(VNW20) VNW(aanw,adv-pron)
set vertex(VNW21) VNW(aanw,det)
set vertex(VNW22) VNW(onbep,pron)
set vertex(VNW23) VNW(onbep,adv-pron)
set vertex(VNW24) VNW(onbep,det,prenom)
set vertex(VNW25) VNW(onbep,det,nom)
set vertex(VNW26) VNW(onbep,det,vrij)
set vertex(VNW27) VNW(onbep,det,dial)
set vertex(TSW) TSW
set vertex(ADJ1) ADJ(prenom)
set vertex(ADJ2) ADJ(nom)
set vertex(ADJ3) ADJ(postnom)
set vertex(ADJ4) ADJ(vrij)
set vertex(ADJ5) ADJ(dial)
set vertex(SPEC) SPEC
set vertex(SPEC2) SPEC(afgebr)
set vertex(SPEC3) SPEC(onverst)
set vertex(SPEC4) SPEC(vreemd)
set vertex(SPEC5) SPEC(deeleigen)
set vertex(SPEC6) SPEC(meta)
set vertex(SPEC8) SPEC(achter)
set vertex(SPEC9) SPEC(comment)
set vertex(VZ1) VZ(init)
set vertex(VZ2) VZ(fin)
set vertex(VZ3) VZ(versm)
set vertex(LID) LID
set vertex(VG1) VG(neven)
set vertex(VG2) VG(onder)
set vertex(BW) BW
set vertex(N1) N(soort,ev,stan)
set vertex(N2) N(soort,ev,gen)
set vertex(N3) N(soort,mv)
set vertex(N4) N(soort,dial)
set vertex(N5) N(eigen,ev,stan)
set vertex(N6) N(eigen,ev,gen)
set vertex(N7) N(eigen,mv)
set vertex(N8) N(eigen,dial)

# ==================
# =    morph2pos   =
# ==================

# 320 morphology tags

set morph2pos(R001) TSW
set morph2pos(R101) N4
set morph2pos(R102) N8
set morph2pos(R201) ADJ12
set morph2pos(R301) WW13
set morph2pos(R401) TW1
set morph2pos(R402) TW2
set morph2pos(R501) VNW5
set morph2pos(R502) VNW7
set morph2pos(R503) VNW10
set morph2pos(R504) VNW11
set morph2pos(R505) VNW14
set morph2pos(R506) VNW17
set morph2pos(R507) VNW14
set morph2pos(R508) VNW27
set morph2pos(R509) VNW16
set morph2pos(R510) VNW18
set morph2pos(R511) VNW19
set morph2pos(R512) VNW21
set morph2pos(R513) VNW22
set morph2pos(R514) VNW27
set morph2pos(R601) LID
set morph2pos(R602) LID
set morph2pos(R701) VZ1
set morph2pos(R702) VZ2
set morph2pos(R801) VG1
set morph2pos(R802) VG2
set morph2pos(R901) BW
set morph2pos(T001) TSW
set morph2pos(T002) SPEC2
set morph2pos(T003) SPEC3
set morph2pos(T004) SPEC4
set morph2pos(T005) SPEC5
set morph2pos(T006) SPEC6
set morph2pos(T007) LET
set morph2pos(T008) SPEC8
set morph2pos(T009) SPEC9
set morph2pos(T101) N1
set morph2pos(T102) N1
set morph2pos(T103) N1
set morph2pos(T104) N2
set morph2pos(T105) N2
set morph2pos(T106) N1
set morph2pos(T107) N3
set morph2pos(T108) N3
set morph2pos(T109) N5
set morph2pos(T110) N5
set morph2pos(T111) N5
set morph2pos(T112) N6
set morph2pos(T113) N6
set morph2pos(T114) N1
set morph2pos(T115) N7
set morph2pos(T116) N7
set morph2pos(T201) ADJ1
set morph2pos(T202) ADJ1
set morph2pos(T203) ADJ1
set morph2pos(T204) ADJ2
set morph2pos(T205) ADJ2
set morph2pos(T206) ADJ2
set morph2pos(T207) ADJ3
set morph2pos(T208) ADJ3
set morph2pos(T209) ADJ3
set morph2pos(T210) ADJ4
set morph2pos(T211) ADJ4
set morph2pos(T212) ADJ4
set morph2pos(T213) ADJ4
set morph2pos(T214) ADJ4
set morph2pos(T215) ADJ5
set morph2pos(T216) ADJ5
set morph2pos(T217) ADJ5
set morph2pos(T218) ADJ5
set morph2pos(T219) ADJ6
set morph2pos(T220) ADJ6
set morph2pos(T221) ADJ6
set morph2pos(T222) ADJ6
set morph2pos(T223) ADJ7
set morph2pos(T224) ADJ7
set morph2pos(T225) ADJ8
set morph2pos(T226) ADJ8
set morph2pos(T227) ADJ9
set morph2pos(T228) ADJ10
set morph2pos(T229) ADJ11
set morph2pos(T230) ADJ9
set morph2pos(T301) WW1
set morph2pos(T302) WW2
set morph2pos(T303) WW3
set morph2pos(T304) WW1
set morph2pos(T305) WW2
set morph2pos(T306) WW3
set morph2pos(T309) WW1
set morph2pos(T310) WW5
set morph2pos(T311) WW5
set morph2pos(T312) WW6
set morph2pos(T314) WW4
set morph2pos(T315) WW8
set morph2pos(T316) WW8
set morph2pos(T317) WW9
set morph2pos(T318) WW9
set morph2pos(T320) WW7
set morph2pos(T321) WW11
set morph2pos(T322) WW11
set morph2pos(T323) WW12
set morph2pos(T324) WW12
set morph2pos(T326) WW10
set morph2pos(T401) TW1
set morph2pos(T402) TW1
set morph2pos(T403) TW1
set morph2pos(T404) TW1
set morph2pos(T405) TW1
set morph2pos(T406) TW1
set morph2pos(T407) TW1
set morph2pos(T408) TW2
set morph2pos(T409) TW2
set morph2pos(T410) TW2
set morph2pos(T411) TW2
set morph2pos(T501a) VNW1
set morph2pos(T501b) VNW1
set morph2pos(T501c) VNW1
set morph2pos(T501d) VNW1
set morph2pos(T501e) VNW1
set morph2pos(T501f) VNW1
set morph2pos(T501g) VNW1
set morph2pos(T501h) VNW1
set morph2pos(T501i) VNW1
set morph2pos(T501p) VNW1
set morph2pos(T501s) VNW1
set morph2pos(T501t) VNW1
set morph2pos(T502a) VNW2
set morph2pos(T502c) VNW2
set morph2pos(T504a) VNW4
set morph2pos(T504b) VNW4
set morph2pos(T504d) VNW4
set morph2pos(T510a) VNW11
set morph2pos(T510c) VNW11
set morph2pos(T510j) VNW11
set morph2pos(T511a) VNW11
set morph2pos(T511b) VNW11
set morph2pos(T511c) VNW11
set morph2pos(T511d) VNW11
set morph2pos(T511g) VNW11
set morph2pos(T511j) VNW11
set morph2pos(T511k) VNW11
set morph2pos(T513a) VNW11
set morph2pos(T513b) VNW11
set morph2pos(T513d) VNW11
set morph2pos(T513e) VNW11
set morph2pos(T516a) VNW13
set morph2pos(T525a) VNW19
set morph2pos(T525b) VNW19
set morph2pos(T530a) VNW21
set morph2pos(T530b) VNW21
set morph2pos(T532a) VNW21
set morph2pos(T533a) VNW21
set morph2pos(T540a) VNW24
set morph2pos(T541a) VNW24
set morph2pos(T541b) VNW24
set morph2pos(T544a) VNW25
set morph2pos(T546a) VNW25
set morph2pos(T601) LID
set morph2pos(T602) LID
set morph2pos(T603) LID
set morph2pos(T605) LID
set morph2pos(T606) LID
set morph2pos(T607) LID
set morph2pos(T609) LID
set morph2pos(T701) VZ1
set morph2pos(T702) VZ2
set morph2pos(T703) VZ3
set morph2pos(T801) VG1
set morph2pos(T802) VG2
set morph2pos(T901) BW
set morph2pos(U117) N1
set morph2pos(U118) N5
set morph2pos(U501j) VNW1
set morph2pos(U501k) VNW1
set morph2pos(U501l) VNW1
set morph2pos(U501m) VNW1
set morph2pos(U501n) VNW1
set morph2pos(U501o) VNW1
set morph2pos(U501q) VNW1
set morph2pos(U501r) VNW1
set morph2pos(U501u) VNW1
set morph2pos(U501v) VNW1
set morph2pos(U502b) VNW2
set morph2pos(U502d) VNW2
set morph2pos(U502e) VNW2
set morph2pos(U502f) VNW2
set morph2pos(U502g) VNW2
set morph2pos(U502h) VNW2
set morph2pos(U502i) VNW2
set morph2pos(U503a) VNW3
set morph2pos(U503b) VNW3
set morph2pos(U503c) VNW3
set morph2pos(U503d) VNW3
set morph2pos(U504c) VNW4
set morph2pos(U504e) VNW4
set morph2pos(U504f) VNW4
set morph2pos(U505a) VNW6
set morph2pos(U505b) VNW6
set morph2pos(U505c) VNW6
set morph2pos(U505d) VNW6
set morph2pos(U505e) VNW6
set morph2pos(U505f) VNW6
set morph2pos(U505g) VNW6
set morph2pos(U505h) VNW6
set morph2pos(U505i) VNW6
set morph2pos(U506a) VNW7
set morph2pos(U506b) VNW7
set morph2pos(U507a) VNW8
set morph2pos(U508a) VNW9
set morph2pos(U509a) VNW11
set morph2pos(U509b) VNW11
set morph2pos(U509c) VNW11
set morph2pos(U509d) VNW11
set morph2pos(U509e) VNW11
set morph2pos(U509f) VNW11
set morph2pos(U509g) VNW11
set morph2pos(U509h) VNW11
set morph2pos(U509i) VNW11
set morph2pos(U509j) VNW11
set morph2pos(U509k) VNW11
set morph2pos(U509l) VNW11
set morph2pos(U509m) VNW11
set morph2pos(U509n) VNW11
set morph2pos(U509o) VNW11
set morph2pos(U509p) VNW11
set morph2pos(U509q) VNW11
set morph2pos(U510b) VNW11
set morph2pos(U510d) VNW11
set morph2pos(U510e) VNW11
set morph2pos(U510f) VNW11
set morph2pos(U510g) VNW11
set morph2pos(U510h) VNW11
set morph2pos(U510i) VNW11
set morph2pos(U510k) VNW11
set morph2pos(U510l) VNW11
set morph2pos(U510m) VNW11
set morph2pos(U511e) VNW11
set morph2pos(U511f) VNW11
set morph2pos(U511h) VNW11
set morph2pos(U511i) VNW11
set morph2pos(U511l) VNW11
set morph2pos(U511m) VNW11
set morph2pos(U512h) VNW11
set morph2pos(U512i) VNW11
set morph2pos(U512j) VNW11
set morph2pos(U512k) VNW11
set morph2pos(U512l) VNW11
set morph2pos(U512m) VNW11
set morph2pos(U512n) VNW11
set morph2pos(U512o) VNW11
set morph2pos(U512p) VNW11
set morph2pos(U512q) VNW11
set morph2pos(U512r) VNW11
set morph2pos(U512s) VNW11
set morph2pos(U512t) VNW11
set morph2pos(U512u) VNW11
set morph2pos(U513c) VNW11
set morph2pos(U513f) VNW11
set morph2pos(U514a) VNW12
set morph2pos(U515a) VNW13
set morph2pos(U515b) VNW13
set morph2pos(U515c) VNW13
set morph2pos(U515d) VNW13
set morph2pos(U516b) VNW14
set morph2pos(U517a) VNW14
set morph2pos(U517b) VNW14
set morph2pos(U518a) VNW14
set morph2pos(U518b) VNW14
set morph2pos(U518c) VNW14
set morph2pos(U519a) VNW15
set morph2pos(U520a) VNW16
set morph2pos(U521a) VNW17
set morph2pos(U521b) VNW17
set morph2pos(U522a) VNW17
set morph2pos(U523a) VNW18
set morph2pos(U524a) VNW19
set morph2pos(U524b) VNW19
set morph2pos(U524c) VNW19
set morph2pos(U526a) VNW20
set morph2pos(U527a) VNW20
set morph2pos(U528a) VNW21
set morph2pos(U528b) VNW21
set morph2pos(U528c) VNW21
set morph2pos(U528d) VNW21
set morph2pos(U529a) VNW21
set morph2pos(U531b) VNW21
set morph2pos(U531c) VNW21
set morph2pos(U534a) VNW21
set morph2pos(U535a) VNW22
set morph2pos(U535b) VNW22
set morph2pos(U536a) VNW22
set morph2pos(U537a) VNW23
set morph2pos(U538a) VNW23
set morph2pos(U539a) VNW24
set morph2pos(U539b) VNW24
set morph2pos(U539c) VNW24
set morph2pos(U539d) VNW24
set morph2pos(U539e) VNW24
set morph2pos(U539f) VNW24
set morph2pos(U542a) VNW24
set morph2pos(U542b) VNW24
set morph2pos(U542c) VNW24
set morph2pos(U542d) VNW24
set morph2pos(U542e) VNW24
set morph2pos(U542f) VNW24
set morph2pos(U543a) VNW25
set morph2pos(U543b) VNW25
set morph2pos(U543c) VNW25
set morph2pos(U545a) VNW25
set morph2pos(U545b) VNW25
set morph2pos(U545d) VNW25
set morph2pos(U545e) VNW25
set morph2pos(U545f) VNW25
set morph2pos(U547a) VNW26
set morph2pos(U548a) VNW26
set morph2pos(U548b) VNW26
set morph2pos(U548c) VNW26
set morph2pos(U604) LID
set morph2pos(U608) LID

#

proc write_log {s} {

    global tmp_dir

    set log [open "$tmp_dir/log.txt" a]
    puts $log $s
    close $log

}
    

proc get_pos_dir {} {

    global pos_model lang

    set dir [tk_chooseDirectory -initialdir $pos_model($lang) -mustexist true -title "Select a POS model directory"]

    if {$dir eq ""} { 
	return 
    } 

    if {[check_existing_files $dir [list attributes classes config contexts features info lexicon number_unknowns tagdict unknowns weights]] == 0} {
	set pos_model($lang) $dir
    }

}

proc get_st_dir {} {

    global st_model lang

    set dir [tk_chooseDirectory -initialdir $st_model($lang) -mustexist true -title "Select a supertag model directory"]

    if {$dir eq ""} { 
	return 
    } 

    if {[check_existing_files $dir [list attributes classes config contexts features info lexicon posdict postags tagdict unknowns weights]] == 0} {
	set st_model($lang) $dir
    }

}


proc check_existing_files {dir list} {

    set savedir [pwd]
    cd $dir

    set missing ""
    set m 0

    foreach f $list {
	if {![file readable $f]} {
	    set missing "$missing - $f\n"
	    incr m
	}
    }

    cd $savedir

    if {$m > 0} {
	tk_dialog .dialog "Missing Files" "Missing files:\n$missing\nDirectory change cancelled." error 0 Ok
    }
    
    return m

}

proc printmode {m} {

    global mode0

    if {[string equal $m "0"]} {
	set m $mode0
    } elseif {[string equal $m "1"]} {
	set m "\u2081"
    } elseif {[string equal $m "2"]} {
	set m "\u2082"
    } elseif {[string equal $m "3"]} {
	set m "\u2083"
    } elseif {[string equal $m "4"]} {
	set m "\u2084"
    }

    return $m

}

proc printform {string} {

    global mode0 spc bullet

    set l [parseform $string]

    if {[llength $l] == 1} {
	return $string
    } else {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "$s1$spc\\$m$spc$s2"
	} elseif {[string equal $c "dr"]} {
	    return "$s1$spc/$m$spc$s2"
	} elseif {[string equal $c "p"]} {
	    return "$s1$spc$bullet$m$spc$s2"
	}
    }
}

proc printform1 {l} {

    global diamond box bullet mode0 spc

    if {[llength $l] == 1} {
	return $l
    } elseif {[llength $l] == 3} {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	if {[string equal $c "dia"]} {
	    return "$diamond$m$s1"
	} elseif {[string equal $c "box"]} {
	    return "$box$m$s1"
	}

    } elseif {[llength $l] == 4} {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "($s1$spc\\$m$spc$s2)"
	} elseif {[string equal $c "dr"]} {
	    return "($s1$spc/$m$spc$s2)"
	} elseif {[string equal $c "p"]} {
	    return "($s1$spc$bullet$m$spc$s2)"
	}
    }
}


# parseform
#
# transform a string containing a formula into a list

proc parseform {string} {

    global res

    set string [string trim $string]

    set i [parseform1 $string 0]
    if {$i != [string length $string]} {
	puts stderr "$i [string length $string]"
	puts stderr "Trailing material deleted! .[string range $string $i end].$res."
    }
	return $res

}

proc parseform1 {string begin} {

    global arg1 arg2 index res

    set i $begin

    if {[string equal [string range $string $i [expr $i+2]] "dl("]} {
	set i [parserest $string [expr $i+3]]
	set res [list dl $index $arg1 $arg2]
    } elseif {[string equal [string range $string $i [expr $i+2]] "dr("]} {
	set i [parserest $string [expr $i+3]]
	set res [list dr $index $arg1 $arg2]
    } elseif {[string equal [string range $string $i [expr $i+1]] "p("]} {
	set i [parserest $string [expr $i+2]]
	set res [list p $index $arg1 $arg2]
    } elseif {[string equal [string range $string $i [expr $i+3]] "dia("]} {
	set i [parserest_u $string [expr $i+4]]
	set res [list dia $index $arg1]
    } elseif {[string equal [string range $string $i [expr $i+3]] "box("]} {
	set i [parserest_u $string [expr $i+4]]
	set res [list box $index $arg1]
    } else {
	# atomic formula
	set j [string first "," $string $i]
	set k [string first ")" $string $i]
	if {$j < $k && $j != -1} {
	    set res [string range $string $begin [expr $j-1]]
	    set i $j
	} elseif {$k != -1} {
	    set res [string range $string $begin [expr $k-1]]
	    set i $k
	} else {
	    set res [string range $string $begin end]
	    set i [string length $string]
	}
    }

    return $i
}

proc parserest {string i} {

    global res arg1 arg2 index

    set i [parseindex $string $i]
    set thisindex $index
    set i [parsecomma $string $i]
    set i [parseform1 $string $i]
    set thisarg1 $res
    set i [parsecomma $string $i]
    set i [parseform1 $string $i]
    set thisarg2 $res
    set i [parseparnc $string $i]

    set index $thisindex
    set arg1 $thisarg1
    set arg2 $thisarg2

    return $i
}

proc parserest_u {string i} {

    global res arg1 index

    set i [parseindex $string $i]
    set thisindex $index
    set i [parsecomma $string $i]
    set i [parseform1 $string $i]
    set thisarg1 $res
    set i [parseparnc $string $i]

    set index $thisindex
    set arg1 $thisarg1

    return $i
}

proc parsecomma {string i} {

    if {[string equal [string range $string $i $i] ","]} {
	return [expr $i+1]
    } else {
	puts stderr "Missing comma: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]"
	exit 1
    }

}

proc parseparnc {string i} {

    if {[string equal [string range $string $i $i] ")"]} {
	return [expr $i+1]
    } else {
	puts stderr "Missing close parenthesis: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]"
	exit 1
    }

}

proc parseindex {string i} {

    global index

    if {[set j [string first "," $string $i]] != -1} {
	set index [string range $string $i [expr $j-1]]
	return $j
    } else {
	puts stderr "Missing comma: $string"
	exit 1
    }

}

proc abbrev {x y string} {
    if {[string length $string] > 13} {
	set this [.c create text $x $y -tags $string -text "[string range $string 0 9]..."]
        .c bind $this <Enter> {
	    printcomment [lindex [.c gettags current] 0]
	}
        .c bind $this <Leave> {
	    printcurrentcomment
        }
    } else {
	.c create text $x $y -text $string
    }
}

proc printst {x y list} {

    global st_conf_color st_other_color xwidth

    set skip -15

    for {set i 0} {$i < [llength $list]} {incr i +2} {

	set x0 [expr $x-10]
	set xr [expr $x0+15]
	set x1 [expr $x0+20]
	set y1 [expr $y-7.5]
	set y2 [expr $y+7.5]
	set f [lindex $list $i]
	set p [lindex $list [expr $i + 1]]
	set xw [expr $x0+($p*15)]
	
	set fstring [string range [printform $f] 0 $xwidth]

	set tags [list "[printform $f]" "([round [expr $p*100.0] 2]\%)"] 
	set r1 [.c create rectangle $xw $y1 $xr $y2 -tags $tags -fill $st_other_color -outline $st_other_color]
	set r2 [.c create rectangle $x0 $y1 $xw $y2 -tags $tags -fill $st_conf_color -outline $st_conf_color]
        set r3 [.c create rectangle $x0 $y1 [expr $xr+1] $y2 -tags $tags]
	set this [.c create text $x1 $y -tags $tags  -text "$fstring" \
		      -anchor w]
		foreach o [list $this $r1 $r2 $r3] {
	    .c bind $o <Enter> {
		printcomment "[lindex [.c gettags current] 0] [lindex [.c gettags current] 1]"
	    }
	    .c bind $o <Leave> {
		printcurrentcomment
	    }
	}
	incr y $skip
    }

}

proc printwords {y list} {

    global skip 

    set x 10

    foreach i $list {
	abbrev [expr $x+($skip/2)] $y $i
	incr x $skip
    }
    update idletasks
    return [expr $x+10]
}

proc printpos {y list} {

    global skip

    global vertex morph2pos lang pos_conf_color pos_other_color

    set x 10

    set y0 [expr $y-5]
    set ym [expr $y+4]

    foreach i $list {
	set item [split $i "|"]
	set pos [lindex $item 1]
#	set pos_list [split $pos "-"]
#	if {[llength $pos_list > 1]} {
#	    set pos_tt   [lindex $pos_list 0]
#	    set pos_melt [lindex $pos_list 1]
#	    if {$pos_tt != ""} {
#		set pos $pos_tt
#	    }
#	}
	set prob [lindex $item 2]
	set right [expr $x+$skip]
	set dist [expr $x+($skip*$prob)]
	set top [expr $y0+15]
	set xm [expr $x+($skip/2)]
	if [catch {set vertex($pos)}] {
	    set tags [list "?" "([expr $prob*100.0]/%)"]
	} else {
	    set tags [list "$vertex($pos)" "([expr $prob*100.0]\%)"]
	}
	set r1 [.c create rectangle $dist $y0 $right $top -tags $tags -fill $pos_other_color]
	set r2 [.c create rectangle $x $y0 $dist $top -tags $tags -fill $pos_conf_color]
	set r3 [.c create rectangle $x $y0 $right $top -tags $tags]
	set this [.c create text $xm $ym -tags $tags -text $pos]
	foreach o [list $this $r1 $r2 $r3] {
	    .c bind $o <Enter> {
		printcomment "[lindex [.c gettags current] 0] [lindex [.c gettags current] 1]"
	    }
	    .c bind $o <Leave> {
		printcurrentcomment
	    }
	}
	incr x $skip
    }
    update idletasks
}

proc backslash_interpunction {word} {

    # convert "European-style" numbers (eg. 12.345.678,90) into real numbers as Prolog likes them
    # (eg. 12345678.90) 
    puts stderr "BI: $word"
    regsub -all {(\d{3})\.(\d{3})} $word {\1\2} word
    regsub {^(\{?)(\d{1,3})\.(\d{3})(\}?)$} $word {\1\2\3\4} word
#    regsub {^(\d{1,3})\.(\d{3})$} $word {\1\2} word
    regsub {^(\d+)\,(\d+)$} $word {\1.\2} word
    puts stderr "BI: $word"
#    regsub -all {([[:digit:]]{3})\.([[:digit:]]{3})} $word "\1\2" word
#    regsub -all {([[:digit:]]{1,3})\.([[:digit:]]{3})} $word "\1\2" word
#    regsub -all {([[:digit:]])\,([[:digit:]])} $word "\1.\2" word
#    regsub -all "(\[0-9]{3})\\\.(\[0-9]{3})" $word "\1\2" word
#    regsub -all "(\[0-9]{1,3})\\\.(\[0-9]{3})" $word "\1\2" word
#    regsub -all "(\[0-9])\\\,(\[0-9])" $word "\1.\2" word
#    puts stderr "BI: $word"

    regsub -all "\'" $word "\\\'" word
#    regsub -all {\(} $word "\\\(" word
#    regsub -all {\)} $word "\\\)" word
#    regsub -all {\"} $word "\\\"" word
#    regsub -all "\;" $word "\\\;" word
#    regsub -all "\," $word "\\\," word
#    regsub -all {\.} $word "\\\." word
#    regsub -all {\?} $word "\\\?" word
#    regsub -all {\!} $word "\\\!" word

    puts stderr "BI: $word"
    puts stderr "---------"

    return $word

}


proc clean {list} {

    global c_pos_list

    set c_pos_list {}
    set result {}

    foreach i $list {
	set item [split $i "|"]
	lappend result [lindex $item 0]
	lappend c_pos_list [lindex $item 1]
    }

    return $result
}

proc givenpos {pos user_pos} {

    set result {}

    for {set n 0} {$n < [llength $pos]} {incr n} {
	set i [lindex $pos $n]
	set items [split $i "|"]
	set wi [lindex $items 0]
	set up [lindex $user_pos $n]
	if {$up != {}} {
	    # use the POS tag provided by the user
	    set pi "[cap_cmd $up]|1"
	} else {
	    # collect all possible tags and select the first which is in Lefff
	    # (or simply the first POS tag if none occur in Lefff)
	    set w_pos {}
	    set w_prob {}
	    for {set j 2} {$j < [llength $items]} {incr j +2} {
		set i_pos [lindex $items $j]
		set i_prob [lindex $items [expr $j+1]]
		lappend w_pos $i_pos
		lappend w_prob $i_prob
	    }
	    set pi [get_lefff_pos $wi $w_pos $w_prob]
	}

	lappend result [join [list $wi $pi] "|"]

    }

    return $result

}

proc cap_cmd {word} {

    global lang morph2pos

    if {[string equal $lang "dutch"]} {
	return [string tolower $word]
    } elseif {[string equal $lang "french"]} {
	return $word
    }

}

proc printcurrentcomment {} {

	global comment

	printcomment $comment

}

proc capitalize {string} {
    return [string toupper $string 0 0]
}

proc uncapitalize {string} {
    return [string tolower $string 0 0]
}

proc get_lemma {word pos} {

    global morph

    set lemma $word

    if {![string equal $pos "NAM"]} {
	set lemma [string tolower $word]
    } else {
	set lemma $word
    }

    # search for word in Lefff
    set rows [mk::select $morph -exact wordpos $word|$pos]
    # in case no matches are found, try the lower-case
    if {$rows == {}} {
	set rows [mk::select $morph -exact wordpos [string tolower $word]|$pos]
    }
    if {$rows == {}} {
	return $word
    }

    set row [lindex $rows 0]
    set lemma [mk::get $morph!$row lemma]

    write_log "Lemma: $word $lemma"

    return $lemma
}

proc get_lefff_details {word} {

    global morph

    write_log "LEFFF: $word"

    set rows [mk::select $morph -exact wordpos $word]
    if {$rows == {}} {
	set rows [mk::select $morph -exact wordpos [string tolower $word]]
    }
    if {$rows == {}} {
	return ""
    }
    set row [lindex $rows end]
    set features [mk::get $morph!$row features]

    set details ""
    set hum [expr ($features>>5)&1]
    set loc [expr ($features>>4)&1]
    set masc [expr ($features>>3)&1]
    set fem [expr ($features>>2)&1]
    set sing [expr ($features>>1)&1]
    set plur [expr $features&1]

    write_log "LEFFF: hum $hum loc $loc m $masc f $fem s $sing p $plur"
    if {$hum == 1 && $loc == 0} {
	set details "$details h"
    }
    if {$hum == 0 && $loc == 1} {
	set details "$details l"
    }
    if {$masc == 1 && $fem == 0} {
	set details "$details m"
    }
    if {$masc == 0 && $fem == 1} {
	set details "$details f"
    }
    if {$sing == 1 && $plur == 0} {
	set details "$details s"
    }
    if {$sing == 0 && $plur == 1} {
	set details "$details p"
    }
    regsub -all " " $details "" details
#   puts stderr "LEFFF: $details"
    return $details

}

proc get_lefff_pos {word pos_l prob_l} {

    global morph

    puts stderr "POS: $word $pos_l"
    set pos [lindex $pos_l 0]
    set prob [lindex $prob_l 0]

    set rows [mk::select $morph -exact word $word|$pos]
    if {$rows == {}} {
	set rows [mk::select $morph -exact word [string tolower $word]|$pos]
    }
    if {$rows == {}} {
	return "$pos|$prob"
    }
    set i 0
    foreach p $pos_l {
	if {[mk::select $morph -exact $word|$p] != {}} {
	    puts stderr "LEFFFPOS: $p ($pos)"
	    return "$p|[lindex $prob_l $i]"
        }  
	if {[mk::select $morph -exact [string tolower $word]|$p] != {}} {
	    puts stderr "LEFFFPOS: $p ($pos)"
	    return "$p|[lindex $prob_l $i]"
        }
    }

    return "$pos|$prob"

}

proc iso8859 {string} {
    # replace "_" by "_"
#    puts stderr $string
    if {[regexp {^[[:punct:]]+$} $string]} {
	return $string
    }
    regsub -all -- "-" $string "_" string
#    puts stderr $string
    # replace sequences of tabs, newlines etc. by a single space
    regsub -all {\s+} $string " " string 
#    puts stderr $string
    # interpunction
    # special characters
    # XML special characters
    regsub -all \&euml\; $string ë string
    regsub -all \&iuml\; $string ï string
    regsub -all \&ouml\; $string ö string
    regsub -all \&uuml\; $string ü string
    regsub -all \&ccedil\; $string ç string
    regsub -all \&eacute\; $string é string
    regsub -all \&egrave\; $string è string
    regsub -all \&agrave\; $string à string
    regsub -all \&Eacute\; $string É string
    regsub -all \&Egrave\; $string È string
    # erase remaining non-alphanumeric characters at beginning
    # and end of words, primarily interpunction symbols at this point
    regsub -all {^[^[:alnum:]_ ]} $string "" string
    regsub -all {[^[:alnum:]_ ]$} $string "" string
    regsub -all {[^[:alnum:]_ ]\s} $string " " string
    regsub -all {\s[^[:alnum:]_ ]} $string " " string
#    puts stderr $string
    return $string 
}

proc printcomment {comment} {
    .l configure -text $comment
}

proc supertag {sentence} {

    global comment grail_cmd pos_cmd pos_model st_cmd st_model
    global beta algo link par grammar_prefix debug debugstring skip
    global lang tmp_dir c_pos_list semantics grail_parse monde_prefix lefff_prefix grail_prefix grail_exec
    global bootstrap_parser_cmd bootstrap_parser_length
    
    .c delete all

    set s_tok [tokenize $sentence]
    puts stderr $s_tok
    set s_tmp [split $s_tok "\n"]
    set s_list {}
    set s_given {}
    foreach s $s_tmp {
	lappend s_list [clean $s]
	lappend s_given $c_pos_list
    }
    
    set sent [lindex $s_list 0]
    set s_given_pos [lindex $s_given 0]

    puts stderr $s_list
    puts stderr $s_given
    set maxx [printwords 190 $sent]

    .c configure -scrollregion [list 0 180 $maxx 200]

    # POS tagger

    set comment "POS tagging"
    printcurrentcomment

    update idletasks

    set f1  [open "$tmp_dir/input.txt" w]
    set log [open "$tmp_dir/supertaglog.txt" a]

    foreach s $s_list {
	puts $f1 $s
    }

    write_log "# sentence"
    write_log $s_tok
    close $f1


    write_log "# POS tag"

    if {[catch {exec $pos_cmd --model $pos_model($lang) --ofmt "%w|%P \\n" --input $tmp_dir/input.txt > $tmp_dir/postag.txt} pos_msg] } {
	puts stderr $pos_msg
    }
    set pt [open "$tmp_dir/postag.txt" r]
    set pw [open "$tmp_dir/st_input.txt" w]

    set p_list {}
    set sentno 0
    while {[gets $pt line] >= 0} {
        set line2 [givenpos $line [lindex $s_given $sentno]]
	incr sentno
	lappend p_list $line2
	puts stderr "Line2: $line2"
	write_log $line
	write_log $line2
	puts $pw $line2

	set lem_list {}
	set pos_list [split $line2]
	foreach member $pos_list {
	    
	    set pl_item [split $member "|"]
	    puts stderr "$member - $pl_item"
	    set lem [get_lemma [lindex $pl_item 0] [lindex $pl_item 1]]
	    lappend lem_list "{$lem}"

	}
	puts stderr "$lem_list"
    }
    printpos 160 [lindex $p_list 0]

    .c configure -scrollregion [list 0 140 $maxx 200]

    close $pt
    close $pw

    set comment "Supertagging"
    printcurrentcomment
    update idletasks

    if {[catch {exec $st_cmd --input $tmp_dir/st_input.txt --output $tmp_dir/supertag.txt --ifmt "%w|%p|%0 \n" --ofmt "%w|%p|%S \n" --model $st_model($lang) --algorithm $algo --beta $beta} super_msg]} {
	puts stderr $super_msg
    }

    set comment "Parsing"
    printcurrentcomment
    update idletasks

    set f2 [open "$tmp_dir/supertag.txt" r]
    set parser_file [open "$tmp_dir/parser.pl" w]
    puts -nonewline $parser_file "% Automatically generated by Supertag.tcl on "
    puts $parser_file [clock format [clock seconds] -format "%a %d %b %Y"]
    puts $parser_file ""

    set counter -1
    while {[gets $f2 line] > 0} {

	incr counter

	set tcomma ""
	puts $parser_file "sent([expr $counter+1], A) :-"
	puts -nonewline $parser_file "     prob_parse(\["


	if {$counter > 0} {
	    .c delete all
	    printwords 190 [lindex $s_list $counter]
	    printpos 160 [lindex $p_list $counter]
	    update idletasks
	 
	}


	if {[string equal $lang "frenchx"]} {
	    set flang "french"
	} else {
	    set flang $lang
	}
	set sem_file_name [join [list "big" $flang $semantics] "_"]

	set list1 [split $line]
	set list2 {}
	set grailcmd "$grail_cmd $debugstring grammar '$grammar_prefix/$sem_file_name.pl' link $link par $par $grail_parse"

	set x 20
	set miny 130

	set list_counter -1
	foreach i $list1 {
	    incr list_counter
	    set il [split $i "|"]
	    set word [lindex $il 0]
	    set pos [lindex $il 1]
	    set nlist [lrange $il 3 end]
	    set tmpy [expr 130-(7.5*[llength $nlist])]
	    if {$tmpy < $miny} {
		set miny $tmpy
	    }
	    printst $x 130 $nlist

	    if {[string equal $pos "NOM"] || [string equal $pos "NAM"]} {
		set details [get_lefff_details $word|$pos]
		if {$details != ""} {
		    set pos "$pos:$details"
		}
	    }
	    set word [backslash_interpunction $word]
	    set lemma [backslash_interpunction [lindex $lem_list $list_counter]]

	    set grailcmd "$grailcmd {$word} [string tolower $pos] $lemma"
	    set flist {}
	    set parselist {}
	    set counter 0
	    set comma ""
	    set length [expr [llength $nlist] -1]
	    puts $parser_file $tcomma
	    puts -nonewline $parser_file "       si('$word', [string tolower $pos], '[lindex $lemma 0]', \["
	    set tcomma ","
	    foreach j $nlist {
		lappend flist $j
		if {[expr {($counter % 2) == 0}]} {
		    puts -nonewline $parser_file "$comma$j"
		} else {
		    puts -nonewline $parser_file "-$j"
		}
		incr counter
		set comma ", "
	    }
	    puts -nonewline $parser_file " \])"
	    set forms [join $flist -]
	    set grailcmd "$grailcmd '$forms'"

	    incr x $skip

	}
	puts $parser_file ""
	puts $parser_file "     \], A)."
	puts $parser_file ""
	close $parser_file
	# finished sentences
	# use bootstrap parser to compute constituent structure
	if {[file executable $bootstrap_parser_cmd]} {
	    # parse input sentences
	    if {[catch {exec $bootstrap_parser_cmd $bootstrap_parser_length $tmp_dir/input.txt} bparse_msg]} {
		puts stderr $bparse_msg
	    }
	    # if a parse has been found, convert it to crosses declarations and add these to the GrailLight input
	    if {[file exists $tmp_dir/input.txt.30.stp]} {
		if {[catch {exec $grail_prefix/read_trees.pl $tmp_dir/input.txt.30.stp} ptrees_msg]} {
		    puts stderr $ptrees_msg
		}
		if {[file exists $tmp_dir/parser_crosses.pl]} {
		    exec cat $tmp_dir/parser_crosses.pl >> $tmp_dir/parser.pl
		}
	    }
	}
	.c configure -scrollregion [list 0 $miny $maxx 200]
	update idletasks

	set string [join $list2]

	# write logs
	write_log "# supertag"
	write_log "$line"
	write_log "# Grail command"
	write_log "$grailcmd"
	write_log "# -------------------------------------------------"

	# write Grail command to a separate file
        set f3 [open "$tmp_dir/grail_cmd.sh" w]
	puts -nonewline $f3 "# Automatically generated by Supertag.tcl on "
	puts $f3 [clock format [clock seconds] -format "%a %d %b %Y"]
	puts $f3 "# $s_tok"
	puts $f3 "$grailcmd"
	close $f3

	puts $line
	puts $forms
	
	puts $grailcmd
	set saved_dir [pwd]
	cd $tmp_dir
	if {[string equal $grail_parse "chart_pos_lemma"]} {
	    puts stderr "$grail_prefix/$grail_exec $tmp_dir/parser.pl"
	    if {[catch {exec $grail_prefix/$grail_exec $tmp_dir/parser.pl} gl_msg]} {
		puts stderr $gl_msg
	    }
	    puts stderr "DONE!"
	} elseif {[catch {exec /bin/sh -c $grailcmd > grail_log.txt} grail_msg]} {
	    puts stderr $grail_msg
	}
	cd $saved_dir
    }

    # if the parse has been sucessful, then a proof has been produced and proof.tex  
    # is not empty
    if {[file size "$tmp_dir/proof.tex"] == 0} {
	set comment "Failed!"
    } else {
	set comment "Done!"
    }

    close $f2
    close $log
    printcurrentcomment

}

wm title . "Supertagger"
frame .f
canvas .c -yscrollcommand [list .sy set] -xscrollcommand [list .sx set]
scrollbar .sx -orient horizontal -command [list .c xview]
scrollbar .sy -orient vertical -command [list .c yview]
frame .f2
label .l -relief flat -bd 2
spinbox .e -width 50 -textvariable phrase
button .b -text "Supertag" -command { supertag $phrase }
pack .f -fill both -expand true
pack .f2 -fill x -expand true
pack .e -side left -fill x -padx 2 -in .f2
pack .b -side right -in .f2
pack .l -side bottom -fill x -padx 2 -pady 2

label .filler

grid .c -in .f -row 0 -column 0 -sticky nsew
grid .sx -row 1 -column 0 -sticky nsew -in .f
grid .sy -row 0 -column 1 -sticky nsew -in .f
grid .filler -row 1 -column 1 -in .f
grid rowconfigure .f 0 -weight 1
grid columnconfigure .f 0 -weight 1

#pack .c -side left -fill both -expand true -in .f



menu .mb -tearoff 0
. configure -menu .mb

.mb add cascade -menu .mb.file -label File -menu .mb.file
.mb add cascade -menu .mb.options -label Options -menu .mb.options
.mb add cascade -menu .mb.colors -label Colors -menu .mb.colors
.mb add cascade -menu .mb.help -label Help -menu .mb.help

menu .mb.file -tearoff 0
.mb.file add command -label "About..." -command {st_dialog}
.mb.file add separator
.mb.file add command -label "Change POS model directory..." -command {get_pos_dir}
.mb.file add command -label "Change supertag model directory..." -command {get_st_dir}
.mb.file add separator
.mb.file add command -label "Export postscript" -command {.c postscript -file "supertag.ps"}
.mb.file add command -label "Proof (LaTeX/pdf)" -command { latex_proofs }
.mb.file add command -label "Semantics (LaTeX/pdf)" -command { catch { exec open "$tmp_dir/semantics.pdf" } }
.mb.file add separator
.mb.file add command -label "Quit" -command {destroy .}

menu .mb.options -tearoff 0
.mb.options add cascade -label Link -menu .mb.options.link
.mb.options add cascade -label Par -menu .mb.options.par
.mb.options add separator
.mb.options add cascade -label Algorithm -menu .mb.options.algo
.mb.options add cascade -label Beta -menu .mb.options.beta
.mb.options add separator
.mb.options add cascade -label Language -menu .mb.options.lang
.mb.options add cascade -label "POS model" -menu .mb.options.pos
.mb.options add separator
.mb.options add cascade -label Parser -menu .mb.options.parser
.mb.options add cascade -label "Proof Output" -menu .mb.options.output
.mb.options add cascade -label Semantics -menu .mb.options.sem
.mb.options add separator
.mb.options add cascade -label Debug -menu .mb.options.debug

menu .mb.options.parser
.mb.options.parser add radio -label "Grail + Chart" -variable grail_parse -value chart_pos_lemma -command {update_menus}
.mb.options.parser add radio -label "Grail + Optimization" -variable grail_parse -value parse_pos_lemma -command {update_menus}
.mb.options.parser add radio -label "Grail" -variable grail_parse -value "nocontinuity parse_pos_lemma" -command {update_menus}

menu .mb.options.output
.mb.options.output add radio -label "Chart proof" -variable grail_exec -value "grail_light.pl" -command {update_menus}
.mb.options.output add radio -label "Natural deduction proof" -variable grail_exec -value "grail_light_nd.pl" -command {update_menus}

menu .mb.options.algo
.mb.options.algo add radio -label Forward-Backward -variable algo -value fwdbwd
.mb.options.algo add radio -label Greedy -variable algo -value greedy
.mb.options.algo add radio -label Noseq -variable algo -value noseq

menu .mb.options.lang
.mb.options.lang add radio -label Dutch -variable lang -value dutch -command {
    update_menus
    set grail_parse "nocontinuity parse_pos_lemma"
}
.mb.options.lang add radio -label "Dutch (detailed)" -variable lang -value dutchx -command {
    update_menus
    set grail_parse "nocontinuity parse_pos_lemma"
} 
.mb.options.lang add radio -label French -variable lang -value french -command {update_menus}
.mb.options.lang add radio -label "French (extrapolated)" -variable lang -value frenchx -command {update_menus}

menu .mb.options.pos
.mb.options.pos add radio -label "Simple" -variable postagset -value simple -command {update_models} 
.mb.options.pos add radio -label "TreeTagger" -variable postagset -value tt -command {update_models} 
.mb.options.pos add radio -label "MElt" -variable postagset -value mel -command {update_models} 
.mb.options.pos add radio -label "Merged" -variable postagset -value merged -command {update_models} 
.mb.options.pos add radio -label "Detailed" -variable postagset -value detailed -command {update_models} 
.mb.options.pos add radio -label "Reduced" -variable postagset -value reduced -command {update_models}

menu .mb.options.sem
.mb.options.sem add radio -label "Discourte Representation Theory (DRT)" -variable semantics -value drt 
.mb.options.sem add radio -label "Montegovian Dynamics" -variable semantics -value dynamics

menu .mb.options.beta
.mb.options.beta add radio -label 1 -variable beta
.mb.options.beta add radio -label 0.1 -variable beta
.mb.options.beta add radio -label 0.05 -variable beta
.mb.options.beta add radio -label 0.01 -variable beta
.mb.options.beta add radio -label 0.005 -variable beta
.mb.options.beta add radio -label 0.001 -variable beta
menu .mb.options.link
.mb.options.link add radio -label auto -variable link
.mb.options.link add radio -label partial -variable link
.mb.options.link add radio -label kbest -variable link
.mb.options.link add radio -label xpce -variable link
menu .mb.options.par
.mb.options.par add radio -label auto -variable par
.mb.options.par add radio -label xpce -variable par
menu .mb.options.debug
.mb.options.debug add radio -label Off -variable debug -value off -command {set debugstring ""}
.mb.options.debug add radio -label "Show failed linkings" -variable debug -value links -command {set debugstring "noregin nozeroheap"}

menu .mb.colors
.mb.colors add command -label "Restore defaults: color" -command { set_default_colors 0}
.mb.colors add command -label "Restore defaults: black & white" -command { set_default_colors 1}
.mb.colors add separator
.mb.colors add command -label "Supertag confidence color" -command {
    set result [tk_chooseColor -initialcolor $st_conf_color -title "Select color for supertag confidence"]
    if {$result != ""} { 
	set st_conf_color $result
    }
}
.mb.colors add command -label "Supertag background color" -command {
    set result [tk_chooseColor -initialcolor $st_other_color -title "Select color for supertag background"]
    if {$result != ""} { 
	set st_other_color $result
    }
}
.mb.colors add command -label "POS-tag confidence color" -command {
    set result [tk_chooseColor -initialcolor $pos_conf_color -title "Select color for POS-tag confidence"]
    if {$result != ""} { 
	set pos_conf_color $result
    }
}
.mb.colors add command -label "POS-tag background color" -command {
    set result [tk_chooseColor -initialcolor $pos_other_color -title "Select color for POS-tag background"]
    if {$result != ""} { 
	set pos_other_color $result
    }
}

menu .mb.help -tearoff 0
.mb.help add command -label "About the supertagger"


bind .e <Return> {
   set sent_values [linsert $sent_values 0 $phrase]
   set sent_values [lrange $sent_values 0 31]
   .e configure -values $sent_values

    supertag $phrase
}
bind . <Control-l> { 
    set phrase "" 
}
bind . <Control-k> { 
    set phrase [capitalize [iso8859 [string trim $phrase]]] 
}
bind . <Control-u> { 
    set phrase [uncapitalize [iso8859 [string trim $phrase]]] 
}
bind . <Control-s> { 
    set phrase [iso8859 [string trim $phrase]] 
}

bind . <Control-o> {

    latex_proofs

}

bind . <Command-o> {

    latex_proofs

}

bind . <Control-p> {

    catch { exec open "$tmp_dir/semantics.pdf" }

}

bind . <Command-p> {

    catch { exec open "$tmp_dir/semantics.pdf" }

}

update_menus
