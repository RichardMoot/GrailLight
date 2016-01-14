#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"

set auto_path [linsert $auto_path 0 /Users/moot/checkout/monde/]

package provide app-treebank_annotator 1.0

package require Mk4tcl
package require Tablelist
package require combobox 2.3
package require tokenize 1.0
package require parseform 1.0

catch {namespace import combobox::*}

set maxrow 0
set editing 0
set postags [lsort -dictionary [list NC-NOM P-PRP PONCT-PUN DET-DET:ART ADJ-ADJ ADV-ADV V-VER:pres NPP-NAM P+D-PRP:det VPP-VER:pper CC-KON VINF-VER:infi DET-NUM PONCT-PUN:cit CLS-PRO:PER ADJ-NUM PROREL-PRO:REL CS-KON DET-DET:POS CLR-PRO:PER NC-SYM NPP-ABR DET-PRO:DEM V-VER:impf NC-NUM DET-PRP:det VPR-VER:ppre NC-ABR CLO-PRO:PER V-VER:futu NC-NAM V-VER:cond PRO-PRO:DEM DET-PRO:IND CLS-PRO:DEM PRO-NUM PRO-PRO:IND DET-PRP ADV-KON VS-VER:subp PRO-PRO:PER PREF-ADV V-VER:simp ET-NOM CS-ADV PONCT-SYM ADJ-PRO:IND ADVWH-ADV VIMP-VER:impe PROWH-PRO:REL CC-ADV P+PRO-PRO:REL ET-PRP PRO-PRO ADV-PRO:IND ADV-ABR ADJWH-PRO:REL ET-ADJ I-INT ET-KON ET-ADV DETWH-PRO:REL ADVWH-PRO:REL PRO-PRO:POS ET-ABR DET-ADV ET-VER:infi ET-DET:ART ADVWH-KON ET-VER:pres ET-PRP:det ]]
set netags [lsort -dictionary [list I-MON I-ORG I-PER I-LOC I-TIM I-PCT O]]
set pos_beta          0.1
set super_beta        0.01
set data_dir          [file normalize "~/Library/TreebankAnnotator"]
set tmp_dir           $data_dir
set exec_dir          [file normalize "."]

set lefff_prefix      [file normalize ".."]
set load_dir          [file normalize "."]

set tagger_prefix     "/Users/moot/Corpus/WSJ/candc-1.00/bin"

set resources_prefix  [file normalize "./Resources"]
set model_prefix      [file normalize "../models"]

set pos_model         "$model_prefix/french_pos_merged"
set super_model       "$model_prefix/french_bootstrap"
set ner_model         "$model_prefix/ner"
set pos_cmd           "$tagger_prefix/mpos"
set super_cmd         "$tagger_prefix/msuper"
set ner_cmd           "$tagger_prefix/ner"

set grail_prefix       /Users/moot/grailexec/bin
set grail_cmd          "$grail_prefix/g3"

set grammar_prefix /Users/moot/checkout/Grail/grammars
set link xpce
set par xpce

if {![file exists $data_dir]} {
    file mkdir $data_dir
}

set syms(0) ""
set syms(1) "."
set syms(2) ".."
set syms(3) "..."

mk::file open lefff $lefff_prefix/lefff.db -readonly
set morph [mk::view layout lefff.morph "wordpos lemma features"]

proc annotate_dialog {} {

    toplevel .anno
    wm title .anno "About Annotate..."
    label .anno.i -image logo
    button .anno.but -text "Ok" -command { destroy .anno }
#    label .anno.bot
    pack .anno.but -side bottom
#    pack .anno.bot -side bottom
    pack .anno.i -side top

    label .anno.t -text "Corpus annotation script 1.0" -anchor w
    label .anno.t2 -text "© 2010-2016 Richard Moot" -anchor w
    label .anno.t3 -text "© 2010-2016 CNRS" -anchor w
    label .anno.t4 -text "© 2010-2011 INRIA" -anchor w

    pack .anno.t .anno.t2 .anno.t3 .anno.t4 -side top -fill x -padx 4

}

proc help_window {} {

    toplevel .help
    wm title .help "Treebank Annotation Help"

    text .help.t -state disabled
    .help.t insert end "This is not very helpful yet."

    pack .help.t -expand 1 -fill both

}

proc editStartCmd {tbl row col text} {

    global table postags netags editing

    set item $table($row)
    set poslist [lindex $item 0]
    set superlist [lindex $item 1]
    set nelist [lindex $item 2]

    set w [$tbl editwinpath]
    switch [$tbl columncget $col -name] {
	pos {
	    set rest $postags
	    foreach i $poslist {
		$w list insert end $i
		set del [lsearch -sorted -dictionary $rest $i]
		lreplace $rest $del $del
	    }
	    $w list insert end "---"
	    foreach i $rest {
		$w list insert end $i
	    }
	}
        ne {
	    set rest $netags
	    foreach i $nelist {
		$w list insert end $i
		set del [lsearch -sorted -dictionary $rest $i]
		lreplace $rest $del $del
	    }
	    $w list insert end "---"
	    foreach i $rest {
		$w list insert end $i
	    }
	}
	super {
	    foreach i $superlist {
		$w list insert end $i
	    }
	}
	word {
	    bind $w <<Paste>> {
		if {[catch {clipboard get}]} {
		    bell
		} else {
		    set text [clipboard get]
		}
	    }
	}
    }
    set editing 1

    return $text
}

proc new_tokenize_words {string} {

    return $string

}

proc editEndCmd {tbl row col text} {

    global formulas table maxrow editing

    $tbl cellconfigure $row,$col -text $text

    set w [$tbl editwinpath]
    set editing 0
    switch [$tbl columncget $col -name] {
	word {
	    set textlist [split [new_tokenize_words $text]]
	    set morewords [lrange $textlist 1 end]
	    set mlen [llength $morewords]
	    set mr1 [expr $maxrow -1]
	    if {[string equal $text ""]} {
		$tbl delete $row $row
		for {set i $row} {$i < $mr1} {incr i} {
		    set table($i) $table([expr $i+1])
		}
		incr maxrow -1
		unset table($maxrow)
		update_superbutton
	    } elseif {$mlen > 0} {
		for {set i [expr $mr1+$mlen]} {$i >= [expr $row+$mlen]} {incr i -1} {
		    set table($i) $table([expr $i-$mlen])
		}
		set ip [expr $row + 1]
		foreach mw $morewords {
		    $tbl insert $ip  [list $mw "" ""]
		    set table($ip) [list {} {} {}]
		    incr ip
		}
		incr maxrow $mlen
		.super configure -state disabled
		.ner configure -state disabled
	    }
	    return [new_tokenize_words [lindex $textlist 0]]
	}
	ne {
	    if {[string equal $text "---"]}  {
		$tbl rejectinput
		return ""
	    } else {
		set w [lindex $table($row) 0]
		set super [lindex $table($row) 1]
		set table($row) [list $w $super $text]
		return $text
	    }
	}
	pos {
	    if {[string equal $text "---"]}  {
		$tbl rejectinput
		return ""
	    } else {
		set super [lindex $table($row) 1]
		set ne [lindex $table($row) 2]
		set table($row) [list $text $super $ne]
		update_superbutton
		return $text
	    }
	}
	super {
	    .msg configure -text ""
	    if {$text == ""} {
		return $text
	    } else {
		if {[catch { set form_msg $formulas($text) }]} {
		    parseform $text
		    set parse_msg [lindex [.msg configure -text] end]
		    if {[string equal $parse_msg ""]} {
			set form_msg "New formula: $text"
		    } else {
			set form_msg "$parse_msg - New formula: $text"
		    }
		    set formulas($text) 1
		} else {
		    incr formulas($text)
		    set form_msg "$formulas($text) $text"
		}
		.msg configure -text $form_msg
		updatesent
		return $text
	    }
	}
    }
}

proc grail_parse {} {

    global sent cursent model_prefix

    set fh [open "ta_parse.pl" w]
    puts $fh [translate_export $fh 1 $sent($cursent)]
	
    if {[catch {exec $model_prefix/grail_light.pl ta_parse.pl} gl_msg]} {
	puts stderr $gl_msg
    }
    puts stderr "DONE!"

}

proc backslash_interpunction {word} {

    regsub -all "\'" $word "\\\'" word

    return $word

}

proc print_table {} {

    global table maxrow

    for {set i 0} {$i < $maxrow} {incr i} {
	puts stderr $table($i)
    }

}

proc print_sent {} {

    global sent maxsent

    for {set i 0} {$i < $maxsent} {incr i} {
	puts stderr $sent($i)
    }

}


proc pos_tag {} {

    global cursent sent pos_cmd pos_model pos_beta data_dir

    updatesent

    set inlist {}

    set infile "$data_dir/pos_in.txt"
    set outfile "$data_dir/pos_out.txt"

    set list [split $sent($cursent)]
    foreach i $list {
	set ilist [split $i "|"]
	lappend inlist [lindex $ilist 0]
    }

    set fh [open $infile w]
    puts $fh [join $inlist]
    close $fh
    catch { exec [file normalize $pos_cmd] --model [file normalize $pos_model] --ofmt "%w|%P \n" --beta $pos_beta --input [file normalize $infile]  --output [file normalize $outfile] }
    if {[file exists $outfile] && [file mtime $outfile] >= [file mtime $infile]} {
	set fh [open $outfile r]
	gets $fh line
	close $fh
    
	set sent($cursent) $line
	fill_table $line
    } else {
	.msg configure -text "Error: No POS tagger output!"
    }
}

proc super_tag {} {

    global cursent sent super_cmd super_model super_beta data_dir

    updatesent

    set inlist {}
    set infile "$data_dir/super_in.txt"
    set outfile "$data_dir/super_out.txt"

    set list [split $sent($cursent)]
    foreach i $list {
	set ilist [split $i "|"]
	lappend inlist "[lindex $ilist 0]|[lindex $ilist 1]"
    }

    set fh [open $infile w]
    puts $fh [join $inlist]
    close $fh
    catch {exec [file normalize $super_cmd] --model [file normalize $super_model] --ifmt "%w|%p \n" --ofmt "%w|%p|%S \n" --beta $super_beta --input [file normalize $infile] --output [file normalize $outfile]}
    if {[file exists $outfile] && [file mtime $outfile] >= [file mtime $infile]} {
	set fh [open $outfile r]
	gets $fh line
	close $fh
    
	set sent($cursent) $line
	fill_table $line
    } else {
	.msg configure -text "Error: No supertagger output!"
    }

}

proc ner_tag {} {

    global cursent sent ner_cmd ner_model data_dir super_beta

    updatesent

    set inlist {}
    set infile "$data_dir/ner_in.txt"
    set outfile "$data_dir/ner_out.txt"

    set list [split $sent($cursent)]
    foreach i $list {
	set ilist [split $i "|"]
	lappend inlist "[lindex $ilist 0]|[lindex $ilist 1]|[lindex $ilist 2]"
    }

    set fh [open $infile w]
    puts $fh [join $inlist]
    close $fh
    catch {exec [file normalize $ner_cmd] --model [file normalize $ner_model] --ifmt "%w|%p|%s \n" --ofmt "%w|%p|%s|%n \n" --input [file normalize $infile] --output [file normalize $outfile]}
    if {[file exists $outfile] && [file mtime $outfile] >= [file mtime $infile]} {
	set fh [open $outfile r]
	gets $fh line
	close $fh
    
	set sent($cursent) $line
	fill_table $line
    } else {
	.msg configure -text "Error: No NER tagger output!"
    }


}

proc load_file {} {

    global currentfile maxsent sent load_dir cursent

    updatesent
    set newfile [tk_getOpenFile -defaultextension ".txt" -initialdir $load_dir]
    if {$newfile != ""} {
	set currentfile $newfile
	read_file $currentfile
	
	set cursent 0
	set entrysent 0

	if {$maxsent >= $cursent} {
	    fill_table $sent($cursent)
	}
	update_arrows

    }

}

proc new_file_name {flist numsep ext} {

    global previous_file

    set previous_file ""
    set outfile "[join $flist $numsep].$ext"

    if {[file exists $outfile]} {
	set previous_file $outfile
	set num 0
	lappend flist $num
	set outfile "[join $flist $numsep].$ext"
	while {[file exists $outfile]} {
	    set previous_file $outfile
	    incr num
	    set flist [lreplace $flist end end $num]
	    set outfile "[join $flist $numsep].$ext"
	    if {[string equal $outfile $previous_file]} {
		puts stderr "Error!"
		exit 1
	    }
	}
    }

    return $outfile
}

proc save_sent {} {

    global sent cursent previous_file data_dir

    updatesent
    set outfile [new_file_name [list "$data_dir/sent$cursent"] "_" "out"]
    set of [open $outfile w]
    puts $of $sent($cursent)
    close $of
    if {[catch {exec diff $previous_file $outfile}]} {
	.msg configure -text "Saved [file tail $outfile]"
    } else {
	file delete $outfile
    }

}

proc save_file {} {

    global sent cursent currentfile

    updatesent
    set root [file rootname $currentfile]
    set outfile [new_file_name [list $root 0 $cursent] "_" "out"] 

    set of [open $outfile w]
    for {set i 0} {$i <= $cursent} {incr i} {
	puts $of $sent($i)
    }
    close $of
    .msg configure -text "Saved [file tail $outfile]"
    if {$cursent > 0} {
	set prevbody [join [list $root 0 [expr $cursent-1]] "_"]
	set prevfile "$prevbody.out"
	if {[file exists $prevfile]} {
	    file delete -force $prevfile
	}
    }
}

proc save_file_name {} {

    global sent cursent

    updatesent

    set filetypes {{{Annotation output files} {.out}} {{Text files} {.txt}} {{All files} {*}}}

    set of [tk_getSaveFile -message "Enter file name" -defaultextension ".out" -filetypes $filetypes]
    if {$of != ""} {
	set fh [open $of w]
	for {set i 0} {$i <= $cursent} {incr i} {
	    puts $fh $sent($i)
	}
	close $fh
	.msg configure -text "Saved [file tail $of]"
    } else {
	.msg configure -text "Save cancelled"
    }

}

proc export_file_name {} {

    global sent maxsent syms

    updatesent

    set filetypes {{{Prolog files} {.pl}} {{Text files} {.txt}} {{All files} {*}}}

    set of [tk_getSaveFile -message "Enter file name" -defaultextension ".pl" -filetypes $filetypes]
    if {$of != ""} {
	set fh [open $of w]
	for {set i 0} {$i <= $maxsent} {incr i} {
	    set rem [expr $i % 4]
	    .msg configure -text "Exporting$syms($rem)"
	    update idletasks
	    puts $fh [translate_export $fh [expr $i + 1] $sent($i)]
	}

	close $fh
	.msg configure -text "Exported [file tail $of]"    
    } else {
	.msg configure -text "Export cancelled"
    }

}

proc get_lemma {word pos} {

    global morph prep_lem

    if {[string equal $word ""]} {
	return ""
    }
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

    return $lemma
}

proc translate_export {fh sentno string} {

    set list [split $string]
    set sentence ""
    
    foreach i $list {
	set item [split $i "|"]
	set word [lindex $item 0]
	if {[llength $item] < 3} {
	    return ""
	}
	set sentence "$sentence $word"
    }

    puts $fh "sent($sentno, A) :- "
    puts $fh "      prob_parse("
    puts $fh "                 \["
    puts -nonewline $fh "                   "

    set max [llength $list]
    set j 0
    foreach i $list {
	set item [split $i "|"]
	set word [lindex $item 0]
	set pos [lindex $item 1]
	set lpos [string tolower $pos]
	set posl [split $pos "-"]
	set ttpos [lindex $posl 1]
	set form [lindex $item 2]
	set lemma [get_lemma $word $ttpos]
	incr j
	if {$j < $max} {
	    set end ", \n                   "
	} else {
	    set end "\n"
	}
	puts -nonewline $fh "si('[backslash_interpunction $word]', $lpos, '[backslash_interpunction $lemma]', \[$form-1\])$end"
    }
    puts $fh "                 \], A)."
    

}

proc backslash_interpunction {word} {

     return [regsub -all "\'" $word "\\\'"]

}

proc cleanup {} {

    global data_dir

    set current [pwd]
    cd $data_dir

    set outfiles [glob -nocomplain *.out]
    if {$outfiles != {}} {
	set cmd "tar cvfz bootstrap.tgz $outfiles"
	puts stderr $cmd
	catch {exec $cmd}
    }
    foreach filename $outfiles {
	file delete -force $filename
    }
    cd $current


}

proc save_all {} {

    global sent maxsent currentfile

    updatesent
    set num 0
    set root [file rootname $currentfile]
    set outfile [new_file_name [list $root] "_" "out"]

    if {[file exists $outfile]} {
	set outfile "$currentfile.out$num"
	while {[file exists $outfile]} {
	    incr num
	    set outfile "$currentfile.out$num"
	}
    }

    set of [open $outfile w]
    for {set i 0} {$i <= $maxsent} {incr i} {
	puts $of $sent($i)
    }
    close $of
    .msg configure -text "Saved [file tail $outfile]"
}

proc updatesent {} {

    global sent cursent maxrow

    set line ""
    for {set i 0} {$i < $maxrow} {incr i} {
	set word [lindex [.table cellconfigure $i,0 -text] end]
	set pos [lindex [.table cellconfigure $i,1 -text] end]
	set super [lindex [.table cellconfigure $i,2 -text] end]
	set ne [lindex [.table cellconfigure $i,3 -text] end]
	if {[string length $super] == 0} {
	    set line "$line $word|$pos"
	} elseif {[string length $ne] == 0} {
	    set line "$line $word|$pos|$super"
	} else {
	    set line "$line $word|$pos|$super|$ne"
	}
    }
    set out [string trim $line]
    set sent($cursent) $out

    return $out
}

proc update_superbutton {} {

    global maxrow table

    if {$maxrow < 1} {
	set super 0
    } else {
	set super 1
	for {set i 0} {$i < $maxrow} {incr i} {
	    set super [expr $super && ([llength [lindex $table($i) 0]] >= 1)]
	}
    }
    if {$super == 0} {
	.super configure -state disabled
	.ner configure -state disabled
    } else {
	.super configure -state normal
	.ner configure -state normal
    }

}

proc new_sent {string} {

    global cursent sent maxsent

    updatesent
    save_sent
    set sents [tokenize $string]
    set list [split $sents "\n"]
    set n 0
    foreach j $list {
	incr maxsent
	incr cursent
	for {set i $maxsent} {$i >= $cursent} {incr i -1} {
	    set sent($i) $sent([expr $i-1])
	}
	set sent($cursent) $j
    }
    print_sent
    fill_table $sent($cursent)
    update_arrows
    updatesent
    save_sent

}

proc delete_sent {} {

    global cursent sent maxsent
    
    if {$maxsent == 0} {
	set cursent 0
	set entrysent 0
	set sent(0) "."
    } elseif {$maxsent > 0} {
	incr maxsent -1
	for {set i $cursent} {$i <= $maxsent} {incr i} {
	    set sent($i) $sent([expr $i+1])
	}
    }

    fill_table $sent($cursent)
    update_arrows
    updatesent
    save_sent

}


proc copy_cmd {} {

    global sent cursent

    clipboard clear
    clipboard append $sent($cursent)

}

proc paste_cmd {} {

    if {[catch {clipboard get}]} {
	bell
    } else {
	new_sent [clipboard get]
    }
}

proc cut_cmd {} {

    global sent cursent

    clipboard clear
    clipboard append $sent($cursent)
    delete_sent

}

proc read_formulas {} {

    global formulas resources_prefix

    set form_file "$resources_prefix/formulas.txt"

    catch {unset formulas}
    if {[file exists $form_file] && [file readable $form_file]} {

	set fh [open $form_file r]
	while {[gets $fh line] > 0} {
	    set line [string trim $line]
	    set list [split $line]
	    set num [lindex $list 0]
	    set form [lindex $list 1]
	    if {![string equal $form "formulas_raw.txt"]} {
		set formulas($form) $num
	    }
	}
    }

}

proc prev {} {

    global cursent maxsent sent entrysent

    if {$cursent > 0} {
	updatesent
	save_sent
	incr cursent -1
	set entrysent $cursent
	set in $sent($cursent)
	fill_table $in
	update_arrows
    }

}

proc next {} {

    global cursent maxsent sent entrysent
    
    if {$cursent < $maxsent} {
	updatesent
	save_sent
	save_file
	incr cursent
	set entrysent $cursent
	set in $sent($cursent)
	fill_table $in
	update_arrows
    }


}

proc goto_first_sent {} {

    global cursent maxsent sent entrysent
    
    if {$cursent != 0} {
	updatesent
	save_sent
	save_file
	set cursent 0
	set entrysent 0
	set in $sent($cursent)
	fill_table $in
	update_arrows
    }

}

proc goto_last_sent {} {

    global cursent maxsent sent entrysent
    
    if {$cursent != $maxsent} {
	updatesent
	save_sent
	save_file
	set cursent $maxsent
	set entrysent $maxsent
	set in $sent($cursent)
	fill_table $in
	update_arrows
    }

}


proc update_arrows {} {

    global cursent maxsent table maxrow

    if {$cursent >= $maxsent} {
	.n configure -image nonextimg
	.n configure -state disabled
    } else {
	.n configure -image nextimg
	.n configure -state normal
    }
    if {$cursent <= 0} {
	.p configure -image noprevimg
	.p configure -state disabled
    } else {
	.p configure -image previmg
	.p configure -state normal
    }

}

proc fill_table {in} {

    global table maxrow pos_valuelist postags

    catch {unset table}
    set list [split $in]
    set maxrow 0

    .table delete 0 end
    .table columnconfigure 0 -name word -editable yes
    .table columnconfigure 1 -name pos -editable yes -editwindow combobox -maxwidth 8
    .table columnconfigure 2 -name super -editable yes -editwindow combobox
    .table columnconfigure 3 -name ne -editable yes -editwindow combobox

    set super_b normal
    foreach i $list {
	set ilist [split $i "|"]
	set valuelist {}
	set pos_valuelist {}
	if {[string is integer -strict [lindex $ilist 1]]} {
	    # no supertags and multiple part-of-speech tags
	    set default ""
	    set nedefault ""
	    set posdefault [lindex $ilist 2]
	    for {set j 2} {$j < [llength $ilist]} {incr j +2} {
		lappend pos_valuelist [lindex $ilist $j]
	    }
	} elseif {[llength $ilist] < 5} {
	    # just a single POS tag or a single POS tag and a single supertag
	    set posdefault [lindex $ilist 1]
	    set default [lindex $ilist 2]
	    set nedefault [lindex $ilist 3]
	    set valuelist [list $default]
	    if {[string equal $posdefault ""]} {
		set super_b disabled
	    }
	    set pos_valuelist $postags
	} else {
	    # multiple supertags
	    set posdefault [lindex $ilist 1]
	    set default [lindex $ilist 3]
	    set nedefault [lindex $ilist end]
	    set pos_valuelist $postags
	    for {set j 3} {$j < [expr [llength $ilist]-1]} {incr j +2} {
		lappend valuelist [lindex $ilist $j]
	    }
	}

	if {[string is double $nedefault]} {
	    set nedefault ""
	}
	.table insert end [list [lindex $ilist 0] $posdefault $default $nedefault]
	set table($maxrow) [list $pos_valuelist $valuelist]
	incr maxrow
	
    }

    .super configure -state $super_b
    .ner configure -state $super_b

}
proc get_pos_dir {} {

    global pos_model

    set dir [tk_chooseDirectory -initialdir $pos_model -mustexist true -title "Select a POS model directory"]

    if {$dir eq ""} { 
	return 
    } 

    if {[check_existing_files $dir [list attributes classes config contexts features info lexicon number_unknowns tagdict unknowns weights]] == 0} {
	set pos_model $dir
    }

}

proc get_st_dir {} {

    global st_model lang

    set dir [tk_chooseDirectory -initialdir $st_model($lang) -mustexist true -title "Select a supertag model directory"]

    if {$dir eq ""} { 
	return 
    } 

    if {[check_existing_files $dir [list attributes classes config contexts features info lexicon posdict postags tagdict unknowns weights]] == 0} {
	set pos_model $dir
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

proc read_file {filename} {

    global sent maxsent load_dir

    if {[file exists $filename]} {

	if {[file readable $filename]} {
	    catch {unset sent}
	    set maxsent -1

	    set fh [open $filename r]
	    while {[gets $fh line] >= 0} {
		incr maxsent
		regsub -all "  " $line " " line
		set sent($maxsent) [string trim $line]
	    }
	    
	    close $fh
	    set load_dir [file dirname [file normalize $filename]]
	    .msg configure -text "Read [file tail $filename]"
	} else {
	    .msg configure -text "Couldn't read [file tail $filename]"
	}
    } else {
	.msg configure -text "Couldn't find [file tail $filename]"
    }
}

tablelist::addOakleyCombobox
tablelist::tablelist .table -columns {0 "Word" 0 "POS tag" 0 "Supertag" 0 "NE"} -relief flat -labelrelief flat -activestyle frame -editstartcommand editStartCmd -editendcommand editEndCmd -forceeditendcommand 1 -stripebackground #e0e8f0 -showseparators yes -background gray98 -stretch all -yscrollcommand [list .fr.sc set]

frame .f -background #FFFFFF -borderwidth 0


image create photo plus -file [file join $resources_prefix "plus26.gif"]
image create photo previmg -file [file join $resources_prefix "prev.gif"]
image create photo nextimg -file [file join $resources_prefix "next.gif"]
image create photo noprevimg -file [file join $resources_prefix "noprev.gif"]
image create photo nonextimg -file [file join $resources_prefix "nonext.gif"]
image create photo logo -file [file join $resources_prefix "Tree-256x256.gif"]


#set ::tk::mac::iconBitmap $logo

menu .mb -tearoff 0

. configure -menu .mb

.mb add cascade -menu .mb.file -label File
.mb add cascade -menu .mb.edit -label Edit
.mb add cascade -menu .mb.options -label Options -menu .mb.options
.mb add cascade -menu .mb.help -label Help -menu .mb.help

menu .mb.help -tearoff 0
.mb.help add command -label "About the Treebank annotator" -command {help_window}

menu .mb.file -tearoff 0
.mb.file add command -label "About..." -command {annotate_dialog}
.mb.file add separator
.mb.file add command -label "Load file..." -accelerator "⌘L" -command {load_file}
.mb.file add command -label "Save file" -accelerator "⌘S" -command {save_file}
.mb.file add command -label "Save file as..." -accelerator "⌘⇧S" -command {save_file_name}
.mb.file add separator
.mb.file add command -label "Export file..." -accelerator "⌘E" -command {export_file_name}
.mb.file add separator
.mb.file add command -label "Quit" -accelerator "⌘Q" -command {destroy .}

menu .mb.edit -tearoff 0
.mb.edit add command -label "Cut" -accelerator "⌘X" -command {cut_cmd}
.mb.edit add command -label "Copy" -accelerator "⌘C" -command {copy_cmd}
.mb.edit add command -label "Paste" -accelerator "⌘V" -command {paste_cmd}
.mb.edit add command -label "Delete" -accelerator "⌘D" -command {delete_sent}


menu .mb.options -tearoff 0
.mb.options add command -label "Change POS model directory..." -command {get_pos_dir}
.mb.options add cascade -label "POS Beta" -menu .mb.options.posbeta
.mb.options add separator
.mb.options add command -label "Change supertag model directory..." -command {get_st_dir}
.mb.options add cascade -label "Super Beta" -menu .mb.options.superbeta

menu .mb.options.posbeta
.mb.options.posbeta add radio -label 1 -variable pos_beta
.mb.options.posbeta add radio -label 0.1 -variable pos_beta
.mb.options.posbeta add radio -label 0.05 -variable pos_beta
.mb.options.posbeta add radio -label 0.01 -variable pos_beta
.mb.options.posbeta add radio -label 0.005 -variable pos_beta
.mb.options.posbeta add radio -label 0.001 -variable pos_beta

menu .mb.options.superbeta
.mb.options.superbeta add radio -label 1 -variable super_beta
.mb.options.superbeta add radio -label 0.1 -variable super_beta
.mb.options.superbeta add radio -label 0.05 -variable super_beta
.mb.options.superbeta add radio -label 0.01 -variable super_beta
.mb.options.superbeta add radio -label 0.005 -variable super_beta
.mb.options.superbeta add radio -label 0.001 -variable super_beta


button .p -image noprevimg -width 26 -height 23 -padx 0 -pady 0 -command {prev} 
button .n -image nonextimg -width 26 -height 23 -padx 0 -pady 0 -command {next}
.p configure -state disabled
.n configure -state disabled

button .plus -image plus -command {new_sent "."}
label .filler
label .filler2

button .pos -text "POS" -command {pos_tag}
button .super -text "Super" -command {super_tag}
button .ner -text "NER" -command {ner_tag}

pack .p -in .f -side left
pack .n -in .f -side left
pack .filler -in .f -side left
pack .plus -in .f -side left
pack .filler2 -in .f -side left

pack .ner -in .f -side right
pack .super -in .f -side right
pack .pos -in .f -side right

pack .f -fill x

frame .bot
label .msg -font "Helvetia-o-normal--7-*" -bg white -justify left -anchor w
entry .entry -textvariable entrysent -width 3 -relief flat
pack .bot -side bottom -fill x
pack .entry -side left -anchor e -in .bot
pack .msg -side left -anchor w -fill x -expand true -in .bot

wm title . "Annotate"

frame .fr -borderwidth 0
scrollbar .fr.sc -orient vertical -command [list .table yview] -elementborderwidth 1
label .fr.corner -background white
pack .fr.corner -side top -fill x
pack .fr.sc -side top -fill y -expand 1
pack .fr -side right -fill y 
pack .table -fill both -expand true -side left

.table columnconfigure 0 -name text -editable yes -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 1 -name pos -editable yes -editwindow combobox -labelbackground gray80 -maxwidth 8 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 2 -name super -editable yes -editwindow combobox -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 3 -name ne -editable yes -editwindow combobox -labelbackground gray80 -labelborderwidth 2 -labelrelief groove

if {$argc > 0} {
    set currentfile [lindex $argv 0]
    read_file $currentfile
    set cursent 0
    if {$maxsent >= $cursent} {
	fill_table $sent($cursent)
    } else {
	set sent(0) "."
	set maxsent 0
	set cursent 0
	fill_table $sent($cursent)
    }
} else {
    set currentfile ""
    set sent(0) "."
    set maxsent 0
    set cursent 0
    fill_table $sent($cursent)
}

cleanup
update_arrows
read_formulas
set entrysent $cursent

bind . <Control-s> { save_sent }
bind . <Command-s> { save_file }
bind . <Command-d> { delete_sent }
bind . <Command-g> { grail_parse }
bind . <Command-Shift-s> { save_file_name }
bind . <Command-l> { load_file }
bind . <Command-q> { destroy . }
bind . <Command-p> {

    catch { exec open "$model_dir/semantics.pdf" }

}
bind . <Key-Prior> { prev }
bind . <Key-Next> { next }
bind . <Key-End> { goto_last_sent }
bind . <Key-Home> { goto_first_sent }
bind .entry <Key-Return> {

    if {![string is integer -strict $entrysent]} {
	set entrysent 0
    }
    if {$entrysent >= $maxsent} {
	set entrysent $maxsent
    }
    if {$entrysent < 0} {
	set entrysent $maxsent
    }
    set cursent $entrysent
    fill_table $sent($cursent)
    update_arrows

}

event add <<Paste>> <Command-v> <Control-y>
event add <<Cut>> <Command-x> <Control-k>
event add <<Copy>> <Command-c>

bind . <<Copy>> {
    if {$editing == 0} {
	copy_cmd
    }
}

bind . <<Paste>> {
    
    if {$editing == 0} {
	paste_cmd
    }

}

bind . <<Cut>> {

    if {$editing == 0} {
	cut_cmd
    }

}

wm focus .
