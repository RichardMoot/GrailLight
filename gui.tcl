#!/bin/sh
# the next line restarts using wish \
exec /usr/bin/wish "$0" "$@"

set auto_path [linsert $auto_path 0 /Users/moot/checkout/monde/]
set resources_prefix "/Users/moot/checkout/monde/Resources"

package require Tablelist
package require combobox 2.3
package require tokenize 1.0

catch {namespace import combobox::*}

set log [open "prolog_log.txt" w]
set cur_sent 0

# parseform
#
# transform a string containing a formula into a list

proc parseform {string} {

    global res

    set string [string trim $string]

    set i [parseform1 $string 0]
    if {$i != [string length $string]} {
	.answer.text configure -state normal
	.answer.text insert end "Trailing material deleted! .[string range $string $i end].$res.\n"
	.answer.text configure -state disabled
	puts stderr "$i [string length $string]"
	puts stderr "Trailing material deleted! .[string range $string $i end].$res."
    }
	return $res

}

proc parseform1 {string begin} {

    global arg1 arg2 index res

    set i $begin

    if {[string range $string $i [expr $i+2]] eq "dl("} {
	set i [parserest $string [expr $i+3]]
	set res [list dl $index $arg1 $arg2]
    } elseif {[string range $string $i [expr $i+2]] eq "dr("} {
	set i [parserest $string [expr $i+3]]
	set res [list dr $index $arg1 $arg2]
    } elseif {[string range $string $i [expr $i+1]] eq "p("} {
	set i [parserest $string [expr $i+2]]
	set res [list p $index $arg1 $arg2]
    } elseif {[string range $string $i [expr $i+3]] eq "dia("} {
	set i [parserest_u $string [expr $i+4]]
	set res [list dia $index $arg1]
    } elseif {[string range $string $i [expr $i+3]] eq "box("} {
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

    if {[string range $string $i $i] eq ","} {
	return [expr $i+1]
    } else {
	.answer.text configure -state normal
	.answer.text insert end "Missing comma: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]\n"
	.answer.text configure -state disabled
	puts stderr "Missing comma: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]"
	exit 1
    }

}

proc parseparnc {string i} {

    if {[string range $string $i $i] eq ")"} {
	return [expr $i+1]
    } else {
	.answer.text configure -state normal
	.answer.text insert end "Missing close parenthesis: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]\n"
	.answer.text configure -state disabled
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


### Formula printing

# Use utf-8 encoding for subscripts, diamonds and boxes
# (lines commented out are for plain ASCII versions)

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

set np_nom "np"
set np_acc "np"
#set np_nom "np\u207f"
#set np_nom "np\u2092"
#set np_acc "np\u1d43"

# unicode subscripts
# \u2080 _0
# \u2081 _1
# \u2090 _a
# \u2091 _e
# \u2092 _o
# \u2093 _x
# \u2095-\u209c (rarely present)
# \u1d62 _i
# \u1d63 _r
# \u1d64 _u
# \u1d65 _v
# \u1d66 _{\beta}
# \u1d67 _{\gamma}
# \u1d68 _{\rho}
# \u1d69 _{\phi}
# \u1d6a _{\xhi}

# s_x
set s_x "s\u2093"
set s_main "s" 
# s_a
set s_pass "s\u1d56\u1d43"
#set s_pass "s\u2090"
# s^p
set s_ppart "s\u1d56"
# s_r
set s_ppres "s\u1d56\u1d63"
# s_i
set s_inf "s\u2071"      
#set s_inf "s\u1d62"      
#set s_q "s_q "
#set s_whq "s

proc printatom {string} {

    global s_main s_pass s_ppart s_ppres s_inf s_x np_nom np_acc

    if {$string eq "s"} {
	return $s_x
    } elseif {$string eq "s_main"} {
	return $s_main
    } elseif {$string eq "s_inf"} {
	return $s_inf
    } elseif {$string eq "s_pass"} {
	return $s_pass
    } elseif {$string eq "s_ppart"} {
	return $s_ppart
    } elseif {$string eq "s_ppres"} {
	return $s_ppres
    } elseif {$string eq "np_nom"} {
	return $np_nom
    } elseif {$string eq "np_acc"} {
	return $np_acc
    } elseif {$string eq "*LPAR*"} {
	return "("
    } elseif {$string eq "*RPAR*"} {
	return ")"
    } elseif {$string eq "*QUOTE*"} {
	return "\""
    } else {
	return [regsub -all {\*COMMA\*} $string ","]
    }
}


proc printpros {string} {

    global bullet
    set prev $bullet
    set bullet "\u25cb"
    set out [printform $string]
    set bullet $prev

    return $out
}

proc printmode {m} {

    global mode0

    if {$m eq "0"} {
	set m $mode0
    } elseif {$m eq "1"} {
	set m "\u2081"
    } elseif {$m eq "2"} {
	set m "\u2082"
    } elseif {$m eq "3"} {
	set m "\u2083"
    } elseif {$m eq "4"} {
	set m "\u2084"
    }

    return $m

}

proc printform {string} {

    global mode0 spc bullet

    set l [parseform $string]

    if {[llength $l] == 1} {
	return [printatom $string]
    } else {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {$c eq "dl"} {
	    return "$s1$spc\\$m$spc$s2"
	} elseif {$c eq "dr"} {
	    return "$s1$spc/$m$spc$s2"
	} elseif {$c eq "p"} {
	    return "$s1$spc$bullet$m$spc$s2"
	}
    }
}

proc printform1 {l} {

    global diamond box bullet mode0 spc

    if {[llength $l] == 1} {
	return [printatom $l]
    } elseif {[llength $l] == 3} {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	if {$c eq "dia"} {
	    return "$diamond$m$s1"
	} elseif {$c eq "box"} {
	    return "$box$m$s1"
	}

    } elseif {[llength $l] == 4} {
	set c [lindex $l 0]
	set m [printmode [lindex $l 1]]
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {$c eq "dl"} {
	    return "($s1$spc\\$m$spc$s2)"
	} elseif {$c eq "dr"} {
	    return "($s1$spc/$m$spc$s2)"
	} elseif {$c eq "p"} {
	    return "($s1$spc$bullet$m$spc$s2)"
	}
    }
}

### Starts up prolog and establishes connections 
# 20130723 changed "-f $fname" to "-l $fname"; for some mysterious reason
#          loading the file with -l (rather than -s or -f) is the only
#          thing which seems to work                                   RM

proc pl_open {fname tw} { 
    set p [open "\|swipl -l $fname -g grail_gui,halt -t 'halt(1)'" r+] 
    gets $p answer 
    while {$answer != "INIT"} { 
	gets $p answer 
    } 
    $tw configure -state normal 
    $tw insert end "START\n" 
    $tw configure -state disabled 
    return $p 
}

proc pl_load {p tw file} {

    global cur_sent max_sent sent

    $tw configure -state normal 

    puts_prolog $p "load('$file').\n"

    gets $p answer
    $tw insert end "$answer\n" 
    while {$answer ne "INIT"} {
        # read a list of sentences
	if {$answer eq "SENTENCES"} {
	    set max_sent 0
	    while {$answer != "LIST END"} {
		if {[string is integer -strict $answer]} {
		    set sent($max_sent) $answer
		    incr max_sent
		}
                # read next line
		gets $p answer
		$tw insert end "$answer\n" 
	    }
	}
	if {$answer eq "CHOOSE ACTIVE"} {
	    update_active $p $tw    
	}
	gets $p answer
	$tw insert end "$answer\n" 
    }

    $tw configure -state disabled
    
    set cur_sent 0
    update_arrows

}

### Closes all the connections and quits 

proc pl_close {p tw} { 
    $tw insert end "?- quit.\n" 
    close $p 
    exit 
}

##### Define prolog interaction 
### Sends query to prolog and collects all answers 

proc pl_command {p query tw} { 
    # Display the query 
    $tw configure -state normal 
    $tw insert end "$query\.\n" 
    $tw configure -state disabled 
    # Send the query to prolog 
    puts_prolog $p "$query.\n"
    # Get the first answer if any 
    gets $p answer 
    while {$answer ne "INIT"} { 
	if {$answer eq "CHOOSE ACTIVE"} {
	    update_active $p $tw
	    gets $p answer
	} elseif {$answer eq "CHOOSE RULE"} {
	    choose_rule $p $tw
	    gets $p answer
	} else {
	    # DEFAULT CASE: add Prolog output to text widget
	    $tw configure -state normal 
	    $tw insert end "   $answer\n" 
	    $tw configure -state disabled 
	    $tw see end
	    update idletasks
	    # read further Prolog output
	    gets $p answer 
	}
    } 
    $tw configure -state normal 
    $tw insert end "\nPROLOG COMMAND END\n" 
    $tw configure -state disabled 
    $tw see end
    update idletasks
}

proc choose_rule {p tw} {

    global ruleselect choose

    unset -nocomplain ruleselect

    rule_dialog
    .rule.l delete 0 end

    set choose 0
    set item 0

    gets $p answer
    while {$answer != "LIST END"} {
	set list [split $answer]
#	set rule [split [lindex $list end] "(,)"]
	set rulename [lindex $list 0]
	set prems [lrange $list 1 end]
	set ruleselect($item) $prems
	.rule.l insert end "$item. [print_rule $rulename]"
	incr item
	gets $p answer
    }

    
    dialog_wait .rule choose .rule.l
    dialog_dismiss .rule



    .table selection clear 0 end
    .table configure -selectmode browse

    update idletasks

}

proc print_rule {s} {

    global bs1 diamond0 diamond1

    if {$s eq "dr"} {
	return "/E"
    } elseif {$s eq "dl"} {
	return "\\E"
    } elseif {$s eq "wr"} {
	return "push $bs1"
    } elseif {$s eq "wpop"} {
	return "pop $bs1"
    } elseif {$s eq "wpop_vp"} {
	return "pop(vp) $bs1"
    } elseif {$s eq "wpop_vpi"} {
	return "pop(vp\u1d62) $bs1"
    } elseif {$s eq "let"} {
	return "let"
    } elseif {$s eq "e_start"} {
	return "push $diamond1"
    } elseif {$s eq "e_end"} {
	return "pop $diamond1"
    } elseif {$s eq "e_start_l"} {
	return "push $diamond0"
    } elseif {$s eq "e_end_l"} {
	return "pop $diamond0"
    } else {
	return $s
    }
}

proc update_active {p tw} {

    # update the table with the current active items
    .table delete 0 end
    .table columnconfigure 0 -name string -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
    .table columnconfigure 1 -name formula -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
    .table columnconfigure 2 -name weight -editable no -maxwidth 5 -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
    .table columnconfigure 3 -name stack -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
    set i 0
    $tw configure -state normal 
    gets $p answer
    while {$answer != "LIST END"} {
	$tw insert end "$i. $answer\n"  
	incr i
	if {[string is integer -strict [lindex $answer 0]]} {
	    .table insert end [list [printpros [lindex $answer 4]] [printform [lindex $answer 3]] [lindex $answer 6] [lindex $answer 7]]
	}
	gets $p answer
    }
    # just read "LIST END"
    $tw see end
    
    update idletasks
}

proc puts_prolog {prolog string} {

    global log

    puts $prolog $string
    flush $prolog

    puts $log $string
    flush $log

}

# Button commands

proc prev {} {

    global cur_sent max_sent sent plfile

    if {$cur_sent > 0} {
	incr cur_sent -1
	update_arrows
	pl_command $plfile "parse($sent($cur_sent))" .answer.text

    }

}

proc next {} {

    global cur_sent max_sent sent plfile
    
    if {$cur_sent < [expr $max_sent - 1]} {
	incr cur_sent
	update_arrows
	pl_command $plfile "parse($sent($cur_sent))" .answer.text
    }

}

proc goto_first_sent {} {

    global cur_sent max_sent sent plfile
    
    if {$cur_sent != 0} {
	set cur_sent 0
	update_arrows
	pl_command $plfile "parse($sent($cur_sent))" .answer.text
    }

}

proc goto_last_sent {} {

    global cur_sent max_sent sent plfile
    
    set mm [expr $max_sent - 1]
    if {$cur_sent != $mm} {
	set cur_sent $mm
	update_arrows
	pl_command $plfile "parse($sent($cur_sent))" .answer.text
    }

}


proc update_arrows {} {

    global cur_sent max_sent

    if {$cur_sent >= [expr $max_sent - 1]} {
	.n configure -image nonextimg
	.n configure -state disabled
    } else {
	.n configure -image nextimg
	.n configure -state normal
    }
    if {$cur_sent <= 0} {
	.p configure -image noprevimg
	.p configure -state disabled
    } else {
	.p configure -image previmg
	.p configure -state normal
    }

}

# Images

image create photo plus -file [file join $resources_prefix "plus26.gif"]
image create photo previmg -file [file join $resources_prefix "prev.gif"]
image create photo nextimg -file [file join $resources_prefix "next.gif"]
image create photo noprevimg -file [file join $resources_prefix "noprev.gif"]
image create photo nonextimg -file [file join $resources_prefix "nonext.gif"]

frame .f -borderwidth 0

button .p -image noprevimg -width 26 -height 23 -padx 0 -pady 0 -command {prev} 
button .n -image nonextimg -width 26 -height 23 -padx 0 -pady 0 -command {next}
.p configure -state disabled
.n configure -state disabled

##### Define query widgets 

tablelist::addOakleyCombobox
tablelist::tablelist .table -columns {0 "String" 0 "Formula" 0 "Weight" 0 "Stacks"} -relief flat -labelrelief flat -activestyle frame \
    -stripebackground #e0e8f0 -showseparators yes -background gray98 -stretch all -yscrollcommand [list .sc set] -exportselection false
#-editstartcommand editStartCmd -editendcommand editEndCmd -forceeditendcommand 1

.table columnconfigure 0 -name string -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 1 -name formula -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 2 -name weight -editable no -maxwidth 5 -labelbackground gray80 -labelborderwidth 2 -labelrelief groove
.table columnconfigure 3 -name stacks -editable no -labelbackground gray80 -labelborderwidth 2 -labelrelief groove


set bodytag [.table bodytag]

# menu bar

menu .mb -tearoff 0
. configure -menu .mb

.mb add cascade -menu .mb.file -label File -menu .mb.file

menu .mb.file -tearoff 0
#.mb.file add command -label "About..." -command {st_dialog}
#.mb.file add separator
.mb.file add command -label "Load file" -command {
    set filename [tk_getOpenFile -typevariable {{Prolog files} {.pl} {All files} {*}}]
    if {$filename != {}} { 
	pl_load $plfile .answer.text $filename
    }
}
.mb.file add separator
.mb.file add command -label "Quit" -command {
    pl_close $plfile .answer.text
    destroy .
}

#

scrollbar .sc -orient vertical -command [list .table yview] -elementborderwidth 1

##### Define answer widgets 

toplevel .answer
#frame .answer -relief raised -bd 2 
label .answer.label -text "Log:" 
text .answer.text \
   -font -adobe-courier-bold-r-normal-*-12-*-*-*-*-*-*-* \
   -wrap word \
   -relief raised -bd 2 \
   -state disabled \
   -yscrollcommand ".answer.scroll set" 
scrollbar .answer.scroll -command ".answer.text yview"  -elementborderwidth 1

##### Show all widgets 

#pack .query -side top -fill x 
#pack .query.label1 -side left 
#pack .query.text -side left -fill x
#pack .query.label2 -side left 
#pack .query.result -side left 
#pack .query.quit -side right 
#pack .query.send -side right 

pack .answer.label -side top 
pack .answer.text -side left -fill x
pack .answer.scroll -side right -fill y 

#

pack .f -fill x
pack .p -in .f -side left
pack .n -in .f -side left

pack .sc -side right -fill y
pack .table -fill both -expand 1 -side left


# 

proc rule_dialog {} {

    global choose

    if {[dialog_create .rule "Select Rule" -borderwidth 5]} {

	listbox .rule.l -exportselection false
	pack .rule.l -fill both -expand 1

	bind .rule.l <<ListboxSelect>> {
	    if {$choose != 1} {
		selection_made %W
	    }
	}

	bind .rule.l <Double-1> {

	    set sel [.rule.l curselection]
	    if {$sel != {}} {
		set i [lindex $sel 0]
		puts stderr "choose($i)"
		pl_command $plfile "choose($i)" .answer.text
		.table selection clear 0 end
		.table configure -selectmode browse
		set choose 1
	    }
	    
	}

	bind .rule.l <Return> {

	    set sel [.rule.l curselection]
	    if {$sel != {}} {
		set i [lindex $sel 0]
		puts stderr "choose($i)"
		pl_command $plfile "choose($i)" .answer.text
		.table selection clear 0 end
		.table configure -selectmode browse
		set choose 1
	    }
	    
	}

    }
}


proc selection_made {w} {

    global ruleselect choose

    set i [lindex [$w curselection] 0]
    .table configure -selectmode multiple
    .table selection clear 0 end
    foreach index [$w curselection] {
	set list $ruleselect($index)
	set slist {}
	foreach j $list {
	    if {[string is integer -strict $j]} {
		.table selection set $j
		lappend slist $j
	    }
	}
    }
    .table configure -selectmode browse
#    $w selection set $i
    .answer.text configure -state normal
    .answer.text insert end "SELECTION $slist.\n"
    .answer.text configure -state disabled
    .answer.text see end
}

proc dialog_create {top title args} {

    global dialog

    if {[winfo exists $top]} {
	switch -- [wm state $top] {
	    normal {
		raise $top
	    }
	    withdrawn -
	    iconic {
		wm deiconify $top
		catch {wm geometry $top $dialog(geo,$top)}
	    }
	}
	return 0
    } else {
	eval {toplevel $top} $args
	wm title $top $title
	return 1
    }
}

proc dialog_wait {top varname {focus {}}} {

    upvar $varname var

    bind $top <Destroy> [list set $varname cancel]

    if {[string length $focus] == 0} {
	set focus $top
    }
    set old [focus -displayof $top]
    focus $focus
    catch {tkwait visibility $top}
    catch {grab $top}

    tkwait variable $varname
    catch {grab release $top}
    focus $old
}

proc dialog_dismiss {top} {

    global dialog
    catch {
	set dialog(geo,$top) [wm geometry $top]
	wm withdraw $top
    }

}


wm title . "Interactive Chart Parser"
wm title .answer "Logs"

# Apple-specific commands

proc ::tk::mac::OpenDocument {args} {
    foreach f $args {pl_load $plfile .answer.text $f}
}

proc ::tk::mac::Quit {} {
    pl_close $plfile .answer.text
    destroy .
}

# Bindings

# aply a rule

bind $bodytag <Return> {
    .answer.text configure -state normal
    .answer.text insert end "RETURN.\n"
    .answer.text configure -state disabled
    set list [.table curselection]
    if {$list != {}} {
	set item [lindex $list 0]
	pl_command $plfile "active($item)" .answer.text
    }

}

bind $bodytag <Double-1> {
    .answer.text configure -state normal
    .answer.text insert end "DOUBLE-1.\n"
    .answer.text configure -state disabled
    .answer.text see end
    set list [.table curselection]
    if {$list != {}} {
	.answer.text configure -state normal
	.answer.text insert end "ACTIVE: $list.\n"
	.answer.text configure -state disabled
	set item [lindex $list 0]
	pl_command $plfile "active($item)" .answer.text
    }
}

# undo

bind $bodytag <u> {

    set list [.table curselection]
    if {$list != {}} {
	set item [lindex $list 0]
	pl_command $plfile "undo($item)" .answer.text
    }

}

bind $bodytag <x> {

    pl_command $plfile "export" .answer.text
    
}

bind . <Control-Key-c> {

#    close $plfile
    exit

}

bind . <Destroy> {

    if {"%W" == "."} {
	close $plfile
    }

}

##### Setup communication with Prolog 

#CR set plfile [pl_open "grail_light_cr.pl" .answer.text] 
#LP set plfile [pl_open "grail_light.pl" .answer.text]

if {$argc > 0} {
    set argfile [lindex $argv 0]
    if {[file exists $argfile]} {
	pl_load $plfile .answer.text $argfile 
    }
}
