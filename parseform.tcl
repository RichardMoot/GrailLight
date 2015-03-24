#!/usr/bin/tclsh

package provide parseform 1.0

# Package to transform Grail prolog-style formula strings into a (recursive) tcl list.
# so
#
# parseform "dr(0,dl(1,a,b),c)"
#
# returns
#
# {dr 0 {dl 1 a b} c}
#
# (normally without the outer braces). 

# parseform
#
# transform a string containing a formula into a list

proc parseform {string} {

    global res

    set string [string trim $string]

    set i [parseform1 $string 0]
    if {$i != [string length $string]} {
	.msg configure -text "Trailing material deleted! .[string range $string $i end].$res."
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
    } elseif {[string equal [string range $string $i [expr $i+5]] "lambda("]} {
	set i [parserest $string [expr $i+3]]
	set res [list lambda $index $arg1 $arg2]
    } elseif {[string equal [string range $string $i [expr $i+4]] "appl("]} {
	set i [parserest $string [expr $i+3]]
	set res [list appl $index $arg1 $arg2]
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
	    set res_end [expr $j-1]
	    set i $j
	} elseif {$k != -1} {
	    set res_end [expr $k-1]
	    set i $k
	} else {
	    set res_end [expr [string length $string]-1]
	    set i [string length $string]
	}
	if {[string is wordchar -strict -failindex fail [string range $string $begin $res_end]]} {   
	    set res [string range $string $begin $res_end]
	} else {
	    .msg configure -text "Illegal character in atom: [string range $string $begin [expr $fail-1]]*HERE*[string range $string $fail end]"
	    set res [string range $string $begin [expr $fail-1]]
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
	.msg configure -text "Missing comma: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]"
    }

}

proc parseparnc {string i} {

    if {[string equal [string range $string $i $i] ")"]} {
	return [expr $i+1]
    } else {
	.msg configure -text "Missing close parenthesis: [string range $string 0 $i]*HERE*[string range $string [expr $i+1] end]"
    }

}

proc parseindex {string i} {

    global index

    if {[set j [string first "," $string $i]] != -1} {
	set index [string range $string $i [expr $j-1]]
	return $j
    } else {
	.msg configure -text "Missing comma: $string"
    }

}

proc printform {string} {

    set l [parseform $string]

    if {[llength $l] == 1} {
	return $string
    } else {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "$s1 \\$m $s2"
	} elseif {[string equal $c "dr"]} {
	    return "$s1 /$m $s2"
	} elseif {[string equal $c "p"]} {
	    return "$s1 *$m $s2"
	}
    }
}

proc printform1 {l} {

    if {[llength $l] == 1} {
	return $l
    } elseif {[llength $l] == 3} {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform1 [lindex $l 2]]
	if {[string equal $c "dia"]} {
	    return "<>$m $s1"
	} elseif {[string equal $c "box"]} {
	    return "\[\]$m $s1"
	}

    } elseif {[llength $l] == 4} {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "($s1 \\$m $s2)"
	} elseif {[string equal $c "dr"]} {
	    return "($s1 /$m $s2)"
	} elseif {[string equal $c "p"]} {
	    return "($s1 *$m $s2)"
	}
    }
}


proc printform_alt {string} {

    set l [parseform $string]

    if {[llength $l] == 1} {
	return $string
    } else {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform_alt1 [lindex $l 2]]
	set s2 [printform_alt1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "$s1 \\\\$m $s2"
	} elseif {[string equal $c "dr"]} {
	    return "$s1 /$m $s2"
	} elseif {[string equal $c "p"]} {
	    return "$s1 *$m $s2"
	}
    }
}

proc printform_alt1 {l} {

    if {[llength $l] == 1} {
	return $l
    } elseif {[llength $l] == 3} {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform1 [lindex $l 2]]
	if {[string equal $c "dia"]} {
	    return "<>$m $s1"
	} elseif {[string equal $c "box"]} {
	    return "\[\]$m $s1"
	}

    } elseif {[llength $l] == 4} {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[string equal $m "0"]} {
	    set m ""
	}
	set s1 [printform1 [lindex $l 2]]
	set s2 [printform1 [lindex $l 3]]
	if {[string equal $c "dl"]} {
	    return "($s1 \\\\$m $s2)"
	} elseif {[string equal $c "dr"]} {
	    return "($s1 /$m $s2)"
	} elseif {[string equal $c "p"]} {
	    return "($s1 *$m $s2)"
	}
    }
}


proc writeform {l} {

    if {[llength $l] == 1} {
	return $l
    } else {
	set c [lindex $l 0]
	set m [lindex $l 1]
	if {[llength $l] == 3} {
	    set s1 [writeform [lindex $l 2]]
		return "$c\($m,$s1\)"
	} else {
	    set s1 [writeform [lindex $l 2]]
	    set s2 [writeform [lindex $l 3]]
	    return "$c\($m,$s1,$s2\)"
	}
    }
}
