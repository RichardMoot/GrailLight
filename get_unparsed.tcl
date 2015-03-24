#!/usr/bin/tclsh

# Given an output file named "unparsed" (as produced by the "chart_parse_all" command in
# "chart.pl") and a filename, this file selects exactly those lines for which no parse
# has been found.


if {$argc == 0} {

    set super [open "au_pays_des_Isards.pos" r]
    set fh [open "unparsed" r]

} elseif {$argc == 1} {

    set super [open [lindex $argv 0] r]
    set fh [open "unparsed" r]

} elseif {$argc == 2} {

    set super [open [lindex $argv 1] r]
    set fh [open [lindex $argv 0] r]

} else {

    puts stderr "Usage: get_unparsed [filterfile] [sourcefile]"

}


set lineno 0

while {[gets $fh line] >= 0} {

    if {[string equal -length 9 "unparsed(" $line]} {
	set endnum [expr [string first "," $line 10] -1]
	set linenum [string trim [string range $line 9 $endnum]]

#	puts stderr "$lineno $linenum"
	while {$lineno < $linenum} {
	    gets $super sline
	    incr lineno
	}
	puts $sline
    }


}