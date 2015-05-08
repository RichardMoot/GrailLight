#!/usr/bin/tclsh

if {$argc == 0} {

    set flist [list "maxentdata.txt"]

} else {

    set flist $argv

}

set sent 0

foreach f $flist {

    set fs [open $f r]

    while {[gets $fs line] >= 0} {
	if {[string equal -length 1 $line "#"]} {
	    puts "$sent [string range $line 2 end]"
	} else {
	    incr sent
	}
    }

    close $fs

}