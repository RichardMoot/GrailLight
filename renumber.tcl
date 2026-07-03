#!/usr/bin/tclsh

if {$argc == 0} {

    set flist [list "annodis.pl"]

} else {

    set flist $argv

}

set sentno 1

foreach f $flist {

#    puts -nonewline stderr "$f..."
    set fs [open $f r]

    while {[gets $fs line] >= 0} {

	if [regexp {^sent} $line] { 
	    puts "sent($sentno, Result) :-"
	    incr sentno
        } else {
	    puts $line
	}
    }
    close $fs
}
