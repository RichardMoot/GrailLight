#!/usr/bin/tclsh



proc header {file} {
    puts $file "% -*- Mode: Prolog -*-"
    puts $file ":- multifile proof/2."
    puts $file ""
}


if {$argc == 0} {

    set flist [list "proofs.pl"]

} else {

    set flist $argv

}

set sentno 0
set batchno 1
set size 1

set outfile [format "proof%05d.pl" $batchno]
set out [open $outfile w]
header $out

foreach f $flist {

#    puts -nonewline stderr "$f..."
    set fs [open $f r]

    while {[gets $fs line] >= 0} {

	if [regexp {^%} $line] {
	    incr sentno
	    if {$sentno > $size} {
		close $out
		set sentno 1
		incr batchno
		set outfile [format "proof%05d.pl" $batchno]
		set out [open $outfile w]
		header $out
	    }
	    incr sentno
	}
	puts $out $line 

    }
    close $fs
    close $out
}
