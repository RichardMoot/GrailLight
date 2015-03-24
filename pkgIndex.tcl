
package ifneeded combobox 2.3 [list source [file join $dir combobox.tcl]]
package ifneeded tokenize 1.0 [list source [file join $dir tokenize.tcl]]
package ifneeded parseform 1.0 [list source [file join $dir parseform.tcl]]
package ifneeded app-treebank_annotator 1.0 [list source [file join $dir treebank_annotator.tcl]]

#==============================================================================
# Tablelist and Tablelist_tile package index file.
#
# Copyright (c) 2000-2009  Csaba Nemethi (E-mail: csaba.nemethi@t-online.de)
#==============================================================================

#
# Regular packages:
#
package ifneeded tablelist         4.12 \
	[list source [file join $dir tablelist.tcl]]
package ifneeded tablelist_tile    4.12 \
	[list source [file join $dir tablelist_tile.tcl]]

#
# Aliases:
#
package ifneeded Tablelist         4.12 \
	[list package require -exact tablelist	    4.12]
package ifneeded Tablelist_tile    4.12 \
	[list package require -exact tablelist_tile 4.12]

#
# Code common to all packages:
#
package ifneeded tablelist::common 4.12 \
        "namespace eval ::tablelist { proc DIR {} {return [list $dir]} } ;\
	 source [list [file join $dir tablelistPublic.tcl]]"
