#!/usr/bin/tclsh

package require Mk4tcl

mk::file open lefff lefff.db
set morph [mk::view layout lefff.morph "wordpos:S lemma:S features:I"]

