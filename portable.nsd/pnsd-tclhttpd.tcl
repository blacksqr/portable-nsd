#!/usr/bin/tclsh
#
# pnsd-tclhttpd.tcl
#
# Initialization code for running OpenACS under tclhttpd
#
# 11/2002
# John Sequeira
# johnseq@pobox.com 


set home [string trimright [file dirname [info script]] ./]
set home [file join [pwd] $home]

source [file join $home pnsd-init.tcl]
set ::nstcl::debug_p 1


source "$pnsd::root/tcl/0-acs-init.tcl"

