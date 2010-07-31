#!/usr/local/bin/tclsh
#
# pnsd-tclhttpd.tcl
#
# Initialization code for running OpenACS under tclhttpd
#
# 11/2002
# John Sequeira
# johnseq@pobox.com 


#set home [string trimright [file dirname [info script]] ./]
set home /var/www/portable-nsd/portable.nsd/
set home [file join [pwd] $home]

source [file join $home pnsd-bootstrap.tcl]
set ::nstcl::debug_p 1

#pnsd::source_openacs

#source "$pnsd::root/tcl/0-acs-init.tcl"

