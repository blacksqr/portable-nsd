#!/usr/bin/tclsh
#  ns_log.tcl
# 	$Id: ns_log.tcl,v 1.4 2003/03/21 20:06:56 john Exp $	
#
# Portable.NSD
# John Sequeira
# johnseq@pobox.com
# 09/2002 


#jjs: 'package require log' -> more flexible,  but slower
#TODO: should switch to David Welton's logger

#log to screen for now
proc ns_log { severity args    }  { 
    if { [llength $args] } {
#	log::log [string tolower $severity] $args
	set message [lindex $args 0]
	puts "$severity : $message \n"
    } else {
	#I should puts a stack trace 
    }
}


package require log

::log::lvChannelForall $::pnsd::log_stream

proc ns_log { severity args  }  { 
    if [string match -nocase $severity "error"] {
	append ::pnsd::error "$args\n"
    }

    if { [llength $args] } {
	log::log [string tolower $severity] $args
    }

}


#TODO: Implement ns_log that disables command tracing.
# proc _broken_ns_log { severity args    }  { 
    
#     variable restore_cmdtrace 0

#     if {[lsearch [namespace children] ::Tclx] == -1} {
# 	if [cmdtrace depth] {
# 	    set restore_cmdtrace 1
# 	    cmdtrace off
# 	#    namespace import ::math::*
# 	}
#     }

#     if { [llength $args] } {
# 	log::log [string tolower $severity] $args
#     }

    
#     if { $restore_cmdtrace } {
# 	cmdtrace on
#     }
# }
