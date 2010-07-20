#!/usr/bin/tclsh 
# pnsd-init.tcl - Initialize all the files that support tclsh abstraction of openacs
# 	$Id: pnsd-init.tcl,v 1.15 2003/12/07 18:56:37 john Exp $	
#
# Copyright 2003 -  John Sequeira  -  http://www.pobox.com/~johnseq
# johnseq@pobox.com
# 

#put here to make tclpro happy (?)
#package require htmlparse
 
package require nstcl 
package require fileutil 
#package require Tclx   ; #used occasionally for debugging

namespace import ::nstcl::*    

#the beginnings of namespace definition
namespace eval ::pnsd { 
#    variable root "/var/www/portable-nsd/portable.nsd/openacs-4.6.3"   ; # root folder of openacs installation
    variable querystring ""    ; # querystring of incoming request
    variable url ""            ; # url of incoming request
    variable __http_stream ""   ; # will contain html of page as it's being built
    variable redirect_p "f"
    variable http_done_p "f"
#    variable home "/var/lib/aolserver/portable.nsd/"    ; # pnsd root folder
    variable invariants ; # array of procs we don't want to have redefined
    variable error 
    variable log_stream ""; # Teste
    
    variable home [file dirname [info script]]; 

    variable root [file join $home openacs-4.6.3]

#######################################################################
# BEGIN CONFIGURATION 
####################################################################### 
# TODO - this should be set from config file.
#    set root [file join [lindex [file split [pwd]] 0 ] temp/openacs-4]
#    set ::pnsd::root /var/lib/aolserver/portable.nsd ;
#    set ::pnsd::root c:/temp/new ; #NUKEME
#    set ::pnsd::root c:/web/openacs-4.bak  ; #NUKEME
#    set ::pnsd::root c:/web/openacs  ; #NUKEME

#JJS: This is a hack for needed now bc openacs allows empty src attributes in master tags, but nstcl does not
    set ::nstcl::template::default_master [file join $::pnsd::root www/default-master]



# #OpenACS allows empty master tags,  but nstcl does not
#     set ::nstcl::template::default_master [file join $root ./www/default-master]

    #Files I need to parse config file
    foreach f { ns_info  } {
	uplevel \#0 [list source [file join [file dirname [info script]] $f.tcl ] ]
    }

    #INITIALIZE LOG
    #JS-TODO move this to ns_info/config file?
    #opening the log file for append works better in production, when multiple interpreters need to write to the same file
#    set ::pnsd::log_stream [open [ns_info log] a ] ; # USE FOR PRODUCTION
    #opening the log file for write works better for development - each run of a script generates its own log.
    set ::pnsd::log_stream [open [ns_info log] w ] ; # USE FOR DEVELOPMENT


#    source [file join $home conf nsd-oracle.tcl ]
    source [file join $home conf nsd.tcl ]

#makes emacs happy,  but comment it out if you read logs in notepad. 
#Also,  probably want to have buffering when running in production
    fconfigure $::pnsd::log_stream -buffering none -translation lf 

    #Files that should follow config file parse

    foreach f {ns_log ns_dbsetup } {
	uplevel \#0 [list source [file join [file dirname [info script]] $f.tcl ] ]
    }

#poor man's query dispatcher    
    if [ regexp -nocase {oracle} [db_name] ] {
	set dbfile pnsd-database-oracle  ; ns_log debug "poor man's query dispatcher selecting pnsd-database-oracle "
    } else {
	set dbfile pnsd-database-postgres ; ns_log debug "poor man's query dispatcher selecting pnsd-database-postgres "
    }

    foreach f [list $dbfile nsv ns_misc ns_xml ns_conn ns_cache] {
	uplevel \#0 [list source [file join [file dirname [info script]] $f.tcl ] ]
    }



    # Set up output Headers
    variable output_headers_id
    set output_headers_id [ns_set create outputheaders]
    
    # Set up headers ... 
    variable headers_id
    set headers_id [ns_set create headers]
    ns_set update $headers_id Host [info host]
    ns_set update $headers_id Referer ""




array set ::pnsd::invariants {}

proc lock_proc { name } {
    set ::pnsd::invariants($name) 1 
}

# NSTCL PROCS THAT CAN NOT BE REDEFINED
foreach name  {
    db_string 
    db_list
    db_list_of_lists
    db_list_of_ns_sets
    db_multirow
    db_0or1row
    db_1row
    db_quote
    db_transaction
    db_abort_transaction
    db_continue_transaction
    db_name
    db_dml
    db_exec
    db_exec_plpgsql
    db_exec_plsql
    db_foreach
    ns_pg_bind_0or1row
    ns_pg_bind


    db_abort_transaction_p
    multirow
    template::multirow
    template::adp_eval
    template::adp_compile
    template::adp_puts
    template::adp_abort
    template::adp_init
    template::adp_prepare
    template::adp_parse
    adp_parse_ad_conn_file
    ns_adp_parse

    template::form::template
    template::form::generate
    template::adp_compile_chunk
    template::form::get_reference

} { 
    lock_proc $name
}
# remove db_source_sql_file for postgres.


# NOTE: These will be overloaded in pnsd-templating.tcl
#    template::adp_abort
#    template::adp_parse
#    template::adp_prepare
#    template::adp_init
#    ad_ns_set_to_tcl_vars
#    set_variables_after_query

#lock pnsd/nstcl defined commmands
proc _lock_procs {} {

    uplevel \#0 {

	rename proc _proc
	_proc ::proc {
	    name 
	    arglist 
	    body
	} {
	    #array set mutable_procs { ::ad_arg_parser }
	    if {[info exists ::pnsd::invariants($name)]} { 
		ns_log info "NOT defining $name ... it's locked"
		
		return
	    }
	    
	    if [llength [info commands ::nstcl::$name]] {  
		
		if {![string match $name "::ad_arg_parser"] 
		    && ![string match $name "ad_proc"]} { 
		    ns_log info "NOT redefining $name"; 
		    return 
		} 
		
		#ns_log debug "redefining $name"
		
	    } 

#	    if  [info proc $name] {  	    		
		#		ns_log "Redefining existing command $name"
#	    }

	    #Use this only for loading proc array
	    #global OACS_PROCS
	    #set OACS_PROCS($name) [info script]; #Use this to store proc->file lookups
	    
	    #ns_log info "defining $name"
	    uplevel 1 [list _proc $name $arglist $body]
	    #     uplevel 1 [list trace add execution $name enterstep [list ::proc_start $name]]
	    
	}
    }

}

proc _unlock_procs {} { 

    uplevel \#0 {
	rename proc ""
	rename _proc proc
    }

}

proc source_openacs {} {

    #need to make sure openacs doesn't redefine nstcl database and templating commands
    # I'll overload proc temporarily,  and check my own do-not-redefine list to see if
    # proc defininition should proceed.

    _lock_procs
    
    set initial_files [lsort [::fileutil::find [file join $::pnsd::root tcl] ]]

    foreach file $initial_files {
		# Change it to use regexp
		# TODO: move CVS filter into initial_files defn
		if {[regexp {^.*\.tcl$} $file]} {
			# Load only Tcl files
			uplevel #0 [ list source $file]
		}
		#if { [string match *CVS* $file ] == 0 } {
	    #	uplevel #0 [list source $file]
		#}
    }
    
    #Since templating commands are almost all overloads,  and is not called during load,  
    #I'll just source it after openacs

    _unlock_procs

    uplevel \#0 source [file join $::pnsd::home pnsd-templating.tcl ]

    # One frequent problem with loading is having a bad cache (when moving files between filesystems, or pointing to a new openacs root)
    # check to see if the XQL file has loaded successfully.

    if ![ns_config -bool pnsd/parameters XqlOff]      {
	if [ns_config -bool pnsd/parameters LoadXqlFromCache  ] {
	    global OACS_FULLQUERIES
	    if { [array size OACS_FULLQUERIES] < 1000 } {
		error "OACS_FULLQUERIES doesn't look like it loaded correctly"
	    }
	    #check a random kernel query to make sure it exists and the file path works
	    set fullquery "dbqd.acs-tcl.tcl.acs-kernel-procs.ad_acs_administrator_exists_p.admin_exists_p"
	    if {![file exists [db_fullquery_get_load_location [db_qd_fetch $fullquery] ] ]} {
		error "OACS_FULLQUERIES has invalid path for file $fullquery \.  Is the cache valid?"
	    }
	}
    }
}
#source_openacs

proc reset_connection {} {
    flush $::pnsd::log_stream

    global request_aborted
    if [info exists request_aborted] {
	unset request_aborted
    }

    #Legacy stuff
    global doc_properties
    if [ array exists doc_properties ] {
	array unset doc_properties 
    }

    #pnsd reset
    set ::pnsd::__http_stream ""
    set ::pnsd::redirect_p "f"
    set ::pnsd::http_done_p "f"

    set ::pnsd::error "" 

    if [ns_set size $::pnsd::output_headers_id] { 
	ns_set truncate $::pnsd::output_headers_id 0
    }

    if [ns_set size $::pnsd::headers_id ] { 
	ns_set truncate $::pnsd::headers_id 0 
    }


#page_contract globals from tcl-documentation-procs.tcl
    
    global ad_page_contract_errorkeys ad_page_contract_complaints 
    if {[info exists ad_page_contract_complaints]} {
	unset ad_page_contract_complaints
    }
    if {[info exists ad_page_contract_errorkeys]} {
	unset ad_page_contract_errorkeys 
    }

    global ad_page_contract_error_string ad_page_contract_validations_passed 
    if {[array exists ad_page_contract_validations_passed]} {
	array unset ad_page_contract_validations_passed
    }
    if {[array exists ad_page_contract_error_string] } { 
	array unset ad_page_contract_error_string
    }

#templating
#    set ::template::parse_level ""
#    namespace eval template variable parse_level "" ;

#nsdb
    

    #openacs?
    ad_conn -reset

}



proc write_html {} {

    puts $::pnsd::__http_stream 

}




#Use this to quickly load xql files ... see  http://mini.net/tcl/3469 for info
proc persistentArray {arrName {filename {}}} {
    upvar 1 $arrName arr
    array set arr {} ;# to make sure it exists, and is an array
    
    if {$filename==""} {set filename $arrName.txt}
    ns_log debug "persisting $arrName to $filename"

    set filename [file join [pwd] $filename]
    if [file exists $filename] {
        set fp [open $filename]
        array set arr [read $fp]
        close $fp
    }

#skip this... use pnsd::load_xql to make sure you have all files
#    uplevel 1 [list trace var $arrName wu [list ::pnsd::persist'save $filename]]
}

#this could be improved so many ways... but it's okay for now
proc persist'save {filename arrName el op} {

    upvar 1 $arrName arr

    
    switch -- $op {
        w {set value $arr($el)}
        u {set value {}}
    }

    set    fp [open $filename a]
    puts  $fp [list $el $value]
    close $fp
}


#Use this command to cache the xql into a .dat file for quick loading.
#This will indescriminantly load each and every query for all supported databases - it can be made more efficient e.g. by only doing pg or oracle queries... but it's fast enough
proc load_xql { {filename ""} } {

    #These files contain dependency functions.
#    source [file join $::pnsd::root packages/acs-bootstrap-installer/tcl/40-db-query-dispatcher-procs.tcl ]
#    source [file join $::pnsd::root packages/acs-tcl/tcl/30-xml-utils-procs.tcl ]

    if [ ns_config -bool pnsd/parameters LoadXqlFromCache ] {
	ns_log error "Your config file has disabled xql-file parsing.  Please change option pnsd/parametere LoadXqlFromCache"
	exit
    }

    if [ string equal {} "[info proc db_qd_load_query_file][info proc ::db_qd_load_query_file]" ] {
	ns_log error "db_qd_load_query_file is not defined.  Did you forget to pnsd::source_openacs?"
	exit
    }

    set arrName OACS_FULLQUERIES
    if [ string equal {} $filename ] {
	set filename [file join $::pnsd::home xql.dat]
    }

    uplevel 1 [list trace var $arrName wu [list ::pnsd::persist'save $filename]]
    
    set xqlfiles  [ ::fileutil::findByPattern $::pnsd::root -glob  *.xql    ] 

    foreach  file  $xqlfiles {
	    db_qd_load_query_file $file
    }
    
    return [llength xqlfiles]

}


}
#namespace

# Make nstcl's db routines aware of Query Dispatcher
source [file join [file dirname [info script]] db.tcl]

#puts [info body db_dml]
#exit





# TOGGLE LOADING OF XQL FILE FROM CACHE
# only do this if a script has not set it already
if [ns_config -bool pnsd/parameters LoadXqlFromCache] {
    #ns_log notice "Not loading xql files.. relying on cache"
    proc db_qd_load_query_file { args } {
	# no-op
	ns_log debug "Skipping db_qg_load_query_file"
    }

    set ::pnsd::invariants(db_qd_load_query_file) 1

}




# THIS WILL MAKE SURE CACHE IS CREATED CORRECTLY.

pnsd::persistentArray OACS_FULLQUERIES [file join $::pnsd::home xql.dat]

#If I'm using a OACS_FULLQUERIES cached file that contains queries for more than one database, use only the right ones.

pnsd::persistentArray OACS_PROCS [file join $::pnsd::home procs.dat]


#TURN OFF VERBOSE QD LOGGING
if [ns_config -bool pnsd/parameters QuietQueryDispatcherLogging] {
    #ns_log notice "Disabling verbose query dispatcher logging"
    proc db_qd_log { args } {}
    set ::pnsd::invariants(db_qd_log) 1
}

