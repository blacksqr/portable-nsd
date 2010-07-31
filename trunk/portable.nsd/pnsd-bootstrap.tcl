#! /usr/bin/env tclsh
# Experimental: These routines will minimally load enough of OpenACS so that the remainder can be auto-loaded


package require profiler
#::profiler::init 
source [file join [file dirname [info script]] pnsd-init.tcl ]

source [file join $::pnsd::home pnsd-templating.tcl ]

pnsd::source_openacs

pnsd::_lock_procs


#skip these
proc db_bootstrap_checks {errors error_p} {}
set ::pnsd::invariants(db_bootstrap_checks) 1
#proc ns_log {args} {}




#puts [info body template::adp_prepare]
lappend auto_path [file join [file dirname [info script]] lib]


#jjs:stolen from 0-acs-init.tcl
set root_directory [file dirname [string trimright [ns_info tcllib] "/"]]
nsv_set acs_properties root_directory $root_directory

#jjs:stolen from bootstrap.tcl ... still need it?
nsv_set proc_source_file . ""

# Initialize ad_after_server_initialization.
nsv_set ad_after_server_initialization . ""


apm_source [file join $root_directory packages/acs-bootstrap-installer/installer-init.tcl ]
set files [glob -nocomplain "$root_directory/packages/acs-bootstrap-installer/tcl/*-procs.tcl"]
if { [llength $files] == 0 } {
    error "Unable to locate $root_directory/packages/acs-bootstrap-installer/tcl/*-procs.tcl."
}

foreach file [lsort $files] {
    ns_log Notice "Bootstrap: sourcing $file"
    apm_source $file
}



db_bootstrap_set_db_type database_problem


#check for  $database_problem
#    if { ![info exists database_problem] } {
#        set db_fn "$root_directory/packages/acs-bootstrap-installer/db-init-checks-[nsv_get ad_database_type .].tcl"
#        if { ![file isfile $db_fn] } {
#            set database_problem "\"$db_fn\" does not exist."
#        } else {
#            apm_source $db_fn
#        }
#        db_bootstrap_checks database_problem error_p
#    }


proc populate_secret_tokens_cache {} {}
set pnsd::invariants(populate_secret_tokens_cache) 1

#Move this into a watched variable?
db_1row get_one_token " select t.token_id, t.token from (select token_id, token,  random()
   from secret_tokens order by 3) t  limit 1"
ns_cache set secret_tokens $token_id $token 



apm_bootstrap_load_libraries -init acs-tcl
#apm_bootstrap_load_libraries -init -procs acs-subsite
#apm_bootstrap_load_libraries -init acs-subsite
#apm_bootstrap_load_libraries -init acs-templating
#apm_bootstrap_load_libraries -init -procs acs-developer-support

#If we're retrieving a tcl or adp file
# from acs-integration-init ...
rp_register_extension_handler adp adp_parse_ad_conn_file
rp_register_extension_handler tcl adp_parse_ad_conn_file
apm_source $root_directory/packages/acs-templating/tcl/0-procs.tcl


#apm_source [file join $root_directory packages/acs-tcl/tcl/00-database-procs-postgresql.tcl]


#set files [glob -nocomplain "$root_directory/packages/acs-tcl/tcl/*-init.tcl"]
#foreach file [lsort $files] {
#    ns_log Notice "Bootstrap: sourcing $file"
#    apm_source $file
#}



#apm_source [file join $root_directory packages/acs-tcl/tcl/request-processor-init.tcl]
#apm_source [file join $root_directory packages/acs-tcl/tcl/request-processor-procs.tcl]

#need this for ad_page_contract
apm_source [file join $root_directory packages acs-tcl tcl tcl-documentation-procs.tcl]


#initialize site nodes
#apm_source [file join $root_directory packages/acs-tcl/tcl/utilities-procs.tcl]

#I don't want to initialize the cache each time...
#proc populate_secret_tokens_cache { args } {}
#set ::pnsd::invariants(populate_secret_tokens_cache) 1
#apm_source [file join $root_directory packages/acs-tcl/tcl/security-init.tcl]

#apm_source [file join $root_directory packages/acs-tcl/tcl/site-nodes-init.tcl]

