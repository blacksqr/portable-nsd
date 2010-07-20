#!/usr/local/bin/tclsh
#
# go.tcl
#
# This tcl script will be used to probe the OpenACS 4.x libraries and return the default start page to STDOUT
# It will import necessary variables/libraries, and define stub functions for appropriate for tclsh execution.
# 
# It can be considered a template for running OpenACS code via tclsh
#
# Inspired by http://michael.cleverly.com/nstcl/
#
# John Sequeira
# johnseq@pobox.com
# 10/2002
# 	$Id: go.tcl,v 1.14 2004/01/21 08:02:18 john Exp $	


#namespace import ::nstcl::

# This loads tclsh-specific aolserver functionality
#package require Tclx
#cmdtrace  3; #notruncate

source [file join [file dirname [info script]] pnsd-init.tcl ]



#puts [info body db_exec ]
#puts [info body db_exec_plpgsql]




#puts "go.tcl: Begin Sourcing init"
#puts "Sourcing (pnsd)OpenACS [time ::pnsd::source_openacs 1]"
#proc populate_secret_tokens_cache { args } {}
#set ::pnsd::invariants("populate_secret_tokens_cache") 1 
#    set SQL [[$dbhandle bind_vars] $SQL]

pnsd::source_openacs

set ::pnsd::url /

set ::pnsd::querystring ""
set package_name "site_node"
#puts [ package_plsql_args "site_node";]

#populate_secret_tokens_cache
ad_user_login 452

#rp_filter ""

#rp_handler

#cmdtrace 4 notruncate  [open cmd.log w] 
array set x  [auth::create_user -username "oneuser" -email "john@pono.com" -last_name "seq" -first_names "jon"     ]
parray x
#::pnsd::write_html
#puts [info commands ::auth::*]
#puts [info body _acs-authentication__auth_create_user]
cmdtrace off
#puts [info body ::auth::create_user]
exit


#sign myself in as sysadmin
#puts [util_memoize {site_nodes_sync_helper}]
#puts [site_node /register/]
#exit

#saveprocs procs.tcl 

#
#rp_handler 
#cmdtrace off
#parray OACS_FULLQUERIES
#puts "the ad_conn file is [ad_conn file]"
#puts $::pnsd::__http_stream
ns_log debug "Finished"
#puts [info body adp_parse_ad_conn_file]
#puts [info body ::template::code::adp::c:/temp/openacs-4/www/index]
#puts [info body ::nstcl::template::adp::compiled::2f898d54f2e0abfe713d906a4edeb179]
exit
