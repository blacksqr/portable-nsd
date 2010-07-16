# $Header: /home/e-smith/files/ibays/cvsroot/files/portable.nsd/conf/nsd.tcl,v 1.3 2004/01/21 08:02:18 john Exp $

#
# sample-config.tcl --  The AOLserver Startup Script
#
#      This is a Tcl script that is sourced when AOLserver starts up.
#      A detailed reference is in "doc/config.txt".
#

#ns_log notice "config.tcl: starting to read config file..."


#
# Set some Tcl variables that are commonly used throughout this file.
#

set httpport               80
set httpsport              8443

# The hostname and address should be set to actual values.
#set hostname               [ns_info hostname]
set hostname               localhost
set address                [ns_info address]

set servername             [ns_info server]
set serverdesc             "Server Name"

set homedir                $::pnsd::home
#set bindir                 [file dirname [ns_info nsd]]

#set pageroot               ${homedir}/servers/${servername}/pages
set directoryfile          index.adp,index.html,index.htm

set dbhost localhost
set db            portable-nsd
set dbuser service0

set dbpwd ""

ns_section "ns/db/driver/postgresql"
set pgbin "/bin/pgsql"

# Global pnsd parameters
ns_section "pnsd/parameters"
ns_param XqlOff "true"  ;  #Set this to true if you've run xql-replace.pl on the source tree

#    ns_param  LoadXqlFromCache  false
    ns_param  LoadXqlFromCache  "true"

    ns_param  QuietQueryDispatcherLogging "true"
#    ns_param  QuietQueryDispatcherLogging "false"
#
# Global server parameters
#
ns_section "ns/parameters"
    ns_param   home            $homedir
    ns_param   debug           false



#
# MIME types.
#
#  Note: AOLserver already has an exhaustive list of MIME types, but in
#  case something is missing you can add it here.
#
ns_section "ns/mimetypes"
ns_param   default         "*/*"     ;# MIME type for unknown extension.
ns_param   noextension     "*/*"     ;# MIME type for missing extension.
#ns_param   ".xls"          "application/vnd.ms-excel"


############################################################
#
# Server-level configuration
#
#  There is only one server in AOLserver, but this is helpful when multiple
#  servers share the same configuration file.  This file assumes that only
#  one server is in use so it is set at the top in the "server" Tcl variable.
#  Other host-specific values are set up above as Tcl variables, too.
#

ns_section "ns/servers"
ns_param   $servername     $serverdesc


#
# Server parameters
#
ns_section "ns/server/${servername}"
ns_param   directoryfile   $directoryfile
#ns_param   pageroot        $pageroot
#ns_param   globalstats     true      ;# Enable built-in statistics.
#ns_param   urlstats        true      ;# Enable URL statistics.
#ns_param   maxurlstats     1000      ;# Max number of URL's to do stats on.
#ns_param   enabletclpages  false     ;# Parse *.tcl files in pageroot.


ns_section "ns/server/${servername}/acs"
ns_param   LoginTimeout   20
ns_param   PermissionCacheP false

#
# ADP (AOLserver Dynamic Page) configuration
#
ns_section "ns/server/${servername}/adp"
ns_param   map             "/*.adp"  ;# Extensions to parse as ADP's.
#ns_param   map             "/*.html" ;# Any extension can be mapped.
ns_param   enableexpire    false     ;# Set "Expires: now" on all ADP's.
ns_param   enabledebug     false     ;# Allow Tclpro debugging with "?debug".

# ADP special pages
#ns_param   errorpage      ${pageroot}/errorpage.adp ;# ADP error page.


#
# ADP custom parsers -- see adp.c
#
#ns_section "ns/server/${servername}/adp/parsers"
#ns_param   adp             ".adp"    ;# adp is the default parser.


#
# Access log -- nslog
#
# ns_section "ns/server/${servername}/module/nslog"
# ns_param   rolllog         true      ;# Should we roll log?
# ns_param   rollonsignal    true      ;# Roll log on SIGHUP.
# ns_param   rollhour        0         ;# Time to roll log.
# ns_param   maxbackup       5         ;# Max number to keep around when rolling.


ns_section "ns/db/pools" 
    ns_param   main       "OpenACS Main Pool" 
    ns_param   log        "OpenACS Log Pool" 
    ns_param   subquery   "OpenACS Subquery Pool"



ns_section "ns/db/pool/main" 
    ns_param Driver postgres 
    ns_param Connections 10                  ;# 5 is a good number. Increase according to your needs
    ns_param DataSource $dbhost:5432:$db   ;# Replace 'yourdb' with the name of your database in PG
    ns_param User $dbuser                   ;# User and password AOLserver will use to connect
    ns_param Password $dbpwd 
    ns_param Verbose Off                    ;# Set it to On to see all queries. Good for debugging SQL.
    ns_param LogSQLErrors On 
    ns_param ExtendedTableInfo On 
   # ns_param MaxOpen 1000000000            ;# Max time to keep idle db connection open
   # ns_param MaxIdle 1000000000            ;# Max time to keep active db connection open 

ns_section "ns/db/pool/log" 
    ns_param Driver postgres 
    ns_param Connections 2 
    ns_param DataSource  $dbhost:5432:$db
    ns_param User $dbuser 
    ns_param Password $dbpwd
    ns_param Verbose On 
    ns_param LogSQLErrors On 
    ns_param ExtendedTableInfo On 
   # ns_param MaxOpen 1000000000 
   # ns_param MaxIdle 1000000000 

ns_section "ns/db/pool/subquery" 
    ns_param Driver postgres 
    ns_param Connections 2
    ns_param DataSource  $dbhost:5432:$db 
    ns_param User $dbuser
    ns_param Password $dbpwd
    ns_param Verbose On 
    ns_param LogSQLErrors On 
    ns_param ExtendedTableInfo On 
   
