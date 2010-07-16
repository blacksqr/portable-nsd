# Create a file xql.dat that is will cache the xql-parsing stage of openacs startup.
#
# Remember to enable xql caching.
#
# John Sequeira 
# johnseq@pobox.com
# 10/2002


source [file join [file dirname [info script]] .. pnsd-init.tcl ]


::pnsd::source_openacs ; #load libraries etc.

# proc db_rdbms_get_type {rdbms} {
#     return oracle
# }

# proc db_rdbms_get_version {rdbms} {
#     return 8.1.6
# }

#proc db_rdbms_create {type version} {
#    return [list oracle "8.1.6"]
#}

#This will make sure we load all the queries ... disable this for single db system.
proc db_rdbms_compatible_p {rdbms_test rdbms_pattern} { return 1 }

set queries [ ::pnsd::load_xql]
if { $queries  > 10000 } {
    puts "$queries queries cached from XQL files"
}

