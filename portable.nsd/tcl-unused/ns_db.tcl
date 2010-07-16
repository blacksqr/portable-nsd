# Test out my bind function.
if {[lsearch [namespace children] ::tcltest] == -1} {
    package require tcltest
    namespace import ::tcltest::*
}



source [file join [file dirname [info script]] .. pnsd-init.tcl]
source [file join  [file dirname [info script]] .. conf nsd.tcl ]




test ns_db-1.0 { check user } {knownBug} {
    [file exists [ns_config ns/db/driver/postresql pgbin]]
} {1}



#pnsd-init
test ns_db-2.0 { pgbin ? } {knownBug} {
    file exists [db_get_pgbin]
} {1}

test ns_db-3.0 {Connections} {
    set pool main
    empty_string_p [ns_config ns/db/pool/$pool Connections]  
} {0}
test ns_db-3.1 {DataSource} {
    set pool main
    empty_string_p [ns_config ns/db/pool/$pool DataSource]  
} {0}

test ns_db-3.2 {User} {
    set pool main
    empty_string_p [ns_config ns/db/pool/$pool User]  
} {0}

#?
#test ns_db-3.3 {Password Empty?} {
#    set pool main
#    empty_string_p [ns_config ns/db/pool/$pool Password]  
#} {0}


::tcltest::cleanupTests
return