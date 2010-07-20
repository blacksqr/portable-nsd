# implement this as a nop for now.
# Then try doing a db lookup
# requires ad_arg_parser
#
# John Sequeira
# johnseq@pobox.com

if {[lsearch [namespace children] ::nstcl] == -1} {
    package require nstcl
    namespace import ::nstcl::*
}

proc ns_cache { cmd args  }  {
    ::nstcl::ad_arg_parser { size timeout thread args } $args
#    puts "Debug: ns_cache $cmd called w/ $args "
    switch $cmd { 
	"create"  { 
	    ns_log info "(no-op)ns_cache creating [lindex $args 0]"
#	    set cachename [lindex $args 0]
#	    global $cachename
#	    array set $cachename {}
	    return 1
#	    return
	}
	"get"     { 
	    set cachename [lindex $args 0]
#	    global $cachename	    
	    set key [lindex $args 1]
	    
	    if {[catch {
		db_1row ns_cache_get "select cache_value from ns_cache where cacheid=:cachename and cache_key = :key"}]} {		
		if {[llength $args] == 3 } {
		    return 0
		} else {		    
		    error "ns_cache get: Couldn't locate key in cache"
		}
	    } else {
		# we were able to retrieve a cache value ... 
		if {[llength $args] == 3 } {
		    set varname [lindex $args 2]		
		    upvar 1 $varname $varname
		    set $varname $cache_value		
		    return 1
		}

		return $cache_value
	    }


	}
	"set"     { 
	    ns_log info "ns_cache SET [info level 0]"
	    set cachename [lindex $args 0]
#	    global $ cachename
	    set key [lindex $args 1]
	    set value [lindex $args 2]
#	    ns_log Debug "ns_cache set called w/ $args "
	    nstcl::db_dml ns_cache_remove "delete from ns_cache where cacheid=:cachename and cache_key=:key"
	    nstcl::db_dml ns_cache_set "insert into ns_cache(cacheid, cache_key, cache_value) values (:cachename, :key, :value)"
	    return 1
	}
	
	"eval"    { 
	    set cachename [lindex $args 0]
#	    global $cachename
	    set key [lindex $args 1]
	    set script [lindex $args 2 ]
	    #puts "eval scrtip = $script"
	    if {[catch { set statement_value [ns_cache get $cachename $key] }]} {
		# not in the cache already
		set statement_value [uplevel 1 $script]
		ns_cache set $cachename $key $statement_value
	    }
	    return $statement_value
	}
	"names" {
	    set cachename [lindex $args 0]
	    return [db_list ns_cache_names "select cache_key from ns_cache where cacheid=:cachename"]
	}	   
	"flush" {
	    set cachename [lindex $args 0]
	    if {[llength $args] == 1} { error "No key passed to flush" } 
	    set key [lindex $args 1]
	    nstcl::db_dml ns_cache_flush "delete from ns_cache where cacheid=:cachename and cache_key = :key"
	    return 1
	}
	"init" {
	    #added by me... it will create the  initial db table for ns_cache

	    #(very)poor man's query dispatcher: qd won't work on files outside openacs source code tree...
	    if { [ns_config ns/db/pool/main Driver]=="oracle" }  {
		return [expr ![catch { nstcl::db_dml ns_cache_initialize "create table ns_cache ( cacheid varchar(3000), cache_key varchar(1000), cache_value long)"} ]]
#TODO create indexes on the cache.
	    } else {
		#postgresql version
		return [expr ![catch { nstcl::db_dml ns_cache_initialize "create table ns_cache ( cacheid text, cache_key varchar(1000), cache_value text)"} ]]
	    }
	    
	    
	}
	"reinit" {
	    ns_log info "ns_cache re-initializing"
	    nstcl::db_dml ns_cache_reinit "delete from ns_cache"
	    return 1
	}
	
    }

    ns_log Error "ns_cache $cmd called with $args"
}

#Not sure I need a catch... ns_cache has its own
catch { ns_cache init }
ns_cache reinit
