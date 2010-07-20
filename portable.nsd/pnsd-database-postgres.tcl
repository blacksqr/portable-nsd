# nstcl variants of openacs' database routines specific to postgres

#TODO: This is not dbn compatible yet...

::nstcl::ad_proc -public db_exec_plsql { -dbn:optional  statement_name sql args } {

    ad_arg_parser { bind_output bind } $args

    # I'm not happy about having to get the fullname here, but right now
    # I can't figure out a cleaner way to do it. I will have to
    # revisit this ASAP. (ben)
    set full_statement_name [db_qd_get_fullname $statement_name]

    if { [info exists bind_output] } {
	return -code error "the -bind_output switch is not currently supported"
    }

    # plsql calls that are simple selects bypass the plpgsql 
    # mechanism for creating anonymous functions (OpenACS - Dan).
    # if a table is being created, we need to bypass things, too (OpenACS - Ben).
    set db ""
    set test_sql [db_qd_replace_sql $full_statement_name $sql]
    set test_sql [::nstcl::database::pseudo_bind_variables $test_sql]
#    puts $test_sql
    if {[regexp -nocase -- {^\s*select} $test_sql match]} {
	db_qd_log Debug "PLPGSQL: bypassed anon function"
	set selection [db_exec 0or1row $db $full_statement_name $sql ]
    } elseif {[regexp -nocase -- {^\s*create table} $test_sql match] || [regexp -nocase -- {^\s*drop table} $test_sql match]} {
	db_qd_log Debug "PLPGSQL: bypassed anon function -- create/drop table"
	uplevel [list ::nstcl::db_dml $full_statement_name $test_sql ]
	return ""
    } else {
	db_qd_log Debug "PLPGSQL: using anonymous function"
	set selection [db_exec_plpgsql $db $full_statement_name $test_sql $statement_name]
    }
    return [ns_set value $selection 0]
}



# emulation of plsql calls from oracle.  This routine takes the plsql 
# statements and wraps them in a function call, calls the function, and then
# drops the function. Future work might involve converting this to cache the 
# function calls

proc db_exec_plpgsql { db statement_name pre_sql fname } {
    set doc {
    A helper procedure to execute a SQL statement, potentially binding
    depending on the value of the $bind variable in the calling environment
    (if set).

    Low level replacement for db_exec which replaces inline code with a proc.
    db proc is dropped after execution.  This is a temporary fix until we can 
    port all of the db_exec_plsql calls to simple selects of the inline code
    wrapped in function calls.

    } 
#    cmdtrace 1
    set start_time [clock clicks]

    db_qd_log Debug "PRE-QD: the SQL is $pre_sql"

    # Query Dispatcher (OpenACS - ben)
    set sql $pre_sql ; #set sql [db_qd_replace_sql $statement_name $pre_sql]

    db_qd_log Debug "POST-QD: the SQL is $sql"

    set unique_id [db_nextval "anon_func_seq"]

    set function_name "__exec_${unique_id}_${fname}"

    # insert tcl variable values (Openacs - Dan)
    if {![string equal $sql $pre_sql]} {
        set sql [uplevel 2 [list subst -nobackslashes $sql]]
    }
    db_qd_log Debug "PLPGSQL: converted: $sql to: select $function_name ()"

    # create a function definition statement for the inline code 
    # binding is emulated in tcl. (OpenACS - Dan)
#    set db ""
#    set errno [catch {
#    } error]
# 	upvar bind bind
# 	if { [info exists bind] && [llength $bind] != 0 } {
# 	    if { [llength $bind] == 1 } {
#                 set bind_vars [list]
#                 set len [ns_set size $bind]
#                 for {set i 0} {$i < $len} {incr i} {
#                     lappend bind_vars [ns_set key $bind $i] \
#                                       [ns_set value $bind $i]
#                 }
# #JS: defer to nstcl?                
# 		set proc_sql [db_bind_var_substitution $sql $bind_vars]
# 	    } else {
# #JS:defer to nstcl?                
# 		set proc_sql [db_bind_var_substitution $sql $bind]
# 	    }
# 	} else {
# #JS:defer to nstcl?
# 	    set proc_sql [uplevel 2 [list db_bind_var_substitution $sql]]
# 	    # ns_log debug "SQL:$sql\n-----\n$proc_sql";
# 	}

#    set proc_sql [::nstcl::database::pseudo_bind_variables $sql]

#Doing this means we won't support the few cases where bind vars are needed
	set proc_sql $pre_sql
#	ns_log Debug "db_exec_plpgsql reports $function_name ..\n $proc_sql"

    ::nstcl::db_dml "anonymousfunction" "create function $function_name () returns varchar as '
                      [DoubleApos $proc_sql]
	    ' language 'plpgsql'"
#    ns_log debug "Now execute it"
#    db_0or1row $statement_name "select $function_name ()" -column_set ret_val
#Must call nstcl directly to bypass query dispatcher
#    db_0or1row $statement_name "select $function_name ()" -column_set ret_val
    ::nstcl::db_0or1row $statement_name "select $function_name ()" -column_set ret_val
    # drop the anonymous function (OpenACS - Dan)
    
    #    ns_log debug "drop the function"
    ::nstcl::db_dml "anonymousfunction_drop" "drop function $function_name ()"
	
    return $ret_val
    
    set error ""

    global errorInfo errorCode
    set errinfo $errorInfo
    set errcode $errorCode
    
    ad_call_proc_if_exists ds_collect_db_call $db 0or1row $statement_name $sql $start_time $errno $error

    if { $errno == 2 } {
	
	return $error
    } else {
        catch {::nstcl::db_dml "myquery" "drop function $function_name ()"}
    }

    return -code $errno -errorinfo $errinfo -errorcode $errcode $error

}


#JS: BROKEN... does not handle bind vars.
::nstcl::ad_proc db_exec { type db statement_name pre_sql {ulevel 2} } {

    A helper procedure to execute a SQL statement, potentially binding
    depending on the value of the $bind variable in the calling environment
    (if set).

} {

    set start_time [clock clicks]

    db_qd_log Debug "PRE-QD: the SQL is $pre_sql for $statement_name"

    # Query Dispatcher (OpenACS - ben)
    set sql [db_qd_replace_sql $statement_name $pre_sql]

    # insert tcl variable values (Openacs - Dan)
    if {![string equal $sql $pre_sql]} {
        set sql [uplevel $ulevel [list subst -nobackslashes $sql]]
    }

    db_qd_log Debug "(js)POST-QD: the SQL is $sql"

    set errno [catch {
	upvar bind bind
	if { [info exists bind] && [llength $bind] != 0 } {
	    if { [llength $bind] == 1 } {
		return [eval [list ns_pg_bind -bind $bind $type $db $sql]]
	    } else {
		set bind_vars [ns_set create bind_vars]
		foreach { name value } $bind {
		    ns_set put $bind_vars $name $value
		}
		return [eval [list ns_pg_bind -bind $bind_vars $type $db  $sql]]
	    }
	} else {
	    return [uplevel $ulevel [list ns_pg_bind $type $db $sql]]
	}
    } error]

    ad_call_proc_if_exists ds_collect_db_call $db $type $statement_name $sql $start_time $errno $error
    if { $errno == 2 } {
	return $error
    }

    global errorInfo errorCode
    return -code $errno -errorinfo $errorInfo -errorcode $errorCode $error
}


#take no chances... nstcl doesn't always return a string openacs recognizes
proc db_type {} { 
    return postgresql
}

set ::pnsd::invariants(db_type) 1 
proc db_name {} { 
    return PostgreSQL
}

set ::pnsd::invariants(db_name) 1 


proc db_bootstrap_checks { errors error_p } { }
set ::pnsd::invariants(db_bootstrap_checks) 1 
ns_log debug "\[PNSD]Overloading db_bootstrap_checks -- pnsd will use it's own tests for this"


::nstcl::ad_proc  db_driverkey {{ -handle_p 0 } { dbn "" } } {
    Overload db_driverkey... we'll assume one db for now.
} { 
    return postgresql
}
set ::pnsd::invariants(db_driverkey) 1 
