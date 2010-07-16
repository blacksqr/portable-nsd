# nstcl variants of openacs' database routines specific to oracle
# JS: I'm going to ignore funky clob handling for now,  if possible.


#namespace eval ::pnsd::database::oracle {}

# wrapper around :nstcl::database::pseudo_bind_variables so that it upvars up the stack the correct amount
# also handle :1 = ... style statements
proc db_exec_plsql { statement_name sql args } {

#    Executes a PL/SQL statement, returning the variable of bind variable <code>:1</code>.

    ns_log debug "db_exec_plsql ... [info level 0]"
    ad_arg_parser { bind_output bind } $args

    # Query Dispatcher (OpenACS - ben)
    set full_statement_name [db_qd_get_fullname $statement_name]

    if { [info exists bind_output] } {
	return -code error "the -bind_output switch is not currently supported"
    }

#    db_with_handle db 
	# Right now, use :1 as the output value if it occurs in the statement,
	# or not otherwise.
    set test_sql [db_qd_replace_sql $full_statement_name $sql]
    if { [regexp {:1} $test_sql] } {
	
	#return [db_exec exec_plsql_bind $db $full_statement_name $sql 2 1 ""]
	#JS : drop down into oratcl to get the return value
	#	set dbh [ns_db gethandle main]; doesn't work
#put in try/catch so statement is always closed	
	set dbhandle [::nstcl::database::api_get_dbhandle $statement_name]
	set conn [$dbhandle conn]
	set sth [oraopen $conn]

	regsub {:1}  $test_sql "__pnsd_retval__" test_sql   ; # Hide return bind var :1 from nstcl... yuck
	set test_sql [::nstcl::database::pseudo_bind_variables $test_sql]
	regsub "__pnsd_retval__"  $test_sql :1 test_sql   ; # Unhide :1 from nstcl... yuck yuck
	
	ns_log debug $test_sql 
	oraplexec $sth $test_sql :1 {}  

	orafetch $sth  -dataarray results
	oraclose $sth

	::nstcl::database::api_free_dbhandle $dbhandle
	ns_log debug "returning $results(:1) "
	return $results(:1)

    } else {
	set test_sql [::nstcl::database::pseudo_bind_variables $test_sql]
	uplevel [list ::nstcl::db_dml $full_statement_name $test_sql]
	ns_log debug "db_exec_plsql w/$test_sql "
	return
    }
    
}


# This does ns_ora stuff that I'll have to access oratcl directly to replace.  Skip it for now.
::nstcl::ad_proc db_exec { type db statement_name pre_sql {ulevel 2} args } {

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

    db_qd_log Debug "POST-QD: the SQL is $sql"

#     set errno [catch {
# 	upvar bind bind
# 	if { [info exists bind] && [llength $bind] != 0 } {
# 	    if { [llength $bind] == 1 } {
# 		return [eval [list ns_ora $type $db -bind $bind $sql] $args]
# 	    } else {
# 		set bind_vars [ns_set create]
# 		foreach { name value } $bind {
# 		    ns_set put $bind_vars $name $value
# 		}
# 		return [eval [list ns_ora $type $db -bind $bind_vars $sql] $args]
# 	    }
# 	} else {
# 	    return [uplevel $ulevel [list ns_ora $type $db $sql] $args]
# 	}
#     } error]

    ad_call_proc_if_exists ds_collect_db_call $db $type $statement_name $sql $start_time $errno $error
    if { $errno == 2 } {
	return $error
    }

    global errorInfo errorCode
    return -code $errno -errorinfo $errorInfo -errorcode $errorCode $error
}


ad_proc db_source_sql_file { {-callback apm_ns_write_callback} file } {

    Sources a SQL file (in SQL*Plus format).

} {
    
    global env
    set user_pass [db_get_sql_user]
    cd [file dirname $file]
    ns_log debug [file join $env(ORACLE_HOME) bin sqlplus] $user_pass "@$file" "r" 

# see http://openacs.org/forums/message-view?message_id=33142
#    set fp [open "|[file join $env(ORACLE_HOME) bin sqlplus] $user_pass @ $file" "r"]
#only an issue on windows.
    set fp [open "|[file join $env(ORACLE_HOME) bin sqlplus] $user_pass < $file" "r"]

    while { [gets $fp line] >= 0 } {
	# Don't bother writing out lines which are purely whitespace.
	if { ![string is space $line] } {
	    apm_callback_and_log $callback "[ad_quotehtml $line]\n"
	}
    }
    close $fp
}


set ::pnsd::invariants(db_source_sql_file) 1 

# proc db_dml { statement_name sql args } {

#     set doc { 
# 	Do a DML statement.
#     } 

#     ad_arg_parser { clobs blobs clob_files blob_files bind } $args

#     # Query Dispatcher (OpenACS - ben)
#     set full_statement_name [db_qd_get_fullname $statement_name]

#     # Only one of clobs, blobs, clob_files, and blob_files is allowed.
#     # Remember which one (if any) is provided.
#     set lob_argc 0
#     set lob_argv [list]
#     set command "dml"
#     if { [info exists clobs] } {
# 	set command "clob_dml"
# 	set lob_argv $clobs
# 	incr lob_argc
#     }
#     if { [info exists blobs] } {
# 	set command "blob_dml"
# 	set lob_argv $blobs
# 	incr lob_argc
#     }
#     if { [info exists clob_files] } {
# 	set command "clob_dml_file"
# 	set lob_argv $clob_files
# 	incr lob_argc
#     }
#     if { [info exists blob_files] } {
# 	set command "blob_dml_file"
# 	set lob_argv $blob_files
# 	incr lob_argc
#     }
#     if { $lob_argc > 1 } {
# 	error "Only one of -clobs, -blobs, -clob_files, or -blob_files may be specified as an argument to db_dml"
#     }
#     db_with_handle db {
# 	if { $lob_argc == 1 } {
# 	    # Bind :1, :2, ..., :n as LOBs (where n = [llength $lob_argv])
# 	    set bind_vars [list]
# 	    for { set i 1 } { $i <= [llength $lob_argv] } { incr i } {
# 		lappend bind_vars $i
# 	    }
# 	    eval [list db_exec "${command}_bind" $db $full_statement_name $sql 2 $bind_vars] $lob_argv
# 	} else {
# 	    eval [list db_exec $command $db $full_statement_name $sql] $lob_argv
# 	}
#     }
# }

#take no chances... nstcl doesn't always return a string openacs recognizes
proc db_type {} { 
    return oracle
}

set ::pnsd::invariants(db_type) 1 
proc db_name {} { 
    return Oracle
}

set ::pnsd::invariants(db_name) 1 


proc db_bootstrap_checks { errors error_p } {

    upvar $errors my_errors
    upvar $error_p my_error_p

    foreach pool [nsv_get db_available_pools .] {
        if { [catch { set db [ns_db gethandle -timeout 15 $pool]}] || ![string compare $db ""] } {
            # This should never happened - we were able to grab a handle previously, why not now?
            append my_errors "(db_bootstrap_checks) Internal error accessing pool \"$pool\".<br>"
            set my_error_p 1
        } else { # DRB: The aD code didn't deallocate the database handle if either of the following
            # errors occured.  Boo hiss...
            if { [catch { ns_db 1row $db "select sysdate from dual" }] ||
                 [catch { db_exec_plsql exec_plsql_bind { begin :1 := 37*73; end; }  }] } {
                append my_errors "Database pool \"$pool\" has been configured with an old version of the Oracle driver.  You'll need version 2.3 or later.<br>"
                set my_error_p 1
            }
            ns_db releasehandle $db
        }
    }

    if { ![info exists my_error_p] } {
        # DRB: I've got the SQL to pick the version to drop in later...what we really want,
        # though, is Oracle's "compat version" number and I'm not sure how to get it (it is
        # reported as 8.1.0 during the Oracle installation process)
        nsv_set ad_database_version . "8.1.6"
    }
}

set ::pnsd::invariants(db_bootstrap_checks) 1 

proc ns_ora {args} {
    ns_log error "ns_ora not supported by portable.nsd yet!!!"
}


::nstcl::ad_proc  db_driverkey {{ -handle_p 0 } { dbn "" } } {
    Overload db_driverkey... we'll assume one db for now.
} { 
    return oracle
}
set ::pnsd::invariants(db_driverkey) 1 
