package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-postgres.tcl
# $Id: nstcl-database-postgres.tcl,v 1.6 2003/08/05 01:12:45 cleverly Exp $
#
# nstcl -- AOLserver/OpenNSD routines for tclsh
#
#     Copyright (c) 2000, 2001, 2002 Michael A. Cleverly
#     
#     Permission is hereby granted, free of charge, to any person obtaining
#     a copy of this software and associated documentation files (the
#     "Software"), to deal in the Software without restriction, including
#     without limitation the rights to use, copy, modify, merge, publish,
#     distribute, sublicense, and/or sell copies of the Software, and to
#     permit persons to whom the Software is furnished to do so, subject to
#     the following conditions:
#     
#     The above copyright notice and this permission notice shall be
#     included in all copies or substantial portions of the Software.
#     
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Author Contact Information:
#
#     Michael A. Cleverly
#     1448 W Pebblecreek Dr
#     Layton, Utah 84041-8112
# 
#     michael@cleverly.com
#     http://michael.cleverly.com
#
# nstcl home page: http://nstcl.sourceforge.net


namespace eval ::nstcl::database::postgres {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::postgres::load_driver {arguments} {
    if {[info commands pg_connect] != ""} then return

    foreach {library name} $arguments break
    if {![info exists library] || [string equal "" $library]} {
        set library libpgtcl.so
    }

    if {![info exists name] || [string equal "" $name]} {
        load [::nstcl::find_shared_library $library]
    } else {
        load [::nstcl::find_shared_library $library $name]
    }
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::postgres::bindrow {dbhandle} {
    set cursor [$dbhandle cursor]
    set setId  [::nstcl::ns_set create $cursor]
    
    foreach attribute [pg_result $cursor -attributes] {
        ::nstcl::ns_set put $setId $attribute ""
    }
    
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::postgres::close {dbhandle} {
    pg_disconnect [$dbhandle conn]
    $dbhandle conn {}
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::postgres::dbtype {} {
    return "PostgreSQL"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::postgres::exec {dbhandle sql} {
    set cursor [$dbhandle cursor]
    set conn   [$dbhandle conn]
 
    # free previously allocated memory
    if {![string equal "" $cursor]} {
        catch { pg_result $cursor -clear }
        $dbhandle cursor {}
    }

    set cursor [pg_exec $conn $sql]
    set error  [pg_result $cursor -error]
   
    if {![string equal "" $error]} {
        pg_result $cursor -clear
        $dbhandle exception_code NSDB
        $dbhandle exception_text $error
        $dbhandle num_rows {}
        $dbhandle curr_row {}
        $dbhandle mode ERROR
        return -code error $error
    } else {
        $dbhandle exception_code {}
        $dbhandle exception_text {}
    }

    if {[pg_result $cursor -numAttrs]} {
        $dbhandle num_rows [pg_result $cursor -numTuples]
        $dbhandle curr_row 0
        $dbhandle cursor $cursor
        $dbhandle mode NS_ROWS
        return NS_ROWS
    } else {
        pg_result $cursor -clear
        $dbhandle num_rows 0
        $dbhandle curr_row 0
        $dbhandle mode NS_DML
        return NS_DML
    }
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::postgres::flush {dbhandle} {
    catch { pg_result [$dbhandle cursor] -clear }

    $dbhandle cursor {}
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::postgres::gethandle {pool 
                                                                  dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set pass       $pools($pool,pass)
    
    if {[string equal "" $datasource]} {
        set datasource ::
    }

    foreach {host port dbname} [split $datasource :] break
    

    set conninfo [list "dbname=$dbname"]
    if {[string length $host]} { lappend conninfo "host=$host" }
    if {[string length $port]} { lappend conninfo "port=$port" }
    if {[string length $user]} { lappend conninfo "user=$user" }
    if {[string length $pass]} { lappend conninfo "password=$pass" }

    # override the default of ::nstcl::database::pseudo_bind_variables
    # since Postgres has a non-standard escape of \ (i.e. \' along w/ '')
    $dbhandle bind_vars ::nstcl::database::postgres::pg_bind_vars

    $dbhandle conn [pg_connect -conninfo $conninfo]
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::postgres::getrow {dbhandle setId} {
    set size     [::nstcl::ns_set size $setId]
    set cursor   [$dbhandle cursor]
    set num_rows [$dbhandle num_rows]
    set curr_row [$dbhandle curr_row]

    if {$num_rows > 0 && $curr_row > $num_rows} {
        return -code error "Database operation \"getrow\" failed"
    }

    if {$num_rows == 0} {
        return 0
    }

    if {$num_rows == $curr_row} {
        return 0
    }
    
    if {$size} {
        ::nstcl::ns_set truncate $setId 0
    }

    pg_result $cursor -tupleArray $curr_row tupleArray
    foreach key [lsort -dictionary [array names tupleArray]] {
        ::nstcl::ns_set put $setId $key $tupleArray($key)
    }

    $dbhandle curr_row [incr curr_row]
    return 1
}


#
# pg_bind_vars: a custom pseudo_bind_variables
#

::nstcl::ad_proc -private ::nstcl::database::postgres::pg_bind_vars {sql} {
    uplevel 1 [list ::nstcl::database::pseudo_bind_variables $sql 1]
} 



package provide nstcl-database-postgres 1.2
