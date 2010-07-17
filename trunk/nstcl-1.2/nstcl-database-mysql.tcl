package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-mysql.tcl
# $Id: nstcl-database-mysql.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::mysql {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::mysql::load_driver {arguments} {
    if {[info commands mysqlsel] != ""} then return

    if {[llength $arguments] > 2} {
        error "Too many arguments specified"
    }
   
    set shared_library [lindex $arguments 0]
    set package_name   [lindex $arguments 1]

    if {[string equal "" $shared_library]} {
        set shared_library libmysqltcl[info sharedlibextension]
    }

    if {[string equal "" $package_name]} {
        set package_name Mysqltcl
    } 

    load [::nstcl::find_shared_library $shared_library] $package_name
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::mysql::bindrow {dbhandle} {
    set setId [::nstcl::ns_set create $dbhandle]
    
    foreach column [$dbhandle columns] {
        ::nstcl::ns_set put $setId $column ""
    }
    
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::mysql::close {dbhandle} {
    set conn [$dbhandle conn]
    catch { mysqlclose $conn } 
    return
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::mysql::dbtype {} {
    return "MySQL"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::mysql::exec {dbhandle sql} {
    set conn [$dbhandle conn]
    $dbhandle curr_row 0
    set RE "^\[ \t\r\n]*(--\[^\r\n]*\[\r\n]\[ \r\n\t]*)*select"

    if {[regexp -nocase -- $RE $sql]} {
        $dbhandle num_rows [mysqlsel $conn $sql]
        $dbhandle columns [mysqlcol $conn -current name]
        return [$dbhandle mode NS_ROWS]
    } else {
        $dbhandle num_rows [mysqlexec $conn $sql]
        $dbhandle columns {}
        return [$dbhandle mode NS_DML]
    }
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::mysql::flush {dbhandle} {
    $dbhandle fetched {}
    $dbhandle columns {}
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle mode {}
    return
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::mysql::gethandle {pool dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set password   $pools($pool,pass)

    foreach {host port db} [split $datasource :] break

    set command [list mysqlconnect]
    foreach option [list user password host port db] {
        if {![string equal "" [set $option]]} {
            lappend command -$option [set $option]
        }
    }

    $dbhandle conn [eval $command]
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::mysql::getrow {dbhandle setId} {
    set size     [::nstcl::ns_set size $setId]
    set conn     [$dbhandle conn]
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


    foreach column [$dbhandle columns] value [mysqlnext $conn] {
        ::nstcl::ns_set put $setId $column $value
    }

    $dbhandle curr_row [incr curr_row]
    return 1
}



package provide nstcl-database-mysql 1.2
