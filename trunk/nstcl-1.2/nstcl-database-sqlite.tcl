package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-sqlite.tcl
# $Id: nstcl-database-sqlite.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::sqlite {
    variable nhandles 0
}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::sqlite::load_driver {arguments} {
    if {[catch { package require sqlite 2.0 }]} {
        load [::nstcl::find_shared_library tclsqlite]
    }
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::bindrow {dbhandle} {
    set setId  [::nstcl::ns_set create [$dbhandle sql]]
    
    foreach key [$dbhandle keys] {
        ::nstcl::ns_set put $setId $key ""
    }
    
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::close {dbhandle} {
    [$dbhandle conn] close
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::dbtype {} {
    return "SQLite"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::exec {dbhandle sql} {
    set cursor [$dbhandle cursor]
    set conn   [$dbhandle conn]
 
    # free previously allocated memory
    if {![string equal "" $cursor]} {
        ::nstcl::database::sqlite::flush $dbhandle
    }

    array set values {}
    set counter 0
    set keys {}
    set rows {}


    # This is a hack-ish way of determining whether or not $sql is DDL/DML.
    # We have to do this test before we eval $sql because if it is DDL
    # (like a create or drop table), the explain would fail (because
    # the table would have already been created or dropped, etc.)
    set mode NS_ROWS
    foreach {addr opcode p1 p2 p3} [$conn eval "explain $sql"] {
        if {$opcode == "OpenWrite"} {
            set mode NS_DML
            break
        }
    }

    set error_p [catch {
        $conn eval $sql values {
            if {[incr counter] == 1} {
                set keys $values(*)
            }

            set row {}
            foreach column $values(*) {
                lappend row $values($column)
            }

            lappend rows $row
        }
    } error]
    
    if {$error_p} {
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

    $dbhandle curr_row 0
    $dbhandle num_rows $counter
    $dbhandle keys $keys
    $dbhandle cursor $rows

    return [$dbhandle mode $mode]
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::flush {dbhandle} {
    $dbhandle cursor {}
    $dbhandle keys {}
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::gethandle {pool 
                                                                dbhandle} {
    variable nhandles
    upvar 0 ::nstcl::database::pools pools
    set poolname [$dbhandle pool]

    set cmd ::nstcl::database::sqlite::sqlite[incr nhandles]
    sqlite $cmd $pools($poolname,datasource)
    $dbhandle conn $cmd
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::sqlite::getrow {dbhandle setId} {
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

    foreach key [$dbhandle keys] val [lindex [$dbhandle cursor] $curr_row] {
            ::nstcl::ns_set put $setId $key $val
    }

    $dbhandle curr_row [incr curr_row]
    return 1
}



package provide nstcl-database-sqlite 1.2
