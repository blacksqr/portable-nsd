package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-odbc.tcl
# $Id: nstcl-database-odbc.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::odbc {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::odbc::load_driver {arguments} {
    package require tclodbc
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::odbc::bindrow {dbhandle} {
    set cursor [$dbhandle cursor]
    set setId  [::nstcl::ns_set create $cursor]
    
    foreach column [$dbhandle columns] {
        ::nstcl::ns_set put $setId $column ""
    }
    
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::odbc::close {dbhandle} {
    set conn   [$dbhandle conn]
    set cursor [$dbhandle cursor]

    catch { $cursor drop }
    catch { $conn disconnect } 

    $dbhandle cursor {}
    $dbhandle conn {}
    return
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::odbc::dbtype {} {
    return "ODBC"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::odbc::exec {dbhandle sql} {
    set cursor [$dbhandle cursor]
    set conn   [$dbhandle conn]
 
    # free previously allocated memory
    if {![string equal "" $cursor]} {
        $dbhandle fetched {}
        $dbhandle columns {}
        catch { $cursor drop }
        set cursor ""
    }


    set SQL [string trim [string tolower $sql]]
    switch -- $SQL {
        "commit work"       -
        "end transaction"   { $conn commit }
        "rollback work"     -
        "abort transaction" { $conn rollback }
    }
 
    switch -- $SQL {
        "begin transaction"  -
        "set autocommit off" { $conn set autcommit off } 
        "abort transaction"  -
        "end transaction"    -
        "set autocommit on"  { $conn set autocommit on }
    }
 
    switch -- $SQL {
        "begin transaction"  -
        "end transaction"    -
        "rollback work"      -
        "abort transaction"  -
        "set autocommit off" -
        "set autocommit on"  {
            $dbhandle num_rows 0
            $dbhandle curr_row 0
            $dbhandle mode NS_DML
            return NS_DML
        }
    }


    set cursor [$conn statement $conn-cursor $sql]
    $cursor execute
    $dbhandle columns [$cursor columns]

    if {[llength [$dbhandle columns]]} {
        $dbhandle curr_row 0
        $dbhandle fetched [$cursor fetch]

        if {[llength [$dbhandle fetched]] == 0} {
            $dbhandle num_rows 0
        } else {
            # odbc isn't gauranteed to report the number of rows so we fake it
            $dbhandle num_rows 1
        }

        $dbhandle mode NS_ROWS
    } else {
        $dbhandle curr_row 0
        $dbhandle num_rows [$cursor rowcount]
        $dbhandle mode NS_DML
    }

    $dbhandle cursor $cursor
    return [$dbhandle mode]
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::odbc::flush {dbhandle} {
    catch { [$dbhandle cursor] drop }

    $dbhandle cursor {}
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle columns {}
    $dbhandle fetched {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::odbc::gethandle {pool dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set pass       $pools($pool,pass)
    set conn ::nstcl::database::odbc::conn-[namespace tail $dbhandle]

    if {[string equal $user ""] && [string equal $pass ""]} {
        set conn [database connect $conn $datasource]
    } else {
        set conn [database connect $conn $datasource $user $pass]
    }

    $dbhandle conn $conn
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::odbc::getrow {dbhandle setId} {
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

    if {[::nstcl::ns_set size $setId]} {
        ::nstcl::ns_set truncate $setId 0
    }

    foreach column [$dbhandle columns] value [$dbhandle fetched] {
        ::nstcl::ns_set put $setId $column $value
    }

    $dbhandle curr_row [incr curr_row]

    set fetched [$cursor fetch]
    if {[llength $fetched]} {
        $dbhandle fetched $fetched
        $dbhandle num_rows [incr num_rows]
    } 

    return 1
}



package provide nstcl-database-odbc 1.2
