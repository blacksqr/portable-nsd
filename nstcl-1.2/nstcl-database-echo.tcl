package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-echo.tcl
# $Id: nstcl-database-echo.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::echo {
    variable data
    array set data {}
}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::echo::load_driver {arguments} {}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::echo::bindrow {dbhandle} {
    variable data
    set setId  [::nstcl::ns_set create $dbhandle]
    
    foreach key [lindex $data($dbhandle) 0] {
        ::nstcl::ns_set put $setId $key ""
    }
    
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::echo::close {dbhandle} {
    variable data

    if {[info exists data($dbhandle)]} {
        unset data($dbhandle)
    }
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::echo::dbtype {} {
    return "Echo"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::echo::exec {dbhandle sql} {
    variable data

    set stack ""
    for {set i [info level]} {$i > 0} {incr i -1} {
        append stack [info level $i]\n
    }
 
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle curr_row 0

    if {[string first dml $stack] != -1 || [string equal "MAGIC DML" $sql]} {
        $dbhandle mode NS_DML
        $dbhandle num_rows 0
    } else {
        $dbhandle mode NS_ROWS
        $dbhandle num_rows [expr {[llength $sql] - 1}]
        set data($dbhandle) $sql
    }

    return [$dbhandle mode]
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::echo::flush {dbhandle} {
    variable data

    if {[info exists data($dbhandle)]} {
        unset data($dbhandle)
    }

    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::echo::gethandle {pool dbhandle} {
    $dbhandle conn $dbhandle
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::echo::getrow {dbhandle setId} {
    variable data

    set size [::nstcl::ns_set size $setId]
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

    $dbhandle curr_row [incr curr_row]

    foreach key [lindex $data($dbhandle) 0] \
            val [lindex $data($dbhandle) $curr_row] {
        ::nstcl::ns_set put $setId $key $val
    }

    return 1
}



package provide nstcl-database-echo 1.2
