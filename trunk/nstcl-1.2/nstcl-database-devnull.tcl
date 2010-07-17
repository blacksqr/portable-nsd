package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-devnull.tcl
# $Id: nstcl-database-devnull.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::devnull {
    variable conn_counter 0
}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::devnull::load_driver {arguments} {
    # appropriately, there are no commands to load the /dev/null driver :^)
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::devnull::bindrow {dbhandle} {
    set cursor [$dbhandle cursor]
    set setId  [::nstcl::ns_set create $cursor]
    return $setId
}



#
# ... close
#

::nstcl::ad_proc -private ::nstcl::database::devnull::close {dbhandle} {
    $dbhandle conn {}
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::devnull::dbtype {} {
    return "devnull"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::devnull::exec {dbhandle sql} {
    set conn [$dbhandle conn]
    
    set stack ""
    for {set i [info level]} {$i > 0} {incr i -1} {
        append stack [info level $i]\n
    }

    if {[string first dml $stack] != -1} {
        $dbhandle mode NS_DML
    } else {
        $dbhandle mode NS_ROWS
    }
 
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows 0
    $dbhandle curr_row 0

    return [$dbhandle mode]
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::devnull::flush {dbhandle} {
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::devnull::gethandle {pool 
                                                                 dbhandle} {
    variable conn_counter
    $dbhandle conn [incr conn_counter]
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::devnull::getrow {dbhandle setId} {
    if {[::nstcl::ns_set size $setId]} {
        ::nstcl::ns_set truncate $setId 0
    }

    set num_rows [$dbhandle num_rows]
    set curr_row [$dbhandle curr_row]

    if {$num_rows > 0 && $curr_row > $num_rows} {
        return -code error "Database operation \"getrow\" failed"
    }

    $dbhandle curr_row [expr {$curr_row + 1}]


    if {$num_rows == 0 || $curr_row == $num_rows} {
        return 0
    } else {
        return 1
    }
}



package provide nstcl-database-devnull 1.2
