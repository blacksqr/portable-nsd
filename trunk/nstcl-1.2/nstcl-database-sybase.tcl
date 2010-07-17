package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-sybase.tcl
# $Id: nstcl-database-sybase.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::sybase {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::sybase::load_driver {arguments} {
    if {[info commands sybconnect] != ""} then return

    switch [llength $arguments] {
        0 { package require Sybtcl 3 }
        1 { load [::nstcl::find_shared_library $arguments] }
        2 { 
            load [::nstcl::find_shared_library [lindex $arguments 0]] \
                [lindex $arguments 1] 
        }
        default { error "Too many arguments specified" }
    }
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::sybase::bindrow {dbhandle} {
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

::nstcl::ad_proc -private ::nstcl::database::sybase::close {dbhandle} {
    set conn [$dbhandle conn]
    catch { sybsql $conn "rollback work" }
    catch { sybclose $conn }
    return [$dbhandle conn {}]
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::sybase::dbtype {} {
    return "Sybase"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::sybase::exec {dbhandle sql} {
    set conn    [$dbhandle conn]
    set result  [sybsql $conn $sql]
    set columns [sybcols $conn]
    
    if {$result == "NO_MORE_ROWS" && [llength $columns] == 0} {
        set num_rows 0
        set mode NS_DML
    } else {
        switch -- $result {
            REG_ROW      { set num_rows 1 }
            NO_MORE_ROWS { set num_rows 0 }
            default      { error "Unexpected result from sybtcl: \"$result\"" }
        } 
        set mode NS_ROWS
    }

    $dbhandle curr_row 0
    $dbhandle num_rows $num_rows
    $dbhandle columns $columns
    $dbhandle mode $mode
    $dbhandle prefetched [sybnext $conn]

    return $mode
}


#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::sybase::flush {dbhandle} {
    catch { sybcancel [$dbhandle conn] }
    $dbhandle exception_code {}
    $dbhandle exception_text {}
    $dbhandle num_rows {}
    $dbhandle curr_row {}
    $dbhandle prefetched {}
    $dbhandle mode {}
}



#
# ... gethandle
#

::nstcl::ad_proc -private ::nstcl::database::sybase::gethandle {pool 
                                                                dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set pass       $pools($pool,pass)

    set datasource  [split  $datasource :]
    set server      [lindex $datasource 0]
    set database    [lindex $datasource 1]
    set application [lindex $datasource 2]

    if {[string equal $server ""]} {
        switch -- [info exists ::env(DSQUERY)] {
            0 { set server SYBASE }
            1 { set server $::env(DSQUERY) }
        }
    }

    set conn [sybconnect $user $pass $server $application]

    if {![string equal $database ""]} {
        sybuse $conn $database
    }

    $dbhandle conn $conn
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::sybase::getrow {dbhandle setId} {
    set size     [::nstcl::ns_set size $setId]
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

    foreach column [$dbhandle columns] value [$dbhandle prefetched] {
        ::nstcl::ns_set put $setId $column $value
    }


    $dbhandle prefetched [sybnext [$dbhandle conn]]
    $dbhandle curr_row   [incr curr_row]

    if {[llength [$dbhandle prefetched]]} {
        $dbhandle num_rows [incr num_rows]
    }

    return 1
}



package provide nstcl-database-sybase 1.2
