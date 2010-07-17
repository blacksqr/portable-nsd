package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-solid.tcl
# $Id: nstcl-database-solid.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::solid {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::solid::load_driver {arguments} {
    if {[info commands sol] != ""} then return

    if {[llength $arguments] > 2} {
        error "Too many arguments specified"
    }
   
    set shared_library [lindex $arguments 0]
    set package_name   [lindex $arguments 1]

    if {[string equal "" $shared_library]} {
        set shared_library soltcl[info sharedlibextension]
    }

    if {[string equal "" $package_name]} {
        set package_name Solid
    } 

    load [::nstcl::find_shared_library $shared_library] $package_name
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::solid::bindrow {dbhandle} {
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

::nstcl::ad_proc -private ::nstcl::database::solid::close {dbhandle} {
    set conn   [$dbhandle conn]
    set cursor [$dbhandle cursor]

    catch { sol FreeStmt $cursor drop }
    catch { sol Transact $conn rollback }
    catch { sol Disconnect $conn }
    catch { sol FreeConnect $conn }

    $dbhandle cursor {}
    $dbhandle conn {}
    return
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::solid::dbtype {} {
    return "Solid"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::solid::exec {dbhandle sql} {
    set cursor [$dbhandle cursor]
    set conn   [$dbhandle conn]
 
    # free previously allocated memory
    if {![string equal "" $cursor]} {
        catch { sol FreeStmt $cursor drop }
        set cursor ""
    }


    set SQL [string trim [string tolower $sql]]
    switch -- $SQL {
        "commit work"       - 
        "end transaction"   { sol Transact $conn commit }
        "rollback work"     -
        "abort transaction" { sol Transact $conn rollback }
    }

    switch -- $SQL {
        "begin transaction"  -
        "set autocommit off" { sol setConnectOption $conn autocommit off }
        "abort transaction"  -
        "end transaction"    -
        "set autocommit on"  { sol setConnectOption $conn autocommit on }
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


    sol AllocStmt $conn cursor
    sol Prepare $cursor $sql
    sol Execute $cursor
    sol rowCount $cursor num_rows

    if {[info exists num_rows] && $num_rows != -1} {
        catch { sol FreeStmt $cursor drop }
        set cursor ""
        set mode NS_DML

        $dbhandle num_rows $num_rows
        $dbhandle curr_row 0
    } else {
        set mode NS_ROWS
        if {![string equal [sol Fetch $cursor] SQL_SUCCESS]} {
            $dbhandle num_rows 0
            $dbhandle curr_row 0
            $dbhandle columns {}
            $dbhandle fetched {}
        } else {
            set fetched {}
            set columns {}
            sol NumResultCols $cursor num_cols

            for {set i 1} {$i <= $num_cols} {incr i} {
                sol DescribeCol $cursor $i col_info
                sol getData $cursor $i value
                lappend columns $col_info(name)
                lappend fetched $value
                unset value
            }

            $dbhandle columns $columns
            $dbhandle fetched $fetched
            $dbhandle num_rows 1
            $dbhandle curr_row 0
        }
    }

    $dbhandle cursor $cursor
    $dbhandle mode $mode
    return $mode
}
   
            

#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::solid::flush {dbhandle} {
    catch { sol FreeStmt [$dbhandle cursor] drop }

    $dbhandle cursor {}
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

::nstcl::ad_proc -private ::nstcl::database::solid::gethandle {pool dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set pass       $pools($pool,pass)

    sol AllocConnect conn
    sol Connect $conn $datasource $user $pass
    $dbhandle conn $conn
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::solid::getrow {dbhandle setId} {
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

    set num_cols 0
    $dbhandle curr_row [incr curr_row]
    foreach column [$dbhandle columns] value [$dbhandle fetched] {
        ::nstcl::ns_set put $setId $column $value
        incr num_cols
    }


    # Is there at least one more row to queue up?
    if {[string equal [sol Fetch $cursor] SQL_SUCCESS]} {
        set fetched {}

        for {set i 1} {$i <= $num_cols} {incr i} {
            sol getData $cursor $i value
            lappend fetched $value
            unset value
        }

        $dbhandle num_rows [incr num_rows]
        $dbhandle fetched $fetched
    }

    return 1
}



package provide nstcl-database-solid 1.2
