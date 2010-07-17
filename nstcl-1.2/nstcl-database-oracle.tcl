package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-database

# nstcl-1.2/nstcl-database-oracle.tcl
# $Id: nstcl-database-oracle.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::database::oracle {}



#
# ... load_driver
#

::nstcl::ad_proc ::nstcl::database::oracle::load_driver {arguments} {
    if {[info commands oralogon] == ""} {
        switch [llength $arguments] {
            0 { 
                  if {[catch { package require Oratcl 4 }]} {
                      package require Oratcl 3
                  }
              }
            1 { load [::nstcl::find_shared_library $arguments] }
            2 { 
                load [::nstcl::find_shared_library [lindex $arguments 0]] \
                    [lindex $arguments 1] 
            }
            default { error "Too many arguments specified" }
        }
    }


    # Oratcl 4 is not completely backwards compatible with Oratcl 3
    variable oratcl_ver [lindex [split [package provide Oratcl] .] 0]

    if {![string is integer -strict $oratcl_ver]} {
        switch -- [info commands oramsg] {
            oramsg  { set oratcl_ver 4 }
            default { set oratcl_ver 3 }
        }
    }
}



#
# ... bindrow
#

::nstcl::ad_proc -private ::nstcl::database::oracle::bindrow {dbhandle} {
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

::nstcl::ad_proc -private ::nstcl::database::oracle::close {dbhandle} {
    set conn   [$dbhandle conn]
    set cursor [$dbhandle cursor]

    catch { oraclose $cursor }
    catch { oraroll $conn }
    catch { oralogoff $conn }

    $dbhandle cursor {}
    $dbhandle conn {}
    return
}



#
# ... dbtype
#

::nstcl::ad_proc -private ::nstcl::database::oracle::dbtype {} {
    return "Oracle"
}



#
# ... exec
#

::nstcl::ad_proc -private ::nstcl::database::oracle::exec {dbhandle sql} {
    variable oratcl_ver

    set cursor [$dbhandle cursor]
    set conn   [$dbhandle conn]
 
    # free previously allocated memory
    if {![string equal "" $cursor]} {
        $dbhandle orafetched {}
        $dbhandle columns {}
        catch { oraclose $cursor }
        set cursor ""
    }


    set RE {^\s*(begin|end|abort)\s+transaction\s*$}
    if {[regexp -nocase -- $RE [string tolower $sql] => action]} {
        switch -- $action {
            end   { oracommit $conn }
            abort { oraroll $conn }
        }

        if {$action == "begin"} {
            oraautocom $conn 0
        } else {
            oraautocom $conn 1
        }

        $dbhandle curr_row 0
        $dbhandle num_rows 0
        $dbhandle mode NS_DML
    } else {
        set cursor [oraopen $conn]
        orasql $cursor $sql

        if {$oratcl_ver <= 3} {
            global oramsg
        } else {
            set oramsg(rc)   [oramsg $cursor rc]
            set oramsg(rows) [oramsg $cursor rows]
        }

        if {[llength [oracols $cursor name]]} {
            $dbhandle curr_row 0

            if {$oratcl_ver <= 3} {
                $dbhandle orafetched [orafetch $cursor]
            } else {
                set orafetched {}
                set oramsg(rc) [orafetch $cursor -datavariable orafetched]
                $dbhandle orafetched $orafetched
            }

            $dbhandle columns [string tolower [oracols $cursor name]]

            if {$oramsg(rc) == 1403} {
                $dbhandle num_rows 0
            } else {
                # Oratcl doesn't report the number of rows so we fake it
                $dbhandle num_rows 1
            }

            $dbhandle mode NS_ROWS
        } else {
            $dbhandle curr_row 0
            $dbhandle num_rows $oramsg(rows)
            $dbhandle mode NS_DML
        }
    }

    $dbhandle cursor $cursor
    return [$dbhandle mode]
}



#
# ... flush
#

::nstcl::ad_proc -private ::nstcl::database::oracle::flush {dbhandle} {
    catch { oraclose [$dbhandle cursor] }

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

::nstcl::ad_proc -private ::nstcl::database::oracle::gethandle {pool 
                                                                dbhandle} {
    upvar 0 ::nstcl::database::pools pools
    set datasource $pools($pool,datasource)
    set user       $pools($pool,user)
    set pass       $pools($pool,pass)

    set connect_string "$user/$pass"    
    if {![string equal "" $datasource]} {
        append connect_string "@$datasource"
    }

    set conn [oralogon $connect_string]
    oraautocom $conn 1
    $dbhandle conn $conn
}



#
# ... getrow
#

::nstcl::ad_proc -private ::nstcl::database::oracle::getrow {dbhandle setId} {
    variable oratcl_ver

    set size     [::nstcl::ns_set size $setId]
    set cursor   [$dbhandle cursor]
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
        $dbhandle curr_row [incr curr_row]
        return 0
    }

    if {$size} {
        ::nstcl::ns_set truncate $setId 0
    }

    foreach column [$dbhandle columns] value [$dbhandle orafetched] {
        ::nstcl::ns_set put $setId $column $value
    }


    if {$oratcl_ver <= 3} {
        global oramsg
        $dbhandle orafetched [orafetch $cursor]
    } else {
        set orafetched {}
        set oramsg(rc) [orafetch $cursor -datavariable orafetched]
        $dbhandle orafetched $orafetched
    }

    if {$oramsg(rc) != 1403} {
        $dbhandle num_rows [incr num_rows]
    } 

    $dbhandle curr_row [incr curr_row]
}



package provide nstcl-database-oracle 1.2
