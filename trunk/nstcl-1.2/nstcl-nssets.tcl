package require nstcl-core
package require nstcl-fwdcompat

# nstcl-1.2/nstcl-nssets.tcl
# $Id: nstcl-nssets.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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

namespace eval ::nstcl {
    namespace export ns_set \
                     ad_ns_set_to_tcl_vars \
                     set_variables_after_query \
                     set_variables_after_subquery \
                     ns_config \
                     ns_configsection \
                     ns_configsections \
                     ns_param \
                     ns_section
}

namespace eval ::nstcl::nssets {
    variable counter -1
    variable section ""
    variable commands

    array set commands {
        copy     { persist 1 setid 1 min 1 max 1 syntax "?-persist? setId" }
        cput     { persist 0 setid 1 min 3 max 3 syntax "setId key value" }
        create   { persist 1 setid 0 min 0 max 1 syntax "?-persist? ?name?" }
        delete   { persist 0 setid 1 min 2 max 2 syntax "setId index" }
        delkey   { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        find     { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        free     { persist 0 setid 1 min 1 max 1 syntax "setId" }
        get      { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        icput    { persist 0 setid 1 min 3 max 3 syntax "setId key value" }
        idelkey  { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        ifind    { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        iget     { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        isnull   { persist 0 setid 1 min 2 max 2 syntax "setId index" }
        iunique  { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        key      { persist 0 setid 1 min 2 max 2 syntax "setId index" }
        merge    { persist 0 setid 2 min 2 max 2 syntax "high low" }
        move     { persist 0 setid 2 min 2 max 2 syntax "to from" }
        name     { persist 1 setid 1 min 1 max 1 syntax "setId" }
        new      { persist 1 setid 0 min 0 max 1 syntax "?-persist? ?name?" }
        print    { persist 0 setid 1 min 1 max 1 syntax "setId" }
        put      { persist 0 setid 1 min 3 max 3 syntax "setId key value" }
        size     { persist 0 setid 1 min 1 max 1 syntax "setId" }
        split    { persist 1 setid 1 min 1 max 2 syntax "?-persist? setId\
                                                         ?splitChar?" }
        truncate { persist 0 setid 1 min 2 max 2 syntax "setId size" }
        unique   { persist 0 setid 1 min 2 max 2 syntax "setId key" }
        update   { persist 0 setid 1 min 3 max 3 syntax "setId key value" }
        value    { persist 0 setid 1 min 2 max 2 syntax "setId index" }
    }
}



#
# [ns_set]
#

::nstcl::ad_proc ::nstcl::ns_set {args} {
    syntax  "<command>::nstcl::ns_set</command> <m>option</m> <m>arg</m> ?<m>...</m>?"
    summary "Manipulate sets of key/value pairs"
    description {
        <p>This command manipulates sets of key/value pairs.</p>

        <commandlist{>
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>copy</method> 
                    ?<option>-persist</option>? <m>setId</m>
                </command}>
        
                <desc{>
                    Returns a new set that has the same name and key/value 
                    pairs as the given <i>setId</i>.  Since <b>nstcl</b> has 
                    no concept of a "transaction" or "connection" 
                    <method>-persist</method> is a no-op, and is included 
                    strictly for source compatability.
                </desc}>
            </commanddef}>
        
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>cput</method> <m>setId</m> 
                    <m>key</m> <m>value</m>
                </command}>
                <desc{>
                    Conditionally appends a new field to the <i>setId</i> set 
                    if a field with the same <i>key</i> does not already exist
                    in the set.  Returns the field number of the new field 
                    (if added), or the field number of the existing 
                    <i>key</i>.
                </desc}>
            </commanddef}>
        
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>create</method> 
                    ?<option>-persist</option>? <m>name</m>
                </command}>
                <desc{>
                    Creates a new set and returns the <i>setId</i>.  Since 
                    <b>nstcl</b> has no concept of a "transaction" or 
                    "connection" <method>-persist</method> is a no-op, and 
                    is included strictly for source compatability.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>delete</method> <m>setId</m> 
                    <m>fieldNumber</m>
                </command}>
                <desc{>
                    Deletes field number <i>fieldNumber</i> from the 
                    <i>setId</i> set.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>delkey</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>
                    Delete the first occurence (only) of <i>key</i> in 
                    the <i>setId</i> set.  If the <i>key</i> does not exist 
                    the contents of <i>setId</i> are not changed, and no 
                    error is raised.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>find</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>
                    Searches the <i>setId</i> set and returns the field number
                    of the first instance of <i>key</i>.  Fields are numbered
                    beginning at position 0 (like list indexes in Tcl).  If 
                    an instance of <i>key</i> is not found then -1 is 
                    returned.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>free</method> <m>setId</m>
                </command}>
                <desc{>
                    Deletes the <i>setId</i> set (thus freeing the memory 
                    used).  <b>nstcl</b> never explicitly frees sets (like 
                    AOLserver does) since <b>nstcl</b> does not have any 
                    notion of a "transaction".  All sets persist until 
                    explicity freed.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>get</method> <m>setId</m> <m>key</m>
                </command}>
                <desc{>
                    Returns the value of the first instance of <i>key</i> in
                    the <i>setId</i> set.  If <i>key</i> is not contained in
                    the <i>setId</i> set an empty string is returned.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>icput</method> <m>setId</m> 
                    <m>key</m> <m>value</m>
                </command}>
                <desc{>
                    Case-insensitive version of <method>cput</method>.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>idelkey</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>
                    Case-insensitive version of <method>delkey</method>.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>ifind</method> <m>setId</m> <m>key</m>
                </command}>
                <desc{>
                    Case-insensitive version of <method>find</method>.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>iget</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>Case-insensitive version of <method>get</method>.</desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>isnull</method> <m>setId</m> 
                    <m>fieldNumber</m>
                </command}>
                <desc{>
                    Returns a 0 or a 1 depending on whether <i>fieldNumber</i>
                    in the <i>setId</i> set is null.  The empty string is not 
                    considered to be a null value, nor is the string with an 
                    ascii value of 0.  This command is included for 
                    compatability with AOLserver only (whose Tcl API has no 
                    way to set a field to be null).  Hence, the <b>nstcl</b> 
                    version of this command will always return 0.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>iunique</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>
                    Case-insensitive version of <method>unique</method>
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>key</method> <m>setId</m> 
                    <m>fieldNumber</m>
                </command}>
                <desc{>
                    Returns the name of the <i>fieldNumber</i> key in the 
                    <i>setId</i> set.  Sets are indexed beginning at position 
                    0.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>merge</method> <m>high</m> <m>low</m>
                </command}>
                <desc{>
                    Any key/value pairs from the <i>low</i> set are appended
                    to the <i>high</i> set if the same key does not already
                    exist in the <i>high</i> set.  The <i>low</i> set is
                    not modified in any way.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>move</method> <m>to</m> <m>from</m>
                </command}>
                <desc{>
                    Moves all the key/value pairs from <i>from</i> to 
                    <i>to</i>, leaving <i>from</i> as a valid (but completely 
                    empty) set.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>name</method> <m>setId</m>
                </command}>
                <desc{>
                    Returns the name of the <i>setId</i> set.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>new</method> 
                    ?<option>-persist</option>? <m>name</m>
                </command}>
                <desc{>Synonym for <method>create</method></desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>print</method> <m>setId</m>
                </command}>
                <desc{>
                    Pretty print the key/value pairs in the <i>setId</i> set 
                    (similar to Tcl's <b>parray</b> command for arrays).  
                    The AOLserver version of this command "prints" to the 
                    server log; the <b>nstcl</b> version returns a string.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>put</method> <m>setId</m> 
                    <m>key</m> <m>value</m>
                </command}>
                <desc{>
                    Appends a new <i>key</i>/<i>value</i> pair to the 
                    <i>setId</i> set and returns the new field number.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>size</method> <m>setId</m>
                </command}>
                <desc{>
                    Returns the number of key/value pairs in the <i>setId</i> 
                    set.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>split</method> 
                    ?<option>-persist</option>? <m>setId</m> 
                    ?<m>splitChar</m>?
                </command}>
                <desc{>
                    <p>Splits one set into multiple new sets based on the 
                    <i>splitChar</i>, and returns a Tcl list of the 
                    newly-allocated sets.  The default <i>splitChar</i>, 
                    if not specified, is a period (.).</p>

                    <dle>
                    <dt>From the AOLserver documentation:</dt>
                    <dd><p>For example, if two fields in the original set 
                    have "dog.food" and "cat.food" as their key names and 
                    "Yummy dog food!" and "Yummy cat food!" as their values, 
                    <b>ns_set split</b> would return two new sets named 
                    "dog" and "cat". The dog set would have a single field 
                    whose key is "food" and whose value is "Yummy dog food!".
                    The cat set would have a single field whose key is "food"
                    and whose value is "Yummy cat food!".</p></dd>
                    </dle>

                    <p>The <option>-persist</option> is a no-op in 
                    <b>nstcl</b>.</p>
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>truncate</method> <m>setId</m> 
                    size
                </command}>
                <desc{>
                    Trunaces the <i>setId</i> set to the specified 
                    <i>size</i> and frees the memory used by the truncated 
                    key/value pairs.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>unique</method> <m>setId</m> 
                    <m>key</m>
                </command}>
                <desc{>
                    Returns a 1 if the specified <i>key</i> is unique 
                    within the <i>setId</i> set, and 0 otherwise.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>update</method> <m>setId</m> 
                    <m>key</m> <m>value</m>
                </command}>
                <desc{>
                    Updates the first instance of <i>key</i> in the 
                    <i>setId</i> set with a new <i>value</i>, or appends 
                    the <i>key</i>/<i>value</i> pair if <i>key</i> does not 
                    already exist within the <i>setId</i> set.
                </desc}>
            </commanddef}>
            
            <commanddef{>
                <command{>
                    <b>ns_set</b> <method>value</method> <m>setId</m> 
                    <m>fieldNumber</m>
                </command}>
                <desc{>
                    Returns the value of the <i>fieldNumber</i> key in 
                    the <i>setId</i> set.  Sets are indexed beginning at 
                    position 0.
                </desc}>
            </commanddef}>
            
        </commandlist}>
    }
} {
    upvar 0 ::nstcl::nssets::commands commands
    set argc [llength $args]

    if {$argc == 0} {
        return -code error "wrong # of args: should be \"ns_set command\
                ?args?\""
    } elseif {$argc == 1 && ($args != "new" && $args != "create")} {
        return -code error "wrong # of args: should be \"ns_set $args\
            setId ?args?\""
    } else {
        set cmd  [lindex $args 0]
        set args [lrange $args 1 end]
        incr argc -1
    }


    if {![info exists commands($cmd)]} {
        return -code error "unknown command \"$cmd\": should be one of\
            [join [lrange [lsort [array names commands]] 0 end-1] ", "] or\
            [lindex [lsort [array names commands]] end]"
    }


    array set command $commands($cmd)
    if {$command(persist)} {
        # AOLserver would have a notion of connection vs persistent
        # but we don't, so we'll ignore the flag (for compatability)
        if {[string equal [lindex $args 0] "-persist"]} {
            # ignore the -persist flag
            set args [lrange $args 1 end]
            incr argc -1

            # if -persist was the only argument we decrement once more
            # so that we'll throw a wrong # args error instead of 
            # invalid set ""
            if {$argc == 0 && $cmd != "new" && $cmd != "create"} {
                incr argc -1
            }
        }
    }


    if {$command(setid) && $argc >= 0} {
        set id [lindex $args 0]
        if {(![regexp {^t[1-9][0-9]*$} $id] && ![string equal $id "t0"]) ||
            ![info exists ::nstcl::nssets::$id]} {
            return -code error "invalid set id: \"$id\""
        }
    }

    if {$command(setid) == 2 && $argc >= 2} {
        set id [lindex $args 1]
        if {(![regexp {^t[1-9][0-9]*$} $id] && ![string equal $id "t0"]) ||
            ![info exists ::nstcl::nssets::$id]} {
            return -code error "invalid set id: \"$id\""
        }
    }


    if {$argc < $command(min) || $argc > $command(max)} {
        return -code error "wrong # of args: should be \"ns_set $cmd\
            $command(syntax)\""
    }


    switch -- $argc {
        0 { set error_p [catch { ::nstcl::nssets::$cmd } result] }

        1 { set error_p [catch { ::nstcl::nssets::$cmd \
                                      [lindex $args 0] } result]
          }

        2 { set error_p [catch { ::nstcl::nssets::$cmd \
                                      [lindex $args 0] \
                                      [lindex $args 1] } result] 
          }

        3 { set error_p [catch {::nstcl::nssets::$cmd \
                                     [lindex $args 0] \
                                     [lindex $args 1] \
                                     [lindex $args 2] } result]
        }
    }

    if {$error_p} {
        return -code error $result
    } else {
        return $result
    }
}


#
# [ns_set copy]
#
::nstcl::ad_proc -private ::nstcl::nssets::copy {id} {
    set copy [::nstcl::nssets::create]
    upvar 0 ::nstcl::nssets::$id   old
    upvar 0 ::nstcl::nssets::$copy new
    array set new [array get old]
    return $copy
}


#
# [ns_set cput]
#
::nstcl::ad_proc -private ::nstcl::nssets::cput {id key value} {
    set index [::nstcl::nssets::find $id $key]
    if {$index == -1} {
        upvar 0 ::nstcl::nssets::$id setId
        set index [llength $setId(keys)]
        lappend setId(keys) $key
        lappend setId(vals) $value
    }
    return $index
}


#
# [ns_set create]
#
::nstcl::ad_proc -private ::nstcl::nssets::create {{name ""}} {
    variable counter
    set id t[incr counter]
    array set ::nstcl::nssets::$id [list keys {} vals {} name $name]
    return $id
}



#
# [ns_set delete]
#
::nstcl::ad_proc -private ::nstcl::nssets::delete {id index} {
    assert_valid_ndx $id $index
    upvar 0 ::nstcl::nssets::$id setId
    set setId(keys) [lreplace $setId(keys) $index $index]
    set setId(vals) [lreplace $setId(vals) $index $index]
    return
}


#
# [ns_set delkey]
#
::nstcl::ad_proc -private ::nstcl::nssets::delkey {id key} {
    set index [::nstcl::nssets::find $id $key]
    if {$index != -1} {
        ::nstcl::nssets::delete $id $index
    }
}


#
# [ns_set find]
#
::nstcl::ad_proc -private ::nstcl::nssets::find {id key} {
    upvar 0 ::nstcl::nssets::$id setId
    return [lsearch -exact $setId(keys) $key]
}


#
# [ns_set free]
#
::nstcl::ad_proc -private ::nstcl::nssets::free {id} {
    unset ::nstcl::nssets::$id
}


#
# [ns_set get]
#
::nstcl::ad_proc -private ::nstcl::nssets::get {id key} {
    set index [::nstcl::nssets::find $id $key]
    if {$index != -1} {
        upvar 0 ::nstcl::nssets::$id setId
        return [lindex $setId(vals) $index]
    }
}


#
# [ns_set icput]
#
::nstcl::ad_proc -private ::nstcl::nssets::icput {id key value} {
    set index [::nstcl::nssets::ifind $id $key]
    if {$index == -1} {
        upvar 0 ::nstcl::nssets::$id setId
        set index [llength $setId(keys)]
        lappend setId(keys) $key
        lappend setId(vals) $value
    }
    return $index
}


#
# [ns_set idelkey]
#
::nstcl::ad_proc -private ::nstcl::nssets::idelkey {id key} {
    set index [::nstcl::nssets::ifind $id $key]
    if {$index != -1} {
        ::nstcl::nssets::delete $id $index
    }
}


#
# [ns_set ifind]
#
::nstcl::ad_proc -private ::nstcl::nssets::ifind {id key} {
    upvar 0 ::nstcl::nssets::$id setId
    return [lsearch -exact [string tolower $setId(keys)] [string tolower $key]]
}


#
# [ns_set iget]
#
::nstcl::ad_proc -private ::nstcl::nssets::iget {id key} {
    set index [::nstcl::nssets::ifind $id $key]
    if {$index != -1} {
        upvar 0 ::nstcl::nssets::$id setId
        return [lindex $setId(vals) $index]
    }
}


#
# [ns_set isnull]
#
::nstcl::ad_proc -private ::nstcl::nssets::isnull {id index} {
    assert_valid_ndx $id $index
    # there isn't any way to put a null into an ns_set via the AOLserver Tcl API
    return 0
}


#
# [ns_set iunique]
#
::nstcl::ad_proc -private ::nstcl::nssets::iunique {id key} {
    set instances 0
    upvar 0 ::nstcl::nssets::$id setId

    foreach key_to_check $setId(keys) {
        if {[string equal -nocase $key_to_check $key]} {
            incr instances
            if {$instances == 2} then break
        }
    }

    return [expr {$instances < 2}]
}


#
# [ns_set key]
#
::nstcl::ad_proc -private ::nstcl::nssets::key {id index} {
    assert_valid_ndx $id $index
    return [lindex [set ::nstcl::nssets::${id}(keys)] $index]
}


# 
# [ns_set merge]
#
::nstcl::ad_proc -private ::nstcl::nssets::merge {high_id low_id} {
    upvar 0 ::nstcl::nssets::$high_id high
    upvar 0 ::nstcl::nssets::$low_id  low

    foreach key $low(keys) val $low(vals) {
        if {[lsearch -exact $high(keys) $key] == -1} {
            lappend high(keys) $key
            lappend high(vals) $val
        }
    }

    return $high_id
}


#
# [ns_set move]
#
::nstcl::ad_proc -private ::nstcl::nssets::move {to_id from_id} {
    upvar 0 ::nstcl::nssets::$to_id   to
    upvar 0 ::nstcl::nssets::$from_id from

    if {$to_id == $from_id} {
        return -code error "Cannot move set \"$to_id\" onto itself"
    }

    foreach key $from(keys) val $from(vals) {
        lappend to(keys) $key
        lappend to(vals) $val
    }

    set from(keys) {}
    set from(vals) {}

    return $to_id
}

            
#
# [ns_set name]
#
::nstcl::ad_proc -private ::nstcl::nssets::name {id} {
    return [set ::nstcl::nssets::${id}(name)]
}


#
# [ns_set new] is the same as [ns_set create]
#
interp alias {} ::nstcl::nssets::new {} ::nstcl::nssets::create


#
# [ns_set print]
#
::nstcl::ad_proc -private ::nstcl::nssets::print {id} {
    upvar 0 ::nstcl::nssets::$id setId
    set result "$setId(name):\n"
    foreach key $setId(keys) val $setId(vals) {
        append result "        $key = $val\n"
    }
    return $result
}


#
# [ns_set put]
#
::nstcl::ad_proc -private ::nstcl::nssets::put {id key value} {
    upvar 0 ::nstcl::nssets::$id setId
    set index [llength $setId(keys)]
    lappend setId(keys) $key
    lappend setId(vals) $value
    return $index
}


#
# [ns_set size]
#
::nstcl::ad_proc -private ::nstcl::nssets::size {id} {
    return [llength [set ::nstcl::nssets::${id}(keys)]]
}


#
# [ns_set split]
#
::nstcl::ad_proc -private ::nstcl::nssets::split {id {splitChar .}} {
    upvar 0 ::nstcl::nssets::$id setId

    if {[string length $splitChar] > 1} {
        set splitChar [string range $splitChar 0 0]
    }

    array set new_sets {}
    array set set_names {}

    foreach key $setId(keys) value $setId(vals) {
        set split_key [::split $key $splitChar]

        if {[llength $split_key] == 1} {
            set new_set {}
            set new_key $split_key
        } else {
            set new_set [lindex $split_key 0]
            set new_key [join [lrange $split_key 1 end] $splitChar]
        }

        if {![info exists new_sets($new_set)]} {
            set id [::nstcl::nssets::create $new_set]
            set new_sets($new_set) $id
            set set_names($id) $new_set
        } else {
            set id $new_sets($new_set)
        }

        ::nstcl::nssets::put $id $new_key $value
    }

    return [lsort [array names set_names]]
}


#
# [ns_set truncate]
#
::nstcl::ad_proc -private ::nstcl::nssets::truncate {id index} {
    assert_valid_ndx $id $index
    incr index -1

    set ::nstcl::nssets::${id}(keys) \
        [lrange [set ::nstcl::nssets::${id}(keys)] 0 $index]

    set ::nstcl::nssets::${id}(vals) \
        [lrange [set ::nstcl::nssets::${id}(vals)] 0 $index]

    return
}


#
# [ns_set unique]
#
::nstcl::ad_proc -private ::nstcl::nssets::unique {id key} {
    set instances 0
    upvar 0 ::nstcl::nssets::$id setId

    foreach key_to_check $setId(keys) {
        if {[string equal $key_to_check $key]} {
            incr instances
            if {$instances == 2} then break
        }
    }

    return [expr {$instances < 2}]
}
            

#
# [ns_set update]
#
::nstcl::ad_proc -private ::nstcl::nssets::update {id key value} {
    upvar 0 ::nstcl::nssets::$id setId
    set index [::nstcl::nssets::find $id $key]
    if {$index == -1} {
        lappend setId(keys) $key
        lappend setId(vals) $value
    } else {
        set setId(keys) [lreplace $setId(keys) $index $index $key]
        set setId(vals) [lreplace $setId(vals) $index $index $value]
    }

    return $value
}   


#
# [ns_set value]
#
::nstcl::ad_proc -private ::nstcl::nssets::value {id index} {
    assert_valid_ndx $id $index
    return [lindex [set ::nstcl::nssets::${id}(vals)] $index]
}



#
# [ns_set] helper for subcommands that take an index as an argument
#

::nstcl::ad_proc -private ::nstcl::nssets::assert_valid_ndx {id index} {
    if {![string is integer -strict $index]} {
        return -code error "expected integer but got \"$index\""
    }

    if {$index < 0} {
        return -code error "Specified negative index ($index)"
    }

    set size [llength [set ::nstcl::nssets::${id}(keys)]]
    if {$index >= $size} {
        return -code error "Can't access index $index; set only has\
            $size fields"
    }

    return $index
}



#
# ad_ns_set_to_tcl_vars
#

::nstcl::ad_proc ::nstcl::ad_ns_set_to_tcl_vars {{-duplicates overwrite}
                                                 {-level 1} setId} {
    summary {
        Sets variables in the caller's enviroment from the contents of an 
        ns_set
    }

    description {
        <p>Takes an ns_set and sets variables in the caller's enviroment 
        correspondingly.  For example, if the <i>setId</i> set contained a 
        key/value pair of <i>foo</i>/<i>bar</i>, then the Tcl variable 
        <i>foo</i> is set to <i>bar</i>.</p>    
    }

    optional_switches {
        <dle>
            <dt><option>-duplicates</option> (defaults to <i>overwrite</i>)</dt>
            <dd>
                This optional switch defines what happens if the Tcl variable 
                already exists (or if there are duplicate instances of the 
                same key in the <i>setId</i> set).  
           
                <b>overwrite</b> just overwrites the variable.  
                <b>ignore</b>    means the variable will not be overwritten.  
                <b>fail</b>      causes an error to be raised (which may help 
                                 track down subtle bugs due to name clashes).
            </dd>
    
            <dt><option>-level</option> (defaults to <i>1</i>)</dt>
            <dd>The level to upvar to.</dd>
        </dle>
    }

    keywords ns_set
} {
    switch -- $duplicates {
        overwrite  -
        ignore     -
        fail       {}
        default    { 
            return -code error "Invalid value \"$duplicates\" for -duplicates\
                switch.  Should be one of: ignore, fail, or overwrite"
        }
    }

    set size [::nstcl::ns_set size $setId]
    for {set i 0} {$i < $size} {incr i} {
        set key [::nstcl::ns_set key $setId $i]
        upvar $level $key value

        if {$duplicates == "fail" && [info exists value]} {
            return -code error "Variable \"$key\" already exists"
        } elseif {$duplicates == "overwrite" || ![info exists value]} {
            set value [::nstcl::ns_set value $setId $i]
        }
    }
}



#
# set_variables_after_query
#

::nstcl::ad_proc ::nstcl::set_variables_after_query {} {
    summary {
        Sets variables in the caller's enviroment from (typically) a 
        database query
    }

    description {
        <p>Sets variables from the <i>setId</i> stored in the variable 
        <i>selection</i> in the callers enviroment.</p>

        <p>This function is included for compatability with old 
        AOLserver/ACS/nstcl code.  New code should use 
        <b>ad_ns_set_to_tcl_vars</b> instead.</p>
    }

    keywords ns_set
} {
    upvar selection selection
    ::nstcl::ad_ns_set_to_tcl_vars -level 2 $selection
}



#
# set_variables_after_subquery
#

::nstcl::ad_proc ::nstcl::set_variables_after_subquery {} {
    summary {
        Sets variables in the caller's enviroment from (typically) a 
        database query
    }

    description {
        <p>Sets variables from the <i>setId</i> stored in the variable 
        <i>sub_selection</i> in the callers enviroment.</p>

        <p>This function is included for compatability with old 
        AOLserver/ACS/nstcl code.  New code should use 
        <b>ad_ns_set_to_tcl_vars</b> instead.</p>
    }

    keywords ns_set
} {
    upvar sub_selection sub_selection
    ::nstcl::ad_ns_set_to_tcl_vars -level 2 $sub_selection
}


#
# ns_section
#

::nstcl::ad_proc ::nstcl::ns_section {section} {
    set ::nstcl::nssets::section $section
}


#
# ns_param
#

::nstcl::ad_proc ::nstcl::ns_param {key value} {
    variable configuration
    upvar 0 ::nstcl::nssets::section section

    set ndx [::nstcl::ns_set find $configuration $section]
    if {$ndx == -1} {
        set setId [::nstcl::ns_set create $section]
        ::nstcl::ns_set put $configuration $section $setId
    } else {
        set setId [::nstcl::ns_set value $configuration $ndx]
    }

    ::nstcl::ns_set update $setId $key $value
}



#
# ns_configsections
#

::nstcl::ad_proc ::nstcl::ns_configsections {} {
    variable configuration
    set result {}
    set size [::nstcl::ns_set size $configuration]

    for {set i 0} {$i < $size} {incr i} {
        set setId [::nstcl::ns_set value $configuration $i]
        lappend result [::nstcl::ns_set copy $setId]
    }

    return $configuration
}



#
# ns_configsection
#

::nstcl::ad_proc ::nstcl::ns_configsection {section} {
    variable configuration
    set setId [::nstcl::ns_set iget $configuration $section]
    if {$setId != ""} {
        return [::nstcl::ns_set copy $setId]
    }
}
    


#
# ns_config
#

::nstcl::ad_proc ::nstcl::ns_config {-exact:boolean
                                     -bool:boolean
                                     -int:boolean
                                     section key {default ""}} {
    variable configuration
    
    if {($exact_p + $bool_p + $int_p) > 1} {
        return -code error "wrong # args: should be \"ns_config\
            ?-exact | -bool | -int? section key ?default?\""
    }

    set setId [::nstcl::ns_set iget $configuration $section]
    if {$setId == ""} {
        return $default
    } 

    switch $exact_p {
        0 { set value [::nstcl::ns_set iget $setId $key] }
        1 { set value [::nstcl::ns_set get $setId $key] }
    }

    if {[string equal "" $value]} {
        set value $default
    }

    if {$int_p} {
        switch [string is integer -strict $value] {
            0 { return }
            1 { return $value }
        }
    } elseif {$bool_p} {
        switch -exact -- [string tolower $value] {
            0       -
            off     -
            n       -
            no      -
            f       -
            false   { return 0 }
            1       -
            on      -
            y       -
            yes     -
            t       -
            true    { return 1 }
            default { return }
        }
    } else {
        return $value
    }
}


#
# define an ns_set to hold configuration details
#

namespace eval ::nstcl {
    variable configuration
    set configuration [::nstcl::ns_set create "nstcl configuration"]
}
    

package provide nstcl-nssets 1.2
