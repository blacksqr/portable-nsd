# nstcl-1.2/nstcl-core.tcl
# $Id: nstcl-core.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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
    variable ad_proc_details 
    array set ad_proc_details {}

    variable proc_doc
    array set proc_doc {}

    variable proc_origin
    array set proc_origin {}

    namespace export ad_proc \
                     ad_arg_parser
}


#
# nstcl::ad_proc ?-public_p? ?-private_p? ?-deprecated_p? ?-warn_p? \
#                proc_name ?documentation? arg_list code
# 

proc ::nstcl::ad_proc args {
    variable ad_proc_details
    variable proc_doc
    variable proc_origin

    # These switches (for the time being) are mostly for compatibility 
    # with code originally written for ACS/OpenACS v4
    array set ad_proc_switches {
        -public_p 0
        -private_p 0
        -deprecated_p 0
        -warn_p 0
        -strict_p 0
    }


    # figure out what switches we've been called with (if any)
    for {set i 0} {$i <= [llength $args]} {incr i} {
        set arg [lindex $args $i]

        if {[string range $arg 0 0] != "-"}  then break
        if {[string compare $arg "--"] == 0} then break

        if {![info exists ad_proc_switches(${arg}_p)]} {
            return -code error "Invalid switch $arg"
        } else {
            set ad_proc_switches(${arg}_p) 1
        }
    }

 
    # Makes no sense to have both -public and -private
    if {$ad_proc_switches(-public_p) && $ad_proc_switches(-private_p)} {
        return -code error "Mutually exclusive switches used:\
            -private and -public"
    }


    # Makes no sense to have -warn without -deprecated
    if {$ad_proc_switches(-warn_p) && !$ad_proc_switches(-deprecated_p)} {
        return -code error "Cannot use -warn without also using -deprecated"
    }



    # after handling any of the siwtches, we should have either 3 arguments
    # (like proc normally takes), or 4 (with a documentation string)
    set args [lrange $args $i end]
    switch [llength $args] {
        3 { foreach {proc_name arglist code} $args break }
        4 { foreach {proc_name arglist docs code} $args break }
        default {
            return -code error "Wrong # of args: should be\
                \"ad_proc ?-public? ?-private? ?-deprecated? ?-warn? proc_name\
                        arglist ?documentation? code\""
        }
    }


    # Because we store info about the arg list for each proc in the
    # namespace array ad_proc_details, we need to have the fully qualified
    # procedure name to avoid clashes
    set namespace [uplevel 1 namespace current]
    if {![string match ::* $proc_name]} {
        if {![string match *:: $namespace]} {
            set proc_name ${namespace}::$proc_name
        } else {
            set proc_name ${namespace}${proc_name}
        }
    }


    set end_of_switches_p 0
    set optional_positionals_p 0
    set argc [llength $arglist]
    set i 0

    set required    {}
    set optional    {}
    set booleans    {}
    set switches    {}
    set positionals {}


    # ad_proc's handles all the normal forms of an arglist that proc does:
    # 
    #     {foo}
    #     {args}
    #     {foo bar}
    #     {foo {bar baz}}
    #     {foo {bar baz} args}
    #
    # but also takes any number of initial switches.  There are three 
    # types of switches:
    #
    #     Boolean:  {-foo:boolean}
    #     Required: {-bar:required}
    #     Optional: {{-baz default}}
    #     Optional: {-zab:optional}
    #
    # In the case of -foo:boolean, a variable named foo_p would exist.
    # It's value would be 1 if the switch was present, 0 otherwise.  If
    # -baz were not specified the variable baz would have a value of "default".
    # If baz were specified it would contain the value specified.  Same
    # for bar.
    #
    # The variable zab would exist only if present.  (This is an 
    # enhancement not found in the stock version of ad_proc from ACS 4.x).
    #
    # Switches may be placed by a caller in any order, provided all
    # switches come before the first positional paramter.

    foreach arg $arglist {
        incr i

        if {[llength $arg] > 2} {
            return -code error "Malformed arguments\
                (more than just a default value given for \"[lindex $arg 0]\")"
        }

        set switch_p [string match -* $arg]

        if {$switch_p && $end_of_switches_p} {
            return -code error "Cannot specify switches after positional\
                paramters"
        }

        if {!$switch_p && !$end_of_switches_p} {
            set end_of_switches_p 1
        }

        if {$switch_p} {
            foreach {switch default} $arg break
            foreach {switch flag} [split $switch :] break

            # optional switches require a default     
            if {[string compare $flag ""] == 0 && [llength $arg] == 1} {
                return -code error "No default value given for switch \"$arg\""
            }

            switch -- $flag {
                boolean  { lappend booleans $switch }
                required { lappend required $switch }
                optional { lappend optional $switch }
                ""       { lappend switches $arg    }
                default  { return -code error "Invalid flag \"$flag\"" }
            }

            if {$flag != "" && [llength $arg] == 2} {
                return -code error "Cannot specify default value for\
                    $flag switch \"$switch\""
            }
        } else {
            # Is this the first positional paramter with a default?
            if {!$optional_positionals_p && [llength $arg] == 2} {
                set optional_positionals_p 1
            }

            if {$optional_positionals_p && [llength $arg] == 1} {
                if {$i != $argc || ![string compare $arg "args"] == 0} {
                    return -code error "Positional parameter \"$arg\" needs\
                        a default value (since it follows another positional\
                        parameter with a default value)"
                }
            }

            lappend positionals $arg
        }
    }

 
    # Store any user provided documentation
    if {[info exists docs]} {
        set proc_doc($proc_name) $docs
        set proc_origin($proc_name) [file join [pwd] [info script]]
    }


    # Store the values of the switches to ad_proc itself (i.e. -private, etc.)
    set ad_proc_details(ad_proc_flags,$proc_name) [array get ad_proc_switches]


    # If code is equal to "-" then the proc is already defined elsewhere
    # and this call to ad_proc is just to gather the documentation together.
    if {[string compare "-" $code] == 0} then return


    # If the procedure being defined does takes any switches
    # then we insert a call to ::nstcl::_ad_proc_parser to parse
    # the received arguments and setup the proper local variables.
    # Otherwise, we just define proc as is.
    if {[llength $required] || 
        [llength $booleans] || 
        [llength $switches] ||
        [llength $optional]} {
        set ad_proc_details(required,$proc_name) $required
        set ad_proc_details(booleans,$proc_name) $booleans
        set ad_proc_details(switches,$proc_name) $switches
        set ad_proc_details(optional,$proc_name) $optional
        set ad_proc_details(positionals,$proc_name) $positionals

        if {!$ad_proc_switches(-strict_p)} {
            array set abbrevs {}
            foreach list {required booleans switches optional} {
                foreach switch [join [set $list]] {
                    set abbrevs([lindex $switch 0]) ""
                }
            }
            set ad_proc_details(abbrevs,$proc_name) [array get abbrevs]
        }

        set code "
            ::nstcl::_ad_proc_parser [list $proc_name] \[set args\]
            $code"

        set arguments args
    } else {
        set arguments $positionals
    }

    proc $proc_name $arguments $code
}


::nstcl::ad_proc ::nstcl::ad_proc {args} {
    summary "Define a procedure"

    syntax {
        <b>::nstcl::ad_proc</b> ?<m>-public</m>? ?<m>-private</m>?  ?<m>-deprecated</m>? ?<m>-warn</m>? <m>arg_list</m> ?<m>documentation</m>? <m>code</m>
    }

    description {
        <p><b>ad_proc</b> is an enhancement over Tcl's built-in <b>proc</b>
        command that supports several types of switches in the 
        <i>arg_list</i> (along with all the types supported by <b>proc</b>).</p>

        <p>
            <b>Types of args understood by <i>proc</i>:</b>

            <dle>
                <dt>foo</dt>
                <dd>Single variable <i>foo</i>.</dd>
    
                <dt>{args}</dt>
                <dd>Special <i>args</i> variable that collects all remaining
                arguments into a list when specified as the last argument.</dd>
    
                <dt>{foo bar}</dt>
                <dd>Multiple variables, <i>foo</i> and <i>bar</i>.</dd>
    
                <dt>{foo {bar baz}}</dt>
                <dd>One required variable <i>foo</i>, and an optional variable 
                <i>bar</i> that will default to a value of <i>baz</i> if another
                value is not provided.</dd>
    
                <dt>{foo {bar baz} args}</dt>
                <dd>One required variable, an optional variable with a default,
                and <i>args</i> as a catch-all.</dd>
            </dle>
        </p>
   
        <p> 
            <b>Additional types understood by <i>ad_proc</i>:</b>
    
            <dle>
                <dt>-foo:boolean</dt>
                <dd>An optional switch <i>foo</i>; a local variable named 
                <i>foo_p</i> will be set to 1 if this switch was present, 0 
                 otherwise.</dd>
     
                <dt>-bar:required</dt>
                <dd>A required switch <i>bar</i>; a local variable named
                <i>bar</i> will be set.  An error is thrown if the procedure
                is invoked without specifying a value for this switch.</dd>
    
                <dt>{-baz default}</dt>
                <dd>An optional switch <i>baz</i>.  If the switch is specified
                the local variable <i>baz</i> is set to the specified value,
                otherwise the local variable <i>baz</i> will be set to 
                <i>default</i>.</dd> 
    
                <dt>-zab:optional</dt>
                <dd>An optional switch <i>zab</i>.  The local variable 
                <i>zab</i> will be set only if specified by the caller; 
                otherwise, the variable will not exist.</dd>
            </dle>
        </p>

        <p>Switch parameters must be listed first before any positional
        parameters.  When invoking a procedure which has optional switches, and
        there is a possibility that the first positional parameter may begin
        with a hyphen, use "--" to indicate that there are no more switches
        to parse.</p>
    }

    optional_switches {
        <dle>
            <dt>-public</dt>
            <dd>Use when the procedure is part of a public API.</dd>

            <dt>-private</dt>
            <dd>Use when the procedure is private.</dd>

            <dt>-deprecated</dt>
            <dd>Used to indicate that the procedure should no longer be 
            used.</dd>

            <dt>-warn</dt>
            <dd>Used to indicate that a warning should be emited when
            the procedure is invoked.</dd>
        </dle>
    }

    see_also {
        <b>ad_arg_parser</b>
    }
} -


# 
# nstcl::_ad_proc_parser procedure args
# 

proc ::nstcl::_ad_proc_parser {proc_name arg_list} {
    variable ad_proc_details

    # unset args in the callers enviroment since we'll set all the variables
    # based on arg_list and the switches/postional params defined by ad_proc
    uplevel 1 unset args

    array set booleans {}
    array set switches {}
    array set required {}
    set positionals $ad_proc_details(positionals,$proc_name)

    set strict_p [expr {![info exists ad_proc_details(abbrevs,$proc_name)]}]
    if {!$strict_p} {
        array set abbrevs $ad_proc_details(abbrevs,$proc_name)
    }

 
    # Default each boolean to false, then we'll record which ones
    # are present
    foreach boolean $ad_proc_details(booleans,$proc_name) {
        set booleans($boolean) 0
    }


    # Default optional switches to their default values, then we'll
    # update the actual values of any present switches
    foreach switch_and_default $ad_proc_details(switches,$proc_name) {
        foreach {switch default} $switch_and_default break
        set switches($switch) $default
    }


    # Keep track of required switches.  Until we process the args (obviously)
    # none will be present.  We will throw an error if, after processing
    # args, any required switch still has a value of 0.
    foreach required_switch $ad_proc_details(required,$proc_name) {
        set required($required_switch) 0
    }


    # Walk through the args, beginning at the front, looking for
    # switches.  When we find either an optional or a required switch
    # we take the next arg to be its value.
    #
    # We stop when we find an arg that does not begin with a "-" (since it
    # wouldn't be a switch), or "--" which indicates that the remaining
    # args should not be treated as switches even if they begin with a -.
    set argc [llength $arg_list]
    for {set i 0} {$i < $argc} {incr i} {
        set arg [lindex $arg_list $i]
        if {![string match -* $arg]} then break
        if {[string compare $arg "--"] == 0} {
            # advance to the next argument
            incr i
            break
        }

        if {!$strict_p} {
            if {![info exists abbrevs($arg)]} {
                set abbrev [array names abbrevs $arg*]
                if {[llength $abbrev] == 1 && [string length $arg] > 1} {
                    set arg $abbrev
                } else {
                    if {[regexp {^([^=]+)=(.+)$} $arg => arg hard_coded_bool] &&
                        [info exists abbrevs($arg)] &&
                        [string is boolean -strict $hard_coded_bool]} {
                        set hard_coded_bool [string is true $hard_coded_bool]
                    }
                }
            }
        }

        if {[info exists booleans($arg)]} {
            if {[info exists hard_coded_bool]} {
                set booleans($arg) $hard_coded_bool
                unset hard_coded_bool
            } else {
                set booleans($arg) 1
            }
        } elseif {[info exists required($arg)]} {
            incr required($arg)
        
            if {($i + 1) == $argc} {
                return -code error "No argument to switch $arg"
            } else {
                set switches($arg) [lindex $arg_list [incr i]]
            }
        } elseif {[lsearch $ad_proc_details(optional,$proc_name) $arg] != -1} {
            if {($i + 1) == $argc} {
                return -code error "No argument to switch $arg"
            } else {
                set switches($arg) [lindex $arg_list [incr i]]
            }
        } elseif {[info exists switches($arg)]} {
            if {($i + 1) == $argc} {
                return -code error "No argument to switch $arg"
            } else {
                set switches($arg) [lindex $arg_list [incr i]]
            }
        } else {
            if {(($argc - $i) == [llength $positionals]) ||
                 (($argc - $i) >  [llength $positionals] &&
                  [string compare [lindex $positionals end] args] == 0)} {
                # Something that looks like a switch, but we can fill
                # up all of our positionals OK without it being a switch.
                #
                # If it is a typo and we're missing a required switch
                # that'll still throw an error later on.
                break
            } else {
                return -code error "Invalid switch: \"$arg\" [list $proc_name \
                    $arg_list]"
            }
        }
    }


    # Throw an error if we're missing any required switch
    foreach switch [array names required] {
        if {!$required($switch)} {
            return -code error "Required switch $switch not provided"
        } 
    }

    # Set the variables for the optional & required switches
    foreach switch [array names switches] {
        upvar 1 [string range $switch 1 end] var
        set var $switches($switch)
    }

    # Set the variables for the boolean switches
    foreach boolean [array names booleans] {
        upvar 1 [string range $boolean 1 end]_p var
        set var $booleans($boolean)
    }


    # Any remaining switches are for positional parameters (and possibly
    # a catch all "args" at the end)
    set arg_list [lrange $arg_list $i end]
    set argc [llength $arg_list]

    # is there a catch-all args?
    if {[string compare [lindex $positionals end] args] == 0} {
        upvar 1 args var
        set var [list]
    }

    if {$argc > [llength $positionals]} {
        if {[string compare [lindex $positionals end] args] == 0} {
            upvar 1 args var
            set argc [expr {[llength $positionals] - 1}]
            set var  [lrange $arg_list $argc end]

            # remove what went into ``args''
            set positionals [lrange $positionals 0 end-1]
            set arg_list [lrange $arg_list 0 [expr {$argc - 1}]]
        } else {
            return -code error "Too many positional parameters specified"
        }
    }

    for {set i 0} {$i < $argc} {incr i} {
        set arg [lindex $arg_list $i]
        foreach {pos_var pos_default} [lindex $positionals $i] break
        upvar 1 $pos_var var
        set var $arg
    }

    # Handle the case where the last positional is args and we only got
    # one args (that needs an extra layer of list quoting because it is "args")
    if {[info exists pos_var] && [string compare "args" $pos_var] == 0} {
        uplevel 1 set args \[list \$args]
    }

    set argc [llength $positionals]
    set last [expr {$argc - 1}]

    for {} {$i < $argc} {incr i} {
        set var_default_pair [lindex $positionals $i]
        foreach {pos_var pos_default} $var_default_pair break

        if {[llength $var_default_pair] == 1} {
            if {$i != $last || ![string compare $pos_var "args"] == 0} {
                return -code error "no value given for parameter \"$pos_var\"\
                    to [lindex [info level -1] 0]"
            }
        }

        upvar 1 $pos_var var
        set var $pos_default
    }

}



#
# ad_arg_parser
#

::nstcl::ad_proc ::nstcl::ad_arg_parser {allowed_args argv} {
    summary "Parses a list of trailing argument switches"
    description {
        <p>Given a list of <i>allowed_args</i> and a list of command-line
        options in <i>argv</i>, set switch values in the corresponding variable
        names in the calling enviroment.</p>

        <p>If the last switch name listed in <i>allowed_args</i> is <b>args</b>
        then extra values will be tolerated after any switches (in <i>argv</i>)
        and these remaining values will be placed in the <i>args</i> list.</p>
    }
} {
    if {[string equal [lindex $allowed_args end] "args"]} {
        upvar 1 args args
        set args {}
        set allowed_args [lrange $allowed_args 0 end-1]
    }

    foreach switch $allowed_args {
        set switches($switch) 0
    }


    set switches_present {}
    foreach {switch value} $argv {
        if {![regexp {^-(.+)$} $switch => switch]} {
            if {[info exists args]} {
                set args $argv
                return $switches_present
            } else {
                return -code error "Expected switch but encountered\
                    \"$switch\""
            }
        }

        if {![info exists switches($switch)]} {
            return -code error "Invalid switch -$switch (expected one of\
                -[join [lsort [array names switches]] ", -"])"
        } elseif {$switches($switch)} {
            return -code error "Switch -$switch already specified"
        } else {
            upvar 1 $switch var
            set var $value
            incr switches($switch)
            lappend switches_present $switch
        }

        # pop the switch/value off of argv so that we can just set args 
        # to argv if need be
        if {[llength $argv] < 2} {
            return -code error "Invalid switch syntax - no argument to final\
                switch \"[lindex $argv end]\""
        } else {
            set argv [lrange $argv 2 end]
        }
    }

    return $switches_present
}



package provide nstcl-core 1.2
