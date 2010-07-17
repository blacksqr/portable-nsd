package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-http

# nstcl-1.2/nstcl-templating.tcl
# $Id: nstcl-templating.tcl,v 1.5 2003/08/05 01:12:45 cleverly Exp $
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

namespace eval ::nstcl::template {}
namespace eval ::nstcl::template::multirow {}

namespace eval ::nstcl::template::adp {
    variable counter 0
    namespace export protect_chars \
                     expand_@variables \
                     expand_@variables_quoted \
                     tag_attributes \
                     parental_node
}

namespace eval ::nstcl::template::adp::compiled {}
namespace eval ::nstcl::template::adp::tags {}
namespace eval ::nstcl::template::adp::tags::_if {}


proc ::nstcl::template::adp::adp_to_pseudo_code {adp} {
    # Protect HTML enteties 
    set adp [string map [list & "&amp;"] $adp]

    # Get rid of certain characters that shouldn't be in here anyway
    set adp [string map [list [format %c  0] "" \
                              [format %c  4] "" \
                              [format %c 26] ""] $adp]

    # Remove extraneous whitespace inside tags at the beginning and end of tags
    regsub -all -- {<\s+} $adp "<" adp
    regsub -all -- {(\s*)>} $adp {\1 >} adp
    regsub -all -- {<(?!/)} $adp "<" adp


    # find the indices of where 
    set open  [regexp -inline -indices -all -- < $adp]
    set close [regexp -inline -indices -all -- > $adp]

    set open  [lsort -unique -integer [join $open]]
    set close [lsort -unique -integer [join $close]]

    # we should have an even match or we're malformed
    if {[llength $open] != [llength $close]} {
        error "Unbalanced # of < & >'s ([llength $open] vs [llength $close])"
    }

    foreach opening $open closing $close {
        set tags($opening) <
        set tags($closing) >
    }

    set positions [list]
    set stack [list]
    foreach pos [lsort -integer [array name tags]] {
        lappend positions $pos $tags($pos)
        append stack $tags($pos)
    }


    set depth 0
    set stack [list]

    foreach {pos tag} $positions {
        switch -- $tag {
            < { incr depth  1
                lappend stack $pos }
            > { incr depth -1 
                set corresponding [lindex $stack end]
                set stack [lrange $stack 0 end-1] }
        }

        if {$depth == -1} {
            set pre_problem_chunk [string range $adp 0 $pos]
            regsub -all -- "\r|\n|\r\n" [string range $adp 0 $pos] \
                "\x00" pre_problem_chunk
            set approx_line_num [llength [split $pre_problem_chunk \x00]]
            error "> occured before corresponding < near pos: $pos\
                (line: $approx_line_num)"
        }

        if {$tag == ">"} {
            set points(open,$corresponding) $pos
            set points(close,$pos) $corresponding
            set points(depth,$pos) $depth
        } else {
            set points(contains,$pos) 0
            foreach containing_point $stack {
                incr points(contains,$containing_point)
            }
        }
    }


    # Tags we care about:
    #
    # <multiple>	</multiple>
    # <group>		</group>
    # <grid>		</grid>
    # <list>		</list>
    # <if>		</if>
    # <else>		</else>
    # <include>
    # <property>	</property>
    # <noparse>		</noparse>
    # <master>
    # <slave>

    array set templating_tags {}
    set married [list multiple group grid list if else property noparse]
    set single  [list include master slave]
    set both    [concat $married $single]

    set RE "^<([join $both |]|/\\s*[join $married "|/\\s*"])(?:\\s|>)"
    foreach pos $open {
        if {[regexp $RE [string range $adp $pos end] => tag]} {
            set templating_tags($pos) [string tolower $tag]
        } 
    }


    set pending_tags [list]
    set prev_end_pos -1

    foreach pos [lsort -integer [array names templating_tags]] {
        set tag $templating_tags($pos)
        set end_pos $points(open,$pos)

        if {$end_pos < $prev_end_pos} {
            error "Illegal nesting of templating tags"
        } else {
            set prev_end_pos $end_pos
        }

        if {[lsearch -exact $single $tag] != -1} continue

        if {[string match /* $tag]} {
            set popped [lindex $pending_tags end]
            set pending_tags [lrange $pending_tags 0 end-1]

            if {![string equal /$popped $tag]} {
                set pre_problem_chunk [string range $adp 0 $pos]
                regsub -all -- "\r|\n|\r\n" [string range $adp 0 $pos] \
                    "\x00" pre_problem_chunk
                set approx_line_num [llength [split $pre_problem_chunk \x00]]
                error "Improperly nested tags (<$popped><$tag>) near pos: $pos\
                    (line: $approx_line_num)"
            }
        } else {
            lappend pending_tags $tag
        }
    }

    if {[llength $pending_tags] != 0} {
        error "[llength \
            $pending_tags] Unclosed tag(s): <[join $pending_tags ">, <"]>"
    }

    

    # Mark non HTML tags for later conversion to HTML entities
    foreach pos [lsort -integer -decreasing [array name tags]] {
        if {$tags($pos) == ">"} {
            set pos_to_check $points(close,$pos)
            set replacement \x04
        } else {
            set pos_to_check $pos
            set replacement \x1a
        }

        if {![info exists templating_tags($pos_to_check)]} {
            set adp [string replace $adp $pos $pos $replacement]
        }
    }


    # Close off any single templating tags so htmlparse -> tree doesn't 
    # get confused
    foreach pos [lsort -integer -decreasing [array names tempalting_tags]] {
        set tag $templating_tags($pos)

        if {[lsearch $single $tag] != -1} {
            set adp "[string range $adp 0 $pos]</$tag>[string range $adp \
                [incr pos] end]"
        }
    }


    # Convert to HTML entities and kill extra white space
    regsub -all -- {\s*([\x04])} $adp {\1} adp
    regsub -all -- {\s*( [\x1a])} $adp {\1} adp
    set adp [string map [list \x04 "&gt;" \x1a "&lt;"] $adp]

    # Convert new lines to our special characters so they don't get lost
    set adp [string map [list \r \x04 \n \x1a] $adp] 

    # Put a null before each < and after each > so we capture significant
    # whitespace (and don't lose it to htmlparse::2tree
    set adp [string map [list < \x00< > >\x00] $adp]

    return $adp
}


proc ::nstcl::template::adp::tags::_multiple {tree code_var action node} {
    upvar 1 $code_var code
    array set data [tag_attributes [$tree get $node -key data]]

    if {$action == "enter"} {
        if {![info exists data(name)]} {
            error "No name attribute specified for <multiple>"
        }

        set name $data(name)
        if {![regexp {^[A-Za-z0-9_]+$} $name]} {
            error "Invalid variable name for <multiple name=$name>"
        }

        if {[info exists data(maxrows)]} {
            set maxrows $data(maxrows)
            if {[string is integer -strict $maxrows] && $maxrows < 0} {
                error "Error in <multiple name=$name>: maxrows must be >= 0"
            }
        } else {
            set maxrows "\[set $name:rowcount]"
        }

        if {[info exists data(startrow)]} {
            set startrow $data(startrow)
            if {[string is integer -strict $startrow] && $startrow < 0} {
                error "Error in <multiple name=$name>: startrow must be >= 0"
            }
        } else {
            set startrow "0"
        }
       

        set snippet "
            # $node: <multiple name=$name>, depth: [$tree depth $node] 
            if {!\[info exists $name:rowcount] || 
                !\[string is integer -strict \[set $name:rowcount]] ||
                \[set $name:rowcount] < 0 } {
                error {No such <multiple name=$name>}
            }

            set @startrow:$node $startrow
            set @maxrows:$node $maxrows

            if {!\[string is integer -strict \[set @startrow:$node]] ||
                \[set @startrow:$node] < 0} {
                error \"Invalid startrow: \[list \[set @startrow:$node]]\"
            }

            for {set @i:$node \[expr {\[set @startrow:$node] + 1}]
                 set @counter:$node 0
                } { \[set @i:$node] <= \[set $name:rowcount] &&
                    \[set @counter:$node] < \[set @maxrows:$node]
                } { incr @i:$node
                    incr @counter:$node } \{

                if {\[info exists $name]} {
                    unset $name
                }

                array set $name \[array get $name:\[set @i:$node]]
                # temp debugging aide (hack)
#                puts stderr \[array get $name]

        "
    }

    if {$action == "leave"} {
        set name $data(name)

        set snippet "
            \}
            # $node: </multiple name=$name>, depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}
           

proc ::nstcl::template::adp::tags::_list {tree code_var action node} {
    upvar 1 $code_var code
    array set data [tag_attributes [$tree get $node -key data]]

    if {$action == "enter"} {
        if {![info exists data(name)]} {
            error "No name attribute specified for <list>"
        } 
      
        set name $data(name)
        if {![regexp {^[A-Za-z0-9_]+$} $name]} {
            error "Invalid variable name for <list name=$name>"
        }

        set snippet "
            # $node: <list name=$name>, depth: [$tree depth $node] 
            if {!\[info exists $name]} {
                error {No such <list name=$name>}
            }
 
            if {\[array exists $name]} {
                error {Invalid <list name=$name>: $name is an array!}
            }

            set $name:rowcount \[llength \[set $name]]
            set $name:rownum 0

            foreach $name:item \[set $name] \{
                incr $name:rownum
        " ;#"
    } else {
        set name $data(name)

        set snippet "
            \}
            # $node: </list name=$name>, depth: [$tree depth $node] 
        " ;#"
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_PCDATA {tree code_var action node} {
    upvar 1 $code_var code

    set data [$tree get $node -type data]

    if {$action == "enter"} {
        set chunk [protect_chars [::htmlparse::mapEscapes $data]]

        upvar 0 $tree state
        switch -exact -- $state(mode) {
            normal   -
            noparse  { set var @output }
            property { set var @properties($state(property)) }
            default  { error "Should be impossible to get here!" }
        }

        if {[string length $chunk]} {
            set snippet "
                # $node: PCDATA (begin), depth: [$tree depth $node]
                append $var \[subst [list [expand_@variables $chunk]]]
            "
        } else {
            set snippet "
                # $node: PCDATA (begin), depth: [$tree depth $node] 
                # null body so we'll skip the \[subst]
            " ;#"
        }
    } else {
        set snippet "
            # $node: PCDATA (end), depth: [$tree depth $node]
        " ;#"
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_if::unary {var op operand negate_p} {
    if {$negate_p} {
        switch $op {
            true  { set op false }
            false { set op true  }
            even  { set op odd   }
            odd   { set op even  }
        }
    }

    if {$op == "true" || $op == "false" || $op == "even" || $op == "odd"} {
        set var [expand_@variables_quoted $var]
        switch $op {
            true  { set fragment "$var"  }
            false { set fragment "!$var" }
            even  { set fragment "($var % 2 == 0)" }
            odd   { set fragment "($var % 2 == 1)" }
        }

        return $fragment
    } 

    regexp {^@([A-Za-z0-9_]+)@$} $var => exists_var
    if {[regexp {^@([A-Za-z0-9_]+)\.([A-Za-z0-9_]+)@$} $var => key val]} {
        set exists_var "${key}(${val})"
    }

    if {![info exists exists_var]} {
        if {!$negate_p} {
            set fragment "\[expr 0]"
        } else {
            set fragment "\[expr 1]"
        }

        return $fragment
    }

    if {$op == "defined" || $op == "exists"} {
        if {!$negate_p} {
            set fragment "\[info exists $exists_var]"
        } else {
            set fragment "!\[info exists $exists_var]"
        }

        return $fragment
    } 

    # not nil / nil
    if {!$negate_p} {
        set fragment "\[::nstcl::template::adp::tags::_if::nil? $exists_var]"
    } else {
        set fragment "!\[::nstcl::template::adp::tags::_if::nil? $exists_var]"
    }

    return $fragment
}


proc ::nstcl::template::adp::tags::_if::nil? {exists_var} {
    upvar 1 $exists_var var
    if {![info exists var]} {
        return 1
    } else {
        if {[string length $var]} {
            return 0
        } else {
            return 1
        }
    }
}


proc ::nstcl::template::adp::tags::_if::binary {var op operand negate_p} {
    # translate into actual op for [expr]   
    if {!$negate_p} { 
#        switch $op {
#            gt { set op >  }
#            ge { set op >= }
#            lt { set op <  }
#            le { set op <= }
#            eq { set op == } 
#            ne { set op != }
#        }

        switch $op {
            gt { set op == ; set val  1 }
            ge { set op != ; set val -1 }
            lt { set op == ; set val -1 }
            le { set op != ; set val  1 }
            eq { set op == ; set val  0 }
            ne { set op != ; set val  0 }
        }
    } else {
#        switch $op {
#            gt { set op <= }
#            ge { set op <  }
#            lt { set op >= }
#            le { set op >  }
#            eq { set op != } 
#            ne { set op == }
#        }

        switch $op {
            gt { set op != ; set val  1 }
            ge { set op == ; set val -1 } 
            lt { set op != ; set val -1 }
            le { set op == ; set val  1 }
            eq { set op != ; set val  0 }
            ne { set op == ; set val  0 }
        }
    }


    # strip quotes for comparisons
    regexp {^"(.+)"$} $var => var
    regexp {^"(.+)"$} $operand => operand
   
    return "\[string compare \[list [expand_@variables $var]] \
                             \[list [expand_@variables $operand]]] $op $val"
}


proc ::nstcl::template::adp::tags::_if::ternary {x op operands negate_p} {
    set y [lindex $operands 0]
    set z [lindex $operands 1]

    set lower_bound [list [expand_@variables_quoted $x] \
                          [expand_@variables_quoted $y]]

    set upper_bound [list [expand_@variables_quoted $x] \
                          [expand_@variables_quoted $z]]

    if {$negate_p} {
        # x not between y and z
        # x < y || x > z
        set op ||
        set lower_bound [join $lower_bound " < "]
        set upper_bound [join $upper_bound " > "]
    } else {
        # x between y and z
        # x >= y && x <= z
        set op &&
        set lower_bound [join $lower_bound " >= "]
        set upper_bound [join $upper_bound " <= "]
    }

    return "(($lower_bound) $op ($upper_bound))"
}


proc ::nstcl::template::adp::tags::_if::n-ary {var op operands negate_p} {
    set values [list]
    set fragment [list]

    regexp {^"(.+)"$} $var => var
    set var [expand_@variables_quoted $var]

    if {!$negate_p} {
        set op ==
        set conj ||
    } else {
        set op !=
        set conj &&
    }

    foreach operand $operands {
        if {[llength $fragment]} {
            lappend fragment $conj
        }

        regexp {^"(.+)"$} $operand => operand
        set operand [expand_@variables_quoted $operand]

        lappend fragment "\[string equal $var $operand] $op 1"
    }
  
    return "[join $fragment " "]"
}


proc ::nstcl::template::adp::tags::_if {tree code_var action node} {
    upvar 1 $code_var code
    #set data [string trim [$tree get $node -key data]]
    set data [$tree get $node -key data]

    if {$action == "enter"} {
        set expression [list]

        while {[llength $data]} {
            set var  [lindex $data 0]
            set op   [string tolower [lindex $data 1]]
            set data [lrange $data 2 end]
    
            set negate_p [string equal -nocase not $op]
            if {$negate_p} {
                set op   [lindex $data 0]
                set data [lrange $data 1 end]
            }

            switch -exact -- $op {
                nil     -
                exists  -
                defined -
                odd     -
                even    -
                true    -
                false   { set operands [list] 
                          set class unary }
                eq      -
                ne      - 
                lt      -
                le      -
                gt      -
                ge      { set operands [lindex $data 0]
                          if {[llength $data] < 1} {
                              error "No operand for <if ... $op ???>"
                          }
                          set data [lrange $data 1 end] 
                          set class binary }
                between { set operands [lrange $data 0 1]
                          if {[llength $data] < 2} {
                              error "Insufficient operands for <if ... between>"
                          }
                          set data [lrange $data 2 end] 
                          set class ternary }
                in      { set operands $data
                          if {[llength $data] == 0} {
                              error "No operands for <if ... in ???>"
                          }
                          set data [list] 
                          set class n-ary }
                default { error "Illegal operator in <if> tag, $op" }
            }

            lappend expression [_if::$class $var $op $operands $negate_p]
            if {[llength $data]} {
                set conj [lindex $data 0]
                set data [lrange $data 1 end]

                switch -exact -- [string tolower $conj] {
                    and { lappend expression && }
                    or  { lappend expression || }
                    default { error "Invalid conjunction in <if> tag: $conj" }
                }
            }
        }

        set snippet "
            # $node <if>, depth: [$tree depth $node]
            set if@$node \[expr [join $expression " "]]
            set else@$node \[expr !\[set if@$node]]

            if {\[set if@$node]} \{
        " ;#"
    }

    if {$action == "leave"} {
        set snippet "
            \}
            # $node </if> depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_else {tree code_var action node} {
    upvar 1 $code_var code

    set pos $node
    while 1 {
        set pos  [$tree previous $pos]
        set type [$tree get $pos -key type]

        if {$type == "PCDATA"} continue else break
    }


    if {$type != "if"} {
        error "<else> must follow an <if>...</if>, not <$type>"
    }

    if {$action == "enter"} {
        set snippet "
            # $node <else>, depth: [$tree depth $node]
            if {\[set else@$pos]} \{
        "
    } else {
        set snippet "
            # $node </else>, depth: [$tree depth $node]
            \}
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_root {tree code_var action node} {
    upvar 1 $code_var code

    if {[string equal $action "enter"]} {
        upvar 0 $tree state

        set snippet "
            #######
            # BEGIN
            #######
            set @output {}\n
            set @pwd [list $state(pwd)]\n
        "

        set code $snippet
    } else {
        set snippet "
            ######
            # END
            ######

            # Is this a slave to a master?
            if {\[info exists @master]} {
                set @master:code \"set @slave \[list \[set @output]]\\n\"

                append @master:code \"
                    \\n# properties: \[array names @properties]\\n\"

                foreach @slave_var \[array names @properties] {
                    append @master:code \"set \[set @slave_var] \[list \
                        \[set @properties(\[set @slave_var])]]\\n\"
                }

                if {\[file exists \[set @master].tcl]} {
                    append @master:code \"\\n### \[set @master] ###\\n\"
                    append @master:code \[::fileutil::cat \[set @master].tcl]
                    append @master:code \"\\n### \[set @master] ###\\n\"
                } else {
                    append @master:code \"
                        \\n\# no datasource for \[set @master].tcl
                        \\n\"
                }
               
                set @master:proc \"\[::nstcl::template::adp_compile -file \
                    \[file join \[set @pwd] \[set @master].adp]]\"

                set @output \[::nstcl::template::adp_eval \[set @master:proc] \
                    -slave \[set @master:code]]
                unset @master
            }
                    

            set @output
        "

        append code $snippet
    }
 
    return $snippet
}


proc ::nstcl::template::adp::tags::_hmstart {tree code_var action node} {
    upvar 1 $code_var code
 
    if {$action == "enter"} {
        set snippet "
            # $node <hmstart>, depth: [$tree depth $node]
        "
    } else {
        set snippet "
            # $node </hmstart>, depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_group {tree code_var action node} {
    upvar 1 $code_var code
    array set data [tag_attributes [$tree get $node -key data]]
    set parent [parental_node $tree $node multiple]

    if {![info exists data(column)]} {
        error "No column attribute specified for <group>"
    }
    set column $data(column)
    unset data

    array set data [tag_attributes [$tree get $parent -key data]]
    set name $data(name)


    if {$action == "enter"} {
        if {![regexp {^[A-Za-z0-9_]+$} $column]} {
            error "Invalid column name for <group column=$column>"
        }

        set snippet "
            # $node: <group column=$column>, depth: [$tree depth $node]
            # inside of <multiple> @ node $parent

            set @group_by:$node \[set $name\($column)]
            while 1 \{
        "
    } else {
        set snippet "
                if {\[set @i:$parent] == \[set $name:rowcount]} break
                incr @i:$parent
                unset $name
                array set $name \[array get $name:\[set @i:$parent]]
                if {!\[string equal \[set $name\($column)] \
                                   \[set @group_by:$node]]} {
                    incr @i:$parent -1
                    break
                }
            \}
            # $node: </group column=$column>, depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_grid {tree code_var action node} {
    upvar 1 $code_var code
    array set data [tag_attributes [$tree get $node -key data]]

    if {$action == "enter"} {
        if {![info exists data(name)]} {
            error "No name attribute specified for <grid>"
        }

        set name $data(name)
        if {![regexp {^[A-Za-z0-9_]+$} $name]} {
            error "Invalid variable name for <grid name=$name>"
        }

        if {![info exists data(cols)]} {
            error "Number of cols not specified for <grid name=$name>"
        }
      
        set cols $data(cols)
        if {![string is integer -strict $data(cols)]} {
            error "Number of cols must be an integer, not \"$cols\""
        }

        if {[string is integer -strict $cols] && $cols <= 0} {
            error "Number of cols must be positive, not \"$cols\""
        }

        
        set snippet "
            # $node: <grid name=$name cols=$cols>, depth: [$tree depth $node]
            set @rows:$node \[expr {int(ceil(\[set $name:rowcount] / $cols.0))}]
            for {set @row:$node 1} {\[set @row:$node] <= \[set @rows:$node]} \
                {incr @row:$node} \{
                for {set @col:$node 1} {\[set @col:$node] <= $cols} \
                    {incr @col:$node} \{


                    set @rownum:$node \[set @row:$node]
                    incr @rownum:$node \[expr {(\[set @col:$node] - 1) * 
                         \[set @rows:$node]}]

                    if {\[info exists $name]} {
                        unset $name
                    }

                    if {\[set @rownum:$node] <= \[set $name:rowcount]} {
                        array set $name \[array get $name:\[set @rownum:$node]]
                    } 
         
                    set $name\(rownum) \[set @rownum:$node]
                    set $name\(row)    \[set @row:$node]
                    set $name\(col)    \[set @col:$node]
        "
    } 

    if {$action == "leave"} {
        set name $data(name)
        set cols $data(cols)

        set snippet "
                \}
            \}
            # $node: </grid name=$name cols=$cols>, depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_master {tree code_var action node} {
    upvar 1 $code_var code

    if {$action == "enter"} {
        upvar 0 $tree state

        if {[info exists state(master)]} {
            error "A template cannot serve two masters!"
        }

        array set data [tag_attributes [$tree get $node -key data]]
        if {![info exists data(src)]} {
            if {[info exists ::nstcl::template::default_master]} {
                set src $::nstcl::template::default_master
            } else {
                error "No src given for <master> and no default defined"
            }
        } else {
            set src $data(src)
        }

        set state(master) $src 
        set snippet "
            # $node: <master src=$src> (begin), depth: [$tree depth $node]
            set @master \[file join \[set @pwd] [list $src]]
        "
    } else {
        set snippet "
            # $node: </master> (end), depth: [$tree depth $node]
        " ;#"
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_slave {tree code_var action node} {
    upvar 1 $code_var code

    if {$action == "enter"} {
        set snippet "
            # $node: slave (begin), depth: [$tree depth $node]
            if {\[info exists @slave]} {
                append @output \[set @slave]
            }
        " ;#"
    } else {
        set snippet "
            # $node: slave (end), depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_include {tree code_var action node} {
    upvar 1 $code_var code

    if {$action == "enter"} {
#puts stderr "include attributes: [$tree get $node -key data]"        
        array set data [tag_attributes [string map [list "&amp;" &] \
            [$tree get $node -key data]]]
#puts stderr "include attributes: [array get data]"

        if {![info exists data(src)]} {
            error "No src specified for <include>"
        }

        set src $data(src)
        unset data(src)

        set snippet "
            # $node: <include src=\"$src\">, depth: [$tree depth $node]
            # [list [array get data]]
            if {\[info exists @include]} {
                unset @include
            }
            set @include {}
        "

        foreach key [array names data] {
            set val $data($key)
            set pass_by value

            if {[string range $key 0 0] == "&"} {
                set key [string range $key 1 end]

                if {[string equal $key ""]} {
                    set key $val
                }

                # &users=e_people means pass a reference to e_people named users
                set swap $val
                set val  $key
                set key  $swap

                if {![regexp {^[A-Za-z0-9_]+$} $val]} {
                    error "Invalid variable name \"$val\" for pass by reference"
                }
   
                set pass_by reference
            } 

            if {![regexp {^[A-Za-z0-9_]+$} $key]} {
                error "Invalid variable in <include src=\"$src\" $key=\"...\">"
            }

            if {$pass_by == "value"} {
                append snippet "
                    # pass by $pass_by for: **[list $val]**
                    append @include \"set $key [list $val]\\n\"
                "
            } else {
                append snippet "
                    # pass by $pass_by
                    if {\[info exists $key:rowcount]} {
                        append @include \
                            \"upvar 1 $key:rowcount $val:rowcount\\n\"
                        append @include \
                            \"upvar 1 $key:columns $val:columns\\n\"

                        for {set @i:$node 1} \
                            {\[set @i:$node] <= \[set $key:rowcount]} \
                            {incr @i:$node} {
                            append @include \
                                \"upvar 1 $key:\[set @i:$node] \
                                          $val:\[set @i:$node]\\n\"
                        }
                    } else {
                        append @include \
                            \"upvar 1 $key $val\\n\"
                    }
                "
            }
        }

        append snippet "
            set @curr_pwd:$node \[set @pwd]
            set @include:$node \[::nstcl::template::adp_compile -file \[file join \[set @pwd] [list $src.adp]]]
            if {\[file exists \[file join \[set @pwd] $src.tcl]]} {
                append @include \"\\n### [list $src.tcl] ###\\n\"
                append @include \[::fileutil::cat \[file join \[set @pwd] $src.tcl ] ]  
                append @include \"\\n### [list $src.tcl] ###\\n\"
            }

            # the included output is appended here:
            append @output \[::nstcl::template::adp_eval \
                \[set @include:$node] -include \[set @include]]

            unset @include:$node
            set @pwd \[set @curr_pwd:$node]
        " ;#"
    } else {
        set snippet "
            # $node: </include>, depth: [$tree depth $node]
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_noparse {tree code_var action node} {
    upvar 1 $code_var code

    if {$action == "enter"} {
        upvar 1 $tree state
        set state(noparse) $node
        set state(mode) noparse

        set snippet "
            # $node noparse (begin), depth: [$tree depth $node] <NOPARSE=on>
        "
    } else {
        set snippet "
            # $node noparse (end), depth: [$tree depth $node] <NOPARSE=off>
        "
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::not_parsed {tree code_var action node} {
    upvar 1 $code_var code
    set type [$tree get $node -key type]
    set data [$tree get $node -key data]


    if {$type == "PCDATA"} {
        set snippet [_PCDATA $tree ignore $action $node]
        set snippet [string map [list \\@ @] $snippet]
    } else {
        upvar 0 $tree state
        switch -exact -- $state(mode) {
            noparse  { set var @output }
            property { set var @properties($state(property)) }
            default  { error "Should have been impossible to get here!" }
        }
        
        if {$action == "leave"} {
            set snippet "
                # $node $type (end), depth: [$tree depth $node] /not parsed/
                append $var \"</$type>\"
            "
        } else {
            set snippet "
                # $node $type (begin), depth: [$tree depth $node] /not parsed/
                append $var \[subst [expand_@variables_quoted \
                    "<[string trim "$type $data"]>"]]
            "
        }
    }

    append code $snippet
    return $snippet
}


proc ::nstcl::template::adp::tags::_property {tree code_var action node} {
    upvar 1 $code_var code
    upvar 0 $tree state

    if {$action == "enter"} {
        array set data [tag_attributes [$tree get $node -key data]]
        if {![info exists data(name)]} {
            error "No name attribute specified for <property>"
        }

        set name $data(name)
        if {![regexp {^[A-Za-z0-9_]+$} $name]} {
            error "Invalid variable name for <property name=$name>"
        }

        set state(noparse) $node
        set state(mode) property
        set state(property) $name

        set snippet "
            # $node: <property name=$name>, depth: [$tree depth $node]
            set @properties($name) {}
        "
    } else {
        set snippet "
            # $node: </property name=$state(property)>
        "

        set state(property) ""
    }

    append code $snippet
    return $snippet
}
 

proc ::nstcl::template::adp::protect_chars {chunk} {
    set protected_chars [list \$ \\\$ \
                              \[ \\\[ \
                              \] \\\] \
                              \" \\\" \
                              \" \\\" \
                              \{ \\\{ \
                              \} \\\} \
                              \\ \\\\ ]

    # Restore newlines
    set chunk [string map [list \x04 \r \x1a \n] $chunk]
    return [string map $protected_chars $chunk]
}


proc ::nstcl::template::adp::expand_@variables {chunk} {
    regsub -all -- {@([A-Za-z0-9_]+)@} $chunk {[set \1]} chunk
    regsub -all -- {@([A-Za-z0-9_]+):([A-Za-z0-9_]+)@} $chunk \
        {[set \1:\2]} chunk
    regsub -all -- {@([A-Za-z0-9_]+)\.([A-Za-z0-9_]+)@} $chunk \
        {[set \1(\2)]} chunk

    return $chunk
}


proc ::nstcl::template::adp::expand_@variables_quoted {chunk} {
    # protect characters with special Tcl meaning
    return "[list [expand_@variables [protect_chars $chunk]]]"
}


proc ::nstcl::template::adp::tag_attributes {attributes} {
    array set data {}

    set RE {^([^\s=]+)(?:=((?:"[^"]*")|(?:'[^']*')|(?:\S+))|\s)?\s*(.*)$} ;#"
    while {[regexp $RE $attributes => key value attributes]} {
        if {[regexp {^(['"])(.*)(['"])$} $value => first middle last] &&
            [string equal $first $last]} {
            set value $middle
        }

        set key   [expand_@variables [protect_chars $key]]
        set value [expand_@variables [protect_chars $value]]

#        set key   [subst $key]
#        set value [subst $value] 

        set data($key) $value
    }

    return [array get data]
}


proc ::nstcl::template::adp::pseudo_code_to_tcl {pseudo_code {pwd ""}} {
    variable counter
    incr counter

    if {[string equal $pwd ""]} {
        set pwd [pwd]
    }

    # trim trailing /. off if present
    regexp {^(.*)/\.$} $pwd => pwd

    set tree [struct::tree ::nstcl::template::adp::__adp$counter]
    htmlparse::2tree $pseudo_code $tree
    array set $tree [list noparse "" mode normal pwd $pwd]

    $tree walk root -order both -type dfs -command {
        set node %n
        set tree %t
        set action %a
        set type [$tree get $node -key type]
        $tree set $node -key data [string trim [$tree get $node -key data] \x00]

        upvar 0 $tree state
        if {$state(noparse) == ""} {
            if {[info command tags::_$type] != ""} {
                tags::_$type $tree code $action $node
            } else {
                append code "# $action [string toupper $type] ($node)"
            }
        } else {
            if {$state(noparse) == $node && $action == "leave"} {
                tags::_$type $tree code $action $node
                set state(noparse) ""
                set state(mode) normal
            } else {
                tags::not_parsed $tree code $action $node
            }
        }
    }

    $tree destroy 
    unset $tree
#puts $code
    return $code
}


proc ::nstcl::template::adp::parental_node {tree node_id parent_type} {
    while 1 {
        set node_id [$tree parent $node_id]
        set type    [$tree get $node_id -key type]
        if {[string equal $type $parent_type]} {
            return $node_id
        }
    }
}


::nstcl::ad_proc ::nstcl::template::adp_compile {source_type source} {
    summary "Converts an ADP template into a chunk of Tcl code."
  
    syntax {
        <p><command>::nstcl::adp_compile</command> <m>source_type</m> <m>source</m></p>

        <p><command>::template::adp_compile</command> <m>source_type</m> <m>source</m></p>
    }

    description {
        <p>Converts an ADP template into a chunk of Tcl code and saves
        it as a procedure so that subsequent re-compilation is not necessary
        for the same template.</p>

        <p><i>source_type</i> should be one of: "-file", "-url", or 
        "-string".  <i>source</i> should be the corresponding filename, 
        URL, or string.</p>

        <p><b>adp_compile</b> returns a token (the generated procedure name
        actually), which should be passed to <b>adp_eval</b> to evaluate
        the compiled template.</p>
    }

    see_also {
        <p>See the <b>adp</b> documentation for information on the structure
        of ADP templates.</p>
    }

    keywords "adp templating"
} {
    package require fileutil
    package require htmlparse
    package require struct
    package require md5

    # htmlparse 0.2 in tcllib has a "bug" in that it maps \ to &bsl;
    # but then doesn't have that mapping in mapEscapes to map back
    # to a regular \, so we'll "fix" it here if it's still broken locally...

    if {![info exists ::htmlparse::escapes(bsl)]} {
        set ::htmlparse::escapes(bsl) \\
    }

    switch -exact -- $source_type {
        -file   { set adp [::fileutil::cat $source] }
        -url    { set adp [::nstcl::ns_httpget $source] }
        -string { set adp $source }
        default { 
            error "Should be one of -file, -string, or -url,\
                not \"$source_type\"" 
        }
    }

    if {$source_type == "-file"} {
        set pwd [file join [pwd] [file dirname $source]]
    } else {
        set pwd [pwd]
    }

    # trim trailing /. off if present
    regexp {^(.*)/\.$} $pwd => pwd

    set adp [string trim $adp]
    set md5 [::md5::md5 $pwd,$adp]
    set proc ::nstcl::template::adp::compiled::$md5
    
    if {[info proc $proc] == ""} {
        set pseudo_code [adp::adp_to_pseudo_code $adp]
        set tcl_script  [adp::pseudo_code_to_tcl $pseudo_code $pwd]

        proc $proc @base_level "
            incr @base_level
#            puts stderr \"This is: $proc\"
#            uplevel \[set @base_level] { 
#                puts stderr \"vars: \[join \[lsort \[info vars *]] \\n]\"
#                puts stderr \"level: \[info level]\"
#            }
            set result \[uplevel \[set @base_level] [list $tcl_script]]
#            puts stderr \"Result from $proc was: \$result\"
            return \$result
        " ;#"
    }

    return $proc
}


::nstcl::ad_proc ::nstcl::template::adp_eval {compiled_proc {mode -inline} 
                                              {code ""}} {
    summary "Evaluates a compiled template"

    syntax {
        <p><command>::nstcl::adp_eval</command> <m>compiled_proc</m></p>
        <p><command>::template::adp_eval</command> <m>compiled_proc</m></p>
    }

    description {
        <p>Evaluates a compiled template in the calling stack frame, and 
        returns the output from the evaluating the compiled template code.</p>
    }

    keywords "adp templating"
} {
    switch -exact -- $mode {
        -inline  { set uplevel 1 }
        -include { set uplevel 0 }
        -slave   -
        -master  { set uplevel 0 }
        default  { error "Invalid mode \"$mode\"" }
    }

    if {$mode == "-inline" && [string length $code]} {
        error "code block cannot be passed when mode is -inline!"
    }

#    puts stderr "$mode"
#    puts stderr [string repeat = 80]
#    puts stderr $code
#    puts stderr [string repeat = 80]

    eval $code
    return [$compiled_proc $uplevel]
}


::nstcl::ad_proc ::nstcl::template::multirow {subcommand args} {
    summary "Access and modify rows and columns of a multirow data source"
    syntax {
        <p><command>::nstcl::multirow</command> <option>option</option> <m>arg</m> ?<m>...</m>?</p>
        <p><command>::template::multirow</command> <option>option</option> <m>arg</m> ?<m>...</m>?</p>
    }

    description {
        <p>This command is an alternative to <b>db_multirow</b> for creating
        multirow data sources, and also allows you to modify an existing
        multirow data source.</p>

        <commandlist{>
            <commanddef{>
                <command{>
                    <b>multirow</b> <method>append</method> <m>name</m>
                    <m>value</m> ?<m>value ...</m>?
                </command}>
                
                <desc{>
                    Append a new row to the end of the data source.  Any
                    extra values are ignored, and any missing values
                    will default to the empty string.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>multirow</b> <method>create</method> <m>name</m>
                    <m>column</m> ?<m>column ...</m>?
                </command}>
 
                <desc{>Creates a new multirow data source.</desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>multirow</b> <method>get</method> <m>name</m>
                    <m>index</m> ?<m>column</m>?
                </command}>

                <desc{>
                    If column is specified, the value of that column,
                    from the specified row, is returned.  If column is
                    omitted, then <i>name</i> will be set to a reference
                    to an array containing the values for the specified
                    row.  <i>index</i> is a natural number, and the
                    rows in a data source are always indexed beginning
                    at 1 and not at zero.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>multirow</b> <method>set</method> <m>name</m> 
                    <m>index</m> <m>column</m> <m>value</m>
                </command}>
                
                <desc{>Sets the value of a column in the specified row.</desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>multirow</b> <method>size</method> <m>name</m>
                </command}>

                <desc{>Returns the number of rows in the data source.</desc}>
            </commanddef}>
        </commandlist}>
    }

    see_also "db_multirow"
    keywords "adp templating"
} {
    if {![string equal _$subcommand [namespace tail \
        [info proc ::nstcl::template::multirow::_$subcommand]]]} { 

        set commands [list]
        foreach command [info procs ::nstcl::template::multirow::*] {
            lappend commands [string trimleft [namespace tail $command] _]
        }

        error "Invalid option \"$subcommand\".  Should be one of: \
            [join $commands ", "]"
    }

    eval ::nstcl::template::multirow::_$subcommand $args
}


proc ::nstcl::template::multirow::_get {name index {column ""}} {
    if {[string length $column]} {
        upvar 2 $name:$index row
        return $row($column)
    } else {
        uplevel 2 upvar 0 $name:$index $name
    }
}


proc ::nstcl::template::multirow::_set {name index column value} {
    upvar 2 $name:$index row
    set row($column) $value
}


proc ::nstcl::template::multirow::_size {name} {
    upvar 2 $name:rowcount size
    return $size
}


proc ::nstcl::template::multirow::_create {name args} {
    upvar 2 $name:rowcount rowcount
    upvar 2 $name:columns  columns

    set rowcount 0
    set columns [list]

    foreach arg $args {
        lappend columns $arg
    }
}


proc ::nstcl::template::multirow::_append {name args} {
    upvar 2 $name:rowcount rowcount
    upvar 2 $name:columns  columns

    incr rowcount

    upvar 2 $name:$rowcount row
    foreach column $columns arg $args {
        set row($column) $arg
        set row(rownum) $rowcount
    }
}


proc ::nstcl::template::multirow::_extend {name args} {
    upvar 2 $name:columns  columns
    upvar 2 $name:rowcount rowcount

    foreach arg $args {
        lappend columns $arg
        for {set i 1} {$i <= $rowcount} {incr i} {
            upvar 2 $name:$i row
            set row($arg) ""
        }
    }
}


proc ::nstcl::template::multirow::_map {" name" " body"} {
    # we use variables with a leading space to minimize the chance of name 
    # collision with possible columns, etc. in the multirow datasource

    upvar 2 $name:rowcount " rowcount"
    upvar 2 $name:columns  " columns"
    set " results" {}

    for {set " i" 1} {[set " i"] <= [set " rowcount"]} {incr " i"} {
        upvar 2 [set " name"]:[set " i"] " row"
        foreach " column" [set " columns"] {
            set [set " column"] [set \ row(\ column)] 
        }

        lappend " results" [eval [set " body"]]
    }

    return [set " results"]
}


namespace eval ::nstcl::template::adp::tags {
    namespace import ::nstcl::template::adp::*
}

namespace eval ::nstcl::template::adp::tags::_if {
    namespace import ::nstcl::template::adp::*
}


::nstcl::ad_proc ::nstcl::template::adp {args} {
    summary "The nstcl templating system"
    syntax ""

    description {
        The <b>nstcl</b> templating system is a re-implementation of the
        templating portions of the ArsDigita Community System's publishing 
        system (as it existed in the ACS 4.2 Tcl version).
    }

    other_sections {
        variables 
        multiple_tag
        group_tag
        grid_tag
        list_tag
        if_tag
        include_tag
        master_tag
        slave_tag
        property_tag
        noparse_tag
    }

    variables {
        <p>Variables server as placeholders for dynamic data.  There are
        two types of variables: scalars and rows.  A variable name may be
        made up of any alphanumeric character, plus the underscore, surrounded
        by @ signs.  The columns of a row variable are referenced by
        separating the data source name and the column with a period.
        A handful of special variables (that the templating system
        automatically makes available in certain situtation) contain
        a colon.</p>

        <p>A scalar variable looks like: <i>@email_address@</i>.  When the
        template is processed the Tcl variable <i>email_address</i> will
        be substituted in place of <i>@email_address@</i>.</p>

        <p>A list variable, inside of a &lt;<b>list</b>&gt; tag, looks like:
        <i>@listname:item@</i>.</p>

        <p>A row variable looks like: <i>@user.city@</i> <i>@user.state@</i>
        <i>@user.zipcode@</i>, etc.  This example could be a one row
        data source (in which case the templating system would look
        for the Tcl array <i>user</i> and substitute its contents), or
        a multirow datasource built up with either the <b>multirow</b>
        templating command, or obtained from a database with
        <b>db_multirow</b>, and used inside of either a &lt;<b>multirow</b>&gt; 
        or &lt;<b>grid</b>&gt; templating tag.</p>

        <p>With the exception of the special <i>@datasource:rowcount@</i>
        variable, multirow and list variables may only be referenced
        inside a containing &lt;<b>multiple</b>&gt;, &lt;<b>grid</b>&gt;,
        or &lt;<b>list</b>&gt; tag.</p>

        <p>Naturally an attempt to reference a variable that does not
        exist results in an error.  You can use the &lt;<b>if</b>&gt; 
        templating tag to check and see whether a variable exists or not.</p>
    }

    multiple_tag {
        <p>The &lt;<b>multiple</b>&gt; tag is used to repeat a portion of
        the template (contained between &lt;multiple&gt; ... 
        &lt;/multiple&gt;), for every row in a specified datasource.  It
        has one required parameter: the <i>name</i> of the multirow data source.
        Multirow datasources can be created with either the <b>multirow</b>
        command, or queried directly from a database with the
        <b>db_multirow</b> command.</p>

        <dle>
        <dt>EXAMPLE: <i>Generating the datasource</i></dt>
        <dd>
<example>
    # In the Tcl datasource file
    db_multirow friends friends_query {
        select name, email, phone, flavor as favorite_ice_cream
          from friends
         order by favorite_ice_cream desc, name
    }
</example>
        </dd>

        <dt>EXAMPLE: <i>In the adp template</i></dt>
        <dd>
<example>
    &lt;!-- in the adp file, to render an HTML table --&gt;
    &lt;table&gt;
    &lt;multiple name="friends"&gt;
        &lt;tr&gt;
            &lt;td&gt;@friends.name@&lt;/td&gt;
            &lt;td&gt;@friends.email@&lt;/td&gt;
            &lt;td&gt;@friends.phone@&lt;/td&gt;
            &lt;td&gt;@friends.favorite_ice_cream@&lt;/td&gt;
        &lt;/tr&gt;
    &lt;/multiple&gt;
    &lt;/table&gt;
</example>
        </dd>

        <dt>LIMITING THE NUMBER OF ROWS:</dt>
        <dd>The <i>maxrows</i> attribute may be used to limit the 
            number of rows that are processed: 
<example>
        &lt;multiple name="foo" maxrows="10"&gt; ... &lt;/multiple&gt;
</example>
        </dd>

        <dt>SKIPPING SOME INITIAL ROWS:</dt>
        <dd>The <i>startrow</i> attribute may be used to skip some
            initial number of rows: 
<example>
        &lt;multiple name="bar" startrow="5"&gt; ... &lt;/multiple&gt;
</example>
        </dd>

        <dt>SPECIAL VARIABLES:</dt>
        <dd>You can determine the number of rows with 
            <i>@datasource:rowcount@</i>.  Along the same lines,
            the variable <i>@datasource.rownum@</i> can be tested
            to determine the current row being evaluated.</dd>

        </dle>
    }

    group_tag {
        <p>The &lt;<b>group</b>&gt; tag is used inside of the body of a 
        &lt;<b>multiple</b>&gt; tag to allow additional formatting control
        between rows of a multirow datasource.  It is illegal to use
        this tag outside of a &lt;<b>multiple</b>&gt; tag.</p>

        <p>The &lt;<b>group</b>&gt; tag takes the name of a column and repeats
        it's enclosed template section for every row as long as the value
        of the column does not change from row to row.  For finer grained 
        control &lt;<b>group</b>&gt; tags may be nested.</p>

        <dle>
        <dt>EXAMPLE: <i>In the adp template</i></dt>
        <dd>
<example>
    &lt;!-- in the adp file, assume same datasource as last example --&gt;
    &lt;h1&gt;My Ice-Cream Loving Friends&lt;/h1&gt;
    &lt;multiple name="friends"&gt;
        &lt;h3&gt;@friends.favorite_ice_cream@&lt;/h3&gt;
        &lt;ul&gt;
        &lt;group column="favorite_ice_cream"&gt;
            &lt;li&gt;@friends.name@ (@friends.phone@)
        &lt;/group&gt;
        &lt;/ul&gt;
    &lt;/multiple&gt;
</example>
        </dd>
        </dle>
    }

    grid_tag {
        <p>The &lt;<b>grid</b>&gt; tag is simillar to the 
        &lt;<b>multiple</b>&gt; tag--it iterates over the rows of a 
        datasource--however, instead of iterating sequentially over each
        row (1, 2, 3, etc.) the &lt;<b>grid</b>&gt; tag iterates in
        column order.  The &lt;<b>grid</b>&gt; tag has two required parameters:
        <i>cols</i>, the number of columns, along with the <i>name</i> of the
        multirow datasource.</p>

        <dle> 
        <dt>EXAMPLE:</dt>
        <dd>If a datasource had 10 rows, and the number of columns was
        specified as <i>3</i>, the rows would be output in this evaluated in
        this order: 1, 5, 9, 2, 6, 10, 3, 7, <i>null (11)</i>, 4, 8, 
        <i>null (12)</i>.  The two <i>null</i> rows are included since 10 
        is not evenly divisible by 3.</dd>
        </dle>

        <dle>
        <dt>SPECIAL VARIABLES:</dt>
        <dd><i>@datasource.rownum@</i> is equal to the current rownum in
        the datasource that is being processed.  <i>@datasource:rowcount@</i>
        is equal to the number of rows in the datasource.  If the number of 
        rows is not evenly divisible by the number of columns then there
        will be iterations when <i>@datasource.rownum@</i> is greater than
        <i>@datasource:rowcount@</i>.  Additionally, <i>@datasource.row@</i>
        and <i>@datasource.col@</i> are set to the current row and column 
        number of the grid.</dd>
        </dle>
    }

    list_tag {
        <p>The &lt;<b>list</b>&gt; repeats a template section for each
        element of a list.  It takes one parameter, <i>name</i>, which 
        is the name of the list (a regular Tcl variable in the datasource).</p>

        <dle>
        <dt>EXAMPLE:</dt>
        <dd>
<example>
    &lt;ul&gt;
    &lt;list name="files"&gt;
        &lt;li&gt; @files:item@
    &lt;/list&gt;
    &lt;/ul&gt;
</example>
        </dd>

        <dt>SPECIAL VARIABLES:</dt>
        <dd><i>@datasource:rownum@</i> is the current position within the list 
        (the first element being 1, not 0), and <i>@datasource:rowcount@</i> 
        is equal to the number of elements in the list.  
        <i>@datasource:item@</i> is the element itself.  Of these, only the
        <i>@datasource:rownum@</i> may be used outside of the body of the
        &lt;<b>list</b>&gt; tag.</dd>
        </dle>
    }

    if_tag {
        <p>The &lt;<b>if</b>&gt; tag tests a condition and selectively 
        evaluates a template section only when the condition is true.  Any
        templating variable may be used in the &lt;<b>if</b>&gt;.  Literal
        text which contains spaces should be enclosed in quotation marks.</p>

        <p>Compound statements may be used be connected with the keywords 
        <i>and</i> &amp; <i>or</i>.  There is no grouping mechanism that 
        changes the order of operation.  An &lt;<b>else</b>&gt; tag may be used
        following an &lt;<b>if</b>&gt; tag.  As you would expect,
        the &lt;<b>else</b>&gt; tag is evaluated only when the 
        condition is not true.</p>

       <p>There are six unary operators: <i>nil</i>, <i>defined</i>,
       <i>odd</i>, <i>even</i>, <i>true</i>, and <i>false</i>.</p>

       <p>There are six binary operators: <i>gt</i> (greater than),
       <i>ge</i> (greater than or equal to), <i>lt</i> (less than),
       <i>le</i> (less than or equal to), <i>eq</i> (equals), and
       <i>ne</i> (not equals).</p>

       <p>There is one ternary operator: <i>between</i>.</p>

       <p>There is one n-ary operator, <i>in</i> which, if used in 
       a compound expression must come last.</p>

       <p>Any operator can be negated by prepending the <i>not</i> keyword.</p>

       <dle>
       <dt>EXAMPLES:</dt>
       <dd>
<example>
    &lt;if @email@ eq "bgates@microsoft.com"&gt;
        You are a rich monopolist!
    &lt;/if&gt;

    &lt;if @temperature@ lt 65&gt;
        It's chilly.
    &lt;/if&gt;
    &lt;else&gt;
        &lt;if @temperature@ between 65 80&gt;
            It's comfortable.
        &lt;/if&gt;
        &lt;if @temperature@ not between 65 80&gt;
            It's hot!
        &lt;/if&gt;
    &lt;/else&gt;

    &lt;if @platform@ in Unix Macintosh Windows&gt;
        You can run Tcl/Tk!
    &lt;/if&gt;

    &lt;multiple name="datasource"&gt;
        &lt;if @datasource.rownum@ odd and @datasource.rownum@ ne @datasource:rowcount@&gt;
            Row #@datasource.rownum@ is odd, and it isn't the last row.
        &lt;/if&gt;
    &lt;/multiple&gt;

    &lt;if @foo@ defined&gt;
        This is true when the variable foo exists.
    &lt;/if&gt;

    &lt;if @foo@ not defined&gt;
        This is true when the variable foo does not exist.
    &lt;/if&gt;

    &lt;if @foo@ not nil&gt;
        This is true when the variable foo exists and it's value
        is not the empty string.
    &lt;/if&gt;

    &lt;if @var@ nil&gt;
        This is true when either the variable foo does not exist, or
        it does, but is equal to the empty string.
    &lt;/if&gt;
</example>
        </dd>
        </dle>
    }

    include_tag {
        <p>The &lt;<b>include</b>&gt; tag allows you to (surprise) include 
        another template within the current template.  The only
        required parameter is <i>src</i>, which should be the path
        to the .adp file of the template.</p>

        <p>Other arguments (given as <i>key</i>=<i>val</i>)  are passed to 
        the included template as variables.   If <i>key</i> is prefixed
        by an <i>&amp;</i>, then the datasource <i>val</i> in the current
        template will be passed to the included template by reference.
        In this case, <i>key</i> and <i>val</i> may be, but do not need
        to be, the same.  A shorthand notation of <i>&amp;=val</i> may
        be used when passing a datasource by reference, and the name
        of the datasource is to be the same in both the current and
        the included template.</p>

        <dle>
        <dt>EXAMPLE:</dt>
        <dd>
<example>
    &lt;include src="include-me.adp" email=@email@ &amp;=vital_stats &amp;friends="amigos"&gt;
</example>
        </dd>
        </dle>
    }

    master_tag {
        <p>The &lt;<b>master</b>&gt;, which has one attribute, <i>src</i>,
        specifies the name of another template in which the output of the
        current template should be placed.  The location of the current
        templates output within the master template is determined by the
        location of the &lt;<b>slave</b>&gt; tag in the master template.</p>

        <p>A master template may itself be a slave to another master,
        provided circular loops are avoided.</p>

        <p>A slave may propogate variables up to it's master by using the
        &lt;<b>property</b>&gt; tag.</p>    
    }

    slave_tag {
        <p>The &lt;<b>slave</b>&gt; tag marks the position within the
        master template where the output of the slave template should
        be placed.</p>

        <dle>
        <dt>EXAMPLE:</dt>
        <dd>
<example>
    &lt;html&gt;
    &lt;head&gt;
    &lt;if @title@ not nil&gt;&lt;title&gt;@title@&lt;/title&gt;&lt;/if&gt;
    &lt;body&gt;
    &lt;if @title@ not nil&gt;&lt;h1&gt;@title@&lt;/h1&gt;&lt;/if&gt;
    &lt;else&gt;&lt;h1&gt;Unknown Document&lt;/h1&gt;&lt;/else&gt;
    &lt;hr&gt;
    &lt;blockquote&gt;
        &lt;slave&gt;
    &lt;/blockquote&gt;
    &lt;/body&gt;
    &lt;/html&gt;
</example>
        </dd>
        </dle>
    }

    property_tag {
        <p>The &lt;<b>property</b>&gt; tag is used to propogate variables
        upwards from the slave to its master.  The &lt;<b>property</b>&gt;
        tag has one attribute, <i>name</i>, which specifies the name
        of the variable to be set in the master template.  The value
        of the variable set in the master is the content enclosed by the
        &lt;<b>property</b>&gt;...&lt;<b>/property</b>&gt; tags.</p>

        <dle>
        <dt>EXAMPLE:</dt>
        <dd>
<example>
    &lt;master src="master"&gt;
    &lt;property name="title"&gt;My homepage as of @todays_date@&lt;/property&gt;
    This is my homepage and today is @todays_date@ ...
</example>
        </dd>
        </dle>
    }

    noparse_tag {
        <p>The &lt;<b>noparse</b>&gt; tag is used to encapsulate a section
        of a template whose template tags should not be parsed.  This is
        mostly useful only when one template is dynamically generating
        another.  Normal variables are interpreted, even inside of 
        a &lt;<b>noparse</b>&gt; tag.  Escape the @ signs with a backslash
        to prevent variable substitution from occuring.</p>
    }

    see_also "adp_compile, adp_eval, db_multirow, multirow"
    keywords adp
} return



interp alias {} ::template::adp_compile {} ::nstcl::template::adp_compile
interp alias {} ::nstcl::adp_compile    {} ::nstcl::template::adp_compile
interp alias {} ::template::adp_eval    {} ::nstcl::template::adp_eval
interp alias {} ::nstcl::adp_eval       {} ::nstcl::template::adp_eval
interp alias {} ::template::multirow    {} ::nstcl::template::multirow
interp alias {} ::nstcl::multirow       {} ::nstcl::template::multirow

namespace eval ::nstcl {
    namespace export adp_compile \
                     adp_eval    \
                     multirow
}


package provide nstcl-templating 1.2
