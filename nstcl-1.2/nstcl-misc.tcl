package require nstcl-core
package require nstcl-fwdcompat

# nstcl-1.2/nstcl-misc.tcl
# $Id: nstcl-misc.tcl,v 1.5 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export coalesce \
                     empty_string_p \
                     invert_array \
                     ns_crypt \
                     ns_rand \
                     ns_sleep \
                     ns_uuencode \
                     ns_uudecode \
                     set_difference \
                     set_difference! \
                     set_intersection \
                     set_intersection! \
                     set_member? \
                     set_minus \
                     set_union \
                     set_union! \
                     swap \
                     ad_get_tcl_call_stack \
                     util_commify_number

    variable ns_sleep
    set ns_sleep(counter) 0
}



#
# coalesce
#

::nstcl::ad_proc ::nstcl::coalesce {-default:optional args} {
    syntax {<command>::nstcl::coalesce</command> ?<option>-default value</option>? <m>var1</m> ?<m>var2</m> ... <m>varn</m>?}

    summary {
        Return the value of the first variable that exists and has a non-null 
        value
    }

    description {
        <p>This command will return the value of the first variable that 
        exists in the callers enviroment that does not equal the empty 
        string, i.e. {}.  If the optional <option>-default</option> switch 
        is present and no non-null variables are found to exist then the 
        value given to the <option>-default</option> switch will be 
        returned.</p>
    }
} {
    foreach arg $args {
        upvar 1 $arg x
        if {[info exists x] && ![string equal "" [set x]]} {
            return [set x]
        }
    }
    if {[info exists default]} {
        return $default
    } else {
        return
    }
}



#
# empty_string_p
#

::nstcl::ad_proc ::nstcl::empty_string_p {string} {
    summary "Tests whether a given string is empty"
    description "<p>Returns a 1 if <i>string</i> is empty, 0 otherwise.</p>"
} {
    return [string equal "" $string]
}



#
# invert_array
#

::nstcl::ad_proc ::nstcl::invert_array {-append:boolean -split:boolean
                                        {-mode get} old_var_name 
                                        {new_var_name ""}} {
    summary "Invert the contents of an array."

    description {
        Inverts an array.  If foo(bar) == baz then the new array will have
        foo(baz) == bar.  This assumes that both the keys and values are unique
        or that the caller is willing to live with lost data.
    }

    optional_switches {
        <dle>
            <dt><option>-append</option></dt>
            <dd>
                If specified, duplicate inverted values are appended as a list
                rather than being blindly overwritten.  For example,
                if <i>foo(bar) == 123</i>, and <i>foo(baz) == 123</i>, and 
                <i>-append</i> is specified then <i>foo(123) == {bar baz}</i>
                as opposed to either <i>foo(123) == bar</i> or 
                <i>foo(123) == baz</i>.
            </dd>
                   
            <dt><option>-split</option></dt>
            <dd>
                If specified, values being split are treated as a list and
                each individual element of the list is added to the inverted
                array as a separate key.  For example, if 
                <i>foo(123) == {bar baz}</i>, and <i>-split</i> were specified,
                then <i>foo(bar) == 123</i> and <i>foo(baz) == 123</i> instead
                of <i>foo(bar baz) == 123</i>.
            </dd>

            <dt><option>-mode</option> (defaults to <i>get</i>)</dt>
            <dd>
                This switch controls how the data is returned.

                <b>get</b> returns the inverted array in a format suitable
                for use with <i>[array set]</i>.

                <b>update</b> updates the contents of the <i>old_var_name</i>
                array in the callers enviroment, in place.

                <b>new</b> creates a new array, named <i>new_var_name</i>,
                in the callers enviroment.  It is an error to specify this
                switch without providing a value for <i>new_var_name</i>, or
                to specify one with either <b>-mode get</b> or 
                <b>mode -update</b>.
            </dd> 
        </dle>
    }
} {
    set modes [list update get new]
 
    if {[lsearch -exact $modes $mode] == -1} {
        error "Invalid mode \"$mode\".  Should be one of [join $modes ", "]."
    }
 
    if {![string equal "" $new_var_name] && $mode != "new"} {
        error "new_var_name should be specified only with -mode new"
    }
 
    if {[string equal "" $new_var_name] && $mode == "new"} {
        error "new_var_name required with -mode new"
    }
 
    upvar 1 $old_var_name old_array
 
    if {![info exists old_array] || ![array exists old_array]} {
        error "$old_var_name does not exist as an array!"
    } else {
        set old_data [array get old_array]
    }
 
    if {$mode == "update"} {
        unset old_array
        upvar 1 $old_var_name new_array
    }
 
    if {$mode == "new"} {
        upvar 1 $new_var_name new_array
    }
 
    if {[info exists new_array] && ![array exists new_array]} {
        error "new_array already exists but isn't an array!"
    }
 
    array set new_array {}
 
 
    switch -exact -- $split_p.$append_p {
        0.0 {
                # no split, no append
                foreach {key value} $old_data {
                    set new_array($value) $key
                }
            }
 
        0.1 {
                # no split, append
                foreach {key value} $old_data {
                    lappend new_array($value) $key
                }
            }
 
        1.0 {
                # split, no append
                foreach {key value} $old_data {
                    foreach split_value $value {
                        set new_array($split_value) $key
                    }
                }
            }
 
        1.1 {
                # split, append
                foreach {key value} $old_data {
                    foreach split_value $value {
                        lappend new_array($split_value) $key
                    }
                }
            }
    }
 
 
    if {$mode == "get"} {
        return [array get new_array]
    }
}



#
# ns_crypt_pure_tcl
#

::nstcl::ad_proc ::nstcl::ns_crypt_pure_tcl {password salt} {
    syntax "<command>::nstcl::ns_crypt</command> <m>key</m> <m>salt</m>"
    summary "Tcl implementation of the C crypt function"

    description {
        This function encrypts the <i>key</i> using the <i>salt</i> and
        returns the result.  The same algorithm as the Unix crypt command
        is used.
    }
} {
    array set IP {
         0 58  1 50  2 42  3 34  4 26  5 18  6 10  7 2
         8 60  9 52 10 44 11 36 12 28 13 20 14 12 15 4
        16 62 17 54 18 46 19 38 20 30 21 22 22 14 23 6
        24 64 25 56 26 48 27 40 28 32 29 24 30 16 31 8
        32 57 33 49 34 41 35 33 36 25 37 17 38  9 39 1
        40 59 41 51 42 43 43 35 44 27 45 19 46 11 47 3
        48 61 49 53 50 45 51 37 52 29 53 21 54 13 55 5
        56 63 57 55 58 47 59 39 60 31 61 23 62 15 63 7}

    array set FP {
         0 40  1 8  2 48  3 16  4 56  5 24  6 64  7 32
         8 39  9 7 10 47 11 15 12 55 13 23 14 63 15 31
        16 38 17 6 18 46 19 14 20 54 21 22 22 62 23 30
        24 37 25 5 26 45 27 13 28 53 29 21 30 61 31 29
        32 36 33 4 34 44 35 12 36 52 37 20 38 60 39 28
        40 35 41 3 42 43 43 11 44 51 45 19 46 59 47 27
        48 34 49 2 50 42 51 10 52 50 53 18 54 58 55 26
        56 33 57 1 58 41 59  9 60 49 61 17 62 57 63 25}

    array set PC1_C {
         0 57  1 49  2 41  3 33  4 25  5 17  6  9
         7  1  8 58  9 50 10 42 11 34 12 26 13 18
        14 10 15  2 16 59 17 51 18 43 19 35 20 27
        21 19 22 11 23  3 24 60 25 52 26 44 27 36}

    array set PC1_D {
         0 63  1 55  2 47  3 39  4 31  5 23  6 15
         7  7  8 62  9 54 10 46 11 38 12 30 13 22
        14 14 15  6 16 61 17 53 18 45 19 37 20 29
        21 21 22 13 23  5 24 28 25 20 26 12 27  4}

    array set shifts {
        0 1 1 1  2 2  3 2  4 2  5 2  6 2  7 2
        8 1 9 2 10 2 11 2 12 2 13 2 14 2 15 1}

    array set PC2_C {
         0 14  1 17  2 11  3 24  4  1  5  5
         6  3  7 28  8 15  9  6 10 21 11 10
        12 23 13 19 14 12 15  4 16 26 17  8
        18 16 19  7 20 27 21 20 22 13 23  2}

    array set PC2_D {
         0 41  1 52  2 31  3 37  4 47  5 55
         6 30  7 40  8 51  9 45 10 33 11 48
        12 44 13 49 14 39 15 56 16 34 17 53
        18 46 19 42 20 50 21 36 22 29 23 32}

    array set e {
         0 32  1  1  2  2  3  3  4  4  5  5
         6  4  7  5  8  6  9  7 10  8 11  9
        12  8 13  9 14 10 15 11 16 12 17 13
        18 12 19 13 20 14 21 15 22 16 23 17
        24 16 25 17 26 18 27 19 28 20 29 21
        30 20 31 21 32 22 33 23 34 24 35 25
        36 24 37 25 38 26 39 27 40 28 41 29
        42 28 43 29 44 30 45 31 46 32 47  1}

    array set S {
        0,0  14 0,1   4 0,2  13 0,3   1 0,4   2 0,5  15 0,6  11 0,7   8
        0,8   3 0,9  10 0,10  6 0,11 12 0,12  5 0,13  9 0,14  0 0,15  7
        0,16  0 0,17 15 0,18  7 0,19  4 0,20 14 0,21  2 0,22 13 0,23  1
        0,24 10 0,25  6 0,26 12 0,27 11 0,28  9 0,29  5 0,30  3 0,31  8
        0,32  4 0,33  1 0,34 14 0,35  8 0,36 13 0,37  6 0,38  2 0,39 11
        0,40 15 0,41 12 0,42  9 0,43  7 0,44  3 0,45 10 0,46  5 0,47  0
        0,48 15 0,49 12 0,50  8 0,51  2 0,52  4 0,53  9 0,54  1 0,55  7
        0,56  5 0,57 11 0,58  3 0,59 14 0,60 10 0,61  0 0,62  6 0,63 13
        1,0  15 1,1   1 1,2   8 1,3  14  1,4  6 1,5  11 1,6   3 1,7   4
        1,8   9 1,9   7 1,10  2 1,11 13 1,12 12 1,13  0 1,14  5 1,15 10
        1,16  3 1,17 13 1,18  4 1,19  7 1,20 15 1,21  2 1,22  8 1,23 14
        1,24 12 1,25  0 1,26  1 1,27 10 1,28  6 1,29  9 1,30 11 1,31  5
        1,32  0 1,33 14 1,34  7 1,35 11 1,36 10 1,37  4 1,38 13 1,39  1
        1,40  5 1,41  8 1,42 12 1,43  6 1,44  9 1,45  3 1,46  2 1,47 15
        1,48 13 1,49  8 1,50 10 1,51  1 1,52  3 1,53 15 1,54  4 1,55  2
        1,56 11 1,57  6 1,58  7 1,59 12 1,60  0 1,61  5 1,62 14 1,63  9

        2,0  10 2,1   0 2,2   9 2,3  14 2,4   6  2,5  3 2,6  15 2,7   5
        2,8   1 2,9  13 2,10 12 2,11  7 2,12 11 2,13  4 2,14  2 2,15  8
        2,16 13 2,17  7 2,18  0 2,19  9 2,20  3 2,21  4 2,22  6 2,23 10
        2,24  2 2,25  8 2,26  5 2,27 14 2,28 12 2,29 11 2,30 15 2,31  1
        2,32 13 2,33  6 2,34  4 2,35  9 2,36  8 2,37 15 2,38  3 2,39  0
        2,40 11 2,41  1 2,42  2 2,43 12 2,44  5 2,45 10 2,46 14 2,47  7
        2,48  1 2,49 10 2,50 13 2,51  0 2,52  6 2,53  9 2,54  8 2,55  7
        2,56  4 2,57 15 2,58 14 2,59  3 2,60 11 2,61  5 2,62  2 2,63 12

        3,0   7 3,1  13 3,2  14 3,3   3  3,4  0  3,5  6 3,6   9 3,7  10
        3,8   1 3,9   2 3,10  8 3,11  5 3,12 11 3,13 12 3,14  4 3,15 15
        3,16 13 3,17  8 3,18 11 3,19  5 3,20  6 3,21 15 3,22  0 3,23  3
        3,24  4 3,25  7 3,26  2 3,27 12 3,28  1 3,29 10 3,30 14 3,31  9
        3,32 10 3,33  6 3,34  9 3,35  0 3,36 12 3,37 11 3,38  7 3,39 13
        3,40 15 3,41  1 3,42  3 3,43 14 3,44  5 3,45  2 3,46  8 3,47  4
        3,48  3 3,49 15 3,50  0 3,51  6 3,52 10 3,53  1 3,54 13 3,55  8
        3,56  9 3,57  4 3,58  5 3,59 11 3,60 12 3,61  7 3,62  2 3,63 14

        4,0   2 4,1  12 4,2   4 4,3   1 4,4   7 4,5  10 4,6  11 4,7   6
        4,8   8 4,9   5 4,10  3 4,11 15 4,12 13 4,13  0 4,14 14 4,15  9
        4,16 14 4,17 11 4,18  2 4,19 12 4,20  4 4,21  7 4,22 13 4,23  1
        4,24  5 4,25  0 4,26 15 4,27 10 4,28  3 4,29  9 4,30  8 4,31  6
        4,32  4 4,33  2 4,34  1 4,35 11 4,36 10 4,37 13 4,38  7 4,39  8
        4,40 15 4,41  9 4,42 12 4,43  5 4,44  6 4,45  3 4,46  0 4,47 14
        4,48 11 4,49  8 4,50 12 4,51  7 4,52  1 4,53 14 4,54  2 4,55 13
        4,56  6 4,57 15 4,58  0 4,59  9 4,60 10 4,61  4 4,62  5 4,63  3

        5,0  12 5,1   1 5,2  10 5,3  15 5,4   9 5,5   2 5,6   6 5,7   8
        5,8   0 5,9  13 5,10  3 5,11  4 5,12 14 5,13  7 5,14  5 5,15 11
        5,16 10 5,17 15 5,18  4 5,19  2 5,20  7 5,21 12 5,22  9 5,23  5
        5,24  6 5,25  1 5,26 13 5,27 14 5,28  0 5,29 11 5,30  3 5,31  8
        5,32  9 5,33 14 5,34 15 5,35  5 5,36  2 5,37  8 5,38 12 5,39  3
        5,40  7 5,41  0 5,42  4 5,43 10 5,44  1 5,45 13 5,46 11 5,47  6
        5,48  4 5,49  3 5,50  2 5,51 12 5,52  9 5,53  5 5,54 15 5,55 10
        5,56 11 5,57 14 5,58  1 5,59  7 5,60  6 5,61  0 5,62  8 5,63 13

        6,0   4 6,1  11 6,2   2 6,3  14 6,4  15 6,5   0 6,6   8 6,7  13
        6,8   3 6,9  12 6,10  9 6,11  7 6,12  5 6,13 10 6,14  6 6,15  1
        6,16 13 6,17  0 6,18 11 6,19  7 6,20  4 6,21  9 6,22  1 6,23 10
        6,24 14 6,25  3 6,26  5 6,27 12 6,28  2 6,29 15 6,30  8 6,31  6
        6,32  1 6,33  4 6,34 11 6,35 13 6,36 12 6,37  3 6,38  7 6,39 14
        6,40 10 6,41 15 6,42  6 6,43  8 6,44  0 6,45  5 6,46  9 6,47  2
        6,48  6 6,49 11 6,50 13 6,51  8 6,52  1 6,53  4 6,54 10 6,55  7
        6,56  9 6,57  5 6,58  0 6,59 15 6,60 14 6,61  2 6,62  3 6,63 12

        7,0  13 7,1   2 7,2   8 7,3   4 7,4   6 7,5  15 7,6  11 7,7   1
        7,8  10 7,9   9 7,10  3 7,11 14 7,12  5 7,13  0 7,14 12 7,15  7
        7,16  1 7,17 15 7,18 13 7,19  8 7,20 10 7,21  3 7,22  7 7,23  4
        7,24 12 7,25  5 7,26  6 7,27 11 7,28  0 7,29 14 7,30  9 7,31  2
        7,32  7 7,33 11 7,34  4 7,35  1 7,36  9 7,37 12 7,38 14 7,39  2
        7,40  0 7,41  6 7,42 10 7,43 13 7,44 15 7,45  3 7,46  5 7,47  8
        7,48  2 7,49  1 7,50 14 7,51  7 7,52  4 7,53 10 7,54  8 7,55 13
        7,56 15 7,57 12 7,58  9 7,59  0 7,60  3 7,61  5 7,62  6 7,63 11}

    array set P {
         0 16  1  7  2 20  3 21
         4 29  5 12  6 28  7 17
         8  1  9 15 10 23 11 26
        12  5 13 18 14 31 15 10
        16  2 17  8 18 24 19 14
        20 32 21 27 22  3 23  9
        24 19 25 13 26 30 27  6
        28 22 29 11 30  4 31 25}

    for {set i 0} {$i < 66} {incr i} {
        set block($i) 0
    }

    set pw [split $password ""]
    set pw_pos 0
    for {set i 0} {[scan [lindex $pw $pw_pos] %c c] != -1 && $i < 64} \
        {incr pw_pos} {

        for {set j 0} {$j < 7} {incr j ; incr i} {
            set block($i) [expr {($c >> (6 - $j)) & 01}]
        }
        incr i

    }

    for {set i 0} {$i < 28} {incr i} {
        set C($i) $block([expr {$PC1_C($i) - 1}])
        set D($i) $block([expr {$PC1_D($i) - 1}])
    }

    for {set i 0} {$i < 16} {incr i} {
        for {set k 0} {$k < $shifts($i)} {incr k} {
            set t $C(0)
            for {set j 0} {$j < 27} {incr j} {
                set C($j) $C([expr {$j + 1}])
            }
            set C(27) $t
            set t $D(0)
            for {set j 0} {$j < 27} {incr j} {
                set D($j) $D([expr {$j + 1}])
            }
            set D(27) $t
        }

        for {set j 0} {$j < 24} {incr j} {
            set KS($i,$j) $C([expr {$PC2_C($j) - 1}])
            set KS($i,[expr {$j + 24}]) $D([expr {$PC2_D($j) - 28 - 1}])
        }
    }

    for {set i 0} {$i < 48} {incr i} {
        set E($i) $e($i)
    }

    for {set i 0} {$i < 66} {incr i} {
        set block($i) 0
    }

    set salt [split $salt ""]
    set salt_pos 0
    set val_Z 90
    set val_9 57
    set val_period 46
    for {set i 0} {$i < 2} {incr i} {
        scan [lindex $salt $salt_pos] %c c
        incr salt_pos
        set iobuf($i) $c
        if {$c > $val_Z} {
            incr c -6
        }
        if {$c > $val_9} {
            incr c -7
        }
        incr c -$val_period
        for {set j 0} {$j < 6} {incr j} {
            if {[expr {($c >> $j) & 01}]} {
                set temp $E([expr {6 * $i + $j}])
                set E([expr {6 * $i + $j}]) $E([expr {6 * $i + $j + 24}])
                set E([expr {6 * $i + $j + 24}]) $temp
            }
        }
    }

    set edflag 0
    for {set h 0} {$h < 25} {incr h} {

        for {set j 0} {$j < 64} {incr j} {
            set L($j) $block([expr {$IP($j) - 1}])
        }

        for {set ii 0} {$ii < 16} {incr ii} {
            if {$edflag} {
                set i [expr {15 - $ii}]
            } else {
                set i $ii
            }

            for {set j 0} {$j < 32} {incr j} {
                set tempL($j) $L([expr {$j + 32}])
            }

            for {set j 0} {$j < 48} {incr j} {
                set preS($j) [expr {$L([expr {$E($j) - 1 + 32}]) ^ $KS($i,$j)}]
            }

            for {set j 0} {$j < 8} {incr j} {
                set t [expr {6 * $j}]
                set k $S($j,[expr {($preS($t)              << 5) + \
                                   ($preS([expr {$t + 1}]) << 3) + \
                                   ($preS([expr {$t + 2}]) << 2) + \
                                   ($preS([expr {$t + 3}]) << 1) + \
                                    $preS([expr {$t + 4}])       + \
                                   ($preS([expr {$t + 5}]) << 4)}])
                set t [expr {4 * $j}]
                set f($t)              [expr {($k >> 3) & 01}]
                set f([expr {$t + 1}]) [expr {($k >> 2) & 01}]
                set f([expr {$t + 2}]) [expr {($k >> 1) & 01}]
                set f([expr {$t + 3}]) [expr { $k       & 01}]
            }

            for {set j 0} {$j < 32} {incr j} {
                set L([expr {$j + 32}]) [expr {$L($j) ^ \
                    $f([expr {$P($j) - 1}])}]
            }

            for {set j 0} {$j < 32} {incr j} {
                set L($j) $tempL($j)
            }
        }

        for {set j 0} {$j < 32} {incr j} {
            set t $L($j)
            set L($j) $L([expr {$j + 32}])
            set L([expr {$j + 32}]) $t
        }

        for {set j 0} {$j < 64} {incr j} {
            set block($j) $L([expr {$FP($j) - 1}])
        }

    }

    for {set i 0} {$i < 11} {incr i} {
        set c 0
        for {set j 0} {$j < 6} {incr j} {
            set c [expr {$c << 1}]
            set c [expr {$c | $block([expr {6 * $i + $j}])}]
        }
        incr c $val_period
        if {$c > $val_9} {
            incr c 7
        }
        if {$c > $val_Z} {
            incr c 6
        }
        set iobuf([expr {$i + 2}]) $c
    }

    if {$iobuf(1) == 0} {
        set iobuf(1) $iobuf(0)
    }

    set elements [lsort -integer [array names iobuf]]
    set encrypted ""

    foreach element $elements {
        append encrypted [format %c $iobuf($element)]
    }

    return $encrypted
}

#
# ns_crypt (Use C implementation if available, Tcl otherwise)
#

namespace eval ::nstcl {
    if {![catch { package require crypt }]} {
        interp alias {} ::nstcl::ns_crypt {} crypt
    } else {
        rename ::nstcl::ns_crypt_pure_tcl ::nstcl::ns_crypt
        interp alias {} ::nstcl::ns_crypt_pure_tcl {} ::nstcl::ns_crypt
    }
}



#
# ns_rand
#

::nstcl::ad_proc ::nstcl::ns_rand {{max ""}} {
    summary "Generate a random number"
    description {
        <p>This function generates a random number.  If <i>max</i> is not
        specified the number returned will be a floating point number with
        a value of 0.0 to 1.0 inclusive.  If <i>max</i> is specified, the
        return value will be an integer in the range of 0 to one less than
        <i>max</i>.  <i>max</i> must be a natural number no larger than
        2147483647.</p>
    }
} {
    if {![string is integer $max]} {
        return -code error "expected integer but got \"$max\""
    }

    if {$max == ""} {
        return [expr {rand()}]
    } else {
        if {$max != "" && $max <= 0} {
            return -code error "invalid max \"$max\": must be > 0"
        } else {
            return [expr {int(rand() * $max)}]
        }
    }
}



#
# ns_sleep
#

::nstcl::ad_proc ::nstcl::ns_sleep {seconds} {
    summary "Sleep for a given number of seconds"
    description {
        <p>This function sleeps for the number of seconds specified in 
        seconds.</p>
    }
} {
    # Adapted from http://wiki.tcl.tk/933.html
    variable ns_sleep
    
    if {![string is integer -strict $seconds]} {
        return -code error "expected integer but got \"$seconds\""
    }

    if {$seconds < 0} {
        return -code error "#seconds must be >= 0"
    }

    if {$seconds == 0} then return

    set i [incr ns_sleep(counter)]
    set ns_sleep($i) 0
    after [expr {$seconds * 1000}] set ::nstcl::ns_sleep($i) 1
    vwait ::nstcl::ns_sleep($i)
    unset ::nstcl::ns_sleep($i)
}



#
# ns_uudecode
#

::nstcl::ad_proc -deprecated ::nstcl::ns_uudecode {string} {
    if {[catch { package require base64 }]} {
        return -code error "ns_uudecode requires the base64 package from tcllib"
    } else {
        return [base64::decode $string]
    }
}



#
# ns_uuencode
#

::nstcl::ad_proc -deprecated ::nstcl::ns_uuencode {string} {
    if {[catch { package require base64 }]} {
        return -code error "ns_uuencode requires the base64 package from tcllib"
    } else {
        return [base64::encode $string]
    }
}



#
# set_append!
#

::nstcl::ad_proc ::nstcl::set_append! {set_name element} {
    summary "Conditionally adds an element to a set"
    description {
        <p>Adds an element to the set named <i>set_name</i> (in the 
        calling enviroment) if it isn't already a member of said set.</p>
    }
} {
    upvar 1 $set_name list
    if {[lsearch -exact $list $element] == -1} {
        lappend list $element
    }
}



#
# set_difference
#

::nstcl::ad_proc ::nstcl::set_difference {x y} {
    summary "Returns the difference of two sets"
    description {
        Returns the different of sets <i>u</i> and <i>v</i>.  That is, all
        the members of <i>u</i> that are not also members of <i>v</i>.
    }
} {
    set z [list]
    if {[package provide Tcl] < 8.4} {
        foreach element $x {
            if {[lsearch -exact $y $element] == -1} {
                lappend z $element
            }
        }
    } else {
        set y [lsort $y]
        foreach element $x {
            if {[lsearch -sorted -exact $y $element] == -1} {
                lappend z $element
            }
        }
    }

    return $z
}



#
# set_difference!
#

::nstcl::ad_proc ::nstcl::set_difference! {set_name set} {
    summary "Compute and update the difference of two sets"
    description {
        <p>Computes the difference of the set stored in the variable named 
        <i>set_name</i> in the calling environment and the set <i>set</i>, 
        sets the variable named <i>set_name</i> in the calling environment 
        to that difference, and also returns that difference.</p>
    }
} {
    upvar 1 $set_name list
    set result [list]

    if {[package provide Tcl] < 8.4} {
        foreach element $list {
            if {[lsearch -exact $set $element] == -1} {
                lappend result $element
            }
        }
    } else {
        set set [lsort $set]
        foreach element $list {
            if {[lsearch -sorted -exact $set $element] == -1} {
                lappend result $element
            }
        }
    }

    return [set list $result]
}



#
# set_intersection
#

::nstcl::ad_proc ::nstcl::set_intersection {x y} {
    summary "Returns the intersection of two sets"

    description {
        Returns the intersection (the elements in common) of two sets
        <i>x</i> and <i>y</i>.
    }
} {
    set z [list]

    if {[package provide Tcl] < 8.4} {
        foreach element $x {
            if {[lsearch -exact $y $element] != -1} {
                lappend z $element
            }
        }
    } else {
        set y [lsort $y]
        foreach element $x {
            if {[lsearch -sorted -exact $y $element] != -1} {
                lappend z $element
            }
        }
    }

    return $z
}



#
# set_intersection!
#

::nstcl::ad_proc ::nstcl::set_intersection! {set_name set} {
    summary "Compute and update the intersection of two sets"
    
    description {
        <p>Computes the intersection of the set stored in the variable named 
        <i>set_name</i> in the calling environment and the set <i>set</i>, 
        sets the variable named <i>set_name</i> in the calling environment
        to that intersection, and also returns that intersection.</p>
    }
} {
    upvar 1 $set_name list
    set result [list]

    if {[package provide Tcl] < 8.4} {
        foreach element $list {
            if {[lsearch -exact $set $element] != -1} {
                lappend result $element
            }
        }
    } else {
        set set [lsort $set]
        foreach element $list {
            if {[lsearch -sorted -exact $set $element] != -1} {
                lappend result $element
            }
        }
    }

    return [set list $result]
}



#
# set_member?
#        

::nstcl::ad_proc ::nstcl::set_member? {set element} {
    summary "Tests whether or not an element is in a set"

    description {
        <p>Returns 1 if <i>element</i> is a member of <i>set</i>, or 0
        otherwise.</p>
    }
} {
    return [expr {[lsearch -exact $set $element] != -1}]
}



#
# set_minus
#

::nstcl::ad_proc ::nstcl::set_minus {set element} {
    summary "Returns a set, possibly minus one element"
   
    description {
        <p>Returns the set <i>set</i>, without the element <i>element</i>
        (if it was a member of the set).</p>
    }
} {
    set pos [lsearch -exact $set $element]
    if {$pos != -1} {
        set set [lreplace $set $pos $pos]
    }
    return $set
}



#
# set_union
#

::nstcl::ad_proc ::nstcl::set_union {x y} {
    summary "Returns the union of two sets"
    description "<p>Returns the union of two sets <i>x</i> and <i>y</i>.</p>"
} {
    if {[package provide Tcl] < 8.4} {
        foreach element $y {
            if {[lsearch -exact $x $element] == -1} {
                lappend x $element
            }
        }
    } else {
        set x [lsort $x]
        foreach element $y {
            if {[lsearch -sorted -exact $x $element] == -1} {
                lappend x $element
            }
        }
    }

    return $x
}



#
# set_union!
#

::nstcl::ad_proc ::nstcl::set_union! {set_name set} {
    summary "Compute and updat the union of two sets"

    description {
        <p>Computes the union of the set stored in the variable named 
        <i>set_name</i> in the calling environment and the set <i>set</i>, 
        sets the variable named <i>set_name</i> in the calling environment 
        to that union, and also returns that union.</p>
    }
} {
    upvar 1 $set_name list

    if {[package provide Tcl] < 8.4} {
        foreach element $set {
            if {[lsearch -exact $list $element] == -1} {
                lappend list $element
            }
        }
    } else {
        set sorted_list [lsort $list]
        foreach element $set {
            if {[lsearch -sorted -exact $sorted_list $element] == -1} {
                lappend list $element
            }
        }
    }

    return $list
}



#
# swap
#

::nstcl::ad_proc ::nstcl::swap {x y} {
    summary "Swap the value of two variables"

    description {
        <p>This command will swap the value of two variables (or the 
        contents of two arrays).  Naturally it is an error to attempt to 
        swap the value of undefined variables, or to attempt to swap an 
        array and a string.</p>
    }
} {
    upvar 1 $x foo $y bar
 
    if {![info exists foo]} {
        error "$x does not exist"
    }
 
    if {![info exists bar]} {
        error "$y does not exist"
    }
 
    if {[array exists foo] != [array exists bar]} {
        error "$x and $y must both be scalars or both be arrays"
    }
 
    if {![array exists foo]} {
        set tmp $bar
        set bar $foo
        set foo $tmp
    } else {
        array set tmp [array get bar]
        unset bar
        array set bar [array get foo]
        unset foo
        array set foo [array get tmp]
    }
 
    return
}


#
# ad_get_tcl_call_stack
#

::nstcl::ad_proc ::nstcl::ad_get_tcl_call_stack {{level -2}} {
    summary "Returns cummulative information about the call stack"

    description {
        <p>Returns cummulative information about the call stack, useful for 
        debugging.  If <option>-level</option> is not specified, <i>level</i> 
        defaults to 2.  This option specifies how far up the stack to begin 
        reporting from.  A setting of 0, for instance, would list the call 
        to <i>ad_get_tcl_call_stack</i> itself.</p> 
    }
} {
    if {![string is integer -strict $level]} {
        error "expected integer but got \"$level\""
    }

    set tcl_call_stack ""
    set level [expr {[info level] + $level + 1}]

    while {[incr level -1] > 0} {
        append tcl_call_stack "    called from [info level $level]\n"
    }

    return [string trimright $tcl_call_stack]
}


::nstcl::ad_proc ::nstcl::util_commify_number {n} {
    summary "Places commas in a number as appropriate"

    description {
        <p>Commifies a number making it pretty.  For example,
        passing it a value of 1234 will return a result of 1,234.</p>
    }
} {
    while {[regsub -- {^(-?\d+)(\d{3})} $n {\1,\2} n]} {}
    return $n
}



package provide nstcl-misc 1.2
