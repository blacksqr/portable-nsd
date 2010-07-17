package require nstcl-core

# nstcl-1.2/nstcl-fwdcompat.tcl
# $Id: nstcl-fwdcompat.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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


namespace eval ::nstcl::fwdcompat {
    variable built_in_string   ::string
    variable built_in_lindex   ::lindex
    variable built_in_linsert  ::linsert
    variable built_in_lrange   ::lrange
    variable built_in_lreplace ::lreplace
    variable built_in_array    ::array

    variable string_options_map

    array set string_options_map {
        e          equal        
        eq         equal        
        equa       equal        
        equal      equal
        f          first
        fi         first
        fir        first
        firs       first
        first      first
        la         last
        las        last
        last       last
        is         is
        in         index
        ind        index
        inde       index
        index      index
        map        map
        mat        match
        matc       match
        match      match
        ra         range
        ran        range
        rang       range
        range      range
        repe       repeat        
        repea      repeat        
        repeat     repeat
        repl       replace
        repla      replace
        replac     replace
        replace    replace
        c          compare
        co         compare
        com        compare
        comp       compare
        compa      compare
        compar     compare
        compare    compare
        tot        totitle
        toti       totitle
        totit      totitle
        totitl     totitle
        totitle    totitle
        tou        toupper
        toup       tuopper
        toupp      toupper
        touppe     toupper
        toupper    toupper
        tol        tolower
        tolo       tolower
        tolow      tolower
        tolowe     tolower
        tolower    tolower
        words      wordstart
        wordst     wordstart
        wordsta    wordstart
        wordstar   wordstart
        wordstart  wordstart
        worde      wordend
        worden     wordend
        wordend    wordend
        b          bytelength        
        by         bytelength        
        byt        bytelength
        byte       bytelength        
        bytel      bytelength        
        bytele     bytelength
        bytelen    bytelength
        byteleng   bytelength
        bytelengt  bytelength
        bytelength bytelength
    }
}

namespace eval ::nstcl::fwdcompat::strings {
    variable built_in_string ::string
}


#
# Forward Compatible version of [string]
#

::nstcl::ad_proc ::nstcl::fwdcompat::string {args} {
    variable built_in_string
    variable string_options_map

    if {[llength $args] < 2} {
        return -code error "wrong # args: should be \"[info level 0]\"\
            option arg ?arg ...?"
    }

    set option [lindex $args 0]
    set args [lrange $args 1 end]


    if {[info exists string_options_map($option)]} {
        set option $string_options_map($option)

        if {$option != "is" && $option != "compare" && $option != "match"} { 
            return [uplevel 1 ::nstcl::fwdcompat::strings::$option $args]
        } 

        if {$option == "is"} {
            return [uplevel 1 ::nstcl::fwdcompat::strings::$option \
                -class $args]
        }

        # $option is either "compare" or "match"
        if {[llength $args] == 2} {
            return [uplevel 1 $built_in_string $option $args]
        } else {
            return [uplevel 1 ::nstcl::fwdcompat::strings::$option $args]
        }
    } else {
        return [uplevel 1 $built_in_string $option $args]
    }
}



#
# [string equal]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::equal {-nocase:boolean
                                                             {-length -1} 
                                                             string1 string2} {
    variable built_in_string

    if {$length >= 0} {
        incr length -1
        set string1 [$built_in_string range $string1 0 $length]
        set string2 [$built_in_string range $string2 0 $length]
    }

    if {$nocase_p} {
        set string1 [$built_in_string tolower $string1]
        set string2 [$built_in_string tolower $string2]
    }

    if {[$built_in_string compare $string1 $string2] == 0} {
        return 1
    } else {
        return 0
    }
}



#
# [string is]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::is {-class:required
                                                           -strict:boolean
                                                           -failindex:optional
                                                           string} {

    variable built_in_string

    set classes {alnum alpha ascii control boolean digit double false
                 graph integer lower print punct space true upper
                 wordchar xdigit}


    if {[info exists failindex]} {
        upvar 1 $failindex ndx
    }

    switch -exact -- $class {
        a  -
        al -
        d  -
        p  { 
            return -code error "ambiguous class \"$class\": must be [join \
                [lrange $classes 0 16] ", "], or [lindex $classes end]"
        }

        aln   -
        alnu  -
        alnum {
            set chars { 48  49  50  51  52  53  54  55  56  57  65  66  67  68
                        69  70  71  72  73  74  75  76  77  78  79  80  81  82
                        83  84  85  86  87  88  89  90  97  98  99 100 101 102 
                       103 104 105 106 107 108 109 110 111 112 113 114 115 116 
                       117 118 119 120 121 122 170 181 186 192 193 194 195 196 
                       197 198 199 200 201 202 203 204 205 206 207 208 209 210 
                       211 212 213 214 216 217 218 219 220 221 222 223 224 225 
                       226 227 228 229 230 231 232 233 234 235 236 237 238 239 
                       240 241 242 243 244 245 246 248 249 250 251 252 253 254 
                       255 }
        }

        alp   -
        alph  -
        alpha { 
            set chars {  65  66  67  68  69  70  71  72  73  74  75  76  77
                         78  79  80  81  82  83  84  85  86  87  88  89  90  
                         97  98  99 100 101 102 103 104 105 106 107 108 109
                        110 111 112 113 114 115 116 117 118 119 120 121 122 
                        170 181 186 192 193 194 195 196 197 198 199 200 201 
                        202 203 204 205 206 207 208 209 210 211 212 213 214 
                        216 217 218 219 220 221 222 223 224 225 226 227 228 
                        229 230 231 232 233 234 235 236 237 238 239 240 241 
                        242 243 244 245 246 248 249 250 251 252 253 254 255 }
        }

        as    -
        asc   -
        asci  -
        ascii { 
            set chars {   1   2   3   4   5   6   7   8   9  10  11  12  13
                         14  15  16  17  18  19  20  21  22  23  24  25  26
                         27  28  29  30  31  32  33  34  35  36  37  38  39
                         40  41  42  43  44  45  46  47  48  49  50  51  52
                         53  54  55  56  57  58  59  60  61  62  63  64  65
                         66  67  68  69  70  71  72  73  74  75  76  77  78
                         79  80  81  82  83  84  85  86  87  88  89  90  91
                         92  93  94  95  96  97  98  99 100 101 102 103 104 
                        105 106 107 108 109 110 111 112 113 114 115 116 117
                        118 119 120 121 122 123 124 125 126 127 }
        }

        b       -
        bo      -
        boo     -
        bool    -
        boole   -
        boolea  -
        boolean {
            switch -- [$built_in_string tolower $string] {
                1     -
                t     
                tr    
                tru   -
                true  -
                y     -
                ye    -
                yes   -
                on    -
                0     -
                n     -
                no    -
                of    -
                off   -
                f     -
                fa    -
                fal   -
                fals  -
                false { return 1 }
                default { 
                    set ndx 0
                    return 0
                }
            }
        }

        c       -
        co      -
        con     -
        cont    -
        contr   -
        contro  -
        control { 
            set chars {   0   1   2   3   4   5   6   7   8   9  10  11  12
                         13  14  15  16  17  18  19  20  21  22  23  24  25
                         26  27  28  29  30  31 127 128 129 130 131 132 133 
                        134 135 136 137 138 139 140 141 142 143 144 145 146 
                        147 148 149 150 151 152 153 154 155 156 157 158 159 }
        }

        di    -
        dig   -
        digi  -
        digit { set chars { 48 49 50 51 52 53 54 55 56 57 } }

        do     -
        dou    -
        doub   -
        doubl  -
        double { 
            if {[catch { format %G $string }]} {
                set ndx -1
                return 0
            } else {
                # string is double returns false on integer overflow
                if {[regexp {^\s*[+-]?[1-9][0-9]*\s*$} $string] &&
                    [catch { format %d $string }]} {
                    set ndx -1
                    return 0
                } else {
                    return 1
                }
            }
        }

        f
        fa
        fal
        fals
        false {
            switch -- [$built_in_string tolower $string] {
                0       -
                of      -
                off     -
                n       -
                no      -
                f       -
                fa      -
                fal     -
                fals    -
                false   { return 1 }
                default { 
                    set ndx 0
                    return 0
                }
            }
        }

        g     -
        gr    -
        gra   -
        grap  -
        graph { 
            set chars {  33  34  35  36  37  38  39  40  41  42  43  44  45
                         46  47  48  49  50  51  52  53  54  55  56  57  58  
                         59  60  61  62  63  64  65  66  67  68  69  70  71  
                         72  73  74  75  76  77  78  79  80  81  82  83  84  
                         85  86  87  88  89  90  91  92  93  94  95  96  97  
                         98  99 100 101 102 103 104 105 106 107 108 109 110 
                        111 112 113 114 115 116 117 118 119 120 121 122 123 
                        124 125 126 160 161 162 163 164 165 166 167 168 169 
                        170 171 172 173 174 175 176 177 178 179 180 181 182
                        183 184 185 186 187 188 189 190 191 192 193 194 195 
                        196 197 198 199 200 201 202 203 204 205 206 207 208 
                        209 210 211 212 213 214 215 216 217 218 219 220 221 
                        222 223 224 225 226 227 228 229 230 231 232 233 234 
                        235 236 237 238 239 240 241 242 243 244 245 246 247 
                        248 249 250 251 252 253 254 255 }
        }

        i       -
        in      -
        int     -
        inte    -
        integ   -
        intege  -
        integer { 
            if {[catch { format %d $string }]} {
                set ndx -1
                return 0
            } else {
                return 1
            }
        }

        l     -
        lo    -
        low   -
        lowe  -
        lower { 
            set chars {  97  98  99 100 101 102 103 104 105 106 107 108 109 
                        110 111 112 113 114 115 116 117 118 119 120 121 122 
                        170 181 186 223 224 225 226 227 228 229 230 231 232 
                        233 234 235 236 237 238 239 240 241 242 243 244 245 
                        246 248 249 250 251 252 253 254 255 }
        }

        pr    -
        pri   -
        prin  -
        print { 
            set chars {  32  33  34  35  36  37  38  39  40  41  42  43  44
                         45  46  47  48  49  50  51  52  53  54  55  56  57  
                         58  59  60  61  62  63  64  65  66  67  68  69  70  
                         71  72  73  74  75  76  77  78  79  80  81  82  83  
                         84  85  86  87  88  89  90  91  92  93  94  95  96  
                         97  98  99 100 101 102 103 104 105 106 107 108 109 
                        110 111 112 113 114 115 116 117 118 119 120 121 122 
                        123 124 125 126 160 161 162 163 164 165 166 167 168 
                        169 170 171 172 173 174 175 176 177 178 179 180 181
                        182 183 184 185 186 187 188 189 190 191 192 193 194 
                        195 196 197 198 199 200 201 202 203 204 205 206 207 
                        208 209 210 211 212 213 214 215 216 217 218 219 220 
                        221 222 223 224 225 226 227 228 229 230 231 232 233 
                        234 235 236 237 238 239 240 241 242 243 244 245 246 
                        247 248 249 250 251 252 253 254 255 }
        }

        pu    -
        pun   -
        punc  -
        punct { 
            set chars {  33  34  35  37  38  39  40  41  42  44  45  46  47  
                         58  59  63  64  91  92  93  95 123 125 161 171 173 
                        183 187 191 }
        }

        s     -
        sp    - 
        spa   -
        spac  -
        space { set chars { 9 10 11 12 13 32 160 } }

        t    -
        tr   -
        tru  -
        true {
            switch -- [$built_in_string tolower $string] {
                1       -
                t       -
                tr      - 
                tru     -
                true    -
                y       -
                ye      -
                yes     -
                on      { return 1 } 
                default {
                    set ndx 0
                    return 0
                }
            }
        }

        u     -
        up    -
        upp   -
        uppe  -
        upper { 
            set chars {  65  66  67  68  69  70  71  72  73  74  75  76  77  
                         78  79  80  81  82  83  84  85  86  87  88  89  90 
                        192 193 194 195 196 197 198 199 200 201 202 203 204 
                        205 206 207 208 209 210 211 212 213 214 216 217 218 
                        219 220 221 222 }
        }

        w        -
        wo       -
        wor      -
        word     -
        wordc    -
        wordch   -
        wordcha  -
        wordchar { 
            set chars {  48  49  50  51  52  53  54  55  56  57  65  66  
                         67  68  69  70  71  72  73  74  75  76  77  78  
                         79  80  81  82  83  84  85  86  87  88  89  90  
                         95  97  98  99 100 101 102 103 104 105 106 107 
                        108 109 110 111 112 113 114 115 116 117 118 119 
                        120 121 122 170 181 186 192 193 194 195 196 197 
                        198 199 200 201 202 203 204 205 206 207 208 209 
                        210 211 212 213 214 216 217 218 219 220 221 222 
                        223 224 225 226 227 228 229 230 231 232 233 234 
                        235 236 237 238 239 240 241 242 243 244 245 246 
                        248 249 250 251 252 253 254 255 }
        }

        x      -
        xd     -
        xdi    -
        xdig   -
        xdigi  -
        xdigit { 
            set chars {  48 49 50 51 52 53 54 55  56  57  65
                         66 67 68 69 70 97 98 99 100 101 102 }
        }

        default {
            return -code error "bad class \"$class\": must be [join \
                [lrange $classes 0 16] ", "], or [lindex $classes end]"
        }
    }

    if {[$built_in_string compare $string ""] == 0} {
        switch $strict_p {
            1 { set ndx 0
                return 0 }
            0 { return 1 }
        }
    }


    set valid_p 1
    set len [$built_in_string length $string]

    for {set i 0} {$i < $len} {incr i} {
        if {[scan [$built_in_string range $string $i $i] %c ascii] == -1} {
            set ascii 0
        }
 
        if {[lsearch -exact $chars $ascii] == -1} {
            set ndx $i
            set valid_p 0
            break
        } 
    }
        
    return $valid_p
}


#
# [string repeat]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::repeat {string count} {
    if {[catch { format %d $count }]} {
        return -code error "expected integer but got \"$count\""
    }

    if {$count <= 0} {
        return
    }

    for {set i 0} {$i < $count} {incr i} {
        append result $string
    }

    return $result
}


#
# [string bytelength]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::bytelength {string} {
    variable built_in_string
    return [$built_in_string length $string]
}


#
# [string replace]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::replace {string 
                                                                first last 
                                                                {new ""}} {
    variable built_in_string


    set first [::nstcl::fwdcompat::strings::relative_ndx $string $first]
    set last  [::nstcl::fwdcompat::strings::relative_ndx $string $last]
    set end   [$built_in_string length $string]

    if {$first > $last} {
        return $string
    }

    incr first -1
    incr last   1

    set before [$built_in_string range $string 0 $first]
    set after  [$built_in_string range $string $last $end]

    return "$before$new$after"
}

#
# [string map]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::map {-nocase:boolean
                                                            char_map string} {
    variable built_in_string

    set map_len [llength $char_map] 
    set str_len [$built_in_string length $string]

    if {$map_len == 0 || $str_len == 0} {
        return $string
    } 

    if {$map_len % 2 != 0} {
        return -code error "char map list unbalanced"
    }

    set output ""
    for {set i 0} {$i < $str_len} {incr i} {
        set buffered [$built_in_string range $string $i end]

        for {set ndx 0} {$ndx < $map_len} {incr ndx 2} {
            set key  [lindex $char_map $ndx]
            set size [$built_in_string length $key]
            set offset [expr {$size - 1}]

            set compare_key $key
            set compare_str [$built_in_string range $buffered 0 $offset]

            if {$nocase_p} {
                set compare_key [$built_in_string tolower $compare_key]
                set compare_str [$built_in_string tolower $compare_str]
            }

            if {[string compare $compare_key $compare_str] == 0} {
                append output [lindex $char_map [expr {$ndx + 1}]]
                incr i $offset
                break
            }
        }

        if {$ndx == $map_len} {
            append output [$built_in_string range $string $i $i]
        }
    }

    return $output
}


#
# [string totitle]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::totitle {string 
                                                                {first ""} 
                                                                {last ""}} {
    variable built_in_string

    if {[$built_in_string length $first] == 0 && 
        [$built_in_string length $last]  == 0} {
        set first 0
        set last end
    } else {
        if {[$built_in_string length $last] == 0} {
            set last $first
        }
    }

    set first [::nstcl::fwdcompat::strings::relative_ndx $string $first]
    set last  [::nstcl::fwdcompat::strings::relative_ndx $string $last]
    set len   [$built_in_string length $string]

    set pre   [$built_in_string range $string 0 [expr {$first - 1}]]
    set mid   [$built_in_string range $string $first $last]
    set post  [$built_in_string range $string [expr {$last + 1}] $len]

    set upper [$built_in_string toupper [$built_in_string range $mid 0 0]]
    set lower [$built_in_string tolower [$built_in_string range $mid 1 end]]

    return $pre$upper$lower$post
}


#
# [string toupper]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::toupper {string 
                                                                {first ""} 
                                                                {last ""}} {
    variable built_in_string

    if {[$built_in_string length $first] == 0 &&
        [$built_in_string length $last]  == 0} {
        set first 0
        set last end
    } else {
        if {[$built_in_string length $last] == 0} {
            set last $first
        }
    }

    if {[$built_in_string compare $first "0"] == 0 &&
        [$built_in_string compare $last "end"] == 0} {
        return [$built_in_string toupper $string]
    }

    set first [::nstcl::fwdcompat::strings::relative_ndx $string $first]
    set last  [::nstcl::fwdcompat::strings::relative_ndx $string $last]
    set end   [$built_in_string length $string]

    set result ""
    append result [$built_in_string range $string 0 [expr {$first - 1}]]
    append result [$built_in_string toupper \
        [$built_in_string range $string $first $last]]
    append result [$built_in_string range $string [expr {$last + 1}] $end]

    return $result
}


#
# [string tolower]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::tolower {string 
                                                                {first ""} 
                                                                {last ""}} {
    variable built_in_string

    if {[$built_in_string length $first] == 0 &&
        [$built_in_string length $last]  == 0} {
        set first 0
        set last end
    } else {
        if {[$built_in_string length $last] == 0} {
            set last $first
        }
    }

    if {[$built_in_string compare $first "0"] == 0 &&
        [$built_in_string compare $last "end"] == 0} {
        return [$built_in_string tolower $string]
    }

    set first [::nstcl::fwdcompat::strings::relative_ndx $string $first]
    set last  [::nstcl::fwdcompat::strings::relative_ndx $string $last]
    set end   [$built_in_string length $string]

    set result ""
    append result [$built_in_string range $string 0 [expr {$first - 1}]]
    append result [$built_in_string tolower \
        [$built_in_string range $string $first $last]]
    append result [$built_in_string range $string [expr {$last + 1}] $end]

    return $result
}


#
# [string compare]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::compare {-nocase:boolean
                                                                {-length ""} 
                                                                string1 
                                                                string2} {
    variable built_in_string

    if {$nocase_p} {
        set string1 [$built_in_string tolower $string1]
        set string2 [$built_in_string tolower $string2]

    }

    if {[$built_in_string length $length] != 0 && $length > 0} {
        incr length -1
        set string1 [$built_in_string range $string1 0 $length]
        set string2 [$built_in_string range $string2 0 $length]
    }

    return [$built_in_string compare $string1 $string2]
}


#
# relative ndx helper for [string]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::relative_ndx {string 
                                                                     index} {
    variable built_in_string

    switch -exact -- $index {
        e  -
        en { set index end }
    }


    if {[regexp {^end(-(0|[1-9][0-9]*|0[xX][0-9A-Fa-f]+|0[0-7]+))?$} $index \
        => offset]} {
        if {$offset == ""} {
            set offset 0
        }

        set index [expr {([$built_in_string length $string] - 1) + $offset}]
    }

    if {[catch { format %d $index }]} {
        return -code error "bad index \"$index\": must be integer or\
            end?-integer?"
    }

    return $index
}


#
# [string range]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::range {string 
                                                              first last} {
    variable built_in_string

    set first [::nstcl::fwdcompat::strings::relative_ndx $string $first]
    set last  [::nstcl::fwdcompat::strings::relative_ndx $string $last]

    return [$built_in_string range $string $first $last]
}

#
# [string first]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::first {string1 string2 
                                                              {index 0}} {
    variable built_in_string

    set index   [::nstcl::fwdcompat::strings::relative_ndx $string2 $index]
    set string2 [::nstcl::fwdcompat::strings::range $string2 $index end]
    set first   [$built_in_string first $string1 $string2]

    if {$first >= 0} {
        incr first $index
    }

    return $first
}


#
# [string last]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::last {string1 string2 
                                                             {index end}} {
    variable built_in_string

    set index   [::nstcl::fwdcompat::strings::relative_ndx $string2 $index]
    set string2 [::nstcl::fwdcompat::strings::range $string2 0 $index]
    return [expr {[$built_in_string last $string1 $string2]}]
}


#
# [string wordstart]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::wordstart {string 
                                                                  index} {
    variable built_in_string

    set index [::nstcl::fwdcompat::strings::relative_ndx $string $index]
    return [$built_in_string wordstart $string $index]
}

#
# [string wordend]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::wordend {string index} {
    variable built_in_string

    set index [::nstcl::fwdcompat::strings::relative_ndx $string $index]
    return [$built_in_string wordend $string $index]
}

#
# [string index]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::index {string index} {
    variable built_in_string
    set index [::nstcl::fwdcompat::strings::relative_ndx $string $index]
    return [$built_in_string index $string $index]
}

#
# [string match]
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::strings::match {-nocase:boolean
                                                    pattern string} {
    variable built_in_string
    if {$nocase_p} {
        set pattern [$built_in_string tolower $pattern]
        set string  [$built_in_string tolower $string]
    }
    return [$built_in_string match $pattern $string]
}



#
# Do we need to provide forward compatability for the enhanced [string] 
# functionality that originated in 8.1?
#

namespace eval ::nstcl::fwdcompat::strings {
    if {[info tclversion] < 8.1 || [info patchlevel] == "8.1.0"} {
        set old ::nstcl::fwdcompat::strings::String

        set ::nstcl::fwdcompat::built_in_string          $old
        set ::nstcl::fwdcompat::strings::built_in_string $old

        rename ::string $old
        interp alias {} ::string {} ::nstcl::fwdcompat::string
    }
}



namespace eval ::nstcl::fwdcompat::lists {}


#
# relative ndx helper for [list] functions
#

::nstcl::ad_proc -private ::nstcl::fwdcompat::lists::relative_ndx {list index 
                                                                   {adj -1}} {
    switch -exact -- $index {
        e  -
        en { set index end }
    }
 
 
    if {[regexp {^end(-(0|[1-9][0-9]*|0[xX][0-9A-Fa-f]+|0[0-7]+))?$} $index \
        => offset]} {
        if {$offset == ""} {
            set offset 0
        }
 
        set index [expr {([llength $list] + $adj) + $offset}]
    }

    if {[catch { format %d $index }]} {
        return -code error "bad index \"$index\": must be integer or\
            end?-integer?"
    }

    return $index
}


#
# [lindex]
#

::nstcl::ad_proc ::nstcl::fwdcompat::lindex {args} {
    variable built_in_lindex

    if {[llength $args] != 2} {
        return -code error {wrong # args: should be "lindex list index"}
    } else {
        foreach {list index} $args break
    }

    set index [::nstcl::fwdcompat::lists::relative_ndx $list $index]
    return [$built_in_lindex $list $index]
}


#
# [linsert]
#

::nstcl::ad_proc ::nstcl::fwdcompat::linsert {args} {
    variable built_in_linsert
    variable built_in_lrange

    if {[llength $args] < 3} {
        return -code error {wrong # args: should be "linsert list\
            index element ?element ...?"}
    } else {
        foreach {list index} $args break
        set args [$built_in_lrange $args 2 end]
    }

    set index [::nstcl::fwdcompat::lists::relative_ndx $list $index 0]
    set result [$built_in_lrange $list 0 [expr {$index - 1}]]
    foreach arg $args {
        lappend result $arg
    }
    foreach arg [$built_in_lrange $list $index end] {
        lappend result $arg
    }
    
    return $result
}


#
# [lrange]
#

::nstcl::ad_proc ::nstcl::fwdcompat::lrange {args} {
    variable built_in_lrange

    if {[llength $args] != 3} {
        return -code error {wrong # args: should be "lrange list first last"}
    } else {
        foreach {list first last} $args break
    }

    set first [::nstcl::fwdcompat::lists::relative_ndx $list $first]
    set last  [::nstcl::fwdcompat::lists::relative_ndx $list $last]
    return [$built_in_lrange $list $first $last]
}


#
# [lreplace] 
#

::nstcl::ad_proc ::nstcl::fwdcompat::lreplace {args} {
    variable built_in_lreplace
    variable built_in_lrange

    if {[llength $args] < 3} {
        return -code error {wrong # args: should be "lreplace list\
            first last ?element element ...?"}
    } else {
        foreach {list first last} $args break
        set args [$built_in_lrange $args 3 end]
    }


    set first [::nstcl::fwdcompat::lists::relative_ndx $list $first]
    set last  [::nstcl::fwdcompat::lists::relative_ndx $list $last]

    if {[llength $list] == 0} {
        return $args
    }

    if {[llength $args]} {
        return [eval $built_in_lreplace [list $list] $first $last $args]
    } else {
        return [eval $built_in_lreplace [list $list] $first $last]
    }
}


namespace eval ::nstcl::fwdcompat {
    if {[info tclversion] < 8.1 || [info patchlevel] == "8.1.0"} {
        foreach cmd [list lindex linsert lrange lreplace] {
            set old ::nstcl::fwdcompat::lists::[string totitle $cmd]

            set ::nstcl::fwdcompat::built_in_$cmd $old
            rename ::$cmd $old
            interp alias {} ::$cmd {} ::nstcl::fwdcompat::$cmd
        } 
    }
}


#
# Forward compatible version of [array]
#

::nstcl::ad_proc ::nstcl::fwdcompat::array {args} {
    variable built_in_array

    if {[llength $args] <= 1} {
        return -code error "wrong # args: should be \"array option arrayName\
            ?arg ...?"
    }

    set subcommand [lindex $args 0]
    set args [lrange $args 1 end]

    switch -- $subcommand {
        u -
        un -
        uns -
        unse -
        unset {
            if {[llength $args] == 0} {
                return -code error "wrong # args: should be \"array option\
                    arrayName ?arg ...?"
            }

            if {[llength $args] > 2} {
                return -code error "wrong # args: should be \"array unset\
                    arrayName ?pattern?"
            }

            upvar 1 [lindex $args 0] arrayName
            if {![info exists arrayName]} then return

            if {[llength $args] == 1} {
                unset arrayName
            } else {
                foreach key [$built_in_array names arrayName [lindex $args 1]] {
                    unset arrayName($key)
                }
            }

            return
        }

        a -
        an -
        any -
        anym -
        anymo -
        anymor -
        anymore -
        d -
        do -
        don -
        done -
        dones -
        donese -
        donesea -
        donesear -
        donesearc -
        donesearch -
        e -
        ex -
        exi -
        exis -
        exist -
        exists -
        g -
        ge -
        get -
        na -
        nam -
        name -
        names -
        ne -
        nex -
        next -
        nexte -
        nextel -
        nextele -
        nextelem -
        nexteleme -
        nextelemen -
        nextelement -
        se -
        set -
        si -
        siz -
        size -
        st -
        sta -
        star -
        start -
        starts -
        startse -
        startsea -
        startsear -
        startsearc -
        startsearch {
            set rc [catch {
                uplevel 1 $built_in_array $subcommand $args
            } result]

            if {$rc != 1} {
                return $result
            } else {
                return -code error $result
            }
        }
        
        n -
        s {
            return -code error "ambiguous option \"$subcommand\": must be\
                anymore, donesearch, exists, get, names, nextelement, set,
                size, startsearch, or unset"
        }

        default {
            return -code error "bad option \"$subcommand\": must be anymore,\
                donesearch, exists, get, names, nextelement, set, size,\
                startsearch, or unset"
        }
    }
}

namespace eval ::nstcl::fwdcompat {
    if {[info tclversion] < 8.3} {
        set old ::nstcl::fwdcompat::Array
        set ::nstcl::fwdcompat::built_in_array $old
        rename ::array $old
        interp alias {} ::array {} ::nstcl::fwdcompat::array
    }
}


package provide nstcl-fwdcompat 1.2
