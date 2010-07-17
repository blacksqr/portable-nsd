package require nstcl-core
package require nstcl-fwdcompat

# nstcl-1.2/nstcl-html.tcl
# $Id: nstcl-html.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export ns_hrefs \
                     ns_quotehtml \
                     ns_striphtml \
                     util_expand_entities
}

namespace eval ::nstcl::html {
    variable entities_map 
    set entities_map(&quot\;)   \"
    set entities_map(&amp\;)    "&"
    set entities_map(&lt\;)     "<"
    set entities_map(&gt\;)     ">"
    set entities_map(&eacute\;) [format %c 0xE9]
    set entities_map(&ob\;)     [format %c 0x7B]
    set entities_map(&laquo\;)  [format %c 0xAB]
    set entities_map(&Icirc\;)  [format %c 0xCE]
    set entities_map(&times\;)  [format %c 0xD7]
    set entities_map(&sup2\;)   [format %c 0xB2]
    set entities_map(&Ugrave\;) [format %c 0xD9]
    set entities_map(&iacute\;) [format %c 0xED]
    set entities_map(&thorn\;)  [format %c 0xFE]
    set entities_map(&shy\;)    [format %c 0xAD]
    set entities_map(&yen\;)    [format %c 0xA5]
    set entities_map(&auml\;)   [format %c 0xE4]
    set entities_map(&curren\;) [format %c 0xA4]
    set entities_map(&Aring\;)  [format %c 0xC5]
    set entities_map(&szlig\;)  [format %c 0xDF]
    set entities_map(&otilde\;) [format %c 0xF5]
    set entities_map(&Oacute\;) [format %c 0xD3]
    set entities_map(&oslash\;) [format %c 0xF8]
    set entities_map(&sup3\;)   [format %c 0xB3]
    set entities_map(&euml\;)   [format %c 0xEB]
    set entities_map(&iquest\;) [format %c 0xBF]
    set entities_map(&ccedil\;) [format %c 0xE7]
    set entities_map(&divide\;) [format %c 0xF7]
    set entities_map(&agrave\;) [format %c 0xE0]
    set entities_map(&ucirc\;)  [format %c 0xFB]
    set entities_map(&uacute\;) [format %c 0xFA]
    set entities_map(&iuml\;)   [format %c 0xEF]
    set entities_map(&not\;)    [format %c 0xAC]
    set entities_map(&cent\;)   [format %c 0xA2]
    set entities_map(&egrave\;) [format %c 0xE8]
    set entities_map(&yacute\;) [format %c 0xFD]
    set entities_map(&ouml\;)   [format %c 0xF6]
    set entities_map(&igrave\;) [format %c 0xEC]
    set entities_map(&reg\;)    [format %c 0xAE]
    set entities_map(&uml\;)    [format %c 0xA8]
    set entities_map(&plusmn\;) [format %c 0xB1]
    set entities_map(&ntilde\;) [format %c 0xF1]
    set entities_map(&ocirc\;)  [format %c 0xF4]
    set entities_map(&eth\;)    [format %c 0xF0]
    set entities_map(&acirc\;)  [format %c 0xE2]
    set entities_map(&copy\;)   [format %c 0xA9]
    set entities_map(&uuml\;)   [format %c 0xFC]
    set entities_map(&atilde\;) [format %c 0xE3]
    set entities_map(&frac34\;) [format %c 0xBE]
    set entities_map(&Aacute\;) [format %c 0xC1]
    set entities_map(&Ograve\;) [format %c 0xD2]
    set entities_map(&Ecirc\;)  [format %c 0xCA]
    set entities_map(&Eacute\;) [format %c 0xC9]
    set entities_map(&yuml\;)   [format %c 0xFF]
    set entities_map(&ugrave\;) [format %c 0xF9]
    set entities_map(&icirc\;)  [format %c 0xEE]
    set entities_map(&micro\;)  [format %c 0xB5]
    set entities_map(&nbsp\;)   [format %c 0xA0]
    set entities_map(&THORN\;)  [format %c 0xDE]
    set entities_map(&Iacute\;) [format %c 0xCD]
    set entities_map(&frac12\;) [format %c 0xBD]
    set entities_map(&Auml\;)   [format %c 0xC4]
    set entities_map(&sect\;)   [format %c 0xA7]
    set entities_map(&Otilde\;) [format %c 0xD5]
    set entities_map(&oacute\;) [format %c 0xF3]
    set entities_map(&cedil\;)  [format %c 0xB8]
    set entities_map(&aring\;)  [format %c 0xE5]
    set entities_map(&Oslash\;) [format %c 0xD8]
    set entities_map(&Euml\;)   [format %c 0xCB]
    set entities_map(&Ccedil\;) [format %c 0xC7]
    set entities_map(&iexcl\;)  [format %c 0xA1]
    set entities_map(&Ucirc\;)  [format %c 0xDB]
    set entities_map(&Agrave\;) [format %c 0xC0]
    set entities_map(&Iuml\;)   [format %c 0xCF]
    set entities_map(&Uacute\;) [format %c 0xDA]
    set entities_map(&brvbar\;) [format %c 0xA6]
    set entities_map(&Egrave\;) [format %c 0xC8]
    set entities_map(&AElig\;)  [format %c 0xC6]
    set entities_map(&aelig\;)  [format %c 0xE6]
    set entities_map(&Yacute\;) [format %c 0xDD]
    set entities_map(&para\;)   [format %c 0xB6]
    set entities_map(&Ouml\;)   [format %c 0xD6]
    set entities_map(&cb\;)     [format %c 0x7D]
    set entities_map(&hibar\;)  [format %c 0xAF]
    set entities_map(&Igrave\;) [format %c 0xCC]
    set entities_map(&raquo\;)  [format %c 0xBB]
    set entities_map(&Ocirc\;)  [format %c 0xD4]
    set entities_map(&Ntilde\;) [format %c 0xD1]
    set entities_map(&deg\;)    [format %c 0xB0]
    set entities_map(&sup1\;)   [format %c 0xB9]
    set entities_map(&frac14\;) [format %c 0xBC]
    set entities_map(&middot\;) [format %c 0xB7]
    set entities_map(&ETH\;)    [format %c 0xD0]
    set entities_map(&Acirc\;)  [format %c 0xC2]
    set entities_map(&acute\;)  [format %c 0xB4]
    set entities_map(&ordm\;)   [format %c 0xBA]
    set entities_map(&Atilde\;) [format %c 0xC3]
    set entities_map(&aacute\;) [format %c 0xE1]
    set entities_map(&Uuml\;)   [format %c 0xDC]
    set entities_map(&ordf\;)   [format %c 0xAA]
    set entities_map(&ograve\;) [format %c 0xF2]
    set entities_map(&ecirc\;)  [format %c 0xEA]
    set entities_map(&pound\;)  [format %c 0xA3]
}


#
# ns_quotehtml
#

::nstcl::ad_proc ::nstcl::ns_quotehtml {html} {
    summary "Quotes special HTML characters"
    
    description {
        <p><i>ns_quotehtml</i> quotes each ampersand, less than, and
        greater than sign in <i>html</i>.  That is, &amp; will become
        &amp;amp;, &lt; will become &amp;lt;, and &gt; will become
        &amp;gt; in the returned string.</p>
    }
} {
    return [string map [list & "&amp;" < "&lt;" > "&gt;"] $html]
}



#
# ns_striphtml
#

::nstcl::ad_proc ::nstcl::ns_striphtml {-tags_only:boolean html} {
    summary "Strip HTML tags"

    description {
        <p><i>ns_striphtml</i> removes all HTML tags from a given chunk of
        <i>html</i>.</p>
    }

    optional_switch {
        <dle>
            <dt><option>-tags_only</option></dt>
            <dd>
                If specified HTML entities are not removed; if not specified,
                they are.
            </dd>
        </dle>
    }
} {
    regsub -all -- {<[^>]+>} $html "" html
    if {!$tags_only_p} {
        regsub -all -- {&[^ \t\n\r;]+;} $html "" html
    }
    return $html
}



#
# util_expand_entities
#

::nstcl::ad_proc ::nstcl::util_expand_entities {html} {
    summary "Expand HTML entities"
  
    description {
        <p><i>util_expand_entities</i> expands any HTML entities in <i>html</i>.
        Quoted HTML, such as &amp;amp; will become &amp; in the returned
        string.</p>
    }
} {
    return [string map [array get ::nstcl::html::entities_map] $html]
}



#
# ns_hrefs_simple
#

::nstcl::ad_proc -private ::nstcl::html::ns_hrefs_simple {html} {
    set hrefs {}
    set RE(anchor) "<\[ \t\n\r\]*a\[ \t\n\r\]+\[^>\]+>"
    set RE(href)   "href=\['\"\]?(\[^ \t\n\r'\"\]+)\['\" \t\r\n\]?\[^>\]*>"

    while {[regexp -indices -nocase -- $RE(anchor) $html range]} {
        foreach {beg end} $range break
        set anchor [string range $html $beg $end]

        if {[regexp -nocase -- $RE(href) $anchor => href]} {
            lappend hrefs $href
        }
     
        set html [string range $html $end end]
    }

    return $hrefs
}



#
# ns_hrefs_advanced
#

::nstcl::ad_proc -private ::nstcl::html::ns_hrefs_advanced {html} {
    package require struct
    package require htmlparse
    package require uri

    if {[info commands ::nstcl::html::parsetree] != ""} {
        ::nstcl::html::parsetree destroy
    }

    # Remove HTML comments
    set html [string map [list <!-- "" --> ""] $html]

    ::struct::tree ::nstcl::html::parsetree
    ::htmlparse::2tree $html ::nstcl::html::parsetree

    set base ""
    set hrefs {}
    set RE {href\s*=\s*(.+)$}

    ::nstcl::html::parsetree walk root -order pre -type dfs -command {
        set node %n
        set type [::nstcl::html::parsetree get $node -key type]

        if {$type == "a" || $type == "base"} {
            set data [::nstcl::html::parsetree get $node -key data]
            if {[regexp -nocase -- $RE $data => url]} {
                set quote [string index $url 0]

                if {$quote == {'} || $quote == {"}} {
                    set pos [string first $quote [string range $url 1 end]]
                    if {$pos != -1} {
                        set url [string trim [string range $url 1 $pos]]
                    }
                } else {
                    regexp {^(\S+)} $url => url
                }

                if {$type == "base"} {
                    set base $url
                } else {
                    if {![string equal $base ""]} {
                        set url [::uri::resolve $base $url]
                    }

                    if {[string match */.* $url]} {
                        set url [::uri::canonicalize $url]
                    } 

                    lappend hrefs $url                       
                }
            } 
        }
    }

    return $hrefs
}



#
# ns_hrefs
#

::nstcl::ad_proc ::nstcl::ns_hrefs {html} {
    summary "Returns a list of URLs from within HTML anchor tags"

    description {
        <p><i>ns_hrefs</i> returns a list of all URLs contained inside
        of &lt;A&gt; tags in the specified <i>html</i>.</p>

        <p>If the struct, htmlparse, and uri packages from tcllib are
        available, and <i>html</i> contains a &lt;base&gt; tag, any relative 
        URLs will be converted from relative to canonical form.</p>
    }
} {
    # first time this procedure is called it redefines itself based upon
    # whether or not Tcl 8.2 + tcllib packages are available
    if {[catch {
        package require struct
        package require htmlparse
        package require uri
    }]} {
        proc ::nstcl::ns_hrefs html [info body ::nstcl::html::ns_hrefs_simple]
    } else {
        proc ::nstcl::ns_hrefs html [info body ::nstcl::html::ns_hrefs_advanced]
    }

    return [::nstcl::ns_hrefs $html]
}
        

package provide nstcl-html 1.2
