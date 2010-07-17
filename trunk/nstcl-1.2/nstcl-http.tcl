package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-nssets

# nstcl-1.2/nstcl-http.tcl
# $Id: nstcl-http.tcl,v 1.5 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export ns_geturl \
                     ns_httpget \
                     ns_parsehttptime \
                     ns_urlencode \
                     ns_urldecode \
                     util_httpget \
                     util_httppost \
                     util_get_http_status
}

namespace eval ::nstcl::http {
    # Support HTTPS transparently if the tls package is available
    if {![catch { package require tls }]} {
        catch {
            package require http
            ::http::register https 443 ::tls::socket 
        }
    }

    variable uri_p [expr {![catch { package require uri }]}]
}



#
# ns_urlencode
#

::nstcl::ad_proc ::nstcl::ns_urlencode {string} {
    summary     "Performs \"URL encoding\" on a given string"
    description { 
        <p><b>ns_urlencode</b> performs URL encoding (see RFC 1738) on a given
        input string and returns the encoded result.</p>
    }
} {
    set allowed_chars  {[a-zA-Z0-9]}
    set encoded_string ""

    foreach char [split $string ""] {
        if {[string match $allowed_chars $char]} {
            append encoded_string $char
        } else {
            scan $char %c ascii
            append encoded_string %[format %02x $ascii]
        }
    }

    return $encoded_string
}



#
# ns_urldecode
#

::nstcl::ad_proc ::nstcl::ns_urldecode  {string} {
    summary     "Performs \"URL decoding\" on a given string"
    description { 
        <p><b>ns_urldecode</b> performs URL decoding (see RFC 1738) on a given
        input string and returns the decoded result.</p>
    }
} {
    set decoded_string ""
    set len [string length $string]

    for {set i 0} {$i < $len} {incr i} {
        set char [string index $string $i]
        if {$char != "%"} {
            append decoded_string $char
        } else {
            set hex [string range $string [expr {$i + 1}] [expr {$i + 2}]]
            incr i 2
            if {[string length $hex] == 2 && [scan $hex %x ascii]} {
               append decoded_string [format %c $ascii]
            } else {
                return
            }
        }
    }

    return $decoded_string
}



#
# ns_parsehttptime
#

::nstcl::ad_proc ::nstcl::ns_parsehttptime {httptime} {
    summary "Returns number of seconds from HTTP time."

    description {
        <p>Given a properly formatted HTTP timestamp, <b>ns_parsehttptime</b>
        returns the number of seconds since the beginning of the epoch.  
        Returns a 0 when the input is not in canonical HTTP timestamp
        format.</p>
    }
} {
    catch {
        set time [clock scan $httptime]
    }

    # AOLserver behavior is to return 0 on malformed input
    if {![info exists time]} {
        return 0
    }

    set canonical [clock format $time -format "%a, %d %b %Y %H:%M:%S %Z" -gmt 1]
    switch [string equal $canonical $httptime] {
        0 { return 0 }
        1 { return $time }
    }
}



#
# ns_geturl
#

::nstcl::ad_proc ::nstcl::ns_geturl {url {headersSetIdVar ""}} {
    summary "Fetch a URL."
 
    description {
        <p><b>ns_geturl</b> retrieves the contents of the specified <i>url</i>.
        If <i>headersSetIdVar</i> is passed in (and is a valid <b>ns_set</b>)
        then the key/value pairs in the ns_set will be sent as headers
        when fetching the <i>url</i>.</p>
    }
} {
    if {[catch { 
        ::nstcl::http::fetch_url -headers $headersSetIdVar $url 
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}




#
# ns_httpget 
#

::nstcl::ad_proc ::nstcl::ns_httpget {url {timeout 30} {depth 10}} {
    summary "Opens an HTTP connection and gets a page"

    description {
        <p><b>ns_httpget</b> opens an HTTP connection and gets a <i>url</i>,
        waiting up to <i>timeout</i> seconds for a response, following at
        most <i>depth</i> HTTP redirects.</p>
    }
} {
    if {[catch { 
        ::nstcl::http::fetch_url -timeout $timeout -depth $depth $url 
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}



#
# util_httpget
#

::nstcl::ad_proc ::nstcl::util_httpget {url {headersSetIdVar ""} 
                                            {timeout 30} {depth 10}} {
    summary "Opens an HTTP connection and gets a page"

    description {
        <p><b>util_httpget</b> opens an HTTP connection and gets a <i>url</i>,
        waiting up to <i>timeout</i> seconds for a response, following at
        most <i>depth</i> HTTP redirects.  If the value of 
        <i>headersSetIdVar</i> is not the empty string, then it is assumed
        to be a valid <b>ns_set</b>, and the key/value pairs from the set
        are included as headers in the request to the remove server.
        If the value of <i>headersSetIdVar</i> is the empty string, then
        the result is the same as calling <b>ns_httpget</b>.</p>
    }
} {
    if {[catch {
        ::nstcl::http::fetch_url -timeout $timeout -depth $depth \
                                 -headers $headersSetIdVar $url
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}



#
# util_httppost
#

::nstcl::ad_proc ::nstcl::util_httppost {-enctype:optional -file:optional
                                         -data:optional -name:optional
                                         -mime_type:optional -filename:optional
                                         url formvars {timeout 30} {depth 10}
                                                      {http_referer ""}} {
    summary "Posts form variables to a given URL"
 
    description {
        <p><b>util_httpost</b> makes an HTTP connection to <i>url</i> and
        uses the POST method to send <i>formvars</i>, waiting up
        to <i>timeout</i> seconds for a response, and following a maximum
        of <i>depth</i> HTTP redirects.  If a value for <i>http_referer</i>
        is specified it will be passed alond to the remote server.</p>

        <p><i>formvars</i> should be of the form: foo=bar&bar=foo&etc=...</p>
    }
} {
    if {![info exists enctype]} {
        set enctype application/x-www-form-urlencoded
    }

    if {![string equal application/x-www-form-urlencoded $enctype] &&
        ![string equal multipart/form-data $enctype]} {
        error "Don't know how to handle POST of type \"$enctype\""
    }

    if {[string equal multipart/form-data $enctype]} {
        if {[info exists file] && [info exists data]} {
            error "Can't specify both -file and -data"    
        }

        if {[info exists file]} {
            if {![file exists $file]} {
                error "File \"$file\" not found"
            }

            if {![file readable $file]} {
                error "Cannot read \"$file\": permission denied"
            }

            set fp [open $file]
            fconfigure $fp -translation binary
            set data [read $fp]
            close $fp
        } elseif {![info exists data]} {
            set data {}
        }

        if {![info exists file]} {
            set file filename.ext
        }

        if {![info exists filename]} {
            set filename $file
        }

        if {[string length $data] && ![info exists name]} {
            error "Need to specify -name of form var to send file upload as"
        }

        if {![info exists mime_type]} {
            set mime_type application/octet-stream
        }

        set boundary [format "%s%010d%10d%06d%09d" \
                             [string repeat - 22] \
                             [expr {abs([clock clicks])}] \
                             [clock seconds] \
                             [pid] \
                             [expr {int(rand()*987654321)}]]

        set payload ""
        if {[string length $data]} {
            package require base64
            append payload $boundary \
                           \r\n \
                           "Content-Disposition: form-data; " \
                           "name=\"$name\"; " \
                           "filename=\"[file tail $filename]\"" \
                           \r\n \
                           "Content-Type: $mime_type" \
                           \r\n \
                           "Content-transfer-encoding: binary" \
                           \r\n \
                           \r\n \
                           $data \
                           \r\n 
        }

        foreach form_var [split $formvars &] {
            set form_var [split $form_var =]
            set key [::nstcl::ns_urldecode [lindex $form_var 0]]
            set val [::nstcl::ns_urldecode [join [lrange $form_var 1 end] =]]
            append payload $boundary \
                           \r\n \
                           "Content-Disposition: form-data; " \
                           "name=\"$key\"" \
                           \r\n \
                           \r\n \
                           $val \
                           \r\n
        }
        
        append payload $boundary-- \r\n
        set formvars $payload
        append enctype "; boundary=[string range $boundary 2 end]"
    }

    if {[catch {
        ::nstcl::http::fetch_url -mode POST -timeout $timeout \
                                 -depth $depth -referer $http_referer \
                                 -formvars $formvars -enctype $enctype -- $url
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}



#
# util_get_http_status
#

::nstcl::ad_proc ::nstcl::util_get_http_status {url {use_get_p 1} 
                                                    {timeout 30}} {
    summary "Returns the HTTP server response code returned from fetching a url"

    description {
        <p><b>util_get_http_status</b> returns the remote servers response
        code (i.e., 200, 404, 503, etc.) from fetching a given <i>url</i> 
        (waiting up to <i>timeout</i> seconds for a response).</p>

        <p>By default the HTTP GET method is used, because some servers
        do not return the same response code for HEAD requests.  However,
        a HEAD request requires less bandwidth (since the server may only
        return the headers and not the actual page).  <i>use_get_p</i> can
        be specified either as a boolean value or as the word HEAD or GET.</p>
    }
} {
    switch -- [string toupper $use_get_p] {
        HEAD { set use_get_p 0 }
        GET  { set use_get_p 1 }
    }

    if {![string is boolean -strict $use_get_p]} {
        error "expected a boolean but got \"$use_get_p\""
    }

    switch [string is true $use_get_p] {
        0 { set mode HEAD }
        1 { set mode GET }
    }
    
    if {[catch {
        ::nstcl::http::fetch_url -mode $mode -timeout $timeout $url
    } result]} {
        error $result
    } else {
        return [lindex $result 0]
    }
}
        


#
# http::url_fetched
#

::nstcl::ad_proc -private ::nstcl::http::url_fetched {token} {
    return $token
}



#
# http::fetch_url
#

::nstcl::ad_proc -private ::nstcl::http::fetch_url {{-mode GET} 
                                                    {-headers ""} 
                                                    {-formvars ""}
                                                    {-timeout 30}
                                                    {-depth 10}
                                                    -enctype:optional 
                                                    {-referer ""} url} {
    package require http
    variable uri_p
     
    if {$mode != "GET" && $mode != "POST" && $mode != "HEAD"} {
        error "Invalid mode \"$mode\".  Should be one of: GET, HEAD, or POST."
    }
     
    if {![string is integer -strict $depth]} {
        error "expected integer but got \"$depth\""
    }
     
    if {![string is integer -strict $timeout]} {
        error "expected integer but got \"$timeout\""
    }
     
    if {$timeout < 0} {
        set timeout 0
    }

    # handle multipart/form-data, etc.
    if {[info exists enctype] && $mode == "POST" && 
        ![string equal $enctype application/x-www-form-urlencoded]} { 
        set mode FORM
    }

      
    # convert timeout from seconds to milliseconds
    set timeout [expr {$timeout * 1000}]

    set http_headers {}
    if {![string equal $headers ""]} {
        set size [::nstcl::ns_set size $headers]
        for {set i 0} {$i < $size} {incr i} {
            lappend http_headers [::nstcl::ns_set key $headers $i] \
                                 [::nstcl::ns_set value $headers $i]
        }
    }

    if {[string length $referer]} {
        lappend http_headers Referer $referer
    }


    set callback ::nstcl::http::url_fetched
    while {$depth > 0} {
        switch -- $mode {
            GET  { set token [::http::geturl $url -timeout $timeout \
                                                  -headers $http_headers \
                                                  -command $callback]
            }
                                               
            HEAD { set token [::http::geturl $url -timeout $timeout \
                                                  -headers $http_headers \
                                                  -command $callback \
                                                  -validate 1]
            }

            POST { set token [::http::geturl $url -timeout $timeout \
                                                  -headers $http_headers \
                                                  -command $callback \
                                                  -query   $formvars] 
            }

            FORM { set token [::http::geturl $url -timeout $timeout \
                                                  -headers $http_headers \
                                                  -command $callback \
                                                  -query   $formvars \
                                                  -type    $enctype]
            }
        }


        ::http::wait $token
        upvar 0 $token state
        array set meta $state(meta)
        set body [::http::data $token]
        set code [::http::ncode $token]
        ::http::cleanup $token

        if {[string match 3* $code] && 
            [info exists meta(Location)] &&
            [string compare $meta(Location) $url] != 0} {

            switch $uri_p {
                1 { set url $meta(Location) }
                0 { set url [::uri::resolve $url $meta(Location)] }
            }

            incr depth -1
        } else {
            set key [array names meta {[Cc]ontent-[Tt]ype}]
            switch [info exists meta($key)] {
                0 { set mime_type "" }
                1 { set mime_type $meta($key) }
            }
                
            return [list $code $body $mime_type]
        }
    }

    error "Maximum recursion limit exceeded"
}



package provide nstcl-http 1.2
