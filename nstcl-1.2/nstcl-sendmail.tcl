package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-nssets

# nstcl-1.2/nstcl-sendmail.tcl
# $Id: nstcl-sendmail.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export ns_sendmail
    variable mailhost 127.0.0.1
    variable mailport 25
}


#
# nstcl::configure_sendmail
#

::nstcl::ad_proc ::nstcl::configure_sendmail {server {port ""}} {
    variable mailhost
    variable mailport

    set mailhost $server
    if {[string is integer -strict $port]} {
        set mailport $port
    }

    return [list $mailhost $mailport]
}



#
# ns_sendmail
#

::nstcl::ad_proc ::nstcl::ns_sendmail {to from subject body 
                                          {headers ""} {bcc ""}} {
    summary "Sends an email via SMTP"

    description {
        <p><i>ns_sendmail</i> sends an email message via SMTP, 
        <i>to</i> one or more recipients, <i>from</i> someone, with the 
        given <i>subject</i> and <i>body</i>.</p>
        <p>Optionally an <i>ns_set</i> of headers to add to the outgoing
        message can be specified, along with a list of <i>bcc</i> (blind
        carbon copy) recipients.</p>
        <p>Multiple recipients may be specified for both <i>to</i> and 
        <i>bcc</i> by separating them with a comma.  (For example, 
        "johndoe@example.com,sallysmith@example.com".)</p>
        <p>By default <i>ns_sendmail</i> will attempt to connect to
        an SMTP server running on localhost at port 25.  You can use
        <i>::nstcl::configure_sendmail server ?port?</i> to change
        these defaults.</p>
    }
} {
    package require mime
    package require smtp

    variable mailhost
    variable mailport

    set mime [::mime::initialize -canonical text/plain -string $body]
    set command [list ::smtp::sendmessage $mime -servers $mailhost \
                                                -ports $mailport]

    lappend command -header [list From $from] 
    lappend command -header [list To $to]
    lappend command -header [list Subject $subject]

    if {[string length $bcc]} {
        lappend command -header [list Bcc $bcc]
    }

    if {[string length $headers]} {
        set size [::nstcl::ns_set size $headers]
        for {set i 0} {$i < $size} {incr i} {
            lappend command -header [list [::nstcl::ns_set key $headers $i] \
                                          [::nstcl::ns_set value $headers $i]]
        }
    }

    set bombed_p [catch { eval $command } result]
    ::mime::finalize $mime
    
    if {$bombed_p} {
        error $result
    }
}
        


package provide nstcl-sendmail 1.2
