ad_library {
    Contains procs to send HTML email outside of the context of
    ACS Mail package.

    @author Doug Harris (dharris@worldbank.org)
    @author Janine Sisk (jsisk@mit.edu)
    @creation-date 25 Feb 2002
    @cvs-id $Id: html-email-procs.tcl,v 1.1.2.1 2003/06/23 19:44:31 janines Exp $
}

ad_proc build_mime_message {
    text_body
    html_body
    {charset "iso-8859-1"}
} {
    Composes multipart/alternative email containing plain text
    and html versions of the message, parses out the headers we need,
    constructs an array  and returns it to the caller. 

    This proc is based on ad_html_sendmail, written by Doug Harris at
    the World Bank.

    Comment from the original code:  The message
    is encoded with iso-8859-x charset (all mail readers I've tested seem
    to be unable to handle utf-8 encoding). A future version of this proc
    should probably support an alternative charset argument or switch.
} {

    # this is always called from a scheduled proc
    set r_dir [acs_root_dir]/packages/acs-tcl/tcl
    source $r_dir/base64.tcl
    source $r_dir/md5.tcl
    source $r_dir/mime.tcl

    package require mime

    # since mime tries to treat =xx as a hex ascii code, replace any
    # equals signs with "=3d" (mime encoding of equals sign)
    regsub -all "=" $text_body "=3d" text_body
    regsub -all "=" $html_body "=3d" html_body_for_non_base64

    # convert text to charset
    set encoding [ns_encodingforcharset $charset]
    if {[lsearch [encoding names] $encoding] != -1} {
	set html_body [encoding convertto $encoding $html_body]
	set text_body [encoding convertto $encoding $text_body]
      } else {
	ns_log error "ad_html_sendmail: unknown charset passed in ($charset)"
    }   

    # build body
    set base64_html_part [mime::initialize -canonical text/html \
 	    -param [list charset $charset] \
	    -encoding base64 \
 	    -string $html_body]
    set html_part [mime::initialize -canonical text/html \
 	    -param [list charset $charset] \
	    -encoding quoted-printable \
 	    -string $html_body_for_non_base64]
    set text_part [mime::initialize -canonical text/plain \
	    -param [list charset $charset] \
	    -encoding quoted-printable \
	    -string $text_body]
     #It works better without 'charset'!
     #    set multi_part [mime::initialize -canonical multipart/alternative \
     #	    -param [list charset $charset] \
     #	    -parts [list $text_part $html_part]]
     set multi_part [mime::initialize -canonical multipart/alternative \
 	    -parts [list $text_part $base64_html_part $html_part]]

     # this gives us a complete mime message, minus the headers because 
     # we don't pass any in.  This code is designed to send a fully-formed 
     # message out through an SMTP socket, but we're not doing that so we
     # have to hijack the process a bit.
     set mime_body [mime::buildmessage $multi_part]

     # the first three lines of the message are special; we need to grab 
     # the info, add it to the message headers, and discard the lines
     set lines [split $mime_body \n]
     set message_data [ns_set new]

     # get mime version
     regexp {MIME-Version: (.*)} [lindex $lines 0] junk mime_version
     ns_set put $message_data MIME-Version $mime_version
     # the content id
     regexp {Content-ID: (.*)} [lindex $lines 1] junk content_id
     ns_set put $message_data Content-ID $content_id
     # and the content type and boundary
     regexp {Content-Type: (.*)} [lindex $lines 2] junk content_type                 
     set content_type "$content_type\n[lindex $lines 3]"
     ns_set put $message_data Content-Type $content_type

     # the rest of the lines form the message body.  We strip off the last 
     # line, which is the last boundary, because ns_sendmail seems to be 
     # adding another one on for us.
     ns_set put $message_data body [join [lrange $lines 4 [expr [llength $lines] - 3]] \n]

     return $message_data
}


ad_proc parse_incoming_email {
  message
} {
  Takes an incoming message and splits it into parts.  The main goal
  of this proc is to return something that can be stuffed into the
  database somewhere, such as a forum message.  Since we aggressively
  filter HTML, the HTML tags are stripped out of the returned content.

  The message may have only plain text, plain text and HTML, or plain
  text and something else (Apple Mail uses text/enhanced, for example).
  To make our lives simpler we support only text/html as a special case;
  in all other cases the plain text is returned.
} {
  # look for the files we need.  If they aren't there, we can't do anything
  # and will just return the message as-is (cringe)
  set source_dir [acs_root_dir]/packages/acs-tcl/tcl
  if { ![file exists $source_dir/base64.tcl] || 
       ![file exists $source_dir/md5.tcl] ||
       ![file exists $source_dir/mime.tcl] } {
    return $message
  }

  source $source_dir/base64.tcl
  source $source_dir/md5.tcl
  source $source_dir/mime.tcl
  package require mime

  set mime [mime::initialize -string $message]
  set content [mime::getproperty $mime content]

  if { [string first "multipart" $content] != -1 } {
      set parts [mime::getproperty $mime parts]
  } else {
      set parts [list $mime]
  }

  foreach part $parts {
      switch [mime::getproperty $part content] {
          "text/plain" {
              set plain [mime::getbody $part]
          }
          "text/html" {
              set html [mime::getbody $part]
          }
      }
  }

  if { [info exists html] } {
    set body [ad_html_to_text $html]
  } elseif { [info exists plain] } {
    set body $plain
  } else {
    set body $message
  }

  mime::finalize $mime -subordinates all
  return $body
}