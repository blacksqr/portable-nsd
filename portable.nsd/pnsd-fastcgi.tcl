# pnsd-fastcgi.tcl - OpenACS/FastCGI commands
# 	$Id: pnsd-fastcgi.tcl,v 1.12 2003/12/07 18:56:37 john Exp $	
#
# This script loads the OpenACS commands and packages into a tcl interpreter instance,  and sets up a wait
# loop to respond to FastCGI client requests.  
#  
# It can be used to serve OpenACS pages from any FastCGI enabled server (Apache, IIS, Zeus, Tux, etc)
# 
# TODO: 
# -The memoize command is vital to getting decent performance from OpenACS,  and it has not been fully implemented.  
# -Scheduled procs are no-ops.  I need to figure out how to launch an interpreter who's sole job is to run scheduled procs.
#   This will likely be platform dependent (a service(?) on windows, a cron job elsewhere, an Apache startup directive, etc.) 
# - Only currently working on Windows as FastCGIExternalServer.  This will require either manual set-up of multi-process launcher and round-robing functionality,  or use of a threaded interpreter instance (which we'd like to avoid)
#
# NOTE:  It includes an inline, slightly modified version of Tom Poindexter's Fcgi.tcl that 
#  fixes a strange bug regarding package loading and redefining the 'gets' and 'reads' command.
#
# 
# JS - 11/2002
#
# Copyright 2003   -   John Sequeira  -   http://www.jsequeira.com
# johnseq@pobox.com



###############################################################################
#
# fcgi.tcl
#
# Copyright 1998, Tom Poindexter,  all rights reserved
# tpoindex@nyx.net
# http://www.nyx.net/~tpoindex
#  see the file LICENSE.TERMS for complete copyright and licensing info
#
#  FastCGI interface for Tcl 8.0
#    Extended Tcl (aka Tclx) is required for 'AppClass' style connections,
#    (the 'package require Tclx' is near the bottom of this file, and only
#    used when needed.)
#    Tcl 8.0+ is required, as fcgi.tcl uses namespace and binary commands.
#    

namespace eval fcgi {
 
variable fcgi
global env
 
###############################################################################
# define fcgi constants (from fastcgi.h)
 
# Values for protocol info
set fcgi(FCGI_LISTENSOCK_FILENO) 0
set fcgi(FCGI_MAX_LENGTH)        [expr 0xffff]
set fcgi(FCGI_HEADER_LEN)        8
set fcgi(FCGI_VERSION_1)         1
 
# Values for type component of FCGI_Header
set fcgi(FCGI_BEGIN_REQUEST)       1
set fcgi(FCGI_ABORT_REQUEST)       2
set fcgi(FCGI_END_REQUEST)         3
set fcgi(FCGI_PARAMS)              4
set fcgi(FCGI_STDIN)               5
set fcgi(FCGI_STDOUT)              6
set fcgi(FCGI_STDERR)              7
set fcgi(FCGI_DATA)                8
set fcgi(FCGI_GET_VALUES)          9
set fcgi(FCGI_GET_VALUES_RESULT)  10
set fcgi(FCGI_UNKNOWN_TYPE)       11
 
# Value for requestId component of FCGI_Header
set fcgi(FCGI_NULL_REQUEST_ID)     0
 
# Mask for flags component of FCGI_BeginRequestBody
set fcgi(FCGI_KEEP_CONN)  1
 
# Values for role component of FCGI_BeginRequestBody
set fcgi(FCGI_RESPONDER)  1
set fcgi(FCGI_AUTHORIZER) 2
set fcgi(FCGI_FILTER)     3
 
# Values for protocolStatus component of FCGI_EndRequestBody
set fcgi(FCGI_REQUEST_COMPLETE) 0
set fcgi(FCGI_CANT_MPX_CONN)    1
set fcgi(FCGI_OVERLOADED)       2
set fcgi(FCGI_UNKNOWN_ROLE)     3
 
# Variable names for FCGI_GET_VALUES / FCGI_GET_VALUES_RESULT records
set fcgi(FCGI_MAX_CONNS)  "FCGI_MAX_CONNS"
set fcgi(FCGI_MAX_REQS)   "FCGI_MAX_REQS"
set fcgi(FCGI_MPXS_CONNS) "FCGI_MPXS_CONNS"
 
###############################################################################
# define fcgi state variables
 
set fcgi(requestId)     -1		;# current requestId in progress
set fcgi(origEnv)    [array names env]	;# list of orignal env names
set fcgi(listenSock)    -1		;# socket on which we listen
set fcgi(acceptCmd)     -1		;# command to accept new sock
set fcgi(newSock)       -1		;# new client socket
global fcgiNewSock 
set fcgiNewSock         -1		;# var to wait on Tcl socket
set fcgi(newClient)     ""		;# ip of client that connected
set fcgi(bufSize)       4096		;# stdout/stderr buffer size
set fcgi(notFcgi)	0		;# if app is running as normal CGI
 
 
###############################################################################
# define fcgi mgmt variables responses

set fcgi(fcgi_max_conns)     1		;# only one connection at a time
set fcgi(fcgi_max_reqs)      1		;# only one request at a time
set fcgi(fcgi_mpxs_conns)    0		;# don't multiplex connections
 
###############################################################################
# per request variables

# these are shown here as comments, actually set in FCGI_Accept
#set fcgi($requestId,sock)      -1	;# socket for connection
#set fcgi($requestId,env)       ""	;# environment
#set fcgi($requestId,paramsEof) 0	;# environment eof marker
#set fcgi($requestId,stdin)     ""	;# stdin buffer
#set fcgi($requestId,stdinEof)  0	;# stdin eof marker
#set fcgi($requestId,data)      ""	;# fcgi data buffer
#set fcgi($requestId,dataEof)   0	;# fcgi data eof marker
#set fcgi($requestId,dataRedir) 0	;# fcgi data redirected to stdin
#set fcgi($requestId,stdout)    ""	;# stdout buffer
#set fcgi($requestId,stdoutFlg) 0       ;# stdout written flag
#set fcgi($requestId,stderr)    ""	;# stderr buffer
#set fcgi($requestId,stderrFlg) 0       ;# stderr written flag
#set fcgi($requestId,keepConn)  0	;# keep connection 
#set fcgi($requestId,exitCode)  0	;# exit code
#set fcgi($requestId,role)      0	;# fcgi role

package require Tclx
# rename Tcl io commands so we can redefine them as fcgi aware
rename gets  _gets_tcl
rename read  _read_tcl
rename flush _flush_tcl
rename puts  _puts_tcl
rename eof   _eof_tcl


}   ;# end of namespace eval fcgi



###############################################################################
#
# replacement stdio procs to act on fcgi stdio/data stream as well as files
#
###############################################################################

###############################################################################
# fcgi "gets" wrapper proc

proc fcgi::gets {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_gets_tcl $args]
  }
  if {[lindex $args 0] == "stdin"} {
    if {$fcgi($requestId,dataRedir) && ! $fcgi($requestId,dataEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "data"]
    } elseif {! $fcgi($requestId,stdinEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "stdin"]
    } else {
      set rc 1	;# force "no error"
    }
    if {$rc <= 0} {
      if {[llength $args] > 1} {
	return 0
      } else {
        return ""
      }
    }
    set idx [string first \n $fcgi($requestId,stdin)]
    if {$idx == -1} {
      set idx [string length $fcgi($requestId,stdin)]
    }
    incr idx -1
    set msg [string range $fcgi($requestId,stdin) 0 $idx]
    incr idx 2
    set fcgi($requestId,stdin) [string range $fcgi($requestId,stdin) $idx end]
    if {[llength $args] > 1} {
      uplevel 1 set [list [lindex $args 1]] [list $msg]
      return [string length $msg]
    } else {
      return $msg
    }
  } else {
    return [uplevel 1 fcgi::_gets_tcl $args]
  }
}


###############################################################################
# fcgi "read" wrapper proc

proc fcgi::read {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_read_tcl $args]
  }

  # fill stdin or data buffer if stdin channel
  if {([lindex $args 0] == "stdin") || \
      ([lindex $args 0] == "-nonewline" &&[lindex $args 1] == "stdin")} {

    if {$fcgi($requestId,dataRedir) && ! $fcgi($requestId,dataEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "data"]
    } elseif {! $fcgi($requestId,stdinEof)} {
      set rc [processFcgiStream $fcgi($requestId,sock) $requestId "stdin"]
    } else {
      set rc 1	;# force "no error"
    }
    if {$rc <= 0} {
      return ""
    }
  }

  if {[lindex $args 0] == "-nonewline"} {
    if {[lindex $args 1] == "stdin"} {
      # read from stdin buf until eof, chop last nl
      set msg [string trim $fcgi($requestId,stdin) \nl]
      returm $msg
    } else {
      return [uplevel 1 fcgi::_read_tcl $args]
    }
  } else  {
    if {[lindex $args 0] == "stdin"} {
      # read from stdin buf specific num of bytes
      if {[llength $args] > 1} {
        set num 0
	scan [lindex $args 1] %d num
        set msg [string range $fcgi($requestId,stdin) 0 [expr $num - 1]]
        set fcgi($requestId,stdin) \
			       [string range $fcgi($requestId,stdin) $num end]
      } else {
	set msg $fcgi($requestId,stdin)
	set fcgi($requestId,stdin) ""
      }
      return $msg
    } else {
      return [uplevel 1 fcgi::_read_tcl $args]
    }
  }
}


###############################################################################
# fcgi "flush" wrapper proc

proc fcgi::flush {file} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_flush_tcl $file]
  }
  if {$file == "stdout" || $file == "stderr"} {
    set num [string length $fcgi($requestId,$file)]
    while {$num > 0} {
      set num [expr $num<$fcgi(FCGI_MAX_LENGTH) ? $num : $fcgi(FCGI_MAX_LENGTH)]
      set msg [string range $fcgi($requestId,$file) 0 [expr $num - 1]]
      set fcgi($requestId,$file) \
			      [string range $fcgi($requestId,$file) $num end]
      if {$file == "stdout"} {
	set type $fcgi(FCGI_STDOUT)
      } else {
	set type $fcgi(FCGI_STDERR)
      }
      writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) $type \
		      $requestId $msg
      set num [string length $fcgi($requestId,$file)]
    }
  } else {
    uplevel 1 fcgi::_flush_tcl $file
  }
  return ""
}



###############################################################################
# fcgi "puts" wrapper proc

proc fcgi::puts {args} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_puts_tcl $args]
  }
  switch [llength $args] {
    1 {
      append fcgi($requestId,stdout) [lindex $args 0] \n
      set file stdout
    }
    2 {
      if {[lindex $args 0] == "-nonewline"} {
        append fcgi($requestId,stdout) [lindex $args 1]
        set file stdout
      } else {
        set file [lindex $args 0]
        if {$file == "stdout" || $file == "stderr"} {
          append fcgi($requestId,$file) [lindex $args 1] \n
        } else {
          uplevel 1 fcgi::_puts_tcl $args
        }
      }
    }
    default {
      set file [lindex $args 1]
      if {[lindex $args 0] == "-nonewline" && \
	 ($file == "stdout" || $file == "stderr")} {
        append fcgi($requestId,$file) [lindex $args 2]
      } else {
        uplevel 1 fcgi::_puts_tcl $args
      }
    }
  }

  # set "written to" flag and check if flush needed
  if {[string compare $file "stdout"] == 0 || \
      [string compare $file "stderr"] == 0} {
    set fcgi($requestId,${file}Flg) 1
    if {[string length $fcgi($requestId,$file)] > $fcgi(bufSize)} {
      flush $file
    }
  }
  return ""

}


###############################################################################
# fcgi "eof" wrapper proc

proc fcgi::eof {file} {
  variable fcgi
  set requestId $fcgi(requestId)
  if {$requestId == -1} {
    return [uplevel 1 fcgi::_eof_tcl $file]
  }
  if {$file == "stdin"} {
    if {[string length $fcgi($requestId,$file)] == 0 && \
	$fcgi($requestId,stdinEof)} {
      return 1
    } else {
      return 0
    }
  } else {
    return [uplevel 1 fcgi::_eof_tcl $file]
  }
}


###############################################################################
#
# fcgi support routines
#
###############################################################################

 
###############################################################################
# read fcgi record
 
proc fcgi::readFcgiRecord {sock} {
  variable fcgi
  set msg ""
 
  while {[string length $msg] != $fcgi(FCGI_HEADER_LEN)} {
    append msg \
	[_read_tcl $sock [expr $fcgi(FCGI_HEADER_LEN) - [string length $msg]]]
  }
 
  set version         0
  set type            0
  set requestId       0
  set contentLength   0
  set paddingLength   0
  set reserved        0
 
  # read the header
  binary scan $msg ccSScc version type requestId contentLength \
                          paddingLength reserved
 
  # convert everything to unsigned int values
  set version       [expr ($version       + 0x100)   % 0x100]
  set type          [expr ($type          + 0x100)   % 0x100]
  set requestId     [expr ($requestId     + 0x10000) % 0x10000]
  set contentLength [expr ($contentLength + 0x10000) % 0x10000]
  set paddingLength [expr ($paddingLength + 0x100)   % 0x100]
 
  # read msg content
  set content ""
  while {[string length $content] != $contentLength} {
    append content \
	[_read_tcl $sock [expr $contentLength - [string length $content]]]
  }
 
  # read msg padding
  set padding ""
  while {[string length $padding] != $paddingLength} {
    append padding \
	[_read_tcl $sock [expr $paddingLength - [string length $padding]]]
  }
 
  return [list $version $type $requestId $contentLength $content]
}

 
###############################################################################
# write fcgi record
 
proc fcgi::writeFcgiRecord {sock version type requestId content} {

  set contentLength [string length $content]
  # ccSScc = version type requestId contentLength padding reserved

  catch {
    _puts_tcl -nonewline $sock \
	   [binary format ccSScc $version $type $requestId $contentLength 0 0]
    _puts_tcl -nonewline $sock $content
    _flush_tcl $sock
  }

}
 
 
###############################################################################
# scan fcgi request body
#   input: message string of type FCGI_BEGIN_REQUEST
 
proc fcgi::scanFcgiRequestBody {msg} {
  set role   0
  set flags  0
  # last 5 bytes are reserved
  binary scan $msg Scc5 role flags reserved
  set role  [expr ($role  + 0x10000) % 0x10000]
  set flags [expr ($flags + 0x100)   % 0x100]
  return [list $role $flags]
}
 

###############################################################################
# format fcgi end request response
 
proc fcgi::formatFcgiEndRequest {appStatus protocolStatus} {
  return [binary format Icc3 $appStatus $protocolStatus {0 0 0}]
}
 
 
###############################################################################
# format fcgi unknown type response
 
proc fcgi::formatFcgiUnknownType {type} {
  return [binary format cc7 $type {0 0 0 0 0 0 0}]
}
 

###############################################################################
# scan fcgi name value pair
 
proc fcgi::scanFcgiNameValue {msg} {
 
  # get name len
  set nlen 0
  binary scan $msg c nlen
  set nlen [expr ($nlen + 0x100) % 0x100]
  if {$nlen > 127} {
    binary scan $msg I nlen
    set nlen [expr $nlen & 0x7fffff]
    set nlenLen 4
  } else {
    set nlenLen 1
  }
 
  # get value len
  set vlen 0
  binary scan $msg "x${nlenLen}c" vlen
  set vlen [expr ($vlen + 0x100) % 0x100]
  if {$vlen > 127} {
    binary scan $msg "x${nlenLen}I" vlen
    set vlen [expr $vlen & 0x7fffff]
    set vlenLen 4
  } else {
    set vlenLen 1
  }
 
  # get name and value
  set fmt [format x%dx%da%da%d $nlenLen $vlenLen $nlen $vlen]
  set name  ""
  set value ""
  binary scan $msg $fmt name value
  set totLen [expr $nlenLen + $vlenLen + $nlen + $vlen]
  return [list $totLen $name $value]
}


###############################################################################
# format fcgi name value pair
 
proc fcgi::formatFcgiNameValue {name value} {
  set nlen [string length $name]
  set vlen [string length $value]
  if {$nlen > 127} {
    set nlenFmt I
    set nlen [expr $nlen | 0x80000000]
  } else {
    set nlenFmt c
  }
  if {$vlen > 127} {
    set vlenFmt I
    set vlen [expr $vlen | 0x80000000]
  } else {
    set vlenFmt c
  }
  set fmt [format %s%sa%da%d $nlenFmt $vlenFmt $nlen $vlen]
  return [binary format $fmt $nlen $vlen $name $value]
}


###############################################################################
# respond to mgmt record requests
 
proc fcgi::respondFcgiMgmtRecord {s msg} {
  variable fcgi
  set requestId $fcgi(requestId)

  set reply ""

  while {[string length $msg] > 0} {
    set nameValue [scanFcgiNameValue $msg]
    set totLen [lindex $nameValue 0]
    set name   [lindex $nameValue 1]
    set value  [lindex $nameValue 2]
    set msg [string range $msg $totLen end]

    # "open" style of switch command
    switch -- $name \
      $fcgi(FCGI_MAX_CONNS)  {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_max_conns)]    
      } \
      $fcgi(FCGI_MAX_REQS) {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_max_reqs)]    
      } \
      $fcgi(FCGI_MPXS_CONNS) {
	 append reply [formatFcgiNameValue $name $fcgi(fcgi_mpxs_conns)]    
      } \
      default {
      }
    
  }

  if {[string length $reply] > 0} {
     writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
	     $fcgi(FCGI_GET_VALUES_RESULT) 0 $reply 
  }
}


###############################################################################
# process fcgi header / new request
# returns list: requestId role flags - new request 
#        {-1 0 0} - server tried to multiplex request
#        { 0 0 0} - socket closed 

proc fcgi::getFcgiBeginRequest {sock} {
  variable fcgi

  set type -1
  while {$type != $fcgi(FCGI_BEGIN_REQUEST)} {
    if {[catch {set msg [readFcgiRecord $sock]}]} {
      # read error
      return {0 0 0}
    }
    set version       [lindex $msg 0]
    set type          [lindex $msg 1]
    set requestId     [lindex $msg 2]
    set contentLength [lindex $msg 3]
    set content       [lindex $msg 4]
    if {$type == $fcgi(FCGI_BEGIN_REQUEST)} {
      set msg [scanFcgiRequestBody $content]
      set role  [lindex $msg 0]
      set flags [lindex $msg 1]
      return [list $requestId $role $flags]
    } elseif {$requestId == 0 || $type == $fcgi(FCGI_GET_VALUES)} {
	respondFcgiMgmtRecord $sock $content
    } else {
      writeFcgiRecord $sock $version $fcgi(FCGI_UNKNOWN_TYPE) 0 \
						 [formatFcgiUnknownType $type]
      return {-1 0 0}
    }
  }
}


###############################################################################
# process fcgi connections
# returns 1 - "waitfor" stream completed
#        -1 - server tried to multiplex request or abort request
#         0 - socket closed 

proc fcgi::processFcgiStream {sock requestId waitfor} {
  variable fcgi

  switch -- $waitfor {
    params {set waitfor fcgi($requestId,paramsEof)}
    stdin  {set waitfor fcgi($requestId,stdinEof)}
    data   {set waitfor fcgi($requestId,dataEof)}
    default {return -1}
  }

  while {! [set $waitfor]} {

    if {[catch {set msg [readFcgiRecord $sock]}]} {
      # read error
      return 0
    }
    set version       [lindex $msg 0]
    set type          [lindex $msg 1]
    set requestId     [lindex $msg 2]
    set contentLength [lindex $msg 3]
    set content       [lindex $msg 4]

    if {$requestId == 0} {
      respondFcgiMgmtRecord $sock $content
      continue
    }

    if {$requestId != $fcgi(requestId)} {
      writeFcgiRecord $sock $version $fcgi(FCGI_END_REQUEST) $requestId \
		 [formatFcgiEndRequest 0 $fcgi(FCGI_CANT_MPX_CONN)]
      return -1
    }

    # "open" style of switch command
    switch -- $type \
      $fcgi(FCGI_PARAMS) {
	if {$contentLength == 0} {
          set fcgi($requestId,paramsEof) 1
	} else {
	  while {[string length $content] > 0} {
            set msg [scanFcgiNameValue $content]
            lappend fcgi($requestId,env)  [lindex $msg 1] [lindex $msg 2]
	    set content [string range $content [lindex $msg 0] end]
	  }
	}
      } \
      $fcgi(FCGI_STDIN) {
	if {$contentLength == 0} {
	  set fcgi($requestId,stdinEof) 1
	} else {
	  if {!$fcgi($requestId,dataRedir)} {
	    append fcgi($requestId,stdin) $content
	  } 
	}
      } \
      $fcgi(FCGI_DATA)   {
	if {$contentLength == 0} {
	  set fcgi($requestId,dataEof) 1
	  if {$fcgi($requestId,dataRedir)} {
	    set fcgi($requestId,stdin) $fcgi($requestId,data)
	    set fcgi($requestId,stdinEof) 1
	  }
	} else {
	  append fcgi($requestId,data) $content
	}
      } \
      $fcgi(FCGI_GET_VALUES) {
	respondFcgiMgmtRecord $sock $content
      } \
      $fcgi(FCGI_ABORT_REQUEST) {
        writeFcgiRecord $sock $fcgi(FCGI_VERSION_1) \
             $fcgi(FCGI_END_REQUEST) $requestId \
             [formatFcgiEndRequest 0 $fcgi(FCGI_REQUEST_COMPLETE)]
	
	return -1
      } \
      $fcgi(FCGI_END_REQUEST) - \
      $fcgi(FCGI_UNKNOWN_TYPE) - \
      $fcgi(FCGI_STDOUT) - \
      $fcgi(FCGI_STDERR) {
	# ignore these packets
      } \
      default {
	# send back unknown type
        writeFcgiRecord $sock $version $fcgi(FCGI_UNKNOWN_TYPE) $requestId \
						 [formatFcgiUnknownType $type]
      } 
    # end of switch 
  }

  return 1
}


###############################################################################
# set up env for new connection

proc fcgi::setupFcgiEnv {requestId} {
  variable fcgi
  global env
  
  # unset all but orignal env names
  foreach {name} [array names env] {
    if {[lsearch $fcgi(origEnv) $name] == -1} {
      unset env($name)
    }
  }

  # add in env for this fcgi connection
  foreach {name value} $fcgi($requestId,env) {
    set env($name) $value
  }
}


###############################################################################
# clean up per request fcgi variables

proc fcgi::cleanUpFcgi {requestId} {
  variable fcgi

  catch {unset fcgi($requestId,sock)     }
  catch {unset fcgi($requestId,env)      }
  catch {unset fcgi($requestId,paramsEof)}
  catch {unset fcgi($requestId,stdin)    }
  catch {unset fcgi($requestId,stdinEof) }
  catch {unset fcgi($requestId,data)     }
  catch {unset fcgi($requestId,dataEof)  }
  catch {unset fcgi($requestId,dataRedir)}
  catch {unset fcgi($requestId,stdout)   }
  catch {unset fcgi($requestId,stdoutFlg)}
  catch {unset fcgi($requestId,stderr)   }
  catch {unset fcgi($requestId,stderrFlg)}
  catch {unset fcgi($requestId,keepConn) }
  catch {unset fcgi($requestId,exitCode) }
  catch {unset fcgi($requestId,role)     }

}


###############################################################################
# reset of cgi.tcl environment

proc fcgi::resetCgiEnv {} {

  # if also using cgi.tcl, get the _cgi global array, and save beginning values.
  # cgi.tcl uses the _cgi array to save state information, which needs to
  # be reset on each FCGI_Accept call

  global _cgi 
  variable fcgi_cgi

  if {[array exists _cgi]} {
    # try to use cgi.tcl reset environment, otherwise do it ourselves
    if {[catch {cgi_reset_env}]} {
      # set _cgi back to beginning values
      if {![array exists fcgi_cgi]} {
	array set fcgi_cgi [array get _cgi]
      }
      catch {unset _cgi}
      array set _cgi [array get fcgi_cgi]
      # unset other _cgi_xxxx vars 
      # untouched are: _cgi_link _cgi_imglink _cgi_link_url
      set cgi_vars {_cgi_uservar _cgi_cookie _cgi_cookie_shadowed _cgi_userfile}
      foreach v $cgi_vars {
        global $v
        catch {unset $v}
      }
    }
  }

}


###############################################################################
#
# application interfaces
#
###############################################################################


###############################################################################
# accept a new fcgi connection, this is the primary call from the application

proc fcgi::FCGI_Accept {} {
  variable fcgi

  global env
  set requestId $fcgi(requestId)

  # if we started with stdin as a real stdin, then fail second time around 
  if {$fcgi(notFcgi)} {
    return -1
  }
  
  # flush and pending request
  if {$fcgi(requestId) != -1} {
    FCGI_Finish
  }

  # execute the accept command, either 'fcgiTclxAccept' or 'fcgiSockAccept'
  set sock [$fcgi(acceptCmd) $fcgi(listenSock)]

  # if we get a null back from accept, means we're running as plain CGI
  if {[string length $sock] == 0} {
    # set to fail on second time
    set fcgi(notFcgi) 1

    # set role as responder
    set env(FCGI_ROLE) RESPONDER

    return 0
  }

  # get the begin request message
  set newFcgi [getFcgiBeginRequest $sock]

  if {[string compare $newFcgi 0] == 0} {
    return -1
  } elseif {[string compare $newFcgi -1] == 0} {
    return  -1
  }
  set requestId [lindex $newFcgi 0]
  set role      [lindex $newFcgi 1]
  set flags     [lindex $newFcgi 2]

  if {$requestId == -1} {
    return -1
  }

  set fcgi(requestId)            $requestId
  set fcgi($requestId,sock)      $sock	;# socket for connection
  set fcgi($requestId,env)       ""	;# environment
  set fcgi($requestId,paramsEof) 0	;# environment eof marker
  set fcgi($requestId,stdin)     ""	;# stdin buffer
  set fcgi($requestId,stdinEof)  0	;# stdin eof marker
  set fcgi($requestId,data)      ""	;# fcgi data buffer
  set fcgi($requestId,dataEof)   0	;# fcgi data eof marker
  set fcgi($requestId,dataRedir) 0	;# fcgi data redirected to stdin
  set fcgi($requestId,stdout)    ""	;# stdout buffer
  set fcgi($requestId,stdoutFlg) 0	;# stdout written flag
  set fcgi($requestId,stderr)    ""	;# stderr buffer
  set fcgi($requestId,stderrFlg) 0	;# stderr written flag
  set fcgi($requestId,keepConn)  $flags	;# keep connection 
  set fcgi($requestId,exitCode)  0	;# exit code
  set fcgi($requestId,role)      $role	;# fcgi role

  # get fcgi params streams until no more params
  set rc [processFcgiStream $sock $requestId "params"]

  if {$rc <= 0} {
    cleanUpFcgi $requestId
    return -1
  }

  setupFcgiEnv $requestId

  # "open" style of switch command
  switch -- $fcgi($requestId,role) \
    $fcgi(FCGI_RESPONDER)  {
      set env(FCGI_ROLE) RESPONDER
    } \
    $fcgi(FCGI_AUTHORIZER) {
      set env(FCGI_ROLE) AUTHORIZER
    } \
    $fcgi(FCGI_FILTER)     {
      set env(FCGI_ROLE) FILTER
    } \
    default {
      set env(FCGI_ROLE) ""
    }
  # end of switch


  # cause cgi.tcl to be sourced, if not already sourced, and reset cgi.tcl
  catch {cgi_lt}
  resetCgiEnv

  return 0
}


###############################################################################
# finish fcgi connection

proc fcgi::FCGI_Finish {} {
  variable fcgi
  set requestId $fcgi(requestId)

  # write stdout and stderr bufs
  foreach {file type} {stdout FCGI_STDOUT stderr FCGI_STDERR} {
    if {$fcgi($requestId,${file}Flg)} {
      flush $file
      # send zero length as eof
      writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
	      $fcgi($type) $requestId ""
    }
  }

  # write end request
  writeFcgiRecord $fcgi($requestId,sock) $fcgi(FCGI_VERSION_1) \
    $fcgi(FCGI_END_REQUEST) $requestId \
    [formatFcgiEndRequest $fcgi($requestId,exitCode) \
		          $fcgi(FCGI_REQUEST_COMPLETE)]

  # check to teardown socket
  if {! ($fcgi($requestId,keepConn) & $fcgi(FCGI_KEEP_CONN) )} {
    close $fcgi($requestId,sock)
  }

  # clean up
  cleanUpFcgi $requestId

  set fcgi(requestId)  -1
  
}


###############################################################################
# set exit status for fcgi 

proc fcgi::FCGI_SetExitStatus {status} {
  variable fcgi

  set requestId $fcgi(requestId)
  set fcgi($requestId,exitCode) $status
  return ""
}


###############################################################################
# start filter data

proc fcgi::FCGI_StartFilterData {} {
  variable fcgi

  set requestId $fcgi(requestId)
  set fcgi($requestId,stdin)    $fcgi($requestId,data)
  set fcgi($requestId,stdinEof) $fcgi($requestId,dataEof)
  set fcgi($requestId,dataRedir) 1
  return ""
}


###############################################################################
# set buffer size, valid sizes: 0 to FCGI_MAX_LENGTH

proc fcgi::FCGI_SetBufSize {size} {
  variable fcgi

  set newSize -1
  catch {scan $size %d newSize}
  if {$newSize >= 0 && $newSize <= $fcgi(FCGI_MAX_LENGTH)} {
    set fcgi(bufSize) $newSize
  }
  return $fcgi(bufSize)
}




###############################################################################
#
# start up fcgi processing
#
###############################################################################

namespace eval fcgi {

variable fcgi
global env


###############################################################################
# procs to handle native Tcl socket accepts & Tclx accepts
#

# callback proc from Tcl's 'socket -server' 
proc fcgiAccept {sock client port} {
  global fcgiNewSock
  variable fcgi
  set fcgi(newClient) $client
  set fcgiNewSock $sock
  update
}

# blocking 'accept' for Tcl sockets
proc fcgiSockAccept {sock} {
  global fcgiNewSock
  variable fcgi
  vwait fcgiNewSock
  set fcgi(newSock) $fcgiNewSock
  fconfigure $fcgi(newSock) -translation binary
  return $fcgiNewSock
}

# blocking 'accept' for TclX sockets 
proc fcgiTclxAccept_ {sock} {
  variable fcgi
  set fcgi(newSock)   ""
  set fcgi(newClient) ""
  # watch for failure, if so then we probably started with stdin = real stdin
  if {[catch {set fcgi(newSock) [server_accept $sock]}] == 0} {
    # we got a good accept, change channel to binary
    fconfigure $fcgi(newSock) -translation binary
    catch {set fcgi(newClient) [lindex [fconfigure -socket $fcgi(newSock)] 0]}
  }
  return $fcgi(newSock)
}

proc fcgiTclxAccept {sock} {
    variable fcgi
    set fcgi(newSock)   ""
    set fcgi(newClient) ""
    # watch for failure, if so then we probably started with stdin = real stdin
    if {[catch {
	fconfigure $sock -translation binary }] == 0} {

	set fcgi(newSock) $sock
	# if we got a good fconfigure, change channel to binary	
	#fconfigure $fcgi(newSock) -translation binary
	catch {set fcgi(newClient) [lindex [fconfigure fcgi(newSock) -socketname] 0]}
    }

    return $fcgi(newSock)

}


###############################################################################
#    look for port on which to listen, either as argument(-port) or env(PORT)
#    if neither, then use file descriptor 0 as server port

set port -1

# check for argv "-port xxx" first
global argv argc
for {set i 0} {$i < $argc} {incr i} {
  if {[string compare [lindex $argv $i] "-port"] == 0} {
    incr i
    scan [lindex $argv $i] %d port
  }
}

# next, check env(PORT)
if {$port < 0} {
  if {[info exists env(PORT)]} {
    scan $env(PORT) %d port
  }
}


# if port was found, then open a server socket on which to listen
# if no port was found, assume we started with as a forked process with 
# stdin = unix domain socket from apache's mod_fastcgi.

if {$port < 0} {
  # we use the fine Tclx extension for this style of connection
  package require Tclx
  set fcgi(listenSock) stdin
  set fcgi(acceptCmd)  fcgiTclxAccept

#   if {[catch {set fcgi(newSock) [server_accept $sock]}] == 0} {

} else {
  set fcgi(listenSock) [socket -server fcgiAccept $port]
  set fcgi(acceptCmd)  fcgiSockAccept
}



# export applications and io wrapper commands
namespace export FCGI_Accept FCGI_Finish FCGI_SetExitStatus \
		 FCGI_StartFilterData FCGI_SetBufSize
namespace export gets read flush puts eof

#  rename gets  _gets_tcl
#   rename read  _read_tcl
#   rename flush _flush_tcl
#   rename puts  _puts_tcl
#   rename eof   _eof_tcl

 
}   ;# end of namespace eval fcgi


# make the application use fcgi wrappers for these io commands
# hmm.  If I'm running under CGI,  I don't want to do anything.
#namespace import -force fcgi::gets
#namespace import -force fcgi::read
proc curry {new args} {eval [list interp alias {} $new {}] $args}

curry gets fcgi::gets
curry read fcgi::read 

namespace import -force fcgi::flush
namespace import -force fcgi::puts
namespace import -force fcgi::eof

# import the application fcgi commands
namespace import fcgi::FCGI_Accept 
namespace import fcgi::FCGI_Finish 
namespace import fcgi::FCGI_SetExitStatus 
namespace import fcgi::FCGI_StartFilterData
namespace import fcgi::FCGI_SetBufSize
#package require Fcgi

###### END FAST CGI CODE ##################################


###### BEGIN FCGI->OPEN ACS OVERLOADED COMMANDS #######################

package require cgi


proc cgi_import_list {} {
    global _cgi
    ns_log debug "cgi_import_list: [array names _cgi]"
    
    if [info exists _cgi(uservars)] {
	return $_cgi(uservars)
    }

    return [list]
}

cgi_debug -on

source pnsd-init.tcl
::pnsd::source_openacs


::pnsd::source_openacs
set ::nstcl::debug_p 1


rename ns_conn ns_conn_default

#FastCGI version -> use CGI globals
proc ns_conn { cmd args } { 
    global env
    #ns_log debug "fcgi::ns_conn called with $cmd"
    switch $cmd {
	"host" { 

	    if { [info exists env(HTTP_HOST)] && $env(HTTP_HOST) != "" } { 
		ns_log debug "\[ns_conn]Host $env(HTTP_HOST)"
		return $env(HTTP_HOST) 
	    } else {
		ns_log debug "\[ns_conn]Host ->[info hostname]"
		return [info hostname]
	    }
	}
	"port" { 
	    if [info exists env(SERVER_PORT)] { 
		return $env(SERVER_PORT) 
	    } 
	    #otherwise we're running command line - use default
	}
	"peeraddr" { return $env(REMOTE_ADDR) }
	"form" { 
	    ns_log debug "ns_connn form says hey [cgi_import_list] there "
	    global _cgi _cgi_uservar
	    set form [ns_set create formdata]	  	    
	    
	    set lst [list new] ;  #err ... why did I do this?
	    lappend lst [cgi_import_list]
	    if [info exists _cgi(uservars) ] {
		ns_log debug "Notice _cgi(uservars) "
		foreach varname $_cgi(uservars) {		    
		    ns_log debug "putting ... $varname into $form "
		    #Is this a list or not?
		    if {-1 == [lsearch $_cgi(uservars,autolist) $varname]} {
			ns_set put $form $varname $_cgi_uservar($varname)
		    } else {
			foreach element [split $_cgi_uservar($varname)] {
			    ns_set put $form $varname $element
			}
		    }

		}
	    }
	    return $form
	}
	"query" { 
	    return ""
	    if { [ info exists env(QUERY_STRING) ] } { 
		return $env(QUERY_STRING) 
	    } else { 
		return "" 
	    }	
	}				
	"protocol" { return $env(SERVER_PROTOCOL) }
	"url" { 
#	    ns_log Debug "url returning $env(SCRIPT_URL) "
	    return $env(SCRIPT_URL)
	}
	"version" { return "1.0" }
	"authuser" { return $env(REMOTE_USER) }
	"contentlength" { return 0 }
	"isconnected" { return 1 }
	
    }
    
    return [ ns_conn_default $cmd $args ]

}

rename ns_config ns_config_default

proc ns_config { section item args} {
    
    switch $item {
	"Hostname" { return [ns_conn host ] }
	"HomeURL" { return "http://[ns_config "" Hostname]/pvt" }
    }

    return [ns_config_default $section $item $args]



}


namespace eval pnsd {

#stolen from tclhttpd
    variable Httpd_Errors
    array set Httpd_Errors {
	200 {Data follows}
	204 {No Content}
	302 {Found}
	304 {Not Modified}
	400 {Bad Request}
	401 {Authorization Required}
	403 {Permission denied}
	404 {Not Found}
	408 {Request Timeout}
	411 {Length Required}
	419 {Expectation Failed}
	500 {Server Internal Error}
	501 {Server Busy}
	503 {Service Unavailable}
	504 {Service Temporarily Unavailable}
    }

    proc ::pnsd::http_head { status type } {
	::pnsd::dump_headers
	cgi_content_type $type 
    }
    

}

#TODO: move to buffered output using fcopy directly  (won't matter so much if caching proxy implemented first)
#doesn't currently work with binary content.

proc ns_getform { } {

    ns_log debug "Entering new ns_getform [cgi_import_list]"

#    ns_log debug [parray env]
    set form [ns_conn form]
    ns_log debug "printing form ...[ns_set print $form] "

    return $form

    global _cgi_uservar

    set form [ ns_set create cgivars ]

#     foreach { pair } [ split $::pnsd::querystring & ] {
# 	set keyval  [ split $pair = ]
# 	set key [lindex $keyval 0]
# 	set val [ncgi::decode [lindex $keyval 1]]
# 	ns_set put $form $key $val	
#     }

    foreach key [ array names _cgi_uservar ] {
	ns_set put $form $key $_cgi_uservar($key)
    }

    ns_set put $form sdsdsds dsdsdsdsds

    ns_log notice "ns_getform returning\n [ns_set print $form]"    

    return $form

}

proc ns_return { status type string } {
    ns_log notice "jjsX: ns_return: status $status type=$type length=[string length $string] "
    
#    set cmd [list ::pnsd::http_head $status $type ]
#[list cgi_status $status; ::pnsd::dump_headers; cgi_content_type $type ]
    
    ns_log debug "ns_return: Command $cmd"
    cgi_http_head [list ::pnsd::http_head $status $type ]
    
    set ::pnsd::__http_mime $type
    set ::pnsd::__http_stream $string

    return 
}


proc ns_returnnotfound { } {

    global _cgi
#    unset _cgi(http_head_in_progress)
#    ns_log debug "ns_rnf Status : [parray _cgi ]"

    cgi_http_head { cgi_status 404 "Not Found"}

    set ::pnsd::http_done_p "t"

}

#jjs: This will do for low-performing serving of  HTML files for now.  
# I should really do an fcopy to the fcgi socket,  but probably better still would be 
# to use a caching reverse proxy and maintain accurate cache/modified headers

#jjs: This will do for low-performing serving of  HTML files for now.  
#I should really do an fcopy to the fcgi socket

proc ns_returnfile { status type file } {
    
#use ::fileutil::cat $source ?
    set chan [open $file r]
    set text [read -nonewline $chan]
    close $chan

    ns_log debug "ns_returnfile $status $type $file "
    ns_log debug "ns_returnfile: [string range $text 0 100] "

    ns_return $status $type $text

}

ad_proc ns_startcontent { {-type "text/html"} }  {
    
    ns_log "ns_startcontent called "   
    cgi_http_head [list ::pnsd::http_head 200 $type ]

    set ::pnsd::__http_mime $type

}

proc ns_returnredirect { location } {
    ns_log debug "redirecting to $location"
#1)
global _cgi
set _cgi(http_head_in_progress) t
#    cgi_redirect $location
#2)
    ::pnsd::dump_headers
    cgi_status 302 Redirected
    cgi_puts "Uri: $location"
    cgi_puts "Location: $location"    
#3)

    #make sure I get cookies
#     variable headers [ad_conn outputheaders]
#     ns_set put $headers "Status" "302 Redirected"
#     ns_set put $headers "Uri" $location
#     ns_set put $headers "Location" $location
    cgi_puts ""

    set ::pnsd::http_done_p t
    unset _cgi(http_head_in_progress) 
    set _cgi(http_head_done) 1

}


 



# I should allow this take a code arg to uplevel
proc ::pnsd::dump_headers {} {
    variable headers
    set headers [ad_conn outputheaders]
    for {set i 0} {$i < [ns_set size $headers]} {incr i} {
	ns_log debug  "header-dump [ns_set key $headers $i]: [ns_set value $headers $i]"	    
	cgi_puts "[ns_set key $headers $i]: [ns_set value $headers $i]"	    
    }

}

proc ad_return_template {args} { 
    ns_log debug "ad_return_template called with $args (no-op)"
}



#BROKEN PROCS
#Since I'm only going to deal with HTML content right now,  this is not
#important
proc util_WriteWithExtraOutputHeaders { args } { 
    # NOT YET WORKING
    ns_log warn "no-op: util_WriteWithExtraOutputHeaders called"
}
set ::pnsd::invariants(util_WriteWithExtraOutputHeaders) 1

#proc ad_table {args}  {
#    ns_log debug "ad_table called with $args (no-op)"
#}



proc ns_queryget { key } {     
    return [ ns_set value [ ns_getform ] $key ]
} 


#curry ns_share global


################### FASTCGI LISTENER ########################
#source c:/Tcl/lib/Fcgi04/fcgi.tcl
#package require Fcgi

set count 0 
ns_log Notice "OpenACS/fastCGI waiting for requests..."

ns_set truncate $::pnsd::headers_id 0
while {[FCGI_Accept] >= 0 } {
#    ns_log Notice "FCGI_Accept begun."
    ns_set update $::pnsd::headers_id Host [ns_conn host]
    ns_set update $::pnsd::headers_id Referer ""
    if [info exists env(HTTP_COOKIE)] {
	ns_set update $::pnsd::headers_id COOKIE $env(HTTP_COOKIE)
    }
#    ns_log debug "incomdingg......."
    #ns_log Debug "Headers comming in : [ns_set print [ad_conn headers]]"

    #grab code I'm testing,  so I don't have to restart server.
    if [file exists new.tcl] {
	source new.tcl
    }
    cgi_eval {
    #set up cgi var's
	cgi_input    
	incr count

	rp_filter "because i said so"

#	ns_log debug "VARS: [info vars]"
#	ns_log debug "VARS:2 [info locals]"

	
	#If I issued a redirect or 404 skip rp_handler.	
	if {$::pnsd::http_done_p != "t" } {
	    rp_handler

	    if  { $::pnsd::__http_stream != "" }  {
		ns_log debug "Now calling dump_headers"		
		cgi_http_head {::pnsd::dump_headers}
		cgi_puts ""
		cgi_puts $::pnsd::__http_stream
#		parray env
#		::pnsd::dump_headers

	    }     

	}



    }

    #clear out any ACS legacy globals...
    pnsd::reset_connection


}

#### NB: Don't define anything below this line... you won't get there.