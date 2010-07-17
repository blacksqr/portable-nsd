package require nstcl-core
package require nstcl-fwdcompat

# nstcl-1.2/nstcl-log.tcl
# $Id: nstcl-log.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export ns_log
    variable ns_log_command
    variable ns_log_application

    if {[string length [info script]]} {
        set ns_log_application [file tail [info script]]
    } else {
        set ns_log_application [file tail [info nameofexecutable]]
    }


    #
    # CriTcl version of ns_log
    #

    catch {
        package require critcl 0.32

        critcl::ccode {
            #include <string.h>
            #include <syslog.h>
            #define STR_EQ(a,b)    (strcasecmp((a),(b)) == 0)
        }

        critcl::cproc ns_log_critcl_c {Tcl_Interp* interp 
                                       char*       level 
                                       char*       message
                                       char*       application} ok {

            int priority;

            if (STR_EQ(level, "debug")) {
                priority = LOG_DEBUG;
            } else if (STR_EQ(level, "notice")) {
                priority = LOG_NOTICE;
            } else if (STR_EQ(level, "warning")) {
                priority = LOG_WARNING;
            } else if (STR_EQ(level, "error")) {
                priority = LOG_ERR;
            } else if (STR_EQ(level, "fatal")) {
                priority = LOG_CRIT;
            } else if (STR_EQ(level, "bug")) {
                priority = LOG_INFO;
            } else {
                Tcl_AppendResult(interp, "unknown severity \"", 
                    level, "\": should be one of: ",
                    "fatal, error, warning, bug, notice, or debug.", NULL);
                return TCL_ERROR;
            }
          
            openlog(application, LOG_PID, LOG_USER); 
            syslog(priority, "%s", message);
            closelog();
            return TCL_OK;
        }

        # force compilation
        catch { ns_log_critcl_c }

        ::nstcl::ad_proc -private ::nstcl::ns_log_critcl {level message} {
            variable ns_log_application
            ::nstcl::ns_log_critcl_c $level $message $ns_log_application
        }
    } 



    #
    # Scotty version of ns_log
    #

    catch {
        package require Tnm
        set ::tnm(syslog) $ns_log_application

        ::nstcl::ad_proc -private ::nstcl::ns_log_scotty {level message} {
            variable ns_log_application
            syslog [::nstcl::ns_log_syslog_level $level] \
                "$ns_log_application: $message"
        }
    }



    # /usr/bin/logger version of ns_log

    catch {
        catch { exec logger -zfoobar } error
        if {[string match "couldn't execute *" $error]} {
            error "Couldn't find logger"
        }

        ::nstcl::ad_proc -private ::nstcl::ns_log_exec {level message} {
            variable ns_log_application
            set syslog_level [::nstcl::ns_log_syslog_level $level]
    
            set fp [open "|logger -p user.$syslog_level -t $ns_log_application"]
            puts $fp $message
            close $fp
        }
    }



    #
    # [puts stderr] version of ns_log
    #

    ::nstcl::ad_proc -private ::nstcl::ns_log_puts {level message} {
        set level  [string tolower $level]
        set levels [::nstcl::ns_log_levels]

        if {[lsearch -exact $levels $level] == -1} {
            error "unknown severity \"$level\": should be one of:\
                fatal, error, warning, bug, notice, or debug."
        }

        set now [clock [seconds] -format {[%Y/%m/%d %H:%M:%S]}]
        puts stderr [format "%s %s: %s" $now [string totitle $level] $message]
    }



    #
    # Pick "best" type of ns_log to use ("puts" is guaranteed to be available)
    #

    foreach suffix [list puts exec scotty critcl] {
        if {[string length [info commands ::nstcl::ns_log_$suffix]]} {
            set ns_log_command ::nstcl::ns_log_$suffix
        }
    }

    ::nstcl::ad_proc ns_log {level message} {
        variable ns_log_command
        if {[catch { $ns_log_command $level $message } error]} {
            return -code error $error
        }
    }
}



#
# ns_log_levels
#

::nstcl::ad_proc -private ::nstcl::ns_log_levels {} {
    return [list fatal error warning bug notice debug]
}



#
# ns_log_syslog_level
#

::nstcl::ad_proc -private ::nstcl::ns_log_syslog_level {level} {
    switch -- [string tolower $level] {
        debug   -
        notice  -
        warning -
        error   { return [string tolower $level] }
        fatal   { return critical }
        bug     { return info }
        default {
            error "unknown severity \"$level\": should be one of:\
                fatal, error, warning, bug, notice, or debug."
        }
    }
}



#
# ns_log
#

::nstcl::ad_proc ::nstcl::ns_log {level message} {
    syntax {<command>::nstcl::ns_log</command> <m>level</m> <m>message</m>}

    summary {Logs a message}

    description {
        <p>This command logs a message at a given priority level to
        syslogd(8).  There are four versions of ns_log available, and
        the first available one is used in this order:</p>
        <p>Valid (case-insensitive) values for level are: fatal,
        error, warning, notice, and debug.  The <m>::nstcl::configure_log</m>
        command can be used to specify the threshold for logging errors.
        By default only errors of "notice" and above are logged and
        "debug" messages are silently discarded.</p>
        <dle>
        <dt><i>critcl</i> package</dt>
        <dd>If the <i>critcl</i> package and gcc are available (see 
        http://wiki.tcl.tk/critcl) then an inline C-coded version is used.</dd>
        <dt><i>Scotty</i> package</dt>
        <dd>If the <i>Scotty</i> package is available (see
        http://wiki.tcl.tk/scotty) then it's syslog facility is used.</dd>
        <dt>Exec'ing the <i>logger</i> utility</dt>
        <dd>If the logger(1) utility is found in the path ns_log will 
        do exec the logger utility to have it pass the message to syslogd.</dd>
        <dt>stderr</dt>
        <dd>If none of the proceeding are available/found, then the
        message will be written to <i>stderr</i> <b>instead</b> of syslog.</dd>
        </dle>
    }

    see_also configure_log
} {
    variable ns_log_command
    variable ns_log_level

    set levels [::nstcl::ns_log_levels]
    if {[lsearch -exact $levels $level] == -1} {
        return -code error "unknown severity \"$level\": should be one of:\
            [join [lrange $levels 0 end-1] ", "] or [lindex $levels end]."
    }

    if {[lsearch -exact $levels $level] <= $ns_log_level} {
        if {[catch { 
            $ns_log_command $level "[string toupper $message]: $message" 
        } error]} {
            return -code error $error
        }
    }
}



#
# configure_log_level
#

::nstcl::ad_proc ::nstcl::configure_log_level {level} {
    variable ns_log_level

    set levels [::nstcl::ns_log_levels]

    if {[string equal -nocase $level quiet] || 
        [string equal -nocase $level none]} {
        return [set ns_log_level [llength $levels]]
    }

    if {[lsearch -exact $levels $level] == -1} {
        error "unknown severity \"$level\": should be one of:\
            [join [lrange $levels 0 end-1] ", "] or [lindex $levels end]."
    }
    
    return [set ns_log_level [lsearch -exact $levels $level]]
}

# By default log everything other than debug messages
::nstcl::configure_log_level notice



package provide nstcl-log 1.2
