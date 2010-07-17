package require nstcl-core
package require nstcl-fwdcompat

# nstcl-1.2/nstcl-time.tcl
# $Id: nstcl-time.tcl,v 1.4 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export duration \
                     elapsed_time \
                     end_of_day \
                     first_day_of_month \
                     first_day_of_week \
                     last_day_of_month \
                     last_day_of_week \
                     ns_fmttime \
                     ns_gmtime \
                     ns_httptime \
                     ns_localtime \
                     ns_time \
                     num_days_difference \
                     start_of_day \
                     sysdate \
                     ns_schedule_proc \
                     ns_unschedule_proc \
                     ns_schedule_daily \
                     ns_schedule_weekly

    variable scheduled_procs
}



#
# duration
#

::nstcl::ad_proc ::nstcl::duration {{-nocomplain:boolean}
                                    {-default 0}
                                    {-absolute_value:boolean}
                                    period} {

    summary "Calculates the number of seconds of a given time period"

    description {
        <p>This function returns the number of seconds of a given 
        <i>period</i> of time.  For example, 
        <i>duration "1 day"</i> would return 86400.  <i>period</i> can
        be in any format understood by <i>[clock scan]</i>.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-absolute_value</option></dt>
            <dd>
                If this switch is present, and the duration is a negative
                number (for example, <i>duration "1 hour ago"</i>), the
                absolute value will be returned instead.
            </dd>
 
            <dt><option>-default</option> (defaults to <i>0</i>)</dt>
            <dd>
                This switch specifies what value should be returned
                if <i>period</i> cannot be parsed, and the 
                <i>-nocomplain</i> option was used.
            </dd>

            <dt><option>-nocomplain</option></dt>
            <dd>
                If this switch is specified, and <i>period</i> cannot
                be parsed by <i>[clock scan]</i>, then rather than
                generating an error, the value given to the <i>-default</i>
                switch will be used.
            </dd>
        </dle>
    }
} {
    set now  [clock seconds]
    catch { set then [clock scan $period -base $now] }
 
    if {![info exists then]} {
        if {$nocomplain_p} {
            return $default
        } else {
            error "unable to convert date-time string \"$period\""
        }
    }
 
    set duration [expr {$then - $now}]
    if {$duration < 0 && $absolute_value_p} {
        set duration [expr {abs($duration)}]
    }
    return $duration
}


#
# elapsed_time
#

::nstcl::ad_proc ::nstcl::elapsed_time {-absolute_value:boolean
                                        -floating_point:boolean
                                        {-units seconds}
                                        date1
                                        {date2 now}} {
    summary "Returns the amount of elapsed time between two dates"
    
    description { 
        <p>Calculates the difference between two dates (which can be
        in any format understood by <i>[clock scan]</i>) and returns
        the elapsed time in <i>units</i>.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-absolute_value</option></dt> 
            <dd>
                If this switch is present, and the calculated 
                <i>elapsed time</i> is a negative number, then the 
                absolute value of <i>elapsed time</i> is returned instead.
            </dd>

            <dt><option>-floating_point</option></dt>
            <dd>
                If this switch is not present then the result of the 
                calculation is truncated to the nearest whole integer
                value.
            </dd>

            <dt><option>-units</option> (defaults to <i>seconds</i>)</dt>
            <dd>
                Specifies the units the <i>elapsed time</i> calculation
                is to be done in.  <i>units</i> may be any meaningful value 
                understood by <i>[clock scan]</i> (such as <i>seconds</i>, 
                <i>minutes</i>, <i>hours</i>, <i>days</i>, <i>weeks</i>, 
                <i>fortnights</i>, etc.)
            </dd>
        </dle>
    }
} {
    if {![string is integer -strict $date1]} {
        if [catch { set date1 [clock scan $date1] }] {
            error "Unable to convert date1: \"$date1\""
        }
    }
 
    if {![string is integer -strict $date2]} {
        if [catch { set date2 [clock scan $date2] }] {
            error "Unable to convert date2: \"$date2\""
        }
    }
 
    set diff [expr {$date1 - $date2}]
    if {$diff < 0 && $absolute_value_p} {
        set diff [expr {$diff * -1}]
    }
 
    if {$floating_point_p} {
        set diff [expr {$diff * 1.0}]
    }
 
    if [catch { set duration [::nstcl::duration $units] }] {
        error "Unable to calculate size of units: \"$units\""
    }
 
    return [expr {$diff / $duration}]
}



#
# end_of_day
#

::nstcl::ad_proc ::nstcl::end_of_day {date} {
    summary "Returns the timestamp of the end of a day"

    description {
        <p>Returns last second of <i>date</i> in the format of 
        "YYYY-MM-DD 23:59:59".  <i>date</i> can be in any form
        understood by <i>[clock scan]</i>.</p>
    }
} {
    return [clock format [clock scan $date] -format "%Y-%m-%d 23:59:59"]
}



#
# first_day_of_month
#

::nstcl::ad_proc ::nstcl::first_day_of_month {{-format "%Y-%m-%d"} date} {
    summary "Returns the first day of a month"

    description {
        <p>Returns the (trivial) first date of the month containing the
        given <i>date</i>.  The format of <i>date</i> can be anything
        that <i>[clock scan]</i> can parse.</p>

        <p>This function is mostly for symetry with <i>last_day_of_month</i>,
        <i>first_day_of_week</i>, and <i>last_day_of_week</i>.</p>
    }

    optional_switch {
        <dle>
            <dt><option>-format</option> (defaults to <i>%Y-%m-%d</i>)</dt>
            <dd>
                Specifies the format that the first date of the month should
                be returned in.  Consult the documentation for the
                <i>[clock format]</i> command for a list of valid field
                descriptors.
            </dd>
        </dle>
    }
} {
    return [clock format [clock scan  [clock format [clock scan $date] \
        -format "%Y-%m-01 %H:%M:%S"]] -format $format]
}



#
# first_day_of_week
#

::nstcl::ad_proc ::nstcl::first_day_of_week {{-format "%Y-%m-%d"} date} {
    summary "Returns the first day of a week"

    description {
        <p>Returns the first day of the week containing the given
        <i>date</i>.  The format of <i>date</i> can be anything
        that <i>[clock scan]</i> can parse.</p>
    }

    optional_switch {
        <dle>
            <dt><option>-format</option> (defaults to <i>%Y-%m-%d</i>)</dt>
            <dd>
                Specifies the format that the first day of the week should
                be returned in.  Consult the documentation for the
                <i>[clock format]</i> command for a list of valid field
                descriptors.
            </dd>
        </dle>
    }
} {
    set time [clock scan $date]
    set dow  [clock format $time -format %w]
    return [clock format [clock scan "-$dow days" -base $time] -format $format]
}



#
# last_day_of_month
#

::nstcl::ad_proc ::nstcl::last_day_of_month {{-format "%Y-%m-%d"} date} {
    summary "Returns the last day of a month"

    description {
        <p>Returns the last date of the month containing the
        given <i>date</i>.  The format of <i>date</i> can be anything
        that <i>[clock scan]</i> can parse.</p>
    }

    optional_switch {
        <dle>
            <dt><option>-format</option> (defaults to <i>%Y-%m-%d</i>)</dt>
            <dd>
                Specifies the format that the last date of the month should
                be returned in.  Consult the documentation for the
                <i>[clock format]</i> command for a list of valid field
                descriptors.
            </dd>
        </dle>
    }
} {
    foreach {month year} [clock format [clock scan $date] -format "%b %Y"] break

    switch $month {
        Jan -
        Mar -
        May -
        Jul -
        Aug -
        Oct -
        Dec { set last_day 31 }
        Apr -
        Jun -
        Sep -
        Nov { set last_day 30 }
        Feb { set last_day 28 }
    }

    if {$last_day == 28 && ($year % 4) == 0} {
        if {($year % 100) != 0 || ($year % 400) == 0} {
            set last_day 29
        }
    }

    return [clock format [clock scan "$month $last_day, $year"] -format $format]
}



#
# last_day_of_week
#

::nstcl::ad_proc ::nstcl::last_day_of_week {{-format "%Y-%m-%d"} date} {
    summary "Returns the last day of a week"

    description {
        <p>Returns the last day of the week containing the given
        <i>date</i>.  The format of <i>date</i> can be anything
        that <i>[clock scan]</i> can parse.</p>
    }

    optional_switch {
        <dle>
            <dt><option>-format</option> (defaults to <i>%Y-%m-%d</i>)</dt>
            <dd>
                Specifies the format that the last day of the week should
                be returned in.  Consult the documentation for the
                <i>[clock format]</i> command for a list of valid field
                descriptors.
            </dd>
        </dle>
    }
} {
    set time   [clock scan $date]
    set dow    [clock format $time -format %w]
    set adjust [expr {6 - $dow}]
    set time   [clock scan "$adjust days" -base $time]
    return     [clock format $time -format $format]
}



#
# ns_fmttime
#

::nstcl::ad_proc ::nstcl::ns_fmttime {time {format "%a %b %d %T %Y"}} {
    summary "Returns a formatted date and time"

    description {
        <p>This function returns the <i>time</i> (represented as the number
        of seconds since the beginning of the epoch) formatted according
        to the value of <i>format</i>.  The default format string is 
        "<i>%a %b %d %T %Y</i>".  Consult the documentation for the
        <i>[clock]</i> command for a list of format string values.</p>
    }
} {
    if {[catch  { clock format $time -format $format } result]} {
        error $result
    } else {
        return $result
    }
}



#
# ns_gmtime
#

::nstcl::ad_proc ::nstcl::ns_gmtime {} {
    summary "Returns the current GMT time as a list of values"
 
    description {
        <p>Returns a list made up of the number of <i>minutes</i>,
        <i>hours</i>, the <i>day of the month</i>, the <i>month</i>,
        <i>year</i>, <i>day of the week</i>, <i>day of the year</i>,
        and a boolean <i>day light savings time</i> value.</p>
    }

    notes {
        <p><i>day of the month</i> is a numeric value (01-31).</p>
        <p><i>month</i> is a numeric value (0-11).</p>
        <p><i>year</i> is the current year, less 1900.</p>
        <p><i>day of the week</i> is a numeric value (0 = Sunday).</p>
        <p><i>day of the year</i> is a numeric value (001 - 366).</p>
    }
} {
    set pieces {minute hours dayofmonth month year dayofweek dayofyear tz}
    set format "%M     %H    %d         %m    %Y   %w        %j        %Z"

    set time [expr {[clock seconds] - [clock scan "1970-01-01 00:00:00"]}]

    foreach $pieces [clock format $time -format $format] break
    foreach piece $pieces {
        set $piece [string trimleft [set $piece] 0]
        if {[set $piece] == ""} {
            set $piece 0
        }
    }

    incr dayofyear -1
    incr month     -1
    incr year      -1900
    set dst [string match *DT $tz]

    return [list \
        $minute $hours $dayofmonth $month $year $dayofweek $dayofyear $dst]
}



#
# ns_httptime
#

::nstcl::ad_proc ::nstcl::ns_httptime {time} {
    summary "Format the given time as an HTTP timestamp"

    description {
        <p>Formats the given <i>time</i> (number of seconds since the epoch)
        in the format specified by the HTTP protocol.</p>
    }
} {
    return [clock format $time -format "%a, %d %b %Y %H:%M:%S %Z" -gmt 1]
}



#
# ns_localtime
#

::nstcl::ad_proc ::nstcl::ns_locatime {} {
    summary "Returns the current local time as a list of values"
 
    description {
        <p>Returns a list made up of the number of <i>minutes</i>,
        <i>hours</i>, the <i>day of the month</i>, the <i>month</i>,
        <i>year</i>, <i>day of the week</i>, <i>day of the year</i>,
        and a boolean <i>day light savings time</i> value.</p>
    }

    notes {
        <p><i>day of the month</i> is a numeric value (01-31).</p>
        <p><i>month</i> is a numeric value (0-11).</p>
        <p><i>year</i> is the current year, less 1900.</p>
        <p><i>day of the week</i> is a numeric value (0 = Sunday).</p>
        <p><i>day of the year</i> is a numeric value (001 - 366).</p>
    }
} {
    set pieces {minute hours dayofmonth month year dayofweek dayofyear tz}
    set format "%M     %H    %d         %m    %Y   %w        %j        %Z"

    foreach $pieces [clock format [clock seconds] -format $format] break
    foreach piece $pieces {
        set $piece [string trimleft [set $piece] 0]
        if {[set $piece] == ""} {
            set $piece 0
        }
    }

    incr dayofyear -1
    incr month     -1
    incr year      -1900
    set dst [string match ?DT $tz]

    return [list \
        $minute $hours $dayofmonth $month $year $dayofweek $dayofyear $dst]
}



#
# ns_time
#

::nstcl::ad_proc ::nstcl::ns_time {} {
    summary "Returns the current time relative to the epoch"

    description {
        <p>Returns the number of elapsed seconds since the beginning of
        the epoch.</p>
    }
} {
    return [clock seconds]
}



#
# num_days_difference
#

::nstcl::ad_proc ::nstcl::num_days_difference {-absolute_value:boolean
                                               {-floor ""} {-ceiling ""} x y} {
    summary "Calculates the number of days of difference between two dates"

    description {
        <p>This function calculates the number of days difference between
        two dates, <i>x</i> and <i>y</i>.  Each date should be either
        an integer (the number of seconds since the epoch), or, a date
        (relative or absolute) in a format parseable by 
        <i>[clock scan]</i>.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-absolute_value</option></dt>
            <dd>
                If this switch is present, and the result is a negative
                number, then the absolute value will be returned instead.
            </dd>

            <dt><option>-floor</option></dt>
            <dd>
                If a value for <i>floor</i> is given, and the calculated
                result is less than <i>floor</i>, then <i>floor</i> will
                be returned instead.
            </dd>

            <dt><option>-ceiling</option></dt>
            <dd>
                If a value for <i>ceiling</i> is given, and the calculated
                result is more than <i>ceiling</i>, then <i>ceiling</i>
                will be returned instead.
            </dd>
        </dle>
    }
} {
    if {![string is integer -strict $x]} {
        set x [clock scan $x]
    }
 
    if {![string is integer -strict $y]} {
        set y [clock scan $y]
    }
 
    if {$absolute_value_p} {
        set diff [expr {abs(($y - $x) / 86400)}]
    } else {
        set diff [expr {($y - $x) / 86400}]
    }
 
    if {[string is integer -strict $floor] && $diff < $floor} {
        return $floor
    } elseif {[string is integer -strict $ceiling] && $diff > $ceiling} {
        return $ceiling
    } else {
        return $diff
    }
}



# 
# start_of_day
#

::nstcl::ad_proc ::nstcl::start_of_day {date} {
    summary "Returns the timestamp of the start of a day"

    description {
        <p>Returns first second of <i>date</i> in the format of 
        "YYYY-MM-DD 23:59:59".  <i>date</i> can be in any form
        understood by <i>[clock scan]</i>.</p>
    }
} {
    return [clock format [clock scan $date] -format "%Y-%m-%d 00:00:00"]
}



#
# sysdate
#

::nstcl::ad_proc ::nstcl::sysdate {{-seconds:boolean}
                                   {-format "%Y-%m-%d"}
                                   {-offset "0 days ago"}
                                   {-base ""}} {
    summary "Returns the current date"

    description {
        <p>This function returns the current date.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-seconds</option></dt>
            <dd>
                This switch causes the result to be returned as the number
                of seconds since the epoch.
            </dd>

            <dt><option>-format</option></dt>
            <dd>
                Specifies the way the result should be formatted.  This
                option has no effect if <i>-seconds</i> is specified.
                For a list of formatting string options, consult the
                documentation for <i>[clock format]</i>.
            </dd>

            <dt><option>-offset</option> (defaults to <i>0 days ago</i>)</dt>
            <dd>
                This switch can be used to shift the date calculation
                from today (<i>0 days ago</i>) to some other relative
                time (such as <i>2 weeks ago</i>) understood by 
                <i>[clock scan]</i>.
            </dd>

            <dt><option>-base</option></dt>
            <dd>
                Calculate the date based on a given (absolute) date as
                a <i>base</i>.  (Any date understood by <i>[clock scan]</i>
                can be used.)  If no <i>base</i> is specified the current
                time is used instead.
            </dd>
        </dle>
    }
} {
    if {[string equal $base ""]} {
        set time [clock seconds]
    } else {
        if {![string is integer -strict $base]} {
            set time [clock scan $base]
        } else {
            set time $base
        }
    }
 
    set time [clock scan $offset -base $time]

    if {$seconds_p} {
        return $time
    } else {
        return [clock format $time -format $format]
    }
}


#
# scheduled procs
#

namespace eval ::nstcl::scheduled {
    variable id 0
}

::nstcl::ad_proc -private ::nstcl::scheduled::PROC {id interval once_p proc
                                                    counter orig_sched 
                                                    beginning} {
    if {![info exists ::nstcl::scheduled_procs($id)]} then return

    if {!$once_p} {
        set ::nstcl::scheduled_procs(\$id) [after $interval \
            ::nstcl::scheduled::PROC $id $interval $once_p $proc \
                [incr counter] $orig_sched $beginning]
    } else {
        if {[info exists ::nstcl::scheduled_procs($id)]} {
            unset ::nstcl::scheduled_procs($id) 
        }
    }

    return [$proc]
}



#
# ns_schedule_proc
#

::nstcl::ad_proc ::nstcl::ns_schedule_proc {-thread:boolean -once:boolean
                                            {-beginning 0} interval args} {
    summary "Schedule a procedure to be run weekly"
    syntax "<command>::nstcl::ns_schedule_proc ?<m>-thread</m>? ?<m>-once</m> interval {script | procname ?args?}</command>"

    description {
        <p>Use this function to schedule a procedure to be run 
        in <i>interval</i> seconds.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-thread</option></dt>
            <dd>
                Specifies whether the procedure should run it's own
                thread or not.  Currently nstcl does not support threading,
                and so this switch is ignored.  (This may well change
                in a future release.)
            </dd>
            <dt><option>-once</option></dt>
            <dd>
                If specified the procedure will be run once.  If it is
                not present, it will be run weekly on the specified
                day at the specified time.
            </dd>
        </dle>
    }

    see_also ns_unschedule_proc
} {
    package require md5
    variable scheduled_procs
    upvar 0 ::nstcl::scheduled::id ids

    if {[llength $args] == 0} {
        return -code error "No script or procname specified to run periodically"
    }

    if {![string is integer -strict $interval]} {
        set interval [::nstcl::duration $interval]
    }

    if {![string is integer -strict $beginning]} {
        return -code error "-beginning must be an integer"
    }

    if {$interval <= 0} {
        return -code error "Minimum interval granularity is one second"
    }

    # convert to miliseconds for "after"
    set interval  [expr {$interval  * 1000}]
    set beginning [expr {$beginning * 1000}]

    if {$beginning <= 0} {
        set beginning $interval
    }

    set proc_name ::nstcl::scheduled::[::md5::md5 $args]

    if {[llength $args] == 1} {
        set proc_body [join $args]
    } else {
        set proc_body "# procname arg ?args?\n"
        foreach arg $args {
            append proc_body " \"[string map [list \\ \\\\ \" \\\"] $arg]\" "
        }
    }


    if {[info commands $proc_name] == ""} {
        proc $proc_name {} $proc_body
    }

    set id [incr ids]

    set scheduled_procs($id) [after $beginning ::nstcl::scheduled::PROC \
        $id $interval $once_p $proc_name 0 [clock seconds] $beginning]

    return $id
}



#
# ns_schedule_daily
#

::nstcl::ad_proc ::nstcl::ns_schedule_daily {-thread:boolean -once:boolean
                                             hour minute args} {
    summary "Schedule a procedure to be run weekly"
    syntax "<command>::nstcl::ns_schedule_weekly ?<m>-thread</m>? ?<m>-once</m> hour minute {script | procname ?args?}</command>"

    description {
        <p>Use this function to schedule a procedure to be run 
        weekly on a given day at a given time.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-thread</option></dt>
            <dd>
                Specifies whether the procedure should run it's own
                thread or not.  Currently nstcl does not support threading,
                and so this switch is ignored.  (This may well change
                in a future release.)
            </dd>
            <dt><option>-once</option></dt>
            <dd>
                If specified the procedure will be run once.  If it is
                not present, it will be run weekly on the specified
                day at the specified time.
            </dd>
        </dle>
    }

    see_also ns_unschedule_proc
} {
    if {![string is integer -strict $hour] || $hour < 0 || $hour > 23} {
        return -code error "Hour should be between 0 and 23, not \"$hour\""
    }

    if {![string is integer -strict $minute] || $minute < 0 || $minute > 59} {
        return -code error "Minute should be between 0 and 59, not \"$minute\""
    }

    set time [clock scan [format "%02d:%02d" $hour $minute]]
    set now  [clock seconds]

    if {$now > $time} {
        set seconds [expr {86400 + ($time - $now)}]
    } else {
        set seconds [expr {$time - $now}]
    }

    eval ::nstcl::ns_schedule_proc -thread=$thread_p \
                                   -once=$once_p \
                                   -beginning $seconds \
                                   day $args
}
        


#
# ns_schedule_weekly
#

::nstcl::ad_proc ::nstcl::ns_schedule_weekly {-thread:boolean -once:boolean
                                              day hour minute args} {
    summary "Schedule a procedure to be run weekly"
    syntax "<command>::nstcl::ns_schedule_weekly ?<m>-thread</m>? ?<m>-once</m> day hour minute {script | procname ?args?}</command>"

    description {
        <p>Use this function to schedule a procedure to be run 
        weekly on a given day at a given time.</p>
        <p><i>day</i> may be either the day name in English (Friday), the 
        standard 3 letter abbreviation (Fri) or a number between 0 and 6 
        (0 == Sunday, 6 == Saturday).</p>
    }

    optional_switches {
        <dle>
            <dt><option>-thread</option></dt>
            <dd>
                Specifies whether the procedure should run it's own
                thread or not.  Currently nstcl does not support threading,
                and so this switch is ignored.  (This may well change
                in a future release.)
            </dd>
            <dt><option>-once</option></dt>
            <dd>
                If specified the procedure will be run once.  If it is
                not present, it will be run weekly on the specified
                day at the specified time.
            </dd>
        </dle>
    }

    see_also ns_unschedule_proc
} {
    set days_of_week [list Sunday Monday Tuesday Wednesday Thursday \
        Friday Saturday]

    set dow [list Sun Mon Tue Wed Thu Fri Sat]

    if {[string is integer -strict $day] && $day >= 0 || $day <= 6} {
        set day [lindex $days_of_week $day]
    } elseif {[lsearch -exact $dow [string totitle $day]] != -1} {
        set day [lindex $days_of_week [lsearch -exact $dow \
            [string totitle $day]]]
    } elseif {[lsearch -exact $days_of_week [string totitle $day]] == -1} {
        return -code error "Day should be between 0 (Sunday) and 6 (Saturday)"
    } 

    if {![string is integer -strict $hour] || $hour < 0 || $hour > 23} {
        return -code error "Hour should be between 0 and 23, not \"$hour\""
    }

    if {![string is integer -strict $minute] || $minute < 0 || $minute > 59} {
        return -code error "Minute should be between 0 and 59, not \"$minute\""
    }

    set time [clock scan [format "%s %02d:%02d" $day $hour $minute]]
    set now  [clock seconds]

    if {$now > $time} {
        set seconds [expr {(86400 * 7) + ($time - $now)}]
    } else {
        set seconds [expr {$time - $now}]
    }

    eval ::nstcl::ns_schedule_proc -thread=$thread_p \
                                   -once=$once_p \
                                   -beginning $seconds \
                                   week $args
}
        


#
# ns_unschedule_proc
#

::nstcl::ad_proc ::nstcl::ns_unschedule_proc id {
    summary "Cancels a scheduled procedure"

    description {
        <p>This function cancels the scheduled procedure associated
        with <i>id</i> (which is the id returned by the <i>ns_schedule*</i>
        command that was used to schedule the procedure initially).</p>
    }

    see_also "ns_schedule_proc, ns_schedule_daily, ns_schedule_weekly"
} {
    variable scheduled_procs

    if {![info exists scheduled_procs($id)]} {
        return -code error "There is no scheduled_proc with an id of \"$id\""
    } else {
        after cancel $scheduled_procs($id)
        unset scheduled_procs($id)
    }
}


package provide nstcl-time 1.2
