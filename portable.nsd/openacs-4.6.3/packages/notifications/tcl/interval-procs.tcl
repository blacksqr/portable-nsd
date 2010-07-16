ad_library {

    Notification Intervals.

    Procs to manage notification intervals. Intervals are the duration of time between notifications.
    Possible intervals range from "instantaneous" to "weekly".

    CURRENTLY DEPRECATED AND USELESS.

    @creation-date 2002-05-24
    @author Ben Adida <ben@openforce.biz>
    @cvs-id $Id: interval-procs.tcl,v 1.1.1.1.2.1 2002/10/30 21:12:05 ben Exp $

}

namespace eval notification::interval {

    ad_proc -public schedule_all {} {
        This schedules all the notification procs
    } {
    }

    ad_proc -public sweep_notifications {
        {-interval_id:required}
    } {
        This sweeps for notifications in a particular interval
    } {

    }

}
