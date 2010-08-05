ad_library {

    Initializes datastructures for utility procs.

    @creation-date 02 October 2000
    @author Bryan Quinn
    @cvs-id $Id: utilities-init.tcl,v 1.8 2008/10/10 11:30:35 gustafn Exp $
}

# initialize the random number generator
randomInit [ns_time]

# Create mutex for util_background_exec
nsv_set util_background_exec_mutex . [ns_mutex create oacs:bg_exec]

# if maxbackup in config is missing or zero, don't run auto-logrolling
set maxbackup [ns_config -int "ns/parameters" maxbackup 0]

if { $maxbackup } {
    ad_schedule_proc -all_servers t -schedule_proc ns_schedule_daily \
	[list 00 00] util::roll_server_log
}
