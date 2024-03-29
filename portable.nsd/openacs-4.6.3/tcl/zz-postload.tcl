# $Id: zz-postload.tcl,v 1.2.4.1 2003/01/30 22:49:01 jeffd Exp $
# Name:        00-ad-postload.tcl
# Author:      Jon Salz <jsalz@mit.edu>
# Date:        24 Feb 2000
# Description: Sources library files that need to be loaded after the rest.

set tcllib [ns_info tcllib]

ns_log "Notice" "Sourcing files for postload..."
foreach file [glob -nocomplain ${tcllib}/*.tcl.postload] {
    ns_log Notice "postloading $file"
    source "$file"
}
ns_log "Notice" "Done."

# This should probably be moved to the end of bootstrap.tcl once all files are
# weeded out of the tcl directory.
ns_log "Notice" "Executing initialization code blocks..."
foreach init_item [nsv_get ad_after_server_initialization .] {
    array set init $init_item

    ns_log "Notice" "Executing initialization code block $init(name) in $init(script)"
    if { [llength $init(args)] == 1 } {
	set init(args) [lindex $init(args) 0]
    }
    if { [catch $init(args) error] } {
	global errorInfo
	ns_log "Error" "Error executing initialization code block $init(name) in $init(script): $errorInfo"
    }
}

# We need to load query files for the top-level stuff in www and tcl
# dirs is the list of directories to walk for xql files.  Packages .xql
# files are parsed elsewhere in the bootstrap process.

set dirs {www tcl}

# The __is_xql helper function is used to filter out just the xql files.
#
# It should return true for directories it should descend as well
# If you had a large static tree with no .xql files you could return 0 on 
# the subdirectory and it would not be searched.

proc __is_xql {arg} { 
    return [expr {[file isdir $arg] || [string match -nocase {*.xql} $arg]}]
}

foreach dir $dirs {
    set files [ad_find_all_files -check_file_func __is_xql [acs_root_dir]/$dir]
    
    ns_log Debug "QD=Postload files to load: $files"

    foreach file $files {
	db_qd_load_query_file $file
    }
}

rename __is_xql {}

nsv_unset ad_after_server_initialization .
