ad_page_contract {
    Schedules all -procs.tcl and xql files of a package to be watched.


    @author Peter Marklund
    @cvs-id $Id: package-watch.tcl,v 1.1.2.1 2003/03/05 14:43:12 lars Exp $
} {
    package_key
} 

apm_watch_all_files $package_key

ad_returnredirect "index"