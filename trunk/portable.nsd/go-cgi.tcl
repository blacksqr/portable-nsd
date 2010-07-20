#!/usr/local/bin/tclsh
# Try to run page code after quick bootstrap.

puts [time { 
    source [file join [file dirname [info script]] pnsd-bootstrap.tcl ] } ]

    set ::pnsd::url /
    set ::pnsd::querystring ""

#ad_user_login 2537

#    rp_filter ""
#    rp_handler



exit


    puts [ site_node /register/ ]
#exit



pnsd::write_html

#puts [info body ::nstcl::template::adp::compiled::46c245a20c8521ff0387a9737ba4b5bd]
#puts [info body template::adp_prepare]
#puts [info body ad_page_contract]
puts [info body util_memoize]
