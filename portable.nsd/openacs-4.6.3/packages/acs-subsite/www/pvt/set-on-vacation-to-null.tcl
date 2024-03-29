# /www/pvt/set-on-vacation-to-null.tcl
ad_page_contract {
    Set on vacation to null.

    @author Multipe
    @cvs-id $Id: set-on-vacation-to-null.tcl,v 1.1.1.1 2001/03/13 22:59:26 ben Exp $
} -properties {
    site_link:onevalue
    home_link:onevalue
}

set user_id [ad_get_user_id]

set no_alerts_until [db_string no_alerts_until {
    select no_alerts_until from users where user_id = :user_id
} -default ""] 

if { ![empty_string_p $no_alerts_until] } {
    set clear [db_null] 
    db_dml pvt_unset_no_alerts_until {
	    update users 
	    set no_alerts_until = :clear
	    where user_id = :user_id
    }
}

set site_link [ad_site_home_link]
set home_link [ad_pvt_home_link]

ad_return_template
