# set-on-vacation-until.tcl
ad_page_contract {
    Set someone as being on vacation till a give date.
    NOTE: on_vacation_util is expected as a date parameter, but
    ad_page_contract doesn't know how to handle that.

    @author
    @creation-date
    @cvs-id $Id: set-on-vacation-until.tcl,v 1.1.1.1 2001/03/13 22:59:26 ben Exp $
} {
} -properties {
    site_link:onevalue
    home_link:onevalue
    pretty_date:onevalue
}
# 

if [catch { ns_dbformvalue [ns_getform] on_vacation_until date on_vacation_until } errmsg] {
    ad_return_error "Invalid date" "AOLserver didn't like the date that you entered."
    return
}

set user_id [ad_get_user_id]

db_transaction {
    # We update the users table to maintain compatibility with acs installations prior to user_vacations
    set bind_vars [ad_tcl_vars_to_ns_set user_id on_vacation_until]
    db_dml pvt_set_vacation_update "update users set no_alerts_until = :on_vacation_until where user_id = :user_id" -bind $bind_vars

}

set home_link [ad_pvt_home_link]
set site_link [ad_site_home_link]
set pretty_date [util_AnsiDatetoPrettyDate $on_vacation_until]

ad_return_template