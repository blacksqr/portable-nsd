ad_page_contract {

    Manage notifications for one user

    @author Tracy Adams (teadams@alum.mit.edu)
    @creation-date 2002-07-22
    @cvs-id $Id: manage.tcl,v 1.1.2.2 2003/01/24 01:55:45 daveb Exp $
} {}

ad_maybe_redirect_for_registration
set user_id [ad_conn user_id]
set return_url [ad_conn url]
set context [list "Manage Notifications"]

db_multirow notifications select_notifications {}

ad_return_template
