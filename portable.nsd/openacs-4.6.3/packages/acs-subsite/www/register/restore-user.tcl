ad_page_contract {
    The page restores a user from the deleted state.
    @cvs-id $Id: restore-user.tcl,v 1.2 2001/09/20 22:44:46 donb Exp $
} {
    user_id:naturalnum
} -properties {
    site_link:onevalue
    export_vars:onevalue
    email:onevalue
}

if {![db_0or1row user_state_info {
    select member_state, email, rel_id from cc_users where user_id = :user_id
}]} { 
    ad_return_error "Couldn't find your record" "User id $user_id is not in the database.  This is probably out programming bug."
    return
}

if { $member_state == "deleted" } {
    
    # they presumably deleted themselves  
    # Note that the only transition allowed if from deleted
    # to authorized.  No other states may be restored

    db_dml member_state_authorized_transistion {
	update membership_rels
	set member_state = 'approved'  
	where rel_id = :rel_id
    }
    
} else {
    ad_return_error "Problem with authentication" "There was a problem with authenticating your account"
}

set site_link [ad_site_home_link]

# One common problem with login is that people can hit the back button
# after a user logs out and relogin by using the cached password in
# the browser. We generate a unique hashed timestamp so that users
# cannot use the back button.

set time [ns_time]
set token_id [sec_get_random_cached_token_id]
set token [sec_get_token $token_id]
set hash [ns_sha1 "$time$token_id$token"]

set export_vars [export_form_vars return_url time token_id hash email]

ad_return_template