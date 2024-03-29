ad_page_contract {
    shows User A what User B has contributed to the community
    
    @param user_id defaults to currently logged in user if there is one
    @cvs-id $Id: community-member.tcl,v 1.4.2.2 2003/07/03 15:31:37 lars Exp $
} {
    user_id:integer
} -properties {
    context:onevalue
    member_state:onevalue
    first_names:onevalue
    last_name:onevalue
    email:onevalue
    inline_portrait_state:onevalue
    portrait_export_vars:onevalue
    width:onevalue
    height:onevalue
    system_name:onevalue
    pretty_creation_date:onevalue
    show_email_p:onevalue
    url:onevalue
    bio:onevalue
    verified_user_id:onevalue
    user_contributions:multirow
}

#See if this page has been overrided by a parameter in kernel 
set community_member_url [ad_parameter -package_id [ad_acs_kernel_id] CommunityMemberURL "/shared/community-member"]
if { $community_member_url != "/shared/community-member" } {
    ad_returnredirect "$community_member_url?user_id=$user_id"
    ad_script_abort
}

set site_wide_admin_p [acs_user::site_wide_admin_p]
set admin_user_url [acs_community_member_admin_url -user_id $user_id]

set verified_user_id [ad_verify_and_get_user_id]

if { [empty_string_p $user_id] } {
    if { $verified_user_id == 0 } {
	# Don't know what to do! 
	ad_return_error "Missing user_id" "We need a user_id to display the community page"
	return
    }
    set user_id $verified_user_id
}

set bind_vars [ad_tcl_vars_to_ns_set user_id]

    
if { ![db_0or1row user_information "select first_names, last_name, email, priv_email, url, creation_date, member_state from cc_users where user_id = :user_id" -bind $bind_vars]} {
    
    ad_return_error "No user found" "There is no community member with the user_id of $user_id"
    ns_log Notice "Could not find user_id $user_id in community-member.tcl from [ad_conn peeraddr]"
    return
}

set bio [person::get_bio -person_id $user_id]

# Do we show the portrait?
set inline_portrait_state "none"
set portrait_export_vars [export_url_vars user_id]

if [db_0or1row portrait_info "
select i.width, i.height, cr.title, cr.description, cr.publish_date
from acs_rels a, cr_items c, cr_revisions cr, images i
where a.object_id_two = c.item_id
and c.live_revision = cr.revision_id
and cr.revision_id = i.image_id
and a.object_id_one = :user_id
and a.rel_type = 'user_portrait_rel'"] {
    # We have a portrait. Let's see if we can show it inline


    if { ![empty_string_p $width] && $width < 300 } {
	# let's show it inline
	set inline_portrait_state "inline"
    } else {
	set inline_portrait_state "link"
    }
}


if { $priv_email <= [ad_privacy_threshold] } {
    set show_email_p 1
} else {
    set show_email_p 0
    # guy doesn't want his email address shown, but we can still put out 
    # the home page
}

set context [list "Community member"]
set system_name [ad_system_name]
set pretty_creation_date [util_AnsiDatetoPrettyDate $creation_date]
set login_export_vars "return_url=[ns_urlencode [acs_community_member_url -user_id $user_id]]"

set login_url [export_vars -base "/register/." { { return_url [ad_return_url]} }]

ad_return_template
