ad_page_contract {
    displays a user's portrait to other users

    @creation-date 26 Sept 1999
    @cvs-id $Id: portrait.tcl,v 1.3 2002/09/06 21:50:11 jeffd Exp $
} {
    user_id:integer
} -properties {
    context:onevalue
    first_names:onevalue
    last_name:onevalue
    description:onevalue
    export_vars:onevalue
    widthheight_param:onevalue
    publish_date:onevalue
}

if ![db_0or1row user_info "select 
  first_names, 
  last_name 
from persons 
where person_id=:user_id"] {
    ad_return_error "Account Unavailable" "We can't find user #$user_id in the users table."
    return
}

if ![db_0or1row get_item_id "select i.width, i.height, cr.title, cr.description, cr.publish_date
from acs_rels a, cr_items c, cr_revisions cr, images i
where a.object_id_two = c.item_id
and c.live_revision = cr.revision_id
and cr.revision_id = i.image_id
and a.object_id_one = :user_id
and a.rel_type = 'user_portrait_rel'"] {
    ad_return_complaint 1 "<li>You shouldn't have gotten here; we don't have a portrait on file for this person."
    return
}

if { ![empty_string_p $width] && ![empty_string_p $height] } {
    set widthheight_param "width=$width height=$height"
} else {
    set widthheight_param ""
}

set context [list [list [acs_community_member_url -user_id $user_id] "One Member"] "Portrait"]
set export_vars [export_url_vars user_id]
set publish_date [util_AnsiDatetoPrettyDate $publish_date]

ad_return_template