ad_page_contract {
    Uploading user portraits

    @cvs-id $Id: upload.tcl,v 1.2 2002/09/06 21:50:14 jeffd Exp $
} {
    {user_id ""}
    {return_url ""}
} -properties {
    first_names:onevalue
    last_name:onevalue
    context:onevalue
    export_vars:onevalue
    
}

set current_user_id [ad_verify_and_get_user_id]

if [empty_string_p $user_id] {
    set user_id $current_user_id
    set admin_p 0
} else {
    set admin_p 1
}

ad_require_permission $user_id "write"

if ![db_0or1row name "select 
  first_names, last_name
from persons 
where person_id=:user_id"] {
    ad_return_error "Account Unavailable" "We can't find you (user #$user_id) in the users table.  Probably your account was deleted for some reason."
    return
}

if {$admin_p} {
    set context [list [list "./?user_id=$user_id" "User's Portrait"] "Upload Portrait"]
} else {
    set context [list [list "./" "Your Portrait"] "Upload Portrait"]
}

set export_vars [export_form_vars user_id return_url]

ad_return_template