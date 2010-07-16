ad_page_contract {
    Erases a portrait

    @cvs-id $Id: erase.tcl,v 1.3 2002/09/06 22:19:33 jeffd Exp $
} {
    {return_url "" }
    {user_id ""}
} -properties {
    context:onevalue
    export_vars:onevalue
    admin_p:onevalue
}

set current_user_id [ad_verify_and_get_user_id]

if [empty_string_p $user_id] {
    set user_id $current_user_id
    set admin_p 0
} else {
    set admin_p 1
}

ad_require_permission $user_id "write"

if {$admin_p} {
    set context [list [list "./?user_id=$user_id" "User's Portrait"] "Erase"]
} else {
    set context [list [list "./" "Your Portrait"] "Erase"]
}

set export_vars [export_form_vars user_id return_url]

