# packages/acs-subsite/www/permissions/toggle-inherit.tcl

ad_page_contract {

    Toggles the security_inherit_p flag.

    @author rhs@mit.edu
    @creation-date 2000-09-30
    @cvs-id $Id: toggle-inherit.tcl,v 1.2.2.2 2003/07/02 19:57:32 lars Exp $
} {
    object_id:integer,notnull
    {application_url ""}
    {return_url {[export_vars -base "one" {application_url object_id}]}}
}

ad_require_permission $object_id admin

permission::toggle_inherit -object_id $object_id

ad_returnredirect $return_url
