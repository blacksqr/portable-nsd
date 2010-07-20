# packages/mbryzek-subsite/www/index.tcl

ad_page_contract {

  @author rhs@mit.edu
  @author mbryzek@mit.edu

  @creation-date 2000-09-18
  @cvs-id $Id: index.tcl,v 1.5.2.2 2003/06/06 09:12:38 lars Exp $
} {
} -properties {
    context:onevalue
    subsite_name:onevalue
    nodes:multirow
    admin_p:onevalue
    user_id:onevalue
}

# We may have to redirect to some application page
set redirect_url [parameter::get -parameter IndexRedirectUrl -default {}]

if { ![empty_string_p $redirect_url] } {
    ad_returnredirect $redirect_url
    ad_script_abort
}

set context [list]
set package_id [ad_conn package_id]
set admin_p [ad_permission_p $package_id admin]

set user_id [ad_conn user_id]

set subsite_name [db_string name {
    select acs_object.name(:package_id) from dual
}]

set node_id [ad_conn node_id]

db_multirow nodes site_nodes {}

set login_url "register/?[export_vars { { return_url {[ad_conn url]}} }]"

ad_return_template