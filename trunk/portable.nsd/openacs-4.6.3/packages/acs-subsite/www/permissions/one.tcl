# packages-core-ui/www/acs_object/permissions/index.tcl
ad_page_contract {
    Display permissions and children for the given object_id

    Templated + cross site scripting holes patched by davis@xarg.net

    @author rhs@mit.edu
    @creation-date 2000-08-20
    @cvs-id $Id: one.tcl,v 1.2.2.4 2003/08/02 05:37:46 rmello Exp $
} {
    object_id:integer,notnull
    {children_p "f"}
    {application_url ""}
}

set user_id [ad_maybe_redirect_for_registration]
ad_require_permission $object_id admin

#
# RBM: Check if this is the Main Site and prevent the user from being
#      able to remove Read permission on "The Public" and locking
#      him/herself out.
#

set node_id [site_node::get_node_id_from_object_id -object_id $object_id]
set ancestor [site_node::closest_ancestor_package -node_id $node_id]
set extra_where_clause ""

if {[empty_string_p $ancestor]} {
	set extra_where_clause "AND grantee_id <> -1"
}

set name [ad_quotehtml [db_string name {}]]

set context [list [list "./" "Permissions"] "Permissions for $name"]

db_multirow inherited inherited_permissions { *SQL* } { 
    set grantee_name [ad_quotehtml $grantee_name]
}

db_multirow acl acl { *SQL* } {
    set grantee_name [ad_quotehtml $grantee_name]
}

set controls [list]

lappend controls "<a href=grant?[export_vars {application_url object_id}]>Grant Permission</a>"

db_1row context { *SQL* }

if { $security_inherit_p == "t" && ![empty_string_p $context_id] } {
    lappend controls "<a href=toggle-inherit?[export_vars {application_url object_id}]>Don't Inherit Permissions from $context_name</a>"
} else {
    lappend controls "<a href=toggle-inherit?[export_vars {application_url object_id}]>Inherit Permissions from $context_name</a>"
}

set controls "\[ [join $controls " | "] \]"

set export_form_vars [export_vars -form {object_id application_url}]

set show_children_url "one?[export_vars {object_id application_url {children_p t}}]"
set hide_children_url "one?[export_vars {object_id application_url {children_p f}}]"

if [string equal $children_p "t"] {
    db_multirow children children { *SQL* } {
        set c_name [ad_quotehtml $c_name]
    }
} else {
    db_1row children_count { *SQL* } 
}