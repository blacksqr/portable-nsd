# packages/acs-core-ui/www/acs_object/permissions/revoke-2.tcl

ad_page_contract {

  @author rhs@mit.edu
  @creation-date 2000-08-20
  @cvs-id $Id: revoke-2.tcl,v 1.2.4.2 2003/05/01 19:18:29 peterm Exp $
} {
  object_id:integer,notnull
  revoke_list:multiple
  { operation "" }
  {application_url ""}
}

permission::require_permission -object_id $object_id -privilege admin

if { [string eq $operation "Yes"] } {
    db_transaction {
	foreach item $revoke_list {
	    set party_id [lindex $item 0]
	    set privilege [lindex $item 1]
            permission::revoke -party_id $party_id -object_id $object_id -privilege $privilege
	}
    }
}

ad_returnredirect "one?[export_vars {object_id application_url}]"
