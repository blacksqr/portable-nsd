# packages/acs-core-ui/www/acs_object/permissions/grant-2.tcl

ad_page_contract {

  @author rhs@mit.edu
  @creation-date 2000-08-20
  @cvs-id $Id: grant-2.tcl,v 1.2.4.1 2003/05/01 19:18:29 peterm Exp $
} {
  object_id:integer,notnull
  party_id:integer,notnull
  privilege
  {application_url ""}
}

ad_require_permission $object_id admin

db_exec_plsql grant {
  begin
    acs_permission.grant_permission(:object_id, :party_id, :privilege);
  end;
}

ad_returnredirect "one?[export_vars {object_id application_url}]"
