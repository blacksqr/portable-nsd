ad_page_contract {
    Unsubscribes from the site

    @cvs-id $Id: unsubscribe-2.tcl,v 1.1.1.1 2001/03/13 22:59:26 ben Exp $
} {} -properties {
    system_name:onevalue
}

set user_id [ad_get_user_id]
set rel_id [db_string rel_id "select rel_id
from group_member_map
where group_id = acs.magic_object_id('registered_users')
  and member_id = :user_id"]


db_exec_plsql unused "
begin
  membership_rel.deleted( rel_id => :rel_id );
end;"

set system_name [ad_system_name]

ad_return_template
