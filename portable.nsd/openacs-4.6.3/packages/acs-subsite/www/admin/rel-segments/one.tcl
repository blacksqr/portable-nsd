# /packages/subsite/www/admin/rel-segments/one.tcl

ad_page_contract {

    Displays information about one relational segment

    @author mbryzek@arsdigita.com
    @creation-date Mon Dec 11 14:38:26 2000
    @cvs-id $Id: one.tcl,v 1.3.2.1 2003/06/11 10:36:20 lars Exp $

} {
    segment_id:naturalnum,notnull
} -properties {
    context:onevalue
    props:onerow
    number_elements:onevalue
    admin_p:onevalue
} -validate {
    segment_exists_p -requires {segment_id:notnull} {
	if { ![rel_segments_permission_p $segment_id] } {
	    ad_complain "The segment either does not exist or you do not have permission to view it"
	}
    }
    segment_in_scope_p -requires {segment_id:notnull segment_exists_p} {
	if { ![application_group::contains_segment_p -segment_id $segment_id]} {
	    ad_complain "The segment either does not exist or does not belong to this subsite."
	}
    }
}

set context [list [list "./" "Relational segments"] "One segment"]

if { ![db_0or1row select_segment_properties {
    select s.segment_id, s.segment_name, s.group_id, acs_object.name(s.group_id) as group_name,
           s.rel_type, acs_object_type.pretty_name(r.rel_type) as rel_type_pretty_name,
           acs_rel_type.role_pretty_plural(r.role_two) as role_pretty_plural
      from rel_segments s, acs_rel_types r
     where s.segment_id = :segment_id
       and s.rel_type = r.rel_type
} -column_array props] } {
    ad_return_error "Segment does not exist" "Segment $segment_id does not exist"
    return
}

set name_edit_url [export_vars -base edit { segment_id }]

# Pull out the number of elements that the current user can see. We do
# this separately to avoid the join above. This query may need to
# removed or changed to handle the permissions check more efficiently

set group_id $props(group_id)
set rel_type $props(rel_type)

set user_id [ad_conn user_id]

db_multirow constraints constraints_select {
    select c.constraint_id, c.constraint_name, c.rel_side
      from rel_constraints c
     where c.rel_segment = :segment_id
}

db_1row select_segment_info {
    select count(*) as number_elements
      from rel_segment_party_map map, acs_object_party_privilege_map perm
     where perm.object_id = map.party_id
       and perm.party_id = :user_id
       and perm.privilege = 'read' 
       and map.segment_id = :segment_id
}
set number_elements [util_commify_number $number_elements]

set admin_p [ad_permission_p $segment_id "admin"]

ad_return_template