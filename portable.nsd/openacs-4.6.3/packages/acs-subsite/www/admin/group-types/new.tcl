# /packages/mbryzek-subsite/www/admin/groups/add.tcl

ad_page_contract {
    Form to add a group type

    @author rhs@mit.edu
    @creation-date 2000-12-04
    @cvs-id $Id: new.tcl,v 1.3 2002/09/06 21:49:58 jeffd Exp $
} {
    { object_type:trim "" }
    { pretty_name:trim "" }
    { pretty_plural:trim "" }
    { supertype:trim "" }
    { approval_policy:trim "" }
} -properties {
    context:onevalue
}

set context [list [list "[ad_conn package_url]admin/group-types/" "Group types"] "Add type"]

template::form create group_type

template::element create group_type object_type \
	-datatype "text" \
	-label "Group type" \
	-html { size 30 maxlength 30 }

set supertype_options [db_list_of_lists "select_group_supertypes" {
	select replace(lpad(' ', (level - 1) * 4), ' ', '&nbsp;') || t.pretty_name, t.object_type
          from acs_object_types t
       connect by prior t.object_type = t.supertype
         start with t.object_type = 'group'}]

template::element create group_type supertype \
	-datatype "text" \
	-widget select \
	-options $supertype_options \
	-label "Supertype"

template::element create group_type pretty_name \
	-datatype "text" \
	-label "Pretty name" \
	-html { size 50 maxlength 100 }

template::element create group_type pretty_plural \
	-datatype "text" \
	-label "Pretty plural" \
	-html { size 50 maxlength 100 }

set approval_policy_options {
    { {Open: Users can create groups of this type} open }
    { {Wait: Users can suggest groups} wait }
    { {Closed: Only administrators can create groups} closed }
}

if { [template::form is_valid group_type] } {

    set exception_count 0
    
    # Verify that the object type (in safe oracle format) is unique
    
    set safe_object_type [plsql_utility::generate_oracle_name -max_length 29 $object_type]
    if { [plsql_utility::object_type_exists_p $safe_object_type] } {
	incr exception_count
	append exception_text "<li> The specified object type, $object_type, already exists. 
[ad_decode $safe_object_type $object_type "" "Note that we converted the object type to \"$safe_object_type\" to ensure that the name would be safe for the database."]
Please back up and choose another.</li>"
    } else {
	# let's make sure the names are unique
	if { [db_string pretty_name_unique {
	    select case when exists (select 1 from acs_object_types t where t.pretty_name = :pretty_name)
                    then 1 else 0 end
	  from dual
	}] } {
	    incr exception_count
	    append exception_text "<li> The specified pretty name, $pretty_name, already exists. Please enter another </li>"
	}

	if { [db_string pretty_name_unique {
	    select case when exists (select 1 from acs_object_types t where t.pretty_plural = :pretty_plural)
                    then 1 else 0 end
	  from dual
	}] } {
	    incr exception_count
	    append exception_text "<li> The specified pretty plural, $pretty_plural, already exists. Please enter another </li>"
	}
    }

    if { $exception_count > 0 } {
	ad_return_complaint $exception_count $exception_text
	ad_script_abort
    }

    db_transaction {	
	group_type::new -group_type $object_type -supertype $supertype $pretty_name $pretty_plural
    }
    ad_returnredirect ""
    return 
}

ad_return_template
