source pnsd-init.tcl

#source ns_dbsetup.tcl
pnsd::source_openacs
ad_page_contract {

    This page can be used to perform a complete non-interactive installation.
    All packages are installed.
    @author Bryan Quinn (bquinn@arsdigita.com)
    @cvs-id $Id: auto-install.tcl,v 1.4 2001/09/06 19:32:18 vinodk Exp $
} {
    {system_name "System Name"}
    {publisher_name "Publisher Name"}
    {system_owner "System Owner"}
    {admin_owner "Admin Owner"}
    {host_administrator "Host Administrator"}
    {outgoing_sender "Outgoing Sender"}
    {email "system"}
    {first_names "system"}
    {last_name "manager"}
    {password "changeme"}
    {password_question "Who am I?"}
    {password_answer "system manager"}
}



db_transaction {
    set user_id 259
    db_exec_plsql grant_admin {
	begin
	acs_permission.grant_permission (
					 object_id => acs.magic_object_id('security_context_root'),
					 grantee_id => :user_id,
					 privilege => 'admin'
					 );
	end;
    }
}



exit
puts [db_exec_plsql apm_file_add {
	:1 := apm_package_version.add_file(
		file_id => NULL,
		version_id => '5',
		path => 'sql/postgresql/acs-create.sql',
		file_type => 'data_model',
                db_type => 'postgresql'
		);
	end;
}]

exit


db_exec_plsql apm_package_version_enable {
	begin
	  apm_package_version.enable(
            version_id => 5
	  );
	end;
    }


set user_id 1017
db_exec_plsql grant_admin {
	    begin
		acs_permission.grant_permission (
		    object_id => acs.magic_object_id('security_context_root'),
		    grantee_id => :user_id,
		    privilege => 'admin'
		);
	    end;
}


exit

