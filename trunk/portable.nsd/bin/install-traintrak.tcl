#
#  Install the traintrak LMS  
#
#

package require Tclx

source [file join [file dirname [info script]] .. pnsd-init.tcl ]
::pnsd::source_openacs ; #load libraries etc.
cmdtrace on procs



proc drop_package { package_key } {

    apm_package_deinstall $package_key ; 
    db_dml nuke_apm_version "delete from apm_package_versions where package_key = :package_key"; 

    db_source_sql_file $path/$file_path    [file join $::pnsd::root packages/ $package_key [string tolower [db_name]] "${package_key}-drop.sql"]

    puts "$package_key removed"; exit
}



set package_key "traintrak"


set spec_file  [file join $::pnsd::root packages/ $package_key ${package_key}.info]

#USE THIS TO START OVER
#drop_package $package_key ; exit;

proc tt_installer {} {
    puts "installing $package_key"; apm_package_install -load_data_model $spec_file
    
    puts "instantiate and mount"; apm_package_instantiate_and_mount -callback apm_dummy_callback $package_key; exit
}
#tt_installer


#exit
#set package_key  traintrak
#apm_package_install_data_model $spec_file



proc load_traintrak_data {} { 
    puts "loading traintrak data"

    db_transaction {
	
	db_0or1row get_user "select min(user_id) as user_id from users"
	
	db_0or1row get_email "select email from parties where party_id = :user_id"
	
	db_0or1row get_name "select first_names, last_name from cc_users where object_id = :user_id"
	
	set company_id [db_nextval "cs_company_id_sequence"]
	
	db_dml insert_employee "insert into cs_company
						(company_id, company_name)
						values ( :company_id, 'Default Company')"
	
	db_dml insert_employee "insert into cs_employee 
						(employee_id, company_id, first_names, last_name, email, position)
						values (:user_id, :company_id, 'Company', 'Admin', :email, '1')"
    }
    
    
}

load_traintrak_data