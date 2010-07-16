#
#
#
#

source [file join [file dirname [info script]] .. pnsd-init.tcl ]
::pnsd::source_openacs ; #load libraries etc.

ad_proc -private apm_package_install { 
    {-callback apm_dummy_callback}
    {-copy_files:boolean}
    {-load_data_model:boolean}
    {-data_model_files 0}
    {-install_path ""}
    spec_file_path } {


set spec_file  [file join $::pnsd::root packages/traintrak/traintrak.info]
puts $spec_file
exit
package require Tclx
#cmdtrace 2
apm_package_install -load_data_model $spec_file
#exit
#set package_key  traintrak
#apm_package_install_data_model $spec_file




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
 
