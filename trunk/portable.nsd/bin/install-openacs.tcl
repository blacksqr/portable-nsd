#!/usr/local/bin/tclsh
#
# John Sequeira
# johnseq@pobox.com
# 9/03


if {[lsearch [namespace children] ::tcltest] == -1} {
    package require tcltest
    namespace import ::tcltest::*
}

source [file join [file dirname [info script]] .. pnsd-init.tcl ]

::pnsd::source_openacs


apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer.tcl"
#proc install-openacs-4.x {} {}
if {0} {
#    apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer.tcl"
    apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer/auto-install.tcl"
}


proc debugging {} {
#    proc install_do_packages_install {} {}
    
    proc install_good_data_model_p {} { return 0 } ; # make installer proceed
    proc install_do_data_model_install {}  {  } ; #skip all the psql load calls
    proc db_source_sql_file { args } {} ; 
#    proc ad_user_new {} { args }
}
debugging

#cmdtrace on procs notruncate [open c:/projects/portable.nsd/cmd.log w]
#site_node::new -name acs-lang -parent_id 429g

namespace eval plpgsql_utility {
    ad_proc  generate_attribute_parameter_call {
	{-prepend "" }
	function_name
	pairs
    } {
	Generates the arg list for a call to a pl/pgsql function

	@author Steve Woodcock (swoodcock@scholastic.co.uk)
	@creation-date 07/2001

    } {

	# Get the list of real args to the function
	#set real_args [util_memoize [list plpgsql_utility::get_function_args $function_name]]
	set real_args  [list plpgsql_utility::get_function_args $function_name]
	puts $real_args

	foreach row $pairs {
	    set attr [string trim [lindex $row 0]]
	    set user_supplied([string toupper $attr]) $attr
	}

	# For each real arg, append default or supplied arg value
	set pieces [list]
	foreach row $real_args {
	    set arg_name [lindex $row 0]
	    set arg_default [lindex $row 1]
	    if { [info exists user_supplied($arg_name)] } {
		lappend pieces "${prepend}$user_supplied($arg_name)"
	    } else {
		if { $arg_default == "" } {
		    lappend pieces "NULL"
		} else {
		    lappend pieces "'[db_quote $arg_default]'"
		}
	    }
	}

	return [join $pieces ","]
    }
}



#proc install-openacs-5 {}  { }
if {1} {
    set ::pnsd::url /installer/install


    set ::pnsd::querystring "email=johnseq%40pobox.com&username=system&first_names=John&last_name=Sequeira&password=test&password_confirmation=test&system_url=http%3A%2F%2FM600&system_name=yourdomain+Network&publisher_name=Yourdomain+Network%2C+Inc.&system_owner=johnseq%40pobox.com&admin_owner=johnseq%40pobox.com&host_administrator=johnseq%40pobox.com&outgoing_sender=johnseq%40pobox.com&new_registrations=johnseq%40pobox.com HTTP/1.1" 

# If you load index,  it will report any configuration anomalies.  Take a look at pnsd::write_html output and address any issues it raises
#    set script index  ; 
    set script install
    set path "[nsv_get acs_properties root_directory]/packages/acs-bootstrap-installer/installer/$script.tcl"
#    cmdtrace on
#    apm_source $path
    source $path
    pnsd::write_html
}

