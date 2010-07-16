#!/cygdrive/d/Tcl/bin/tclsh
# This script will autoinstall OpenACS 4.x 
# It will import necessary variables/libraries, and define stub functions

#
# Inspired by http://michael.cleverly.com/nstcl/database%5fapi.html
#


# John Sequeira
# johnseq@pobox.com
# 10/2002

#source pnsd-init.tcl

pnsd::source_openacs

#rename apm_package_install ""

::nstcl::ad_proc  apm_package_install { 
    {-callback apm_dummy_callback}
    {-copy_files:boolean}
    {-load_data_model:boolean}
    {-data_model_files 0}
    {-install_path ""}
    spec_file_path } {

    Registers a new package and/or version in the database, returning the version_id.
    If $callback is provided, periodically invokes this procedure with a single argument
    containing a human-readable (English) status message.
    @param spec_file_path The path to an XML .info file relative to
    @return The version_id if successfully installed, 0 otherwise.
} {
    set version_id 0
    array set version [apm_read_package_info_file $spec_file_path]
    set package_key $version(package.key)

    if { $copy_files_p } {
	if { [empty_string_p $install_path] } {
	    set install_path [apm_workspace_install_dir]/$package_key
	}
	ns_log Notice "Copying $install_path to [acs_package_root_dir $package_key]"
	exec "cp" "-r" -- "$install_path/$package_key" [acs_root_dir]/packages/
    }

    # Install Queries (OpenACS Query Dispatcher - ben)
#    apm_package_install_queries $package_key $version(files)

    if { $load_data_model_p } {
	    apm_package_install_data_model -callback $callback -data_model_files $data_model_files $spec_file_path
    }


	set package_uri $version(package.url)
	set package_type $version(package.type)
	set package_name $version(package-name)
	set pretty_plural $version(pretty-plural)
	set initial_install_p $version(initial-install-p)
	set singleton_p $version(singleton-p)
	set version_name $version(name)
	set version_uri $version(url)
	set summary $version(summary)
	set description_format $version(description.format)
	set description $version(description)
	set release_date $version(release-date)
	set vendor $version(vendor)
	set vendor_uri $version(vendor.url)
	set split_path [split $spec_file_path /]
	set relative_path [join [lreplace $split_path 0 [lsearch -exact $package_key $split_path]] /] 
	# Register the package if it is not already registered.
	if { ![apm_package_registered_p $package_key] } {
	    apm_package_register $package_key $package_name $pretty_plural $package_uri $package_type $initial_install_p $singleton_p $relative_path
	}
	    
	# If an older version already exists in apm_package_versions, update it;
	# otherwise, insert a new version.
	if { [db_0or1row version_exists_p {
	    select version_id 
	    from apm_package_versions 
	    where package_key = :package_key
	    and version_id = apm_package.highest_version(:package_key)
	} ]} {
	    set version_id [apm_package_install_version -callback $callback $package_key $version_name \
		    $version_uri $summary $description $description_format $vendor $vendor_uri $release_date]
	    apm_version_upgrade $version_id
	    apm_package_upgrade_parameters -callback $callback $version(parameters) $package_key
	} else {
	    set version_id [apm_package_install_version -callback $callback $package_key $version_name \
				$version_uri $summary $description $description_format $vendor $vendor_uri $release_date]

	    ns_log Notice "INSTALL-HACK-LOG-BEN: version_id is $version_id"

	    if { !$version_id } {
		# There was an error.
		apm_callback_and_log $callback "The package version could not be created."
	    }
	    # Install the paramters for the version.
	    apm_package_install_parameters -callback $callback $version(parameters) $package_key
	}
	# Update all other package information.
	apm_package_install_dependencies -callback $callback $version(provides) $version(requires) $version_id
	apm_package_install_owners -callback $callback $version(owners) $version_id
	apm_package_install_files -callback $callback $version(files) $version_id
	apm_callback_and_log $callback "<p>Installed $version(package-name), version $version(name).<p>"


    if {![string compare $package_type "apm_service"] && ![string compare $singleton_p "t"]} {
	# This is a singleton package.  Instantiate it automatically.
	if {[catch {
	    db_exec_plsql package_instantiate_mount {
	        declare
  	            instance_id   apm_packages.package_id%TYPE;
	        begin
	            instance_id := apm_package.new(
	                          instance_name => :package_name,
			  	  package_key => :package_key,
				  context_id => acs.magic_object_id('default_context')
				  );
	        end;
	    }
	} errmsg]} {
	    apm_callback_and_log $callback "[string totitle $package_key] not instantiated.<p> Error:
	    <pre><blockquote>[ad_quotehtml $errmsg]</blockquote></pre>"
	} else {
	    apm_callback_and_log $callback "[string totitle $package_key] instantiated as $package_key.<p>"
	}
    }
    return $version_id
}


proc apm_file_add { args } { }
set ::pnsd::invariants(apm_file_add) 1 


#Will this work?
::nstcl::ad_proc util_memoize_____ {tcl_statement {oldest_acceptable_value_in_seconds ""}} {
    Returns the result of evaluating the Tcl statement argument
    and remembers that value in a cache; the memory persists
    for the specified number of seconds
    (or until the server is restarted if the second argument is not supplied)
    or until someone calls util_memoize_flush with the same Tcl statement.
    Note that this procedure should be used with care because it calls the
    eval built-in procedure (and therefore an unscrupulous user could do Bad Things.
} {
    # we look up the statement in the cache to see if it has already
    # been eval'd.  The statement itself is the key
    if {[catch {
            set statement_value [nsv_get util_memoize_cache_value $tcl_statement]
        }] \
        || (![empty_string_p $oldest_acceptable_value_in_seconds] \
            && [nsv_get util_memoize_cache_timestamp $tcl_statement] + $oldest_acceptable_value_in_seconds < [ns_time]) \
    } {
        # not in the cache already OR the caller spec'd an expiration
        # time and our cached value is too old

        set statement_value [eval $tcl_statement]
        nsv_set util_memoize_cache_value $tcl_statement $statement_value
        # store the time in seconds since 1970
        nsv_set util_memoize_cache_timestamp $tcl_statement [ns_time]
    }

    return $statement_value
}



apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer.tcl"

apm_source "$::pnsd::root/packages/acs-bootstrap-installer/installer/auto-install.tcl"





