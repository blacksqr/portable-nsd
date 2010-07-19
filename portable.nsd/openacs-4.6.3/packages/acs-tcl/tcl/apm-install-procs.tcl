ad_library {

    Routines used for installing packages.

    @creation-date September 11 2000
    @author Bryan Quinn (bquinn@arsdigita.com)
    @cvs-id $Id: apm-install-procs.tcl,v 1.15.2.9 2003/06/18 13:07:28 peterm Exp $
}


ad_proc apm_scan_packages { 
    {-callback apm_dummy_callback}
    {-new:boolean}
    {path ""}
} {
    Scans a directory for unregistered package specification files.
    @param new.  Specify this parameter if you don't want packages that are already present
    to be picked up by the scan.  The initial installer needs to specify this.
    @return A list of unregistered .info files that can be parsed for further information.
} {

    if { [empty_string_p $path] } {
	set path "[apm_workspace_install_dir]"
    }

    ### Scan for all unregistered .info files.
    
    ns_log "Notice" "Scanning for new unregistered packages..."
    set new_spec_files [list]
    # Loop through all directories in the /packages directory, searching each for a
    # .info file.
    foreach dir [lsort [glob -nocomplain "$path/*"]] {
	set package_key [file tail $dir]
	if { ![file isdirectory $dir] } {
	    continue
	}
	if { [apm_ignore_file_p $dir] } {
	    apm_callback_and_log $callback "Skipping the directory \"$package_key\"."
	    continue
	}

	# At this point, we should have a directory that is equivalent to a package_key.
	if { [apm_package_installed_p $package_key] } {
	    if {$new_p} {
		continue
	    }
	}
	
	# Locate the .info file for this package.
	if { [catch { set info_file [apm_package_info_file_path -path $path $package_key] } error] } {
	    apm_callback_and_log -severity Warning $callback "Unable to locate specification file for package $package_key: $error"
	    continue
	}
	# We found the .info file.
	lappend new_spec_files $info_file
    }
        
    if { [llength $new_spec_files] == 0 } {
	ns_log "Notice" "No new packages found."
    }   
    return $new_spec_files
}
 

ad_proc -public apm_dependency_provided_p {
    {
	-dependency_list [list]
    }
    dependency_uri dependency_version
} {
    Returns 1 if the current system provides the dependency inquired about.
    Returns -1 if the version number is too low.
    Returns 0 otherwise.
    @param dependency_list Specify this if you want to a check a list of dependencies of form 
	{dependency_name dependency_version} in addition to querying the database for what the 
	system currently provides.
    @param dependency_uri The dependency that is being checked.
    @param dependency_version The version of the dependency being checked.
} {
    set old_version_p 0
    set found_p 0
    ns_log Debug "Scanning for $dependency_uri version $dependency_version"
    db_foreach apm_dependency_check {} {
	if { $version_p >= 0 } {
	    ns_log Debug "Dependency satisfied by previously installed package"
	    set found_p 1
	} elseif { $version_p == -1 } {
	    set old_version_p 1
	}
    }

    # Can't return while inside a db_foreach.
    if {$found_p} {
	return 1
    }

    if { ![empty_string_p $dependency_list] } {
	# They provided a list of provisions.
	foreach prov $dependency_list {
	    if { [string equal $dependency_uri [lindex $prov 0]] } {

                set provided_version [lindex $prov 1]
                set provided_p [db_string version_greater_p {}]

                if { $provided_p >= 0 } {
		    ns_log Debug "Dependency satisfied in list of provisions."
                    return 1
                } else { 
                    set old_version_p 1
                }
	    }
	}
    }
	
    if { $old_version_p} {
	return -1
    } else {
	return 0
    }
}

ad_proc -private pkg_info_new { package_key spec_file_path provides requires {dependency_p ""} {comment ""}} {
    
    Returns a datastructure that maintains information about a package.
    @param package_key The key of the package.
    @param spec_file_path The path to the package specification file
    @param provides A list of dependencies provided by the package.
    @param requires A list of requirements provided by the package..
    @param dependency_p Can the package be installed without violating dependency checking.
    @param comment Some text about the package.  Useful to explain why it fails dependency check.
    @return a list whose first element is a package key and whose second element is a path 
    to the associated .info file.
} {
    return [list $package_key $spec_file_path $provides $requires $dependency_p $comment]
}

ad_proc -private pkg_info_key {pkg_info} {

    @return The package-key  stored in the package info map.

} {
    return [lindex $pkg_info 0]
}

ad_proc -private pkg_info_spec {pkg_info} {

    @return The .info file stored in the package info map.

} {
    return [lindex $pkg_info 1]
}


ad_proc -private pkg_info_provides {pkg_info} {

    @return The dependencies provided by the package.

} {
    return [lindex $pkg_info 2]
}

ad_proc -private pkg_info_requires {pkg_info} {

    @return The dependencies required by the package info map.

} {
    return [lindex $pkg_info 3]
}

ad_proc -private pkg_info_dependency_p {pkg_info} {

    @return Does it pass the dependency checker?  "" Means it has not been run yet.

} {
    return [lindex $pkg_info 4]
}

ad_proc -private pkg_info_comment {pkg_info} {

    @return Any comment specified about this package.

} {
    return [lindex $pkg_info 5]
}

# DRB: This routine does more than check dependencies, it also parses spec files,
# something that really should be done separately, at least for bootstrap installation.
# I'm leaving it alone for now, though, and kludging it further by passing in a
# boolean to determine whether to process all spec files or just those needed for
# initial bootstrap installation.  I've also modified it to screen out packages that
# don't support the currently running RDBMS - a bit of a hack to do it here but it
# needed doing somewhere...

ad_proc -private apm_dependency_check {
    {-callback apm_dummy_callback}
    {-initial_install:boolean}
    {-pkg_info_all {}}
    spec_files
} {
    Check dependencies of all the packages provided.
    @param spec_files A list of spec files to be processed.
    @param initial_install Only process spec files with the initial install attribute.
    @param pkg_info_all If you supply this argument, when a
    requirement goes unsatisfied, instead of failing, this proc will
    try to add whatever other packages are needed to the install set. The list of package keys to
    add will be the third element in the list returned.
    @return A list whose first element indicates whether dependencies were satisfied (1 if so, 0 otherwise).\
    The second element is the package info list with the packages ordered according to dependencies.\
    Packages that can be installed come first.  Any packages that failed the dependency check come last.
    The third element is a list of package keys on additional packages to install, in order to satisfy dependencies.
} {
    #### Iterate over the list of info files.
    ## Every time we satisfy another package, remove it from install_pend, and loop again.
    ## If we don't satisfy at least one more package, halt.
    ## install_in - Package info structures for packages that can be installed in a satisfactory order.
    ## install_pend - Stores package info structures fro packages that might have their dependencies satisfied 
    ##		      by packages in the install set.
    ## extra_package_keys - package keys of extra packages to install to satisfy all requirements.

    set extra_package_keys [list]

    set updated_p 1
    set install_in [list]
    foreach spec_file $spec_files {
	if { [catch {
	    array set package [apm_read_package_info_file $spec_file]
	    if { ([string equal $package(initial-install-p) "t"] || !$initial_install_p) && \
                    [apm_package_supports_rdbms_p -package_key $package(package.key)] } {
                lappend install_pend [pkg_info_new $package(package.key) $spec_file $package(provides) $package(requires) ""]
            }

            # Remove this package from the pkg_info_all list ...
            # either we're already installing it, or it can't be installed
            set counter 0
            foreach pkg_info $pkg_info_all {
                if { [string equal [pkg_info_key $pkg_info] $package(package.key)] } {
                    set pkg_info_all [lreplace $pkg_info_all $counter $counter]
                    break
                }
                incr counter
            }
	} errmsg]} {
	    # Failed to parse the specificaton file.
	    apm_callback_and_log $callback "$spec_file could not be parsed correctly.  It is not being installed. 
	    The error: $errmsg"
	}
    }

    # Outer loop tries to find a package from the pkg_info_all list to add if 
    # we're stuck because of unsatisfied dependencies
    set updated_p 1
    while { $updated_p } {

        # Inner loop tries to add another package from the install_pend list
        while { $updated_p && [exists_and_not_null install_pend]} {
            set install_in_provides [list]
            set new_install_pend [list]
            set updated_p 0
            # Generate the list of dependencies currently provided by the install set.
            foreach pkg_info $install_in {
                foreach prov [pkg_info_provides $pkg_info] {
                    lappend install_in_provides $prov
                }
            }	
            # Now determine if we can add another package to the install set.
            foreach pkg_info $install_pend {
                set satisfied_p 1
                foreach req [pkg_info_requires $pkg_info] {
                    if {[apm_dependency_provided_p -dependency_list $install_in_provides \
                            [lindex $req 0] [lindex $req 1]] != 1} {
                        # Unsatisfied dependency.
                        set satisfied_p 0
                        # Check to see if we've recorded it already
                        set errmsg "Requires [lindex $req 0] of version >= [lindex $req 1]."
                        if { ![info exists install_error([pkg_info_key $pkg_info])] || \
                                [lsearch -exact $install_error([pkg_info_key $pkg_info]) $errmsg] == -1} {
                            lappend install_error([pkg_info_key $pkg_info]) $errmsg
                        }
                        lappend new_install_pend $pkg_info
                        break
                    }
                }
                if { $satisfied_p } {
                    # At least one more package was added to the list that can be installed, so repeat.
                    lappend install_in [pkg_info_new [pkg_info_key $pkg_info] [pkg_info_spec $pkg_info] \
                            [pkg_info_provides $pkg_info] [pkg_info_requires $pkg_info] \
                            "t" "Package satisfies dependencies."]
                    set updated_p 1
                }
            }
            set install_pend $new_install_pend
        }

        set updated_p 0
        
        if { [exists_and_not_null install_pend] && [llength $pkg_info_all] > 0 } {
            # Okay, there are some packages that could not be installed
            
            # Let's find a package, which
            # - have unsatisfied requirements
            # - and we have a package in pkg_info_all which provides what this package requires

            foreach pkg_info $install_pend {
                set satisfied_p 1
                foreach req [pkg_info_requires $pkg_info] {
                    set counter 0
                    foreach pkg_info_add $pkg_info_all {
                        # Will this package do anything to change whether this requirement has been satisfied?
                        if { [apm_dependency_provided_p [lindex $req 0] [lindex $req 1]] != 1 && \
                                [apm_dependency_provided_p -dependency_list [pkg_info_provides $pkg_info_add] \
                                [lindex $req 0] [lindex $req 1]] == 1 } {

                            # It sure does. Add it to list of packages to install
                            lappend install_pend $pkg_info_add

                            # Add it to list of extra package keys
                            lappend extra_package_keys [pkg_info_key $pkg_info_add]
                            
                            # Remove it from list of packages that we can possibly install
                            set pkg_info_all [lreplace $pkg_info_all $counter $counter]

                            # Note that we've made changes
                            set updated_p 1

                            # Now break out of pkg_info_all loop
                            break
                        }
                        incr counter
                    }
                    if { $updated_p } {
                        break
                    }
                }
                if { $updated_p } {
                    break
                }
            }
        }
    }
        
    set install_order(order) $install_in
    # Update all of the packages that cannot be installed.
    if { [exists_and_not_null install_pend] } {
	foreach pkg_info $install_pend {
	    lappend install_in [pkg_info_new [pkg_info_key $pkg_info] [pkg_info_spec $pkg_info] \
				    [pkg_info_provides $pkg_info] [pkg_info_requires $pkg_info] \
				    "f" $install_error([pkg_info_key $pkg_info])]
	}
	return [list 0 $install_in]
    }

    return [list 1 $install_in $extra_package_keys]
}

ad_proc -private apm_package_install { 
    {-enable:boolean}
    {-callback apm_dummy_callback}
    {-copy_files:boolean}
    {-load_data_model:boolean}
    {-data_model_files 0}
    {-install_path ""}
    {-mount_path ""}
    spec_file_path 
} {
    Registers a new package and/or version in the database, returning the version_id.
    If $callback is provided, periodically invokes this procedure with a single argument
    containing a human-readable (English) status message.

    @param spec_file_path The path to an XML .info file relative to
    @return The version_id if successfully installed, 0 otherwise.
} {
    set version_id 0
    array set version [apm_read_package_info_file $spec_file_path]
    set package_key $version(package.key)

    # Determine if we are upgrading or installing.
    set upgrade_from_version_name [apm_package_upgrade_from $package_key $version(name)]
    set upgrade_p [expr ![empty_string_p $upgrade_from_version_name]]

    if { $copy_files_p } {
	if { [empty_string_p $install_path] } {
	    set install_path [apm_workspace_install_dir]/$package_key
	}
	ns_log Notice "Copying $install_path to [acs_package_root_dir $package_key]"
	exec "cp" "-r" -- "$install_path/$package_key" [acs_root_dir]/packages/
    }

    with_catch errmsg {
	set package_uri $version(package.url)
	set package_type $version(package.type)
	set package_name $version(package-name)
	set pretty_plural $version(pretty-plural)
	set initial_install_p $version(initial-install-p)
	set singleton_p $version(singleton-p)
        set auto_mount $version(auto-mount)
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
	    apm_package_register \
                -spec_file_path $relative_path \
                $package_key \
                $package_name \
                $pretty_plural \
                $package_uri \
                $package_type \
                $initial_install_p \
                $singleton_p
	}

	# If an older version already exists in apm_package_versions, update it;
	# otherwise, insert a new version.
	if { $upgrade_p } {
            # We are upgrading a package

	    set version_id [apm_package_install_version -callback $callback $package_key $version_name \
		    $version_uri $summary $description $description_format $vendor $vendor_uri $auto_mount $release_date]
	    apm_version_upgrade $version_id
	    apm_package_upgrade_parameters -callback $callback $version(parameters) $package_key

	} else {
            # We are installing a new package

	    set version_id [apm_package_install_version \
                                -callback $callback \
                                $package_key $version_name \
				$version_uri $summary $description $description_format $vendor $vendor_uri $auto_mount $release_date]

	    if { !$version_id } {
		# There was an error.
                ns_log Error "Package $package_key could not be installed. Received version_id $version_id"
		apm_callback_and_log $callback "The package version could not be created."
	    }
	    # Install the parameters for the version.
	    apm_package_install_parameters -callback $callback $version(parameters) $package_key
	}

	# Update all other package information.
	apm_package_install_dependencies -callback $callback $version(provides) $version(requires) $version_id
	apm_package_install_owners -callback $callback $version(owners) $version_id
        apm_package_install_callbacks -callback $callback $version(callbacks) $version_id

	apm_callback_and_log $callback "<p>Installed $version(package-name), version $version(name).<p>"
    } {
	apm_callback_and_log -severity Error $callback "<p>Failed to install $version(package-name), version $version(name).  The following error was generated:
<pre><blockquote>
[ad_quotehtml $errmsg]
</blockquote></pre>"
	return 0
    }

    # Source Tcl procs and queries to be able
    # to invoke any Tcl callbacks after mounting and instantiation. Note that this reloading 
    # is only done in the Tcl interpreter of this particular request.
    apm_load_libraries -procs -force_reload -packages $package_key
    apm_load_queries -packages $package_key

    if { $upgrade_p } {
        # Run before-upgrade

        # DRB: another one-time hack to allow the upgrade of acs-kernel 4.6.1 to 4.6.2 from the APM.
        # We know that if the callback list is empty that we don't need to invoke any callbacks,
        # and that acs-kernel has no APM callbacks defined (in 4.6.2 at least) ...

        if { [llength $version(callbacks)] > 0 } {
            apm_invoke_callback_proc -version_id $version_id -type before-upgrade -arg_list [list from_version_name $upgrade_from_version_name to_version_name $version(name)]
        }

    } else {
        # Run before-install
        apm_invoke_callback_proc -version_id $version_id -type before-install
    }

    if { $load_data_model_p } {
        apm_package_install_data_model -callback $callback -data_model_files $data_model_files $spec_file_path
    }

    # Enable the package
    if { $enable_p } {
        nsv_set apm_enabled_package $package_key 1    

	apm_version_enable -callback $callback $version_id
    }
    
    # Instantiating, mounting, and after-install callback only invoked on initial install
    if { ! $upgrade_p } {
        # After install Tcl proc callback
        apm_invoke_callback_proc -version_id $version_id -type after-install

        set priority_mount_path [ad_decode $version(auto-mount) "" $mount_path $version(auto-mount)]
        if { ![empty_string_p $priority_mount_path] } {
            # This is a package that should be auto mounted

            set parent_id [site_node::get_node_id -url "/"]

            if { [catch {
                db_transaction {            
                    set node_id [site_node::new -name $priority_mount_path -parent_id $parent_id]
                }
            } error] } {
                # There is already a node with that path, check if there is a package mounted there
                array set node [site_node::get -url "/${priority_mount_path}"]
                if { [empty_string_p $node(object_id)] } {
                   # There is no package mounted there so go ahead and mount the new package
                   set node_id $node(node_id)
                } else {
                   # Don't unmount already mounted packages
                   set node_id ""
                }
           }

           if { ![empty_string_p $node_id] } {

                ns_log Notice "Mounting new instance of package $package_key at /${priority_mount_path}"
                site_node::instantiate_and_mount \
                                             -node_id $node_id \
                                             -node_name $priority_mount_path \
                                             -package_name $version(package-name) \
                                             -package_key $package_key

                apm_callback_and_log $callback "<p> Mounted an instance of the package at /${priority_mount_path} </p>"
           } {
                # Another package is mounted at the path so we cannot mount
                global errorInfo
                set error_text "Package $version(package-name) could not be mounted at /$version(auto-mount) , there may already me a package mounted there, the error is: $error"
                ns_log Error "$error_text \n\n$errorInfo"
                apm_callback_and_log $callback "<p> $error_text </p>"
            } 

        } elseif { [string equal $package_type "apm_service"] && [string equal $singleton_p "t"] } {
            # This is a singleton package.  Instantiate it automatically, but don't mount.

            # Using empty context_id
            ns_log Notice "Creating singleton instance of package $package_key"
            apm_package_instance_new $version(package-name) "" $package_key
        }
    } else {
        # After upgrade Tcl proc callback
        apm_invoke_callback_proc -version_id $version_id -type after-upgrade -arg_list [list from_version_name $upgrade_from_version_name to_version_name $version(name)]
    }

    # Flush the installed_p cache
    util_memoize_flush [list apm_package_installed_p_not_cached $package_key]

    return $version_id
}

ad_proc -private apm_package_install_version {
    {-callback apm_dummy_callback}
    {-version_id ""}
    package_key version_name version_uri summary description description_format vendor vendor_uri auto_mount {release_date ""} 
} {
    Installs a version of a package.

    @return The assigned version id.
} {
    if { [empty_string_p $version_id] } {
	set version_id ""
    }
    if { [empty_string_p $release_date] } {
	set release_date ""
    }

    # DRB: one time hack to allow us to upgrade from 4.6.1 to 4.6.2
    # from the APM.  This depends on the fact that the kernel itself
    # doesn't require the new callback feature.

    if { [catch {set ret [db_exec_plsql version_insert ""]} errmsg]} {
        ns_log Notice "Version insert failed, trying OpenACS 4.6.1 version, errmsg: $errmsg"
        return [db_exec_plsql version_insert_4.6.1 {}]
    } else {
        return $ret
    }

}


ad_proc -private apm_package_deinstall {
    {
	-callback apm_dummy_callback
    } package_key
} {

    Deinstalls a package from the filesystem.
    @param package_key The package  to be deinstaleled.

} {
    if {![apm_package_registered_p $package_key]} {
	apm_callback_and_log $callback "This package is not installed.  Done."
	return 0
    }

    # Obtain the portion of the email address before the at sign. We'll use this in the name of
    # the backup directory for the package.
    regsub {@.+} [cc_email_from_party [ad_get_user_id]] "" my_email_name

    set backup_dir "[apm_workspace_dir]/$package_key-removed-$my_email_name-[ns_fmttime [ns_time] "%Y%m%d-%H:%M:%S"]"
    
    apm_callback_and_log $callback "
    <li>Moving <tt>packages/$package_key</tt> to $backup_dir... "

    if { [catch { file rename "[acs_root_dir]/packages/$package_key" $backup_dir } error] } {
	apm_callback_and_log $callback "<font color=red>[ns_quotehtml $error]</font>"
    } else {
	apm_callback_and_log $callback "moved."
    }

    db_dml apm_uninstall_record {
	update apm_package_versions
	set    installed_p = 'f', enabled_p = 'f'
	where package_key = :package_key
    }

    apm_callback_and_log $callback "<li>Package marked as deinstalled.
    "
    return 1
}

ad_proc -private apm_package_delete {
    { 
	-callback apm_dummy_callback

    }
    {-remove_files:boolean}
    package_key
} {
    
    Deinstalls and deletes a package from the ACS and the filesystem.

} {
    apm_callback_and_log $callback "<li>Deleting $package_key..."
    db_exec_plsql apm_package_delete {
	begin
	    apm_package_type.drop_type(
	        package_key => :package_key,
	        cascade_p => 't'
            );
	end;
    }
    # Remove the files from the filesystem
    if {$remove_files_p==1} {
	if { [catch { 
	    file delete -force [acs_package_root_dir $package_key] 
	} error] } {
	    apm_callback_and_log $callback "<li>Unable to delete [acs_package_root_dir $package_key]:<font color=red>$error</font>"
	}
    }

    # Flush the installed_p cache
    util_memoize_flush [list apm_package_installed_p_not_cached $package_key]

    apm_callback_and_log $callback "<p>Done."
}

ad_proc -private apm_package_version_delete {
    {
	-callback apm_dummy_callback
    }
    version_id
} {
    Deletes a version from the database.
} {
    db_exec_plsql apm_version_delete {
	begin
	 apm_package_version.delete(version_id => :version_id);	 
	end;
    }
}

ad_proc -public apm_package_version_count {package_key} {
    
    @return The number of versions of the indicated package.
} {
    return [db_string apm_package_version_count {
	select count(*) from apm_package_versions
	where package_key = :package_key
    } -default 0]
}

ad_proc -private apm_package_install_data_model {
    {-callback apm_dummy_callback}
    {-upgrade_from_version_name ""}
    {-data_model_files "0"}
    {-path ""}
    spec_file
} {
    Given a spec file, reads in the data model files to load from it.
} {
    array set version [apm_read_package_info_file $spec_file]
    set package_key $version(package.key)
    set upgrade_to_version_name $version(name)

    if { [empty_string_p $path] } {
	set path "[acs_package_root_dir $package_key]"
    }
    set ul_p 0

    if {($data_model_files == 0)} {
	set data_model_files [apm_data_model_scripts_find \
		-upgrade_from_version_name $upgrade_from_version_name \
		-upgrade_to_version_name $upgrade_to_version_name \
		$package_key]
    }

    if { ![empty_string_p $data_model_files] } {
	apm_callback_and_log $callback "<p><li>Installing data model for $version(package-name) $version(name)...\n"
    }
    
    foreach item $data_model_files {
	set file_path [lindex $item 0]
	set file_type [lindex $item 1]
	ns_log Debug "APM: Now processing $file_path of type $file_type"
	if {![string compare $file_type "data_model_create"] || \
		![string compare $file_type "data_model_upgrade"] } {
	    if { !$ul_p } {
		apm_callback_and_log $callback "<ul>\n"
		set ul_p 1
	    }
	    apm_callback_and_log $callback "<li>Loading data model $path/$file_path...
<blockquote><pre>
"
	    db_source_sql_file -callback $callback $path/$file_path
	    apm_callback_and_log $callback "</pre></blockquote>\n"
	} elseif { ![string compare $file_type "sqlj_code"] } {
	    if { !$ul_p } {
		apm_callback_and_log $callback "<ul>\n"
		set ul_p 1
	    }
	    apm_callback_and_log $callback "<li>Loading SQLJ code $path/$file_path...
<blockquote><pre>
"
	    db_source_sqlj_file -callback $callback "$path/$file_path"
	    apm_callback_and_log $callback "</pre></blockquote>\n"
	}
    }
    if {$ul_p} {
	apm_callback_and_log $callback "</ul><p>"
    }
}

ad_proc -private apm_package_upgrade_parameters { 
    {-callback apm_dummy_callback} parameters package_key
} {

    Upgrades the parameters to the current version.

} {
    # Update each parameter that exists.
    foreach parameter $parameters {
	set parameter_name [lindex $parameter 0]
	# Keep a running tally of all parameters that are in the current version.
	set description [lindex $parameter 1]
	set section_name [lindex $parameter 2]
	set datatype [lindex $parameter 3]
	set min_n_values [lindex $parameter 4]
	set max_n_values [lindex $parameter 5]
	set default_value [lindex $parameter 6]
	if {[db_0or1row parameter_id_get {
	    select parameter_id from apm_parameters
	    where parameter_name = :parameter_name
	    and package_key = :package_key
	}]} {
	    ns_log Debug "APM: Updating parameter, $parameter_name:$parameter_id"
	    apm_parameter_update $parameter_id $package_key $parameter_name $description \
		    $default_value $datatype $section_name $min_n_values $max_n_values
	} else {
	    ns_log Debug "APM: Registering parameter, $parameter_name."
	    apm_parameter_register $parameter_name $description $package_key $default_value \
		    $datatype $section_name $min_n_values $max_n_values
	}	
    }
    ns_log Debug "APM: Parameter Upgrade Complete."
}

ad_proc -private apm_package_install_parameters { {-callback apm_dummy_callback} parameters package_key } {

    Installs a set of parameters into the package denoted by package_key.

} {
    foreach parameter $parameters {
	set parameter_name [lindex $parameter 0]
	set description [lindex $parameter 1]
	set section_name [lindex $parameter 2]
	set datatype [lindex $parameter 3]
	set min_n_values [lindex $parameter 4]
	set max_n_values [lindex $parameter 5]
	set default_value [lindex $parameter 6]
	apm_parameter_register $parameter_name $description $package_key $default_value $datatype \
	    $section_name $min_n_values $max_n_values
    }
}

ad_proc -private apm_package_install_dependencies { {-callback apm_dummy_callback} provides requires version_id} {

    Install all package dependencies.

} {
    ns_log Debug "APM: Installing dependencies."
    # Delete any dependencies register for this version.
    db_foreach all_dependencies_for_version {
	select dependency_id from apm_package_dependencies
	where version_id = :version_id
    } {
	apm_dependency_remove $dependency_id
    }



    foreach item $provides {
	set interface_uri [lindex $item 0]
	set interface_version [lindex $item 1]
	ns_log Debug "Registering dependency $interface_uri, $interface_version for $version_id"
	apm_interface_add $version_id $interface_uri $interface_version
    }

    foreach item $requires {
	set dependency_uri [lindex $item 0]
	set dependency_version [lindex $item 1]
	ns_log Debug "Registering dependency $dependency_uri, $dependency_version for $version_id"
	apm_dependency_add $version_id $dependency_uri $dependency_version
    }
}

ad_proc -private apm_package_install_owners_prepare {owner_names owner_uris } {

    Prepare the owners data structure for installation.

} {
    set owners [list]
    for {set i 0} {$i < [llength $owner_names] } {incr i} {
	if { ![empty_string_p [lindex $owner_names $i]] } {
	    lappend owners [list [lindex $owner_names $i] [lindex $owner_uris $i]]
	}
    }
    return $owners
}

ad_proc -private apm_package_install_owners { {-callback apm_dummy_callback} owners version_id} {
    
    Install all of the owners of the package version.
    
} {
    db_dml apm_delete_owners {
	delete from apm_package_owners where version_id = :version_id
    }
    set counter 0
    foreach item $owners {
	set owner_name [lindex $item 0]
	set owner_uri [lindex $item 1]
	db_dml owner_insert {
	    insert into apm_package_owners(version_id, owner_uri, owner_name, sort_key)
	    values(:version_id, :owner_uri, :owner_name, :counter)
	}
	incr counter
    }
}

ad_proc -private apm_package_install_files { {-callback apm_dummy_callback} files version_id } {

    Install all files related to the package.

} {
    db_dml files_delete {
	delete from apm_package_files where version_id = :version_id
    }
    
    db_transaction {
        foreach item $files {
	
	    set path [lindex $item 0]
	    set file_type [lindex $item 1]
            set db_type [lindex $item 2]
	    apm_file_add $version_id $path $file_type $db_type
        }
    }
}

ad_proc -private apm_package_install_callbacks {
    {-callback apm_dummy_callback}
    callback_list
    version_id
} {
    Install the Tcl proc callbacks for the package version.

    @author Peter Marklund
} {

    # DRB: one-time hack to allow for the upgrading of the 4.6.1 kernel to
    # 4.6.2 from the APM

    with_catch errmsg {
        db_dml delete_all_callbacks {}
    } {
        ns_log Notice "apm_package_install_callbacks failed (probably because you're upgrading from 4.6.1->4.6.2, if so ignore)"
        ns_log Notice "the error message was: $errmsg"
    }

    foreach {type proc} $callback_list {
        apm_set_callback_proc -version_id $version_id -type $type $proc
    }
}

ad_proc -private apm_package_install_spec { version_id } {

    Writes the XML-formatted specification for a package to disk,
    marking it in the database as the only installed version of the package.
    Creates the package directory if it doesn't already exist. Overwrites
    any existing specification file; or if none exists yet, creates
    $package_key/$package_key.info and adds this new file to apm_version_files
    in the database.

} {
    set spec [apm_generate_package_spec $version_id]
    apm_version_info $version_id
    db_1row package_version_info_select {
	select package_key, version_id
	from apm_package_version_info 
	where version_id = :version_id
    }

    ns_log Debug "APM: Checking existence of package directory."
    set root [acs_package_root_dir $package_key]
    if { ![file exists $root] } {
	file mkdir $root
# doesn't work under windows.  its not very useful anyway.
#	file attributes $root -permissions [ad_parameter "InfoFilePermissionsMode" "apm" 0755]
    }

    db_transaction {
	ns_log Debug "APM: Determining path of .info file."
	set info_file_name "$package_key.info"
	# Make sure we have a .info file set up in the data model.
	if { [db_0or1row package_spec_path_select {
            select path
            from apm_package_files
            where version_id = :version_id
            and file_type = 'package_spec'
	    and path = :info_file_name
        }] } {
	    # The .info file was already there. The path to is is now in $path.
	} else {
	    # Nothing there! We need to add a .info file.
	    set path "$package_key.info"
	    apm_file_add $version_id $path package_spec ""
	}
	ns_log Debug "APM: Writing APM .info file to the database."
	db_dml apm_spec_file_register {
	    update apm_package_types
		set spec_file_path = :path
	        where package_key = :package_key
	}
	ns_log Debug "APM: Writing .info file."
	set path "$root/$package_key.info"
	set file [open $path "w"]
	puts -nonewline $file $spec
	close $file

	# Mark $version_id as the only installed version of the package.
	db_dml version_mark_installed {
            update apm_package_versions
            set    installed_p = decode(version_id, :version_id, 't', 'f')
            where  package_key = :package_key
        }
    }
    ns_log Debug "APM: Done updating .info file."
}



proc_doc -public apm_version_enable { {-callback apm_dummy_callback} version_id } {

    Enables a version of a package (disabling any other version of the package).
    @param version_id The id of the version to be enabled.
} {
    db_exec_plsql apm_package_version_enable {
	begin
	  apm_package_version.enable(
            version_id => :version_id
	  );
	end;
    }
    apm_callback_and_log $callback  "<p>Package enabled."
}

proc_doc -public apm_version_disable { {-callback apm_dummy_callback} version_id } {

    Disables a version of a package.

    @param version_id The id of the version to be disabled.
} {
    db_exec_plsql apm_package_version_disable {
	begin
	  apm_package_version.disable(
            version_id => :version_id
	  );
	end;
    }
    apm_callback_and_log $callback  "<p>Package disabled."
}

ad_proc -public apm_package_register {
    {-spec_file_path ""} 
    {-spec_file_mtime ""}
    package_key 
    pretty_name 
    pretty_plural 
    package_uri 
    package_type 
    initial_install_p 
    singleton_p
} {
    Register the package in the system.
} {

    if { [empty_string_p $spec_file_path] } {
	set spec_file_path [db_null]
    } 

    if { [empty_string_p $spec_file_mtime] } {
	set spec_file_mtime [db_null]
    }

    if { ![string compare $package_type "apm_application"] } {
	db_exec_plsql application_register {}
    } elseif { ![string compare $package_type "apm_service"] } {
	db_exec_plsql service_register {}
    } else {
	error "Unrecognized package type: $package_type"
    }
}

ad_proc -public apm_version_update {
    {
	-callback apm_dummy_callback
    }
    version_id version_name version_uri summary description description_format vendor vendor_uri auto_mount {release_date ""} 
} {

    Update a version in the system to new information.
} {
    if { [empty_string_p $release_date] } {
 	set release_date [db_null]
    }

    return [db_exec_plsql apm_version_update {}]
}


ad_proc -private apm_packages_full_install {
    {
	-callback apm_dummy_callback
    } pkg_info_list 
} {

    Loads the data model, installs, enables, instantiates, and mounts all of the packages in pkg_list.
} {

    foreach pkg_info $pkg_info_list {
	if { [catch {
	    set spec_file [pkg_info_spec $pkg_info]
	    set package_key [pkg_info_key $pkg_info]
	    apm_package_install_data_model -callback $callback $spec_file
	    set version_id [apm_version_enable -callback $callback \
				[apm_package_install -callback $callback $spec_file]]
	} errmsg] } {
            global errorInfo
	    apm_callback_and_log -severity Error $callback "<p><font color=red>[string totitle $package_key] not installed.</font>
<p> Error:
<pre><blockquote>[ad_quotehtml $errmsg]</blockquote><blockquote>[ad_quotehtml $errorInfo]</blockquote></pre>"
	} 
    }
}

ad_proc -private apm_package_upgrade_p {package_key version_name} {
    @return 1 if a version of the indicated package_key of version lower than version_name \
	    is already installed in the system, 0 otherwise.
} {
    return [db_string apm_package_upgrade_p {
	select apm_package_version.version_name_greater(:version_name, version_name) upgrade_p
	from apm_package_versions
	where package_key = :package_key
	and version_id = apm_package.highest_version (:package_key)
    } -default 0]
}

ad_proc -private apm_package_upgrade_from { package_key version_name } {
    @param package_key The package you're installing
    @param version_name The version of the package you're installing
    @return the version of the package currently installed, which we're upgrading from, if it's 
    different from the version_name passed in. If this is not an upgrade, returns the empty string.
} {
    return [db_string apm_package_upgrade_from {
        select version_name 
        from   apm_package_versions
        where  package_key = :package_key
        and    version_id = apm_package.highest_version(:package_key)
        and    version_name != :version_name
    } -default ""]
}


ad_proc -private apm_version_upgrade {version_id} {

    Upgrade a package to a locally maintained later version.

} {
    db_exec_plsql apm_version_upgrade {
	begin
	    apm_package_version.upgrade(version_id => :version_id);
	end;

    }
} 

ad_proc -private apm_upgrade_for_version_p {path initial_version_name final_version_name} {

    @return 1 if the file indicated by path is valid .sql script to upgrade initial_version_name
    to final_version_name

} {
    ns_log Debug "upgrade_p $path, $initial_version_name $final_version_name"
    return [db_exec_plsql apm_upgrade_for_version_p {
	begin
	    :1 := apm_package_version.upgrade_p(
	              path => :path,
	              initial_version_name => :initial_version_name,
	              final_version_name => :final_version_name
	          );
	end;
    }]
}

ad_proc -private apm_order_upgrade_scripts {upgrade_script_names} {
    
    Upgrade scripts are ordered so that they may be executed in a sequence
    that upgrades package.  For example, if you start at version 1.0, and need to go
    to version 2.0, a correct order would be 1.0-1.5, 1.5-1.6, 1.6-2.0.
    @return an ordered list of upgrade script names.

} {
    return [lsort -increasing -command apm_upgrade_script_compare $upgrade_script_names]
}

ad_proc -private apm_upgrade_script_compare {f1 f2} {

    @return 1 if f1 comes after f2, 0 if they are the same, -1 if f1 comes before f2.

} {
    # Strip off any path information.
    set f1 [lindex [split $f1 /] end]
    set f2 [lindex [split $f2 /] end]
    
    # Get the version number from, e.g. the 2.0 from upgrade-2.0-3.0.sql 
    if {[regexp {\-(.*)-.*.sql} $f1 match f1_version_from] && 
    [regexp {\-(.*)-.*.sql} $f2 match f2_version_from]} {
	# At this point we should have something like 2.0 and 3.1d which Tcl string
	# comparison can handle. 
	set f1_version_from [db_exec_plsql test_f1 {}]
	set f2_version_from [db_exec_plsql test_f2 {}]
	return [string compare $f1_version_from $f2_version_from]
    } else {
	error "Invalid upgrade script syntax.  Should be \"upgrade-major.minor-major.minor.sql\"."
    }
}

ad_proc -private apm_data_model_scripts_find {
    {-upgrade_from_version_name ""}
    {-upgrade_to_version_name ""}
    package_key
} {
    @param version_id What version the files belong to.
    @param upgrade Set this switch if you want the scripts for upgrading.
    @file_list A list of files and file types of form [list [list "foo.sql" "data_model_upgrade"] ...] 
} {
    set types_to_retrieve [list "sqlj_code"]
    if {[empty_string_p $upgrade_from_version_name]} {
	lappend types_to_retrieve "data_model_create"
    } else {
	lappend types_to_retrieve "data_model_upgrade"
    }
    set data_model_list [list]
    set upgrade_file_list [list]
    set file_list [apm_get_package_files -file_types $types_to_retrieve -package_key $package_key]
    foreach path $file_list {
        set file_type [apm_guess_file_type $package_key $path]
        set file_db_type [apm_guess_db_type $package_key $path]
	apm_log APMDebug "APM: Checking \"$path\" of type \"$file_type\" and db_type \"$file_db_type\"."

        # DRB: we return datamodel files which match the given database type or for which no db_type
        # is defined.  The latter case allows for sharing of common SQL files in the package's
        # sql/common directory.

	if {[lsearch -exact $types_to_retrieve $file_type] != -1 && \
            ([db_compatible_rdbms_p $file_db_type])} {
	    if { ![string compare $file_type "data_model_upgrade"] } {
		if {[apm_upgrade_for_version_p $path $upgrade_from_version_name \
			$upgrade_to_version_name]} {
		    # Its a valid upgrade script.
		    ns_log Debug "APM: Adding $path to the list of upgrade files."
		    lappend upgrade_file_list [list $path $file_type $package_key]
		}
	    } else {
		apm_log APMDebug "APM: Adding $path to the list of data model files."
		lappend data_model_list [list $path $file_type $package_key ]
	    }
	}
    }
    set file_list [concat [apm_order_upgrade_scripts $upgrade_file_list] $data_model_list]
    apm_log APMDebug "APM: Data model scripts for $package_key: $file_list"
    return $file_list
}

ad_proc -private apm_query_files_find {
    package_key
    file_list
} {
    @file_list A list of files and file types of form [list [list "foo.sql" "data_model_upgrade"] ...] 
} {

    set query_file_list [list]

    foreach file $file_list {
	set path [lindex $file 0]
	set file_type [lindex $file 1]
        set file_db_type [lindex $file 2]
	ns_log Debug "APM/QD: Checking \"$path\" of type \"$file_type\" and db_type \"$file_db_type\"."

        # DRB: we return query files which match the given database type or for which no db_type
        # is defined, which we interpret to mean a file containing queries that work with all of our
        # supported databases.

	if {[lsearch -exact "query_file" $file_type] != -1 && \
            ([empty_string_p $file_db_type] || ![string compare [db_type] $file_db_type])} {
            ns_log Debug "APM: Adding $path to the list of query files."
            lappend query_file_list $path
	}
    }
    ns_log Notice "APM: Query files for $package_key: $query_file_list"
    return $query_file_list
}

ad_proc -private apm_mount_core_packages {} {
    <p>
    Mount, and set permissions for a number of packages
    part of the OpenACS core. The packages are singletons that have
    already been instantiated during installation. The main site
    needs to have been set up prior to invoking this proc.
    </p>

    <p>
      The reason mounting is done here and not via the auto-mount
      feature of the APM is that there is a circular dependency between
      acs-subsite and acs-content-repository. The package acs-subsite
      requires acs-content-repository and so we cannot install acs-subsite
      before acs-content-repository in order to be able to mount acs-content-repository.
    </p>

    @author Peter Marklund
} {
    ns_log Notice "Starting instantiation and mounting of core packages"

    # Mount acs-admin
    ns_log Notice "Mounting acs-admin"    
    site_node::instantiate_and_mount -package_key acs-admin

    # Mount acs-service-contract
    ns_log Notice "Mounting acs-service-contract"    
    site_node::instantiate_and_mount -package_key acs-service-contract

    # Mount the acs-content-repository
    ns_log Notice "Mounting acs-content-repository"    
    site_node::instantiate_and_mount -package_key acs-content-repository

    # Mount acs-core-docs
    ns_log Notice "Mounting acs-core-docs"    
    site_node::instantiate_and_mount -node_name doc \
                                     -package_key acs-core-docs

    # Mount the acs-api-browser
    ns_log Notice "Mounting acs-api-browser"    
    set api_browser_id \
        [site_node::instantiate_and_mount -node_name api-doc \
                                          -package_key acs-api-browser]
    # Only registered users should have permission to access the
    # api-browser
    permission::grant -party_id [acs_magic_object registered_users] \
                      -object_id $api_browser_id \
                      -privilege read
    permission::set_not_inherit -object_id $api_browser_id

    ns_log Notice "Core packages instantiated and mounted"
}

ad_proc -private apm_version_name_compare {
    version_name_1
    version_name_2
} {
    Compare two version names (e.g. '1.2d3' and '3.5b') as for which comes before which. The example here would return -1.
    @param version_name_1 the first version name
    @param version_name_2 the second version name
    @return 1 if version_name_1 comes after version_name_2, 0 if they are the same, -1 if version_name_1 comes before version_name_2.
    @author Lars Pind
} {
    db_1row select_sortable_versions {}
    return [string compare $sortable_version_1 $sortable_version_2]
}

ad_proc -public apm_version_names_compare {
    version_name_1
    version_name_2
} {
    Compare two version names (e.g. '1.2d3' and '3.5b') as for which comes before which. The example here would return -1.
    @param version_name_1 the first version name
    @param version_name_2 the second version name
    @return 1 if version_name_1 comes after version_name_2, 0 if they are the same, -1 if version_name_1 comes before version_name_2.

    @author Lars Pind
} {
    db_1row select_sortable_versions {}
    return [string compare $sortable_version_1 $sortable_version_2]
}

ad_proc -private apm_upgrade_logic_compare {
    from_to_key_1
    from_to_key_2
} {
    Compare the from-versions in two of apm_upgrade_logic's array entries on the form 'from_version_name,to_version_name'. 

    @param from_to_key the key from the array in apm_upgrade_logic
    @return 1 if 1 comes after 2, 0 if they are the same, -1 if 1 comes before 2.

    @author Lars Pind
} {
    return [apm_version_names_compare [lindex [split $from_to_key_1 ","] 0] [lindex [split $from_to_key_2 ","] 0]]
}

ad_proc -public apm_upgrade_logic { 
    {-from_version_name:required}
    {-to_version_name:required}
    {-spec:required}
} {
    Logic to help upgrade a package.
    The spec contains a list on the form \{ from_version to_version code_chunk from_version to_version code_chunk ... \}.
    The list is compared against the from_version_name and to_version_name parameters supplied, and the code_chunks that 
    fall within the from_version_name and to_version_name it'll get executed in the caller's namespace, ordered by the from_version.

    <p>
    
    Example:
    
    <blockquote><pre>

    ad_proc my_upgrade_callback {
        {-from_version_name:required}
        {-to_version_name:required}
    } {
        apm_upgrade_logic \ 
                -from_version_name $from_version_name \ 
                -to_version_name $to_version_name \ 
                -spec {
            1.1 1.2 {
                ...
            }
            1.2 1.3 {
                ...
            }
            1.4d 1.4d1 {
                ...
            }
            2.1 2.3 {
                ...
            }
            2.3 2.4 {
                ...
            }
        }
    }
    
    </pre></blockquote>
    
    @param from_version_name The version you're upgrading from, e.g. '1.3'.
    @param to_version_name The version you're upgrading to, e.g. '2.4'.
    @param spec The code chunks in the format described above

    @author Lars Pind
} {
    if { [expr [llength $spec] % 3] != 0 } {
        error "The length of spec should be dividable by 3"
    }
    
    array set chunks [list]
    foreach { elm_from elm_to elm_chunk } $spec {

        # Check that
        # from_version_name < elm_from < elm_to < to_version_name

        if { [apm_version_names_compare $from_version_name $elm_from] <= 0 && \
                 [apm_version_names_compare $elm_from $elm_to] <= 0 && \
                 [apm_version_names_compare $elm_to $to_version_name] <= 0 } {
            set chunks($elm_from,$elm_to) $elm_chunk
        }
    }

    foreach key [lsort -increasing -command apm_upgrade_logic_compare [array names chunks]] {
        uplevel $chunks($key)
    }
}


##############
#
# Deprecated Procedures
#
#############

ad_proc -private -deprecated -warn apm_package_instantiate_and_mount {
    {-callback apm_dummy_callback} 
    package_key
} { 
    Instantiate and mount a package of the indicated type. This proc
    has been deprecated and will be removed. Please change to using
    site_node::instantiate_and_mount instead.
 
    @see site_node::instantiate_and_mount
} {
    site_node::instantiate_and_mount -package_key $package_key
}