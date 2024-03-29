ad_library {

    Routines used by the package manager.

    @creation-date 13 Apr 2000
    @author Bryan Quinn (bquinn@arsdigita.com)
    @author Jon Salz (jsalz@arsdigita.com)
    @cvs-id $Id: apm-procs.tcl,v 1.23.2.5 2003/06/12 08:25:36 lars Exp $
}

#####
# Globals used by the package manager:
#
#     apm_current_package_key
#         Identifies which package is currently being loaded.
#
#
#
# NSV arrays used by the package_manager: (note that all paths are relative
# to [acs_path_root] unless otherwise indicated)
#
#     apm_version_properties($info_file_path)
#
#         Contains a list [list $mtime $properties_array], where $properties_array
#         is a cached copy of the version properties array last returned by
#         [apm_read_package_info_file $info_file_path], and $mtime is the
#         modification time of the $info_file_path when it was last examined.
#
#         This is a cache for apm_read_package_info_file.
#
#     apm_library_mtime($path)
#
#         The modification time of $file (a *-procs.tcl, *-init.tcl or .xql file)
#         when it was last loaded.
#
#     apm_version_procs_loaded_p($version_id)
#     apm_version_init_loaded_p($version_id)
#
#         1 if the *-procs.tcl and *-init.tcl files (respectively) have been
#         loaded for package version $version_id.
#
#     apm_vc_status($path)
#
#         A cached result from apm_fetch_cached_vc_status (of the form
#         [list $mtime $path]) containing the last-known CVS status of
#         $path.
#
#     apm_properties(reload_level)
#
#         The current "reload level" for the server.
#
#     apm_reload($reload_level)
#
#         A list of files which need to be loaded to bring the current interpreter
#         up to reload level $reload_level from level $reload_level - 1.
#
#     apm_reload_watch($path)
#
#         Indicates that $path is a -procs.tcl file which should be examined
#         every time apm_load_any_changed_libraries is invoked, to see whether
#         it has changed since last loaded. The path starts at acs_root_dir.
#
# RELOADING VOODOO
#
#     To allow for automatically reloading of Tcl libraries, we introduce the
#     concept of a server-wide "reload level" (starting at zero) stored in
#     the apm_properties(reload_level) NSV array entry. Whenever we determine
#     we want to have all interpreters source a particular -procs.tcl file,
#     we:
#
#         1) Increment apm_properties(reload_level), as a signal to each
#            interpreter that it needs to source some new -procs.tcl files
#            to bring itself up to date.
#         2) Set apm_reload($reload_level), where $reload_level is the new
#            value of apm_properties(reload_level) set in step #1, to the
#            list of files which actually need to be sourced.
#
#     Each interpreter maintains its private, interpreter-specific reload level
#     as a proc named apm_reload_level_in_this_interpreter. Every time the
#     request processor sees a request, it invokes
#     apm_load_any_changed_libraries, which compares the server-wide
#     reload level to the interpreter-private one. If it notes a difference,
#     it reloads the set of files necessary to bring itself up-to-date (i.e.,
#     files noted in the applicable entries of apm_reload).
#
#     Example:
#
#         - The server is started. apm_properties(reload_level) is 0.
#         - I modify /packages/acs-tcl/utilities-procs.tcl.
#         - Through the package manager GUI, I invoke
#           apm_mark_version_for_reload. It notices that utilities-procs.tcl
#           has changed. It increments apm_properties(reload_level) to 1,
#           and sets apm_reload(1) to [list "packages/acs-tcl/utilities-procs.tcl"].
#         - A request is handled in some other interpreter, whose reload
#           level (as returned by apm_reload_level_in_this_interpreter)
#           is 0. apm_load_any_changed_libraries notes that
#           [apm_reload_level_in_this_interpreter] != [nsv_get apm_properties reload_level],
#           so it sources the files listed in apm_reload(1) (i.e., utilities-procs.tcl)
#           and redefines apm_reload_level_in_this_interpreter to return 1.
#
#####


### Callback functions are used to control the logging that occurs during
### the execution of any apm_package that uses the -callback argument.

ad_proc -public apm_dummy_callback { string } {

    A dummy callback routine which does nothing.

} {
    # Do nothing!
}

ad_proc -public apm_ns_write_callback { string } {
 
    A simple callback which prints out the log message to the server stream.
   
} {
    ns_write $string
}

ad_proc -public apm_doc_body_callback { string } {
    This callback uses the document api to append more text to the stream.
} {
    doc_body_append $string
}

ad_proc apm_callback_and_log { { -severity Debug } callback message } {

    Executes the $callback callback routine with $message as an argument,
    and calls ns_log with the given $severity.

} {
    $callback $message
    ns_log $severity $message
}   


ad_proc -public apm_version_loaded_p { version_id } {

    Returns 1 if a version of a package has been loaded and initialized, or 0 otherwise.

} {
    return [nsv_exists apm_version_init_loaded_p $version_id]
}

ad_proc -private apm_mark_files_for_reload { 
    {-force_reload:boolean}
    file_list 
} {
    Mark the given list of Tcl and query files for reload in all
    interpreters. Only marks files for reload if they haven't been
    loaded before or they have changed since last reload.

    @param file_list A list of paths relative to acs_root_dir
    @param force_reload Mark the files for reload even if their modification
                        time in the nsv cache doesn't differ from the one
                        in the filesystem.

    @return The list of files marked for reload.

    @author Peter Marklund
} {
    set changed_files [list]
    foreach relative_path $file_list {
        set full_path "[acs_root_dir]/$relative_path"

	# If the file exists, and either has never been loaded or has an mtime
	# which differs the mtime it had when last loaded, mark to be loaded.
	if { [file isfile $full_path] } {
	    set mtime [file mtime $full_path]

	    if { $force_reload_p || (![nsv_exists apm_library_mtime $relative_path] || \
		    [nsv_get apm_library_mtime $relative_path] != $mtime) } {

		lappend changed_files $relative_path
	    }
	} 
    }

    if { [llength $changed_files] > 0 } {
	set reload [nsv_incr apm_properties reload_level]
	nsv_set apm_reload $reload $changed_files
    }

    return $changed_files
}

ad_proc -private apm_mark_version_for_reload { version_id { changed_files_var "" } } {

    Examines all tcl_procs files in package version $version_id; if any have
    changed since they were loaded, marks (in the apm_reload array) that
    they must be reloaded by each Tcl interpreter (using the
    apm_load_any_changed_libraries procedure).
    
    <p>Saves a list of files that have changed (and thus marked to be reloaded) in
    the variable named <code>$file_info_var</code>, if provided. Each element
    of this list is the path of a reloaded file, relative to the web server root 
    (e.g., packages/package-key/tcl/foo-procs.tcl)
} {
    if { ![empty_string_p $changed_files_var] } {
	upvar $changed_files_var changed_files
    }

    set package_key [apm_package_key_from_version_id $version_id]

    set changed_files [list]

    set file_types [list tcl_procs query_file]
    if { [apm_load_tests_p] } {
        lappend file_types test_procs
    }

    foreach path [apm_get_package_files -package_key $package_key -file_types $file_types] {

	set full_path "[acs_package_root_dir $package_key]/$path"
	set relative_path "packages/$package_key/$path"

        set reload_file [apm_mark_files_for_reload $relative_path]
        if { [llength $reload_file] > 0 } {
            # The file marked for reload
            lappend changed_files $relative_path
        }
    }
}

ad_proc -private apm_version_load_status { version_id } {

    If a version needs to be reloaded (i.e., a <code>-procs.tcl</code> has changed
    or been added since the version was loaded), returns "needs_reload".
    If the version has never been loaded, returns "never_loaded". If the
    version is up-to-date, returns "up_to_date".
    
} {
    # See if the version was ever loaded.
    if { ![apm_package_version_enabled_p $version_id] } {
	return "never_loaded"
    }

    set package_key [apm_package_key_from_version_id $version_id]
    set procs_types [list tcl_procs]
    if { [apm_load_tests_p] } {
        lappend procs_types test_procs
    }
    foreach file [apm_get_package_files -package_key $package_key -file_types $procs_types] {
	# If $file has never been loaded, i.e., it has been added to the version
	# since the version was initially loaded, return needs_reload.
	if { ![nsv_exists apm_library_mtime "packages/$package_key/$file"] } {
	    return "needs_reload"
	}

	set full_path "[acs_package_root_dir $package_key]/$file"
	# If $file had a different mtime when it was last loaded, return
	# needs_reload. (If the file should exist but doesn't, just skip it.)
	if { [file exists $full_path] && 
	[file mtime $full_path] != [nsv_get apm_library_mtime "packages/$package_key/$file"] } {
	    return "needs_reload"
	}
    }

    foreach file [apm_get_package_files -package_key $package_key -file_types "query_file"] {
	# If $file has never been loaded, i.e., it has been added to the version
	# since the version was initially loaded, return needs_reload.
	if { ![nsv_exists apm_library_mtime "packages/$package_key/$file"] } {
	    return "needs_reload"
	}

	set full_path "[acs_package_root_dir $package_key]/$file"
	# If $file had a different mtime when it was last loaded, return
	# needs_reload. (If the file should exist but doesn't, just skip it.)
	if { [file exists $full_path] && 
	[file mtime $full_path] != [nsv_get apm_library_mtime "packages/$package_key/$file"] } {
	    return "needs_reload"
	}
    }

    return "up_to_date"
}

ad_proc -private apm_load_libraries { 
    {-force_reload:boolean 0}
    {-packages {}}
    {-callback apm_dummy_callback}
    {-procs:boolean} 
    {-init:boolean}
    {-test_procs:boolean} 
    {-test_init:boolean}
} {

    Loads all -procs.tcl (if $procs_or_init is "procs") or -init.tcl  files into the 
    current interpreter for installed, enabled packages. Only loads
    files which have not yet been loaded. This is intended to be called only during server
    initialization (since it loads libraries only into the running interpreter, as opposed
    to in *all* active interpreters).

} {
    set file_types [list]
    if { $procs_p } {
        lappend file_types tcl_procs
    }
    if { $init_p } {
        lappend file_types tcl_init
    }
    if { $test_procs_p } {
        lappend file_types test_procs
    }
    if { $test_init_p } {
        lappend file_types test_init
    }
 
    if { [empty_string_p $packages] } {
        set packages [apm_enabled_packages]
    }

    # Scan the package directory for files to source.    
    set files [list]    
    foreach package $packages {

        set paths [apm_get_package_files -package_key $package -file_types $file_types]

	foreach path [lsort $paths] {
	    lappend files [list $package $path]
	}
    }
      
    # Release all outstanding database handles (since the file we're sourcing
    # might be using the ns_db database API as opposed to the new db_* API).
    db_release_unused_handles
    apm_files_load -force_reload=$force_reload_p -callback $callback $files
}

ad_proc -public apm_load_tests_p {} {
    Determine whether to load acs-automated-testing tests
    for packages.

    @return 1 if tests should be loaded and 0 otherwise

    @author Peter Marklund
} {
    return [apm_package_enabled_p "acs-automated-testing"]
}

ad_proc -public apm_load_packages {
    {-force_reload:boolean 0}
    {-load_libraries_p 1}
    {-load_queries_p 1}
    {-packages {}}
} {
    Load Tcl libraries and queries for the packages with given keys. Only
    loads procs into the current interpreter. Will
    load Tcl tests if the acs-automated-testing package is enabled.

    @param force_reload Reload Tcl libraries even if they are already loaded.
    @param load_libraries Switch to indicate if Tcl libraries in (-procs.tcl and -init.tcl)
                          files should be loaded. Defaults to true.
    @param load_queries   Switch to indicate if xql query files should be loaded. Default true.
    @param packages     A list of package_keys for packages to be loaded. Defaults to 
                        all enabled packages

    @see apm_mark_version_for_reload

    @author Peter Marklund
} {
    if { [empty_string_p $packages] } {
        set packages [apm_enabled_packages]
    }

    # Should acs-automated-testing tests be loaded?
    set load_tests_p [apm_load_tests_p]

    # Load *-procs.tcl files
    if { $load_libraries_p } {
        apm_load_libraries -force_reload=$force_reload_p -packages $packages -procs
    }
    
    # Load up the Queries (OpenACS, ben@mit.edu)
    if { $load_queries_p } {
        apm_load_queries -packages $packages
    }

    # Load up the Automated Tests and associated Queries if necessary
    if {$load_tests_p} {
      apm_load_libraries -force_reload=$force_reload_p -packages $packages -test_procs
      apm_load_queries -packages $packages -test_queries
    }

    if { $load_libraries_p } {
        apm_load_libraries -force_reload=$force_reload_p -init -packages $packages
    }

    # Load up the Automated Tests initialisation scripts if necessary
    if {$load_tests_p} {
      apm_load_libraries -force_reload=$force_reload_p -packages $packages -test_init
    }
}

ad_proc -private apm_load_queries {
    {-packages {}}
    {-callback apm_dummy_callback}
    {-test_queries:boolean}
} {
    Load up the queries for all enabled packages
    (or all specified packages). Follows the pattern 
    of the load_libraries proc, but only loads query information

    @param packages Optional list of keys for packages to load queries for.

    @author ben@mit.edu
} {
    if { [empty_string_p $packages] } {
        set packages [apm_enabled_packages]
    }

    # Scan the package directory for files to source.    
    set files [list]    
    foreach package $packages {

        set files [ad_find_all_files [acs_root_dir]/packages/$package]
        if { [llength $files] == 0 } {
    	    ns_log Error "apm_load_queries: Unable to locate [acs_root_dir]/packages/$package/*. when scanning for SQL queries to load."
        }

        set testdir    "[acs_root_dir]/packages/$package/tcl/test"
        set testlength [string length $testdir]

        foreach file [lsort $files] {

            set file_db_type [apm_guess_db_type $package $file]
            set file_type [apm_guess_file_type $package $file]

            if {![string compare -length $testlength $testdir $file]} {
              set is_test_file_p 1
            } else {
              set is_test_file_p 0
            }

            #
            # Note this exclusive or represents the following:
            # test_queries_p - Load normal xql files or load test xql files
            # is_test_file_p - Current file is a test file or not.
            #
            # !(test_queries_p ^ is_test_file_p)  = Load it or not?
            #             !( 0 ^ 0 )             = Yep
            #             !( 0 ^ 1 )             = Nope
            #             !( 1 ^ 0 )             = Nope
            #             !( 1 ^ 1 )             = Yep
            #
            if {![expr $test_queries_p ^ $is_test_file_p] &&
                [string equal $file_type query_file] &&
                ([empty_string_p $file_db_type] || [string equal $file_db_type [db_type]])} {
	        db_qd_load_query_file $file
            } 
        }
    }
    ns_log Notice "APM/QD = DONE looping through files from which to load queries"
}

ad_proc -private apm_subdirs { path } {

    Returns a list of subdirectories of path (including path itself)

} {
    set dirs [list]
    lappend dirs $path
    foreach subdir [glob -nocomplain -type d [file join $path *]] {
       set dirs [concat $dirs [apm_subdirs $subdir]]
    }
    return $dirs
}

ad_proc -private apm_pretty_name_for_file_type { type } {

    Returns the pretty name corresponding to a particular file type key
    (memoizing to save a database hit here and there).

} {
    return [util_memoize [list db_string pretty_name_select "
        select pretty_name
        from apm_package_file_types
        where file_type_key = :type
    " -default "Unknown" -bind [list type $type]]]
}

ad_proc -private apm_pretty_name_for_db_type { db_type } {

    Returns the pretty name corresponding to a particular file type key
    (memoizing to save a database hit here and there).

} {
    return [util_memoize [list db_string pretty_db_name_select "
        select pretty_db_name
        from apm_package_db_types
        where db_type_key = :db_type
    " -default "all" -bind [list db_type $db_type]]]
}

ad_proc -public apm_load_any_changed_libraries {} {
    
    In the running interpreter, reloads files marked for reload by
    apm_mark_version_for_reload. If any watches are set, examines watched
    files to see whether they need to be reloaded as well. This is intended
    to be called only by the request processor (since it should be invoked
    before any filters or registered procedures are applied).

} {
    # Determine the current reload level in this interpreter by calling
    # apm_reload_level_in_this_interpreter. If this fails, we define the reload level to be
    # zero.
    if { [catch { set reload_level [apm_reload_level_in_this_interpreter] } error] } {
	proc apm_reload_level_in_this_interpreter {} { return 0 }
	set reload_level 0
    }

    # Check watched files, adding them to files_to_reload if they have
    # changed.
    set files_to_reload [list]
    foreach file [nsv_array names apm_reload_watch] {
	set path "[acs_root_dir]/$file"
	ns_log Debug "APM: File being watched: $path"

	if { [file exists $path] && \
		(![nsv_exists apm_library_mtime $file] || \
		[file mtime $path] != [nsv_get apm_library_mtime $file]) } {
	    lappend files_to_reload $file
	}
    }

    # If there are any changed watched files, stick another entry on the
    # reload queue.
    if { [llength $files_to_reload] > 0 } {
	ns_log "Notice" "Watched file[ad_decode [llength $files_to_reload] 1 "" "s"] [join $files_to_reload ", "] [ad_decode [llength $files_to_reload] 1 "has" "have"] changed: reloading."
	set new_level [nsv_incr apm_properties reload_level]
	nsv_set apm_reload $new_level $files_to_reload
    }

    set changed_reload_level_p 0

    # Keep track of which files we've reloaded in this loop so we never
    # reload the same one twice.
    array set reloaded_files [list]
    while { $reload_level < [nsv_get apm_properties reload_level] } {
	incr reload_level
	set changed_reload_level_p 1
	# If there's no entry in apm_reload for that reload level, back out.
	if { ![nsv_exists apm_reload $reload_level] } {
	    incr reload_level -1
	    break
	}
	foreach file [nsv_get apm_reload $reload_level] {
	    # If we haven't yet reloaded the file in this loop, source it.
	    if { ![info exists reloaded_files($file)] } {
		if { [array size reloaded_files] == 0 } {
		    # Perform this ns_log only during the first iteration of this loop.
		    ns_log "Notice" "APM: Reloading *-procs.tcl files in this interpreter..."
		}
		ns_log "Notice" "APM: Reloading $file..."
		# File is usually of form packages/package_key
		set file_path "[acs_root_dir]/$file"
                switch [apm_guess_file_type "" $file] {
                    tcl_procs { apm_source [acs_root_dir]/$file }
                    query_file { db_qd_load_query_file [acs_root_dir]/$file }
                }

		set reloaded_files($file) 1
	    }
	}
    }

    # We changed the reload level in this interpreter, so redefine the
    # apm_reload_level_in_this_interpreter proc.
    if { $changed_reload_level_p } {
	proc apm_reload_level_in_this_interpreter {} "return $reload_level"
    }

}

ad_proc -private apm_package_version_release_tag { package_key version_name } {

    Returns a CVS release tag for a particular package key and version name.

2} {
    regsub -all {\.} [string toupper "$package_key-$version_name"] "-" release_tag
    return $release_tag
}

ad_proc -public apm_package_parameters {package_key} {
    @return A list of all the package parameter names.
} {
    return [db_list get_names {
	select parameter_name from apm_parameters
	where package_key = :package_key
    }]
}

ad_proc -public apm_package_registered_p {
    package_key
} {
    Returns 1 if there is a registered package with the indicated package_key.  
    Returns 0 otherwise.
} {
    ### Query the database for the indicated package_key
    return [db_string apm_package_registered_p {
	select 1 from apm_package_types 
	where package_key = :package_key
    } -default 0]
}

ad_proc -public apm_package_installed_p {
    package_key
} {
    Returns 1 if there is an installed package version corresponding to the package_key,
    0 otherwise. Uses a cached value for performance.
} {
    if { [util_memoize_initialized_p] } {
        return [util_memoize [list apm_package_installed_p_not_cached $package_key]]
    } else {
        return [apm_package_installed_p_not_cached $package_key]
    }
}

ad_proc -private apm_package_installed_p_not_cached {
    package_key
} {
    return [db_string apm_package_installed_p {} -default 0]    
}

ad_proc -public apm_package_enabled_p {
    package_key
} {
    Returns 1 if there is an enabled package version corresponding to the package_key
    and 0 otherwise.
} {
    return [db_string apm_package_enabled_p {} -default 0]
}

ad_proc -public apm_enabled_packages {} {
    Returns a list of package_key's for all enabled packages.

    @author Peter Marklund
} {
    return [db_list enabled_packages {}]
}


ad_proc -public apm_version_installed_p {
    version_id
} {
    @return Returns 1 if the specified version_id is installed, 0 otherwise.
} {
    return [db_string apm_version_installed_p {} -default 0]
}

ad_proc -public apm_highest_version {package_key} {
    Return the highest version of the indicated package.
    @return the version_id of the highest installed version of a package.
} {
    return [db_exec_plsql apm_highest_version {
	begin
	:1 := apm_package.highest_version (
                    package_key => :package_key
		    );
	end;
    }]
}

ad_proc -public apm_num_instances {package_key} {

    @return The number of instances of the indicated package.
} {
    return [db_exec_plsql apm_num_instances {
	begin
	:1 := apm_package.num_instances(
		package_key => :package_key
		);
	end;
    }]

}

ad_proc -public apm_parameter_update {
    {
	-callback apm_dummy_callback
    }
    parameter_id package_key parameter_name description default_value datatype \
	{section_name ""} {min_n_values 1} {max_n_values 1} 
} {
    @return The parameter id that has been updated.
} {
    if {[empty_string_p $section_name]} {
	set section_name [db_null]
    }

    db_dml parameter_update {
       update apm_parameters 
	set parameter_name = :parameter_name,
            default_value  = :default_value,
            datatype       = :datatype, 
	    description	   = :description,
	    section_name   = :section_name,
            min_n_values   = :min_n_values,
            max_n_values   = :max_n_values
      where parameter_id = :parameter_id
    }
    
    return $parameter_id
}

ad_proc -public apm_parameter_register { 
    {
	-callback apm_dummy_callback
	-parameter_id ""
    } 
    parameter_name description package_key default_value datatype {section_name ""} {min_n_values 1} {max_n_values 1}
} {
    Register a parameter in the system.
    @return The parameter id of the new parameter.

} {
    if {[empty_string_p $parameter_id]} {
	set parameter_id [db_null]
    }

    if {[empty_string_p $section_name]} {
	set section_name [db_null]
    }

    ns_log Notice "Registering $parameter_name, $section_name, $default_value"

    set parameter_id [db_exec_plsql parameter_register {
	    begin
	    :1 := apm.register_parameter(
					 parameter_id => :parameter_id,
					 parameter_name => :parameter_name,
					 package_key => :package_key,
					 description => :description,
					 datatype => :datatype,
					 default_value => :default_value,
					 section_name => :section_name,
					 min_n_values => :min_n_values,
					 max_n_values => :max_n_values
	                                );
	    end;
	}]

    # Update the cache.
    db_foreach apm_parameter_cache_update {
	select v.package_id, p.parameter_name, nvl(p.default_value, v.attr_value) as attr_value
	from apm_parameters p, apm_parameter_values v
	where p.package_key = :package_key
	and p.parameter_id = v.parameter_id (+)
    } {
	ad_parameter_cache -set $attr_value $package_id $parameter_name
    }
    return $parameter_id
}

ad_proc -public apm_parameter_unregister { 
    {
	-callback apm_dummy_callback
    } 
    parameter_id
} {
    Unregisters a parameter from the system.
} {
    ns_log Debug "APM Unregistering parameter $parameter_id."
    db_foreach all_parameters_packages {
	select package_id, parameter_id, parameter_name 
	from apm_packages p, apm_parameters ap
	where p.package_key = ap.package_key
	and ap.parameter_id = :parameter_id

    } {
	ad_parameter_cache -delete $package_id $parameter_name
    } if_no_rows {
	return
    }
	
    db_exec_plsql parameter_unregister {
	begin
	delete from apm_parameter_values 
	where parameter_id = :parameter_id;
	delete from apm_parameters 
	where parameter_id = :parameter_id;
	acs_object.delete(:parameter_id);
	end;
    }   
}

ad_proc -public apm_dependency_add {
    {
	-callback apm_dummy_callback
	-dependency_id ""
    } version_id dependency_uri dependency_version
} {
    
    Add a dependency to a version.
    @return The id of the new dependency.
} {

    if {[empty_string_p $dependency_id]} {
	set dependency_id [db_null]
    }
    
    return [db_exec_plsql dependency_add {
	begin
	:1 := apm_package_version.add_dependency(
            dependency_id => :dependency_id,
	    version_id => :version_id,
	    dependency_uri => :dependency_uri,
	    dependency_version => :dependency_version
        );					 
	end;
    }]
}

ad_proc -public apm_dependency_remove {dependency_id} {
    
    Removes a dependency from the system.

} {
    db_exec_plsql dependency_remove {
	begin
	apm_package_version.remove_dependency(
             dependency_id => :dependency_id
	);
	end;					        
    }
}

ad_proc -public apm_interface_add {
    {
	-callback apm_dummy_callback
	-interface_id ""
    } version_id interface_uri interface_version
} {
    
    Add a interface to a version.
    @return The id of the new interface.
} {

    if {[empty_string_p $interface_id]} {
	set interface_id [db_null]
    }
    
    return [db_exec_plsql interface_add {
	begin
	:1 := apm_package_version.add_interface(
            interface_id => :interface_id,
	    version_id => :version_id,
	    interface_uri => :interface_uri,
	    interface_version => :interface_version
        );					 
	end;
    }]
}

ad_proc -public apm_interface_remove {interface_id} {
    
    Removes a interface from the system.

} {
    db_exec_plsql interface_remove {
	begin
	apm_package_version.remove_interface(
             interface_id => :interface_id
	);
	end;					        
    }
}

#
# package_id -> package_key
#

ad_proc -public apm_package_key_from_id {package_id} {
    @return The package key of the instance.
} {
    return [util_memoize "apm_package_key_from_id_mem $package_id"]
}

proc apm_package_key_from_id_mem {package_id} {
    return [db_string apm_package_key_from_id {
	select package_key from apm_packages where package_id = :package_id
    } -default ""]
}

#
# package_id -> instance_name
#

ad_proc -public apm_instance_name_from_id {package_id} {
    @return The name of the instance.
} {
    return [util_memoize "apm_instance_name_from_id_mem $package_id"]
}

proc apm_instance_name_from_id_mem {package_id} {
    return [db_string apm_package_key_from_id {
	select instance_name from apm_packages where package_id = :package_id
    } -default ""]
}


#
# package_key -> package_id
#

ad_proc -public apm_package_id_from_key {package_key} {
    @return The package id of the instance of the package.
    0 if no instance exists, error if several instances exist.
} {
    return [util_memoize "apm_package_id_from_key_mem $package_key"]
}

proc apm_package_id_from_key_mem {package_key} {
    return [db_string apm_package_id_from_key {
	select package_id from apm_packages where package_key = :package_key
    } -default 0]
}

#
# package_id -> package_url
#

ad_proc -public apm_package_url_from_id {package_id} {
    @return The package url of the instance of the package.
    only valid for singleton packages.
} {
    return [util_memoize "apm_package_url_from_id_mem $package_id"]
}

ad_proc -public apm_package_url_from_id_mem {package_id} {
    return [db_string apm_package_url_from_id {
	select site_node.url(node_id) 
          from site_nodes 
         where object_id = :package_id
    } -default ""]
}

#
# package_key -> package_url
#

ad_proc -public apm_package_url_from_key {package_key} {
    @return The package url of the instance of the package.
    only valid for singleton packages.
} {
    return [util_memoize "apm_package_url_from_key_mem $package_key"]
}

ad_proc -public apm_package_url_from_key_mem {package_key} {
    set package_id [apm_package_id_from_key $package_key]
    if {!$package_id} { 
        return -code error -errorinfo "Package $package_key was not found.  May not be mounted."
    } 
    return [apm_package_url_from_id $package_id]
}

#
# package_key -> version_id 
#

ad_proc -public apm_version_id_from_package_key { package_key } {
    Return the id of the enabled version of the given package_key.
    If no such version id can be found, returns the empty string.

    @author Peter Marklund
} {
    return [db_string get_id {} -default ""]
}

#
# version_id -> package_key
#

ad_proc -public apm_package_key_from_version_id {version_id} {
    Returns the package_key for the given APM package version id. Goes to the database
    the first time called and then uses a cached value. Calls the proc apm_package_key_from_version_id_mem.    

    @author Peter Marklund (peter@collaboraid.biz)
} {
    return [util_memoize "apm_package_key_from_version_id_mem $version_id"]
    
}

ad_proc -private apm_package_key_from_version_id_mem {version_id} {
    Returns the package_key for the given APM package version id. Goes to the database
    everytime called.

    @author Peter Marklund (peter@collaboraid.biz)
} {
    return [db_string apm_package_id_from_key {
        select package_key from apm_package_version_info where version_id = :version_id
    } -default 0]
}

ad_proc -public apm_version_info {version_id} {

    Sets a set of common package information in the caller's environment.

} {

    uplevel 1 {
	db_1row apm_package_by_version_id {
	    select pretty_name, version_name, package_key, installed_p, distribution_uri, tagged_p
	    from apm_package_version_info where version_id = :version_id
	}
    } 
}

ad_proc -public apm_package_version_installed_p {package_key version_name} {

    @return 1 if the indiciated package version is installed, 0 otherwise.

} {
    return [db_string apm_package_version_installed_p {}]
}

ad_proc -public apm_package_version_enabled_p {version_id} {

    @return 1 if the indiciated package version is installed, 0 otherwise.

} {
    return [db_string apm_package_version_enabled_p {}]
}


ad_proc -private apm_post_instantiation_tcl_proc_from_key { package_key } {
    Generates the name of the TCL procedure we execute for
    post-instantiation. 

    @author Michael Bryzek (mbryzek@arsdigita.com)
    @creation-date 2001-03-05

    @return The name of a tcl procedure, if it exists, or empty string
    if no such tcl procedure was found.
} {
    set procedure_name [string tolower "[string trim $package_key]_post_instantiation"]
    # Change all "-" to "_" to mimic our tcl standards
    regsub -all {\-} $procedure_name "_" procedure_name
    if { [empty_string_p [info procs ::$procedure_name]] } {
	# No such procedure exists... 
	return ""
    }
    # Procedure exists
    return $procedure_name
}


ad_proc -public -deprecated -warn apm_package_create_instance {
    {-package_id 0}
    instance_name 
    context_id 
    package_key
} {
    Creates a new instance of a package. Deprecated - please use
    apm_package_instance_new instead.

    @see apm_package_instance_new
} {    
    return [apm_package_instance_new -package_id $package_id \
                                     $instance_name \
                                     $context_id \
                                     $package_key]
}

ad_proc -public apm_set_callback_proc {
    {-version_id ""}
    {-package_key ""}
    {-type:required}
    proc
} {
    Set the name of an APM Tcl procedure callback for a certain package version.
    Checks if the callback already exists and updates if it does.
    If version_id is not supplied the id of the currently enabled version
    of the package will be used.

    @see apm_supported_callback_types

    @author Peter Marklund
} {
    apm_assert_callback_type_supported $type

    if { [empty_string_p $version_id] } {
        if { [empty_string_p $package_key] } {
            error "apm_set_package_callback_proc: Invoked with both version_id and package_key empty. You must supply either of these"
        }
        
        set version_id [apm_version_id_from_package_key $package_key]
    }

    set current_proc [apm_get_callback_proc -type $type -version_id $version_id]

    if { [empty_string_p $current_proc] } {
        # We are adding
        db_dml insert_proc {}
    } else {
        # We are editing
        db_dml update_proc {}
    }    
}

ad_proc -public apm_get_callback_proc {
    {-type:required}
    {-package_key ""}
    {-version_id ""}
} {
    Return Tcl procedure name for the callback of a certain
    type for the given package. If no callback proc for the
    given type is present returns the empty string.

    @see apm_supported_callback_types

    @author Peter Marklund
} {
    apm_assert_callback_type_supported $type

    if { [empty_string_p $version_id] } {
        set version_id [apm_version_id_from_package_key $package_key]
    }

    return [db_string select_proc {} -default ""]
}

ad_proc -public apm_remove_callback_proc {
    {-type:required}
    {-package_key:required}    
} {
    Remove the callback of a certain type for the given package.

    @author Peter Marklund
} {
    apm_assert_callback_type_supported $type

    return [db_dml delete_proc {}]
}

ad_proc -public apm_unused_callback_types {
    {-version_id:required}
} {
    Get a list enumerating the supported callback types
    that are not used by the given package version.
} {
    set used_callback_types [db_list used_callback_types {
        select distinct type
        from apm_package_callbacks
        where version_id = :version_id
    }]

    set supported_types [apm_supported_callback_types]

    set unused_types [list]
    foreach supported_type $supported_types {
        if { [lsearch -exact $used_callback_types $supported_type] < 0 } {
            lappend unused_types $supported_type
        }
    }

    return $unused_types
}

ad_proc -public apm_invoke_callback_proc {
    {-version_id ""}
    {-package_key ""}
    {-arg_list {}}
    {-type:required}
} {
    Invoke the Tcl callback proc of a given type 
    for a given package version. Any errors during
    invocation are logged.

    @return 1 if invocation
    was carried out successfully, 0 if no proc to invoke could
    be found. Will propagate any error thrown by the callback.

    @author Peter Marklund
} {
    array set arg_array $arg_list

    set proc_name [apm_get_callback_proc \
                       -version_id $version_id \
                       -package_key $package_key \
                       -type $type]
    
    if { [empty_string_p $proc_name] } {
        if { [string equal $type "after-instantiate"] } {
            # We check for the old proc on format: package_key_post_instantiation package_id
            if { [empty_string_p $package_key] } {
                set package_key [apm_package_key_from_version_id $version_id]
            }
            set proc_name [apm_post_instantiation_tcl_proc_from_key $package_key]
            if { [empty_string_p $proc_name] } {
                # No callback and no old-style callback proc - no options left
                return 0
            }

            $proc_name $arg_array(package_id)

            return 1
            
        } else {
            # No other callback procs to fall back on
            return 0
        }
    }

    # We have a non-empty name of a callback proc to invoke
    # Form the full command including arguments
    set command "${proc_name} [apm_callback_format_args -type $type -arg_list $arg_list]"

    # We are ready for invocation
    ns_log Notice "Invoking callback $type with command $command"
    eval $command

    return 1
}

ad_proc -public apm_assert_callback_type_supported { type } {
    Throw an error if the given callback type is not supported.

    @author Peter Marklund
} {
    if { ![apm_callback_type_supported_p $type]  } {
        error "The supplied callback type $type is not supported. Supported types are: [apm_supported_callback_types]"
    }
}

ad_proc -public apm_callback_type_supported_p { type } {
    Return 1 if the given type of callback is supported and 0
    otherwise.

    @author Peter Marklund
} {
    return [expr [lsearch -exact [apm_supported_callback_types] $type] >= 0]
}

ad_proc -public apm_callback_format_args {
    {-version_id ""}
    {-package_key ""}
    {-type:required}
    {-arg_list {}}
} {
    Return a string on format -arg_name1 arg_value1 -arg_name2 arg_value2 ...
    for the callback proc of given type.

    @author Peter Marklund
} {
    array set args_array $arg_list

    set arg_string ""
    set provided_arg_names [array names args_array]
    foreach required_arg_name [apm_arg_names_for_callback_type -type $type] {
        if { [lsearch -exact $provided_arg_names $required_arg_name] < 0 } {
            error "required argument $required_arg_name not supplied to callback proc of type $type"
        }

        append arg_string " -${required_arg_name} $args_array($required_arg_name)"
    }

    return $arg_string
}

ad_proc -public apm_arg_names_for_callback_type {
    {-type:required}
} {
    Return the list of required argument names for the given callback type.

    @author Peter Marklund
} {
    array set arguments {
        after-instantiate { 
            package_id 
        }
        before-uninstantiate {
            package_id
        }
        before-unmount {
            package_id 
            node_id
        }
        after-mount {
            package_id
            node_id
        }
        before-upgrade {
            from_version_name
            to_version_name
        }
        after-upgrade {
            from_version_name 
            to_version_name
        }
    }

    if { [info exists arguments($type)] } {
        return $arguments($type)
    } else {
        return {}
    }
}

ad_proc -public apm_supported_callback_types {} {
    Gets the list of package callback types
    that are supported by the system.
    Each callback type represents a certain event or time
    when a Tcl procedure should be invoked, such as after-install

    @author Peter Marklund
} {
    return {
        before-install
        after-install 
        before-upgrade
        after-upgrade
        before-uninstall
        after-instantiate
        before-uninstantiate
        after-mount
        before-unmount
    }
}

ad_proc -private apm_callback_has_valid_args {
    {-type:required}
    {-proc_name:required}
} {
    Returns 1 if the specified callback proc of a certain
    type has a valid argument list in its definition and 0
    otherwise. Assumes that the callback proc is defined with
    ad_proc.

    @author Peter Marklund
} {

    if { [empty_string_p [info procs ::${proc_name}]] } {
        return 0
    }

    set test_arg_list ""
    foreach arg_name [apm_arg_names_for_callback_type -type $type] {
        append test_arg_list " -${arg_name} value"
    }

    if { [empty_string_p $test_arg_list] } {
        # The callback proc should take no args
        return [empty_string_p [info args ::${proc_name}]]
    }

    # The callback proc should have required arg switches. Check
    # that the ad_proc arg parser doesn't throw an error with
    # test arg list
    if { [catch { 
           set args $test_arg_list
           ::${proc_name}__arg_parser 
       } errmsg] } {
        return 0
    } else {
        return 1
    }
}

ad_proc -public apm_package_instance_new {
    {-package_id 0}
    instance_name 
    context_id 
    package_key
} {

    Creates a new instance of a package and call the post instantiation proc, if any.

    @param instance_name The name of the package instance, defaults to the pretty name of the
                         package type.

    @return The id of the instantiated package
} {
    if { [empty_string_p $instance_name] } {
        set instance_name [db_string pretty_name_from_key {select pretty_name 
                                                          from apm_enabled_package_versions 
                                                          where package_key = :package_key}]
    }

    if {$package_id == 0} {
	set package_id [db_null]
    } 

    set package_id [db_exec_plsql invoke_new {}]
   
    apm_parameter_sync $package_key $package_id

    apm_invoke_callback_proc -package_key $package_key -type "after-instantiate" -arg_list [list package_id $package_id]

    return $package_id
}

ad_proc apm_parameter_sync {package_key package_id} {
    
    Syncs the parameters in the database with the memory cache.  This must be called
    after creating a new package instance.
    
} {

    # Get all the parameter names and values for this package_id.
    set names_and_values [db_list_of_lists apm_parameter_names_and_values {
	select parameter_name, attr_value
	from apm_parameters p, apm_parameter_values v, apm_packages a
	where p.parameter_id = v.parameter_id
	and a.package_id = v.package_id
	and a.package_id = :package_id
    }]
    
    # Put it in the cache.
    foreach name_value_pair $names_and_values {	
	ad_parameter_cache -set [lindex $name_value_pair 1] $package_id [lindex $name_value_pair 0]
    }
}

ad_proc -public apm_package_instance_delete {
    package_id
} {
    Deletes an instance of a package
} {    
    apm_invoke_callback_proc -package_key [apm_package_key_from_id $package_id] \
                            -type before-uninstantiate \
                            -arg_list [list package_id $package_id]

    db_exec_plsql apm_package_instance_delete {}
}

##
## Logging
##

ad_proc -public apm_log {
    level
    msg
} {
    Centralized APM logging. If you want to debug the APM, change
    APMDebug to Debug and restart the server.  
} {
    if {![string equal "APMDebug" $level]} {
        ns_log $level "$msg"
    }
}
