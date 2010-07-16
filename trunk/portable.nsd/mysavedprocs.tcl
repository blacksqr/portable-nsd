# tcl procs saved on Wed Mar 19 2:34:36 PM Eastern Standard Time 2003
#package require Tclx

proc apm_mark_version_for_reload {version_id {file_info_var {}}} {
    if { ![empty_string_p $file_info_var] } {
	upvar $file_info_var file_info
    }

    db_1row package_key_select "select package_key from apm_package_version_info where version_id = :version_id"

    set changed_files [list]
    set file_info [list]

    db_foreach file_info {
        select file_id, path
        from   apm_package_files
        where  version_id = :version_id
        and    file_type in ('tcl_procs', 'query_file')
        and    (db_type is null or db_type = '[db_type]')
        order by path
    } {
	set full_path "[acs_package_root_dir $package_key]/$path"
	set relative_path "packages/$package_key/$path"

	# If the file exists, and either has never been loaded or has an mtime
	# which differs the mtime it had when last loaded, mark to be loaded.
	if { [file isfile $full_path] } {
	    set mtime [file mtime $full_path]

	    if { ![nsv_exists apm_library_mtime $relative_path] ||  [nsv_get apm_library_mtime $relative_path] != $mtime } {
		lappend changed_files $relative_path
		lappend file_info [list $file_id $path $relative_path]
		nsv_set apm_library_mtime $relative_path $mtime
	    }
	}
    }

    if { [llength $changed_files] > 0 } {
	set reload [nsv_incr apm_properties reload_level]
	nsv_set apm_reload $reload $changed_files
    }
}
proc ad_page_contract__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -type {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -type"
                }
                upvar type val ; set val [lindex $args [incr i]]
            }
            -properties {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -properties"
                }
                upvar properties val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { docstring } $n_args_remaining]"
    }
    upvar docstring val ; set val [lindex $args [expr { $i + 0 }]]
    set args [lrange $args [expr { $i + 1 }] end]
}
proc apm_parameter_register__arg_parser {} {    upvar args args
    upvar min_n_values val ; set val 1
    upvar section_name val ; set val {}
    upvar parameter_id val ; set val {}
    upvar callback val ; set val apm_dummy_callback
    upvar max_n_values val ; set val 1

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }
            -parameter_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -parameter_id"
                }
                upvar parameter_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 5 } {
        return -code error "No value specified for argument [lindex { parameter_name description package_key default_value datatype } $n_args_remaining]"
    }
    upvar parameter_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar description val ; set val [lindex $args [expr { $i + 1 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 2 }]]
    upvar default_value val ; set val [lindex $args [expr { $i + 3 }]]
    upvar datatype val ; set val [lindex $args [expr { $i + 4 }]]
    if { $n_args_remaining > 5 } {
        upvar section_name val ; set val [lindex $args [expr { $i + 5 }]]
    }
    if { $n_args_remaining > 6 } {
        upvar min_n_values val ; set val [lindex $args [expr { $i + 6 }]]
    }
    if { $n_args_remaining > 7 } {
        upvar max_n_values val ; set val [lindex $args [expr { $i + 7 }]]
    }
    if { $n_args_remaining > 8 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc site_node_mount_application args {    site_node_mount_application__arg_parser


    # First create the new node beneath parent_node_id
    set node_id [db_exec_plsql create_node {
	begin
	  :1 := site_node.new (
                    parent_id => :parent_node_id,
                    name => :instance_name,
                    directory_p => 't',
                    pattern_p => 't'
	  );
	end;
    }]

    # If there is an object mounted at the parent_node_id
    # then use that object_id, instead of the parent_node_id,
    # as the context_id
    if { ![db_0or1row get_context {
        select object_id as context_id
          from site_nodes 
         where node_id = :parent_node_id
    }] } {
	set context_id $parent_node_id
    }    

    set package_id [site_node_create_package_instance -sync_p $sync_p $node_id $package_name $context_id $package_key]

    if { [string eq $return "package_id"] } {
	return $package_id
    } elseif { [string eq $return "node_id"] } {
	return $node_id
    } elseif { [string eq $return "package_id,node_id"] } {
	return [list $package_id $node_id]
    }

    error "Unknown return key: $return. Must be either package_id, node_id"
}
proc ad_acs_release_date {} {
    set release_tag {}
    regexp "R(\[0-9\]+)" $release_tag match release_date

    if {[info exists release_date]} {
      set year  [string range $release_date 0 3]
      set month [string range $release_date 4 5]
      set day   [string range $release_date 6 7]
      return [util_AnsiDatetoPrettyDate "$year-$month-$day"]
    } else {
      return "not released"
    }
}
proc ad_var_type_check_safefilename_p value {

    if [string match *..* $value] {
        return 0
    } else {
        return 1
    }
}
proc ns_info value { 


    switch $value {
	"tcllib"  { return "$pnsd::root/tcl/" }
	"server"  { return "openacs" }
	"log"     { return "log.txt" }
	"pageroot" { return "$pnsd::root/www/" }
	"platform" { return "win32" }
	"hostname" { return "localhost" }
    }
    ns_log warning "ns_info missed $value\n"
}
proc db_get_pgbin {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    set driver [ns_config ns/db/pool/$pool Driver]    
    return [ns_config ns/db/driver/$driver pgbin]
}
proc db_null {} {
    return ""
}
proc rp_debug args {    rp_debug__arg_parser

    if { [util_memoize {ad_parameter -package_id [ad_acs_kernel_id] DebugP request-processor 0} 60] } {
	global ad_conn
	set clicks [clock clicks]
        ad_call_proc_if_exists ds_add rp [list debug $string $clicks $clicks]
    }
    if {
        [util_memoize {ad_parameter -package_id [ad_acs_kernel_id] LogDebugP request-processor 0} 60] || [string equal $debug t] || [string equal $debug 1]
    } {
	global ad_conn
	if { [info exists ad_conn(start_clicks)] } {
           set timing " ([expr {([clock clicks] - $ad_conn(start_clicks))/1000.0}] ms)"
	} else {
           set timing ""
	}
       ns_log $ns_log_level "RP$timing: $string"
    }
}
proc ad_script_abort {} {
  ad_raise ad_script_abort
}
proc pkg_mkIndex args {
    global errorCode errorInfo
    set usage {"pkg_mkIndex ?-direct? ?-lazy? ?-load pattern? ?-verbose? ?--? dir ?pattern ...?"};

    set argCount [llength $args]
    if {$argCount < 1} {
	return -code error "wrong # args: should be\n$usage"
    }

    set more ""
    set direct 1
    set doVerbose 0
    set loadPat ""
    for {set idx 0} {$idx < $argCount} {incr idx} {
	set flag [lindex $args $idx]
	switch -glob -- $flag {
	    -- {
		# done with the flags
		incr idx
		break
	    }
	    -verbose {
		set doVerbose 1
	    }
	    -lazy {
		set direct 0
		append more " -lazy"
	    }
	    -direct {
		append more " -direct"
	    }
	    -load {
		incr idx
		set loadPat [lindex $args $idx]
		append more " -load $loadPat"
	    }
	    -* {
		return -code error "unknown flag $flag: should be\n$usage"
	    }
	    default {
		# done with the flags
		break
	    }
	}
    }

    set dir [lindex $args $idx]
    set patternList [lrange $args [expr {$idx + 1}] end]
    if {[llength $patternList] == 0} {
	set patternList [list "*.tcl" "*[info sharedlibextension]"]
    }

    set oldDir [pwd]
    cd $dir

    if {[catch {eval glob $patternList} fileList]} {
	global errorCode errorInfo
	cd $oldDir
	return -code error -errorcode $errorCode -errorinfo $errorInfo $fileList
    }
    foreach file $fileList {
	# For each file, figure out what commands and packages it provides.
	# To do this, create a child interpreter, load the file into the
	# interpreter, and get a list of the new commands and packages
	# that are defined.

	if {[string equal $file "pkgIndex.tcl"]} {
	    continue
	}

	# Changed back to the original directory before initializing the
	# slave in case TCL_LIBRARY is a relative path (e.g. in the test
	# suite). 

	cd $oldDir
	set c [interp create]

	# Load into the child any packages currently loaded in the parent
	# interpreter that match the -load pattern.

	if {[string length $loadPat]} {
	    if {$doVerbose} {
		tclLog "currently loaded packages: '[info loaded]'"
		tclLog "trying to load all packages matching $loadPat"
	    }
	    if {![llength [info loaded]]} {
		tclLog "warning: no packages are currently loaded, nothing"
		tclLog "can possibly match '$loadPat'"
	    }
	}
	foreach pkg [info loaded] {
	    if {! [string match $loadPat [lindex $pkg 1]]} {
		continue
	    }
	    if {$doVerbose} {
		tclLog "package [lindex $pkg 1] matches '$loadPat'"
	    }
	    if {[catch {
		load [lindex $pkg 0] [lindex $pkg 1] $c
	    } err]} {
		if {$doVerbose} {
		    tclLog "warning: load [lindex $pkg 0] [lindex $pkg 1]\nfailed with: $err"
		}
	    } elseif {$doVerbose} {
		tclLog "loaded [lindex $pkg 0] [lindex $pkg 1]"
	    }
	    if {[string equal [lindex $pkg 1] "Tk"]} {
		# Withdraw . if Tk was loaded, to avoid showing a window.
		$c eval [list wm withdraw .]
	    }
	}
	cd $dir

	$c eval {
	    # Stub out the package command so packages can
	    # require other packages.

	    rename package __package_orig
	    proc package {what args} {
		switch -- $what {
		    require { return ; # ignore transitive requires }
		    default { eval __package_orig {$what} $args }
		}
	    }
	    proc tclPkgUnknown args {}
	    package unknown tclPkgUnknown

	    # Stub out the unknown command so package can call
	    # into each other during their initialilzation.

	    proc unknown {args} {}

	    # Stub out the auto_import mechanism

	    proc auto_import {args} {}

	    # reserve the ::tcl namespace for support procs
	    # and temporary variables.  This might make it awkward
	    # to generate a pkgIndex.tcl file for the ::tcl namespace.

	    namespace eval ::tcl {
		variable file		;# Current file being processed
		variable direct		;# -direct flag value
		variable x		;# Loop variable
		variable debug		;# For debugging
		variable type		;# "load" or "source", for -direct
		variable namespaces	;# Existing namespaces (e.g., ::tcl)
		variable packages	;# Existing packages (e.g., Tcl)
		variable origCmds	;# Existing commands
		variable newCmds	;# Newly created commands
		variable newPkgs {}	;# Newly created packages
	    }
	}

	$c eval [list set ::tcl::file $file]
	$c eval [list set ::tcl::direct $direct]

	# Download needed procedures into the slave because we've
	# just deleted the unknown procedure.  This doesn't handle
	# procedures with default arguments.

	foreach p {pkg_compareExtension} {
	    $c eval [list proc $p [info args $p] [info body $p]]
	}

	if {[catch {
	    $c eval {
		set ::tcl::debug "loading or sourcing"

		# we need to track command defined by each package even in
		# the -direct case, because they are needed internally by
		# the "partial pkgIndex.tcl" step above.

		proc ::tcl::GetAllNamespaces {{root ::}} {
		    set list $root
		    foreach ns [namespace children $root] {
			eval lappend list [::tcl::GetAllNamespaces $ns]
		    }
		    return $list
		}

		# init the list of existing namespaces, packages, commands

		foreach ::tcl::x [::tcl::GetAllNamespaces] {
		    set ::tcl::namespaces($::tcl::x) 1
		}
		foreach ::tcl::x [package names] {
		    set ::tcl::packages($::tcl::x) 1
		}
		set ::tcl::origCmds [info commands]

		# Try to load the file if it has the shared library
		# extension, otherwise source it.  It's important not to
		# try to load files that aren't shared libraries, because
		# on some systems (like SunOS) the loader will abort the
		# whole application when it gets an error.

		if {[pkg_compareExtension $::tcl::file [info sharedlibextension]]} {
		    # The "file join ." command below is necessary.
		    # Without it, if the file name has no \'s and we're
		    # on UNIX, the load command will invoke the
		    # LD_LIBRARY_PATH search mechanism, which could cause
		    # the wrong file to be used.

		    set ::tcl::debug loading
		    load [file join . $::tcl::file]
		    set ::tcl::type load
		} else {
		    set ::tcl::debug sourcing
		    source $::tcl::file
		    set ::tcl::type source
		}

		# As a performance optimization, if we are creating 
		# direct load packages, don't bother figuring out the 
		# set of commands created by the new packages.  We 
		# only need that list for setting up the autoloading 
		# used in the non-direct case.
		if { !$::tcl::direct } {
		    # See what new namespaces appeared, and import commands
		    # from them.  Only exported commands go into the index.
		    
		    foreach ::tcl::x [::tcl::GetAllNamespaces] {
			if {! [info exists ::tcl::namespaces($::tcl::x)]} {
			    namespace import -force ${::tcl::x}::*
			}

			# Figure out what commands appeared
			
			foreach ::tcl::x [info commands] {
			    set ::tcl::newCmds($::tcl::x) 1
			}
			foreach ::tcl::x $::tcl::origCmds {
			    catch {unset ::tcl::newCmds($::tcl::x)}
			}
			foreach ::tcl::x [array names ::tcl::newCmds] {
			    # determine which namespace a command comes from
			    
			    set ::tcl::abs [namespace origin $::tcl::x]
			    
			    # special case so that global names have no leading
			    # ::, this is required by the unknown command
			    
			    set ::tcl::abs  [lindex [auto_qualify $::tcl::abs ::] 0]
			    
			    if {[string compare $::tcl::x $::tcl::abs]} {
				# Name changed during qualification
				
				set ::tcl::newCmds($::tcl::abs) 1
				unset ::tcl::newCmds($::tcl::x)
			    }
			}
		    }
		}

		# Look through the packages that appeared, and if there is
		# a version provided, then record it

		foreach ::tcl::x [package names] {
		    if {[string compare [package provide $::tcl::x] ""]  && ![info exists ::tcl::packages($::tcl::x)]} {
			lappend ::tcl::newPkgs  [list $::tcl::x [package provide $::tcl::x]]
		    }
		}
	    }
	} msg] == 1} {
	    set what [$c eval set ::tcl::debug]
	    if {$doVerbose} {
		tclLog "warning: error while $what $file: $msg"
	    }
	} else {
	    set what [$c eval set ::tcl::debug]
	    if {$doVerbose} {
		tclLog "successful $what of $file"
	    }
	    set type [$c eval set ::tcl::type]
	    set cmds [lsort [$c eval array names ::tcl::newCmds]]
	    set pkgs [$c eval set ::tcl::newPkgs]
	    if {$doVerbose} {
		tclLog "commands provided were $cmds"
		tclLog "packages provided were $pkgs"
	    }
	    if {[llength $pkgs] > 1} {
		tclLog "warning: \"$file\" provides more than one package ($pkgs)"
	    }
	    foreach pkg $pkgs {
		# cmds is empty/not used in the direct case
		lappend files($pkg) [list $file $type $cmds]
	    }

	    if {$doVerbose} {
		tclLog "processed $file"
	    }
	    interp delete $c
	}
    }

    append index "# Tcl package index file, version 1.1\n"
    append index "# This file is generated by the \"pkg_mkIndex$more\" command\n"
    append index "# and sourced either when an application starts up or\n"
    append index "# by a \"package unknown\" script.  It invokes the\n"
    append index "# \"package ifneeded\" command to set up package-related\n"
    append index "# information so that packages will be loaded automatically\n"
    append index "# in response to \"package require\" commands.  When this\n"
    append index "# script is sourced, the variable \$dir must contain the\n"
    append index "# full path name of this file's directory.\n"

    foreach pkg [lsort [array names files]] {
	set cmd {}
	foreach {name version} $pkg {
	    break
	}
	lappend cmd ::pkg::create -name $name -version $version
	foreach spec $files($pkg) {
	    foreach {file type procs} $spec {
		if { $direct } {
		    set procs {}
		}
		lappend cmd "-$type" [list $file $procs]
	    }
	}
	append index "\n[eval $cmd]"
    }

    set f [open pkgIndex.tcl w]
    puts $f $index
    close $f
    cd $oldDir
}
proc db_dml {statement_name pre_sql args} {
#        ns_log info "db_dml called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_dml $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_dml $statement_name $sql] $args]
    }
proc ad_return_complaint {exception_count exception_text} {
    # there was an error in the user input 
    if { $exception_count == 1 } {
	set problem_string "a problem"
	set please_correct "it"
    } else {
	set problem_string "some problems"
	set please_correct "them"
    }
	    
    doc_return 200 text/html "[ad_header_with_extra_stuff "Problem with Your Input" "" ""]
    
<h2>Problem with Your Input</h2>

to <a href=/>[ad_system_name]</a>

<hr>

We had $problem_string processing your entry:
	
<ul> 
	
$exception_text
	
</ul>
	
Please back up using your browser, correct $please_correct, and
resubmit your entry.
	
<p>
	
Thank you.
	
[ad_footer]
";					#"emacs
    # raise abortion flag, e.g., for templating
    global request_aborted
    set request_aborted [list 200 "Problem with Your Input"]
}
proc db_bootstrap_set_db_type errors {

    # Might as well get el grosso hacko out of the way...
    upvar $errors database_problem

    # DRB: I've reorganized this a bit from the standard ACS 4.1.  In their
    # version, they'd allocate a handle from the server default pool, check
    # for a recent Oracle driver and assume everything was A-OK if the check
    # succeeded.
    #
    # There are some problems with this approach:
    #
    # 1. The "AvailablePool" parameter specifies the pools to be used by the ACS. 
    #    The default pool needn't be listed as an available pool, therefore in a
    #    mixed db environment the check strategy described above might not actually
    #    be checking any pool designated for ACS use.
    #
    #    In fact, if it weren't for the bootstrap check code there would be no
    #    need at all to configure a default database pool!
    #
    #    The standard ACS check was fine as far as it went in the normal case
    #    where no AvailablePool parameters exist, as in that case the ACS
    #    slurps up all pools.  I expect mixed db environments to be more common
    #    within the OpenACS community, though, so we should do a better job of
    #    checking.  This will especially be true of users migrating from an
    #    [Open]ACS 3.x site or ACS 4.x classic site.
    #
    # 2. There was no checking to make sure that *all* pools are correctly
    #    configured.  Even in an Oracle-only environment one could easy mistype a
    #    user name or the like for one of the pools set aside for ACS use, and
    #    this would not be cleanly caught and reported.
    #
    # 3. There was no checking to make sure that *all* pools are of the same RDBMS
    #    type.  This is important in a mixed-db environment.
    #
    # The strategy I've adopted is to initialize the list of available pools in the
    # bootstrap code, then check that each pool can be allocated, that each is of
    # a recognized database type (oracle and postgres as of 3/2001), and that each
    # pool is of the same database type.  We could also make certain that we're
    # connecting to the same database and user in each pool, but at the moment
    # that's seems anal even by DRB's standards.

    # Initialize the list of known database types .  User code should use the database
    # API routine db_known_database_types rather than reference the nsv list directly.
    # We might change the way this is implemented later.  Each database type is
    # represented by a list consisting of the internal name, driver name, and
    # "pretty name" (used by the APM to list the available database engines that 
    # one's package can choose to support).  The driver name and "pretty name" happen
    # to be the same for Postgres and Oracle but let's not depend on that being true
    # in all cases...

    nsv_set ad_known_database_types .  [list [list "oracle" "Oracle8" "Oracle8"] [list "postgresql" "PostgreSQL" "PostgreSQL"]]

    #
    # Initialize the list of available pools
    #

    set server_name [ns_info server]
    append config_path "ns/server/$server_name/acs/database"
    set the_set [ns_configsection $config_path]
    set pools [list]

    if { [string length $the_set] > 0 } {
        for {set i 0} {$i < [ns_set size $the_set]} {incr i} {
            if { [string tolower [ns_set key $the_set $i]] ==  "availablepool" } {
                lappend pools [ns_set value $the_set $i]
            }
        }
    }

    if { [llength $pools] == 0 } {
        set pools [ns_db pools]
    }

    # DRB: if the user hasn't given us enough database pools might as well tell
    # them in plain english

    if { [llength $pools] == 0 } {
        set database_problem "There are no database pools specified in your OpenNSD
    configuration file."
    } elseif { [llength $pools] < 3 } {
        set database_problem "OpenACS requires three database pools in order to
    run correctly."
    }

    ns_log Notice "Database API: The following pools are available: $pools"
    nsv_set db_available_pools . $pools

    # DRB: Try to allocate a handle from each pool and determine its database type.
    # I wrote this to break after the first allocation failure because a defunct
    # oracle process is created if Oracle's not running at all, causing AOLserver
    # to hang on the second attempt to allocate a handle.   At least on my RH 6.2
    # Linux box, it does.

    nsv_set ad_database_type . ""
    nsv_set ad_database_version . ""

    set bad_pools [list]
    set long_error 0
    foreach pool $pools {
        if { [catch { set db [ns_db gethandle -timeout 15 $pool]}] || ![string compare $db ""] } {
            ns_log Notice "Couldn't allocate a handle from database pool \"$pool\"."
            lappend bad_pools "<li>OpenACS could not allocate a handle from database pool \"$pool\"."
            set long_error 1
            break
        } else {
            set this_suffix ""
            if { [catch { set driver [ns_db dbtype $db] } errmsg] } {
                set database_problem "RDBMS type could not be determined: $errmsg"
                ns_log Error "RDBMS type could not be determined: $errmsg"
            } else {
                foreach known_database_type [nsv_get ad_known_database_types .] {
                    if { ![string compare $driver [lindex $known_database_type 1]] } {
                        set this_suffix [lindex $known_database_type 0]
                        break
                    }
                }
            }
            ns_db releasehandle $db
            if { [string length $this_suffix] == 0 } {
                ns_log Notice "Couldn't determine RDBMS type of database pool \"$pool\"."
                lappend bad_pools "<li>OpenACS could not determine the RDBMS type associated with
    pool \"$pool\"."
                set long_error 1
            } elseif { [string length [nsv_get ad_database_type .]] == 0 } {
                nsv_set ad_database_type . $this_suffix
            } elseif { ![string match $this_suffix [nsv_get ad_database_type .]] } {
                ns_log Notice "Database pool \"$pool\" type \"$this_suffix\" differs from
    \"[nsv_get ad_database_type .]\"."
                lappend bad_pools "<li>Database pool \"$pool\" is of type \"$this_suffix\".  The
    first database pool available to OpenACS was of type \"[nsv_get ad_database_type .]\".  All database
    pools must be configured to use the same RDMBS engine, user and database."
            }
        }
    }

    if { [string length [nsv_get ad_database_type .]] == 0 } {
        set database_problem "RDBMS type could not be determined for any pool."
        ns_log Error "RDBMS type could not be determined for any pool."
    }

    if { [llength $bad_pools] > 0 } {
        set database_problem "<p>The following database pools generated errors:
    <ul>[join $bad_pools "\n"]</ul><p>\n"
        if { $long_error } {
            append database_problem "Possible causes might include:<p>
    <ul>
    <li>The database is not running.
    <li>The database driver has not been correctly installed.
    <li>The datasource or database user/password are incorrect.
    <li>You didn't define any database pools.
    </ul><p>"
        }
    }
}
proc util_url_valid_p query_url {
    return [regexp {https?://.+} $query_url]
}
proc apm_package_version_release_tag {package_key version_name} {
    regsub -all {\.} [string toupper "$package_key-$version_name"] "-" release_tag
    return $release_tag
}
proc ns_returnerror {status msg} {
    set ::pnsd::__http_mime $status
    set ::pnsd::__http_stream $msg

#    ns_log notice "ns_returnerror (disabled) called with $args"
       
}
proc ad_verify_signature_with_expr args {    ad_verify_signature_with_expr__arg_parser

    set token_id [lindex $signature 0]
    set expire_time [lindex $signature 1]
    set hash [lindex $signature 2]

    if { [__ad_verify_signature $value $token_id $secret $expire_time $hash] } {
	return $expire_time
    } else {
	return 0
    }

}
proc ad_var_type_check_fail_p value {
    return 0
}
proc monitor {name args} {
    ns_log Debug "monitor $name"
    upvar $name value
    ns_log Debug "monitor reports '[info level 1]' changes '$name' to '$value'"
}
proc set_variables_after_query {} {
    upvar selection selection
    ::nstcl::ad_ns_set_to_tcl_vars -level 2 $selection
}
proc ns_write text { 
    ns_log notice "ns_write says: [string range $text 0 200] "
    
    append ::pnsd::__http_stream $text
}
proc ad_tcl_vars_to_ns_set__arg_parser {} {    upvar args args
    upvar put_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set_id"
                }
                upvar set_id val ; set val [lindex $args [incr i]]
            }
            -put - -put=1 {
                uplevel set put_p 1
            }
            -put=0 {
                uplevel set put_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    set args [lrange $args [expr { $i + 0 }] end]
}
proc export_vars args {    export_vars__arg_parser

    
    if { $form_p && $url_p } {
	return -code error "You must select either form format or url format, not both."
    }

    # default to URL format
    if { !$form_p && !$url_p } {
	set url_p 1
    }

    #####
    # 
    # Parse the arguments
    #
    #####
    
    # 1. if they're in override, use those
    # 2. if they're in vars, but not in exclude or override, use those
    
    # There'll always be an entry here if the variable is to be exported
    array set exp_precedence_type [list]

    # This contains entries of the form exp_flag(name:flag) e.g., exp_flag(foo:multiple)
    array set exp_flag [list]

    # This contains the value if provided, otherwise we'll pull it out of the caller's environment
    array set exp_value [list]

    foreach precedence_type { override exclude vars } {
	foreach var_spec [set $precedence_type] {
	    if { [llength $var_spec] > 2 } {
		return -code error "A varspec must have either one or two elements."
	    }
	    set name_spec [split [lindex $var_spec 0] ":"]
	    set name [lindex $name_spec 0]

	    # If we've already encountered this varname, ignore it
	    if { ![info exists exp_precedence_type($name)] } {

		set exp_precedence_type($name) $precedence_type

		if { ![string equal $precedence_type "exclude"] } {

		    set flags [split [lindex $name_spec 1] ","]
		    foreach flag $flags {
			set exp_flag($name:$flag) 1
		    }
		    
		    if { $sign_p } {
			set exp_flag($name:sign) 1
		    }
		    
		    if { [llength $var_spec] > 1 } {
			set exp_value($name) [uplevel subst \{[lindex $var_spec 1]\}]
		    } else {
			upvar 1 $name upvar_variable
			if { [info exists upvar_variable] } {
			    if { [array exists upvar_variable] } {
				set exp_value($name) [array get upvar_variable]
				set exp_flag($name:array) 1
			    } else {
				set exp_value($name) $upvar_variable
				if { [info exists exp_flag($name:array)] } {
				    return -code error "Variable \"$name\" is not an array"
				}
			    }
			}
		    }
		}
	    }
	}
    }

    #####
    #
    # Put the variables into the export_set
    #
    #####
    
    # We use an ns_set, because there may be more than one entry with the same name
    set export_set [ns_set create]

    foreach name [array names exp_precedence_type] {
	if { ![string equal $exp_precedence_type($name) "exclude"] } {
	    if { [info exists exp_value($name)] } {
		if { [info exists exp_flag($name:array)] } {
		    if { [info exists exp_flag($name:multiple)] } {
			foreach { key value } $exp_value($name) {
			    foreach item $value {
				ns_set put $export_set "${name}.${key}" $item
			    }
			}
		    } else {
			foreach { key value } $exp_value($name) {
			    ns_set put $export_set "${name}.${key}" $value
			}
		    }
		    if { [info exists exp_flag($name:sign)] } { 

                        # DRB: array get does not define the order in which elements are returned,
                        # meaning that arrays constructed in different ways can have different
                        # signatures unless we sort the returned list.  I ran into this the
                        # very first time I tried to sign an array passed to a page that used
                        # ad_page_contract to verify the veracity of the parameter.

			ns_set put $export_set "$name:sig" [ad_sign [lsort $exp_value($name)]]

		    }
		} else {
		    if { [info exists exp_flag($name:multiple)] } {
			foreach item $exp_value($name) {
			    ns_set put $export_set $name $item
			}
		    } else {
			ns_set put $export_set $name "$exp_value($name)"
		    }
		    if { [info exists exp_flag($name:sign)] } {
			ns_set put $export_set "$name:sig" [ad_sign $exp_value($name)]
		    }
		}
	    }
	}
    }
    
    #####
    #
    # Translate it into the appropriate format
    #
    #####
    
    set export_size [ns_set size $export_set]
    set export_string {}
    
    if { $url_p } {
	set export_list [list]
	for { set i 0 } { $i < $export_size } { incr i } {
	    lappend export_list "[ns_urlencode [ns_set key $export_set $i]]=[ns_urlencode [ns_set value $export_set $i]]"
	}
	set export_string [join $export_list "&"]
    } else {
	for { set i 0 } { $i < $export_size } { incr i } {
	    append export_string "<input type=\"hidden\" name=\"[ad_quotehtml [ns_set key $export_set $i]]\" value=\"[ad_quotehtml "[ns_set value $export_set $i]"]\">\n"
	}
    }

    if { $quotehtml_p } {
	set export_string [ad_quotehtml $export_string]
    }
    
    return $export_string
}
proc ns_uudecode string {
    if {[catch { package require base64 }]} {
        return -code error "ns_uudecode requires the base64 package from tcllib"
    } else {
        return [base64::decode $string]
    }
}
proc ad_page_contract_filter_rule_proc_html {name filters_varname} {upvar $filters_varname filters

    foreach flag $filters {
	if { [lsearch { nohtml html allhtml integer naturalnum } $flag] != -1 } {
	    return
	}
    }
    lappend filters nohtml
}
proc apm_interface_add__arg_parser {} {    upvar args args
    upvar interface_id val ; set val {}
    upvar callback val ; set val apm_dummy_callback

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }
            -interface_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -interface_id"
                }
                upvar interface_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { version_id interface_uri interface_version } $n_args_remaining]"
    }
    upvar version_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar interface_uri val ; set val [lindex $args [expr { $i + 1 }]]
    upvar interface_version val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc getclock {} {
    return [clock seconds]
}
proc ns_striphtml args {
            ::nstcl::_ad_proc_parser ::nstcl::ns_striphtml [set args]
            
    regsub -all -- {<[^>]+>} $html "" html
    if {!$tags_only_p} {
        regsub -all -- {&[^ \t\n\r;]+;} $html "" html
    }
    return $html
}
proc number_p str {
  return [regexp {^[-+]?[0-9]*(.[0-9]+)?$} $str]
}
proc util_prepare_update_multi_key {table_name primary_key_name_list primary_key_value_list form} {

    set form_size [ns_set size $form]
    set form_counter_i 0
    set bind_vars [ns_set create]

    while {$form_counter_i<$form_size} {

	set form_var_name [ns_set key $form $form_counter_i]
	set value [string trim [ns_set value $form $form_counter_i]]

	if { [lsearch -exact $primary_key_name_list $form_var_name] == -1 } {

	    # this is not one of the keys
	    ad_tcl_list_list_to_ns_set -set_id $bind_vars [list [list $form_var_name $value]]
	    lappend the_sets "$form_var_name = :$form_var_name"

	}

	incr form_counter_i
    }

    for {set i 0} {$i<[llength $primary_key_name_list]} {incr i} {

	set this_key_name [lindex $primary_key_name_list $i]
	set this_key_value [lindex $primary_key_value_list $i]

	ad_tcl_list_list_to_ns_set -set_id $bind_vars [list [list $this_key_name $this_key_value]]
	lappend key_eqns "$this_key_name = :$this_key_name"

    }

    return [list "update $table_name\nset [join $the_sets ",\n"] \n where [join $key_eqns " AND "]" $bind_vars]
}
proc database_to_tcl_string_or_null {dbhandle SQL {result {}}} {
    set selection [::nstcl::ns_db 0or1row $dbhandle $SQL]
    if {$selection != ""} {
        set result [::nstcl::ns_set value $selection 0]
        ::nstcl::ns_set free $selection
    }
    return $result
}
proc apm_package_instance_new__arg_parser {} {    upvar args args
    upvar package_id val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -package_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_id"
                }
                upvar package_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { instance_name context_id package_key } $n_args_remaining]"
    }
    upvar instance_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar context_id val ; set val [lindex $args [expr { $i + 1 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ns_pg_bind {command db sql {bind {}}} { 
    #set cmd [list db_bind_var_substitution $sql $bind ] 
    #set sql [uplevel 1  $cmd ] 
#    ns_log Info "ns_pg_bind: sql =  $sql"
#First replace bound variables ... skip this - nstcl should do this for me.
#     proc quotify { var } {
# 	upvar 2 $var var2
# 	regsub -all {'} $var2 {''} var2
# 	return "'$var2'"
#     }

#     #puts [pg_bind $sql]
#     regsub -all {\[} $sql {\\[} sql
#     regsub -all {\]} $sql {\\]} sql
#     regsub -all  {\:(\w+)} $sql  { [ quotify \1 ]}  sql

#     set sql [subst $sql ] 


#    set poolname [ns_db poolname $db ]
#Then run the query

    #ignore db for now
    switch $command {
	"select" { 	
	    #no name was passed in...
	    set cmd [list ns_db select $db $sql]	    
#	    return [ns_db select $db $sql]	    
	    uplevel 1 $cmd
	    return
	    
	}
	"1row"   {	    
	    set cmd [list db_1row "ns_pg_bind_1row"  $sql -column_set setid]
	    uplevel 1 $cmd
#	    db_1row "ns_pg_bind_1row"  $sql -column_set setid
#	    return $setid
	    upvar 1 setid setid
	    return $setid
	}
	"0or1row" { 	    
	    set cmd [list db_0or1row "ns_pg_bind_0or1row" $sql -column_set setid ]
	    uplevel 1 $cmd
	    upvar 1 setid setid
	    return $setid

	}
	"dml" {

	    db_dml "ns_pg_bnd_dml" $sql
	    #return dummy results
	    set dml_setid  [ns_set create dml_results]
	    ns_set put $dml_setid result 1
	    return $dml_setid
	}

    }
    

    ns_log Error "ns_pg_bind called with cmd = $command"
}
proc acs_community_member_admin_link args {    acs_community_member_admin_link__arg_parser

    if {[empty_string_p $label]} {
        set label [db_string select_community_member_link_label {
            select persons.first_names || ' ' || persons.last_name
            from persons
            where person_id = :user_id
        } -default $user_id]
    }

    return "<a href=\"[acs_community_member_admin_url -user_id $user_id]\">$label</a>"
}
proc util_AnsiDatetoPrettyDate sql_date {
    set sql_date [string range $sql_date 0 9]
    if ![regexp {(.*)-(.*)-(.*)$} $sql_date match year month day] {
	return ""
    } else {
	set allthemonths {January February March April May June July August September October November December}

	# we have to trim the leading zero because Tcl has such a 
	# brain damaged model of numbers and decided that "09-1"
	# was "8.0"

	set trimmed_month [string trimleft $month 0]
	set pretty_month [lindex $allthemonths [expr $trimmed_month - 1]]

	set trimmed_day [string trimleft $day 0]

	return "$pretty_month $trimmed_day, $year"
    }
}
proc doc_property_exists_p name {
    global doc_properties
    return [info exists doc_properties($name)]
}
proc db_abort_transaction_p {} {
    return $::nstcl::database::transaction(abort_p)
}
proc fmtclock {clockval {format {}} {zone {}}} {
    lappend cmd clock format $clockval
    if ![lempty $format] {
        lappend cmd -format $format
    }
    if ![lempty $zone] {
        lappend cmd -gmt 1
    }
    return [eval $cmd]
}
proc doc_body_flush {} {
    # Currently a no-op.
}
proc set_union {x y} {
    foreach element $y {
        if {[lsearch -exact $x $element] == -1} {
            lappend x $element
        }
    }
    return $x
}
proc ad_schedule_proc args {    ad_schedule_proc__arg_parser

    # we don't schedule a proc to run if we have enabled server clustering,
    # we're not the canonical server, and the procedure was not requested to run on all servers.
    if { [server_cluster_enabled_p] && ![ad_canonical_server_p] && $all_servers == "f" } {
        return
    } 

    # Protect the list of scheduled procs with a mutex.
    ns_mutex lock [nsv_get ad_procs mutex]
    set proc_info [list $thread $once $interval $proc $args [ns_time] 0 $debug]
    ns_log "Notice" "Scheduling proc $proc"
    
    # Add to the list of scheduled procedures, for monitoring.
    set procs [nsv_get ad_procs .]
    lappend procs $proc_info
    nsv_set ad_procs . $procs
    ns_mutex unlock [nsv_get ad_procs mutex]

    set my_args [list]
    if { $thread == "t" } {
	lappend my_args "-thread"
    }
    if { $once == "t" } {
	lappend my_args "-once"
    }

    # Schedule the wrapper procedure (ad_run_scheduled_proc).
    eval [concat [list ns_schedule_proc] $my_args [list $interval ad_run_scheduled_proc [list $proc_info]]]
}
proc ad_approval_system_inuse_p {} {
    if {[ad_parameter RegistrationRequiresEmailVerification] && [ad_parameter RegistrationRequiresApprovalP] } {
	return 1
    } else {
	return 0
    }
}
proc ad_tcl_vars_to_ns_set args {    ad_tcl_vars_to_ns_set__arg_parser

    if { ![info exists set_id] } {
	set set_id [ns_set create]
    }

    if { $put_p } {
	set command put
    } else {
	set command update
    }

    foreach varname $args {
	upvar $varname var
	ns_set $command $set_id $varname $var
    }
    return $set_id
}
proc tclMacPkgSearch dir {
    foreach x [glob -directory $dir -nocomplain *.shlb] {
	if {[file isfile $x]} {
	    set res [resource open $x]
	    foreach y [resource list TEXT $res] {
		if {[string equal $y "pkgIndex"]} {source -rsrc pkgIndex}
	    }
	    catch {resource close $res}
	}
    }
}
proc ad_tcl_vars_list_to_ns_set__arg_parser {} {    upvar args args
    upvar put_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set_id"
                }
                upvar set_id val ; set val [lindex $args [incr i]]
            }
            -put - -put=1 {
                uplevel set put_p 1
            }
            -put=0 {
                uplevel set put_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { vars_list } $n_args_remaining]"
    }
    upvar vars_list val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ns_returnbadrequest args {
    ns_log Debug "(no-op)ns_returnbadrequest called with $args "
}
proc util_wrap_list args {    util_wrap_list__arg_parser

    set out "<pre>"
    set line_length 0
    foreach item $items {
	regsub -all {<[^>]+>} $item "" item_notags
	if { $line_length > $indent } {
	    if { $line_length + 1 + [string length $item_notags] > $length } {
		append out "$eol\n"
		for { set i 0 } { $i < $indent } { incr i } {
		    append out " "
		}
		set line_length $indent
	    } else {
		append out " "
		incr line_length
	    }
	}
	append out $item
	incr line_length [string length $item_notags]
    }
    append out "</pre>"
    return $out
}
proc util_PrettySexManWoman {m_or_f {default default}} {
    if { $m_or_f == "M" || $m_or_f == "m" } {
	return "Man"
    } elseif { $m_or_f == "F" || $m_or_f == "f" } {
	return "Woman"
    } else {
	# Note that we can't compare default to the empty string as in 
	# many cases, we are going want the default to be the empty
	# string
	if { [string compare $default "default"] == 0 } {
	    return "Person of Unknown Sex"
	} else {
	    return $default
	}
    }
}
proc export_url_vars__arg_parser {} {    upvar args args
    upvar sign_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sign - -sign=1 {
                uplevel set sign_p 1
            }
            -sign=0 {
                uplevel set sign_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    set args [lrange $args [expr { $i + 0 }] end]
}
proc ad_call_proc_if_exists {proc args} {
    if { [llength [info procs $proc]] == 1 } {
	eval $proc $args
    }
}
proc db_known_database_types {} {
    return [nsv_get ad_known_database_types .]
}
proc apm_ns_write_callback string {
    ns_write $string
}
proc tclPkgUnknown {name version {exact {}}} {
    global auto_path tcl_platform env

    if {![info exists auto_path]} {
	return
    }
    # Cache the auto_path, because it may change while we run through
    # the first set of pkgIndex.tcl files
    set old_path [set use_path $auto_path]
    while {[llength $use_path]} {
	set dir [lindex $use_path end]
	# we can't use glob in safe interps, so enclose the following
	# in a catch statement, where we get the pkgIndex files out
	# of the subdirectories
	catch {
	    foreach file [glob -directory $dir -join -nocomplain  * pkgIndex.tcl] {
		set dir [file dirname $file]
		if {[file readable $file] && ![info exists procdDirs($dir)]} {
		    if {[catch {source $file} msg]} {
			tclLog "error reading package index file $file: $msg"
		    } else {
			set procdDirs($dir) 1
		    }
		}
	    }
	}
	set dir [lindex $use_path end]
	set file [file join $dir pkgIndex.tcl]
	# safe interps usually don't have "file readable", nor stderr channel
	if {([interp issafe] || [file readable $file]) &&  ![info exists procdDirs($dir)]} {
	    if {[catch {source $file} msg] && ![interp issafe]}  {
		tclLog "error reading package index file $file: $msg"
	    } else {
		set procdDirs($dir) 1
	    }
	}
	# On the Macintosh we also look in the resource fork 
	# of shared libraries
	# We can't use tclMacPkgSearch in safe interps because it uses glob
	if {(![interp issafe]) &&  [string equal $tcl_platform(platform) "macintosh"]} {
	    set dir [lindex $use_path end]
	    if {![info exists procdDirs($dir)]} {
		tclMacPkgSearch $dir
		set procdDirs($dir) 1
	    }
	    foreach x [glob -directory $dir -nocomplain *] {
		if {[file isdirectory $x] && ![info exists procdDirs($x)]} {
		    set dir $x
		    tclMacPkgSearch $dir
		    set procdDirs($dir) 1
		}
	    }
	}
	set use_path [lrange $use_path 0 end-1]
	if {[string compare $old_path $auto_path]} {
	    foreach dir $auto_path {
		lappend use_path $dir
	    }
	    set old_path $auto_path
	}
    }
}
proc util_httpget {url {headersSetIdVar {}} {timeout 30} {depth 10}} {
    if {[catch {
        ::nstcl::http::fetch_url -timeout $timeout -depth $depth  -headers $headersSetIdVar $url
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}
proc root_of_host host {
    # The main hostname is mounted at /.
    if { [string equal $host [ns_config ns/server/[ns_info server]/module/nssock Hostname]] } {
        return ""
    }
    # Other hostnames map to subsites.
    set found_node_id [db_0or1row node_id {
	select node_id 
	from host_node_map
	where host = :host
    }]

    if { $found_node_id == 1 } {
       db_1row root_get {
           select site_node.url(:node_id) as url
           from dual
       }
       return [string range $url 0 [expr [string length $url]-2]]
    } else {
       # Hack to provide a useful default
       return ""
    }
}
proc ad_set_cookie__arg_parser {} {    upvar args args
    upvar domain val ; set val {}
    upvar replace val ; set val f
    upvar value val ; set val {}
    upvar path val ; set val /
    upvar expire val ; set val f
    upvar max_age val ; set val {}
    upvar secure val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -replace {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -replace"
                }
                upvar replace val ; set val [lindex $args [incr i]]
            }
            -secure {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secure"
                }
                upvar secure val ; set val [lindex $args [incr i]]
            }
            -expire {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -expire"
                }
                upvar expire val ; set val [lindex $args [incr i]]
            }
            -max_age {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -max_age"
                }
                upvar max_age val ; set val [lindex $args [incr i]]
            }
            -domain {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -domain"
                }
                upvar domain val ; set val [lindex $args [incr i]]
            }
            -path {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -path"
                }
                upvar path val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar value val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_parameter_unregister args {    apm_parameter_unregister__arg_parser

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
proc validate_integer {field_name string} {
    if { ![regexp {^[0-9]+$} $string] } {
	error "$field_name is not an integer"
    }
    # trim leading zeros, so as not to confuse Tcl
    set string [string trimleft $string "0"]
    if { [empty_string_p $string] } {
	# but not all of the zeros
	return "0"
    }
    return $string
}
proc ad_library doc_string {
    ad_parse_documentation_string $doc_string doc_elements
    nsv_set api_library_doc [ad_make_relative_path [info script]] [array get doc_elements]
}
proc ad_publisher_name {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  PublisherName]
}
proc ad_set_signed_cookie args {    ad_set_signed_cookie__arg_parser

    if { $max_age == "inf" } {
	set signature_max_age ""
    } elseif { $max_age != "" } {
	set signature_max_age $max_age
    } else {
	# this means we want a session level cookie,
	# but that is a user interface expiration, that does
	# not give us a security expiration. (from the
	# security perspective, we use SessionLifetime)
	ns_log Debug "Security: SetSignedCookie: Using sec_session_lifetime [sec_session_lifetime]"
	set signature_max_age [sec_session_lifetime]
    }

    set cookie_value [ad_sign -secret $secret -token_id $token_id -max_age $signature_max_age $value]

    set data [ns_urlencode [list $value $cookie_value]]

    ad_set_cookie -replace $replace -secure $secure -max_age $max_age -domain $domain -path $path $name $data
}
proc ad_page_contract_filter_proc_float {name value_varname} {upvar $value_varname value

    # remove the first decimal point, the theory being that
    # at this point a valid float will pass an integer test
    regsub {\.} $value "" value_to_be_tested

    if { ![regexp {^[0-9]+$} $value_to_be_tested] } {
	ad_complain "Value is not an decimal number."
	return 0
    }
    # trim leading zeros, so as not to confuse Tcl
    set value [string trimleft $value "0"]
    if { [empty_string_p $value] } {
	# but not all of the zeros
	set value "0"
    }
    return 1
}
proc db_get_username {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    return [ns_config ns/db/pool/$pool User]    
}
proc ad_parse_html_attributes__arg_parser {} {    upvar args args
    upvar pos val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -attribute_array {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -attribute_array"
                }
                upvar attribute_array val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { html } $n_args_remaining]"
    }
    upvar html val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar pos val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_conn args {
  global ad_conn

  set flag [lindex $args 0]
  if {[string index $flag 0] != "-"} {
    set var $flag
    set flag "-get"
  } else {
    set var [lindex $args 1]
  }

  switch -- $flag {
    -connected_p {
      return [info exists ad_conn(request)]
    }

    -set {
      set ad_conn($var) [lindex $args 2]
    }

    -unset {
      unset ad_conn($var)
    }

    -reset {
      if {[info exists ad_conn]} {
	unset ad_conn
      }
      array set ad_conn {
	request ""
	sec_validated ""
	browser_id ""
	session_id ""
	user_id ""
	token ""
	last_issue ""
	deferred_dml ""
	start_clicks ""
	node_id ""
	object_id ""
	object_url ""
	object_type ""
	package_id ""
	package_url ""
	package_key ""
	extra_url ""
	file ""
	system_p 0
	path_info ""
      }
    }

    -get {
      if { [info exists ad_conn($var)] } {
	return $ad_conn($var)
      } else {
	return [ns_conn $var]
      }
    }

    default {
      error "ad_conn: unknown flag $flag"
    }
  }
}
proc ad_text_to_html__arg_parser {} {    upvar args args
    upvar no_links_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -no_links - -no_links=1 {
                uplevel set no_links_p 1
            }
            -no_links=0 {
                uplevel set no_links_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { text } $n_args_remaining]"
    }
    upvar text val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_pvt_home_link {} {
    return "<a href=\"[ad_pvt_home]\">[ad_pvt_home_name]</a>"
}
proc db_html_select_options args {    db_html_select_options__arg_parser


    set select_options ""

    if { ![empty_string_p $bind] } {
	set options [db_list $stmt_name $sql -bind $bind]
    } else {
	set options [db_list $stmt_name $sql]
    }

    foreach option $options {
	if { [string compare $option $select_option] == 0 } {
	    append select_options "<option selected>$option\n"
	} else {
	    append select_options "<option>$option\n"
	}
    }
    return $select_options

}
proc sec_get_random_cached_token_id {} {
 
    set list_of_names [ns_cache names secret_tokens]
    set random_seed [ns_rand [llength $list_of_names]]

    return [lindex $list_of_names $random_seed]
    
}
proc ad_pvt_home {} {
    return [ad_parameter -package_id [ad_acs_kernel_id] HomeURL]
}
proc ns_httpget {url {timeout 30} {depth 10}} {
    if {[catch { 
        ::nstcl::http::fetch_url -timeout $timeout -depth $depth $url 
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}
proc leap_year_p year {
    expr ( $year % 4 == 0 ) && ( ( $year % 100 != 0 ) || ( $year % 400 == 0 ) )
}
proc apm_pretty_name_for_db_type db_type {
    return [util_memoize [list db_string pretty_db_name_select "
        select pretty_db_name
        from apm_package_db_types
        where db_type_key = :db_type
    " -default "all" -bind [list db_type $db_type]]]
}
proc ad_page_contract_filter_proc_range {name value_varname range} {upvar $value_varname value

    if { [llength $range] != 2 } {
	error "Invalid number of parameters passed to filter range/"
	ad_script_abort
    }
    set min [lindex $range 0]
    set max [lindex $range 1]
    if { $value < $min || $value > $max } {
	ad_complain "$name is not in the range \[$min, $max\]"
	return 0
    }
    return 1
}
proc ad_header_with_extra_stuff args {    ad_header_with_extra_stuff__arg_parser

    set html "<html>
<head>
$extra_stuff_for_document_head
<title>$page_title</title>
</head>
"

    array set attrs [list]

    if { [info exists prefer_text_only_p] && $prefer_text_only_p == "f" && [ad_graphics_site_available_p] } {
        set attrs(bgcolor) [ad_parameter -package_id [ad_acs_kernel_id]  bgcolor "" "white"]
	set attrs(background) [ad_parameter -package_id [ad_acs_kernel_id]  background "" "/graphics/bg.gif"]
	set attrs(text) [ad_parameter -package_id [ad_acs_kernel_id]  textcolor "" "black"]
    } else {
	set attrs(bgcolor) [ad_parameter -package_id [ad_acs_kernel_id]  bgcolor "" "white"]
	set attrs(text) [ad_parameter -package_id [ad_acs_kernel_id]  textcolor "" "black"]
    }

    if { ![empty_string_p $focus] } {
	set attrs(onLoad) "javascript:document.${focus}.focus()"
    }

    foreach attr [array names attrs] {
	lappend attr_list "$attr=\"$attrs($attr)\""
    }
    append html "<body [join $attr_list]>\n"

    append html $pre_content_html
    return $html
}
proc sec_allocate_session {} {
    
    global tcl_max_value
    global tcl_current_sequence_id

    if { ![info exists tcl_max_value] || ![info exists tcl_current_sequence_id] || $tcl_current_sequence_id > $tcl_max_value } {
	# Thread just spawned or we exceeded preallocated count.
	set tcl_current_sequence_id [db_nextval sec_id_seq]
	set tcl_max_value [expr $tcl_current_sequence_id + 100]
    } 

    set session_id $tcl_current_sequence_id
    incr tcl_current_sequence_id

    return $session_id
}
proc empty_string_p string {
    return [string equal "" $string]
}
proc server_cluster_httpget_from_peers__arg_parser {} {    upvar args args
    upvar timeout val ; set val 5

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -timeout {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -timeout"
                }
                upvar timeout val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { url } $n_args_remaining]"
    }
    upvar url val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_expand_entities_ie_style html {
    array set entities { lt < gt > quot \" ob \{ cb \} amp & }

    # Expand HTML entities on the value
    for { set i [string first & $html] }  { $i != -1 }  { set i [string first & $html $i] } {
	
	set match_p 0
	switch -regexp -- [string index $html [expr $i+1]] {
	    # {
		switch -regexp -- [string index $html [expr $i+2]] {
		    [xX] {
			regexp -indices -start [expr $i+3] {[0-9a-eA-E]*} $html hex_idx
			set hex [string range $html [lindex $hex_idx 0] [lindex $hex_idx 1]]
			set html [string replace $html $i [lindex $hex_idx 1]  [subst -nocommands -novariables "\\x$hex"]]
			set match_p 1
		    }
		    [0-9] {
			regexp -indices -start [expr $i+2] {[0-9]*} $html dec_idx
			set dec [string range $html [lindex $dec_idx 0] [lindex $dec_idx 1]]
			set html [string replace $html $i [lindex $dec_idx 1]  [format "%c" $dec]]
			set match_p 1
		    }
		}
	    }
	    [a-zA-Z] {
		if { [regexp -indices -start [expr $i] {\A&([^\s;]+)} $html match entity_idx] } {
		    set entity [string tolower [string range $html [lindex $entity_idx 0] [lindex $entity_idx 1]]]
		    if { [info exists entities($entity)] } {
			set html [string replace $html $i [lindex $match 1] $entities($entity)]
		    }
		    set match_p 1
		}
	    }
	}
	incr i
	if { $match_p } {
	    # remove trailing semicolon
	    if { [string equal [string index $html $i] {;}] } {
		set html [string replace $html $i $i]
	    }
	}
    }
    return $html
}
proc ad_privacy_threshold {} {
    set session_user_id [ad_get_user_id]
    if {$session_user_id == 0} {
	# viewer of this page isn't logged in, only show stuff 
	# that is extremely unprivate
	set privacy_threshold 0
    } else {
	set privacy_threshold 5
    }
    return $privacy_threshold
}
proc util_httppost {url formvars {timeout 30} {depth 10} {http_referer {}}} {
    if {[catch {
        ::nstcl::http::fetch_url -mode POST -timeout $timeout -depth $depth  -referer $http_referer -formvars $formvars $url
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}
proc ad_verify_signature__arg_parser {} {    upvar args args
    upvar secret val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { value signature } $n_args_remaining]"
    }
    upvar value val ; set val [lindex $args [expr { $i + 0 }]]
    upvar signature val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_verify_signature_with_expr__arg_parser {} {    upvar args args
    upvar secret val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { value signature } $n_args_remaining]"
    }
    upvar value val ; set val [lindex $args [expr { $i + 0 }]]
    upvar signature val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_header__arg_parser {} {    upvar args args
    upvar extra_stuff_for_document_head val ; set val {}
    upvar focus val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -focus {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -focus"
                }
                upvar focus val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { page_title } $n_args_remaining]"
    }
    upvar page_title val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar extra_stuff_for_document_head val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc doc_set_page_documentation_mode page_documentation_mode_p {
    global ad_conn
    set ad_conn(api_page_documentation_mode_p) $page_documentation_mode_p
}
proc ad_verify_and_get_user_id args {    ad_verify_and_get_user_id__arg_parser

    return [ad_conn user_id]
}
proc ad_page_contract_verify_datasources {} {
  # for now this is a dummy proc.
  # todo: check if all datasources are defined based on property declarations
  return 1;				# ok
}
proc ns_crypt {password salt} {
    array set IP {
         0 58  1 50  2 42  3 34  4 26  5 18  6 10  7 2
         8 60  9 52 10 44 11 36 12 28 13 20 14 12 15 4
        16 62 17 54 18 46 19 38 20 30 21 22 22 14 23 6
        24 64 25 56 26 48 27 40 28 32 29 24 30 16 31 8
        32 57 33 49 34 41 35 33 36 25 37 17 38  9 39 1
        40 59 41 51 42 43 43 35 44 27 45 19 46 11 47 3
        48 61 49 53 50 45 51 37 52 29 53 21 54 13 55 5
        56 63 57 55 58 47 59 39 60 31 61 23 62 15 63 7}

    array set FP {
         0 40  1 8  2 48  3 16  4 56  5 24  6 64  7 32
         8 39  9 7 10 47 11 15 12 55 13 23 14 63 15 31
        16 38 17 6 18 46 19 14 20 54 21 22 22 62 23 30
        24 37 25 5 26 45 27 13 28 53 29 21 30 61 31 29
        32 36 33 4 34 44 35 12 36 52 37 20 38 60 39 28
        40 35 41 3 42 43 43 11 44 51 45 19 46 59 47 27
        48 34 49 2 50 42 51 10 52 50 53 18 54 58 55 26
        56 33 57 1 58 41 59  9 60 49 61 17 62 57 63 25}

    array set PC1_C {
         0 57  1 49  2 41  3 33  4 25  5 17  6  9
         7  1  8 58  9 50 10 42 11 34 12 26 13 18
        14 10 15  2 16 59 17 51 18 43 19 35 20 27
        21 19 22 11 23  3 24 60 25 52 26 44 27 36}

    array set PC1_D {
         0 63  1 55  2 47  3 39  4 31  5 23  6 15
         7  7  8 62  9 54 10 46 11 38 12 30 13 22
        14 14 15  6 16 61 17 53 18 45 19 37 20 29
        21 21 22 13 23  5 24 28 25 20 26 12 27  4}

    array set shifts {
        0 1 1 1  2 2  3 2  4 2  5 2  6 2  7 2
        8 1 9 2 10 2 11 2 12 2 13 2 14 2 15 1}

    array set PC2_C {
         0 14  1 17  2 11  3 24  4  1  5  5
         6  3  7 28  8 15  9  6 10 21 11 10
        12 23 13 19 14 12 15  4 16 26 17  8
        18 16 19  7 20 27 21 20 22 13 23  2}

    array set PC2_D {
         0 41  1 52  2 31  3 37  4 47  5 55
         6 30  7 40  8 51  9 45 10 33 11 48
        12 44 13 49 14 39 15 56 16 34 17 53
        18 46 19 42 20 50 21 36 22 29 23 32}

    array set e {
         0 32  1  1  2  2  3  3  4  4  5  5
         6  4  7  5  8  6  9  7 10  8 11  9
        12  8 13  9 14 10 15 11 16 12 17 13
        18 12 19 13 20 14 21 15 22 16 23 17
        24 16 25 17 26 18 27 19 28 20 29 21
        30 20 31 21 32 22 33 23 34 24 35 25
        36 24 37 25 38 26 39 27 40 28 41 29
        42 28 43 29 44 30 45 31 46 32 47  1}

    array set S {
        0,0  14 0,1   4 0,2  13 0,3   1 0,4   2 0,5  15 0,6  11 0,7   8
        0,8   3 0,9  10 0,10  6 0,11 12 0,12  5 0,13  9 0,14  0 0,15  7
        0,16  0 0,17 15 0,18  7 0,19  4 0,20 14 0,21  2 0,22 13 0,23  1
        0,24 10 0,25  6 0,26 12 0,27 11 0,28  9 0,29  5 0,30  3 0,31  8
        0,32  4 0,33  1 0,34 14 0,35  8 0,36 13 0,37  6 0,38  2 0,39 11
        0,40 15 0,41 12 0,42  9 0,43  7 0,44  3 0,45 10 0,46  5 0,47  0
        0,48 15 0,49 12 0,50  8 0,51  2 0,52  4 0,53  9 0,54  1 0,55  7
        0,56  5 0,57 11 0,58  3 0,59 14 0,60 10 0,61  0 0,62  6 0,63 13
        1,0  15 1,1   1 1,2   8 1,3  14  1,4  6 1,5  11 1,6   3 1,7   4
        1,8   9 1,9   7 1,10  2 1,11 13 1,12 12 1,13  0 1,14  5 1,15 10
        1,16  3 1,17 13 1,18  4 1,19  7 1,20 15 1,21  2 1,22  8 1,23 14
        1,24 12 1,25  0 1,26  1 1,27 10 1,28  6 1,29  9 1,30 11 1,31  5
        1,32  0 1,33 14 1,34  7 1,35 11 1,36 10 1,37  4 1,38 13 1,39  1
        1,40  5 1,41  8 1,42 12 1,43  6 1,44  9 1,45  3 1,46  2 1,47 15
        1,48 13 1,49  8 1,50 10 1,51  1 1,52  3 1,53 15 1,54  4 1,55  2
        1,56 11 1,57  6 1,58  7 1,59 12 1,60  0 1,61  5 1,62 14 1,63  9

        2,0  10 2,1   0 2,2   9 2,3  14 2,4   6  2,5  3 2,6  15 2,7   5
        2,8   1 2,9  13 2,10 12 2,11  7 2,12 11 2,13  4 2,14  2 2,15  8
        2,16 13 2,17  7 2,18  0 2,19  9 2,20  3 2,21  4 2,22  6 2,23 10
        2,24  2 2,25  8 2,26  5 2,27 14 2,28 12 2,29 11 2,30 15 2,31  1
        2,32 13 2,33  6 2,34  4 2,35  9 2,36  8 2,37 15 2,38  3 2,39  0
        2,40 11 2,41  1 2,42  2 2,43 12 2,44  5 2,45 10 2,46 14 2,47  7
        2,48  1 2,49 10 2,50 13 2,51  0 2,52  6 2,53  9 2,54  8 2,55  7
        2,56  4 2,57 15 2,58 14 2,59  3 2,60 11 2,61  5 2,62  2 2,63 12

        3,0   7 3,1  13 3,2  14 3,3   3  3,4  0  3,5  6 3,6   9 3,7  10
        3,8   1 3,9   2 3,10  8 3,11  5 3,12 11 3,13 12 3,14  4 3,15 15
        3,16 13 3,17  8 3,18 11 3,19  5 3,20  6 3,21 15 3,22  0 3,23  3
        3,24  4 3,25  7 3,26  2 3,27 12 3,28  1 3,29 10 3,30 14 3,31  9
        3,32 10 3,33  6 3,34  9 3,35  0 3,36 12 3,37 11 3,38  7 3,39 13
        3,40 15 3,41  1 3,42  3 3,43 14 3,44  5 3,45  2 3,46  8 3,47  4
        3,48  3 3,49 15 3,50  0 3,51  6 3,52 10 3,53  1 3,54 13 3,55  8
        3,56  9 3,57  4 3,58  5 3,59 11 3,60 12 3,61  7 3,62  2 3,63 14

        4,0   2 4,1  12 4,2   4 4,3   1 4,4   7 4,5  10 4,6  11 4,7   6
        4,8   8 4,9   5 4,10  3 4,11 15 4,12 13 4,13  0 4,14 14 4,15  9
        4,16 14 4,17 11 4,18  2 4,19 12 4,20  4 4,21  7 4,22 13 4,23  1
        4,24  5 4,25  0 4,26 15 4,27 10 4,28  3 4,29  9 4,30  8 4,31  6
        4,32  4 4,33  2 4,34  1 4,35 11 4,36 10 4,37 13 4,38  7 4,39  8
        4,40 15 4,41  9 4,42 12 4,43  5 4,44  6 4,45  3 4,46  0 4,47 14
        4,48 11 4,49  8 4,50 12 4,51  7 4,52  1 4,53 14 4,54  2 4,55 13
        4,56  6 4,57 15 4,58  0 4,59  9 4,60 10 4,61  4 4,62  5 4,63  3

        5,0  12 5,1   1 5,2  10 5,3  15 5,4   9 5,5   2 5,6   6 5,7   8
        5,8   0 5,9  13 5,10  3 5,11  4 5,12 14 5,13  7 5,14  5 5,15 11
        5,16 10 5,17 15 5,18  4 5,19  2 5,20  7 5,21 12 5,22  9 5,23  5
        5,24  6 5,25  1 5,26 13 5,27 14 5,28  0 5,29 11 5,30  3 5,31  8
        5,32  9 5,33 14 5,34 15 5,35  5 5,36  2 5,37  8 5,38 12 5,39  3
        5,40  7 5,41  0 5,42  4 5,43 10 5,44  1 5,45 13 5,46 11 5,47  6
        5,48  4 5,49  3 5,50  2 5,51 12 5,52  9 5,53  5 5,54 15 5,55 10
        5,56 11 5,57 14 5,58  1 5,59  7 5,60  6 5,61  0 5,62  8 5,63 13

        6,0   4 6,1  11 6,2   2 6,3  14 6,4  15 6,5   0 6,6   8 6,7  13
        6,8   3 6,9  12 6,10  9 6,11  7 6,12  5 6,13 10 6,14  6 6,15  1
        6,16 13 6,17  0 6,18 11 6,19  7 6,20  4 6,21  9 6,22  1 6,23 10
        6,24 14 6,25  3 6,26  5 6,27 12 6,28  2 6,29 15 6,30  8 6,31  6
        6,32  1 6,33  4 6,34 11 6,35 13 6,36 12 6,37  3 6,38  7 6,39 14
        6,40 10 6,41 15 6,42  6 6,43  8 6,44  0 6,45  5 6,46  9 6,47  2
        6,48  6 6,49 11 6,50 13 6,51  8 6,52  1 6,53  4 6,54 10 6,55  7
        6,56  9 6,57  5 6,58  0 6,59 15 6,60 14 6,61  2 6,62  3 6,63 12

        7,0  13 7,1   2 7,2   8 7,3   4 7,4   6 7,5  15 7,6  11 7,7   1
        7,8  10 7,9   9 7,10  3 7,11 14 7,12  5 7,13  0 7,14 12 7,15  7
        7,16  1 7,17 15 7,18 13 7,19  8 7,20 10 7,21  3 7,22  7 7,23  4
        7,24 12 7,25  5 7,26  6 7,27 11 7,28  0 7,29 14 7,30  9 7,31  2
        7,32  7 7,33 11 7,34  4 7,35  1 7,36  9 7,37 12 7,38 14 7,39  2
        7,40  0 7,41  6 7,42 10 7,43 13 7,44 15 7,45  3 7,46  5 7,47  8
        7,48  2 7,49  1 7,50 14 7,51  7 7,52  4 7,53 10 7,54  8 7,55 13
        7,56 15 7,57 12 7,58  9 7,59  0 7,60  3 7,61  5 7,62  6 7,63 11}

    array set P {
         0 16  1  7  2 20  3 21
         4 29  5 12  6 28  7 17
         8  1  9 15 10 23 11 26
        12  5 13 18 14 31 15 10
        16  2 17  8 18 24 19 14
        20 32 21 27 22  3 23  9
        24 19 25 13 26 30 27  6
        28 22 29 11 30  4 31 25}

    for {set i 0} {$i < 66} {incr i} {
        set block($i) 0
    }

    set pw [split $password ""]
    set pw_pos 0
    for {set i 0} {[scan [lindex $pw $pw_pos] %c c] != -1 && $i < 64}  {incr pw_pos} {

        for {set j 0} {$j < 7} {incr j ; incr i} {
            set block($i) [expr {($c >> (6 - $j)) & 01}]
        }
        incr i

    }

    for {set i 0} {$i < 28} {incr i} {
        set C($i) $block([expr {$PC1_C($i) - 1}])
        set D($i) $block([expr {$PC1_D($i) - 1}])
    }

    for {set i 0} {$i < 16} {incr i} {
        for {set k 0} {$k < $shifts($i)} {incr k} {
            set t $C(0)
            for {set j 0} {$j < 27} {incr j} {
                set C($j) $C([expr {$j + 1}])
            }
            set C(27) $t
            set t $D(0)
            for {set j 0} {$j < 27} {incr j} {
                set D($j) $D([expr {$j + 1}])
            }
            set D(27) $t
        }

        for {set j 0} {$j < 24} {incr j} {
            set KS($i,$j) $C([expr {$PC2_C($j) - 1}])
            set KS($i,[expr {$j + 24}]) $D([expr {$PC2_D($j) - 28 - 1}])
        }
    }

    for {set i 0} {$i < 48} {incr i} {
        set E($i) $e($i)
    }

    for {set i 0} {$i < 66} {incr i} {
        set block($i) 0
    }

    set salt [split $salt ""]
    set salt_pos 0
    set val_Z 90
    set val_9 57
    set val_period 46
    for {set i 0} {$i < 2} {incr i} {
        scan [lindex $salt $salt_pos] %c c
        incr salt_pos
        set iobuf($i) $c
        if {$c > $val_Z} {
            incr c -6
        }
        if {$c > $val_9} {
            incr c -7
        }
        incr c -$val_period
        for {set j 0} {$j < 6} {incr j} {
            if {[expr {($c >> $j) & 01}]} {
                set temp $E([expr {6 * $i + $j}])
                set E([expr {6 * $i + $j}]) $E([expr {6 * $i + $j + 24}])
                set E([expr {6 * $i + $j + 24}]) $temp
            }
        }
    }

    set edflag 0
    for {set h 0} {$h < 25} {incr h} {

        for {set j 0} {$j < 64} {incr j} {
            set L($j) $block([expr {$IP($j) - 1}])
        }

        for {set ii 0} {$ii < 16} {incr ii} {
            if {$edflag} {
                set i [expr {15 - $ii}]
            } else {
                set i $ii
            }

            for {set j 0} {$j < 32} {incr j} {
                set tempL($j) $L([expr {$j + 32}])
            }

            for {set j 0} {$j < 48} {incr j} {
                set preS($j) [expr {$L([expr {$E($j) - 1 + 32}]) ^ $KS($i,$j)}]
            }

            for {set j 0} {$j < 8} {incr j} {
                set t [expr {6 * $j}]
                set k $S($j,[expr {($preS($t)              << 5) +  ($preS([expr {$t + 1}]) << 3) +  ($preS([expr {$t + 2}]) << 2) +  ($preS([expr {$t + 3}]) << 1) +  $preS([expr {$t + 4}])       +  ($preS([expr {$t + 5}]) << 4)}])
                set t [expr {4 * $j}]
                set f($t)              [expr {($k >> 3) & 01}]
                set f([expr {$t + 1}]) [expr {($k >> 2) & 01}]
                set f([expr {$t + 2}]) [expr {($k >> 1) & 01}]
                set f([expr {$t + 3}]) [expr { $k       & 01}]
            }

            for {set j 0} {$j < 32} {incr j} {
                set L([expr {$j + 32}]) [expr {$L($j) ^  $f([expr {$P($j) - 1}])}]
            }

            for {set j 0} {$j < 32} {incr j} {
                set L($j) $tempL($j)
            }
        }

        for {set j 0} {$j < 32} {incr j} {
            set t $L($j)
            set L($j) $L([expr {$j + 32}])
            set L([expr {$j + 32}]) $t
        }

        for {set j 0} {$j < 64} {incr j} {
            set block($j) $L([expr {$FP($j) - 1}])
        }

    }

    for {set i 0} {$i < 11} {incr i} {
        set c 0
        for {set j 0} {$j < 6} {incr j} {
            set c [expr {$c << 1}]
            set c [expr {$c | $block([expr {6 * $i + $j}])}]
        }
        incr c $val_period
        if {$c > $val_9} {
            incr c 7
        }
        if {$c > $val_Z} {
            incr c 6
        }
        set iobuf([expr {$i + 2}]) $c
    }

    if {$iobuf(1) == 0} {
        set iobuf(1) $iobuf(0)
    }

    set elements [lsort -integer [array names iobuf]]
    set encrypted ""

    foreach element $elements {
        append encrypted [format %c $iobuf($element)]
    }

    return $encrypted
}
proc ns_section section {
    set ::nstcl::nssets::section $section
}
proc util_memoize_flush_local script {
    ns_cache flush util_memoize $script
}
proc apm_post_instantiation_tcl_proc_from_key package_key {
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
proc db_resultrows {} {
    global db_state
    return [ns_pg ntuples $db_state(last_used)]
}
proc persist'save {filename arrName el op} {
    upvar 1 $arrName arr

    
    switch -- $op {
        w {set value $arr($el)}
        u {set value {}}
    }

    set    fp [open $filename a]
    puts  $fp [list $el $value]
    close $fp
}
proc util_WriteWithExtraOutputHeaders {headers_so_far {first_part_of_page {}}} {
    ns_set put [ad_conn outputheaders] Server "[ns_info name]/[ns_info version]"
    set set_headers_i 0
    set set_headers_limit [ns_set size [ad_conn outputheaders]]
    while {$set_headers_i < $set_headers_limit} {
	append headers_so_far "[ns_set key [ad_conn outputheaders] $set_headers_i]: [ns_set value [ad_conn outputheaders] $set_headers_i]\r\n"
	incr set_headers_i
    }
    append entire_string_to_write $headers_so_far "\r\n" $first_part_of_page
    ns_write $entire_string_to_write
}
proc ns_jpegsize file {
    if {![file exists $file] || ![file readable $file]} {
        error "Could not open \"$file\""
    }
 
    set fp [open $file]
    fconfigure $fp -translation binary
    set start_of_image [read $fp 2]
    binary scan $start_of_image H4 file_type

    # make sure a malformed file doesn't get us stuck in an eternal loop
    set num_seeks 0
 
    if {$file_type == "ffd8"} {
        set data ""
        while {![eof $fp] && [incr num_seeks] < 100} {
            while {$data != "ff"} {
                binary scan [read $fp 1] H2 data
            }
 
            while {$data == "ff"} {
                binary scan [read $fp 1] H2 data
            }
            if {$data >= "c0" && $data <= "c3"} {
                binary scan [read $fp 7] x3SS height width
                close $fp
                return [list $width $height]
            }
 
            binary scan [read $fp 2] S offset
            seek $fp [expr $offset - 2] current
        }
    }
 
    close $fp
    error "invalid jpeg file: \"$file\""
}
proc ad_export_vars args {    ad_export_vars__arg_parser


    ####################
    #
    # Build up an array of values to export
    #
    ####################

    array set export [list]

    set override_p 0
    foreach argument { include override } {
	foreach arg [set $argument] {
	    if { [llength $arg] == 1 } { 
		if { $override_p || [lsearch -exact $exclude $arg] == -1 } {
		    upvar $arg var
		    if { [array exists var] } {
			# export the entire array
			foreach name [array names var] {
			    if { $override_p || [lsearch -exact $exclude "${arg}($name)"] == -1 } {
				set export($arg.$name) $var($name)
			    }
			}
		    } elseif { [info exists var] } {
			if { $override_p || [lsearch -exact $exclude $arg] == -1 } {
			    # if the var is part of an array, we'll translate the () into a dot.
			    set left_paren [string first ( $arg]
			    if { $left_paren == -1 } {
				set export($arg) $var
			    } else {
				# convert the parenthesis into a dot before setting
				set export([string range $arg 0 [expr { $left_paren - 1}]].[string  range $arg [expr { $left_paren + 1}] end-1]) $var
			    }
			}
		    }
		}
	    } elseif { [llength $arg] %2 == 0 } {
		foreach { name value } $arg {
		    if { $override_p || [lsearch -exact $exclude $name] == -1 } {
			set left_paren [string first ( $name]
			if { $left_paren == -1 } {
			    set export($name) [lindex [uplevel list \[subst [list $value]\]] 0]
			} else {
			    # convert the parenthesis into a dot before setting
			    set export([string range $arg 0 [expr { $left_paren - 1}]].[string  range $arg [expr { $left_paren + 1}] end-1])  [lindex [uplevel list \[subst [list $value]\]] 0]
			}
		    }
		}
	    } else {
		return -code error "All the exported values must have either one or an even number of elements"
	    }
	}
	incr override_p
    }
    
    ####################
    #
    # Translate this into the desired output form
    #
    ####################

    if { !$form_p } {
	set export_list [list]
	foreach varname [array names export] {
	    lappend export_list "[ns_urlencode $varname]=[ns_urlencode $export($varname)]"
	}
	return [join $export_list &]
    } else {
	set export_list [list]
	foreach varname [array names export] {
	    lappend export_list "<input type=hidden name=\"[ad_quotehtml $varname]\" value=\"[ad_quotehtml $export($varname)]\">"
	}
	return [join $export_list \n]
    }
}
proc rp_registered_proc_info_compare {info1 info2} {
    set info1_path [lindex $info1 1]
    set info2_path [lindex $info2 1]

    set info1_path_length [string length $info1_path]
    set info2_path_length [string length $info2_path]

    if { $info1_path_length < $info2_path_length } {
	return 1
    }
    if { $info1_path_length > $info2_path_length } {
	return -1
    }
    return 0
}
proc acs_community_member_url args {    acs_community_member_url__arg_parser

    return "[ad_parameter -package_id [ad_acs_kernel_id] CommunityMemberURL]?[export_vars user_id]"
}
proc ns_db args {
    upvar 0 ::nstcl::database::commands commands
    set argc [llength $args]

    if {$argc == 0} {
        return -code error "wrong # of args: should be \"ns_db command ?args?\""
    } 

    set cmd  [lindex $args 0]
    set args [lrange $args 1 end]
    incr argc -1

    if {![info exists commands($cmd)]} {
        return -code error "unknown command \"$cmd\": should be one of [join [lrange [lsort [array names commands]] 0 end-1] ", "] or [lindex [lsort [array names commands]] end]"
    }


    # valid handle?
    array set command $commands($cmd)
    if {$command(handle) && $argc >= 0} {
        set db [lindex $args 0]
        if {[regexp {^::nstcl::database::nsdb-[^-]+-[1-9][0-9]*$} $db] == 0 ||
            [info exists $db] == 0 || [interp alias {} $db] == "" ||
            [$db allocated_p] == 0} {
            return -code error "invalid database id: \"$db\""
        }
    }

    if {$argc < $command(min) || $argc > $command(max)} {
        if {$command(syntax) != ""} {
            return -code error "wrong # of args: should be \"ns_db $cmd $command(syntax)\""
        } else {
            return -code error "wrong # of args: should be \"ns_db $cmd\""
        }
    }

    if {$command(syntax) == "dbhandle sql"} {
        $db sql [lindex $args 1]
    }

    switch -- $argc {
        0 { set error_p [catch { ::nstcl::database::$cmd } result] }
 
        1 { set error_p [catch { ::nstcl::database::$cmd  [lindex $args 0] } result] 
          }

        2 { set error_p [catch { ::nstcl::database::$cmd  [lindex $args 0]  [lindex $args 1] } result]
          }

        3 { set error_p [catch { ::nstcl::database::$cmd  [lindex $args 0]  [lindex $args 1]  [lindex $args 2] } result]
          }

        4 { set error_p [catch { ::nstcl::database::$cmd  [lindex $args 0]  [lindex $args 1]  [lindex $args 2]  [lindex $args 3] } result]
          }
    }

    if {$error_p} {
        set ::nstcl::database::errorInfo $::errorInfo
        return -code error $result
    } else {
        return $result
    }
}
proc ad_tcl_list_list_to_ns_set__arg_parser {} {    upvar args args
    upvar put_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set_id"
                }
                upvar set_id val ; set val [lindex $args [incr i]]
            }
            -put - -put=1 {
                uplevel set put_p 1
            }
            -put=0 {
                uplevel set put_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { kv_pairs } $n_args_remaining]"
    }
    upvar kv_pairs val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_page_contract_filter_script filter {
    return [lindex [nsv_get ad_page_contract_filters $filter] 3]
}
proc db_qd_log {level msg} {
    # Centralized DB QD logging
    # We switch everything to debug for now
    ns_log $level "QD_LOGGER = $msg"
}
proc ad_try {code args} {
  global errorInfo errorCode

  if {[set errno [catch {uplevel $code} result]]} {
    if {$errno == 1 && [string equal [lindex $errorCode 0] "AD"] &&  [string equal [lindex $errorCode 1] "EXCEPTION"]} {
      set exception [lindex $errorCode 2]

      set matched 0
      for {set i 0} {$i < [llength $args]} {incr i 3} {
	if {[string match [lindex $args $i] $exception]} {
	  set matched 1
	  break
	}
      }

      if $matched {
	upvar [lindex $args [expr $i + 1]] var
	set var $result
	set errno [catch {uplevel [lindex $args [expr $i + 2]]} result]
      }
    }

    return -code $errno -errorcode $errorCode -errorinfo $errorInfo $result
  }
}
proc apply {func arglist} {
    set func_and_args [concat $func $arglist]
    return [uplevel $func_and_args]
}
proc rp_handle_tcl_request {} {
    namespace eval template variable parse_level [info level]
    source [ad_conn file]
}
proc doc_tag_ad_property {contents params} {
    set name [ns_set iget $params name]
    if { [empty_string_p $name] } {
	return "<em>No <tt>name</tt> property in <tt>AD-PROPERTY</tt> tag</em>"
    }
    doc_set_property $name $contents
}
proc db_get_database {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    set datasource [ns_config ns/db/pool/$pool DataSource]    
    set last_colon_pos [string last ":" $datasource]
    if { $last_colon_pos == -1 } {
        ns_log Error "datasource contains no \":\"? datasource = $datasource"
        return ""
    }
    return [string range $datasource [expr $last_colon_pos + 1] end]
}
proc ns_quotehtml html {
    return [string map [list & "&amp;" < "&lt;" > "&gt;"] $html]
}
proc site_node_closest_ancestor_package__arg_parser {} {    upvar args args
    upvar default val ; set val {}
    upvar url val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -default {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -default"
                }
                upvar default val ; set val [lindex $args [incr i]]
            }
            -url {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -url"
                }
                upvar url val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { package_key } $n_args_remaining]"
    }
    upvar package_key val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_dateentrywidget_default_to_today column {
    set today [lindex [split [ns_localsqltimestamp] " "] 0]
    return [ad_dateentrywidget $column $today]
}
proc ns_pngsize file {
     if {[file size $file] < 33} {
         error "File $file not large enough to contain PNG header"
     }

     set f [open $file r]
     fconfigure $f -encoding binary -translation binary

     # Read PNG file signature
     binary scan [read $f 8] c8 sig
     foreach b1 $sig b2 {-119 80 78 71 13 10 26 10} {
         if {$b1 != $b2} {
             close $f
             error "$file is not a PNG file"
         }
     }

     # Read IHDR chunk signature
     binary scan [read $f 8] c8 sig
     foreach b1 $sig b2 {0 0 0 13 73 72 68 82} {
         if {$b1 != $b2} {
             close $f
             error "$file is missing a leading IHDR chunk"
         }
     }

     # Read off the size of the image
     binary scan [read $f 8] II width height

     close $f
     return [list $width $height]
}
proc ns_guesstype filename { 
     global mimetypes
          
     set ext [ file extension $filename ]	

     if { [info exists mimetypes($ext)] } { 
          return $mimetypes($ext)
     }

#take a guess
     return "text/html"
}
proc ns_xml {command args} {
    switch $command {
	"parse" { 
	    set xml [lindex $args 0] 

	    #ignore the -persist flag...
	    if [string equal $xml "-persist"] {
		set xml [lindex $args 1] 
	    }

	    #hack xql files into valid xml - sigh
#	    regsub -all  {<querytext>} $xml {<querytext><![CDATA[} xml
#	    regsub -all  {</querytext>} $xml {]]></querytext>} xml

	    set doc [dom parse $xml]; 
	    return $doc 
	}  
	"doc" { 
	    switch [lindex $args 0] {
		"root"  { set root  [[lindex $args 1] documentElement]; return $root }
		"free" { [lindex $args 1] delete; return  } 
	    }
	    ns_log Error "ns_xml called with bad 'doc' args: $args"
	}

	"node"  {  


	    switch [lindex $args 0] { 
		"children" { set children [ [lindex $args 1] childNodes] ; return $children }		
		"name"     { 
		    set node [lindex $args 1]
#		    if {[$node nodeType] == "ELEMENT_NODE" } {
#			if {[$node hasAttribute name]} {
#			    return [$node getAttribute name ]
#			} else { 
#			    puts "$node $errmsg"			    
#			}
#		    }
		    set name [$node nodeName]
		    return $name; 
		}


	        "getattr"  { 
		    set node [lindex $args 1]
		    set name [lindex $args 2]
		    
		    if {[$node nodeType] == "ELEMENT_NODE" } {
			if {[$node hasAttribute $name] } {
			    return [$node getAttribute $name ]
			} else  {
			    #no attribute here
			    return ""; 
			}
		    }

		}
		"getcontent"  { 
		    set node [lindex $args 1]
		    switch [$node nodeType] {
			"ELEMENT_NODE" {set content [$node text ]}
			"TEXT_NODE" { set content [$node data]}
		    }
		    return $content
		}

		
	    }
	}
	
    }
    ns_log Error "ns_xml called with bad args: $args"
}
proc db_transaction {code args} {
    set argc [llength $args]
    if {($argc != 0 && $argc != 2) ||
        ($argc == 2 && ![string equal [lindex $args 0] "on_error"])} {
        error "db_transaction called with invalid syntax.  Should be: \"db_transaction code ?on_error on_error_code?\"" ;# "
    }

    if {$argc == 2} {
        set exception_code [lindex $args 1]
    }

    upvar 0 ::nstcl::database::transaction transaction
    incr transaction(depth)

    if {$transaction(depth) == 1} {
        set transaction(abort_p) 0
        set transaction(available) {}
    }

    set return_code [catch { uplevel 1 $code } result]

    global errorInfo
    global errorCode

    if {$return_code != 0} {
        set transaction(errorInfo) $errorInfo
        set transaction(errorCode) $errorCode

        ::nstcl::db_abort_transaction
    }

    if {[::nstcl::db_abort_transaction_p]} {
        if {[info exists exception_code]} {
            set return_code [catch { uplevel 1 $exception_code } result]
            if {$return_code != 0} {
                set transaction(errorInfo)  [join [list $transaction(errorInfo) $errorInfo)] \n]
                ::nstcl::db_abort_transaction
            }
        }
    }

    incr transaction(depth) -1
    
    if {$transaction(depth) == 0} {
        switch [::nstcl::db_abort_transaction_p] {
            0 { set action "end transaction" }
            1 { set action "abort transaction" }
        }

        foreach dbhandle $transaction(dbhandles) {
            catch { ::nstcl::ns_db flush $dbhandle }
            ::nstcl::ns_db dml $dbhandle $action
            ::nstcl::ns_db releasehandle $dbhandle
        }

        set transaction(dbhandles) {}
    }

    if {[::nstcl::db_abort_transaction_p]} {
        if {$transaction(depth) == 0} {
            set transaction(abort_p) 0
        }
        error $result $transaction(errorInfo) $transaction(errorCode)
    }
}
proc ad_user_class_parameters {} {
    return [list category_id country_code usps_abbrev intranet_user_p group_id last_name_starts_with email_starts_with expensive user_state sex age_above_years age_below_years registration_during_month registration_before_days registration_after_days registration_after_date last_login_before_days last_login_after_days last_login_equals_days number_visits_below number_visits_above user_class_id sql_post_select crm_state curriculum_elements_completed]
}
proc ad_change_password {user_id new_password} {

    # In case someone wants to change the salt from now on, you can do
    # this and still support old users by changing the salt below.
    set salt [sec_random_token]
    set new_password [ns_sha1 "$new_password$salt"]
    db_dml password_update "update users set password = :new_password, salt = :salt where user_id = :user_id"
    
}
proc ns_urldecode string {
    set decoded_string ""
    set len [string length $string]

    for {set i 0} {$i < $len} {incr i} {
        set char [string index $string $i]
        if {$char != "%"} {
            append decoded_string $char
        } else {
            set hex [string range $string [expr {$i + 1}] [expr {$i + 2}]]
            incr i 2
            if {[string length $hex] == 2 && [scan $hex %x ascii]} {
               append decoded_string [format %c $ascii]
            } else {
                return
            }
        }
    }

    return $decoded_string
}
proc util_complete_url_p string {
  if {[regexp -nocase {^[a-z]+:} $string]} {
     return 1
  } else {
     return 0
  }
}
proc ad_canonical_server_p {} {
    # we're using IP:port to uniquely identify the canonical server, since
    # hostname or IP does not always uniquely identify an instance of
    # aolserver (for instance, if we have the aolservers sitting behind a
    # load balancer). we could put something into the /home/aol30/server.ini file
    # but since we don't distribute a standard .ini file in the ACS,
    # we don't want to do that. richardl@arsdigita.com, 28 June 2000.
    set canonical_server [ad_parameter -package_id [ad_acs_kernel_id] CanonicalServer server-cluster]
    if { [empty_string_p $canonical_server] } {
	ns_log Error "Error: Your .ini file is set up incorrectly for server clustering. Please ensure that you have the CanonicalServer parameter set correctly."
	return 1
    }

    if { ![regexp {(.*):(.*)} $canonical_server match canonical_ip canonical_port] } {
	set canonical_port 80
	set canonical_ip $canonical_server
    }
   
    if { [ns_config ns/server/[ns_info server]/module/nssock Address] == $canonical_ip &&  [ns_config ns/server/[ns_info server]/module/nssock Port 80] == $canonical_port } {
	return 1
    }

    return 0
}
proc db_lst_of_lists {statement_name pre_sql args} {
#        ns_log info "db_lst_of_lists called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_lst_of_lists $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_lst_of_lists $statement_name $sql] $args]
    }
proc db_qd_replace_sql {statement_name sql} {
    set fullquery [db_qd_fetch $statement_name]

    if {![empty_string_p $fullquery]} {
	set sql [db_fullquery_get_querytext $fullquery]
    } else {
	db_qd_log Debug "NO FULLQUERY FOR $statement_name --> using default SQL"
    }

    return $sql
}
proc ad_verify_and_get_user_id__arg_parser {} {    upvar args args
    upvar secure val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -secure {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secure"
                }
                upvar secure val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc sec_get_token token_id {
    
    global tcl_secret_tokens

    if { [info exists tcl_secret_tokens($token_id)] } {
	return $tcl_secret_tokens($token_id)
    } else {
	set token [ns_cache eval secret_tokens $token_id {
	    set token [db_string get_token {select token from secret_tokens
                       	                 where token_id = :token_id} -default 0]

	    # Very important to throw the error here if $token == 0
	    # see: http://www.arsdigita.com/sdm/one-ticket?ticket_id=10760

            if { $token == 0 } {
	        error "Invalid token ID"
	    }

	    return $token
	}]

	set tcl_secret_tokens($token_id) $token
	return $token
	
    }

}
proc ad_decode args {
    set num_args [llength $args]
    set input_value [lindex $args 0]

    set counter 1

    while { $counter < [expr $num_args - 2] } {
	lappend from_list [lindex $args $counter]
	incr counter
	lappend to_list [lindex $args $counter]
	incr counter
    }

    set default_value [lindex $args $counter]

    if { $counter < 2 } {
	return $default_value
    }

    set index [lsearch -exact $from_list $input_value]
    
    if { $index < 0 } {
	return $default_value
    } else {
	return [lindex $to_list $index]
    }
}
proc doc_init {} {
    global doc_properties
    if { [info exists doc_properties] } {
	unset doc_properties
    }
    array set doc_properties {}
}
proc ns_returnredirect redir { 
    ns_log Notice "ns_returnredirect to $redir"
    array set url [uri::split $redir]

    set $::pnsd::url $url(path)
    set $::pnsd::querystring  $url(query)
    rp_handler

}
proc merge_form_with_query args {    merge_form_with_query__arg_parser

    set set_id [ns_set create]

    ns_log Notice "statement_name = $statement_name"
    ns_log Notice "sql_qry = $sql_qry"
    ns_log Notice "set_id = $set_id"

    db_0or1row $statement_name $sql_qry -bind $bind -column_set set_id
    
    if { $set_id != "" } {
	
	for {set i 0} {$i<[ns_set size $set_id]} {incr i} {
	    set form [ns_formvalueput $form [ns_set key $set_id $i] [ns_set value $set_id $i]]
	}
	
    }
    return $form    
}
proc ns_cache {cmd args} {
    ::nstcl::ad_arg_parser { size timeout thread args } $args
#    puts "Debug: ns_cache $cmd called w/ $args "
    switch $cmd { 
	"create"  { 
	    ns_log info "(no-op)ns_cache creating [lindex $args 0]"
#	    set cachename [lindex $args 0]
#	    global $cachename
#	    array set $cachename {}
	    return 1
#	    return
	}
	"get"     { 
	    set cachename [lindex $args 0]
#	    global $cachename	    
	    set key [lindex $args 1]
	    
	    if {[catch {
		db_1row ns_cache_get "select cache_value from ns_cache where cacheid=:cachename and cache_key = :key"}]} {		
		if {[llength $args] == 3 } {
		    return 0
		} else {		    
		    error "ns_cache get: Couldn't locate key in cache"
		}
	    } else {
		# we were able to retrieve a cache value ... 
		if {[llength $args] == 3 } {
		    set varname [lindex $args 2]		
		    upvar 1 $varname $varname
		    set $varname $cache_value		
		    return 1
		}

		return $cache_value
	    }


	}
	"set"     { 
	    ns_log info "ns_cache SET [info level 0]"
	    set cachename [lindex $args 0]
#	    global $ cachename
	    set key [lindex $args 1]
	    set value [lindex $args 2]
#	    ns_log Debug "ns_cache set called w/ $args "
	    db_dml ns_cache_remove "delete from ns_cache where cacheid=:cachename and cache_key=:key"
	    db_dml ns_cache_set "insert into ns_cache(cacheid, cache_key, cache_value) values (:cachename, :key, :value)"
	    return 1
	}
	
	"eval"    { 
	    set cachename [lindex $args 0]
#	    global $cachename
	    set key [lindex $args 1]
	    set script [lindex $args 2 ]
	    #puts "eval scrtip = $script"
	    if {[catch { set statement_value [ns_cache get $cachename $key] }]} {
		# not in the cache already
		set statement_value [uplevel 1 $script]
		ns_cache set $cachename $key $statement_value
	    }
	    return $statement_value
	}
	"names" {
	    set cachename [lindex $args 0]
	    return [db_list ns_cache_names "select cache_key from ns_cache where cacheid=:cachename"]
	}	   
	"flush" {
	    set cachename [lindex $args 0]
	    if {[llength $args] == 1} { error "No key passed to flush" } 
	    set key [lindex $args 1]
	    db_dml ns_cache_flush "delete from ns_cache where cacheid=:cachename and cache_key = :key"
	    return 1
	}
	"init" {
	    #added by me... it will create the  initial db table for ns_cache
	    
	    return expr ![catch { db_dml ns_cache_initialize "create table ns_cache ( cacheid text, cache_key varchar(1000), cache_value text)"} ]
	    
	}
	"reinit" {
	    ns_log info "ns_cache re-initializing"
	    db_dml ns_cache_reinit "delete from ns_cache"
	    return 1
	}
	
    }

    ns_log Error "ns_cache $cmd called with $args"
}
proc util_wrap_list__arg_parser {} {    upvar args args
    upvar eol val ; set val \ \\
    upvar length val ; set val 70
    upvar indent val ; set val 4

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -eol {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -eol"
                }
                upvar eol val ; set val [lindex $args [incr i]]
            }
            -indent {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -indent"
                }
                upvar indent val ; set val [lindex $args [incr i]]
            }
            -length {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -length"
                }
                upvar length val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { items } $n_args_remaining]"
    }
    upvar items val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_release_unused_handles {} {
    global db_state

    if { [info exists db_state(n_handles_used)] } {
	# Examine the elements at the end of db_state(handles), killing off
	# handles that are unused and not engaged in a transaction.

	set index_to_examine [expr { [llength $db_state(handles)] - 1 }]
	while { $index_to_examine >= $db_state(n_handles_used) } {
	    set db [lindex $db_state(handles) $index_to_examine]

	    # Stop now if the handle is part of a transaction.
	    if { [info exists db_state(transaction_level,$db)] &&  $db_state(transaction_level,$db) > 0 } {
		break
	    }

	    set start_time [clock clicks]
	    ns_db releasehandle $db
	    ad_call_proc_if_exists ds_collect_db_call $db releasehandle "" "" $start_time 0 ""
	    incr index_to_examine -1
	}
	set db_state(handles) [lrange $db_state(handles) 0 $index_to_examine]
    }
}
proc ad_template_return {{file_stub {}}} {
    uplevel 1 "ad_return_template $file_stub"
}
proc ns_returnnotfound {} {
    ns_log warning "(no-op) ns_returnnotfound "
}
proc ad_page_contract_filter_proc filter {
    return [lindex [nsv_get ad_page_contract_filters $filter] 1]
}
proc doc_serve_document {} {
    if { ![doc_exists_p] } {
	error "No document has been built."
    }

    set mime_type [doc_get_property mime_type]
    if { [empty_string_p $mime_type] } {
	if { [doc_property_exists_p title] } {
	    set mime_type "text/html;content-pane"
	} else {
	    set mime_type "text/html"
	}
    }

    switch $mime_type {
	text/html;content-pane - text/x-html-content-pane {
	    # It's a content pane. Find the appropriate template.
	    set template_path [doc_find_template [ad_conn file]]
	    if { [empty_string_p $template_path] } {
		ns_returnerror 500 "Unable to find master template"
	        ns_log error  "Unable to find master template for file '[ad_conn file]'"
	    } else {
	        doc_serve_template $template_path
	    }
	}
	default {
	    # Return a complete document.
	    ns_return 200 $mime_type [doc_get_property body]
	}
    }
}
proc ad_outgoing_sender {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  OutgoingSender]
}
proc util_PrettyTclBoolean zero_or_one {
    if $zero_or_one {
	return "Yes"
    } else {
	return "No"
    }
}
proc ad_return args {
    eval return $args
}
proc ad_url {} {
    # this will be called by email alerts. Do not use ad_conn location
    return [ad_parameter -package_id [ad_acs_kernel_id] SystemURL]
}
proc post_args_to_query_string {} {
    set arg_form [ns_getform]
    if {$arg_form!=""} {
	set form_counter_i 0
	while {$form_counter_i<[ns_set size $arg_form]} {
	    append query_return "[ns_set key $arg_form $form_counter_i]=[ns_urlencode [ns_set value $arg_form $form_counter_i]]&"
	    incr form_counter_i
	}
	set query_return [string trim $query_return &]
    }
}
proc end_of_day date {
    return [clock format [clock scan $date] -format "%Y-%m-%d 23:59:59"]
}
proc ad_run_scheduled_proc proc_info {
    # Grab information about the scheduled procedure.
    set thread [lindex $proc_info 0]
    set once [lindex $proc_info 1]
    set interval [lindex $proc_info 2]
    set proc [lindex $proc_info 3]
    set args [lindex $proc_info 4]
    set time [lindex $proc_info 5]
    set count 0
    set debug [lindex $proc_info 7]

    ns_mutex lock [nsv_get ad_procs mutex]
    set procs [nsv_get ad_procs .]

    # Find the entry in the shared variable. Splice it out.
    for { set i 0 } { $i < [llength $procs] } { incr i } {
	set other_proc_info [lindex $procs $i]
	for { set j 0 } { $j < 5 } { incr j } {
	    if { [lindex $proc_info $j] != [lindex $other_proc_info $j] } {
		break
	    }
	}
	if { $j == 5 } {
	    set count [lindex $other_proc_info 6]
	    set procs [lreplace $procs $i $i]
	    break
	}
    }

    if { $once == "f" } {
	# The proc will run again - readd it to the shared variable (updating ns_time and
	# incrementing the count).
	lappend procs [list $thread $once $interval $proc $args [ns_time] [expr { $count + 1 }] $debug]
    }
    nsv_set ad_procs . $procs

    ns_mutex unlock [nsv_get ad_procs mutex]

    if { $debug == "t" } {
	ns_log "Notice" "Running scheduled proc $proc..."
    }
    # Actually run the procedure.
    eval [concat [list $proc] $args]
    if { $debug == "t" } {
	ns_log "Notice" "Done running scheduled proc $proc."
    }
}
proc ad_return_error {title explanation} {
    ad_return_exception_page 500 $title $explanation
}
proc rp_filter why {
    #####
    #
    # Initialize the environment: reset ad_conn, and populate it with
    # a few things.
    #
    #####

    ad_conn -reset
    ad_conn -set request [nsv_incr rp_properties request_count]
    ad_conn -set user_id 0
    ad_conn -set start_clicks [clock clicks]

    # -------------------------------------------------------------------------
    # Start of patch "hostname-based subsites"
    # -------------------------------------------------------------------------
    # 1. determine the root of the host and the requested URL
    set root [root_of_host [ad_host]]
    set url [ad_conn url]
    # 2. handle special case: if the root is a prefix of the URL, 
    #                         remove this prefix from the URL, and redirect.
    if { ![empty_string_p $root] && [regexp "^${root}(.*)$" $url match url] } {
	if [regexp {^GET [^\?]*\?(.*) HTTP} [ns_conn request] match vars] {
	    append url ?$vars
	}
        if {[ad_secure_conn_p]} {
            # it's a secure connection.
            ad_returnredirect https://[ad_host][ad_port]$url
	    return "filter_return"
        } else {
            ad_returnredirect http://[ad_host][ad_port]$url
	    return "filter_return"
        }
    }
    # Normal case: Prepend the root to the URL.
    # 3. set the intended URL
    ad_conn -set url ${root}${url}
    # 4. set urlv and urlc for consistency
    set urlv [lrange [split $root /] 1 end]
    ad_conn -set urlc [expr [ad_conn urlc]+[llength $urlv]]
    ad_conn -set urlv [concat $urlv [ad_conn urlv]]
    # -------------------------------------------------------------------------
    # End of patch "hostname-based subsites"
    # -------------------------------------------------------------------------

    # DRB: a bug in ns_conn causes urlc to be set to one and urlv to be set to
    # {} if you hit the site with the host name alone.  This confuses code that
    # expects urlc to be set to zero and the empty list.  This bug is probably due
    # to changes in list handling in Tcl 8x vs. Tcl 7x.

    if { [ad_conn urlc] == 1 && [lindex [ad_conn urlv] 0] == "" } {
        ad_conn -set urlc 0
        ad_conn -set urlv [list]
    }

    rp_debug -ns_log_level debug -debug t "rp_filter: setting up request: [ns_conn method] [ns_conn url] [ns_conn query]"

    global tcl_site_nodes
    if [catch {
      if [catch { array set node $tcl_site_nodes([ad_conn url]) }] {
	array set node [site_node [ad_conn url]]
	set tcl_site_nodes([ad_conn url]) [array get node]
      }
    } errmsg] {
        # log and do nothing
        rp_debug -debug t "error within rp_filter [ns_conn method] [ns_conn url] [ns_conn query].  $errmsg"
    } else {
	if {[string equal $node(url) "[ad_conn url]/"]} {
	    ad_returnredirect $node(url)
            rp_debug "rp_filter: returnredirect $node(url)"
            rp_debug "rp_filter: return filter_return"
	    return "filter_return"
	}

	ad_conn -set node_id $node(node_id)
	ad_conn -set object_id $node(object_id)
	ad_conn -set object_url $node(url)
	ad_conn -set object_type $node(object_type)
	ad_conn -set package_id $node(object_id)
	ad_conn -set package_key $node(package_key)
	ad_conn -set package_url $node(url)
	ad_conn -set extra_url [string range [ad_conn url] [string length $node(url)] end]
    }

    #####
    #
    # See if any libraries have changed. This may look expensive, but all it
    # does is check an NSV.
    #
    #####

    if ![rp_performance_mode] {
      # We wrap this in a catch, because we don't want an error here to 
      # cause the request to fail.
      if { [catch { apm_load_any_changed_libraries } error] } {
        global errorInfo
        ns_log "Error" $errorInfo
      }
    }

    #####
    #
    # Read in and/or generate security cookies.
    #
    #####

    # sec_handler (defined in security-procs.tcl) sets the ad_conn
    # session-level variables such as user_id, session_id, etc. we can
    # call sec_handler at this point because the previous return
    # statements are all error-throwing cases or redirects.
    # ns_log Notice "OACS= RP start"
    sec_handler
    # ns_log Notice "OACS= RP end"

    #####
    #
    # Make sure the user is authorized to make this request. 
    #
    #####
    if { ![empty_string_p [ad_conn object_id]]} {
      ad_try {
	if {[string match "admin/*" [ad_conn extra_url]]} {
	  ad_require_permission [ad_conn object_id] admin
	} else {
	  ad_require_permission [ad_conn object_id] read
	}
      } ad_script_abort val {
	rp_finish_serving_page
        rp_debug "rp_filter: return filter_return"
	return "filter_return"
      }
    }
    rp_debug "rp_filter: return filter_ok"
    return "filter_ok"
}
proc export_form_vars args {    export_form_vars__arg_parser
 
    set hidden ""
    foreach var_spec $args {
	set var_spec_pieces [split $var_spec ":"]
	set var [lindex $var_spec_pieces 0]
	set type [lindex $var_spec_pieces 1]
	upvar 1 $var value
	if { [info exists value] } {
	    switch $type {
		multiple {
		    foreach item $value {
			append hidden "<input type=\"hidden\" name=\"[ad_quotehtml $var]\" value=\"[ad_quotehtml $item]\">\n"
		    }
		}
		default {
		    append hidden "<input type=\"hidden\" name=\"[ad_quotehtml $var]\" value=\"[ad_quotehtml $value]\">\n"
		}
	    }
	    if { $sign_p } {
		append hidden "<input type=\"hidden\" name=\"[ad_quotehtml "$var:sig"]\" value=\"[ad_quotehtml [ad_sign $value]]\">\n"
	    }
	}
    }
    return $hidden
}
proc ns_log {severity args} { 
    if [string match -nocase $severity "error"] {
	append ::pnsd::error "$args\n"
    }

    if { [llength $args] } {
	log::log [string tolower $severity] $args
    }

}
proc db_html_select_value_options__arg_parser {} {    upvar args args
    upvar option_index val ; set val 1
    upvar value_index val ; set val 0
    upvar select_option val ; set val {}
    upvar bind val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -bind {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -bind"
                }
                upvar bind val ; set val [lindex $args [incr i]]
            }
            -select_option {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -select_option"
                }
                upvar select_option val ; set val [lindex $args [incr i]]
            }
            -value_index {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -value_index"
                }
                upvar value_index val ; set val [lindex $args [incr i]]
            }
            -option_index {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -option_index"
                }
                upvar option_index val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { stmt_name sql } $n_args_remaining]"
    }
    upvar stmt_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar sql val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_close_html_tags {html_fragment {break_soft 0} {break_hard 0}} {
    set frag $html_fragment 

    set syn(A) nobr
    set syn(ADDRESS) nobr
    set syn(NOBR) nobr
    #
    set syn(FORM) discard
    #
    set syn(BLINK) remove 
    #
    set syn(TABLE) close
    set syn(FONT) close
    set syn(B) close
    set syn(BIG) close
    set syn(I) close
    set syn(S) close
    set syn(SMALL) close
    set syn(STRIKE) close
    set syn(SUB) close
    set syn(SUP) close
    set syn(TT) close
    set syn(U) close
    set syn(ABBR) close
    set syn(ACRONYM) close
    set syn(CITE) close
    set syn(CODE) close
    set syn(DEL) close
    set syn(DFN) close
    set syn(EM) close
    set syn(INS) close
    set syn(KBD) close
    set syn(SAMP) close
    set syn(STRONG) close
    set syn(VAR) close
    set syn(DIR) close
    set syn(DL) close
    set syn(MENU) close
    set syn(OL) close
    set syn(UL) close
    set syn(H1) close
    set syn(H2) close
    set syn(H3) close
    set syn(H4) close
    set syn(H5) close
    set syn(H6) close
    set syn(BDO) close
    set syn(BLOCKQUOTE) close
    set syn(CENTER) close
    set syn(DIV) close
    set syn(PRE) close
    set syn(Q) close
    set syn(SPAN) close

    set out {} 
    set out_len 0

    # counts how deep we are nested in nonbreaking tags, tracks the nobr point
    # and what the nobr string length would be
    set nobr 0
    set nobr_out_point 0
    set nobr_tagptr 0
    set nobr_len 0

    set discard 0

    set tagptr -1

    # first thing we do is chop off any trailing unclosed tag 
    # since when we substr blobs this sometimes happens
    
    # this should in theory cut any tags which have been cut open.
    while {[regexp {(.*)<[^>]*$} $frag match frag]} {}

    while { "$frag" != "" } {
        # here we attempt to cut the string into "pretag<TAG TAGBODY>posttag"
        # and build the output list.

        if {![regexp "(\[^<]*)(<(/?)(\[^ \r\n\t>]+)(\[^>]*)>)?(.*)" $frag match pretag fulltag close tag tagbody frag]} {
            # should never get here since above will match anything.
            # puts "NO MATCH: should never happen! frag=$frag"
            append out $frag 
            set frag {}
        } else {
            # puts "\n\nmatch=$match\n pretag=$pretag\n fulltag=$fulltag\n close=$close\n tag=$tag\n tagbody=$tagbody\nfrag=$frag\n\n"
            if { ! $discard } {
                # figure out if we can break with the pretag chunk 
                if { $break_soft } {
                    if {! $nobr && [expr [string length $pretag] + $out_len] > $break_soft } {
                        # first chop pretag to the right length
                        set pretag [string range $pretag 0 [expr $break_soft - $out_len]]
                        # clip the last word
                        regsub "\[^ \t\n\r]*$" $pretag {} pretag
                        append out [string range $pretag 0 $break_soft]
                        break
                    } elseif { $nobr &&  [expr [string length $pretag] + $out_len] > $break_hard } {
                        # we are in a nonbreaking tag and are past the hard break
                        # so chop back to the point we got the nobr tag...
                        set tagptr $nobr_tagptr 
                        if { $nobr_out_point > 0 } { 
                            set out [string range $out 0 [expr $nobr_out_point - 1]]
                        } else { 
                            # here maybe we should decide if we should keep the tag anyway 
                            # if zero length result would be the result...
                            set out {}
                        }
                        break
                    } 
                }
                
                # tack on pretag
                append out $pretag
                incr out_len [string length $pretag]
            }
            
            # now deal with the tag if we got one...
            if  { $tag == "" } { 
                # if the tag is empty we might have one of the bad matched that are not eating 
                # any of the string so check for them 
                if {[string length $match] == [string length $frag]} { 
                    append out $frag
                    set frag {}
                }
            } else {
                set tag [string toupper $tag]            
                if { ![info exists syn($tag)]} {
                    # if we don't have an entry in our syntax table just tack it on 
                    # and hope for the best.
                    if { ! $discard } {
                        append  out $fulltag
                    }
                } else {
                    if { $close != "/" } {
                        # new tag 
                        # "remove" tags are just ignored here
                        # discard tags 
                        if { $discard } { 
                            if { $syn($tag) == "discard" } {
                                incr discard 
                                incr tagptr 
                                set tagstack($tagptr) $tag
                            }
                        } else {
                            switch $syn($tag) {
                                nobr { 
                                    if { ! $nobr } {
                                        set nobr_out_point [string length $out]
                                        set nobr_tagptr $tagptr
                                        set nobr_len $out_len
                                    }
                                    incr nobr
                                    incr tagptr 
                                    set tagstack($tagptr) $tag
                                    append out $fulltag
                                }
                                discard { 
                                    incr discard 
                                    incr tagptr 
                                    set tagstack($tagptr) $tag
                                }
                                close {                                 
                                    incr tagptr 
                                    set tagstack($tagptr) $tag
                                    append out $fulltag
                                }
                            }
                        }
                    } else { 
                        # we got a close tag
                        if { $discard } { 
                            # if we are in discard mode only watch for 
                            # closes to discarded tags
                            if { $syn($tag) == "discard"} {
                                if {$tagptr > -1} {
                                    if { $tag != $tagstack($tagptr) } {
                                        #puts "/$tag without $tag"
                                    } else {
                                        incr tagptr -1
                                        incr discard -1
                                    }
                                }
                            }
                        } else {
                            if { $syn($tag) != "remove"} {
                                # if tag is a remove tag we just ignore it...
                                if {$tagptr > -1} {
                                    if {$tag != $tagstack($tagptr) } {
                                        # puts "/$tag without $tag"
                                    } else {
                                        incr tagptr -1
                                        if { $syn($tag) == "nobr"} {
                                            incr nobr -1
                                        } 
                                        append out $fulltag
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    # on exit of the look either we parsed it all or we truncated. 
    # we should now walk the stack and close any open tags.

    for {set i $tagptr} { $i > -1 } {incr i -1} { 
        # append out "<!-- autoclose --> </$tagstack($i)>"
        append out "</$tagstack($i)>"
    }
    
    return $out
}
proc db_exec_plsql {statement_name sql args} {

    ad_arg_parser { bind_output bind } $args

    # I'm not happy about having to get the fullname here, but right now
    # I can't figure out a cleaner way to do it. I will have to
    # revisit this ASAP. (ben)
    set full_statement_name [db_qd_get_fullname $statement_name]

    if { [info exists bind_output] } {
	return -code error "the -bind_output switch is not currently supported"
    }

    # plsql calls that are simple selects bypass the plpgsql 
    # mechanism for creating anonymous functions (OpenACS - Dan).
    # if a table is being created, we need to bypass things, too (OpenACS - Ben).
    set db ""
    set test_sql [db_qd_replace_sql $full_statement_name $sql]
    if {[regexp -nocase -- {^\s*select} $test_sql match]} {
	db_qd_log Debug "PLPGSQL: bypassed anon function"
	set selection [db_exec 0or1row $db $full_statement_name $sql]
    } elseif {[regexp -nocase -- {^\s*create table} $test_sql match] || [regexp -nocase -- {^\s*drop table} $test_sql match]} {
	db_qd_log Debug "PLPGSQL: bypassed anon function -- create/drop table"
	set selection [db_exec dml $db $full_statement_name $sql]
	return ""
    } else {
	db_qd_log Debug "PLPGSQL: using anonymous function"
	set selection [db_exec_plpgsql $db $full_statement_name $sql  $statement_name]
    }
    return [ns_set value $selection 0]
}
proc db_qd_internal_parse_one_query_from_xml_node {one_query_node {default_rdbms {}} {file_path {}}} {
    db_qd_log Debug "parsing one query node in XML with name -[xml_node_get_name $one_query_node]-"

    # Check that this is a fullquery
    if {[xml_node_get_name $one_query_node] != "fullquery"} {
	return ""
    }
    
    set queryname [xml_node_get_attribute $one_query_node name]

    # Get the text of the query
    set querytext [xml_node_get_content [xml_node_get_first_child_by_name $one_query_node querytext]]

    # Get the RDBMS
    set rdbms_nodes [xml_node_get_children_by_name $one_query_node rdbms]
    
    # If we have no RDBMS specified, use the default
    if {[llength $rdbms_nodes] == 0} {
	db_qd_log Debug "Wow, Nelly, no RDBMS for this query, using default rdbms $default_rdbms"
	set rdbms $default_rdbms
    } else {
	set rdbms_node [lindex $rdbms_nodes 0]
	set rdbms [db_rdbms_parse_from_xml_node $rdbms_node]
    }

    return [db_fullquery_create $queryname $querytext [list] "" $rdbms $file_path]
}
proc ad_header_with_extra_stuff__arg_parser {} {    upvar args args
    upvar extra_stuff_for_document_head val ; set val {}
    upvar focus val ; set val {}
    upvar pre_content_html val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -focus {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -focus"
                }
                upvar focus val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { page_title } $n_args_remaining]"
    }
    upvar page_title val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar extra_stuff_for_document_head val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        upvar pre_content_html val ; set val [lindex $args [expr { $i + 2 }]]
    }
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc set_difference! {set_name set} {
    upvar 1 $set_name list
    set result {}
    foreach element $list {
        if {[lsearch -exact $set $element] == -1} {
            lappend result $element
        }
    }
    return [set list $result]
}
proc ad_parameter_cache args {    ad_parameter_cache__arg_parser

    if {$delete_p} {
	if {[nsv_exists ad_param_$package_id $parameter_name]} {
	    nsv_unset ad_param_$package_id $parameter_name
	}
	return
    }
    if {[info exists set]} {
	nsv_set "ad_param_$package_id" $parameter_name $set
	return $set
    } elseif { [nsv_exists ad_param_$package_id $parameter_name] } {
	return [nsv_get ad_param_$package_id $parameter_name]
    } else {
        ns_log Warning "APM: $parameter_name does not exist"
	return ""
    }
}
proc ad_require_permission {object_id privilege} {
  set user_id [ad_verify_and_get_user_id]
  if {![ad_permission_p $object_id $privilege]} {
    if {$user_id == 0} {
      ad_maybe_redirect_for_registration
    } else {
      ns_log Notice "$user_id doesn't have $privilege on object $object_id"
      ad_return_forbidden "Security Violation" "<blockquote>
      You don't have permission to $privilege [db_string name {select acs_object.name(:object_id) from dual}].
      <p>
      This incident has been logged.
      </blockquote>"
    }
    ad_script_abort
  }
}
proc ad_page_contract_set_validation_passed key {
    global ad_page_contract_validations_passed
    set ad_page_contract_validations_passed($key) 1
}
proc ad_verify_signature args {    ad_verify_signature__arg_parser

    set token_id [lindex $signature 0]
    set expire_time [lindex $signature 1]
    set hash [lindex $signature 2]

    return [__ad_verify_signature $value $token_id $secret $expire_time $hash]

}
proc swap {x y} {
    upvar 1 $x foo $y bar
 
    if {![info exists foo]} {
        error "$x does not exist"
    }
 
    if {![info exists bar]} {
        error "$y does not exist"
    }
 
    if {[array exists foo] != [array exists bar]} {
        error "$x and $y must both be scalars or both be arrays"
    }
 
    if {![array exists foo]} {
        set tmp $bar
        set bar $foo
        set foo $tmp
    } else {
        array set tmp [array get bar]
        unset bar
        array set bar [array get foo]
        unset foo
        array set foo [array get tmp]
    }
 
    return
}
proc ad_page_contract_filter_invoke {filter name value_varname {parameters {}}} {
    upvar $value_varname value
    if { [empty_string_p $parameters] } {
	set filter_result [[ad_page_contract_filter_proc $filter] $name value]
    } else {
	set filter_result [[ad_page_contract_filter_proc $filter] $name value $parameters]
    }
    if { $filter_result } {
	ad_page_contract_set_validation_passed $name:$filter
    }
    return $filter_result
}
proc ad_page_contract args {    ad_page_contract__arg_parser

    ad_complaints_init

    ####################
    #
    # Parse arguments
    #
    ####################


    if { [llength $args] == 0 } {
	set query [list]
    } else {
	
	set valid_args { validate errors return_errors properties }; 	# add type later

	# If the first arg isn't a switch, it should be the query
	if { [string index [lindex $args 0] 0] != "-" } {
	    set query [lindex $args 0]
	    set args [lrange $args 1 end]
	} else {
	    # otherwise, accept a -query argument
	    lappend valid_args query
	    set query [list]
	}
	
	ad_arg_parser $valid_args $args
    }

    if {[info exists type] && [info exists properties]} {
	return -code error "You can't have both a -type and a -properties argument with ad_page_contract"
    }

    ####################
    # 
    #   Check supplied query form and set up variables in caller's environment
    # 
    ####################
    #
    # These are the steps:
    # 1. go over the formal args, massaging it into an internal data structure that's easier to manipulate
    # 2. go over the form (actual args), match actual to formal args, apply filters
    # 3. go over the formal args again: defaulting, post filters, complain if required but not supplied
    # 4. execute the validation blocks
    #
    ####################


    ####################
    #
    # Step 1: Massage the query arg into some useful data structure.
    #
    ####################
    # BASIC STUFF:
    # list apc_formals                list of formals in the order specified by in the arguments
    # array apc_formal($name)         1 if there is a formal argument with that name
    # array apc_default_value($name)  the default value, if any
    # 
    # FILTERS:
    # array apc_internal_filter($name:$flag):        1 if the given flag is set, undefined
    # array apc_filters($name):                      contains a list of the filters to apply
    # array apc_post_filters($name):                 contains a list of the post filters to apply
    # array apc_filter_parameters($name:$flag:):      contains a list of the parameters for a filter
    #
    # DOCUMENTATION:
    # array apc_flags($name):         contains a list of the flags that apply
    #
    
    set apc_formals [list]
    array set apc_formal [list]
    array set apc_default_value [list]

    array set apc_internal_filter [list]
    array set apc_filters [list]
    array set apc_post_filters [list]
    array set apc_filter_parameters [list]

    array set apc_flags [list]

    foreach element $query {
	set element_len [llength $element]

	if { $element_len > 2 } {
	    return -code error "Argspec '$element' is invalid, because it contains more than two elements."
	}

	set arg_spec [lindex $element 0]

	if { ![regexp {^([^ \t:]+)(?::([a-zA-Z0-9_,(|)]*))?$} $arg_spec match name flags] } {
	    return -code error "Argspec '$arg_spec' doesn't have the right format. It must be var\[:flag\[,flag ...\]\]"
	}
	
	lappend apc_formals $name
        set apc_formal($name) 1
	     
	if { $element_len == 2 } {
	    set apc_default_value($name) [lindex $element 1]
	} 

	set pre_flag_list [split [string tolower $flags] ,]
	set flag_list [list]

	# find parameterized flags
	foreach flag $pre_flag_list {
	    set left_paren [string first "(" $flag]
	    if { $left_paren == -1 } {
		lappend flag_list $flag
	    } else {
		if { ![string equal [string index $flag end] ")"] } {
		    return -code error "Missing or misplaced end parenthesis for flag '$flag' on argument '$name'"
		}
		set flag_parameters [string range $flag [expr $left_paren + 1] [expr [string length $flag]-2]]
		set flag [string range $flag 0 [expr $left_paren - 1]]

		lappend flag_list $flag
		foreach flag_parameter [split $flag_parameters "|"] {
		    lappend apc_filter_parameters($name:$flag) $flag_parameter
		}
	    }
	}

	#
	# Apply filter rules
	#

	foreach filter_rule [nsv_array names ad_page_contract_filter_rules] { 
	    [ad_page_contract_filter_rule_proc $filter_rule] $name flag_list
	}

	# 
	# Sort the flag list according to priority
	#

	set flag_list_for_sorting [list]
	foreach flag $flag_list {
	    lappend flag_list_for_sorting [list [ad_page_contract_filter_priority $flag] $flag]
	}
	set flag_list_sorted [lsort -index 0 $flag_list_for_sorting]

	#
	# Split flag_list up into the different kinds, i.e. internal, filter (classic) or post_filter.
	#
	# apc_flags($name) is for documentation only.
	#

	set apc_flags($name) [list]
	set apc_filters($name) [list]
	set apc_post_filters($name) [list]

	foreach flag_entry $flag_list_sorted {
	    set flag [lindex $flag_entry 1]
	    lappend apc_flags($name) $flag
	    
	    switch [ad_page_contract_filter_type $flag] {
		internal {
		    set apc_internal_filter($name:$flag) 1
		}
		filter {
		    lappend apc_filters($name) $flag
		}
		post {
		    lappend apc_post_filters($name) $flag
		}
		default {
		    return -code error "Unrecognized flag or filter \"$flag\" specified for query argument $name"
		}
	    }
	}
    }

    ####################
    #
    # Documentation-gathering mode
    #
    ####################

    if { [api_page_documentation_mode_p] } {
	# Just gather documentation for this page

	ad_parse_documentation_string $docstring doc_elements

	# copy all the standard elements over
	foreach element { query type properties } {
	    if { [info exists $element] } {
		set doc_elements($element) [set $element]
	    }
	}
	# then the arrays
	foreach element { apc_default_value apc_flags } {
	    set doc_elements($element) [array get $element]
	}
	# then the array names
	set doc_elements(apc_arg_names) $apc_formals
	
	# figure out where the calling script is located, relative to the ACS root
	set root_dir [nsv_get acs_properties root_directory]
	set script [info script]
	set root_length [string length $root_dir]
	if { ![string compare $root_dir [string range $script 0 [expr { $root_length - 1 }]]] } {
	    set script [string range $script [expr { $root_length + 1 }] end]
	}

	error [array get doc_elements] "ad_page_contract documentation"
    }

    #
    # Page serving mode
    #

    ####################
    #
    # Parse -properties argument
    #
    ####################
    # This must happen even if the query (aka parameters, formals) is empty

    ns_log Debug "jjs ... looking for properties" 
    if { [info exists properties] } {
	upvar 1 __page_contract_property property
	array set property [doc_parse_property_string $properties]
#	ns_log Debug "jjs.. 1) $property ... "
	ns_log Debug "jjs.. 1a) [array names property] "
	ns_log Debug "jjs.. 2) [doc_parse_property_string $properties] "
	
    }

    # If there are no query arguments to process, we're done
    if { ![info exists query] || [empty_string_p $query] } {
	return
    }

    ####################
    #
    # Parse -validate block
    #
    ####################
    #
    # array apc_validation_blocks($name): an array of lists that contain the validation blocks
    #                                    the list will contain either 1 or 2 elements, a possible
    #                                    list of required completed filters/blocks and the code block
    #                                    for the validation block. Once the block has executed, this entry
    #                                    self destructs, i.e. unset apc_validation_blocks($name)

    array set apc_validation_blocks [list]

    if { ![info exists validate] } {
	set validate [list]
    }

    set validate_len [llength $validate]
    for { set i 0 } { $i < $validate_len } { incr i } {
	set name [lindex $validate $i]

	if { [string first : $name] != -1 } {
	    return -code error "Validation block names cannot contain colon"
	}
	if { [info exists apc_formal($name)] } {
	    return -code error "You can't name your validation blocks the same as a formal argument"
	}
	if { [info exists apc_validation_blocks($name)] } {
	    return -code error "You can't have two validation blocks named '$name'"
	}

	incr i
	if { [string index [lindex $validate $i] 0] == "-" } {
	    if { ![string equal [lindex $validate $i] -requires] } {
		return -code error "Valid switches are: -requires"
	    }
	    set requires [lindex $validate [incr i]]

	    foreach element $requires {
		if { [string first , $element] != -1 } {
		    return -code error "The -requires element \"$element\" has a comma in it."
		}
		set parts_v [split $element ":"]
		set parts_c [llength $parts_v]
		if { $parts_c > 2 }  {
		    return -code error "The -requires element \"$element\" has too many colons"
		}
		set req_filter [lindex $parts_v 1]
		if { [string equal $req_filter array] || [string equal $req_filter multiple] } {
		    return -code error "You can't require \"$req_name:$req_filter\" for block \"$name\"."
		}
	    }
	    incr i
	} else {
	    set requires [list]
	}
	set code [lindex $validate $i]
	set apc_validation_blocks($name) [list $requires $code]
    }

    ####################
    #
    # Parse -errors argument
    #
    ####################

    if { [info exists errors] } {
	ad_complaints_parse_error_strings $errors
    }

    ####################
    #
    # Step 2: Go through all the actual arguments supplied in the form
    # 
    ####################

    set form [ns_getform]
    
    if { [empty_string_p $form] } {
	set form_size 0
    } else {
	set form_size [ns_set size $form]
    }

    # This is the array in which we store the signature variables as we come across them
    # Whenever we see a variable named foo:sig, we record it here as apc_signatures(foo).
    array set apc_signatures [list]
	
    for { set form_counter_i 0 } { $form_counter_i < $form_size } { incr form_counter_i } {
	
	#
	# Map actual argument to formal argument ... only complication is from arrays
	#


	# The name of the argument passed in the form
	set actual_name [ns_set key $form $form_counter_i]

	# The name of the formal argument in the page
	set formal_name $actual_name

	# This will be var(key) for an array
	set variable_to_set var

	# This is the value	
	set actual_value [ns_set value $form $form_counter_i]

	# It may be a signature for another variable
	if { [regexp {^(.*):sig$} $actual_name match formal_name] } {
	    set apc_signatures($formal_name) $actual_value
	    # We're done with this variable
	    continue
	}
	
        # If there is no formal with this name, _or_ the formal that has this name is an array, 
        # in which case it can't be the right formal, since we'd have to have a dot and then the key
	if { ![info exists apc_formal($formal_name)] || [info exists apc_internal_filter($formal_name:array)] } {

	    # loop over all the occurrences of dot in the argument name
	    # and search for a variable spec with that name, e.g.
	    # foo.bar.greble can be interpreted as foo(bar.greble) or foo.bar(greble)
	    set found_p 0
	    set actual_name_v [split $actual_name "."]
	    set actual_name_c [expr { [llength $actual_name_v] - 1 }]
	    for { set i 0 } { $i < $actual_name_c } { incr i } {
		set formal_name [join [lrange $actual_name_v 0 $i] "."]
		if { [info exists apc_internal_filter($formal_name:array)] } {
		    set found_p 1
		    set variable_to_set var([join [lrange $actual_name_v [expr $i+1] end] "."])
		    break
		}
	    }
	    if { !$found_p } {
		# The user supplied a value for which we didn't have any arg_spec
		# It might be safest to fail completely in this case, but for now, 
		# we just ignore it and go on with the next arg
		continue
	    }
	}
	
	# Remember that we've found the spec so we don't complain that argument is missing
	ad_page_contract_set_validation_passed $formal_name

	#
	# Apply filters
	#

	if { [info exists apc_internal_filter($formal_name:trim)] } {
	    set actual_value [string trim $actual_value]
	    ad_page_contract_set_validation_passed $formal_name:trim
	}

	if { [empty_string_p $actual_value] } {
	    if { [info exists apc_internal_filter($formal_name:notnull)] } {
		ad_complain -key $formal_name:notnull "You must specify something for $formal_name"
		continue
	    } else { 
		ad_page_contract_set_validation_passed $formal_name:notnull
	    }
	} else {
	    global ad_page_contract_errorkeys ad_page_contract_validations_passed
	    set ad_page_contract_validations_passed($formal_name:notnull) 1

		foreach filter $apc_filters($formal_name) {
		    set ad_page_contract_errorkeys [concat $formal_name:$filter $ad_page_contract_errorkeys]
		    if { ![info exists apc_filter_parameters($formal_name:$filter)] } {
			set filter_ok_p [[ad_page_contract_filter_proc $filter] $formal_name actual_value]
		    } else {
			set filter_ok_p [[ad_page_contract_filter_proc $filter] $formal_name actual_value  $apc_filter_parameters($formal_name:$filter)]
		    }
		    set ad_page_contract_errorkeys [lrange $ad_page_contract_errorkeys 1 end]
		    
		    if { $filter_ok_p } {
			set ad_page_contract_validations_passed($formal_name:$filter) 1
		    } else {
			break
		    }
		}
	}

	#
	# Set the variable in the caller's environment
	#
	
	upvar 1 $formal_name var
	
	if { [info exists apc_internal_filter($formal_name:multiple)] } {
	    lappend $variable_to_set $actual_value
	} else {
	    if { [info exists $variable_to_set] } {
		ad_complain -key $formal_name:-doublevalue "You've supplied two values for '$formal_name'"
		continue
	    } else {
		set $variable_to_set $actual_value
	    }
	}
    }
    
    ####################
    # 
    # Step 3: Pass over each formal argument to make sure all the required
    # things are there, and setting defaults if they're provided, 
    # apply post filters, and validate signatures.
    #
    ####################

    foreach formal_name $apc_formals {
	
	upvar 1 $formal_name var
	
	if { [ad_page_contract_get_validation_passed_p $formal_name] } {
	    
	    if { [info exists apc_internal_filter($formal_name:verify)] } {
		if { ![info exists apc_internal_filter($formal_name:array)] } {
 		    # This is not an array, verify the variable
		    if { ![info exists apc_signatures($formal_name)] ||  ![ad_verify_signature $var $apc_signatures($formal_name)] } {
			ad_complain -key $formal_name:verify "The signature for the variable '$formal_name' was incorrect."
			continue
		    }
		} else {
		    # This is an array: verify the [array get] form of the array
		    if { ![info exists apc_signatures($formal_name)] ||  ![ad_verify_signature [lsort [array get var]] $apc_signatures($formal_name)] } {
			ad_complain -key $formal_name:verify "The signature for the variable '$formal_name' was incorrect."
			continue
		    }
		}
	    }

	    # Apply post filters
	    if { [info exists var] } {
		foreach filter $apc_post_filters($formal_name) {
		    ad_complaints_with_key $formal_name:$filter {
			if { ![info exists apc_filter_parameters($formal_name:$filter)] } {
			    set filter_ok_p [[ad_page_contract_filter_proc $filter] $formal_name var]
			} else {
			    set filter_ok_p [[ad_page_contract_filter_proc $filter] $formal_name var $apc_filter_parameters($formal_name:$filter)]
			}
		    }
		    if { $filter_ok_p } {
			ad_page_contract_set_validation_passed $formal_name:$filter
		    } else { 
			break
		    }
		}
	    }
	    
	} else {
	    
	    # no value supplied for this arg spec
	    
	    if { [info exists apc_default_value($formal_name)] } {
		
		# Only use the default value if there has been no complaints so far
		# Why? Because if there are complaints, the page isn't going to serve anyway,
		# and because one default value may depend on another variable having a correct value.
		if { [ad_complaints_count] == 0 } {
		    # we need to set the default value
		    if { [info exists apc_internal_filter($formal_name:array)] } {
			array set var [uplevel subst \{$apc_default_value($formal_name)\}]
		    } else {
			set var [uplevel subst \{$apc_default_value($formal_name)\}]
		    }
		}
		
	    } elseif { ![info exists apc_internal_filter($formal_name:optional)] } {
		ad_complain -key $formal_name "You must supply a value for $formal_name"
	    }
	}
    }

    ####################
    #
    # Step 4: Execute validation blocks 
    #
    ####################

    set done_p 0
    global ad_page_contract_validations_passed ad_page_contract_errorkeys
    while { !$done_p } {

	set done_p 1
	foreach validation_name [array names apc_validation_blocks] {
	    set dependencies [lindex $apc_validation_blocks($validation_name) 0]
	    set code [lindex $apc_validation_blocks($validation_name) 1]

	    set dependencies_met_p 1
	    foreach dependency $dependencies {
		if { ![info exists ad_page_contract_validations_passed($dependency)] } {
		    set dependencies_met_p 0
		    break
		}
	    }

	    if { $dependencies_met_p } {

		# remove from validation blocks array, so we don't execute the same block twice
		unset apc_validation_blocks($validation_name)

		set no_complaints_before [ad_complaints_count]

		# Execute the validation block with an environment with a default error key set
		set ad_page_contract_errorkeys [concat $validation_name $ad_page_contract_errorkeys]
		set validation_ok_p [ad_page_contract_eval uplevel 1 $code]
		set ad_page_contract_errorkeys [lrange $ad_page_contract_errorkeys 1 end]

		if { [empty_string_p $validation_ok_p] ||  (![string equal $validation_ok_p 1] && ![string equal $validation_ok_p 0])} {
		    set validation_ok_p [expr [ad_complaints_count] == $no_complaints_before]
		}
		
		if { $validation_ok_p } {
		    set ad_page_contract_validations_passed($validation_name) 1
		    # more stuff to process still
		    set done_p 0
		}
		    
	    }
	}
    }

    ####################
    #
    # Done. Spit out error, if any
    #
    ####################

    # Initialize the list of page variables for other scripts to use
    global ad_page_contract_variables
    set ad_page_contract_variables $apc_formals

    if { [ad_complaints_count] > 0 } {
	if { [info exists return_errors] } {
	    upvar 1 $return_errors error_list
	    set error_list [ad_complaints_get_list]
	} else {
	    ad_return_complaint [ad_complaints_count] "<li>[join [ad_complaints_get_list] "\n<li>"]"
	    ad_script_abort
	}
    }
}
proc apm_version_info version_id {

    uplevel 1 {
	db_1row apm_package_by_version_id {
	    select pretty_name, version_name, package_key, installed_p, distribution_uri, tagged_p
	    from apm_package_version_info where version_id = :version_id
	}
    } 
}
proc server_cluster_enabled_p {} {
    return [ad_parameter -package_id [ad_acs_kernel_id] ClusterEnabledP server-cluster 0]
}
proc unknown args {
    global auto_noexec auto_noload env unknown_pending tcl_interactive
    global errorCode errorInfo

    # If the command word has the form "namespace inscope ns cmd"
    # then concatenate its arguments onto the end and evaluate it.

    set cmd [lindex $args 0]
    if {[regexp "^namespace\[ \t\n\]+inscope" $cmd] && [llength $cmd] == 4} {
        set arglist [lrange $args 1 end]
	set ret [catch {uplevel 1 ::$cmd $arglist} result]
        if {$ret == 0} {
            return $result
        } else {
	    return -code $ret -errorcode $errorCode $result
        }
    }

    # Save the values of errorCode and errorInfo variables, since they
    # may get modified if caught errors occur below.  The variables will
    # be restored just before re-executing the missing command.

    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo
    set name [lindex $args 0]
    if {![info exists auto_noload]} {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if {[info exists unknown_pending($name)]} {
	    return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [catch {auto_load $name [uplevel 1 {::namespace current}]} msg]
	unset unknown_pending($name);
	if {$ret != 0} {
	    append errorInfo "\n    (autoloading \"$name\")"
	    return -code $ret -errorcode $errorCode -errorinfo $errorInfo $msg
	}
	if {![array size unknown_pending]} {
	    unset unknown_pending
	}
	if {$msg} {
	    set errorCode $savedErrorCode
	    set errorInfo $savedErrorInfo
	    set code [catch {uplevel 1 $args} msg]
	    if {$code ==  1} {
		#
		# Strip the last five lines off the error stack (they're
		# from the "uplevel" command).
		#

		set new [split $errorInfo \n]
		set new [join [lrange $new 0 [expr {[llength $new] - 6}]] \n]
		return -code error -errorcode $errorCode  -errorinfo $new $msg
	    } else {
		return -code $code $msg
	    }
	}
    }

    if {([info level] == 1) && [string equal [info script] ""]  && [info exists tcl_interactive] && $tcl_interactive} {
	if {![info exists auto_noexec]} {
	    set new [auto_execok $name]
	    if {[string compare {} $new]} {
		set errorCode $savedErrorCode
		set errorInfo $savedErrorInfo
		set redir ""
		if {[string equal [info commands console] ""]} {
		    set redir ">&@stdout <@stdin"
		}
		return [uplevel 1 exec $redir $new [lrange $args 1 end]]
	    }
	}
	set errorCode $savedErrorCode
	set errorInfo $savedErrorInfo
	if {[string equal $name "!!"]} {
	    set newcmd [history event]
	} elseif {[regexp {^!(.+)$} $name dummy event]} {
	    set newcmd [history event $event]
	} elseif {[regexp {^\^([^^]*)\^([^^]*)\^?$} $name dummy old new]} {
	    set newcmd [history event -1]
	    catch {regsub -all -- $old $newcmd $new newcmd}
	}
	if {[info exists newcmd]} {
	    tclLog $newcmd
	    history change $newcmd 0
	    return [uplevel 1 $newcmd]
	}

	set ret [catch {set cmds [info commands $name*]} msg]
	if {[string equal $name "::"]} {
	    set name ""
	}
	if {$ret != 0} {
	    return -code $ret -errorcode $errorCode  "error in unknown while checking if \"$name\" is a unique command abbreviation: $msg"
	}
	if {[llength $cmds] == 1} {
	    return [uplevel 1 [lreplace $args 0 0 $cmds]]
	}
	if {[llength $cmds]} {
	    if {[string equal $name ""]} {
		return -code error "empty command name \"\""
	    } else {
		return -code error  "ambiguous command name \"$name\": [lsort $cmds]"
	    }
	}
    }
    return -code error "invalid command name \"$name\""
}
proc apm_subdirs path {
    set dirs [list]
    lappend dirs $path
    foreach subdir [glob -nocomplain -type d [file join $path *]] {
       set dirs [concat $dirs [apm_subdirs $subdir]]
    }
    return $dirs
}
proc db_column_exists {table_name column_name} {
    set columns [list]
    set n_rows [db_string column_exists {
	select count(*) 
	from user_tab_columns
	where table_name = upper(:table_name)
	and column_name = upper(:column_name)
    }]
    return [expr $n_rows > 0]
}
proc ad_return_template__arg_parser {} {    upvar args args
    upvar template val ; set val {}
    upvar string_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -string - -string=1 {
                uplevel set string_p 1
            }
            -string=0 {
                uplevel set string_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        upvar template val ; set val [lindex $args [expr { $i + 0 }]]
    }
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc auto_import pattern {
    global auto_index

    # If no namespace is specified, this will be an error case

    if {![string match *::* $pattern]} {
	return
    }

    set ns [uplevel 1 [list ::namespace current]]
    set patternList [auto_qualify $pattern $ns]

    auto_load_index

    foreach pattern $patternList {
        foreach name [array names auto_index $pattern] {
            if {[string equal "" [info commands $name]]
		    && [string equal [namespace qualifiers $pattern]  [namespace qualifiers $name]]} {
                uplevel #0 $auto_index($name)
            }
        }
    }
}
proc sec_session_timeout {} {
    return "600"
}
proc db_qd_load_query_file args {
	# no-op
    }
proc auto_execok name {
    global auto_execs env tcl_platform

    if {[info exists auto_execs($name)]} {
	return $auto_execs($name)
    }
    set auto_execs($name) ""

    set shellBuiltins [list cls copy date del erase dir echo mkdir  md rename ren rmdir rd time type ver vol]
    if {[string equal $tcl_platform(os) "Windows NT"]} {
	# NT includes the 'start' built-in
	lappend shellBuiltins "start"
    }
    if {[info exists env(PATHEXT)]} {
	# Add an initial ; to have the {} extension check first.
	set execExtensions [split ";$env(PATHEXT)" ";"]
    } else {
	set execExtensions [list {} .com .exe .bat]
    }

    if {[lsearch -exact $shellBuiltins $name] != -1} {
	# When this is command.com for some reason on Win2K, Tcl won't
	# exec it unless the case is right, which this corrects.  COMSPEC
	# may not point to a real file, so do the check.
	set cmd $env(COMSPEC)
	if {[file exists $cmd]} {
	    set cmd [file attributes $cmd -shortname]
	}
	return [set auto_execs($name) [list $cmd /c $name]]
    }

    if {[llength [file split $name]] != 1} {
	foreach ext $execExtensions {
	    set file ${name}${ext}
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) [list $file]]
	    }
	}
	return ""
    }

    set path "[file dirname [info nameof]];.;"
    if {[info exists env(WINDIR)]} {
	set windir $env(WINDIR) 
    }
    if {[info exists windir]} {
	if {[string equal $tcl_platform(os) "Windows NT"]} {
	    append path "$windir/system32;"
	}
	append path "$windir/system;$windir;"
    }

    foreach var {PATH Path path} {
	if {[info exists env($var)]} {
	    append path ";$env($var)"
	}
    }

    foreach dir [split $path {;}] {
	# Skip already checked directories
	if {[info exists checked($dir)] || [string equal {} $dir]} { continue }
	set checked($dir) {}
	foreach ext $execExtensions {
	    set file [file join $dir ${name}${ext}]
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) [list $file]]
	    }
	}
    }
    return ""
}
proc doc_parse_property_string properties {
    set property_array_list [list]
    
    set lines [split $properties \n]
    foreach line_raw $lines {
	set line [string trim $line_raw]
	if { [empty_string_p $line] } {
	    continue
	}
	
	if { ![regexp {^([^:]+)(?::([^(]+)(?:\(([^)]+)\))?)?$} $line  match name_raw type_raw columns] } {
	    return -code error  "Property doesn't have the right format, i.e. our regexp failed"
	}

	set name [string trim $name_raw]

	if { ![string is wordchar -strict $name] } {
	    return -code error "Property name $name contains characters that are not Unicode word characters, but we don't allow that."
	}

	if { [info exists type_raw] && ![empty_string_p $type_raw] } { 
	    set type [string trim $type_raw]
	} else {
	    set type onevalue
	}

	# The following statement will set "type_repr" to our internal
	# representation of the type of this property.
	switch -- $type {
	    onevalue - onelist - multilist { 
		set type_repr $type
	    }
	    onerow -
	    multirow {
		if { ![info exists columns] } {
		    return -code error "Columns not defined for $type type property $name"
		}
		set column_split [split $columns ","]
		set column_list [list]
		foreach column_raw $column_split {
		    set column [string trim $column_raw]
		    if { [empty_string_p $column] } {
			return -code error "You have an empty column name in the definition of the $property property in the type $type"
		    }
		    lappend column_list $column
		}
		set type_repr [list $type $column_list]
	    }
	    default {
		return -code error  "Unknown property type $type for property $name"
	    }
	}

	lappend property_array_list $name $type_repr
    }
    
    return $property_array_list
}
proc ad_ns_set_to_tcl_vars__arg_parser {} {    upvar args args
    upvar level val ; set val 1
    upvar duplicates val ; set val overwrite

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -duplicates {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -duplicates"
                }
                upvar duplicates val ; set val [lindex $args [incr i]]
            }
            -level {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -level"
                }
                upvar level val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { set_id } $n_args_remaining]"
    }
    upvar set_id val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_page_contract_filter_proc_time {name time_varname} {upvar $time_varname time

    foreach time_element { time ampm } {
	if { ![info exists time($time_element)] } {
	    ad_complain "Invalid time: $time_element is missing"
	    return 0
	}
    }
  
    # check if all elements are blank
    if { [empty_string_p "$time(time)$time(ampm)"] } {
	return 1
    } 

    set time_element_values [split $time(time) ":"]
    if { [llength $time_element_values] != 3 } {
	ad_complain "Invalid time: $time(time) is in invalid format"
	return 0
    }

    set time_element_names [list hours minutes seconds]

    for { set i 0 } { $i < 3 } { incr i } {
	array set time [list [lindex $time_element_names $i] [lindex $time_element_values $i]]
    }

    if {
	[string match "" $time(hours)]  || [string match "" $time(minutes)]  || [string match "" $time(seconds)]  || (![string equal -nocase "pm" $time(ampm)] && ![string equal -nocase "am" $time(ampm)])
	    || $time(hours) < 1 || $time(hours) > 12  || $time(minutes) < 0 || $time(minutes) > 59  || $time(seconds) < 0 || $time(seconds) > 59
    } {
	ad_complain "Invalid time: $time(time) $time(ampm)"
	return 0
    }

    return 1
}
proc db_qd_internal_store_cache fullquery {

    # Check if it's compatible at all!
    if {![db_rdbms_compatible_p [db_fullquery_get_rdbms $fullquery] [db_current_rdbms]]} {
	db_qd_log Debug "Query [db_fullquery_get_name $fullquery] is *NOT* compatible"
	return
    }

    set name [db_fullquery_get_name $fullquery]

    db_qd_log Debug "Query $name is compatible! fullquery = $fullquery, name = $name"

    # If we already have a query for that name, we need to
    # figure out which one is *most* compatible.
    if {[nsv_exists OACS_FULLQUERIES $name]} {
	set old_fullquery [nsv_get OACS_FULLQUERIES $name]

	set fullquery [db_qd_pick_most_specific_query [db_current_rdbms] $old_fullquery $fullquery]
    }

    nsv_set OACS_FULLQUERIES $name $fullquery
}
proc rp_report_error args {    rp_report_error__arg_parser

    if { ![info exists message] } {
	upvar #0 errorInfo message
    }

    set error_url [ad_conn url]

    if { [llength [info procs ds_collection_enabled_p]] == 1 && [ds_collection_enabled_p] } {
	ds_add conn error $message
    }

    if {![ad_parameter -package_id [ad_acs_kernel_id] "RestrictErrorsToAdminsP" dummy 0] ||  [ad_permission_p [ad_conn package_id] admin] } {
	if { [ad_parameter -package_id [ad_acs_kernel_id] "AutomaticErrorReportingP" "rp" 0] } { 
	    set error_info $message
	    set report_url [ad_parameter -package_id [ad_acs_kernel_id] "ErrorReportURL" "rp" ""]
	    if { [empty_string_p $report_url] } {
		ns_log "Automatic Error Reporting Misconfigured.  Please add a field in the acs/rp section of form ErrorReportURL=http://your.errors/here."
	    } else {
		set auto_report 1
		ns_returnerror 200 "</table></table></table></h1></b></i>
               <form method=POST action='$report_url'>
[export_form_vars error_url error_info]
This file has generated an error.  
<input type=submit value='Report this error'>
</form><hr>
	<blockquote><pre>[ns_quotehtml $error_info]</pre></blockquote>[ad_footer]"
	    }
	} else {
	    # No automatic report.
	    ns_returnerror 200 "</table></table></table></h1></b></i>
	<blockquote><pre>[ns_quotehtml $message]</pre></blockquote>[ad_footer]"
	}
    } else {
	ns_returnerror 200 "
      The server has encountered an internal server error. The error
      has been logged and will be investigated by our system
      programmers.
      "
    }
    ns_log Error "[ns_conn method] $error_url [ns_conn query] $message"
}
proc ad_call_method {method_name object_id args} {
    return [apply ${method_name}__[util_memoize "acs_object_type $object_id"] [concat $object_id $args]]
}
proc rp_report_error__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -message {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -message"
                }
                upvar message val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ReturnHeadersWithCookie {cookie_content {content_type text/html}} {

    ns_write "HTTP/1.0 200 OK
MIME-Version: 1.0
Content-Type: $content_type
Set-Cookie:  $cookie_content\r\n"

     ns_startcontent -type $content_type
}
proc ad_tcl_list_list_to_ns_set args {    ad_tcl_list_list_to_ns_set__arg_parser


    if { ![info exists set_id] } {
	set set_id [ns_set create]
    }

    if { $put_p } {
	set command put
    } else {
	set command update
    }

    foreach kv_pair $kv_pairs {
	ns_set $command $set_id [lindex $kv_pair 0] [lindex $kv_pair 1]
    }

    return $set_id
}
proc acs_community_member_admin_link__arg_parser {} {    upvar args args
    upvar label val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -user_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -user_id"
                }
                upvar user_id val ; set val [lindex $args [incr i]]
            }
            -label {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -label"
                }
                upvar label val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
    if { ![uplevel info exists user_id] } {
        return -code error "Required switch -user_id not provided"
    }
}
proc apm_dependency_add__arg_parser {} {    upvar args args
    upvar callback val ; set val apm_dummy_callback
    upvar dependency_id val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }
            -dependency_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -dependency_id"
                }
                upvar dependency_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { version_id dependency_uri dependency_version } $n_args_remaining]"
    }
    upvar version_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar dependency_uri val ; set val [lindex $args [expr { $i + 1 }]]
    upvar dependency_version val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc remove_nulls_from_ns_set old_set_id {

    set new_set_id [ns_set new "no_nulls$old_set_id"]

    for {set i 0} {$i<[ns_set size $old_set_id]} {incr i} {
	if { [ns_set value $old_set_id $i] != "" } {

	    ns_set put $new_set_id [ns_set key $old_set_id $i] [ns_set value $old_set_id $i]

	}

    }

    return $new_set_id

}
proc site_node_closest_ancestor_package_url args {    site_node_closest_ancestor_package_url__arg_parser

    set subsite_pkg_id [site_node_closest_ancestor_package $package_key]
    if { [empty_string_p $subsite_pkg_id] } {
	# No package was found... return the default
	return $default
    }
    return [db_string select_url {
	select site_node.url(node_id) from site_nodes where object_id=:subsite_pkg_id
    } -default ""]
}
proc ad_parse_html_attributes_upvar__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -attribute_array {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -attribute_array"
                }
                upvar attribute_array val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { html_varname pos_varname } $n_args_remaining]"
    }
    upvar html_varname val ; set val [lindex $args [expr { $i + 0 }]]
    upvar pos_varname val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_string {statement_name pre_sql args} {
#        ns_log info "db_string called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_string $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_string $statement_name $sql] $args]
    }
proc apm_dummy_callback string {
    # Do nothing!
}
proc util_escape_quotes_for_csv string {
    regsub -all {"} $string {\"}  result
    return $result
}
proc site_map_unmount_application args {    site_map_unmount_application__arg_parser

    db_dml unmount {
	update site_nodes
	   set object_id = null
	 where node_id = :node_id
    }

    if { [string eq $delete_p "t"] } {
	# Delete the node from the site map
	db_exec_plsql node_delete {
	    begin site_node.delete(:node_id); end;
	}	
    }

    if { [string eq $sync_p "t"] } {
	site_nodes_sync
    }
}
proc util_unlist {list args} {
    for { set i 0 } { $i < [llength $args] } { incr i } {
	upvar [lindex $args $i] val
	set val [lindex $list $i]
    }
}
proc ad_parameter_all_values_as_list__arg_parser {} {    upvar args args
    upvar package_id val ; set val {}
    upvar subsection val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -package_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_id"
                }
                upvar package_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar subsection val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_get_password {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    return [ns_config ns/db/pool/$pool Password]
}
proc ad_html_to_text_put_newline output_var {
    upvar $output_var output

    append output(text) \n
    set output(linelen) 0
    append output(text) [string repeat {    } $output(blockquote)]
}
proc ad_permission_p__arg_parser {} {    upvar args args
    upvar user_id val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -user_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -user_id"
                }
                upvar user_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { object_id privilege } $n_args_remaining]"
    }
    upvar object_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar privilege val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_find_all_files__arg_parser {} {    upvar args args
    upvar check_file_func val ; set val {}
    upvar include_dirs val ; set val 0
    upvar max_depth val ; set val 10

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -include_dirs {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -include_dirs"
                }
                upvar include_dirs val ; set val [lindex $args [incr i]]
            }
            -max_depth {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -max_depth"
                }
                upvar max_depth val ; set val [lindex $args [incr i]]
            }
            -check_file_func {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -check_file_func"
                }
                upvar check_file_func val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { path } $n_args_remaining]"
    }
    upvar path val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_source_sql_file args {    db_source_sql_file__arg_parser

    global tcl_platform 
    set file_name [file tail $file]

    set pguser [db_get_username]
    if { ![string equal $pguser ""] } {
	set pguser "-U $pguser"
    }

    set pgport [db_get_port]
    if { ![string equal $pgport ""] } {
	set pgport "-p $pgport"
    }

    set pgpass [db_get_password]
    if { ![string equal $pgpass ""] } {
	set pgpass "<<$pgpass"
    }

    # DRB: Submitted patch was in error - the driver opens a -h hostname connection
    # unless the hostname is localhost.   We need to do the same here.  The submitted
    # patch checked for a blank hostname, which fails in the driver.  Arguably the
    # driver's wrong but a lot of non-OpenACS folks use it, and even though I'm the
    # maintainer we shouldn't break existing code over such trivialities...

    if { [string equal [db_get_dbhost] "localhost"] || [string equal [db_get_dbhost] ""] } {
        set pghost ""
    } else {
	set pghost "-h [db_get_dbhost]"
    }

    cd [file dirname $file]
 
    if { $tcl_platform(platform) == "windows" } {
        set fp [open "|[file join [db_get_pgbin] psql] -h [ns_info hostname] $pgport $pguser -f $file_name [db_get_database]" "r"]
    } else {
        set fp [open "|[file join [db_get_pgbin] psql] $pghost $pgport $pguser -f $file_name [db_get_database] $pgpass" "r"]
    }

    while { [gets $fp line] >= 0 } {
 	# Don't bother writing out lines which are purely whitespace.
	if { ![string is space $line] } {
	    apm_callback_and_log $callback "[ad_quotehtml $line]\n"
	}
    }

    # PSQL dumps errors and notice information on stderr, and has no option to turn
    # this off.  So we have to chug through the "error" lines looking for those that
    # really signal an error.

    set errno [ catch {
        close $fp
    } error]

    if { $errno == 2 } {
	return $error
    }

    # Just filter out the "NOTICE" lines, so we get the stack dump along with real
    # ERRORs.  This could be done with a couple of opaque-looking regexps...

    set error_found 0
    foreach line [split $error "\n"] {
        if { [string first NOTICE $line] == -1 } {
            append error_lines "$line\n"
            set error_found [expr { $error_found || [string first ERROR $line] != -1 ||  [string first FATAL $line] != -1 } ]
        }
    }

    if { $error_found } {
        global errorCode
        return -code error -errorinfo $error_lines -errorcode $errorCode
    }
}
proc util_report_library_entry {{extra_message {}}} {
    set tentative_path [info script]
    regsub -all {/\./} $tentative_path {/} scrubbed_path
    if { [string compare $extra_message ""] == 0 } {
	set message "Loading $scrubbed_path"
    } else {
	set message "Loading $scrubbed_path; $extra_message"
    }
    ns_log Notice $message
}
proc ad_redirect_for_registration {} {
    set form [ns_getform] 
    set url_args ""
    
    # note that there is no built-in function that will change
    # posted variables to url variables, so we write our own
    
    if ![empty_string_p $form] {
	set form_size [ns_set size $form]
	set form_counter_i 0
	while { $form_counter_i<$form_size } {
	    if {[string compare $form_counter_i "0"] == 0} {
		append url_args "?"
	    } else {
		append url_args "&"
	    }
	    append url_args "[ns_set key $form $form_counter_i]=[ns_urlencode [ns_set value $form $form_counter_i]]"
	    incr form_counter_i
	}
    }
    ad_returnredirect "/register/?return_url=[ns_urlencode [ad_conn url]$url_args]"
    return
}
proc ad_get_signed_cookie_with_expr args {    ad_get_signed_cookie_with_expr__arg_parser


    if { $include_set_cookies == "t" } {
	set cookie_value [ns_urldecode [ad_get_cookie $name]]
    } else {
	set cookie_value [ns_urldecode [ad_get_cookie -include_set_cookies f $name]]
    }

    if { [empty_string_p $cookie_value] } {
	error "Cookie does not exist."
    }

    set value [lindex $cookie_value 0]
    set signature [lindex $cookie_value 1]

    set expr_time [ad_verify_signature_with_expr $value $signature]

    ns_log Debug "Security: Done calling get_cookie $cookie_value for $name; received $expr_time expiration, getting $value and $signature."

    if { $expr_time } {
	return [list $value $expr_time]
    }

    error "Cookie could not be authenticated."
}
proc ad_page_contract_filter_proc_date {name date_varname} {upvar $date_varname date

    foreach date_element { day month year } {
	if { ![info exists date($date_element)] } {
	    ad_complain "Invalid date: $date_element is missing"
	    return 0
	}
    }
  
    # check if all elements are blank
    if { [empty_string_p "$date(day)$date(month)$date(year)"] } {
	set date(date) {}
	return 1
    } 

    foreach date_element { day year } {
	if { ![regexp {^(0*)(([1-9][0-9]*|0))$} $date($date_element) match zeros real_value] } {
	    ad_complain "Invalid date: $date_element is not a natural number"
	    return 0
	}
	set date($date_element) $real_value
    }

    if { ![empty_string_p $date(year)] && [string length $date(year)] != 4 } {
	ad_complain "Invalid date: The year must contain 4 digits."
	return 0
    } 

    if { [regexp {^(0*)(([1-9][0-9]*|0))$} $date(month) match zeros real_value] } {
	set date(month) $real_value
    } else {
	set date(month) [expr [lsearch [nsv_get _nsdb months] $date(month)] + 1]
    }

    if {
	[string match "" $date(month)]  || [string match "" $date(day)]  || [string match "" $date(year)]  || $date(month) < 1 || $date(month) > 12  || $date(day) < 1 || $date(day) > 31  || $date(year) < 1  || ($date(month) == 2 && $date(day) > 29)  || (($date(year) % 4) != 0 && $date(month) == 2 && $date(day) > 28)  || ($date(month) == 4 && $date(day) > 30)  || ($date(month) == 6 && $date(day) > 30)  || ($date(month) == 9 && $date(day) > 30)  || ($date(month) == 11 && $date(day) > 30)
    } {
	ad_complain "Invalid date: $date(month) $date(day) $date(year)"
	return 0
    }

    set date(date) [format "%04d-%02d-%02d" $date(year) $date(month) $date(day)]
    return 1
}
proc __ad_verify_signature {value token_id secret expire_time hash} {

    if { [empty_string_p $secret] } {
	if { [empty_string_p $token_id] } {
	    return 0
	}
	set secret_token [sec_get_token $token_id]
    } else {
	set secret_token $secret
    }

    ns_log Debug "Security: Getting token_id $token_id, value $secret_token"
    ns_log Debug "Security: Expire_Time is $expire_time (compare to [ns_time]), hash is $hash"

    # validate cookie: verify hash and expire_time

    set computed_hash [ns_sha1 "$value$token_id$expire_time$secret_token"]

    if { [string compare $computed_hash $hash] == 0 && ($expire_time > [ns_time] || $expire_time == 0) } {
	return 1
    }

    # check to see if IE is lame (and buggy!) and is expanding \n to \r\n
    # See: http://www.arsdigita.com/bboard/q-and-a-fetch-msg?msg_id=000bfF
    set value [string map [list \r ""] $value]
    set computed_hash [ns_sha1 "$value$token_id$expire_time$secret_token"]

    if { [string compare $computed_hash $hash] == 0 && ($expire_time > [ns_time] || $expire_time == 0) } {
	return 1
    }


    ns_log Debug "Security: The string compare is [string compare $computed_hash $hash]."
    # signature could not be authenticated
    return 0

}
proc proc_doc args {
    eval ad_proc $args
}
proc doc_body_append str {
    global doc_properties
    append doc_properties(body) $str
}
proc ad_register_filter args {    ad_register_filter__arg_parser

    if { [string equal $method "*"] } {
	# Shortcut to allow registering filter for all methods.
	foreach method { GET POST HEAD } {
	    ad_register_filter -debug $debug -priority $priority -critical $critical $kind $method $path $proc $arg
	}
	return
    }

    if { [lsearch -exact { GET POST HEAD } $method] == -1 } {
	error "Method passed to ad_register_filter must be one of GET, POST, or HEAD"
    }

    # Append the filter to the list.
    nsv_lappend rp_filters .  [list $priority $kind $method $path $proc $arg $debug $critical $description [info script]]
}
proc safe_eval args {
    foreach arg $args {
	if { [regexp {[\[;]} $arg] } {
	    return -code error "Unsafe argument to safe_eval: $arg"
	}
    }
    return [apply uplevel $args]
}
proc start_of_day date {
    return [clock format [clock scan $date] -format "%Y-%m-%d 00:00:00"]
}
proc rp_handle_html_request {} {
    ad_serve_html_page [ad_conn file]
}
proc export_vars__arg_parser {} {    upvar args args
    upvar override val ; set val {}
    upvar sign_p val ; set val 0
    upvar exclude val ; set val {}
    upvar quotehtml_p val ; set val 0
    upvar url_p val ; set val 0
    upvar form_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sign - -sign=1 {
                uplevel set sign_p 1
            }
            -sign=0 {
                uplevel set sign_p 0
            }
            -form - -form=1 {
                uplevel set form_p 1
            }
            -form=0 {
                uplevel set form_p 0
            }
            -url - -url=1 {
                uplevel set url_p 1
            }
            -url=0 {
                uplevel set url_p 0
            }
            -quotehtml - -quotehtml=1 {
                uplevel set quotehtml_p 1
            }
            -quotehtml=0 {
                uplevel set quotehtml_p 0
            }
            -exclude {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -exclude"
                }
                upvar exclude val ; set val [lindex $args [incr i]]
            }
            -override {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -override"
                }
                upvar override val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { vars } $n_args_remaining]"
    }
    upvar vars val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_page_contract_filter_proc_time24 {name time_varname} {upvar $time_varname time

    if { ![info exists time(time)] } {
	ad_complain "Invalid time: time is missing"
	return 0
    }
  
    # check if all elements are blank
    if { [empty_string_p "$time(time)"] } {
	return 1
    } 

    set time_element_values [split $time(time) ":"]
    if { [llength $time_element_values] != 3 } {
	ad_complain "Invalid time: $time(time) is in invalid format"
	return 0
    }

    set time_element_names [list hours minutes seconds]

    for { set i 0 } { $i < 3 } { incr i } {
	array set time [list [lindex $time_element_names $i] [lindex $time_element_values $i]]
    }

    if {
	[string match "" $time(hours)]  || [string match "" $time(minutes)]  || [string match "" $time(seconds)]  || $time(hours) < 0 || $time(hours) > 23  || $time(minutes) < 0 || $time(minutes) > 59  || $time(seconds) < 0 || $time(seconds) > 59
    } {
	ad_complain "Invalid time: $time(time) $time(ampm)"
	return 0
    }

    return 1
}
proc ad_admin_present_user {user_id name} {
    return [acs_community_member_admin_link -user_id $user_id -label $name]
}
proc ad_admin_footer {} {
    if { [llength [info procs ds_link]] == 1 } {
	set ds_link [ds_link]
    } else {
	set ds_link ""
    }
    return "<hr>
$ds_link
<a href=\"mailto:[ad_admin_owner]\"><address>[ad_admin_owner]</address></a>
</body>
</html>"
}
proc check_for_form_variable_naughtiness {name value} {
    # security patch contributed by michael@cleverly.com
    if { [string match "QQ*" $name] } {
        error "Form variables should never begin with QQ!"
    }

    # contributed by michael@cleverly.com
    if { [string match Vform_counter_i $name] } {
        error "Vform_counter_i not an allowed form variable"
    }

    # The statements below make ACS more secure, because it prevents
    # overwrite of variables from something like set_the_usual_form_variables
    # and it will be better if it was in the system. Yet, it is commented
    # out because it will cause an unstable release. To add this security
    # feature, we will need to go through all the code in the ACS and make
    # sure that the code doesn't try to overwrite intentionally and also
    # check to make sure that when tcl files are sourced from another proc,
    # the appropriate variables are unset.  If you want to install this
    # security feature, then you can look in the release notes for more info.
    # 
    # security patch contributed by michael@cleverly.com,
    # fixed by iwashima@arsdigita.com
    #
    # upvar 1 $name name_before
    # if { [info exists name_before] } {
    # The variable was set before the proc was called, and the
    # form attempts to overwrite it
    # error "Setting the variables from the form attempted to overwrite existing variable $name"
    # }
    
    # no naughtiness with uploaded files (discovered by ben@mit.edu)
    # patch by richardl@arsdigita.com, with no thanks to
    # jsc@arsdigita.com.
    if { [string match "*tmpfile" $name] } {
        set tmp_filename [ns_queryget $name]

        # ensure no .. in the path
        ns_normalizepath $tmp_filename

        set passed_check_p 0

        # check to make sure path is to an authorized directory
        set tmpdir_list [ad_parameter_all_values_as_list TmpDir]
        if [empty_string_p $tmpdir_list] {
            set tmpdir_list [list "/var/tmp" "/tmp"]
        }

        foreach tmpdir $tmpdir_list {
            if { [string match "$tmpdir*" $tmp_filename] } {
                set passed_check_p 1
                break
            }
        }

        if { !$passed_check_p } {
            error "You specified a path to a file that is not allowed on the system!"
        }
    
    }

    # integrates with the ad_set_typed_form_variable_filter system
    # written by dvr@arsdigita.com

    # see if this is one of the typed variables
    global ad_typed_form_variables    

    if [info exists ad_typed_form_variables] { 

        foreach typed_var_spec $ad_typed_form_variables {
            set typed_var_name [lindex $typed_var_spec 0]
        
            if ![string match $typed_var_name $name] {
                # no match. Go to the next variable in the list
                continue
            }
        
            # the variable matched the pattern
            set typed_var_type [lindex $typed_var_spec 1]
        
            if [string match "" $typed_var_type] {
                # if they don't specify a type, the default is 'integer'
                set typed_var_type integer
            }

            set variable_safe_p [ad_var_type_check_${typed_var_type}_p $value]
        
            if !$variable_safe_p {
                ns_returnerror 500 "variable $name failed '$typed_var_type' type check"
                ns_log Error "[ad_conn url] called with \$$name = $value"
                error "variable $name failed '$typed_var_type' type check"
            }

            # we've found the first element in the list that matches,
            # and we don't want to check against any others
            break
        }
    }
}
proc ad_check_password {user_id password_from_form} {

    if { ![db_0or1row password_select {select password, salt from users where user_id = :user_id}] } {
	return 0
    }

    set salt [string trim $salt]

    if { [string compare $password [ns_sha1 "$password_from_form$salt"]] } {
	return 0
    }

    return 1
}
proc ad_html_text_convert args {    ad_html_text_convert__arg_parser

    switch $from {
        text/html -
	html {
	    switch $to {
                text/html -
		html {
		    ad_html_security_check $text
		    return [util_close_html_tags $text]
		}
                text/plain -
		text {
		    return [ad_html_to_text -- $text]
		}
		default {
		    return -code error "Can only convert to text or html"
		}
	    }
	} 
        text/plain -
	text {
	    switch $to {
                text/html -
		html {
		    return [ad_text_to_html -- $text]
		}
                text/plain -
		text {
		    return [wrap_string $text 70]
		}
		default {
		    return -code error "Can only convert to text or html"
		}
	    }
	} 
	default {
	    return -code error "Can only convert from text or html"
	}
    }
}
proc db_rdbms_get_type rdbms {
    return [lindex $rdbms 0]
}
proc ad_page_contract_filter_proc_usphone {name value_varname} {upvar $value_varname value

    if {![empty_string_p [string trim $value]] && ![regexp {[1-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]} $value]} {
	ad_complain "$name does not appear to be a valid US phone number."
	return 0
    }
    return 1
}
proc apm_first_time_loading_p {} {
    global apm_first_time_loading_p
    return [info exists apm_first_time_loading_p]
}
proc db_get_port {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    set datasource [ns_config ns/db/pool/$pool DataSource]
    set last_colon_pos [string last ":" $datasource]
    if { $last_colon_pos == -1 } {
        ns_log Error "datasource contains no \":\"? datasource = $datasource"
        return ""
    }
    set first_colon_pos [string first ":" $datasource]

    if { $first_colon_pos == $last_colon_pos || [expr $last_colon_pos - $first_colon_pos] == 1 } {
	# No port specified
	return ""
    }

    return [string range $datasource [expr $first_colon_pos + 1] [expr $last_colon_pos - 1] ]
}
proc db_qd_fetch {fullquery_name {rdbms {}}} {
    # For now we consider that everything is cached
    # from startup time
    return [db_qd_internal_get_cache $fullquery_name]
}
proc db_qd_internal_parse_init {stuff_to_parse file_path} {
    
    # Do initial parse
    set parsed_doc [xml_parse -persist $stuff_to_parse]

    # Initialize the parsing state
    set index 0

    # Get the list of queries out
    set root_node [xml_doc_get_first_node $parsed_doc]

    # Check that it's a queryset
    if {[xml_node_get_name $root_node] != "queryset"} {
	db_qd_log Debug "OH OH, error, first node is [xml_node_get_name $root_node]"
	# CHANGE THIS: throw an error!!!
	return ""
    }

    # Extract the default RDBMS if there is one
    set rdbms_nodes [xml_node_get_children_by_name $root_node rdbms]
    if {[llength $rdbms_nodes] > 0} {
	set default_rdbms [db_rdbms_parse_from_xml_node [lindex $rdbms_nodes 0]]
	db_qd_log Debug "Detected DEFAULT RDBMS for whole queryset: $default_rdbms"
    } else {
	set default_rdbms ""
    }

    set parsed_stuff [xml_node_get_children_by_name $root_node fullquery]

    db_qd_log Debug "end of parse_init: $index; $parsed_stuff; $parsed_doc; $default_rdbms; $file_path"

    return [list $index $parsed_stuff $parsed_doc $default_rdbms $file_path]
}
proc ad_var_type_check_nocheck_p {{value {}}} {
    return 1
}
proc ad_decorate_top {simple_headline potential_decoration} {
    if [empty_string_p $potential_decoration] {
	return $simple_headline
    } else {
	return "<table cellspacing=10><tr><td>$potential_decoration<td>$simple_headline</tr></table>"
    }
}
proc db_fullquery_get_querytext fullquery {
    return [lindex $fullquery 1]
}
proc acs_community_member_link__arg_parser {} {    upvar args args
    upvar label val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -user_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -user_id"
                }
                upvar user_id val ; set val [lindex $args [incr i]]
            }
            -label {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -label"
                }
                upvar label val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
    if { ![uplevel info exists user_id] } {
        return -code error "Required switch -user_id not provided"
    }
}
proc ad_chdir_and_exec {dir arg_list} {
    cd $dir
    eval exec $arg_list
}
proc ad_admin_header args {    ad_admin_header__arg_parser

    
    # if {[ad_parameter -package_id [ad_acs_kernel_id]  MenuOnAdminPagesP pdm] == 1} {
	
	# return [ad_header_with_extra_stuff -focus $focus $page_title [ad_pdm "admin" 5 5] [ad_pdm_spacer "admin"]]
	
	# } else {}

	return [ad_header_with_extra_stuff -focus $focus $page_title]
}
proc ad_dbclick_check_dml__arg_parser {} {    upvar args args
    upvar bind val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -bind {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -bind"
                }
                upvar bind val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 6 } {
        return -code error "No value specified for argument [lindex { statement_name table_name id_column_name generated_id return_url insert_dml } $n_args_remaining]"
    }
    upvar statement_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar table_name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar id_column_name val ; set val [lindex $args [expr { $i + 2 }]]
    upvar generated_id val ; set val [lindex $args [expr { $i + 3 }]]
    upvar return_url val ; set val [lindex $args [expr { $i + 4 }]]
    upvar insert_dml val ; set val [lindex $args [expr { $i + 5 }]]
    if { $n_args_remaining > 6 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_just_the_digits input_string {
    if [regsub -all {[^0-9]} $input_string "" output_string] {
	return $output_string 
    } else {
	return $input_string
    }
}
proc apm_package_key_from_id_mem package_id {
    return [db_string apm_package_key_from_id {
	select package_key from apm_packages where package_id = :package_id
    } -default ""]
}
proc ad_parameter_cache_all {} { 
    # Cache all parameters for enabled packages. .
    db_foreach parameters_get_all {
	select v.package_id, p.parameter_name, v.attr_value
	from apm_parameters p, apm_parameter_values v
	where p.parameter_id = v.parameter_id
    } {
	ad_parameter_cache -set $attr_value $package_id $parameter_name
    }	
}
proc db_fullquery_get_query_type fullquery {
    return [lindex $fullquery 3]
}
proc db_getrow {db selection} {
    set start_time [clock clicks]
    set errno [catch { return [ns_db getrow $db $selection] } error]
    ad_call_proc_if_exists ds_collect_db_call $db getrow "" "" $start_time $errno $error
    if { $errno == 2 } {
	return $error
    }
    global errorInfo errorCode
    return -code $errno -errorinfo $errorInfo -errorcode $errorCode $error
}
proc ad_page_contract_filter_proc_email {name value_varname} {upvar $value_varname value

    set valid_p [regexp "^\[^@\t ]+@\[^@.\t]+(\\.\[^@.\n ]+)+$" $value]
    if { !$valid_p } {
	ad_complain "$name does not appear to be a valid email address."
	return 0
    }
    return 1
}
proc first_day_of_month args {
            ::nstcl::_ad_proc_parser ::nstcl::first_day_of_month [set args]
            
    return [clock format [clock scan  [clock format [clock scan $date]  -format "%Y-%m-01 %H:%M:%S"]] -format $format]
}
proc ad_returnredirect target_url {
  if {[util_complete_url_p $target_url]} {
      # http://myserver.com/foo/bar.tcl style - just pass to ns_returnredirect
      set url $target_url
  } elseif {[util_absolute_path_p $target_url]} {
      # /foo/bar.tcl style - prepend the current location:
      set url [util_current_location]$target_url
  } else {
      # URL is relative to current directory.
      if {[string equal $target_url "."]} {
	  set url [util_current_location][util_current_directory]
      } else {
	  set url [util_current_location][util_current_directory]$target_url
      }
  }
  #Ugly workaround to deal with IE5.0 bug handling multipart/form-data using 
  #Meta Refresh page instead of a redirect. 
  # jbank@arsdigita.com 6/7/2000
  set use_metarefresh_p 0
  set type [ns_set iget [ad_conn headers] content-type]
  if {[string match *multipart/form-data* [string tolower $type]]} {
      set user_agent [ns_set get [ad_conn headers] User-Agent]
      set use_metarefresh_p [regexp -nocase "msie" $user_agent match]
  }
  if {$use_metarefresh_p != 0} {
      util_ReturnMetaRefresh $url 
  } else {
      ns_returnredirect $url
  }
}
proc util_httpopen {method url {rqset {}} {timeout 30} {http_referer {}}} {
    
	if ![string match http://* $url] {
		return -code error "Invalid url \"$url\":  _httpopen only supports HTTP"
	}
	set url [split $url /]
	set hp [split [lindex $url 2] :]
	set host [lindex $hp 0]
	set port [lindex $hp 1]
	if [string match $port ""] {set port 80}
	set uri /[join [lrange $url 3 end] /]
	set fds [ns_sockopen -nonblock $host $port]
	set rfd [lindex $fds 0]
	set wfd [lindex $fds 1]
	if [catch {
		_ns_http_puts $timeout $wfd "$method $uri HTTP/1.0\r"
		if {$rqset != ""} {
			for {set i 0} {$i < [ns_set size $rqset]} {incr i} {
				_ns_http_puts $timeout $wfd  "[ns_set key $rqset $i]: [ns_set value $rqset $i]\r"
			}
		} else {
			_ns_http_puts $timeout $wfd  "Accept: */*\r"

		    	_ns_http_puts $timeout $wfd "User-Agent: Mozilla/1.01 \[en\] (Win95; I)\r"    
		    	_ns_http_puts $timeout $wfd "Referer: $http_referer \r"    
	}

    } errMsg] {
		global errorInfo
		#close $wfd
		#close $rfd
		if [info exists rpset] {ns_set free $rpset}
		return -1
	}
	return [list $rfd $wfd ""]
    
}
proc ad_html_text_convert__arg_parser {} {    upvar args args
    upvar from val ; set val text
    upvar to val ; set val html

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -from {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -from"
                }
                upvar from val ; set val [lindex $args [incr i]]
            }
            -to {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -to"
                }
                upvar to val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { text } $n_args_remaining]"
    }
    upvar text val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_register_proc args {    ad_register_proc__arg_parser

    if { [string equal $method "*"] } {
	# Shortcut to allow registering filter for all methods. Just
        # call ad_register_proc again, with each of the three methods.
	foreach method { GET POST HEAD } {
	    ad_register_proc -debug $debug -noinherit $noinherit $method $path $proc $arg
	}
	return
    }

    if { [lsearch -exact { GET POST HEAD } $method] == -1 } {
	error "Method passed to ad_register_proc must be one of GET, POST, or HEAD"
    }

    set proc_info [list $method $path $proc $arg $debug $noinherit $description [info script]]
    nsv_lappend rp_registered_procs . $proc_info
}
proc ns_url2file url {
    return [file join [ns_info pageroot] $url]
}
proc ns_uuencode string {
    if {[catch { package require base64 }]} {
        return -code error "ns_uuencode requires the base64 package from tcllib"
    } else {
        return [base64::encode $string]
    }
}
proc apm_callback_and_log__arg_parser {} {    upvar args args
    upvar severity val ; set val Debug

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -severity {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -severity"
                }
                upvar severity val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { callback message } $n_args_remaining]"
    }
    upvar callback val ; set val [lindex $args [expr { $i + 0 }]]
    upvar message val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc philg_quote_double_quotes arg {
    return [ad_quotehtml $arg]
}
proc ns_config {section item args} {
    
    switch $item {
	"User"     { return "Administrator" }
	"Password" { return "" }
	"DataSource" { return "localhost::openacs" }
	"Driver"   { return "" }
	"Verbose"  { return "On" }
	"pgbin"    { return [file join [lindex [file split [pwd]] 0 ] "/cygwin/bin"] } 
	"DebugP"   { return "1" }
	"LogDebugP" { return "1" }
	"Hostname" { return "localhost" }
	"ClusterEnabledP" { return "false" }
	"SessionTimeout" { return 600 }
	"SessionRenew"   { return 600 }
	"NumberOfCachedSecretTokens" { return 30 }
	"PerformanceModeP" { return 0 }
	"SessionSweepInterval" { return 60 }
	"directorylisting" { return simple 
	    return fancy }
	"MaxSize" { 
	    # for memoize
	    return 200000
	} 
	"SystemOwner" { return johnseq@pobox.com }
	"ExtensionPrecedence" { return ".tcl .adp" }
	"SystemName" { return "PortableNSD" }
	"AllowPersistentLoginP" { return On }
	"PersistentLoginDefaultP" { return On }
	"RestrictErrorsToAdminsP" { return Off }
	"AutomaticErrorReportingP" { return On }
	"RefreshCache"  { return On }
	"HomeURL" { return "http://localhost/pvt" }
	"HomeName" { return "Home Base" }
    }
    
    ns_log notice "ns_config called with $section $item"

}
proc acs_community_member_link args {    acs_community_member_link__arg_parser

    if {[empty_string_p $label]} {
        set label [db_string select_community_member_link_label {
            select persons.first_names || ' ' || persons.last_name
            from persons
            where person_id = :user_id
        } -default $user_id]
    }

    return "<a href=\"[acs_community_member_url -user_id $user_id]\">$label</a>"
}
proc ad_complaints_get_list {} {
    global ad_page_contract_complaints
    return $ad_page_contract_complaints
}
proc ns_schedule_proc args { 
    ns_log Debug "(no-op)ns_schedule_proc called with $args "

}
proc ad_schedule_proc__arg_parser {} {    upvar args args
    upvar thread val ; set val f
    upvar once val ; set val f
    upvar all_servers val ; set val f
    upvar debug val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -thread {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -thread"
                }
                upvar thread val ; set val [lindex $args [incr i]]
            }
            -once {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -once"
                }
                upvar once val ; set val [lindex $args [incr i]]
            }
            -debug {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -debug"
                }
                upvar debug val ; set val [lindex $args [incr i]]
            }
            -all_servers {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -all_servers"
                }
                upvar all_servers val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { interval proc } $n_args_remaining]"
    }
    upvar interval val ; set val [lindex $args [expr { $i + 0 }]]
    upvar proc val ; set val [lindex $args [expr { $i + 1 }]]
    set args [lrange $args [expr { $i + 2 }] end]
}
proc apm_parameter_register args {    apm_parameter_register__arg_parser

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
proc db_compatible_rdbms_p db_type {
    return [expr { [empty_string_p $db_type] || [string equal [db_type] $db_type] }]
}
proc ad_text_to_html args {    ad_text_to_html__arg_parser


    if { !$no_links_p } {
	# We start by putting a space in front so our URL/email highlighting will work
	# for URLs/emails right in the beginning of the text.
	set text " $text"
	
	# if something is " http://" or " https://"
	# we assume it is a link to an outside source. 
	
	# (bd) The only purpose of thiese sTaRtUrL and
	# eNdUrL markers is to get rid of trailing dots,
	# commas and things like that.  Note that there
	# is a TAB before and after each marker.
	
	regsub -nocase -all {([^a-zA-Z0-9]+)(http://[^\(\)"<>\s]+)} $text "\\1\tsTaRtUrL\\2eNdUrL\t" text
	regsub -nocase -all {([^a-zA-Z0-9]+)(https://[^\(\)"<>\s]+)} $text "\\1\tsTaRtUrL\\2eNdUrL\t" text
	regsub -nocase -all {([^a-zA-Z0-9]+)(ftp://[^\(\)"<>\s]+)} $text "\\1\tsTaRtUrL\\2eNdUrL\t" text
	
	# email links have the form xxx@xxx.xxx
	regsub -nocase -all {([^a-zA-Z0-9]+)([^\(\)\s:;,@<>]+@[^\(\)\s.:;,@<>]+[.][^\(\)\s:;,@<>]+)} $text  "\\1\tsTaRtEmAiL\\2eNdEmAiL\t" text

    }    

    # At this point, before inserting some of our own <, >, and "'s
    # we quote the ones entered by the user:
    set text [ad_quotehtml $text]

    # turn CRLFCRLF into <P>
    if { [regsub -all {\r\n\r\n} $text "<p>" text] == 0 } {
	# try LFLF
	if { [regsub -all {\n\n} $text "<p>" text] == 0 } {
		# try CRCR
	    regsub -all {\r\r} $text "<p>" text
	}
    }
    
    if { !$no_links_p } {
	# Dress the links and emails with A HREF
	regsub -all {([]!?.:;,<>\(\)\}-]+)(eNdUrL\t)} $text {\2\1} text
	regsub -all {([]!?.:;,<>\(\)\}-]+)(eNdEmAiL\t)} $text {\2\1} text
	regsub -all {\tsTaRtUrL([^\t]*)eNdUrL\t} $text {<a href="\1">\1</a>} text
	regsub -all {\tsTaRtEmAiL([^\t]*)eNdEmAiL\t} $text {<a href="mailto:\1">\1</a>} text
	set text [string trimleft $text]
    }

    return $text
}
proc ad_convert_to_html args {    ad_convert_to_html__arg_parser

    if { [string equal $html_p t] } {
	set from html
    } else {
	set from text
    }
    return [ad_html_text_convert -from $from -to html -- $text]
}
proc ns_register_adptag args {
    ns_log Debug "ns_register_adptag called with $args "

}
proc duration args {
            ::nstcl::_ad_proc_parser ::nstcl::duration [set args]
            
    set now  [clock seconds]
    catch { set then [clock scan $period -base $now] }
 
    if {![info exists then]} {
        if {$nocomplain_p} {
            return $default
        } else {
            error "unable to convert date-time string \"$period\""
        }
    }
 
    set duration [expr {$then - $now}]
    if {$duration < 0 && $absolute_value_p} {
        set duration [expr {abs($duration)}]
    }
    return $duration
}
proc db_1row {statement_name pre_sql args} {
#        ns_log info "db_1row called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_1row $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_1row $statement_name $sql] $args]
    }
proc util_prepare_update {table_name primary_key_name primary_key_value form} {

    set form_size [ns_set size $form]
    set form_counter_i 0
    set column_list [db_columns $table_name]
    set bind_vars [ad_tcl_list_list_to_ns_set [list [list $primary_key_name $primary_key_value]]]

    while {$form_counter_i<$form_size} {

	set form_var_name [ns_set key $form $form_counter_i]
	set value [string trim [ns_set value $form $form_counter_i]]

	if { ($form_var_name != $primary_key_name) && ([lsearch $column_list $form_var_name] != -1) } {

	    ad_tcl_list_list_to_ns_set -set_id $bind_vars [list [list $form_var_name $value]]
	    lappend the_sets "$form_var_name = :$form_var_name"

	}

	incr form_counter_i
    }

    return [list "update $table_name\nset [join $the_sets ",\n"] \n where $primary_key_name = :$primary_key_name" $bind_vars]
   
}
proc template_tag {name arglist body} {

  switch [llength $arglist] {

    1 { 

      # empty tag
      eval "proc template_tag_$name { params } {
	
	template::adp_tag_init $name
	
	$body

        return \"\"
      }"

      ns_register_adptag $name template_tag_$name 
    }

    2 { 

      # balanced tag so push on/pop off tag name and parameters on a stack
      eval "proc template_tag_$name { chunk params } {
	
	template::adp_tag_init $name

	variable template::tag_stack
	lappend tag_stack \[list $name \$params\]
	
	$body

	template::util::lpop tag_stack

        return \"\"
      }"

      ns_register_adptag $name /$name template_tag_$name 
    }

    default { error "Invalid number of arguments to tag handler." }
  }
}
proc site_node_create_package_instance__arg_parser {} {    upvar args args
    upvar sync_p val ; set val t
    upvar package_id val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -package_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_id"
                }
                upvar package_id val ; set val [lindex $args [incr i]]
            }
            -sync_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -sync_p"
                }
                upvar sync_p val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 4 } {
        return -code error "No value specified for argument [lindex { node_id instance_name context_id package_key } $n_args_remaining]"
    }
    upvar node_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar instance_name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar context_id val ; set val [lindex $args [expr { $i + 2 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 3 }]]
    if { $n_args_remaining > 4 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_set_typed_form_variable_filter {url_pattern args} {
    ad_register_filter postauth GET  $url_pattern ad_set_typed_form_variables $args
    ad_register_filter postauth POST $url_pattern ad_set_typed_form_variables $args
}
proc ad_user_logout {} {
    ad_set_cookie -replace t -max_age 0 ad_session_id ""
    ad_set_cookie -replace t -max_age 0 ad_secure_token ""
    ad_set_cookie -replace t -max_age 0 ad_user_login ""
    ad_set_cookie -replace t -max_age 0 ad_user_login_secure ""
}
proc ad_page_contract_get_validation_passed_p key {
    global ad_page_contract_validations_passed
    return [info exists ad_page_contract_validations_passed($key)]
}
proc sec_generate_session_id_cookie {} {
    set user_id [ad_conn user_id]
    set session_id [ad_conn session_id]
    ns_log Notice "Security: [ns_time] sec_generate_session_id_cookie setting $session_id, $user_id."
    ns_log Debug "Security: [ns_time] sec_generate_session_id_cookie setting $session_id, $user_id."
    ad_set_signed_cookie -replace t -max_age [sec_session_timeout]  "ad_session_id" "$session_id,$user_id"
}
proc ad_return_top_of_page {first_part_of_page {content_type text/html}} {
    set all_the_headers "HTTP/1.0 200 OK
MIME-Version: 1.0
Content-Type: $content_type\r\n"
     util_WriteWithExtraOutputHeaders $all_the_headers

    ns_startcontent -type $content_type

    if ![empty_string_p $first_part_of_page] {
	ns_write $first_part_of_page
    }
}
proc ad_port {} {
    set host_and_port [ns_set iget [ns_conn headers] Host]
    if { [regexp {^([^:]+):([0-9]+)} $host_and_port match host port] } {
	return ":$port"
    } else {
	return ""
    }
}
proc apm_pretty_name_for_file_type type {
    return [util_memoize [list db_string pretty_name_select "
        select pretty_name
        from apm_package_file_types
        where file_type_key = :type
    " -default "Unknown" -bind [list type $type]]]
}
proc ad_page_contract_filter_proc_naturalnum {name value_varname} {upvar $value_varname value

    if { ![regexp {^(0*)(([1-9][0-9]*|0))$} $value match zeros value] } {
	ad_complain "$name is not a natural number, that is an integer greater than or equal to 0."
	return 0
    }
    return 1
}
proc sec_session_lifetime {} {
    # default value is 7 days ( 7 * 24 * 60 * 60 )
    return [ad_parameter -package_id [ad_acs_kernel_id] SessionLifetime security 604800]
}
proc with_transaction {body on_error} {
    upvar errmsg errmsg
    global errorInfo errorCode
    if { [catch {db_transaction { uplevel $body }} errmsg] } {
        db_abort_transaction
        set code [catch {uplevel $on_error} string]
        # Return out of the caller appropriately.
        if { $code == 1 } {
            return -code error -errorinfo $errorInfo -errorcode $errorCode $string
        } elseif { $code == 2 } {
            return -code return $string
        } elseif { $code == 3 } {
            return -code break
	} elseif { $code == 4 } {
	    return -code continue
        } elseif { $code > 4 } {
            return -code $code $string
        }
    }        
}
proc apm_dependency_remove dependency_id {
    db_exec_plsql dependency_remove {
	begin
	apm_package_version.remove_dependency(
             dependency_id => :dependency_id
	);
	end;					        
    }
}
proc nsv_incr {id key} {

    global $id
    if {! [array exists $id] } { 
	set ${id}($key) 0
    }

    set retval [ expr 1 + [nsv_get $id $key] ]
    nsv_set $id $key $retval    
    
}
proc ns_configsection path { 
    ns_log warning "no-op: ns_configsection called with arg $path "
    return 
}
proc util_report_successful_library_load {{extra_message {}}} {
    set tentative_path [info script]
    regsub -all {/\./} $tentative_path {/} scrubbed_path
    if { [string compare $extra_message ""] == 0 } {
	set message "Done... $scrubbed_path"
    } else {
	set message "Done... $scrubbed_path; $extra_message"
    }
    ns_log Notice $message
}
proc tclx_findinit {w defaultLib version noInit} {
    upvar #0 env env ${w}x_library libDir tcl_platform tcl_platform
    set dirs {}
    set envVar [string toupper ${w}X_LIBRARY]
    if {[info exists libDir]} {
        lappend dirs $libDir
    } else {
        if [info exists env($envVar)] {lappend dirs $env($envVar)}
        lappend dirs [file join [file dirname [info library]] ${w}X$version]
        if [info exists env(EXT_FOLDER)] {
	    lappend dirs [file join $env(EXT_FOLDER) "Tool Command Language" ${w}X$version]
        }
	if {[string length $defaultLib]} {
            lappend dirs $defaultLib
	}
        set libDir {}
set prefix [file dirname [info nameofexecutable]]
        set plat [file tail $prefix]
        set prefix [file dirname $prefix]
        lappend dirs [file join $prefix lib ${w}X$version]
        set prefix [file dirname $prefix]
        lappend dirs [file join $prefix ${w}X${version} $w $plat]
        lappend dirs [file join [file dirname $prefix] ${w}X${version} $w $plat]
    }
    foreach libDir $dirs {
        set init [file join $libDir ${w}x.tcl]
        if [file exists $init] {
            if !$noInit {uplevel #0 source [list $init]}; return
        }
    }
    set libDir {}
    set msg "Can't find ${w}x.tcl in the following directories: 
"
    foreach d $dirs {append msg "  $d
"}
    append msg "This probably means that TclX wasn't installed properly.
"
    error $msg
}
proc db_qd_null_path {} {
    return "[db_qd_root_path].NULL"
}
proc sec_sweep_sessions {} {
    set current_time [ns_time]
    set property_life [sec_session_lifetime]

    db_dml sessions_sweep {
	delete from sec_session_properties
	where  :current_time - last_hit > :property_life
    }
}
proc showproc args {
    if [lempty $args] {
        set args [info procs]
    }
    set out {}

    foreach procname $args {
        if [lempty [info procs $procname]] {
            auto_load $procname
        }
        set arglist [info args $procname]
        set nargs {}
        while {[llength $arglist] > 0} {
            set varg [lvarpop arglist 0]
            if [info default $procname $varg defarg] {
                lappend nargs [list $varg $defarg]
            } else {
                lappend nargs $varg
            }
        }
        append out "proc $procname [list $nargs] \{[info body $procname]\}\n"
    }
    return $out
}
proc nsv_array {command array_name args} {
    switch $command {

	"set"  { 
	    
	    global $array_name
	    set values [lindex $args 0 ]
	    if {$values == [list]} { 
		array set $array_name [list]
	    } else {  
		array set $array_name $values
	    }
	    return
	}
	"names" { 
	    global $array_name
	    return [array names $array_name]
	}
	"reset" {
	    #not sure about this...
#	    puts "resetting"
#	    nsv_array set $array_name $args
	    global $array_name
	    set values [lindex $args 0 ]
	    array set $array_name $values
#	    eval $cmd
	    return
	}
	
    }

    ns_log Error "nsv_array called args $args "

}
proc ad_record_query_string {query_string subsection n_results {user_id {[db_null]}}} {  

    if { $user_id == 0 } {
	set user_id [db_null]
    }

    db_dml query_string_record {
	insert into query_strings 
	(query_date, query_string, subsection, n_results, user_id) values
	(sysdate, :query_string, :subsection, :n_results, :user_id)
    }
}
proc ad_page_contract_filter_proc_phone {name value_varname} {upvar $value_varname value

    if { ![empty_string_p [string trim $value]] } {
	if ![regexp {^\(?([1-9][0-9]{2})\)?(-|\.|\ )?([0-9]{3})(-|\.|\ )?([0-9]{4})} $value] {
	    ad_complain "$value does not appear to be a valid U.S. phone
	    number."
	    return 0
	}
    }
    return 1
}
proc util_get_http_status {url {use_get_p 1} {timeout 30}} {
    switch -- [string toupper $use_get_p] {
        HEAD { set use_get_p 0 }
        GET  { set use_get_p 1 }
    }

    if {![string is boolean -strict $use_get_p]} {
        error "expected a boolean but got \"$use_get_p\""
    }

    switch [string is true $use_get_p] {
        0 { set mode HEAD }
        1 { set mode GET }
    }
    
    if {[catch {
        ::nstcl::http::fetch_url -mode $mode -timeout $timeout $url
    } result]} {
        error $result
    } else {
        return [lindex $result 0]
    }
}
proc server_cluster_httpget_from_peers args {    server_cluster_httpget_from_peers__arg_parser

    if { ![string match /* $url] } {
	set url "/$url"
    }
    foreach host [server_cluster_peer_hosts] {
	# Schedule the request. Don't actually issue the request in this thread, since
	# (a) we want to parallelize the requests, and (b) we want this procedure to
	# return immediately.
	ad_schedule_proc -once t -thread f -debug t 0 server_cluster_do_httpget "http://$host$url" $timeout
    }
}
proc util_link_responding_p {url {list_of_bad_codes 404}} {
    if [catch { set status [util_get_http_status $url] } errmsg] {
	# got an error; definitely not valid
	return 0
    } else {
	# we got the page but it might have been a 404 or something
	if { [lsearch $list_of_bad_codes $status] != -1 } {
	    return 0
	} else {
	    return 1
	}
    }
}
proc db_table_exists table_name {
    set n_rows [db_string table_count {
	select count(*) from pg_class
        where relname = lower(:table_name) and
	    relname !~ '^pg_' and relkind = 'r'
    }]
    return $n_rows
}
proc apm_highest_version package_key {
    return [db_exec_plsql apm_highest_version {
	begin
	:1 := apm_package.highest_version (
                    package_key => :package_key
		    );
	end;
    }]
}
proc nsv_array_set {array_name values} {

    global $array_name
    if {$values == {}} { 
	array set $array_name [list]
    } else {  
	array set $array_name $values
    }
    
}
proc sec_lookup_property {id module name} {
    if {
	![db_0or1row property_lookup_sec {
	    select property_value, secure_p
	    from sec_session_properties
	    where session_id = :id
	    and module = :module
	    and property_name = :name
	}]
    } {
	return ""
    }

    set new_last_hit [clock seconds]

    db_dml update_last_hit_dml {
        update sec_session_properties
           set last_hit = :new_last_hit
         where session_id = :id and
               property_name = :name
    }

    return [list $property_value $secure_p]
}
proc ad_set_cookie args {    ad_set_cookie__arg_parser

    set headers [ad_conn outputheaders]
    if { $replace != "f" } {
	# Try to find an already-set cookie named $name.
	for { set i 0 } { $i < [ns_set size $headers] } { incr i } {
	    if { ![string compare [string tolower [ns_set key $headers $i]] "set-cookie"] &&  [regexp "^$name=" [ns_set value $headers $i]] } {
		ns_set delete $headers $i
	    }
	}
    }

    # need to set some value, so we put "" as the cookie value
    if { $value == "" } {
	set cookie "$name=\"\""
    } else {
	set cookie "$name=$value"
    }

    if { $path != "" } {
	append cookie "; Path=$path"
    }

    if { $max_age == "inf" } {
        if { ![string equal $expire "t"] } {
            # netscape seemed unhappy with huge max-age, so we use
            # expires which seems to work on both netscape and IE
            append cookie "; Expires=Fri, 01-Jan-2035 01:00:00 GMT"
        }
    } elseif { $max_age != "" } {
	append cookie "; Max-Age=$max_age"
    }

    if { [string equal $expire "t"] } {
        append cookie "; Expires=Fri, 01-Jan-1980 01:00:00 GMT"
    }

    if { $domain != "" } {
	append cookie "; Domain=$domain"
    }

    if { $secure != "f" } {
	append cookie "; Secure"
    }

    ns_set put $headers "Set-Cookie" $cookie
}
proc rp_path_prefixes path {
  set path [string trimright $path /]
  if {[string index $path 0] != "/"} {
    set path "/$path"
  }
  set components [split $path "/"]
  set prefixes [list]
  for {set i [expr [llength $components] -1]} {$i >= 0} {incr i -1} {
    lappend prefixes "[join [lrange $components 0 $i] "/"]/"
  }

  return $prefixes
}
proc stack_dump {} {
  append page "<h1>Tcl Call Trace</h1>
  <ul>"
  
  for {set x [info level]} {$x > 0} {incr x -1} {
    append page "<li>$x.  [info level $x]<ul>
[stack_frame_values $x]    </ul>\n"
  }
  
  append page "</ul>
  <h2>Globals</h2>
  <ul> [stack_frame_values 0] </ul>\n"
}
proc util_current_directory {} {
   set path [ad_conn url]

   set lastchar [string range $path [expr [string length $path]-1] end]
   if {![string compare $lastchar /]} {
        return $path
   } else { 
        set file_dirname [file dirname $path]
        # Treat the case of the root directory special
        if {![string compare $file_dirname /]} {
            return /
        } else {
            return  $file_dirname/
        }
   }
}
proc db_tables__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -pattern {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -pattern"
                }
                upvar pattern val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_return_exception_template args {    ad_return_exception_template__arg_parser

    set template_params [list]
    foreach param $params {
        switch [llength $param] {
            1 { lappend template_params "&"
                lappend template_params [lindex $param 0]
              }
            2 { lappend template_params [lindex $param 0]
                lappend template_params [lindex $param 1]
              }
            default { return -code error "Error in parameter list" }
        }
    }
    ns_return $status text/html [template::adp_parse [template::util::url_to_file $template [ad_conn file]] $template_params]
    ad_script_abort
}
proc tclLog string {
	catch {puts stderr $string}
    }
proc ad_page_contract_filter_proc_tmpfile {name value_varname} {upvar $value_varname value
 
    # ensure no .. in the path
    ns_normalizepath $value
    
    # check to make sure path is to an authorized directory
    set tmpdir_list [ad_parameter_all_values_as_list TmpDir]
    if [empty_string_p $tmpdir_list] {
	set tmpdir_list [list "/var/tmp" "/tmp"]
    }
    
    foreach tmpdir $tmpdir_list {
	if { [string match "$tmpdir*" $value] } {
	    return 1
	}
    }
    
    ad_complain "You specified a path to a file that is not allowed on the system."
    return 0
}
proc db_name {} {  
set doc {

    Returns the name of the database as reported by the driver.

} 

    return [ns_db dbtype $db]


}
proc ad_make_relative_path path {
    set root_length [string length [acs_root_dir]]
    if { ![string compare [acs_root_dir] [string range $path 0 [expr { $root_length - 1 }]]] } {
	return [string range $path [expr { $root_length + 1 }] [string length $path]]
    }
    error "$path is not under the path root ([acs_root_dir])"
}
proc proc_source_file_full_path proc_name {
    if ![nsv_exists proc_source_file $proc_name] {
	return ""
    } else {
	set tentative_path [nsv_get proc_source_file $proc_name]
	regsub -all {/\./} $tentative_path {/} result
	return $result
    }
}
proc set_intersection {x y} {
    set z {}
    foreach element $x {
        if {[lsearch -exact $y $element] != -1} {
            lappend z $element
        }
    }
    return $z
}
proc rp_invoke_filter {conn filter_info why} {
    set startclicks [clock clicks]

    util_unlist $filter_info filter_index debug_p arg_count proc arg

#      if { $debug_p } {
#      ns_log "Notice" "Invoking $why filter $proc"
#      }
    rp_debug -debug $debug_p "Invoking $why filter $proc"

    switch $arg_count {
	0 { set errno [catch { set result [$proc] } error] }
	1 { set errno [catch { set result [$proc $why] } error] }
	2 { set errno [catch { set result [$proc $conn $why] } error] }
	default { set errno [catch {
	  ad_try {
	    set result [$proc $conn $arg $why]
	  } ad_script_abort val {
	    set result "filter_return"
	  }
	} error] }
    }

    global errorCode
    if { $errno } {
      # Uh-oh - an error occurred.
      global errorInfo
      ad_call_proc_if_exists ds_add rp [list filter [list $why [ns_conn method] [ns_conn url] $proc $arg] $startclicks [clock clicks] "error" $errorInfo]
      # make sure you report catching the error!
      rp_debug -debug t "error in filter $proc for [ns_conn method] [ns_conn url]?[ns_conn query] errno is $errno message is $errorInfo"
      rp_report_error
      set result "filter_return"
    } elseif { [string compare $result "filter_ok"] && [string compare $result "filter_break"] &&  [string compare $result "filter_return"] } {
       set error_msg "error in filter $proc for [ns_conn method] [ns_conn url]?[ns_conn query].  Filter returned invalid result \"$result\""
       ad_call_proc_if_exists ds_add rp [list filter [list $why [ns_conn method] [ns_conn url] $proc $arg] $startclicks [clock clicks] "error" $error_msg]
        # report the bad filter_return message
        rp_debug -debug t error $error_msg
       rp_report_error -message $error_msg
	set result "filter_return"
    } else {
       ad_call_proc_if_exists ds_add rp [list filter [list $why [ns_conn method] [ns_conn url] $proc $arg] $startclicks [clock clicks] $result]
    }

#      if { $debug_p } {
#      ns_log "Notice" "Done invoking $why filter $proc (returning $result)"
#      }
    rp_debug -debug $debug_p "Done invoking $why filter $proc (returning $result)"

    if { [string compare $result "filter_return"] } {
      rp_finish_serving_page
    }

    return $result
}
proc ad_tcl_vars_list_to_ns_set args {    ad_tcl_vars_list_to_ns_set__arg_parser

    if { ![info exists set_id] } {
	set set_id [ns_set create]
    }

    if { $put_p } {
	set command put
    } else {
	set command update
    }

    foreach varname $vars_list {
	upvar $varname var
	ns_set $command $set_id $varname $var
    }
    return $set_id
}
proc site_node_closest_ancestor_package args {    site_node_closest_ancestor_package__arg_parser

    if { [empty_string_p $url] } {
	set url [ad_conn url]
    }
    # Try the URL as is.
    if {[catch {nsv_get site_nodes $url} result] == 0} {
	array set node $result
	if { [string eq $node(package_key) $package_key] } {
	    return $node(package_id)
	}
    }

    # Add a trailing slash and try again.
    if {[string index $url end] != "/"} {
	append url "/"
	if {[catch {nsv_get site_nodes $url} result] == 0} {
	    array set node $result
	    if { [string eq $node(package_key) $package_key] } {
		return $node(package_id)
	    }
	}
    }

    # Try successively shorter prefixes.
    while {$url != ""} {
	# Chop off last component and try again.
	set url [string trimright $url /]
	set url [string range $url 0 [string last / $url]]
	
	if {[catch {nsv_get site_nodes $url} result] == 0} {
	    array set node $result
	    if {$node(pattern_p) == "t" && $node(object_id) != "" && [string eq $node(package_key) $package_key] } {
		return $node(package_id)
	    }
	}
    }

    return $default
}
proc db_write_clob {statement_name sql args} {
    ad_arg_parser { bind } $args

    db_with_handle db {
	db_exec write_clob $db $statement_name $sql
    }
}
proc doc_get_property name {
    global doc_properties
    if { [info exists doc_properties($name)] } {
	return $doc_properties($name)
    }
    return ""
}
proc apm_load_libraries args {    apm_load_libraries__arg_parser

    
    # DRB: query extractor's dumb about repeated query
    # names so I changed these to be unique.  We should
    # really be sharing these at some level rather than
    # duping them anyway.
    set packages [db_list apm_enabled_packages_l {
	select distinct package_key
	from apm_package_versions
	where enabled_p='t'
    }]

    # Scan the package directory for files to source.    
    set files [list]    
    foreach package $packages {

	set base "[acs_root_dir]/packages/$package/"
	set base_len [string length $base]
	set dirs [list  $base  ${base}tcl ]
	set paths [list]
      
	foreach dir $dirs {
	    if {$procs_p} {
		set paths [concat $paths [glob -nocomplain "$dir/*procs.tcl"]]
                set paths [concat $paths [glob -nocomplain "$dir/*procs-[db_type].tcl"]]
	    } 
	    if {$init_p} {
		set paths [concat $paths [glob -nocomplain "$dir/*init.tcl"]]
                set paths [concat $paths [glob -nocomplain "$dir/*init-[db_type].tcl"]]
	    }    
	}
	
	foreach path [lsort $paths] {
	    set rel_path [string range $path $base_len end]
	    lappend files [list $package $rel_path]
	}
    }
      
    # Release all outstanding database handles (since the file we're sourcing
    # might be using the ns_db database API as opposed to the new db_* API).
    db_release_unused_handles
    apm_files_load -callback $callback $files
}
proc db_version {} {
    return [nsv_get ad_database_version .]
}
proc string_contains_p {small_string big_string} {
    if { [string first $small_string $big_string] == -1 } {
	return 0
    } else {
	return 1
    }
}
proc db_column_type {table_name column_name} {

    return [db_string column_type_select "
	select data_type as data_type
	  from user_tab_columns
	 where upper(table_name) = upper(:table_name)
	   and upper(column_name) = upper(:column_name)
    " -default "-1"]

}
proc ns_rand args { 
    if { [llength $args ] == 0 } {
	return [::math::random]
    } else {
	return [::math::random [lindex $args 0 ] ]
    }
}
proc ad_get_client_property args {    ad_get_client_property__arg_parser

    if { [empty_string_p $session_id] } {
        set id [ad_conn session_id]
    } else {
        set id $session_id
    }

    set cmd [list sec_lookup_property $id $module $name]

    if { $cache_only == "t" && ![util_memoize_value_cached_p $cmd] } {
	return ""
    }

    if { $cache != "t" } {
	util_memoize_flush $cmd
    }

    set property [util_memoize $cmd [sec_session_timeout]]
    if { $property == "" } {
	return $default
    }
    set value [lindex $property 0]
    set secure_p [lindex $property 1]
    
    if { $secure_p != "f" && ![ad_secure_conn_p] } {
	return ""
    }

    return $value
}
proc apm_package_version_enabled_p version_id {
    return [db_string apm_package_version_installed_p {
	select decode(count(*), 0, 0, 1) from apm_package_versions
	where version_id = :version_id
	and enabled_p = 't'
    } -default 0]
}
proc doc_tag_ad_document {contents params} {
    for { set i 0 } { $i < [ns_set size $params] } { incr i } {
	doc_set_property [ns_set key $params $i] [ns_set value $params $i]
    }
    doc_set_property _adp 1
    return [ns_adp_parse -string $contents]
}
proc rp_html_directory_listing dir {
    # Create the table header.
    set list "
<table>
<tr align=left><th>File</th><th>Size</th><th>Date</th></tr>
<tr align=left><td colspan=3><a href=../>..</a></td></tr>
"

    # Loop through the files, adding a row to the table for each.
    foreach file [lsort [glob -nocomplain $dir/*]] {
	set tail [file tail $file]
	set link "<a href=$tail>$tail</a>"

	# Build the stat array containing information about the file.
	file stat $file stat
	set size [expr $stat(size) / 1000 + 1]K
	set mtime $stat(mtime)
	set time [clock format $mtime -format "%d-%h-%Y %H:%M"]

	# Write out the row.
	append list "<tr align=left><td>$link</td><td>$size</td><td>$time</td></tr>\n"
    }
    append list "</table>"
    return $list
}
proc db_multirow__arg_parser {} {    upvar args args
    upvar extend val ; set val {}
    upvar local_p val ; set val 0
    upvar append_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -local - -local=1 {
                uplevel set local_p 1
            }
            -local=0 {
                uplevel set local_p 0
            }
            -append - -append=1 {
                uplevel set append_p 1
            }
            -append=0 {
                uplevel set append_p 0
            }
            -extend {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -extend"
                }
                upvar extend val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { var_name statement_name sql } $n_args_remaining]"
    }
    upvar var_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar statement_name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar sql val ; set val [lindex $args [expr { $i + 2 }]]
    set args [lrange $args [expr { $i + 3 }] end]
}
proc DoubleApos string {
    regsub -all -- ' $string '' string
    return $string
}
proc apm_bootstrap_load_libraries args {    apm_bootstrap_load_libraries__arg_parser


    set root_directory [nsv_get acs_properties root_directory]
    set db_type [nsv_get ad_database_type .]

    # This is the first time each of these files is being loaded (see
    # the documentation for the apm_first_time_loading_p proc).
    global apm_first_time_loading_p
    set apm_first_time_loading_p 1

    set files [ad_find_all_files $root_directory/packages/$package_key]
    if { [llength $files] == 0 } {
		error "Unable to locate $root_directory/packages/$package_key/*."
    }

    foreach file [lsort $files] {

        set file_db_type [apm_guess_db_type $package_key $file]
        set file_type [apm_guess_file_type $package_key $file]

        if {([empty_string_p $file_db_type] ||  [string equal $file_db_type $db_type]) &&
            ([string equal $file_type tcl_procs] && $procs_p ||
             [string equal $file_type tcl_init] && $init_p)} {

		    apm_bootstrap_load_file $root_directory $file

            # Call db_release_unused_handles, only if the library defining it
            # (10-database-procs.tcl) has been sourced yet.
            if { [llength [info procs db_release_unused_handles]] != 0 } {
                db_release_unused_handles
            }
        }
    }

    unset apm_first_time_loading_p
}
proc ad_user_class_query_count_only set_id {
    set new_set [ns_set copy $set_id]
    ns_set update $new_set count_only_p 1
    return [ad_user_class_query $new_set]
}
proc db_rdbms_parse_from_xml_node rdbms_node {
    # Check that it's RDBMS
    if {[xml_node_get_name $rdbms_node] != "rdbms"} {
	db_qd_log Debug "PARSER = BAD RDBMS NODE!"
	return ""
    }

    # Get the type and version tags
    set type [xml_node_get_content [xml_node_get_first_child_by_name $rdbms_node type]]
    set version [xml_node_get_content [xml_node_get_first_child_by_name $rdbms_node version]]

    db_qd_log Debug "PARSER = RDBMS parser - $type - $version"

    return [db_rdbms_create $type $version]
}
proc acs_community_member_admin_url__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -user_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -user_id"
                }
                upvar user_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
    if { ![uplevel info exists user_id] } {
        return -code error "Required switch -user_id not provided"
    }
}
proc export_ns_set_vars {{format url} {exclusion_list {}} {setid {}}} {

    if [empty_string_p $setid] {
	set setid [ns_getform]
    }

    set return_list [list]
    if ![empty_string_p $setid] {
        set set_size [ns_set size $setid]
        set set_counter_i 0
        while { $set_counter_i<$set_size } {
            set name [ns_set key $setid $set_counter_i]
            set value [ns_set value $setid $set_counter_i]
            if {[lsearch $exclusion_list $name] == -1 && ![empty_string_p $name]} {
                if {$format == "url"} {
                    lappend return_list "[ns_urlencode $name]=[ns_urlencode $value]"
                } else {
                    lappend return_list " name=\"[ad_quotehtml $name]\" value=\"[ad_quotehtml $value]\""
                }
            }
            incr set_counter_i
        }
    }
    if {$format == "url"} {
        return [join $return_list "&"]
    } else {
        return "<input type=\"hidden\" [join $return_list ">\n <input type=\"hidden\" "] >"
    }
}
proc ad_parameter__arg_parser {} {    upvar args args
    upvar package_key val ; set val {}
    upvar package_id val ; set val {}
    upvar default val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set"
                }
                upvar set val ; set val [lindex $args [incr i]]
            }
            -package_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_id"
                }
                upvar package_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar package_key val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        upvar default val ; set val [lindex $args [expr { $i + 2 }]]
    }
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_commify_number num {
    while { 1 } {
	# Regular Expression taken from Mastering Regular Expressions (Jeff Friedl)
	# matches optional leading negative sign plus any
	# other 3 digits, starting from end
	if { ![regsub -- {^(-?[0-9]+)([0-9][0-9][0-9])} $num {\1,\2} num] } {
	    break
	}
    }
    return $num
}
proc util_memoize_cached_p {script {max_age {}}} {
    if {![ns_cache get util_memoize $script pair]} {
        return 0
    }

    if {[string equal $max_age ""]} {
        return 1
    } else {
        set cache_time [lindex $pair 0]
        return [expr {[ns_time] - $cache_time <= $max_age}]
    }
}
proc apm_bootstrap_load_libraries__arg_parser {} {    upvar args args
    upvar procs_p val ; set val 0
    upvar init_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -init - -init=1 {
                uplevel set init_p 1
            }
            -init=0 {
                uplevel set init_p 0
            }
            -procs - -procs=1 {
                uplevel set procs_p 1
            }
            -procs=0 {
                uplevel set procs_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { package_key } $n_args_remaining]"
    }
    upvar package_key val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_dispatch {method_name type args} {
    return [apply ${method_name}__$type $args]
}
proc ad_html_security_check html {
    if { [string first <% $html] > -1 } {
	return "For security reasons, you're not allowed to have the less-than-percent combination in your input."
    }
    
    array set allowed_attribute [list]
    array set allowed_tag [list]
    array set allowed_protocol [list]

    # Use the antispam tags for this package instance and whatever is on the kernel.
    set allowed_tags_list [concat  [ad_parameter_all_values_as_list -package_id [ad_acs_kernel_id] AllowedTag antispam]  [ad_parameter_all_values_as_list AllowedTag antispam]]

    set allowed_attributes_list [concat  [ad_parameter_all_values_as_list -package_id [ad_acs_kernel_id] AllowedAttribute antispam]  [ad_parameter_all_values_as_list AllowedAttribute antispam]]

    set allowed_url_attributes_list [concat  [ad_parameter_all_values_as_list -package_id [ad_acs_kernel_id] AllowedURLAttribute antispam] [ad_parameter_all_values_as_list AllowedURLAttribute antispam]]

    set allowed_protocols_list [concat  [ad_parameter_all_values_as_list -package_id [ad_acs_kernel_id] AllowedProtocol antispam]  [ad_parameter_all_values_as_list AllowedProtocol antispam]]
    set all_allowed_attributes_list [concat $allowed_attributes_list $allowed_url_attributes_list]
    ns_log Debug "All attributes:  $all_allowed_attributes_list"

    foreach tag $allowed_tags_list {
	set allowed_tag([string tolower $tag]) 1
    }
    foreach attribute $all_allowed_attributes_list {
	set allowed_attribute([string tolower $attribute]) 1
    }
    foreach attribute $allowed_url_attributes_list {
	set url_attribute([string tolower $attribute]) 1
    }
    foreach tagname $allowed_tags_list {
	set allowed_tag([string tolower $tagname]) 1
    }
    foreach protocol $allowed_protocols_list {
	set allowed_protocol([string tolower $protocol]) 1
    }
    
    # loop over all tags
    for { set i [string first < $html] } { $i != -1 } { set i [string first < $html $i] } {
	# move past the tag-opening <
	incr i

	if { ![regexp -indices -start $i {\A/?([-_a-zA-Z0-9]+)\s*} $html match name_idx] } {
	    # The tag-opener isn't followed by USASCII letters (with or without optional inital slash)
	    # Not considered a tag. Shouldn't do any harm in browsers.
	    # (Tested with digits, with &#65; syntax, with whitespace)
	} else {
	    # The tag was valid ... now let's see if it's on the allowed list.
	    set tagname [string tolower [string range $html [lindex $name_idx 0] [lindex $name_idx 1]]]

	    if { ![info exists allowed_tag($tagname)] } {
		# Nope, this was a naughty tag.
		return "For security reasons we only accept the submission of HTML
		containing the following tags: <code>[join $allowed_tags_list " "]</code>. 
		You have a &lt;$tagname&gt; tag in there."
	    } else {
		# Legal tag.
		
		# Make i point to the first character inside the tag, after the tag name and any whitespace
		set i [expr { [lindex $match 1] + 1}]

		set attr_list [ad_parse_html_attributes_upvar html i]

		set attr_count 0
		foreach attribute $attr_list {
		    incr attr_count
		    set attr_name [lindex $attribute 0]
		    set attr_value [lindex $attribute 1]
		    
		    if { ![info exists allowed_attribute($attr_name)] } {
			return "The attribute '$attr_name' is not allowed for <$tagname> tags"
		    }
		    
		    if { [regexp {^\s*([^\s:]+):} $attr_value match protocol] } {
			if { ![info exists allowed_protocol([string tolower $protocol])] } {
			    return "Your URLs can only use these protocols: [join $allowed_protocols_list ", "].
			    You have a '$protocol' protocol in there."
			}
		    }
		}
	    }
	}
    }
    return {}
}
proc ad_page_contract_filter_proc_sql_identifier {name value_varname} {upvar $value_varname value
		
    if { ![string is wordchar $value] } {
	ad_complain "$name is not a valid SQL identifier"
	return 0 
    } 
    return 1
}
proc set_form_variables_string_trim_DoubleAposQQ {} {
    # The variable names are prefixed with a V to avoid confusion with the form variables while checking for naughtiness.
    uplevel {
	set Vform [ns_getform] 
	if {$Vform == ""} {
	    ns_returnerror 500 "Missing form data"
	    return;
	}
	set Vform_size [ns_set size $Vform]
	set Vform_counter_i 0
	while {$Vform_counter_i<$Vform_size} {
	    set Vname [ns_set key $Vform $Vform_counter_i]
	    set Vvalue [ns_set value $Vform $Vform_counter_i]
	    check_for_form_variable_naughtiness $Vname $Vvalue
	    set QQ$Vname [DoubleApos [string trim $Vvalue]]
	    incr Vform_counter_i
	}
    }
}
proc acs_community_member_url__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -user_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -user_id"
                }
                upvar user_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
    if { ![uplevel info exists user_id] } {
        return -code error "Required switch -user_id not provided"
    }
}
proc num_days_difference args {
            ::nstcl::_ad_proc_parser ::nstcl::num_days_difference [set args]
            
    if {![string is integer -strict $x]} {
        set x [clock scan $x]
    }
 
    if {![string is integer -strict $y]} {
        set y [clock scan $y]
    }
 
    if {$absolute_value_p} {
        set diff [expr {abs(($y - $x) / 86400)}]
    } else {
        set diff [expr {($y - $x) / 86400}]
    }
 
    if {[string is integer -strict $floor] && $diff < $floor} {
        return $floor
    } elseif {[string is integer -strict $ceiling] && $diff > $ceiling} {
        return $ceiling
    } else {
        return $diff
    }
}
proc lmap {list proc_name} {
    set lmap [list]
    foreach item $list {
	lappend lmap [safe_eval $proc_name $item]
    }
    return $lmap
}
proc db_qd_get_fullname {local_name {added_stack_num 1}} {

    # We do a check to see if we already have a fullname.
    # Since the DB procs are a bit incestuous, this might get
    # called more than once. DAMMIT! (ben)
    if {![db_qd_relative_path_p $local_name]} {
	return $local_name
    }

    # Get the proc name being executed.
    # We catch this in case we're being called from the top level
    # (eg. from bootstrap.tcl), in which case we return what we
    # were given
    if { [catch {string trimleft [info level [expr "-1 - $added_stack_num"]] ::} proc_name] } {
	return $local_name
    }

    # If util_memoize, we have to go back up one in the stack
    if {[lindex $proc_name 0] == "util_memoize"} {
	db_qd_log Debug "util_memoize! going up one level"
	set proc_name [info level [expr "-2 - $added_stack_num"]]
    }

    set list_of_source_procs {ns_sourceproc apm_source template::adp_parse template::frm_page_handler rp_handle_tcl_request
    adp_parse_ad_conn_file}

    # We check if we're running the special ns_ proc that tells us
    # whether this is an URL or a Tcl proc.
    if {[lsearch $list_of_source_procs [lindex $proc_name 0]] != -1} {

	# Means we are running inside an URL

	# TEST
	for {set i 0} {$i < 6} {incr i} {
	    if {[catch {db_qd_log Debug "LEVEL=$i= [info level [expr "-1 - $i"]]"} errmsg]} {}
	}

	# Check the ad_conn stuff
	if {[ns_conn isconnected]} {
	    if {[catch {db_qd_log Debug "the ad_conn file is [ad_conn file]"} errmsg]} {}
	}

	# Now we do a check to see if this is a directly accessed URL or a 
        # sourced URL

        # added case for handling .vuh files which are sourced from 
        # rp_handle_tcl_request.  Otherwise, QD was forming fullquery path 
        # with the assumption that the query resided in the 
        # rp_handle_tcl_request proc itself. (Openacs - DanW)

        switch $proc_name {

            ns_sourceproc {
                db_qd_log Debug "We are in a WWW page, woohoo!"
                set real_url_p 1
                set url [ns_conn url]
            }


            rp_handle_tcl_request {
                db_qd_log Debug "We are in a VUH page sourced by rp_handle_tcl_request, woohoo!"
                set real_url_p 0
                regsub {\.vuh} [ad_conn file] {} url
                set url [ad_make_relative_path $url]
                regsub {^/?packages} $url {} url
            }
	    adp_parse_ad_conn_file {
                db_qd_log Debug "We are in a VUH page sourced by adp_parse_ad_conn_file, woohoo!"
                set real_url_p 0
                regsub {\.adp} [ad_conn file] {} url
                set url [ad_make_relative_path $url]
                regsub {^/?packages} $url {} url
            }

            template::frm_page_handler {
                db_qd_log Debug "We are in the template system's form page debugger!"
                set real_url_p 1
                regsub {\.frm} [ad_conn url] {} url
            }

            default {
                db_qd_log Debug "We are in a WWW page sourced by apm_source, woohoo!"
                set real_url_p 0
                set url [lindex $proc_name 1]
                set url [ad_make_relative_path $url]
                regsub {^/?packages} $url {} url
            }
        }

	# Get the URL and remove the .tcl
	regsub {^/} $url {} url
	regsub {\.tcl$} $url {} url
	regsub {\.vuh$} $url {} url

	# Change all dots to colons, and slashes to dots
	regsub -all {\.} $url {:} url
	regsub -all {/} $url {.} url

	# We insert the "www" after the package key
	regexp {^([^\.]*)(.*)} $url all package_key rest

	db_qd_log Debug "package key is $package_key and rest is $rest"

	if {$real_url_p} {
	    set full_name [db_qd_make_absolute_path "${package_key}.www${rest}." $local_name]
	    # set full_name "acs.${package_key}.www${rest}.${local_name}"
	} else {
	    set full_name [db_qd_make_absolute_path "${package_key}${rest}." $local_name]
	    # set full_name "acs.${package_key}${rest}.${local_name}"
	}
    } else {
	# Let's find out where this Tcl proc is defined!!
	# Get the first word, which is the Tcl proc
	regexp {^([^ ]*).*} $proc_name all proc_name

        # check to see if a package proc is being called without 
        # namespace qualification.  If so, add the package qualification to the
        # proc_name, so that the correct query can be looked up. 
        # (Openacs - DanW)

        set calling_namespace [string range [uplevel [expr 1 + $added_stack_num] {namespace current}] 2 end]
        db_qd_log Debug "calling namespace = $calling_namespace"

        if {![string equal $calling_namespace ""] && 
            ![regexp {::} $proc_name all]} {

            set proc_name ${calling_namespace}::${proc_name}
        }
	db_qd_log Debug "proc_name is -$proc_name-"

	# We use the ad_proc construct!! 
	# (woohoo, can't believe that was actually useful!)
	
	# First we check if the proc is there. If not, then we're
	# probably dealing with one of the bootstrap procs, and so we just
	# return a bogus proc name
	if {![nsv_exists api_proc_doc $proc_name]} {
	    db_qd_log Debug "there is no documented proc with name $proc_name -- we used default SQL"
	    return [db_qd_null_path]
	}

	array set doc_elements [nsv_get api_proc_doc $proc_name]
	set url $doc_elements(script)

	db_qd_log Debug "tcl file is $url"

	regsub {.tcl$} $url {} url

	# Change all dots to colons, and slashes to dots
	regsub -all {\.} $url {:} url
	regsub -all {/} $url {.} url

	# We get something like packages.acs-tcl.tcl.acs-kernel-procs
	# We need to remove packages.
	regexp {^packages\.(.*)} $url all rest

	db_qd_log Debug "TEMP - QD: proc_name is $proc_name"
	db_qd_log Debug "TEMP - QD: local_name is $local_name"

	# set full_name "acs.$rest.${proc_name}.${local_name}"
	set full_name [db_qd_make_absolute_path "${rest}.${proc_name}." $local_name]
    }

    db_qd_log Debug "generated fullname of $full_name"
    return $full_name
}
proc ad_var_type_check_noquote_p value {

    if [string match *'* $value] {
        return 0
    } else {
        return 1
    }
}
proc server_cluster_logging_p {} {
    return [ad_parameter -package_id [ad_acs_kernel_id] EnableLoggingP server-cluster 0]
}
proc apm_source __file {
    if { ![file exists $__file] } {
		ns_log "Error" "Unable to source $__file: file does not exist."
	return 0
    }

    # Actually do the source.
    if { [catch { source $__file }] } {
	global errorInfo
		ns_log "Error" "Error sourcing $__file:\n$errorInfo"
	return 0
    }

    return 1
}
proc ad_quotehtml arg {
    # we have to do & first or we'll hose ourselves with the ones lower down
    regsub -all & $arg \\&amp\; arg
    regsub -all \" $arg \\&quot\; arg
    regsub -all < $arg \\&lt\; arg
    regsub -all > $arg \\&gt\; arg
    return $arg
}
proc db_qd_relative_path_p path {
    set root_path [db_qd_root_path]
    set root_path_length [string length $root_path]

    # Check if the path starts with the root
    if {[string range $path 0 [expr "$root_path_length - 1"]] == $root_path} {
	return 0
    } else {
	return 1
    }
}
proc ad_permission_p args {    ad_permission_p__arg_parser

  if {[empty_string_p $user_id]} {
    set user_id [ad_verify_and_get_user_id]
  }

  if { [db_string result {
    select count(*) 
      from dual
     where acs_permission.permission_p(:object_id, :user_id, :privilege) = 't'
  }] } {
      return 1
  }

  # This user doesn't have permission. If we're not in performance mode, 
  # Let's check the name of the privilege and throw an error if no
  # such privilege exists.
  if { ![rp_performance_mode] && ![db_string n_privs {
      select count(*)
        from acs_privileges
       where privilege = :privilege
  }] } {
    error "$privilege isn't a valid privilege"
  }

  return 0
}
proc randomInit seed {
    nsv_set rand ia 9301
    nsv_set rand ic 49297
    nsv_set rand im 233280
    nsv_set rand seed $seed
}
proc validate_zip_code {field_name zip_string country_code} {
    if { $country_code == "" || [string toupper $country_code] == "US" } {
	if { [regexp {^[0-9][0-9][0-9][0-9][0-9](-[0-9][0-9][0-9][0-9])?$} $zip_string] } {
	    set zip_5 [string range $zip_string 0 4]
	    if {
		![db_0or1row zip_code_exists {
		    select 1
		      from dual
		     where exists (select 1
				     from zip_codes
				    where zip_code like :zip_5)
		}]
	    } {
		error "The entry for $field_name, \"$zip_string\" is not a recognized zip code"
	    }
	} else {
	    error "The entry for $field_name, \"$zip_string\" does not look like a zip code"
	}
    } else {
	if { $zip_string != "" } {
	    error "Zip code is not needed outside the US"
	}
    }
    return $zip_string
}
proc ad_permission_revoke {user_id object_id privilege} {
    db_exec_plsql revoke_permission {}
}
proc ad_page_contract_filter_proc_boolean {name value_varname} {upvar $value_varname value


    set lcase_value [string tolower $value]
    if {[string match $value "0"] ||  [string match $value "1"] ||  [string match $lcase_value "f"] ||  [string match $lcase_value "t"] ||  [string match $lcase_value "true"] ||  [string match $lcase_value "false"] ||  [string match $lcase_value "y"] ||  [string match $lcase_value "n"] ||  [string match $lcase_value "yes"] ||  [string match $lcase_value "no"] } {
	return 1
    } else {
	ad_complain "$name does not appear to be a boolean value."
	return 0
    }
}
proc tclPkgSetup {dir pkg version files} {
    global auto_index

    package provide $pkg $version
    foreach fileInfo $files {
	set f [lindex $fileInfo 0]
	set type [lindex $fileInfo 1]
	foreach cmd [lindex $fileInfo 2] {
	    if {[string equal $type "load"]} {
		set auto_index($cmd) [list load [file join $dir $f] $pkg]
	    } else {
		set auto_index($cmd) [list source [file join $dir $f]]
	    } 
	}
    }
}
proc ad_html_to_text_put_text {output_var text} {
    upvar $output_var output

    # Expand entities before outputting
    set text [util_expand_entities $text]

    # If we're not in a PRE
    if { $output(pre) <= 0 } {
	# collapse all whitespace
	regsub -all {\s+} $text { } text
	
	# if there's only spaces in the string, wait until later
	if { [string equal $text " "] } {
	    set output(space) 1
	    return
	}
	
	# if it's nothing, do nothing
	if { [empty_string_p $text] } {
	    return
	}
	
	# if the first character is a space, set the space bit
	if { [string equal [string index $text 0] " "] } {
	    set output(space) 1
	    set text [string trimleft $text]
	}
    } else {
	# we're in a PRE: clean line breaks and tabs
	regsub -all {\r\n} $text {\n} text
	regsub -all {\r} $text {\n} text
	# tabs become four spaces
	regsub -all {[\v\t]} $text {    } text
    }

    # output any pending paragraph breaks, line breaks or spaces.
    # as long as we're not at the beginning of the document
    if { $output(p) || $output(br) || $output(space) } {
	if { ![empty_string_p $output(text)] } {
	    if { $output(p) } {
		ad_html_to_text_put_newline output
		ad_html_to_text_put_newline output
	    } elseif { $output(br) } {
		ad_html_to_text_put_newline output
	    } else {
		# Don't add the space if we're at the beginning of a line,
		# unless we're in a PRE
		if { $output(pre) > 0 || $output(linelen) != 0 } {
		    append output(text) " "
		    incr output(linelen)
		}
	    }
	}
	set output(p) 0
	set output(br) 0
	set output(space) 0
    }
    
    # if the last character is a space, save it until the next time
    if { [regexp {^(.*) $} $text match text] } {
	set output(space) 1
    }

    # If there's a blockquote in the beginning of the text, we wouldn't have caught it before
    if { [empty_string_p $output(text)] } {
	append output(text) [string repeat {    } $output(blockquote)]
    }

    # Now output the text.
    while { [regexp {^( +|\s|\S+)(.*)$} $text match word text] } {

	# convert &nbsp;'s
	# We do this now, so that they're displayed, but not treated, whitespace.
	regsub -all {&nbsp;} $word { } word

	set wordlen [string length $word]
	switch -glob -- $word {
	    " *" {
		append output(text) "$word"
		incr output(linelen) $wordlen
	    }
	    "\n" {
		if { ![empty_string_p $output(text)] } {
		    ad_html_to_text_put_newline output
		}
	    }
	    default {
		if { [expr $output(linelen) + $wordlen] > $output(maxlen) && $output(maxlen) != 0 } {
		    ad_html_to_text_put_newline output
		}
		append output(text) "$word"
		incr output(linelen) $wordlen
	    }
	}
    }
}
proc db_qd_pick_most_specific_query {rdbms query_1 query_2} {
    set rdbms_1 [db_fullquery_get_rdbms $query_1]
    set rdbms_2 [db_fullquery_get_rdbms $query_2]

    # We ASSUME that both queries are at least compatible.
    # Otherwise this is a stupid exercise

    if {[empty_string_p [db_rdbms_get_version $rdbms_1]]} {
	return $query_2
    }

    if {[empty_string_p [db_rdbms_get_version $rdbms_2]]} {
	return $query_1
    }

    if {[db_rdbms_get_version $rdbms_1] > [db_rdbms_get_version $rdbms_2]} {
	return $query_1
    } else {
	return $query_2
    }
}
proc ns_time {} {
    return [clock seconds]
}
proc ns_hrefs html {
    # first time this procedure is called it redefines itself based upon
    # whether or not Tcl 8.2 + tcllib packages are available
    if {[catch {
        package require struct
        package require htmlparse
        package require uri
    }]} {
        proc ::nstcl::ns_hrefs html [info body ::nstcl::html::ns_hrefs_simple]
    } else {
        proc ::nstcl::ns_hrefs html [info body ::nstcl::html::ns_hrefs_advanced]
    }

    return [::nstcl::ns_hrefs $html]
}
proc ns_geturl {url {headersSetIdVar {}}} {
    if {[catch { 
        ::nstcl::http::fetch_url -headers $headersSetIdVar $url 
    } result]} {
        error $result
    } else {
        return [lindex $result 1]
    }
}
proc ad_return_forbidden {title explanation} {
    ad_return_exception_page 403 $title $explanation
}
proc sec_handler {} {
    if { [catch { 
	set cookie_list [ad_get_signed_cookie_with_expr "ad_session_id"]
    } errmsg ] } {
	# cookie is invalid because either:
	# -> it was never set
	# -> it failed the cryptographic check
	# -> it expired.

        set new_user_id 0
	# check for permanent login cookie
	if { ![ad_secure_conn_p] } {
            catch {
                set new_user_id [ad_get_signed_cookie "ad_user_login"]
            }
	} else {
            catch {
                set new_user_id [lindex [split [ad_get_signed_cookie "ad_user_login_secure"] {,}] 0]
            }
	}
	# ns_log Notice "OACS= setting up session"
	sec_setup_session $new_user_id
	# ns_log Notice "OACS= done setting up session"
    } else {
	# The session already exists and is valid.
	set cookie_data [split [lindex $cookie_list 0] {,}]
	set session_expr [lindex $cookie_list 1]

	set session_id [lindex $cookie_data 0]
	set user_id [lindex $cookie_data 1]

	# If it's a secure page and not a login page, we check
	# secure token (can't check login page because they aren't
	# issued their secure tokens until after they pass through)
	# It is important to note that the entire secure login
	# system depends on these two functions
  	if { [ad_secure_conn_p] && ![ad_login_page] } {
  	    if { [catch { set sec_token [split [ad_get_signed_cookie "ad_secure_token"] {,}] }] } {
  		# token is incorrect or nonexistent, so we force relogin.
  		ad_returnredirect "/register/index?return_url=[ns_urlencode [ad_conn url]?[ad_conn query]]"
  		return filter_break
  	    } else {
		# need to check only one of the user_id and session_id
		# if the cookie had been tampered.
		if { ![string match [lindex $sec_token 0] $session_id] } {
		    ad_returnredirect "/register/index?return_url=[ns_urlencode [ad_conn url]?[ad_conn query]]"
		    return filter_break
		}
	    }
	}

	ad_conn -set session_id $session_id
	ad_conn -set user_id $user_id

	# reissue session cookie so session doesn't expire if the
	# renewal period has passed. this is a little tricky because
	# the cookie doesn't know about sec_session_renew; it only
	# knows about sec_session_timeout.
	# [sec_session_renew] = SessionTimeout - SessionRenew (see security-init.tcl)
	# $session_expr = PreviousSessionIssue + SessionTimeout
	if { $session_expr - [sec_session_renew] < [ns_time] } {
	    sec_generate_session_id_cookie
	}
    }
}
proc doc_set_mime_type mime_type {
    doc_set_property mime_type $mime_type
}
proc ns_urlencode string {
    set allowed_chars  {[a-zA-Z0-9]}
    set encoded_string ""

    foreach char [split $string ""] {
        if {[string match $allowed_chars $char]} {
            append encoded_string $char
        } else {
            scan $char %c ascii
            append encoded_string %[format %02x $ascii]
        }
    }

    return $encoded_string
}
proc db_qd_make_absolute_path {relative_root suffix} {
    return "[db_qd_root_path]${relative_root}$suffix"
}
proc ns_parsehttptime httptime {
    catch {
        set time [clock scan $httptime]
    }

    # AOLserver behavior is to return 0 on malformed input
    if {![info exists time]} {
        return 0
    }

    set canonical [clock format $time -format "%a, %d %b %Y %H:%M:%S %Z" -gmt 1]
    switch [string equal $canonical $httptime] {
        0 { return 0 }
        1 { return $time }
    }
}
proc db_list_of_lists {statement_name SQL} {
    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]
    set bomb_p [catch {
        set selection [::nstcl::ns_db select $dbhandle $SQL]
    } result]

    if {$bomb_p} {
        ::nstcl::database::api_free_dbhandle $dbhandle
        error $result
    }

    set results {}
    set size [::nstcl::ns_set size $selection]

    while {[::nstcl::ns_db getrow $dbhandle $selection]} {
        set this_row {}
        for {set i 0} {$i < $size} {incr i} {
            lappend this_row [::nstcl::ns_set value $selection $i]
        }
        lappend results $this_row
    }
    
    ::nstcl::ns_set free $selection
    ::nstcl::database::api_free_dbhandle $dbhandle
    
    return $results
}
proc apm_package_create_instance__arg_parser {} {    upvar args args
    upvar package_id val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -package_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_id"
                }
                upvar package_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { instance_name context_id package_key } $n_args_remaining]"
    }
    upvar instance_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar context_id val ; set val [lindex $args [expr { $i + 1 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_page_contract_filter_proc_string_length {name value_varname length} {upvar $value_varname value

    if { [lindex $length 0] == "min" } {
	if { [string length $value] < [lindex $length 1] } {
	    ad_complain "$name is too short.  Please enter a value of at least [lindex $length 1] characters long. The value you entered was [string length $value] characters long."
	    return 0
	}
    } else {
	if { [string length $value] > [lindex $length 1] } {
	    ad_complain "$name is too long.  Please enter a value of at most [lindex $length 1] characters long. The value you entered was [string length $value] characters long."
	    return 0
	}
    }
    return 1
}
proc ad_system_name {} {
    return [ad_parameter -package_id [ad_acs_kernel_id] SystemName]
}
proc last_day_of_month args {
            ::nstcl::_ad_proc_parser ::nstcl::last_day_of_month [set args]
            
    foreach {month year} [clock format [clock scan $date] -format "%b %Y"] break

    switch $month {
        Jan -
        Mar -
        May -
        Jul -
        Aug -
        Oct -
        Dec { set last_day 31 }
        Apr -
        Jun -
        Sep -
        Nov { set last_day 30 }
        Feb { set last_day 28 }
    }

    if {$last_day == 28 && ($year % 4) == 0} {
        if {($year % 100) != 0 || ($year % 400) == 0} {
            set last_day 29
        }
    }

    return [clock format [clock scan "$month $last_day, $year"] -format $format]
}
proc util_memoize_seed {script value {max_age {}}} {
    util_memoize_flush $script

    ns_cache set util_memoize $script [list [ns_time] $value]
}
proc db_with_handle_x {db code_block} {
    upvar 1 $db dbh

    global db_state

    # Initialize bookkeeping variables.
    if { ![info exists db_state(handles)] } {
	set db_state(handles) [list]
    }
    if { ![info exists db_state(n_handles_used)] } {
	set db_state(n_handles_used) 0
    }
    if { $db_state(n_handles_used) >= [llength $db_state(handles)] } {
	set pool [db_nth_pool_name $db_state(n_handles_used)]
	set start_time [clock clicks]
	set errno [catch {
	    set db [ns_db gethandle $pool]
	} error]
	ad_call_proc_if_exists ds_collect_db_call $db gethandle "" $pool $start_time $errno $error
	lappend db_state(handles) $db
	if { $errno } {
	    global errorInfo errorCode
	    return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
	}
    }
    set my_dbh [lindex $db_state(handles) $db_state(n_handles_used)]
    set dbh $my_dbh
    set db_state(last_used) $my_dbh

    incr db_state(n_handles_used)
    set errno [catch { uplevel 1 $code_block } error]
    incr db_state(n_handles_used) -1

    # This may have changed while the code_block was being evaluated.
    set db_state(last_used) $my_dbh

#    ns_db releasehandle $dbh
    # Unset dbh, so any subsequence use of this variable will bomb.
    if { [info exists dbh] } {
	unset dbh
    }


    # If errno is 1, it's an error, so return errorCode and errorInfo;
    # if errno = 2, it's a return, so don't try to return errorCode/errorInfo
    # errno = 3 or 4 give undefined results
    
    if { $errno == 1 } {
	
	# A real error occurred
	global errorInfo errorCode
	return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
    }
    
    if { $errno == 2 } {
	
	# The code block called a "return", so pass the message through but don't try
	# to return errorCode or errorInfo since they may not exist
	
	return -code $errno $error
    }
}
proc db_rdbms_compatible_p {rdbms_test rdbms_pattern} {
    db_qd_log Debug "The RDBMS_TEST is [db_rdbms_get_type $rdbms_test] - [db_rdbms_get_version $rdbms_test]"
    db_qd_log Debug "The RDBMS_PATTERN is [db_rdbms_get_type $rdbms_pattern] - [db_rdbms_get_version $rdbms_pattern]"

    # If the pattern is for all RDBMS, then yeah, compatible
    if {[empty_string_p [db_rdbms_get_type $rdbms_test]]} {
	return 1
    }

    # If the RDBMS types are not the same, we have a problem
    if {[db_rdbms_get_type $rdbms_test] != [db_rdbms_get_type $rdbms_pattern]} {
	db_qd_log Debug "compatibility - RDBMS types are different!"
	return 0
    }

    # If the pattern has no version
    if {[empty_string_p [db_rdbms_get_version $rdbms_pattern]]} {
	return 1
    }

    # If the query being tested was written for a version that is older than 
	# the current RDBMS then we have compatibility. Otherwise we don't.
    if {[db_rdbms_get_version $rdbms_test] <= [db_rdbms_get_version $rdbms_pattern]} {
	return 1
    }

    db_qd_log Debug "compatibility - version numbers are bad!"
    return 0
}
proc ad_get_signed_cookie_with_expr__arg_parser {} {    upvar args args
    upvar secret val ; set val {}
    upvar include_set_cookies val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -include_set_cookies {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -include_set_cookies"
                }
                upvar include_set_cookies val ; set val [lindex $args [incr i]]
            }
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc elapsed_time args {
            ::nstcl::_ad_proc_parser ::nstcl::elapsed_time [set args]
            
    if {![string is integer -strict $date1]} {
        if [catch { set date1 [clock scan $date1] }] {
            error "Unable to convert date1: \"$date1\""
        }
    }
 
    if {![string is integer -strict $date2]} {
        if [catch { set date2 [clock scan $date2] }] {
            error "Unable to convert date2: \"$date2\""
        }
    }
 
    set diff [expr {$date1 - $date2}]
    if {$diff < 0 && $absolute_value_p} {
        set diff [expr {$diff * -1}]
    }
 
    if {$floating_point_p} {
        set diff [expr {$diff * 1.0}]
    }
 
    if [catch { set duration [::nstcl::duration $units] }] {
        error "Unable to calculate size of units: \"$units\""
    }
 
    return [expr {$diff / $duration}]
}
proc ad_parse_html_attributes_upvar args {    ad_parse_html_attributes_upvar__arg_parser
 
    upvar $html_varname html
    upvar $pos_varname i
    if { [info exists attribute_array] } {
	upvar $attribute_array attribute_array_var
    }

    # This is where we're going to return the result
    set attributes {}

    # Loop over the attributes.
    # We maintain counter is so that we don't accidentally enter an infinite loop
    set count 0
    while { $i < [string length $html] && ![string equal [string index $html $i] {>}] } {
	if { [incr count] > 100 } {
	    error "There appears to be a programming bug in ad_parse_html_attributes_upvar: We've entered an infinite loop."
	}
	if { [string equal [string range $html $i [expr { $i + 1 }]] "/>"] } {
	    # This is an XML-style tag ending: <... />
	    break
	}
	
	# This regexp matches an attribute name and an equal sign, if present. 
	# Also eats whitespace before or after.
	# The \A corresponds to ^, except it matches the position we're starting from, not the start of the string
	if { ![regexp -indices -start $i {\A\s*([^\s=>]+)\s*(=?)\s*} $html match attr_name_idx equal_sign_idx] } {
	    # Apparantly, there's no attribute name here. Let's eat all whitespace and lonely equal signs.
	    regexp -indices -start $i {\A[\s=]*} $html match
	    set i [expr { [lindex $match 1] + 1 }]
	} {
	    set attr_name [string tolower [string range $html [lindex $attr_name_idx 0] [lindex $attr_name_idx 1]]]
	    
	    # Move past the attribute name just found
	    set i [expr { [lindex $match 1] + 1}]
	    
	    # If there is an equal sign, we're expecting the next token to be a value
	    if { [lindex $equal_sign_idx 1] - [lindex $equal_sign_idx 0] < 0 } {
		# No equal sign, no value
		lappend attributes $attr_name
		if { [info exists attribute_array] } {
		    set attribute_array_var($attr_name) {}
		}
	    } else {
		
		# is there a single or double quote sign as the first character?
		switch -- [string index $html $i] {
		    {"} { set exp {\A"([^"]*)"\s*} }
		    {'} { set exp {\A'([^']*)'\s*} }
		    default { set exp {\A([^\s>]*)\s*} }
		}
		if { ![regexp -indices -start $i $exp $html match attr_value_idx] } {
		    # No end quote.
		    set attr_value [string range $html [expr {$i+1}] end]
		    set i [string length $html]
		} else {
		    set attr_value [string range $html [lindex $attr_value_idx 0] [lindex $attr_value_idx 1]]
		    set i [expr { [lindex $match 1] + 1}]
		}

		set attr_value [util_expand_entities_ie_style $attr_value]
		
		lappend attributes [list $attr_name $attr_value]
		if { [info exists attribute_array] } {
		    set attribute_array_var($attr_name) $attr_value
		}
	    }
	}
    }
    return $attributes
}
proc with_catch {error_var body on_error} { 
    upvar 1 $error_var $error_var 
    global errorInfo errorCode 
    if [catch { uplevel $body } $error_var] { 
        set code [catch {uplevel $on_error} string] 
        # Return out of the caller appropriately. 
        if { $code == 1 } { 
            return -code error -errorinfo $errorInfo -errorcode $errorCode $string 
        } elseif { $code == 2 } { 
            return -code return $string 
        } elseif { $code == 3 } { 
            return -code break
	} elseif { $code == 4 } {
	    return -code continue
        } elseif { $code > 4 } { 
            return -code $code $string 
        } 
    }         
}
proc apm_load_queries__arg_parser {} {    upvar args args
    upvar callback val ; set val apm_dummy_callback

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_num_instances package_key {
    return [db_exec_plsql apm_num_instances {
	begin
	:1 := apm_package.num_instances(
		package_key => :package_key
		);
	end;
    }]

}
proc apm_interface_add args {    apm_interface_add__arg_parser


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
proc ad_return_template args {    ad_return_template__arg_parser

    if {![empty_string_p $template]} {
	template::set_file  [template::util::url_to_file $template [ad_conn file]]
    }
    
    if { $string_p } {
	return [template::adp_parse  [template::util::url_to_file $template [ad_conn file]] {}]
    }
}
proc ad_pretty_mailing_address_from_args {line1 line2 city state postal_code country_code} {
    set lines [list]
    if [empty_string_p $line2] {
	lappend lines $line1
    } elseif [empty_string_p $line1] {
	lappend lines $line2
    } else {
	lappend lines $line1
	lappend lines $line2
    }
    lappend lines "$city, $state $postal_code"
    if { ![empty_string_p $country_code] && $country_code != "us" } {
	lappend lines [ad_country_name_from_country_code $country_code]
    }
    return [join $lines "\n"]
}
proc util_email_valid_p query_email {
    # This regexp was very kindly contributed by Jeff Friedl, author of 
    # _Mastering Regular Expressions_ (O'Reilly 1997).

    return [regexp "^\[^@<>\"\t ]+@\[^@<>\".\t]+(\\.\[^@<>\".\n ]+)+$" $query_email]
}
proc util_striphtml html {
    return [ad_html_to_text -- $html]
}
proc rp_serve_abstract_file__arg_parser {} {    upvar args args
    upvar nodirectory_p val ; set val 0
    upvar extension_pattern val ; set val .*
    upvar noredirect_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -noredirect - -noredirect=1 {
                uplevel set noredirect_p 1
            }
            -noredirect=0 {
                uplevel set noredirect_p 0
            }
            -nodirectory - -nodirectory=1 {
                uplevel set nodirectory_p 1
            }
            -nodirectory=0 {
                uplevel set nodirectory_p 0
            }
            -extension_pattern {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -extension_pattern"
                }
                upvar extension_pattern val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { path } $n_args_remaining]"
    }
    upvar path val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_package_url_from_key_mem package_key {
    set package_id [apm_package_id_from_key $package_key]
    return [db_string apm_package_url_from_key {
	select site_node.url(node_id) 
          from site_nodes 
         where object_id = :package_id
    } -default ""]
}
proc ns_fmttime {time {format {%a %b %d %T %Y}}} {
    if {[catch  { clock format $time -format $format } result]} {
        error $result
    } else {
        return $result
    }
}
proc ad_parse_documentation_string {doc_string elements_var} {
    upvar $elements_var elements
    if { [info exists elements] } {
        unset elements
    }

    set lines [split $doc_string "\n\r"]

    array set elements [list]
    set current_element main
    set buffer ""

    foreach line $lines {
	
	# lars@pinds.com, 8 July, 2000
	# We don't do a string trim anymore, because it breaks the formatting of 
	# code examples in the documentation, something that we want to encourage.
        
	# set line [string trim $line]

        if { [regexp {^[ \t]*@([-a-zA-Z_]+)(.*)$} $line "" element remainder] } {
            lappend elements($current_element) [string trim $buffer]

            set current_element $element
            set buffer "$remainder\n"
        } else {
            append buffer $line "\n"
        }
    }

    lappend elements($current_element) [string trim $buffer]
}
proc db_continue_transaction {} {
    if {$::nstcl::database::transaction(depth) == 0} {
        error "Can't continue transaction: not currently inside a transaction"
    } else {
        return [set ::nstcl::database::transaction(abort_p) 0]
    }
}
proc ad_ns_set_keys__arg_parser {} {    upvar args args
    upvar exclude val ; set val {}
    upvar colon_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -colon - -colon=1 {
                uplevel set colon_p 1
            }
            -colon=0 {
                uplevel set colon_p 0
            }
            -exclude {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -exclude"
                }
                upvar exclude val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { set_id } $n_args_remaining]"
    }
    upvar set_id val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc populate_secret_tokens_db {} {

    set num_tokens [ad_parameter -package_id [ad_acs_kernel_id] NumberOfCachedSecretTokens security 100]
    # we assume sample size of 10%.
    set num_tokens [expr {$num_tokens * 10}]
    set counter 0
    set list_of_tokens [list]

    # the best thing to use here would be an array_dml, except
    # that an array_dml makes it hard to use sysdate and sequences.
    while { $counter < $num_tokens } {
	set random_token [sec_random_token]

	db_dml insert_random_token {
	    insert /*+ APPEND */ into secret_tokens(token_id, token, timestamp)
	    values(sec_security_token_id_seq.nextval, :random_token, sysdate)
	}
	incr counter
    }

    db_release_unused_handles

}
proc db_html_select_value_options args {    db_html_select_value_options__arg_parser

    set select_options ""

    if { ![empty_string_p $bind] } {
	set options [db_list_of_lists $stmt_name $sql -bind $bind]
    } else {
	set options [uplevel [list db_list_of_lists $stmt_name $sql]]
    }

    foreach option $options {
	if { [string compare $select_option [lindex $option $value_index]] == 0 } {
	    append select_options "<option value=\"[util_quote_double_quotes [lindex $option $value_index]]\" selected>[lindex $option $option_index]\n"
	} else {
	    append select_options "<option value=\"[util_quote_double_quotes [lindex $option $value_index]]\">[lindex $option $option_index]\n"
	}
    }
    return $select_options

}
proc server_cluster_do_httpget {url timeout} {
    if { [catch {
	set page [ns_httpget $url $timeout 0]
	if { ![regexp -nocase successful $page] } {
	    ns_log "Error" "Clustering: ns_httpget $url returned unexpected value. Is /SYSTEM/flush-memoized-statement.tcl set up on this host?"
	}
    } error] } {
	ns_log "Error" "Clustering: Unable to ns_httpget $url (with timeout $timeout): $error"
    }
}
proc ns_sha1 args { 
    return [::sha1::sha1 [lindex args 0]]
}
proc merge_form_with_query__arg_parser {} {    upvar args args
    upvar bind val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -bind {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -bind"
                }
                upvar bind val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { form statement_name sql_qry } $n_args_remaining]"
    }
    upvar form val ; set val [lindex $args [expr { $i + 0 }]]
    upvar statement_name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar sql_qry val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc edprocs args {
    global env

    set tmpFilename /tmp/tcldev.[id process]

    set fp [open $tmpFilename w]
    try_eval {
        puts $fp "\n# TEMP EDIT BUFFER -- YOUR CHANGES ARE FOR THIS SESSION ONLY\n"
        puts $fp [eval "showproc $args"]
    } {} {
        close $fp
    }

    if [info exists env(EDITOR)] {
        set editor $env(EDITOR)
    } else {
	set editor vi
    }

    set startMtime [file mtime $tmpFilename]
    system "$editor $tmpFilename"

    if {[file mtime $tmpFilename] != $startMtime} {
	source $tmpFilename
	echo "Procedures were reloaded."
    } else {
	echo "No changes were made."
    }
    unlink $tmpFilename
    return
}
proc ad_acs_version {} {
    set release_tag {}
    regexp "acs-(\[0-9\]+)-(\[0-9\]+)-(\[0-9\]+)"  $release_tag match major minor release

    if {[info exists major] && [info exists minor] && [info exists release]} {
      return "$major.$minor.$release"
    } else {
      return "development"
    }
}
proc apm_parameter_sync {package_key package_id} {

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
proc ad_looks_like_html_p text {
    if { [regexp -nocase {<p>} $text] || [regexp -nocase {<br>} $text] || [regexp -nocase {</a} $text] } {
	return 1
    } else {
	return 0
    }
}
proc db_fullquery_get_rdbms fullquery {
    return [lindex $fullquery 4]
}
proc set_union! {set_name set} {
    upvar 1 $set_name list
    foreach element $set {
        if {[lsearch -exact $list $element] == -1} {
            lappend list $element
        }
    }
    return $list
}
proc util_aolserver_2_p {} {
    if {[string index [ns_info version] 0] == "2"} {
	return 1
    } else {
	return 0
    }
}
proc acs_package_root_dir package {
    return "[file join [acs_root_dir] packages $package]"
}
proc ad_footer {{signatory {}} {suppress_curriculum_bar_p 0}} {
    global sidegraphic_displayed_p
    if [empty_string_p $signatory] {
	set signatory [ad_system_owner]
    } 
    if { [info exists sidegraphic_displayed_p] && $sidegraphic_displayed_p } {
	# we put in a BR CLEAR=RIGHT so that the signature will clear any side graphic
	# from the ad-sidegraphic.tcl package
	set extra_br "<br clear=right>"
    } else {
	set extra_br ""
    }
    if { [ad_parameter -package_id [ad_acs_kernel_id]  EnabledP curriculum 0] && [ad_parameter -package_id [ad_acs_kernel_id]  StickInFooterP curriculum 0] && !$suppress_curriculum_bar_p} {
	set curriculum_bar "<center>[curriculum_bar]</center>"
    } else {
	set curriculum_bar ""
    }
    if { [llength [info procs ds_link]] == 1 } {
	set ds_link [ds_link]
    } else {
	set ds_link ""
    }
    return "
$extra_br
$curriculum_bar
<hr>
$ds_link
<a href=\"mailto:$signatory\"><address>$signatory</address></a>
</body>
</html>"
}
proc apm_package_create_instance args {    apm_package_create_instance__arg_parser

    if {$package_id == 0} {
	set package_id [db_null]
    } 

    set package_id [db_exec_plsql apm_package_instance_new {
	begin
      :1 := apm_package.new(
        package_id => :package_id,
        instance_name => :instance_name,
        package_key => :package_key,
        context_id => :context_id
      );
	end;
    }]
   
    apm_parameter_sync $package_key $package_id
    
    return $package_id
}
proc ad_set_typed_form_variables {conn args why} {

    global ad_typed_form_variables

    eval lappend ad_typed_form_variables [lindex $args 0]

    return filter_ok
}
proc ns_gifsize file {
    if {![file exists $file] || ![file readable $file]} {
        error "Could not open file \"$file\""
    }

    set fp [open $file r]
    fconfigure $fp -translation binary
    set gif [read $fp 10]
    close $fp

    if {![regexp {^GIF8[79]a(..)(..)$} $gif match width height]} {
        error "Bad file \"$file\""
    }

    binary scan $width s width
    binary scan $height s height
    return [list $width $height]
}
proc apm_version_installed_p version_id {
    return [db_string apm_version_installed_p {
	select 1 from apm_package_versions
	where version_id = :version_id
	and installed_p = 't'
    } -default 0]
}
proc ad_var_type_check_integerlist_p value {

    if [regexp {[^ 0-9,]} $value] {
        return 0
    } else {
        return 1
    }
}
proc site_node_create_package_instance args {    site_node_create_package_instance__arg_parser


    # Create  the package.

    set package_id [apm_package_create_instance $instance_name $context_id $package_key]

    # Update the site map
    db_dml update_site_nodes {
	update site_nodes
	   set object_id = :package_id
	 where node_id = :node_id
    }

    # Flush the in-memory site node map
    if { [string eq $sync_p "t"] } {
	site_nodes_sync
    }

    apm_package_call_post_instantiation_proc $package_id $package_key

    return $package_id

}
proc ad_ssl_available_p {} {
    if { [ns_config ns/server/[ns_info server]/modules nsssl] != "" ||
         [ns_config ns/server/[ns_info server]/modules nsssle] != "" ||
         [ns_config ns/server/[ns_info server]/modules nsopenssl] != "" } {
	return 1
    } else {
	return 0
    }
}
proc ad_get_cookie args {    ad_get_cookie__arg_parser

    if { $include_set_cookies == "t" } {
	set headers [ad_conn outputheaders]
	for { set i 0 } { $i < [ns_set size $headers] } { incr i } {
	    if { ![string compare [string tolower [ns_set key $headers $i]] "set-cookie"] &&  [regexp "^$name=(\[^;\]*)" [ns_set value $headers $i] "" "value"] } {
		return $value
	    }
	}
    }

    set headers [ad_conn headers]
    set cookie [ns_set iget $headers Cookie]
    if { [regexp " $name=(\[^;\]*)" " $cookie" match value] } {

        # If the cookie was set to a blank value we actually stored two quotes.  We need
        # to undo the kludge on the way out.

        if { $value == "\"\"" } {
              set value ""
        }
	return $value
    }

    return $default
}
proc ns_queryget key {     
    return [ ns_set value [ ns_getform ] $key ]
}
proc db_current_rdbms {} {
    return [db_rdbms_create [db_type] [db_version]]
}
proc ad_requested_object_id {} {
    set package_id ""
    #  Use the object id stored in ad_conn.
    if { [ad_conn -connected_p] } {
	set package_id [ad_conn package_id]
    }

    if { [empty_string_p $package_id] } {
	if { [catch {
	    set package_id [ad_acs_kernel_id]
	}] } {
	    set package_id 0
	}
    }
    return $package_id
}
proc apm_callback_and_log args {    apm_callback_and_log__arg_parser

    $callback $message
    ns_log $severity $message
}
proc db_package_supports_rdbms_p db_type_list {
    if { [lsearch $db_type_list [db_type]] != -1 } {
        return 1
    }

    # DRB: Legacy package check - we allow installation of old aD Oracle 4.2 packages,
    # though we don't guarantee that they work.

    if { [db_type] == "oracle" && [lsearch $db_type_list "oracle-8.1.6"] != -1 } {
        return 1
    }

    return 0
}
proc ad_get_tcl_call_stack {{level -2}} {
    if {![string is integer -strict $level]} {
        error "expected integer but got \"$level\""
    }

    set tcl_call_stack ""
    set level [expr {[info level] + $level + 1}]

    while {[incr level -1] > 0} {
        append tcl_call_stack "    called from [info level $level]\n"
    }

    return [string trimright $tcl_call_stack]
}
proc ad_get_signed_cookie args {    ad_get_signed_cookie__arg_parser


    if { $include_set_cookies == "t" } {
	set cookie_value [ns_urldecode [ad_get_cookie $name]]
    } else {
	set cookie_value [ns_urldecode [ad_get_cookie -include_set_cookies f $name]]
    }

    if { [empty_string_p $cookie_value] } {
	error "Cookie does not exist."
    }

    ns_log Debug "Security: Done calling get_cookie $cookie_value for $name."

    set value [lindex $cookie_value 0]
    set signature [lindex $cookie_value 1]

    if { [ad_verify_signature $value $signature] } {
	return $value
    }

    error "Cookie could not be authenticated."
}
proc db_exec_lob {type db statement_name pre_sql {file {}}} {
    set start_time [clock clicks]

    # Query Dispatcher (OpenACS - ben)
    set sql [db_qd_replace_sql $statement_name $pre_sql]

    # insert tcl variable values (Openacs - Dan)
    if {![string equal $sql $pre_sql]} {
        set sql [uplevel 2 [list subst -nobackslashes $sql]]
    }

    # create a function definition statement for the inline code 
    # binding is emulated in tcl. (OpenACS - Dan)

    set errno [catch {
	upvar bind bind
	if { [info exists bind] && [llength $bind] != 0 } {
	    if { [llength $bind] == 1 } {
                set bind_vars [list]
                set len [ns_set size $bind]
                for {set i 0} {$i < $len} {incr i} {
                    lappend bind_vars [ns_set key $bind $i]  [ns_set value $bind $i]
                }
                set lob_sql [db_bind_var_substitution $sql $bind_vars]
	    } else {
                set lob_sql [db_bind_var_substitution $sql $bind]
	    }
	} else {
            set lob_sql [uplevel 2 [list db_bind_var_substitution $sql]]
	}

        # get the content - asssume it is in column 0, or optionally it can
        # be returned as "content" with the storage type indicated by the 
        # "storage_type" column.

        set selection [ns_db 1row $db $lob_sql]
        set content [ns_set value $selection 0]
        for {set i 0} {$i < [ns_set size $selection]} {incr i} {
            set name [ns_set key $selection $i]
            if {[string equal $name storage_type]} {
                set storage_type [ns_set value $selection $i]
            } elseif {[string equal $name content]} {
                set content [ns_set value $selection $i]
            }
        }

        # this is an ugly hack, but it allows content to be written
        # to a file/connection if it is stored as a lob or if it is
        # stored in the content-repository as a file. (DanW - Openacs)

        switch $type {

            blob_get {

                if {[info exists storage_type]} {
                    switch $storage_type {
                        file {
                            if {[file exists $content]} {
                                set ifp [open $content r]

                                # DRB: this could be made faster by setting the buffersize
                                # to the size of the file, but for very large files allocating
                                # that much more memory on top of that needed by Tcl for storage
                                # of the data might not be wise.

                                fconfigure $ifp -translation binary

                                set data [read $ifp]
                                close $ifp
                                return $data
                            } else {
                                error "file: $content doesn't exist"
                            }
                        }

                        lob {
                            if {[regexp {^[0-9]+$} $content match]} {
                                return [ns_pg blob_get $db $content]
                            } else {
                                error "invalid lob_id: should be an integer"
                            }
                        }

                        default {
                            error "invalid storage type"
                        }
                    }
                } elseif {[file exists $content]} {
                    set ifp [open $content r]
                    fconfigure $ifp -translation binary
                    set data [read $ifp]
                    close $ifp
                    return $data
                } elseif {[regexp {^[0-9]+$} $content match]} {
                    return [ns_pg blob_get $db $content]
                } else {
                    error "invalid query"
                }
            }


            blob_select_file {

                if {[info exists storage_type]} {
                    switch $storage_type {
                        file {
                            if {[file exists $content]} {
                                set ifp [open $content r]
                                set ofp [open $file w]
                                ns_cpfp $ifp $ofp
                                close $ifp
                                close $ofp
                            } else {
                                error "file: $content doesn't exist"
                            }
                        }

                        lob {
                            if {[regexp {^[0-9]+$} $content match]} {
                                ns_pg blob_select_file $db $content $file
                            } else {
                                error "invalid lob_id: should be an integer"
                            }
                        }

                        default {
                            error "invalid storage type"
                        }
                    }
                } elseif {[file exists $content]} {
                    set ifp [open $content r]
                    set ofp [open $file w]
                    ns_cpfp $ifp $ofp
                    close $ifp
                    close $ofp
                } elseif {[regexp {^[0-9]+$} $content match]} {
                    ns_pg blob_select_file $db $content $file
                } else {
                    error "invalid query"
                }
            }

            write_blob {

                if {[info exists storage_type]} {
                    switch $storage_type {
                        file {
                            if {[file exists $content]} {
                                set ofp [open $content r]
                                ns_writefp $ofp
                                close $ofp
                            } else {
                                error "file: $content doesn't exist"
                            }
                        }

                        text {
                            ns_write $content
                        }

                        lob {
                            if {[regexp {^[0-9]+$} $content match]} {
                                ns_pg blob_write $db $content
                            } else {
                                error "invalid lob_id: should be an integer"
                            }
                        }

                        default {
                            error "invalid storage type"
                        }
                    }
                } elseif {[file exists $content]} {
                    set ofp [open $content r]
                    ns_writefp $ofp
                    close $ofp
                } elseif {[regexp {^[0-9]+$} $content match]} {
                    ns_pg blob_write $db $content
                } else {
                    ns_write $content
                }
            }
        }

        return

    } error]

    global errorInfo errorCode
    set errinfo $errorInfo
    set errcode $errorCode

    ad_call_proc_if_exists ds_collect_db_call $db 0or1row $statement_name $sql $start_time $errno $error

    if { $errno == 2 } {
	return $error
    }

    return -code $errno -errorinfo $errinfo -errorcode $errcode $error
}
proc util_prepare_insert {table_name form} {

    set form_size [ns_set size $form]
    set form_counter_i 0
    set bind_vars [ns_set create]

    while { $form_counter_i < $form_size } {

 	ns_set update $bind_vars [ns_set key $form $form_counter_i] [string trim [ns_set value $form $form_counter_i]]
 	incr form_counter_i

    }

    return [list "insert into $table_name\n([join [ad_ns_set_keys $bind_vars] ", "])\n values ([join [ad_ns_set_keys -colon $bind_vars] ", "])" $bind_vars]
}
proc ad_login_page {} {

    set url [ad_conn url]
    if { [string match "*register/*" $url] || [string match "/index*" $url] ||  [string match "/" $url] } {
	return 1
    }

    return 0
}
proc ad_get_signed_cookie__arg_parser {} {    upvar args args
    upvar secret val ; set val {}
    upvar include_set_cookies val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -include_set_cookies {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -include_set_cookies"
                }
                upvar include_set_cookies val ; set val [lindex $args [incr i]]
            }
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc auto_qualify {cmd namespace} {

    # count separators and clean them up
    # (making sure that foo:::::bar will be treated as foo::bar)
    set n [regsub -all {::+} $cmd :: cmd]

    # Ignore namespace if the name starts with ::
    # Handle special case of only leading ::

    # Before each return case we give an example of which category it is
    # with the following form :
    # ( inputCmd, inputNameSpace) -> output

    if {[regexp {^::(.*)$} $cmd x tail]} {
	if {$n > 1} {
	    # ( ::foo::bar , * ) -> ::foo::bar
	    return [list $cmd]
	} else {
	    # ( ::global , * ) -> global
	    return [list $tail]
	}
    }
    
    # Potentially returning 2 elements to try  :
    # (if the current namespace is not the global one)

    if {$n == 0} {
	if {[string equal $namespace ::]} {
	    # ( nocolons , :: ) -> nocolons
	    return [list $cmd]
	} else {
	    # ( nocolons , ::sub ) -> ::sub::nocolons nocolons
	    return [list ${namespace}::$cmd $cmd]
	}
    } elseif {[string equal $namespace ::]} {
	#  ( foo::bar , :: ) -> ::foo::bar
	return [list ::$cmd]
    } else {
	# ( foo::bar , ::sub ) -> ::sub::foo::bar ::foo::bar
	return [list ${namespace}::$cmd ::$cmd]
    }
}
proc apm_interface_remove interface_id {
    db_exec_plsql interface_remove {
	begin
	apm_package_version.remove_interface(
             interface_id => :interface_id
	);
	end;					        
    }
}
proc ad_header args {    ad_header__arg_parser

    
    #    if {[ad_parameter MenuOnUserPagesP pdm] == 1} {
    #	return [ad_header_with_extra_stuff -focus $focus $page_title [ad_pdm] [ad_pdm_spacer]]
    #    } else {
    #    }
    return [ad_header_with_extra_stuff -focus $focus $page_title $extra_stuff_for_document_head]

}
proc apm_version_load_status version_id {
    # See if the version was ever loaded.
    if { ![apm_package_version_enabled_p $version_id] } {
	return "never_loaded"
    }

    db_1row package_key_select {
        select package_key
        from apm_package_version_info
        where version_id = :version_id
    }

    foreach file [apm_version_file_list -type "tcl_procs" -db_type [db_type] $version_id] {
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

    foreach file [apm_version_file_list -type "query_file" -db_type [db_type] $version_id] {
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
proc max args {
    set max [lindex $args 0]
    foreach arg $args {
	if { $arg > $max } {
	    set max $arg
	}
    }
    return $max
}
proc ns_httptime time {
    return [clock format $time -format "%a, %d %b %Y %H:%M:%S %Z" -gmt 1]
}
proc ad_proc args {
    set public_p 0
    set private_p 0
    set deprecated_p 0
    set warn_p 0
    set debug_p 0

    # Loop through args, stopping at the first argument which is
    # not a switch.
    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]

        # If the argument doesn't begin with a hyphen, break.
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }

        # If the argument is "--", stop parsing for switches (but
        # bump up $i to the next argument, which is the first
        # argument which is not a switch).
        if { [string equal $arg "--"] } {
            incr i
            break
        }

        switch -- $arg {
            -public { set public_p 1 }
            -private { set private_p 1 }
            -deprecated { set deprecated_p 1 }
            -warn { set warn_p 1 }
            -debug { set debug_p 1 }
            default {
                return -code error "Invalid switch [lindex $args $i] passed to ad_proc"
            }
        }
    }

    if { $public_p && $private_p } {
        return -code error "Mutually exclusive switches -public and -private passed to ad_proc"
    }

    if { $warn_p && !$deprecated_p } {
        return -code error "Switch -warn can be provided to ad_proc only if -deprecated is also provided"
    }

    # Now $i is set to the index of the first non-switch argument.
    # There must be either three or four arguments remaining.
    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining != 3 && $n_args_remaining != 4 } {
        return -code error "Wrong number of arguments passed to ad_proc"
    }

    # Set up the remaining arguments.
    set proc_name [lindex $args $i]

    # (SDW - OpenACS). If proc_name is being defined inside a namespace, we
    # want to use the fully qualified name. Except for actually defining the
    # proc where we want to use the name as passed to us. We always set
    # proc_name_as_passed and conditionally make proc_name fully qualified
    # if we were called from inside a namespace eval.

    set proc_name_as_passed $proc_name
    set proc_namespace [uplevel {namespace current}]
    if { $proc_namespace != "::" } {
	regsub {^::} $proc_namespace {} proc_namespace
	set proc_name "${proc_namespace}::${proc_name}"
    }

    set arg_list [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining == 3 } {
        # No doc string provided.
        array set doc_elements [list]
	set doc_elements(main) ""
    } else {
        # Doc string was provided.
        ad_parse_documentation_string [lindex $args end-1] doc_elements
    }
    set code_block [lindex $args end]

    #####
    #
    #  Parse the argument list.
    #
    #####

    set switches [list]
    set positionals [list]
    set seen_positional_with_default_p 0
    set n_positionals_with_defaults 0
    array set default_values [list]
    array set flags [list]
    set varargs_p 0
    set switch_code ""

    # If the first element contains 0 or more than 2 elements, then it must
    # be an old-style ad_proc. Mangle effective_arg_list accordingly.
    if { [llength $arg_list] > 0 } {
        set first_arg [lindex $arg_list 0]
        if { [llength $first_arg] == 0 || [llength $first_arg] > 2 } {
            set new_arg_list [list]
            foreach { switch default_value } $first_arg {
                lappend new_arg_list [list $switch $default_value]
            }
            set arg_list [concat $new_arg_list [lrange $arg_list 1 end]]
        }
    }

    set effective_arg_list $arg_list

    set last_arg [lindex $effective_arg_list end]
    if { [llength $last_arg] == 1 && [string equal [lindex $last_arg 0] "args"] } {
        set varargs_p 1
        set effective_arg_list [lrange $effective_arg_list 0 [expr { [llength $effective_arg_list] - 2 }]]
    }

    set check_code ""
    foreach arg $effective_arg_list {
        if { [llength $arg] == 2 } {
            set default_p 1
            set default_value [lindex $arg 1]
            set arg [lindex $arg 0]
        } else {
            if { [llength $arg] != 1 } {
                return -code error "Invalid element \"$arg\" in argument list"
            }
            set default_p 0
        }

        set arg_flags [list]
        set arg_split [split $arg ":"]
        if { [llength $arg_split] == 2 } {
            set arg [lindex $arg_split 0]
            foreach flag [split [lindex $arg_split 1] ","] {
                if { ![string equal $flag "required"] && ![string equal $flag "boolean"] } {
                    return -code error "Invalid flag \"$flag\""
                }
                lappend arg_flags $flag
            }
        } elseif { [llength $arg_split] != 1 } {
            return -code error "Invalid element \"$arg\" in argument list"
        }

        if { [string equal [string index $arg 0] "-"] } {
            if { [llength $positionals] > 0 } {
                return -code error "Switch -$arg specified after positional parameter"
            }

            set switch_p 1
            set arg [string range $arg 1 end]
            lappend switches $arg

            if { [lsearch $arg_flags "boolean"] >= 0 } {
                set default_values(${arg}_p) 0
		append switch_code "            -$arg - -$arg=1 {
                uplevel set ${arg}_p 1
            }
            -$arg=0 {
                uplevel set ${arg}_p 0
            }
"
            } else {
		append switch_code "            -$arg {
                if { \$i >= \[llength \$args\] - 1 } {
                    return -code error \"No argument to switch -$arg\"
                }
                upvar ${arg} val ; set val \[lindex \$args \[incr i\]\]\n"
		append switch_code "            }\n"
            }

            if { [lsearch $arg_flags "required"] >= 0 } {
                append check_code "    if { !\[uplevel info exists $arg\] } {
        return -code error \"Required switch -$arg not provided\"
    }
"
            }
        } else {
            set switch_p 0
            if { $default_p } {
                incr n_positionals_with_defaults
            }
            if { !$default_p && $n_positionals_with_defaults != 0 } {
                return -code error "Positional parameter $arg needs a default value (since it follows another positional parameter with a default value)"
            }
            lappend positionals $arg
        }

        set flags($arg) $arg_flags

        if { $default_p } {
            set default_values($arg) $default_value
        }

        if { [llength $arg_split] > 2 } {
            return -code error "Invalid format for parameter name: \"$arg\""
        }
    }

    foreach element { public_p private_p deprecated_p warn_p varargs_p arg_list switches positionals } {
        set doc_elements($element) [set $element]
    }
    foreach element { default_values flags } {
        set doc_elements($element) [array get $element]
    }
    
    set root_dir [nsv_get acs_properties root_directory]
    set script [info script]
    set root_length [string length $root_dir]
    if { ![string compare $root_dir [string range $script 0 [expr { $root_length - 1 }]]] } {
        set script [string range $script [expr { $root_length + 1 }] end]
    }
    
    set doc_elements(script) $script
    if { ![nsv_exists api_proc_doc $proc_name] } {
        nsv_lappend api_proc_doc_scripts $script $proc_name
    }

    nsv_set api_proc_doc $proc_name [array get doc_elements]

    # Backward compatibility: set proc_doc and proc_source_file
    nsv_set proc_doc $proc_name [lindex $doc_elements(main) 0]
    if { [nsv_exists proc_source_file $proc_name]  && [string compare [nsv_get proc_source_file $proc_name] [info script]] != 0 } {
        ns_log Notice "Multiple definition of $proc_name in [nsv_get proc_source_file $proc_name] and [info script]"
    }
    nsv_set proc_source_file $proc_name [info script]

    if { [string equal $code_block "-"] } {
        return
    }

    if { [llength $switches] == 0 } {
        uplevel [list proc $proc_name_as_passed $arg_list $code_block]
    } else {
        set parser_code "    upvar args args\n"

        foreach { name value } [array get default_values] {
            append parser_code "    upvar $name val ; set val [list $value]\n"
        }
        
        append parser_code "
    for { set i 0 } { \$i < \[llength \$args\] } { incr i } {
        set arg \[lindex \$args \$i\]
        if { !\[ad_proc_valid_switch_p \$arg\] } {
            break
        }
        if { \[string equal \$arg \"--\"\] } {
            incr i
            break
        }
        switch -- \$arg {
$switch_code
            default { return -code error \"Invalid switch: \\\"\$arg\\\"\" }
        }
    }
"

        set n_required_positionals [expr { [llength $positionals] - $n_positionals_with_defaults }]
        append parser_code "
    set n_args_remaining \[expr { \[llength \$args\] - \$i }\]
    if { \$n_args_remaining < $n_required_positionals } {
        return -code error \"No value specified for argument \[lindex { [lrange $positionals 0 [expr { $n_required_positionals - 1 }]] } \$n_args_remaining\]\"
    }
"
        for { set i 0 } { $i < $n_required_positionals } { incr i } {
            append parser_code "    upvar [lindex $positionals $i] val ; set val \[lindex \$args \[expr { \$i + $i }\]\]\n"
        }
        for {} { $i < [llength $positionals] } { incr i } {
		append parser_code "    if { \$n_args_remaining > $i } {
        upvar [lindex $positionals $i] val ; set val \[lindex \$args \[expr { \$i + $i }\]\]
    }
"
        }
    
        if { $varargs_p } {
            append parser_code "    set args \[lrange \$args \[expr { \$i + [llength $positionals] }\] end\]\n"
        } else {
            append parser_code "    if { \$n_args_remaining > [llength $positionals] } {
        return -code error \"Too many positional parameters specified\"
    }
    unset args
"
        }

        append parser_code $check_code

        if { $debug_p } {
            ns_write "PARSER CODE:\n\n$parser_code\n\n"
        }

        uplevel [list proc ${proc_name_as_passed}__arg_parser {} $parser_code]
        uplevel [list proc $proc_name_as_passed args "    ${proc_name_as_passed}__arg_parser\n$code_block"]
    }
}
proc apm_parameter_update args {    apm_parameter_update__arg_parser

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
proc nmc_IllustraDatetoPrettyDate sql_date {

    regexp {(.*)-(.*)-(.*)$} $sql_date match year month day

    set allthemonths {January February March April May June July August September October November December}

    # we have to trim the leading zero because Tcl has such a 
    # brain damaged model of numbers and decided that "09-1"
    # was "8.0"

    set trimmed_month [string trimleft $month 0]
    set pretty_month [lindex $allthemonths [expr $trimmed_month - 1]]

    return "$pretty_month $day, $year"

}
proc sysdate args {
            ::nstcl::_ad_proc_parser ::nstcl::sysdate [set args]
            
    if {[string equal $base ""]} {
        set time [clock seconds]
    } else {
        if {![string is integer -strict $base]} {
            set time [clock scan $base]
        } else {
            set time $base
        }
    }
 
    set time [clock scan $offset -base $time]

    if {$seconds_p} {
        return $time
    } else {
        return [clock format $time -format $format]
    }
}
proc rp_serve_abstract_file args {    rp_serve_abstract_file__arg_parser

  if { [string equal [string index $path end] "/"] } {
    if { [file isdirectory $path] } {
      # The path specified was a directory; return its index file.

      # Directory name with trailing slash. Search for an index.* file.
      # Remember the name of the directory in $dir_index, so we can later
      # generate a directory listing if necessary.
      set dir_index $path
      set path "[string trimright $path /]/index"

  } else {

    # If there's a trailing slash on the path, the URL must refer to a
    # directory (which we know doesn't exist, since [file isdirectory $path]
    # returned 0).
      ad_raise notfound
    }
  }

  ### no more trailing slash.

  if { [file isfile $path] } {
    # It's actually a file.
    ad_conn -set file $path
  } else {
    # The path provided doesn't correspond directly to a file - we
    # need to glob.   (It could correspond directly to a directory.)

    if { ![file isdirectory [file dirname $path]] } {
      ad_raise notfound
    }

    ad_conn -set file [rp_concrete_file -extension_pattern $extension_pattern $path]
    
    if { [empty_string_p [ad_conn file]] } {
      
      if { [file isdirectory $path] && !$noredirect_p } {
	# Directory name with no trailing slash. Redirect to the same
	# URL but with a trailing slash.
	
	set url "[ad_conn url]/"
	if { [ad_conn query] != "" } {
	  append url "?[ad_conn query]"
	}
	
	ad_raise redirect $url
      } else {
	if { [info exists dir_index] && !$nodirectory_p } {
	  ad_raise directory $dir_index
	} else {
	  # Nothing at all found! 404 time.
	  ad_raise notfound
	}
      }
    }
  }

  rp_serve_concrete_file [ad_conn file]
}
proc ad_permission_grant {user_id object_id privilege} {
    db_exec_plsql grant_permission {}
}
proc ad_page_contract_filter_proc_html {name value_varname} {upvar $value_varname value

    set naughty_prompt [ad_html_security_check $value]
    if { ![empty_string_p $naughty_prompt] } {
	ad_complain $naughty_prompt
	return 0
    }
    return 1
}
proc set_member? {set element} {
    return [expr {[lsearch -exact $set $element] != -1}]
}
proc ad_html_to_text__arg_parser {} {    upvar args args
    upvar maxlen val ; set val 70
    upvar showtags_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -maxlen {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -maxlen"
                }
                upvar maxlen val ; set val [lindex $args [incr i]]
            }
            -showtags - -showtags=1 {
                uplevel set showtags_p 1
            }
            -showtags=0 {
                uplevel set showtags_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { html } $n_args_remaining]"
    }
    upvar html val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_load_libraries__arg_parser {} {    upvar args args
    upvar callback val ; set val apm_dummy_callback
    upvar procs_p val ; set val 0
    upvar init_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }
            -procs - -procs=1 {
                uplevel set procs_p 1
            }
            -procs=0 {
                uplevel set procs_p 0
            }
            -init - -init=1 {
                uplevel set init_p 1
            }
            -init=0 {
                uplevel set init_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_package_url_from_key package_key {
    return [util_memoize "apm_package_url_from_key_mem $package_key"]
}
proc export_url_vars args {    export_url_vars__arg_parser
 
    set params {} 
    foreach var_spec $args { 
	if { [string first "=" $var_spec] != -1 } {
	    # There shouldn't be more than one equal sign, since the value should already be url-encoded.
	    set var_spec_pieces [split $var_spec "="]
	    set var [lindex $var_spec_pieces 0]
	    set value [lindex $var_spec_pieces 1]
	    lappend params "$var=$value"
	    if { $sign_p } {
		lappend params "[ns_urlencode [ns_urldecode $var]:sig]=[ns_urlencode [ad_sign [ns_urldecode $value]]]"
	    }
	} else {
	    set var_spec_pieces [split $var_spec ":"]
	    set var [lindex $var_spec_pieces 0]
	    set type [lindex $var_spec_pieces 1]
	    
	    upvar 1 $var upvar_value
	    if { [info exists upvar_value] } {
		switch $type {
		    multiple {
			foreach item $upvar_value {
			    lappend params "[ns_urlencode $var]=[ns_urlencode $item]"
			}
		    }
		    default {
			lappend params "[ns_urlencode $var]=[ns_urlencode $upvar_value]" 
		    }
		}
		if { $sign_p } {
		    lappend params "[ns_urlencode "$var:sig"]=[ns_urlencode [ad_sign $upvar_value]]"
		}
	    }
	}
    }
    
  return [join $params "&"]
}
proc ad_package_admin_home package_key {
    return "[ad_admin_home]/$package_key"
}
proc ad_get_cookie__arg_parser {} {    upvar args args
    upvar default val ; set val {}
    upvar include_set_cookies val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -include_set_cookies {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -include_set_cookies"
                }
                upvar include_set_cookies val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { name } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        upvar default val ; set val [lindex $args [expr { $i + 1 }]]
    }
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_convert_plaintext_to_html raw_string {
    if { [regexp -nocase {<p>} $raw_string] || [regexp -nocase {<br>} $raw_string] } {
	# user was already trying to do this as HTML
	return $raw_string
    } else {
	return [ad_text_to_html -no_links -- $raw_string]
    }
}
proc random {} {
    nsv_set rand seed [expr ([nsv_get rand seed] * [nsv_get rand ia] + [nsv_get rand ic]) % [nsv_get rand im]]
    return [expr [nsv_get rand seed]/double([nsv_get rand im])]
}
proc util_email_unique_p email {
    return [db_string email_unique_p {}]
}
proc db_foreach {statement_name pre_sql args} {
#        ns_log info "db_foreach called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_foreach $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_foreach $statement_name $sql] $args]
    }
proc set_form_variables {{error_if_not_found_p 1}} {
    if { $error_if_not_found_p == 1} {
	uplevel { if { [ns_getform] == "" } {
	    ns_returnerror 500 "Missing form data"
	    return
	}
       }
     } else {
	 uplevel { if { [ns_getform] == "" } {
	     # we're not supposed to barf at the user but we want to return
	     # from this subroutine anyway because otherwise we'd get an error
	     return
	 }
     }
  }

    # at this point we know that the form is legal
    # The variable names are prefixed with a V to avoid confusion with the form variables while checking for naughtiness.
    uplevel {
	set Vform [ns_getform] 
	set Vform_size [ns_set size $Vform]
	set Vform_counter_i 0
	while {$Vform_counter_i<$Vform_size} {
	    set Vname [ns_set key $Vform $Vform_counter_i]
	    set Vvalue [ns_set value $Vform $Vform_counter_i]
	    check_for_form_variable_naughtiness $Vname $Vvalue
	    set $Vname $Vvalue
	    incr Vform_counter_i
	}
    }
}
proc db_nextval sequence {
    return [db_string nextval "select ${sequence}.nextval"]
}
proc set_form_variables_string_trim {} {
    # The variable names are prefixed with a V to avoid confusion with the form variables while checking for naughtiness.
    uplevel {
	set Vform [ns_getform] 
	if {$Vform == ""} {
	    ns_returnerror 500 "Missing form data"
	    return;
	}
	set Vform_size [ns_set size $Vform]
	set Vform_counter_i 0
	while {$Vform_counter_i<$Vform_size} {
	    set Vname [ns_set key $Vform $Vform_counter_i]
	    set Vvalue [ns_set value $Vform $Vform_counter_i]
	    check_for_form_variable_naughtiness $Vname $Vvalue
	    set $Vname [string trim $Vvalue]
	    incr Vform_counter_i
	}
    }
}
proc util_expand_entities html {
    return [string map [array get ::nstcl::html::entities_map] $html]
}
proc apm_load_any_changed_libraries {} {
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

	if { [file exists $path] &&  (![nsv_exists apm_library_mtime $file] ||  [file mtime $path] != [nsv_get apm_library_mtime $file]) } {
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

		nsv_set apm_library_mtime $file [file mtime $file_path]
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
proc apm_parameter_unregister__arg_parser {} {    upvar args args
    upvar callback val ; set val apm_dummy_callback

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { parameter_id } $n_args_remaining]"
    }
    upvar parameter_id val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_arg_parser {allowed_args argv} {
    if {[string equal [lindex $allowed_args end] "args"]} {
        upvar 1 args args
        set args {}
        set allowed_args [lrange $allowed_args 0 end-1]
    }

    foreach switch $allowed_args {
        set switches($switch) 0
    }


    set switches_present {}
    foreach {switch value} $argv {
        if {![regexp {^-(.+)$} $switch => switch]} {
            if {[info exists args]} {
                set args $argv
                return $switches_present
            } else {
                return -code error "Expected switch but encountered \"$switch\""
            }
        }

        if {![info exists switches($switch)]} {
            return -code error "Invalid switch -$switch (expected one of -[join [lsort [array names switches]] ", -"])"
        } elseif {$switches($switch)} {
            return -code error "Switch -$switch already specified"
        } else {
            upvar 1 $switch var
            set var $value
            incr switches($switch)
            lappend switches_present $switch
        }

        # pop the switch/value off of argv so that we can just set args 
        # to argv if need be
        if {[llength $argv] < 2} {
            return -code error "Invalid switch syntax - no argument to final switch \"[lindex $argv end]\""
        } else {
            set argv [lrange $argv 2 end]
        }
    }

    return $switches_present
}
proc db_map snippet_name {
    set fullname [db_qd_get_fullname $snippet_name]
    set fullquery [db_qd_fetch $fullname]
    set sql [db_fullquery_get_querytext $fullquery]

    db_qd_log Debug "PARTIALQUERY FOR $fullname: $sql"
    return [uplevel 1 [list subst -nobackslashes $sql]]
}
proc ad_page_contract_filter_type filter {
    if { [nsv_exists ad_page_contract_filters $filter] } {
	return [lindex [nsv_get ad_page_contract_filters $filter] 0]
    } else {
	return {}
    }
}
proc apm_doc_body_callback string {
    doc_body_append $string
}
proc db_bind_var_substitution {sql {bind {}}} {
    if {[string equal $bind ""]} {
        upvar __db_sql lsql
        set lsql $sql
        uplevel {            
            set __db_lst [regexp -inline -indices -all -- {:?:\w+} $__db_sql]
            for {set __db_i [expr [llength $__db_lst] - 1]} {$__db_i >= 0} {incr __db_i -1} {
                set __db_ws [lindex [lindex $__db_lst $__db_i] 0]
                set __db_we [lindex [lindex $__db_lst $__db_i] 1]
                set __db_bind_var [string range $__db_sql $__db_ws $__db_we]
                if {![string match "::*" $__db_bind_var]} {
                    set __db_tcl_var [string range $__db_bind_var 1 end]
                    set __db_tcl_var [set $__db_tcl_var]
                    if {[string equal $__db_tcl_var ""]} {
                        set __db_tcl_var null
                    } else {
                        set __db_tcl_var "'[DoubleApos $__db_tcl_var]'"
                    }
                    set __db_sql [string replace $__db_sql $__db_ws $__db_we $__db_tcl_var]
                }                
            }
        }
    } else {

        array set bind_vars $bind

        set lsql $sql
        set lst [regexp -inline -indices -all -- {:?:\w+} $sql]
        for {set i [expr [llength $lst] - 1]} {$i >= 0} {incr i -1} {
            set ws [lindex [lindex $lst $i] 0]
            set we [lindex [lindex $lst $i] 1]
            set bind_var [string range $sql $ws $we]
            if {![string match "::*" $bind_var]} {
                set tcl_var [string range $bind_var 1 end]
                set val $bind_vars($tcl_var)
                if {[string equal $val ""]} {
                    set val null
                } else {
                    set val "'[DoubleApos $val]'"
                }
                set lsql [string replace $lsql $ws $we $val]
            }                
        }
    }

    return $lsql
}
proc db_type {} {
    return [nsv_get ad_database_type .]
}
proc sub_page_validation args {
    # to allow this to be at any level, we search up the stack for {%%exception_list}
    set depth [info level]
    for {set level 1} {$level <= $depth} {incr level} {
	upvar $level {%%exception_list} {%%exception_list}
	if { [info exists {%%exception_list}] } {
	    break
	}
    }
    if { ![info exists {%%exception_list}] } {
	error "sub_page_validation not inside page_validation"
    }
    foreach validation_block $args {
	if { [catch {uplevel $validation_block} errmsg] } {
	    lappend {%%exception_list} $errmsg
	}
    }
}
proc ns_param {key value} {
    variable configuration
    upvar 0 ::nstcl::nssets::section section

    set ndx [::nstcl::ns_set find $configuration $section]
    if {$ndx == -1} {
        set setId [::nstcl::ns_set create $section]
        ::nstcl::ns_set put $configuration $section $setId
    } else {
        set setId [::nstcl::ns_set value $configuration $ndx]
    }

    ::nstcl::ns_set update $setId $key $value
}
proc ad_admin_header__arg_parser {} {    upvar args args
    upvar focus val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -focus {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -focus"
                }
                upvar focus val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { page_title } $n_args_remaining]"
    }
    upvar page_title val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ns_share args {
            ::nstcl::_ad_proc_parser ::ns_share [set args]
            

    global $name
    if ![array exists $name] {
	array set $name [list]
    }

    uplevel [list global $name ]

    ns_log Warn  "ns_share not really implemented:  [info level -1]"

}
proc sec_random_token {} {
    # tcl_sec_seed is used to maintain a small subset of the previously
    # generated random token to use as the seed for the next
    # token. this makes finding a pattern in sec_random_token harder
    # to guess when it is called multiple times in the same thread.
    global tcl_sec_seed

    if { [ad_conn -connected_p] } {
        set request [ad_conn request]
	set start_clicks [ad_conn start_clicks]
    } else {
	set request "yoursponsoredadvertisementhere"
	set start_clicks "developer.arsdigita.com"
    }
    
    if { ![info exists tcl_sec_seed] } {
	set tcl_sec_seed "listentowmbr89.1"
    }

    set random_base [ns_sha1 "[ns_time][ns_rand]$start_clicks$request$tcl_sec_seed"]
    set tcl_sec_seed [string range $random_base 0 10]
    
    return [ns_sha1 [string range $random_base 11 39]]
}
proc db_with_handle {db code_block} {
    upvar 1 $db dbh

    global db_state

    # Initialize bookkeeping variables.
    if { ![info exists db_state(handles)] } {
	set db_state(handles) [list]
    }
    if { ![info exists db_state(n_handles_used)] } {
	set db_state(n_handles_used) 0
    }
    if { $db_state(n_handles_used) >= [llength $db_state(handles)] } {
	set pool [db_nth_pool_name $db_state(n_handles_used)]
	set start_time [clock clicks]
	set errno [catch {
	    set db [ns_db gethandle $pool]
	} error]
	ad_call_proc_if_exists ds_collect_db_call $db gethandle "" $pool $start_time $errno $error
	lappend db_state(handles) $db
	if { $errno } {
	    global errorInfo errorCode
	    return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
	}
    }
    set my_dbh [lindex $db_state(handles) $db_state(n_handles_used)]
    set dbh $my_dbh
    set db_state(last_used) $my_dbh

    incr db_state(n_handles_used)
    set errno [catch { uplevel 1 $code_block } error]
    incr db_state(n_handles_used) -1

    # This may have changed while the code_block was being evaluated.
    set db_state(last_used) $my_dbh

    # Unset dbh, so any subsequence use of this variable will bomb.
    if { [info exists dbh] } {
	unset dbh
    }


    # If errno is 1, it's an error, so return errorCode and errorInfo;
    # if errno = 2, it's a return, so don't try to return errorCode/errorInfo
    # errno = 3 or 4 give undefined results
    
    if { $errno == 1 } {
	
	# A real error occurred
	global errorInfo errorCode
	return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
    }
    
    if { $errno == 2 } {
	
	# The code block called a "return", so pass the message through but don't try
	# to return errorCode or errorInfo since they may not exist
	
	return -code $errno $error
    }
}
proc ad_page_variables variable_specs {
    set exception_list [list]
    set form [ns_getform]
    if { $form != "" } {
	set form_size [ns_set size $form]
	set form_counter_i 0

	# first pass -- go through all the variables supplied in the form
	while {$form_counter_i<$form_size} {
	    set variable [ns_set key $form $form_counter_i]
	    set value [ns_set value $form $form_counter_i]
	    check_for_form_variable_naughtiness $variable $value
	    set found "not"
	    # find the matching variable spec, if any
	    foreach variable_spec $variable_specs {
		if { [llength $variable_spec] >= 2 } {
		    switch -- [lindex $variable_spec 1] {
			-multiple-list {
			    if { [lindex $variable_spec 0] == $variable } {
				# variable gets a list of all the values
				upvar 1 $variable var
				lappend var $value
				set found "done"
				break
			    }
			}
			-array {
			    set varname [lindex $variable_spec 0]
			    set pattern "($varname)_(.+)"
			    if { [regexp $pattern $variable match array index] } {
				if { ![empty_string_p $array] } {
				    upvar 1 $array arr
				    set arr($index) [ns_set value $form $form_counter_i]
				}
				set found "done"
				break
			    }
			}
			default {
			    if { [lindex $variable_spec 0] == $variable } {
				set found "set"
				break
			    }
			}
		    }
		} elseif { $variable_spec == $variable } {
		    set found "set"
		    break
		}
	    }
	    if { $found == "set" } {
		upvar 1 $variable var
		if { ![info exists var] } {
		    # take the leftmost value, if there are multiple ones
		    set var $value
		}
	    }
	    incr form_counter_i
	}
    }

    # now make a pass over each variable spec, making sure everything required is there
    # and doing defaulting for unsupplied things that aren't required
    foreach variable_spec $variable_specs {
	set variable [lindex $variable_spec 0]
	upvar 1 $variable var

	if { [llength $variable_spec] >= 2 } {
	    if { ![info exists var] } {
		set default_value_or_flag [lindex $variable_spec 1]
		
		switch -- $default_value_or_flag {
		    -array {
			# don't set anything
		    }
		    -multiple-list {
			set var [list]
		    }
		    default {
			# Needs to be set.
			uplevel [list eval set $variable "\[subst [list $default_value_or_flag]\]"]
			# This used to be:
			#
			#   uplevel [list eval [list set $variable "$default_value_or_flag"]]
			#
			# But it wasn't properly performing substitutions.
		    }
		}
	    }

	    # no longer needed because we QQ everything by default now
	    #	    # if there is a QQ or qq or any variant after the var_spec,
	    #	    # make a "QQ" variable
	    #	    if { [regexp {^[Qq][Qq]$} [lindex $variable_spec 2]] && [info exists var] } {
	    #		upvar QQ$variable QQvar
	    #		set QQvar [DoubleApos $var]
	    #	    }

	} else {
	    if { ![info exists var] } {
		lappend exception_list "\"$variable\" required but not supplied"
	    }
	}

        # modified by rhs@mit.edu on 1/31/2000
	# to QQ everything by default (but not arrays)
        if {[info exists var] && ![array exists var]} {
	    upvar QQ$variable QQvar
	    set QQvar [DoubleApos $var]
	}

    }

    set n_exceptions [llength $exception_list]
    # this is an error in the HTML form
    if { $n_exceptions == 1 } {
	ns_returnerror 500 [lindex $exception_list 0]
	return -code return
    } elseif { $n_exceptions > 1 } {
	ns_returnerror 500 "<li>[join $exception_list "\n<li>"]\n"
	return -code return
    }
}
proc ReturnHeaders {{content_type text/html}} {
    set all_the_headers "HTTP/1.0 200 OK
MIME-Version: 1.0
Content-Type: $content_type\r\n"
     util_WriteWithExtraOutputHeaders $all_the_headers
     ns_startcontent -type $content_type
}
proc site_nodes_sync_helper args {
  db_foreach nodes_select {
    select site_node.url(n.node_id) as url, n.node_id, n.directory_p,
           n.pattern_p, n.object_id, o.object_type, n.package_key, n.package_id
    from acs_objects o, (select n.node_id, n.directory_p, n.pattern_p, n.object_id, p.package_key, p.package_id
                           from site_nodes n, apm_packages p
                          where n.object_id = p.package_id) n
    where n.object_id = o.object_id (+)
  } {

    set val(url) $url
    set val(node_id) $node_id
    set val(directory_p) $directory_p
    set val(pattern_p) $pattern_p
    set val(object_id) $object_id
    set val(object_type) $object_type
    set val(package_key) $package_key
    set val(package_id) $package_id

    set nodes($url) [array get val]
  }
  return [array get nodes]
}
proc ad_restrict_entire_server_to_registered_users {conn args why} {
    if {![string match "/index.tcl" [ad_conn url]] && ![string match "/" [ad_conn url]] && ![string match "/register/*" [ad_conn url]] && ![string match "/SYSTEM/*" [ad_conn url]] && ![string match "/user_please_login.tcl" [ad_conn url]]} {
	# not one of the magic acceptable URLs
	set user_id [ad_conn user_id]
	if {$user_id == 0} {
	    ad_returnredirect "/register/?return_url=[ns_urlencode [ad_conn url]?[ad_conn query]]"
	    return filter_return
	}
    }
    return filter_ok
}
proc nsv_set {id key value} {  

#    ns_log Notice "Using nsv_set \n"
#	    namespace eval nsv {	#	    puts "array doesn't exist" }
#do I need to put this in global before checking info?
    global $id
    if {! [array exists $id] } { 
#	puts "Globalizing $id"
#	global $id
	array set $id [list]

    }

#    if { $key == "." } { 
#    	eval [format {set %s(.) { %s } }   $id $key $value ]
#    } else {
	set ${id}($key) $value  
#	eval [format {set %s(%s) { %s } }   $id $key $value ]
#    }
    #puts "nsv_set: $id - $key = $value\n\n"   

    #puts [format {set %s(%s) { %s } } $id $key $value ]
    #If array doesn't exist,  create it
    #    if {[info exists $set]} 
    #if { $key == "." } { 
    #	eval [format {set %s(.) { %s } }   $id $key $value ]
    #} else {
    #	eval [format {set %s(%s) { %s } }   $id $key $value ]
        
    #}
    #set value
    #parray ${id}    
    return $value
}
proc apm_load_queries args {    apm_load_queries__arg_parser

    set packages [db_list apm_enabled_packages_q {
	select distinct package_key
	from apm_package_versions
	where enabled_p='t'
    }]

    # Scan the package directory for files to source.    
    set files [list]    
    foreach package $packages {

        set files [ad_find_all_files [acs_root_dir]/packages/$package]
        if { [llength $files] == 0 } {
    	    error "Unable to locate [acs_root_dir]/packages/$package/*."
        }

        foreach file [lsort $files] {

            set file_db_type [apm_guess_db_type $package $file]
            set file_type [apm_guess_file_type $package $file]

            if {[string equal $file_type query_file] &&
                ([empty_string_p $file_db_type] || [string equal $file_db_type [db_type]])} {
	        db_qd_load_query_file $file
            } 
        }
    }
    ns_log Notice "APM/QD = DONE looping through files to load queries from"
}
proc db_fullquery_get_load_location fullquery {
    return [lindex $fullquery 5]
}
proc set_the_usual_form_variables {{error_if_not_found_p 1}} {
    if { [ns_getform] == "" } {
	if $error_if_not_found_p {
	    uplevel { 
		ns_returnerror 500 "Missing form data"
		return
	    }
	} else {
	    return
	}
    }

    # The variable names are prefixed with a V to avoid confusion with the form variables while checking for naughtiness.
    uplevel {
	set Vform [ns_getform] 
	set Vform_size [ns_set size $Vform]
	set Vform_counter_i 0
	while {$Vform_counter_i<$Vform_size} {
	    set Vname [ns_set key $Vform $Vform_counter_i]
	    set Vvalue [ns_set value $Vform $Vform_counter_i]
	    check_for_form_variable_naughtiness $Vname $Vvalue
	    set QQ$Vname [DoubleApos [string trim $Vvalue]]
	    set $Vname $Vvalue
	    incr Vform_counter_i
	}
    }
}
proc apm_package_id_from_key package_key {
    return [util_memoize "apm_package_id_from_key_mem $package_key"]
}
proc ad_complaints_with_key {errorkey code} {
    global ad_page_contract_errorkeys
    set ad_page_contract_errorkeys [concat $errorkey $ad_page_contract_errorkeys]
    uplevel 1 $code
    set ad_page_contract_errorkeys [lrange $ad_page_contract_errorkeys 1 end]
}
proc ad_page_contract_filter_rule_proc_tmpfile {name filters_varname} {upvar $filters_varname filters

    if { [string match "*tmpfile" $name] && [lsearch -exact $filters tmpfile] == -1 } {
	lappend filters tmpfile
    }
}
proc util_quotehtml arg {
    return [ad_quotehtml $arg]
}
proc site_map_unmount_application__arg_parser {} {    upvar args args
    upvar sync_p val ; set val t
    upvar delete_p val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sync_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -sync_p"
                }
                upvar sync_p val ; set val [lindex $args [incr i]]
            }
            -delete_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -delete_p"
                }
                upvar delete_p val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { node_id } $n_args_remaining]"
    }
    upvar node_id val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_columns table_name {
    set columns [list]
    db_foreach table_column_names {
	select lower(column_name) as column_name
	from user_tab_columns
	where table_name = upper(:table_name)
    } {
	lappend columns $column_name
    }
    return $columns
}
proc rp_register_extension_handler {extension args} {
    if { [llength $args] == 0 } {
	error "Must specify a procedure name"
    }
    ns_log "Notice" "Registering [join $args " "] to handle files with extension $extension"
    nsv_set rp_extension_handlers ".$extension" $args
}
proc ad_page_contract_filter_proc_string_length_range {name value_varname range} {upvar $value_varname value

    if { [string length $value] < [lindex $range 0] } {
	ad_complain "$name is too short.  Please enter a value of at least [lindex $range 0] characters long. The value you entered was [string length $value] characters long."
	return 0
    } elseif { [string length $value] > [lindex $range 1] } {
	ad_complain "$name is too long.  Please enter a value of at most [lindex $range 1] characters long. The value you entered was [string length $value] characters long."
	return 0
    }
    return 1
}
proc util_quote_double_quotes arg {
    return [ad_quotehtml $arg]
}
proc ad_parameter args {    ad_parameter__arg_parser

    if {[empty_string_p $package_id]} {
	set package_id [ad_requested_object_id] } {
    }

    if { [info exists set] } {
	if { ![empty_string_p $package_id] } {
	    # Write to the database.
	    db_exec_plsql ad_parameter_set {
		begin
		  apm.set_value(
			  package_id => :package_id,
			  parameter_name => :name,
			  attr_value => :set
			);
		end;
	    }
	    ad_parameter_cache -set $set $package_id $name
	}
	return $set
    } else {
	if { [empty_string_p $package_key] } {
	    set ns_param [ns_config "ns/server/[ns_info server]/acs" $name]
	} else {
	    set ns_param [ns_config "ns/server/[ns_info server]/acs/$package_key" $name]
	}
	if { [empty_string_p $ns_param] } {
            # Just retrieve the parameter from the cache if it exists.
            set ns_param [ad_parameter_cache $package_id $name]
	}
    }
    if { ![empty_string_p $ns_param] } {
	return $ns_param
    } else {
	return $default
    }
}
proc ns_register_proc {type path procname} {
    ns_log Debug "ns_register_proc called with TYPE: $type PATH: $path PROCNAM: $procname "
}
proc ad_registration_finite_state_machine_admin_links {member_state email_verified_p user_id {return_url {}}} {
    set user_finite_state_links [list]
    switch $member_state {
	"approved" {
	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=banned>ban</a>"
 	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=deleted>delete</a>"
	}
	"deleted" {
	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=approved>undelete</a>"
 	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=banned>ban</a>"
	}
	"needs approval" {
	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=approved>approve</a>"
 	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=rejected>reject</a>"
	}
	"rejected" {
 	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=approved>approve</a>"
	}
	"banned" {
 	    lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&member_state=approved>approve</a>"
	}
    }
    if { $email_verified_p == "t" } {
 	lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&email_verified_p=f>require email verification</a>"	
    } else {
 	lappend user_finite_state_links "<a href=member-state-change?[export_url_vars user_id return_url]&email_verified_p=t>approve email</a>"
    }
    return $user_finite_state_links
}
proc ReturnHeadersWithCookieNoCache {cookie_content {content_type text/html}} {

    ns_write "HTTP/1.0 200 OK
MIME-Version: 1.0
Content-Type: $content_type
Set-Cookie:  $cookie_content
pragma: no-cache\r\n"

     ns_startcontent -type $content_type
}
proc ad_parameter_cache__arg_parser {} {    upvar args args
    upvar delete_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set"
                }
                upvar set val ; set val [lindex $args [incr i]]
            }
            -delete - -delete=1 {
                uplevel set delete_p 1
            }
            -delete=0 {
                uplevel set delete_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { package_id parameter_name } $n_args_remaining]"
    }
    upvar package_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar parameter_name val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_package_parameters package_key {
    return [db_list get_names {
	select parameter_name from apm_parameters
	where package_key = :package_key
    }]
}
proc sec_generate_secure_token_cookie {} {
    ad_set_signed_cookie -secure t "ad_secure_token" "[ad_conn session_id],[ad_conn user_id],[ns_time]"
}
proc populate_secret_tokens_cache {} {}
proc ad_return_exception_page {status title explanation} {
    ns_return $status text/html "[ad_header_with_extra_stuff $title "" ""]
<h2>$title</h2>
<hr>
$explanation
[ad_footer]";				#"emacs
    # raise abortion flag, e.g., for templating
    global request_aborted
    set request_aborted [list $status $title]
}
proc sec_session_renew {} {
    return "0"
}
proc rp_serve_concrete_file file {
  set extension [file extension $file]
  set startclicks [clock clicks]

  if { [nsv_exists rp_extension_handlers $extension] } {
    set handler [nsv_get rp_extension_handlers $extension]

    if { [set errno [catch {
      ad_try {
	$handler
      } ad_script_abort val {
	# do nothing
      }
      rp_finish_serving_page
      ad_call_proc_if_exists ds_add rp [list serve_file [list $file $handler] $startclicks [clock clicks]]
    } error]] } {
      global errorCode errorInfo
      ad_call_proc_if_exists ds_add rp [list serve_file [list $file $handler] $startclicks [clock clicks] error "$errorCode: $errorInfo"]
      return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
    }
  } else {
    # Some other random kind of file - guess the type and return it.
    set type [ns_guesstype $file]
    ad_call_proc_if_exists ds_add rp [list serve_file [list $file $type] $startclicks [clock clicks]]
    ns_returnfile 200 $type $file
  }
}
proc ad_parse_html_attributes args {    ad_parse_html_attributes__arg_parser

    if { [info exists attribute_array] } {
	upvar $attribute_array attribute_array_var
	return [ad_parse_html_attributes_upvar -attribute_array attribute_array_var html pos]
    } else {
	return [ad_parse_html_attributes_upvar html pos]
    }
}
proc ad_present_user {user_id name} {
    return [acs_community_member_link -user_id $user_id -label $name]
}
proc db_exec_plpgsql {db statement_name pre_sql fname} {
    set start_time [clock clicks]

    db_qd_log Debug "PRE-QD: the SQL is $pre_sql"

    # Query Dispatcher (OpenACS - ben)
    set sql [db_qd_replace_sql $statement_name $pre_sql]

    db_qd_log Debug "POST-QD: the SQL is $sql"

    set unique_id [db_nextval "anon_func_seq"]

    set function_name "__exec_${unique_id}_${fname}"

    # insert tcl variable values (Openacs - Dan)
    if {![string equal $sql $pre_sql]} {
        set sql [uplevel 2 [list subst -nobackslashes $sql]]
    }
    db_qd_log Debug "PLPGSQL: converted: $sql to: select $function_name ()"

    # create a function definition statement for the inline code 
    # binding is emulated in tcl. (OpenACS - Dan)
    set db ""
#    set errno [catch {
#    } error]
	upvar bind bind
	if { [info exists bind] && [llength $bind] != 0 } {
	    if { [llength $bind] == 1 } {
                set bind_vars [list]
                set len [ns_set size $bind]
                for {set i 0} {$i < $len} {incr i} {
                    lappend bind_vars [ns_set key $bind $i]  [ns_set value $bind $i]
                }
#JS: defer to nstcl                
		set proc_sql [db_bind_var_substitution $sql $bind_vars]
	    } else {
#JS:defer to nstcl                
		set proc_sql [db_bind_var_substitution $sql $bind]
	    }
	} else {
#JS:defer to nstcl?
            set proc_sql [uplevel 2 [list db_bind_var_substitution $sql]]
	    
	}

#	set proc_sql $sql
#	ns_log Debug "db_exec_plpgsql reports $function_name ..\n $proc_sql"

        db_dml "anonymousfunction" "create function $function_name () returns varchar as '
                      [DoubleApos $proc_sql]
	    ' language 'plpgsql'"

        db_0or1row $statement_name "select $function_name ()" -column_set ret_val
        # drop the anonymous function (OpenACS - Dan)
#JS: restore this
        db_dml "anonymousfunction_drop" "drop function $function_name ()"
	
        return $ret_val

    set error ""

    global errorInfo errorCode
    set errinfo $errorInfo
    set errcode $errorCode
    
    ad_call_proc_if_exists ds_collect_db_call $db 0or1row $statement_name $sql $start_time $errno $error

    if { $errno == 2 } {
	
	return $error
    } else {
        catch {db_dml "myquery" "drop function $function_name ()"}
    }

    return -code $errno -errorinfo $errinfo -errorcode $errcode $error
}
proc exists_and_not_null varname {
    upvar 1 $varname var 
    return [expr { [info exists var] && ![empty_string_p $var] }] 
}
proc site_node_closest_ancestor_package_url__arg_parser {} {    upvar args args
    upvar package_key val ; set val acs-subsite
    upvar default val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -default {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -default"
                }
                upvar default val ; set val [lindex $args [incr i]]
            }
            -package_key {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -package_key"
                }
                upvar package_key val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_convert_to_text__arg_parser {} {    upvar args args
    upvar html_p val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -html_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -html_p"
                }
                upvar html_p val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { text } $n_args_remaining]"
    }
    upvar text val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc set_minus {set element} {
    set pos [lsearch -exact $set $element]
    if {$pos != -1} {
        set set [lreplace $set $pos $pos]
    }
    return $set
}
proc ns_sleep seconds {
    # Adapted from http://wiki.tcl.tk/933.html
    variable ns_sleep
    
    if {![string is integer -strict $seconds]} {
        return -code error "expected integer but got \"$seconds\""
    }

    if {$seconds < 0} {
        return -code error "#seconds must be >= 0"
    }

    if {$seconds == 0} then return

    set i [incr ns_sleep(counter)]
    set ns_sleep($i) 0
    after [expr {$seconds * 1000}] set ::nstcl::ns_sleep($i) 1
    vwait ::nstcl::ns_sleep($i)
    unset ::nstcl::ns_sleep($i)
}
proc site_nodes_sync args {

    if { [util_memoize_cached_p {site_nodes_sync_helper}] } {
	util_memoize_flush {site_nodes_sync_helper}
    }
    nsv_array reset site_nodes [util_memoize {site_nodes_sync_helper}]
    ns_eval {
	global tcl_site_nodes
	if {[info exists tcl_site_nodes]} {
	    unset tcl_site_nodes
	}
    }

}
proc db_legacy_package_p db_type_list {
    if { [lsearch $db_type_list "oracle-8.1.6"] != -1 } {
        return 1
    }
    return 0
}
proc set_csv_variables_after_query {} {
    uplevel {
	    set set_variables_after_query_i 0
	    set set_variables_after_query_limit [ns_set size $selection]
	    while {$set_variables_after_query_i<$set_variables_after_query_limit} {
		set [ns_set key $selection $set_variables_after_query_i] [ns_set value $selection $set_variables_after_query_i]
		set EQ[ns_set key $selection $set_variables_after_query_i] [util_escape_quotes_for_csv [string trim [ns_set value $selection $set_variables_after_query_i]]]
		set QEQQ[ns_set key $selection $set_variables_after_query_i] "\"[util_escape_quotes_for_csv [string trim [ns_set value $selection $set_variables_after_query_i]]]\""
		incr set_variables_after_query_i
	    }
    }
}
proc nsv_get {id key} { 
    global $id
#    puts "getting $id ... $key "
#    return     
#    if info exists $id

    if { [info exists ${id}($key)] } {

	return [eval [format {set %s(%s)} $id $key ]]
    } 
       
    ns_log error "nsv_get failed for $id ( $key ) "
    
}
proc site_node_id url {
  array set node [site_node $url]
  return $node(node_id)
}
proc validate_ad_dateentrywidget {field_name column form {allow_null 0}} {
    set col $column
    set day [ns_set get $form "$col.day"]
    ns_set update $form "$col.day" [string trimleft $day "0"]
    set month [ns_set get $form "$col.month"]
    set year [ns_set get $form "$col.year"]

    # check that either all elements are blank
    # date value is formated correctly for ns_dbformvalue
    if { [empty_string_p "$day$month$year"] } {
	if { $allow_null == 0 } {
	    error "$field_name must be supplied"
	} else {
	    return ""
	}
    } elseif { ![empty_string_p $year] && [string length $year] != 4 } {
	error "The year must contain 4 digits."
    } elseif { [catch  { ns_dbformvalue $form $column date date } errmsg ] } {
	error "The entry for $field_name had a problem:  $errmsg."
    }

    return $date
}
proc db_abort_transaction {} {
    if {$::nstcl::database::transaction(depth) == 0} {
        error "Can't abort transaction: not currently inside a transaction"
    } else {
        return [set ::nstcl::database::transaction(abort_p) 1]
    }
}
proc ad_get_client_property__arg_parser {} {    upvar args args
    upvar session_id val ; set val {}
    upvar cache val ; set val t
    upvar default val ; set val {}
    upvar cache_only val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -cache {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -cache"
                }
                upvar cache val ; set val [lindex $args [incr i]]
            }
            -cache_only {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -cache_only"
                }
                upvar cache_only val ; set val [lindex $args [incr i]]
            }
            -default {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -default"
                }
                upvar default val ; set val [lindex $args [incr i]]
            }
            -session_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -session_id"
                }
                upvar session_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { module name } $n_args_remaining]"
    }
    upvar module val ; set val [lindex $args [expr { $i + 0 }]]
    upvar name val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_user_class_description set_id {
    set clauses [list]
    set pretty_description ""

    # turn all the parameters in the ns_set into tcl vars
    ad_ns_set_to_tcl_vars -duplicates fail $set_id 
    
    # All the SQL statements are named after the criteria name (e.g. category_id)

    foreach criteria [ad_user_class_parameters] {
	if { [info exists $criteria] && ![empty_string_p [set $criteria]] } {

	    switch $criteria {
		"category_id" {
		    set pretty_category [db_string $criteria {
			select category from categories where category_id = :category_id
		    } ]
		    lappend clauses "said they were interested in $pretty_category"
		}
		"country_code" {
		    set pretty_country [db_string $criteria {
			select country_name from country_codes where iso = :country_code
		    } ]
		    lappend clauses "told us that they live in $pretty_country"
		}
		"usps_abbrev" {
		    set pretty_state [db_string $criteria {
			select state_name from states where usps_abbrev = :usps_abbrev
		    } ]
		    lappend clauses "told us that they live in $pretty_state"
		}
		"intranet_user_p" {
		    lappend clauses "are an employee"
		}
		"group_id" {
		    set group_name [db_string $criteria {
			select group_name from groups where group_id = :group_id
		    } ]
		    lappend clauses "are a member of $group_name"
		}
		"last_name_starts_with" {
		    lappend clauses "have a last name starting with $last_name_starts_with"
		}
		"email_starts_with" {
		    lappend clauses "have an email address starting with $email_starts_with"
		}	
		"expensive" {
		    lappend clauses "have accumulated unpaid charges of more than [ad_parameter ExpensiveThreshold "member-value"]"
		}
		"user_state" {
		    lappend clauses "have user state of $user_state"
		}
		"sex" {
		    lappend clauses "are $sex."
		}
		"age_above_years" {
		    lappend clauses "is older than $age_above_years years"
		}
		"age_below_years" {
		    lappend clauses "is younger than $age_below_years years"
		}
		"registration_during_month" {
		    set pretty_during_month [db_string $criteria {
			select to_char(to_date(:registration_during_month,'YYYYMM'),'fmMonth YYYY') from dual
		    } ]
		    lappend clauses "registered during $pretty_during_month"
		}
		"registration_before_days" {
		    lappend clauses "registered over $registration_before_days days ago"
		}
		"registration_after_days" {
		    lappend clauses "registered in the last $registration_after_days days"
		}
		"registration_after_date" {
		    lappend clauses "registered on or after $registration_after_date"
		}
		"last_login_before_days" {
		    lappend clauses "have not visited the site in $last_login_before_days days"
		}
		"last_login_after_days" {
		    lappend clauses "have not visited the site in $last_login_after_days days"
		}
		"last_login_equals_days" {
		    if { $last_login_equals_days == 1 } {
			lappend clauses "visited the site exactly 1 day ago"
		    } else {
			lappend clauses "visited the site exactly $last_login_equals_days days ago"
		    }
		}
		"number_of_visits_below" {
		    lappend clauses "have visited less than $number_visits_below times"
		}
		"number_of_visits_above" {
		    lappend clauses "have visited more than $number_visits_above times"
		}
		"user_class_id" {
		    set pretty_class_name [db_string $criteria {
			select name from user_classes where user_class_id = :user_class_id
		    } ]
		    lappend clauses "are in the user class $pretty_class_name"
		}
		"sql_post_select" {
		    lappend clauses "are returned by \"<i>select users(*) from $sql_post_select</i>"
		}
		"crm_state" {
		    lappend clauses "are in the customer state \"$crm_state\""
		}
		"curriculum_elements_completed" {
		    if { $curriculum_elements_completed == 1 } {
			lappend clauses "who have completed exactly $curriculum_elements_completed curriculum element"
		    } else {
			lappend clauses "who have completed exactly $curriculum_elements_completed curriculum elements"
		    }
		}
	    }
	}
    }

    if { [info exists combine_method] && $combine_method == "or" } {
	set pretty_description [join $clauses " or "]
    } else {
	set pretty_description [join $clauses " and "]
    }

    return $pretty_description
}
proc doc_exists_p {} {
    global doc_properties
    if { [array size doc_properties] > 0 } {
	return 1
    }
    return 0
}
proc database_to_tcl_list {dbhandle SQL {values {}}} {
    set selection [ns_db select $dbhandle $SQL]
    while {[::nstcl::ns_db getrow $dbhandle $selection]} {
        lappend values [::nstcl::ns_set value $selection 0]
    }
    ::nstcl::ns_set free $selection
    return $values
}
proc rp_debug__arg_parser {} {    upvar args args
    upvar ns_log_level val ; set val notice
    upvar debug val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -debug {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -debug"
                }
                upvar debug val ; set val [lindex $args [incr i]]
            }
            -ns_log_level {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -ns_log_level"
                }
                upvar ns_log_level val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { string } $n_args_remaining]"
    }
    upvar string val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc persistentArray {arrName {filename {}}} {
    upvar 1 $arrName arr
    array set arr {} ;# to make sure it exists, and is an array
    if {$filename==""} {set filename $arrName.txt}
    set filename [file join [pwd] $filename]
    if [file exists $filename] {
        set fp [open $filename]
        array set arr [read $fp]
        close $fp
    }
    uplevel 1 [list trace var $arrName wu [list persist'save $filename]]
}
proc db_list {statement_name pre_sql args} {
#        ns_log info "db_list called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_list $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_list $statement_name $sql] $args]
    }
proc ns_startcontent {} { 
    ns_log warning "ns_startcontent (disabled) called with $args"
    
}
proc ad_host {} {
    set host_and_port [ns_set iget [ns_conn headers] Host]
    if { [regexp {^([^:]+)} $host_and_port match host] } {
	return $host
    } else {
	return "unknown host"
    }
}
proc rp_concrete_file args {    rp_concrete_file__arg_parser

  # Sub out funky characters in the pathname, so the user can't request
  # http://www.arsdigita.com/*/index (causing a potentially expensive glob
  # and bypassing registered procedures)!
  regsub -all {[^0-9a-zA-Z_/:.]} $path {\\&} path_glob

  # Grab a list of all available files with extensions.
  set files [glob -nocomplain "$path_glob$extension_pattern"]

  # Search for files in the order specified in ExtensionPrecedence.
  set precedence [ad_parameter -package_id [ad_acs_kernel_id] "ExtensionPrecedence" "request-processor" "tcl"]
  foreach extension [split [string trim $precedence] ","] {
    if { [lsearch -glob $files "*.$extension"] != -1 } {
      return "$path.$extension"
    }
  }

  # None of the extensions from ExtensionPrecedence were found - just pick
  # the first in alphabetical order.
  if { [llength $files] > 0 } {
    set files [lsort $files]
    return [lindex $files 0]
  }

  # Nada!
  return ""
}
proc ns_set args {
    upvar 0 ::nstcl::nssets::commands commands
    set argc [llength $args]

    if {$argc == 0} {
        return -code error "wrong # of args: should be \"ns_set command ?args?\""
    } elseif {$argc == 1 && ($args != "new" && $args != "create")} {
        return -code error "wrong # of args: should be \"ns_set $args setId ?args?\""
    } else {
        set cmd  [lindex $args 0]
        set args [lrange $args 1 end]
        incr argc -1
    }


    if {![info exists commands($cmd)]} {
        return -code error "unknown command \"$cmd\": should be one of [join [lrange [lsort [array names commands]] 0 end-1] ", "] or [lindex [lsort [array names commands]] end]"
    }


    array set command $commands($cmd)
    if {$command(persist)} {
        # AOLserver would have a notion of connection vs persistent
        # but we don't, so we'll ignore the flag (for compatability)
        if {[string equal [lindex $args 0] "-persist"]} {
            # ignore the -persist flag
            set args [lrange $args 1 end]
            incr argc -1

            # if -persist was the only argument we decrement once more
            # so that we'll throw a wrong # args error instead of 
            # invalid set ""
            if {$argc == 0 && $cmd != "new" && $cmd != "create"} {
                incr argc -1
            }
        }
    }


    if {$command(setid) && $argc >= 0} {
        set id [lindex $args 0]
        if {(![regexp {^t[1-9][0-9]*$} $id] && ![string equal $id "t0"]) ||
            ![info exists ::nstcl::nssets::$id]} {
            return -code error "invalid set id: \"$id\""
        }
    }

    if {$command(setid) == 2 && $argc >= 2} {
        set id [lindex $args 1]
        if {(![regexp {^t[1-9][0-9]*$} $id] && ![string equal $id "t0"]) ||
            ![info exists ::nstcl::nssets::$id]} {
            return -code error "invalid set id: \"$id\""
        }
    }


    if {$argc < $command(min) || $argc > $command(max)} {
        return -code error "wrong # of args: should be \"ns_set $cmd $command(syntax)\""
    }


    switch -- $argc {
        0 { set error_p [catch { ::nstcl::nssets::$cmd } result] }

        1 { set error_p [catch { ::nstcl::nssets::$cmd  [lindex $args 0] } result]
          }

        2 { set error_p [catch { ::nstcl::nssets::$cmd  [lindex $args 0]  [lindex $args 1] } result] 
          }

        3 { set error_p [catch {::nstcl::nssets::$cmd  [lindex $args 0]  [lindex $args 1]  [lindex $args 2] } result]
        }
    }

    if {$error_p} {
        return -code error $result
    } else {
        return $result
    }
}
proc ad_get_user_info {} {
    uplevel {
	set user_id [ad_conn user_id]
	if [catch {
	    db_1row user_name_select {
		select first_names, last_name, email
		from persons, parties
		where person_id = :user_id
		and person_id = party_id
	    }
	} errmsg] {
	    ad_return_error "Couldn't find user info" "Couldn't find user info."
	    return
	}
    }
}
proc value_if_exists var_name {
    upvar $var_name $var_name
    if [info exists $var_name] {
        return [set $var_name]
    }
}
proc pkg_compareExtension {fileName {ext {}}} {
    global tcl_platform
    if {![string length $ext]} {set ext [info sharedlibextension]}
    if {[string equal $tcl_platform(platform) "windows"]} {
        return [string equal -nocase [file extension $fileName] $ext]
    } else {
        # Some unices add trailing numbers after the .so, so
        # we could have something like '.so.1.2'.
        set root $fileName
        while {1} {
            set currExt [file extension $root]
            if {[string equal $currExt $ext]} {
                return 1
            } 

	    # The current extension does not match; if it is not a numeric
	    # value, quit, as we are only looking to ignore version number
	    # extensions.  Otherwise we might return 1 in this case:
	    #		pkg_compareExtension foo.so.bar .so
	    # which should not match.

	    if { ![string is integer -strict [string range $currExt 1 end]] } {
		return 0
	    }
            set root [file rootname $root]
	}
    }
}
proc ad_set_signed_cookie__arg_parser {} {    upvar args args
    upvar domain val ; set val {}
    upvar replace val ; set val f
    upvar token_id val ; set val {}
    upvar path val ; set val /
    upvar secret val ; set val {}
    upvar max_age val ; set val {}
    upvar secure val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -replace {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -replace"
                }
                upvar replace val ; set val [lindex $args [incr i]]
            }
            -secure {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secure"
                }
                upvar secure val ; set val [lindex $args [incr i]]
            }
            -max_age {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -max_age"
                }
                upvar max_age val ; set val [lindex $args [incr i]]
            }
            -domain {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -domain"
                }
                upvar domain val ; set val [lindex $args [incr i]]
            }
            -path {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -path"
                }
                upvar path val ; set val [lindex $args [incr i]]
            }
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }
            -token_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -token_id"
                }
                upvar token_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { name value } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar value val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc rp_handler {} {
  ad_call_proc_if_exists ds_collect_connection_info

  set startclicks [clock clicks]
  rp_debug "rp_handler: handling request: [ns_conn method] [ns_conn url]?[ns_conn query]"
  if [set code [catch {
    if [rp_performance_mode] {
      global tcl_url2file tcl_url2path_info
      if ![catch {
	set file $tcl_url2file([ad_conn url])
	set path_info $tcl_url2path_info([ad_conn url])
      } errmsg] {
	ad_conn -set file $file
	ad_conn -set path_info $path_info
	rp_serve_concrete_file $file
	return
      }
      rp_debug -debug t "error in rp_handler: $errmsg"
    }

    set paths [list]

    lappend paths "[ns_info pageroot]"
    lappend paths [string trimleft [ad_conn url] /]

    if {![empty_string_p [ad_conn package_key]]} {
      lappend paths "[acs_root_dir]/packages/[ad_conn package_key]/www"
      lappend paths [ad_conn extra_url]
    }

    foreach {root path} $paths {
      ad_call_proc_if_exists ds_comment "rp_handler: trying rp_serve_abstract_file $root / $path "
      ad_try {
	rp_serve_abstract_file "$root/$path"
	set tcl_url2file([ad_conn url]) [ad_conn file]
	set tcl_url2path_info([ad_conn url]) [ad_conn path_info]
      } notfound val {
          ad_call_proc_if_exists ds_comment "rp_handler: not found"
          ad_call_proc_if_exists ds_add rp [list transformation [list notfound "$root / $path" $val] $startclicks [clock clicks]]
	continue
      } redirect url {
          ad_call_proc_if_exists ds_comment "rp_handler: redirect"
          ad_call_proc_if_exists ds_add rp [list transformation [list redirect $root/$path $url] $startclicks [clock clicks]]
	ad_returnredirect $url
      } directory dir_index {
          ad_call_proc_if_exists ds_comment "rp_handler: dir_index"
          ad_call_proc_if_exists ds_add rp [list transformation [list directory $root/$path $dir_index] $startclicks [clock clicks]]
	continue
      }

      return
    }

    if {[info exists dir_index]} {
      if { [nsv_get rp_directory_listing_p .] } {
	ns_returnnotice 200 "Directory listing of $dir_index"  [rp_html_directory_listing $dir_index]
	return
      }
    }

    # Ok, we didn't find a normal file. Let's look for a path info style
    # thingy.
    # First set up a list of candidate file paths to try
    set candidates [list]
    foreach {root path} $paths {
      set cand [list]
      foreach prefix [rp_path_prefixes $path] {
	lappend cand [list $root $path $prefix]
      }
      lappend candidates $cand
    }
    # the candidates "matrix" typically has two row of different length, like
    #	{ro00 pa00 pr00} {ro01 pa01 pr01} {ro02 pa02 pr02} {ro03 pa03 pr03}
    #	{ro10 pa10 pr10} {ro11 pa11 pr11}
    # It needs to be transposed, i.e. accessed column- instead of row-wise

    # Assume (paths and hence) candidates has two elements (rows).
    # If package_key is empty, there's only one -- fix that
    lappend candidates {}

    # Now visit the candidates columnwise: from most specific to least
    foreach cand0 [lindex $candidates 0] cand1 [lindex $candidates 1] {
      foreach candidate [list $cand0 $cand1] {
	if [empty_string_p $candidate] continue;
	set root   [lindex $candidate 0]; # fewer instructions than util_unlist
	set path   [lindex $candidate 1]
	set prefix [lindex $candidate 2]
	ad_try {
	  ad_conn -set path_info  [string range $path [expr [string length $prefix] - 1] end]
	  rp_serve_abstract_file -noredirect -nodirectory  -extension_pattern ".vuh" "$root$prefix"
	  set tcl_url2file([ad_conn url]) [ad_conn file]
	  set tcl_url2path_info([ad_conn url]) [ad_conn path_info]
	} notfound val {
          ad_call_proc_if_exists ds_add rp [list transformation [list notfound $root/$path $val] $startclicks [clock clicks]]
	  continue
	} redirect url {
          ad_call_proc_if_exists ds_add rp [list transformation [list redirect $root/$path $url] $startclicks [clock clicks]]
	  ad_returnredirect $url
	} directory dir_index {
          ad_call_proc_if_exists ds_add rp [list transformation [list directory $root/$path $dir_index] $startclicks [clock clicks]]
	  continue
	}

	return
      }
    }

    ad_call_proc_if_exists ds_add rp [list transformation [list notfound $root/$path notfound] $startclicks [clock clicks]]
    ns_returnnotfound
  } errmsg]] {
    if {$code == 1} {
        if {![string equal [ns_conn query] ""]} {
            set q ?
        } else {
            set q ""
        }
        rp_debug -debug t "error in rp_handler: serving [ns_conn method] [ns_conn url]$q[ns_conn query] \n\tad_url \"[ad_conn url]\" maps to file \"[ad_conn file]\"\nerrmsg is $errmsg"
        rp_report_error
    }
  }
}
proc ad_acs_kernel_id {} {
    return 162
}
proc ad_acs_admin_id {} {
    return 350
}
proc wrap_string {input {threshold 80}} {
    set result_rows [list]
    set start_of_line_index 0
    while 1 {
	set this_line [string range $input $start_of_line_index [expr $start_of_line_index + $threshold - 1]]
	if { $this_line == "" } {
	    return [join $result_rows "\n"]
	}
	set first_new_line_pos [string first "\n" $this_line]
	if { $first_new_line_pos != -1 } {
	    # there is a newline
	    lappend result_rows [string range $input $start_of_line_index [expr $start_of_line_index + $first_new_line_pos - 1]]
	    set start_of_line_index [expr $start_of_line_index + $first_new_line_pos + 1]
	    continue
	}
	if { [expr $start_of_line_index + $threshold + 1] >= [string length $input] } {
	    # we're on the last line and it is < threshold so just return it
		lappend result_rows $this_line
		return [join $result_rows "\n"]
	}
	set last_space_pos [string last " " $this_line]
	if { $last_space_pos == -1 } {
	    # no space found!  Try the first space in the whole rest of the string
	    set next_space_pos [string first " " [string range $input $start_of_line_index end]]
	    set next_newline_pos [string first "\n" [string range $input $start_of_line_index end]]
	    if {$next_space_pos == -1} {
		set last_space_pos $next_newline_pos
	    } elseif {$next_space_pos < $next_newline_pos} {
		set last_space_pos $next_space_pos
	    } else {
		set last_space_pos $next_newline_pos
	    }
	    if { $last_space_pos == -1 } {
		# didn't find any more whitespace, append the whole thing as a line
		lappend result_rows [string range $input $start_of_line_index end]
		return [join $result_rows "\n"]
	    } 
	}
	# OK, we have a last space pos of some sort
	set real_index_of_space [expr $start_of_line_index + $last_space_pos]
	lappend result_rows [string range $input $start_of_line_index [expr $real_index_of_space - 1]]
	set start_of_line_index [expr $start_of_line_index + $last_space_pos + 1]
    }
}
proc ad_page_contract_filter_proc_allhtml {name value_varname} {upvar $value_varname value

    return 1
}
proc ad_after_server_initialization {name args} {
#    puts "ad_after_server_init [info level 0]"
    nsv_lappend ad_after_server_initialization . [list name $name script [info script] args $args]
}
proc site_node_mount_application__arg_parser {} {    upvar args args
    upvar return val ; set val package_id
    upvar sync_p val ; set val t

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sync_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -sync_p"
                }
                upvar sync_p val ; set val [lindex $args [incr i]]
            }
            -return {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -return"
                }
                upvar return val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 4 } {
        return -code error "No value specified for argument [lindex { parent_node_id instance_name package_key package_name } $n_args_remaining]"
    }
    upvar parent_node_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar instance_name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 2 }]]
    upvar package_name val ; set val [lindex $args [expr { $i + 3 }]]
    if { $n_args_remaining > 4 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_reload_level_in_this_interpreter {} { return 0 }
proc saveprocs {fileName args} {
    set fp [open $fileName w]
    try_eval {
        puts $fp "# tcl procs saved on [fmtclock [getclock]]\n"
        puts $fp [eval "showproc $args"]
    } {} {
        close $fp
    }
}
proc db_write_blob {statement_name sql args} {
    ad_arg_parser { bind } $args

    set full_statement_name [db_qd_get_fullname $statement_name]

    db_with_handle db { 
	db_exec_lob write_blob $db $full_statement_name $sql
    }
}
proc db_qd_root_path {} {
    return "dbqd."
}
proc doc_set_property {name value} {
    global doc_properties
    set doc_properties($name) $value
}
proc ad_page_contract_get_variables {} {
    global ad_page_contract_variables
    if { [exists_and_not_null ad_page_contract_variables] } {
	return $ad_page_contract_variables
    } 
    return [list]
}
proc ad_verify_and_get_session_id__arg_parser {} {    upvar args args
    upvar secure val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -secure {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secure"
                }
                upvar secure val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_page_contract_filter_rule_script filter {
    return [lindex [nsv_get ad_page_contract_filter_rules $filter] 2]
}
proc ad_admin_owner {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  AdminOwner]
}
proc ad_sign__arg_parser {} {    upvar args args
    upvar token_id val ; set val {}
    upvar max_age val ; set val {}
    upvar secret val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -secret {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secret"
                }
                upvar secret val ; set val [lindex $args [incr i]]
            }
            -token_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -token_id"
                }
                upvar token_id val ; set val [lindex $args [incr i]]
            }
            -max_age {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -max_age"
                }
                upvar max_age val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { value } $n_args_remaining]"
    }
    upvar value val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_complaints_init {} {
    global ad_page_contract_complaints ad_page_contract_errorkeys
    set ad_page_contract_complaints [list]
    set ad_page_contract_errorkeys [list]
}
proc ad_admin_filter {} {
  ad_require_permission [ad_conn object_id] admin
  return filter_ok
}
proc db_fullquery_get_bind_vars fullquery {
    return [lindex $fullquery 2]
}
proc db_fullquery_compatible_p {fullquery {rdbms {}}} {
    set query_rdbms [db_fullquery_get_rdbms $fullquery]

    # NOTE: not complete
    # return something depending on compatibility of RDBMSs
}
proc get_referrer {} {
    return [ns_set get [ad_conn headers] Referer]
}
proc ad_return_if_another_copy_is_running {{max_simultaneous_copies 1} {call_adp_break_p 0}} {
    # first let's figure out how many are running and queued
    set this_connection_url [ad_conn url]
    set n_matches 0
    foreach connection [ns_server active] {
	set query_connection_url [lindex $connection 4]
	if { $query_connection_url == $this_connection_url } {
	    # we got a match (we'll always get at least one
	    # since we should match ourselves)
	    incr n_matches
	}
    }
    if { $n_matches > $max_simultaneous_copies } {
	ad_return_warning "Too many copies" "This is an expensive page for our server, which is already running the same program on behalf of some other users.  Please try again at a less busy hour."
	# blow out of the caller as well
	if $call_adp_break_p {
	    # we were called from an ADP page; we have to abort processing
	    ns_adp_break
	}
	return -code return
    }
    # we're okay
    return 1
}
proc ns_eval args {
    ns_log notice "ns_eval (disabled) called with $args"
#    uplevel #0 $args 
}
proc util_ns_set_to_list__arg_parser {} {    upvar args args

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -set {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -set"
                }
                upvar set val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
    if { ![uplevel info exists set] } {
        return -code error "Required switch -set not provided"
    }
}
proc ad_page_contract_filter_rule_proc filter {
    return [lindex [nsv_get ad_page_contract_filter_rules $filter] 0]
}
proc util_PrettyBoolean {t_or_f {default default}} {
    if { $t_or_f == "t" || $t_or_f == "T" } {
	return "Yes"
    } elseif { $t_or_f == "f" || $t_or_f == "F" } {
	return "No"
    } else {
	# Note that we can't compare default to the empty string as in 
	# many cases, we are going want the default to be the empty
	# string
	if { [string compare $default "default"] == 0 } {
	    return "Unknown (\"$t_or_f\")"
	} else {
	    return $default
	}
    }
}
proc NsSettoTclString set_id {
    set result ""
    for {set i 0} {$i<[ns_set size $set_id]} {incr i} {
	append result "[ns_set key $set_id $i] : [ns_set value $set_id $i]\n"
    }
    return $result
}
proc ns_gmtime {} {
    set pieces {minute hours dayofmonth month year dayofweek dayofyear tz}
    set format "%M     %H    %d         %m    %Y   %w        %j        %Z"

    set time [expr {[clock seconds] - [clock scan "1970-01-01 00:00:00"]}]

    foreach $pieces [clock format $time -format $format] break
    foreach piece $pieces {
        set $piece [string trimleft [set $piece] 0]
        if {[set $piece] == ""} {
            set $piece 0
        }
    }

    incr dayofyear -1
    incr month     -1
    incr year      -1900
    set dst [string match *DT $tz]

    return [list  $minute $hours $dayofmonth $month $year $dayofweek $dayofyear $dst]
}
proc ad_user_class_query set_id {
    # we might need this 
    set where_clauses [list]
    set join_clauses [list]
    set group_clauses [list]
    set having_clauses [list]
    set tables [list users]

    # turn all the parameters in the ns_set into tcl vars
    ad_ns_set_to_tcl_vars -duplicates fail $set_id 

    # if we are using a user_class, just get the info

    # Get all the non-LOB columns.
    set user_columns [list]
    foreach column [db_columns users] {
	if { $column != "portrait" && $column != "portrait_thumbnail" } {
	    lappend user_columns "users.$column"
	}
    }

    if { [info exists count_only_p] && $count_only_p } {
	set select_list "count(users.user_id)"
    } else {
	set select_list $user_columns
    }

    if { [info exists include_contact_p] && $include_contact_p} {
	lappend select_list "user_contact_summary(users.user_id) as contact_summary"
    }
    if { [info exists include_demographics_p] && $include_demographics_p} {
	lappend select_list "user_demographics_summary(users.user_id) as demographics_summary"
    }
    
    if { [info exists user_class_id] && ![empty_string_p $user_class_id] } {
	set sql_post_select [db_string sql_post_select_for_user_class "
	    select sql_post_select
	    from user_classes where user_class_id = [ns_dbquotevalue $user_class_id]
	"]

	return "select [join $select_list ",\n    "]\n$sql_post_select"
    }
    
    if { [info exists sql_post_select] && ![empty_string_p $sql_post_select] } {
	return "select [join $select_list ",\n    "]\n$sql_post_select"
    }

    foreach criteria [ad_user_class_parameters] {
	if { [info exists $criteria] && ![empty_string_p [set $criteria]] } {
	    switch $criteria {
		"category_id" {
		    if {[lsearch $tables "users_interests"] == -1 } {
		    lappend tables "users_interests"
			lappend join_clauses "users.user_id = users_interests.user_id"
		    }
		    lappend where_clauses "users_interests.category_id = [ns_dbquotevalue $category_id]"
		}
		"Country_code" {
		    if {[lsearch $tables "users_contact"] == -1 } {
			lappend tables "users_contact"
			lappend join_clauses "users.user_id = users_contact.user_id"
		    }
		    lappend where_clauses "users_contact.ha_country_code = [ns_dbquotevalue $country_code]"
		    
		}
		"usps_abbrev" {
		    if {[lsearch $tables "users_contact"] == -1 } {
			lappend tables "users_contact"
			lappend join_clauses "users.user_id = users_contact.user_id"
		    }
		    lappend where_clauses "(users_contact.ha_state = [ns_dbquotevalue $usps_abbrev] and (users_contact.ha_country_code is null or users_contact.ha_country_code = 'us'))"
		    
		}
		"intranet_user_p" {
		    if {$intranet_user_p == "t" && [lsearch $tables "intranet_users"] == -1 } {
			lappend tables "intranet_users"
			lappend join_clauses "users.user_id = intranet_users.user_id"
		    }
		}
		"group_id" {
		    lappend tables "group_member_map"
		    lappend join_clauses "users.user_id = group_member_map.member_id"
		    lappend where_clauses "group_member_map.group_id = $group_id"
		    
		}
		
		"last_name_starts_with" {
		    lappend where_clauses "upper(users.last_name) like upper([ns_dbquotevalue "${last_name_starts_with}%"])"
		    # note the added percent sign  here
		    
		}
		"email_starts_with" {
		    lappend where_clauses "upper(users.email) like upper([ns_dbquotevalue "${email_starts_with}%"])"
		    # note the added percent sign  here
		    
		}
		"expensive" {
		    if { [info exists count_only_p] && $count_only_p } {
			lappend where_clauses "[ad_parameter ExpensiveThreshold "member-value"] < (select sum(amount) from users_charges where users_charges.user_id = users.user_id)"
		    } else {
			if {[lsearch $tables "user_charges"] == -1 } {
			    lappend tables "users_charges"
			    lappend join_clauses "users.user_id = users_charges.user_id"
			}

			set group_clauses [concat $group_clauses $user_columns]

			lappend having_clauses "sum(users_charges.amount) > [ad_parameter ExpensiveThreshold "member-value"]"
			# only the ones where they haven't paid
			lappend where_clauses "users_charges.order_id is null"
		    }
		}
		"user_state" {
		    lappend where_clauses "users.user_state = [ns_dbquotevalue $user_state]"
		    
		}
		"sex" {
		    if {[lsearch $tables "users_demographics"] == -1 } {
			lappend tables "users_demographics"
			lappend join_clauses "users.user_id = users_demographics.user_id"
		    }
		    lappend where_clauses "users_demographics.sex = [ns_dbquotevalue $sex]"
		    
		    
		}
		"age_below_years" {
		    if {[lsearch $tables "users_demographics"] == -1 } {
			lappend tables "users_demographics"
			lappend join_clauses "users.user_id = users_demographics.user_id"
		    }
		    lappend where_clauses "users_demographics.birthdate > sysdate - ([ns_dbquotevalue $age_below_years] * 365.25)"
		    
		}
		"age_above_years" {
		    if {[lsearch $tables "users_demographics"] == -1 } {
			lappend tables "users_demographics"
			lappend join_clauses "users.user_id = users_demographics.user_id"
		    }
		    lappend where_clauses "users_demographics.birthdate < sysdate - ([ns_dbquotevalue $age_above_years] * 365.25)"
		    
		}
		"registration_during_month" {
		    lappend where_clauses "to_char(users.registration_date,'YYYYMM') = [ns_dbquotevalue $registration_during_month]"
		    
		}
		"registration_before_days" {
		    lappend where_clauses "users.registration_date < sysdate - [ns_dbquotevalue $registration_before_days]"
		    
		}
		"registration_after_days" {
		    lappend where_clauses "users.registration_date > sysdate - [ns_dbquotevalue $registration_after_days]"
		    
		}
		"registration_after_date" {
		    lappend where_clauses "users.registration_date > [ns_dbquotevalue $registration_after_date]"
		    
		}
		"last_login_before_days" {
		    lappend where_clauses "users.last_visit < sysdate - [ns_dbquotevalue $last_login_before_days]"
		    
		}
		"last_login_after_days" {
		    lappend where_clauses "users.last_visit > sysdate - [ns_dbquotevalue $last_login_after_days]"
		    
		}
		"last_login_equals_days" {
		    lappend where_clauses "round(sysdate-last_visit) = [ns_dbquotevalue $last_login_equals_days]"
		    
		}
		"number_visits_below" {
		    lappend where_clauses "users.n_sessions < [ns_dbquotevalue $number_visits_below]"
		    
		}
		"number_visits_above" {
		    lappend where_clauses "users.n_sessions > [ns_dbquotevalue $number_visits_above]"
		    
		}
		"crm_state" {
		    lappend where_clauses "users.crm_state = [ns_dbquotevalue $crm_state]"
		    
		}
		"curriculum_elements_completed" {
		    lappend where_clauses "[ns_dbquotevalue $curriculum_elements_completed] = (select count(*) from user_curriculum_map ucm where ucm.user_id = users.user_id and ucm.curriculum_element_id in (select curriculum_element_id from curriculum))"
		    
		}
	    }
	}
    }
    #stuff related to the query itself
    
    if { [info exists combine_method] && $combine_method == "or" } {
	set complete_where [join $where_clauses " or "]
    } else {
	set complete_where [join $where_clauses " and "]
    }
    

    if { [info exists include_accumulated_charges_p] && $include_accumulated_charges_p && (![info exists count_only_p] || !$count_only_p) } {
	# we're looking for expensive users and not just counting them
	lappend select_list "sum(users_charges.amount) as accumulated_charges"
    }
    if { [llength $join_clauses] == 0 } {
	set final_query "select [join $select_list ",\n    "]
	from [join $tables ", "]"
	if ![empty_string_p $complete_where] {
	    append final_query "\nwhere $complete_where"
	}
    } else {
	# we're joining at 
	set final_query "select [join $select_list ",\n    "]
	from [join $tables ", "]
	where [join $join_clauses "\nand "]"
	if ![empty_string_p $complete_where] {
	    append final_query "\n and ($complete_where)"
	}
    }
    if { [llength $group_clauses] > 0 } {
	append final_query "\ngroup by [join $group_clauses ", "]"
    }
    if { [llength $having_clauses] > 0 } {
	append final_query "\nhaving [join $having_clauses " and "]"
    }

    return $final_query
}
proc db_qd_internal_get_queryname_root relative_path {
    # remove the prepended "/packages/" string
    regsub {^\/?packages\/} $relative_path {} relative_path

    # remove the last chunk of the file name, since we're just looking for the root path
    # NOTE: THIS MAY NEED BETTER ABSTRACTION, since this assumes a naming scheme
    # of -rdbms.XXX (ben)
    regsub {\.xql} $relative_path {} relative_path
    regsub -- "\-[db_type]$" $relative_path {} relative_path

    # Change all . to :
    regsub -all {\.} $relative_path {:} relative_path    

    # Change all / to . (hah, no reference to News for Nerds)
    regsub -all {/} $relative_path {.} relative_path

    # We append a "." at the end, since we want easy concatenation
    return "${relative_path}."
}
proc db_multirow args {
            ::nstcl::_ad_proc_parser ::db_multirow [set args]
            
	
	set full_statement_name [db_qd_get_fullname $statement_name]
	set sql [ db_qd_replace_sql $full_statement_name $pre_sql]   
	
        set cmd [concat [list ::nstcl::db_multirow $var_name $statement_name $sql] $args]
        ns_log debug "JJS: db_multirow executing the following : $cmd "

	uplevel 1 $cmd
   
#	puts "db_multirow is done"
}
proc get_referrer_and_query_string {} {
    if {[ad_conn method]!="GET"} {
	set query_return [post_args_to_query_string]
	return "[get_referrer]?${query_return}"
    } else {
	return [get_referrer]
    }
}
proc db_blob_get_file {statement_name sql args} {
    ad_arg_parser { bind file args } $args

    set full_statement_name [db_qd_get_fullname $statement_name]

    db_with_handle db {
	db_exec_lob blob_select_file $db $full_statement_name $sql $file
    }
}
proc db_quote string {
    regsub -all -- ' $string '' string
    return $string
}
proc ns_getform {} {

    

    set form [ ns_set create cgivars ]
    foreach { pair } [ split $::pnsd::querystring & ] {
	set keyval  [ split $pair = ]
	set key [lindex $keyval 0]
	set val [ncgi::decode [lindex $keyval 1]]
	ns_set put $form $key $val
	
    }



    ns_log notice "ns_getform returning\n [ns_set print $form]"    
    return $form
    
}
proc util_remove_html_tags html {
    regsub -all {<[^>]*>} $html {} html
    return $html
}
proc apm_package_registered_p package_key {
    ### Query the database for the indicated package_key
    return [db_string apm_package_registered_p {
	select 1 from apm_package_types 
	where package_key = :package_key
    } -default 0]
}
proc db_nth_pool_name n {
    set available_pools [nsv_get db_available_pools .]
    if { $n < [llength $available_pools] } {
	set pool [lindex $available_pools $n]
    } else {
	return -code error "Ran out of database pools ($available_pools)"
    }
    return $pool
}
proc ad_user_filter {} {
  ad_require_permission [ad_conn object_id] read
  return filter_ok
}
proc ad_var_type_check_dirname_p value {

    if [regexp {[/\\]} $value] {
        return 0
    } else {
        return 1
    }
}
proc db_exec {type db statement_name pre_sql {ulevel 2}} {
    set start_time [clock clicks]

    db_qd_log Debug "PRE-QD: the SQL is $pre_sql for $statement_name"

    # Query Dispatcher (OpenACS - ben)
    set sql [db_qd_replace_sql $statement_name $pre_sql]

    # insert tcl variable values (Openacs - Dan)
    if {![string equal $sql $pre_sql]} {
        set sql [uplevel $ulevel [list subst -nobackslashes $sql]]
    }

    db_qd_log Debug "POST-QD: the SQL is $sql"

    set errno [catch {
	upvar bind bind
	if { [info exists bind] && [llength $bind] != 0 } {
	    if { [llength $bind] == 1 } {
		return [eval [list ns_pg_bind $type $db -bind $bind $sql]]
	    } else {
		set bind_vars [ns_set create]
		foreach { name value } $bind {
		    ns_set put $bind_vars $name $value
		}
		return [eval [list ns_pg_bind $type $db -bind $bind_vars $sql]]
	    }
	} else {
	    return [uplevel $ulevel [list ns_pg_bind $type $db $sql]]
	}
    } error]

    ad_call_proc_if_exists ds_collect_db_call $db $type $statement_name $sql $start_time $errno $error
    if { $errno == 2 } {
	return $error
    }

    global errorInfo errorCode
    return -code $errno -errorinfo $errorInfo -errorcode $errorCode $error
}
proc ad_page_contract_filter args {    ad_page_contract_filter__arg_parser


    if { ![string is wordchar $name] || [empty_string_p $name] } {
	return -code error "Flag name must be a valid identifier"
    }
    if { ![string equal [string tolower $name] $name] } {
	return -code error "Flag names must be all lowercase"
    }
    if { ![string match $type filter] && ![string match $type post] } {
	return -code error "Filter type must be 'filter' or 'post'"
    }

    set proc_args_len [llength $proc_args]

    if { $proc_args_len != 2 && $proc_args_len != 3 } {
	return -code error "Invalid number of arguments declared for the proc: the argument name and the value and possibly parameters"
    }

    set script [info script]
    set proc_name ad_page_contract_filter_proc_$name

    #
    # Register the filter
    #

    set mutex [nsv_get ad_page_contract_mutex filters]
    ns_mutex lock $mutex

    set prior_type [ad_page_contract_filter_type $name]

    if { [string equal $prior_type internal] } {
	ns_mutex unlock $mutex
	return -code error "The flag name \"$name\" is reserved for ad_page_contract"
    } elseif { ![empty_string_p $prior_type] } {
	set prior_script [ad_page_contract_filter_script $name]
	if { ![string equal $prior_script $script] } {
	    ns_log Warning "Multiple definitions of ad_page_contract filter \"$name\" in $script and $prior_script"
	}
    }

    nsv_set ad_page_contract_filters $name [list $type $proc_name $doc_string $script $priority]
    ns_mutex unlock $mutex

    #
    # Declare the proc
    #

    # this may look complicated, but it's really pretty simple: 
    # If you declare a filter like this: ad_page_contract_filter foo { name value } { ... }
    # it turns into this proc:
    # ad_proc ad_page_contract_filter_proc_foo { name value_varname } { upvar $value_varname value ; ... }
    # so that when the filter proc is passed the name of a variable, the body of the proc
    # will have access to that variable as if the value had been passed.

    set arg0 [lindex $proc_args 0]
    set arg1 [lindex $proc_args 1]
    if { $proc_args_len == 2 } {
	ad_proc $proc_name [list $arg0 ${arg1}_varname] $doc_string "upvar \$${arg1}_varname $arg1\n$body"
    } else {
	set arg2 [lindex $proc_args 2]
	ad_proc $proc_name [list $arg0 ${arg1}_varname $arg2] $doc_string "upvar \$${arg1}_varname $arg1\n$body"
    }
}
proc export_entire_form {} {
    set hidden ""
    set the_form [ns_getform]
    if { ![empty_string_p $the_form] } {
	for {set i 0} {$i<[ns_set size $the_form]} {incr i} {
	    set varname [ns_set key $the_form $i]
	    set varvalue [ns_set value $the_form $i]
	    append hidden "<input type=\"hidden\" name=\"[ad_quotehtml $varname]\" value=\"[ad_quotehtml $varvalue]\">\n"
	}
    }
    return $hidden
}
proc ad_admin_home {} {
    return "/admin"
}
proc ad_set_client_property__arg_parser {} {    upvar args args
    upvar session_id val ; set val {}
    upvar clob val ; set val f
    upvar persistent val ; set val t
    upvar secure val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -clob {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -clob"
                }
                upvar clob val ; set val [lindex $args [incr i]]
            }
            -secure {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -secure"
                }
                upvar secure val ; set val [lindex $args [incr i]]
            }
            -persistent {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -persistent"
                }
                upvar persistent val ; set val [lindex $args [incr i]]
            }
            -session_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -session_id"
                }
                upvar session_id val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { module name value } $n_args_remaining]"
    }
    upvar module val ; set val [lindex $args [expr { $i + 0 }]]
    upvar name val ; set val [lindex $args [expr { $i + 1 }]]
    upvar value val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc coalesce args {
            ::nstcl::_ad_proc_parser ::nstcl::coalesce [set args]
            
    foreach arg $args {
        upvar 1 $arg x
        if {[info exists x] && ![string equal "" [set x]]} {
            return [set x]
        }
    }
    if {[info exists default]} {
        return $default
    } else {
        return
    }
}
proc ad_proc_valid_switch_p str {
  return [expr [string equal "-" [string index $str 0]] && ![number_p $str]]
}
proc rp_finish_serving_page {} {
    global doc_properties
    if { [info exists doc_properties(body)] } {
        set l [string length $doc_properties(body)]
       rp_debug "Returning page: $l [ad_quotehtml [string range $doc_properties(body) 0 100]]"
	doc_return 200 text/html $doc_properties(body)
    }
}
proc apm_parameter_update__arg_parser {} {    upvar args args
    upvar min_n_values val ; set val 1
    upvar section_name val ; set val {}
    upvar callback val ; set val apm_dummy_callback
    upvar max_n_values val ; set val 1

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 6 } {
        return -code error "No value specified for argument [lindex { parameter_id package_key parameter_name description default_value datatype } $n_args_remaining]"
    }
    upvar parameter_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar package_key val ; set val [lindex $args [expr { $i + 1 }]]
    upvar parameter_name val ; set val [lindex $args [expr { $i + 2 }]]
    upvar description val ; set val [lindex $args [expr { $i + 3 }]]
    upvar default_value val ; set val [lindex $args [expr { $i + 4 }]]
    upvar datatype val ; set val [lindex $args [expr { $i + 5 }]]
    if { $n_args_remaining > 6 } {
        upvar section_name val ; set val [lindex $args [expr { $i + 6 }]]
    }
    if { $n_args_remaining > 7 } {
        upvar min_n_values val ; set val [lindex $args [expr { $i + 7 }]]
    }
    if { $n_args_remaining > 8 } {
        upvar max_n_values val ; set val [lindex $args [expr { $i + 8 }]]
    }
    if { $n_args_remaining > 9 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc set_form_variables_string_trim_DoubleApos {} {
    # The variable names are prefixed with a V to avoid confusion with the form variables while checking for naughtiness.
    uplevel {
	set Vform [ns_getform] 
	if {$Vform == ""} {
	    ns_returnerror 500 "Missing form data"
	    return;
	}
	set Vform_size [ns_set size $Vform]
	set Vform_counter_i 0
	while {$Vform_counter_i<$Vform_size} {
	    set Vname [ns_set key $Vform $Vform_counter_i]
	    set Vvalue [ns_set value $Vform $Vform_counter_i]
	    check_for_form_variable_naughtiness $Vname $Vvalue
	    set $Vname [DoubleApos [string trim $Vvalue]]
	    incr Vform_counter_i
	}
    }
}
proc adp_parse_ad_conn_file {} {
#    handle a request for an adp and/or tcl file in the template system.

    set __adp_stub [file root [ad_conn file]]
    namespace eval template variable parse_level ""
    ns_log Debug "YYY(adp_parse_ad_conn_file): [ad_conn file]\n [file exists [ad_conn file]] [file root [ad_conn file]] "

    ad_conn -set subsite_id [site_node_closest_ancestor_package "acs-subsite"]
    
#    set src [ad_parameter -package_id [ad_conn subsite_id] DefaultMaster dummy "/www/default-master"]
#    set ::nstcl::template::default_master [template::util::url_to_file $src [ad_conn url]]
#    set ::nstcl::template::default_master [file join [ns_info "pageroot"] default-master]

    
#  set mime_type [get_mime_type]
#  set template_extension [get_mime_template_extension $mime_type]

    #set up tcl data sources in this stack frame if a tcl file exists
    if { [catch { template::adp_prepare } errMsg] } {
	
	# return without rendering any HTML if the code aborts
	if { [string equal $errMsg ADP_ABORT] } { 
	    return "" 
	} else {
	    global errorInfo errorCode
	    error $errMsg $errorInfo $errorCode
	}
    }
    # if we get here, adp_prepare ran without throwing an error.
    # and errMsg contains its return value
    
    # initialize the ADP output
    set __adp_output ""

    variable parsed_template ""
    if { [file exists $__adp_stub.adp] } { 

	ns_log Debug puts "Parsing template ..." 	
	template::adp_init adp $__adp_stub

	#execute adp proc 
# should use this...
#	set parsed_template [ template::code::adp::$__adp_stub ]
# (jjs)initially changed to this
	set parsed_template [ ns_adp_parse -file $__adp_stub.adp ] 
# (jjs)now using this to satisfy qd
#	set parsed_template [template::adp_parse $__adp_stub]

    }
    db_release_unused_handles
    
    if {![empty_string_p $parsed_template]} {
        set mime_type [template::get_mime_type]
        set header_preamble [template::get_mime_header_preamble $mime_type]

	ns_return 200 $mime_type "$header_preamble $parsed_template"
    }

    
}
proc merge_form_with_ns_set {form set_id} {

    for {set i 0} {$i<[ns_set size $set_id]} {incr i} {
	set form [ns_formvalueput $form [ns_set key $set_id $i] [ns_set value $set_id $i]]
    }

    return $form

}
proc ad_check_for_naughty_html user_submitted_html {
    set tag_names [list div font]
    # look for a less than sign, zero or more spaces, then the tag
    if { ! [empty_string_p $tag_names]} { 
        if [regexp "< *([join $tag_names "\[ \n\t\r\f\]|"]\[ \n\t\r\f\])" [string tolower $user_submitted_html]] {
            return "<p>For security reasons we do not accept the submission of any HTML 
	    containing the following tags:</p> <code>[join $tag_names " "]</code>" 
        }
    }

    # HTML was okay as far as we know
    return ""
}
proc site_node_create args {    site_node_create__arg_parser

    # Generate an ID if we need one
    if {[empty_string_p $new_node_id]} {
	set new_node_id [db_nextval acs_object_id_seq]
    }

    set user_id [ad_verify_and_get_user_id]
    set ip_address [ad_conn peeraddr]

    set node_id [db_exec_plsql node_new {}]

    return $node_id
}
proc ns_configsections {} {
    variable configuration
    set result {}
    set size [::nstcl::ns_set size $configuration]

    for {set i 0} {$i < $size} {incr i} {
        set setId [::nstcl::ns_set value $configuration $i]
        lappend result [::nstcl::ns_set copy $setId]
    }

    return $configuration
}
proc apm_package_instance_new args {    apm_package_instance_new__arg_parser

    set package_id [apm_package_create_instance -package_id $package_id $instance_name $context_id $package_key]
    apm_package_call_post_instantiation_proc $package_id $package_key
}
proc ad_ns_set_to_tcl_vars args {
            ::nstcl::_ad_proc_parser ::nstcl::ad_ns_set_to_tcl_vars [set args]
            
    switch -- $duplicates {
        overwrite  -
        ignore     -
        fail       {}
        default    { 
            return -code error "Invalid value \"$duplicates\" for -duplicates switch.  Should be one of: ignore, fail, or overwrite"
        }
    }

    set size [::nstcl::ns_set size $setId]
    for {set i 0} {$i < $size} {incr i} {
        set key [::nstcl::ns_set key $setId $i]
        upvar $level $key value

        if {$duplicates == "fail" && [info exists value]} {
            return -code error "Variable \"$key\" already exists"
        } elseif {$duplicates == "overwrite" || ![info exists value]} {
            set value [::nstcl::ns_set value $setId $i]
        }
    }
}
proc ns_adp_parse args { 

    set block1 [ subst { ::template::adp_compile $args }]

    set ccmd  [ uplevel 1  $block1 ]   
#    return "<pre>[    info body $ccmd ]</pre>"
    
    #set block2 [ subst { ::template::adp_eval $ccmd  } ]
    ns_log debug "ns_adp_parse evaling $block1 "

    set block2 [ subst { ::template::adp_eval $ccmd -inline } ]
    return [uplevel 1 $block2 ]
 
#    return "$ccmd"

}
proc apm_package_installed_p package_key {
    return [db_string apm_package_installed_p {
	select 1 from apm_package_versions
	where package_key = :package_key
	and installed_p = 't'
    } -default 0]
}
proc ad_secure_conn_p {} {
    return [string match "https:*" [ad_conn location]]
}
proc rp_invoke_proc {conn argv} {
    set startclicks [clock clicks]

    util_unlist $argv proc_index debug_p arg_count proc arg

#      if { $debug_p } {
#      ns_log "Notice" "Invoking registered procedure $proc"
#      }
    rp_debug -debug $debug_p "Invoking registered procedure $proc"

    switch $arg_count {
	0 { set errno [catch $proc error] }
	1 { set errno [catch "$proc $arg" error] }
	default { set errno [catch {
	  ad_try {
	    $proc [list $conn] $arg
	  } ad_script_abort val {
	    # do nothing
	  }
	} error] }
    }

    global errorCode
    if { $errno } {
      # Uh-oh - an error occurred.
      global errorInfo
      ad_call_proc_if_exists ds_add rp [list registered_proc [list $proc $arg] $startclicks [clock clicks] "error" $errorInfo]
      rp_debug -debug t "error in $proc for [ns_conn method] [ns_conn url]?[ns_conn query] errno is $errno message is $errorInfo"
      rp_report_error
    } else {
      ad_call_proc_if_exists ds_add rp [list registered_proc [list $proc $arg] $startclicks [clock clicks]]
    }

#      if { $debug_p } {
#        ns_log "Notice" "Done invoking registered procedure $proc"
#      }
    rp_debug -debug $debug_p "Done Invoking registered procedure $proc"

    rp_finish_serving_page
}
proc util_search_list_of_lists {list_of_lists query_string {sublist_element_pos 0}} {
    set sublist_index 0
    foreach sublist $list_of_lists {
	set comparison_element [lindex $sublist $sublist_element_pos]
	if { [string compare $query_string $comparison_element] == 0 } {
	    return $sublist_index
	}
	incr sublist_index
    }
    # didn't find it
    return -1
}
proc rp_handle_adp_request {} {
#    ns_log debug [info 
    doc_init
    set adp [ns_adp_parse -file [ad_conn file]]

    if { [doc_exists_p] } {
	doc_set_property body $adp
	doc_serve_document
    } else {
	set content_type [ns_set iget [ad_conn outputheaders] "content-type"]
	if { $content_type == "" } {
	    set content_type "text/html"
	}
	doc_return 200 $content_type $adp
    }
}
proc set_variables_after_subquery {} {
    upvar sub_selection sub_selection
    ::nstcl::ad_ns_set_to_tcl_vars -level 2 $sub_selection
}
proc apm_package_version_installed_p {package_key version_name} {
    return [db_string apm_package_version_installed_p {
	select decode(count(*), 0, 0, 1) from apm_package_versions
	where package_key = :package_key
	and version_name = :version_name
    } -default 0]
}
proc ad_page_contract_eval args {
    uplevel 1 $args
}
proc util_memoize {script {max_age {}}} {

    if {![string equal $max_age ""] && $max_age < 0} {
        error "max_age must not be negative"
    }

    set current_time [ns_time]

    set cached_p [ns_cache get util_memoize $script pair]

    if {$cached_p && [string compare $max_age ""] != 0} {
        set cache_time [lindex $pair 0]
        if {$current_time - $cache_time > $max_age} {
	    ns_cache flush util_memoize $script
	    set cached_p 0
        }
    }

    if {!$cached_p} {
        set pair [ns_cache eval util_memoize $script {
	    list $current_time [eval $script]
	}]
    }

    return [lindex $pair 1]
}
proc ad_export_vars__arg_parser {} {    upvar args args
    upvar include val ; set val {}
    upvar override val ; set val {}
    upvar exclude val ; set val {}
    upvar form_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -form - -form=1 {
                uplevel set form_p 1
            }
            -form=0 {
                uplevel set form_p 0
            }
            -exclude {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -exclude"
                }
                upvar exclude val ; set val [lindex $args [incr i]]
            }
            -override {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -override"
                }
                upvar override val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        upvar include val ; set val [lindex $args [expr { $i + 0 }]]
    }
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_var_type_check_word_p value {

    if [regexp {[^-A-Za-z0-9_]} $value] {
        return 0
    } else {
        return 1
    }
}
proc invert_array args {
            ::nstcl::_ad_proc_parser ::nstcl::invert_array [set args]
            
    set modes [list update get new]
 
    if {[lsearch -exact $modes $mode] == -1} {
        error "Invalid mode \"$mode\".  Should be one of [join $modes ", "]."
    }
 
    if {![string equal "" $new_var_name] && $mode != "new"} {
        error "new_var_name should be specified only with -mode new"
    }
 
    if {[string equal "" $new_var_name] && $mode == "new"} {
        error "new_var_name required with -mode new"
    }
 
    upvar 1 $old_var_name old_array
 
    if {![info exists old_array] || ![array exists old_array]} {
        error "$old_var_name does not exist as an array!"
    } else {
        set old_data [array get old_array]
    }
 
    if {$mode == "update"} {
        unset old_array
        upvar 1 $old_var_name new_array
    }
 
    if {$mode == "new"} {
        upvar 1 $new_var_name new_array
    }
 
    if {[info exists new_array] && ![array exists new_array]} {
        error "new_array already exists but isn't an array!"
    }
 
    array set new_array {}
 
 
    switch -exact -- $split_p.$append_p {
        0.0 {
                # no split, no append
                foreach {key value} $old_data {
                    set new_array($value) $key
                }
            }
 
        0.1 {
                # no split, append
                foreach {key value} $old_data {
                    lappend new_array($value) $key
                }
            }
 
        1.0 {
                # split, no append
                foreach {key value} $old_data {
                    foreach split_value $value {
                        set new_array($split_value) $key
                    }
                }
            }
 
        1.1 {
                # split, append
                foreach {key value} $old_data {
                    foreach split_value $value {
                        lappend new_array($split_value) $key
                    }
                }
            }
    }
 
 
    if {$mode == "get"} {
        return [array get new_array]
    }
}
proc rp_concrete_file__arg_parser {} {    upvar args args
    upvar extension_pattern val ; set val .*

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -extension_pattern {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -extension_pattern"
                }
                upvar extension_pattern val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { path } $n_args_remaining]"
    }
    upvar path val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc db_qd_internal_get_cache fullquery_name {

    # If we have no record
    if {![nsv_exists OACS_FULLQUERIES $fullquery_name]} {
	return ""
    }

    set fullquery_array [nsv_get OACS_FULLQUERIES $fullquery_name]

    # If this isn't cached!
    if {$fullquery_array == ""} {
	# we need to do something
	return ""
    }

    # See if we have the correct location for this query
    db_qd_log Debug "query $fullquery_name from [db_fullquery_get_load_location $fullquery_array]"

    # reload the fullquery
    set fullquery_array [nsv_get OACS_FULLQUERIES $fullquery_name]

    # What we get back from the cache is the FullQuery structure
    return $fullquery_array
}
proc nsv_lappend {id key value args} {
   global $id 
    lappend ${id}($key) $value

}
proc ad_host_administrator {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  HostAdministrator]
}
proc ad_block_sql_urls {conn args why} {
    set form [ns_getform]
    if [empty_string_p $form] { return filter_ok }

    # Check each form data variable to see if it contains malicious
    # user input that we don't want to interpolate into our SQL
    # statements.
    #
    # We do this by scanning the variable for suspicious phrases; at
    # this time, the phrases we look for are: UNION, UNION ALL, and
    # OR.
    #
    # If one of these phrases is found, we construct a test SQL query
    # that incorporates the variable into its WHERE clause and ask
    # the database to parse it. If the query does parse successfully,
    # then we know that the suspicious user input would result in a
    # executing SQL that we didn't write, so we abort processing this
    # HTTP request.
    #
    set n_form_vars [ns_set size $form]
    for { set i 0 } { $i < $n_form_vars } { incr i } {
        set key [ns_set key $form $i]
        set value [ns_set value $form $i]

	# michael@arsdigita.com:
	#
	# Removed 4000-character length check, because that allowed
	# malicious users to smuggle SQL fragments greater than 4000
	# characters in length.
	#
        if {
	    [regexp -nocase {[^a-z_]or[^a-z0-9_]} $value] ||
	    [regexp -nocase {union([^a-z0-9_].*all)?[^a-z0-9_].*select} $value]
	} {
	    # Looks like the user has added "union [all] select" to
	    # the variable, # or is trying to modify the WHERE clause
	    # by adding "or ...".
	    #
            # Let's see if Oracle would accept this variables as part
	    # of a typical WHERE clause, either as string or integer.
	    #
	    # michael@arsdigita.com: Should we grab a handle once
	    # outside of the loop?
	    #
            set parse_result_integer [db_string sql_test_1 "select test_sql('select 1 from dual where 1=[DoubleApos $value]') from dual"]

            if { [string first "'" $value] != -1 } {
		#
		# The form variable contains at least one single
		# quote. This can be a problem in the case that
		# the programmer forgot to QQ the variable before
		# interpolation into SQL, because the variable
		# could contain a single quote to terminate the
		# criterion and then smuggled SQL after that, e.g.:
		#
		#   set foo "' or 'a' = 'a"
		#
		#   db_dml "delete from bar where foo = '$foo'"
		#
		# which would be processed as:
		#
		#   delete from bar where foo = '' or 'a' = 'a'
		#
		# resulting in the effective truncation of the bar
		# table.
		#
                set parse_result_string [db_string sql_test_2 "select test_sql('select 1 from dual where 1=[DoubleApos "'$value'"]') from dual"]
            } else {
                set parse_result_string 1
            }

            if {
		$parse_result_integer == 0 ||
		$parse_result_integer == -904  ||
		$parse_result_integer == -1789 ||
		$parse_result_string == 0 ||
		$parse_result_string == -904 ||
		$parse_result_string == -1789
	    } {
                # Code -904 means "invalid column", -1789 means
		# "incorrect number of result columns". We treat this
		# the same as 0 (no error) because the above statement
		# just selects from dual and 904 or 1789 only occur
		# after the parser has validated that the query syntax
		# is valid.

                ns_log Error "ad_block_sql_urls: Suspicious request from [ad_conn peeraddr]. Parameter $key contains code that looks like part of a valid SQL WHERE clause: [ad_conn url]?[ad_conn query]"

		# michael@arsdigita.com: Maybe we should just return a
		# 501 error.
		#
                ad_return_error "Suspicious Request" "Parameter $key looks like it contains SQL code. For security reasons, the system won't accept your request."

                return filter_return
            }
        }
    }

    return filter_ok
}
proc ad_acs_admin_id_mem {} {
    return [db_string acs_kernel_id_get {
	select package_id from apm_packages
	where package_key = 'acs-admin'
    } -default 0]
}
proc ad_acs_kernel_id_mem {} {
    return [db_string acs_kernel_id_get {
        select package_id from apm_packages
        where package_key = 'acs-kernel'
    } -default 0]
}
proc ad_urlencode string {
    set encoded_string [ns_urlencode $string]
    regsub -all {%2d} $encoded_string {-} encoded_string
    regsub -all {%5f} $encoded_string {_} ad_encoded_string
    return $ad_encoded_string
}
proc ad_user_login__arg_parser {} {    upvar args args
    upvar forever_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -forever - -forever=1 {
                uplevel set forever_p 1
            }
            -forever=0 {
                uplevel set forever_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { user_id } $n_args_remaining]"
    }
    upvar user_id val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ns_return {status type string} {
    ns_log notice " ns_return: status $status type=$type length=[string length $string] "    

    set ::pnsd::__http_mime $type
    set ::pnsd::__http_stream $string
    
    return 
}
proc ad_raise {exception {value {}}} {
  return -code error -errorcode [list "AD" "EXCEPTION" $exception] $value
}
proc ad_return_warning {title explanation} {
    ad_return_exception_page 200 $title $explanation
}
proc ad_register_filter__arg_parser {} {    upvar args args
    upvar description val ; set val {}
    upvar arg val ; set val {}
    upvar priority val ; set val 10000
    upvar critical val ; set val f
    upvar debug val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -debug {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -debug"
                }
                upvar debug val ; set val [lindex $args [incr i]]
            }
            -priority {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -priority"
                }
                upvar priority val ; set val [lindex $args [incr i]]
            }
            -critical {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -critical"
                }
                upvar critical val ; set val [lindex $args [incr i]]
            }
            -description {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -description"
                }
                upvar description val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 4 } {
        return -code error "No value specified for argument [lindex { kind method path proc } $n_args_remaining]"
    }
    upvar kind val ; set val [lindex $args [expr { $i + 0 }]]
    upvar method val ; set val [lindex $args [expr { $i + 1 }]]
    upvar path val ; set val [lindex $args [expr { $i + 2 }]]
    upvar proc val ; set val [lindex $args [expr { $i + 3 }]]
    if { $n_args_remaining > 4 } {
        upvar arg val ; set val [lindex $args [expr { $i + 4 }]]
    }
    if { $n_args_remaining > 5 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc first_day_of_week args {
            ::nstcl::_ad_proc_parser ::nstcl::first_day_of_week [set args]
            
    set time [clock scan $date]
    set dow  [clock format $time -format %w]
    return [clock format [clock scan "-$dow days" -base $time] -format $format]
}
proc nsv_unset {id key} {  
    global $id
    array unset $id $key

}
proc ad_get_user_id {} {
    return [ad_conn user_id]
}
proc ad_find_all_files args {    ad_find_all_files__arg_parser

    # Use the examined_files array to track files that we've examined.
    array set examined_files [list]

    # A list of files that we will return (in the order in which we
    # examined them).
    set files [list]

    # A list of files that we still need to examine.
    set files_to_examine [list $path]

    # Perform a breadth-first search of the file tree. For each level,
    # examine files in $files_to_examine; if we encounter any directories,
    # add contained files to $new_files_to_examine (which will become
    # $files_to_examine in the next iteration).
    while { [incr max_depth -1] > 0 && [llength $files_to_examine] != 0 } {
	set new_files_to_examine [list]
	foreach file $files_to_examine {
	    # Only examine the file if we haven't already. (This is just a safeguard
	    # in case, e.g., Tcl decides to play funny games with symbolic links so
	    # we end up encountering the same file twice.)
	    if { ![info exists examined_files($file)] } {
		# Remember that we've examined the file.
		set examined_files($file) 1

		if { [empty_string_p $check_file_func] || [eval "$check_file_func $file"] } {
		    # If it's a file, add to our list. If it's a
		    # directory, add its contents to our list of files to
		    # examine next time.
		    if { [file isfile $file] } {
			lappend files $file
		    } elseif { [file isdirectory $file] } {
			if { $include_dirs == 1 } {
			    lappend files $file
			}
			set new_files_to_examine [concat $new_files_to_examine [glob -nocomplain "$file/*"]]
		    }
		}
	    }
	}
	set files_to_examine $new_files_to_examine
    }
    return $files
}
proc site_node_create__arg_parser {} {    upvar args args
    upvar directory_p val ; set val t
    upvar pattern_p val ; set val t
    upvar new_node_id val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -new_node_id {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -new_node_id"
                }
                upvar new_node_id val ; set val [lindex $args [incr i]]
            }
            -directory_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -directory_p"
                }
                upvar directory_p val ; set val [lindex $args [incr i]]
            }
            -pattern_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -pattern_p"
                }
                upvar pattern_p val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { parent_node_id name } $n_args_remaining]"
    }
    upvar parent_node_id val ; set val [lindex $args [expr { $i + 0 }]]
    upvar name val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_verify_and_get_session_id args {    ad_verify_and_get_session_id__arg_parser

    return [ad_conn session_id]
}
proc db_qd_internal_prepare_queryfile_content file_content {
    
    set new_file_content ""

    # The lazy way to do it.  partialquery was added for clarification of 
    # the query files, but in fact a partialquery and a fullquery are parsed 
    # exactly the same.  Doing this saves the bother of having to tweak the 
    # rest of the parsing code to handle partialquery.  (OpenACS - DanW)

    regsub -all {(</?)partialquery([ >])} $file_content {\1fullquery\2} rest_of_file_content

    set querytext_open "<querytext>"
    set querytext_close "</querytext>"

    set querytext_open_len [string length $querytext_open]
    set querytext_close_len [string length $querytext_close]

    # We're going to ns_quotehtml the querytext,
    # because ns_xml will choke otherwise
    while {1} {
	set first_querytext_open [string first $querytext_open $rest_of_file_content]
	set first_querytext_close [string first $querytext_close $rest_of_file_content]

	# We have no more querytext to process
	if {$first_querytext_open == -1} {
	    append new_file_content $rest_of_file_content
	    break
	}

	# append first chunk before the querytext including "<querytext>"
	append new_file_content [string range $rest_of_file_content 0 [expr "$first_querytext_open + $querytext_open_len - 1"]]

	# append quoted querytext
	append new_file_content [ns_quotehtml [string range $rest_of_file_content [expr "$first_querytext_open + $querytext_open_len"] [expr "$first_querytext_close - 1"]]]

	# append close querytext
	append new_file_content $querytext_close

	# Set up the rest
	set rest_of_file_content [string range $rest_of_file_content [expr "$first_querytext_close + $querytext_close_len"] end]
    }

#    db_qd_log Debug "new massaged file content: \n $new_file_content \n"

    return $new_file_content
}
proc convertclock {dateString {zone {}} {baseClock {}}} {
    lappend cmd clock scan $dateString
    if ![lempty $zone] {
        lappend cmd -gmt 1
    }
    if ![lempty $baseClock] {
        lappend cmd -base $baseClock
    }
    return [eval $cmd]
}
proc ad_method {method_name type argblock docblock body} {
    ad_proc ${method_name}__$type $argblock $docblock $body
}
proc db_bootstrap_checks {errors error_p} {}
proc ns_sendmail {to from subject body {headers {}} {bcc {}}} {
    package require mime
    package require smtp

    variable mailhost
    variable mailport

    set mime [::mime::initialize -canonical text/plain -string $body]
    set command [list ::smtp::sendmessage $mime -servers $mailhost  -ports $mailport]

    lappend command -header [list From $from] 
    lappend command -header [list To $to]
    lappend command -header [list Subject $subject]

    if {[string length $bcc]} {
        lappend command -header [list Bcc $bcc]
    }

    if {[string length $headers]} {
        set size [::nstcl::ns_set size $headers]
        for {set i 0} {$i < $size} {incr i} {
            lappend command -header [list [::nstcl::ns_set key $headers $i]  [::nstcl::ns_set value $headers $i]]
        }
    }

    set bombed_p [catch { eval $command } result]
    ::mime::finalize $mime
    
    if {$bombed_p} {
        error $result
    }
}
proc db_qd_internal_parse_one_query parsing_state {
    
    # Find the index that we're looking at
    set index [lindex $parsing_state 0]
    
    # Find the list of nodes
    set node_list [lindex $parsing_state 1]

    # Parsed Doc Pointer
    set parsed_doc [lindex $parsing_state 2]

    # Default RDBMS
    set default_rdbms [lindex $parsing_state 3]
    set file_path [lindex $parsing_state 4]

    db_qd_log Debug "default_rdbms is $default_rdbms"

    db_qd_log Debug "node_list is $node_list with length [llength $node_list] and index $index"

    # BASE CASE
    if {[llength $node_list] <= $index} {
	# Clean up
	xml_doc_free $parsed_doc

	db_qd_log Debug "Cleaning up, done parsing"

	# return nothing
	return ""
    }

    # Get one query
    set one_query_xml [lindex $node_list $index]
    
    # increase index
    incr index

    # Update the parsing state so we know
    # what to parse next 
    set parsing_state [list $index $node_list [lindex $parsing_state 2] $default_rdbms $file_path]

    # Parse the actual query from XML
    set one_query [db_qd_internal_parse_one_query_from_xml_node $one_query_xml $default_rdbms $file_path]

    # Return the query and the parsing state
    return [list $one_query $parsing_state]

}
proc acs_root_dir {} {
	# If we can't load the nsv_set, use the parameter
	if {[nsv_get acs_properties root_directory] eq ""} {
    	return [nsv_get acs_properties root_directory]
	} else {
		return $::pnsd::root
	}	
}
proc remove_whitespace input_string {
    if [regsub -all "\[\015\012\t \]" $input_string "" output_string] {
	return $output_string 
    } else {
	return $input_string
    }
}
proc ad_convert_to_text args {    ad_convert_to_text__arg_parser

    if { [string equal $html_p t] } {
	set from html
    } else {
	set from text
    }
    return [ad_html_text_convert -from $from -to text -- $text]
}
proc server_cluster_peer_hosts {} {
    set peer_hosts [list]
    set my_ip [ns_config ns/server/[ns_info server]/module/nssock Address]

    foreach host [server_cluster_all_hosts] {
	if { $host != $my_ip } {
	    lappend peer_hosts $host
	}
    }

    return $peer_hosts
}
proc ReturnHeadersNoCache {{content_type text/html}} {

    ns_write "HTTP/1.0 200 OK
MIME-Version: 1.0
Content-Type: $content_type
pragma: no-cache\r\n"

     ns_startcontent -type $content_type
}
proc server_cluster_authorized_p ip {
    if { ![server_cluster_enabled_p] } {
	return 0
    }

    if { $ip == "127.0.0.1" } {
	return 1
    }
    # lsearch -glob appears to crash AOLserver 2. Oh well.
    foreach glob [ad_parameter -package_id [ad_acs_kernel_id] ClusterAuthorizedIP server-cluster] {
	if { [string match $glob $ip] } {
	    return 1
	}
    }
    return 0
}
proc apm_package_key_from_id package_id {
    return [util_memoize "apm_package_key_from_id_mem $package_id"]
}
proc ad_page_contract_filter_rule {name proc_args doc_string body} {
    if { [llength $proc_args] != 2 } {
	return -code error "The proc must accept two  arguments, the name of the variable and a list of filters"
    }

    set script [info script]
    set proc_name ad_page_contract_filter_rule_proc_$name

    set mutex [nsv_get ad_page_contract_mutex filter_rules]
    ns_mutex lock $mutex

    if { [nsv_exists ad_page_contract_filter_rules $name] } {
	set prior_script [ad_page_contract_filter_rule_script $name]
	if { ![string equal $script $prior_script] } {
	    ns_log Warning "Multiple definitions of the ad_page_contract_filter_rule \"$name\" in $script and $prior_script"
	}
    }

    nsv_set ad_page_contract_filter_rules $name [list $proc_name $doc_string $script]
    ns_mutex unlock $mutex

    # same trick as ad_page_contract_filter does.

    set arg0 [lindex $proc_args 0]
    set arg1 [lindex $proc_args 1]
    ad_proc $proc_name [list $arg0 ${arg1}_varname] $doc_string "upvar \$${arg1}_varname $arg1\n$body"
}
proc api_page_documentation_mode_p {} {
    global ad_conn
    if { [info exists ad_conn(api_page_documentation_mode_p)] } {
	return $ad_conn(api_page_documentation_mode_p)
    }
    return 0
}
proc ns_mutex {cmd args} { 
    return $cmd
}
proc util_memoize_flush script {
        server_cluster_httpget_from_peers "/SYSTEM/flush-memoized-statement.tcl?statement=[ns_urlencode $script]"
    
    ns_cache flush util_memoize $script
}
proc db_0or1row {statement_name pre_sql args} {
#        ns_log info "db_0or1row called with [info level 0]"
        set full_statement_name statement_name
	if [catch {
            set full_statement_name [db_qd_get_fullname $statement_name]
            set sql [ db_qd_replace_sql $full_statement_name $pre_sql] } ] {
	    set sql $pre_sql
	}
	
#        if {! [string equal $pre_sql $sql] } {
#          puts [list "QD interceptifier:" $statement_name $full_statement_name $pre_sql $sql  ]
#        }
#        puts " Full Statement Name: $full_statement_name"
        set cmd [list ::nstcl::db_0or1row $statement_name $sql $args]
#        puts $cmd
	uplevel 1 [concat [list ::nstcl::db_0or1row $statement_name $sql] $args]
    }
proc util_absolute_path_p path {
   set firstchar [string index $path 0]
   if {[string compare $firstchar /]} {
        return 0
   } else {
        return 1
   }
}
proc stack_frame_values level {
  set varlist ""
  foreach i [if $level {
    uplevel \#$level {info locals}
  } else {info globals} ] {
    append varlist "    <li><b>$i</b> = "
    if {[string equal $i page] && $level == [info level]-1 ||
	[string equal $i "__adp_output"] || [string equal $i "errorInfo"]} {
      append varlist "<em>value withheld to avoid messy page</em>\n"
    } elseif {[string match -nocase "*secret*" $i]} {
      append varlist "<em>value withheld as the name contains \"secret\"</em>\n"
    } else {
      if [uplevel \#$level array exists $i] {
	append varlist "<em>ARRAY</em><ul>\n"
	foreach {key value} [uplevel \#$level array get $i] {
	  append varlist "        <li><b>$key</b> = '$value'\n"
	}
	append varlist "        </ul>\n"
      } else {
	if [catch {append varlist "'[uplevel #$level set $i]'\n"}] {
	  append varlist "<em>bad string value</em>\n"
	}
      }
    }
  }
  return $varlist
}
proc ad_var_type_check_third_urlv_integer_p {{args {}}} {

    set third_url_element [lindex [ad_conn urlv] 3]

    if [regexp {[^0-9]} $third_url_element] {
        return 0
    } else {
        return 1
    }
}
proc ad_parameter_from_file {name {package_key {}}} {
    set ns_param ""

    # The below is really a hack because none of the calls to ad_parameter in the system
    # actually call 'ad_parameter param_name acs-kernel'.

    if { [empty_string_p $package_key] || $package_key == "acs-kernel"} {
	set ns_param [ns_config "ns/server/[ns_info server]/acs" $name]
    } else {
	set ns_param [ns_config "ns/server/[ns_info server]/acs/$package_key" $name]
    }
    return $ns_param
}
proc db_source_sql_file__arg_parser {} {    upvar args args
    upvar callback val ; set val apm_ns_write_callback

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -callback {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -callback"
                }
                upvar callback val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { file } $n_args_remaining]"
    }
    upvar file val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc util_maybe_convert_to_html {raw_string html_p} {
    if { $html_p == "t" } {
	return $raw_string
    } else {
	return [util_convert_plaintext_to_html $raw_string]
    }
}
proc doc_return args {
    db_release_unused_handles
    eval "ns_return $args"
}
proc ad_page_contract_filter__arg_parser {} {    upvar args args
    upvar priority val ; set val 1000
    upvar type val ; set val filter

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -type {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -type"
                }
                upvar type val ; set val [lindex $args [incr i]]
            }
            -priority {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -priority"
                }
                upvar priority val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 4 } {
        return -code error "No value specified for argument [lindex { name proc_args doc_string body } $n_args_remaining]"
    }
    upvar name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar proc_args val ; set val [lindex $args [expr { $i + 1 }]]
    upvar doc_string val ; set val [lindex $args [expr { $i + 2 }]]
    upvar body val ; set val [lindex $args [expr { $i + 3 }]]
    if { $n_args_remaining > 4 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc apm_package_call_post_instantiation_proc {package_id package_key} {

    # Check for a post-instantiation TCL procedure
    set procedure_name [apm_post_instantiation_tcl_proc_from_key $package_key]
    if { ![empty_string_p $procedure_name] } {
	with_catch errmsg {
	    $procedure_name $package_id
	} {
	    ns_log Error "APM: Post-instantiation procedure, $procedure_name, failed: $errmsg"
	}
    }
    
}
proc ad_complain__arg_parser {} {    upvar args args
    upvar message val ; set val {}
    upvar key val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -key {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -key"
                }
                upvar key val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    if { $n_args_remaining > 0 } {
        upvar message val ; set val [lindex $args [expr { $i + 0 }]]
    }
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc doc_serve_template __template_path {
    upvar #0 doc_properties __doc_properties
    foreach __name [array names __doc_properties] {
	set $__name $__doc_properties($__name)
    }

    set adp [ns_adp_parse -file $__template_path]
    set content_type [ns_set iget [ad_conn outputheaders] "content-type"]
    if { [empty_string_p $content_type] } {
	set content_type "text/html"
    }
    doc_return 200 $content_type $adp
}
proc ad_pvt_home_name {} {
    return [ad_parameter -package_id [ad_acs_kernel_id] HomeName]
}
proc ad_html_to_text args {    ad_html_to_text__arg_parser

    set output(text) {}
    set output(linelen) 0
    set output(maxlen) $maxlen
    set output(pre) 0
    set output(p) 0
    set output(br) 0
    set output(space) 0
    set output(blockquote) 0
    
    set length [string length $html]
    set last_tag_end 0

    # For showing the URL of links.
    set href_urls [list]
    set href_stack [list]

    for { set i [string first < $html] } { $i != -1 } { set i [string first < $html $i] } {
	# append everything up to and not including the tag-opening <
	ad_html_to_text_put_text output [string range $html $last_tag_end [expr {$i - 1}]]
    
	# we're inside a tag now. Find the end of it

	# make i point to the char after the <
	incr i
	set tag_start $i
	
	set count 0
	while 1 {
	    if { [incr count] > 100 } {
		error "There appears to be a programming bug in ad_html_to_text: We've entered an infinite loop."
	    }
	    # Find the positions of the first quote, apostrophe and greater-than sign.
	    set quote_idx [string first \" $html $i]
	    set apostrophe_idx [string first ' $html $i]
	    set gt_idx [string first > $html $i]

	    # If there is no greater-than sign, then the tag isn't closed.
	    if { $gt_idx == -1 } {
		set i $length
		break
	    }

	    # Find the first of the quote and the apostrophe 
	    if { $apostrophe_idx == -1 } {
		set string_delimiter_idx $quote_idx
	    } else {
		if { $quote_idx == -1 } {
		    set string_delimiter_idx $apostrophe_idx
		} else {
		    if { $apostrophe_idx < $quote_idx } {
			set string_delimiter_idx $apostrophe_idx
		    } else {
			set string_delimiter_idx $quote_idx
		    }
		}
	    }
	    set string_delimiter [string index $html $string_delimiter_idx]

	    # If the greeater than sign appears before any of the string delimters, we've found the tag end.
	    if { $gt_idx < $string_delimiter_idx || $string_delimiter_idx == -1 } {
		# we found the tag end
		set i $gt_idx
		break
	    }   

	    # Otherwise, we'll have to skip past the ending string delimiter
	    set i [string first $string_delimiter $html [incr string_delimiter_idx]]
	    if { $i == -1 } {
		# Missing string end delimiter
		set i $length
		break
	    }
	    incr i
	}
	
	set full_tag [string range $html $tag_start [expr { $i - 1 }]]

	if { ![regexp {(/?)([^\s]+)[\s]*(\s.*)?} $full_tag match slash tagname attributes] } {
	    # A malformed tag -- just delete it
	} else {

	    # Reset/create attribute array
            array unset attribute_array

	    # Parse the attributes
	    ad_parse_html_attributes -attribute_array attribute_array $attributes

	    switch -- [string tolower $tagname] {
		p - ul - ol - table {
		    set output(p) 1
		}
		br {
		    ad_html_to_text_put_newline output
		}
		tr - td - th {
		    set output(br) 1
		}
		h1 - h2 - h3 - h4 - h5 - h6 {
		    set output(p) 1
		    if { [empty_string_p $slash] } {
			ad_html_to_text_put_text output [string repeat "*" [string index $tagname 1]]
		    }
		}
		li {
		    set output(br) 1
		    if { [empty_string_p $slash] } {
			ad_html_to_text_put_text output "- "
		    }
		}
		strong - b {
		    ad_html_to_text_put_text output "*"
		}
		em - i - cite - u {
		    ad_html_to_text_put_text output "_"
		}
		a {
		    if { [empty_string_p $slash] } {
			if { [info exists attribute_array(href)] } {
			    if { [info exists attribute_array(title)] } {
				set title ": '$attribute_array(title)'"
			    } else {
				set title ""
			    }
			    set href_no [expr [llength $href_urls] + 1]
			    lappend href_urls "\[$href_no\] $attribute_array(href) "
			    lappend href_stack "\[$href_no$title\]"
			} elseif { [info exists attribute_array(title)] } {
			    lappend href_stack "\[$attribute_array(title)\]"
			} else {
			    lappend href_stack {}
			}
		    } else {
			if { [llength $href_stack] > 0 } {
			    if { ![empty_string_p [lindex $href_stack end]] } {
				ad_html_to_text_put_text output [lindex $href_stack end]
			    }
			    set href_stack [lreplace $href_stack end end]
			}
		    }
		}
		pre {
		    set output(p) 1
		    if { [empty_string_p $slash] } {
			incr output(pre)
		    } else {
			incr output(pre) -1
		    }
		}
		blockquote {
		    set output(p) 1
		    if { [empty_string_p $slash] } {
			incr output(blockquote)
			incr output(maxlen) -4
		    } else {
			incr output(blockquote) -1
			incr output(maxlen) 4
		    }
		}
		hr {
		    set output(p) 1
		    ad_html_to_text_put_text output [string repeat "-" $output(maxlen)]
		    set output(p) 1
		}
		q {
		    ad_html_to_text_put_text output \"
		}
		img {
		    if { [empty_string_p $slash] } {
			set img_info {}
			if { [info exists attribute_array(alt)] } {
			    lappend img_info "'$attribute_array(alt)'"
			}   
			if { [info exists attribute_array(src)] } {
			    lappend img_info $attribute_array(src)
			}
			if { [llength $img_info] == 0 } {
			    ad_html_to_text_put_text output {[IMAGE]}
			} else {
			    ad_html_to_text_put_text output "\[IMAGE: [join $img_info " "]\]"
			}
		    }
		}
		default {
		    # Other tag
		    if { $showtags_p } {
			ad_html_to_text_put_text output "&lt;$slash$tagname$attributes&gt;"
		    }
		}
	    }
	}
	
	# set end of last tag to the character following the >
	set last_tag_end [incr i]
    }
    # append everything after the last tag
    ad_html_to_text_put_text output [string range $html $last_tag_end end]
    
    # Close any unclosed tags
    set output(pre) 0
    while { $output(blockquote) > 0 } {
	incr $output(blockquote) -1
	incr $output(maxlen) 4
    }
    
    # write out URLs, if necessary:
    if { [llength $href_urls] > 0 } {
	append output(text) "\n\n[join $href_urls "\n"]"
    }

    return $output(text)
}
proc ns_conn {cmd args} {

    switch $cmd {
	authpassword { 
	    ns_log Warn "ns_conn authpassword not implemented yet"
	    return ""
	}
 	authuser { 
	    ns_log Warn "ns_conn authuser not implemented yet"
	    return ""
	}
 	close { 
	    ns_log Warn "ns_conn close not implemented yet"
	    return }
 	contentlength { ns_log Warn "ns_conn contentlength not implemented yet"
	    return }
 	driver { ns_log Warn "ns_conn driver not implemented yet"
	    return }
 	form { ns_log Warn "ns_conn form not implemented yet"
	    return }
 	headers { #ns_log Warn "ns_conn headers not fully implemented yet"
	    return $::pnsd::headers_id 
	}
 	host { #ns_log Warn "ns_conn host not implemented yet"
	    return [info hostname] }
 	isconnected { 
	    #ns_log Warn "ns_conn isconnected not implemented yet"
	    return 1}
 	location { 
	    return "http://[ns_conn host]:[ns_conn port]/" }
 	method { 
#	    ns_log Warn "ns_conn method not fully implemented yet"
	    return "GET"
	}

 	outputheaders { #ns_log Warn "ns_conn outputheaders not implemented yet"
	    return $::pnsd::output_headers_id
	}
 	peeraddr { 
	    #ns_log Warn "ns_conn peeraddr not implemented yet"
	    return "127.0.0.1"
	}
 	port { 
#	    ns_log Warn "ns_conn port not implemented yet"
	    return "80"
	}
 	protocol { 
	    return "http"}
 	query { 
#	    ns_log Warn "ns_conn query not implemented yet"

	    return $::pnsd::querystring
	}
 	request { 
	    return "GET [ns_conn url] HTTP/[ns_conn version]" 
	}
 	url { 
#	    ns_log Warn "ns_conn url not fully implemented yet"
	    return $::pnsd::url
	}
 	urlv { 
#	    ns_log Warn "ns_conn urlc not implemented yet"
	    set urllist [split [ns_conn url] /]
	    return [lrange $urllist 1 [llength $urllist]]
	}
 	urlc { 
# 	    ns_log Warn "ns_conn urlv not implemented yet"
	    set urlv [expr [llength [split [ns_conn url] /]] - 1]
	    if {$urlv == ""} { return 0 }
	    return $urlv
	}
 	version { 
#	    ns_log Warn "ns_conn version not implemented yet"
	    return "1.0"
	}
    }

}
proc database_to_tcl_string {dbhandle SQL} {
    set selection [::nstcl::ns_db 1row $dbhandle $SQL]
    set result [::nstcl::ns_set value $selection 0]
    ::nstcl::ns_set free $selection
    return $result
}
proc sec_update_user_session_info user_id {
    db_dml update_last_visit {
        update users
        set second_to_last_visit = last_visit,
            last_visit = sysdate,
            n_sessions = n_sessions + 1
        where user_id = :user_id
    }
}
proc acs_community_member_admin_url args {    acs_community_member_admin_url__arg_parser

    return "[ad_parameter -package_id [ad_acs_kernel_id] CommunityMemberAdminURL]?[export_vars user_id]"
}
proc apm_bootstrap_load_queries package_key {

    # Load up queries.

    set root_directory [nsv_get acs_properties root_directory]
    set db_type [nsv_get ad_database_type .]

    # DRB: We can't parse the $package_key.info file at this point in time, primarily because
    # grabbing the package information uses not only the XML file but tables from the APM,
	# which haven't been loaded yet if we're installing.  So we just snarf all of the
	# queryfiles in this package that match the current database or no database
    # (which we interpret to mean all supported databases).

    set files [ad_find_all_files $root_directory/packages/$package_key]
    if { [llength $files] == 0 } {
	error "Unable to locate $root_directory/packages/$package_key/*."
    }

    foreach file [lsort $files] {

        set file_db_type [apm_guess_db_type $package_key $file]
        set file_type [apm_guess_file_type $package_key $file]

        if {[string equal $file_type query_file] &&
            ([empty_string_p $file_db_type] || [string equal $file_db_type $db_type])} {
	    db_qd_load_query_file $file
        } 
    }
}
proc ad_set_client_property args {    ad_set_client_property__arg_parser


    if { $secure != "f" && ![ad_secure_conn_p] } {
	error "Unable to set secure property in insecure or invalid session"
    }

    if { [empty_string_p $session_id] } {
        set session_id [ad_conn session_id]
    }

    if { $persistent == "t" } {
        # Write to database - either defer, or write immediately. First delete the old
        # value if any; then insert the new one.
	
	set last_hit [ns_time]

	db_transaction {

            # DRB: Older versions of this code did a delete/insert pair in an attempt
            # to guard against duplicate insertions.  This didn't work if there was
            # no value for this property in the table and two transactions ran in
            # parallel.  The problem is that without an existing row the delete had
            # nothing to lock on, thus allowing the two inserts to conflict.  This
            # was discovered on a page built of frames, where the two requests from
            # the browser spawned two AOLserver threads to service them.

            # Oracle doesn't allow a RETURNING clause on an insert with a
            # subselect, so this code first inserts a dummy value if none exists
            # (ensuring it does exist afterwards) then updates it with the real
            # value.  Ugh.  

            set clob_update_dml [db_map prop_update_dml_clob]

            db_dml prop_insert_dml ""

            if { $clob == "t" && ![empty_string_p $clob_update_dml] } {
                db_dml prop_update_dml_clob "" -clobs [list $value]
            } else {
                db_dml prop_update_dml ""
	    }
	}
    }

    # Remember the new value, seeding the memoize cache with the proper value.
    util_memoize_seed [list sec_lookup_property $session_id $module $name] [list $value $secure]
}
proc ad_page_contract_handle_datasource_error error {
  # copied from defs-procs.tcl: ad_return_complaint

  doc_return 200 text/html "[ad_header_with_extra_stuff  "Problem with a Templated Page" "" ""]
    
<h2>Problem with a Page (or maybe Your Input)</h2>

<hr>

We had a problem processing your request:
	
<ul>
  <li>$error
</ul>
	
<p>
	
Sorry.
	
[ad_footer]
"
}
proc ad_page_contract_filter_priority filter {
    return [lindex [nsv_get ad_page_contract_filters $filter] 4]
}
proc util_PrettySex {m_or_f {default default}} {
    if { $m_or_f == "M" || $m_or_f == "m" } {
	return "Male"
    } elseif { $m_or_f == "F" || $m_or_f == "f" } {
	return "Female"
    } else {
	# Note that we can't compare default to the empty string as in 
	# many cases, we are going want the default to be the empty
	# string
	if { [string compare $default "default"] == 0 } {
	    return "Unknown (\"$m_or_f\")"
	} else {
	    return $default
	}
    }
}
proc apm_package_id_from_key_mem package_key {
    return [db_string apm_package_id_from_key {
	select package_id from apm_packages where package_key = :package_key
    } -default 0]
}
proc ad_complain args {    ad_complain__arg_parser

    global ad_page_contract_complaints ad_page_contract_errorkeys ad_page_contract_error_string

    # if no key was specified, grab one from the internally kept stack
    if { [empty_string_p $key] && [info exists ad_page_contract_errorkeys] } {
	set key [lindex $ad_page_contract_errorkeys 0]
    }
    if { [info exists ad_page_contract_error_string($key)] } {
	lappend ad_page_contract_complaints $ad_page_contract_error_string($key)
    } elseif { [empty_string_p $message] } {
	lappend ad_page_contract_complaints "Validation \"$key\" complained"
    } else {
	lappend ad_page_contract_complaints $message
    }
}
proc auto_load {cmd {namespace {}}} {
    global auto_index auto_oldpath auto_path

    if {[string length $namespace] == 0} {
	set namespace [uplevel 1 [list ::namespace current]]
    }
    set nameList [auto_qualify $cmd $namespace]
    # workaround non canonical auto_index entries that might be around
    # from older auto_mkindex versions
    lappend nameList $cmd
    foreach name $nameList {
	if {[info exists auto_index($name)]} {
	    uplevel #0 $auto_index($name)
	    return [expr {[info commands $name] != ""}]
	}
    }
    if {![info exists auto_path]} {
	return 0
    }

    if {![auto_load_index]} {
	return 0
    }
    foreach name $nameList {
	if {[info exists auto_index($name)]} {
	    uplevel #0 $auto_index($name)
	    # There's a couple of ways to look for a command of a given
	    # name.  One is to use
	    #    info commands $name
	    # Unfortunately, if the name has glob-magic chars in it like *
	    # or [], it may not match.  For our purposes here, a better
	    # route is to use 
	    #    namespace which -command $name
	    if { ![string equal [namespace which -command $name] ""] } {
		return 1
	    }
	}
    }
    return 0
}
proc apm_bootstrap_load_file {root_directory file} {
    set relative_path [string range $file  [expr { [string length "$root_directory/packages"] + 1 }] end]
    ns_log "Notice" "Loading packages/$relative_path..."
    apm_source $file
    nsv_set apm_library_mtime packages/$relative_path [file mtime $file]
}
proc db_rdbms_get_version rdbms {
    return [lindex $rdbms 1]
}
proc ad_var_type_check_number_p value {
    if [catch {expr 1.0 * $value}] {
        return 0
    } else {
        return 1
    }
}
proc ad_system_owner {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  SystemOwner]
}
proc set_difference {x y} {
    set z {}
    foreach element $x {
        if {[lsearch -exact $y $element] == -1} {
            lappend z $element
        }
    }
    return $z
}
proc db_qd_internal_load_queries {file_pointer file_tag} {
    # While there are surely efficient ways of loading large files,
    # we're going to assume smaller files for now. Plus, this doesn't happen
    # often.

    db_qd_log Debug "Loading $file_tag"

    # Read entire contents
    set whole_file [read $file_pointer]

    # PREPARE THE FILE (ben - this is in case the file needs massaging before parsing)
    set whole_file [db_qd_internal_prepare_queryfile_content $whole_file]

    # Iterate and parse out each query
    set parsing_state [db_qd_internal_parse_init $whole_file $file_tag]
    
    db_qd_log Debug "parsing state - $parsing_state"

    # We need this for queries with relative paths
    set acs_file_path [ad_make_relative_path $file_tag]
    set queryname_root [db_qd_internal_get_queryname_root $acs_file_path]

    db_qd_log Debug "queryname root is $queryname_root"

    while {1} {
	set result [db_qd_internal_parse_one_query $parsing_state]
	
	db_qd_log Debug "one parse result -$result-"

	# If we get the empty string, we are done parsing
	if {$result == ""} {
	    break
	}

	set one_query [lindex $result 0]
	set parsing_state [lindex $result 1]

	db_qd_log Debug "loaded one query - [db_fullquery_get_name $one_query]"

	# Relative Path for the Query
	if {[db_qd_relative_path_p [db_fullquery_get_name $one_query]]} {
	    set new_name [db_qd_make_absolute_path $queryname_root [db_fullquery_get_name $one_query]]

	    set new_fullquery [db_fullquery_create  $new_name  [db_fullquery_get_querytext $one_query]  [db_fullquery_get_bind_vars $one_query]  [db_fullquery_get_query_type $one_query]  [db_fullquery_get_rdbms $one_query]  [db_fullquery_get_load_location $one_query]]

	    set one_query $new_fullquery

	    db_qd_log Debug "relative path, replaced name with $new_name"
	}

	# Store the query
	db_qd_internal_store_cache $one_query
    }

    set relative_path [string range $file_tag  [expr { [string length [acs_root_dir]] + 1 }] end]
    nsv_set apm_library_mtime $relative_path [file mtime $file_tag]
}
proc ad_parameter_all_values_as_list args {    ad_parameter_all_values_as_list__arg_parser
  
    return [join [ad_parameter -package_id $package_id $name $subsection] " "]
}
proc ad_generate_random_string {{length 8}} {
    return [string range [sec_random_token] 0 $length]
}
proc db_html_select_options__arg_parser {} {    upvar args args
    upvar select_option val ; set val {}
    upvar bind val ; set val {}

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -bind {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -bind"
                }
                upvar bind val ; set val [lindex $args [incr i]]
            }
            -select_option {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -select_option"
                }
                upvar select_option val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 2 } {
        return -code error "No value specified for argument [lindex { stmt_name sql } $n_args_remaining]"
    }
    upvar stmt_name val ; set val [lindex $args [expr { $i + 0 }]]
    upvar sql val ; set val [lindex $args [expr { $i + 1 }]]
    if { $n_args_remaining > 2 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_complaints_parse_error_strings errorstrings {
    global ad_page_contract_error_string
    array set ad_page_contract_error_string [list]

    foreach { errorkeys text } $errorstrings {
	foreach errorkey $errorkeys {
	    set errorkeyv [split $errorkey ":"]
	    if { [llength $errorkeyv] > 2 } {
		return -code error "Error name '$error' doesn't have the right format. It must be var\[:flag\]"
	    }
	    set name [lindex $errorkeyv 0]
	    set flags [lindex $errorkeyv 1]
	    if { [empty_string_p $flags] } {
		set ad_page_contract_error_string($name) $text
	    } else {
		foreach flag [split $flags ","] {
		    if { ![empty_string_p $flag] } {
			set ad_page_contract_error_string($name:$flag) $text
		    } else {
			set ad_page_contract_error_string($name) $text
		    }
		}
	    }
	}
    }
}
proc export_entire_form_as_url_vars {{vars_to_passthrough {}}} {
    set params [list]
    set the_form [ns_getform]
    if { ![empty_string_p $the_form] } {
	for {set i 0} {$i<[ns_set size $the_form]} {incr i} {
	    set varname [ns_set key $the_form $i]
	    set varvalue [ns_set value $the_form $i]
	    if {
		$vars_to_passthrough == "" ||
		([lsearch -exact $vars_to_passthrough $varname] != -1)
	    } {
		lappend params "[ns_urlencode $varname]=[ns_urlencode $varvalue]" 
	    }
	}
	return [join $params "&"]
    }
}
proc apm_guess_file_type {package_key path} {
    set components [split $path "/"]
    set dirs_in_pageroot [llength [split [ns_info pageroot] "/"]]	   ;# See comments by RBM

    # Fix to cope with both full and relative paths
    if { [string index $path 0] == "/"} {                          
	set components_lesser [lrange $components $dirs_in_pageroot end] 
    } else {
	set components_lesser $components
    }
    set extension [file extension $path]
    set type ""


    # DRB: someone named a file "acs-mail-create-packages.sql" rather than
    # the conventional "acs-mail-packages-create.sql", causing it to be
    # recognized as a data_model_create file, causing it to be explicitly
    # run by the installer (the author intended it to be included by
    # acs-mail-create.sql only).  I've tightened up the regexp below to
    # avoid this problem, along with renaming the file...

    if { [string equal $extension ".sql"] } {
	if { [lsearch -glob $components "*upgrade-*-*"] >= 0 } {
	    set type "data_model_upgrade"
	} elseif { [regexp -- "$package_key-(create|drop)\.sql" [file tail $path] "" kind] } {
	    set type "data_model_$kind"
	} else {
	    set type "data_model"
	}
    } elseif { [string equal $extension ".sqlj"] } {
	set type "sqlj_code"
    } elseif { [string equal $extension ".info"] } {
	set type "package_spec"
    } elseif { [string equal $extension ".xql"] } {
	set type "query_file"
    } elseif { [string equal $extension ".java"] } {
	set type "java_code"
    } elseif { [string equal $extension ".jar"] } {
	set type "java_archive"
    } elseif { [lsearch $components "doc"] >= 0 } {
	set type "documentation"
    } elseif { [string equal $extension ".pl"] ||  [string equal $extension ".sh"] ||  [lsearch $components "bin"] >= 0 } {
	set type "shell"
    } elseif { [lsearch $components "templates"] >= 0 } {
	set type "template"
    } elseif { [llength $components] == 1 &&  ([string equal $extension ".html"] || [string equal $extension ".adp"]) } {
		# HTML or ADP file in the top level of a package - assume it's documentation.
	set type "documentation"

        # RBM: Changed the next elseif to check for 'www' or 'admin-www' only n levels down
        # the path, since that'd be the minimum in a path counting from the pageroot

    } elseif { [lsearch $components_lesser "www"] >= 0 || [lsearch $components_lesser "admin-www"] >= 0 } {
	set type "content_page"
    } else {
	if { [string equal $extension ".tcl"] } {
            if { [regexp -- {-(procs|init)(-[0-9a-zA-Z]*)?\.tcl$} [file tail $path] "" kind] } {
	        set type "tcl_$kind"
            } else {
                set type "tcl_util"
	    }
	}
    }
    return $type
}
proc export_form_vars__arg_parser {} {    upvar args args
    upvar sign_p val ; set val 0

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sign - -sign=1 {
                uplevel set sign_p 1
            }
            -sign=0 {
                uplevel set sign_p 0
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 0 } {
        return -code error "No value specified for argument [lindex {  } $n_args_remaining]"
    }
    set args [lrange $args [expr { $i + 0 }] end]
}
proc rp_performance_mode {} {
    return 0
  }
proc ad_register_proc__arg_parser {} {    upvar args args
    upvar description val ; set val {}
    upvar noinherit val ; set val f
    upvar sitewide_p val ; set val 0
    upvar arg val ; set val {}
    upvar debug val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -sitewide - -sitewide=1 {
                uplevel set sitewide_p 1
            }
            -sitewide=0 {
                uplevel set sitewide_p 0
            }
            -debug {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -debug"
                }
                upvar debug val ; set val [lindex $args [incr i]]
            }
            -noinherit {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -noinherit"
                }
                upvar noinherit val ; set val [lindex $args [incr i]]
            }
            -description {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -description"
                }
                upvar description val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 3 } {
        return -code error "No value specified for argument [lindex { method path proc } $n_args_remaining]"
    }
    upvar method val ; set val [lindex $args [expr { $i + 0 }]]
    upvar path val ; set val [lindex $args [expr { $i + 1 }]]
    upvar proc val ; set val [lindex $args [expr { $i + 2 }]]
    if { $n_args_remaining > 3 } {
        upvar arg val ; set val [lindex $args [expr { $i + 3 }]]
    }
    if { $n_args_remaining > 4 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc get_server_root {} {
    file dir [ns_info tcllib]
}
proc apm_guess_db_type {package_key path} {
    set components [split $path "/"]
  
    if { [string match "data_model*" [apm_guess_file_type $package_key $path]] } {
        set sql_index [lsearch $components "sql"]
        if { $sql_index >= 0 } {
            set db_dir [lindex $components [expr $sql_index + 1]]
            foreach known_database_type [db_known_database_types] {
                if { [string equal [lindex $known_database_type 0] $db_dir] } {
                    return $db_dir
                }
            }
        }
        return "oracle"
    }

    set file_name [file tail $path]
    foreach known_database_type [nsv_get ad_known_database_types .] {
        if { [string match "*-[lindex $known_database_type 0]\.*" $file_name] } {
            return [lindex $known_database_type 0]
        }
    }

    return ""
}
proc site_node url {

  # Try the URL as is.
  if {[catch {nsv_get site_nodes $url} result] == 0} {
    return $result
  }

  # Add a trailing slash and try again.
  if {[string index $url end] != "/"} {
    append url "/"
    if {[catch {nsv_get site_nodes $url} result] == 0} {
      return $result
    }
  }

  # Try successively shorter prefixes.
  while {$url != ""} {
    # Chop off last component and try again.
    set url [string trimright $url /]
    set url [string range $url 0 [string last / $url]]
    
    if {[catch {nsv_get site_nodes $url} result] == 0} {
      array set node $result
      if {$node(pattern_p) == "t" && $node(object_id) != ""} {
	return $result
      }
    }
  }

  error "site node not found"
}
proc ns_unregister_proc {type path args} { 
    set procname ""
    if { [info exists args] } {
	set procname [lindex $args 0]
    }
    ns_log Debug "ns_unregister_proc called with TYPE: $type PATH: $path PROCNAM: $procname  "
}
proc db_blob_get {statement_name sql args} {
    ad_arg_parser { bind } $args

    set full_statement_name [db_qd_get_fullname $statement_name]

    db_with_handle db { 
	set data [db_exec_lob blob_get $db $full_statement_name $sql]
    }

    return $data
}
proc do_dml_transactions dml_stmt_list {
    db_transaction {
	foreach dml_stmt $dml_stmt_list {
	    if { [catch {db_dml $dml_stmt} errmsg] } {
		db_abort_transaction
		return $errmsg
	    }
	}
    }
    return ""
}
proc ad_dbclick_check_dml args {    ad_dbclick_check_dml__arg_parser

    if [catch {
	if { ![empty_string_p $bind] } {
	    	db_dml $statement_name $insert_dml -bind $bind
	} else {
	    db_dml $statement_name $insert_dml 
	}
    } errmsg] {
	# Oracle choked on the insert
	
	# detect double click
        if {
	    [db_0or1row double_click_check "
		
		select 1 as one
		from $table_name
		where $id_column_name = :generated_id
		
	    " -bind [ad_tcl_vars_to_ns_set generated_id]]
	} {
	    ad_returnredirect $return_url
	    return
	}
	
	ns_log Error "[info script] choked. Oracle returned error:  $errmsg"

	ad_return_error "Error in insert" "
	We were unable to do your insert in the database. 
	Here is the error that was returned:
	<p>
	<blockquote>
	<pre>
	$errmsg
	</pre>
	</blockquote>"
	return
    }

    ad_returnredirect $return_url
    return
}
proc db_fullquery_get_name fullquery {
    return [lindex $fullquery 0]
}
proc ns_localsqltimestamp {} {
    return [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]
}
proc ad_var_type_check_integer_p value {

    if [regexp {[^0-9]} $value] {
        return 0
    } else {
        return 1
    }
}
proc db_qd_internal_load_cache file_path {
    # First we actually need to flush queries that are associated with that file tag
    # in case they are not all replaced by reloading that file. That is nasty! Oh well.

    # We'll do this later
    
    # we just reparse the file
    set stream [open $file_path "r"]
    db_qd_internal_load_queries $stream $file_path
    close $stream
}
proc db_fullquery_create {queryname querytext bind_vars_lst query_type rdbms load_location} {
    return [list $queryname $querytext $bind_vars_lst $query_type $rdbms $load_location]
}
proc ns_register_filter {stage http_method match name args} { 
    
    ns_log notice "ns_register_filter: registering $stage $name for $http_method all $match \n\n"
    
}
proc ad_ns_set_keys args {    ad_ns_set_keys__arg_parser

    set keys [list]
    set size [ns_set size $set_id]
    for { set i 0 } { $i < $size } { incr i } {
	set key [ns_set key $set_id $i]
	if { [lsearch -exact $exclude $key] == -1 } {
	    if { $colon_p } { 
		lappend keys ":$key"
	    } else {
		lappend keys $key
	    }
	}
    }
    return $keys
}
proc ns_dbquotename name {
    if {[string first " " $name] != -1} {
        return "\"$name\""
    } else {
        return $name
    }
}
proc last_day_of_week args {
            ::nstcl::_ad_proc_parser ::nstcl::last_day_of_week [set args]
            
    set time   [clock scan $date]
    set dow    [clock format $time -format %w]
    set adjust [expr {6 - $dow}]
    set time   [clock scan "$adjust days" -base $time]
    return     [clock format $time -format $format]
}
proc ad_restrict_to_https {conn args why} {
    if { [ad_conn driver] == "nsssl" ||
         [ad_conn driver] == "nsssle" ||
	 [ad_conn driver] == "nsopenssl"} {
 	return "filter_ok"
    } 

    set http_port [ns_config -int "ns/server/[ns_info server]/module/nssock" Port 80]
    if { [ns_config ns/server/[ns_info server]/modules nsssl] != "" } {
    set ssl_port [ns_config -int "ns/server/[ns_info server]/module/nsssl" Port 443]
    } elseif { [ns_config ns/server/[ns_info server]/modules nsopenssl] != "" } {
	set ssl_port [ns_config -int "ns/server/[ns_info server]/module/nsopenssl" ServerPort 443]
    } elseif { [ns_config ns/server/[ns_info server]/modules nsssle] != "" } {
    set ssl_port [ns_config -int "ns/server/[ns_info server]/module/nsssle" Port 443]
    }
    
    set host [ns_set iget [ad_conn headers] "host"]
    if { [regexp {^(.*?):(.*)$} $host match host port] == 0 || [string compare $port $http_port] == 0 } {
	set url [ad_conn url]

        # ArsDigita probably meant to pass along any form variables (I hope)...
        set form [ns_getform]

        if {$form != ""} {
            set size [ns_set size $form]
            set url_args [list]
 
            for {set i 0} {$i < $size} {incr i} {
                set key [ns_set key $form $i]
                set val [ns_set value $form $i]
                lappend url_args [ns_urlencode $key]=[ns_urlencode $val]
            }
 
           append url "?[join $url_args &]"
        }
        

	if { $ssl_port == 443 } {
	    set redir "https://$host$url"
	} else {
	    set redir "https://$host:$ssl_port$url"
	}
	ad_returnredirect $redir
    } else {
	ad_return_forbidden "Please use HTTPS" "Sorry, you must use HTTPS to access this page."
    }
    
    return "filter_return"
}
proc ad_maybe_redirect_for_registration {} {
    set user_id [ad_conn user_id]
    if { $user_id != 0 } {
	# user is in fact logged in, terminate
	return $user_id
    }
    ad_redirect_for_registration
    ad_script_abort
}
proc ad_column_type {table_name column_name} {

    set column_type [db_column_type $table_name $column_name]

    if { $column_type == -1 } {
	return "Either table $table_name doesn't exist or column $column_name doesn't exist"
    } elseif { [string compare $column_type "NUMBER"] } {
	return "numeric"
    } else {
	return "text"
    }
}
proc ad_sign args {    ad_sign__arg_parser

    # pick a random token_id
    if { [empty_string_p $secret] } {
	set token_id [sec_get_random_cached_token_id]
	set secret_token [sec_get_token $token_id]
    } else {
	set secret_token $secret
    }

    ns_log Debug "Security: Getting token_id $token_id, value $secret_token"

    if { $max_age == "" } {
	set expire_time 0
    } else {
	set expire_time [expr $max_age + [ns_time]]
    }

    set hash [ns_sha1 "$value$token_id$expire_time$secret_token"]

    set signature [list $token_id $expire_time $hash]

    return $signature
}
proc nsv_exists {id key} { 
    global $id
    return [info exists ${id}($key)]

}
proc ns_normalizepath path { 
    ns_log notice " ns_normalizepath called with $path "
    return $path

    #    return [file normalize $path] # nb: this works in 8.4
}
proc db_rdbms_create {type version} {
    return [list $type $version]
}
proc util_current_location {} {
   set host_from_header [ns_set iget [ad_conn headers] Host]
   # host_from_header now hopefully contains hostname[:port]
   set location_from_config_file [ad_conn location]
   if {[empty_string_p $host_from_header]} {
      # Hmm, there is no Host header.  This must be
      # an old browser such as MSIE 3.  All we can do is:
      return $location_from_config_file
   } else {
      # Replace the hostname[:port] part of $location_from_config_file with $host_from_header:
      regsub -nocase {(^[a-z]+://).*}  $location_from_config_file \\1$host_from_header location_from_host_header
      return $location_from_host_header
   }
}
proc apm_dependency_add args {    apm_dependency_add__arg_parser


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
proc ad_complaints_count {} {
    global ad_page_contract_complaints
    return [llength $ad_page_contract_complaints]
}
proc ad_site_home_link {} {
    if { [ad_get_user_id] != 0 } {
	return "<a href=\"[ad_pvt_home]\">[ad_system_name]</a>"
    } else {
	# we don't know who this person is
	return "<a href=\"/\">[ad_system_name]</a>"
    }
}
proc server_cluster_all_hosts {} {
    if { ![server_cluster_enabled_p] } {
	return [list]
    }
    return [ad_parameter -package_id [ad_acs_kernel_id] ClusterPeerIP server-cluster]
}
proc ad_return_exception_template__arg_parser {} {    upvar args args
    upvar params val ; set val {}
    upvar status val ; set val 500

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -status {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -status"
                }
                upvar status val ; set val [lindex $args [incr i]]
            }
            -params {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -params"
                }
                upvar params val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { template } $n_args_remaining]"
    }
    upvar template val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc doc_find_template filename {
    set path_root [acs_root_dir]

    set start [clock clicks]

    set dir [file dirname $filename]
    while { [string length $dir] > 1 && [string first $path_root $dir] == 0 } {
	# Only look in directories under the path root.
	if { [file isfile "$dir/master.adp"] } {
	    return "$dir/master.adp"
	}
	set dir [file dirname $dir]
    }

    if { [file exists "$path_root/templates/master.adp"] } {
	return "$path_root/templates/master.adp"
    }

    # Uhoh. Nada!
    return ""
}
proc ns_dbquotevalue {value {type text}} {
    if {[string length $value] == 0} {
        return NULL
    }

    if {[lsearch -exact {decimal double integer int real smallint
                         bigint bit float numeric tinyint} $type] != -1} {
        return $value
    }

    regsub -all -- "'" $value "''" value
    return '$value'
}
proc ad_page_contract_filter_proc_integer {name value_varname} {upvar $value_varname value

    if { ![regexp {^(-)?(0*)(([1-9][0-9]*|0))$} $value match sign zeros value] } {
	ad_complain "$name is not an integer"
	return 0
    }
    # Trim the value for any leading zeros
    set value $sign$value
    return 1
}
proc ad_user_login args {    ad_user_login__arg_parser

    set prev_user_id [ad_conn user_id]

    # deal with the permanent login cookies (ad_user_login and ad_user_login_secure)
    if { $forever_p } {
	# permanent login
	if { [ad_secure_conn_p] } {
	    ad_set_signed_cookie -max_age inf -secure t ad_user_login_secure "$user_id,[ns_time]"
	    ad_set_signed_cookie -max_age inf -secure f ad_user_login "$user_id"
	} else {
	    ad_set_signed_cookie -max_age inf -secure f ad_user_login "$user_id"
	    # Hose the secure permanent login token if this user is different 
	    # from the previous one.
	    if { $prev_user_id != $user_id } {
		ad_set_cookie -max_age 0 ad_user_login_secure ""
	    }
	}
    } elseif { $prev_user_id == $user_id && [ad_secure_conn_p] } {
	# nonpermanent secure login requested
	ad_set_cookie -max_age 0 ad_user_login_secure ""
    } else {
	ad_set_cookie -max_age 0 ad_user_login ""
	ad_set_cookie -max_age 0 ad_user_login_secure ""
    }

    # deal with the current session
    sec_setup_session $user_id
}
proc ad_page_contract_filter_proc_nohtml {name value_varname} {upvar $value_varname value

    if { [string first < $value] >= 0 } {
	ad_complain "Value for $name contains HTML tags"
	return 0
    }
    return 1
}
proc apm_version_loaded_p version_id {
    return [nsv_exists apm_version_init_loaded_p $version_id]
}
proc util_ReturnMetaRefresh {url {seconds_delay 0}} {
    ReturnHeaders
    ns_write "
    <head>
    <META HTTP-EQUIV=\"REFRESH\" CONTENT=\"$seconds_delay;URL=$url\">
    </head>
    <body>
    If your browser does not automatically redirect you, please go <a href=$url>here</a>.
    </body>"
}
proc ad_convert_to_html__arg_parser {} {    upvar args args
    upvar html_p val ; set val f

    for { set i 0 } { $i < [llength $args] } { incr i } {
        set arg [lindex $args $i]
        if { ![ad_proc_valid_switch_p $arg] } {
            break
        }
        if { [string equal $arg "--"] } {
            incr i
            break
        }
        switch -- $arg {
            -html_p {
                if { $i >= [llength $args] - 1 } {
                    return -code error "No argument to switch -html_p"
                }
                upvar html_p val ; set val [lindex $args [incr i]]
            }

            default { return -code error "Invalid switch: \"$arg\"" }
        }
    }

    set n_args_remaining [expr { [llength $args] - $i }]
    if { $n_args_remaining < 1 } {
        return -code error "No value specified for argument [lindex { text } $n_args_remaining]"
    }
    upvar text val ; set val [lindex $args [expr { $i + 0 }]]
    if { $n_args_remaining > 1 } {
        return -code error "Too many positional parameters specified"
    }
    unset args
}
proc ad_graphics_site_available_p {} {
    return [ad_parameter -package_id [ad_acs_kernel_id]  GraphicsSiteAvailableP]
}
proc set_intersection! {set_name set} {
    upvar 1 $set_name list
    set result {}
    foreach element $list {
        if {[lsearch -exact $set $element] != -1} {
            lappend result $element
        }
    }
    return [set list $result]
}
proc db_with_handle_brief {db code_block} {
    set doc { Places a usable database handle in $db and executes $code_block. } 
    upvar 1 $db dbh

    set dbh [ns_db gethandle]

    set errno [catch { uplevel 1 $code_block } error]

#    ns_db bouncepool $dbh
   ns_db releasehandle $dbh
#    if { [info exists dbh] } {
#	ns_db releasehandle $dbh
#    }


    # If errno is 1, it's an error, so return errorCode and errorInfo;
    # if errno = 2, it's a return, so don't try to return errorCode/errorInfo
    # errno = 3 or 4 give undefined results
    
    if { $errno == 1 } {
	
	# A real error occurred
	global errorInfo errorCode
	return -code $errno -errorcode $errorCode -errorinfo $errorInfo $error
    }
    
    if { $errno == 2 } {
	
	# The code block called a "return", so pass the message through but don't try
	# to return errorCode or errorInfo since they may not exist
	
	return -code $errno $error
    }
}
proc ad_dateentrywidget {column {default_date 1940-11-03}} {
    ns_share NS

    set output "<SELECT name=$column.month>\n"
    for {set i 0} {$i < 12} {incr i} {
	append output "<OPTION> [lindex $NS(months) $i]\n"
    }

    append output  "</SELECT>&nbsp;<INPUT NAME=$column.day TYPE=text SIZE=3 MAXLENGTH=2>&nbsp;<INPUT NAME=$column.year TYPE=text SIZE=5 MAXLENGTH=4>"

    return [ns_dbformvalueput $output $column date $default_date]
}
proc db_get_dbhost {} {

    set pool [lindex [nsv_get db_available_pools .] 0]
    set datasource [ns_config ns/db/pool/$pool DataSource]    
    set first_colon_pos [string first ":" $datasource]
    if { $first_colon_pos == -1 } {
        ns_log Error "datasource contains no \":\"? datasource = $datasource"
        return ""
    }
    return [string range $datasource 0 [expr $first_colon_pos - 1]]
}
proc db_tables args {    db_tables__arg_parser

    set tables [list]
    
    if { [info exists pattern] } {
	db_foreach table_names_with_pattern {
	    select relname
	    from pg_class
	    where relname like lower(:pattern) and
                relname !~ '^pg_' and relkind = 'r'
	} {
	    lappend tables $relname
	}
    } else {
	db_foreach table_names_without_pattern {
	    select relname
	    from pg_class
	    where relname !~ '^pg_' and relkind = 'r'
	} {
	    lappend tables $relname
	}
    }
    return $tables
}
proc util_ns_set_to_list args {    util_ns_set_to_list__arg_parser

    set result [list]

    for {set i 0} {$i < [ns_set size $set]} {incr i} {
        lappend result [ns_set key $set $i]
        lappend result [ns_set value $set $i]
    }

    return $result
}
proc sec_setup_session new_user_id {
    set session_id [ad_conn session_id]

    # figure out the session id, if we don't already have it
    if { [empty_string_p $session_id]} {

	# ns_log Notice "OACS= empty session_id"

	set session_id [sec_allocate_session]
        # if we have a user on an newly allocated session, update
        # users table

	# ns_log Notice "OACS= newly allocated session $session_id"

        if { $new_user_id != 0 } {
	    # ns_log Notice "OACS= about to update user session info, user_id NONZERO"
            sec_update_user_session_info $new_user_id
	    # ns_log Notice "OACS= done updating user session info, user_id NONZERO"
        }
    } else {
        # $session_id is an active verified session
        # this call to sec_setup_session is either a user logging in
        # on an active unidentified session, or a change in identity
        # for a browser that is already logged in

        # this is an active session [ad_conn user_id] will not return
        # the empty string
        set prev_user_id [ad_conn user_id]

        if { $prev_user_id != 0 && $prev_user_id != $new_user_id } {
            # this is a change in identity so we should create
            # a new session so session-level data is not shared
            set session_id [sec_allocate_session]
        }

        if { $prev_user_id != $new_user_id } {
            # a change of user_id on an active session
            # demands an update of the users table
            sec_update_user_session_info $new_user_id
        }
    }

    # su, set the session_id global var, and then generate the cookie
    ad_conn -set user_id $new_user_id
    ad_conn -set session_id $session_id
    
    # ns_log Notice "OACS= about to generate session id cookie"

    sec_generate_session_id_cookie

    # ns_log Notice "OACS= done generating session id cookie"

    if { [ad_secure_conn_p] } {
        # this is a secure session, so the browser needs
        # a cookie marking it as such
	sec_generate_secure_token_cookie
    }
}
proc page_validation args {
    if { [info exists {%%exception_list}] } {
	error "Something's wrong"
    }
    # have to put this in the caller's frame, so that sub_page_validation can see it
    # that's because the "uplevel" used to evaluate the code blocks hides this frame
    upvar {%%exception_list} {%%exception_list}
    set {%%exception_list} [list]
    foreach validation_block $args {
	if { [catch {uplevel $validation_block} errmsg] } {
	    lappend {%%exception_list} $errmsg
	}
    }
    set exception_list ${%%exception_list}
    unset {%%exception_list}
    set n_exceptions [llength $exception_list]
    if { $n_exceptions != 0 } {
	set complain_proc [ad_parameter ComplainProc "" ad_return_complaint]
	if { $n_exceptions == 1 } {
	    $complain_proc $n_exceptions [lindex $exception_list 0]
	} else {
	    $complain_proc $n_exceptions "<li>[join $exception_list "\n<li>"]\n"
	}
	return -code return
    }
}

#the beginnings of namespace definition
namespace eval ::pnsd { 
    variable root    ; # root folder of openacs installation
    variable querystring ""    ; # querystring of incoming request
    variable url ""            ; # url of incoming request
    variable __http_stream ""   ; # will contain html of page as it's being built
    variable redirect_p "f"
    variable http_done_p "f"
    variable home ""    ; # pnsd root folder
    variable invariants ; # array of procs we don't want to have redefined
    variable error 



    
    set home [file dirname [info script]]; 

    set ::pnsd::root [file join $home openacs-4]

# Warning - this should be set from config file.
    set root [file join [lindex [file split [pwd]] 0 ] temp/openacs-4]
#    set root x:/temp/openacs-4 ; #NUKEME


#JJS: This is a hack for needed now bc openacs allows empty src attributes in master tags, but nstcl does not
    set ::nstcl::template::default_master [file join $::pnsd::root www/default-master]



# #OpenACS allows empty master tags,  but nstcl does not
#     set ::nstcl::template::default_master [file join $root ./www/default-master]


    variable log_stream ""
    set ::pnsd::log_stream [open [file join $::pnsd::home log.txt] w ]

#makes emacs happy,  but comment it out if you read logs in notepad
    fconfigure $::pnsd::log_stream -buffering none -translation lf 

    # Set up output Headers
    variable output_headers_id
    set output_headers_id [ns_set create outputheaders]
    
    # Set up headers ... 
    variable headers_id
    set headers_id [ns_set create headers]
    ns_set update $headers_id Host [info host]
    ns_set update $headers_id Referer ""


}



# NSTCL PROCS THAT CAN NOT BE REDEFINED
array set ::pnsd::invariants {}

# TOGGLE LOADING OF XQL FILE FROM CACHE

#set LOAD_XQL_FROM_CACHE 0    ; #  parse xml files and build xql ns-set
set LOAD_XQL_FROM_CACHE 1    ; #  don't parse xml files -> just load queries from single file

if { $LOAD_XQL_FROM_CACHE  } {

    proc db_qd_load_query_file { args } {
	# no-op
    }

    set ::pnsd::invariants(db_qd_load_query_file) 1

}




# THIS WILL MAKE SURE CACHE IS CREATED CORRECTLY.

persistentArray OACS_FULLQUERIES [file join $::pnsd::home xql.dat]
persistentArray OACS_PROCS [file join $::pnsd::home procs.dat]
