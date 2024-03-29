ad_library {

    parameter procs

    @author yon (yon@openforce.net)
    @creation-date May 12, 2002
    @cvs-id $Id: parameter-procs.tcl,v 1.6.2.1 2003/06/05 16:54:58 lars Exp $

}

namespace eval parameter {

    ad_proc -public set_value {
        {-package_id ""}
        {-parameter:required}
        {-value:required}
    } {
        set a parameter

        @param package_id what package to set the parameter in. defaults to
                          [ad_conn package_id]
        @param parameter which parameter's value to set
        @param value what value to set said parameter to
    } {
        if {[empty_string_p $package_id]} {
            ::set package_id [ad_requested_object_id]
        }

        db_exec_plsql set_parameter_value {}

        return [ad_parameter_cache -set $value $package_id $parameter]
    }

    ad_proc -public get {
        {-package_id ""}
        {-parameter:required}
        {-default ""}
    } {
        get a parameter

        @param package_id what package to get the parameter from. defaults to
                          [ad_conn package_id]
        @param parameter which parameter's value to get
        @param default what to return if we don't find a value. Defaults to returning the empty string.
    } {

        if {[empty_string_p $package_id]} {
            ::set package_id [ad_requested_object_id]
        }

	::set package_key ""
	::set value ""
	if {![empty_string_p $package_id]} {
	    # This can fail at server startup--OpenACS calls parameter::get to
	    # get the size of the util_memoize cache so it can setup the cache.
	    # apm_package_key_from_id needs that cache, but on server start 
	    # when the toolkit tries to get the parameter for the cache size
	    # the cache doesn't exist yet, so apm_package_key_from_id fails
	    catch {
		::set package_key [apm_package_key_from_id $package_id]
	    }
	}

	# If I convert the package_id to a package_key, is there a parameter by this
	# name in the parameter file?  If so, it takes precedence.
	# 1. use the parameter file
	if {![empty_string_p $package_key]} {
	    ::set value [ad_parameter_from_file $parameter $package_key]
	}

        # 2. check the parameter cache
        if {[empty_string_p $value]} {
	    ::set value [ad_parameter_cache $package_id $parameter]
	}
        # 3. use the default value
        if {[empty_string_p $value]} {
            ::set value $default
        }


        return $value
    }

    ad_proc -public set_from_package_key {
        {-package_key:required}
        {-parameter:required}
        {-value:required}
    } {
        set_value \
            -package_id [apm_package_id_from_key $package_key] \
            -parameter $parameter \
            -value $value
    }

    ad_proc -public get_from_package_key {
        {-package_key:required}
        {-parameter:required}
        {-default ""}
    } {
        get a parameter

        @param package_key what package to get the parameter from. we will try
                           to get the package_id from the package_key. this
                           may cause an error if there are more than one
                           instance of this package
        @param parameter which parameter's value to get
        @param default what to return if we don't find a value
    } {
        # 1. check to see if this parameter is being set in the server's
        # configuration file; this value has highest precedence
        ::set value [ad_parameter_from_file $parameter $package_key]

        # 2. try to get a package_id for this package_key and use the standard
        # parameter::get function to get the value
        if {[empty_string_p $value]} {
            with_catch errmsg {
                ::set value [get \
                    -package_id [apm_package_id_from_key $package_key] \
                    -parameter $parameter \
                    -default $default \
                ]
            } {
                ::set value $default
            }
        }

        return $value
    }

}
