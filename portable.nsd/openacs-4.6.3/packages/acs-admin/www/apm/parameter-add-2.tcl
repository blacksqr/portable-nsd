ad_page_contract {
    Adds a parameter to a version.
    @author Todd Nightingale (tnight@arsdigita.com)
    @author Bryan Quinn (bquinn@arsdigita.com)
    @creation-date 10 September 2000
    @cvs-id $Id: parameter-add-2.tcl,v 1.3 2002/09/10 22:21:59 jeffd Exp $
} {
    version_id:naturalnum,notnull
    parameter_id:naturalnum,notnull
    package_key:notnull
    parameter_name:notnull
    section_name
    description:notnull,nohtml
    datatype:notnull
    {default_value [db_null]}
    {min_n_values:integer 1}
    {max_n_values:integer 1}
} -validate {
    datatype_type_ck {
	if {$datatype != "number" && $datatype != "string"} {
	    ad_complain
	}
    }
    param_name_unique_ck {
	if {[db_string param_name_unique_ck {
	    select decode(count(*), 0, 0, 1) 
	    from apm_parameters
	    where parameter_name = :parameter_name
            and package_key= :package_key
	}]} {
	    ad_complain "The parameter name $parameter_name already exists for this package"	    
	}
    }
} -errors {
    datatype_type_ck {The datatype must be either a number or a string.}
}

db_transaction {
    apm_parameter_register -parameter_id $parameter_id $parameter_name $description $package_key \
	$default_value $datatype $section_name $min_n_values $max_n_values
    apm_generate_package_spec $version_id
} on_error {
    if {![db_string apm_parameter_register_doubleclick_p {
	select 1 from apm_parameters where parameter_id = :parameter_id
    } -default 0]} {
	ad_return_error "Database Error" "The database is complaining about the parameter you entered:<p>
<blockquote><pre>[ad_quotehtml $errmsg]</pre></blockquote>"
	ad_script_abort
    }
}

ad_returnredirect version-parameters?[export_url_vars version_id]
