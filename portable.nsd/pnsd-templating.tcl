# Templating routines defined in OpenACS that I need to redefine for pnsd
# John Sequeira
# johnseq@pobox.com


proc template::adp_init { type file_stub } {
 
  # this will return the name of the proc if it exists
  set proc_name [info procs ::template::mtimes::${type}::$file_stub]

  set pkg_id [apm_package_id_from_key acs-templating]
  set refresh_cache [ad_parameter -package_id $pkg_id RefreshCache dummy\
			 "when needed"]

  if {[string equal $proc_name {}] || [string compare $refresh_cache "never"]} {
    set mtime [file mtime $file_stub.$type]
    if {[string equal $proc_name {}] || $mtime != [$proc_name]
	|| [string equal $refresh_cache "always"]} {

      # either the procedure does not already exist or is not up-to-date

      switch -exact $type {

	tcl {
	  set code [template::util::read_file $file_stub.tcl]
	}
	default { # works for adp and wdp

	    #jjs: added base-level argument
	    set code "[adp_compile -file $file_stub.$type] 1"
	}
      }

      # wrap the code for both types of files within an uplevel in
      # the declared procedure, so that data sources are set in the 
      # same frame as the code that outputs the template.
      set proc_name ::template::code::${type}::$file_stub

      proc $proc_name {} "
\#        lappend ::template::parse_level \[expr \[info level]-1]
        ns_log notice \"$proc_name - level \$::template::parse_level\"
    	uplevel {
    	  $code
    	}
      "
      proc ::template::mtimes::${type}::$file_stub {} "return $mtime"
    }
  }

  ns_log debug "template::adp_init returning $proc_name "
#  puts "proc $proc_name {}  { [info body $proc_name] } "

  return $proc_name

}

proc ns_adp_parse { args }  { 

    set block1 [ subst { ::template::adp_compile $args }]

    ns_log debug "Compiling ... $block1"
    set ccmd  [ uplevel 1  $block1 ]   
#    return "<pre>[    info body $ccmd ]</pre>"
    
    #set block2 [ subst { ::template::adp_eval $ccmd  } ]
    ns_log debug "ns_adp_parse evaling $ccmd "

    set block2 [ subst { ::template::adp_eval $ccmd -inline } ]
    return [uplevel 1 $block2 ]
 
#    return "$ccmd"

}


proc  template::adp_prepare {} {

  uplevel {

    if { [file exists $__adp_stub.tcl] } {

	# ensure that data source preparation procedure exists and is up-to-date
	template::adp_init tcl $__adp_stub

	# remember the file_stub in case the procedure changes it
	set __adp_remember_stub $__adp_stub

	# execute data source preparation procedure
	::template::code::tcl::$__adp_stub
	ns_log debug "Executed data procedure"
#	eval $data_proc

	# propagate aborting
	global request_aborted
	if [info exists request_aborted] {
	    ns_log warning "propagating abortion from $__adp_remember_stub.tcl\
          (status [lindex $request_aborted 0]): '[lindex $request_aborted 1]')"
	    template::adp_abort
	}
	
	# if the file has changed than prepare again
	if { ! [string equal $__adp_stub $__adp_remember_stub] } {
	    template::adp_prepare;			# propagate result up
	} { return 1 }
    }
    return 0
}
}


# @public adp_abort

# Terminates processing of a template and throws away all output.


proc template::adp_abort {} { error ADP_ABORT }

# This proc gets run by each compiled template... it will initialize the keys of the __page_contract_property array
# It doesn't follow good design by using uplevel like this... i.e. we could collide with __name.
proc template::init_properties {} {
    upvar 1 __page_contract_property __page_contract_property
#    catch { ns_log Debug "initing properties [string range  [info level -1] 0 30]" }

    if {[info exists __page_contract_property]} {
	#	    ns_log Debug "properties exist"
	foreach __name [array names __page_contract_property] { 
	    ns_log Debug "setting '$__name' $__page_contract_property($__name)"

	    #node is used by templating...
	    upvar 1 $__name var
	    if {![info exists var]  && ![array exists var]} { 		
		set var ""

		#trace variable $name w monitor
	    }     
	}
	
    } else {
	ns_log warn "template::init_properties: __couldn't find __page_contract_property"
    }

}


#Overload this command from acs-templating
proc adp_parse_ad_conn_file {} {
#    handle a request for an adp and/or tcl file in the template system.

    set __adp_stub [file root [ad_conn file]]

    namespace eval template variable parse_level "" ;

    ns_log Debug "YYY(adp_parse_ad_conn_file): [ad_conn file]\nExists?[file exists [ad_conn file]] Root:[file root [ad_conn file]] "

    ad_conn -set subsite_id [site_node_closest_ancestor_package "acs-subsite"]
    
#    set src [ad_parameter -package_id [ad_conn subsite_id] DefaultMaster dummy "/www/default-master"]
#    set ::nstcl::template::default_master [template::util::url_to_file $src [ad_conn url]]
#    set ::nstcl::template::default_master [file join [ns_info "pageroot"] default-master]

    
#  set mime_type [get_mime_type]
#  set template_extension [get_mime_template_extension $mime_type]



    #set up tcl data sources in this stack frame if a tcl file exists
    if { [catch { template::adp_prepare } errMsg] } {
	ns_log error $errMsg
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

	ns_log Debug "Parsing template ..." 	

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

#jjs: not used
proc template::adp_parse { __adp_stub } {
    ns_log error "template::adp_parse called --> it should be deprecated for pnsd"
    template::adp_init adp $__adp_stub
    set parsed_template [ ns_adp_parse -file $__adp_stub.adp ] 
    
    return parsed_template

}

#hacked ad_table
::nstcl::ad_proc ad_table { 
    {-Torder_target_url "" }
    {-Torderby ""}
    {-Tasc_order_img {^}}
    {-Tdesc_order_img {v}}
    {-Tmissing_text "<em>No data found.</em>"}
    {-Tsuffix ""}
    {-Tcolumns ""}
    {-Taudit ""}
    {-Trows_per_band 1}
    {-Tband_colors {{} {"#ececec"}}}
    {-Trows_per_page 0}
    {-Tmax_rows 0}
    {-Ttable_extra_html ""}
    {-Theader_row_extra {bgcolor="#f8f8f8"}}
    {-Ttable_break_html "<p>"}
    {-Tpre_row_code ""}
    {-Trow_code {[subst $Trow_default]}}
    {-Tpost_data_ns_sets ""}
    {-Textra_vars ""}
    {-Textra_rows ""}
    {-bind ""}    
    statement_name sql_qry Tdatadef
} {
    Note: all the variables in this function are named Tblah since we could potentially 
    have namespace collisions
    <p>
    build and return an html fragment given an active query and a data definition.
    <ul> 
    <li> sql_qry -- The query that should be executed to generate the table. <br> 
    You can specify an optional -bind argument to specify a ns_set of bind variables.
    <li> Tdatadef -- the table declaration.
    </ul>

    Datadef structure :
    <pre> 
    { 
        {column_id "Column_Heading" order_clause display_info}
        ...
    }
    </pre>
    <ul>
    <li> column_id -- what to set as orderby for sorting and also is 
         the default variable for the table cell.

    <li> the text for the heading to be wrapped in &lt;th&gt; and &lt;/th&gt; tags. 
         I am not entirely happy that things are wrapped automatically since you might not 
         want plain old th tags but I also don;t want to add another field in the structure.

    <li> order_clause -- the order clause for the field.  If null it defaults to 
         "column_id $order".  It is also interpolated, with orderby and order
         defined as variables so that:
         <pre>
             {upper(last_name) $order, upper(first_names) $order}
         </pre>
         would do the right thing.
         <p> 
         the value "no_sort" should be used for columns which should not allow sorting.
	 <p>
	 the value "sort_by_pos" should be used if the columns passed in
	 are column positions rather than column names.

    <li> display_info.  If this is a null string you just default to generating 
         &lt;td&gt;column_id&lt;/td&gt;.  If it is a string in the lookup list
         then special formatting is applied; this is l r c tf 01 for
         align=left right center, Yes/No (from tf), 
         Yes/No from 0/1.
          
         <p>
         if the display stuff is not any of the above then it is interpolated and the results 
         returned (w/o any &lt;td&gt; tags put in).
    An example:
    <pre>
    set table_def { 
        {ffn "Full Name" 
            {upper(last_name) $order, upper(first_names) $order}
            {&lt;td&gt;&lt;a href="/admin/users/one.tcl?user_id=$user_id"&gt;$first_names&nbsp;$last_name&lt;/a&gt;&lt;/td&gt;}}
        {email "e-Mail" {} {&lt;td&gt;&lt;a href="mailto:$email"&gt;$email&lt;/a&gt;}}
        {email_bouncing_p "e-Bouncing?" {} tf}
        {user_state "State" {} {}}
        {last_visit "Last Visit" {} r}
        {actions "Actions" no_sort {&lt;td&gt;
                &lt;a href="/admin/users/basic-info-update.tcl?user_id=$user_id"&gt;Edit Info&lt;/a&gt; | 
                &lt;a href="/admin/users/password-update.tcl?user_id=$user_id"&gt;New Password&lt;/a&gt; |
            [ad_registration_finite_state_machine_admin_links $user_state $user_id]}}
    }
    </pre>
    </ul>

} {

    set full_statement_name [db_qd_get_fullname $statement_name]

    # This procedure needs a full rewrite!
#    db_with_handle Tdb {}
#    cmdtrace on
        set Tdb [ns_db gethandle main]
	# Execute the query
        set retval [ns_db exec $Tdb $sql_qry]

        set selection [ns_set create rowdata]
        #ns_db getrow $Tdb $selection

        set retval 
	set Tcount 0
	set Tband_count 0
	set Tpage_count 0
	set Tband_color 0
	set Tn_bands [llength $Tband_colors]
	set Tform [ad_conn form]
	
	# export variables from calling environment
	if {![empty_string_p $Textra_vars]} {
	    foreach Tvar $Textra_vars {
		upvar $Tvar $Tvar
	    }
	}
	
	# get the current ordering information
	set Torderbykey {::not_sorted::}
	set Treverse {}
	regexp {^([^*,]+)([*])?} $Torderby match Torderbykey Treverse
	if {$Treverse == "*"} {
	    set Torder desc
	} else { 
	    set Torder asc
	}
	
	# set up the target url for new sorts
	if {[empty_string_p $Torder_target_url]} {
	    set Torder_target_url [ad_conn url]
	}
	set Texport "[uplevel [list export_ns_set_vars url [list orderby$Tsuffix]]]&"
	if {$Texport == "&"} {
	    set Texport {}
	}
	set Tsort_url "$Torder_target_url?${Texport}orderby$Tsuffix="
	
	set Thtml {}
	set Theader {}
	
	# build the list of columns to display...
	set Tcolumn_list [ad_table_column_list $Tdatadef $Tcolumns]
	
	# generate the header code 
	#
	append Theader "<table $Ttable_extra_html>\n"
	if {[empty_string_p $Theader_row_extra]} {
	    append Theader "<tr>\n"
	} else {
	    append Theader "<tr $Theader_row_extra>\n"
	}



	foreach Ti $Tcolumn_list {
	    set Tcol [lindex $Tdatadef $Ti]
	    if { ( [ns_set find $selection [lindex $Tcol 0]] < 0
		   && ([empty_string_p [lindex $Tcol 2]] || 
	               ([string compare [lindex $Tcol 2] "sort_by_pos"] != 0)
	              )
	         )
		 || [string compare [lindex $Tcol 2] no_sort] == 0
	     } {
		
		# not either a column in the select or has sort code
		# then just a plain text header so do not do sorty things
		append Theader " <th>[lindex $Tcol 1]</th>\n"
	    } else {
		if {[string compare [lindex $Tcol 0] $Torderbykey] == 0} {
		    if {$Torder == "desc"} {
			set Tasord $Tasc_order_img
		    } else {
			set Tasord $Tdesc_order_img
		    }
		} else {
		    set Tasord {}
		}
		append Theader " <th><a href=\"$Tsort_url[ns_urlencode [ad_new_sort_by [lindex $Tcol 0] $Torderby]]\">\n"
		append Theader "[lindex $Tcol 1]</a>&nbsp;$Tasord</th>\n"
	    }
	}
	append Theader "</tr>\n"
	
	#
	# This has gotten kind of ugly.  Here we are looping over the 
	# rows returned and then potentially a list of ns_sets which can 
	# be passed in (grrr.  Richard Li needs for general protections stuff
	# for "fake" public record which does not exist in DB).
	# 
	
	set Tpost_data 0
	
	while { 1 } { 
	    if {!$Tpost_data && [ns_db getrow $Tdb $selection]} {     
		# in all its evil majesty
		#set_variables_after_query
		::nstcl::ad_ns_set_to_tcl_vars $selection ; #jjs
	    } else { 
		# move on to fake rows...
		incr Tpost_data
	    } 
	    
	    if { $Tpost_data && $Tpost_data <= [llength $Tpost_data_ns_sets] } { 
		# bind the Tpost_data_ns_sets row of the passed in data
		#set_variables_after_query_not_selection [lindex $Tpost_data_ns_sets [expr $Tpost_data - 1]]
		::nstcl::ad_ns_set_to_tcl_vars [lindex $Tpost_data_ns_sets [expr $Tpost_data - 1]] ; #jjs
	    } elseif { $Tpost_data } { 
		# past the end of the fake data drop out.
		break
	    }
	    
	    if { $Tmax_rows && $Tcount >= $Tmax_rows } {
		if { ! $Tpost_data } { 
		    # we hit max count and had rows left to read...
		    ns_db flush $Tdb
		}
		break
	    }
	    
	    # deal with putting in the header if need 
	    if { $Tcount == 0 } {
		append Thtml "$Theader"
	    } elseif { $Tpage_count == 0 }  { 
		append Thtml "</table>\n$Ttable_break_html\n$Theader"
	    }

	    # first check if we are in audit mode and if the audit columns have changed
	    set Tdisplay_changes_only 0
	    if {![empty_string_p $Taudit] && $Tcount > 0} { 
		# check if the audit key columns changed 
		foreach Taudit_key $Taudit { 
		    if {[string compare [set $Taudit_key] [set P$Taudit_key]] == 0} { 
			set Tdisplay_changes_only 1
		    }
		}
	    }

	    # this is for breaking on sorted field etc.
	    append Thtml [subst $Tpre_row_code]

	    if { ! $Tdisplay_changes_only } {
		# in audit mode a record spans multiple rows.
		incr Tcount
		incr Tband_count
	    }
	    incr Tpage_count

	    if { $Trows_per_page && $Tpage_count >= $Trows_per_page } { 
		set Tband_color 0
		set Tband_count 0
		set Tpage_count 0

	    }

	    # generate the row band color 
	    if { $Tn_bands } {
		if { $Tband_count >= $Trows_per_band } {
		    set Tband_count 0
		    set Tband_color [expr ($Tband_color + 1) % $Tn_bands ]
		}
		# do this check since we would like the ability to band with
		# page background as well
		if {[empty_string_p [lindex $Tband_colors $Tband_color]]} {
		    set Trow_default "<tr>\n"
		} else {
		    set Trow_default "<tr bgcolor=[lindex $Tband_colors $Tband_color]>\n"
		}
	    } else { 
		set Trow_default "<tr>\n"
	    }
	    
	    append Thtml [subst $Trow_code]
	    
	    foreach Ti $Tcolumn_list {
		set Tcol [lindex $Tdatadef $Ti]
		# If we got some special formatting code we handle it
		# single characters r l c are special for alignment 
		set Tformat [lindex $Tcol 3]
		set Tcolumn [lindex $Tcol 0]
		switch $Tformat {
		    "" {set Tdisplay_field " <td>[set $Tcolumn]</td>\n"}
		    r {set Tdisplay_field " <td align=right>[set $Tcolumn]</td>\n"}
		    l {set Tdisplay_field " <td align=left>[set $Tcolumn]</td>\n"}
		    c {set Tdisplay_field " <td align=center>[set $Tcolumn]</td>\n"}
		    tf {set Tdisplay_field " <td align=center>[util_PrettyBoolean [set $Tcolumn]]</td>\n"}
		    01 {set Tdisplay_field " <td align=center>[util_PrettyTclBoolean [set $Tcolumn]]</td>\n"}
		    bz {set Tdisplay_field " <td align=right>&nbsp;[blank_zero [set $Tcolumn]]</td>\n"}
		    default {set Tdisplay_field " [subst $Tformat]\n"}
		}

		if { $Tdisplay_changes_only 
		     && [string compare $Tdisplay_field $Tlast_display($Ti)] == 0} { 
		    set Tdisplay_field {<td>&nbsp;</td>}
		} else { 
		    set Tlast_display($Ti) $Tdisplay_field
		} 
		append Thtml $Tdisplay_field
	    }

	    append Thtml "</tr>\n"

	    # keep the last row around so we can do fancy things.
	    # so on next row we can say things like if $Pvar != $var not blank
	    if { $Tpost_data && $Tpost_data <= [llength $Tpost_data_ns_sets] } { 
		# bind the Tpost_data_ns_sets row of the passed in data
		set_variables_after_query_not_selection [lindex $Tpost_data_ns_sets [expr $Tpost_data - 1]] P	
	    } else { 
		set_variables_after_query_not_selection $selection P
	    }
	}

    
    if { $Tcount > 0} {
	append Thtml "$Textra_rows
</table>\n"
    } else { 
	append Thtml $Tmissing_text
    }
    #{}    
    ns_db releasehandle $Tdb
    
    return $Thtml
}

::pnsd::lock_proc ad_table




namespace eval template {
    namespace eval form  {}
}


::nstcl::ad_proc -public template::form::template { id { style "" } } {

  get_reference 

  set elements:rowcount 0

  foreach element_ref $elements {

    incr elements:rowcount

    # get a reference by index for the multirow data source
    upvar #$level $element_ref elements:${elements:rowcount} 
    set "elements:${elements:rowcount}(rownum)" ${elements:rowcount}
  }
    
  if { [string equal $style {}] } { set style standard }
  set file_stub [template::get_resource_path]/forms/$style

  # set the asset url for images
  set assets "[template::get_resource_path]/assets"
  # assume resources are under page root (not safe)
  regsub "^[ns_info pageroot]" $assets {} assets

  # ensure that the style template has been compiled and is up-to-date
  template::adp_init adp $file_stub

  # get result of template output procedure into __adp_output
  # the only data source on which this template depends is the "elements"
  # multirow data source.  The output of this procedure will be
  # placed in __adp_output in this stack frame.

  #JS: Awful ugly Hack ... this is being evaluated at the wrong level--push it down 1 lower.
  eval "proc xxxx {} {  template::code::adp::$file_stub }"
  xxxx
  #template::code::adp::$file_stub
  rename xxxx ""

  #NUKEME:
  set __adp_output [set @output]
  # unprotect registered tags and variable references
      set __adp_output [string map { ~ < + @ } $__adp_output]

      return $__adp_output
  
  

}

proc template::form::generate { id { style "" } } {

  set __adp_output [template $id $style]
#  regsub -all {<formwidget[^>]+?>} $__adp_output {&</formwidget>} __adp_output
  
  set level [template::adp_level]
  
  # compile the template
  set code [template::adp_compile -string $__adp_output]

  # these variables are expected by the formwidget and formgroup tags
  set form:id $id
  upvar #$level $id:elements $id:elements__ formerror formerror 
  set id:elements [lappend $id:elements__]
  upvar #$level $id:properties form_properties

  
  foreach element_ref [set $id:elements]  { 

    # get a reference by element ID for formwidget and formgroup tags
    upvar #$level $element_ref $element_ref
  }

#JS: Hack to make sure this exists.  Can this be moved?
#  foreach element_id $form_properties(element_names) { 
#      set formerror($element_id) ""
#  }


  # evaluate the code and return the rendered HTML for the form
#  return [template::adp_eval $code -include] ; #JS - not sure if include is the way to go...
  return [template::adp_eval $code -inline] ; #JS - not sure if include is the way to go...


}



# Render the HTML for the form widget, incorporating any additional
# markup attributes specified in the template.

#if {[lsearch -exact ::nstcl::template::adp::registered_tags::single "formwidget"] == -1 } {
#    lappend  ::nstcl::template::adp::registered_tags::single "formwidget" 
#}
proc ::nstcl::template::adp::tags::_formwidget {tree code_var action node} {

# NSTCL->OpenACS conversion/preamble
    upvar 1 $code_var code

    if {$action != "enter"} {
	return
    }


    array set data [tag_attributes [$tree get $node -key data]]

    
    set params [ns_set create]
    foreach {key value} [ array get data ] { 
	ns_set put $params $key $value
    }
# (end)NSTCL->OpenACS conversion

    set id [template::get_attribute formwidget $params id]

    # get any additional HTML attributes specified by the designer
    set tag_attributes [template::util::set_to_list $params id]
    
    template::adp_append_string \
	"\[template::element render \${form:id} $id { $tag_attributes } \]"
}


#    template_tag formerror { chunk params } { }

#if {[lsearch -exact ::nstcl::template::adp::registered_tags::multiple formerror] == -1 } {
#    lappend  ::nstcl::template::adp::registered_tags::multiple "formerror" 
#}

proc ::nstcl::template::adp::tags::_formerror {tree code_var action node} {

# NSTCL->OpenACS conversion
    upvar 1 $code_var code

    if {$action != "enter"} {
	template::adp_append_code "\# END formerror $node"	
	template::adp_append_code "\}"
	return
    }

    template::adp_append_code "\# BEGIN formerror $node"	

    array set data [tag_attributes [$tree get $node -key data]]    
    set params [ns_set create]
    foreach {key value} [ array get data ] { 
	ns_set put $params $key $value
    }

# get chunk
    set children [$tree children $node ]

    variable chunk 
    if {[llength children ] == 1 } { 
	
	set chunk [$tree get $children -key data]
    }

# (end)NSTCL->OpenACS conversion

  set id [template::get_attribute formwidget $params id]
  set type [ns_set get $params type]

  if { [string equal $type {}] } {
    set key $id
  } else {
    set key $id:$type
  }

  template::adp_append_code "
    if \{ \[info exists formerror($key)\] \} \{
      set formerror($id) \$formerror($key)
    "

  if { [string equal $chunk {}] } {

    template::adp_append_string "\$formerror($key)"

  } else {

# By default, nstcl will compile the inner chunk
#   template::adp_compile_chunk $chunk

  }


}

# my tweaked version ... doesn't append to parse list but returns it directly.
proc template::adp_compile_chunk { chunk } { 

    set __code [info body [::template::adp_compile -inline $chunk]]
    
    return $__code

}


::nstcl::ad_proc -public template::form::get_reference {} {

  uplevel {
    set level [template::adp_level]

      upvar #$level $id:elements _elements $id:properties properties 
      set elements $_elements   ; #JS: need to copy by value because the form template over-writes teh value of elements.
      upvar #$level $id:properties form_properties

      if { ! [info exists _elements] } {
	  error "Form $id does not exist"
      }
  }
}






