
source [file join [file dirname [info script]] .. pnsd-init.tcl]; source_openacs

#if {[lsearch [namespace children] ::tcltest] == -1} {
#    package require tcltest
#    namespace import ::tcltest::*
#}



set table_def {
    { package_key "Key" "" "<td><a href=\"version-view?version_id=$version_id\">$package_key</a></td>" }
    { pretty_name "Name" "" "<td><a href=\"version-view?version_id=$version_id\">$pretty_name</a></td>" }
    { version_name "Ver." "" "" }
    { n_files "Files" "" {<td align=right>&nbsp;&nbsp;<a href=\"version-files?version_id=$version_id\">$n_files</a>&nbsp;</td>} }
    {
	status "Status" "" {<td align=center>&nbsp;&nbsp;[eval {
	    if { $installed_p == "t" } {
		if { $enabled_p == "t" } {
		    set status "Enabled"
		} else {
		    set status "Disabled"
	}	
	    } elseif { $superseded_p } {
		set status "Superseded"
	    } else {
		set status "Uninstalled"
	    }
	    format $status
	}]&nbsp;&nbsp;</td>}
    }
    { maintained "Maintained" "" {<td align=center>[ad_decode $distribution_uri "" "Locally" "Externally"]</td>} }
    {
	action "" "" {<td bgcolor=white>&nbsp;&nbsp;[eval {
	    ns_log Notice "Status for $version_id: [apm_version_load_status $version_id]"
	    if { $installed_p == "t" && $enabled_p == "t" && \
		    [string equal [apm_version_load_status $version_id] "needs_reload"] } {
		format "<a href=\"version-reload?version_id=$version_id\">reload</a>"
	    } else {
		format ""
	    }
	}]&nbsp;&nbsp;</td>}
    }
}



set missing_text "<strong>No packages match criteria.</strong>"
set table [ad_table -Torderby "package_key" -Tmissing_text $missing_text "apm_table" "select * from apm_package_versions"]


set orderby worksite_name
set no_worksites_msg "no worksites"
set ab_query 
set table_def {
    { "" "<input name=\"allbox\" type=\"checkbox\" value=\"Check All\" onClick=\"CheckAll();\">" {worksite_id "&nbsp;"} { <td align=center valign=top><font face=verdana size=1><input type=checkbox name=\"msg_ids\" value=$worksite_id></font></td>} } 
    {worksite_name "<font face=verdana color=white size=1>Worksite" {} { <td valign=top><a href=\"worksite-edit?worksite_id=$worksite_id\"><font face=verdana size=1>$worksite_name</a></td>} }
    { "" "<font face=verdana color=white size=1>Manager" {} { <td valign=top><font face=verdana size=1>[ set managers "" ]
							[db_foreach get_managers "select b.last_name || ', ' || b.first_names as manager
							 from cs_worksite_manager a, cs_employee b 
							 where a.worksite_id = $worksite_id
							 and b.employee_id = a.employee_id" { append managers "<li>$manager" }]$managers</td> } }
}


set ab_query {SELECT worksite_id, worksite_name
	      FROM cs_worksite
    WHERE company_id = 1}

set worksite_table [ad_table -Torderby $orderby -Tmissing_text "<table width=650><tr><td bgcolor=#E5E5E5 align=center><b>$no_worksites_msg</b></td></tr></table>" -Ttable_extra_html "border=0 cellspacing=1 cellpadding=0 width=650" -Theader_row_extra bgcolor="#ffffff" -Theader_row_extra bgcolor="#3599EA" -Tband_colors [list "\#E5E5E5" "\#E5E5E5"]  -Trows_per_page 1000 -Tasc_order_img "<img src=/graphics/up.gif alt=\"^\">" -Tdesc_order_img "<img src=/graphics/down.gif alt=\"v\">" ab_headers $ab_query $table_def]
