ad_page_contract {
    @author Neophytos Demetriou <k2pts@cytanet.com.cy>
    @creation-date September 01, 2001
    @cvs-id $Id: search.tcl,v 1.9 2002/09/18 12:04:29 jeffd Exp $
} {
    q:notnull,trim
    {t:trim ""}
    {offset:integer 0}
    {num:integer 0}
    {dfs:trim ""}
    {dts:trim ""}
} -errors {
    q:notnull {You must specify some keywords.}
}

ns_startcontent -type "text/html"

set this_dir [file dirname [ad_conn file]]
set template_top_file "$this_dir/search-results-top"
set template_one_file "$this_dir/search-results-one"
set template_bottom_file "$this_dir/search-results-bottom"

set user_id [ad_conn user_id]
set package_id [ad_conn package_id]
set driver [ad_parameter -package_id $package_id FtsEngineDriver]
array set info [acs_sc_call FtsEngineDriver info [list] $driver]

if { [array get info] == "" } {
    ReturnHeaders
    ns_write "FtsEngineDriver not available!"
    return
} 
if { $num <= 0} {
    set limit [ad_parameter -package_id $package_id LimitDefault]
} else {
    set limit $num
}


set df ""
set dt ""
if { $dfs == "all" } { set dfs "" }
array set symbol2interval [ad_parameter -package_id $package_id Symbol2Interval]
if { $dfs != "" } { set df [db_exec_plsql get_df "select now() + '$symbol2interval($dfs)'::interval"] }
if { $dts != "" } { set dt [db_exec_plsql get_dt "select now() + '$symbol2interval($dts)'::interval"] }






set title "Search Results"
set context_bar [ad_context_bar {Search Results}]

set q [string tolower $q]
set urlencoded_query [ad_urlencode $q]
if { $offset < 0 } { set offset 0 }
set t0 [clock clicks -milliseconds]
array set result [acs_sc_call FtsEngineDriver search [list $q $offset $limit $user_id $df $dt] $driver]
set tend [clock clicks -milliseconds]

if { $t == "Feeling Lucky" && $result(count) > 0} {
    set object_id [lindex $result(ids) 0]
    set object_type [acs_object_type $object_id]
    set url [acs_sc_call FtsContentProvider url [list $object_id] $object_type]
    ad_returnredirect $url
    ad_script_abort
}

set elapsed [format "%.02f" [expr double(abs($tend - $t0)) / 1000.0]]
if { $offset >= $result(count) } { set offset [expr ($result(count) / $limit) * $limit] }
set low [expr $offset + 1]
set high [expr $offset + $limit]
if { $high > $result(count) } { set high $result(count) }
if { $info(automatic_and_queries_p) && ([lsearch -exact $q and] > 0) } {
    set and_queries_notice_p 1
} else {
    set and_queries_notice_p 0
}
set url_advanced_search ""
append url_advanced_search "advanced-search?q=${urlencoded_query}"
if { $num > 0 } { append url_advanced_search "&num=${num}" }


set template_top [template::adp_parse $template_top_file [list \
	t $t \
	title $title \
	context_bar $context_bar \
	query $q \
	nquery [llength $q] \
	and_queries_notice_p $and_queries_notice_p \
	stopwords $result(stopwords) \
	nstopwords [llength $result(stopwords)] \
	low $low \
	high $high \
	count $result(count) \
	elapsed $elapsed \
	url_advanced_search $url_advanced_search]]

ReturnHeaders "text/html"
ns_write $template_top

    for { set __i 0 } { $__i < [expr $high - $low +1] } { incr __i } {

	set object_id [lindex $result(ids) $__i]
	set object_type [acs_object_type $object_id]
	array set datasource [acs_sc_call FtsContentProvider datasource [list $object_id] $object_type]
	search_content_get txt $datasource(content) $datasource(mime) $datasource(storage_type)
	set title_summary [acs_sc_call FtsEngineDriver summary [list $q $datasource(title)] $driver]
	set txt_summary [acs_sc_call FtsEngineDriver summary [list $q $txt] $driver]
	set url [acs_sc_call FtsContentProvider url [list $object_id] $object_type]


	set template_one [template::adp_parse $template_one_file [list \
		title_summary $title_summary \
		txt_summary $txt_summary \
		url $url]]

	ns_write $template_one

    }

    set from_result_page 1
    set current_result_page [expr ($low / $limit) + 1]
    set to_result_page [expr ceil(double($result(count)) / double($limit))]

    set url_previous ""
    set url_next ""
    append url_previous "search?q=${urlencoded_query}"
    append url_next "search?q=${urlencoded_query}"
    if { [expr $current_result_page - 1] > $from_result_page } { 
	append url_previous "&offset=[expr ($current_result_page - 2) * $limit]"
    }
    if { $current_result_page < $to_result_page } { 
	append url_next "&offset=[expr $current_result_page * $limit]"
    }
    if { $num > 0 } {
	append url_previous "&num=$num"
	append url_next "&num=$num"
    }


    set items [list]
    set links [list]
    set values [list]
    for { set __i $from_result_page } { $__i <= $to_result_page} { incr __i } {
	set link ""
	append link "search?q=${urlencoded_query}"
	if { $__i > 1 } { append link "&offset=[expr ($__i - 1) * $limit]" }
	if { $num > 0 } { append link "&num=$num" }

	lappend items $__i
	lappend links $link
	lappend values $__i
    }

    set search_the_web [ad_parameter -package_id $package_id SearchTheWeb]
    if [llength $search_the_web] {
	set stw ""
	foreach {url site} $search_the_web {
	    append stw "<a href=[format $url $urlencoded_query]>$site</a> "
	}
    }

set template_bottom [template::adp_parse $template_bottom_file [list \
	query $q \
	count $result(count) \
	urlencoded_query $urlencoded_query \
	from_result_page $from_result_page \
	current_result_page $current_result_page \
	to_result_page $to_result_page \
	url_previous $url_previous \
	choice_bar [search_choice_bar $items $links $values $current_result_page] \
	url_next $url_next \
	stw $stw\
	  ]]

ns_write $template_bottom