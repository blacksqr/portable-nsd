# /packages/mbryzek-subsite/www/admin/rel-segments/constraints-redirect.tcl

ad_page_contract {

    Optionally redirects user to enter constraints

    @author mbryzek@arsdigita.com
    @creation-date Thu Jan  4 11:20:37 2001
    @cvs-id $Id: constraints-redirect.tcl,v 1.1.1.1 2001/03/13 22:59:26 ben Exp $

} {
    segment_id:naturalnum,notnull
    { operation "" }
    { return_url "" }
}

set operation [string trim [string tolower $operation]]

if { [string eq $operation "yes"] } {
    ad_returnredirect "constraints/new?rel_segment=$segment_id&[ad_export_vars return_url]"
} else {
    if { [empty_string_p $return_url] } {
	set return_url "one?[ad_export_vars segment_id]"
    }
    ad_returnredirect $return_url
}

