#/packages/acs-lang/www/show-catalog.tcl
ad_page_contract {

    List contents of message catalog

    @author Henry Minsky (hqm@ardigita.com)
    @creation-date 29 September 2000
    @cvs-id $Id: show-catalog.tcl,v 1.1.1.1 2001/04/20 20:51:08 donb Exp $
} { }

set title "Show Message Catalog"
set header [ad_header $title]
# set navbar [ad_context_bar "Show Message Catalog "]
set footer [ad_footer]

# Test 3 checks that the timezone tables are installed
# Need this data to check that test 4 works
set cat_sql "SELECT key, lang, message, registered_p
               FROM lang_messages
              ORDER BY key, lang"

db_multirow catalog catalog_data $cat_sql

ad_return_template