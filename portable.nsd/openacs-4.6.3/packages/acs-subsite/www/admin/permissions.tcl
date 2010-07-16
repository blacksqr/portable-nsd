ad_page_contract {
    Permissions for the subsite itself.
    
    @author Lars Pind (lars@collaboraid.biz)
    @creation-date 2003-06-13
    @cvs-id $Id: permissions.tcl,v 1.1.2.1 2003/07/04 12:56:13 lars Exp $
}

set page_title "[ad_conn instance_name] Permissions"

set context [list "Permissions"]

set subsite_id [ad_conn subsite_id]
