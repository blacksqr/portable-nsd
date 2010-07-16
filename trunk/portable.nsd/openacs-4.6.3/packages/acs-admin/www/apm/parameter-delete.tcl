#packages/acs-core/admin-www/apm/parameter-delete-2.tcl
ad_page_contract {
    Adds a parameter to a version.
    @author Todd Nightingale [tnight@arsdigita.com]
    @creation-date 17 April 2000
    @cvs-id $Id: parameter-delete.tcl,v 1.2 2002/09/10 22:21:59 jeffd Exp $
} {
    parameter_id:naturalnum,notnull
    version_id:naturalnum,notnull
}

apm_parameter_unregister $parameter_id

ad_returnredirect version-parameters?version_id=$version_id
