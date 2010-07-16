ad_page_contract {     
    Generates a tarball for a package into its distribution_tarball field.    
    
    @param version_id The package version to be processed.
    @author Bryan Quinn (bquinn@arsdigita.com)
    @creation-date 9 May 2000
    @cvs-id $Id: version-generate-tarball.tcl,v 1.2 2002/09/10 22:21:59 jeffd Exp $
} {
    {version_id:integer}
}
db_transaction {
    apm_filelist_update $version_id
    apm_generate_tarball $version_id
} on_error {
    ad_return_complaint "APM Generation Error" "The database returned the following error message:
<pre>
<blockquote>
[ad_quotehtml $errmsg]
</blockquote>
</pre>
"
}

ad_returnredirect "version-view?version_id=$version_id"
