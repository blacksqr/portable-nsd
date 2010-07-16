# /packages/lang/www/locale-set.tcl
ad_page_contract {

    Sets a locale for the browser

    @author John Lowry (lowry@ardigita.com)
    @creation-date 29 September 2000
    @cvs-id $Id: locale-set.tcl,v 1.1.1.1 2001/04/20 20:51:08 donb Exp $
} {
    locale
    { redirect_url {} }
}

# set the locale property
ad_locale_set locale $locale

if [empty_string_p $redirect_url] {
    set redirect_url  [ns_set iget [ns_conn headers] referer]
}

ad_returnredirect $redirect_url

