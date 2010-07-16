# /www/register/logout.tcl

ad_page_contract {
    Logs a user out

    @cvs-id $Id: logout.tcl,v 1.1.1.1.4.1 2003/02/02 20:23:03 jeffd Exp $

} {
	{return_url "/"}
    
}

ad_user_logout 
db_release_unused_handles

ad_returnredirect $return_url

