# /www/register/bad-password.tcl

ad_page_contract {
    Informs the user that they have typed in a bad password.
    @cvs-id $Id: bad-password.tcl,v 1.1.1.1 2001/03/13 22:59:26 ben Exp $
} {
    {user_id:naturalnum}
    {return_url ""}
} -properties {
    system_name:onevalue
    email_forgotten_password_p:onevalue
    user_id:onevalue
}

set email_forgotten_password_p [ad_parameter EmailForgottenPasswordP security 1]

set system_name [ad_system_name]

ad_return_template