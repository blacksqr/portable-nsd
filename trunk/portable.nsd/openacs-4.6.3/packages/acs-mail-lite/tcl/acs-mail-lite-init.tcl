ad_library {

    initialization for acs_mail_lite module

    @author Eric Lorenzo (eric@openforce.net)
    @creation-date 22 March, 2002
    @cvs-id $Id: acs-mail-lite-init.tcl,v 1.2 2002/09/10 22:22:09 jeffd Exp $

}

# Default interval is 1 minute.
ad_schedule_proc -thread t 60 acs_mail_lite::sweeper
