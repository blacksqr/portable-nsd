#!c:/Tcl/bin/tclsh.exe
#!/usr/bin/tclsh
#
# createuser.tcl 
#
# John Sequeira
# johnseq@pobox.com
# 12/2003
#

# This loads tclsh-specific aolserver functionality
source [file join [file dirname [info script]] pnsd-init.tcl ]

# Now do the standard tcl source'ing that openacs does in aolserver
pnsd::source_openacs

#I don't think these four lines are necessary, but just in case ...
set ::pnsd::url /
set ::pnsd::querystring ""
ad_user_login 439   ; # administrator
rp_filter ""  ;   

array set result  [auth::create_user -username "oneuser" -email "john@pono.com" -last_name "seq" -first_names "jon"     ]
parray result   ; # output result of create_user,  in case there were errors
