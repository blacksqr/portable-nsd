#@+leo
#@+node:0::@file pnsd-cgi-bootstrap.tcl
#@+body
#@+doc
# 
# 
# Messing around... I'm using this file to create the autoindex for autoloading.
# 
# 

#@-doc
#@@code

package require Tclx
source pnsd-init.tcl


auto_mkindex openacs-4/tcl *.tcl

exit

source c:/temp/openacs-4/packages/acs-tcl/tcl/security-procs.tcl
proc unknown {cmdName args} {
    global OACS_PROCS
    puts "$cmdName $OACS_PROCS($cmdName)"

    if [info exists $OACS_PROCS($cmdName)] {
	ns_log info "Sourcing $OACS_PROCS($cmdName)"
	
	uplevel 1 [subst {$cmdName $args}]
	return
    }

    ns_log Error "$cmdName not found!"
    error "$cmdName not found!"
}
puts $OACS_PROCS(ad_user_login)
cmdtrace on notruncate
ad_user_login 2537

#packages/acs-tcl/tcl/security-init.tcl
#packages/acs-tcl/tcl/site-nodes-init.tcl

#Inits I don't really need:
#packages/acs-tcl/tcl/utilities-init.tcl
#packages/acs-tcl/tcl/request-processor-init.tcl
#notice    {Loading packages/acs-tcl/tcl/20-memoize-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/admin-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/database-init.tcl...}
#notice    {Loading packages/acs-tcl/tcl/document-init.tcl...}

#@-body
#@-node:0::@file pnsd-cgi-bootstrap.tcl
#@-leo
