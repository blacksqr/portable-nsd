#  ns_dbsetup.tcl
# 	$Id: ns_dbsetup.tcl,v 1.10 2003/07/19 18:02:33 john Exp $	
#
#  Setup the database connections,  and define nstcl-customized, database-specific API's
#  TODO: break out connection init from db-apis
#  TODO: Add Oracle support
#  John Sequeira
#  johnseq@pobox.com



package require nstcl-database 


if {[lsearch [namespace children] ::nstcl] == -1} {
    package require nstcl 
    namespace import ::nstcl::*    
}


proc setup_pool { pool } {
    nstcl::configure_pool [nstcl::ns_config ns/db/pool/$pool Driver] $pool \
	[nstcl::ns_config ns/db/pool/$pool Connections]  \
	[nstcl::ns_config ns/db/pool/$pool DataSource]   \
	[nstcl::ns_config ns/db/pool/$pool User]   \
	[nstcl::ns_config ns/db/pool/$pool Password]   	
}


#define three pools 
nstcl::load_driver [nstcl::ns_config ns/db/pool/main Driver]


setup_pool main 
setup_pool log
setup_pool subquery

nstcl::set_default_pool main

proc db_name {} {  
#    Returns the name of the database as reported by the driver. 

    set dbh [nstcl::ns_db gethandle main]
    set dbtype [nstcl::ns_db dbtype $dbh]
    nstcl::ns_db releasehandle $dbh
    return $dbtype


}





