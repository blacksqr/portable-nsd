
set file "D:/temp/openacs-4/packages/acs-tcl/tcl/apm-install-procs-oracle.xql"

package require fileutil 

set xml [fileutil::cat $file]

regexp  {<rdbms>\s*<type>\s*(\w+)</type>\s*<version>(.*?)</version>} $xml line db version

set s "zzzzpqqqqxxxxxxxqq"
regexp {\w+?(?=qq)} $s match bit
puts $match


set queryname ""
set query ""
regexp {<fullquery\s+name\s*=\s*"([^"]+?)">\s*<querytext>( )fullquery} $xml match queryname query
puts "query: $queryname $query"

#puts $xml
