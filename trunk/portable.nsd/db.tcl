
#Overload nstcl database routines to make them QD-aware


foreach proc { 
    db_dml 
    db_string 
    db_foreach
    db_0or1row
    db_1row
    db_list
    db_list_of_lists
} {
    
   ad_proc $proc { {-dbn "" } statement_name  pre_sql args } "
\#        ns_log notice \"$proc called with \[info level 0]\"
        set full_statement_name statement_name 
	if \[catch {
            set full_statement_name \[db_qd_get_fullname \$statement_name]
            set sql \[ db_qd_replace_sql \$full_statement_name \$pre_sql] } ] {
	    set sql \$pre_sql
	}
	
\#        if {! \[string equal \$pre_sql \$sql] } {
\#          puts \[list \"QD interceptifier:\" \$statement_name \$full_statement_name \$pre_sql \$sql  ]
\#        }
\#        puts \" Full Statement Name: \$full_statement_name\"
        set script  \[list subst \$sql]
        set sql \[uplevel 1 \[list if 1 \$script]] ; \#caches bytecode
\#        set cmd [concat \[list ::nstcl::$proc  \$statement_name \$sql] \$args]
\#        puts \$cmd
	uplevel 1 \[concat \[list ::nstcl::$proc \$statement_name \$sql] \$args]
    "

}


::nstcl::ad_proc db_multirow {-local:boolean 
    -append:boolean 
    -extend:optional 
    var_name statement_name pre_sql args } { 

	overload db_multirow with qd support

    } {
	
	set full_statement_name [db_qd_get_fullname $statement_name]
	set sql [ db_qd_replace_sql $full_statement_name $pre_sql]   
	
        set cmd [concat [list ::nstcl::db_multirow $var_name $statement_name $sql] $args]
        ns_log debug "JJS: db_multirow executing the following : $cmd "

	uplevel 1 $cmd
   
#	puts "db_multirow is done"
}

