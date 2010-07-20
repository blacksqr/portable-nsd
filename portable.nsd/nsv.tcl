#namespace eval ::nstcl::nsv {
    # set up globals... how do I do that?
proc nsv_set { id key value } {  

#    ns_log Notice "Using nsv_set \n"
#	    namespace eval nsv {	#	    puts "array doesn't exist" }
#do I need to put this in global before checking info?
    
    global $id
    if {! [array exists $id] } { 
#	puts "Globalizing $id"
#	global $id
	array set $id [list]

    }

#    if { $key == "." } { 
#    	eval [format {set %s(.) { %s } }   $id $key $value ]
#    } else {
	set ${id}($key) $value  
#	eval [format {set %s(%s) { %s } }   $id $key $value ]
#    }
    #puts "nsv_set: $id - $key = $value\n\n"   

    #puts [format {set %s(%s) { %s } } $id $key $value ]
    #If array doesn't exist,  create it
    #    if {[info exists $set]} 
    #if { $key == "." } { 
    #	eval [format {set %s(.) { %s } }   $id $key $value ]
    #} else {
    #	eval [format {set %s(%s) { %s } }   $id $key $value ]
        
    #}
    #set value
    #parray ${id}    
    return $value
}




proc nsv_incr { id key } {

    global $id
    if {! [array exists $id] } { 
	set ${id}($key) 0
    }

    set retval [ expr 1 + [nsv_get $id $key] ]
    nsv_set $id $key $retval    
    
}


proc nsv_get { id key } { 
    global $id
#    puts "getting $id ... $key "
#    return     
#    if info exists $id

    if { [info exists ${id}($key)] } {

	return [eval [format {set %s(%s)} $id $key ]]
    } else {
		return ""
	}
       
    #ns_log error "nsv_get failed for $id ( $key ) "
    error "nsv_get failed for $id ( $key ) "
}

# 
proc nsv_array { command array_name args } {
    switch $command {

	"set"  { 
	    
	    global $array_name
	    set values [lindex $args 0 ]
	    if {$values == [list]} { 
		array set $array_name [list]
	    } else {  
		array set $array_name $values
	    }
	    return
	}
	"names" { 
	    global $array_name
	    return [array names $array_name]
	}
	"reset" {
	    #not sure about this...
#	    puts "resetting"
#	    nsv_array set $array_name $args
	    global $array_name
	    set values [lindex $args 0 ]
	    array set $array_name $values
#	    eval $cmd
	    return
	}
	
    }

    ns_log Error "nsv_array called args $args "

}
proc nsv_array_set { array_name values  } {

    global $array_name
    if {$values == {}} { 
	array set $array_name [list]
    } else {  
	array set $array_name $values
    }
    
}
#}

proc nsv_exists { id key } { 
    global $id
    return [info exists ${id}($key)]

}

proc nsv_lappend { id key value args } {
   global $id 
    lappend ${id}($key) $value

}




proc nsv_unset { id key } {  
    global $id
    array unset $id $key

}



nstcl::ad_proc ns_share { {-init ""} name } {
    # don't really do anything for now    
} {

    global $name
    if ![array exists $name] {
	array set $name [list]
    }

    uplevel [list global $name ]

    ns_log Warn  "ns_share not really implemented:  [info level -1]"

}
