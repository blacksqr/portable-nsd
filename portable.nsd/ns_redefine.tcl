#I'll put commands I want to redefine here

rename xml_parse xml_parse.orig
proc xml_parse args {
    if {[lindex $args 0] == "-persist"} {
	#JS
	return [ns_xml parse [lindex $args 1]]
#	return [ns_xml parse -persist [lindex $args 1]]
    } else {
	return [ns_xml parse [lindex $args 0]]
    }
}


#I'm using ns_tcl's db_string

