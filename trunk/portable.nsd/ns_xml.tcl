# ns_xml.tcl 
# Map ns_xml calls to tdom
# 	$Id: ns_xml.tcl,v 1.4 2003/01/09 13:15:06 john Exp $	

# Copyright 2003 John Sequeira 
# johnseq@pobox.com
# 


#package require nslog
package require tdom

proc ns_xml { command args } {
    switch $command {
	"parse" { 
	    set xml [lindex $args 0] 

	    #ignore the -persist flag...
	    if [string equal $xml "-persist"] {
		set xml [lindex $args 1] 
	    }

	    #hack xql files into valid xml - sigh
#	    regsub -all  {<querytext>} $xml {<querytext><![CDATA[} xml
#	    regsub -all  {</querytext>} $xml {]]></querytext>} xml

	    set doc [dom parse $xml]; 
	    return $doc 
	}  
	"doc" { 
	    switch [lindex $args 0] {
		"root"  { set root  [[lindex $args 1] documentElement]; return $root }
		"free" { [lindex $args 1] delete; return  } 
	    }
	    ns_log Error "ns_xml called with bad 'doc' args: $args"
	}

	"node"  {  


	    switch [lindex $args 0] { 
		"children" { set children [ [lindex $args 1] childNodes] ; return $children }		
		"name"     { 
		    set node [lindex $args 1]
#		    if {[$node nodeType] == "ELEMENT_NODE" } {
#			if {[$node hasAttribute name]} {
#			    return [$node getAttribute name ]
#			} else { 
#			    puts "$node $errmsg"			    
#			}
#		    }
		    set name [$node nodeName]
		    return $name; 
		}


	        "getattr"  { 
		    set node [lindex $args 1]
		    set name [lindex $args 2]
		    
		    if {[$node nodeType] == "ELEMENT_NODE" } {
			if {[$node hasAttribute $name] } {
			    return [$node getAttribute $name ]
			} else  {
			    #no attribute here
			    return ""; 
			}
		    }

		}
		"getcontent"  { 
		    set node [lindex $args 1]
		    switch [$node nodeType] {
			"ELEMENT_NODE" {set content [$node text ]}
			"TEXT_NODE" { set content [$node data]}
		    }
		    return $content
		}

		
	    }
	}
	
    }
    ns_log Error "ns_xml called with bad args: $args"
}
