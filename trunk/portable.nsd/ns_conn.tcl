# ns_conn  defines all connection/request parameters.
# Since this is the defaul implementation of ns_conn,  we'll just 
# supply dummy values that will make sense for the installer
# Actual run-time environments like fastCGI or mod_dtcl should redefine
# this command with their own version
# TODO It should be coded to be able to run from a file of stored values for debugging

# Copyright 2003   -   John Sequeira  -   http://www.jsequeira.com
# johnseq@pobox.com



proc ns_conn { cmd args } {

    switch $cmd {
	authpassword { 
	    ns_log Warn "pnsd: ns_conn authpassword not implemented yet"
	    return ""
	}
 	authuser { 
	    ns_log Warn "pnsd: ns_conn authuser not implemented yet"
	    return ""
	}
 	close { 
	    ns_log Warn "pnsd: ns_conn close not implemented yet"
	    return }
 	contentlength { ns_log Warn "ns_conn contentlength not implemented yet"
	    return }
 	driver { 
	    return nssock ; #not sure about this
	    ns_log Warn "pnsd: ns_conn driver not implemented yet"
	    return }
 	form { ns_log Warn "pnsd: ns_conn form not implemented yet"
	    return }
 	headers { #ns_log Warn "ns_conn headers not fully implemented yet"
	    return $::pnsd::headers_id 
	}
 	host { #ns_log Warn "ns_conn host not implemented yet"
	    return [info hostname] }
 	isconnected { 
	    #ns_log Warn "ns_conn isconnected not implemented yet"
	    return 1}
 	location { 
	    return "http://[ns_conn host]:[ns_conn port]/" }
 	method { 
#	    ns_log Warn "ns_conn method not fully implemented yet"
	    return "GET"
	}

 	outputheaders { #ns_log Warn "ns_conn outputheaders not implemented yet"
	    return $::pnsd::output_headers_id
	}
 	peeraddr { 
	    #ns_log Warn "ns_conn peeraddr not implemented yet"
	    return "127.0.0.1"
	}
 	port { 
#	    ns_log Warn "ns_conn port not implemented yet"
	    return "80"
	}
 	protocol { 
	    return "http"}
 	query { 
#	    ns_log Warn "ns_conn query not implemented yet"

	    return $::pnsd::querystring
	}
 	request { 
	    return "GET [ns_conn url] HTTP/[ns_conn version]" 
	}
 	url { 
#	    ns_log Warn "ns_conn url not fully implemented yet"
	    return $::pnsd::url
	}
 	urlv { 
#	    ns_log Warn "ns_conn urlc not implemented yet"
	    set urllist [split [ns_conn url] /]
	    return [lrange $urllist 1 [llength $urllist]]
	}
 	urlc { 
# 	    ns_log Warn "ns_conn urlv not implemented yet"
	    set urlv [expr [llength [split [ns_conn url] /]] - 1]
	    if {$urlv == ""} { return 0 }
	    return $urlv
	}
 	version { 
#	    ns_log Warn "ns_conn version not implemented yet"
	    return "1.0"
	}
    }

}
