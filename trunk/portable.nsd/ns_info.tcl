# Portable.NSD ns_info
# Copyright 2003 John Sequeira
# johnseq@pobox.com

#TODO:  Migrate this to pull from config file.
proc ns_info  { value } { 


    switch $value {
	"tcllib"  { return "$pnsd::root/tcl/" }
	"server"  { return "$pnsd::root" }
	"log"     { return [file join $::pnsd::home log.txt] }
	"pageroot" { return "$pnsd::root/www/" }
	"platform" { return "linux" }
	"hostname" { return "localhost" }
	"config"  { return $::pnsd::home }
	"address" { return "127.0.0.1" }
    }
    ns_log warning "ns_info missed $value\n"
}
