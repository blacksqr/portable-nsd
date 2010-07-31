# ns_misc.tcl - miscellaneous ns_* api procedure stubs which allow aolserver code to be executed from tclsh
# 	$Id: ns_misc.tcl,v 1.16 2004/08/09 13:40:04 john Exp $	
# 
# Copyright 2003 John Sequeira
# johnseq@pobox.com
# 10/2002


package require uri
package require sha1
package require ncgi 
#puts [db_string current_time_from_solid { select now() }]
#

proc ns_register_filter { stage http_method match name args } { 
    
    ns_log notice "ns_register_filter: registering $stage $name for $http_method all $match \n\n"
    
}

proc ns_config { args } {

    set section [lindex $args 0]
    set item [lindex $args 1]

    set retval [eval [concat nstcl::ns_config $args]]
    if { $retval != "" }  { return $retval }
	
#js: TODO: Migrate these hardcoded options over to nsd.tcl
    switch $item {
#	"User"     { return "Administrator" }
#	"Password" { return "" }
#	"DataSource" { return "localhost:5432:openacs2" }
#	"Driver"   { return "" }
	"Verbose"  { ns_log warn "default ns_config returning..."; return "On" }
	"pgbin"    { ns_log warn "default ns_config returning..."; return [file join [lindex [file split [pwd]] 0 ] "/usr/local/pgsql/bin"] } 
	"DebugP"   { ns_log warn "default ns_config returning..."; return "1" }
	"LogDebugP" { ns_log warn "default ns_config returning..."; return "1" }
	"Hostname" { ns_log warn "default ns_config returning..."; return "localhost" }
	"ClusterEnabledP" { ns_log warn "default ns_config returning..."; return "false" }
	"SessionTimeout" { ns_log warn "default ns_config returning..."; return 600 }
	"SessionRenew"   { return 600 }
	"NumberOfCachedSecretTokens" { return 30 }
	"PerformanceModeP" { return 0 }
	"SessionSweepInterval" { return 60 }
	"directorylisting" { return simple 
	    return fancy }
	"MaxSize" { 
	    # for memoize
	    return 200000
	} 
	"SystemOwner" { return johnseq@pobox.com }
	"ExtensionPrecedence" { return ".tcl .adp" }
	"SystemName" { return "PortableNSD" }
	"AllowPersistentLoginP" { return On }
	"PersistentLoginDefaultP" { return On }
	"RestrictErrorsToAdminsP" { return Off }
	"AutomaticErrorReportingP" { return On }
	"RefreshCache"  { return On }
	"HomeURL" { return "http://localhost/pvt" }
	"HomeName" { return "Home Base" }
    }
    
    ns_log notice "ns_config called with $section $item"

}

 proc ns_configsection { path }  { 
     ns_log warning "no-op: ns_configsection called with arg $path "
     return 
 }

proc ns_mutex { cmd args } { 
    return $cmd
}




#Must handle -bind flag
::nstcl::ad_proc ns_pg_bind { -bind:optional  command db sql } { 
    #set cmd [list db_bind_var_substitution $sql $bind ] 
    #set sql [uplevel 1  $cmd ] 
#    ns_log Info "ns_pg_bind: sql =  $sql"
#First replace bound variables ... skip this - nstcl should do this for me.
#     proc quotify { var } {
# 	upvar 2 $var var2
# 	regsub -all {'} $var2 {''} var2
# 	return "'$var2'"
#     }

#     #puts [pg_bind $sql]
#     regsub -all {\[} $sql {\\[} sql
#     regsub -all {\]} $sql {\\]} sql
#     regsub -all  {\:(\w+)} $sql  { [ quotify \1 ]}  sql

#     set sql [subst $sql ] 


#    set poolname [ns_db poolname $db ]
#Then run the query

    #ignore db for now
    switch $command {
	"select" { 	
	    #no name was passed in...
	    set cmd [list ns_db select $db $sql]	    
#	    return [ns_db select $db $sql]	    
	    uplevel 1 [list if 1 $cmd]
	    return
	    
	}
	"1row"   {	    
	    set cmd [list db_1row "ns_pg_bind_1row"  $sql -column_set setid]
	    uplevel 1 [list if 1 $cmd]
#	    db_1row "ns_pg_bind_1row"  $sql -column_set setid
#	    return $setid
	    upvar 1 setid setid
	    return $setid
	}
	"0or1row" { 	    
	    set cmd [list db_0or1row "ns_pg_bind_0or1row" $sql -column_set setid ]
	    uplevel 1 [list if 1 $cmd]
	    upvar 1 setid setid
	    return $setid

	}
	"dml" {

	    db_dml "ns_pg_bnd_dml" $sql
	    #return dummy results
	    set dml_setid  [ns_set create dml_results]
	    ns_set put $dml_setid result 1
	    return $dml_setid
	}

    }
    

    ns_log Error "ns_pg_bind called with cmd = $command"
}
 

proc ns_write  { text } { 
    ns_log notice "ns_write says: [string range $text 0 200] "
    
    append ::pnsd::__http_stream $text
}



#    convert querystring into ns_set
proc ns_getform {} {

    

    set form [ ns_set create cgivars ]
    foreach { pair } [ split $::pnsd::querystring & ] {
	set keyval  [ split $pair = ]
	set key [lindex $keyval 0]
	set val [ncgi::decode [lindex $keyval 1]]
	ns_set put $form $key $val
	
    }



    ns_log notice "ns_getform returning\n [ns_set print $form]"    
    return $form
    
} 



proc ns_sha1 { args } { 
    return [::sha1::sha1 [lindex args 0]]
}




proc ns_schedule_proc { args } { 
    ns_log Debug "(no-op)ns_schedule_proc called with $args "

}

proc ns_returnbadrequest { args } {
    ns_log Debug "(no-op)ns_returnbadrequest called with $args "
}


proc ns_register_adptag { args } {
    ns_log Debug "ns_register_adptag called with $args "

}


#proc ns_register_proc { type path procname } {
proc ns_register_proc { args  } {
#    ns_log Debug "ns_register_proc called with TYPE: $type PATH: $path PROCNAM: $procname "
}

proc ns_unregister_proc { type path args } { 
    set procname ""
    if { [info exists args] } {
	set procname [lindex $args 0]
    }
    ns_log Debug "ns_unregister_proc called with TYPE: $type PATH: $path PROCNAM: $procname  "
}

proc template::filter { args } {

    ns_log Debug "template::filter called with $args "
}


# incomplete
proc ns_eval { args } {
    ns_log notice "ns_eval (disabled) called with $args"
#    uplevel #0 $args 
}

proc ns_returnerror {status msg } {
    set ::pnsd::__http_mime $status
    set ::pnsd::__http_stream $msg

#    ns_log notice "ns_returnerror (disabled) called with $args"
       
}

if {[lsearch [namespace children] ::math] == -1} {
    package require math
#    namespace import ::math::*
}

proc ns_rand { args } { 
    if { [llength $args ] == 0 } {
	return [::math::random]
    } else {
	return [::math::random [lindex $args 0 ] ]
    }
}


#proc ad_return_template 
# {source_type source


proc ns_returnredirect { redir } { 
    ns_log Notice "ns_returnredirect to $redir"
    array set url [uri::split $redir]

    set $::pnsd::url $url(path)
    set $::pnsd::querystring  $url(query)
    rp_handler

}


#overload this..
proc ns_return { status type string } {
    ns_log notice " ns_return: status $status type=$type length=[string length $string] "    

    set ::pnsd::__http_mime $type
    set ::pnsd::__http_stream $string
    
    return 
}



proc ns_normalizepath { path } { 
    ns_log notice " ns_normalizepath called with $path "
    return $path

    #    return [file normalize $path] # nb: this works in 8.4
}


array set mimetypes [list \
     .ai application/postscript \
     .aif audio/aiff \
     .aiff audio/aiff \
     .ani application/x-navi-animation \
     .au audio/basic \
     .avi video/x-msvideo \
     .bin application/x-macbinary \
     .bmp image/bmp \
     .dp application/commonground \
     .exe application/octet-stream \
     .gif image/gif \
     .gz application/x-compressed \
     .hqx application/mac-binhex40 \
     .htm text/html \
     .html text/html \
     .jfif image/jpeg \
     .jpe image/jpeg \
     .jpg image/jpeg \
     .jpeg image/jpeg \
     .map application/x-navimap \
     .mov video/quicktime \
     .mpe video/mpeg \
     .mpeg video/mpeg \
     .mpg video/mpeg \
     .nvd application/x-navidoc \
     .nvm application/x-navimap \
     .pbm image/x-portable-bitmap \
     .pdf application/pdf \
     .pgm image/x-portable-graymap \
     .pic image/pict \
     .pict image/pict \
     .pnm image/x-portable-anymap \
     .ps application/postscript \
     .qt video/quicktime \
     .ras image/x-cmu-raster \
     .rgb image/x-rgb \
     .rtf application/rtf \
     .sit application/x-stuffit \
     .snd audio/basic \
     .stl application/x-navistyle \
     .tar appliation/x-tar \
     .text text/plain \
     .tgz application/x-compressed  \
     .tif image/tiff \
     .tiff image/tiff \
     .txt text/plain \
     .xbm image/x-xbitmap \
     .xpm image/x-xpixmap \
     .wav audio/x-wav \
     .z application/x-compressed \
     .zip application/x-compressed ]


proc ns_guesstype { filename } { 
     global mimetypes
          
     set ext [ file extension $filename ]	

     if { [info exists mimetypes($ext)] } { 
          return $mimetypes($ext)
     }

#take a guess
     return "text/html"
}

proc ns_startcontent {   }  { 
    ns_log warning "ns_startcontent (disabled) called with $args"
    
}

proc ns_returnnotfound {} {
    ns_log warning "(no-op) ns_returnnotfound "
}

proc ns_url2file { url } {
    return [file join [ns_info pageroot] $url]
}



			  
#generic script to call when tracing variables
proc monitor {name args} {
    ns_log Debug "monitor $name"
    upvar $name value
    ns_log Debug "monitor reports '[info level -1]' changes '$name' to '$value'"
}


proc ns_queryget { key } {     
    return [ ns_set get [ ns_getform ] $key ]
} 

proc ns_queryexists { key } {

    # key found?
    if { [ns_set find [ns_getform] $key] != -1 } {
	return 1
    }
    
    #key not found
    return 0
}

proc ns_schedule_daily { args } {
    ns_log warning "[lindex [info level 0] 0] not implemented"
}

#TODO: JS : return something meaningful
proc ns_encodingforcharset { args } {
    ns_log warning "[lindex [info level 0] 0] not implemented"
}


proc ns_mktemp { template args } {

    set temp $template

    # Doesn't exist on Windows ... re-route to Win32 temp file (TODO: This breaks if we want a folder)
    if [string match "/tmp" $template  ] { 
	return ::fileutil::tempfile 
    }

    set i 0

    #provide an upper limit just in case 
    while {[file exists $temp] && $i < 100} {
	regsub {XXXXXX} $template [ns_rand 999999] temp	
	incr i
    }

    return $temp

}

proc ns_tmpnam {args } { 
    ns_log error "ns_tmpnam not implemented"
    return ::fileutil::tempfile 
}


# Only exists in tcllib 1.5.1+ ... so we're implement cut and paste reuse
# ::fileutil::tempfile --
#
#   generate a temporary file name suitable for writing to
#   the file name will be unique, writable and will be in the 
#   appropriate system specific temp directory
#   Code taken from http://mini.net/tcl/772 attributed to
#    Igor Volobouev and anon.
#
# Arguments:
#   prefix     - a prefix for the filename, p
# Results:
#   returns a file name
#
if {[llength [info commands ::fileutil::tempfile]]==0} {
proc ::fileutil::tempfile {{prefix {}}} {
    global  tcl_platform
    switch $tcl_platform(platform) {
	unix {
	    set tmpdir /tmp;   # or even $::env(TMPDIR), at times.
	} macintosh {
	    set tmpdir $env(TRASH_FOLDER)  ;# a better place?
	} default {
s	    set tmpdir [pwd]
	    catch {set tmpdir $env(TMP)}
	    catch {set tmpdir $env(TEMP)}
	}
    }

    set chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    set nrand_chars 10
    set maxtries 10
    set access [list RDWR CREAT EXCL TRUNC]
    set permission 0600
    set channel ""
    set checked_dir_writable 0
    set mypid [pid]
    for {set i 0} {$i < $maxtries} {incr i} {
 	set newname $prefix
 	for {set j 0} {$j < $nrand_chars} {incr j} {
 	    append newname [string index $chars \
		    [expr {([clock clicks] ^ $mypid) % 62}]]
 	}
	set newname [file join $tmpdir $newname]
 	if {[file exists $newname]} {
 	    after 1
 	} else {
 	    if {[catch {open $newname $access $permission} channel]} {
 		if {!$checked_dir_writable} {
 		    set dirname [file dirname $newname]
 		    if {![file writable $dirname]} {
 			error "Directory $dirname is not writable"
 		    }
 		    set checked_dir_writable 1
 		}
 	    } else {
 		# Success
		close $channel
 		return $newname
 	    }
 	}
    }
    if {[string compare $channel ""]} {
 	return -code error "Failed to open a temporary file: $channel"
    } else {
 	return -code error "Failed to find an unused temporary file name"
    }
}
}
