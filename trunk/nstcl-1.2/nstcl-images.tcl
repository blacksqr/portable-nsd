package require nstcl-core

# nstcl-1.2/nstcl-images.tcl
# $Id: nstcl-images.tcl,v 1.3 2003/08/05 01:12:45 cleverly Exp $
#
# nstcl -- AOLserver/OpenNSD routines for tclsh
#
#     Copyright (c) 2000, 2001, 2002 Michael A. Cleverly
#     
#     Permission is hereby granted, free of charge, to any person obtaining
#     a copy of this software and associated documentation files (the
#     "Software"), to deal in the Software without restriction, including
#     without limitation the rights to use, copy, modify, merge, publish,
#     distribute, sublicense, and/or sell copies of the Software, and to
#     permit persons to whom the Software is furnished to do so, subject to
#     the following conditions:
#     
#     The above copyright notice and this permission notice shall be
#     included in all copies or substantial portions of the Software.
#     
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Author Contact Information:
#
#     Michael A. Cleverly
#     1448 W Pebblecreek Dr
#     Layton, Utah 84041-8112
# 
#     michael@cleverly.com
#     http://michael.cleverly.com
#
# nstcl home page: http://nstcl.sourceforge.net

namespace eval ::nstcl { 
    namespace export ns_gifsize \
                     ns_jpegsize \
                     ns_pngsize
}



#
# ns_gifsize
#

::nstcl::ad_proc ::nstcl::ns_gifsize {file} {
    summary "Returns the width and height of a gif file"

    description {
        <p>Returns a list made up of the width and height of the GIF
        <i>file</i>.</p>
    }

    see_also {
        <p>This code is adapted from the Tcl'ers Wiki.  See:
        http://wiki.tcl.tk/758.html</p>
    }
} {
    if {![file exists $file] || ![file readable $file]} {
        error "Could not open file \"$file\""
    }

    set fp [open $file r]
    fconfigure $fp -translation binary
    set gif [read $fp 10]
    close $fp

    if {![regexp {^GIF8[79]a(..)(..)$} $gif match width height]} {
        error "Bad file \"$file\""
    }

    binary scan $width s width
    binary scan $height s height
    return [list $width $height]
}



#
# ns_jpegsize
#

::nstcl::ad_proc ::nstcl::ns_jpegsize {file} {
    summary "Returns the width and height of a jpeg file"

    description {
        <p>Returns a list made up of the width and height of the JPEG
        <i>file</i>.</p>
    }

    see_also {
        <p>This code is adapted from the Tcl'ers Wiki.  See:
        http://wiki.tcl.tk/757.html</p>
    }
} {
    if {![file exists $file] || ![file readable $file]} {
        error "Could not open \"$file\""
    }
 
    set fp [open $file]
    fconfigure $fp -translation binary
    set start_of_image [read $fp 2]
    binary scan $start_of_image H4 file_type

    # make sure a malformed file doesn't get us stuck in an eternal loop
    set num_seeks 0
 
    if {$file_type == "ffd8"} {
        set data ""
        while {![eof $fp] && [incr num_seeks] < 100} {
            while {$data != "ff"} {
                binary scan [read $fp 1] H2 data
            }
 
            while {$data == "ff"} {
                binary scan [read $fp 1] H2 data
            }
            if {$data >= "c0" && $data <= "c3"} {
                binary scan [read $fp 7] x3SS height width
                close $fp
                return [list $width $height]
            }
 
            binary scan [read $fp 2] S offset
            seek $fp [expr $offset - 2] current
        }
    }
 
    close $fp
    error "invalid jpeg file: \"$file\""
}



# 
# ns_pngsize
#

::nstcl::ad_proc ::nstcl::ns_pngsize {file} {
    summary "Returns the width and height of a png file"

    description {
        <p>Returns a list made up of the width and height of the PNG
        <i>file</i>.</p>
    }

    see_also {
        <p>This code is from the Tcl'ers Wiki.  See:
        http://wiki.tcl.tk/759.html</p>
    }
} {
     if {[file size $file] < 33} {
         error "File $file not large enough to contain PNG header"
     }

     set f [open $file r]
     fconfigure $f -encoding binary -translation binary

     # Read PNG file signature
     binary scan [read $f 8] c8 sig
     foreach b1 $sig b2 {-119 80 78 71 13 10 26 10} {
         if {$b1 != $b2} {
             close $f
             error "$file is not a PNG file"
         }
     }

     # Read IHDR chunk signature
     binary scan [read $f 8] c8 sig
     foreach b1 $sig b2 {0 0 0 13 73 72 68 82} {
         if {$b1 != $b2} {
             close $f
             error "$file is missing a leading IHDR chunk"
         }
     }

     # Read off the size of the image
     binary scan [read $f 8] II width height

     close $f
     return [list $width $height]
}



package provide nstcl-images 1.2
