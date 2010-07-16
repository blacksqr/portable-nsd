# Master test script -- 
# John Sequeira
# johnseq@pobox.com
# 10/02


if {[lsearch [namespace children] ::tcltest] == -1} {
    package require tcltest
    namespace import ::tcltest::*
}

set ::tcltest::testSingleFile false
set ::tcltest::testsDirectory [file dir [info script]]

#puts [::tcltest::getMatchingFiles]  ; exit
foreach file [::tcltest::getMatchingFiles] {
    puts stdout $file
    if {[catch {source $file} msg]} {
        puts stdout $msg
    }
}

::tclttest::cleanupTests 1
return