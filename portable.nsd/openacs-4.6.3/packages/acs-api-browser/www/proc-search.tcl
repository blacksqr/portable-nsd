# acs-api-browser/www/proc-search.tcl

ad_page_contract {
    Searches for procedures with containing query_string
    if lucky redirects to best match
    Weight the different hits with the propper weights

    Shows a list of returned procs with links to proc-view

    Note: api documentation information taken from nsv array

    @author Todd Nightingale (tnight@arsdigita.com)
    @creation-date Jul 14, 2000
    @cvs-id $Id: proc-search.tcl,v 1.6 2002/09/24 09:13:48 jeffd Exp $
} {
    {name_weight:optional 0}
    {doc_weight:optional 0}
    {param_weight:optional 0}
    {source_weight:optional 0}
    {search_type:optional 0}
    query_string
} -properties {
    title:onevalue
    context:onevalue
    name_weight:onevalue
    doc_weight:onevalue
    param_weight:onevalue
    source_weight:onevalue
    query_string:onevalue
    results:multirow
}

##########################################################
##  Begin Page

set quick_view [string equal $search_type "Feeling Lucky"]

#########################
## Optimizes quick search
if {$quick_view && [nsv_exists api_proc_doc $query_string]} {
    ad_returnredirect [api_proc_url $query_string]
    ad_script_abort
}

###########################
# No weighting use default:
if { ($name_weight == 0) && ($doc_weight == 0) && ($param_weight == 0) && ($source_weight ==0) } {
    set name_weight 1
}

set counter 0
set matches ""

# place a [list proc_name score positionals] into matches for every proc
foreach proc [nsv_array names api_proc_doc] { 

    set score 0
    array set doc_elements [nsv_get api_proc_doc $proc]

    ###############
    ## Name Search:
    ###############
    if {$name_weight} {
        # JCD: this was a little perverse since exact matches were
        # actually worth less than matches in the name (if there were
        # 2 or more, which happens with namespaces) so I doubled the
        # value of an exact match.

        ##Exact match:
        if {[string tolower $query_string] == [string tolower $proc]} {
            incr score [expr $name_weight * 2]
        }
        incr score [expr $name_weight * [ad_keywords_score $query_string $proc]] 
    }
   
    ################
    ## Param Search:
    ################
    if {$param_weight} {
        incr score [expr $param_weight * [ad_keywords_score $query_string "$doc_elements(positionals) $doc_elements(switches)"]]
    }
    

    ##############
    ## Doc Search:
    ##############
    if {$doc_weight} {
        
        set doc_string "[lindex $doc_elements(main) 0]"
        if [info exists doc_elements(param)] {
            foreach parameter $doc_elements(param) {
                append doc_string " $parameter"
            }
        }
        if [info exists doc_elements(return)] {
            append doc_string " $doc_elements(return)"
        }
        incr score [expr $doc_weight * [ad_keywords_score $query_string $doc_string]]
        
    }
    
    #################
    ## Source Search:
    #################
    if {$source_weight} {
        if {![catch {set source [info body $proc]}]} {
            incr score [expr $source_weight * [ad_keywords_score $query_string $source]] 
        }    
    }

    #####
    ## Place Needed info in matches
    if {$score} {
        if {$doc_elements(varargs_p)} { 
            set args "$doc_elements(positionals) \[&nbsp;args...&nbsp;\]"
        } else { 
            set args $doc_elements(positionals)
        }   
        lappend matches [list $proc $score $args]
    }
}

set matches [lsort -command ad_sort_by_score_proc $matches]

if {$quick_view && ![empty_string_p $matches]} {
    ad_returnredirect [api_proc_url [lindex [lindex $matches 0] 0]]
    ad_script_abort
}

set title "Procedure Search for: \"$query_string\""
set context [list "Search: $query_string"]

multirow create results score proc args url

foreach output $matches {
    incr counter
    set proc  [lindex $output 0]    
    set score [lindex $output 1]
    set args  [lindex $output 2]
    set url   [api_proc_url $proc]
    multirow append results $score $proc $args $url
}










