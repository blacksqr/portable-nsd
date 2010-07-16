# Here I'll stub out the query dispatcher procs if
# I'm using a source tree which doesn't have them

proc db_qd_get_fullname {args}   {

    return "pnsd-querydispatcher.skipping.xql"

}

::pnsd::lock_proc db_qd_get_fullname

proc db_qd_replace_sql { statement_name pre_sql } {
    return $pre_sql
}

::pnsd::lock_proc db_qd_replace_sql
