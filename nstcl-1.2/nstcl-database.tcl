package require nstcl-core
package require nstcl-fwdcompat
package require nstcl-nssets

# nstcl-1.2/nstcl-database.tcl
# $Id: nstcl-database.tcl,v 1.10 2003/08/05 01:12:45 cleverly Exp $
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
    namespace export ns_db \
                     database_to_tcl_list \
                     database_to_tcl_string \
                     database_to_tcl_string_or_null \
                     db_0or1row \
                     db_1row \
                     db_abort_transaction \
                     db_abort_transaction_p \
                     db_continue_transaction \
                     db_dml \
                     db_foreach \
                     db_list \
                     db_list_of_lists \
                     db_multirow \
                     db_quote \
                     db_string \
                     db_transaction \
                     DoubleApos \
                     ns_dbquotename \
                     ns_dbquotevalue \
                     ns_localsqltimestamp
}

namespace eval ::nstcl::database {
    variable raw 0
    variable errorInfo ""

    variable pools
    array set pools {}

    variable transaction
    array set transaction {
        depth 0 abort_p 0 dbhandles {} available {} errorInfo "" errorCode ""
    }

    variable commands
    array set commands {
        bindrow       { handle 1 min 1 max 1 syntax "dbhandle" }
        bouncepool    { handle 1 min 1 max 1 syntax "dbhandle" }
        cancel        { handle 1 min 1 max 1 syntax "dbhandle" }
        close         { handle 1 min 1 max 1 syntax "dbhandle" }
        connected     { handle 1 min 1 max 1 syntax "dbhandle" }
        datasource    { handle 1 min 1 max 1 syntax "dbhandle" }
        dbtype        { handle 1 min 1 max 1 syntax "dbhandle" }
        dml           { handle 1 min 2 max 2 syntax "dbhandle sql" }
        driver        { handle 1 min 1 max 1 syntax "dbhandle" }
        exception     { handle 1 min 1 max 1 syntax "dbhandle" }
        exec          { handle 1 min 2 max 2 syntax "dbhandle sql" }
        flush         { handle 1 min 1 max 1 syntax "dbhandle" }
        gethandle     { handle 0 min 0 max 4 syntax "?-timeout timeout?\
                                                     \[poolname\] nhandles" }
        getrow        { handle 1 min 2 max 2 syntax "dbhandle setId" }
        open          { handle 0 min 4 max 4 syntax "driver datasource user\
                                                     password" }
        poolname      { handle 1 min 1 max 1 syntax "dbhandle" }
        pools         { handle 0 min 0 max 0 syntax "" }
        releasehandle { handle 1 min 1 max 1 syntax "dbhandle" }
        1row          { handle 1 min 2 max 2 syntax "dbhandle sql" }
        0or1row       { handle 1 min 2 max 2 syntax "dbhandle sql" }
        password      { handle 1 min 1 max 1 syntax "dbhandle" }
        select        { handle 1 min 2 max 2 syntax "dbhandle sql" }
        setexception  { handle 1 min 3 max 3 syntax "dbhandle code message" }
        user          { handle 1 min 1 max 1 syntax "dbhandle" }
        verbose       { handle 1 min 1 max 2 syntax "dbhandle ?on | off?" }
    }

    variable api_commands
    array set api_commands {
        db_dml           {min 2 max 2 syntax "statement_name sql"} 
        db_list          {min 2 max 2 syntax "statement_name sql"}
        db_list_of_lists {min 2 max 2 syntax "statement_name sql"}
        db_string        {min 2 max 4 syntax "statement_name sql\
                                              ?-default default?"}
        db_0or1row       {min 2 max 4 syntax "statement_name sql\
                                              ?-column_array array |\
                                               -column_set set?"} 
        db_1row          {min 2 max 4 syntax "statement_name sql\
                                              ?-column_array array |\
                                               -column_set set?"}
        db_foreach       {min 3 max 7 syntax "statement_name sql\
                                              ?-column_array array |\
                                               -column_set set?\
                                              code\
                                              ?if_no_rows code | else code?"}
        db_multirow      {min 3 max 10 syntax "?-local? ?-append? ?-extend\
                                              extendVars?\
                                              var_name statement_name sql\
                                              ?code?\
                                              ?if_no_rows code | else code?"}

        db_abort_transaction    {min 0 max 0 syntax ""}
        db_abort_transaction_p  {min 0 max 0 syntax ""}
        db_continue_transaction {min 0 max 0 syntax ""}
        db_transaction          {min 0 max 0 syntax ""}
    }
}



#
# ns_db
#

::nstcl::ad_proc ::nstcl::ns_db {args} {
    syntax "<command>::nstcl::ns_db</command> <m>option</m> ?<m>arg</m>? ?<m>...</m>?"
    summary "Access a database"
    description {
        <p>This command allows access to one or more database(s).</p>

        <commandlist{>
            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>bindrow</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns an <b>ns_set</b> whose keys correspond to the
                    column names returned from the database after an
                    SQL statement was executed from <b>ns_db exec</b>.

                    If the result from <b>ns_db exec</b> was not
                    NS_ROWS (i.e. the SQL was a DDL or DML statement, and
                    not a query), then an error will be thrown.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>bouncepool</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Marks all of the handle(s) from the pool that
                    <b>dbhandle</b> is a member of as stale, causing
                    the database connection(s) to be reset upon subsequent
                    use.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>cancel</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Cancels the current operation (essentially the same
                    thing as <b>ns_db flush <i>dbhandle</i></b>).
                </desc}>
            </commanddef}>
             
            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>close</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Close <i>dbhandle</i>'s connection to the database.
                    This should only be used with handles obtained from
                    <b>ns_db open</b>.
                </desc}>
            </commanddef}>
             
            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>connected</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    As currently implemented, a boolean value of 1 is
                    always returned.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>datasource</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the data source for the database pool that
                    <i>dbhandle</i> is a member of.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>dbtype</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the type of database that <i>dbhandle</i> is
                    connected to (i.e. "Oracle", "Postgres", etc.)
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>dml</method> <m>dbhandle</m>
                    <m>sql</m>
                </command}>
                <desc{>
                    Execute a DML (or DDL) <i>sql</i> statement (such as
                    an "insert", "update", "create table", etc.)
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>driver</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the name of the database driver of the handle.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>exception</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the most recent error message (if any) for
                    <i>dbhandle</i>.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>exec</method> <m>dbhandle</m> 
                    <m>sql</m>
                </command}>
                <desc{>
                    <p>Executes a <i>sql</i> statement, and returns
                    NS_ROWS (if the <i>sql</i> statement was a query), or
                    NS_DML (if the <i>sql</i> statement was a DML or DDL
                    command).</p>
                     
                    <p>If the <i>sql</i> statement raises an exception
                    in the database, then an error is thrown.</p>
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>flush</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Flushes the pending results from the previous
                    SQL select statement (freeing you from having
                    to call <b>ns_db getrow</b> repeatedly to cycle
                    through the entire result set).
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>gethandle</method> 
                    ?<m>-timeout timeout</m>? ?<m>poolname</m>? 
                    ?<m>nhandles</m>?
                </command}>
                <desc{>
                    <p><b>ns_db gethandle</b> returns the specified number 
                    of handles (in the form of a Tcl list) for the given
                    pool.</p>

                   <p>The <i>-timeout timeout</i> switch is included for
                   source compatability with AOLserver.  However, since
                   <b>nstcl</b> is designed to be used in a single
                   threaded enviroment, there is nothing to wait on, and
                   this switch is ignored.</p>

                    <p>If <i>poolname</i> is not specified, handle(s) from
                    the default pool will be returned.  In <b>nstcl</b> the
                    "default pool" is either the pool explicity set as the
                    default with a call to <b>::nstcl::set_default_pool</b>,
                    or the only pool (if only a single pool has been defined).
                    There is no default pool if multiple pools have been
                    created and none of them have been set as the default.
                    In this case calling <b>ns_db gethandle</b> without
                    a poolname results in an error.</p>

                    <p>The <i>nhandles</i> defaults to one.  An error
                    will be thrown if you attempt to allocate more 
                    handles than have been defined for a pool.</p>

                    <p>Each handle returned by <b>ns_db gethandle</b> needs
                    (eventually) to be released with an explicit call to
                    <b>ns_db releasehandle</b>.  Until you have released
                    all the handles you have from a particular pool you 
                    will not be able to get any more handles from the 
                    same pool.</p>
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>getrow</method> <m>dbhandle</m>
                    <m>setId</m>
                </command}>
                <desc{>
                    <p>Fetches the next row from the database after an initial
                    call to <b>ns_db select</b> and places it in the 
                    <i>setId</i> <b>ns_set</b> (which should normally
                    be the same set returned by <b>ns_db select</b>).</p>

                    <p>As long as there is at least one row remaining
                    in the databases result set <b>ns_db getrow</b> will
                    return 1.  When the last row is fetched, 
                    <b>ns_db getrow</b> will return 0, and any subsequent
                    call (barring another query first) will raise an error.</p>
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>open</method> <m>driver</m>
                    <m>datasource</m> <m>user</m> <m>password</m>
                </command}>
                <desc{>
                    This opens a single connection to a database outside
                    of the handle(s) in the defined database pool(s).  A 
                    database handle returned if the connection is opened
                    successfully, or any error in opening the connection
                    is propogated.
                </desc}> 
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>poolname</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the name of the database pool that <i>dbhandle</i>
                    came from.
                </desc}>
            </commanddef}>
 
            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>pools</method>
                </command}>
                <desc{>
                    Returns a list of all of the defined database pools.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>releasehandle</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Places <i>dbhandle</i> back into the list of available
                    database handles of the pool it belongs to.  Unlike
                    AOLserver, which has a concept of the lifetime of a 
                    connection, and will automatically release allocated
                    handles at the end of the connection, with <b>nstcl</b>
                    you must explicitly release every handle you use when
                    you are done with it.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>1row</method> <m>dbhandle</m> 
                    <m>SQL</m>
                </command}>
                <desc{>
                    Assuming that <i>SQL</i> is a query that will return
                    exactly one row (an error is thrown if this assumption
                    turns out to be false), the row is returned as an
                    <b>ns_set</b>.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>0or1row</method> <m>dbhandle</m>
                    <m>SQL</m>
                </command}>
                <desc{>
                    Assuming that <i>SQL</i> is a query that will return
                    at most one row (an error is thrown if this assumption
                    turns out to be false), either an empty string (when
                    the database returns no rows), or an <b>ns_set</b>
                    is returned (when the database returns one row).
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>password</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the database password specified when the pool 
                    (of which <i>dbhandle</i> is a member of) was configured. 
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>select</method> <m>dbhandle</m>
                    <m>SQL</m>
                </command}>
                <desc{>
                    Executes the <i>SQL</i> query on the database and
                    returns an <b>ns_set</b> whose keys correspond to the
                    column names selected.  Use <b>ns_db getrow</b> to
                    retrieve the actual result rows.  You cannot call
                    <b>ns_db select</b> a second time (with the same
                    <i>dbhandle</i>) until you have retrieved all the 
                    rows or called <b>ns_db flush</b>.
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>setexception</method> <m>dbhandle</m>
                    <m>code</m> <m>message</m>
                </command}>
                <desc{>
                    Returns the specified status code and message. 
                </desc}>
            </commanddef}>

            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>user</method> <m>dbhandle</m>
                </command}>
                <desc{>
                    Returns the database username specified when the pool 
                    (of which <i>dbhandle</i> is a member of) was configured. 
                </desc}>
            </commanddef}>
          
            <commanddef{>
                <command{>
                    <b>ns_db</b> <method>verbose</method> <m>dbhandle</m>
                    ?<m>on | off</m>?
                </command}>
                <desc{>
                    Queries (or changes) the verbosity setting of 
                    <i>dbhandle</i>'s database pool.
                </desc}>
            </commanddef}>
        </commandlist}>
    }
} {
    upvar 0 ::nstcl::database::commands commands
    set argc [llength $args]

    if {$argc == 0} {
        return -code error "wrong # of args: should be \"ns_db command ?args?\""
    } 

    set cmd  [lindex $args 0]
    set args [lrange $args 1 end]
    incr argc -1

    if {![info exists commands($cmd)]} {
        return -code error "unknown command \"$cmd\": should be one of\
            [join [lrange [lsort [array names commands]] 0 end-1] ", "] or\
            [lindex [lsort [array names commands]] end]"
    }


    # valid handle?
    array set command $commands($cmd)
    if {$command(handle) && $argc >= 0} {
        set db [lindex $args 0]


        # 30 Oct 2002:
        #   
        # the check below had the following additional checks between
        # the first & last check:
        #
        #    [info exists $db] == 0 || [interp alias {} $db] == "" ||
        #
        # removed because the first check ensures well-formedness,
        # and the last check ensures that it's an allocated handle.
        #
        # if it's well formed, but doesn't exist, then the later check
        # will thrown an error anyway... if someone is monkeying around
        # they could just redefine the code in this proc all together anyway.

        if {[regexp {^::nstcl::database::nsdb-[^-]+-[1-9][0-9]*$} $db] == 0 ||
            [$db allocated_p] == 0} {
            return -code error "invalid database id: \"$db\""
        }
    }

    if {$argc < $command(min) || $argc > $command(max)} {
        if {$command(syntax) != ""} {
            return -code error "wrong # of args: should be \"ns_db $cmd\
                $command(syntax)\""
        } else {
            return -code error "wrong # of args: should be \"ns_db $cmd\""
        }
    }

    if {$command(syntax) == "dbhandle sql"} {
        $db sql [lindex $args 1]
    }

    switch -- $argc {
        0 { set error_p [catch { ::nstcl::database::$cmd } result] }
 
        1 { set error_p [catch { ::nstcl::database::$cmd \
                                        [lindex $args 0] } result] 
          }

        2 { set error_p [catch { ::nstcl::database::$cmd \
                                        [lindex $args 0] \
                                        [lindex $args 1] } result]
          }

        3 { set error_p [catch { ::nstcl::database::$cmd \
                                        [lindex $args 0] \
                                        [lindex $args 1] \
                                        [lindex $args 2] } result]
          }

        4 { set error_p [catch { ::nstcl::database::$cmd \
                                        [lindex $args 0] \
                                        [lindex $args 1] \
                                        [lindex $args 2] \
                                        [lindex $args 3] } result]
          }
    }

    if {$error_p} {
        set ::nstcl::database::errorInfo $::errorInfo
        return -code error $result
    } else {
        return $result
    }
}



#
# load_driver
#

::nstcl::ad_proc ::nstcl::database::load_driver {driver args} {
    syntax "<command>::nstcl::load_driver</command> <m>driver</m> ?<m>shared_lib</m>?  ?<m>packageName</m>?"

    summary "Load a database driver"
    description {
        <p>This command loads a supported database driver, which, in turn,
        requires a working Tcl database extension.</p>

        <p>As of this version, <b>nstcl</b> supports the following databases:
        <i>Oracle</i> (with <b>oratcl</b>), 
        <i>Postgres</i> (with <b>libpgtcl</b>),
        <i>Solid</i> (with <b>soltcl</b>), 
        <i>Sybase</i> (with <b>sybtcl</b>),
        <i>Sqlite</i> (with <b>tclsqlite</b>),
        <i>MySQL</i> (with <b>libmysqltcl</b>),
        <i>ODBC</i> (with <b>TclODBC</b>).  
        Also provided for testing 
        are two pseudo-drivers, <i>devnull</i> and <i>echo</i>, which probably
        won't be of general interest.</p>

        <p>This command is <b>not</b> exported from the <b>nstcl</b> 
        namespace.</p>
    }

    other_sections implementing_a_new_driver

    implementing_a_new_driver {
        <p>To implement a new database driver for <b>nstcl</b>, you need
        to create a mere 8 wrapper functions (<i>load_driver</i>, 
        <i>bindrow</i>, <i>close</i>, <i>dbtype</i>, <i>exec</i>,
        <i>flush</i>, <i>gethandle</i>, and <i>getrow</i>) around 
        the Tcl extension for the new database.  Consult the existing
        drivers to see how they are implemented.</p>
    }

    see_also configure_pool
} {
    package require nstcl-database-$driver
    ::nstcl::database::${driver}::load_driver $args
}

interp alias {} ::nstcl::load_driver {} ::nstcl::database::load_driver



#
# configure_pool
#

::nstcl::ad_proc ::nstcl::database::configure_pool {-immediately:boolean
                                                    -default:boolean
                                                    driver poolname connections
                                                    {datasource ""} {user ""} 
                                                    {password ""} {verbose 0}} {
    syntax "<command>::nstcl::configure_pool</command> ?<m>-immediately</m>? ?<m>-default</m>? <m>driver</m> <m>poolname</m> <m>connections</m> ?<m>datasource</m>? ?<m>username</m>? ?<m>password</m>? ?<m>verbose</m>?"

    summary "Configure a database pool"

    description {
        <p>This command is used to configure a database pool.  
        <i>connections</i> is the number of simultaneous connections to the
        database than can be made.  <i>verbose</i> should be an integer,
        either 0 or 1, and defaults to 0.</p>

        <p><i>username</i> and <i>password</i> are optional depending on
        which database driver you are using, and how you have your 
        database configured.  The format of the <i>datasource</i>
        parameter differs depending on the database driver you are using.</p>

        <p>This command is <b>not</b> exported from the <b>nstcl</b> 
        namespace.</p>
    }

    optional_switches {
        <dle>
        <dt><option>-immediately</option></dt>
        <dd>Causes a connection to be made to the database immediately,
        causing an immediate error if the connection information (datasource,
        username, password, etc.) is not correct.  Otherwise an error
        will only be generated the first time that <b>ns_db gethandle</b>
        is called (or one of the <i>database_api</i> commands is used).</dd>

        <dt><option>-default</option></dt>
        <dd>Sets this pool as the default pool (when multiple pools are
        defined).</dd>
        </dle>
    }

    other_sections {
        oracle_datasource
        postgres_datasource
        solid_datasource
        sybase_datasource
        sqlite_datasource
        mysql_datasource
        odbc_datasource
    }

    oracle_datasource {
        The Oracle driver uses a connection string made up of 
        $username/$password, if <i>datasource</i> is the empty string, or
        $username/$password@$datasource if not.
    }

    postgres_datasource {
        The Postgres driver's <i>datasource</i> is a string in the form
        of: <i>host</i>:<i>port</i>:<i>dbname</i>.  Depending on your
        Postgres configuration, some of these, or the entire data source
        string, may be optional.
    }

    solid_datasource {
        The Solid driver's <i>datasource</i> is a string made up of:
        "tcp <i>host</i> <i>port</i>".
    }

    sybase_datasource {
        The Sybase driver's <i>datasource</i> is a string made up of the
        form: <i>server</i>:<i>database</i>:<i>application</i>.  If 
        the datasource is not provided the value of the <i>DSQUERY</i>
        enviroment variable will be used (if defined), or the literal string
        <i>SYBASE</i> if it is not.  Depending on your configuration,
        the <i>datasource</i> is optional.
    }

    sqlite_datasource {
        The SQLite driver's <i>datasource</i> is the path to the SQLite
        database file.
    }

    mysql_datasource {
        The MySQL driver's <i>datasource</i> is a string in the form
        of: <i>host</i>:<i>port</i>:<i>database</i>.  Depending on your
        MySQL configuration, some of these, or the entire data source
        string, may be optional.
    }

    odbc_datasource {
        The ODBC driver's <i>datasource</i> should be a string 
        that is a valid ODBC DSN.
    }

    see_also "load_driver, set_default_pool, database_api"
} {
    variable pools

    if {[lsearch -exact [namespace children ::nstcl::database] \
        ::nstcl::database::$driver] == -1} {
        error "Driver \"$driver\" not loaded"
    }

    if {![string is word -strict $poolname]} {
        error "Invalid poolname \"$poolname\""
    }

    if {[info exists pools($poolname)]} {
        error "Pool \"$poolname\" already defined"
    }

    if {![string is integer -strict $connections]} {
        error "expected integer but got \"$connections\""
    }

    if {$connections <= 0} {
        error "invalid number of connections \"$connections\"; should be > 0"
    }

    if {![string is boolean -strict $verbose]} {
        error "expected boolean value but got \"$verbose\""
    }

    set pools($poolname,user) $user
    set pools($poolname,pass) $password
    set pools($poolname,datasource) $datasource
    set pools($poolname,driver) $driver
    set pools($poolname,verbose) [string is true $verbose]
    lappend pools(pools) $poolname


    # setup handles 
    for {set i 1} {$i <= $connections} {incr i} {
        set var nsdb-${poolname}-$i
        variable $var
        upvar 0 $var handle

        set handle [::nstcl::ns_set create $var]
        ::nstcl::ns_set put $handle driver $driver
        ::nstcl::ns_set put $handle pool   $poolname
        ::nstcl::ns_set put $handle allocated_p 0
        ::nstcl::ns_set put $handle bounced_p   0
        ::nstcl::ns_set put $handle verbose_p $pools($poolname,verbose)
        ::nstcl::ns_set put $handle bind_vars \
            ::nstcl::database::pseudo_bind_variables

        interp alias {} ::nstcl::database::$var \
            {} ::nstcl::database::handle $driver $poolname $var

        lappend pools($poolname,handles) ::nstcl::database::$var
    }

    if {$immediately_p} {
        ::nstcl::ns_db releasehandle [::nstcl::ns_db gethandle $poolname]
    }

    if {$default_p} {
        ::nstcl::set_default_pool $poolname
    }
}

interp alias {} ::nstcl::configure_pool {} ::nstcl::database::configure_pool



#
# set_default_pool
#

::nstcl::ad_proc ::nstcl::database::set_default_pool {pool} {
    syntax "<command>::nstcl::set_default_pool</command> <m>poolname</m>"
    summary "Sets a default database pool"

    description {
        <p>This command sets the default database pool for the database api
        commands, and for <b>ns_db gethandle</b> when a pool is not 
        explicitly requested by name.</p>

        <p>This command is <b>not</b> exported from the <b>nstcl</b> 
        namespace.</p>
    }

    see_also "configure_pool, database_api"
} {
    variable pools

    if {[lsearch $pools(pools) $pool] == -1} {
        error "Pool \"$pool\" has not been configured yet."
    }

    set pools(default_pool) $pool
}

interp alias {} ::nstcl::set_default_pool {} ::nstcl::database::set_default_pool


#
# handle
#

::nstcl::ad_proc -private ::nstcl::database::handle {driver pool var args} {
    variable $var
    set setId [set $var]

    switch [llength $args] {
        0 { return $setId }
        1 { return [::nstcl::ns_set get    $setId [lindex $args 0]] }
        2 { return [::nstcl::ns_set update $setId [lindex $args 0] \
                                                  [lindex $args 1]] }
    }

    error "Too many arguments to [lindex [info level 0] 0]"
}



#
# op_failed
#

::nstcl::ad_proc -private ::nstcl::database::op_failed {-sql:optional
                                                        dbhandle cmd 
                                                        exception message} {

    if {[info exists sql]} {
        set message [string trimright $message .]:
        append message "\n-- begin sql"
        append message "\n$sql"
        append message "\n-- end sql"
    }

    ::nstcl::database::setexception $dbhandle $exception $message

    return -code error "Database operation \"$cmd\" failed\
        (exception $exception, \"$message\")"
}



#
# ns_db gethandle
#

::nstcl::ad_proc -private ::nstcl::database::gethandle {{-timeout 0} {pool ""} 
                                                       {nhandles 1}} {
    variable pools

    if {![string is integer -strict $timeout]} {
        error "expected integer but got \"$timeout\""
    }

    if {![string is integer -strict $nhandles]} {
        error "expected integer but got \"$nhandles\""
    }

    if {$nhandles < 1} {
        error "invalid nhandles \"$nhandles\": should be greater than 0."
    }

    if {[string equal "" $pool]} {
        if {[info exists pools(default_pool)]} {
            set pool $pools(default_pool)
        } else {
            set non_raw_pools [::nstcl::database::pools]
            if {[llength $non_raw_pools] == 1} {
                set pool $non_raw_pools
            }
        }
    }

    if {![info exists pools($pool,handles)]} {
        error "do not have access to pool: \"$pool\""
    }
   
    set handles {} 
    foreach handle $pools($pool,handles) {
        if {![$handle allocated_p]} {
            lappend handles $handle
        }
    }

    if {$nhandles > [llength $handles]} {
        error "could not allocate $nhandles handle(s) from pool \"$pool\""
    }

    set handles [lrange $handles 0 [expr {$nhandles - 1}]]
    foreach handle $handles {
        $handle allocated_p 1
        if {[$handle conn] == ""} {
            ::nstcl::database::[$handle driver]::gethandle $pool $handle
        }
    }

    return $handles
}



#
# ns_db driver
#

::nstcl::ad_proc -private ::nstcl::database::driver {dbhandle} {
    return [$dbhandle driver]
}



#
# ns_db dbtype
#

::nstcl::ad_proc -private ::nstcl::database::dbtype {dbhandle} {
    return [::nstcl::database::[$dbhandle driver]::dbtype]
}



#
# ns_db datasource
#

::nstcl::ad_proc -private ::nstcl::database::datasource {dbhandle} {
    return $::nstcl::database::pools([$dbhandle pool],datasource)
}



#
# ns_db user
#

::nstcl::ad_proc -private ::nstcl::database::user {dbhandle} {
    return $::nstcl::database::pools([$dbhandle pool],user)
}



#
# ns_db password
#

::nstcl::ad_proc -private ::nstcl::database::password {dbhandle} {
    return $::nstcl::database::pools([$dbhandle pool],pass)
}



#
# ns_db poolname
#

::nstcl::ad_proc -private ::nstcl::database::poolname {dbhandle} {
    return [$dbhandle pool]
}



#
# ns_db pools
#

::nstcl::ad_proc -private ::nstcl::database::pools {} {
    set pools {}
    foreach pool $::nstcl::database::pools(pools) {
        if {![regexp {^raw@} $pool]} {
            lappend pools $pool
        } 
    }
    return $pools
}



#
# ns_db dml
#

::nstcl::ad_proc -private ::nstcl::database::dml {dbhandle sql} {
    ::nstcl::database::[$dbhandle driver]::exec $dbhandle $sql
    if {[$dbhandle mode] != "NS_DML"} {
        catch { ::nstcl::database::[$dbhandle driver]::flush $dbhandle }
        return -code error "Database operation \"dml\" failed\
            (exception NSDB, \"Query was not a DML or DDL command.\")" 
    }
}



#
# ns_db exec
#

::nstcl::ad_proc -private ::nstcl::database::exec {dbhandle sql} {
    return [::nstcl::database::[$dbhandle driver]::exec $dbhandle $sql]
}



#
# ns_db flush
#

::nstcl::ad_proc -private ::nstcl::database::flush {dbhandle} {
    return [::nstcl::database::[$dbhandle driver]::flush $dbhandle]
}



#
# ns_db cancel
#

::nstcl::ad_proc -private ::nstcl::database::cancel {dbhandle} {
    return [::nstcl::database::[$dbhandle driver]::flush $dbhandle]
}



#
# ns_db setexception
#

::nstcl::ad_proc -private ::nstcl::database::setexception {dbhandle code 
                                                           message} {
    $dbhandle exception_code $code
    $dbhandle exception_text $message
}



#
# ns_db exception
#

::nstcl::ad_proc -private ::nstcl::database::exception {dbhandle} {
    return [list [$dbhandle exception_code] [$dbhandle exception_text]]
}



#
# ns_db bouncepool
#

::nstcl::ad_proc -private ::nstcl::database::bouncepool {dbhandle} {
    variable pools
    foreach handle $pools([$dbhandle pool],handles) {
        $handle bounced_p 1
        if {![$handle allocated_p]} {
            ::nstcl::database::releasehandle $handle
        }
    }
}


#
# ns_db bindrow
#

::nstcl::ad_proc -private ::nstcl::database::bindrow {dbhandle} {
    if {[$dbhandle mode] != "NS_ROWS"} {
        error "Database operation \"bindrow\" failed."
    }
    return [::nstcl::database::[$dbhandle driver]::bindrow $dbhandle]
}



#
# ns_db getrow
#

::nstcl::ad_proc -private ::nstcl::database::getrow {dbhandle setId} {
    return [::nstcl::database::[$dbhandle driver]::getrow $dbhandle $setId]
}



#
# ns_db select
#

::nstcl::ad_proc -private ::nstcl::database::select {dbhandle sql 
                                                     {command select}} {
    ::nstcl::database::exec $dbhandle $sql
    if {[$dbhandle mode] != "NS_ROWS"} {
        ::nstcl::database::flush $dbhandle
        ::nstcl::database::op_failed -sql $sql $dbhandle $command \
            NSDB "Query was not a statement returning rows."
    } else {
        return [::nstcl::database::bindrow $dbhandle]
    }
}



#
# ns_db 1row
#

::nstcl::ad_proc -private ::nstcl::database::1row {dbhandle sql} {
    set rc [catch { ::nstcl::database::0or1row $dbhandle $sql } result]
    
    if {$rc == 1} {
        ::nstcl::database::op_failed -sql $sql $dbhandle 1row NSINT \
            "Query returned more than one row."
    } else {
        if {$result == ""} {
            ::nstcl::database::op_failed -sql $sql $dbhandle 1row NSINT \
                "Query did not return a row."
        } else {
            return $result
        }
    }
}



#
# ns_db 0or1row
#

::nstcl::ad_proc ::nstcl::database::0or1row {dbhandle sql} {
    set cursor [::nstcl::database::select $dbhandle $sql 0or1row]
    set num_rows 0

    while {[::nstcl::ns_db getrow $dbhandle $cursor] && [incr num_rows]} {
        if {$num_rows == 2} then break
    }

    switch $num_rows {
        0 { return }
        1 { return $cursor }
        default { 
            if {[info exists ::nstcl::debug_p] && $::nstcl::debug_p} {
                ::nstcl::ns_set print [$dbhandle]
            }

            ::nstcl::ns_set free $cursor
            ::nstcl::database::flush $dbhandle
            ::nstcl::database::op_failed -sql $sql $dbhandle 0or1row NSINT \
                "Query returned more than one row."
        }
    }
}



#
# ns_db connected
#

::nstcl::ad_proc -private ::nstcl::database::connected {dbhandle} {
    return 1
}



#
# ns_db verbose
#

::nstcl::ad_proc -private ::nstcl::database::verbose {dbhandle args} {
    if {[llength $args] == 0} {
        return [$dbhandle verbose_p]
    } 

    set toggle [lindex $args 0]
    if {![string is boolean -strict $toggle]} {
        error "expected boolean but got \"$toggle\""
    }

    return [$dbhandle verbose_p [string is true $toggle]]
}



#
# ns_db open
#

::nstcl::ad_proc -private ::nstcl::database::open {driver datasource
                                                   user password} {
    variable pools
    variable raw

    if {[lsearch -exact [namespace children ::nstcl::database] \
        ::nstcl::database::$driver] == -1} {
        error "Driver \"$driver\" not loaded"
    }

    set poolname "raw@[incr raw]" 
    set pools($poolname,user) $user
    set pools($poolname,pass) $password
    set pools($poolname,datasource) $datasource
    set pools($poolname,driver) $driver
    set pools($poolname,verbose) 0
    lappend pools(pools) $poolname


    # setup handles 
    set var nsdb-${poolname}-1
    variable $var
    upvar 0 $var handle

    set handle [::nstcl::ns_set create $var]
    ::nstcl::ns_set put $handle driver $driver
    ::nstcl::ns_set put $handle pool   $poolname
    ::nstcl::ns_set put $handle allocated_p 0
    ::nstcl::ns_set put $handle bounced_p   0
    ::nstcl::ns_set put $handle verbose_p   0

    interp alias {} ::nstcl::database::$var \
        {} ::nstcl::database::handle $driver $poolname $var

    set pools($poolname,handles) ::nstcl::database::$var
    return [::nstcl::database::gethandle $poolname]
}



#
# ns_db close
#

::nstcl::ad_proc ::nstcl::database::close {dbhandle} {
    if {[$dbhandle allocated_p]} {
        catch { ::nstcl::database::[$dbhandle driver]::flush $dbhandle }
        ::nstcl::database::releasehandle $dbhandle
    }
    ::nstcl::database::[$dbhandle driver]::close $dbhandle
}



#
# ns_db releasehandle
#

::nstcl::ad_proc ::nstcl::database::releasehandle {dbhandle} {
    $dbhandle allocated_p 0
    if {[$dbhandle bounced_p]} {
        ::nstcl::database::[$dbhandle driver]::close $dbhandle
        $dbhandle bounced_p 0
    }
}



# 
# ns_dbquotevalue
#

::nstcl::ad_proc ::nstcl::ns_dbquotevalue {value {type text}} {
    summary "Prepares a value string for inclusion in an SQL statement"

    syntax "<command>::nstcl::ns_dbquotevalue</command> <m>value</m> ?<m>type</m>?"
 
    description {
        <p>Prepares a string for inclusion in an SQL statement.  The
        default <i>type</i> is "text".  Values of a numeric type 
        ("decimal", "double", "integer", "int", "real", "smallint", 
        "bigint", "bit", "float", "numeric", or "tinyint") are left 
        alone.  All other values are surrounded by single quotes and 
        any single quotes in value are escaped with a pair of single
        quotes.</p>
        
        <p>If <i>value</i> is the empty string, the value <b>NULL</b> is
        returned instead.</p>
    }
} {
    if {[string length $value] == 0} {
        return NULL
    }

    if {[lsearch -exact {decimal double integer int real smallint
                         bigint bit float numeric tinyint} $type] != -1} {
        return $value
    }

    regsub -all -- "'" $value "''" value
    return '$value'
}



#
# ns_dbquotename
#

::nstcl::ad_proc ::nstcl::ns_dbquotename {name} {
    summary "Possibly quotes the name of a database column"
    description {
        <p>Wraps <i>name</i> in double quotes if <i>name</i> contains a 
        space.</p>
    }
} {
    if {[string first " " $name] != -1} {
        return "\"$name\""
    } else {
        return $name
    }
}



#
# database_to_tcl_list
#

::nstcl::ad_proc ::nstcl::database_to_tcl_list {dbhandle SQL {values {}}} {
    summary "Performs an SQL query and returns a list of results"

    syntax "<command>::nstcl::database_to_tcl_list</command> <m>dbhandle</m> <m>SQL</m> ?<m>values</m>?"

    description {
        <p>Performs an <i>SQL</i> query, using <i>dbhandle</i> and 
        returns a list made up of the first column of each of the result
        row(s).</p>

        <p>If specified, results are <b>lappend</b>ed to <i>values</i>.</p>
    }
} {
    set selection [ns_db select $dbhandle $SQL]
    while {[::nstcl::ns_db getrow $dbhandle $selection]} {
        lappend values [::nstcl::ns_set value $selection 0]
    }
    ::nstcl::ns_set free $selection
    return $values
}



#
# database_to_tcl_string
#

::nstcl::ad_proc ::nstcl::database_to_tcl_string {dbhandle SQL} {
    summary "Get a single result as a string from a database query"

    description {
        <p>Returns the first column of the result of an <i>SQL</i> query
        against the database referenced by <i>dbhandle</i>.  An error is 
        thrown if the database does not return exactly one row.</p>
    }
} {
    set selection [::nstcl::ns_db 1row $dbhandle $SQL]
    set result [::nstcl::ns_set value $selection 0]
    ::nstcl::ns_set free $selection
    return $result
}



#
# database_to_tcl_string_or_null
#

::nstcl::ad_proc ::nstcl::database_to_tcl_string_or_null {dbhandle SQL 
                                                          {result ""}} {
    summary "Returns one (possibly non-existant) value from an SQL query"

    syntax "<command>::nstcl::database_to_tcl_string_or_null</command> <m>dbhandle</m> <m>SQL</m> ?<m>default</m>?"

    description {
        <p>Returns the first column of the result of an <i>SQL</i> query.
        An error is thrown if the database returns more than one row.  If
        no rows are returned, and <i>default</i> is specified then
        the value of <i>default</i> is returned (otherwise the empty string
        is returned).</p>
    }
} {
    set selection [::nstcl::ns_db 0or1row $dbhandle $SQL]
    if {$selection != ""} {
        set result [::nstcl::ns_set value $selection 0]
        ::nstcl::ns_set free $selection
    }
    return $result
}



#
# ns_localsqltimestamp
#

::nstcl::ad_proc ::nstcl::ns_localsqltimestamp {} {
    summary "Returns the current local time in SQL timestamp format"
    description {
        <p>Returns the current local time in SQL timestamp format, that is,
        "YYYY-MM-DD HH24:MI:SS".</p>
    }
} {
    return [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]
}



#
# DoubleApos
#

::nstcl::ad_proc ::nstcl::DoubleApos {string} {
    summary "Double-up any single quotes in a string"
    description {
        <p>Doubles-up any single quotes within a string so that '$string' can
        be used safely in a database query.</p>
    }
} {
    regsub -all -- ' $string '' string
    return $string
}



#
# db_quote
#

::nstcl::ad_proc ::nstcl::db_quote {string} {
    summary "Double-up any single quotes in a string"
    description {
        <p>Doubles-up any single quotes within a string so that '$string' can
        be used safely in a database query.</p>
    }
} {
    regsub -all -- ' $string '' string
    return $string
}



#
# database::api_get_dbhandle
#

::nstcl::ad_proc -private ::nstcl::database::api_get_dbhandle {statement_name} {
    variable pools
    variable transaction
     
    if {![regexp {^([^:]+):} $statement_name => pool]} {
        set non_raw_pools [::nstcl::database::pools]

        if {[info exists pools(default_pool)]} {
            set pool $pools(default_pool)
        } elseif {[llength $non_raw_pools] == 1} {
            set pool $non_raw_pools
        } else {
            return -code error "No database pool specified, and no\
                default database pool has been defined."
        }
    }

    if {[lsearch -exact $pools(pools) $pool] == -1} {
        return -code error "Pool \"$pool\" has not been configured."
    }

    if {$transaction(depth) > 0} {
        array set available $transaction(available)

        if {[info exists available($pool)]} {
            set dbhandle  [lindex $available($pool) 0]
            set available($pool) [lrange $available($pool) 1 end]

            if {[llength $available($pool)] == 1} {
                unset available($pool)
            } 

            set transaction(available) [array get available]
        }
    }

    if {![info exists dbhandle]} {
        set dbhandle [::nstcl::ns_db gethandle $pool]

        if {$transaction(depth) > 0} {
            ::nstcl::ns_db dml $dbhandle "begin transaction"
            lappend transaction(dbhandles) $dbhandle
        }
    }

    return $dbhandle
}



#
# database::api_free_dbhandle
#

::nstcl::ad_proc -private ::nstcl::database::api_free_dbhandle {dbhandle} {
    variable transaction

    if {$transaction(depth) == 0} {
        return [::nstcl::ns_db releasehandle $dbhandle]
    }

    # 12 Dec 2002:
    #
    # As was done earlier in the code for ns_db, removed these two
    # additional tests that appeared between the first and last clause of
    # the following if.
    #
    #   [info exists $dbhandle] == 0 || [interp alias {} $dbhandle] == "" ||
    #

    # valid handle?
    if {[regexp {^::nstcl::database::nsdb-[^-]+-[1-9][0-9]*$} $dbhandle] == 0 ||
        [$dbhandle allocated_p] == 0} {
        return -code error "invalid database id: \"$db\""
    }

    set pool [$dbhandle pool]
    array set available [array get $transaction(available)]
    lappend available($pool) $dbhandle
    set transaction(available) [array get available]
}


#
# database::pseudo_bind_variables
#

::nstcl::ad_proc ::nstcl::database::pseudo_bind_variables {sql {backslash 0}} {
    if {[string first : $sql] == -1} {
        return $sql
    }

    set in_string_literal_p 0
    set in_comment_p 0
    set in_variable_p 0

    set var  ""
    set SQL  ""
    set last ""
    set curr ""

    foreach next [split "$sql\n\n" ""] {
        if {$in_string_literal_p} {
            append SQL $curr
            if {!$backslash && $curr == "'" && $next != "'" && $last != "'"} {
                set in_string_literal_p 0
            } elseif {$backslash && $curr == "'"} {
                # postgres is an example of a database that allows non-standard
                # escapes of the form: \'
                if {$next != "'" && $next != "\\" && 
                    $last != "'" && $last != "\\"} {
                    set in_string_literal_p 0
                }
            }
        } elseif {$in_comment_p} {
            append SQL $curr
            if {$curr == "\n" || $curr == "\r"} {
                set in_comment_p 0
            }
        } elseif {$in_variable_p} {
            if {[string is alnum -strict $curr] || $curr == "_" ||
                ($curr == "#" && [string length $var] == 0)} {
                append var $curr
            } elseif {$curr == "(" && [string first "(" $var] == -1} {
                append var $curr
            } elseif {$curr == ")" && [string first "(" $var] != -1} {
                append var $curr
                set in_variable_p 0
            } else {
                set in_variable_p 0
            }

            if {$in_variable_p == 0 && [string length $var]} {
                # Most databases can handle 'quotes' around numbers.
                # Solid, for instance, can't except on inserts; Sybase
                # doesn't at all.  So, we invent this hacking syntax of
                # :#foo for bind variable foo that should be a number and
                # not quoted
                regexp {^(#?)(.*)$} $var => numeric var
         
                # up one level is db_* api function, so 2nd level
                # should be the callers enviroment
                upvar 2 $var value

                if {![info exists value]} {
                    return -code error "bind variable $var does not exist"
                }

                if {[string equal $value ""]} {
                    append SQL NULL
                } elseif {$numeric == "" && $backslash == 0} {
                    append SQL '[string map [list ' ''] $value]'
                } elseif {$numeric == "" && $backslash == 1} {
                    append SQL '[string map [list ' '' \\ \\\\] $value]'
                } elseif {![string is double -strict [string trim $value]]} {
                    return -code error "numeric bind variable $var is not a\
                        number"
                } else {
                    append SQL $value
                }

                if {$curr != ")" || [string first "(" $var] == -1} {
                    append SQL $curr
                }

                set var ""
            } elseif {$in_variable_p == 0 && [string length $var] == 0} {
                append SQL :
            }
        } else {
            # To avoid confusion with namespaced variables & type-casts
            # in certain SQL-dialects (such as Postgres), we do not treat
            # :: as bind variable syntax.  See nstcl bug #621007.
            switch -glob -- "$last$curr$next" {
                {::?}             -
                {?::}             {}
                {?--}             { set in_comment_p  1 }
                {?'?}             { set in_string_literal_p 1 }
                {?:[a-zA-Z0-9_#]} { set in_variable_p 1 }
            }

            if {!$in_variable_p} {
                append SQL $curr
            }
        }

        set last $curr
        set curr $next
    }

    # remove the extra \n we added above
    set SQL [string range $SQL 0 end-1]
    return $SQL
}


#
# db_string
#

::nstcl::ad_proc ::nstcl::db_string {statement_name SQL args} {
    summary "Returns the first column from an SQL query"
    syntax "<command>::nstcl::db_string</command> <m>statement_name</m> <m>SQL</m> ?<m>-default default_value</m>?"

    description {
        <p>Returns the first column of the result of an <i>SQL</i> query.
        An error is thrown if the database returns more than one row.  Also,
        if no rows are returned an error will be thrown unless the optional
        <i>-default</i> switch is provided, in which case the 
        <i>default_value</i> will be returned instead.</p>
    }

    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    ad_arg_parser default $args
    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]

    set SQL [[$dbhandle bind_vars] $SQL]

    set bomb_p [catch {
        set selection [::nstcl::ns_db 0or1row $dbhandle $SQL]
    } error_msg]

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$bomb_p} {
        error $error_msg
    }

    if {$selection == ""} {
        if {[info exists default]} {
            return $default
        }

        error "Query did not return any results, and no default provided."
    }

    set result [::nstcl::ns_set value $selection 0]
    ::nstcl::ns_set free $selection
    return $result
}



#
# db_list
#

::nstcl::ad_proc ::nstcl::db_list {statement_name SQL} {
    summary "Returns a list of the first column from an SQL query"
   
    description {
        <p>Returns a list containing the first column of each row
        returned by the <i>SQL</i> query.</p>
    } 

    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]
    set bomb_p [catch {
        set selection [::nstcl::ns_db select $dbhandle $SQL]
    } result]

    if {$bomb_p} {
        ::nstcl::database::api_free_dbhandle $dbhandle
        error $result
    }

    while {[::nstcl::ns_db getrow $dbhandle $selection]} {
        lappend values [::nstcl::ns_set value $selection 0]
    }

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {[info exists values]} {
       ::nstcl::ns_set free $selection
       return $values
    }
}



#
# db_list_of_lists
#

::nstcl::ad_proc ::nstcl::db_list_of_lists {statement_name SQL} {
    summary "Evaluates an SQL query and returns the results as a list of lists"

    description {
        <p>Returns a list containing lists of the values in each
        column of each row returned by the <i>SQL</i> query.</p>
    }

    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]
    set bomb_p [catch {
        set selection [::nstcl::ns_db select $dbhandle $SQL]
    } result]

    if {$bomb_p} {
        ::nstcl::database::api_free_dbhandle $dbhandle
        error $result
    }

    set results {}
    set size [::nstcl::ns_set size $selection]

    while {[::nstcl::ns_db getrow $dbhandle $selection]} {
        set this_row {}
        for {set i 0} {$i < $size} {incr i} {
            lappend this_row [::nstcl::ns_set value $selection $i]
        }
        lappend results $this_row
    }
    
    ::nstcl::ns_set free $selection
    ::nstcl::database::api_free_dbhandle $dbhandle
    
    return $results
}



#
# db_dml
#

::nstcl::ad_proc ::nstcl::db_dml {statement_name SQL} {
    summary "Executes a DML statement"
    
    description {
        <p>Executes the specified DML statement.</p>
    }

    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    set dbhandle [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]
    set bomb_p [catch {
        ::nstcl::ns_db dml $dbhandle $SQL
    } result]

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$bomb_p} {
        error $result
    }
}



#
# db_0or1row
#

::nstcl::ad_proc ::nstcl::db_0or1row {statement_name SQL args} {
    summary "Set variables for a query returning at most one row"

    syntax "<command>::nstcl::ns_db0or1row</command> <m>statement_name</m> <m>SQL</m> ?<m>-column_array array_name | -column_set setId</m>?"
  
    description {
        <p>Performs the <i>SQL</i> query, setting variables to column values
        if one row is returned.  No variables are set if no rows are returned.
        An error will be raised if multiple rows are returned.  
        <b>ns_db0or1row</b> will return the number of rows returned 
        (0 or 1).</p>
    }

    optional_switches {
        <dle>
            <dt><option>-column_array array_name</option></dt>
            <dd>
                Instead of setting individual variables (foo, bar, etc.), set 
                each column in the result row as a field in <i>array_name</i>
                (i.e. array_name(foo), array_name(bar), etc.)
            </dd>

            <dt><option>-column_set setId</option></dt>
            <dd>
                Instead of setting individual variables, updates the 
                <b>ns_set</b> referenced by <i>setId</i> with the key/value
                pairs returned by the database.
            </dd>
        </dle>
    }
                
    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    ::nstcl::ad_arg_parser {column_array column_set} $args
    
    if {[info exists column_array] && [info exists column_set]} {
        error "Can't specify both column_array and column_set"
    }

    if {[info exists column_array]} {
        upvar 1 $column_array array
        if {[info exists array]} {
            unset array
        }
    }

    if {[info exists column_set]} {
        upvar 1 $column_set selection
    }

    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]

    set bomb_p [catch { 
        set selection [::nstcl::ns_db 0or1row $dbhandle $SQL] 
    } error_msg]

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$bomb_p} {
        error $error_msg
    }

    if {$selection == ""} {
        return 0
    }

    if {[info exists column_set]} {
        return 1
    }

    set size [::nstcl::ns_set size $selection]

    for {set i 0} {$i < $size} {incr i} {
        set key [::nstcl::ns_set key $selection $i]
        set val [::nstcl::ns_set value $selection $i]

        if {[info exists column_array]} {
            set array($key) $val
        } else {
            upvar 1 $key value
            set value $val
        }
    }

    return 1
}



#
# db_1row
#

::nstcl::ad_proc ::nstcl::db_1row {statement_name SQL args} {
    summary "Set variables for a query returning exactly one row"

    syntax "<command>::nstcl::ns_db1row</command> <m>statement_name</m> <m>SQL</m> ?<m>-column_array array_name | -column_set setId</m>?"
  
    description {
        <p>Performs the <i>SQL</i> query, setting variables to column values.
        An error will be raised if either no rows, or multiple rows, are
        returned.</p>
    }

    optional_switches {
        <dle>
            <dt><option>-column_array array_name</option></dt>
            <dd>
                Instead of setting individual variables (foo, bar, etc.), set 
                each column in the result row as a field in <i>array_name</i>
                (i.e. array_name(foo), array_name(bar), etc.)
            </dd>

            <dt><option>-column_set setId</option></dt>
            <dd>
                Instead of setting individual variables, updates the 
                <b>ns_set</b> referenced by <i>setId</i> with the key/value
                pairs returned by the database.
            </dd>
        </dle>
    }
                
    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    ::nstcl::ad_arg_parser {column_array column_set} $args

    if {[info exists column_array] && [info exists column_set]} {
        error "Can't specify both column_array and column_set"
    }

    if {[info exists column_array]} {
        upvar 1 $column_array array
        if {[info exists array]} {
            unset array
        }
    }

    if {[info exists column_set]} {
        upvar 1 $column_set selection
    }

    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]

    set bomb_p [catch { 
        set selection [::nstcl::ns_db 1row $dbhandle $SQL] 
    } error_msg]

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$bomb_p} {
        error $error_msg
    }

    if {[info exists column_set]} {
        return 1
    }

    set size [::nstcl::ns_set size $selection]

    for {set i 0} {$i < $size} {incr i} {
        set key [::nstcl::ns_set key $selection $i]
        set val [::nstcl::ns_set value $selection $i]

        if {[info exists column_array]} {
            set array($key) $val
        } else {
            upvar 1 $key value
            set value $val
        }
    }

    return 1
}



#
# db_multirow
#

::nstcl::ad_proc ::nstcl::db_multirow {-local:boolean -append:boolean 
                                       -extend:optional var_name 
                                       statement_name SQL args} {
    syntax "<command>::nstcl::db_multirow</command> ?<m>-local</m> ?<m>-append</m>? ?<m>-extend extendVariables</m>? <m>varName</m> <m>statementName</m> <m>SQL</m> ?<m>code</m>? ?<m>if_no_rows</m>?  ?<m>elseCode</m>?"
    

    summary "Create a multirow templating datasource from an SQL query"

    description {
        <p>This command is used to create a multirow templating datasource 
        from an <i>SQL</i> query.  The name of the datasource created is 
        <i>varName</i>.  See the <b>database_api</b> documentation for
        information on <i>statement_name</i>.</p>

        <p>If a <i>code</i> block is given, it is evaluated for each
        result row from the database.  Any modifications to the variables
        selected from the database are stored in the datasource, unless
        the Tcl command <b>continue</b> is called (in which case the row
        is effectively skipped).</p>

        <p>If the database returns no rows for the <i>SQL</i>, and the
        optional <i>if_no_rows</i> clause is present, then the <i>elseCode</i>
        is evaluated.</p> 
    }

    optional_switches {
        <dle>
        <dt><option>-local</option></dt>
        <dd>This switch is included for source code compatability with OpenACS
        and has no effect one way or the other.  This switch did not exist 
        in the ACS.</dd>

        <dt><option>-append</option></dt>
        <dd>This switch may be used to append to an existing multirow 
        datasource, rather than overwriting it, allowing you to build
        up the datasource with successive queries.  It is an error, however,
        to use this switch with a query which returns a different set of
        columns from the database.</dd>
  
        <dt><option>-extend extendVariables</option></dt>
        <dd>This switch takes a list of additional variables to add to the
        multirow datasource which are not returned by the database.  Instead,
        your <i>code</i> block should perform some calculation and set the
        Tcl variables of the same name, which will then be stored in the
        multirow datasource.  This is an alternative (pioneered in OpenACS)
        to forcing the database to return an extra variable by using the
        "<i>select null as foo</i>" idiom.</dd>
        </dle>
    }

    see_also "adp, database_api, multirow"
    keywords templating
} {
    set argc [llength $args]

    if {$argc > 3} {
        error "Too many parameters specified."
    }

    if {$argc == 3 && [lsearch {if_no_rows else} [lindex $args 1]] == -1} {
        error "Expected if_no_rows as second to last argument: $args"
    }

    if {$argc == 2} {
        error "Expected 0, 1, or 3 arguments, not 2"
    }

    if {$argc >= 1} {
        set code_block [lindex $args 0]
    }

    if {$argc == 3} {
        set else_block [lindex $args 2]
    }

    upvar 1 $var_name:rowcount counter
    upvar 1 $var_name:columns columns

    set rc 0
    if {!$append_p || ![info exists counter]} {
        set counter 0
    }

    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]

    set bomb_p [catch {
        set selection [::nstcl::ns_db select $dbhandle $SQL]
    } result]
    
    if {$bomb_p} {
    ::nstcl::database::api_free_dbhandle $dbhandle
        error $result
    }

    set size [::nstcl::ns_set size $selection]
    set keys [list]

    for {set i 0} {$i < $size} {incr i} {
        lappend keys [::nstcl::ns_set key $selection $i]
    }

    if {[info exists extend]} {
        foreach key $extend {
            lappend keys $key
        }
    }

    if {!$append_p || ![info exists columns]} {
        set columns $keys
    } else {
        set old [lsort -dictionary $columns]
        set new [lsort -dictionary $keys]
        if {![string equal $old $new]} {
            return -code error "Cannot append to a multirow with different\
                columns.  (Previously: $old; Now: $new)"
        }
    }

    while {[::nstcl::ns_db getrow $dbhandle $selection] != 0} {
        upvar 1 $var_name:[incr counter] array
        for {set i 0} {$i < $size} {incr i} {
            set key [::nstcl::ns_set key $selection $i]
            set val [::nstcl::ns_set value $selection $i]
            set array($key) $val

            if {[info exists code_block]} {
                upvar 1 $key column_value
                set column_value $val
            }
        }

        if {[info exists extend]} {
            foreach key $extend {
                set array($key) ""
                upvar 1 $key column_value
                set column_value ""
            }
        }
                
        
        if {[info exists code_block]} {
            set rc [catch { uplevel 1 $code_block } result]

            # on return -code ok, return, or break we suck the values back in
            if {$rc == 0 || $rc == 2 || $rc == 3} {
                foreach key [array names array] {
                    upvar 1 $key column_value
                    set array($key) $column_value
                }
            }

            if {$rc == 1 || $rc == 2 || $rc == 3} then break
        }
    }

    ::nstcl::ns_set free $selection

    if {$rc == 4} {
        set rc 0
    }

    if {$rc != 0} {
        ::nstcl::ns_db flush $dbhandle
    } 

    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$counter == 0 && [info exists else_block] && $rc == 0} { 
        set rc [catch { uplevel 1 $else_block } result]
    }

    switch -- $rc {
        0 -
        2 -
        3 { return }
        1 { error $result }
    }
}



#
# db_foreach
#

::nstcl::ad_proc ::nstcl::db_foreach {statement_name SQL args} {
    summary "Execute a block of code for each result row from a SQL query"
    syntax "<command>::nstcl::db_foreach</command> <m>statement_name</m> <m>SQL</m> <m>code</m> ?<m>if_no_rows if_no_rows_code</m>?"

    description {
        <p><b>db_foreach</b> sends the <i>SQL</i> query to the database and
        executes <i>code</i> once for each of the result row(s) returned from 
        the database.</p>

        <p>Each column selected by <i>SQL</i> sets a variable by that name
        that can be used in the context of <i>code</i>.</p>

        <p>Optionally, for the case where <i>SQL</i> returns no rows, 
        an <i>if_no_rows_code</i> code block can be specified which will
        be evaluated once.  Naturally no variables will be set, since no row 
        was returned by the database.</p>
    }

    see_also {
        <p>The documentation for the <b>database_api</b> for information 
        on <i>statement_name</i> and which database pool is used to 
        evaluate <i>SQL</i> when multiple pools are defined.</p>
    }
} {
    ::nstcl::ad_arg_parser {column_array column_set args} $args

    set argc [llength $args]
    if {$argc != 1 && $argc != 3} {
        error "Expected 1 or 3 arguments after switches"
    }

    foreach {code else if_no_rows_code} $args break
    switch -- $else {
        else       -
        if_no_rows {}
        {}         { unset if_no_rows_code }
        default    { error "Expected if_no_rows as second-to-last argument" }
    }

    if {[info exists column_array] && [info exists column_set]} {
        error "Can't specify both column_array and column_set"
    }

    if {[info exists column_array]} {
        upvar 1 $column_array array
    }

    if {[info exists column_set]} {
        upvar 1 $column_set selection
    }

    set dbhandle  [::nstcl::database::api_get_dbhandle $statement_name]
    set SQL [[$dbhandle bind_vars] $SQL]

    set bomb_p [catch {
        set selection [::nstcl::ns_db select $dbhandle $SQL]
    } result]

    if {$bomb_p} {
        ::nstcl::database::api_free_dbhandle $dbhandle
        error $result
    }

    set size [::nstcl::ns_set size $selection]
    set num_rows  0

    while {[::nstcl::ns_db getrow $dbhandle $selection] && [incr num_rows]} {
        if {[info exists column_array] && [info exists array]} {
            unset array
        }

        if {![info exists column_set]} {
            for {set i 0} {$i < $size} {incr i} {
                set key [string tolower [::nstcl::ns_set key $selection $i]]
                set val [::nstcl::ns_set value $selection $i]

                if {[info exists column_array]} {
                    set array($key) $val
                } else {
                    upvar 1 $key this_key
                    set this_key $val
                }
            }
        }

        set return_code [catch { uplevel 1 $code } result]

        switch -- $return_code {
            0       -
            4       {}

            2       -
            3       {
                ::nstcl::ns_db flush $dbhandle
                break
            }
            1       -
            default {
                global errorInfo
                global errorCode
                ::nstcl::ns_db flush $dbhandle
                ::nstcl::ns_set free $selection
                ::nstcl::database::api_free_dbhandle $dbhandle
                error $result $errorInfo $errorCode
            }
        }
    }

    ::nstcl::ns_set free $selection
    ::nstcl::database::api_free_dbhandle $dbhandle

    if {$num_rows == 0 && [info exists if_no_rows_code]} {
        uplevel 1 $if_no_rows_code
    }
}



#
# db_transaction
#

::nstcl::ad_proc ::nstcl::db_transaction {code args} {
    summary "Execute database statements within a transaction"

    syntax "<command>::nstcl::db_transaction</command> <m>code</m> ?<m>on_error on_error_code</m>?"

    description {
        <p><b>db_transaction</b> evalutates <i>code</i> inside of a database
        transaction.  An optional <i>on_error_code</i> can be specified which
        is evaluated each time there is an error.  The <i>on_error_code</i>
        may choose to let the transaction continue by calling 
        <b>db_continue_transaction</b>.  If the transaction is not explicitly
        continued (or if there is no <i>on_error_code</i> specified) then
        the transaction is aborted, and the database is rolled back to the
        state it was before the transaction began.</p>
    }
} {
    set argc [llength $args]
    if {($argc != 0 && $argc != 2) ||
        ($argc == 2 && ![string equal [lindex $args 0] "on_error"])} {
        error "db_transaction called with invalid syntax.  Should be:\
            \"db_transaction code ?on_error on_error_code?\"" ;# "
    }

    if {$argc == 2} {
        set exception_code [lindex $args 1]
    }

    upvar 0 ::nstcl::database::transaction transaction
    incr transaction(depth)

    if {$transaction(depth) == 1} {
        set transaction(abort_p) 0
        set transaction(available) {}
    }

    set return_code [catch { uplevel 1 $code } result]

    global errorInfo
    global errorCode

    if {$return_code != 0} {
        set transaction(errorInfo) $errorInfo
        set transaction(errorCode) $errorCode

        ::nstcl::db_abort_transaction
    }

    if {[::nstcl::db_abort_transaction_p]} {
        if {[info exists exception_code]} {
            set return_code [catch { uplevel 1 $exception_code } result]
            if {$return_code != 0} {
                set transaction(errorInfo) \
                    [join [list $transaction(errorInfo) $errorInfo)] \n]
                ::nstcl::db_abort_transaction
            }
        }
    }

    incr transaction(depth) -1
    
    if {$transaction(depth) == 0} {
        switch [::nstcl::db_abort_transaction_p] {
            0 { set action "end transaction" }
            1 { set action "abort transaction" }
        }

        foreach dbhandle $transaction(dbhandles) {
            catch { ::nstcl::ns_db flush $dbhandle }
            ::nstcl::ns_db dml $dbhandle $action
            ::nstcl::ns_db releasehandle $dbhandle
        }

        set transaction(dbhandles) {}
    }

    if {[::nstcl::db_abort_transaction_p]} {
        if {$transaction(depth) == 0} {
            set transaction(abort_p) 0
        }
        error $result $transaction(errorInfo) $transaction(errorCode)
    }
}



#
# db_continue_transaction
#

::nstcl::ad_proc ::nstcl::db_continue_transaction {} {
    summary "Continue a transaction that was to be aborted"
 
    description {
        <p>This command can be called in an <i>on_error</i> code block
        of <b>db_transaction</b> to signal that the transaction should
        be continued, and not aborted.</p>

        <p>It is en error to call this command outside of a transaction.</p>
    }
} {
    if {$::nstcl::database::transaction(depth) == 0} {
        error "Can't continue transaction: not currently inside a transaction"
    } else {
        return [set ::nstcl::database::transaction(abort_p) 0]
    }
}



#
# db_abort_transaction
#

::nstcl::ad_proc ::nstcl::db_abort_transaction {} {
    summary "Abort a database transaction"
    
    description {
        <p>Aborts a <b>db_transaction</b>.  Note that an <i>on_error</i>
        code block can still call <b>db_continue_transaction</b>.</p>

        <p>It is en error to call this command outside of a transaction.</p>
    }
} {
    if {$::nstcl::database::transaction(depth) == 0} {
        error "Can't abort transaction: not currently inside a transaction"
    } else {
        return [set ::nstcl::database::transaction(abort_p) 1]
    }
}



#
# db_abort_transaction_p
#

::nstcl::ad_proc ::nstcl::db_abort_transaction_p {} {
    summary "Returns boolean abortion status"

    description {
        <p>Returns 1 if the current <b>db_transaction</b> is set to be
        aborted, 0 otherwise.</p>
    }
} {
    return $::nstcl::database::transaction(abort_p)
}



#
# find_shared_library
#

::nstcl::ad_proc -private ::nstcl::find_shared_library {filename} {
    if {![string match *[info sharedlibextension] $filename]} {
        append filename [info sharedlibextension]
    }

    if {[file exists $filename]} {
        return $filename
    }

    foreach dir $::auto_path {
        if {[file exists [file join $dir $filename]]} {
            return [file join $dir $filename]
        }

        foreach subdir [lsort [glob -nocomplain [file join $dir *]]] {
            if {[file isdirectory $subdir]} {
                if {[file exists [file join $subdir $filename]]} {
                    return [file join $subdir $filename]
                }
            }
        }
    }

    # could not find it--let the OS search for it (cf. /etc/ld.so.conf)
    return $filename
}



#
# database_api docs
#

::nstcl::ad_proc ::nstcl::database_api {} {
    summary "The nstcl database api"
    syntax ""
    
    description {
        <p>The <b>nstcl</b> database api is a re-implementation of the ACS &amp;
        OpenACS database APIs that make it possible to work with the database
        at a higher level than the AOLserver <b>ns_db</b> commands.</p>

        <p>Bind variable emulation is available for all databases.  That is,
        instead of writing <i>select * from foo where bar = $id</i> you
        can write <i>select * from foo where bar = :id</i>.  The emulation
        takes care of escaping double apostrophes and such.  
        For Solid RDBMS, which does not allow numeric types to be enclosed
        in single quotes (except on inserts), an alternate syntax of 
        <i>:#id</i> may be used which does not wrap the number in single 
        quotes.</p>

        <p>Each of the database api commands takes a statement name.  In
        the original ACS 4 these statement names served merely as documentation.
        The OpenACS project took advantage of the statement_name's to 
        create a query dispatcher that could find and select the proper
        version of a query, depending on the database being used.</p>

        <p><b>nstcl</b> does not contain a query dispatcher at this time.
        It does have, however, one benefit to the current OpenACS setup:
        it can work with multiple databases at the same time.</p>

        <p>If you only create one database pool, that pool will always be
        used.  You do not need to explicitly set a default.  If you 
        configure multiple pools, you can use 
        <b>::nstcl::set_default_pool <i>poolname</i></b> to set a default,
        or use the <option>-default</option> switch on one of the
        calls to <b>nstcl::configure_pool</b>.</p>

        <p>To specify an alternate pool, other than the default, preface the
        statement_name with the name of the pool, followed by a colon.</p>

        <dle>
        <dt>EXAMPLE:</dt>
        <dd>
<example>
    package require nstcl

    nstcl::load_driver oracle
    nstcl::load_driver postgres
    nstcl::load_driver solid
    nstcl::configure_pool oracle oracle 1 "" username password
    nstcl::configure_pool postgres postgres 1 "localhost:5432:dbname" 
    nstcl::configure_pool solid solid 1 "tcp localhost 1313" username password
    nstcl::set_default_pool solid

    # uses default pool
    puts [db_string current_time_from_solid { select now() }]

    # explicitly identifies which alternate pool to use
    db_foreach oracle:old_customers {
        select first_names, last_name, email
          from customers
    } {
        db_dml postgres:new_customers {
            insert into customers values (:first_names, :last_name, :email)
        }
    }
</example>
    </dd>
    </dle>
    }

    see_also {
        db_0or1row, db_1row, db_transaction, db_abort_transaction, 
        db_continue_transaction, db_dml, db_foreach, db_list, db_list_of_lists,
        db_multirow, db_string
    }
} return



package provide nstcl-database 1.2
