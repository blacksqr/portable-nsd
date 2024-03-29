ad_library {
    Manage Membership Relations

    @author yon (yon@openforce.net)
    @creation-date 2002-03-15
    @cvs-id $Id: membership-rel-procs.tcl,v 1.2.2.1 2003/06/06 08:53:13 lars Exp $
}

namespace eval membership_rel {

    ad_proc -public change_state {
        {-rel_id:required}
        {-state:required}
    } {
        Change the state of a membership relation
    } {
        db_transaction {
            switch -exact $state {
                "approved" { db_exec_plsql approve {} }
                "banned" { db_exec_plsql ban {} }
                "rejected" { db_exec_plsql reject {} }
                "deleted" { db_exec_plsql delete {} }
                "needs approval" { db_exec_plsql unapprove {} }
            }

            # Record who changed the state
            # This will trigger an update of the acs_objects.modified_date column.
            # We use this in the ApprovalExpiration feature to make sure that a user isn't 
            # bumped back to needs_approval right after an administrator has approved them,
            # even if the user doesn't log in in the meantime.

            if { [ad_conn isconnected] } {
                set user_id [ad_conn user_id]
            } else {
                set user_id [db_null]
            }
            db_dml update_modifying_user {}
        }
    }

    ad_proc -public approve {
        {-rel_id:required}
    } {
        Approve a membership relation
    } {
        change_state -rel_id $rel_id -state "approved"
    }

    ad_proc -public ban {
        {-rel_id:required}
    } {
        Ban a membership relation
    } {
        change_state -rel_id $rel_id -state "banned"
    }

    ad_proc -public reject {
        {-rel_id:required}
    } {
        Reject a membership relation
    } {
        change_state -rel_id $rel_id -state "rejected"
    }

    ad_proc -public delete {
        {-rel_id:required}
    } {
        Delete a membership relation
    } {
        change_state -rel_id $rel_id -state "deleted"
    }

    ad_proc -public unapprove {
        {-rel_id:required}
    } {
        Unapprove a membership relation
    } {
        change_state -rel_id $rel_id -state "needs approval"
    }

}
