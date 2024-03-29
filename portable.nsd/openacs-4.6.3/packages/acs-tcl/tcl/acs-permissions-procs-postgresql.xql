<?xml version="1.0"?>
<queryset>
    <rdbms><type>postgresql</type><version>7.1</version></rdbms>

    <fullquery name="permission::grant.grant_permission">
        <querytext>
                select acs_permission__grant_permission(
                    :object_id,
                    :party_id,
                    :privilege
                );
        </querytext>
    </fullquery>

    <fullquery name="permission::revoke.revoke_permission">
        <querytext>
             select acs_permission__revoke_permission(
                    :object_id,
                    :party_id,
                    :privilege
                );
        </querytext>
    </fullquery>

    <fullquery name="permission::permission_p_not_cached.select_permission_p">
        <querytext>
            select 1
            where 't' = acs_permission__permission_p(:object_id, :party_id, :privilege)
        </querytext>
    </fullquery>

    <fullquery name="permission::require_permission.name">      
        <querytext>
            select acs_object__name(:object_id)
            from dual
        </querytext>
    </fullquery>

    <fullquery name="permission::toggle_inherit.toggle_inherit">
        <querytext>
            update acs_objects
            set security_inherit_p = not security_inherit_p
            where object_id = :object_id
        </querytext>
    </fullquery>

</queryset>
