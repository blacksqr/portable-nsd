<?xml version="1.0"?>
<queryset>

    <fullquery name="select_question">
        <querytext>
            select password_question
            from users
            where user_id = :user_id
        </querytext>
    </fullquery>

</queryset>