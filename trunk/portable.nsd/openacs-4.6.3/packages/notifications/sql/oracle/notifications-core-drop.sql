--
-- The Notifications Package
--
-- @author Ben Adida (ben@openforce.net)
-- @version $Id: notifications-core-drop.sql,v 1.3 2002/08/09 20:51:49 yon Exp $
--
-- Copyright (C) 2000 MIT
--
-- GNU GPL v2
--

drop table notification_user_map;
drop table notifications;
drop table notification_requests;
drop table notification_types_del_methods;
drop table notification_types_intervals;
drop table notification_types;
drop table notification_intervals;
drop table notification_delivery_methods;

--
-- Object Types
--
declare
begin

    acs_object_type.drop_type (
        object_type => 'notification_interval'
    );

    acs_object_type.drop_type (
        object_type => 'notification_delivery_method'
    );

    acs_object_type.drop_type (
        object_type => 'notification_type'
    );

    acs_object_type.drop_type (
        object_type => 'notification_request'
    );

    acs_object_type.drop_type (
        object_type => 'notification'
    );

end;
/
show errors