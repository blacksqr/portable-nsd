-- packages/acs-service-contract/sql/oracle/upgrade/upgrade-4.5.1-4.6.sql
--
-- @author Vinod Kurup (vinod@kurup.com)
-- @creation_date 2002-10-08
--
-- $Id: upgrade-4.5.1-4.6.sql,v 1.1.2.1 2002/10/29 01:51:52 vinodk Exp $

-- UPGRADE ISSUE #1
-- add timestamp datatype

declare
    v_msg_type_id	acs_sc_msg_types.msg_type_id%TYPE;
begin
	if acs_sc_msg_type.get_id('timestamp') is null then
	   v_msg_type_id := acs_sc_msg_type.new('timestamp','');
	end if;
end;
/
show errors
