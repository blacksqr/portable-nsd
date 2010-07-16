-- packages/acs-reference/sql/common/timezone-drop.sql
--
-- Drop the timezone package
--
-- @author  jon@jongriffin.com
-- @created 2000-12-04
-- @cvs-id  $Id: ref-timezones-drop.sql,v 1.2 2001/10/15 19:33:38 donb Exp $

declare
    v_repository_id integer;
begin
    select repository_id into v_repository_id
    from   acs_reference_repositories
    where  lower(table_name) = 'timezones';

    acs_reference.delete(v_repository_id);
end;
/
show errors;

drop sequence timezone_seq;
drop table    timezone_rules;
drop table    timezones;
drop package  timezone;


