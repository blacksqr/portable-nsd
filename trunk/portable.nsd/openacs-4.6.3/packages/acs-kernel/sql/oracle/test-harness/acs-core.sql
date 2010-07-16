--
-- acs-core/sql/acs-core-test-harness.sql
--
-- Test harness for ACS Core's PL/SQL API
--
-- @author Michael Yoon (michael@arsdigita.com)
--
-- @creation-date 2000-08-05
--
-- @cvs-id $Id: acs-core.sql,v 1.1 2001/03/20 22:51:56 donb Exp $
--

declare
 uid users.user_id%TYPE;
begin
 dbms_output.put_line('Calling acs_user.new() to create user 1');

 uid :=
  acs_user.new(email => 'jane.doe@arsdigita.com',
               first_names => 'Jane',
               last_name => 'Doe',
               password => 'janedoerules',
               creation_ip => '127.0.0.1',
               user_id => 1);

 dbms_output.put_line('Calling acs_object.name() to get the name of user 1: ' || acs_object.name(1));

 dbms_output.put_line('Calling acs_user.delete() to delete user 1');

 acs_user.delete(1);
end;
/
show errors


