--
-- Upgrade script from Beta 1 to Beta 2.
--
-- @author Richard Li (richardl@arsdigita.com)
-- @cvs-id $Id: upgrade-beta-1-beta-2.sql,v 1.1 2001/03/20 22:51:56 donb Exp $
-- @creation-date 10 October 2000

@@ acs-logs-create

create or replace package acs_permission
as

  procedure grant_permission (
    object_id	 acs_permissions.object_id%TYPE,
    grantee_id	 acs_permissions.grantee_id%TYPE,
    privilege	 acs_permissions.privilege%TYPE
  );

  procedure revoke_permission (
    object_id	 acs_permissions.object_id%TYPE,
    grantee_id	 acs_permissions.grantee_id%TYPE,
    privilege	 acs_permissions.privilege%TYPE
  );

  function permission_p (
    object_id	 acs_objects.object_id%TYPE,
    party_id	 parties.party_id%TYPE,
    privilege	 acs_privileges.privilege%TYPE
  ) return char;

end acs_permission;
/
show errors

-- from acs-permissions

create or replace package body acs_permission
as

  procedure grant_permission (
    object_id	 acs_permissions.object_id%TYPE,
    grantee_id	 acs_permissions.grantee_id%TYPE,
    privilege	 acs_permissions.privilege%TYPE
  )
  as
  begin
    insert into acs_permissions
      (object_id, grantee_id, privilege)
    values
      (object_id, grantee_id, privilege);
  exception
    when dup_val_on_index then
      return;
  end grant_permission;

  procedure revoke_permission (
    object_id	 acs_permissions.object_id%TYPE,
    grantee_id	 acs_permissions.grantee_id%TYPE,
    privilege	 acs_permissions.privilege%TYPE
  )
  as
  begin
    delete from acs_permissions
    where object_id = revoke_permission.object_id
    and grantee_id = revoke_permission.grantee_id
    and privilege = revoke_permission.privilege;
  end revoke_permission;

  function permission_p (
    object_id	 acs_objects.object_id%TYPE,
    party_id	 parties.party_id%TYPE,
    privilege	 acs_privileges.privilege%TYPE
  ) return char
  as
    exists_p char(1);
  begin
    select decode(count(*),0,'f',t') into exists_p
      from acs_object_party_privilege_map
     where object_id = permission_p.object_id
       and party_id in (permission_p.party_id, -1)
       and privilege = permission_p.privilege);
    return exists_p;
  end;

end acs_permission;
/
show errors

create or replace package acs_user
as

 function new (
  user_id	in users.user_id%TYPE default null,
  object_type	in acs_objects.object_type%TYPE
		   default 'user',
  creation_date	in acs_objects.creation_date%TYPE
		   default sysdate,
  creation_user	in acs_objects.creation_user%TYPE
		   default null,
  creation_ip	in acs_objects.creation_ip%TYPE default null,
  email		in parties.email%TYPE,
  url		in parties.url%TYPE default null,
  first_names	in persons.first_names%TYPE,
  last_name	in persons.last_name%TYPE,
  password	in users.password%TYPE,
  salt		in users.salt%TYPE,
  password_question     in users.password_question%TYPE default null,
  password_answer	in users.password_answer%TYPE default null,
  screen_name	in users.screen_name%TYPE default null,
  email_verified_p in users.email_verified_p%TYPE default 't'
 )
 return users.user_id%TYPE;

 function receives_alerts_p (
  user_id	in users.user_id%TYPE
 )
 return char;

 procedure approve_email (
  user_id	in users.user_id%TYPE
 );

 procedure unapprove_email (
  user_id	in users.user_id%TYPE
 );

 procedure delete (
  user_id	in users.user_id%TYPE
 );

end acs_user;
/
show errors

-- from community-core.sql



create or replace package body party
as

 function new (
  party_id	in parties.party_id%TYPE default null,
  object_type	in acs_objects.object_type%TYPE
		   default 'party',
  creation_date	in acs_objects.creation_date%TYPE
		   default sysdate,
  creation_user	in acs_objects.creation_user%TYPE
		   default null,
  creation_ip	in acs_objects.creation_ip%TYPE default null,
  email		in parties.email%TYPE,
  url		in parties.url%TYPE default null
 )
 return parties.party_id%TYPE
 is
  v_party_id parties.party_id%TYPE;
 begin
  v_party_id :=
   acs_object.new(party_id, object_type,
                  creation_date, creation_user, creation_ip);

  insert into parties
   (party_id, email, url)
  values
   (v_party_id, lower(email), url);

  return v_party_id;
 end new;

 procedure delete (
  party_id	in parties.party_id%TYPE
 )
 is
 begin
  delete from parties
  where party_id = party.delete.party_id;

  acs_object.delete(party_id);
 end delete;

 function name (
  party_id	in parties.party_id%TYPE
 )
 return varchar2
 is
 begin
  if party_id = -1 then
   return 'The Public';
  else
   return null;
  end if;
 end name;

end party;
/
show errors

create or replace package body acs_user
as

 function new (
  user_id	in users.user_id%TYPE default null,
  object_type	in acs_objects.object_type%TYPE
		   default 'user',
  creation_date	in acs_objects.creation_date%TYPE
		   default sysdate,
  creation_user	in acs_objects.creation_user%TYPE
		   default null,
  creation_ip	in acs_objects.creation_ip%TYPE default null,
  email		in parties.email%TYPE,
  url		in parties.url%TYPE default null,
  first_names	in persons.first_names%TYPE,
  last_name	in persons.last_name%TYPE,
  password	in users.password%TYPE,
  salt		in users.salt%TYPE,
  password_question     in users.password_question%TYPE default null,
  password_answer	in users.password_answer%TYPE default null,
  screen_name	in users.screen_name%TYPE default null,
  email_verified_p in users.email_verified_p%TYPE default 't'
 )
 return users.user_id%TYPE
 is
  v_user_id users.user_id%TYPE;
 begin
  v_user_id :=
   person.new(user_id, object_type,
              creation_date, creation_user, creation_ip,
              email, url,
              first_names, last_name);

  insert into users
   (user_id, password, salt, password_question, password_answer, screen_name,
    email_verified_p)
  values
   (v_user_id, password, salt, password_question, password_answer, screen_name,
    email_verified_p);

  insert into user_preferences
    (user_id)
    values
    (v_user_id);

  return v_user_id;
 end new;

 function receives_alerts_p (
  user_id in users.user_id%TYPE
 )
 return char
 is
  counter	char(1);
 begin
  select decode(count(*),0,'f','t') into counter
   from users
   where no_alerts_until >= sysdate
   and user_id = acs_user.receives_alerts_p.user_id;

  return counter;
 end receives_alerts_p;

 procedure approve_email (
  user_id	in users.user_id%TYPE
 )
 is
 begin
    update users
    set email_verified_p = 't'
    where user_id = approve_email.user_id;
 end approve_email;


 procedure unapprove_email (
  user_id	in users.user_id%TYPE
 )
 is
 begin
    update users
    set email_verified_p = 'f'
    where user_id = unapprove_email.user_id;
 end unapprove_email;

 procedure delete (
  user_id	in users.user_id%TYPE
 )
 is
 begin
  delete from user_preferences
  where user_id = acs_user.delete.user_id;

  delete from users
  where user_id = acs_user.delete.user_id;

  person.delete(user_id);
 end delete;

end acs_user;
/
show errors

-- from acs-relationships-create.sql

create or replace package acs_rel
as

  function new (
    rel_id		in acs_rels.rel_id%TYPE default null,
    rel_type		in acs_rels.rel_type%TYPE default 'relationship',
    object_id_one	in acs_rels.object_id_one%TYPE,
    object_id_two	in acs_rels.object_id_two%TYPE,
    context_id		in acs_objects.context_id%TYPE default null,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null
  ) return acs_rels.rel_id%TYPE;

  procedure delete (
    rel_id	in acs_rels.rel_id%TYPE
  );

end;
/
show errors

create or replace package body acs_rel
as

  function new (
    rel_id		in acs_rels.rel_id%TYPE default null,
    rel_type		in acs_rels.rel_type%TYPE default 'relationship',
    object_id_one	in acs_rels.object_id_one%TYPE,
    object_id_two	in acs_rels.object_id_two%TYPE,
    context_id		in acs_objects.context_id%TYPE default null,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null
  ) return acs_rels.rel_id%TYPE
  is
    v_rel_id acs_rels.rel_id%TYPE;
  begin
    -- XXX This should check that object_id_one and object_id_two are
    -- of the appropriate types.
    v_rel_id := acs_object.new (
      object_id => rel_id,
      object_type => rel_type,
      context_id => context_id,
      creation_user => creation_user,
      creation_ip => creation_ip
    );

    insert into acs_rels
     (rel_id, rel_type, object_id_one, object_id_two)
    values
     (v_rel_id, new.rel_type, new.object_id_one, new.object_id_two);

     return v_rel_id;
  end;

  procedure delete (
    rel_id	in acs_rels.rel_id%TYPE
  )
  is
  begin
    delete from acs_rels
    where rel_id = acs_rel.delete.rel_id;

    acs_object.delete(rel_id);
  end;

end;
/
show errors

-- from groups-create.sql

create or replace package membership_rel
as

  function new (
    rel_id		in membership_rels.rel_id%TYPE default null,
    rel_type		in acs_rels.rel_type%TYPE default 'membership_rel',
    object_id_one	in acs_rels.object_id_one%TYPE,
    object_id_two	in acs_rels.object_id_two%TYPE,
    member_state	in membership_rels.member_state%TYPE default null,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null
  ) return membership_rels.rel_id%TYPE;

  procedure ban (
    rel_id	in membership_rels.rel_id%TYPE
  );

  procedure approve (
    rel_id	in membership_rels.rel_id%TYPE
  );

  procedure reject (
    rel_id	in membership_rels.rel_id%TYPE
  );

  procedure unapprove (
    rel_id	in membership_rels.rel_id%TYPE
  );

  procedure deleted (
    rel_id	in membership_rels.rel_id%TYPE
  );

  procedure delete (
    rel_id	in membership_rels.rel_id%TYPE
  );

end membership_rel;
/
show errors

create or replace package body membership_rel
as

  function new (
    rel_id		in membership_rels.rel_id%TYPE default null,
    rel_type		in acs_rels.rel_type%TYPE default 'membership_rel',
    object_id_one	in acs_rels.object_id_one%TYPE,
    object_id_two	in acs_rels.object_id_two%TYPE,
    member_state	in membership_rels.member_state%TYPE default null,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null
  ) return membership_rels.rel_id%TYPE
  is
    v_rel_id integer;
  begin
    v_rel_id := acs_rel.new (
      rel_id => rel_id,
      rel_type => rel_type,
      object_id_one => object_id_one,
      object_id_two => object_id_two,
      context_id => object_id_one,
      creation_user => creation_user,
      creation_ip => creation_ip
    );

    insert into membership_rels
     (rel_id, member_state)
    values
     (v_rel_id, new.member_state);

    return v_rel_id;
  end;

  procedure ban (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    update membership_rels
    set member_state = 'banned'
    where rel_id = ban.rel_id;
  end;

  procedure approve (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    update membership_rels
    set member_state = 'approved'
    where rel_id = approve.rel_id;
  end;

  procedure reject (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    update membership_rels
    set member_state = 'rejected'
    where rel_id = reject.rel_id;
  end;

  procedure unapprove (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    update membership_rels
    set member_state = ''
    where rel_id = unapprove.rel_id;
  end;

  procedure deleted (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    update membership_rels
    set member_state = 'deleted'
    where rel_id = deleted.rel_id;
  end;

  procedure delete (
    rel_id	in membership_rels.rel_id%TYPE
  )
  is
  begin
    delete from membership_rels
    where rel_id = membership_rel.delete.rel_id;

    acs_rel.delete(rel_id);
  end;

end membership_rel;
/
show errors

create or replace package acs_group
is
 function new (
  group_id		in groups.group_id%TYPE default null,
  object_type		in acs_objects.object_type%TYPE
			   default 'group',
  creation_date		in acs_objects.creation_date%TYPE
			   default sysdate,
  creation_user		in acs_objects.creation_user%TYPE
			   default null,
  creation_ip		in acs_objects.creation_ip%TYPE default null,
  email			in parties.email%TYPE default null,
  url			in parties.url%TYPE default null,
  group_name            in groups.group_name%TYPE
 ) return groups.group_id%TYPE;

 procedure delete (
   group_id     in groups.group_id%TYPE
 );

 function name (
  group_id	in groups.group_id%TYPE
 ) return varchar2;

 function member_p (
  party_id	in parties.party_id%TYPE
 ) return char;

end acs_group;
/
show errors

create or replace package body acs_group
is
 function new (
  group_id		in groups.group_id%TYPE default null,
  object_type		in acs_objects.object_type%TYPE
			   default 'group',
  creation_date		in acs_objects.creation_date%TYPE
			   default sysdate,
  creation_user		in acs_objects.creation_user%TYPE
			   default null,
  creation_ip		in acs_objects.creation_ip%TYPE default null,
  email			in parties.email%TYPE default null,
  url			in parties.url%TYPE default null,
  group_name            in groups.group_name%TYPE
 )
 return groups.group_id%TYPE
 is
  v_group_id groups.group_id%TYPE;
 begin
  v_group_id :=
   party.new(group_id, object_type,
             creation_date, creation_user, creation_ip, email, url);

  insert into groups
   (group_id, group_name)
  values
   (v_group_id, group_name);

  return v_group_id;
 end new;

 procedure delete (
   group_id     in groups.group_id%TYPE
 )
 is
 begin

   for row in (select m.rel_id
	       from acs_rels r, membership_rels m
	       where r.rel_id = m.rel_id
	       and (r.object_id_one = group_id
	            or r.object_id_two = group_id)) loop
     membership_rel.delete(row.rel_id);
   end loop;

   for row in (select c.rel_id
	       from acs_rels r, composition_rels c
	       where r.rel_id = c.rel_id
	       and (r.object_id_one = group_id
	            or r.object_id_two = group_id)) loop
     composition_rel.delete(row.rel_id);
   end loop;

   acs_object.delete(group_id);
 end delete;

 function name (
  group_id	in groups.group_id%TYPE
 )
 return varchar2
 is
  group_name varchar2(200);
 begin
  select group_name
  into group_name
  from groups
  where group_id = name.group_id;

  return group_name;
 end name;

 function member_p (
  party_id	in parties.party_id%TYPE
 )
 return char
 is
 begin
  -- TO DO: implement this for real
  return 't';
 end member_p;

end acs_group;
/
show errors

-- from apm
create or replace package apm
as
  function register_package (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    package_type		in apm_package_types.package_type%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_type%TYPE;

  function update_package (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE
    	    	    	    	default null,
    pretty_plural		in apm_package_types.pretty_plural%TYPE
    	    	    	    	default null,
    package_uri			in apm_package_types.package_uri%TYPE
    	    	    	    	default null,
    package_type		in apm_package_types.package_type%TYPE
    	    	    	    	default null,
    singleton_p			in apm_package_types.singleton_p%TYPE 
    	    	    	    	default null,    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
    	    	    	    	default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_type%TYPE;   
   
  procedure unregister_package (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 't'
  );

  function register_p (
    package_key		in apm_package_types.package_key%TYPE
  ) return integer;

  -- Informs the APM that this application is available for use.
  function register_application (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_key%TYPE;

  -- Remove the application from the system. 
  procedure unregister_application (
    package_key		in apm_package_types.package_key%TYPE,
    -- Delete all objects associated with this application.	
    cascade_p		in char default 'f'
  ); 

  function register_service (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_key%TYPE;

  -- Remove the service from the system. 
  procedure unregister_service (
    package_key		in apm_package_types.package_key%TYPE,
    -- Delete all objects associated with this service.	
    cascade_p		in char default 'f'
  ); 

  -- Indicate to APM that a parameter is available to the system.
  function register_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE 
				default null,
    package_key			in apm_parameters.package_key%TYPE,				
    parameter_name		in apm_parameters.parameter_name%TYPE,
    description			in apm_parameters.description%TYPE
				default null,
    datatype			in apm_parameters.datatype%TYPE 
				default 'string',
    default_value		in apm_parameters.default_value%TYPE 
				default null,
    section_name		in apm_parameters.section_name%TYPE
				default null,
    min_n_values		in apm_parameters.min_n_values%TYPE 
				default 1,
    max_n_values		in apm_parameters.max_n_values%TYPE 
				default 1
  ) return apm_parameters.parameter_id%TYPE;

  function update_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE
    	    	    	    	default null,
    description			in apm_parameters.description%TYPE
				default null,
    datatype			in apm_parameters.datatype%TYPE 
				default 'string',
    default_value		in apm_parameters.default_value%TYPE 
				default null,
    section_name		in apm_parameters.section_name%TYPE
				default null,
    min_n_values		in apm_parameters.min_n_values%TYPE 
				default 1,
    max_n_values		in apm_parameters.max_n_values%TYPE 
				default 1
  ) return apm_parameters.parameter_name%TYPE;

  function parameter_p(
    package_key                 in apm_package_types.package_key%TYPE,
    parameter_name              in apm_parameters.parameter_name%TYPE
  ) return integer;

  -- Remove any uses of this parameter.
  procedure unregister_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE 
				default null
  );

  -- Return the value of this parameter for a specific package and parameter.
  function get_value (
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    package_id			in apm_packages.package_id%TYPE		    
  ) return apm_parameter_values.attr_value%TYPE;

  function get_value (
    package_id			in apm_packages.package_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE
  ) return apm_parameter_values.attr_value%TYPE;

  -- Sets a value for a parameter for a package instance.
  procedure set_value (
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    package_id			in apm_packages.package_id%TYPE,	    
    attr_value			in apm_parameter_values.attr_value%TYPE
  );

  procedure set_value (
    package_id			in apm_packages.package_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE,
    attr_value			in apm_parameter_values.attr_value%TYPE
  );	
    		    

end apm;
/
show errors

create or replace package apm_package
as

function new (
  package_id		in apm_packages.package_id%TYPE 
			default null,
  instance_name		in apm_packages.instance_name%TYPE
			default null,
  package_key		in apm_packages.package_key%TYPE,
  object_type		in acs_objects.object_type%TYPE
			default 'apm_package', 
  creation_date		in acs_objects.creation_date%TYPE 
			default sysdate,
  creation_user		in acs_objects.creation_user%TYPE 
			default null,
  creation_ip		in acs_objects.creation_ip%TYPE 
			default null,
  context_id		in acs_objects.context_id%TYPE 
			default null
  ) return apm_packages.package_id%TYPE;

  procedure delete (
   package_id		in apm_packages.package_id%TYPE
  );

  function singleton_p (
	package_key		in apm_packages.package_key%TYPE
  ) return integer;

  function num_instances (
	package_key		in apm_package_types.package_key%TYPE
  ) return integer;

  function name (
    package_id		in apm_packages.package_id%TYPE
  ) return varchar2;

 -- Enable a package to be utilized by a subsite.
  procedure enable (
   package_id		in apm_packages.package_id%TYPE
  );
  
  procedure disable (
   package_id		in apm_packages.package_id%TYPE
  );

  function highest_version (
   package_key		in apm_package_types.package_key%TYPE
  ) return apm_package_versions.version_id%TYPE;
  
end apm_package;
/
show errors

create or replace package apm_package_version
as
  function new (
    version_id			in apm_package_versions.version_id%TYPE
					default null,
    package_key			in apm_package_versions.package_key%TYPE,
    version_name		in apm_package_versions.version_name%TYPE 
					default null,
    version_uri			in apm_package_versions.version_uri%TYPE,
    summary			in apm_package_versions.summary%TYPE,
    description_format		in apm_package_versions.description_format%TYPE,
    description			in apm_package_versions.description%TYPE,
    release_date		in apm_package_versions.release_date%TYPE,
    vendor			in apm_package_versions.vendor%TYPE,
    vendor_uri			in apm_package_versions.vendor_uri%TYPE,
    installed_p			in apm_package_versions.installed_p%TYPE
					default 'f',
    data_model_loaded_p		in apm_package_versions.data_model_loaded_p%TYPE
				        default 'f'
  ) return apm_package_versions.version_id%TYPE;

  procedure delete (
      version_id		in apm_packages.package_id%TYPE
  );

  procedure enable (
       version_id			in apm_package_versions.version_id%TYPE
  );

  procedure disable (
       version_id			in apm_package_versions.version_id%TYPE
  );

 function edit (
      new_version_id		in apm_package_versions.version_id%TYPE
				default null,
      version_id		in apm_package_versions.version_id%TYPE,
      version_name		in apm_package_versions.version_name%TYPE 
				default null,
      version_uri		in apm_package_versions.version_uri%TYPE,
      summary			in apm_package_versions.summary%TYPE,
      description_format	in apm_package_versions.description_format%TYPE,
      description		in apm_package_versions.description%TYPE,
      release_date		in apm_package_versions.release_date%TYPE,
      vendor			in apm_package_versions.vendor%TYPE,
      vendor_uri		in apm_package_versions.vendor_uri%TYPE,
      installed_p		in apm_package_versions.installed_p%TYPE
				default 'f',
      data_model_loaded_p	in apm_package_versions.data_model_loaded_p%TYPE
				default 'f'
    ) return apm_package_versions.version_id%TYPE;

  -- Add a file to the indicated version.
  function add_file(
    file_id			in apm_package_files.file_id%TYPE
				default null,
    version_id			in apm_package_versions.version_id%TYPE,
    path			in apm_package_files.path%TYPE,
    file_type			in apm_package_file_types.file_type_key%TYPE
  ) return apm_package_files.file_id%TYPE;

  -- Remove a file from the indicated version.
  procedure remove_file(
    version_id			in apm_package_versions.version_id%TYPE,
    path			in apm_package_files.path%TYPE
  );

  -- Add an interface provided by this version.
  function add_interface(
    interface_id		in apm_package_dependencies.dependency_id%TYPE
			        default null,
    version_id			in apm_package_versions.version_id%TYPE,
    interface_uri		in apm_package_dependencies.service_uri%TYPE,
    interface_version		in apm_package_dependencies.service_version%TYPE
  ) return apm_package_dependencies.dependency_id%TYPE;

  procedure remove_interface(
    interface_id		in apm_package_dependencies.dependency_id%TYPE
  );

  procedure remove_interface(
    interface_uri		in apm_package_dependencies.service_uri%TYPE,
    interface_version		in apm_package_dependencies.service_version%TYPE,
    version_id			in apm_package_versions.version_id%TYPE
  );

  -- Add a requirement for this version.  A requirement is some interface that this
  -- version depends on.
  function add_dependency(
    dependency_id		in apm_package_dependencies.dependency_id%TYPE
			        default null,
    version_id			in apm_package_versions.version_id%TYPE,
    dependency_uri		in apm_package_dependencies.service_uri%TYPE,
    dependency_version		in apm_package_dependencies.service_version%TYPE
  ) return apm_package_dependencies.dependency_id%TYPE;

  procedure remove_dependency(
    dependency_id		in apm_package_dependencies.dependency_id%TYPE
  );

  procedure remove_dependency(
    dependency_uri		in apm_package_dependencies.service_uri%TYPE,
    dependency_version		in apm_package_dependencies.service_version%TYPE,
    version_id			in apm_package_versions.version_id%TYPE
  );

  -- Given a version_name (e.g. 3.2a), return
  -- something that can be lexicographically sorted.
  function sortable_version_name (
    version_name		in apm_package_versions.version_name%TYPE
  ) return varchar2;

  -- Given two version names, return 1 if one > two, -1 if two > one, 0 otherwise. 
  -- Deprecate?
  function compare(
    version_name_one		in apm_package_versions.version_name%TYPE,
    version_name_two		in apm_package_versions.version_name%TYPE
  ) return integer;

end apm_package_version;
/
show errors

create or replace package apm_package_type
as
 function create_type(
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in acs_object_types.pretty_name%TYPE,
    pretty_plural		in acs_object_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    package_type		in apm_package_types.package_type%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE,
    spec_file_path		in apm_package_types.spec_file_path%TYPE default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE default null
  ) return apm_package_types.package_type%TYPE;

  function update_type (    
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in acs_object_types.pretty_name%TYPE
    	    	    	    	default null,
    pretty_plural		in acs_object_types.pretty_plural%TYPE
    	    	    	    	default null,
    package_uri			in apm_package_types.package_uri%TYPE
    	    	    	    	default null,    
    package_type		in apm_package_types.package_type%TYPE
    	    	    	    	default null,
    singleton_p			in apm_package_types.singleton_p%TYPE
    	    	    	    	default null,
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
    	    	    	    	default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE
    	    	    	    	 default null
  ) return apm_package_types.package_type%TYPE;
  
  procedure drop_type (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 'f'
  );

end apm_package_type;
/
show errors



-- Private APM System API for managing parameter values.
create or replace package apm_parameter_value
as
  function new (
    value_id			in apm_parameter_values.value_id%TYPE default null,
    package_id			in apm_packages.package_id%TYPE,
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    attr_value			in apm_parameter_values.attr_value%TYPE
  ) return apm_parameter_values.value_id%TYPE;

  procedure delete (
    value_id			in apm_parameter_values.value_id%TYPE default null
  );
 end apm_parameter_value;
/
show errors

create or replace package apm_application
as

function new (
    application_id	in acs_objects.object_id%TYPE default null,
    instance_name	in apm_packages.instance_name%TYPE
			default null,
    package_key		in apm_package_types.package_key%TYPE,
    object_type		in acs_objects.object_type%TYPE
			   default 'apm_application',
    creation_date	in acs_objects.creation_date%TYPE default sysdate,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null,
    context_id		in acs_objects.context_id%TYPE default null
  ) return acs_objects.object_id%TYPE;

  procedure delete (
    application_id		in acs_objects.object_id%TYPE
  );

end;
/
show errors


create or replace package apm_service
as

  function new (
    service_id		in acs_objects.object_id%TYPE default null,
    instance_name	in apm_packages.instance_name%TYPE
			default null,
    package_key		in apm_package_types.package_key%TYPE,
    object_type		in acs_objects.object_type%TYPE default 'apm_service',
    creation_date	in acs_objects.creation_date%TYPE default sysdate,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null,
    context_id		in acs_objects.context_id%TYPE default null
  ) return acs_objects.object_id%TYPE;

  procedure delete (
    service_id		in acs_objects.object_id%TYPE
  );

end;
/
show errors

create or replace package body apm
as
  function register_package (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    package_type		in apm_package_types.package_type%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_type%TYPE
  is
  begin
    return apm_package_type.create_type(
    	package_key => register_package.package_key,
	pretty_name => register_package.pretty_name,
	pretty_plural => register_package.pretty_plural,
	package_uri => register_package.package_uri,
	package_type => register_package.package_type,
	singleton_p => register_package.singleton_p,
	spec_file_path => register_package.spec_file_path,
	spec_file_mtime => spec_file_mtime
    );
  end register_package;

  function update_package (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE
    	    	    	    	default null,
    pretty_plural		in apm_package_types.pretty_plural%TYPE
    	    	    	    	default null,
    package_uri			in apm_package_types.package_uri%TYPE
    	    	    	    	default null,
    package_type		in apm_package_types.package_type%TYPE
    	    	    	    	default null,
    singleton_p			in apm_package_types.singleton_p%TYPE 
    	    	    	    	default null,    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
    	    	    	    	default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_type%TYPE
  is
  begin
 
    return apm_package_type.update_type(
    	package_key => update_package.package_key,
	pretty_name => update_package.pretty_name,
	pretty_plural => update_package.pretty_plural,
	package_uri => update_package.package_uri,
	package_type => update_package.package_type,
	singleton_p => update_package.singleton_p,
	spec_file_path => update_package.spec_file_path,
	spec_file_mtime => update_package.spec_file_mtime
    );

  end update_package;    


 procedure unregister_package (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 't'
  )
  is
  begin
   apm_package_type.drop_type(
	package_key => unregister_package.package_key,
	cascade_p => unregister_package.cascade_p
   );
  end unregister_package;

  function register_p (
    package_key		in apm_package_types.package_key%TYPE
  ) return integer
  is
    v_register_p integer;
  begin
    select decode(count(*),0,0,1) into v_register_p from apm_package_types 
    where package_key = register_p.package_key;
    return v_register_p;
  end register_p;

  function register_application (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_key%TYPE
  is
  begin
   return apm.register_package(
	package_key => register_application.package_key,
	pretty_name => register_application.pretty_name,
	pretty_plural => register_application.pretty_plural,
	package_uri => register_application.package_uri,
	package_type => 'apm_application',
	singleton_p => register_application.singleton_p,
	spec_file_path => register_application.spec_file_path,
	spec_file_mtime => register_application.spec_file_mtime
   ); 
  end register_application;  

  procedure unregister_application (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 'f'
  )
  is
  begin
   apm.unregister_package (
	package_key => unregister_application.package_key,
	cascade_p => unregister_application.cascade_p
   );
  end unregister_application; 

  function register_service (
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in apm_package_types.pretty_name%TYPE,
    pretty_plural		in apm_package_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE 
				default 'f',    
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
				default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE 
				default null
  ) return apm_package_types.package_key%TYPE
  is
  begin
   return apm.register_package(
	package_key => register_service.package_key,
	pretty_name => register_service.pretty_name,
	pretty_plural => register_service.pretty_plural,
	package_uri => register_service.package_uri,
	package_type => 'apm_service',
	singleton_p => register_service.singleton_p,
	spec_file_path => register_service.spec_file_path,
	spec_file_mtime => register_service.spec_file_mtime
   );   
  end register_service;

  procedure unregister_service (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 'f'
  )
  is
  begin
   apm.unregister_package (
	package_key => unregister_service.package_key,
	cascade_p => unregister_service.cascade_p
   );
  end unregister_service;

  -- Indicate to APM that a parameter is available to the system.
  function register_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE 
				default null,
    package_key			in apm_parameters.package_key%TYPE,				
    parameter_name		in apm_parameters.parameter_name%TYPE,
    description			in apm_parameters.description%TYPE
				default null,
    datatype			in apm_parameters.datatype%TYPE 
				default 'string',
    default_value		in apm_parameters.default_value%TYPE 
				default null,
    section_name		in apm_parameters.section_name%TYPE
				default null,
    min_n_values		in apm_parameters.min_n_values%TYPE 
				default 1,
    max_n_values		in apm_parameters.max_n_values%TYPE 
				default 1
  ) return apm_parameters.parameter_id%TYPE
  is
    v_parameter_id apm_parameters.parameter_id%TYPE;
  begin
    
    v_parameter_id := acs_object.new(
      object_id => parameter_id,
      object_type => 'apm_parameter'
    );
    insert into apm_parameters 
    (parameter_id, parameter_name, description, package_key, datatype, 
    default_value, section_name, min_n_values, max_n_values)
    values
    (v_parameter_id, register_parameter.parameter_name, register_parameter.description,
    register_parameter.package_key, register_parameter.datatype, 
    register_parameter.default_value, register_parameter.section_name, 
	register_parameter.min_n_values, register_parameter.max_n_values);	
	
    return v_parameter_id;
    -- XXX: create an attribute using the metadata system.

  end register_parameter;

  function update_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE
    	    	    	    	default null,
    description			in apm_parameters.description%TYPE
				default null,
    datatype			in apm_parameters.datatype%TYPE 
				default 'string',
    default_value		in apm_parameters.default_value%TYPE 
				default null,
    section_name		in apm_parameters.section_name%TYPE
				default null,
    min_n_values		in apm_parameters.min_n_values%TYPE 
				default 1,
    max_n_values		in apm_parameters.max_n_values%TYPE 
				default 1
  ) return apm_parameters.parameter_name%TYPE
  is
  begin
    update apm_parameters 
	set parameter_name = nvl(update_parameter.parameter_name, parameter_name),
            default_value  = nvl(update_parameter.default_value, default_value),
            datatype       = nvl(update_parameter.datatype, datatype), 
	    description	   = nvl(update_parameter.description, description),
	    section_name   = nvl(update_parameter.section_name, section_name),
            min_n_values   = nvl(update_parameter.min_n_values, min_n_values),
            max_n_values   = nvl(update_parameter.max_n_values, max_n_values)
      where parameter_id = update_parameter.parameter_id;
    return parameter_id;
  end;

  function parameter_p(
    package_key                 in apm_package_types.package_key%TYPE,
    parameter_name              in apm_parameters.parameter_name%TYPE
  ) return integer 
  is
    v_parameter_p integer;
  begin
    select decode(count(*),0,0,1) into v_parameter_p 
    from apm_parameters
    where package_key = parameter_p.package_key
    and parameter_name = parameter_p.parameter_name;
    return v_parameter_p;
  end parameter_p;

  procedure unregister_parameter (
    parameter_id		in apm_parameters.parameter_id%TYPE 
				default null
  )
  is
  begin
    delete from apm_parameter_values 
    where parameter_id = unregister_parameter.parameter_id;
    delete from apm_parameters 
    where parameter_id = unregister_parameter.parameter_id;
    acs_object.delete(parameter_id);
  end unregister_parameter;

  function id_for_name (
    parameter_name		in apm_parameters.parameter_name%TYPE,
    package_key			in apm_parameters.package_key%TYPE
  ) return apm_parameters.parameter_id%TYPE
  is
    a_parameter_id apm_parameters.parameter_id%TYPE; 
  begin
    select parameter_id into a_parameter_id
    from apm_parameters p
    where p.parameter_name = id_for_name.parameter_name and
          p.package_key = id_for_name.package_key;
    return a_parameter_id;
  end id_for_name;
		
  function get_value (
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    package_id			in apm_packages.package_id%TYPE		    
  ) return apm_parameter_values.attr_value%TYPE
  is
    value apm_parameter_values.attr_value%TYPE;
  begin
    select attr_value into value from apm_parameter_values v
    where v.package_id = get_value.package_id
    and parameter_id = get_value.parameter_id;
    return value;
  end get_value;

  function get_value (
    package_id			in apm_packages.package_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE
  ) return apm_parameter_values.attr_value%TYPE
  is
    v_parameter_id apm_parameter_values.parameter_id%TYPE;
  begin
    select parameter_id into v_parameter_id 
    from apm_parameters 
    where parameter_name = get_value.parameter_name
    and package_key = (select package_key  from apm_packages
			where package_id = get_value.package_id);
    return apm.get_value(
	parameter_id => v_parameter_id,
	package_id => get_value.package_id
    );	
  end get_value;	


  -- Sets a value for a parameter for a package instance.
  procedure set_value (
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    package_id			in apm_packages.package_id%TYPE,	    
    attr_value			in apm_parameter_values.attr_value%TYPE
  ) 
  is
    v_value_id apm_parameter_values.value_id%TYPE;
  begin
    -- Determine if the value exists
    select value_id into v_value_id from apm_parameter_values 
     where parameter_id = set_value.parameter_id 
     and package_id = set_value.package_id;
    update apm_parameter_values set attr_value = set_value.attr_value
     where parameter_id = set_value.parameter_id 
     and package_id = set_value.package_id;    
     exception 
       when NO_DATA_FOUND
       then
         v_value_id := apm_parameter_value.new(
            package_id => package_id,
            parameter_id => parameter_id,
            attr_value => attr_value
         );
   end set_value;

  procedure set_value (
    package_id			in apm_packages.package_id%TYPE,
    parameter_name		in apm_parameters.parameter_name%TYPE,
    attr_value			in apm_parameter_values.attr_value%TYPE
  ) 
  is
    v_parameter_id apm_parameter_values.parameter_id%TYPE;
  begin
    select parameter_id into v_parameter_id 
    from apm_parameters 
    where parameter_name = set_value.parameter_name
    and package_key = (select package_key  from apm_packages
			where package_id = set_value.package_id);
    apm.set_value(
	parameter_id => v_parameter_id,
	package_id => set_value.package_id,
	attr_value => set_value.attr_value
    );	
  end set_value;	
end apm;
/
show errors  

create or replace package body apm_package
as
  procedure initialize_parameters (
    package_id			in apm_packages.package_id%TYPE,
    package_key		        in apm_package_types.package_key%TYPE
  )
  is
   v_value_id apm_parameter_values.value_id%TYPE;
   cursor cur is
       select parameter_id, default_value
       from apm_parameters
       where package_key = initialize_parameters.package_key;
  begin
    -- need to initialize all params for this type
    for cur_val in cur
      loop
        v_value_id := apm_parameter_value.new(
          package_id => initialize_parameters.package_id,
          parameter_id => cur_val.parameter_id,
          attr_value => cur_val.default_value
        ); 
      end loop;   
  end initialize_parameters;

 function new (
  package_id		in apm_packages.package_id%TYPE 
			default null,
  instance_name		in apm_packages.instance_name%TYPE
			default null,
  package_key		in apm_packages.package_key%TYPE,
  object_type		in acs_objects.object_type%TYPE
			default 'apm_package', 
  creation_date		in acs_objects.creation_date%TYPE 
			default sysdate,
  creation_user		in acs_objects.creation_user%TYPE 
			default null,
  creation_ip		in acs_objects.creation_ip%TYPE 
			default null,
  context_id		in acs_objects.context_id%TYPE 
			default null
  ) return apm_packages.package_id%TYPE
  is 
   v_singleton_p integer;
   v_package_type apm_package_types.package_type%TYPE;
   v_num_instances integer;
   v_package_id apm_packages.package_id%TYPE;
   v_instance_name apm_packages.instance_name%TYPE; 
  begin
   v_singleton_p := apm_package.singleton_p(
			package_key => apm_package.new.package_key
		    );
   v_num_instances := apm_package.num_instances(
			package_key => apm_package.new.package_key
		    );
  
   if v_singleton_p = 1 and v_num_instances >= 1 then
       select package_id into v_package_id 
       from apm_packages
       where package_key = apm_package.new.package_key;
       return v_package_id;
   else
       v_package_id := acs_object.new(
          object_id => package_id,
          object_type => object_type,
          creation_date => creation_date,
          creation_user => creation_user,
	  creation_ip => creation_ip,
	  context_id => context_id
	 );
       if instance_name is null then 
	 v_instance_name := package_key || ' ' || v_package_id;
       else
	 v_instance_name := instance_name;
       end if;

       select package_type into v_package_type
       from apm_package_types
       where package_key = apm_package.new.package_key;

       insert into apm_packages
       (package_id, package_key, instance_name)
       values
       (v_package_id, package_key, v_instance_name);

       if v_package_type = 'apm_application' then
	   insert into apm_applications
	   (application_id)
	   values
	   (v_package_id);
       else
	   insert into apm_services
	   (service_id)
	   values
	   (v_package_id);
       end if;

       initialize_parameters(
	   package_id => v_package_id,
	   package_key => apm_package.new.package_key
       );
       return v_package_id;

  end if;
end new;
  
  procedure delete (
   package_id		in apm_packages.package_id%TYPE
  )
  is
    cursor all_values is
    	select value_id from apm_parameter_values
	where package_id = apm_package.delete.package_id;
    cursor all_site_nodes is
    	select node_id from site_nodes
	where object_id = apm_package.delete.package_id;
  begin
    -- Delete all parameters.
    for cur_val in all_values loop
    	apm_parameter_value.delete(value_id => cur_val.value_id);
    end loop;    
    delete from apm_applications where application_id = apm_package.delete.package_id;
    delete from apm_services where service_id = apm_package.delete.package_id;
    delete from apm_packages where package_id = apm_package.delete.package_id;
    -- Delete the site nodes for the objects.
    for cur_val in all_site_nodes loop
    	site_node.delete(cur_val.node_id);
    end loop;
    -- Delete the object.
    acs_object.delete (
	object_id => package_id
    );
   end delete;

    function singleton_p (
	package_key		in apm_packages.package_key%TYPE
    ) return integer
    is
        v_singleton_p integer;
    begin
        select 1 into v_singleton_p
	from apm_package_types
	where package_key = singleton_p.package_key
        and singleton_p = 't';
	return v_singleton_p;
	
	exception 
	    when NO_DATA_FOUND
            then
                return 0;
    end singleton_p;

    function num_instances (
	package_key		in apm_package_types.package_key%TYPE
    ) return integer
    is
        v_num_instances integer;
    begin
        select count(*) into v_num_instances
	from apm_packages
	where package_key = num_instances.package_key;
        return v_num_instances;
	
	exception
	    when NO_DATA_FOUND
	    then
	        return 0;
    end num_instances;

  function name (
    package_id		in apm_packages.package_id%TYPE
  ) return varchar2
  is
    v_result apm_packages.instance_name%TYPE;
  begin
    select instance_name into v_result
    from apm_packages
    where package_id = name.package_id;

    return v_result;
  end name;

    procedure enable (
       package_id			in apm_packages.package_id%TYPE
    )
    is
    begin
      update apm_packages 
      set enabled_p = 't'
      where package_id = enable.package_id;	
    end enable;
    
    procedure disable (
       package_id			in apm_packages.package_id%TYPE
    )
    is
    begin
      update apm_packages 
      set enabled_p = 'f'
      where package_id = disable.package_id;	
    end disable;
	
   function highest_version (
     package_key		in apm_package_types.package_key%TYPE
   ) return apm_package_versions.version_id%TYPE
   is
     v_version_id apm_package_versions.version_id%TYPE;
   begin
     select version_id into v_version_id
	from apm_package_version_info i 
	where apm_package_version.sortable_version_name(version_name) = 
             (select max(apm_package_version.sortable_version_name(v.version_name))
	             from apm_package_version_info v where v.package_key = highest_version.package_key)
	and package_key = highest_version.package_key;
     return v_version_id;
     exception
         when NO_DATA_FOUND
         then
         return 0;
   end highest_version;
end apm_package;
/
show errors

create or replace package body apm_package_version 
as
    function new (
      version_id		in apm_package_versions.version_id%TYPE
				default null,
      package_key		in apm_package_versions.package_key%TYPE,
      version_name		in apm_package_versions.version_name%TYPE 
				default null,
      version_uri		in apm_package_versions.version_uri%TYPE,
      summary			in apm_package_versions.summary%TYPE,
      description_format	in apm_package_versions.description_format%TYPE,
      description		in apm_package_versions.description%TYPE,
      release_date		in apm_package_versions.release_date%TYPE,
      vendor			in apm_package_versions.vendor%TYPE,
      vendor_uri		in apm_package_versions.vendor_uri%TYPE,
      installed_p		in apm_package_versions.installed_p%TYPE
				default 'f',
      data_model_loaded_p	in apm_package_versions.data_model_loaded_p%TYPE
				default 'f'
    ) return apm_package_versions.version_id%TYPE
    is
      v_version_id apm_package_versions.version_id%TYPE;
    begin
      if version_id is null then
         select acs_object_id_seq.nextval
	 into v_version_id
	 from dual;
      else
         v_version_id := version_id;
      end if;
	v_version_id := acs_object.new(
		object_id => version_id,
		object_type => 'apm_package_version'
        );
      insert into apm_package_versions
      (version_id, package_key, version_name, version_uri, summary, description_format, description,
      release_date, vendor, vendor_uri, installed_p, data_model_loaded_p)
      values
      (v_version_id, package_key, version_name, version_uri,
       summary, description_format, description,
       release_date, vendor, vendor_uri,
       installed_p, data_model_loaded_p);
      return v_version_id;		
    end new;

    procedure delete (
      version_id		in apm_packages.package_id%TYPE
    )
    is
        v_package_key apm_package_types.package_key%TYPE;    	
    	v_num_versions integer;
    begin
      select package_key into v_package_key
      from apm_package_versions
      where version_id = apm_package_version.delete.version_id;
      delete from apm_package_owners 
      where version_id = apm_package_version.delete.version_id; 
      delete from apm_package_files
      where version_id = apm_package_version.delete.version_id;
      delete from apm_package_dependencies
      where version_id = apm_package_version.delete.version_id;
      delete from apm_package_versions 
	where version_id = apm_package_version.delete.version_id;
      acs_object.delete(apm_package_version.delete.version_id);
      select count(*) into v_num_versions
      from apm_package_versions
      where package_key = v_package_key;
      if v_num_versions = 0 then
        apm.unregister_package(package_key => v_package_key);
      end if;
    end delete;

    procedure enable (
       version_id			in apm_package_versions.version_id%TYPE
    )
    is
    begin
      update apm_package_versions set enabled_p = 't'
      where version_id = enable.version_id;	
    end enable;
    
    procedure disable (
       version_id			in apm_package_versions.version_id%TYPE
    )
    is
    begin
      update apm_package_versions 
      set enabled_p = 'f'
      where version_id = disable.version_id;	
    end disable;


  function copy(
	version_id in apm_package_versions.version_id%TYPE,
	new_version_id in apm_package_versions.version_id%TYPE default null,
	new_version_name in apm_package_versions.version_name%TYPE,
	new_version_uri in apm_package_versions.version_uri%TYPE
  ) return apm_package_versions.version_id%TYPE
    is
	v_version_id integer;
    begin
	v_version_id := acs_object.new(
		object_id => new_version_id,
		object_type => 'apm_package_version'
        );    

	insert into apm_package_versions(version_id, package_key, version_name,
					version_uri, summary, description_format, description,
					release_date, vendor, vendor_uri)
	    select v_version_id, package_key, copy.new_version_name,
		   copy.new_version_uri, summary, description_format, description,
		   release_date, vendor, vendor_uri
	    from apm_package_versions
	    where version_id = copy.version_id;
    
	insert into apm_package_dependencies(dependency_id, version_id, dependency_type, service_uri, service_version)
	    select acs_object_id_seq.nextval, v_version_id, dependency_type, service_uri, service_version
	    from apm_package_dependencies
	    where version_id = copy.version_id;
    
	insert into apm_package_files(file_id, version_id, path, file_type)
	    select acs_object_id_seq.nextval, v_version_id, path, file_type
	    from apm_package_files
	    where version_id = copy.version_id;
    
	insert into apm_package_owners(version_id, owner_uri, owner_name, sort_key)
	    select v_version_id, owner_uri, owner_name, sort_key
	    from apm_package_owners
	    where version_id = copy.version_id;
    
	return v_version_id;
    end copy;
    
    function edit (
      new_version_id		in apm_package_versions.version_id%TYPE
				default null,
      version_id		in apm_package_versions.version_id%TYPE,
      version_name		in apm_package_versions.version_name%TYPE 
				default null,
      version_uri		in apm_package_versions.version_uri%TYPE,
      summary			in apm_package_versions.summary%TYPE,
      description_format	in apm_package_versions.description_format%TYPE,
      description		in apm_package_versions.description%TYPE,
      release_date		in apm_package_versions.release_date%TYPE,
      vendor			in apm_package_versions.vendor%TYPE,
      vendor_uri		in apm_package_versions.vendor_uri%TYPE,
      installed_p		in apm_package_versions.installed_p%TYPE
				default 'f',
      data_model_loaded_p	in apm_package_versions.data_model_loaded_p%TYPE
				default 'f'
    ) return apm_package_versions.version_id%TYPE
    is 
      v_version_id apm_package_versions.version_id%TYPE;
      version_unchanged_p integer;
    begin
       -- Determine if version has changed.
       select decode(count(*),0,0,1) into version_unchanged_p
       from apm_package_versions
       where version_id = edit.version_id
       and version_name = edit.version_name;
       if version_unchanged_p < 1 then
         v_version_id := copy(
			 version_id => edit.version_id,
			 new_version_id => edit.new_version_id,
			 new_version_name => edit.version_name,
			 new_version_uri => edit.version_uri
			);
         else 
	   v_version_id := edit.version_id;			
       end if;
       
       update apm_package_versions 
		set version_uri = edit.version_uri,
		summary = edit.summary,
		description_format = edit.description_format,
		description = edit.description,
		release_date = trunc(sysdate),
		vendor = edit.vendor,
		vendor_uri = edit.vendor_uri,
		installed_p = edit.installed_p,
		data_model_loaded_p = edit.data_model_loaded_p
	    where version_id = v_version_id;
	return v_version_id;
    end edit;

  function add_file(
    file_id			in apm_package_files.file_id%TYPE
				default null,
    version_id			in apm_package_versions.version_id%TYPE,
    path			in apm_package_files.path%TYPE,
    file_type			in apm_package_file_types.file_type_key%TYPE
  ) return apm_package_files.file_id%TYPE
  is
    v_file_id apm_package_files.file_id%TYPE;
    v_file_exists_p integer;
  begin
	select file_id into v_file_id from apm_package_files
  	where version_id = add_file.version_id 
	and path = add_file.path;
        return v_file_id;
	exception 
	       when NO_DATA_FOUND
	       then
	       	if file_id is null then
	          select acs_object_id_seq.nextval into v_file_id from dual;
	        else
	          v_file_id := file_id;
	        end if;

  	        insert into apm_package_files 
		(file_id, version_id, path, file_type) 
		values 
		(v_file_id, add_file.version_id, add_file.path, add_file.file_type);
	        return v_file_id;
     end add_file;

  -- Remove a file from the indicated version.
  procedure remove_file(
    version_id			in apm_package_versions.version_id%TYPE,
    path			in apm_package_files.path%TYPE
  )
  is
  begin
    delete from apm_package_files 
    where version_id = remove_file.version_id
    and path = remove_file.path;
  end remove_file; 


-- Add an interface provided by this version.
  function add_interface(
    interface_id		in apm_package_dependencies.dependency_id%TYPE
			        default null,
    version_id			in apm_package_versions.version_id%TYPE,
    interface_uri		in apm_package_dependencies.service_uri%TYPE,
    interface_version		in apm_package_dependencies.service_version%TYPE
  ) return apm_package_dependencies.dependency_id%TYPE
  is
    v_dep_id apm_package_dependencies.dependency_id%TYPE;
  begin
      if add_interface.interface_id is null then
          select acs_object_id_seq.nextval into v_dep_id from dual;
      else
          v_dep_id := add_interface.interface_id;
      end if;
  
      insert into apm_package_dependencies
      (dependency_id, version_id, dependency_type, service_uri, service_version)
      values
      (v_dep_id, add_interface.version_id, 'provides', add_interface.interface_uri,
	add_interface.interface_version);
      return v_dep_id;
  end add_interface;

  procedure remove_interface(
    interface_id		in apm_package_dependencies.dependency_id%TYPE
  )
  is
  begin
    delete from apm_package_dependencies 
    where dependency_id = remove_interface.interface_id;
  end remove_interface;

  procedure remove_interface(
    interface_uri		in apm_package_dependencies.service_uri%TYPE,
    interface_version		in apm_package_dependencies.service_version%TYPE,
    version_id			in apm_package_versions.version_id%TYPE
  )
  is
      v_dep_id apm_package_dependencies.dependency_id%TYPE;
  begin
      select dependency_id into v_dep_id from apm_package_dependencies
      where service_uri = remove_interface.interface_uri 
      and interface_version = remove_interface.interface_version;
      remove_interface(v_dep_id);
  end remove_interface;

  -- Add a requirement for this version.  A requirement is some interface that this
  -- version depends on.
  function add_dependency(
    dependency_id		in apm_package_dependencies.dependency_id%TYPE
			        default null,
    version_id			in apm_package_versions.version_id%TYPE,
    dependency_uri		in apm_package_dependencies.service_uri%TYPE,
    dependency_version		in apm_package_dependencies.service_version%TYPE
  ) return apm_package_dependencies.dependency_id%TYPE
  is
    v_dep_id apm_package_dependencies.dependency_id%TYPE;
  begin
      if add_dependency.dependency_id is null then
          select acs_object_id_seq.nextval into v_dep_id from dual;
      else
          v_dep_id := add_dependency.dependency_id;
      end if;
  
      insert into apm_package_dependencies
      (dependency_id, version_id, dependency_type, service_uri, service_version)
      values
      (v_dep_id, add_dependency.version_id, 'requires', add_dependency.dependency_uri,
	add_dependency.dependency_version);
      return v_dep_id;
  end add_dependency;

  procedure remove_dependency(
    dependency_id		in apm_package_dependencies.dependency_id%TYPE
  )
  is
  begin
    delete from apm_package_dependencies 
    where dependency_id = remove_dependency.dependency_id;
  end remove_dependency;


  procedure remove_dependency(
    dependency_uri		in apm_package_dependencies.service_uri%TYPE,
    dependency_version		in apm_package_dependencies.service_version%TYPE,
    version_id			in apm_package_versions.version_id%TYPE
  )
  is
    v_dep_id apm_package_dependencies.dependency_id%TYPE;
  begin
      select dependency_id into v_dep_id from apm_package_dependencies 
      where service_uri = remove_dependency.dependency_uri 
      and service_version = remove_dependency.dependency_version;
      remove_dependency(v_dep_id);
  end remove_dependency;

   function sortable_version_name (
    version_name		in apm_package_versions.version_name%TYPE
  ) return varchar2
    is
	a_start integer;
	a_end   integer;
	a_order varchar2(1000);
	a_char  char(1);
	a_seen_letter char(1) := 'f';
    begin
	a_start := 1;
	loop
	    a_end := a_start;
    
	    -- keep incrementing a_end until we run into a non-number        
	    while substr(version_name, a_end, 1) >= '0' and substr(version_name, a_end, 1) <= '9' loop
		a_end := a_end + 1;
	    end loop;
	    if a_end = a_start then
		raise_application_error(-20000, 'Expected number at position ' || a_start);
	    end if;
	    if a_end - a_start > 4 then
		raise_application_error(-20000, 'Numbers within versions can only be up to 4 digits long');
	    end if;
    
	    -- zero-pad and append the number
	    a_order := a_order || substr('0000', 1, 4 - (a_end - a_start)) ||
		substr(version_name, a_start, a_end - a_start) || '.';
	    if a_end > length(version_name) then
		-- end of string - we're outta here
		if a_seen_letter = 'f' then
		    -- append the "final" suffix if there haven't been any letters
		    -- so far (i.e., not development/alpha/beta)
		    a_order := a_order || '  3F.';
		end if;
		return a_order;
	    end if;
    
	    -- what's the next character? if a period, just skip it
	    a_char := substr(version_name, a_end, 1);
	    if a_char = '.' then
		null;
	    else
		-- if the next character was a letter, append the appropriate characters
		if a_char = 'd' then
		    a_order := a_order || '  0D.';
		elsif a_char = 'a' then
		    a_order := a_order || '  1A.';
		elsif a_char = 'b' then
		    a_order := a_order || '  2B.';
		else
		    -- uhoh... some wacky character. bomb
		    -- raise_application_error(-20000, 'Illegal character ''' || a_char ||
		--	' in version name ' || version_name || '''');
		    a_order := a_order || ' OD.';
		end if;
    
		-- can't have something like 3.3a1b2 - just one letter allowed!
		if a_seen_letter = 't' then
		    raise_application_error(-20000, 'Not allowed to have two letters in version name '''
			|| version_name || '''');
		end if;
		a_seen_letter := 't';
    
		-- end of string - we're done!
		if a_end = length(version_name) then
		    return a_order;
		end if;
	    end if;
	    a_start := a_end + 1;
	end loop;
    end sortable_version_name;

  function compare(
    version_name_one		in apm_package_versions.version_name%TYPE,
    version_name_two		in apm_package_versions.version_name%TYPE
  ) return integer is
	a_order_a varchar2(1000);
	a_order_b varchar2(1000);
    begin
	a_order_a := sortable_version_name(version_name_one);
	a_order_b := sortable_version_name(version_name_two);
	if a_order_a < a_order_b then
	    return -1;
	elsif a_order_a > a_order_b then
	    return 1;
	end if;
	return 0;
    end compare;

  function upgrade_p(
    path			in apm_package_files.path%TYPE,
    initial_version_name	in apm_package_versions.version_name%TYPE,
    final_version_name		in apm_package_versions.version_name%TYPE
   ) return integer
    is
	v_pos1 integer;
	v_pos2 integer;
	v_path apm_package_files.path%TYPE;
	v_version_from apm_package_versions.version_name%TYPE;
	v_version_to apm_package_versions.version_name%TYPE;
    begin
	-- Set v_path to the tail of the path (the file name).
	v_path := substr(v_path, instr(v_path, '/', -1) + 1);

	-- Remove the extension, if it's .sql.
	v_pos1 := instr(v_path, '.', -1);
	if v_pos1 > 0 and substr(v_path, v_pos1) = '.sql' then
	    v_path := substr(v_path, 1, v_pos1 - 1);
	end if;

	-- Figure out the from/to version numbers for the individual file.
	v_pos1 := instr(v_path, '-', -1, 2);
	v_pos2 := instr(v_path, '-', -1);
	if v_pos1 = 0 or v_pos2 = 0 then
	    -- There aren't two hyphens in the file name. Bail.
	    return 0;
	end if;

	v_version_from := substr(v_path, v_pos1 + 1, v_pos2 - v_pos1 - 1);
	v_version_to := substr(v_path, v_pos2 + 1);

	if sortable_version_name(upgrade_p.initial_version_name) <= sortable_version_name(v_version_from) and
	    sortable_version_name(upgrade_p.final_version_name) >= sortable_version_name(v_version_to) then
	    return 1;
	end if;

	return 0;
    exception when others then
	-- Invalid version number.
	return 0;
    end upgrade_p;
    
end apm_package_version;
/
show errors

create or replace package body apm_package_type
as
 function create_type(
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in acs_object_types.pretty_name%TYPE,
    pretty_plural		in acs_object_types.pretty_plural%TYPE,
    package_uri			in apm_package_types.package_uri%TYPE,
    package_type		in apm_package_types.package_type%TYPE,
    singleton_p			in apm_package_types.singleton_p%TYPE,
    spec_file_path		in apm_package_types.spec_file_path%TYPE default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE default null
  ) return apm_package_types.package_type%TYPE
  is
  begin
   insert into apm_package_types
    (package_key, pretty_name, pretty_plural, package_uri, package_type,
    spec_file_path, spec_file_mtime, singleton_p)
   values
    (create_type.package_key, create_type.pretty_name, create_type.pretty_plural,
     create_type.package_uri, create_type.package_type, create_type.spec_file_path, 
     create_type.spec_file_mtime, create_type.singleton_p);
   return create_type.package_key;
  end create_type;

  function update_type(    
    package_key			in apm_package_types.package_key%TYPE,
    pretty_name			in acs_object_types.pretty_name%TYPE
    	    	    	    	default null,
    pretty_plural		in acs_object_types.pretty_plural%TYPE
    	    	    	    	default null,
    package_uri			in apm_package_types.package_uri%TYPE
    	    	    	    	default null,
    package_type		in apm_package_types.package_type%TYPE
    	    	    	        default null,
    singleton_p			in apm_package_types.singleton_p%TYPE
    	    	    	    	default null,
    spec_file_path		in apm_package_types.spec_file_path%TYPE 
    	    	    	    	default null,
    spec_file_mtime		in apm_package_types.spec_file_mtime%TYPE
    	    	    	    	 default null
  ) return apm_package_types.package_type%TYPE
  is
  begin       
      UPDATE apm_package_types SET
      	pretty_name = nvl(update_type.pretty_name, pretty_name),
    	pretty_plural = nvl(update_type.pretty_plural, pretty_plural),
    	package_uri = nvl(update_type.package_uri, package_uri),
    	package_type = nvl(update_type.package_type, package_type),
    	spec_file_path = nvl(update_type.spec_file_path, spec_file_path),
    	spec_file_mtime = nvl(update_type.spec_file_mtime, spec_file_mtime),
    	singleton_p = nvl(update_type.singleton_p, singleton_p)
      where package_key = update_type.package_key;
      return update_type.package_key;
  end update_type;
  
  procedure drop_type (
    package_key		in apm_package_types.package_key%TYPE,
    cascade_p		in char default 'f'
  )
  is
      cursor all_package_ids is
       select package_id
       from apm_packages
       where package_key = drop_type.package_key;
       
      cursor all_parameters is
       select parameter_id from apm_parameters
       where package_key = drop_type.package_key; 
  begin
    if cascade_p = 't' then
        for cur_val in all_package_ids
        loop
            apm_package.delete(
	        package_id => cur_val.package_id
	    );
        end loop;
	-- Unregister all parameters.
        for cur_val in all_parameters 
	loop
	    apm.unregister_parameter(parameter_id => cur_val.parameter_id);
	end loop;

    end if;
    delete from apm_package_types
    where package_key = drop_type.package_key;
  end drop_type;
end apm_package_type;
/
show errors

create or replace package body apm_parameter_value
as
   function new (
    value_id			in apm_parameter_values.value_id%TYPE default null,
    package_id			in apm_packages.package_id%TYPE,
    parameter_id		in apm_parameter_values.parameter_id%TYPE,
    attr_value			in apm_parameter_values.attr_value%TYPE
  ) return apm_parameter_values.value_id%TYPE
  is 
  v_value_id apm_parameter_values.value_id%TYPE;
  begin
   v_value_id := acs_object.new(
     object_id => value_id,
     object_type => 'apm_parameter_value'
   );
   insert into apm_parameter_values 
    (value_id, package_id, parameter_id, attr_value)
     values
    (v_value_id, apm_parameter_value.new.package_id, 
    apm_parameter_value.new.parameter_id, 
    apm_parameter_value.new.attr_value);
   return v_value_id;
  end new;

  procedure delete (
    value_id			in apm_parameter_values.value_id%TYPE default null
  )
  is
  begin
    delete from apm_parameter_values 
    where value_id = apm_parameter_value.delete.value_id;
    acs_object.delete(value_id);
  end delete;

 end apm_parameter_value;
/
show errors;

create or replace package body apm_application
as

  function new (
    application_id	in acs_objects.object_id%TYPE default null,
    instance_name	in apm_packages.instance_name%TYPE
			default null,
    package_key		in apm_package_types.package_key%TYPE,
    object_type		in acs_objects.object_type%TYPE
			   default 'apm_application',
    creation_date	in acs_objects.creation_date%TYPE default sysdate,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null,
    context_id		in acs_objects.context_id%TYPE default null
  ) return acs_objects.object_id%TYPE
  is
    v_application_id	integer;
  begin
    v_application_id := apm_package.new (
      package_id => application_id,
      instance_name => instance_name,
      package_key => package_key,
      object_type => object_type,
      creation_date => creation_date,
      creation_user => creation_user,
      creation_ip => creation_ip,
      context_id => context_id
    );
    return v_application_id;
  end new;

  procedure delete (
    application_id		in acs_objects.object_id%TYPE
  )
  is
  begin
    delete from apm_applications
    where application_id = apm_application.delete.application_id;
    apm_package.delete(
        package_id => application_id);
  end delete;

end;
/
show errors

create or replace package body apm_service
as

  function new (
    service_id		in acs_objects.object_id%TYPE default null,
    instance_name	in apm_packages.instance_name%TYPE
			default null,
    package_key		in apm_package_types.package_key%TYPE,
    object_type		in acs_objects.object_type%TYPE default 'apm_service',
    creation_date	in acs_objects.creation_date%TYPE default sysdate,
    creation_user	in acs_objects.creation_user%TYPE default null,
    creation_ip		in acs_objects.creation_ip%TYPE default null,
    context_id		in acs_objects.context_id%TYPE default null
  ) return acs_objects.object_id%TYPE
  is
    v_service_id	integer;
  begin
    v_service_id := apm_package.new (
      package_id => service_id,
      instance_name => instance_name,
      package_key => package_key,
      object_type => object_type,
      creation_date => creation_date,
      creation_user => creation_user,
      creation_ip => creation_ip,
      context_id => context_id
    );
    return v_service_id;
  end new;

  procedure delete (
    service_id		in acs_objects.object_id%TYPE
  )
  is
  begin
    delete from apm_services
    where service_id = apm_service.delete.service_id;
    apm_package.delete(
	package_id => service_id
    );
  end delete;

end;
/
show errors

-- security stuff.

drop trigger users_update_login_token;

@@ security-drop.sql
@@ security-create.sql
