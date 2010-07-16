<?xml version="1.0"?>

<queryset>
   <rdbms><type>postgresql</type><version>7.1</version></rdbms>

<fullquery name="apm_enabled_version_info">      
      <querytext>
    select version_id as installed_version_id, version_name as installed_version_name,
           enabled_p as installed_enabled_p,
           apm_package_version__version_name_greater(version_name, :version_name) as version_name_greater
    from   apm_package_versions
    where  package_key = :package_key
    and    installed_p = 't'
    limi