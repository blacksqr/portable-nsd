<?xml version="1.0"?>

<queryset>
   <rdbms><type>postgresql</type><version>7.1</version></rdbms>

<fullquery name="apm_higher_version_installed_p.apm_higher_version_installed_p">      
      <querytext>

		select apm_package_version__version_name_greater(:version_name, highest.version_name)
                from (select version_name
		      from apm_package_versions
                      where version_id = apm_package__highest_version(:package_key)
                     ) highest
    
      </querytext>
</fullquery>

 
</queryset>
