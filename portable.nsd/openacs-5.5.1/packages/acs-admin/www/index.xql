<?xml version="1.0"?>
<queryset>

  <fullquery name="count_subsites">
    <querytext>
      select count(*)
      from apm_packages
      where package_key in ($package_keys)
    </querytext>
  </fullquery>

  <fullquery name="installed_packages">
    <querytext>
      select package_key,
             pretty_name as pretty_name
      from   apm_package_types
      order  by upper(pretty_name), pretty_name
    </querytext>
  </fullquery>

</queryset>
