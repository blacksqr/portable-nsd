<?xml version="1.0"?>
<queryset>
   <rdbms><type>postgresql</type><version>7.1</version></rdbms>

<fullquery name="ns_cache_initialize">      
      <querytext>
create table ns_cache ( cacheid text, cache_key varchar(1000), cache_value text)
</querytext>
</fullquery>
</queryset>


