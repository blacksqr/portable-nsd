<?xml version="1.0"?>
<queryset>
   <rdbms><type>oracle</type><version>8.1.6</version></rdbms>

<fullquery name="ns_cache_initialize">      
<querytext>
create table ns_cache ( cacheid varchar(1000), cache_key varchar(2000), cache_value clob);
</querytext>
</fullquery>
</queryset>
