<?xml version="1.0"?>

<queryset>
   <rdbms><type>postgresql</type><version>7.1</version></rdbms>

<fullquery name="lang_system_time_select">      
      <querytext>
      SELECT to_char(current_timestamp, 'YYYY-MM-DD HH24:MI:SS') AS system_time
      </querytext>
</fullquery>

 
</queryset>
