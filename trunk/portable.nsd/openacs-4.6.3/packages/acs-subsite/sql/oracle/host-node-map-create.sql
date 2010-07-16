-- @author Mark Dettinger (mdettinger@arsdigita.com)
-- $Id: host-node-map-create.sql,v 1.1.4.1 2003/01/06 20:18:31 donb Exp $

create table host_node_map (
   host                 varchar(200) 
	constraint host_node_map_host_pk primary key 
	constraint host_node_map_host_nn not null,
   node_id              integer 
	constraint host_node_map_node_id_fk references site_nodes
);
