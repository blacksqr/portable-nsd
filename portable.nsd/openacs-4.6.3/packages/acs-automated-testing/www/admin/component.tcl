ad_page_contract {
  @cvs-id $Id: component.tcl,v 1.2 2002/09/10 22:22:04 jeffd Exp $
} {
  component_id:nohtml
  package_key:nohtml
} -properties {
  title:onevalue
  context_bar:onevalue
  component_desc:onevalue
  component_file:onevalue
  component_body:onevalue
}

set title "Component $component_id ($package_key)"
set context_bar [list $title]

set component_bodys {}
foreach component [nsv_get aa_test components] {
  if {$component_id == [lindex $component 0] &&
      $package_key == [lindex $component 1]} {
    set component_desc     [lindex $component 2]
    set component_file     [lindex $component 3]
    set component_body    [lindex $component 4]
  }
}

ad_return_template