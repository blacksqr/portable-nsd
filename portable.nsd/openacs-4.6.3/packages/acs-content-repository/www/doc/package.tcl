request create -params {
  package_name -datatype keyword
}

# declare the datasource for the introductory stuff.

doc::package_info $package_name info

# declare the datasource for the methods

doc::func_multirow $package_name methods
