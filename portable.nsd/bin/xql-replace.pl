#!c:/perl5.8.2/bin/perl
# -*- CPerl -*-
# xql-replace.pl

=pod
=head1 Description

Merge openacs tcl and xql files from a source tree.  This script will write the new set of tcl files into a separate directory tree.  To be able to run openacs from this tree,  you will need to copy all non-xql files from the source directory into the target.

=head1 Usage

Change the $root and $target variables to point to the source and target directories

=head1 Author

 John Sequeira http://www.jsequeira.com
 johnseq@pobox.com
 2004


=cut

use strict;
use warnings;
use Text::Balanced qw( extract_multiple
		       extract_quotelike
		       extract_codeblock
		       extract_bracketed);
use Data::Dumper;
use IO::All;
use XML::Simple;
use File::Spec;
use File::Path;
use File::Find::Rule;
use File::Basename;
use Carp::Assert;

our $root = 'c:/temp/openacs5/packages with xql';  #process files in $root
our $target = 'c:/temp/openacs5/packages';   # write files to $target ...

our %skip_proc = map { $_=> 1 } 
  qw( db_bootstrap_set_db_type
      db_qd_log
      db_qd_internal_parse_init
      db_known_database_types
      db_qd_get_fullname
      whic_table_exist);

our %good_proc =map { $_=> 1 }  qw(
          db_blob_get_file
          db_map
          db_list
          db_list_of_lists
          db_list_of_list
          db_dml
          db_multirow
          db_foreach
          db_0or1row

          db_write_blob
          db_blob_get
          db_string
          db_1row
          db_exec_plsql
        );

our %procnames;



#print strip_xql('c:/temp/openacs5/packages with xql/acs-tcl/tcl/request-processor-init.tcl');
#print strip_xql('c:/temp/openacs5/packages/acs-lang/tcl/lang-message-procs.tcl');
#main ('c:/temp/openacs5/packages with xql/acs-tcl/tcl/site-nodes-procs.tcl'); 
#die strip_xql($root . '/acs-tcl/tcl/apm-procs.tcl');
#print strip_xql($root . '/acs-service-contract/tcl/acs-service-contract-init.tcl'); exit;
#print strip_xql($root . '/acs-tcl/tcl/community-core-procs.tcl'); exit;



main(File::Find::Rule->file()
    ->name( '*.tcl', '*.vuh' )
      ->in( $root )) if (1);


#print Dumper \%procnames;


sub main
  {
  # find all the .tcl files
  my @files = @_;

  for my $file (@files) 
    {
      print STDERR  "processing $file\n";
      my $out = strip_xql( $file );

      my $outfile = outfile( $file );

      #maybe create path to output
      mkpath File::Basename::dirname($outfile);

      $out > io( $outfile );

    }
}


=item outfile

  determine output path to processed files

=cut

sub outfile
  {
    my $file = shift;
    $file =~ s/\Q$root\E//;
    return File::Spec->catfile($target, $file);
  }

=item xml_files

  determine output path to processed files

=cut


sub xml_files
{

  my $file = shift;

  my @retval = ();  my $xml;

  ($xml = $file) =~ s/\.\w{3}$/.xql/;
  
  push @retval, $xml if -e $xml;
  ($xml = $file) =~ s/\.\w{3}$/-postgresql.xql/;
  
  push @retval, $xml if -e $xml;

  return @retval;

}

=item queries

@_ => list of xml files to parse,

return name => querytext hash

=cut

sub queries
  {
    my %retval;

    for (@_)
      {

      print STDERR "reading $_\n";
      my $ref;
      my $xml = io( $_ )->slurp;
      $xml =~ s/<\/?querytext>//gs;

	eval { $ref = XMLin($xml,
			    KeyAttr => {fullquery => 'name', partialquery => 'name'}, 
			    ForceArray => [ 'fullquery' ],
			    ContentKey => '-content') };

      for (qw(fullquery partialquery))
	{
	  next unless ref $ref->{$_} eq 'HASH';
	  %retval = (%retval,   #anything previously parsed
		     %{ $ref->{$_} }  # name => query		     
		    );
	}
    }


    while (my ($key, $value) = each %retval) 
      {
	my $key2;
	($key2 = $key) =~ s/(\w+::)+//;
	$key =~ s/(\w+\.)+//g;
	$value =~ s/^\s+//gs;
	$value =~ s/\s+$//gs;
	$retval{$key} = $value;
	$retval{$key2} = $value;	
    }

#    die Dumper \%retval;
    return %retval;
}

sub process_query_names 
  {
    my %queries = @_;
    #replace with brackets if there aren't any dollar signs or brackets in the 
    while (my ($key, $value) = each %queries) 
      {
	if ($value =~ /(\$|\[)/) 
	  {
	    $queries{$key} = qq([subst { $value }]);
	  }
	else
	  {
	    $queries{$key} = qq("$value");
	  }
      }

    return %queries;
}


# Given a query name and a db_ API call,  resolve the query and return it inline along with
#
#  $name => query identifier
#  $proc => name of db_ proc
#
# If it cant' resolve the query it will carp and do nothing
#

sub xql_substitution
  {
    my ($qry, $proc, $dsn, $name);

    #polymorphism: if there's no $dsn, the third arg will be the name...
    if ($_[3] ne "")
      {
	($qry, $proc, $dsn, $name) = (@_);
      }
    else
      {
	($qry, $proc, $name) = (@_);
      }

    assert(defined $name);
    assert($proc);

    no warnings;
    #Not sure whether whitelist or blacklist works better in general case
    return if $proc && exists $skip_proc{$proc};
    return unless $good_proc{$proc};
    use warnings;

	my %queries = %{ $qry };
	my $retval;

	my @values = ( $queries{$name} ) if exists $queries{$name};

    unless (scalar @values) 
      {
	push @values,
	  map { $queries{$_} }
	    # find keys matching name and append them
	    grep { /\Q$name\E$/ }
	      keys(%queries)  ;
      }
    

# 	print Dumper "MULTIHIT!", "\nARGS" , \@_, 
# 	  "\nQUERIES\n",
# 	  \@values,
# 	    "\nKEYS\n",
# 	    [ grep { /\Q$name\E$/ }
# 		  keys %queries ],
# 	    "\nQUERIES\n",
# 	    \%queries,
    # 	  if  scalar @values  > 1;

    #sanity check... make sure they all resolved the same
    if (scalar @values > 1 )
      {
	(my $query = $values[0]) =~ s/\s//gs;
	for (@values)
	  {
	    s/\s//gs;
	    print  STDERR "Ambiguous query retrieval", Dumper \@values, \@_ 	    if ($_ ne $query);

	  } # for
      } # if
    $retval = $values[0];

    if ($retval)
      {
	#strip SQL linefeeds
	    $retval =~ s/\n/ /gs; # TODO: this catches some bugs, but makes the sql look ugly.
	    $procnames{$proc}++;   #logging for metrics

	    return "$proc $dsn $name $retval <EOSQL>";

	  }

    print STDERR "couldn't resolve [$proc]$name\n";

    return;
  }


sub strip_xql
  {
    my $file = shift;

    # make two passes through the file.  in the first, insert the new sql
    # in the second,  remove the obsolete SQL


    my %queries = process_query_names ( queries( xml_files($file) ) );

    # use curry/closure... prob. overkill but cleaner than globals
    # This will either return a lookup, or the no-op of "$proc<space>$queryname"
    my $xql_substitution = sub { my $retval= xql_substitution( \%queries, @_); };


   #warn Dumper [keys %queries];
   #warn Dumper \%queries;


    # read in the file,  replace all db_* <queryname> invocations with
    # db_* <queryname> <query from xql> <EOSQL> <original query>
    # we'll get rid of the <EOSQL> <original query> in a second pass using Text::Balanced
    my $tcl = io( $file )->slurp ;
    #test();

    #can handle -dsn... either replace it with a xql query or do nothing
    $tcl =~ s/(db_\w+)( ?-dsn\s+\S+)? +(\w+)/$xql_substitution->($1,$3,$4) or $&/eg;
    #die Dumper [  $tcl =~ m/(db_\w+)( ?-dsn\s+\S+)? +(\w+)/sg ];

    #(string|foreach|0or1row|1row|list|list_of_lists|multirow)

    my @code =     split "<EOSQL>", $tcl;

    #You can't use a regex to match nested quotes,  so we'll re-arrange our
    # code into a list of fragments,  each beginning with the older queries,
    # and we'll get extract them w/Text::Balanced
    my $out = shift @code;   #pull off first element --> nothing to remove here.
    $out .=
      join "",
	map { remove_query( $_ ) }
	  @code;
    return $out;

}

sub remove_query
  {

    my $code = shift;

    my $text;
    $text = extract_quotelike($code);
    #only try for bracketed if quotelike fails
    $text ||= extract_bracketed($code,'{}[]');

    return $code;

}


=item text

Test the substitution sub

=cut

sub test
  {
    my $queries =    
      {       'get_url_from_object_id.select_url_from_object_id' => '"select site_node__url(node_id)
            from site_nodes
            where object_id = :object_id
            order by site_node__url(node_id) desc"',
          'site_node::update_cache.select_site_node' => '"

	    select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join 
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
            where n.node_id = :node_id
        
    "',
          'site_node::mount.mount_object' => '"
        
            update site_nodes
            set object_id = :object_id
            where node_id = :node_id
        
    "',
          'site_node::select_child_site_nodes' => '"select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
	    where n.tree_sortkey between site_node_get_tree_sortkey(:node_id)
	                         and tree_right(site_node_get_tree_sortkey(:node_id))
	    order by n.tree_sortkey"',
          'site_node::get_root_node_id' => '"select node_id
            from site_nodes
            where parent_id is null"',
          'update_cache.select_site_node' => '"select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join 
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
            where n.node_id = :node_id"',
          'rename_node' => '"update site_nodes
            set    name = :name
            where  node_id = :node_id"',
          'site_node::init_cache.get_root_node_id' => '"
        
            select node_id
            from site_nodes
            where parent_id is null
        
    "',
          'site_node::unmount.unmount_object' => '"
        
            update site_nodes
            set object_id = null
            where node_id = :node_id
        
    "',
          'update_cache.select_child_site_nodes' => '"select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
	    where n.tree_sortkey between site_node_get_tree_sortkey(:node_id)
	                         and tree_right(site_node_get_tree_sortkey(:node_id))
	    order by n.tree_sortkey"',
          'site_node::delete_site_node' => '"select site_node__delete(:node_id);"',
          'site_node::unmount_object' => '"update site_nodes
            set object_id = null
            where node_id = :node_id"',
          'init_cache.get_root_node_id' => '"select node_id
            from site_nodes
            where parent_id is null"',
          'site_node::select_site_node' => '"select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join 
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
            where n.node_id = :node_id"',
          'site_node::update_cache.select_child_site_nodes' => '"
        
	    select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
	    where n.tree_sortkey between site_node_get_tree_sortkey(:node_id)
	                         and tree_right(site_node_get_tree_sortkey(:node_id))
	    order by n.tree_sortkey
        
    "',
          'mount_object' => '"update site_nodes
            set object_id = :object_id
            where node_id = :node_id"',
          'site_node::mount_object' => '"update site_nodes
            set object_id = :object_id
            where node_id = :node_id"',
          'unmount.unmount_object' => '"update site_nodes
            set object_id = null
            where node_id = :node_id"',
          'select_site_node' => '"select n.node_id,
		   n.parent_id,
		   n.name,
		   n.directory_p,
		   n.pattern_p,
		   n.object_id,
		   p.package_key,
		   p.package_id,
		   p.instance_name,
		   t.package_type
            from site_nodes n left join 
                 apm_packages p on n.object_id = p.package_id left join
                 apm_package_types t using (package_key)
            where n.node_id = :node_id"',
          'site_node::select_url_from_object_id' => '"select site_node__url(node_id)
            from site_nodes
            where object_id = :object_id
            order by site_node__url(node_id) desc"',
          'unmount_object' => '"update site_nodes
            set object_id = null
            where node_id = :node_id"',
          'site_node::rename.rename_node' => '"
        
            update site_nodes
            set    name = :name
            where  node_id = :node_id
        
    "',
          'mount.mount_object' => '"update site_nodes
            set object_id = :object_id
            where node_id = :node_id"',
          'site_node::rename_node' => '"update site_nodes
            set    name = :name
            where  node_id = :node_id"',
          'delete_site_node' => '"select site_node__delete(:node_id);"',
          'get_root_node_id' => '"select node_id
            from site_nodes
            where parent_id is null"',
          'site_node::delete.delete_site_node' => '"
        
            select site_node__delete(:node_id);
        
    "',
          'delete.delete_site_node' => '"select site_node__delete(:node_id);"',
          'rename.rename_node' => '"update site_nodes
            set    name = :name
            where  node_id = :node_id"',
          'site_node::get_url_from_object_id.select_url_from_object_id' => '"
        
            select site_node__url(node_id)
            from site_nodes
            where object_id = :object_id
            order by site_node__url(node_id) desc
        
    "'
        };

    print xql_substitution($queries, "db_foreach", "select_child_site_nodes");

}

=pod




I should check to proc to make sure its not one I shouldnt be handling
$VAR1 = {
          'db_api' => 9,
          'db_blob_get_file' => 4,
          'db_release_unused_handles' => 1,
          'db_list_of_lists' => 33,
          'db_state' => 2,
          'db_match_p' => 1,
          'db_transaction_test' => 10,
          'db_0or1row' => 87,
          'db_1row' => 142,
          'db_type_info' => 1,
          'db_exec_lob' => 5,
          'db_multirow__next_row' => 3,
          'db_resultrows' => 2,
          'db_array' => 3,
          'db_list_of_list' => 1,
          'db_sql' => 1,
          'db_helper_checks' => 1,
          'db_foreach' => 102,
          'db_tcl_var' => 1,
          'db_installer_checks' => 1,
          'db_bootstrap_set_db_type' => 1,
          'db_bind_var' => 1,
          'db_exec_plsql' => 155,
          'db_columns' => 1,
          'db_map' => 36,
          'db_known_database_types' => 1,
          'db_pool_to_dbn' => 1,
          'db_multirow' => 69,
          'db_load_sql_data' => 2,
          'db_pool_to_dbn_init' => 1,
          'db_table_exists' => 9,
          'db_type' => 6,
          'db_bootstrap_checks' => 1,
          'db_blob_get' => 3,
          'db_nextval' => 16,
          'db_transactions' => 5,
          'db_type_key' => 1,
          'db_exec' => 30,
          'db_list' => 40,
          'db_qd_internal_parse_init' => 1,
          'db_with_handle' => 7,
          'db_type_list' => 1,
          'db_dml' => 196,
          'db_available_pools' => 3,
          'db_messages' => 2,
          'db_write_blob' => 1,
          'db_string' => 266,
          'db_transaction' => 2,
          'db_transaction_test_pk' => 1,
          'db_qd_get_fullname' => 1,
          'db_html_select_value_options' => 1,
          'db_qd_log' => 38,
          'db_driverkey' => 2
        };

=cut


__END__
my $tcl = <<HERE;

db_string heythere "sdsdsds

dsdsd
s
dsd
sds"

db_foreach sdsdsdsd sqlstatemetn {}

db_dml dsssds [


sds

] #db_dml

    return [db_string acs_mail_link_get_body_id "      

      
select body_id from acs_mail_links where mail_link_id = :link_id
    

"
] 


HERE

$queries{"heythere"} =  1121;
$queries{"sdsdsdsd"} =  2222;
$queries{"dsssds"}   = 9999;
$queries{'acs_mail_link_get_body_id'}= "xyz";   
