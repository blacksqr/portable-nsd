#!c:/perl5.8/bin/perl
#!/usr/bin/perl 

=pod
=head


DEPRECATED... use /bin/xql-replace.pl

xparse.pl - parses OpenACS XQL files and inlines the SQL found into the corresponding TCL files so that we do not need any Query Dispatcher support.
 
NOTE:You will see lots of hardcoded constants in this file specific to my system.

=head USAGE

./xparse.pl

=head author


 John Sequeira
 johnseq@pobox.com


=cut

# 	$Id: xparse.pl,v 1.4 2003/04/15 18:40:09 john Exp $	

use strict;
use warnings;
use XML::Simple;
#use Data::Denter;
use Data::Dumper;
use Text::Balanced qw (
                            extract_delimited
                            extract_bracketed
                            extract_quotelike
                            extract_codeblock
                            extract_variable
                            extract_tagged
                            extract_multiple

                            gen_delimited_pat
                            gen_extract_tagged
                           );

use Carp::Assert;
use File::Slurp;
use constant DEBUG => 1;
use File::Find;
use Log::Dispatch;
use Log::Dispatch::File;

our $log = Log::Dispatch->new;

$log->add( Log::Dispatch::File->new( name => 'file1',
				     min_level => 'debug',
				     filename => 'xparse.log' ) );

use constant LIBPATH => 'c:/temp/openacs5/';
#use constant DEBUG_FILE => 'one.';


# The base path to begin parsing
#use constant LIBPATH => 'c:/temp/openacs-4/packages/acs-content-repository/';

# set this if you just want to do one file then quit ??
#use constant DEBUG_FILE => 'index';

# To process all files ...
use constant DEBUG_FILE => '';


main();

sub main{
    our %files_generic;
    our %files_native;

#Callback to handle files we want to parse
    sub wanted {
	my $tcl = $File::Find::name;

	return unless $tcl =~ /(tcl|vuh)$/;
	return if -d $tcl;

	(my $xql = $tcl) =~ s/\.(tcl|vuh)$/-postgresql.xql/;
	if ( -e $xql ) {
	    $files_native{$tcl} = $xql;
	}

	$xql =~ s/-postgresql\.xql$/.xql/;
	if ( -e $xql ) {
	    $files_generic{$tcl} = $xql;
	}
    }


#For each tcl file,  look for postgres xql file... then look for xql file
#if a matching xql file exists, add it to hash


    find (\&wanted, LIBPATH);
    $log->info( ( Dumper \%files_generic, \%files_native ));
    

    my $debug_file = DEBUG_FILE;
    while (my ($key, $value) = each %files_generic) {
#    next unless $key =~ /\Qindex.tcl/ && $key =~ /\Qservice-contract/;
#	next unless $key =~ /\Qapplication-group-procs/;

	if ($debug_file) {
	    next unless $key =~ /\Q$debug_file/;
	}
	qd_filter ($key, $value);
    }

    while (my ($key, $value) = each %files_native) {
#	next unless $key =~ /\Qapplication-group-procs/;
#	next unless $key =~ /\Qacs-messaging-procs/;

	if ($debug_file) {
	    next unless $key =~ /\Q$debug_file/;
	}

	qd_filter ($key, $value);
    }

}





sub qd_filter 
{ 
  our ($tcl_file, $xml_file) = @_;

    $log->debug( sprintf "FILES: TCL(%s) XML(%s)", $tcl_file, $xml_file . "\n" );
    my $file = read_file($tcl_file);

    my $xml = read_file($xml_file);

    #make bad xml good.
    my $cdata_open= '<querytext><![CDATA[';
    my $cdata_close = ']]></querytext>';

    $xml =~ s/<querytext>/$cdata_open/sg;
    $xml =~ s/<\/querytext>/$cdata_close/sg;


    our $ref;
    eval {$ref = XMLin($xml, keyattr => "name") };
    if ($@) {
	$log->error( "Parse Error: $@\n");
	return;
    }


    #handle the case where there's only one query in the file... XML::Simple auto-munges it in a different way

    our $queries; 
    if ( defined $ref->{fullquery}->{name}  ) {
	$queries->{ $ref->{fullquery}->{name} }->{querytext}  =  $ref->{fullquery}->{querytext};
#	print "GOGO";
    } else {               
	$queries = $ref->{fullquery};
    }

#    print Denter $queries;

# for each queryname, find a db command followed by the queryname,  and replace the contents of the 
# brackets or quotes with the real query
# print Denter keys %{ $queries }, "\n\n";
# grep /create_revision/,
    our $newtext = $file;
    my @start  = ();
    for my $name ( keys %{ $queries } ) { 

	my $sql = \$queries->{$name}->{querytext};	
	#$name =~ s/^[^\.]+\.//;
	#if this is a path,  just use the last bit
	my @path  = split /\./, $name;
	$name = pop @path if scalar @path;


	our $text = $newtext;
#	print "Name is $name\n";
#TODO:  handle leading line continuation \ character

	while ( $text =~ s/.*(db_\w+) $name\b//sg ) {	    
	    my $match = $1;
	    next if $match eq 'db_multirow';
	    process_inline_query($match, $name, $sql);	    
	}

	$text = $newtext;
	while ( $text =~ s/.*db_multirow \s+ \w+ \s+ $name\b//sgx ) {	    	    
	    process_inline_query("db_multirow", $name, $sql);	    
	}

	sub process_inline_query { 
	    my ($match, $name, $sql) = (@_);
	    $text =~ s/^\s*$name//s; # handle the case where $name is duplicated...

	      next if $match eq 'db_map';


	    my $extracted; 
	    my $remainder;

	    if ( $text =~ m/^\s*\{/ ) {

	      ($extracted, $remainder) = extract_bracketed($text, '{}',);
	      $log->debug( "b($name):"); #, substr(0, 30, $text), "\n";

	    } elsif ($text =~ m/^\s*\"/) {

	      ($extracted, $remainder) = extract_quotelike($text);   
	      $log->debug( "q($name):");

	    } elsif ( $text =~ m/^\s*\$/ ) {
	      $log->debug( "v($name):" );
	      ($extracted, $remainder) = extract_variable($text);	    
	      #Bug  - need to handle empty quotes
	      #            } elsif ($text =~ m/^\s*""/) {
	      #		$extracted = '""';
	      #		$remainder = $text; $remainder =~ s/^(\s*)""/$1/s;
	    } else {

	      $log->debug(substr ($text, 0, 30) );
	      $log->warning (sprintf 'match found but not categorized for "%s" (in %s) ', $name, $tcl_file);
	    }
#    print "text", substr ($text, 0,30);
	$log->debug( "[$match]\n $extracted \n ------------ $$sql \n");
#  } else {
#    print "$name --> NO MATCH \n -------------- \n";
	#append the stuff I've chewed threw to newstring. then append $$sql.
	#when I'm done... grab end of string.
#	$extracted =~ s/\$/\\\$/g;  #deference dollar signs
#TODO: Add backreference to make sure it's replacing only db_xxx queryname entries
	    if ($extracted) {
	      $newtext =~ s/\Q$extracted\E/{\n$$sql\n}/sg;  # \Q .. \E makes for literal find/replace
	    }


	  }



      }

    write_file("$tcl_file", $newtext) unless $newtext eq $file;
    write_file("$tcl_file.bak", $file) unless $newtext eq $file;

}



__END__
	my $file_ = <<'EOT';
    
    # content item hasen't been created yet - create one.        
        set item_id [db_exec_plsql create_item $create_item]
        db_dml set_item_id "update apm_package_versions 
                               set item_id = :item_id 
                             where version_id = :version_id"
        set revision_id [db_exec_plsql create_revision $create_revision]

        set revision_id [db_exec create_revision {
Stinky foob ar } ]


EOT


__END__
<?xml version="1.0"?>
<queryset>

<fullquery name="ad_user_class_query.sql_post_select_for_user_class"> 
      <querytext>
      
	    select sql_post_select
	    from user_classes 
            where user_class_id = [ns_dbquotevalue $user_class_id]
	
      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.category_id">      
      <querytext>
        select category from categories where category_id = :category_id

      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.country_code">      
      <querytext>

        select country_name from country_codes where iso = :country_code

      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.usps_abbrev">      
      <querytext>

        select state_name from states where usps_abbrev = :usps_abbrev

      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.group_id">      
      <querytext>

        select group_name from groups where group_id = :group_id

      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.registration_during_month">      
      <querytext>

        select to_char(to_date(:registration_during_month,'YYYYMM'),'fmMonth YYYY') from dual

      </querytext>
</fullquery>

<fullquery name="ad_user_class_description.user_class_id">      
      <querytext>

        select name from user_classes where user_class_id = :user_class_id

      </querytext>
</fullquery>
 
</queryset>
