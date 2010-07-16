use strict;
use XML::Simple;
use Data::Denter;
use lib qw(d:/Projects/ms_oacs/);
use PgParser;
use Carp::Assert;
my $parser = load_parser( metadata => 1, precompiled => 1); #Parse::RecDescent->new($grammar);
$::RD_HINT = 1;
#$fn->name("test_function");
#$fn->return_type("int");
$::RD_TRACE  = 1;
use vars qw ($fn);

sub parse() {
    my ($rule, $body ) = @_;

#    return defined ($parser->$rule($body));
#    print "Testing rule $rule ...\n";
    if ($body =~/(\'\'|\-\-)/){ warn "WARNING comments (--) and double single-quotes ('') will choke the parser\n"; }
    my $retval = $parser->$rule($body);
#    $fn->body($fn->body() . "\n" . $retval);
    print "\nRETURNING :\n$retval\n";     
#croak "It didn't work!" unless defined ($retval);
    return  $retval ;

}

my $xml = join "\n", <DATA>;

#print $xml;
#package MyFilter;
#use XML::Filter::Base;
#@ISA = ('XML::Filter::Base');   
#    use XML::Parser;
#$p1 = new XML::Parser(Style => 'Debug');
#$p1->parse($xml);
#exit;

my $ref = XMLin($xml, keyattr => "name");
use Data::Dumper;


#print Denter $ref;
my $queries = $ref->{fullquery};
for my $name ( keys %{ $queries } ) { 
    my $sql = \$queries->{$name}->{querytext};
#    $$sql = "x";
    print "$name->SQL:", $$sql;
    print &parse("stmt_select",$$sql);
#    last;
}
#print Dumper $ref;

#iterate over queries.  

#attempt to convert.
#

#print XMLout($ref);

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
