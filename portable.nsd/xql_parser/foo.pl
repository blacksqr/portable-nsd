


use strict;


my $m  = "d * e";
my $mm =  join('\\', split(//, $m));
my $text = " a b c d * e ";
my @x = split $text;
#print $mm;
$text =~ s/\Q$m\E/ss/;
print $text;
exit;

substr $text, 1, 2, '____';
print $text;

__END__
$text = substr $text, $match->{start}, $match->{length}, $match->{sql};
	}

my $VAR1 = [ '20020116', '20020117', '20020118', '20020119', '20020120', '20020121', '20020122', '20020123', '20020124', '20020125', '20020126' ]; 


use Data::Denter;
use Date::Simple;
#for (@$VAR1) {
#    s/(\d{4}?) (\d{2}?) (\d{2}?)//x;
#    print "$1 \t$2 \t $3\n ";
#}

print Denter [ map { my $x = $_; $x =~ s/(\d{4}?) (\d{2}?) (\d{2}?)/$1-$2-$3/x; Date::Simple->new($x) }	       @$VAR1 ];
#print Denter \@dates;
 
