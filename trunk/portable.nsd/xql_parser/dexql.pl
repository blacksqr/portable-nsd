=pod




John  Sequeira
johnseq@pobox.com

=cut
use strict;
use warnings;

use Find::File::Rule;
use IO::All;

use constant ACSDIR => '';
use XML::Simple;
use Data::Denter;
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

# slurp up all the files create a big hash of XQL->




# Walk the tcl files, run regex against them 


