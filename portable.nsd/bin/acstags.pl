#!/usr/bin/perl
# acstags.pl - A script for creating emacs TAGS files from an OpenACS source code tree. 
#
# It has some advantages over using a quick command line w/ find, xargs etc.
# Importantly,  it's cross platform and indexes postgresSQL files (functions, tables, indexes).  
#
# I'm working on adding query dispatcher/xql file support as well.
#
# John Sequeira - johnseq@pobox.com
#

system ("rm TAGS");

use Data::Dumper;
use File::Find;
use vars qw/*name *dir *prune @files/;
*name   = *File::Find::name;
*dir    = *File::Find::dir;
*prune  = *File::Find::prune;

# Traverse desired filesystems
#File::Find::find({wanted => \&wanted}, 'c:/projects/portable.nsd/openacs-4.6.3/packages/');
File::Find::find({wanted => \&wanted}, 'c:/web/openacs-4.bak/packages/');

sub wanted {

    return unless (/\.(tcl|sql)/ && ! (/foo/ || /^_/ || /,v$/));

#JUST SQL FILES
#    return unless /\.sql$/;

#SKIP GARBAGE, ORACLE, RC FILES, WEB PAGES
    return if (/foo/ || 
	       /^_/ || 
	       /,v$/ || 
	       /oracle/ || 
	       /upgrade/ || 
	       /drop/ 
	        );
    return if ($File::Find::name =~ /(oracle|upgrade|drop|\/www\/)/ );

    push @files, $File::Find::name;
}

#Index my big-file-o-xql cache
push @files, 'c:/projects/portable.nsd/xql.dat';

#die Dumper(@files); 
#$tagparser=new Tags ('sql',
#	   	     'create +table +(\w+)');
$tagparser=new Tags ('tcl',
  		     {pattern=>q{(?:ad_)?proc(?:_doc)? (?:\s+ (?:-public|-private))? \s+ ((\w|:)+)},
  		      help=>'procs'},

	   	     {pattern=>q{create \s+ (?:table|view|index|function|trigger) \s+ (\w+)},
  		      option=>'pgtable',
  		      help=>'tables'}, 

		     {pattern=>q{(?<=dbqd)((?:[a-z]|[.])+)},
		      option=>'xql',
		      help=>'xql'},
		     );

#$tagparser->parse(files=>['test.sql'], emacs=>'TAGS',add=>['pgtable']);
$tagparser->parse(files=>\@files, emacs=>'TAGS', );
$tagparser->parse(files=>\@files, emacs=>'TAGS',add=>['pgtable']);

exit;
# $tagparser=new Tags ('tcl',
#   		     {pattern=>q{(?:ad_)?proc (?:-public|-private)? (\w+)},
#   		      help=>'procs'},
#    		     {pattern=>q{create +function +(\w+)},
#   		      option=>'pgfn',
#   		      help=>'plpgsql functions'},
# 	   	     {pattern=>q{create +table +(\w+)},
#   		      option=>'pgtable',
#   		      help=>'tables'});


# -*-Perl-*-
package Tags;

require 5.000;
use English;
use Carp;
#use vars qw($VERSION); # for perl version > 5.002
#use strict;

##############################################################################
#	Pod documentation.
##############################################################################

=head1 NAME

Tags.pm - A basic perl class for creating tag files for various languages.

=head1 VERSION
    
$Id: acstags.pl,v 1.5 2004/01/21 08:02:18 john Exp $

=head1 SYNOPSIS

    use Tags;
    $tagparser=new Tags ('perl',
		     {pattern=>q{^\s*sub\s+(?:\w+(?:\'|::))*(\w+)\s+},
		      help=>'subroutines'},
		     {pattern=>q{^[\$\@\%]{1}(\w+).*=},
		      option=>'variables',
		      help=>'global variables'});

    $tagparser->parse(files=>["Tags.pm", "ptags"],vi=>'undef', emacs=>'TAGS');

    $tagparser->parse_command_line(@ARGV);

=head1 DESCRIPTION

This packages defines a class for tag parsers that will be able to
create tag files for various language. A tag parser is defined by a
set of patterns that will match interesting tags in a partiular
language. Some of these patterns can be optionnal.

=over

=item C<new>

    ($language:string, $pat1, $pat2,...)->Tags
Returns a tag parser for a particular language. $language is the name
of the language. $pati can be either a string containing a pattern, or
a reference to an hash. The hash can contain the following values:

=over

=item 'pattern'

The pattern used to find the tag. The name part of the tag must
be matched by the first parenthesis group. ($1).

=item 'help'

A short message indicating what sort of tag is match by this pattern

=item 'option'

If present, the pattern is optional, and will be used only if
the optionname is asked for.

=item 'first'

If present, only the first match of a particular tag will be kept, and
the next one will be fully ignored (no warnings).

=back

All the following are method defined for tag parsers.

=item C<parse>

    (files=>["file1","file2"],
     vi=>'vi_tag_file', emacs=>undef,
     include=>["tagfile1", "tagfile2"],
     update=>undef,
     add=>['variables']
     )->()
Create tag file(s) for the indicated files.

=over

=item 'add'

(ref to an array of strings).
If present tels the parsers to also use the optionnal pattern named
by 'variables'

=item 'vi'

(string)
If present, will create a tag file F<vi_tag_file> for vi. If undefined,
defaults to F<tags>.

=item 'emacs'

(string)
Idem. defaults to F<TAGS>.

=item 'update'

If present, and only for vi-style file it will update the tagfile
instead of recreating it, ie will preserve tags that are not found.

=item 'pat_limit'

(string)
what pattern delimiter will be used for patterns in vi-style tag
files.

=item 'include'

(ref to an array of strings)
if present, and only for emacs-style file, it will include the
tag files F<tagfile1> and F<Tagfile2>

=item 'warn'

if present, some warnings will be issued if for example a tag is
duplicated.

=back    

=item C<parse_command_line>

    (@ARGV)->()
For a simple usage, a Tag parser can handle himself the parsing of the
command line (@ARGV). It will then also handles help, usage and
version messages. For the syntax of the command line, see the help
given by ... --help

=item C<help_patterns>

    ()->string
Returns a pretty print of the tags the tag parser will search for.
=back

=head1 EXPORTS

None

=head1 SEE ALSO

L<ptags> and B<ptags --help>

http://www.eleves.ens.fr:8080/home/nthiery/Tags/

L<perl>
=head1 COPYRIGHT

    Copyright (c) 1996 Nicolas Thiery. All rights reserved. This
    program is free software; you can redistribute it and/or modify it
    under the same terms as Perl itself.

=head1 AUTHOR

Nicolas Thiery

Nicolas.Thiery@ens.fr

http://www.eleves.ens.fr:8080/home/nthiery/

=cut

##############################################################################
#	End of the pod documentation.    
##############################################################################

$VERSION = do { my @r = (q$Revision: 1.5 $ =~ /\d+/g); sprintf "%d." . "%02d" x $#r, @r };
# on one line for MakeMaker

sub new {
    my $type=shift;
    my @pat=();
    my %opt_pat=();
    my $language=shift;
    my $pat;
    while ($pat=shift) {
	if (ref($pat)) {
	    if (defined($pat->{option}))  {
		$opt_pat{$pat->{option}}=$pat;
	    } else {
		push(@pat, $pat);
	    }
	} else {
	    push(@pat, {pattern=>$pat});
	}
    };
    return bless {
	language		=> $language,
	patterns		=> \@pat,
	(%opt_pat ? (opt_patterns => \%opt_pat): ())
	}, $type;
}

sub parse {
    my $self=shift;
    my %args=@_;
    my ($tagfile, $TAGfile,%tags,$pat_limit,@include);
    if (exists($args{vi})) {
	$tagfile= defined($args{vi}) ? $args{vi} : 'tags';
	%tags	= ();
	$pat_limit=$args{pat_limit}||'/';
	# délimiteurs des patterns ecrits pour vi
	exists($args{include}) and do {
	    croak "can't include with vi-style tags";
	};
	exists($args{update}) and do { # gestion of update for vi-style tags
	    open(TAGSVI, $tagfile) or
		croak "cannot open $tagfile for reading";
	    while(<TAGSVI>) {
		/^(\S+)\s+.*$/ and $tags{$1}=$_;
	    };
	    close(TAGSVI);
	};
	open(TAGSVI, ">$tagfile") or
	    croak "cannot open $tagfile for writing: $::ERRNO";
    };
    if (exists($args{emacs})) {
	$TAGfile= defined($args{emacs}) ? $args{emacs} : 'TAGS';
	exists($args{include}) and do {
	    @include=@{$args{include}};
	};
	exists($args{update}) and
	    croak "can't update with emacs-style tags";
	open(TAGS, ">$TAGfile") or
	    croak "cannot open $TAGfile for writing: $::ERRNO";
    }
    # patterns used to search for tags in the files
    my @patterns	= @{$self->{patterns}};
    exists($args{add}) and do {
	my $opt;
	foreach $opt (@{$args{add}}) {
	    exists($self->{opt_patterns}{$opt}) or do {
		croak "unknown additionnal pattern `$opt`";
	    };
	    push(@patterns,$self->{opt_patterns}{$opt});
	};
    };

# recherche de tags dans les fichiers indiques
    my $file;
    my %NEW={};
    for $file (@{$args{files}}) {
	open(FILE, "$file") or croak "unable to open $file: $!";
	my $line=0;			# ligne courante
	my $char=0;			# caractere courant
	my $TAGS='';		# table de tous les tags dans ce fichier
	my $TAGsize=0; # taille de la description de tous les tags dans ce fichier
	my ($name, $pat);	
	while (<FILE>) {
	    foreach $pat (@patterns) {
		/$pat->{pattern}/x and do {
		    $name=$1;
#		    $TAGS.="$MATCH$line,$char\n" if defined($TAGfile);
		    $TAGS.="$&$line,$char\n" if defined($TAGfile);
#		    $TAGS.="$1$line,$char\n" if defined($TAGfile);
		    if (defined($tagfile)) {
			if (defined($NEW{$name})) {
			    exists($pat->{first}) and next;
			    carp "ignoring duplicate definition for $name, ".
				"$file line $line\n"
				    if $args{"warn"};
			}
			else {
			    print(TAGSVI "$name\t$file\t$pat_limit^",
				  quotemeta($::MATCH),"$pat_limit\n");
			}
			delete($tags{$name}) if defined(%tags);
		    };
		    $NEW{$name}=0;
		};
	    };
	    $line++;
	    $char+=length($_);
	}
	close(FILE);

	if (defined($TAGfile)) {
	    print TAGS "\f\n";
	    print TAGS "$file,",length($TAGS),"\n",$TAGS;
	}
    }
# fin d'écriture et fermeture des fichiers de tags.
    if (defined($TAGfile)) {
	if (@include) {
	    for (@include) {
		print TAGS "\n$_,include\n";
	    }
	};
	close(TAGS);
    }
    if (defined($tagfile)){
	my $tag;
	for $tag (sort(keys(%tags))) {
	    print TAGSVI $tags{$tag};
	}
	close(TAGSVI);
    }
}

sub help_patterns {
    my $self=shift;
    my $result="Searched tags:\n";
    
    foreach (@{$self->{patterns}}) {
	$result.= "- $_->{help}\t(pattern:/$_->{pattern}/)\n";
    };
    exists($self->{opt_patterns}) and do {
	$result.="Optional tags that can be searched with --add:\n";
	foreach (values(%{$self->{opt_patterns}})) {
	    $result.=
		"- $_->{help}\t(pattern: /$_->{pattern}/, option: $_->{option})\n";
	}
    };
    return $result;
}

sub help {
    my $self=shift;
    print STDERR <<EOM;
Usage: $::PROGRAM_NAME [-h/-?/--help] [-V,--version]
    [-v/--vi/-e/--emacs] [-o/-O outfile] [-F-B]
    [-a/--add opt1,opt2]
    [input1] [input2] ...
	creates tag files for vi and or emacs from $self->{language} files
    -h, -?, --help: prints this help messages and quit
    -V,--version: prints $::PROGRAM_NAME version
    -w, --warn: issues some warnings if necessary
    -v,--vi,--noemacs: creates a tag file only for vi
    -e,--emacs,-novi: creates a tag file only for emacs
        default: both are created
    -o: name of the tag file for vi
    -O: name of the tag file for emacs
        default: "tags" pour vi et "TAGS" pour emacs
    -F: regular expressions for vi tag files are created with '/'
	(forward search).
    -B: regular expressions for vi tag files are created with '?'
	(backward search).
    -i file, --include file: add a note in the emacs tag file for including
	other tag files.
	do not work with vi tag files.
    -u, --update: updates the tag file for vi. (leave unchanged tags in
	non visited files).
	do not work with emacs tag files
    -a, --add: add the optional tags opt1 and opt2 to the search.
EOM
    print STDERR $self->help_patterns;
}

sub usage {
    $_[0] && print STDERR "$::PROGRAM_NAME: $_[0]\n";
    print STDERR <<EOM;
for more information try:
$::PROGRAM_NAME --help
EOM
}

sub version {
    print STDERR <<EOM
\$Id: acstags.pl,v 1.5 2004/01/21 08:02:18 john Exp $
Version $VERSION
EOM
}

sub parse_command_line {
    my $self=shift;
    my %args;
    my @include=();
    my @files=();
    my @add=();
    $args{emacs}=undef;
    $args{vi}=undef;
    while ($_=shift()) {
	/^-(h|\?|-help)$/i	&& do { $self->help;		exit 0; };
	/^-(o|-output)$/	&& do { $args{vi}= shift;	next; };
	/^-(O|-Output)$/	&& do { $args{emacs}= shift;	next; };
	/^-F$/			&& do { $args{pat_limit}='/';	next; };
	/^-B$/			&& do { $args{pat_limit}='?';	next; };
	/^-(V|-version)$/	&& do { version;		exit 0; };
	/^-(v|-vi|-noemacs)$/	&& do { delete($args{emacs});	next; };
	/^-(e|-emacs|-novi)$/	&& do { delete($args{vi});	next; };
	/^-(u|-update|m|-merge)$/ && do { $args{update}=1;	next; };
	/^-(i|-include)$/	&& do { push(@include, shift()); next };
	/^-(w|--warn)$/		&& do { $args{"warn"}=1;	next; };
	/^-(a|-add)$/		&& do {
	    push(@add, split(',', shift()));
	    next;
	};
	/^-/ and do { usage "unknown option: $_"; exit -1; };
	push (@files, $_);
    };
    @files or do { usage "no input files"; exit -1; };
    $args{files}=\@files;
    $args{add}=\@add if @add;
    $args{include}=\@include if @include;
    eval $self->parse(%args);
    $::EVAL_ERROR and do { usage $::EVAL_ERROR; exit -1; }
};

1;

#eval join('',<main::DATA>) || die $@ unless caller(); 

__END__;
# TODO: some tests ...


