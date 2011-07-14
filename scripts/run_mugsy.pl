#!/usr/bin/env perl
use strict;
use Getopt::Long qw(:config no_ignore_case no_auto_abbrev);
use File::Basename;
use Pod::Usage;
use POSIX;

my %options;
my $results = GetOptions (\%options, 
    'prefix|p=s',
    'directory=s',
    'distance|d=s',
    'minlength|c=s',
    'fullsearch',
    'tree|t=s',
    'treefile|f=s',
    'skipsearch',
    'skiprefine',
    'refine',
    'colinear',
    'skipunique',
    'duplications=s',
    'keeptmpfiles',
    'keepsearchfiles',
    'tba|s',
    'mugsywga|s',
    'nucmeropts|o=s',
    'plot',
    'nofilter|n',
    'translated|s',
    'debug=s',
    'log=s',
    'help|h'
    ) || pod2usage(-verbose => 3);

pod2usage(-verbose=>3) if($options{'help'});

my @inputseqfiles = @ARGV;

pod2usage(-verbose=>3, -message => "Need to specify valid input fasta file") if(! scalar(@inputseqfiles));

my $mafoutput;
my $pwfasta = "$absprefix.xmfa";
my $allfsafile = "$absprefix.all.fsa";

print STDERR "Starting MUGSYWGA: ",`date`;
my $pwdupsfasta = "$absprefix.dups.xmfa";
print $logfh `rm -f $allfsafile`;
foreach my $fsafile (@allfastafiles){
    #HACK 
    #Temp fix for headers 
    my $perlcmd = q|perl -ne 'if(/^\>([^\s\:]+)\:([^\s\:]+)/){if($1 ne $2){ print ">$1.$2 $1\n";} elsif(defined $1 && defined $2){print ">$1.$2\n";}else {print ">$1\n";}}else{die if(/\>/);print $_}'|;
    print $logfh "CMD:cat $fsafile | $perlcmd >> $allfsafile\n";
    print $logfh `cat $fsafile | $perlcmd >> $allfsafile`;
    unlink $fsafile if(! defined $options{'keeptmpfiles'});
}
unlink "$pwfasta";
foreach my $maf (@maffiles){
    my $maf2fasta = "$maf2fastacmd < $maf >> $pwfasta";
    print $logfh "CMD:$maf2fasta\n" if($options{'debug'});
    print $logfh `$maf2fasta`;
}
if($options{'duplications'}){
    unlink "$pwdupsfasta";
    foreach my $maf (@dupmaffiles){
	my $maf2fasta = "$maf2fastacmd < $maf >> $pwdupsfasta";
	print $logfh "CMD:$maf2fasta\n" if($options{'debug'});
	print $logfh `$maf2fasta`;
	unlink $maf if(! defined $options{'keeptmpfiles'});
    }
} 
if(scalar(@maffiles)==0 || -z "$pwfasta"){
    open FILE, "+>$absprefix.maf" or die "Can't open $absprefix.maf";
    print FILE "##maf version=1 scoring=mugsy\n";
    print FILE "##eof maf\n";
    close FILE; 
    $mafoutput="$absprefix.maf";
}
else{
    $mafoutput = &runMugsywga($allfsafile,$pwfasta,$pwdupsfasta,$options{'distance'},$options{'minlength'});
    print STDERR "\nFinished MUGSYWGA: ",`date`;
    unlink $allfsafile if(! defined $options{'keeptmpfiles'});
}
