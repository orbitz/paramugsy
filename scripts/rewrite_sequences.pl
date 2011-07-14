#!/usr/bin/env perl
use strict;
use Getopt::Long qw(:config no_ignore_case no_auto_abbrev);
use File::Basename;
use Pod::Usage;
use POSIX;

my $problemchars = "\.\?\-";
my $cleanregex = '[\-]';

my %options;
my $results = GetOptions (\%options, 
    'prefix|p=s',
    'directory=s',
    'debug=s',
    'log=s',
    'help|h'
    ) || pod2usage(-verbose => 3);

my @inputseqfiles = @ARGV;

pod2usage(-verbose=>3, -message => "Need to specify valid input fasta file") if(! scalar(@inputseqfiles));

if(defined $options{'directory'}){
    if(! -d "$options{'directory'}"){
	die "-directory must be a directory";
    }
    elsif($options{'directory'} !~ /\/$/){
	$options{'directory'} .= "/";
    }
}
else{
    $options{'directory'} = "/tmp/";
}

my $absprefix =  $options{'directory'};
if(!defined $options{'prefix'}){
    $absprefix .= "tmp";
}
else{
    if($options{'prefix'} =~ /([$problemchars])/){
	die "Character '$1' found in --prefix=$options{'prefix'}.  Please choose another --prefix that excludes '$1'.\n";
    }
    $absprefix .= $options{'prefix'};
}

my $seqfiles = {};
my $genome2seqslookup = {}; 
my $seqlengthlookup = {};

my @allfastafiles;


#Aggregate all sequences for a lineage and concatenate together    
foreach my $seqfile (@inputseqfiles){
    if(! -e $seqfile){
	die "Invalid input file. Can't find $seqfile\n";
    }
    #default species name will be basename of the file
    #upto the first dot
    my $fname = basename($seqfile);
    $fname =~ s/\.[^.]+//g;
    $fname =~ s/$cleanregex/_/g;
    my $speciesname = $fname;
    unlink "$options{'directory'}/$speciesname" if(-e "$options{'directory'}/$speciesname");
    my $header;
    my $seqlen=0;
    my @seqs;
    open FILE,"$seqfile" or die "Can't open file $seqfile";
    while(my $line=<FILE>){
	if($line =~ /^>/){
	    if($seqlen>0){
		&printFASTA("$options{'directory'}/$speciesname","$speciesname:$header:1:+:$seqlen",\@seqs);
		$genome2seqslookup->{$speciesname} = [] if (!exists $genome2seqslookup->{$speciesname});
		my $tbaheader;
		if($speciesname eq $header){
		    $tbaheader = "$speciesname";
		}
		else{
		    $tbaheader = "$speciesname.$header";
		}
		push @{$genome2seqslookup->{$speciesname}},["$speciesname:$header:1:+:$seqlen",$tbaheader,$seqlen,"$options{'directory'}/$speciesname"];
		$seqfiles->{"$options{'directory'}/$speciesname"}++;
	    }
	    $seqlen=0;
	    @seqs=();
	    $header='';
	    chomp $line;
	    if($line =~ /^>([^:]+):([^:]+):/){
		#multiz,tba formatted headers
		#species name specified, override filename
		$speciesname = $1;
		$header = $2;
		#print $logfh "Parsing FASTA entry header:$header speciesname:$speciesname\n" if($options{'debug'});
	    }
	    elsif($line =~ /gi\|\d+\|\w+\|([^.]+)\S+\|/){
		#special handling of ncbi formatted headers
		#just pull accession
		$header = $1;
		#print $logfh "Parsing FASTA entry header:$header speciesname:$speciesname\n" if($options{'debug'});
	    }
	    elsif($line =~ /^>(\S+)/){
		#plain ole header
		$header = $1;
                $header =~ s/$cleanregex/_/g;
		#print $logfh "Parsing FASTA entry header:$header speciesname:$speciesname\n" if($options{'debug'});
		
	    }
	    else{
		die "Can't parse FASTA header for $seqfile";
	    }
	}
	else{
	    $line =~ s/\s//g;
	    $seqlen += length($line);
	    push @seqs,$line;
	}
    }
    #
    if($seqlen){
	$seqlengthlookup->{$speciesname} = $seqlen;
	&printFASTA("$options{'directory'}/$speciesname","$speciesname:$header:1:+:$seqlen",\@seqs);
	$genome2seqslookup->{$speciesname} = [] if (!exists $genome2seqslookup->{$speciesname});
	my $tbaheader;
	if($speciesname eq $header){
	    $tbaheader = "$speciesname";
	}
	else{
	    $tbaheader = "$speciesname.$header";
	}
	die "Cannot file FASTA file $options{'directory'}/$speciesname" if(! -e "$options{'directory'}/$speciesname");
	
	push @{$genome2seqslookup->{$speciesname}},
	["$speciesname:$header:1:+:$seqlen",$tbaheader,$seqlen,"$options{'directory'}/$speciesname"];
	
	$seqfiles->{"$options{'directory'}/$speciesname"}++;
    }
    close FILE; 
    push @allfastafiles,"$options{'directory'}/$speciesname";

}

foreach my $f (@allfastafiles) {
    print $f, "\n"
}

sub printFASTA{
    my($fname,$header,$seqs) = @_;

    #print $logfh "Writing file $fname\n" if($options{'debug'});

    open FFILE,">>$fname" or die "Can't open file\n";
    print FFILE ">$header\n";
    foreach my $s (@$seqs){
	print FFILE $s,"\n";
    }
    close FFILE;
}
