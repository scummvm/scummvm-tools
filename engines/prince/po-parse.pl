#!perl

# Generate .po file for The Prince and the Coward game

use utf8;

sub process_inv($);
sub process_varia($);
sub process_mob($);
sub process_credits($);
sub process_talk($);

use open qw/:std :utf8/;

if ($#ARGV != 1) {
	die "Usage: $0 <language-code> <file>";
}

my %data;

my $lang = $ARGV[0];

my $fname = "";
my $idx1 = "";
my $idx2 = "";
my $seenheader = 0;

my %data;

while (<$ARGV[1]>) {
	chomp;

	if (/^#: ([^:]]+):(\d+)$/) {
		$fname = $1;
		$idx1 = $2;

		$seenheader = 1;

		next;
	}

	if (/^msgid (.*)$/) {
		my $s = $1;

		$s =~ s/(^")|("$)//g;
		$s =~ s/\\"/"/g;

		$data{$fname}{$idx1} = $s;
	}
}


process_inv "invtxt.txt.out1";
process_varia "variatxt.txt.out1";
process_mob "mob.txt.out1";
process_credits "credits.txt.out1";
process_talk "talktxt.txt.out1";

exit;

sub process_inv($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}

sub process_varia($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}

sub process_mob($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}

sub process_credits($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}

sub process_talk($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}
