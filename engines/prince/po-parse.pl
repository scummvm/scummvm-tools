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

my $lang = $ARGV[0];

my $fname = "";
my $idx1 = "";
my $idx2 = "";
my $seenheader = 0;

my $inmsgid = 0;
our %data;

open IN, $ARGV[1];

while (<IN>) {
	chomp;

	if (/^#: ([^:]+):(\d+)$/) {
		$fname = $1;
		$idx1 = $2;

		$seenheader = 1;

		next;
	}

	if (/^msgid ""$/) {
		$inmsgid = 1;
		next;
	}

	if (/^msgid (.*)$/) {
		my $s = $1;

		$s =~ s/(^")|("$)//g;

		$data{$fname}{$idx1} = $s;

		$inmsgid = 0;
	}

	if (/^"(.*)"$/) {
		if ($inmsgid) {
			$data{$fname}{$idx1} .= $1;
		}
	}

	if (/^msgstr/) {
		$inmsgid = 0;
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

	print OUT "invtxt.dat\n";

	for $n (sort {$a<=>$b} keys $data{'invtxt.txt'}) {
		print OUT "$n. $data{'invtxt.txt'}{$n}\n";
	}

	close OUT;
}

sub process_varia($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "variatxt.dat\n";

	for $n (sort {$a<=>$b} keys $data{'variatxt.txt'}) {
		print OUT "$n. $data{'variatxt.txt'}{$n}\n";
	}

	close OUT;
}

sub process_mob($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "mob.lst\n";

	my $pn = 0;

	for $n (sort {$a<=>$b} keys $data{'mob.lst'}) {
		my $p1 = int($n / 1000);

		if ($p1 != $pn) {
			if ($p1 > 1) {
				for my $i (($pn+1)..$p1) {
					print OUT "$i.\n";
				}
			}
			$pn = $p1;
		}
		print OUT "$data{'mob.lst'}{$n}\n";
	}

	for my $i (($pn+1)..61) {
		print OUT "$i.\n";
	}

	close OUT;
}

sub process_credits($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "credits.dat\n";

	for $n (sort {$a<=>$b} keys $data{'credits.txt'}) {
		$data{'credits.txt'}{$n} =~ s/\\n/\n/g;

		print OUT "$data{'credits.txt'}{$n}";
	}

	close OUT;
}

sub process_talk($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	close OUT;
}
