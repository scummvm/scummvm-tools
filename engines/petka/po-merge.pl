#!perl

# Parse .po files for Red Comrades

use utf8;

use open qw/:std :utf8/;

if ($#ARGV < 0) {
	die "Usage: $0 [-pot] <file.po>...";
}

my $idx1 = "";
my $idx2 = "";
my $seenheader = 0;

my $inmsgid = 0;
my $inmsgstr = 0;
our %data;
our %data1;
our %data3;

my $prevvalue;

my @poOrder;

my $potmode = 0;

foreach my $file (@ARGV) {
	if ($file eq '-pot') {
		$potmode = 1;
		next;
	}

    unless (-e $file) {
		next;
	}

	print STDERR "Processing $file...";

	open IN , '<'.$file  or die $!;
	while (<IN>) {
		chomp;

		if (/^#\. (.*)$/) {
			$idx1 = $1;

			$seenheader = 1;

			$inmsgid = $inmsgstr = 0;

			$prevvalue = $data{$idx1};

			push @poOrder, $idx1;

			next;
		}

		if (/^msgid ""$/) {
			$inmsgid = 1;
			next;
		}

		if (/^msgstr ""$/) {
			$inmsgid = 0;
			$inmsgstr = 1;

			if (length $prevvalue) {
				if ($prevvalue ne $data{$idx1}) {
					warn "Not matching $file:$idx1\n";
				}
			}
			next;
		}

		if (/^msgid (.*)$/) {
			my $s = $1;

			$s =~ s/^"\s*|\s*"$//g;

			$data{$idx1} = $s;

			$inmsgid = 0;
		}

		if (/^msgstr (.*)$/) {
			my $s = $1;

			$s =~ s/(^")|("$)//g;

			$data1{$idx1} = $s;

			$inmsgstr = 0;

			if (length $prevvalue) {
				if ($prevvalue ne $data{$idx1}) {
					warn "Not matching $file:$idx1 ($prevvalue) <=> ($data{$idx1})\n";
				}
			}
		}

		if (/^"(.*)"$/) {
			if ($inmsgid) {
				$data{$idx1} .= $1;
			} elsif ($inmsgstr) {
				$data{$idx1} =~ s/^\s+|\s+$//g;
				$data1{$idx1} .= $1;
			}
		}
	}
	close IN;

	print STDERR "done\n";
}

# Read strings

foreach my $file ("strings.bytes", "stringsEN.bytes") {
	print STDERR "Processing $file...";

	open IN , '<'.$file  or die $!;
	while (<IN>) {
		chomp;

		next if /^\/\//;
		next unless length;

		/^([^ ]+) (.*)$/;

		$data3{$file}{$1} = $2;
	}
	close IN;

	print STDERR "done\n";
}

# Check for differently translated strings
my %dict;
for $k (keys %{ $data3{"strings.bytes"} }) {
	my $w = $data3{"strings.bytes"}{$k};
	my $t = $data3{"stringsEN.bytes"}{$k};

	next if ($t =~ /[А-Яа-я]/);
	$w =~ s/"/\\"/g;
	$t =~ s/"/\\"/g;
	$w =~ s/^\s+|\s+$//g;
	$t =~ s/^\s+|\s+$//g;

	if (exists $dict{$w}) {
		if ($dict{$w} ne $t) {
			warn "Double translated $k: ($dict{$w}) vs ($t)\n";
		}
		next;
	}

	$dict{$w} = $t;
}

# Now print out .pot file

print <<EOF;
# LANGUAGE translation for Petka1.
# Copyright (C) 2020 ScummVM Team
# This file is distributed under the same license as the ScummVM package.
# Eugene Sandulenko <sev\@scummvm.org>, 2020.

msgid ""
msgstr ""
"Project-Id-Version: Petka1 0.0.1\\n"
"Report-Msgid-Bugs-To: scummvm-devel\@lists.scummvm.org\\n"
"POT-Creation-Date: 2020-12-07 22:01+0000\\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"
"Last-Translator: FULL NAME <EMAIL\@ADDRESS>\\n"
"Language-Team: LANGUAGE <LL\@li.org>\\n"
"Language: \\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=utf-8\\n"
"Content-Transfer-Encoding: 8bit\\n"

EOF

my $missings;
my $matches;
for my $k (@poOrder) {
	print "#. $k\n";
	print "msgid \"$data{$k}\"\n";

	if ($data{$k} =~ /^I(.*)/) {
		if (exists $dict{$1}) {
			print "msgstr \"I$dict{$1}\"\n\n";
			$matches++;
			next;
		}
	}

	if (exists $dict{$data{$k}} && !$potmode) {
		print "msgstr \"$dict{$data{$k}}\"\n";
		$matches++;
	} else {
		print "msgstr \"\"\n";
		$missings++;
	}

	print "\n";
}

warn "$matches matches, $missings missings\n";
