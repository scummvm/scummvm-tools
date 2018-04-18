#!perl

# Generate .po file for The Prince and the Coward game

use utf8;

sub process_inv($);
sub process_varia($);
sub process_mob($);
sub process_credits($);

use open qw/:std :utf8/;

if ($#ARGV != 0) {
	die "Usage: $0 <language-code>";
}

my %data;

my $lang = $ARGV[0];

print <<EOF;
# The Prince and the Coward lanugage file
# Copyright (C) 2018 ScummVM Team
# This file is distributed under the same license as the ScummVM package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
msgid ""
msgstr ""
"Project-Id-Version: Prince\\n"
"Report-Msgid-Bugs-To: scummvm-devel@lists.scummvm.org\\n"
"POT-Creation-Date: 2018-04-17 19:53+0100\\n"
"PO-Revision-Date: 2018-04-17 18:01+0000\\n"
"Last-Translator: Eugene Sandulenko <sev@scummvm.org>\\n"
"Language-Team: none\\n"
"Language: $lang\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=2; plural=n != 1;\\n"
"X-Generator: Weblate 2.9\\n"
EOF

process_inv "invtxt.txt.out";
process_varia "variatxt.txt.out";
process_mob "mob.txt.out";
process_credits "credits.txt.out";

exit;

sub process_inv {
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	while (<IN>) {
		chomp;

		next if $_ eq 'invtxt.dat';

		/^([\d]+)\.\s+(.*)$/;

		$data{'invtxt.dat'}{$1} = $2;

		print <<EOF;

#: invtxt.txt:$1
msgid "$2"
msgstr ""
EOF
	}

	close IN;
}

sub process_varia {
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	while (<IN>) {
		chomp;

		next if $_ eq 'variatxt.dat';

		/^([\d]+)\.\s+(.*)$/;

		$data{'variatxt.dat'}{$1} = $2;

		print <<EOF;

#: variatxt.txt:$1
msgid "$2"
msgstr ""
EOF
	}

	close IN;
}

sub process_mob {
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	my $num = 1;
	my $line = 1;

	while (<IN>) {
		chomp;

		next if $_ eq 'mob.lst';

		if (/^([\d]+)\.$/) {
			$num = $1;
			$line = 1;
			next;
		}

		my $n = sprintf("%d%03d", $num, $line);

		$line++;

		$data{'mob.lst'}{$n} = $_;

		print <<EOF;

#: mob.lst:$n
msgid "$_"
msgstr ""
EOF
	}

	close IN;
}

sub process_credits {
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	my $n = 0;
	my $line = 0;
	my $str = "";

	while (<IN>) {
		chomp;

		next if $_ eq 'credits.txt';

		$line++;
		$str .= "\"$_\\n\"\n";

		if ($line == 10) {
			$data{'credits.txt'}{$n} = $str;

			$n++;
			$line = 0;

			print <<EOF;

#: credits.txt:$n
msgid ""
$str
msgstr ""
EOF

			$str = "";
		}
	}

	$data{'credits.txt'}{$n} = $str;

	print <<EOF;

#: credits.txt:$n
msgid ""
${str}msgstr ""
EOF

	close IN;
}
