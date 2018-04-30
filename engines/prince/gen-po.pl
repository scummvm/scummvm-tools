#!perl

# Generate .po file for The Prince and the Coward game

use utf8;

sub process_inv($$);
sub process_varia($$);
sub process_mob($$);
sub process_credits($$);
sub process_talk($$);

sub process_talkWithDialog($$$);
sub process_talkNoDialog($$$);

use open qw/:std :utf8/;

if ($#ARGV != 1) {
	die "Usage: $0 <language-code> <directory-to-polish>";
}

our %data;

our $lang = $ARGV[0];
my $poldir = $ARGV[1];

print <<EOF;
# The Prince and the Coward lanugage file
# Copyright (C) 2018 ScummVM Team
# This file is distributed under the same license as the ScummVM package.
# Eugene Sandulenko <sev\@scummvm.org>, 2018.
#
msgid ""
msgstr ""
"Project-Id-Version: Prince\\n"
"Report-Msgid-Bugs-To: scummvm-devel\@lists.scummvm.org\\n"
"POT-Creation-Date: 2018-04-17 19:53+0100\\n"
"PO-Revision-Date: 2018-04-17 18:01+0000\\n"
"Last-Translator: Eugene Sandulenko <sev\@scummvm.org>\\n"
"Language-Team: none\\n"
"Language: $lang\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=2; plural=n != 1;\\n"
"X-Generator: Weblate 2.9\\n"
EOF

process_inv $lang, "invtxt.txt.out";
process_varia $lang, "variatxt.txt.out";
process_mob $lang, "mob.txt.out";
process_credits $lang, "credits.txt.out";
process_talk $lang, "talktxt.txt.out";

exit;

sub process_inv($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	while (<IN>) {
		chomp;

		next if $_ eq 'invtxt.dat';

		/^([\d]+)\.\s+(.*)$/;

		$data{$lang}{'invtxt.dat'}{$1} = $2;

		print <<EOF;

#: invtxt.txt:$1
msgid "$2"
msgstr ""
EOF
	}

	close IN;
}

sub process_varia($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	while (<IN>) {
		chomp;

		next if $_ eq 'variatxt.dat';

		/^([\d]+)\.\s+(.*)$/;

		$data{$lang}{'variatxt.dat'}{$1} = $2;

		print <<EOF;

#: variatxt.txt:$1
msgid "$2"
msgstr ""
EOF
	}

	close IN;
}

sub process_mob($$) {
	my $lang = shift;
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

		$data{$lang}{'mob.lst'}{$n} = $_;

		print <<EOF;

#: mob.lst:$n
msgid "$_"
msgstr ""
EOF
	}

	close IN;
}

sub process_credits($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	my $n = 0;
	my $line = 0;
	my $str = "";

	while (<IN>) {
		chomp;

		next if $_ eq 'credits.dat';

		$line++;
		$str .= "\"$_\\n\"\n";

		if ($line == 10) {
			$data{$lang}{'credits.txt'}{$n} = $str;

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

	$data{$lang}{'credits.txt'}{$n} = $str;

	print <<EOF;

#: credits.txt:$n
msgid ""
${str}msgstr ""
EOF

	close IN;
}

sub process_talk($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	my $n = 0;
	my $dialog = 1;
	my $str = "";

	while (<IN>) {
		chomp;

		next if $_ eq 'talktxt.dat';

		my $d = sprintf("%03d", $dialog);

		if ($_ eq "\@DIALOGBOX_LINES:") {
			process_talkWithDialog($lang, $d, IN);
		} elsif ($_ eq "\@NORMAL_LINES:") {
			process_talkNoDialog($lang, $d, IN);
		}

		$dialog++;
	}

	close IN;
}


sub process_talkWithDialog($$$) {
	my $lang = shift;
	my $dialog = shift;
	my $in = shift;

	my $s;
	my $line = 0;

	my $fname = "dialog$dialog.txt";

	while (<$in>) {
		chomp;

		if (/^#HERO$/) {
			$s .= "HERO: ";
		} elsif (/^#OTHER$/) {
			$s .= "OTHER: ";
		} elsif (/^#OTHER2$/) {
			$s .= "OTHER2: ";
		} elsif (/^#PAUSE$/) {
			$s .= "P#";
		} elsif (/^#BOX 0$/) {
			$_ = <$in>; # skip #END
			last; # Break
		} else {
			$line++;
			$data{$lang}{$fname}{$line} = "$s$_";
			print <<EOF;

#: $fname:$line
msgid "$s$_"
msgstr ""
EOF

			$s = "";
		}
	}

	my $box;

	while (<$in>) {
		chomp;

		if (/^\@DIALOG_BOX (\d+)$/) {
			$box = $1 + 1;
			if ($box > 9) {
				die "Too big DIALOG_BOX: $box";
			}
			next;
		} elsif (/^\@DIALOG_OPT (\d+)$/) {
			$box = $1;
			$line = 0;
			last;
		} elsif (/^#END$/) {
			next;
		} elsif (/^\$(\d+)$/) {
			$s = "$_: ";
			$line++;
		} else {
			my $n = sprintf("%d%02d", $box, $line);
			$data{$lang}{$fname}{$n} = "$s$_";
			print <<EOF;

#: $fname:$n
msgid "$s$_"
msgstr ""
EOF

		}
	}

	$s = "";

	my $needPrint = 0;
	my $snew;

	while (<$in>) {
		chomp;

		if (/^#HERO$/) {
			$snew .= "HERO: ";
			$needPrint = 1;
		} elsif (/^#OTHER$/) {
			$snew .= "OTHER: ";
			$needPrint = 1;
		} elsif (/^#OTHER2$/) {
			$snew .= "OTHER2: ";
			$needPrint = 1;
		} elsif (/^#PAUSE$/) {
			$snew .= "P#";
		} elsif (/^#ENABLE (\d+)$/) {
			$s .= "#E$1"
		} elsif (/^#DISABLE (\d+)$/) {
			$s .= "#D$1"
		} elsif (/^#BOX (\d+)$/) {
			$s .= "#B$1"
		} elsif (/^#EXIT (\d+)$/) {
			$s .= "#X$1"
		} elsif (/^#FLAG (\d+)$/) {
			$s .= "#F$1"
		} elsif (/^#END$/) {
			$needPrint = 1;
			$snew = "";

			if ($line == 0) {
				$line = 1;   # Force print empty lines
			}
		} elsif (/^\@DIALOG_OPT (\d+)$/) {
			$box = $1;
			$line = 0;
		} elsif (/^#ENDEND$/) {
			last;
		} else {
			$line++;
			$s .= $_;
		}

		if ($needPrint) {
			my $n = sprintf("%d%02d", 1000 + $box, $line);

			if ($line) {
				$data{$lang}{$fname}{$n} = "$s";
				print <<EOF;

#: $fname:$n
msgid "$s"
msgstr ""
EOF
			}

			$s = $snew;
			$snew = "";
			$needPrint = 0;
		}
	}
}

sub process_talkNoDialog($$$) {
	my $lang = shift;
	my $dialog = shift;
	my $in = shift;

	my $s;
	my $line = 0;

	my $fname = "dialog$dialog.txt";

	while (<$in>) {
		chomp;

		if (/^#HERO$/) {
			$s .= "HERO: ";
		} elsif (/^#OTHER$/) {
			$s .= "OTHER: ";
		} elsif (/^#OTHER2$/) {
			$s .= "OTHER2: ";
		} elsif (/^#PAUSE$/) {
			$s .= "P#";
		} elsif (/^#END$/) {
			last; # Break
		} else {
			$line++;
			$data{$lang}{$fname}{$line} = "$s$_";
			print <<EOF;

#: $fname:$line
msgid "$s$_"
msgstr ""
EOF

			$s = "";
		}
	}
}
