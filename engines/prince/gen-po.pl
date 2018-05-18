#!perl

# Generate .po file for The Prince and the Coward game

use utf8;

sub process_inv($$);
sub process_varia($$);
sub process_mob($$);
sub process_credits($$);
sub process_talk($$);
sub process_talk_ids($$);

sub process_talkWithDialog($$$);
sub process_talkNoDialog($$$);

use open qw/:std :utf8/;

if ($#ARGV != 1) {
	die "Usage: $0 <language-code> <directory-to-polish>";
}

our %data;
our %data_ids;

our $lang = $ARGV[0];
my $poldir = $ARGV[1];

my $git = "<unknown>";

open GIT, "git describe --tags |" or warn "Can't run git";

$git = <GIT>;

close GIT;

chomp $git;


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
"X-Generator: gen-po.pl $git\\n"
EOF

process_talk_ids $lang, "talktxt_ids.txt";
process_credits $lang, "credits.txt.out";
process_talk $lang, "talktxt.txt.out";
process_inv $lang, "invtxt.txt.out";
process_mob $lang, "mob.txt.out";
process_varia $lang, "variatxt.txt.out";

process_talk_ids 'pl', "$poldir/talktxt_ids.txt";
process_credits 'pl', "$poldir/credits.txt.out";
process_talk 'pl', "$poldir/talktxt.txt.out";
process_inv 'pl', "$poldir/invtxt.txt.out";
process_mob 'pl', "$poldir/mob.txt.out";
process_varia 'pl', "$poldir/variatxt.txt.out";

my $miss_tr = 0;
my $extra_tr = 0;

for my $f (sort keys $data{$lang}) {
	for my $n (sort {$a<=>$b} keys $data{$lang}{$f}) {
		if (exists $data{$lang}{$f}{$n} and not exists $data{'pl'}{$f}{$n}) {
			warn "$lang:$f:$n extra";

			if (index($data{$lang}{$f}{$n}, "\\n") != -1) {  # Multiline
				$data{'pl'}{$f}{$n} = "\"extra$extra_tr\\n\"";
			} else {
				$data{'pl'}{$f}{$n} = "extra$extra_tr";
			}

			$extra_tr++;
		}
	}
}

for my $f (sort keys $data{'pl'}) {
	for my $n (sort {$a<=>$b} keys $data{'pl'}{$f}) {
		if (not exists $data{$lang}{$f}{$n}) {
			warn "$lang:$f:$n missing";
			if (index($data{'pl'}{$f}{$n}, "\\n") != -1) {  # Multiline
				$data{$lang}{$f}{$n} = "\"\\n\"";
			} else {
				$data{$lang}{$f}{$n} = "";
			}

			$miss_tr++;
		}

		if (index($data{'pl'}{$f}{$n}, "\\n") != -1) {  # Multiline
			chomp $data{'pl'}{$f}{$n};
			chomp $data{$lang}{$f}{$n};

			print <<EOF;

#: $f:$n
msgid ""
$data{'pl'}{$f}{$n}
msgstr ""
$data{$lang}{$f}{$n}
EOF
		} else {
			print <<EOF;

#: $f:$n
msgid "$data{'pl'}{$f}{$n}"
msgstr "$data{$lang}{$f}{$n}"
EOF
		}

		delete $data{$lang}{$f}{$n};
	}
}

print "\n\n# Missing: $miss_tr  Extra: $extra_tr" if ($miss_tr || $extra_tr);
warn "Missing: $miss_tr  Extra: $extra_tr" if ($miss_tr || $extra_tr);

exit;

sub process_inv($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	while (<IN>) {
		chomp;

		next if $_ eq 'invtxt.dat';

		/^([\d]+)\.\s+(.*)$/;

		$data{$lang}{'invtxt.txt'}{$1} = $2;
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

		$data{$lang}{'variatxt.txt'}{$1} = $2;
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

		s/\x1//g; # Remove ^A symbol

		my $n = sprintf("%d%03d", $num, $line);

		$line++;

		$data{$lang}{'mob.lst'}{$n} = $_;
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

		if ($line == 20) {
			$n++;
			$data{$lang}{'credits.txt'}{$n} = $str;
			$line = 0;
			$str = "";
		}
	}
	$n++;

	$data{$lang}{'credits.txt'}{$n} = $str;
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

		my $d = sprintf("%04d", ($data_ids{$lang}[$dialog] || 1000 + $dialog));

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
			$s = "";
		}
	}
}

sub process_talk_ids($$) {
	my $lang = shift;
	my $file = shift;

	open(*IN, $file) or die "Cannot open file $file: $!";

	my $n = 0;

	while (<IN>) {
		chomp;

		next if $_ eq 'talktxt_ids';

		#$data_ids{$lang}[$n] = $_;
		$data_ids{$lang}[$_] = $n if $_ > 0;

		$n++;
	}

	close IN;
}
