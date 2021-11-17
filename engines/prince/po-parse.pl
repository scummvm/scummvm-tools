#!perl

# Parse .po file for The Prince and the Coward game

use utf8;

sub process_inv($);
sub process_varia($);
sub process_mob($);
sub process_credits($);
sub process_talk($);
sub process_talktxt_ids($);
sub convert_utf($$);

use open qw/:std :utf8/;

if ($#ARGV != 1) {
	die "Usage: $0 <language-code> <file.po>";
}

my $lang = $ARGV[0];

my $fname = "";
my $idx1 = "";
my $idx2 = "";
my $seenheader = 0;

my $inmsgid = 0;
my $inmsgstr = 0;
our %data;
our %data1;

open IN, $ARGV[1];

while (<IN>) {
	chomp;

	if (/^#: ([^:]+):(\d+)$/) {
		$fname = $1;
		$idx1 = $2;

		$seenheader = 1;

		$inmsgid = $inmsgstr = 0;

		next;
	}

	if (/^msgid ""$/) {
		$inmsgid = 1;
		next;
	}

	if (/^msgstr ""$/) {
		$inmsgid = 0;
		$inmsgstr = 1;
		next;
	}

	$_ = convert_utf $lang, $_;

	if (/^msgid (.*)$/) {
		my $s = $1;

		$s =~ s/(^")|("$)//g;

		$data{$fname}{$idx1} = $s;

		$inmsgid = 0;
	}

	if (/^msgstr (.*)$/) {
		my $s = $1;

		$s =~ s/(^")|("$)//g;

		$data1{$fname}{$idx1} = $s;

		$inmsgstr = 0;
	}

	if (/^"(.*)"$/) {
		if ($inmsgid) {
			$data{$fname}{$idx1} .= $1;
		} elsif ($inmsgstr) {
			$data1{$fname}{$idx1} .= $1;
		}
	}
}

for my $f (keys %data) {
	for my $k (keys %{$data{$f}}) {
		if (not exists $data1{$f}{$k}) {
			warn "Missing msgstr $f:$k";
		} else {
			my $pref1;
			my $pref2;
			($pref1) = ($data{$f}{$k} =~ /^([P#HEROT0-9\$]+:)/);
			($pref2) = ($data1{$f}{$k} =~ /^([P#HEROT0-9\$]+:)/);

			if ($pref1 cmp $pref2) {
				warn "Incorrect prefix in $f:$k: $pref1 != $pref2";
			}

			($pref1) = ($data{$f}{$k} =~ /([#EDBXF0-9]+)$/);
			($pref2) = ($data1{$f}{$k} =~ /([#EDBXF0-9]+)$/);

			if ($pref1 cmp $pref2) {
				warn "Incorrect postfix in $f:$k: $pref1 != $pref2";
			}
		}
	}
}


process_inv "invtxt.txt";
process_varia "variatxt.txt";
process_mob "mob.txt";
process_credits "credits.txt";
process_talk "talktxt.txt";
process_talktxt_ids "talktxt_ids.txt";

exit;

sub convert_utf($$) {
	my $lang = shift;
	my $s = shift;

	return $s;
}

sub process_inv($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "invtxt.dat\nitemNr. name - exam text\n";

	for my $n (sort {$a<=>$b} keys $data1{'invtxt.txt'}) {
		print OUT "$n. $data1{'invtxt.txt'}{$n}\n";
	}

	close OUT;
}

sub process_varia($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "variatxt.dat\nstringId. string\n";

	for my $n (sort {$a<=>$b} keys $data1{'variatxt.txt'}) {
		print OUT "$n. $data1{'variatxt.txt'}{$n}\n";
	}

	close OUT;
}

sub process_mob($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "mob.lst\nmob_name - exam text\n1.\n";

	my $pn = 0;

	for my $n (sort {$a<=>$b} keys $data1{'mob.lst'}) {
		my $p1 = int($n / 1000);

		if ($p1 != $pn) {
			if ($p1 > 1) {
				for my $i (($pn+1)..$p1) {
					print OUT "$i.\n";
				}
			}
			$pn = $p1;
		}
		print OUT "$data1{'mob.lst'}{$n}\n";
	}

	for my $i (($pn+1)..61) {
		print OUT "$i.\n";
	}

	close OUT;
}

sub process_talk($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "talktxt.dat\n";

	for my $f (sort grep /^dialog/, keys %data1) {
		$f =~ /dialog(\d+)/;
		my $dialog = $1;
		my $hasDialog = !!grep { $_ > 100 } keys $data1{$f};

		if ($hasDialog) {
			print OUT "\@DIALOGBOX_LINES:\n";
		} else {
			print OUT "\@NORMAL_LINES:\n";
		}

		my $seenDialogBox = 0;
		my $prevBox = -1;

		for my $n (sort {$a<=>$b} keys $data1{$f}) {
			my $s = $data1{$f}{$n};
			if ($n < 100) {
				while ($s =~ /^P#/) {
					print OUT "#PAUSE\n";
					$s = substr $s, 2;
				}

				if ($s =~ /^([^:]+): (.*)$/) {
					print OUT "#$1\n$2\n";
				} else {
					print OUT "$s\n";
				}
			} elsif ($n < 100000) {
				if (!$seenDialogBox) {
					print OUT "#BOX 0\n";
					$seenDialogBox = 1;
				}
				my $box = int($n/100) - 1;
				if ($box != $prevBox) {
					print OUT "#END\n\@DIALOG_BOX $box\n";
					$prevBox = $box;
				}
				$s =~ /^(\$\d+): (.*)$/;
				print OUT "$1\n$2\n";
			} else {
				if ($seenDialogBox < 2) {
					$prevBox = -1;
					$seenDialogBox = 2;
				}
				my $box = int($n/100) - 1000;
				if ($box != $prevBox) {
					print OUT "#END\n\@DIALOG_OPT $box\n";
					$prevBox = $box;
				}

				while ($s =~ /^P#/) {
					print OUT "#PAUSE\n";
					$s = substr $s, 2;
				}

				if ($s =~ /^([^:]+): (.*)$/) {
					print OUT "#$1\n";
					$s = $2;
				}
				my @l = split /#/, $s;
				print OUT "$l[0]\n" if $l[0] ne "";
				shift @l;

				for my $d (@l) {
					$d =~ s/^E/#ENABLE /;
					$d =~ s/^D/#DISABLE /;
					$d =~ s/^B/#BOX /;
					$d =~ s/^X/#EXIT /;
					$d =~ s/^F/#FLAG /;
					print OUT "$d\n";
				}
			}
		}
		print OUT "#END\n";
		if ($hasDialog) {
			print OUT "#ENDEND\n";
		}
	}

	close OUT;
}

sub process_talktxt_ids($) {
	my $file = shift;

	my @data = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
		31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
		61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
		91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
		121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150,
		151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180,
		181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
		211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240,
		241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270,
		271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300,
		301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330,
		331, 332, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350,
		351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380,
		381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, );

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	print OUT "talktxt_ids\n";

	for my $d (@data) {
		print OUT "$d\n";
	}

	print OUT "0\n" x (1999 - $#data);

	close OUT;
}

sub process_credits($) {
	my $file = shift;

	open(*OUT, ">$file") or die "Cannot open file $file: $!";

	$now_string = gmtime;

	print OUT <<EOF;
credits.dat




GALADOR - THE PRINCE AND THE COWARD
v2.0 - ENGLISH
Built on: $now_string




#



SCENARIO

Adrian Chmielarz
Jacek Piekara



#
GRAPHICS AND ANIMATION

Andrzej Kukuta
Grzegorz Miechowski
Juliusz Gruber
Pawet Piotrowski
Andrzej Mitula
Ireneusz Konior
Pawet Miechowski

#



PROGRAMMING

Maciej Marzec




#

MUSIC

Karim Martusewicz

SOUND EFFECTS

Adam "Scorpik" Skorupa


#



ENGLISH LOCALIZATION

ShinjiGR
ScummVM Team
Eugene Sandulenko
Anna Baldur / Balduranne


##
EOF

	close OUT;
}
