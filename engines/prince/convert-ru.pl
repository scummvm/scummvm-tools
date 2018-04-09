#!perl

# Convert Russian talktxt.txt to UTF-8

use utf8;

binmode STDOUT, ":utf8";

my $skip = 0;

while (<>) {
	if (/^[#@]/ or $skip) {
		print;
		next;
	}

	if (/^nj b dsqltn.$/) {  # After this phrase we have German
		$skip = 1;
	}

	tr /qwertyuiopasdfghjklzxcvbnm\x85\xb3\xca\xa5\xea\xe6/йцукенгшщзфывапролдячсмитьхюэжбъ/;
	tr /QWERTYUIOPASDFGHJKLZXCVBNM\x82\x7f\x83\x81\x84/ЙЦУКЕНГШЩЗФЫВАПРОЛДЯЧСМИТЬЭХБЖЮ/;
	print;
}
