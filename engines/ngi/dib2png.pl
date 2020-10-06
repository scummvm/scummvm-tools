#!/bin/perl
#
#

use Fcntl qw(SEEK_END);

my $file = shift or die "Usage: $0 file";

open IN, $file or die "Cannot open file $file: $!";

seek IN, -32, SEEK_END;

if (read(IN, $header, 32) != 32) {
  die "Something terrible happened";
}

(undef, undef, $width, $height) = unpack "VVVV", $header;

print "$width x $height\n";

exec "convert -size ${width}x${height} -flip gray:$file $file.png";

