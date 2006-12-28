#! /bin/bash
# Script for converting Feeble Files video to ScummVM format
#
# modify the paths below
#
# (c) 2006 ScummVM Team
# (c) 2006 crowley
#
# $URL$
#
# $Id$

WINE=wine
VIDEO_PATH=~/games/Feeble/video
BINK=~/.wine/drive_c/Program\ Files/RADVideo/binkconv.exe
ENCODE_DXA=~/scummvm/tools/encode_dxa
FORMAT=vorbis

cd "$VIDEO_PATH"

for i in *.SMK
do
    in=`basename "$i" .SMK`;
    mv "$i" "$in.smk";
done

for i in *.smk
do
    in=`basename "$i" .smk`;
    if [ ! -f "$in.dxa" ]
    then
	$WINE "$BINK" "$i" "$in.png" /n-1 /z1 /#;
	$WINE "$BINK" "$i" "$in.wav" /v /#;
	$ENCODE_DXA --$FORMAT "$in.smk";
	rm *.png *.wav;
    fi
done
