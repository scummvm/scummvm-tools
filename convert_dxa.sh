#! /bin/bash
# Script for converting SMK video to ScummVM DXA format
#
# Put this batch file into the game directory you want to convert.
# This script will search for all SMK files in all subtree directories,
# and encode them as DXA files next to the original files.
#
# (c) 2006-2019 ScummVM Team
# (c) 2006 crowley
#

echo ScummVM SMK2DXA batch convertor

###################################################################
## Edit audio parameter to mp3/vorbis/flac and additional params ##
###################################################################

AUDIO=--vorbis
AUDIO_PARAMS=

###################################################################
## Default paths for tools and ffmpeg                            ##
###################################################################

FFMPEG=ffmpeg
SCUMMVM_TOOLS_CLI=scummvm-tools-cli

for i in $(find $1 -name "*.SMK")
do
	in=`basename "$i" .SMK`;
	folder=`dirname "$i"`;
	mv "$i" "$folder/$in.smk";
done

for i in $(find $1 -name "*.smk")
do
	in=`basename "$i" .smk`;
	folder=`dirname "$i"`;
	if [ ! -f "$folder/$in.dxa" ]
	then
	$FFMPEG -i "$i" -f image2 "$folder/$in.smk%03d.png";
	$FFMPEG -i "$i" -acodec pcm_u8 "$folder/$in.wav";
	previous=$cwd
	cd $folder
	$SCUMMVM_TOOLS_CLI --tool encode_dxa $AUDIO $AUDIO_PARAMS "$in.smk";
	rm $in*.png "$in.wav";
	cd $previous
	fi
done

