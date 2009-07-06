/* Scumm Tools
 * Copyright (C) 2004-2006  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#ifndef COMPRESS_H
#define COMPRESS_H

#include "tool.h"

#ifndef DISABLE_BUILTIN_VORBIS
#include <vorbis/vorbisenc.h>
#endif
#ifndef DISABLE_BUILTIN_FLAC
#define FLAC__NO_DLL 1
#include <FLAC/stream_encoder.h>
#endif

/* We use string constants here, so we can insert the
 * constants directly into literals
 * These are given integer definitions below
 */

/* These are the defaults parameters for the Lame invocation */
#define minBitrDef_str "24"
#define maxBitrDef_str "64"
#define algqualDef_str "2"
#define vbrqualDef_str "4"

/* The default for oggenc invocation is to use the --quality option only */
#define oggqualDef_str "3"

/* These are the default parameters for the FLAC invocation */
#define flacCompressDef_str "8"
#define flacBlocksizeDef_str "1152"

#define TEMP_WAV	"tempfile.wav"
#define TEMP_RAW	"tempfile.raw"
#define TEMP_MP3	"tempfile.mp3"
#define TEMP_OGG	"tempfile.ogg"
#define TEMP_FLAC	"tempfile.fla"



/**
 * Compression tool
 * A tool, which can compress to either MP3, Vorbis or FLAC formats
 */

class CompressionTool : public Tool {
public:
	CompressionTool(const std::string &name);

	void parseAudioArguments();
public:
	AudioFormat _format;

	// Settings
	// mp3 settings
	std::string _mp3CompressionType;
	std::string _mp3MpegQuality;
	std::string _mp3ABRBitrate;
	std::string _mp3VBRMinBitrate;
	std::string _mp3VBRMaxBitrate;
	std::string _mp3VBRQuality;

	// flac
	std::string _flacCompressionLevel;
	std::string _flacBlockSize;
	
	// vorbis
	std::string _oggQuality;
	std::string _oggMinBitrate;
	std::string _oggAvgBitrate;
	std::string _oggMaxBitrate;
};

/*
 * Stuff which is in compress.cpp
 */
const extern char *tempEncoded;

extern AudioFormat process_audio_params(int argc, char *argv[], int* i);
extern int process_mp3_parms(int argc, char *argv[], int* i);
extern int process_ogg_parms(int argc, char *argv[], int* i);
extern int process_flac_parms(int argc, char *argv[], int* i);

extern void extractAndEncodeVOC(const char *outName, FILE *input, AudioFormat compMode);
extern void extractAndEncodeWAV(const char *outName, FILE *input, AudioFormat compMode);

extern void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, AudioFormat compmode);
extern void encodeRaw(char *rawData, int length, int samplerate, const char *outname, AudioFormat compmode);
extern void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample);


/* Integer definitions for the constants above */
#define minBitrDef atoi(minBitrDef_str)
#define maxBitrDef atoi(maxBitrDef_str)
#define algqualDef atoi(algqualDef_str)
#define vbrqualDef atoi(vbrqualDef_str)
#define oggqualDef atoi(oggqualDef_str)
#define flacCompressDef atoi(flacCompressDef_str)
#define flacBlocksizeDef atoi(flacBlocksizeDef_str)

/* Perhaps this should be in a seperate file */
#define kCompressionAudioHelp \
	"\nParams:\n" \
	" --mp3        encode to MP3 format (default)\n" \
	" --vorbis     encode to Vorbis format\n" \
	" --flac       encode to Flac format\n" \
	"(If one of these is specified, it must be the first parameter.)\n" \
	\
	"\nMP3 mode params:\n" \
	" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:" minBitrDef_str "%d)\n" \
	" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%" maxBitrDef_str ")\n" \
	" --vbr        LAME uses the VBR mode (default)\n" \
	" --abr        LAME uses the ABR mode\n" \
	" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:" vbrqualDef_str "%d)\n" \
	" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:" algqualDef_str ")\n" \
	" --silent     the output of LAME is hidden (default:disabled)\n" \
	\
	"\nVorbis mode params:\n" \
	" -b <rate>    <rate> is the nominal bitrate (default:unset)\n" \
	" -m <rate>    <rate> is the minimum bitrate (default:unset)\n" \
	" -M <rate>    <rate> is the maximum bitrate (default:unset)\n" \
	" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:" oggqualDef_str ")\n" \
	" --silent     the output of oggenc is hidden (default:disabled)\n" \
	\
	"\nFlac mode params:\n" \
	" --fast       FLAC uses compression level 0\n" \
	" --best       FLAC uses compression level 8\n" \
	" -<value>     specifies the value (0 - 8) of compression (8=best)(default:" flacCompressDef_str ")\n" \
	" -b <value>   specifies a blocksize of <value> samples (default:" flacBlocksizeDef_str ")\n" \
	" --verify     files are encoded and then decoded to check accuracy\n" \
	" --silent     the output of FLAC is hidden (default:disabled)\n" \
	\
	"\n --help     this help message\n" \
	\
	"\n\nIf a parameter is not given the default value is used\n" \
	"If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n";

#endif
