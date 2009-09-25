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
#define minBitrDef	24
#define maxBitrDef 64
#define algqualDef 2
#define vbrqualDef 4

/* The default for oggenc invocation is to use the --quality option only */
#define oggqualDef 3

/* These are the default parameters for the FLAC invocation */
#define flacCompressDef 8
#define flacBlocksizeDef 1152

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
	CompressionTool(const std::string &name, ToolType type);

	virtual std::string getHelp() const;

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
	
public:
	bool processMp3Parms();
	bool processOggParms();
	bool processFlacParms();

	void extractAndEncodeVOC(const char *outName, File &input, AudioFormat compMode);
	void extractAndEncodeWAV(const char *outName, File &input, AudioFormat compMode);

	void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, AudioFormat compmode);
	void encodeRaw(char *rawData, int length, int samplerate, const char *outname, AudioFormat compmode);
	void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample);
};

/*
 * Stuff which is in compress.cpp
 */
const extern char *tempEncoded;

#endif
