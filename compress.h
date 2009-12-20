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

enum {
	/* These are the defaults parameters for the Lame invocation */
	minBitrDef	= 24,
	maxBitrDef	= 64,
	algqualDef	= 2,
	vbrqualDef	= 4,

	/* The default for oggenc invocation is to use the --quality option only */
	oggqualDef	= 3,

	/* These are the default parameters for the FLAC invocation */
	flacCompressDef		= 8,
	flacBlocksizeDef	= 1152
};

#define TEMP_WAV	"tempfile.wav"
#define TEMP_RAW	"tempfile.raw"
#define TEMP_MP3	"tempfile.mp3"
#define TEMP_OGG	"tempfile.ogg"
#define TEMP_FLAC	"tempfile.fla"



/** 
 * Different audio formats.
 * You can bitwise them to represent several formats.
 */
enum AudioFormat {
	AUDIO_NONE = 0,
	AUDIO_VORBIS = 1,
	AUDIO_FLAC = 2,
	AUDIO_MP3 = 4,
	AUDIO_ALL = AUDIO_VORBIS | AUDIO_FLAC | AUDIO_MP3
};

/**
 * Another enum, which cannot be ORed.
 * These are the values written to the output files.
 */
enum CompressionFormat {
	COMPRESSION_NONE = 0,
	COMPRESSION_MP3 = 1,
	COMPRESSION_OGG = 2,
	COMPRESSION_FLAC = 3
};

const char *audio_extensions(AudioFormat format);
CompressionFormat compression_format(AudioFormat format);


/**
 * A tool, which can compress to either MP3, Vorbis or FLAC formats.
 */
class CompressionTool : public Tool {
public:
	CompressionTool(const std::string &name, ToolType type);

	virtual std::string getHelp() const;

	void parseAudioArguments();

public:
	// FIXME: These vars should not be public, but the ToolGUI currently
	// accesses them directly. We should fix this.

	/** Formats supported by this tool. */
	AudioFormat _supportedFormats;

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

	void setTempFileName();

	void extractAndEncodeVOC(const char *outName, Common::File &input, AudioFormat compMode);
	void extractAndEncodeWAV(const char *outName, Common::File &input, AudioFormat compMode);

	void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, AudioFormat compmode);
	void encodeRaw(char *rawData, int length, int samplerate, const char *outname, AudioFormat compmode);
	void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample);
};

/*
 * Stuff which is in compress.cpp
 *
 * TODO: What is this? Document it?
 */
const extern char *tempEncoded;

#endif
