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


enum {
	/* These are the defaults parameters for the Lame invocation */
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
	AUDIO_VORBIS = 1 << 0,
	AUDIO_FLAC   = 1 << 1,
	AUDIO_MP3    = 1 << 2,

	AUDIO_NONE = 0,
	AUDIO_ALL = AUDIO_VORBIS | AUDIO_FLAC | AUDIO_MP3
};

enum CompressionType {
	CBR,
	ABR,
	VBR
};

const char *audio_extensions(AudioFormat format);
int compression_format(AudioFormat format);


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
	// These functions are used by the GUI Tools and by CLI argument parsing functions
	// mp3 settings
	void setMp3LamePath(const std::string&);
	void setMp3CompressionType(const std::string&);
	void setMp3CompressionType(CompressionType);
	void setMp3MpegQuality(const std::string&);
	void setMp3TargetBitrate(const std::string&);
	void setMp3MinBitrate(const std::string&);
	void setMp3MaxBitrate(const std::string&);
	void unsetMp3MinBitrate();
	void unsetMp3MaxBitrate();
	void setMp3VBRQuality(const std::string&);

	// flac
	void setFlacCompressionLevel(const std::string&);
	void setFlacBlockSize(const std::string&);

	// vorbis
	void setOggQuality(const std::string&);
	void setOggMinBitrate(const std::string&);
	void setOggAvgBitrate(const std::string&);
	void setOggMaxBitrate(const std::string&);
	void unsetOggMinBitrate();
	void unsetOggMaxBitrate();


public:
	bool processMp3Parms();
	bool processOggParms();
	bool processFlacParms();

	void setTempFileName();

	void extractAndEncodeVOC(const char *outName, Common::File &input, AudioFormat compMode);
	void extractAndEncodeWAV(const char *outName, Common::File &input, AudioFormat compMode);

	void extractAndEncodeAIFF(const char *inName, const char *outName, AudioFormat compMode);

	void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, AudioFormat compmode);
	void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample);

protected:

	void encodeRaw(const char *rawData, int length, int samplerate, const char *outname, AudioFormat compmode);
};

/*
 * Stuff which is in compress.cpp
 *
 * TODO: What is this? Document it?
 */
const extern char *tempEncoded;

#endif
