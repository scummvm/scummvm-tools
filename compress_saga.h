/* compress_saga - Compress SAGA engine digital sound files into
 * MP3 and Ogg Vorbis format
 * Copyright (C) 2004, Marcoen Hirschberg
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

#ifndef COMPRESS_SAGA_H
#define COMPRESS_SAGA_H

#include "compress.h"

enum SAGAGameSoundTypes {
	kSoundPCM = 0,
	kSoundVOX = 1,
	kSoundVOC = 2,
	kSoundWAV = 3,
	kSoundMacPCM = 4
};

enum SAGAGameType {
	GType_ITE = 0,
	GType_IHNM = 1
};

class CompressSaga : public CompressionTool {
public:
	CompressSaga(const std::string &name = "compress_saga");

	virtual void execute();
	
	virtual InspectionMatch inspectInput(const Filename &filename);

	// Declarations should be inside the class to prevent linker errors

	struct GameFileDescription {
		const char *fileName;
		bool swapEndian;
		const char *md5;
		SAGAGameSoundTypes resourceType;
		long frequency;
		bool stereo;
	};

	struct GameDescription {
		SAGAGameType gameType;
		int filesCount;
		GameFileDescription *filesDescriptions;
	};

protected:

	GameDescription *_currentGameDescription;
	GameFileDescription *_currentFileDescription;

	uint16 _sampleRate;
	uint32 _sampleSize;
	uint8 _sampleBits;
	uint8 _sampleStereo;

	bool detectFile(Filename *infile);
	uint32 copyFile(const char *fromFileName, File &outputFile);
	void copyFile(File &inputFile, uint32 inputSize, const char *toFileName);
	void writeBufferToFile(uint8 *data, uint32 inputSize, const char *toFileName);
	void writeHeader(File &outputFile);
	uint32 encodeEntry(File &inputFile, uint32 inputSize, File &outputFile);
	void sagaEncode(Filename *inpath, Filename *outpath);
};

#endif
