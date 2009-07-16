/* compress_sword1 - Compress Broken Sword I sound clusters into MP3/Ogg Vorbis
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

#ifndef COMPRESS_SWORD1_H
#define COMPRESS_SWORD1_H

#include "compress.h"


class CompressSword1 : public CompressionTool {
public:
	CompressSword1(const std::string &name = "compress_sword1");
	
	virtual bool inspectInput(const Filename &filename);

	virtual void execute();

protected:
	void parseExtraArguments();

	std::string _audioOuputFilename;
	bool _compSpeech;
	bool _compMusic;

	int16 *uncompressSpeech(File &clu, uint32 idx, uint32 cSize, uint32 *returnSize);
	uint8 *convertData(uint8 *rawData, uint32 rawSize, uint32 *resSize);
	void convertClu(File &clu, File &cl3);
	void compressSpeech(const Filename *inpath, const Filename *outpath);
	void compressMusic(const Filename *inpath, const Filename *outpath);
	void checkFilesExist(bool checkSpeech, bool checkMusic, const Filename *inpath);
};

#endif


