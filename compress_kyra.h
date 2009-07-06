/* compress_kyra_bun - compressor for kyra sound file packages
 * Copyright (C) 2006  The ScummVM Team
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

#ifndef COMPRESS_KYRA_H
#define COMPRESS_KYRA_H

#include "compress.h"

class CompressKyra : public CompressionTool {
public:
	CompressKyra(const std::string &name = "compress_kyra");

	virtual void execute();

protected:
	struct DuplicatedFile;

	uint16 clip8BitSample(int16 sample);
	int decodeChunk(File &in, File &out);
	void compressAUDFile(File &input, const char *outfile);
	const DuplicatedFile *findDuplicatedFile(uint32 resOffset, const DuplicatedFile *list, const uint32 maxEntries);
	void process(Filename *infile, Filename *output);
	void processKyra3(Filename *infile, Filename *output);
	bool detectKyra3File(Filename *infile);
};

#endif
