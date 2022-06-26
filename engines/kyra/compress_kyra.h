/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* compressor for kyra sound file packages */

#ifndef COMPRESS_KYRA_H
#define COMPRESS_KYRA_H

#include "compress.h"

class CompressKyra : public CompressionTool {
public:
	CompressKyra(const std::string &name = "compress_kyra");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:
	struct DuplicatedFile;

	uint16 clip8BitSample(int16 sample);
	int decodeChunk(Common::File &in, Common::File &out);
	void compressAUDFile(Common::File &input, const char *outfile);
	const DuplicatedFile *findDuplicatedFile(uint32 resOffset, const DuplicatedFile *list, const uint32 maxEntries);
	void process(Common::Filename *infile, Common::Filename *output);
	void processKyra3(Common::Filename *infile, Common::Filename *output);
	bool detectKyra3File(Common::Filename *infile);
};

#endif
