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

#ifndef COMPRESS_GOB_H
#define COMPRESS_GOB_H

#include "compress.h"

#define confSTK21 "STK21"
#define confSTK10 "STK10"

enum {
	MODE_NORMAL = 0,
	MODE_HELP   = 1,
	MODE_FORCE  = 2,
	MODE_SET    = 4
};

class CompressGob : public CompressionTool {
public:
	CompressGob(const std::string &name = "compress_gob");
	~CompressGob();

	virtual void execute();

protected:
	struct Chunk;

	uint8 _execMode;
	Chunk *_chunks;

	void parseExtraArguments();

	Chunk *readChunkConf(Common::File &gobconf, Common::Filename &stkName, uint16 &chunkCount);
	void writeEmptyHeader(Common::File &stk, uint16 chunkCount);
	void writeBody(Common::Filename *inpath, Common::File &stk, Chunk *chunks);
	uint32 writeBodyStoreFile(Common::File &stk, Common::File &src);
	uint32 writeBodyPackFile(Common::File &stk, Common::File &src);
	void rewriteHeader(Common::File &stk, uint16 chunkCount, Chunk *chunks);
	bool filcmp(Common::File &src1, Common::Filename &stkName);
	bool checkDico(byte *unpacked, uint32 unpackedIndex, int32 counter, byte *dico, uint16 currIndex, uint16 &pos, uint8 &length);

};

#endif
