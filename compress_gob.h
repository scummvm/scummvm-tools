/* Scumm Tools
 * Copyright (C) 2007 The ScummVM project
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

	Chunk *readChunkConf(File &gobconf, Filename &stkName, uint16 &chunkCount);
	void writeEmptyHeader(File &stk, uint16 chunkCount);
	void writeBody(Filename *inpath, File &stk, Chunk *chunks);
	uint32 writeBodyStoreFile(File &stk, File &src);
	uint32 writeBodyPackFile(File &stk, File &src);
	void rewriteHeader(File &stk, uint16 chunkCount, Chunk *chunks);
	bool filcmp(File &src1, Chunk *compChunk);
	bool checkDico(byte *unpacked, uint32 unpackedIndex, int32 counter, byte *dico, uint16 currIndex, uint16 &pos, uint8 &length);

};

#endif
