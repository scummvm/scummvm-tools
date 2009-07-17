/* extract_gob_stk.h - Extractor for Coktel Vision game's .stk/.itk archives
 * Copyright (C) 2009 The ScummVM project
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
 * $URL
 * $Id
 *
 */

#ifndef EXTRACT_GOB_STK_H
#define EXTRACT_GOB_STK_H

#include "tool.h"
class ExtractGobStk : public Tool {
public:
	ExtractGobStk(const std::string &name = "extract_gob_stk");
	~ExtractGobStk();

	virtual void execute();

protected:
	struct Chunk;

	Chunk *_chunks;

	void readChunkList(File &stk, File &gobConf);
	void readChunkListV2(File &stk, File &gobConf);
	void extractChunks(Filename &outpath, File &stk);
	byte *unpackData(byte *src, uint32 &size);
	byte *unpackPreGobData(byte *src, uint32 &size, uint32 &compSize);
};

#endif
