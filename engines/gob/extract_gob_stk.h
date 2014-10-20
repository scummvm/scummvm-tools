/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Extractor for Coktel Vision game's .stk/.itk archives */

#ifndef EXTRACT_GOB_STK_H
#define EXTRACT_GOB_STK_H

#include "tool.h"
class ExtractGobStk : public Tool {
public:
	ExtractGobStk(const std::string &name = "extract_gob_stk");
	~ExtractGobStk();

	virtual void execute();
	
	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:
	struct Chunk;

	Chunk *_chunks;

	void readChunkList(Common::File &stk, Common::File &gobConf);
	void readChunkListV2(Common::File &stk, Common::File &gobConf);
	void extractChunks(Common::Filename &outpath, Common::File &stk);
	byte *unpackData(byte *src, uint32 &size);
	byte *unpackPreGobData(byte *src, uint32 &size, uint32 &compSize);
};

#endif
