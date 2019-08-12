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

#ifndef EXTRACT_HDB_H
#define EXTRACT_HDB_H

#include "tool.h"
#include "common/array.h"

enum DataType {
	TYPE_ERROR,
	TYPE_BINARY,
	TYPE_TILE32,
	TYPE_FONT,
	TYPE_ICON32,
	TYPE_PIC,

	ENDOFTYPES
};

struct MPCEntry {
	char	filename[64];	// filename
	int32	offset;			// offset in MSD file of data
	int32	length;			// compressed length of data
	int32	ulength;		// uncompressed length
	DataType	type;		// type of data
};

#define MPCIterator Common::Array<MPCEntry *>::iterator

class ExtractHDB : public Tool {
public:
	ExtractHDB(const std::string &name = "extract_hdb");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:
	bool openMPC(Common::Filename &filename);

	Common::File _mpcFile;

	struct {
		uint32 id;
		uint32 dirSize;
	} _dataHeader;

	Common::Array<MPCEntry *> _dir;
	bool _compressed;
};

#endif
