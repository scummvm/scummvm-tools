/* Scumm Tools
 * Copyright (C) 2002-2006 The ScummVM project
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

#ifndef EXTRACT_PARALLACTION_H
#define EXTRACT_PARALLACTION_H

#include "tool.h"
#include "util.h"

uint32 depackedlen(byte *packed, uint32 plen);
void ppdepack(byte *packed, byte *depacked, uint32 plen, uint32 unplen);

#define MAX_ARCHIVE_ENTRIES		384

#define ARCHIVE_HEADER_SIZE		22
#define ARCHIVE_FILENAME_LEN		32


struct Archive {
	// Required for printing errors
	Tool &_tool;

	uint32	_numSlots;
	uint32	_numFiles;
	File    _file;
	char	_name[260];

	// file data
	byte*	_fileData;
	uint32	_fileSize;
	uint32	_filePos;

	char 	_names[MAX_ARCHIVE_ENTRIES+1][ARCHIVE_FILENAME_LEN];
	uint32 	_sizes[MAX_ARCHIVE_ENTRIES];
	uint32 	_offsets[MAX_ARCHIVE_ENTRIES];

	void 	open(const char* filename, bool smallArchive);

	void 	openSubfile(uint32 index);
	void 	openSubfile(const char* filename);
	void 	readSubfile(byte* buf, uint32 size);
	uint32  getSizeOfSubfile();

	Archive(Tool &tool);
	~Archive();

	// debug stuff
	void dumpStructs(FILE*);

private:
	int32	findSubfile(const char* filename);
	void	closeSubfile();

	bool	isPackedSubfile(byte* data);
	uint32	getSizeOfPackedSubfile(byte* packedData, uint32 packedSize);
	void	unpackSubfile(byte* packedData, uint32 packedSize);

};

class ExtractParallaction : public Tool {
public:
	ExtractParallaction(const std::string &name = "extract_parallaction");

	virtual void execute();

	bool _small;

protected:
	void parseExtraArguments();
};

#endif
