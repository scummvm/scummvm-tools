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

#ifndef PRINCE_UTILS_H
#define PRINCE_UTILS_H

#include "common/file.h"
#include "common/str.h"

struct FileData {
	byte *_fileTable;
	uint32 _size;
};

class Decompressor {
public:
	void decompress(byte *source, byte *dest, uint32 destSize);
protected:
	byte *_src, *_dst;
	byte _bitBuffer;
	int _bitsLeft;
	int getBit();
};

class Databank {
	struct FileEntry {
		Common::String _name;
		uint32 _offset;
		uint32 _size;
	};

public:
	Databank(Common::String name);
	~Databank();

	bool isOpen() { return _databank.isOpen(); }

	byte *openDatabank();
	static void decrypt(byte *buffer, uint32 size);

	int getFileIndex(Common::String name);
	FileData loadFile(int fileIndex);
	FileData loadFile(Common::String name);

private:
	Common::Array<FileEntry> _items;
	Common::File _databank;
};

#endif
