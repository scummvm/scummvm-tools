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

#include "common/scummsys.h"
#include "common/endian.h"
#include "utils.h"

// John_Doe's implementation
static const uint16 table1[] = {
	0x8000, 0x0002,
	0x4000, 0x0004,
	0x2000, 0x0008,
	0x1000, 0x0010,
	0x0800, 0x0020,
	0x0400, 0x0040,
	0x0200, 0x0080,
	0x0100, 0x0100,
	0x0080, 0x0200,
	0x0040, 0x0400
};

static const uint32 table2[] = {
	0x0000F000,
	0x0020FC00,
	0x00A0FF00,
	0x02A0FF80,
	0x06A0FFC0,
	0x0EA0FFE0,
	0x1EA0FFF0,
	0x3EA0FFF8
};

static const uint16 table3[] = {
	0x8000, 0x0000,
	0x4000, 0x0002,
	0x2000, 0x0006,
	0x1000, 0x000E,
	0x0800, 0x001E,
	0x0400, 0x003E,
	0x0200, 0x007E,
	0x0100, 0x00FE,
	0x0080, 0x01FE,
	0x0040, 0x03FE,
	0x0020, 0x07FE,
	0x0010, 0x0FFE,
	0x0008, 0x1FFE,
	0x0004, 0x3FFE,
	0x0002, 0x7FFE,
	0x0001, 0xFFFE
};

void Decompressor::decompress(byte *source, byte *dest, uint32 destSize) {
	byte *destEnd = dest + destSize;
	int more;
	_src = source;
	_dst = dest;
	_bitBuffer = 0x80;
	while (_dst < destEnd) {
		uint32 ebp;
		uint16 offset, length;
		if (getBit()) {
			if (getBit()) {
				if (getBit()) {
					if (getBit()) {
						if (getBit()) {
							if (getBit()) {
								uint32 tableIndex = 0;
								while (getBit())
									tableIndex++;
								length = table3[tableIndex * 2 + 0];
								do {
									more = !(length & 0x8000);
									length = (length << 1) | getBit();
								} while (more);
								length += table3[tableIndex * 2 + 1];
								length++;
								memcpy(_dst, _src, length);
								_src += length;
								_dst += length;
							}
							*_dst++ = *_src++;
						}
						*_dst++ = *_src++;
					}
					*_dst++ = *_src++;
				}
				*_dst++ = *_src++;
			}
			*_dst++ = *_src++;
		}
		if (!getBit()) {
			if (getBit()) {
				uint32 tableIndex = getBit();
				tableIndex = (tableIndex << 1) | getBit();
				tableIndex = (tableIndex << 1) | getBit();
				ebp = table2[tableIndex];
				length = 1;
			} else {
				ebp = 0x0000FF00;
				length = 0;
			}
		} else {
			uint32 tableIndex = 0;
			while (getBit())
				tableIndex++;
			length = table1[tableIndex * 2 + 0];
			do {
				more = !(length & 0x8000);
				length = (length << 1) | getBit();
			} while (more);
			length += table1[tableIndex * 2 + 1];
			tableIndex = getBit();
			tableIndex = (tableIndex << 1) | getBit();
			tableIndex = (tableIndex << 1) | getBit();
			ebp = table2[tableIndex];
		}
		offset = ebp & 0xFFFF;
		do {
			if (_bitBuffer == 0x80) {
				if (offset >= 0xFF00) {
					offset = (offset << 8) | *_src++;
				}
			}
			more = offset & 0x8000;
			offset = (offset << 1) | getBit();
		} while (more);
		offset += (ebp >> 16);
		length += 2;
		while (length--) {
			if (_dst >= destEnd) {
				return;
			}
			*_dst = *(_dst - offset);
			_dst++;
		}
	}
}

int Decompressor::getBit() {
	int bit = (_bitBuffer & 0x80) >> 7;
	_bitBuffer <<= 1;
	if (_bitBuffer == 0) {
		_bitBuffer = *_src++;
		bit = (_bitBuffer & 0x80) >> 7;
		_bitBuffer <<= 1;
		_bitBuffer |= 1;
	}
	return bit;
}

Databank::Databank(Common::String name) {
	_databank.open(name, "rb");

	if (!_databank.isOpen())
		return;

	_databank.readUint32LE(); // magic
	uint32 fileTableOffset = _databank.readUint32LE() ^ 0x4D4F4B2D; // MOK-
	uint32 fileTableSize = _databank.readUint32LE() ^ 0x534F4654; // SOFT

	_databank.seek(fileTableOffset, SEEK_SET);

	byte *fileTable = (byte *)malloc(fileTableSize);
	byte *fileTableEnd = fileTable + fileTableSize;
	_databank.read_throwsOnError(fileTable, fileTableSize);

	decrypt(fileTable, fileTableSize);

	for (byte *fileItem = fileTable; fileItem < fileTableEnd; fileItem += 32) {
		FileEntry item;
		item._name = (const char *)fileItem;
		item._offset = READ_LE_UINT32(fileItem + 24);
		item._size = READ_LE_UINT32(fileItem + 28);
		_items.push_back(item);
	}

	free(fileTable);
}

Databank::~Databank() {
	_databank.close();
}

void Databank::decrypt(byte *buffer, uint32 size) {
	uint32 key = 0xDEADF00D;
	while (size--) {
		*buffer++ += key & 0xFF;
		key ^= 0x2E84299A;
		key += 0x424C4148;
		key = ((key & 1) << 31) | (key >> 1);
	}
}

int Databank::getFileIndex(Common::String name) {
	int itemIndex = -1;

	if (!_databank.isOpen())
		return itemIndex;

	for (size_t i = 0; i < _items.size(); i++) {
		if (!scumm_stricmp(_items[i]._name.c_str(), name.c_str())) {
			itemIndex = i;
			break;
		}
	}

	return itemIndex;
}

FileData Databank::loadFile(Common::String name) {
	FileData fileData;
	fileData._fileTable = 0;
	fileData._size = 0;

	int index = getFileIndex(name);

	if (index == -1)
		return fileData;

	return loadFile(index);
}

FileData Databank::loadFile(int itemIndex) {
	FileData fileData;
	fileData._fileTable = 0;
	fileData._size = 0;

	if (itemIndex == -1)
		return fileData;

	const FileEntry &entryHeader = _items[itemIndex];

	if (entryHeader._size < 4) {
		return fileData;
	}

	fileData._size = entryHeader._size;

	_databank.seek(entryHeader._offset, SEEK_SET);

	fileData._fileTable = (byte *)malloc(fileData._size);
	_databank.read_throwsOnError(fileData._fileTable, fileData._size);

	if (READ_BE_UINT32(fileData._fileTable) == 0x4D41534D) {
		Decompressor dec;
		uint32 decompLen = READ_BE_UINT32(fileData._fileTable + 14);
		byte *decompData = (byte *)malloc(decompLen);
		dec.decompress(fileData._fileTable + 18, decompData, decompLen);
		free(fileData._fileTable);
		fileData._size = decompLen;
		fileData._fileTable = decompData;
	}

	return fileData;
}
