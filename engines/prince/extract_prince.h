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

#ifndef EXTRACT_PRINCE_H
#define EXTRACT_PRINCE_H

#include "tool.h"
#include "common/array.h"

class ExtractPrince : public Tool {
public:
	ExtractPrince(const std::string &name = "extract_prince");
	
	virtual void execute();
	
	virtual InspectionMatch inspectInput(const Common::Filename &filename);
	
protected:
	struct FileEntry {
		std::string _name;
		uint32 _offset;
		uint32 _size;
	};

	struct FileData {
		byte *_fileTable;
		uint32 _size;
	};

	byte *openDatabank();
	static void decrypt(byte *buffer, uint32 size);

	FileData loadFile(int itemIndex);
	FileData loadFile(const Common::String &fileName);
	char correctPolishLetter(char c);

	void exportMobs(FileData fileData);
	void exportVariaTxt(FileData fileData);
	void exportInvTxt(FileData fileData);
	void exportCredits(FileData fileData);
	void exportTalkTxt(FileData fileData);
	byte *talkTxtWithDialog(byte *talkTxt);
	byte *talkTxtNoDialog(byte *talkTxt);
	bool printSpecialDialogData(byte c);

	Common::Array<FileEntry> _items;
	Common::File _databank, _fFiles;
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

#endif
