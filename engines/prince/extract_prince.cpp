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

#include <string.h>
#include "extract_prince.h"
#include "common\endian.h"

struct FileData;

ExtractPrince::ExtractPrince(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "/";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract The Prince and the Coward text data for translation.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] <inputfile>\n";
}

void ExtractPrince::execute() {
	const int kNumberOfLocations = 61;

	Common::Filename mainDir = _inputPaths[0].path;
	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty()) {
		_outputPath.setFullPath("./");
	}

	char *pathBuffer = (char *)malloc(17 * sizeof(char));
	printf("Unpacking The Prince and the Coward text data... \n");

	std::string fullName = mainDir.getFullPath();
	sprintf(pathBuffer, "all/databank.ptc");
	fullName += pathBuffer;
	Common::Filename filename(fullName);
	_databank.open(filename, "rb");
	if (!_databank.isOpen()) {
		_fFiles.close();
		error("Unable to open all/databank.ptc");
	}
	byte *fileTable = openDatabank();
	for (size_t i = 0; i < _items.size(); i++) {
		if (!scumm_stricmp(_items[i]._name.c_str(), "variatxt.dat")) {
			exportVariaTxt(loadFile(i));
		}
		if (!scumm_stricmp(_items[i]._name.c_str(), "invtxt.dat")) {
			exportInvTxt(loadFile(i));
		}
		if (!scumm_stricmp(_items[i]._name.c_str(), "talktxt.dat")) {
			exportTalkTxt(loadFile(i));
		}
	}
	free(fileTable);
	_databank.close();
	_items.clear();

	_outputPath.setFullName("mob.txt");
	_fFiles.open(_outputPath, "w");
	if (!_fFiles.isOpen()) {
		error("Unable to create mob.txt");
	}
	_fFiles.print("mob.lst\nmob_name - exam text\n");
	for (int loc = 1; loc <= kNumberOfLocations; loc++) {
		if (loc != 44 && loc != 45) {
			_fFiles.print("\nLocation %d\n", loc);
			std::string fullName = mainDir.getFullPath();
			sprintf(pathBuffer, "%02d/databank.ptc", loc);
			fullName += pathBuffer;
			Common::Filename filename(fullName);
			_databank.open(filename, "rb");
			if (!_databank.isOpen()) {
				_fFiles.close();
				error("Unable to open %02d/databank.ptc", loc);
			}
			byte *fileTable = openDatabank();
			for (size_t i = 0; i < _items.size(); i++) {
				if (!scumm_stricmp(_items[i]._name.c_str(), "mob.lst")) {
					exportMobs(loadFile(i));
				}
			}
			free(fileTable);
			_databank.close();
			_items.clear();
		}
	}
	printf("mob.txt - done\n");
	printf("All done!\n");
	free(pathBuffer);
	_fFiles.close();
}

InspectionMatch ExtractPrince::inspectInput(const Common::Filename &filename) {
	std::string file = filename.getFullName();
	if (scumm_stricmp(file.c_str(), "databank.ptc") == 0) {
		return IMATCH_PERFECT;
	}
	return IMATCH_AWFUL;
}

byte *ExtractPrince::openDatabank() {
	_databank.readUint32LE(); // magic
	uint32 fileTableOffset = _databank.readUint32LE() ^ 0x4D4F4B2D; // MOK-
	uint32 fileTableSize = _databank.readUint32LE() ^ 0x534F4654; // SOFT

	//printf("fileTableOffset : %08X\n", fileTableOffset);
	//printf("fileTableSize: %08X\n", fileTableSize);

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
		//printf("%12s %8X %d\n", (const char *)fileItem, item._offset, item._size);
		_items.push_back(item);
	}
	return fileTable;
}

void ExtractPrince::decrypt(byte *buffer, uint32 size) {
	uint32 key = 0xDEADF00D;
	while (size--) {
		*buffer++ += key & 0xFF;
		key ^= 0x2E84299A;
		key += 0x424C4148;
		key = ((key & 1) << 31) | (key >> 1);
	}
}

ExtractPrince::FileData ExtractPrince::loadFile(int itemIndex) {
	FileData fileData;
	fileData._fileTable = nullptr;
	fileData._size = 0;

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

// TODO - export offset/id of mob?
void ExtractPrince::exportMobs(FileData fileData) {
	if (fileData._fileTable != nullptr) {
		const int kMobsStructSize = 32;
		const int kMobsTextOffsetsPos = 24;
		int streamPos = 0;

		while (READ_LE_UINT16(fileData._fileTable + streamPos) != 0xFFFF) {
			byte *mobTextOffset = fileData._fileTable + streamPos + kMobsTextOffsetsPos;

			uint32 nameOffset = READ_LE_UINT32(mobTextOffset);
			mobTextOffset += 4;
			uint32 examTextOffset = READ_LE_UINT32(mobTextOffset);

			std::string mobName, mobExamText;
			byte c;

			byte *namePointer = fileData._fileTable + nameOffset;
			mobName.clear();
			while ((c = *namePointer)) {
				c = correctPolishLetter(c);
				mobName += c;
				namePointer++;
			}

			byte *examPointer = fileData._fileTable + examTextOffset;
			mobExamText.clear();
			c = *examPointer;
			examPointer++;
			if (c) {
				// TODO - add this in packing tool
				//mobExamText += c;
				do {
					c = *examPointer;
					c = correctPolishLetter(c);
					if (c == 10) {
						c = '|';
					}
					mobExamText += c;
					examPointer++;
				} while (c != 255);
			}
			_fFiles.print("%s - %s\n", mobName.c_str(), mobExamText.c_str());
			streamPos += kMobsStructSize;
		}
		free(fileData._fileTable);
	}
}

void ExtractPrince::exportVariaTxt(FileData fileData) {
	if (fileData._fileTable != nullptr) {
		_outputPath.setFullName("variatxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create variatxt.txt");
		}
		_fFiles.print("variatxt.dat\nstringId. string\n");
		const int variaTxtSize = 6000;
		for (int stringId = 0; stringId < variaTxtSize; stringId++) {
			uint32 stringOffset = READ_LE_UINT32(fileData._fileTable + stringId * 4);
			if (stringOffset > fileData._size) {
				assert(false);
			}
			std::string variaTxtString;
			byte c;
			byte *txtPointer = fileData._fileTable + stringOffset;
			variaTxtString.clear();
			txtPointer++; // TODO -  remove this in packing tool
			while ((c = *txtPointer)) {
				c = correctPolishLetter(c);
				variaTxtString += c;
				txtPointer++;
			}
			if (variaTxtString.size()) {
				_fFiles.print("%d. %s\n", stringId, variaTxtString.c_str());
			}
		}
		free(fileData._fileTable);
		_fFiles.close();
		printf("variatxt.txt - done\n");
	}
}

void ExtractPrince::exportInvTxt(FileData fileData) {
	if (fileData._fileTable != nullptr) {
		std::string itemName, itemExamText;
		const int kItems = 100;
		_outputPath.setFullName("invtxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create invtxt.txt");
		}
		_fFiles.print("invtxt.dat\nitemNr. name - exam text\n");
		for (int itemNr = 0; itemNr < kItems; itemNr++) {
			int txtOffset = READ_LE_UINT32(fileData._fileTable + itemNr * 8);
			int examTxtOffset = READ_LE_UINT32(fileData._fileTable + itemNr * 8 + 4);

			byte c;
			byte *nameTxt = fileData._fileTable + txtOffset;
			itemName.clear();
			while ((c = *nameTxt)) {
				c = correctPolishLetter(c);
				itemName += c;
				nameTxt++;
			}

			byte *examTxt = fileData._fileTable + examTxtOffset;
			itemExamText.clear();
			while ((c = *examTxt)) {
				c = correctPolishLetter(c);
				if (c == 10) {
					c = '|';
				}
				itemExamText += c;
				examTxt++;
			}

			if (itemName.size() != 2) {
				_fFiles.print("%d. %s - %s\n", itemNr, itemName.c_str(), itemExamText.c_str());
			}
		}
		free(fileData._fileTable);
		_fFiles.close();
		printf("invtxt.txt - done\n");
	}
}

void ExtractPrince::exportTalkTxt(FileData fileData) {
	if (fileData._fileTable != nullptr) {
		_outputPath.setFullName("talktxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create talktxt.txt");
		}
		_fFiles.print("talktxt.dat\n");
		const int textOffset = 8000;
		byte *talkTxt = fileData._fileTable + textOffset;
		uint32 pos = 0;
		int size;
		while (pos + textOffset < fileData._size) {
			if (*talkTxt == 0xFF) {
				size = talkTxtWithDialog(talkTxt);
				talkTxt += size;
				pos += size;
			} else {
				size = talkTxtNoDialog(talkTxt);
				talkTxt += size;
				pos += size;
			}
		}
		free(fileData._fileTable);
		_fFiles.close();
		printf("talktxt.txt - done\n");
	}
}

int ExtractPrince::talkTxtWithDialog(byte *talkTxt) {
	byte *mainString;
	byte *dialogBoxAddr[32];
	byte *dialogOptAddr[32];
	byte c;
	std::string lineString;
	int size = 0;

	byte *stringCurrOff = talkTxt;
	stringCurrOff++;
	size++;
	int32 adressOfFirstSequence = (int)READ_LE_UINT16(stringCurrOff);
	mainString = talkTxt + adressOfFirstSequence;
	stringCurrOff += 2;
	size += 2;

	for (int i = 0; i < 32; i++) {
		dialogBoxAddr[i] = 0;
		dialogOptAddr[i] = 0;
	}

	int16 off;
	byte *line = nullptr;

	int dialogBox = 0;
	while ((off = (int)READ_LE_UINT16(stringCurrOff)) != -1) {
		stringCurrOff += 2;
		size += 2;
		if (off) {
			line = talkTxt + off;
		}
		dialogBoxAddr[dialogBox] = line;
		dialogBox++;
	}
	stringCurrOff += 2;
	size += 2;

	int dialogOpt = 0;
	while ((off = (int)READ_LE_UINT16(stringCurrOff)) != -1) {
		stringCurrOff += 2;
		size += 2;
		if (off) {
			line = talkTxt + off;
		}
		dialogOptAddr[dialogOpt] = line;
		dialogOpt++;
	}
	size += 2;

	_fFiles.print("@DIALOGBOX_LINES:\n");
	while ((c = *mainString) != 255) {
		if (printSpecialDialogData(c)) {
			mainString++;
			size++;
			_fFiles.print("%c\n", (*mainString) + 48);
		}
		mainString++;
		size++;
	}
	size++;
	_fFiles.print("#END\n");
			
	for (int i = 0; i < dialogBox; i++) {
		_fFiles.print("@DIALOG_BOX %d\n", i);
		while ((c = *dialogBoxAddr[i]) != 255) {
			_fFiles.print("$%d\n", *dialogBoxAddr[i]);
			dialogBoxAddr[i]++;
			size++;
			lineString.clear();
			while ((c = *dialogBoxAddr[i])) {
				c = correctPolishLetter(c);
				if (c == 10) {
					c = '|';
				}
				dialogBoxAddr[i]++;
				size++;
				lineString += c;
			}
			dialogBoxAddr[i]++;
			size++;
			_fFiles.print("%s\n", lineString.c_str());
		}
		size++;
		_fFiles.print("#END\n");
	}

	for (int i = 0; i < dialogOpt; i++) {
		_fFiles.print("@DIALOG_OPT %d\n", i);
		while ((c = *dialogOptAddr[i]) != 255) {
			if (printSpecialDialogData(c)) {
				dialogOptAddr[i]++;
				size++;
				_fFiles.print("%c\n", (*dialogOptAddr[i]) + 48);
			}
			dialogOptAddr[i]++;
			size++;
		}
		size++;
		_fFiles.print("#END\n");
	}
	_fFiles.print("#ENDEND\n");
	return size;
}

int ExtractPrince::talkTxtNoDialog(byte *talkTxt) {
	byte c;
	int size = 0;
	_fFiles.print("@NORMAL_LINES:\n");
	while ((c = *talkTxt) != 255) {
		if (printSpecialDialogData(c)) {
			talkTxt++;
			size++;
			_fFiles.print("%c\n", (*talkTxt) + 48);
		}
		talkTxt++;
		size++;
	}
	size++;
	_fFiles.print("#END\n");
	return size;
}

// Returns 'true' if next char is a value for 'enable option', 
// 'disable option', 'show dialog box", 'flag code' or 'exit code'
bool ExtractPrince::printSpecialDialogData(byte c) {
	switch (c) {
	case 0:
		_fFiles.print("\n");
		return false;
	case 1:
		_fFiles.print("#HERO\n");
		return false;
	case 4:
		_fFiles.print("#OTHER\n");
		return false;
	case 5:
		_fFiles.print("#OTHER2\n");
		return false;
	case 10:
		_fFiles.print("|");
		return false;
	case 254:
		_fFiles.print("#PAUSE\n");
		return false;
	case 240:
		_fFiles.print("#ENABLE ");
		return true;
	case 241:
		_fFiles.print("#DISABLE ");
		return true;
	case 242:
		_fFiles.print("#BOX ");
		return true;
	case 243:
		_fFiles.print("#EXIT ");
		return true;
	case 244:
		_fFiles.print("#FLAG ");
		return true;
	default:
		c = correctPolishLetter(c);
		_fFiles.print("%c", c);
		return false;
	}
}

// Conversion from Mazovia to Windows-1250
char ExtractPrince::correctPolishLetter(char c) {
	switch (c) {
	case '\x86':
		return '\xB9';
	case '\x92':
		return '\xB3';
	case '\x9E':
		return '\x9C';
	case '\x8D':
		return '\xE6';
	case '\xA4':
		return '\xF1';
	case '\xA6':
		return '\x9F';
	case '\x91':
		return '\xEA';
	case '\xA2':
		return '\xF3';
	case '\xA7':
		return '\xBF';
	case '\x8F':
		return '\xA5';
	case '\x9C':
		return '\xA3';
	case '\x98':
		return '\x8C';
	case '\x95':
		return '\xC6';
	case '\xA5':
		return '\xD1';
	case '\xA0':
		return '\x8F';
	case '\x90':
		return '\xCA';
	case '\xA3':
		return '\xD3';
	case '\xA1':
		return '\xAF';
	default:
		return c;
	}
}

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

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif
