/* extract_gob_stk - Extractor for Coktel Vision game's .stk/.itk archives
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

#include "util.h"
#include "extract_gob_stk.h"

#define confSTK21 "STK21"
#define confSTK10 "STK10"

struct ExtractGobStk::Chunk {
	char name[64];
	uint32 size, offset;
	bool packed;
	bool preGob;

	Chunk *next;

	Chunk() : next(0) { }
	~Chunk() { delete next; }
};

ExtractGobStk::ExtractGobStk(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	_chunks = NULL;
	
	ToolInput input;
	input.format = "*.stk";
	_inputPaths.push_back(input);

	_shorthelp = "Extracts the files from a Stick file used by 'gob' engine (.STK/.ITK/.LTK).";
	_helptext = "\nUsage: " + getName() + " [-o outputname] stickname\nwhere\n  ouputname is used to force the gob config filename (used by compress_gob)\n  stickname is the name of the file to extract/decompress";
}

ExtractGobStk::~ExtractGobStk() {
	delete _chunks;
}

void ExtractGobStk::execute() {
	char signature[7];
	File stk;
	File gobConf;

	File f1;
	File f2;

	Filename inpath(_inputPaths[0].path);

	stk.open(inpath.getFullPath(), "rb");

	if (_outputPath.empty())
		_outputPath = inpath;

	_outputPath.setExtension(".gob");

	gobConf.open(_outputPath.getFullPath(), "w");
	gobConf.printf("%s\n", inpath.getFullName().c_str());

	stk.read(signature, 1, 6);

	if (strncmp(signature, "STK2.1", 6) == 0) {
		print("Signature of new STK format (STK 2.1) detected in file \"%s\"", inpath.getFullPath().c_str());
		gobConf.printf("%s\n", confSTK21);
		readChunkListV2(stk, gobConf);
	} else {
		gobConf.printf("%s\n", confSTK10);
		stk.rewind();
		readChunkList(stk, gobConf);
	}

	extractChunks(_outputPath, stk);
}

void ExtractGobStk::readChunkList(File &stk, File &gobConf) {
	uint16 numDataChunks = stk.readUint16LE();

	// If we are run multiple times, free previous chunk list
	if (_chunks)
		delete _chunks;
	_chunks = new Chunk;
	Chunk *curChunk = _chunks;
	char *fakeTotPtr;

	while (numDataChunks-- > 0) {
		stk.read(curChunk->name, 1, 13);

		curChunk->size = stk.readUint32LE();
		curChunk->offset = stk.readUint32LE();
		curChunk->packed = stk.readByte() != 0;
		curChunk->preGob = false;

		// Geisha TOTs are compressed without having the flag set
		fakeTotPtr = strstr(curChunk->name, "0OT");
		if (fakeTotPtr != 0) {
			strncpy(fakeTotPtr, "TOT", 3);
			curChunk->packed = true;
			curChunk->preGob = true;
		}

		// Write the chunk info in the gob Conf file
		gobConf.printf("%s %d\n", curChunk->name, curChunk->packed ? 1 : 0);

		if (numDataChunks > 0) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
		}
	}
}

void ExtractGobStk::readChunkListV2(File &stk, File &gobConf) {
	uint32 numDataChunks;
	_chunks = new Chunk;
	Chunk *curChunk = _chunks;

//	char *fakeTotPtr;

	int cpt = 0;
	char buffer[64];
	char debugStr[256];
	uint32 filenamePos;
	uint32 miscPos;
	uint32 filePos;
	uint32 compressFlag;
	uint32 decompSize;

	// Header (Signature already read)
	// ======
	// Structure of header is :
	// + 06 bytes : Signature
	// + 14 bytes : Date time of STK/ITK creation (format DDMMYYYYHH24MISS)
	// + 08 bytes : Name / acronym of STK/ITK creator
	// + 04 bytes : Start position of Filenames Section

	stk.read(buffer, 1, 14);

	buffer[14] = '\0';
	sprintf(debugStr, "File generated on %s by ", buffer);

	stk.read(buffer, 1, 8);

	buffer[8] = '\0';
	strcat(debugStr, buffer);
	print("%s",debugStr);
	filenamePos = stk.readUint32LE();

	// Filenames - Header
	// ==================
	// Structure of the header of Filename section is :
	// + 04 bytes : Number of files stored in STK/ITK
	// + 04 bytes : Start position of Misc Section

	stk.seek(filenamePos, SEEK_SET);

	numDataChunks = stk.readUint32LE();
	miscPos = stk.readUint32LE();

	if (numDataChunks == 0)
		throw ToolException("Empty ITK/STK !");

	while (numDataChunks-- > 0) {
		// Misc
		// ====
		// This section contains Misc infos concerning the files.
		// For each file, the info is the following :
		// + 04 bytes : Start position of the filename
		// + 14 bytes : Date time of the file last modification (format DDMMYYYYHH24MISS)
		// + 14 bytes : Date time of the file creation (format DDMMYYYYHH24MISS)
		// + 08 bytes : Name / acronym of STK/ITK creator
		// + 04 bytes : File section size
		// + 04 bytes : Uncompressed file size (redondant with info in File Section)
		// TODO : Understand the use of the unknown bytes !
		// + 05 bytes : Unknown
		// + 04 bytes : Start position of the File Section
		// + 04 bytes : Compression flag (AFAIK : 0= uncompressed, 1= compressed)

		stk.seek(miscPos + (cpt * 61), SEEK_SET);
		filenamePos = stk.readUint32LE();

		stk.read(buffer, 1, 36);
		curChunk->size = stk.readUint32LE();
		decompSize = stk.readUint32LE();

		stk.read(buffer, 1, 5);

		filePos = stk.readUint32LE();
		compressFlag = stk.readUint32LE();

		if (compressFlag == 1) {
			curChunk->packed = true;
		} else {
			if ((curChunk->size != decompSize) | (compressFlag != 0)) {
				sprintf(debugStr,
						"Unexpected value in compress flag : %d - Size : %d Uncompressed size : %d",
						compressFlag, curChunk->size, decompSize);
				throw ToolException(debugStr);
			} else {
				curChunk->packed=false;
			}
		}

		// Filenames
		// =========
		// Filename are stored one after the other, separated by 0x00.
		// Those are now long filenames, at the opposite of previous STK version.

		stk.seek(filenamePos, SEEK_SET);

		strcpy(curChunk->name, stk.readString().c_str());

		// Files
		// =====
		// The structure of the file section if the following :
		// + 04 bytes : Uncompressed size (redondant with the one in Misc info)
		// + ?? bytes : Compressed data

		curChunk->offset = filePos;
		curChunk->preGob = false;

		// Write the chunk info in the gob Conf file
		gobConf.printf("%s %d\n", curChunk->name, curChunk->packed ? 1 : 0);

		if (numDataChunks > 0) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
		}
		cpt++;
	}
}

void ExtractGobStk::extractChunks(Filename &outpath, File &stk) {
	Chunk *curChunk = _chunks;
	byte *unpackedData = NULL;

	while (curChunk != 0) {
		print("Extracting \"%s\"", curChunk->name);

		outpath.setFullName(curChunk->name);
		File chunkFile(outpath, "wb");

		if (curChunk->size > 0) {
			stk.seek(curChunk->offset, SEEK_SET);

			byte *data = new byte[curChunk->size];

			stk.read(data, curChunk->size, 1);

			try {
				if (curChunk->packed) {
					uint32 realSize;

					if (curChunk->preGob) {
						unpackedData = unpackPreGobData(data, realSize, curChunk->size);
					} else {
						unpackedData = unpackData(data, realSize);
					}

					chunkFile.write(unpackedData, realSize, 1);

					delete[] unpackedData;
				} else {
					chunkFile.write(data, curChunk->size, 1);
				}
			} catch(...) {
				delete[] data;
				delete[] unpackedData;
				throw;
			}
			delete[] data;
		}

		curChunk = curChunk->next;
	}
}

// Some LZ77-variant
byte *ExtractGobStk::unpackData(byte *src, uint32 &size) {
	uint32 counter;
	uint16 cmd;
	byte tmpBuf[4114];
	int16 off;
	byte len;
	uint16 tmpIndex;

	counter = size = READ_LE_UINT32(src);

	for (int i = 0; i < 4078; i++)
		tmpBuf[i] = 0x20;
	tmpIndex = 4078;

	src += 4;

	byte *unpacked = new byte[size];
	byte *dest = unpacked;

	cmd = 0;
	while (1) {
		cmd >>= 1;
		if ((cmd & 0x0100) == 0) {
			cmd = *src | 0xFF00;
			src++;
		}
		if ((cmd & 1) != 0) { /* copy */
			*dest++ = *src;
			tmpBuf[tmpIndex] = *src;
			src++;
			tmpIndex++;
			tmpIndex %= 4096;
			counter--;

			if (counter == 0)
				break;
		} else { /* copy string */
			off = *src++;
			off |= (*src & 0xF0) << 4;
			len = (*src & 0x0F) + 3;
			src++;

			for (int i = 0; i < len; i++) {
				*dest++ = tmpBuf[(off + i) % 4096];
				if (--counter == 0)
					return unpacked;

				tmpBuf[tmpIndex] = tmpBuf[(off + i) % 4096];
				tmpIndex++;
				tmpIndex %= 4096;
			}
		}
	}

	return unpacked;
}

// Some LZ77-variant
byte *ExtractGobStk::unpackPreGobData(byte *src, uint32 &size, uint32 &compSize) {
	uint16 cmd;
	byte tmpBuf[4114];
	int16 off;
	byte len;
	uint16 tmpIndex;
	uint32 dummy1;
	int32 newCounter;

	newCounter = compSize;
	size = 0;

	dummy1 = READ_LE_UINT16(src);
	src += 2;
	newCounter -= 2;

//  The 6 first bytes are grouped by 2 :
//  - bytes 0&1 : if set to 0xFFFF, the real size is in bytes 2&3. Else : unknown 
//  - bytes 2&3 : Either the real size or 0x007D. Directly related to the size of the file.
//  - bytes 4&5 : 0x0000 (files are small) ;)
	if (dummy1 == 0xFFFF)
		print("Real size %d", READ_LE_UINT32(src));
	else
		print("Unknown real size %xX %xX", dummy1>>8, dummy1 & 0x00FF);

//	counter = size = READ_LE_UINT32(src);

	for (int i = 0; i < 4078; i++)
		tmpBuf[i] = 0x20;
	tmpIndex = 4078;

	src += 4;
	newCounter -= 4;

	byte *unpacked = new byte[500000];//[size] Replaced by dummy as real size is not always known;
	byte *dest = unpacked;

	cmd = 0;
	while (1) {
		cmd >>= 1;
		if ((cmd & 0x0100) == 0) {
			cmd = *src | 0xFF00;
			src++;
			newCounter--;
			if (newCounter == 0)
				break;
		}

		if ((cmd & 1) != 0) { /* copy */
			*dest++ = *src;
			size++;
			tmpBuf[tmpIndex] = *src;
			src++;
			newCounter--;

			if (newCounter == 0)
				break;

			tmpIndex++;
			tmpIndex %= 4096;
		} else { /* copy string */
			off = *src++;
			off |= (*src & 0xF0) << 4;
			len = (*src & 0x0F) + 3;
			src++;
			newCounter -= 2;

			for (int i = 0; i < len; i++) {
				*dest++ = tmpBuf[(off + i) % 4096];
				size++;
				tmpBuf[tmpIndex] = tmpBuf[(off + i) % 4096];
				tmpIndex++;
				tmpIndex %= 4096;
			}
			if (newCounter <= 0)
				break;
		}
	}

	return unpacked;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractGobStk gob_stk(argv[0]);
	return gob_stk.run(argc, argv);
}
#endif

