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
#define confSTK21 "STK21"
#define confSTK10 "STK10"

struct Chunk {
	char name[64];
	uint32 size, offset;
	bool packed;
	bool preGob;

	Chunk *next;

	Chunk() : next(0) { }
	~Chunk() { delete next; }
};

void extractError(FILE *f1, FILE *f2, Chunk *chunks, const char *msg);
Chunk *readChunkList(FILE *stk, FILE *gobConf);
Chunk *readChunkListV2(FILE *stk, FILE *gobConf);
void extractChunks(Filename *outpath, FILE *stk, Chunk *chunks);
byte *unpackData(byte *src, uint32 &size);
byte *unpackPreGobData(byte *src, uint32 &size, uint32 &compSize);

int main(int argc, char **argv) {
	char signature[7];
	Chunk *chunks;
	FILE *stk;
	FILE *gobConf;

	int first_arg = 1;
	int last_arg = argc - 1;

	Filename inpath, outpath;

	// Check if we should display some heplful text
	parseHelpArguments(argv, argc);
	
	// Continuing with finding out output directory
	// also make sure we skip those arguments
	if (parseOutputArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else
		// Standard output dir
		outpath.setFullPath("out/");

	// We only got one input file
	if (last_arg == first_arg)
		error("Only one input file expected!\n");

	inpath.setFullPath(argv[first_arg]);

	if (!(stk = fopen(inpath.getFullPath(), "rb")))
		error("Couldn't open file \"%s\"", inpath.getFullPath());

	if (inpath.empty())
		outpath = inpath;
	outpath.addExtension(".gob");

	if (!(gobConf = fopen(outpath.getFullPath(), "w")))
		error("Couldn't create config file \"%s\"", outpath.getFullPath());

	if (fread(signature, 1, 6, stk) < 6)
		error("Unexpected EOF while reading signature in \"%s\"", argv[1]);

	if (strncmp(signature, "STK2.1", 6) == 0) {
		warning("Signature of new STK format (STK 2.1) detected in file \"%s\"", argv[1]);
		fprintf(gobConf, "%s\n", confSTK21);
		chunks = readChunkListV2(stk, gobConf);
	} else {
		fprintf(gobConf, "%s\n", confSTK10);
		rewind(stk);
		chunks = readChunkList(stk, gobConf);
	}

	fclose(gobConf);

	extractChunks(&outpath, stk, chunks);

	delete chunks;
	fclose(stk);

	return 0;
}

void extractError(FILE *f1, FILE *f2, Chunk *chunks, const char *msg) {
	if (f1)
		fclose(f1);
	if (f2)
		fclose(f2);
	delete chunks;

	error(msg);
}

Chunk *readChunkList(FILE *stk, FILE *gobConf) {
	uint16 numDataChunks = readUint16LE(stk);
	Chunk *chunks = new Chunk;
	Chunk *curChunk = chunks;
	char *fakeTotPtr;

	while (numDataChunks-- > 0) {
		if (fread(curChunk->name, 1, 13, stk) < 13)
			extractError(stk, gobConf, chunks, "Unexpected EOF");

		curChunk->size = readUint32LE(stk);
		curChunk->offset = readUint32LE(stk);
		curChunk->packed = readByte(stk) != 0;
		curChunk->preGob = false;

		// Geisha TOTs are compressed without having the flag set
		fakeTotPtr = strstr(curChunk->name, "0OT");
		if (fakeTotPtr != 0) {
			strncpy(fakeTotPtr, "TOT", 3);
			curChunk->packed = true;
			curChunk->preGob = true;
		}

		// Write the chunk info in the gob Conf file
		fprintf(gobConf, "%s %d\n", curChunk->name, curChunk->packed ? 1 : 0);

		if (numDataChunks > 0) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
		}
	}

	return chunks;
}

Chunk *readChunkListV2(FILE *stk, FILE *gobConf) {
	uint32 numDataChunks;
	Chunk *chunks = new Chunk;
	Chunk *curChunk = chunks;

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

	if (fread(buffer, 1, 14, stk) < 14)
		extractError(stk, gobConf, chunks, "Unexpected EOF");

	buffer[14] = '\0';
	sprintf(debugStr, "File generated on %s by ", buffer);

	if (fread(buffer, 1, 8, stk) < 8)
		extractError(stk, gobConf, chunks, "Unexpected EOF");

	buffer[8] = '\0';
	strcat(debugStr, buffer);
	printf("%s\n",debugStr);
	filenamePos = readUint32LE(stk);

	// Filenames - Header
	// ==================
	// Structure of the header of Filename section is :
	// + 04 bytes : Number of files stored in STK/ITK
	// + 04 bytes : Start position of Misc Section

	if (fseek(stk, filenamePos, SEEK_SET) != 0)
		extractError(stk, gobConf, chunks, "Unable to locate Filename Section");

	numDataChunks = readUint32LE(stk);
	miscPos = readUint32LE(stk);

	if (numDataChunks == 0)
		extractError(stk, gobConf, chunks, "Empty ITK/STK !");

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

		if (fseek(stk, miscPos + (cpt * 61), SEEK_SET) != 0)
			extractError(stk, gobConf, chunks, "Unable to locate Misc Section");
		filenamePos = readUint32LE(stk);

		if (fread(buffer, 1, 36, stk) < 36)
			extractError(stk, gobConf, chunks, "Unexpected EOF in Misc Section");
		curChunk->size = readUint32LE(stk);
		decompSize = readUint32LE(stk);

		if (fread(buffer, 1, 5, stk) < 5)
			extractError(stk, gobConf, chunks, "Unexpected EOF in Misc Section");

		filePos = readUint32LE(stk);
		compressFlag = readUint32LE(stk);

		if (compressFlag == 1) {
			curChunk->packed = true;
		} else {
			if ((curChunk->size != decompSize) | (compressFlag != 0)) {
				sprintf(debugStr,
						"Unexpected value in compress flag : %d - Size : %d Uncompressed size : %d",
						compressFlag, curChunk->size, decompSize);
				extractError(stk, gobConf, chunks, debugStr);
			} else {
				curChunk->packed=false;
			}
		}

		// Filenames
		// =========
		// Filename are stored one after the other, separated by 0x00.
		// Those are now long filenames, at the opposite of previous STK version.

		if (fseek(stk, filenamePos, SEEK_SET) != 0)
			extractError(stk, gobConf, chunks, "Unable to locate filename");

		if (fgets(curChunk->name, 64, stk) == 0)
			extractError(stk, gobConf, chunks, "Unable to read filename");

		// Files
		// =====
		// The structure of the file section if the following :
		// + 04 bytes : Uncompressed size (redondant with the one in Misc info)
		// + ?? bytes : Compressed data

		curChunk->offset = filePos;
		curChunk->preGob = false;

		// Write the chunk info in the gob Conf file
		fprintf(gobConf, "%s %d\n", curChunk->name, curChunk->packed ? 1 : 0);

		if (numDataChunks > 0) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
		}
		cpt++;
	}

	return chunks;
}

void extractChunks(Filename *outpath, FILE *stk, Chunk *chunks) {
	Chunk *curChunk = chunks;
	byte *unpackedData;

	while (curChunk != 0) {
		printf("Extracting \"%s\"\n", curChunk->name);

		FILE *chunkFile;
		outpath->setFullName(curChunk->name);
		if (!(chunkFile = fopen(outpath->getFullPath(), "wb")))
			extractError(stk, 0, chunks, "Couldn't write file");

		if (fseek(stk, curChunk->offset, SEEK_SET) == -1)
			extractError(stk, chunkFile, chunks, "Unexpected EOF");

		byte *data = new byte[curChunk->size];

		if (fread((char *) data, curChunk->size, 1, stk) < 1)
			extractError(stk, chunkFile, chunks, "Unexpected EOF");

		if (curChunk->packed) {
			uint32 realSize;

			if (curChunk->preGob) {
				unpackedData = unpackPreGobData(data, realSize, curChunk->size);
			} else {
				unpackedData = unpackData(data, realSize);
			}

			if (fwrite((char *) unpackedData, realSize, 1, chunkFile) < 1)
				extractError(stk, chunkFile, chunks, "Couldn't write");

			delete[] unpackedData;

		} else {
			if (fwrite((char *) data, curChunk->size, 1, chunkFile) < 1)
				extractError(stk, chunkFile, chunks, "Couldn't write");
		}

		delete[] data;
		fclose(chunkFile);

		curChunk = curChunk->next;
	}
}

// Some LZ77-variant
byte *unpackData(byte *src, uint32 &size) {
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
byte *unpackPreGobData(byte *src, uint32 &size, uint32 &compSize) {
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
//  - bytes 0&1 : if set to 0xFFFF, the real size is in bytes 2&3. Else : not understand
//  - bytes 2&3 : Either the real size or 0x007D. Directly related to the size of the file.
//  - bytes 4&5 : 0x0000 (files are small) ;)
	if (dummy1 == 0xFFFF)
		printf("Real size %d\n", READ_LE_UINT32(src));
	else
		printf("Unknown real size %xX %xX\n", dummy1>>8, dummy1 & 0x00FF);

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
