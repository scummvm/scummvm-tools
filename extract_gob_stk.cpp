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

struct Chunk {
	char name[14];
	uint32 size, offset;
	bool packed; 

	Chunk *next;

	Chunk() : next(0) { }
	~Chunk() { delete next; }
};

void extractError(FILE *f1, FILE *f2, Chunk *chunks, const char *msg);
Chunk *readChunkList(FILE *stk);
void extractChunks(FILE *stk, Chunk *chunks);
byte *unpackData(byte *src, uint32 &size);

int main(int argc, char **argv) {

	if ((argc < 2) || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {

		printf("Usage: %s <file>\n\n", argv[0]);
		printf("The files will be extracted into the current directory.\n");

		return -1;
	}

	FILE *stk;

	if (!(stk = fopen(argv[1], "rb")))
		error("Couldn't open file \"%s\"", argv[1]);

	Chunk *chunks = readChunkList(stk);
	extractChunks(stk, chunks);
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

Chunk *readChunkList(FILE *stk) {
	uint16 numDataChunks = readUint16LE(stk);
	Chunk *chunks = new Chunk;
	Chunk *curChunk = chunks;

	while (numDataChunks-- > 0) {
		if (fread(curChunk->name, 13, 1, stk) < 1)
			extractError(stk, 0, chunks, "Unexpected EOF");

		curChunk->size = readUint32LE(stk);
		curChunk->offset = readUint32LE(stk);
		curChunk->packed = readByte(stk) != 0;

		if (numDataChunks > 0) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
		}
	}

	return chunks;
}

void extractChunks(FILE *stk, Chunk *chunks) {
	Chunk *curChunk = chunks;

	while (curChunk != 0) {
		printf("Extracting \"%s\"\n", curChunk->name);

		FILE *chunkFile;
		if (!(chunkFile = fopen(curChunk->name, "wb")))
			extractError(stk, 0, chunks, "Couldn't write file");

		if (fseek(stk, curChunk->offset, SEEK_SET) == -1)
			extractError(stk, chunkFile, chunks, "Unexpected EOF");

		byte *data = new byte[curChunk->size];

		if (fread((char *) data, curChunk->size, 1, stk) < 1)
			extractError(stk, chunkFile, chunks, "Unexpected EOF");

		if (curChunk->packed) {
			uint32 realSize;

			byte *unpackedData = unpackData(data, realSize);

			if (fwrite((char *) unpackedData, realSize, 1, chunkFile) < 1)
				extractError(stk, chunkFile, chunks, "Couldn't write");

			delete[] unpackedData;

		} else
			if (fwrite((char *) data, curChunk->size, 1, chunkFile) < 1)
				extractError(stk, chunkFile, chunks, "Couldn't write");

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
