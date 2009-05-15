/* compress_gob - .stk/.itk archive creation tool, based on a conf file.
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

	Chunk *next;

	Chunk() : next(0) { }
	~Chunk() { delete next; }
};

Chunk *readChunkConf (FILE *gobconf, uint16 &chunkCount);
void *rewriteHeader (FILE *stk, uint16 chunkCount, Chunk *chunks);
void *writeBody (FILE *stk, uint16 chunkcount, Chunk *chunks);

Chunk *readChunkList(FILE *stk, FILE *gobConf);

void extractChunks(FILE *stk, Chunk *chunks);
byte *unpackData(byte *src, uint32 &size);

int main(int argc, char **argv) {
	char *outFilename;
	char *tmpStr;
	Chunk *chunks;
	FILE *stk;
	FILE *gobConf;
	uint16 chunkCount;

	if ((argc < 2) || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printf("Usage: %s <Conf file>\n\n", argv[0]);
		printf("The archive will be created into the current directory.\n");
		return -1;
	}

	if (!(gobConf = fopen(argv[1], "r")))
		error("Couldn't open conf file \"%s\"", argv[1]);

	outFilename = new char[strlen(argv[1]) + 5];
	getFilename(argv[1], outFilename);

	tmpStr = strstr(outFilename, ".");
	if (tmpStr != 0)
		strcpy(tmpStr, ".stk");
	else
		strcat(outFilename, ".stk");

	if (!(stk = fopen(outFilename, "wb")))
		error("Couldn't create file \"%s\"", outFilename);

	chunks = readChunkConf(gobConf, chunkCount);
	fclose(gobConf);
	
	writeBody(stk, chunkCount, chunks);
	rewriteHeader(stk, chunkCount, chunks);

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

Chunk *readChunkConf(FILE *gobConf, uint16 &chunkCount) {
	Chunk *chunks = new Chunk;
	Chunk *curChunk = chunks;
	char buffer [1024];

	chunkCount = 1;

// first read (signature, not yet used)
	fscanf(gobConf, "%s", buffer);
	fscanf(gobConf, "%s", buffer);

	while (!feof(gobConf)) {
		strcpy(curChunk->name, buffer);
		fscanf(gobConf, "%s", buffer);
		if (strcmp(buffer, "1") == 0) 
			curChunk->packed = true;
		else
			curChunk->packed = false;

		fscanf(gobConf, "%s", buffer);
		if (!feof(gobConf)) {
			curChunk->next = new Chunk;
			curChunk = curChunk->next;
			chunkCount++;
		}
	}
	return chunks;
}

void *writeBody(FILE *stk, uint16 chunkCount, Chunk *chunks) {
	Chunk *curChunk = chunks;
	FILE *src;
	char buffer[4096];
	int count;

	for (count = 0; count < 2 + (chunkCount * 22); count++)
		fputc(0, stk);

	while (curChunk) {
		if (!(src = fopen(curChunk->name, "rb")))
			error("Couldn't open conf file \"%s\"", curChunk->name);

		curChunk->size = 0;

		do {
			count = fread(buffer, 1, 4096, src);
			fwrite(buffer, 1, count, stk);
			curChunk->size += count;
		} while (count == 4096);

		printf("File: %s - Size: %d\n", curChunk->name, curChunk->size);
		fclose(src);

		curChunk = curChunk->next;
	}

	return 0;
}

void *rewriteHeader(FILE *stk, uint16 chunkCount, Chunk *chunks) {
	uint16 i;
	char buffer[1024];
	Chunk *curChunk = chunks;
	uint32 filPos;

	rewind(stk);

//	The structure of the header is the following :
//+ 2 bytes : numbers of files archived in the .stk/.itk
//	Then, for each files :
//+ 13 bytes : the filename, terminated by '\0'. In original, there's 
//  garbage after if the filename has not the maximum length
//+ 4  bytes : size of the chunk
//+ 4  bytes : start position of the chunk in the file
//+ 1  byte  : If 0 : not compressed, if 1 : compressed
	filPos = 2 + (chunkCount * 22);

	buffer[0] = chunkCount & 0xFF;
	buffer[1] = chunkCount >> 8;
	fwrite(buffer, 1, 2, stk);

	// TODO : Implement STK21
	while (curChunk) {
		for (i = 0; i < 13; i++)
			if (i < strlen(curChunk->name))
				buffer[i] = curChunk->name[i];
			else
				buffer[i] = '\0';
		fwrite(buffer, 1, 13, stk);
		
		buffer[0] = curChunk->size;
		buffer[1] = curChunk->size >> 8;
		buffer[2] = curChunk->size >> 16;
		buffer[3] = curChunk->size >> 24;
		buffer[4] = filPos;
		buffer[5] = filPos >> 8;
		buffer[6] = filPos >> 16;
		buffer[7] = filPos >> 24;

// Compression not yet implemented => always uncompressed
//		buffer[8]=curChunk->packed?'\1':'\0';
		buffer[8] = '\0';
		fwrite(buffer, 1, 9, stk);
		filPos += curChunk->size;

		curChunk = curChunk->next;
	}

	return 0;
}
