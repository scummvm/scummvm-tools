/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "compress_cge.h"

#define BUFFER_SIZE 8192
#define SEED        0xA5
#define MAX_FILES   5000

static void readData(FILE *f, byte *buff, int size) {
	int bytesRead = fread(buff, 1, size, f);
	for (int i = 0; i < bytesRead; ++i)
		buff[i] ^= SEED;
}

static void writeData(FILE *f, byte *buff, int size) {
	for (int i = 0; i < size; ++i)
		buff[i] ^= SEED;
	fwrite(buff, 1, size, f);
}

static void unpack() {
	printf("Unpacking...\n");

	FILE *volCat, *volDat, *fOut, *fFiles;
	BtPage btPage;

	if ((volCat = fopen("vol.cat", "rb")) == NULL) {
		printf("FATAL: Unable to open vol.cat\n");
		exit(0);
	}

	if ((volDat = fopen("vol.dat", "rb")) == NULL) {
		printf("FATAL: Unable to open vol.dat\n");
		exit(0);
	}

	if ((fFiles = fopen("files.txt", "w")) == NULL) {
		printf("FATAL: Unable to create files.txt\n");
		exit(0);
	}

	// Get in a list of pages individual files will be on
	readData(volCat, (byte *)&btPage, sizeof(BtPage));

	int pageList[1000];
	int pageCount = btPage._hea._count;
	pageList[0] = btPage._hea._down;
	for (int i = 0; i < pageCount; ++i)
		pageList[i + 1] = btPage._inn[i]._down;
	
	bool first = true;
	// Loop through the pages of individual files
	for (int i = 0; i <= pageCount; ++i) {
		// Move to correct page and read it
		fseek(volCat, pageList[i] * sizeof(BtPage), SEEK_SET);
		readData(volCat, (byte *)&btPage, sizeof(BtPage));
		
		// Process the files
		for (unsigned int fileNum = 0; fileNum < btPage._hea._count; ++fileNum) {
			char fname[256];
			strcpy(fname, btPage._lea[fileNum]._key);
			
			// Add filename to files list
			if (!first)
				fprintf(fFiles, "\n%s", btPage._lea[fileNum]._key);
			else {
				fprintf(fFiles, "%s", btPage._lea[fileNum]._key);
				first = false;
			}

			fOut = fopen(fname, "wb");
			byte *buffer = (byte *)malloc(btPage._lea[fileNum]._size);

			fseek(volDat, btPage._lea[fileNum]._mark, SEEK_SET);
			readData(volDat, buffer, btPage._lea[fileNum]._size);
			fwrite(buffer, 1, btPage._lea[fileNum]._size, fOut);

			fclose(fOut);
			free(buffer);
		}
	}

	fclose(volCat);
	fclose(volDat);
	fclose(fFiles);
}

static void pack() {
	FILE *volCat, *volDat, *fIn;
	BtPage btPage;
	printf("Packing...\n");

	// Load in the list of files to recompress
	char files[MAX_FILES][kBtKeySize];
	int fileCount = 0;
	if ((fIn = fopen("files.txt", "r")) == NULL) {
		printf("FATAL: Unable to open files.txt\n");
		exit(0);
	}

	while (!feof(fIn)) {
		fscanf(fIn, "%s", &files[fileCount++][0]);
		if (fileCount == MAX_FILES) {
			printf("FATAL: Max files reached\n");
			exit(0);
		}
	}
	fclose(fIn);

	// Open vol cat and dat files for writing
	if ((volCat = fopen("vol.cat", "wb")) == NULL) {
		printf("FATAL: Unable to create vol.cat\n");
		exit(0);
	}
	if ((volDat = fopen("vol.dat", "wb")) == NULL) {
		printf("FATAL: Unable to create vol.dat\n");
		exit(0);
	}

	/* Build the index page */
	// Header
	memset(&btPage, 0, sizeof(BtPage));
	int pageCount = fileCount / LEA_SIZE;
	btPage._hea._count = pageCount;
	btPage._hea._down = 1;

	// Innert file list - lists the first file of the next page
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int nextFile = (pageNum + 1) * LEA_SIZE;

		btPage._inn[pageNum]._down = pageNum + 2;
		strcpy((char *)&btPage._inn[pageNum]._key[0], files[nextFile]);
	}

	// Write out the index page
	writeData(volCat, (byte *)&btPage, sizeof(BtPage));

	// Loop through processing each page and the dat file
	pageCount = (fileCount + LEA_SIZE - 1) / LEA_SIZE;
	int fileIndex = 0;
	for (int pageNum = 0; pageNum < pageCount; ++pageNum) {
		int startFile = pageNum * LEA_SIZE;
		int lastFile = (pageNum + 1) * LEA_SIZE - 1;
		if (lastFile >= fileCount)
			lastFile = fileCount - 1;

		// Header
		memset(&btPage, 0, sizeof(BtPage));
		btPage._hea._count = lastFile - startFile + 1;
		btPage._hea._down = 0xffff;

		for (int fileNum = 0; fileNum < btPage._hea._count; ++fileNum, ++fileIndex) {
			// Set filename and offset in dat file
			strcpy(btPage._lea[fileNum]._key, &files[fileIndex][0]);
			btPage._lea[fileNum]._mark = ftell(volDat);

			// Load the given file and write it into the dat file
			char fname[32];
			strcpy(fname, files[fileIndex]);

			// Open the file and get the size
			fIn = fopen(fname, "rb");
			if (!fIn) {
				printf("FATAL: Error opening %s\n", fname);
				exit(-1);
			}
			fseek(fIn, 0, SEEK_END);
			int fileSize = ftell(fIn);
			fseek(fIn, 0, SEEK_SET);
			btPage._lea[fileNum]._size = fileSize;

			// Allocate buffer space for the file
			byte *buffer = (byte *)malloc(fileSize);

			// Read it in, encrypt it, and write it out
			fread(buffer, 1, fileSize, fIn);
			writeData(volDat, buffer, fileSize);

			free(buffer);
			fclose(fIn);
		}

		// Write out the page
		writeData(volCat, (byte *)&btPage, sizeof(BtPage));
	}

	fclose(volCat);
	fclose(volDat);
}


int main(int argc,  const char *argv[]) {
	if (argc !=  2) {
		printf("Usage:\n\t%s <option>\n", argv[0]);
		printf("where option is\n");
		printf("\t-e\tExtract Soltys game data (generates files.txt)\n");
		printf("\t-p\tPack Soltys game data (requires files.txt)\n");
		return -1;
	}
 
	const char *option = argv[1];
	if (!strcmp(option, "-e"))
		unpack();
	else if (!strcmp(option, "-p"))
		pack();
	else
		printf("Invalid option specified\n");
	return 0;
}

