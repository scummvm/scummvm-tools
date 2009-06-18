/* extract_t7g_mac - Extractor for the Mac version of The 7th Guest
 * Copyright (C) 2008-2009 The ScummVM project
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

// Resource fork format taken from:
// http://developer.apple.com/documentation/mac/MoreToolbox/MoreToolbox-99.html

#include "util.h"

#define offsetResFork 128
uint32 offsetResourceData;

char *readString(FILE *ifp) {
	byte len = readByte(ifp);
	char *name = new char[len + 1];
	fread(name, len, 1, ifp);
	name[len] = 0;
	return name;
}

void dumpResource(FILE *ifp, char *name) {
	// Show the resource details
	uint32 fileSize = readUint32BE(ifp);
	printf("  \"%s\" (%d bytes)\n", name, fileSize);

	// Read the resource contents
	byte *buf = new byte[fileSize];
	if (!buf) {
		fclose(ifp);
		error("Could not allocate %ld bytes of memory", fileSize);
	}

	// Dump the resource to the output file
	FILE *ofp = fopen(name, "wb");
	fread(buf, 1, fileSize, ifp);
	fwrite(buf, 1, fileSize, ofp);
	fclose(ofp);

	// Free the resource memory
	delete[] buf;
}

void handleReferenceList(FILE *ifp, uint32 offsetRefList, uint16 numRes, uint32 offsetResNames) {
	for (int i = 0; i < numRes; i++) {
		if (fseek(ifp, offsetRefList + 12 * i + 2, SEEK_SET)) {
			fclose(ifp);
			error("Seek error");
		}
		uint32 offsetResName = offsetResNames + readUint16BE(ifp);
		uint32 offsetResData = offsetResourceData + (readUint32BE(ifp) & 0xFFFFFF);

		// Read the resource name
		if (fseek(ifp, offsetResName, SEEK_SET)) {
			fclose(ifp);
			error("Seek error");
		}
		char *name = readString(ifp);

		// Dump the resource
		if (fseek(ifp, offsetResData, SEEK_SET)) {
			fclose(ifp);
			error("Seek error");
		}
		dumpResource(ifp, name);

		// Free the resource name
		delete[] name;
	}
}

int main(int argc, char *argv[]) {
	FILE *ifp;

	if (argc != 2) {
		printf("Usage: %s <file>\n", argv[0]);
		exit(2);
	}

	if ((ifp = fopen(argv[1], "rb")) == NULL) {
		error("Could not open \'%s\'", argv[1]);
	}

	// Read the resource fork header
	if (fseek(ifp, offsetResFork, SEEK_SET)) {
		fclose(ifp);
		error("Seek error");
	}
	offsetResourceData = offsetResFork + readUint32BE(ifp);
	uint32 offsetResMap = offsetResFork + readUint32BE(ifp);

	// Read the resource map
	if (fseek(ifp, offsetResMap + 24, SEEK_SET)) {
		fclose(ifp);
		error("Seek error");
	}
	uint32 offsetResTypes = offsetResMap + readUint16BE(ifp);
	uint32 offsetResNames = offsetResMap + readUint16BE(ifp);

	// Handle the resource types
	if (fseek(ifp, offsetResTypes, SEEK_SET)) {
		fclose(ifp);
		error("Seek error");
	}
	uint numResTypes = readUint16BE(ifp) + 1;
	char resType[5];
	resType[4] = 0;
	for (uint i = 0; i < numResTypes; i++) {
		if (fseek(ifp, offsetResTypes + 2 + 8 * i, SEEK_SET)) {
			fclose(ifp);
			error("Seek error");
		}

		// Read the resource type name
		fread(resType, 4, 1, ifp);
		switch (READ_BE_UINT32(resType)) {
			case MKID_BE('csnd'):
			case MKID_BE('snd '):
			case MKID_BE('Midi'):
			case MKID_BE('cmid'):
			//case MKID_BE('SMOD'):
			case MKID_BE('SONG'):
			case MKID_BE('INST'):
			case MKID_BE('T7GM'):
			{
				printf("Extracting \"%s\" resources\n", resType);
				uint16 numRes = readUint16BE(ifp);
				uint32 offsetRefList = offsetResTypes + readUint16BE(ifp);

				handleReferenceList(ifp, offsetRefList, numRes, offsetResNames);
				break;
			}
			default:
				printf("Skipping \"%s\" resources\n", resType);
				break;
		}
	}

	fclose(ifp);
	return 0;
}
