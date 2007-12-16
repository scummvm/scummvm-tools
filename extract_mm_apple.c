/* extract_mm_apple - Extract data files from Apple II version of Maniac Mansion
 * Copyright (C) 2004-2006  The ScummVM Team
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
#include <stdarg.h>

void writeByteAlt(FILE *fp, uint8 b) {
	writeByte(fp, (uint8)(b ^ 0xFF));
}

void writeUint16LEAlt(FILE *fp, uint16 value) {
	writeUint16LE(fp, (uint16)(value ^ 0xFFFF));
}

#define writeByte writeByteAlt
#define writeUint16LE writeUint16LEAlt

#define NUM_ROOMS 55
unsigned char room_disks[NUM_ROOMS], room_tracks[NUM_ROOMS], room_sectors[NUM_ROOMS];
static const int SectorOffset[36] = {
 	         0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256,
 	         272, 288, 304, 320, 336, 352, 368,
 	         384, 400, 416, 432, 448, 464,
 	         480, 496, 512, 528, 544, 560
 	 };
static const int ResourcesPerFile[NUM_ROOMS] = {
         0, 11,  1,  3,  9, 12,  1, 13, 10,  6,
         4,  1,  7,  1,  1,  2,  7,  8, 19,  9,
         6,  9,  2,  6,  8,  4, 16,  8,  3,  3,
        12, 12,  2,  8,  1,  1,  2,  1,  9,  1,
         3,  7,  3,  3, 13,  5,  4,  3,  1,  1,
         3, 10,  1,  0,  0
};

int main (int argc, char **argv) {
	FILE *input1, *input2, *output;
	char fname[1024];
	char inputPath[768];
	int i, j;
	unsigned short signature;

	if (argc < 3) {
		printf("Usage: %s <disk1.dsk> <disk2.dsk>\n", argv[0]);
		exit(2);
	}

	getPath(argv[argc - 1], inputPath);

	if (!(input1 = fopen(argv[1], "rb"))) {
		error("Unable to open file %s for input", argv[1]);
	}

	if (!(input2 = fopen(argv[2], "rb"))) {
		error("Unable to open file %s for input", argv[2]);
	}

	fseek(input1, 142080, SEEK_SET);
	fseek(input2, 143104, SEEK_SET);

	/* check signature */
	signature = readUint16LE(input1);

	if (signature != 0x0A31) {
		error("Signature not found in disk 1!");
	}

	signature = readUint16LE(input2);

	if (signature != 0x0032) {
		error("Signature not found in disk 2!");
	}

	sprintf(fname, "%s/00.LFL", inputPath);
	if (!(output = fopen(fname, "wb"))) {
		error("Unable to create index file!");
	}

	notice("Creating 00.LFL...");

	/* write signature */
	writeUint16LE(output, signature);

	/* copy object flags */
	for (i = 0; i < 256; i++) {
		writeByte(output, readByte(input1));
	}

	/* copy room offsets */
	for (i = 0; i < 55; i++) {
		room_disks[i] = readByte(input1);
		writeByte(output, room_disks[i]);
	}

	for (i = 0; i < 55; i++) {
		room_sectors[i] = readByte(input1);
		writeByte(output, room_sectors[i]);
		room_tracks[i] = readByte(input1);
		writeByte(output, room_tracks[i]);
	}

	/* copy costume offsets */
	for (i = 0; i < 25; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 25; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	/* copy script offsets */
	for (i = 0; i < 160; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 160; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	/* copy sound offsets */
	for (i = 0; i < 70; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 70; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	/* NOTE: Extra 92 bytes of unknown data */

	fclose(output);

	for (i = 0; i < NUM_ROOMS; i++) {
		FILE *input;

		if (room_disks[i] == '1') {
			input = input1;
		} else if (room_disks[i] == '2') {
			input = input2;
		} else {
			continue;
		}

		sprintf(fname, "%s/%02i.LFL", inputPath, i);
		output = fopen(fname, "wb");

		if (output == NULL) {
			error("Unable to create %s!", fname);
		}

		notice("Creating %s...", fname);
		fseek(input, (SectorOffset[room_tracks[i]] + room_sectors[i]) * 256, SEEK_SET);

		for (j = 0; j < ResourcesPerFile[i]; j++) {
			unsigned short len = readUint16LE(input);
			writeUint16LE(output, len);

			for (len -= 2; len > 0; len--) {
				writeByte(output, readByte(input));
			}
		}

		rewind(input);
	}

	notice("All done!");

	return 0;
}
