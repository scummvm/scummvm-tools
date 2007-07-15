/* extract_zak_c64 - Extract data files from C64 version of Zak McKracken
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

typedef int BOOL;
#define TRUE 1
#define FALSE 0

#ifdef _MSC_VER
	#define	vsnprintf _vsnprintf
#endif

void writeByteAlt(FILE *fp, uint8 b) {
	writeByte(fp, (uint8)(b ^ 0xFF));
}

void writeUint16LEAlt(FILE *fp, uint16 value) {
	writeUint16LE(fp, (uint16)(value ^ 0xFFFF));
}

#define writeByte writeByteAlt
#define writeUint16LE writeUint16LEAlt

void notice(const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stdout, "%s\n", buf);
}

unsigned char room_disks[59], room_tracks[59], room_sectors[59];

int main (int argc, char **argv) {
	FILE *input1, *input2, *output;
	char fname[256];
	int i, j;
	unsigned short signature;

	if (argc < 3) {
		printf("Usage: %s <disk1.d64> <disk2.d64>\n",argv[0]);
		exit(2);
	}

	if (!(input1 = fopen(argv[1],"rb"))) {
		error("Unable to open file %s for input!",argv[1]);
	}

	if (!(input2 = fopen(argv[2],"rb"))) {
		error("Unable to open file %s for input!",argv[2]);
	}

	/* check signature */
	signature = readUint16LE(input1);

	if (signature != 0x0A31) {
		error("Signature not found in disk 1!");
	}

	signature = readUint16LE(input2);

	if (signature != 0x0132) {
		error("Signature not found in disk 2!");
	}

	if (!(output = fopen("00.LFL","wb"))) {
		error("Unable to create index file!");
	}

	notice("Creating 00.LFL...");

	/* write signature */
	writeUint16LE(output, signature);

	/* copy object flags */
	for (i = 0; i < 775; i++) {
		writeByte(output, readByte(input1));
	}

	/* copy room offsets */
	for (i = 0; i < 59; i++) {
		room_disks[i] = readByte(input1);
		writeByte(output, room_disks[i]);
	}

	for (i = 0; i < 59; i++) {
		room_sectors[i] = readByte(input1);
		room_tracks[i] = readByte(input1);
		writeByte(output, room_sectors[i]);
		writeByte(output, room_tracks[i]);
	}

	/* copy costume offsets */
	for (i = 0; i < 38; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 38; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	/* copy script offsets */
	for (i = 0; i < 155; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 155; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	/* copy sound offsets */
	for (i = 0; i < 127; i++) {
		writeByte(output, readByte(input1));
	}

	for (i = 0; i < 127; i++) {
		writeUint16LE(output, readUint16LE(input1));
	}

	fclose(output);

	for (i = 0; i < 59; i++) {
		const int SectorOffset[36] = {
			0,
			0, 21, 42, 63, 84, 105, 126, 147, 168, 189, 210, 231, 252, 273, 294, 315, 336,
			357, 376, 395, 414, 433, 452, 471,
			490, 508, 526, 544, 562, 580,
			598, 615, 632, 649, 666
		};

		const int ResourcesPerFile[59] = {
			 0, 29, 12, 14, 13,  4,  4, 10,  7,  4,
			14, 19,  5,  4,  7,  6, 11,  9,  4,  4,
			 1,  3,  3,  5,  1,  9,  4, 10, 13,  6,
			 7, 10,  2,  6,  1, 11,  2,  5,  7,  1,
			 7,  1,  4,  2,  8,  6,  6,  6,  4, 13,
			 3,  1,  2,  1,  2,  1, 10,  1,  1
		};

		FILE *input;

		if (room_disks[i] == '1') {
			input = input1;
		} else if (room_disks[i] == '2') {
			input = input2;
		} else {
			continue;
		}

		sprintf(fname,"%02i.LFL", i);
		output = fopen(fname, "wb");

		if (output == NULL) {
			error("Unable to create %s!",fname);
		}

		notice("Creating %s...",fname);
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
