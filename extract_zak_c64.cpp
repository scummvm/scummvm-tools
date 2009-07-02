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

void writeByteAltC64(FILE *fp, uint8 b) {
	writeByte(fp, (uint8)(b ^ 0xFF));
}

void writeUint16LEAltC64(FILE *fp, uint16 value) {
	writeUint16LE(fp, (uint16)(value ^ 0xFFFF));
}

#define writeByte writeByteAltC64
#define writeUint16LE writeUint16LEAltC64

#define NUM_ROOMS 59
unsigned char room_disks_c64[NUM_ROOMS], room_tracks_c64[NUM_ROOMS], room_sectors_c64[NUM_ROOMS];

static const int SectorOffset[36] = {
	0,
	0, 21, 42, 63, 84, 105, 126, 147, 168, 189, 210, 231, 252, 273, 294, 315, 336,
	357, 376, 395, 414, 433, 452, 471,
	490, 508, 526, 544, 562, 580,
	598, 615, 632, 649, 666
};
static const int ResourcesPerFile[NUM_ROOMS] = {
	 0, 29, 12, 14, 13,  4,  4, 10,  7,  4,
	14, 19,  5,  4,  7,  6, 11,  9,  4,  4,
	 1,  3,  3,  5,  1,  9,  4, 10, 13,  6,
	 7, 10,  2,  6,  1, 11,  2,  5,  7,  1,
	 7,  1,  4,  2,  8,  6,  6,  6,  4, 13,
	 3,  1,  2,  1,  2,  1, 10,  1,  1
};

int export_main(extract_zak_c64)(int argc, char *argv[]) {
	const char *helptext = "\nUsage: %s [-o <output dir> = out/] <disk1.d64> <disk2.d64>\n";

	FILE *input1, *input2, *output;
	int i, j;
	unsigned short signature;

	int first_arg = 1;
	int last_arg = argc - 1;

	char fname[1024];
	Filename outpath;

	// Check if we should display some helpful text
	parseHelpArguments(argv, argc, helptext);
	
	// Continuing with finding out output directory
	// also make sure we skip those arguments
	if (parseOutputDirectoryArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputDirectoryArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else
		// Standard output dir
		outpath.setFullPath("out/");

	if (last_arg - first_arg == 1)
		error("Requires two disk files");

	if (!(input1 = fopen(argv[first_arg],"rb")))
		error("Unable to open file %s for input!",argv[first_arg]);
	++first_arg;
	if (!(input2 = fopen(argv[first_arg],"rb")))
		error("Unable to open file %s for input!",argv[first_arg]);

	/* check signature */
	signature = readUint16LE(input1);
	if (signature != 0x0A31)
		error("Signature not found in disk 1!");
	signature = readUint16LE(input2);
	if (signature != 0x0132)
		error("Signature not found in disk 2!");
	outpath.setFullName("00.LFL");
	if (!(output = fopen(outpath.getFullPath(), "wb")))
		error("Unable to create index file!");
	notice("Creating 00.LFL...");

	/* write signature */
	writeUint16LE(output, signature);

	/* copy object flags */
	for (i = 0; i < 775; i++)
		writeByte(output, readByte(input1));

	/* copy room offsets */
	for (i = 0; i < NUM_ROOMS; i++) {
		room_disks_c64[i] = readByte(input1);
		writeByte(output, room_disks_c64[i]);
	}
	for (i = 0; i < NUM_ROOMS; i++) {
		room_sectors_c64[i] = readByte(input1);
		writeByte(output, room_sectors_c64[i]);
		room_tracks_c64[i] = readByte(input1);
		writeByte(output, room_tracks_c64[i]);
	}

	/* copy costume offsets */
	for (i = 0; i < 38; i++)
		writeByte(output, readByte(input1));
	for (i = 0; i < 38; i++)
		writeUint16LE(output, readUint16LE(input1));

	/* copy script offsets */
	for (i = 0; i < 155; i++)
		writeByte(output, readByte(input1));
	for (i = 0; i < 155; i++)
		writeUint16LE(output, readUint16LE(input1));

	/* copy sound offsets */
	for (i = 0; i < 127; i++)
		writeByte(output, readByte(input1));
	for (i = 0; i < 127; i++)
		writeUint16LE(output, readUint16LE(input1));

	fclose(output);

	for (i = 0; i < NUM_ROOMS; i++) {
		FILE *input;

		if (room_disks_c64[i] == '1')
			input = input1;
		else if (room_disks_c64[i] == '2')
			input = input2;
		else
			continue;

		sprintf(fname,"%02i.LFL", i);
		outpath.setFullName(fname);
		output = fopen(outpath.getFullPath(), "wb");

		if (output == NULL) {
			error("Unable to create %s!", fname);
		}

		notice("Creating %s...", fname);
		fseek(input, (SectorOffset[room_tracks_c64[i]] + room_sectors_c64[i]) * 256, SEEK_SET);

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

#ifdef UNIX
int main(int argc, char *argv[]) __attribute__((weak));
int main(int argc, char *argv[]) {
	return export_main(extract_zak_c64)(argc, argv);
}
#endif

