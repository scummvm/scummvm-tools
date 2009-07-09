/* extract_mm_c64 - Extract data files from C64 version of Maniac Mansion
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
#include "extract_mm_c64.h"

#define NUM_ROOMS	55

unsigned char room_disks[NUM_ROOMS], room_tracks[NUM_ROOMS], room_sectors[NUM_ROOMS];
static const int SectorOffset[36] = {
	0,
	0, 21, 42, 63, 84, 105, 126, 147, 168, 189, 210, 231, 252, 273, 294, 315, 336,
	357, 376, 395, 414, 433, 452, 471,
	490, 508, 526, 544, 562, 580,
	598, 615, 632, 649, 666
};
static const int ResourcesPerFile[NUM_ROOMS] = {
	 0, 11,  1,  3,  9, 12,  1, 13, 10,  6,
	 4,  1,  7,  1,  1,  2,  7,  8, 19,  9,
	 6,  9,  2,  6,  8,  4, 16,  8,  3,  3,
	12, 12,  2,  8,  1,  1,  2,  1,  9,  1,
	 3,  7,  3,  3, 13,  5,  4,  3,  1,  1,
	 3, 10,  1,  0,  0
};

ExtractMMC64::ExtractMMC64(const std::string &name) : Tool(name) {
	_helptext = "\nUsage: " + _name + " [-o <output dir> = out/] <disk1.d64> <disk2.d64>\n";
}

void ExtractMMC64::execute() {
	int i, j;
	unsigned short signature;
	char fname[256];
	
	// Two disks...
	if (_inputPaths.size() != 2)
		error("Two input files expected!");
	Filename inpath1(_inputPaths[0]);
	Filename inpath2(_inputPaths[1]);
	Filename &outpath = _outputPath;

	if (outpath.empty())
		// Standard output path
		outpath.setFullPath("out/");
	
	File input1(inpath1, "rb");
	File input2(inpath2, "rb");

	/* check signature */
	signature = input1.readUint16LE();
	if (signature != 0x0A31)
		error("Signature not found in disk 1!");
	signature = input2.readUint16LE();
	if (signature != 0x0132)
		error("Signature not found in disk 2!");

	outpath.setFullName("00.LFL");
	File output(outpath, "wb");
	output.setXorMode(0xFF);
	print("Creating 00.LFL...");

	/* write signature */
	output.writeUint16LE(signature);

	/* copy object flags */
	for (i = 0; i < 256; i++)
		output.writeByte(input1.readByte());
	/* copy room offsets */
	for (i = 0; i < NUM_ROOMS; i++) {
		room_disks[i] = input1.readByte();
		output.writeByte(room_disks[i]);
	}
	for (i = 0; i < NUM_ROOMS; i++) {
		room_sectors[i] = input1.readByte();
		output.writeByte(room_sectors[i]);
		room_tracks[i] = input1.readByte();
		output.writeByte(room_tracks[i]);
	}

	/* copy costume offsets */
	for (i = 0; i < 25; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 25; i++)
		output.writeUint16LE(input1.readUint16LE());

	/* copy script offsets */
	for (i = 0; i < 160; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 160; i++)
		output.writeUint16LE(input1.readUint16LE());

	/* copy sound offsets */
	for (i = 0; i < 70; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 70; i++)
		output.writeUint16LE(input1.readUint16LE());
	output.close();

	for (i = 0; i < NUM_ROOMS; i++) {
		File *input;

		if (room_disks[i] == '1')
			input = &input1;
		else if (room_disks[i] == '2')
			input = &input2;
		else
			continue;

		sprintf(fname, "%02i.LFL", i);
		outpath.setFullName(fname);
		output.open(outpath, "wb");

		print("Creating %s...", fname);
		input->seek((SectorOffset[room_tracks[i]] + room_sectors[i]) * 256, SEEK_SET);

		for (j = 0; j < ResourcesPerFile[i]; j++) {
			unsigned short len = input->readUint16LE();
			output.writeUint16LE(len);

			for (len -= 2; len > 0; len--) {
				output.writeByte(input->readByte());
			}
		}

		input->rewind();
	}

	print("All done!");
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractMMC64 mm_c64(argv[0]);
	return mm_c64.run(argc, argv);
}
#endif

