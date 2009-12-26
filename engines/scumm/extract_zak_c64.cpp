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

#include <cstdio>
#include <stdarg.h>
#include "extract_zak_c64.h"

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

ExtractZakC64::ExtractZakC64(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input1;
	input1.format = "*.d64";
	_inputPaths.push_back(input1);

	ToolInput input2;
	input2.format = "*.d64";
	_inputPaths.push_back(input2);

	_shorthelp = "Extract data files from the C64 version of Zak McKracken.";
	_helptext = "\nUsage: " + _name + " [-o <output dir> = out/] <disk1.d64> <disk2.d64>\n\t" + _shorthelp + "\n";
}

void ExtractZakC64::execute() {
	int i, j;
	unsigned short signature;
	char fname[256];

	// Two disks...
	Common::Filename inpath1(_inputPaths[0].path);
	Common::Filename inpath2(_inputPaths[1].path);
	Common::Filename &outpath = _outputPath;

	if (outpath.empty())
		// Standard output path
		outpath.setFullPath("out/");
	
	Common::File input1(inpath1, "rb");
	Common::File input2(inpath2, "rb");

	/* check signature */
	signature = input1.readUint16LE();
	if (signature != 0x0A31)
		error("Signature not found in disk 1!");
	signature = input2.readUint16LE();
	if (signature != 0x0132)
		error("Signature not found in disk 2!");

	outpath.setFullName("00.LFL");
	Common::File output(outpath, "wb");
	output.setXorMode(0xFF);
	print("Creating 00.LFL...\n");

	/* write signature */
	output.writeUint16LE(signature);

	/* copy object flags */
	for (i = 0; i < 775; i++)
		output.writeByte(input1.readByte());

	/* copy room offsets */
	for (i = 0; i < NUM_ROOMS; i++) {
		room_disks_c64[i] = input1.readByte();
		output.writeByte(room_disks_c64[i]);
	}
	for (i = 0; i < NUM_ROOMS; i++) {
		room_sectors_c64[i] = input1.readByte();
		output.writeByte(room_sectors_c64[i]);
		room_tracks_c64[i] = input1.readByte();
		output.writeByte(room_tracks_c64[i]);
	}

	/* copy costume offsets */
	for (i = 0; i < 38; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 38; i++)
		output.writeUint16LE(input1.readUint16LE());

	/* copy script offsets */
	for (i = 0; i < 155; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 155; i++)
		output.writeUint16LE(input1.readUint16LE());

	/* copy sound offsets */
	for (i = 0; i < 127; i++)
		output.writeByte(input1.readByte());
	for (i = 0; i < 127; i++)
		output.writeUint16LE(input1.readUint16LE());

	output.close();

	for (i = 0; i < NUM_ROOMS; i++) {
		Common::File *input;

		if (room_disks_c64[i] == '1')
			input = &input1;
		else if (room_disks_c64[i] == '2')
			input = &input2;
		else
			continue;

		sprintf(fname, "%02i.LFL", i);
		outpath.setFullName(fname);
		output.open(outpath, "wb");

		print("Creating %s...\n", fname);
		input->seek((SectorOffset[room_tracks_c64[i]] + room_sectors_c64[i]) * 256, SEEK_SET);

		for (j = 0; j < ResourcesPerFile[i]; j++) {
			unsigned short len = input->readUint16LE();
			output.writeUint16LE(len);

			for (len -= 2; len > 0; len--) {
				output.writeByte(input->readByte());
			}
		}

		input->rewind();
	}

	print("All done!\n");
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractZakC64 z64(argv[0]);
	return z64.run(argc, argv);
}
#endif

