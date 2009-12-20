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

#include <stdarg.h>
#include "extract_mm_apple.h"

#define NUM_ROOMS	55
unsigned char room_disks_apple[NUM_ROOMS], room_tracks_apple[NUM_ROOMS], room_sectors_apple[NUM_ROOMS];

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

ExtractMMApple::ExtractMMApple(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	
	ToolInput input1;
	input1.format = "*.dsk";
	_inputPaths.push_back(input1);

	ToolInput input2;
	input2.format = "*.dsk";
	_inputPaths.push_back(input2);

	_shorthelp = "Extract Maniac Mansion data files, from the Apple II version.";
	_helptext = "\nUsage: " + _name + " [-o <output dir> = out/] <disk1.dsk> <disk2.dsk>\n\t" + _shorthelp + "\n";
}

void ExtractMMApple::execute() {
	int i, j;
	unsigned short signature;
	char fname[256];

	Filename inpath1(_inputPaths[0].path);
	Filename inpath2(_inputPaths[1].path);
	Filename &outpath = _outputPath;

	if (outpath.empty())
		// Standard output path
		outpath.setFullPath("out/");

	File input1(inpath1, "rb");
	File input2(inpath2, "rb");

	input1.seek(142080, SEEK_SET);
	input2.seek(143104, SEEK_SET);

	/* check signature */
	signature = input1.readUint16LE();
	if (signature != 0x0A31)
		error("Signature not found in disk 1!");

	signature = input2.readUint16LE();
	if (signature != 0x0032)
		error("Signature not found in disk 2!");

	outpath.setFullName("00.LFL");
	File output(fname, "wb");
	// All output should be xored
	output.setXorMode(0xFF);
	print("Creating 00.LFL...\n");

	/* write signature */
	output.writeUint16LE(signature);

	/* copy object flags */
	for (i = 0; i < 256; i++)
		output.writeByte(input1.readByte());

	/* copy room offsets */
	for (i = 0; i < NUM_ROOMS; i++) {
		room_disks_apple[i] = input1.readByte();
		output.writeByte(room_disks_apple[i]);
	}
	for (i = 0; i < NUM_ROOMS; i++) {
		room_sectors_apple[i] = input1.readByte();
		output.writeByte(room_sectors_apple[i]);
		room_tracks_apple[i] = input1.readByte();
		output.writeByte(room_tracks_apple[i]);
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

	/* NOTE: Extra 92 bytes of unknown data */

	for (i = 0; i < NUM_ROOMS; i++) {
		File *input;

		if (room_disks_apple[i] == '1')
			input = &input1;
		else if (room_disks_apple[i] == '2')
			input = &input2;
		else
			continue;

		sprintf(fname, "%02i.LFL", i);
		outpath.setFullName(fname);
		output.open(fname, "wb");

		print("Creating %s...\n", fname);
		input->seek((SectorOffset[room_tracks_apple[i]] + room_sectors_apple[i]) * 256, SEEK_SET);

		for (j = 0; j < ResourcesPerFile[i]; j++) {
			unsigned short len = input->readUint16LE();
			output.writeUint16LE(len);

			for (len -= 2; len > 0; len--)
				output.writeByte(input->readByte());
		}
		input->rewind();
	}
	print("All done!");
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractMMApple mmapple(argv[0]);
	return mmapple.run(argc, argv);
}
#endif

