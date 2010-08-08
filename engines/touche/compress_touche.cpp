/* compress_touche - Compress Touche Speech Data Files
 * Copyright (C) 2006  The ScummVM Team
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

#include <string.h>
#include <stdio.h>

#include "compress.h"
#include "compress_touche.h"

#define CURRENT_VER     1
#define HEADER_SIZE     4
#define MAX_OFFSETS   140
#define OBJ_HDR_LEN   200
#define Vxx_HDR_LEN  1024

#define OUTPUT_MP3   "TOUCHE.SO3"
#define OUTPUT_OGG   "TOUCHE.SOG"
#define OUTPUT_FLA   "TOUCHE.SOF"

static uint32 input_OBJ_offs[OBJ_HDR_LEN];
static uint32 input_OBJ_size[OBJ_HDR_LEN];
static uint32 input_Vxx_offs[Vxx_HDR_LEN];
static uint32 input_Vxx_size[Vxx_HDR_LEN];

CompressTouche::CompressTouche(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input;
	input.format = "/";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress Touche speech files (Vxxx and OBJ).";
	_helptext = "\nUsage: " + getName() + " [params] [-o outputfile TOUCHE.*] <inputdir>\n* differs with compression type.\n" + _shorthelp + "\n";
}

InspectionMatch CompressTouche::inspectInput(const Common::Filename &filename) {
	// We don't care for the actual file, just what's in this directory
	Common::Filename probe(filename);

	probe.setFullName("TOUCHE.DAT");
	if (probe.exists())
		return IMATCH_PERFECT;

	return IMATCH_AWFUL;
}

uint32 CompressTouche::compress_sound_data_file(uint32 current_offset, Common::File &output, Common::File &input, uint32 *offs_table, uint32 *size_table, int len) {
	int i, size;
	uint8 buf[2048];
	uint32 start_offset = current_offset;

	/* write 0 offsets/sizes table */
	for (i = 0; i < len; ++i) {
		offs_table[i] = input.readUint32LE();
		size_table[i] = input.readUint32LE();
		output.writeUint32LE(0);
		output.writeUint32LE(0);
		current_offset += 8;
	}
	for (i = 0; i < len; ++i) {
		if (size_table[i] == 0) {
			offs_table[i] = 0;
		} else {
			input.seek(offs_table[i], SEEK_SET);
			input.read_throwsOnError(buf, 8);

			if (memcmp(buf, "Creative", 8) != 0) {
				error("Invalid VOC data found");
			}

			print("VOC found (pos = %d) :\n", offs_table[i]);
			input.seek(18, SEEK_CUR);
			extractAndEncodeVOC(TEMP_RAW, input, _format);

			/* append converted data to output file */
			Common::File temp(tempEncoded, "rb");

			size_table[i] = 0;

			while ((size = temp.read_noThrow(buf, 2048)) > 0) {
				output.write(buf, size);
				size_table[i] += size;
			}

			offs_table[i] = current_offset;
			current_offset += size_table[i];
		}
	}

	/* fix data offsets table */
	output.seek(start_offset, SEEK_SET);
	for (i = 0; i < len; ++i) {
		output.writeUint32LE(offs_table[i]);
		output.writeUint32LE(size_table[i]);
	}
	output.seek(0, SEEK_END);

	return current_offset;
}

void CompressTouche::compress_sound_data(Common::Filename *inpath, Common::Filename *outpath) {
	int i;
	uint32 current_offset;
	uint32 offsets_table[MAX_OFFSETS];

	Common::File output(*outpath, "wb");

	output.writeUint16LE(1); /* current version */
	output.writeUint16LE(0); /* flags */

	current_offset = HEADER_SIZE;

	/* write 0 offsets table */
	for (i = 0; i < MAX_OFFSETS; ++i) {
		offsets_table[i] = 0;
		output.writeUint32LE(offsets_table[i]);
		current_offset += 4;
	}

	/* process 'OBJ' file */
	inpath->setFullName("OBJ");
	Common::File input(*inpath, "rb");

	offsets_table[0] = current_offset;
	current_offset = compress_sound_data_file(current_offset, output, input, input_OBJ_offs, input_OBJ_size, OBJ_HDR_LEN);
	input.close();
	print("Processed '%s'.\n", inpath->getFullPath().c_str());

	/* process Vxx files */
	for (i = 1; i < MAX_OFFSETS; ++i) {
		updateProgress(i, MAX_OFFSETS);

		char d[16];
		sprintf(d, "V%d", i);
		inpath->setFullName(d);

		try {
			input.open(*inpath, "rb");
			offsets_table[i] = current_offset;
			current_offset = compress_sound_data_file(current_offset, output, input, input_Vxx_offs, input_Vxx_size, Vxx_HDR_LEN);
			input.close();
			print("Processed '%s'.\n", inpath->getFullPath().c_str());
		} catch (...) {
			//print("Skipping '%s'.\n", inpath->getFullPath().c_str());
		}
	}

	/* fix global offsets table at the beginning of the file */
	output.seek(HEADER_SIZE, SEEK_SET);
	for (i = 0; i < MAX_OFFSETS; ++i) {
		output.writeUint32LE(offsets_table[i]);
	}

	output.close();

	/* cleanup */
	Common::removeFile(TEMP_RAW);
	Common::removeFile(tempEncoded);

	print("Done.\n");
}

void CompressTouche::execute() {
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	if (outpath.empty()) {
		switch(_format) {
		case AUDIO_MP3:
			outpath.setFullName(OUTPUT_MP3);
			break;
		case AUDIO_VORBIS:
			outpath.setFullName(OUTPUT_OGG);
			break;
		case AUDIO_FLAC:
			outpath.setFullName(OUTPUT_FLA);
			break;
		default:
			throw ToolException("Unknown audio format");
			break;
		}
	}

	compress_sound_data(&inpath, &outpath);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressTouche touche(argv[0]);
	return touche.run(argc, argv);
}
#endif

