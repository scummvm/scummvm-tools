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

#include "compress.h"

#define CURRENT_VER     1
#define HEADER_SIZE     4
#define MAX_OFFSETS   140
#define OBJ_HDR_LEN   200
#define Vxx_HDR_LEN  1024

#define OUTPUT_MP3   "TOUCHE.SO3"
#define OUTPUT_OGG   "TOUCHE.SOG"
#define OUTPUT_FLA   "TOUCHE.SOF"

static CompressMode gCompMode = kMP3Mode;

static uint32 input_OBJ_offs[OBJ_HDR_LEN];
static uint32 input_OBJ_size[OBJ_HDR_LEN];
static uint32 input_Vxx_offs[Vxx_HDR_LEN];
static uint32 input_Vxx_size[Vxx_HDR_LEN];

static uint32 compress_sound_data_file(uint32 current_offset, FILE *output, FILE *input, uint32 *offs_table, uint32 *size_table, int len) {
	FILE *temp;
	int i, size;
	uint8 buf[2048];
	uint32 start_offset = current_offset;

	/* write 0 offsets/sizes table */
	for (i = 0; i < len; ++i) {
		offs_table[i] = readUint32LE(input);
		size_table[i] = readUint32LE(input);
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
		current_offset += 8;
	}
	for (i = 0; i < len; ++i) {
		if (size_table[i] == 0) {
			offs_table[i] = 0;
		} else {
			fseek(input, offs_table[i], SEEK_SET);
			fread(buf, 1, 8, input);

			if (memcmp(buf, "Creative", 8) != 0) {
				error("Invalid VOC data found");
			}

			printf("VOC found (pos = %d) :\n", offs_table[i]);
			fseek(input, 18, SEEK_CUR);
			extractAndEncodeVOC(TEMP_RAW, input, gCompMode);

			/* append converted data to output file */
			temp = fopen(tempEncoded, "rb");
			if (!temp) {
				error("Cannot open file '%s' for reading", tempEncoded);
			}

			size_table[i] = 0;

			while ((size = fread(buf, 1, 2048, temp)) > 0) {
				fwrite(buf, 1, size, output);
				size_table[i] += size;
			}

			fclose(temp);
			offs_table[i] = current_offset;
			current_offset += size_table[i];
		}
	}

	/* fix data offsets table */
	fseek(output, start_offset, SEEK_SET);
	for (i = 0; i < len; ++i) {
		writeUint32LE(output, offs_table[i]);
		writeUint32LE(output, size_table[i]);
	}
	fseek(output, 0, SEEK_END);

	return current_offset;
}

static void compress_sound_data(Filename *inpath, Filename *outpath) {
	int i;
	FILE *output, *input;
	uint32 current_offset;
	uint32 offsets_table[MAX_OFFSETS];

	output = fopen(outpath->getFullPath(), "wb");
	if (!output) {
		error("Cannot open file '%s' for writing", outpath->getFullPath());
	}

	writeUint16LE(output, 1); /* current version */
	writeUint16LE(output, 0); /* flags */

	current_offset = HEADER_SIZE;

	/* write 0 offsets table */
	for (i = 0; i < MAX_OFFSETS; ++i) {
		offsets_table[i] = 0;
		writeUint32LE(output, offsets_table[i]);
		current_offset += 4;
	}

	/* process 'OBJ' file */
	inpath->setFullName("OBJ");
	input = fopen(inpath->getFullPath(), "rb");
	if (!input) {
		error("Cannot open file 'OBJ' for reading");
	}

	offsets_table[0] = current_offset;
	current_offset = compress_sound_data_file(current_offset, output, input, input_OBJ_offs, input_OBJ_size, OBJ_HDR_LEN);
	fclose(input);
	printf("Processed '%s'.\n", inpath->getFullPath());

	/* process Vxx files */
	for (i = 1; i < MAX_OFFSETS; ++i) {

		char d[16];
		sprintf(d, "V%d", i);
		inpath->setFullName(d);

		input = fopen(inpath->getFullPath(), "rb");
		if (input) {
			offsets_table[i] = current_offset;
			current_offset = compress_sound_data_file(current_offset, output, input, input_Vxx_offs, input_Vxx_size, Vxx_HDR_LEN);
			fclose(input);
			printf("Processed '%s'.\n", inpath->getFullPath());
		}
	}

	/* fix global offsets table at the beginning of the file */
	fseek(output, HEADER_SIZE, SEEK_SET);
	for (i = 0; i < MAX_OFFSETS; ++i) {
		writeUint32LE(output, offsets_table[i]);
	}

	fclose(output);

	/* cleanup */
	unlink(TEMP_RAW);
	unlink(tempEncoded);

	printf("Done.\n");
}

const char *helptext = "\nUsage: %s [params] [-o outputfile TOUCHE.*] <inputdir>\n* differs with compression type.\n" kCompressionAudioHelp;

int main(int argc, char *argv[]) {
	Filename inpath, outpath;
	int first_arg = 1;
	int last_arg = argc - 1;

	parseHelpArguments(argv, argc, helptext);

	// compression mode
	gCompMode = process_audio_params(argc, argv, &first_arg);

	if(gCompMode == kNoAudioMode) {
		// Unknown mode (failed to parse arguments), display help and exit
		displayHelp(helptext, argv[0]);
	}

	// Now we try to find the proper output file
	// also make sure we skip those arguments
	if (parseOutputFileArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputFileArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else {
		switch(gCompMode) {
		case kMP3Mode:
			outpath.setFullName(OUTPUT_MP3);
			break;
		case kVorbisMode:
			outpath.setFullName(OUTPUT_OGG);
			break;
		case kFlacMode:
			outpath.setFullName(OUTPUT_FLA);
			break;
		default:
			displayHelp(helptext, argv[0]);
			break;
		}
	}

	inpath.setFullPath(argv[first_arg]);

	// Append '/' if it's not already done
	// TODO: We need a way to detect a directory here!
	size_t s = strlen(inpath._path);
	if(inpath._path[s-1] == '/' || inpath._path[s-1] == '\\') {
		inpath._path[s] = '/';
		inpath._path[s+1] = '\0';
	}

	compress_sound_data(&inpath, &outpath);
	return 0;
}
