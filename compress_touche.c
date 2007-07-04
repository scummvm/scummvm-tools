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

static CompressMode g_mode = kMP3Mode;
static const char *g_output_filename = OUTPUT_MP3;
static const char *g_input_directory = NULL;

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
			extractAndEncodeVOC(TEMP_RAW, input, g_mode);

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

static void compress_sound_data() {
	int i;
	char filepath[512];
	FILE *output, *input;
	uint32 current_offset;
	uint32 offsets_table[MAX_OFFSETS];

	output = fopen(g_output_filename, "wb");
	if (!output) {
		error("Cannot open file '%s' for writing", g_output_filename);
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
	sprintf(filepath, "%s/OBJ", g_input_directory);
	input = fopen(filepath, "rb");
	if (!input) {
		error("Cannot open file 'OBJ' for reading");
	}
	offsets_table[0] = current_offset;
	current_offset = compress_sound_data_file(current_offset, output, input, input_OBJ_offs, input_OBJ_size, OBJ_HDR_LEN);
	fclose(input);
	printf("Processed '%s'.\n", filepath);

	/* process Vxx files */
	for (i = 1; i < MAX_OFFSETS; ++i) {
		sprintf(filepath, "%s/V%d", g_input_directory, i);
		input = fopen(filepath, "rb");
		if (input) {
			offsets_table[i] = current_offset;
			current_offset = compress_sound_data_file(current_offset, output, input, input_Vxx_offs, input_Vxx_size, Vxx_HDR_LEN);
			fclose(input);
			printf("Processed '%s'.\n", filepath);
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

static void showhelp(const char *exename) {
	printf("\nUsage: %s [params] <inputdir>\n", exename);

	printf("\nParams:\n");

	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf(" --fast       FLAC uses compresion level 0\n");
	printf(" --best       FLAC uses compresion level 8\n");
	printf(" -<value>     specifies the value (0 - 8) of compresion (8=best)(default:%d)\n", flacCompressDef);
	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	int i;

	if (argc < 2) {
		showhelp(argv[0]);
	}

	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		g_mode = kMP3Mode;
		g_output_filename = OUTPUT_MP3;
		++i;
	} else if (strcmp(argv[1], "--vorbis") == 0) {
		g_mode = kVorbisMode;
		g_output_filename = OUTPUT_OGG;
		++i;
	} else if (strcmp(argv[1], "--flac") == 0) {
		g_mode = kFlacMode;
		g_output_filename = OUTPUT_FLA;
		++i;
	}

	g_input_directory = argv[argc - 1];

	switch (g_mode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	}

	compress_sound_data();
	return 0;
}
