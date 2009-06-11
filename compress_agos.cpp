/* compress_agos - Compress Simon the Sorcerer 1/2 digital sound files into MP3-format
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

#include "compress.h"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;

static void end(Filename *inputPath) {
	int size;
	char fbuf[2048];

	inputPath->setExtension(audio_extensions[gCompMode]);

	fclose(output_snd);
	fclose(output_idx);
	fclose(input);

	output_idx = fopen(inputPath->getFullPath(), "wb");

	input = fopen(TEMP_IDX, "rb");
	while ((size = fread(fbuf, 1, 2048, input)) > 0) {
		fwrite(fbuf, 1, size, output_idx);
	}

	fclose(input);

	input = fopen(TEMP_DAT, "rb");
	while ((size = fread(fbuf, 1, 2048, input)) > 0) {
		fwrite(fbuf, 1, size, output_idx);
	}

	fclose(input);
	fclose(output_idx);

	/* And some clean-up :-) */
	unlink(TEMP_IDX);
	unlink(TEMP_DAT);
	unlink(TEMP_RAW);
	unlink(tempEncoded);
	unlink(TEMP_WAV);

	exit(0);
}


static int get_offsets(uint32 filenums[], uint32 offsets[]) {
	int i;
	char buf[8];

	for (i = 0;; i++) {
		fread(buf, 1, 8, input);
		if (!memcmp(buf, "Creative", 8) || !memcmp(buf, "RIFF", 4)) {
			return i;
		}
		fseek(input, -8, SEEK_CUR);

		offsets[i] = readUint32LE(input);
	}
}

static int get_offsets_mac(uint32 filenums[], uint32 offsets[]) {
	int i, size;
	size = fileSize(input);

	for (i = 1; i <= size / 6; i++) {
		filenums[i] = readUint16BE(input);
		offsets[i] = readUint32BE(input);
	}

	return(size/6);
}


static uint32 get_sound(uint32 offset) {
	FILE *f;
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];
	char buf[8];

	fseek(input, offset, SEEK_SET);

	fread(buf, 1, 8, input);
	if (!memcmp(buf, "Creative", 8)) {
		printf("VOC found (pos = %d) :\n", offset);
		fseek(input, 18, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, input, gCompMode);
	} else if (!memcmp(buf, "RIFF", 4)) {
		printf("WAV found (pos = %d) :\n", offset);
		extractAndEncodeWAV(TEMP_WAV, input, gCompMode);
	} else {
		error("Unexpected data at offset: %d", offset);
	}

	/* Append the converted data to the master output file */
	sprintf(outname, tempEncoded);
	f = fopen(outname, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, output_snd);
	}
	fclose(f);

	return(tot_size);
}


static void convert_pc(Filename* inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	input = fopen(inputPath->getFullPath(), "rb");
	if (!input) {
		printf("Cannot open file: %s\n", inputPath->getFullPath());
		exit(-1);
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for write!\n" );
		exit(-1);
	}

	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		printf("Can't open file " TEMP_DAT " for write!\n" );
		exit(-1);
	}

	num = get_offsets(filenums, offsets);
	if (!num) {
		printf("This does not seem to be a valid file\n");
		exit(-1);
	}
	size = num * 4;

	writeUint32LE(output_idx, 0);
	writeUint32LE(output_idx, size);

	for (i = 1; i < num; i++) {
		if (offsets[i] == offsets[i + 1]) {
			writeUint32LE(output_idx, size);
			continue;
		}

		if (offsets[i] != 0)
			size += get_sound(offsets[i]);
		if (i < num - 1)
			writeUint32LE(output_idx, size);
	}
}

static void convert_mac(Filename *inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	inputPath->setFullName("voices.idx");
	input = fopen(inputPath->getFullPath(), "rb");
	if (!input) {
		printf("Cannot open file: %s\n", "voices.idx");
		exit(-1);
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for write!\n" );
		exit(-1);
	}

	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		printf("Can't open file " TEMP_DAT " for write!\n" );
		exit(-1);
	}

	num = get_offsets_mac(filenums, offsets);
	if (!num) {
		printf("This does not seem to be a valid file\n");
		exit(-1);
	}
	size = num * 4;

	writeUint32LE(output_idx, 0);
	writeUint32LE(output_idx, size);

	for (i = 1; i < num; i++) {
		if (filenums[i] == filenums[i + 1] && offsets[i] == offsets[i + 1]) {
			writeUint32LE(output_idx, size);
			continue;
		}

		if (filenums[i] != filenums[i - 1]) {
			char filename[256];
			sprintf(filename, "voices%d.dat", filenums[i]);
			inputPath->setFullName(filename);

			if (input) {
				fclose(input);
			}

			input = fopen(inputPath->getFullPath(), "rb");
			if (!input) {
				printf("Cannot open file: %s\n", inputPath->getFullPath());
				exit(-1);
			}
		}

		size += get_sound(offsets[i]);

		if (i < num - 1) {
			writeUint32LE(output_idx, size);
		}
	}
}

const char *helptext = "\nUsage: %s [params] [--mac] <file>\n" kCompressionAudioHelp;

int main(int argc, char *argv[]) {
	bool convertMac = false;

	Filename inpath;
	int first_arg = 1;
//	int last_arg = argc - 1;

	parseHelpArguments(argv, argc, helptext);

	gCompMode = process_audio_params(argc, argv, &first_arg);

	if(gCompMode == kNoAudioMode) {
		// Unknown mode (failed to parse arguments), display help and exit
		printf(helptext, argv[0]);
		exit(2);
	}

	inpath.setFullPath(argv[first_arg]);

	if (convertMac) {
		convert_mac(&inpath);
		inpath.setFullName("simon2");
	} else {
		convert_pc(&inpath);
	}

	end(&inpath);

	return(0);
}

