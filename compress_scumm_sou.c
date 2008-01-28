/* compress_scumm_sou - monster.sou to MP3-compressed monster.so3 converter
 * Copyright (C) 2002-2006  The ScummVM Team
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


static const char f_hdr[] = {
	'S', 'O', 'U', ' ', 0, 0, 0, 0, 0
};

#define OUTPUT_MP3	"monster.so3"
#define OUTPUT_OGG	"monster.sog"
#define OUTPUT_FLAC	"monster.sof"

static const char *outputName = OUTPUT_MP3;

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;


void end_of_file(char *inputPath) {
	FILE *in;
	int idx_size = ftell(output_idx);
	size_t size;
	char tmp[1024];
	char buf[2048];

	fclose(output_snd);
	fclose(output_idx);

	sprintf(tmp, "%s/%s", inputPath, outputName);
	output_idx = fopen(tmp , "wb");
	writeUint32BE(output_idx, (uint32)idx_size);

	in = fopen(TEMP_IDX, "rb");
	while ((size = fread(buf, 1, 2048, in)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(in);
	in = fopen(TEMP_DAT, "rb");
	while ((size = fread(buf, 1, 2048, in)) > 0) {
		fwrite(buf, 1, size, output_idx);
	}
	fclose(in);
	fclose(output_idx);
	fclose(input);

	/* And some clean-up :-) */
	unlink(TEMP_IDX);
	unlink(TEMP_DAT);
	unlink(TEMP_RAW);
	unlink(tempEncoded);

	exit(-1);
}

void append_byte(int size, char buf[]) {
	int i;
	for (i = 0; i < (size - 1); i++)
		buf[i] = buf[i + 1];
	buf[i] = fgetc(input);
}

void get_part(char *inputPath) {
	FILE *f;
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];

	char buf[2048];
	int pos = ftell(input);
	uint32 tags;

	/* Scan for the VCTL header */
	fread(buf, 1, 4, input);
	/* The demo (snmdemo) and floppy version of Sam & Max use VTTL */
	while (memcmp(buf, "VCTL", 4)&&memcmp(buf, "VTTL", 4)) {
		pos++;
		append_byte(4, buf);
		if (feof(input))
			end_of_file(inputPath);
	}

	tags = readUint32BE(input);
	if (tags < 8)
		exit(-1);
	tags -= 8;

	writeUint32BE(output_idx, (uint32)pos);
	writeUint32BE(output_idx, (uint32)ftell(output_snd));
	writeUint32BE(output_idx, tags);
	while (tags > 0) {
		fputc(fgetc(input), output_snd);
		tags--;
	}

	fread(buf, 1, 8, input);
	if (!memcmp(buf, "Creative", 8))
		fseek(input, 18, SEEK_CUR);
	else if (!memcmp(buf, "VTLK", 4))
		fseek(input, 26, SEEK_CUR);
	else
		error("Unexpected data encountered");
	printf("Voice file found (pos = %d) :", pos);

	/* Convert the VOC data */
	extractAndEncodeVOC(TEMP_RAW, input, gCompMode);

	/* Append the converted data to the master output file */
	sprintf(outname, tempEncoded);
	f = fopen(outname, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, output_snd);
	}
	fclose(f);

	writeUint32BE(output_idx, tot_size);
}

void showhelp(char *exename) {
	printf("\nUsage: %s [params] monster.sou\n", exename);

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
	char inputPath[768];
	char buf[2048];
	int i;

	if (argc < 2) {
		showhelp(argv[0]);
	}

	/* Compression mode */
	gCompMode = kMP3Mode;
	i = 1;

	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	} else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	} else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		outputName = OUTPUT_MP3;
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		outputName = OUTPUT_OGG;
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		outputName = OUTPUT_FLAC;
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	getPath(argv[argc - 1], inputPath);

	input = fopen(argv[argc - 1], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		exit(-1);
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for write!\n" );
		exit(-1);
	}
	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		printf("Can't open file " TEMP_DAT " for write!\n");
		exit(-1);
	}

	/* Get the 'SOU ....' header */
	fread(buf, 1, 8, input);
	if (strncmp(buf, f_hdr, 8)) {
		printf("Bad SOU\n");
		exit(-1);
	}

	while (1)
		get_part(inputPath);

	return 0;
}
