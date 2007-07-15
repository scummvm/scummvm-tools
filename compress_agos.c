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

static void end(char *inputPath, char* inputFile) {
	int size;
	char fbuf[2048];
	char tmp[1024];
	char *p;
	const char *head;

	switch (gCompMode) {
	case kMP3Mode:
		head = "mp3";
		break;
	case kVorbisMode:
		head = "ogg";
		break;
	case kFlacMode:
		head = "fla";
		break;
	default:
		error("Unknown compression mode");
	}

	fclose(output_snd);
	fclose(output_idx);
	fclose(input);

	/* Remove the extension from the filename if it exists
	 * so that we can append the new extension
	*/
	p = strrchr(inputFile, '.');
	if (p) {
		*p = '\0';
	}

	sprintf(tmp, "%s/%s.%s", inputPath, inputFile, head);
	output_idx = fopen(tmp, "wb");

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

void showhelp(char *exename) {
	printf("\nUsage: %s [params] [--mac] <file>\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf(" --mac        encode simon2mac sounds\n");
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
	printf("\nIf converting simon2mac sounds, use the --mac option\n");
	printf("and replace <file> with the path to the 'voices' folder\n");
	printf("If the input directory is the same as the current directory use '.'\n");
	exit(2);
}


static void convert_pc(char *inputPath, char *inputFile) {
	int i, size, num;
	char tmp[1024];
	uint32 filenums[32768];
	uint32 offsets[32768];

	sprintf(tmp, "%s/%s", inputPath, inputFile);
	input = fopen(tmp, "rb");
	if (!input) {
		printf("Cannot open file: %s\n", tmp);
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

static void convert_mac(char *inputPath) {
	int i, size, num;
	char tmp[1024];
	uint32 filenums[32768];
	uint32 offsets[32768];

	sprintf(tmp, "%s/voices.idx", inputPath);
	input = fopen(tmp, "rb");
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
			sprintf(tmp, "%s/voices%d.dat", inputPath, filenums[i]);

			if (input) {
				fclose(input);
			}

			input = fopen(tmp, "rb");
			if (!input) {
				printf("Cannot open file: %s\n", tmp);
				exit(-1);
			}
		}

		size += get_sound(offsets[i]);

		if (i < num - 1) {
			writeUint32LE(output_idx, size);
		}
	}
}

int main(int argc, char *argv[]) {
	int i;
	char *p;
	char inputFile[256];
	char inputPath[768];
	bool convertMac = false;

	if (argc < 2) {
		showhelp(argv[0]);
	}

	/* compression mode */
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

	if (strcmp(argv[2], "--mac") == 0) {
		convertMac = true;
		i++;
	}

	switch (gCompMode) {
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

	/* Find the last occurence of '/' or '\'
	 * Everything before this point is the path
	 * Everything after this point is the filename
	 */
	p = strrchr(argv[argc - 1], '/');
	if (!p) {
		p = strrchr(argv[argc - 1], '\\');

		if (!p) {
			p = argv[argc - 1] - 1;
		}
	}

	/* For simon2mac the filename defaults to "simon2" and the path is the whole arguement
	 * Otherwise, the filename is everything after p and the path is everything before p,
	 * unless the file is in the current directory, in which case the path is '.'
	 */
	if (convertMac) {
		strcpy(inputFile, "simon2");
		strcpy(inputPath, argv[argc - 1]);
	} else if (p < argv[argc - 1]) {
		strcpy(inputFile, p + 1);
		strcpy(inputPath, ".");
	} else {
		strcpy(inputFile, p + 1);
		strncpy(inputPath, argv[argc - 1], p - argv[argc - 1]);
	}

	if (convertMac) {
		convert_mac(inputPath);
	} else {
		convert_pc(inputPath, inputFile);
	}

	end(inputPath, inputFile);

	return(0);
}

