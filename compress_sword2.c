/* compress_sword2 - Compress Broken Sword II sound clusters into MP3/Ogg Vorbis
 * Copyright (C) 2002, 2003  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include "extract.h"

#define TEMP_IDX	"tempfile.idx"
#define TEMP_DAT	"tempfile.dat"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> file.clu\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%i)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%i)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%i)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%i)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%i)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf(" [params]     optional arguments passed directly to the encoder\n");
	printf("              recommended is: --best -b 1152\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

uint32 append_to_file(FILE *f1, const char *filename) {
	FILE *f2;
	uint32 length, orig_length;
	size_t size;
	char fbuf[2048];

	f2 = fopen(filename, "rb");
	if (!f2) {
		printf("Can't open file %s for reading!\n", filename);
		exit(-1);
	}

	orig_length = length = fileSize(f2);

	while (length > 0) {
		size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, f2);
		if (size <= 0)
			break;
		length -= size;
		fwrite(fbuf, 1, size, f1);
	}

	fclose(f2);
	return orig_length;
}

#define GetCompressedShift(n)      ((n) >> 4)
#define GetCompressedSign(n)       (((n) >> 3) & 1)
#define GetCompressedAmplitude(n)  ((n) & 7)

int main(int argc, char *argv[]) {
	char output_filename[40];
	FILE *output, *f;
	char *ptr;
	int i, j;
	uint32 indexSize;
	uint32 totalSize;
	uint32 length;
	
	if (argc < 2)
		showhelp(argv[0]);
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	} else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		process_mp3_parms(argc, argv, i);
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		process_ogg_parms(argc, argv, i);
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		process_flac_parms(argc, argv, i);
		break;
	}

	i = argc - 1;

	input = fopen(argv[i], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		return EXIT_FAILURE;
	}

	indexSize = readUint32LE(input);
	totalSize = 12 * (indexSize + 1);

	if (readUint32BE(input) != 0xfff0fff0) {
		printf("This doesn't look like a cluster file\n");
		return EXIT_FAILURE;
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for writing!\n");
		return EXIT_FAILURE;
	}

	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		printf("Can't open file " TEMP_DAT " for writing!\n");
		return EXIT_FAILURE;
	}

	writeUint32LE(output_idx, indexSize);
	writeUint32BE(output_idx, 0xfff0fff0);
	writeUint32BE(output_idx, 0xfff0fff0);

	for (j = strlen(argv[i]) - 1; j >= 0; j--) {
		if (argv[i][j] == '/' || argv[i][j] == '\\' || argv[i][j] == ':') {
			j++;
			break;
		}
	}

	if (j < 0)
		j = 0;

	strncpy(output_filename, argv[i] + j, sizeof(output_filename) - 1);
	output_filename[sizeof(output_filename) - 1] = 0;

	ptr = output_filename + strlen(output_filename) - 1;

	switch (gCompMode) {
	case kMP3Mode:
		*ptr = '3';
		break;
	case kVorbisMode:
		*ptr = 'g';
		break;
	case kFlacMode:
		*ptr = 'f';
		break;
	}

	for (i = 0; i < indexSize; i++) {
		uint32 pos;
		uint32 enc_length;

		fseek(input, 8 * (i + 1), SEEK_SET);

		pos = readUint32LE(input);
		length = readUint32LE(input);

		if (pos != 0 && length != 0) {
			uint16 prev;

			f = fopen(TEMP_WAV, "wb");
			if (!f) {
				printf("Can't open file %s for writing!\n", TEMP_WAV);
				return EXIT_FAILURE;
			}

			/*
			 * Our encoding function assumes that raw data means
			 * 8-bit data. Rather than going through the trouble of
			 * adding support for 16-bit data at various byte
			 * orders, let's just prepend a simple WAV header.
			 */

			writeUint32BE(f, 0x52494646);	/* "RIFF" */
			writeUint32LE(f, 2 * length + 36);
			writeUint32BE(f, 0x57415645);	/* "WAVE" */
			writeUint32BE(f, 0x666d7420);	/* "fmt " */
			writeUint32LE(f, 16);
			writeUint16LE(f, 1);		/* PCM */
			writeUint16LE(f, 1);		/* mono */
			writeUint32LE(f, 22050);	/* sample rate */
			writeUint32LE(f, 44100);	/* bytes per second */
			writeUint16LE(f, 2);		/* basic block size */
			writeUint16LE(f, 16);		/* sample width */
			writeUint32BE(f, 0x64617461);	/* "data" */
			writeUint32LE(f, 2 * length);

			fseek(input, pos, SEEK_SET);

			/*
			 * The first sample is stored uncompressed. Subsequent
			 * samples are stored as some sort of 8-bit delta.
			 */

			prev = readUint16LE(input);

			writeUint16LE(f, prev);

			for (j = 1; j < length; j++) {
				byte data;
				uint16 out;

				data = readByte(input);
				if (GetCompressedSign(data))
					out = prev - (GetCompressedAmplitude(data) << GetCompressedShift(data));
				else
					out = prev + (GetCompressedAmplitude(data) << GetCompressedShift(data));

				writeUint16LE(f, out);
				prev = out;
			}
			fclose(f);

			encodeAudio(TEMP_WAV, false, -1, tempEncoded, gCompMode);
			enc_length = append_to_file(output_snd, tempEncoded);

			writeUint32LE(output_idx, totalSize);
			writeUint32LE(output_idx, length);
			writeUint32LE(output_idx, enc_length);
			totalSize = totalSize + enc_length;
		} else {
			writeUint32LE(output_idx, 0);
			writeUint32LE(output_idx, 0);
			writeUint32LE(output_idx, 0);
		}
	}

	fclose(output_idx);
	fclose(output_snd);

	output = fopen(output_filename, "wb");
	if (!output) {
		printf("Can't open file %s for writing!\n", output_filename);
		return EXIT_FAILURE;
	}

	append_to_file(output, TEMP_IDX);
	append_to_file(output, TEMP_DAT);

	fclose(output);
	unlink(TEMP_DAT);
	unlink(TEMP_IDX);
	unlink(TEMP_MP3);
	unlink(TEMP_OGG);
	unlink(TEMP_FLAC);
	unlink(TEMP_WAV);

	return EXIT_SUCCESS;
}
