/* compress_saga - Compress SAGA engine digital sound files into
 * MP3 and Ogg Vorbis format
 * Copyright (C) 2004, Marcoen Hirschberg
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

#include <stdio.h>
#include "compress.h"

typedef struct RECORD {
	uint32 offset;
	uint32 size;
} RECORD;

static CompressMode gCompMode = kMP3Mode;

#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8

void sagaEncode(char *infile) {
	FILE *res_file;
	FILE *outputfile;

	uint32 res_tbl_ct;
	uint32 res_tbl_offset;
	uint32 res_size;
	uint32 outtable_offset;

	uint32 t;

	struct RECORD  *table;
	struct RECORD  *outtable;
	int length;
	FILE *tempf;
	char fbuf[2048];
	const char *output;
	size_t size;
	bool audio;
	char buf[8];

	res_file = fopen(infile, "rb");
	res_size = fileSize(res_file);
	printf("filesize: %ul\n", res_size);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	fseek(res_file, res_size - RSC_TABLEINFO_SIZE, SEEK_SET);

	res_tbl_offset = readUint32LE(res_file);
	res_tbl_ct = readUint32LE(res_file);

	printf("tabel offset: %ul\nnumber of records: %ul\n", res_tbl_offset, res_tbl_ct);

	if (res_tbl_offset != res_size - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * res_tbl_ct) {
		printf("Something's wrong with your resource file..\n");
		exit(2);

	}
	/* Go to beginning of the table */
	fseek(res_file, res_tbl_offset, SEEK_SET);

	table = malloc(res_tbl_ct * sizeof(struct RECORD));
	outtable = malloc(res_tbl_ct * sizeof(struct RECORD));

	/* Put offsets of all the records in a table */
	for (t = 0; t < res_tbl_ct; t++) {

		table[t].offset = readUint32LE(res_file);
		table[t].size = readUint32LE(res_file);

		printf("record: %ul, offset: %ul, size: %ul\n", t, table[t].offset, table[t].size);

		if ((table[t].offset > res_size) ||
		    (table[t].size > res_size)) {
			printf("The offset points outside the file!");
			exit(2);
		}

	}


	outputfile = fopen("out.res", "wb");

	for (t = 0; t < res_tbl_ct; t++) {

		audio = 0;
		fseek(res_file, table[t].offset, SEEK_SET);
		fread(buf, 1, 8, res_file);
		if (memcmp(buf, "RIFF", 4) == 0) {
			printf("Wave file: ");
			audio = -1;
		} else if (memcmp(buf, "FORM", 4) == 0) {
			printf("XMIDI file: ");
		} else if (memcmp(buf, "Ogg", 3) == 0) {
			printf("Ogg file: ");
		} else {
			printf("unknown file: ");
		}
		length = table[t].size;
		fseek(res_file, table[t].offset, SEEK_SET);
		/* Copy the WAV data to a temporary file */
		tempf = fopen(TEMP_WAV, "wb");
		while (length > 0) {
			size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, res_file);
			if (size <= 0)
				break;
			length -= size;
			fwrite(fbuf, 1, size, tempf);
		}
		fclose(tempf);

		if (audio) {
		/* Convert the WAV temp file to OGG/MP3 */
			encodeAudio(TEMP_WAV, false, -1, tempEncoded, gCompMode);
			output = tempEncoded;
		} else {
			output = TEMP_WAV;
		}
		tempf = fopen(output, "rb");
		outtable[t].offset = ftell(outputfile);
		printf("Offset: %ul, ", outtable[t].offset);
		while ((size = fread(fbuf, 1, 2048, tempf)) > 0) {
			fwrite(fbuf, 1, size, outputfile);
		}
		outtable[t].size = ftell(tempf);
		printf("Size: %ul\n", outtable[t].size);
		fclose(tempf);

	}
	outtable_offset = ftell(outputfile);
	for (t = 0; t < res_tbl_ct; t++) {
		writeUint32LE(outputfile, outtable[t].offset);
		writeUint32LE(outputfile, outtable[t].size);
	}
	writeUint32LE(outputfile, outtable_offset);
	writeUint32LE(outputfile, res_tbl_ct);	/* Should be the same number of entries */

	fclose(outputfile);

	free(table);
	free(outtable);
	fclose(res_file);
	printf("Done!\n");
}

void showhelp(char *exename) {
	printf("\nUsage: %s <params> [<file> | mac]\n", exename);

	printf("\nParams:\n");

	printf("--mp3        encode to MP3 format (default)\n");
	printf("--vorbis     encode to Vorbis format\n");
	printf("--flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf("-b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf("-m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf("-M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf("-q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf("--silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf("[params]     optional Arguments passed to the Encoder\n");
	printf("             recommended is: --best -b 1152\n");

	printf("\n--help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	int		i;

	if (argc < 2)
		showhelp(argv[0]);

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
	switch (gCompMode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	i = argc - 1;

	sagaEncode(argv[i]);

	return (0);
}
