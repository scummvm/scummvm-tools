/* Extract - monster.sou to MP3-compressed monster.so3 converter
 * Copyright (C) 2002, 2003  The ScummVM Team
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include "extract.h"


char f_hdr[] = {
	'S', 'O', 'U', ' ', 0, 0, 0, 0, 0
};

byte v_hdr[] = {
	'V', 'C', 'T', 'L', 0, 0, 0, 0xA, 0xF, 0xFF
};

char c_hdr[] = {
	'C', 'r', 'e', 'a', 't', 'i', 'v', 'e', ' ', 'V', 'o', 'i', 'c', 'e',
	' ', 'F', 'i', 'l', 'e', 0x1a, 0x1a, 0x00, 0x0A, 0x01, 0x29, 0x11
};

#define OUTPUT_MP3	"monster.so3"
#define OUTPUT_OGG	"monster.sog"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"


void end_of_file(void)
{
	FILE *in;
	int idx_size = ftell(output_idx);
	size_t size;
	char buf[2048];

	fclose(output_snd);
	fclose(output_idx);

	output_idx = fopen(oggmode ? OUTPUT_OGG : OUTPUT_MP3, "wb");
	writeUint32BE(input, (uint32)idx_size);

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
	unlink(oggmode ? TEMP_OGG : TEMP_MP3);
	
	exit(-1);
}

void get_string(uint32 size, char buf[])
{
	uint32 i = 0;
	while (i < size) {
		int c = fgetc(input);
		if (c == EOF)
			end_of_file();
		buf[i++] = c;
	}
	buf[i] = '\0';
}

void append_byte(int size, char buf[])
{
	int i;
	int c;
	for (i = 0; i < (size - 1); i++)
		buf[i] = buf[i + 1];
	c = fgetc(input);
	if (c == EOF)
		end_of_file();
	buf[i] = c;
}

void get_part(void)
{
	FILE *f;
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];

	char buf[2048];
	int pos = ftell(input);
	uint32 tags;

	/* The VCTL header */
	get_string(4, buf);
	while (strncmp(buf, "VCTL", 4)) {
		pos++;
		append_byte(4, buf);
	}
	tags = readUint32BE(input);
	if (tags < 8)
		exit(-1);
	tags -= 8;

	writeUint32BE(input, (uint32)pos);
	writeUint32BE(input, (uint32)ftell(output_snd));
	writeUint32BE(input, tags);
	while (tags > 0) {
		fputc(fgetc(input), output_snd);
		tags--;
	}

	get_string(8, buf);
	if (!memcmp(buf, "Creative", 8)) {
		get_string(18, buf);
	} else if (!memcmp(buf, "VTLK", 4)) {
		get_string(26, buf);
	} else {
		error("Unexpected data encountered");
	}
	printf("Voice file found (pos = %d) :", pos);

	/* Conver the VOC data */
	get_voc();
	
	/* Append the converted data to the master output file */
	sprintf(outname, oggmode ? TEMP_OGG : TEMP_MP3);
	f = fopen(outname, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, output_snd);
	}
	fclose(f);

	writeUint32BE(input, tot_size);
}

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> monster.sou\n", exename);
	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
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
	printf("\n --help     this help message\n");
	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[])
{
	char buf[2048];
	int i;
	if (argc < 2)
		showhelp(argv[0]);
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		oggmode = 0;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		oggmode = 1;
		i++;
	}

	if (oggmode)
		process_ogg_parms(argc, argv, i);
	else
		process_mp3_parms(argc, argv, i);

	i = argc - 1;
	input = fopen(argv[i], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[i]);
		exit(-1);
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for write!\n");
		exit(-1);
	}
	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		printf("Can't open file " TEMP_DAT " for write!\n");
		exit(-1);
	}
	
	/* Get the 'SOU ....' header */
	get_string(8, buf);
	if (strncmp(buf, f_hdr, 8)) {
		printf("Bad SOU\n");
		exit(-1);
	}
	while (1)
		get_part();
	return 0;
}
