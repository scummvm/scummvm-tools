/* compress_simon - Compress Simon the Sorcerer 1/2 digital sound files into MP3-format
 * Copyright (C) 2004-2005  The ScummVM Team
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

#include "compress.h"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;

static char infile_base[256];

static void end(void)
{
	int size;
	char fbuf[2048];
	char tmp[256];
	const char *head;

	switch (gCompMode) {
	case kMP3Mode:
		head = "mp3"; break;
	case kVorbisMode:
		head = "ogg"; break;
	case kFlacMode:
		head = "fla"; break;
	default:
		error("Unknown compression mode");
	}

	fclose(output_snd);
	fclose(output_idx);
	fclose(input);

	sprintf(tmp, "%s.%s", infile_base, head);
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

	
static int get_offsets(uint32 filenums[], uint32 offsets[])
{
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

static int get_offsets_mac(uint32 filenums[], uint32 offsets[])
{
	int i, size;
	fseek(input, 0, SEEK_END);
	size = ftell(input);
	fseek(input, 0, SEEK_SET);

	for (i = 1; i <= size / 6; i++) {
		filenums[i] = readUint16BE(input);
		offsets[i] = readUint32BE(input);
	}
	return(size/6);
}


static uint32 get_sound(uint32 offset)
{
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

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> [<file> | mac]\n", exename);

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
	printf(" [params]     optional arguments passed directly to the encoder\n");
	printf("              recommended is: --best -b 1152\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	printf("Use the `mac' option instead of a filename if converting simon2mac sounds\n");
	exit(2);
}


static void convert_pc(char *infile)
{
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];
	char *p;

	p = strrchr(infile, '/');
	if (!p) {
		p = strrchr(infile, '\\');
		if (!p) {
			p = infile - 1;
		}
	}
	strcpy(infile_base, p + 1);
	p = strrchr(infile_base, '.');
	if (p) {
		*p = '\0';
	}

	input = fopen(infile, "rb");
	if (!input) {
		printf("Cannot open file: %s\n", infile);
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
	size = num*4;

	writeUint32LE(output_idx, 0);
	writeUint32LE(output_idx, size);

	for (i = 1; i < num; i++) {
		if (offsets[i] == offsets[i+1]) {
			writeUint32LE(output_idx, size);
			continue;
		}

		size += get_sound(offsets[i]);
		if (i < num - 1)
			writeUint32LE(output_idx, size);
	}
}

static void convert_mac(void)
{
	int i, size, num;
	char tmp[256];
	uint32 filenums[32768];
	uint32 offsets[32768];
	
	sprintf(infile_base, "simon2");

	input = fopen("voices.idx", "rb");
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
	size = num*4;

	writeUint32LE(output_idx, 0);
	writeUint32LE(output_idx, size);

	for (i = 1; i < num; i++) {
		if (filenums[i] == filenums[i+1] && offsets[i] == offsets[i+1]) {
			writeUint32LE(output_idx, size);
			continue;
		}

		if (filenums[i] != filenums[i-1]) {
			sprintf(tmp, "voices%d.dat", filenums[i]);
			if (input)
				fclose(input);
			input = fopen(tmp, "rb"); 
			if (!input) {
				printf("Cannot open file: %s\n", tmp);
				exit(-1);
			}
		}

		size += get_sound(offsets[i]);
		if (i < num - 1)
			writeUint32LE(output_idx, size);
	}
}

int main(int argc, char *argv[])
{
	int i;
	
	if (argc < 2)
		showhelp(argv[0]);

	/* compression mode */
	gCompMode = kMP3Mode;
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	}
	else if (strcmp(argv[1], "--flac") == 0) {
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

	if (strcmp(argv[i], "mac") == 0) {
		convert_mac();
	} else {
		convert_pc(argv[i]);
	}

	end();

	return(0);
}

