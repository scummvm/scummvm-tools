/* Simon2mp3 - Compress Simon the Sorcerer 1/2 digital sound files into MP3-format
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

unsigned int filenums[32768];
unsigned int offsets[32768];

char infile_base[256];
char buf[256];

void end(void)
{
	int size;
	char fbuf[2048];
	char tmp[256];

	fclose(output_snd);
	fclose(output_idx);
	fclose(input);

	sprintf(tmp, "%s%s", infile_base, oggmode ? "ogg" : "mp3");
	output_idx = fopen(tmp, "wb");

	sprintf(tmp, "%sidx", infile_base);
	input = fopen(tmp, "rb");
	while ((size = fread(buf, 1, 2048, input)) > 0) {
		fwrite(fbuf, 1, size, output_idx);
	}
	fclose(input);
	sprintf(tmp, "%sdat", infile_base);
	input = fopen(tmp, "rb");
	while ((size = fread(fbuf, 1, 2048, input)) > 0) {
		fwrite(fbuf, 1, size, output_idx);
	}
	fclose(input);
	fclose(output_idx);

	/* And some clean-up :-) */
	sprintf(tmp, "%sidx", infile_base);
	unlink(tmp);
	sprintf(tmp, "%sdat", infile_base);
	unlink(tmp);
	unlink(TEMP_RAW);
	unlink(oggmode ? TEMP_OGG : TEMP_MP3);
	unlink("tempfile.wav");
	
	exit(0);
}

void get_string(int size)
{
	int i = 0;
	while (i < size) {
		int c = fgetc(input);
		buf[i++] = c;
	}
	buf[i] = '\0';
}

unsigned int get_int32LE(void)
{
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 0; i < 4; i++) {
		c = fgetc(input);
		ret |= c << i*8;
	}
	return ret;
}

unsigned int get_int32BE(void)
{
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 3; i >= 0; i--) {
		c = fgetc(input);
		ret |= c << i*8;
	}
	return ret;
}

unsigned int get_int16BE(void)
{
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 1; i >= 0; i--) {
		c = fgetc(input);
		ret |= c << i*8;
	}
	return ret;
}

void put_int(unsigned int val)
{
	int i;
	for (i = 3; i >= 0; i--) {
		fputc((val << i*8) >> 24, output_idx);
	}
}

int get_offsets(void)
{
	int i;

	for (i = 0;; i++) {
		get_string(8);
		if (!memcmp(buf, "Creative", 8) || !memcmp(buf, "RIFF", 4)) {
			return(i);
		}
		fseek(input, -8, SEEK_CUR);

		offsets[i] = get_int32LE();
	}
}

int get_offsets_mac(void)
{
	int i, size;
	fseek(input, 0, SEEK_END);
	size = ftell(input);
	fseek(input, 0, SEEK_SET);

	for (i = 1; i <= size / 6; i++) {
		filenums[i] = get_int16BE();
		offsets[i] = get_int32BE();
	}
	return(size/6);
}

void get_wav(void);

unsigned int get_sound(int sound)
{
	FILE *f;
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];

	fseek(input, offsets[sound], SEEK_SET);

	get_string(8);
	if (!memcmp(buf, "Creative", 8)) {
		printf("VOC found (pos = %d) :\n", offsets[sound]);
		get_string(18);
		get_voc();
	} else if (!memcmp(buf, "RIFF", 4)) {
		printf("WAV found (pos = %d) :\n", offsets[sound]);
		get_wav();
	} else {
		error("Unexpected data at offset: %i", offsets[sound]);
	}

	/* Append the converted data to the master output file */
	sprintf(outname, oggmode ? TEMP_OGG : TEMP_MP3);
	f = fopen(outname, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, output_snd);
	}
	fclose(f);

	return(tot_size);
}

void get_wav(void) {
	int length;
	FILE *f;
	char fbuf[2048];
	char *tmp;
	size_t size;
	char wavname[256];
	char outname[256];

	fseek(input, -4, SEEK_CUR);
	length = get_int32LE();
	length += 8;
	fseek(input, -8, SEEK_CUR);

	/* Copy the WAV data to a temporary file */
	sprintf(wavname, "tempfile.wav");
	f = fopen(wavname, "wb");
	while (length > 0) {
		size = fread(fbuf, 1, length > 2048 ? 2048 : length, input);
		if (size <= 0)
			break;
		length -= size;
		fwrite(fbuf, 1, size, f);
	}
	fclose(f);

	/* Convert the WAV temp file to OGG/MP3 */
	/* TODO: Unify this with the conversion code in get_voc() */
	sprintf(outname, oggmode ? TEMP_OGG : TEMP_MP3);
	tmp = fbuf;
	if (oggmode) {
		tmp += sprintf(tmp, "oggenc ");
		if (oggparms.nominalBitr != -1)
			tmp += sprintf(tmp, "--bitrate=%i ", oggparms.nominalBitr);
		if (oggparms.minBitr != -1)
			tmp += sprintf(tmp, "--min-bitrate=%i ", oggparms.minBitr);
		if (oggparms.maxBitr != -1)
			tmp += sprintf(tmp, "--max-bitrate=%i ", oggparms.maxBitr);
		if (oggparms.silent)
			tmp += sprintf(tmp, "--quiet ");
		tmp += sprintf(tmp, "--quality=%i ", oggparms.quality);
		tmp += sprintf(tmp, "--output=%s ", outname);
		tmp += sprintf(tmp, "%s ", wavname);
		system(fbuf);
	} else {
		tmp += sprintf(tmp, "lame -t -m m ");
		if (encparms.abr == 1)
			tmp += sprintf(tmp, "--abr %i ", encparms.minBitr);
		else
			tmp += sprintf(tmp, "--vbr-new -b %i ", encparms.minBitr);
		if (encparms.silent == 1)
			tmp += sprintf(tmp, " --silent ");

		tmp += sprintf(tmp, "-q %i ", encparms.algqual);
		tmp += sprintf(tmp, "-V %i ", encparms.vbrqual);
		tmp += sprintf(tmp, "-B %i ", encparms.maxBitr);
		tmp += sprintf(tmp, "%s %s ", wavname, outname);
		system(fbuf);
	}
}

void showhelp(char *exename)
{
	printf("\nUsage: %s <params> [<file> | mac]\n", exename);
	printf("\nParams:\n");
	/*
	printf("--mp3        encode to MP3 format (default)\n");
	printf("--vorbis     encode to Vorbis format (not yet implemented)\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");
	printf("\nMP3 mode params:\n");
	*/
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%i)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%i)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%i)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%i)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");
	/*
	printf("\nVorbis mode params:\n");
	printf("-b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf("-m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf("-M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf("-q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%i)\n", oggqualDef);
	printf("--silent     the output of oggenc is hidden (default:disabled)\n");
	*/
	printf("\n--help     this help message\n");
	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	printf("Use the `mac' option instead of a filename if converting simon2mac sounds\n");
	exit(2);
}


void convert_pc(char *infile)
{
	int i, n, size, num;
	char tmp[256];

	memccpy(infile_base, infile, '.', strlen(infile));
	n = strlen(infile_base);
	if (infile_base[n-1] == '.')
		infile_base[n] = '\0';
	else {
		infile_base[n] = '.';
		infile_base[n + 1] = '\0';
	}

	input = fopen(infile, "rb");
	if (!input) {
		printf("Cannot open file: %s\n", infile);
		exit(-1);
	}

	sprintf(tmp, "%sidx", infile_base);
	output_idx = fopen(tmp, "wb");

	sprintf(tmp, "%sdat", infile_base);
	output_snd = fopen(tmp, "wb");

	num = get_offsets();

	if (!num) {
		printf("This does not seem to be a valid file\n");
		exit(-1);
	}
	size = num*4;

	put_int(0);
	put_int(size);

	for (i = 1; i < num; i++) {
		if (offsets[i] == offsets[i+1]) {
			put_int(size);
			continue;
		}

		size += get_sound(i);
		if (i < num - 1)
			put_int(size);
	}
}

void convert_mac(void)
{
	int i, size, num;
	char tmp[256];

	sprintf(infile_base, "simon2.");

	input = fopen("voices.idx", "rb");
	if (!input) {
		printf("Cannot open file: %s\n", "voices.idx");
		exit(-1);
	}

	sprintf(tmp, "%sidx", infile_base);
	output_idx = fopen(tmp, "wb");

	sprintf(tmp, "%sdat", infile_base);
	output_snd = fopen(tmp, "wb");

	num = get_offsets_mac();

	if (!num) {
		printf("This does not seem to be a valid file\n");
		exit(-1);
	}
	size = num*4;

	put_int(0);
	put_int(size);

	for (i = 1; i < num; i++) {
		if (filenums[i] == filenums[i+1] && offsets[i] == offsets[i+1]) {
			put_int(size);
			continue;
		}

		if (filenums[i] != filenums[i-1]) {
			sprintf(tmp, "voices%d.dat", filenums[i]);
			if (input)
				fclose(input);
			input = fopen(tmp, "rb"); 
		}

		size += get_sound(i);
		if (i < num - 1)
			put_int(size);
	}
}

int main(int argc, char *argv[])
{
	int i;
	
	if (argc < 2)
		showhelp(argv[0]);
	i = 1;
	if (strcmp(argv[1], "--mp3") == 0) {
		oggmode = 0;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		printf("Vorbis not implemented\n");
		exit(0);
		oggmode = 1;
		i++;
	}

	if (oggmode)
		process_ogg_parms(argc, argv, i);
	else
		process_mp3_parms(argc, argv, i);

	i = argc - 1;

	if (strcmp(argv[i], "mac") == 0) {
		convert_mac();
	} else {
		convert_pc(argv[i]);
	}

	end();

	return(0);
}

