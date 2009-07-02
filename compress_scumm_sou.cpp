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

static const char *g_output_filename = OUTPUT_MP3;

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;


void end_of_file(const char *inputPath) {
	FILE *in;
	int idx_size = ftell(output_idx);
	size_t size;
	char buf[2048];

	fclose(output_snd);
	fclose(output_idx);

	output_idx = fopen(g_output_filename, "wb");
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

void get_part(const char *inputPath) {
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
	assert(tags >= 8);
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


int export_main(compress_scumm_sou)(int argc, char *argv[]) {
	const char *helptext = "\nUsage: %s [mode] [mode params] monster.sou\n" kCompressionAudioHelp;

	char buf[2048];
	Filename inpath, outpath;
	int first_arg = 1;
	int last_arg = argc - 1;

	// Should we help the user?
	parseHelpArguments(argv, argc, helptext);

	// compression mode
	gCompMode = process_audio_params(argc, argv, &first_arg);

	if (gCompMode == kNoAudioMode) {
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
			g_output_filename = OUTPUT_MP3;
			break;
		case kVorbisMode:
			g_output_filename = OUTPUT_OGG;
			break;
		case kFlacMode:
			g_output_filename = OUTPUT_FLAC;
			break;
		default:
			displayHelp(helptext, argv[0]);
			break;
		}
	}

	inpath.setFullPath(argv[first_arg]);

	input = fopen(inpath.getFullPath(), "rb");
	if (!input) {
		error("Cannot open file: %s", inpath.getFullPath());
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		error("Cannot open file " TEMP_IDX " for write" );
	}
	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		error("Cannot open file " TEMP_DAT " for write");
	}

	/* Get the 'SOU ....' header */
	fread(buf, 1, 8, input);
	if (strncmp(buf, f_hdr, 8)) {
		error("Bad SOU");
	}

	while (1)
		get_part(inpath.getFullPath());

	return 0;
}

#if defined(UNIX) && defined(EXPORT_MAIN)
int main(int argc, char *argv[]) __attribute__((weak));
int main(int argc, char *argv[]) {
	return export_main(compress_scumm_sou)(argc, argv);
}
#endif

