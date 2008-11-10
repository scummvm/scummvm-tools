/* compress_tucker - Compress Bud Tucker Sound Data Files
 * Copyright (C) 2008  The ScummVM Team
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

#define CURRENT_VER  1
#define HEADER_SIZE  4

#define OUTPUT_MP3  "TUCKER.SO3"
#define OUTPUT_OGG  "TUCKER.SOG"
#define OUTPUT_FLA  "TUCKER.SOF"

static CompressMode g_mode = kMP3Mode;
static const char *g_output_filename = OUTPUT_MP3;
static const char *g_output_directory = NULL;
static const char *g_input_directory = NULL;

#define MAX_SOUND_FILES    500
#define MAX_MUSIC_FILES     60
#define MAX_SPEECH_FILES 10000

struct CompressedData {
	int offset;
	int size;
};

static CompressedData sound_table[MAX_SOUND_FILES];
static CompressedData music_table[MAX_MUSIC_FILES];
static CompressedData speech_table[MAX_SPEECH_FILES];

#define SOUND_TYPES_COUNT 3

struct SoundDirectory {
	const char *name;
	const char *fmt;
};

static struct SoundDirectory sound_directory_table[SOUND_TYPES_COUNT] = {
	{ "FX",     "fx%d.wav"    },
	{ "MUSIC",  "mus%d.wav"   },
	{ "SPEECH", "sam%04d.wav" }
};

struct SoundDataDirectory {
	CompressedData *p;
	int sz;
};

static struct SoundDataDirectory sound_data_directory_table[SOUND_TYPES_COUNT] = {
	{ &sound_table[0],  MAX_SOUND_FILES  },
	{ &music_table[0],  MAX_MUSIC_FILES  },
	{ &speech_table[0], MAX_SPEECH_FILES }
};

static uint32 compress_sounds_directory(FILE *output, const struct SoundDirectory *dir, struct SoundDataDirectory *data_dir) {
	char filepath[1024];
	char *filename;
	struct stat s;
	int i, sz, pos;
	uint32 current_offset;
	FILE *input_wav, *input_temp;
	char buf[2048];

	snprintf(filepath, sizeof(filepath), "%s/%s", g_input_directory, dir->name);
	if (stat(filepath, &s) != 0 || !S_ISDIR(s.st_mode)) {
		error("Cannot stat directory '%s'", filepath);
	}
	strcat(filepath, "/");
	filename = filepath + strlen(filepath);

	pos = ftell(output);

	/* write 0 offsets/sizes table */
	for (i = 0; i < data_dir->sz; ++i) {
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
	}

	/* compress .wav files in directory */
	current_offset = 0;
	for (i = 0; i < data_dir->sz; ++i) {
		data_dir->p[i].offset = current_offset;
		data_dir->p[i].size = 0;

		sprintf(filename, dir->fmt, i);	
		input_wav = fopen(filepath, "rb");
		if (input_wav) {
			if (fread(buf, 1, 8, input_wav) == 8 && memcmp(buf, "RIFF", 4) == 0) {
				extractAndEncodeWAV(TEMP_WAV, input_wav, g_mode);
				input_temp = fopen(tempEncoded, "rb");
				if (input_temp) {
					while ((sz = fread(buf, 1, sizeof(buf), input_temp)) > 0) {
						if ((sz = fwrite(buf, 1, sz, output)) > 0) {
							data_dir->p[i].size += sz;
						}
					}
					fclose(input_temp);
				}
			}
			fclose(input_wav);
		}

		current_offset += data_dir->p[i].size;
	}

	/* fix offsets/sizes table */
	fseek(output, pos, SEEK_SET);
	for (i = 0; i < data_dir->sz; ++i) {
		writeUint32LE(output, data_dir->p[i].offset);
		writeUint32LE(output, data_dir->p[i].size);
	}

	fseek(output, 0, SEEK_END);
	return current_offset + data_dir->sz * 8;
}

static void compress_sound_files() {
	int i;
	FILE *output;
	char filepath[1024];
	uint32 current_offset;
	uint32 directory_size[SOUND_TYPES_COUNT];

	snprintf(filepath, sizeof(filepath), "%s/%s", g_output_directory, g_output_filename);
	output = fopen(filepath, "wb");
	if (!output) {
		error("Cannot open file '%s' for writing", filepath);
	}

	writeUint16LE(output, CURRENT_VER);
	writeUint16LE(output, 0); /* flags */

	/* write 0 offsets/count */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
	}

	/* compress the .wav files in each directory */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		printf("Processing directory '%s'...\n", sound_directory_table[i].name);
		directory_size[i] = compress_sounds_directory(output, &sound_directory_table[i], &sound_data_directory_table[i]);
		printf("Done (%d bytes)\n", directory_size[i]);
	}

	/* fix sound types offsets/counts */
	fseek(output, HEADER_SIZE, SEEK_SET);
	current_offset = 0;
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		writeUint32LE(output, current_offset);
		writeUint32LE(output, sound_data_directory_table[i].sz);
		current_offset += directory_size[i];
	}

	fclose(output);

	/* cleanup */
	unlink(TEMP_WAV);
	unlink(tempEncoded);

	printf("Done.\n");
}

static void showhelp(const char *exename) {
	printf("\nUsage: %s [params] <inputdir> <outputdir>\n", exename);
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

	g_input_directory = argv[argc - 2];
	g_output_directory = argv[argc - 1];

	switch (g_mode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc - 1, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc - 1, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc - 1, argv, i)) {
			showhelp(argv[0]);
		}
		break;
	}

	compress_sound_files();
	return 0;
}
