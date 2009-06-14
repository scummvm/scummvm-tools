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
#define HEADER_FLAG_AUDIO_INTRO (1 << 0)

#define OUTPUT_MP3  "TUCKER.SO3"
#define OUTPUT_OGG  "TUCKER.SOG"
#define OUTPUT_FLA  "TUCKER.SOF"

static CompressMode g_mode = kMP3Mode;
static const char *g_output_filename = OUTPUT_MP3;
static const char *g_output_directory = NULL;
static const char *g_input_directory = NULL;

struct CompressedData {
	int offset;
	int size;
};

static CompressedData temp_table[10000];

static int append_compress_file(FILE *output) {
	char buf[2048];
	FILE *input_temp;
	int sz, compress_sz = 0;

	input_temp = fopen(tempEncoded, "rb");
	if (input_temp) {
		while ((sz = fread(buf, 1, sizeof(buf), input_temp)) > 0) {
			if ((sz = fwrite(buf, 1, sz, output)) > 0) {
				compress_sz += sz;
			}
		}
		fclose(input_temp);
	}
	return compress_sz;
}

static int compress_file_wav(FILE *input, FILE *output) {
	char buf[8];

	if (fread(buf, 1, 8, input) == 8 && memcmp(buf, "RIFF", 4) == 0) {
		extractAndEncodeWAV(TEMP_WAV, input, g_mode);
		return append_compress_file(output);
	}
	return 0;
}

static int compress_file_raw(const char *input, bool is16, FILE *output) {
	if (is16) {
		setRawAudioType(true, false, 16);
	} else {
		setRawAudioType(false, false, 8);
	}
	encodeAudio(input, true, 22050, tempEncoded, g_mode);
	return append_compress_file(output);
}

#define SOUND_TYPES_COUNT 3

#define MAX_SOUND_FILES    500
#define MAX_MUSIC_FILES     60
#define MAX_SPEECH_FILES 10000

struct SoundDirectory {
	const char *name;
	const char *fmt;
	int count;
};

static SoundDirectory sound_directory_table[SOUND_TYPES_COUNT] = {
	{ "FX",     "fx%d.wav",    MAX_SOUND_FILES  },
	{ "MUSIC",  "mus%d.wav",   MAX_MUSIC_FILES  },
	{ "SPEECH", "sam%04d.wav", MAX_SPEECH_FILES }
};

static uint32 compress_sounds_directory(FILE *output, const struct SoundDirectory *dir) {
	char filepath[1024];
	char *filename;
	struct stat s;
	int i, pos;
	uint32 current_offset;
	FILE *input;

	assert(dir->count <= ARRAYSIZE(temp_table));

	snprintf(filepath, sizeof(filepath), "%s/%s", g_input_directory, dir->name);
	if (stat(filepath, &s) != 0 || !S_ISDIR(s.st_mode)) {
		error("Cannot stat directory '%s'", filepath);
	}
	strcat(filepath, "/");
	filename = filepath + strlen(filepath);

	pos = ftell(output);

	/* write 0 offsets/sizes table */
	for (i = 0; i < dir->count; ++i) {
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
	}

	/* compress .wav files in directory */
	current_offset = 0;
	for (i = 0; i < dir->count; ++i) {
		temp_table[i].offset = current_offset;
		sprintf(filename, dir->fmt, i);
		input = fopen(filepath, "rb");
		if (input) {
			temp_table[i].size = compress_file_wav(input, output);
			fclose(input);
		} else {
			temp_table[i].size = 0;
		}
		current_offset += temp_table[i].size;
	}

	/* fix offsets/sizes table */
	fseek(output, pos, SEEK_SET);
	for (i = 0; i < dir->count; ++i) {
		writeUint32LE(output, temp_table[i].offset);
		writeUint32LE(output, temp_table[i].size);
	}

	fseek(output, 0, SEEK_END);
	return current_offset + dir->count * 8;
}

static const char *audio_files_list[] = {
	"demomenu.raw",
	"demorolc.raw",
	"fx101.wav",
	"fx102.wav",
	"fx103.wav",
	"fx104.wav",
	"fx105.wav",
	"fx107.wav",
	"fx108.wav",
	"fx109.wav",
	"fx110.wav",
	"fx111.wav",
	"fx112.wav",
	"fx113.wav",
	"fx114.wav",
	"fx116.wav",
	"fx117.wav",
	"fx32.wav",
	"fx33.wav",
	"fx34.wav",
	"fx35.wav",
	"fx36.wav",
	"fx37.wav",
	"fx38.wav",
	"fx39.wav",
	"fx40.wav",
	"fx42.wav",
	"fx43.wav",
	"fx44.wav",
	"fx45.wav",
	"fx47.wav",
	"fx48.wav",
	"fx49.wav",
	"fx50.wav",
	"fx52.wav",
	"fx53.wav",
	"fx55.wav",
	"fx56.wav",
	"fx57.wav",
	"fx58.wav",
	"fx59.wav",
	"fx60.wav",
	"fx61.wav",
	"fx62.wav",
	"fx63.wav",
	"fx64.wav",
	"fx66.wav",
	"fx67.wav",
	"fx68.wav",
	"fx69.wav",
	"fx70.wav",
	"fx71.wav",
	"fx72.wav",
	"fx73.wav",
	"fx74.wav",
	"fx75.wav",
	"fx76.wav",
	"fx77.wav",
	"fx78.wav",
	"fx79.wav",
	"fx80.wav",
	"fx81.wav",
	"fx83.wav",
	"fx86.wav",
	"fx91.wav",
	"fx92.wav",
	"fx93.wav",
	"fx97.wav",
	"fx98.wav",
	"int1.raw",
	"int14.raw",
	"int15.raw",
	"int16.raw",
	"int17.raw",
	"int18.raw",
	"int19.raw",
	"int2.raw",
	"int20.raw",
	"int21.raw",
	"int22.raw",
	"int23.raw",
	"int24.raw",
	"int25.raw",
	"int26.raw",
	"int27.raw",
	"int28.raw",
	"int29.raw",
	"int3.raw",
	"int30.raw",
	"int31.raw",
	"int32.raw",
	"int33.raw",
	"int34.raw",
	"int35.raw",
	"int36.raw",
	"int37.raw",
	"int38.raw",
	"int4.raw",
	"int41.raw",
	"int42.raw",
	"int5.raw",
	"int6.raw",
	"int7.raw",
	"introdua.wav",
	"merilogo.raw",
	"rdfx1.wav",
	"rdfx12.wav",
	"rdfx13.wav",
	"rdfx14.wav",
	"rdfx15.wav",
	"rdfx16.wav",
	"rdfx17.wav",
	"rdfx18.wav",
	"rdfx20.wav",
	"rdfx21.wav",
	"rdfx22.wav",
	"rdfx24.wav",
	"rdfx25.wav",
	"rdfx26.wav",
	"rdfx27.wav",
	"rdfx28.wav",
	"rdfx3.wav",
	"rdfx30.wav",
	"rdfx31.wav",
	"rdfx33.wav",
	"rdfx36.wav",
	"rdfx37.wav",
	"rdfx38.wav",
	"rdfx8.wav",
	"rdfx9.wav"
};

static const int audio_formats_table[] = {
	3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
	1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
	1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 
	1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 
	1, 1, 1, 1, 1, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
	4, 4, 4, 4, 4, 4, 4, 2, 3, 1, 1, 2, 1, 1, 1, 1, 
	1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	2, 1
};

static uint32 compress_audio_directory(FILE *output) {
	char filepath[1024];
	int i, pos, count;
	uint32 current_offset;
	FILE *input;

	count = ARRAYSIZE(audio_files_list);
	pos = ftell(output);

	/* write 0 offsets/sizes table */
	for (i = 0; i < count; ++i) {
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
	}

	current_offset = 0;
	for (i = 0; i < count; ++i) {
		temp_table[i].offset = current_offset;
		sprintf(filepath, "%s/audio/%s", g_input_directory, audio_files_list[i]);
		input = fopen(filepath, "rb");
		if (!input) {
			warning("Can't open file '%s'", filepath);
			temp_table[i].size = 0;
		} else {
			switch (audio_formats_table[i]) {
			case 1:
			case 2:
				temp_table[i].size = compress_file_wav(input, output);
				break;
			case 3:
				temp_table[i].size = compress_file_raw(filepath, 0, output);
				break;
			case 4:
				temp_table[i].size = compress_file_raw(filepath, 1, output);
				break;
			}
			fclose(input);
		}
		current_offset += temp_table[i].size;
	}

	/* fix offsets/sizes table */
	fseek(output, pos, SEEK_SET);
	for (i = 0; i < count; ++i) {
		writeUint32LE(output, temp_table[i].offset);
		writeUint32LE(output, temp_table[i].size);
	}

	fseek(output, 0, SEEK_END);
	return current_offset + count * 8;
}

static void compress_sound_files() {
	int i;
	FILE *output;
	char filepath[1024];
	uint32 current_offset;
	uint32 sound_directory_size[SOUND_TYPES_COUNT];
	uint32 audio_directory_size;
	const uint16 flags = 0; // HEADER_FLAG_AUDIO_INTRO;

	snprintf(filepath, sizeof(filepath), "%s/%s", g_output_directory, g_output_filename);
	output = fopen(filepath, "wb");
	if (!output) {
		error("Cannot open file '%s' for writing", filepath);
	}

	writeUint16LE(output, CURRENT_VER);
	writeUint16LE(output, flags);

	/* write 0 offsets/count */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		writeUint32LE(output, 0);
		writeUint32LE(output, 0);
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		writeUint32LE(output, 0);
	}

	/* compress the .wav files in each directory */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		printf("Processing directory '%s'...\n", sound_directory_table[i].name);
		sound_directory_size[i] = compress_sounds_directory(output, &sound_directory_table[i]);
		printf("Done (%d bytes)\n", sound_directory_size[i]);
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		printf("Processing directory 'audio'...\n");
		audio_directory_size = compress_audio_directory(output);
		printf("Done (%d bytes)\n", audio_directory_size);
	}

	/* fix sound types offsets/counts */
	fseek(output, HEADER_SIZE, SEEK_SET);
	current_offset = 0;
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		writeUint32LE(output, current_offset);
		writeUint32LE(output, sound_directory_table[i].count);
		current_offset += sound_directory_size[i];
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		writeUint32LE(output, current_offset);
		writeUint32LE(output, ARRAYSIZE(audio_files_list));
		current_offset += audio_directory_size;
	}

	fclose(output);

	/* cleanup */
	unlink(TEMP_WAV);
	unlink(TEMP_RAW);
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
