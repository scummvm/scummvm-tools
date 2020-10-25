/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Compress Bud Tucker Sound Data Files */

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "common/util.h"
#include "compress.h"
#include "compress_tucker.h"

#define CURRENT_VER  1
#define HEADER_SIZE  4
#define HEADER_FLAG_AUDIO_INTRO (1 << 0)

#define OUTPUT_MP3  "TUCKER.SO3"
#define OUTPUT_OGG  "TUCKER.SOG"
#define OUTPUT_FLA  "TUCKER.SOF"

struct CompressedData {
	int offset;
	int size;
};

static CompressedData temp_table[10000];

CompressTucker::CompressTucker(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input;
	input.format = "/";
	input.file = false;
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress the Bud Tucker data files.";
	_helptext = "\nUsage: " + getName() + " [mode params] [-o outputdir] <inputdir>\n";
}

int CompressTucker::append_compress_file(Common::File &output) {
	char buf[2048];
	int sz, compress_sz = 0;

	Common::File input_temp(tempEncoded, "rb");
	while ((sz = input_temp.read_noThrow(buf, sizeof(buf))) > 0) {
		if ((sz = output.write(buf, sz)) > 0) {
			compress_sz += sz;
		}
	}
	return compress_sz;
}

int CompressTucker::compress_file_wav(Common::File &input, Common::File &output) {
	char buf[8];

	if (input.read_noThrow(buf, 8) == 8 && memcmp(buf, "RIFF", 4) == 0) {
		extractAndEncodeWAV(TEMP_WAV, input, _format);
		return append_compress_file(output);
	}
	return 0;
}

int CompressTucker::compress_file_raw(const char *input, bool is16, Common::File &output) {
	if (is16) {
		setRawAudioType(true, false, 16);
	} else {
		setRawAudioType(false, false, 8);
	}
	encodeAudio(input, true, 22050, tempEncoded, _format);
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
	{ "FX",     "FX%d.WAV",    MAX_SOUND_FILES  },
	{ "MUSIC",  "MUS%d.WAV",   MAX_MUSIC_FILES  },
	{ "SPEECH", "SAM%04d.WAV", MAX_SPEECH_FILES }
};

uint32 CompressTucker::compress_sounds_directory(const Common::Filename *inpath, const Common::Filename *outpath, Common::File &output, const struct SoundDirectory *dir) {
	char filepath[1024];
	int i, pos, len;
	uint32 current_offset;
	Common::File input;

	assert(dir->count <= ARRAYSIZE(temp_table));

	// We can't use setFullName since dir->name can contain '/'
	len = snprintf(filepath, sizeof(filepath), "%s/%s/", inpath->getPath().c_str(), dir->name);

	pos = output.pos();

	/* write 0 offsets/sizes table */
	for (i = 0; i < dir->count; ++i) {
		output.writeUint32LE(0);
		output.writeUint32LE(0);
	}

	/* compress .wav files in directory */
	current_offset = 0;
	for (i = 0; i < dir->count; ++i) {
		temp_table[i].offset = current_offset;
		snprintf(&filepath[len], sizeof(filepath) - len, dir->fmt, i);
		try {
			input.open(filepath, "rb");
			temp_table[i].size = compress_file_wav(input, output);
		} catch (...) {
			temp_table[i].size = 0;
		}
		current_offset += temp_table[i].size;
	}

	/* fix offsets/sizes table */
	output.seek(pos, SEEK_SET);
	for (i = 0; i < dir->count; ++i) {
		output.writeUint32LE(temp_table[i].offset);
		output.writeUint32LE(temp_table[i].size);
	}

	output.seek(0, SEEK_END);
	return current_offset + dir->count * 8;
}

static const char *audio_files_list[] = {
	"DEMOMENU.RAW",
	"DEMOROLC.RAW",
	"FX101.WAV",
	"FX102.WAV",
	"FX103.WAV",
	"FX104.WAV",
	"FX105.WAV",
	"FX107.WAV",
	"FX108.WAV",
	"FX109.WAV",
	"FX110.WAV",
	"FX111.WAV",
	"FX112.WAV",
	"FX113.WAV",
	"FX114.WAV",
	"FX116.WAV",
	"FX117.WAV",
	"FX32.WAV",
	"FX33.WAV",
	"FX34.WAV",
	"FX35.WAV",
	"FX36.WAV",
	"FX37.WAV",
	"FX38.WAV",
	"FX39.WAV",
	"FX40.WAV",
	"FX42.WAV",
	"FX43.WAV",
	"FX44.WAV",
	"FX45.WAV",
	"FX47.WAV",
	"FX48.WAV",
	"FX49.WAV",
	"FX50.WAV",
	"FX52.WAV",
	"FX53.WAV",
	"FX55.WAV",
	"FX56.WAV",
	"FX57.WAV",
	"FX58.WAV",
	"FX59.WAV",
	"FX60.WAV",
	"FX61.WAV",
	"FX62.WAV",
	"FX63.WAV",
	"FX64.WAV",
	"FX66.WAV",
	"FX67.WAV",
	"FX68.WAV",
	"FX69.WAV",
	"FX70.WAV",
	"FX71.WAV",
	"FX72.WAV",
	"FX73.WAV",
	"FX74.WAV",
	"FX75.WAV",
	"FX76.WAV",
	"FX77.WAV",
	"FX78.WAV",
	"FX79.WAV",
	"FX80.WAV",
	"FX81.WAV",
	"FX83.WAV",
	"FX86.WAV",
	"FX91.WAV",
	"FX92.WAV",
	"FX93.WAV",
	"FX97.WAV",
	"FX98.WAV",
	"INT1.RAW",
	"INT14.RAW",
	"INT15.RAW",
	"INT16.RAW",
	"INT17.RAW",
	"INT18.RAW",
	"INT19.RAW",
	"INT2.RAW",
	"INT20.RAW",
	"INT21.RAW",
	"INT22.RAW",
	"INT23.RAW",
	"INT24.RAW",
	"INT25.RAW",
	"INT26.RAW",
	"INT27.RAW",
	"INT28.RAW",
	"INT29.RAW",
	"INT3.RAW",
	"INT30.RAW",
	"INT31.RAW",
	"INT32.RAW",
	"INT33.RAW",
	"INT34.RAW",
	"INT35.RAW",
	"INT36.RAW",
	"INT37.RAW",
	"INT38.RAW",
	"INT4.RAW",
	"INT41.RAW",
	"INT42.RAW",
	"INT5.RAW",
	"INT6.RAW",
	"INT7.RAW",
	"INTRODUA.WAV",
	"MERILOGO.RAW",
	"RDFX1.WAV",
	"RDFX12.WAV",
	"RDFX13.WAV",
	"RDFX14.WAV",
	"RDFX15.WAV",
	"RDFX16.WAV",
	"RDFX17.WAV",
	"RDFX18.WAV",
	"RDFX20.WAV",
	"RDFX21.WAV",
	"RDFX22.WAV",
	"RDFX24.WAV",
	"RDFX25.WAV",
	"RDFX26.WAV",
	"RDFX27.WAV",
	"RDFX28.WAV",
	"RDFX3.WAV",
	"RDFX30.WAV",
	"RDFX31.WAV",
	"RDFX33.WAV",
	"RDFX36.WAV",
	"RDFX37.WAV",
	"RDFX38.WAV",
	"RDFX8.WAV",
	"RDFX9.WAV"
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

uint32 CompressTucker::compress_audio_directory(const Common::Filename *inpath, const Common::Filename *outpath, Common::File &output) {
	char filepath[1024];
	int i, pos, count;
	uint32 current_offset;

	count = ARRAYSIZE(audio_files_list);
	pos = output.pos();

	/* write 0 offsets/sizes table */
	for (i = 0; i < count; ++i) {
		output.writeUint32LE(0);
		output.writeUint32LE(0);
	}

	current_offset = 0;
	for (i = 0; i < count; ++i) {
		temp_table[i].offset = current_offset;
		snprintf(filepath, sizeof(filepath), "%sAUDIO/%s", inpath->getPath().c_str(), audio_files_list[i]);

		try {
			Common::File input(filepath, "rb");

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
		} catch (...) {
			warning("Can't open file '%s'", filepath);
			temp_table[i].size = 0;
		}

		current_offset += temp_table[i].size;
	}

	/* fix offsets/sizes table */
	output.seek(pos, SEEK_SET);
	for (i = 0; i < count; ++i) {
		output.writeUint32LE(temp_table[i].offset);
		output.writeUint32LE(temp_table[i].size);
	}

	output.seek(0, SEEK_END);
	return current_offset + count * 8;
}

void CompressTucker::compress_sound_files(const Common::Filename *inpath, const Common::Filename *outpath) {
	int i;
	uint32 current_offset;
	uint32 sound_directory_size[SOUND_TYPES_COUNT];
	uint32 audio_directory_size;
	static const uint16 flags = HEADER_FLAG_AUDIO_INTRO;

	Common::File output(*outpath, "wb");

	output.writeUint16LE(CURRENT_VER);
	output.writeUint16LE(flags);

	/* write 0 offsets/count */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		output.writeUint32LE(0);
		output.writeUint32LE(0);
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		output.writeUint32LE(0);
		output.writeUint32LE(0);
	}

	/* compress the .wav files in each directory */
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		updateProgress(i, SOUND_TYPES_COUNT + 1);

		print("Processing directory '%s'...", sound_directory_table[i].name);
		sound_directory_size[i] = compress_sounds_directory(inpath, outpath, output, &sound_directory_table[i]);
		print("Done (%d bytes)", sound_directory_size[i]);
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		updateProgress(1, 1);

		print("Processing directory 'audio'...");
		audio_directory_size = compress_audio_directory(inpath, outpath, output);
		print("Done (%d bytes)", audio_directory_size);
	}

	/* fix sound types offsets/counts */
	output.seek(HEADER_SIZE, SEEK_SET);
	current_offset = 0;
	for (i = 0; i < SOUND_TYPES_COUNT; ++i) {
		output.writeUint32LE(current_offset);
		output.writeUint32LE(sound_directory_table[i].count);
		current_offset += sound_directory_size[i];
	}
	if (flags & HEADER_FLAG_AUDIO_INTRO) {
		output.writeUint32LE(current_offset);
		output.writeUint32LE(ARRAYSIZE(audio_files_list));
		current_offset += audio_directory_size;
	}

	output.close();

	/* cleanup */
	Common::removeFile(TEMP_WAV);
	Common::removeFile(TEMP_RAW);
	Common::removeFile(tempEncoded);

	print("Done.");
}

static const char *inputDirs[] = { "AUDIO", "FX", "MUSIC", "SPEECH", 0 };

void CompressTucker::execute() {
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	// Ensure necessary directories are present
	for (int i = 0; inputDirs[i]; ++i) {
		char path[1024];
		snprintf(path, sizeof(path), "%s%s", inpath.getPath().c_str(), inputDirs[i]);
		if (!Common::isDirectory(path)) {
			error("Missing input directory '%s'", path);
		}
	}

	// Default out is same as in directory, file names differ by extension
	if (outpath.empty()) {
		outpath = inpath;
	}

	// Temporary output file
	switch(_format) {
	case AUDIO_MP3:
		tempEncoded = TEMP_MP3;
		outpath.setFullName(OUTPUT_MP3);
		break;
	case AUDIO_VORBIS:
		tempEncoded = TEMP_OGG;
		outpath.setFullName(OUTPUT_OGG);
		break;
	case AUDIO_FLAC:
		tempEncoded = TEMP_FLAC;
		outpath.setFullName(OUTPUT_FLA);
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	compress_sound_files(&inpath, &outpath);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	return export_main(compress_tucker)(argc, argv);
}
#endif

