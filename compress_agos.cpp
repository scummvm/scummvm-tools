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

#include "compress_agos.h"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

CompressAgos::CompressAgos(const std::string &name) : CompressionTool(name) {
	_compMode = AUDIO_MP3;
	_convertMac = false;

	_helptext = "\nUsage: %s [mode] [mode params] [--mac] <infile>\n" kCompressionAudioHelp
}

void CompressAgos::end() {
	int size;
	char fbuf[2048];

	_output_idx.open(_outputPath, "wb");

	_input.open(TEMP_IDX, "rb");
	while ((size = fread(fbuf, 1, 2048, _input)) > 0) {
		fwrite(fbuf, 1, size, _output_idx);
	}

	_input.open(TEMP_DAT, "rb");
	while ((size = fread(fbuf, 1, 2048, _input)) > 0) {
		fwrite(fbuf, 1, size, _output_idx);
	}

	_input.close();
	_output_idx.close();
	_output_snd.close();

	/* And some clean-up :-) */
	unlink(TEMP_IDX);
	unlink(TEMP_DAT);
	unlink(TEMP_RAW);
	unlink(tempEncoded);
	unlink(TEMP_WAV);
}


int CompressAgos::get_offsets(uint32 filenums[], uint32 offsets[]) {
	int i;
	char buf[8];

	for (i = 0;; i++) {
		fread(buf, 1, 8, _input);
		if (!memcmp(buf, "Creative", 8) || !memcmp(buf, "RIFF", 4)) {
			return i;
		}
		fseek(_input, -8, SEEK_CUR);

		offsets[i] = _input.readU32LE();
	}
}

int CompressAgos::get_offsets_mac(uint32 filenums[], uint32 offsets[]) {
	int i, size;
	size = _input.size();

	for (i = 1; i <= size / 6; i++) {
		filenums[i] = _input.readU16BE();
		offsets[i] = _input.readU32BE();
	}

	return(size/6);
}


uint32 CompressAgos::get_sound(uint32 offset) {
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];
	char buf[8];

	fseek(_input, offset, SEEK_SET);

	fread(buf, 1, 8, _input);
	if (!memcmp(buf, "Creative", 8)) {
		print("VOC found (pos = %d) :\n", offset);
		fseek(_input, 18, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, _input, _compMode);
	} else if (!memcmp(buf, "RIFF", 4)) {
		print("WAV found (pos = %d) :\n", offset);
		extractAndEncodeWAV(TEMP_WAV, _input, _compMode);
	} else {
		error("Unexpected data at offset: %d", offset);
	}

	/* Append the converted data to the master output file */
	sprintf(outname, tempEncoded);
	File f(outname, "rb");
	tot_size = 0;
	while ((size = fread(fbuf, 1, 2048, f)) > 0) {
		tot_size += size;
		fwrite(fbuf, 1, size, _output_snd);
	}

	return(tot_size);
}


void CompressAgos::convert_pc(Filename* inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	_input.open(*inputPath, "rb");

	_output_idx.open(TEMP_IDX, "wb");

	_output_snd.open(TEMP_DAT, "wb");

	num = get_offsets(filenums, offsets);
	if (!num) {
		error("This does not seem to be a valid file");
	}
	size = num * 4;

	_output_idx.writeU32LE(0);
	_output_idx.writeU32LE(size);

	for (i = 1; i < num; i++) {
		if (offsets[i] == offsets[i + 1]) {
			_output_idx.writeU32LE(size);
			continue;
		}

		if (offsets[i] != 0)
			size += get_sound(offsets[i]);
		if (i < num - 1)
			_output_idx.writeU32LE(size);
	}
}

void CompressAgos::convert_mac(Filename *inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	inputPath->setFullName("voices.idx");
	_input.open(*inputPath, "rb");

	_output_idx.open(TEMP_IDX, "wb");

	_output_snd.open(TEMP_DAT, "wb");

	num = get_offsets_mac(filenums, offsets);
	if (!num) {
		error("This does not seem to be a valid file");
	}
	size = num * 4;

	_output_idx.writeU32LE(0);
	_output_idx.writeU32LE(size);

	for (i = 1; i < num; i++) {
		if (filenums[i] == filenums[i + 1] && offsets[i] == offsets[i + 1]) {
			_output_idx.writeU32LE(size);
			continue;
		}

		if (filenums[i] != filenums[i - 1]) {
			char filename[256];
			sprintf(filename, "voices%d.dat", filenums[i]);
			inputPath->setFullName(filename);

			_input.open(*inputPath, "rb");
		}

		size += get_sound(offsets[i]);

		if (i < num - 1) {
			_output_idx.writeU32LE(size);
		}
	}
}

void CompressAgos::parseExtraArguments() {
	if (_arguments[_arguments_parsed] == "--mac") {
		_convertMac = true;
	}
}

void CompressAgos::execute() {

	// We only got one input file
	if (_inputPaths.size() > 1)
		error("Only one input file expected!");
	Filename inpath(_inputPaths[0]);

	if (_outputPath.empty()) {
		_outputPath = inpath;
		_outputPath.setExtension(audio_extensions(_compMode));
	}

	if (_convertMac) {
		convert_mac(&inpath);
		inpath.setFullName("simon2");
	} else {
		convert_pc(&inpath);
	}

	end();
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressAgos agos(argv[0]);
	return agos.run(argc, argv);
}
#endif

