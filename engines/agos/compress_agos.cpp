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

#include <cstring>
#include <stdio.h>

#include "compress_agos.h"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

CompressAgos::CompressAgos(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_convertMac = false;
	_outputToDirectory = false;
	_supportsProgressBar = true;
	
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Compresses Simon the Sorcer and Feeble Files data files.";
	_helptext = "\nUsage: " + getName() + " [mode params] [-o outfile] [--mac] <infile>\n";
}

void CompressAgos::end() {
	int size;
	char fbuf[2048];

	_output_idx.close();
	_output_snd.close();

	Common::File outputFile(_outputPath, "wb");

	_input.open(TEMP_IDX, "rb");
	while ((size = _input.read_noThrow(fbuf, 2048)) > 0) {
		outputFile.write(fbuf, size);
	}

	_input.open(TEMP_DAT, "rb");
	while ((size = _input.read_noThrow(fbuf, 2048)) > 0) {
		outputFile.write(fbuf, size);
	}

	_input.close();
	outputFile.close();

	/* And some clean-up :-) */
	unlink(TEMP_IDX);
	unlink(TEMP_DAT);
	unlink(TEMP_RAW);
	unlink(tempEncoded);
	unlink(TEMP_WAV);
}


int CompressAgos::get_offsets(size_t maxcount, uint32 filenums[], uint32 offsets[]) {
	for (size_t i = 0; i < maxcount; i++) {
		char buf[8];
		_input.read_throwsOnError(buf, 8);
		if (!memcmp(buf, "Creative", 8) || !memcmp(buf, "RIFF", 4)) {
			return i;
		}
		_input.seek(-8, SEEK_CUR);

		offsets[i] = _input.readUint32LE();
	}

	// We exceeded size of array
	throw ToolException("Too many indexes read, file does not appear to be of the correct format.");
}

int CompressAgos::get_offsets_mac(size_t maxcount, uint32 filenums[], uint32 offsets[]) {
	int size = _input.size();

	for (int i = 1; i <= size / 6; i++) {
		if ((size_t)i >= maxcount)
			throw ToolException("Too many indexes read, file does not appear to be of the correct format.");
		filenums[i] = _input.readUint16BE();
		offsets[i] = _input.readUint32BE();
	}

	return (size/6);
}


uint32 CompressAgos::get_sound(uint32 offset) {
	uint32 tot_size;
	char outname[256];
	int size;
	char fbuf[2048];
	char buf[8];

	_input.seek(offset, SEEK_SET);

	_input.read_throwsOnError(buf, 8);
	if (!memcmp(buf, "Creative", 8)) {
		print("VOC found (pos = %d) :\n", offset);
		_input.seek(18, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, _input, _format);
	} else if (!memcmp(buf, "RIFF", 4)) {
		print("WAV found (pos = %d) :\n", offset);
		extractAndEncodeWAV(TEMP_WAV, _input, _format);
	} else {
		error("Unexpected data at offset: %d", offset);
	}

	/* Append the converted data to the master output file */
	sprintf(outname, "%s", tempEncoded);
	Common::File f(outname, "rb");
	tot_size = 0;
	while ((size = f.read_noThrow(fbuf, 2048)) > 0) {
		tot_size += size;
		_output_snd.write(fbuf, size);
	}

	return(tot_size);
}


void CompressAgos::convert_pc(Common::Filename* inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	_input.open(*inputPath, "rb");

	_output_idx.open(TEMP_IDX, "wb");

	_output_snd.open(TEMP_DAT, "wb");

	num = get_offsets(32768, filenums, offsets);
	if (!num) {
		error("This does not seem to be a valid file");
	}
	size = num * 4;

	_output_idx.writeUint32LE(0);
	_output_idx.writeUint32LE(size);

	for (i = 1; i < num; i++) {
		updateProgress(i, num);

		if (offsets[i] == offsets[i + 1]) {
			_output_idx.writeUint32LE(size);
			continue;
		}

		if (offsets[i] != 0)
			size += get_sound(offsets[i]);
		if (i < num - 1)
			_output_idx.writeUint32LE(size);
	}
}

void CompressAgos::convert_mac(Common::Filename *inputPath) {
	int i, size, num;
	uint32 filenums[32768];
	uint32 offsets[32768];

	inputPath->setFullName("voices.idx");
	_input.open(*inputPath, "rb");

	_output_idx.open(TEMP_IDX, "wb");

	_output_snd.open(TEMP_DAT, "wb");

	num = get_offsets_mac(32768, filenums, offsets);
	if (!num) {
		error("This does not seem to be a valid file");
	}
	size = num * 4;

	_output_idx.writeUint32LE(0);
	_output_idx.writeUint32LE(size);

	for (i = 1; i < num; i++) {
		updateProgress(i, num);

		if (filenums[i] == filenums[i + 1] && offsets[i] == offsets[i + 1]) {
			_output_idx.writeUint32LE(size);
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
			_output_idx.writeUint32LE(size);
		}
	}
}

void CompressAgos::parseExtraArguments() {
	if (!_arguments.empty() && _arguments.front() == "--mac") {
		_convertMac = true;
		_arguments.pop_front();
	}
}

void CompressAgos::execute() {
	Common::Filename inpath(_inputPaths[0].path);

	if (_outputPath.empty()) {
		_outputPath = inpath;
		_outputPath.setExtension(audio_extensions(_format));
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

