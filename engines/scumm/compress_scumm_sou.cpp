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

#include <assert.h>
#include <string.h>

#include "compress_scumm_sou.h"


static const char f_hdr[] = {
	'S', 'O', 'U', ' ', 0, 0, 0, 0, 0
};

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"


CompressScummSou::CompressScummSou(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	ToolInput input;
	input.format = "*.sou";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress .sou files of SCUMM games.";
	_helptext = "\nUsage: " + getName() + " [mode] [mode params] monster.sou\n";
	_supportsProgressBar = true;
}

void CompressScummSou::end_of_file() {
	int idx_size = _output_idx.pos();
	size_t size;
	char buf[2048];

	_output_snd.close();
	_output_idx.close();

	_output_idx.open(_outputPath, "wb");
	_output_idx.writeUint32BE((uint32)idx_size);

	Common::File in(TEMP_IDX, "rb");
	while ((size = in.read_noThrow(buf, 2048)) > 0) {
		_output_idx.write(buf, size);
	}

	in.open(TEMP_DAT, "rb");
	while ((size = in.read_noThrow(buf, 2048)) > 0) {
		_output_idx.write(buf, size);
	}
	in.close();
	_output_idx.close();
	_input.close();

	/* And some clean-up :-) */
	unlink(TEMP_IDX);
	unlink(TEMP_DAT);
	unlink(TEMP_RAW);
	unlink(tempEncoded);
}

void CompressScummSou::append_byte(int size, char buf[]) {
	int i;
	for (i = 0; i < (size - 1); i++)
		buf[i] = buf[i + 1];
	buf[i] = _input.readByte();
}

bool CompressScummSou::get_part() {
	uint32 tot_size;
	int size;
	char buf[2048];
	int pos = _input.pos();
	uint32 tags;

	try {
		/* Scan for the VCTL header */
		_input.read_throwsOnError(buf, 4);
		/* The demo (snmdemo) and floppy version of Sam & Max use VTTL */
		while (memcmp(buf, "VCTL", 4)&&memcmp(buf, "VTTL", 4)) {
			pos++;
			append_byte(4, buf);
		}
	} catch (Common::FileException &) {
		// EOF reached
		return false;
	}

	tags = _input.readUint32BE();
	assert(tags >= 8);
	tags -= 8;

	_output_idx.writeUint32BE((uint32)pos);
	_output_idx.writeUint32BE((uint32)_output_snd.pos());
	_output_idx.writeUint32BE(tags);
	while (tags > 0) {
		_output_snd.writeChar(_input.readChar());
		tags--;
	}

	_input.read_throwsOnError(buf, 8);
	if (!memcmp(buf, "Creative", 8))
		_input.seek(18, SEEK_CUR);
	else if (!memcmp(buf, "VTLK", 4))
		_input.seek(26, SEEK_CUR);
	else
		error("Unexpected data encountered");
	print("Voice file found (pos = %d) :", pos);

	/* Convert the VOC data */
	extractAndEncodeVOC(TEMP_RAW, _input, _format);

	/* Append the converted data to the master output file */
	Common::File f(tempEncoded, "rb");
	tot_size = 0;
	while ((size = f.read_noThrow(buf, 2048)) > 0) {
		tot_size += size;
		_output_snd.write(buf, size);
	}

	_output_idx.writeUint32BE(tot_size);

	updateProgress(_input.pos(), _file_size);

	return true;
}

std::string CompressScummSou::getOutputName() const {
	switch (_format) {
	case AUDIO_MP3:    return "monster.so3";
	case AUDIO_VORBIS: return "monster.sog";
	case AUDIO_FLAC:   return "monster.sof";
	default:
		throw ToolException("Unknown audio format");
	}
}

void CompressScummSou::execute() {
	char buf[2048];

	Common::Filename inpath(_inputPaths[0].path);
	//Common::Filename &outpath = _outputPath;

	if (_outputPath.directory())
		_outputPath.setFullName(getOutputName());
	else if (_outputPath.empty())
		_outputPath.setFullPath(getOutputName());

	_input.open(inpath, "rb");
	_output_idx.open(TEMP_IDX, "wb");
	_output_snd.open(TEMP_DAT, "wb");
	
	_file_size = _input.size();

	/* Get the 'SOU ....' header */
	_input.read_throwsOnError(buf, 8);
	if (strncmp(buf, f_hdr, 8)) {
		error("Bad SOU");
	}

	while (get_part())
		(void)0;// Do nothing
	end_of_file();
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressScummSou scummsou(argv[0]);
	return scummsou.run(argc, argv);
}
#endif

