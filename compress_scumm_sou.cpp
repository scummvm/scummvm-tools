
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

#include "compress_scumm_sou.h"


static const char f_hdr[] = {
	'S', 'O', 'U', ' ', 0, 0, 0, 0, 0
};

#define OUTPUT_MP3	"monster.so3"
#define OUTPUT_OGG	"monster.sog"
#define OUTPUT_FLAC	"monster.sof"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_IDX	"tempfile.idx"

void CompressScummSou::end_of_file(const char *inputPath) {
	int idx_size = _output_idx.pos();
	size_t size;
	char buf[2048];

	_output_snd.close();
	_output_idx.close();

	_output_idx.open(_audioOuputFilename, "wb");
	_output_idx.writeUint32BE((uint32)idx_size);

	File in(TEMP_IDX, "rb");
	while ((size = in.readN(buf, 1, 2048)) > 0) {
		_output_idx.write(buf, 1, size);
	}

	in.open(TEMP_DAT, "rb");
	while ((size = in.readN(buf, 1, 2048)) > 0) {
		_output_idx.write(buf, 1, size);
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

void CompressScummSou::get_part(const char *inputPath) {
	uint32 tot_size;
	int size;
	char fbuf[2048];

	char buf[2048];
	int pos = _input.pos();
	uint32 tags;

	/* Scan for the VCTL header */
	_input.read(buf, 1, 4);
	/* The demo (snmdemo) and floppy version of Sam & Max use VTTL */
	while (memcmp(buf, "VCTL", 4)&&memcmp(buf, "VTTL", 4)) {
		pos++;
		append_byte(4, buf);
		if (feof(_input)) {
			end_of_file(inputPath);
			return;
		}
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

	_input.read(buf, 1, 8);
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
	File f(tempEncoded, "rb");
	tot_size = 0;
	while ((size = f.read(fbuf, 1, 2048)) > 0) {
		tot_size += size;
		_output_snd.write(fbuf, 1, size);
	}

	_output_idx.writeUint32BE(tot_size);
}

CompressScummSou::CompressScummSou(const std::string &name) : CompressionTool(name) {
	_audioOuputFilename = OUTPUT_MP3;
	
	ToolInput input;
	input.format = "*.sou";
	_inputPaths.push_back(input);

	_helptext = "\nUsage: " + _name + " [mode] [mode params] monster.sou\n" kCompressionAudioHelp;
}

void CompressScummSou::execute() {
	char buf[2048];

	Filename inpath(_inputPaths[0].path);
	//Filename &outpath = _outputPath;

	switch (_format) {
	case AUDIO_MP3:
		_audioOuputFilename = OUTPUT_MP3;
		break;
	case AUDIO_VORBIS:
		_audioOuputFilename = OUTPUT_OGG;
		break;
	case AUDIO_FLAC:
		_audioOuputFilename = OUTPUT_FLAC;
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	_input.open(inpath.getFullPath().c_str(), "rb");
	_output_idx.open(TEMP_IDX, "wb");
	_output_snd.open(TEMP_DAT, "wb");

	/* Get the 'SOU ....' header */
	_input.read(buf, 1, 8);
	if (strncmp(buf, f_hdr, 8)) {
		error("Bad SOU");
	}

	while (true)
		get_part(inpath.getFullPath().c_str());
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressScummSou scummsou(argv[0]);
	return scummsou.run(argc, argv);
}
#endif

