/* compress_sword2 - Compress Broken Sword II sound clusters into MP3/Ogg Vorbis
 * Copyright (C) 2004-2006  The ScummVM Team
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
 *
 * $URL$
 * $Id$
 *
 */

#include "compress_sword2.h"

#define TEMP_IDX	"tempfile.idx"
#define TEMP_DAT	"tempfile.dat"

uint32 CompressSword2::append_to_file(Common::File &f1, const char *filename) {
	uint32 length, orig_length;
	size_t size;
	char fbuf[2048];

	Common::File f2(filename, "rb");
	orig_length = length = f2.size();

	while (length > 0) {
		size = f2.read_noThrow(fbuf, length > sizeof(fbuf) ? sizeof(fbuf) : length);
		if (size <= 0) {
			break;
		}

		length -= size;
		f1.write(fbuf, size);
	}
	return orig_length;
}

#define GetCompressedShift(n)      ((n) >> 4)
#define GetCompressedSign(n)       (((n) >> 3) & 1)
#define GetCompressedAmplitude(n)  ((n) & 7)

CompressSword2::CompressSword2(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input;
	input.format = "*.clu";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress Broken Sword 2 data files.";
	_helptext = "\nUsage: " + getName() + " [params] <file>\n\n";
}

void CompressSword2::execute() {
	int j;
	uint32 indexSize;
	uint32 totalSize;
	uint32 length;
	
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	switch (_format) {
	case AUDIO_MP3:
		_audioOutputFilename = TEMP_MP3;
		break;
	case AUDIO_VORBIS:
		_audioOutputFilename = TEMP_OGG;
		break;
	case AUDIO_FLAC:
		_audioOutputFilename = TEMP_FLAC;
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	if (outpath.empty())
		// Extensions change between the in/out files, so we can use the same directory
		outpath = inpath;

	_input.open(inpath, "rb");

	indexSize = _input.readUint32LE();
	totalSize = 12 * (indexSize + 1);

	if (_input.readUint32BE() != 0xfff0fff0) {
		error("This doesn't look like a cluster file");
	}

	_output_idx.open(TEMP_IDX, "wb");
	_output_snd.open(TEMP_DAT, "wb");

	_output_idx.writeUint32LE(indexSize);
	_output_idx.writeUint32BE(0xfff0fff0);
	_output_idx.writeUint32BE(0xfff0fff0);

	for (int i = 0; i < (int)indexSize; i++) {
		// Update progress, this loop is where most of the time is spent
		updateProgress(i, indexSize);

		uint32 pos;
		uint32 enc_length;

		_input.seek(8 * (i + 1), SEEK_SET);

		pos = _input.readUint32LE();
		length = _input.readUint32LE();

		if (pos != 0 && length != 0) {
			uint16 prev;

			Common::File f(TEMP_WAV, "wb");

			/*
			 * The number of decodeable 16-bit samples is one less
			 * than the length of the resource.
			 */

			length--;

			/*
			 * Back when this tool was written, encodeAudio() had
			 * no way of encoding 16-bit data, so it was simpler to
			 * output a WAV file.
			 */

			f.writeUint32BE(0x52494646);	/* "RIFF" */
			f.writeUint32LE(2 * length + 36);
			f.writeUint32BE(0x57415645);	/* "WAVE" */
			f.writeUint32BE(0x666d7420);	/* "fmt " */
			f.writeUint32LE(16);
			f.writeUint16LE(1);		/* PCM */
			f.writeUint16LE(1);		/* mono */
			f.writeUint32LE(22050);	/* sample rate */
			f.writeUint32LE(44100);	/* bytes per second */
			f.writeUint16LE(2);		/* basic block size */
			f.writeUint16LE(16);		/* sample width */
			f.writeUint32BE(0x64617461);	/* "data" */
			f.writeUint32LE(2 * length);

			_input.seek(pos, SEEK_SET);

			/*
			 * The first sample is stored uncompressed. Subsequent
			 * samples are stored as some sort of 8-bit delta.
			 */

			prev = _input.readUint16LE();

			f.writeUint16LE(prev);

			for (j = 1; j < (int)length; j++) {
				byte data;
				uint16 out;

				data = _input.readByte();
				if (GetCompressedSign(data))
					out = prev - (GetCompressedAmplitude(data) << GetCompressedShift(data));
				else
					out = prev + (GetCompressedAmplitude(data) << GetCompressedShift(data));

				f.writeUint16LE(out);
				prev = out;
			}

			encodeAudio(TEMP_WAV, false, -1, tempEncoded, _format);
			enc_length = append_to_file(_output_snd, tempEncoded);

			_output_idx.writeUint32LE(totalSize);
			_output_idx.writeUint32LE(length);
			_output_idx.writeUint32LE(enc_length);
			totalSize = totalSize + enc_length;
		} else {
			_output_idx.writeUint32LE(0);
			_output_idx.writeUint32LE(0);
			_output_idx.writeUint32LE(0);
		}
	}

	Common::File output(outpath, "wb");

	append_to_file(output, TEMP_IDX);
	append_to_file(output, TEMP_DAT);

	output.close();
	_output_snd.close();
	_output_idx.close();

	unlink(TEMP_DAT);
	unlink(TEMP_IDX);
	unlink(TEMP_MP3);
	unlink(TEMP_OGG);
	unlink(TEMP_FLAC);
	unlink(TEMP_WAV);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressSword2 sword2(argv[0]);
	return sword2.run(argc, argv);
}
#endif

