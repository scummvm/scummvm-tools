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

#include "compress.h"

#define TEMP_IDX	"tempfile.idx"
#define TEMP_DAT	"tempfile.dat"

static FILE *input, *output_idx, *output_snd;

static CompressMode gCompMode = kMP3Mode;

uint32 append_to_file(FILE *f1, const char *filename) {
	FILE *f2;
	uint32 length, orig_length;
	size_t size;
	char fbuf[2048];

	f2 = fopen(filename, "rb");
	if (!f2) {
		error("Cannot open file %s for reading", filename);
	}

	orig_length = length = fileSize(f2);

	while (length > 0) {
		size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, f2);
		if (size <= 0) {
			break;
		}

		length -= size;
		fwrite(fbuf, 1, size, f1);
	}

	fclose(f2);
	return orig_length;
}

#define GetCompressedShift(n)      ((n) >> 4)
#define GetCompressedSign(n)       (((n) >> 3) & 1)
#define GetCompressedAmplitude(n)  ((n) & 7)

const char *helptext = "\nUsage: %s [params] <file>\n\n" kCompressionAudioHelp;

int main(int argc, char *argv[]) {
	FILE *output, *f;
	int j;
	uint32 indexSize;
	uint32 totalSize;
	uint32 length;
	
	Filename inpath, outpath;
	int first_arg = 1;
	int last_arg = argc - 1;

	parseHelpArguments(argv, argc, helptext);

	/* compression mode */
	gCompMode = process_audio_params(argc, argv, &first_arg);

	// Now we try to find the proper output file
	// also make sure we skip those arguments
	if (parseOutputFileArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputFileArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;

	switch(gCompMode) {
	case kMP3Mode:
		tempEncoded = TEMP_MP3;
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		break;
	default:
		displayHelp(helptext, argv[0]);
		break;
	}

	inpath.setFullPath(argv[first_arg]);

	if(outpath.empty())
		// Extensions change between the in/out files, so we can use the same directory
		outpath = inpath;

	input = fopen(inpath.getFullPath(), "rb");
	if (!input) {
		error("Cannot open file: %s", inpath.getFullPath());
	}

	indexSize = readUint32LE(input);
	totalSize = 12 * (indexSize + 1);

	if (readUint32BE(input) != 0xfff0fff0) {
		error("This doesn't look like a cluster file");
	}

	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		error("Cannot open file " TEMP_IDX " for writing");
	}

	output_snd = fopen(TEMP_DAT, "wb");
	if (!output_snd) {
		error("Cannot open file " TEMP_DAT " for writing");
	}

	writeUint32LE(output_idx, indexSize);
	writeUint32BE(output_idx, 0xfff0fff0);
	writeUint32BE(output_idx, 0xfff0fff0);

	for (int i = 0; i < (int)indexSize; i++) {
		uint32 pos;
		uint32 enc_length;

		fseek(input, 8 * (i + 1), SEEK_SET);

		pos = readUint32LE(input);
		length = readUint32LE(input);

		if (pos != 0 && length != 0) {
			uint16 prev;

			f = fopen(TEMP_WAV, "wb");
			if (!f) {
				error("Cannot open file %s for writing", TEMP_WAV);
			}

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

			writeUint32BE(f, 0x52494646);	/* "RIFF" */
			writeUint32LE(f, 2 * length + 36);
			writeUint32BE(f, 0x57415645);	/* "WAVE" */
			writeUint32BE(f, 0x666d7420);	/* "fmt " */
			writeUint32LE(f, 16);
			writeUint16LE(f, 1);		/* PCM */
			writeUint16LE(f, 1);		/* mono */
			writeUint32LE(f, 22050);	/* sample rate */
			writeUint32LE(f, 44100);	/* bytes per second */
			writeUint16LE(f, 2);		/* basic block size */
			writeUint16LE(f, 16);		/* sample width */
			writeUint32BE(f, 0x64617461);	/* "data" */
			writeUint32LE(f, 2 * length);

			fseek(input, pos, SEEK_SET);

			/*
			 * The first sample is stored uncompressed. Subsequent
			 * samples are stored as some sort of 8-bit delta.
			 */

			prev = readUint16LE(input);

			writeUint16LE(f, prev);

			for (j = 1; j < (int)length; j++) {
				byte data;
				uint16 out;

				data = readByte(input);
				if (GetCompressedSign(data))
					out = prev - (GetCompressedAmplitude(data) << GetCompressedShift(data));
				else
					out = prev + (GetCompressedAmplitude(data) << GetCompressedShift(data));

				writeUint16LE(f, out);
				prev = out;
			}
			fclose(f);

			encodeAudio(TEMP_WAV, false, -1, tempEncoded, gCompMode);
			enc_length = append_to_file(output_snd, tempEncoded);

			writeUint32LE(output_idx, totalSize);
			writeUint32LE(output_idx, length);
			writeUint32LE(output_idx, enc_length);
			totalSize = totalSize + enc_length;
		} else {
			writeUint32LE(output_idx, 0);
			writeUint32LE(output_idx, 0);
			writeUint32LE(output_idx, 0);
		}
	}

	fclose(output_idx);
	fclose(output_snd);

	output = fopen(outpath.getFullPath(), "wb");
	if (!output) {
		error("Cannot open file %s for writing", outpath.getFullPath());
	}

	append_to_file(output, TEMP_IDX);
	append_to_file(output, TEMP_DAT);

	fclose(output);
	unlink(TEMP_DAT);
	unlink(TEMP_IDX);
	unlink(TEMP_MP3);
	unlink(TEMP_OGG);
	unlink(TEMP_FLAC);
	unlink(TEMP_WAV);

	return EXIT_SUCCESS;
}
