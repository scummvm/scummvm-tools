/* compress_kyra_bun - compressor for kyra sound file packages
 * Copyright (C) 2006  The ScummVM Team
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

#include "compress_kyra.h"

#include "compress.h"
#include "kyra_pak.h"

#define TEMPFILE "TEMP.VOC"

CompressKyra::CompressKyra(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress Legend of Kyrandia games.";
	_helptext = "\nUsage: " + getName() + " [mode params] [-o outfile] <infile>\n";
}

void CompressKyra::execute() {
	Filename inpath(_inputPaths[0].path);
	Filename &outpath = _outputPath;

	if (inpath == outpath)
		error("Infile and outfile cannot be the same file");

	bool isKyra3 = detectKyra3File(&inpath);
	if (!isKyra3)
		process(&inpath, &outpath);
	else
		processKyra3(&inpath, &outpath);
}

void CompressKyra::process(Filename *infile, Filename *outfile) {
	PAKFile input, output;

	if (!input.loadFile(infile->getFullPath().c_str(), false))
		return;

	if (!output.loadFile(NULL, false))
		return;

	PAKFile::cFileList *list = input.getFileList();

	for (; list; list = list->next) {
		// Detect VOC file from content instead of extension. This is needed for Lands of Lore TLK files.
		if (memcmp(list->data, "Creative Voice File", 19) != 0)
			continue;

		if (list->data[26] != 1) {
			warning("'%s' contains broken VOC file '%s' skipping it...", infile->getFullPath().c_str(), list->filename);
			continue;
		}

		Filename outputName;
		input.outputFileAs(list->filename, TEMPFILE);
		outputName._path = list->filename;

		File tempFile(TEMPFILE, "rb");
		tempFile.seek(26, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, tempFile, _format);
		tempFile.close();

		outputName.setExtension(audio_extensions(_format));

		output.addFile(outputName.getFullPath().c_str(), tempEncoded);

		unlink(TEMPFILE);
		unlink(TEMP_RAW);
		unlink(tempEncoded);
	}

	if (output.getFileList())
		output.saveFile(outfile->getFullPath().c_str());
	else
		print("file '%s' doesn't contain any .voc files\n", infile->getFullPath().c_str());
}

// Kyra3 specifc code

uint16 CompressKyra::clip8BitSample(int16 sample) {
	if (sample > 255)
		return 255;
	if (sample < 0)
		return 0;
	return sample;
}

int CompressKyra::decodeChunk(File &in, File &out) {
	uint16 size = in.readUint16LE();
	uint16 outSize = in.readUint16LE();
	uint32 id = in.readUint32LE();
	byte *inputBuffer, *outputBuffer;
	int bytesRead = 0;

	int16 curSample;
	uint8 code;
	int8 count;
	uint16 input;
	int i, j;

	uint16 remaining;

	const int8 WSTable2Bit[] = { -2, -1, 0, 1 };
	const int8 WSTable4Bit[] = {
		-9, -8, -6, -5, -4, -3, -2, -1,
		 0,  1,  2,  3,  4,  5,  6,  8
	};

	assert(id == 0x0000DEAF);

	bytesRead += (8 + size);

	outputBuffer = (byte *)malloc(outSize);
	assert(outputBuffer);

	if (size == outSize) {
		int readSize = size;
		while (readSize > 0) {
			int read = in.read(outputBuffer, 1, readSize);
			if (read <= 0)
				error("[1] Couldn't read data");
			readSize -= read;
		}
		while (size > 0)  {
			int written = out.write(outputBuffer, 1, size);
			size -= written;
		}
		free(outputBuffer);
		return bytesRead;
	}

	inputBuffer = (byte *)malloc(size);
	assert(inputBuffer);

	int readSize = size;
	while (readSize > 0) {
		int read = in.read(inputBuffer, 1, readSize);
		if (read <= 0)
			error("[2] Couldn't read data");
		readSize -= read;
	}

	curSample = 0x80;
	i = 0;
	j = 0;

	remaining = outSize;

	while (remaining > 0) {
		input = inputBuffer[i++] << 2;
		code = (input >> 8) & 0xff;
		count = (input & 0xff) >> 2;

		switch (code) {
		case 2:
			if (count & 0x20) {
				/* NOTE: count is signed! */
				count <<= 3;
				curSample += (count >> 3);
				outputBuffer[j++] = (byte)curSample;
				remaining--;
			} else {
				for (; count >= 0; count--) {
					outputBuffer[j++] = inputBuffer[i++];
					remaining--;
				}
				curSample = inputBuffer[i - 1];
			}
			break;
		case 1:
			for (; count >= 0; count--) {
				code = inputBuffer[i++];

				curSample += WSTable4Bit[code & 0x0f];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable4Bit[code >> 4];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				remaining -= 2;
			}
			break;
		case 0:
			for (; count >= 0; count--) {
				code = inputBuffer[i++];

				curSample += WSTable2Bit[code & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 2) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 4) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				curSample += WSTable2Bit[(code >> 6) & 0x03];
				curSample = clip8BitSample(curSample);
				outputBuffer[j++] = (byte)curSample;

				remaining -= 4;
			}
			break;
		default:
			for (; count >= 0; count--) {
				outputBuffer[j++] = (byte)curSample;
				remaining--;
			}
		}
	}

	while (outSize > 0)  {
		int written = out.write(outputBuffer, 1, outSize);
		if (written <= 0)
			error("[2] Couldn't write data");
		outSize -= written;
	}

	free(inputBuffer);
	free(outputBuffer);

	return bytesRead;
}

typedef struct {
	uint16 freq;
	uint32 size;
	byte flags;
	byte type;
} AUDHeader;

void CompressKyra::compressAUDFile(File &input, const char *outfile) {
	AUDHeader header;

	header.freq = input.readUint16LE();
	header.size = input.readUint32LE();
	header.flags = input.readByte();
	header.type = input.readByte();
	//print("%d Hz, %d bytes, type %d (%08X)\n", header.freq, header.size, header.type, header.flags);

	File output(TEMP_RAW, "wb");

	uint32 remaining = header.size;
	while (remaining > 0)
		remaining -= decodeChunk(input, output);

	encodeAudio(TEMP_RAW, true, header.freq, outfile, _format);

	unlink(TEMP_RAW);
}

struct CompressKyra::DuplicatedFile {
	uint32 resFilename;
	uint32 resOffset;
};

const CompressKyra::DuplicatedFile *CompressKyra::findDuplicatedFile(uint32 resOffset, const DuplicatedFile *list, const uint32 maxEntries) {
	for (uint32 i = 0; i < maxEntries; ++i) {
		if (list[i].resOffset == resOffset && list[i].resOffset != 0)
			return &list[i];
	}

	return 0;
}

void CompressKyra::processKyra3(Filename *infile, Filename *outfile) {
	if (infile->hasExtension("AUD")) {
		outfile->setExtension(audio_extensions(_format));

		File input(*infile, "rb");

		compressAUDFile(input, outfile->getFullPath().c_str());
	} else if (infile->hasExtension("TLK")) {
		PAKFile output;

		File input(*infile, "rb");

		if (!output.loadFile(NULL, false))
			return;

		uint16 files = input.readUint16LE();
		DuplicatedFile *red = new DuplicatedFile[files];
		memset(red, 0, sizeof(DuplicatedFile)*files);

		for (uint16 i = 0; i < files; ++i) {
			uint32 resFilename = input.readUint32LE();
			uint32 resOffset = input.readUint32LE();

			char outname[16];
			snprintf(outname, 16, "%.08u.%s", resFilename, audio_extensions(_format));

			const DuplicatedFile *file = findDuplicatedFile(resOffset, red, files);
			if (file) {
				char linkname[16];
				snprintf(linkname, 16, "%.08u.%s", file->resFilename, audio_extensions(_format));

				output.linkFiles(outname, linkname);
			} else {
				red[i].resFilename = resFilename;
				red[i].resOffset = resOffset;

				uint32 pos = (uint32)input.pos();
				input.seek(resOffset + 4, SEEK_SET);

				compressAUDFile(input, outname);

				output.addFile(outname, outname);

				unlink(outname);

				input.seek(pos, SEEK_SET);
			}
		}

		delete[] red;

		if (output.getFileList())
			output.saveFile(outfile->getFullPath().c_str());
	} else {
		error("Unsupported file '%s'", infile->getFullPath().c_str());
	}
}

bool CompressKyra::detectKyra3File(Filename *infile) {
	if (infile->hasExtension("AUD")) {
		return true;
	} else if (infile->hasExtension("VRM") || infile->hasExtension("PAK")) {
		if (!PAKFile::isPakFile(infile->getFullPath().c_str()))
			error("Unknown filetype of file: '%s'", infile->getFullPath().c_str());
		return false;
	} else if (infile->hasExtension("TLK")) {
		if (PAKFile::isPakFile(infile->getFullPath().c_str()))
			return false;

		File f(*infile, "rb");

		uint16 entries = f.readUint16LE();
		uint32 entryTableSize = (entries * 8);
		const uint32 filesize = f.size();

		if (entryTableSize + 2 > filesize) {
			error("Unknown filetype of file: '%s'", infile->getFullPath().c_str());
		}

		uint32 offset = 0;
		for (uint16 i = 0; i < entries; ++i) {
			f.readUint32LE();
			offset = f.readUint32LE();

			if (offset > filesize)
				error("Unknown filetype of file: '%s'", infile->getFullPath().c_str());
		}

		return true;
	}

	error("Unknown filetype of file: '%s'", infile->getFullPath().c_str());

	// Never reached
	return false;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	return export_main(compress_kyra)(argc, argv);
}
#endif

