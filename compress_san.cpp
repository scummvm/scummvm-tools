/* compress_san - zlib compressor for FOBJ chunks in smush san files
 * Copyright (C) 2004  The ScummVM Team
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include "util.h"
#include "zlib.h"

const char *tag2str(uint32 tag) {
	static char str[5];
	str[0] = (char)(tag >> 24);
	str[1] = (char)(tag >> 16);
	str[2] = (char)(tag >> 8);
	str[3] = (char)tag;
	str[4] = '\0';
	return str;
}

void showhelp(char *exename) {
	printf("\nUsage: %s <inputfile>.san <outputfile>.san [<file>.flu>]\n", exename);
//	printf("\nParams:\n");
//	printf("\n --help     this help message\n");
	exit(2);
}

struct FrameInfo {
	int32 frameSize;
	int32 offsetOutput;
	int32 fobjDecompressedSize;
	int32 fobjCompressedSize;
};

int main(int argc, char *argv[]) {
	int i;
	if (argc < 3)
		showhelp(argv[0]);

	FILE *input = fopen(argv[1], "rb");
	if (!input) {
		printf("Cannot open file: %s\n", argv[1]);
		exit(-1);
	}

	FILE *output = fopen(argv[2], "wb");
	if (!output) {
		printf("Cannot open file: %s\n", argv[2]);
		exit(-1);
	}

	FILE *flu = NULL;
	if (argc == 4) {
		flu = fopen(argv[3], "wb");
		if (!flu) {
			printf("Cannot open file: %s\n", argv[3]);
			exit(-1);
		}
	}

	uint32 tag;
	int32 l, size;

	writeUint32BE(output, readUint32BE(input)); // ANIM
	int32 animChunkSize = readUint32BE(input); // ANIM size
	writeUint32BE(output, animChunkSize);

	writeUint32BE(output, readUint32BE(input)); // AHDR
	size = readUint32BE(input);
	writeUint32BE(output, size); // AHDR size
	writeUint16BE(output, readUint16BE(input)); // version
	int32 nbframes = readUint16LE(input); // number frames
	writeUint16LE(output, nbframes);
	writeUint16BE(output, readUint16BE(input)); // unk
	for (l = 0; l < size - 6; l++) {
		writeByte(output, readByte(input)); // 0x300 palette + some bytes
	}

	FrameInfo *frameInfo = (FrameInfo *)malloc(sizeof(FrameInfo) * nbframes);

	for (l = 0; l < nbframes; l++) {
		printf("frame: %d\n", l);
		tag = readUint32BE(input); // chunk tag
		assert(tag == TO_LE_32('FRME'));
		writeUint32BE(output, tag); // FRME
		int32 frameSize = readUint32BE(input); // FRME size
		frameInfo[l].frameSize = frameSize;
		frameInfo[l].offsetOutput = ftell(output);
		frameInfo[l].fobjDecompressedSize = 0;
		frameInfo[l].fobjCompressedSize = 0;
		writeUint32BE(output, frameSize);
		for (;;) {
			tag = readUint32BE(input); // chunk tag
			if (feof(input))
				break;
			if (tag == TO_LE_32('FRME')) {
				fseek(input, -4, SEEK_CUR);
				break;
			} else if (tag != TO_LE_32('FOBJ')) {
				size = readUint32BE(input); // chunk size
				writeUint32BE(output, tag);
				writeUint32BE(output, size);
				if ((size & 1) != 0)
					size++;
				for (int k = 0; k < size; k++) {
					writeByte(output, readByte(input)); // chunk datas
				}
			} else if (tag == TO_LE_32('FOBJ')) {
				size = readUint32BE(input); // FOBJ size
				if ((size & 1) != 0)
					size++;
				unsigned long outputSize = size + (size / 9);
				byte *zlibInputBuffer = (byte *)malloc(size);
				byte *zlibOutputBuffer = (byte *)malloc(outputSize);
				for (int k = 0; k < size; k++) {
					*(zlibInputBuffer + k) = readByte(input); // FOBJ datas
				}
				int result = compress2(zlibOutputBuffer, &outputSize, zlibInputBuffer, size, 9);
				if (result != Z_OK) {
					error("compression error");
				}
				if ((outputSize & 1) != 0)
					outputSize++;
				frameInfo[l].fobjDecompressedSize = size;
				frameInfo[l].fobjCompressedSize = outputSize;
				writeUint32BE(output, 'ZFOB');
				writeUint32BE(output, outputSize + 4);
				writeUint32BE(output, size);
				for (int k = 0; k < outputSize; k++) {
					writeByte(output, *(zlibOutputBuffer + k)); // compressed FOBJ datas
				}
				free(zlibInputBuffer);
				free(zlibOutputBuffer);
			}
		}
	}

	fclose(input);

	int32 sumDiff = 0;
	for (l = 0; l < nbframes; l++) {
		if (frameInfo[l].fobjCompressedSize == 0)
			continue;
		fseek(output, frameInfo[l].offsetOutput, SEEK_SET);
		int32 diff = frameInfo[l].fobjDecompressedSize - (frameInfo[l].fobjCompressedSize + 4);
		sumDiff += diff;
		writeUint32BE(output, frameInfo[l].frameSize - diff);
	}

	fseek(output, 4, SEEK_SET);
	writeUint32BE(output, animChunkSize - sumDiff);

	if (flu) {
		fseek(flu, 0x308, SEEK_SET);
		for (l = 0; l < nbframes; l++) {
			writeUint32BE(output, frameInfo[l].offsetOutput - 4);
		}
		fclose(flu);
	}

	free(frameInfo);
	
	fclose(output);
		
	return 0;
}
