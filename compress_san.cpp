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
	int32 lessIACTSize;
};

static byte _IACToutput[0x1000];
static int _IACTpos = 0;
static FILE *_waveTmpFile;
static int32 _waveDataSize;

void encodeWaveWithOgg(char *filename) {
	char fbuf[2048];
	char *tmp = fbuf;
	bool err = false;

	sprintf(fbuf, "oggenc -q 0 %s", filename);
	err = system(fbuf) != 0;
	if (err) {
		printf("Got error from encoder. (check your parameters)\n");
		printf("Encoder Commandline: %s\n", fbuf );
		exit(-1);
	}
}

void writeWaveHeader(int s_size) {
	int rate = 22050;
	int bits = 16;
	int chan = 2;
	byte wav[44];
	memset (wav, 0,	44);
	wav[0] = 'R';
	wav[1] = 'I';
	wav[2] = 'F';
	wav[3] = 'F';
	wav[4] = (s_size + 36) & 0xff;
	wav[5] = ((s_size +	36)	>> 8) &	0xff;
	wav[6] = ((s_size +	36)	>> 16) & 0xff;
	wav[7] = ((s_size +	36)	>> 24) & 0xff;
	wav[8] = 'W';
	wav[9] = 'A';
	wav[10]	= 'V';
	wav[11]	= 'E';
	wav[12]	= 'f';
	wav[13]	= 'm';
	wav[14]	= 't';
	wav[15]	= 0x20;
	wav[16]	= 16;
	wav[20]	= 1;
	wav[22]	= chan;
	wav[24]	= rate & 0xff;
	wav[25]	= (rate	>> 8) &	0xff;
	wav[26]	= (rate	>> 16) & 0xff;
	wav[27]	= (rate	>> 24) & 0xff;
	wav[28]	= (rate	* chan * (bits / 8)) & 0xff;
	wav[29]	= ((rate * chan	* (bits	/ 8))>>	8) & 0xff;
	wav[30]	= ((rate * chan	* (bits	/ 8)) >> 16) & 0xff;
	wav[31]	= ((rate * chan	* (bits	/ 8)) >> 24) & 0xff;
	wav[32]	= (chan	* (bits	/ 8)) &	0xff;
	wav[33]	= ((chan * (bits / 8)) >> 8) & 0xff;
	wav[34]	= bits;
	wav[36]	= 'd';
	wav[37]	= 'a';
	wav[38]	= 't';
	wav[39]	= 'a';
	wav[40]	= s_size & 0xff;
	wav[41]	= (s_size >> 8)	& 0xff;
	wav[42]	= (s_size >> 16) & 0xff;
	wav[43]	= (s_size >> 24) & 0xff;

	fseek(_waveTmpFile, 0, SEEK_SET);
	if (fwrite(wav, 1, 44, _waveTmpFile) != 44) {
		printf("error write temp wave file");
		exit(1);
	}
	fclose(_waveTmpFile);
	_waveTmpFile = NULL;
}
void writeToTempWave(byte *output_data, int size) {
	if (!_waveTmpFile) {
		_waveTmpFile = fopen("tmp.wav", "wb");
		if (!_waveTmpFile) {
			printf("error write temp wave file");
			exit(1);
		}
		byte wav[44];
		memset(wav, 0, 44);
		if (fwrite(output_data, 1, 44, _waveTmpFile) != 44) {
			printf("error write temp wave file");
			exit(1);
		}
		_waveDataSize = 0;
	}
	for (int j = 0; j < size; j += 2) {
		byte tmp = output_data[j + 0];
		output_data[j + 0] = output_data[j + 1];
		output_data[j + 1] = tmp;
	}

	if (fwrite(output_data, 1, size, _waveTmpFile) != size) {
		printf("error write temp wave file");
		exit(1);
	}
	_waveDataSize += 0x1000;
}

void decompressComiIACT(byte *output_data, byte *d_src, int bsize) {
	byte value;

	while (bsize > 0) {
		if (_IACTpos >= 2) {
			int32 len = *(uint16 *)(_IACToutput);
			len = TO_BE_16(len) + 2;
			len -= _IACTpos;
			if (len > bsize) {
				memcpy(_IACToutput + _IACTpos, d_src, bsize);
				_IACTpos += bsize;
				bsize = 0;
			} else {
				memcpy(_IACToutput + _IACTpos, d_src, len);
				byte *dst = output_data;
				byte *d_src2 = _IACToutput;
				d_src2 += 2;
				int32 count = 1024;
				byte variable1 = *d_src2++;
				byte variable2 = variable1 / 16;
				variable1 &= 0x0f;
				do {
					value = *(d_src2++);
					if (value == 0x80) {
						*dst++ = *d_src2++;
						*dst++ = *d_src2++;
					} else {
						int16 val = (int8)value << variable2;
						*dst++ = val >> 8;
						*dst++ = (byte)(val);
					}
					value = *(d_src2++);
					if (value == 0x80) {
						*dst++ = *d_src2++;
						*dst++ = *d_src2++;
					} else {
						int16 val = (int8)value << variable1;
						*dst++ = val >> 8;
						*dst++ = (byte)(val);
					}
				} while (--count);
				writeToTempWave(output_data, 0x1000);
				bsize -= len;
				d_src += len;
				_IACTpos = 0;
			}
		} else {
			if (bsize > 1 && _IACTpos == 0) {
				*(_IACToutput + 0) = *d_src++;
				_IACTpos = 1;
				bsize--;
			}
			*(_IACToutput + _IACTpos) = *d_src++;
			_IACTpos++;
			bsize--;
		}
	}
}

int main(int argc, char *argv[]) {
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
		flu = fopen(argv[3], "rb+");
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
		bool first_fobj = true;
		tag = readUint32BE(input); // chunk tag
		assert(tag == 'FRME');
		writeUint32BE(output, tag); // FRME
		int32 frameSize = readUint32BE(input); // FRME size
		frameInfo[l].frameSize = frameSize;
		frameInfo[l].offsetOutput = ftell(output);
		frameInfo[l].fobjDecompressedSize = 0;
		frameInfo[l].fobjCompressedSize = 0;
		frameInfo[l].lessIACTSize = 0;
		writeUint32BE(output, frameSize);
		for (;;) {
			tag = readUint32BE(input); // chunk tag
			if (feof(input))
				break;
			if (tag == 'FRME') {
				fseek(input, -4, SEEK_CUR);
				break;
			} else if ((tag == 'FOBJ') && (first_fobj)) {
				size = readUint32BE(input); // FOBJ size
				if ((size & 1) != 0)
					size++;
				first_fobj = false;
				unsigned long outputSize = size + (size / 9) + 100;
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
				for (unsigned int k = 0; k < outputSize; k++) {
					writeByte(output, *(zlibOutputBuffer + k)); // compressed FOBJ datas
				}
				free(zlibInputBuffer);
				free(zlibOutputBuffer);
				continue;
			} else if (tag == 'IACT') {
				size = readUint32BE(input); // chunk size
				fseek(input, 2, SEEK_CUR);
				int flags = readUint16LE(input);
				int unk = readUint16BE(input);
				int track_flags = readUint16LE(input);
				// to be sure that is comi chunk
				if ((track_flags != 0) || (unk != 0) || (flags != 46)) {
					fseek(input, -8, SEEK_CUR);
					goto skip;
				}
				fseek(input, 10, SEEK_CUR);
				int bsize = size - 18;
				byte output_data[0x1000];
				byte *src = (byte *)malloc(bsize);
				fread(src, bsize, 1, input);
				decompressComiIACT(output_data, src, bsize);
				free(src);

				if ((size & 1) != 0) {
					fseek(input, 1, SEEK_CUR);
					size++;
				}
				frameInfo[l].lessIACTSize = size + 8;
				continue;
			} else {
skip:
				size = readUint32BE(input); // chunk size
				writeUint32BE(output, tag);
				writeUint32BE(output, size);
				if ((size & 1) != 0)
					size++;
				for (int k = 0; k < size; k++) {
					writeByte(output, readByte(input)); // chunk datas
				}
			}
		}
	}

	if (_waveTmpFile) {
		writeWaveHeader(_waveDataSize);
		encodeWaveWithOgg("tmp.wav");
	}

	fclose(input);

	printf("Fixing frames header...");
	int32 sumDiff = 0;
	for (l = 0; l < nbframes; l++) {
		int32 diff = 0;
		if (frameInfo[l].fobjCompressedSize != 0) {
			diff += frameInfo[l].fobjDecompressedSize - (frameInfo[l].fobjCompressedSize + 4);
		}
		if (frameInfo[l].lessIACTSize != 0) {
			diff += frameInfo[l].lessIACTSize;
		}
		fseek(output, frameInfo[l].offsetOutput, SEEK_SET);
		sumDiff += diff;
		if (diff != 0)
			writeUint32BE(output, frameInfo[l].frameSize - diff);
	}
	printf("done.\n");

	printf("Fixing anim header...");
	fseek(output, 4, SEEK_SET);
	writeUint32BE(output, animChunkSize - sumDiff);
	printf("done.\n");

	if (flu) {
		printf("Fixing flu offsets...");
		fseek(flu, 0x324, SEEK_SET);
		for (l = 0; l < nbframes; l++) {
			writeUint32LE(flu, frameInfo[l].offsetOutput - 4);
		}
		fclose(flu);
		printf("done.\n");
	}

	free(frameInfo);
	
	fclose(output);

	printf("compression done.\n");
		
	return 0;
}
