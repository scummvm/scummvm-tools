/* compress_scumm_bun - compressor for bundle files
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

#include <cassert>
#include <cstdlib>
#include <cstring>

#include "compress_scumm_bun.h"
#include "common/endian.h"

/*
 * The "IMC" codec below (see cases 13 & 15 in decompressCodec) is actually a
 * variant of the IMA codec, see also
 *   <http://www.multimedia.cx/simpleaudio.html>
 *
 * It is somewhat different, though: the standard ADPCM codecs use a fixed
 * size for their data packets (4 bits), while the codec implemented here
 * varies the size of each "packet" between 2 and 7 bits.
 */

static byte _imcTableEntryBitCount[89];

static const int16 imcTable[89] = {
		7,	  8,	9,	 10,   11,	 12,   13,	 14,
	   16,	 17,   19,	 21,   23,	 25,   28,	 31,
	   34,	 37,   41,	 45,   50,	 55,   60,	 66,
	   73,	 80,   88,	 97,  107,	118,  130,	143,
	  157,	173,  190,	209,  230,	253,  279,	307,
	  337,	371,  408,	449,  494,	544,  598,	658,
	  724,	796,  876,	963, 1060, 1166, 1282, 1411,
	 1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024,
	 3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484,
	 7132, 7845, 8630, 9493,10442,11487,12635,13899,
	15289,16818,18500,20350,22385,24623,27086,29794,
	32767
};

static const byte imxOtherTable[6][64] = {
	{
		0xFF,
		4
	},

	{
		0xFF, 0xFF,
		   2,    8
	},

	{
		0xFF, 0xFF, 0xFF, 0xFF,
		   1,    2,    4,    6
	},

	{
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		   1,    2,    4,    6,    8,   12,   16,   32
	},

	{
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		   1,    2,    4,    6,    8,   10,   12,   14,
		  16,   18,   20,   22,   24,   26,   28,   32
	},

	{
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
		   1,    2,    3,    4,    5,    6,    7,    8,
		   9,   10,   11,   12,   13,   14,   15,   16,
		  17,   18,   19,   20,   21,   22,   23,   24,
		  25,   26,   27,   28,   29,   30,   31,   32
	}
};

void initializeImcTables() {
	int pos;

	for (pos = 0; pos < ARRAYSIZE(imcTable); ++pos) {
		byte put = 0;
		int32 tableValue = ((imcTable[pos] * 4) / 7) / 2;
		while (tableValue != 0) {
			tableValue /= 2;
			put++;
		}
		if (put < 2) {
			put = 2;
		}
		if (put > 7) {
			put = 7;
		}
		_imcTableEntryBitCount[pos] = put;
	}
}

#define NextBit                            \
	do {                                   \
		bit = mask & 1;                    \
		mask >>= 1;                        \
		if (!--bitsleft) {                 \
			mask = READ_LE_UINT16(srcptr); \
			srcptr += 2;                   \
			bitsleft = 16;                 \
		}                                  \
	} while (0)

int32 CompressScummBun::compDecode(byte *src, byte *dst) {
	byte *result, *srcptr = src, *dstptr = dst;
	int data, size, bit, bitsleft = 16, mask = READ_LE_UINT16(srcptr);
	srcptr += 2;

	for (;;) {
		NextBit;
		if (bit) {
			*dstptr++ = *srcptr++;
		} else {
			NextBit;
			if (!bit) {
				NextBit;
				size = bit << 1;
				NextBit;
				size = (size | bit) + 3;
				data = *srcptr++ | 0xffffff00;
			} else {
				data = *srcptr++;
				size = *srcptr++;

				data |= 0xfffff000 + ((size & 0xf0) << 4);
				size = (size & 0x0f) + 3;

				if (size == 3)
					if (((*srcptr++) + 1) == 1)
						return (int32)(dstptr - dst);
			}
			result = dstptr + data;
			while (size--)
				*dstptr++ = *result++;
		}
	}
}
#undef NextBit

int32 CompressScummBun::decompressCodec(int32 codec, byte *comp_input, byte *comp_output, int32 input_size) {
	int32 output_size, channels;
	int32 offset1, offset2, offset3, length, k, c, s, j, r, t, z;
	byte *src, *t_table, *p, *ptr;
	byte t_tmp1, t_tmp2;

	switch (codec) {
	case 0:
		memcpy(comp_output, comp_input, input_size);
		output_size = input_size;
		break;

	case 1:
		output_size = compDecode(comp_input, comp_output);
		break;

	case 2:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];
		break;

	case 3:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];
		break;

	case 4:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memset(t_table, 0, output_size);

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 0;
		if (length > 0) {
			c = -12;
			s = 0;
			j = 0;
			do {
				ptr = src + length + (k >> 1);
				t_tmp2 = src[j];
				if (k & 1) {
					r = c >> 3;
					t_table[r + 2] = ((t_tmp2 & 0x0f) << 4) | (ptr[1] >> 4);
					t_table[r + 1] = (t_tmp2 & 0xf0) | (t_table[r + 1]);
				} else {
					r = s >> 3;
					t_table[r + 0] = ((t_tmp2 & 0x0f) << 4) | (ptr[0] & 0x0f);
					t_table[r + 1] = t_tmp2 >> 4;
				}
				s += 12;
				c += 12;
				k++;
				j++;
			} while (k < length);
		}
		offset1 = ((length - 1) * 3) >> 1;
		t_table[offset1 + 1] = (t_table[offset1 + 1]) | (src[length - 1] & 0xf0);
		memcpy(src, t_table, output_size);
		free(t_table);
		break;

	case 5:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memset(t_table, 0, output_size);

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 1;
		c = 0;
		s = 12;
		t_table[0] = src[length] >> 4;
		t = length + k;
		j = 1;
		if (t > k) {
			do {
				t_tmp1 = *(src + length + (k >> 1));
				t_tmp2 = src[j - 1];
				if (k & 1) {
					r = c >> 3;
					t_table[r + 0] = (t_tmp2 & 0xf0) | t_table[r];
					t_table[r + 1] = ((t_tmp2 & 0x0f) << 4) | (t_tmp1 & 0x0f);
				} else {
					r = s >> 3;
					t_table[r + 0] = t_tmp2 >> 4;
					t_table[r - 1] = ((t_tmp2 & 0x0f) << 4) | (t_tmp1 >> 4);
				}
				s += 12;
				c += 12;
				k++;
				j++;
			} while (k < t);
		}
		memcpy(src, t_table, output_size);
		free(t_table);
		break;

	case 6:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memset(t_table, 0, output_size);

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 0;
		c = 0;
		j = 0;
		s = -12;
		t_table[0] = src[output_size - 1];
		t_table[output_size - 1] = src[length - 1];
		t = length - 1;
		if (t > 0) {
			do {
				t_tmp1 = *(src + length + (k >> 1));
				t_tmp2 = src[j];
				if (k & 1) {
					r = s >> 3;
					t_table[r + 2] = (t_tmp2 & 0xf0) | t_table[r + 2];
					t_table[r + 3] = ((t_tmp2 & 0x0f) << 4) | (t_tmp1 >> 4);
				} else {
					r = c >> 3;
					t_table[r + 2] = t_tmp2 >> 4;
					t_table[r + 1] = ((t_tmp2 & 0x0f) << 4) | (t_tmp1 & 0x0f);
				}
				s += 12;
				c += 12;
				k++;
				j++;
			} while (k < t);
		}
		memcpy(src, t_table, output_size);
		free(t_table);
		break;

	case 10:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memcpy(t_table, p, output_size);

		offset1 = output_size / 3;
		offset2 = offset1 << 1;
		offset3 = offset2;
		src = comp_output;

		while (offset1--) {
			offset2 -= 2;
			offset3--;
			t_table[offset2 + 0] = src[offset1];
			t_table[offset2 + 1] = src[offset3];
		}

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 0;
		if (length > 0) {
			c = -12;
			s = 0;
			do {
				j = length + (k >> 1);
				t_tmp1 = t_table[k];
				if (k & 1) {
					r = c >> 3;
					t_tmp2 = t_table[j + 1];
					src[r + 2] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 >> 4);
					src[r + 1] = (src[r + 1]) | (t_tmp1 & 0xf0);
				} else {
					r = s >> 3;
					t_tmp2 = t_table[j];
					src[r + 0] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 & 0x0f);
					src[r + 1] = t_tmp1 >> 4;
				}
				s += 12;
				c += 12;
				k++;
			} while (k < length);
		}
		offset1 = ((length - 1) * 3) >> 1;
		src[offset1 + 1] = (t_table[length] & 0xf0) | src[offset1 + 1];
		free(t_table);
		break;

	case 11:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memcpy(t_table, p, output_size);

		offset1 = output_size / 3;
		offset2 = offset1 << 1;
		offset3 = offset2;
		src = comp_output;

		while (offset1--) {
			offset2 -= 2;
			offset3--;
			t_table[offset2 + 0] = src[offset1];
			t_table[offset2 + 1] = src[offset3];
		}

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 1;
		c = 0;
		s = 12;
		t_tmp1 = t_table[length] >> 4;
		src[0] = t_tmp1;
		t = length + k;
		if (t > k) {
			do {
				j = length + (k >> 1);
				t_tmp1 = t_table[k - 1];
				t_tmp2 = t_table[j];
				if (k & 1) {
					r = c >> 3;
					src[r + 0] = (src[r]) | (t_tmp1 & 0xf0);
					src[r + 1] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 & 0x0f);
				} else {
					r = s >> 3;
					src[r + 0] = t_tmp1 >> 4;
					src[r - 1] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 >> 4);
				}
				s += 12;
				c += 12;
				k++;
			} while (k < t);
		}
		free(t_table);
		break;

	case 12:
		output_size = compDecode(comp_input, comp_output);
		p = comp_output;
		for (z = 2; z < output_size; z++)
			p[z] += p[z - 1];
		for (z = 1; z < output_size; z++)
			p[z] += p[z - 1];

		t_table = (byte *)malloc(output_size);
		memcpy(t_table, p, output_size);

		offset1 = output_size / 3;
		offset2 = offset1 << 1;
		offset3 = offset2;
		src = comp_output;

		while (offset1--) {
			offset2 -= 2;
			offset3--;
			t_table[offset2 + 0] = src[offset1];
			t_table[offset2 + 1] = src[offset3];
		}

		src = comp_output;
		length = (output_size << 3) / 12;
		k = 0;
		c = 0;
		s = -12;
		src[0] = t_table[output_size - 1];
		src[output_size - 1] = t_table[length - 1];
		t = length - 1;
		if (t > 0) {
			do {
				j = length + (k >> 1);
				t_tmp1 = t_table[k];
				t_tmp2 = t_table[j];
				if (k & 1) {
					r = s >> 3;
					src[r + 2] = (src[r + 2]) | (t_tmp1 & 0xf0);
					src[r + 3] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 >> 4);
				} else {
					r = c >> 3;
					src[r + 2] = t_tmp1 >> 4;
					src[r + 1] = ((t_tmp1 & 0x0f) << 4) | (t_tmp2 & 0x0f);
				}
				s += 12;
				c += 12;
				k++;
			} while (k < t);
		}
		free(t_table);
		break;

	case 13:
	case 15:
		if (codec == 13) {
			channels = 1;
		} else {
			channels = 2;
		}

		{
			// Decoder for the the IMA ADPCM variants used in COMI.
			// Contrary to regular IMA ADPCM, this codec uses a variable
			// bitsize for the encoded data.

			const int MAX_CHANNELS = 2;
			int32 outputSamplesLeft;
			int32 destPos;
			int16 firstWord;
			byte initialTablePos[MAX_CHANNELS] = {0, 0};
			int32 initialimcTableEntry[MAX_CHANNELS] = {7, 7};
			int32 initialOutputWord[MAX_CHANNELS] = {0, 0};
			int32 totalBitOffset, curTablePos, outputWord;
			byte *dst;
			int i;

			// We only support mono and stereo
			assert(channels == 1 || channels == 2);

			src = comp_input;
			dst = comp_output;
			output_size = 0x2000;
			outputSamplesLeft = 0x1000;

			// Every data packet contains 0x2000 bytes of audio data
			// when extracted. In order to encode bigger data sets,
			// one has to split the data into multiple blocks.
			//
			// Every block starts with a 2 byte word. If that word is
			// non-zero, it indicates the size of a block of raw audio
			// data (not encoded) following it. That data we simply copy
			// to the output buffer and the proceed by decoding the
			// remaining data.
			//
			// If on the other hand the word is zero, then what follows
			// are 7*channels bytes containing seed data for the decoder.
			firstWord = READ_BE_UINT16(src);
			src += 2;
			if (firstWord != 0) {
				// Copy raw data
				memcpy(dst, src, firstWord);
				dst += firstWord;
				src += firstWord;
				assert((firstWord & 1) == 0);
				outputSamplesLeft -= firstWord / 2;
			} else {
				// Read the seed values for the decoder.
				for (i = 0; i < channels; i++) {
					initialTablePos[i] = *src;
					src += 1;
					initialimcTableEntry[i] = READ_BE_UINT32(src);
					src += 4;
					initialOutputWord[i] = READ_BE_UINT32(src);
					src += 4;
				}
			}

			totalBitOffset = 0;
			// The channels are encoded separately.
			for (int chan = 0; chan < channels; chan++) {
				// Read initial state (this makes it possible for the data stream
				// to be split & spread across multiple data chunks.
				curTablePos = initialTablePos[chan];
				//imcTableEntry = initialimcTableEntry[chan];
				outputWord = initialOutputWord[chan];

				// We need to interleave the channels in the output; we achieve
				// that by using a variables dest offset:
				destPos = chan * 2;

				const int bound = (channels == 1)
									? outputSamplesLeft
									: ((chan == 0)
										? (outputSamplesLeft+1) / 2
										: outputSamplesLeft / 2);
				for (i = 0; i < bound; ++i) {
					// Determine the size (in bits) of the next data packet
					const int32 curTableEntryBitCount = _imcTableEntryBitCount[curTablePos];
					assert(2 <= curTableEntryBitCount && curTableEntryBitCount <= 7);

					// Read the next data packet
					const byte *readPos = src + (totalBitOffset >> 3);
					const uint16 readWord = (uint16)(READ_BE_UINT16(readPos) << (totalBitOffset & 7));
					const byte packet = (byte)(readWord >> (16 - curTableEntryBitCount));

					// Advance read position to the next data packet
					totalBitOffset += curTableEntryBitCount;

					// Decode the data packet into a delta value for the output signal.
					const byte signBitMask = (1 << (curTableEntryBitCount - 1));
					const byte dataBitMask = (signBitMask - 1);
					const byte data = (packet & dataBitMask);

					int32 delta = imcTable[curTablePos] * (2 * data + 1) >> (curTableEntryBitCount - 1);

					// The topmost bit in the data packet tells is a sign bit
					if ((packet & signBitMask) != 0) {
						delta = -delta;
					}

					// Accumulate the delta onto the output data
					outputWord += delta;

					// Clip outputWord to 16 bit signed, and write it into the destination stream
					if (outputWord > 0x7fff)
						outputWord = 0x7fff;
					if (outputWord < -0x8000)
						outputWord = -0x8000;
					WRITE_BE_UINT16(dst + destPos, outputWord);
					destPos += channels << 1;

					// Adjust the curTablePos
					curTablePos += (int8)imxOtherTable[curTableEntryBitCount - 2][data];
					if (curTablePos < 0)
						curTablePos = 0;
					else if (curTablePos >= ARRAYSIZE(imcTable))
						curTablePos = ARRAYSIZE(imcTable) - 1;
				}
			}
		}
		break;

	default:
		print("decompressCodec() Unknown codec %d!", (int)codec);
		output_size = 0;
		break;
	}

	return output_size;
}

void CompressScummBun::encodeWaveWithFlac(char *filename) {
	char fbuf[2048];
	char fbuf2[2048];
	sprintf(fbuf, "%s.wav", filename);
	sprintf(fbuf2, "%s.fla", filename);
	encodeAudio(fbuf, false, -1, fbuf2, AUDIO_FLAC);
}

void CompressScummBun::encodeWaveWithOgg(char *filename) {
	char fbuf[2048];
	char fbuf2[2048];
	sprintf(fbuf, "%s.wav", filename);
	sprintf(fbuf2, "%s.ogg", filename);
	encodeAudio(fbuf, false, -1, fbuf2, AUDIO_VORBIS);
}

void CompressScummBun::encodeWaveWithLame(char *filename) {
	char fbuf[2048];
	char fbuf2[2048];

	sprintf(fbuf, "%s.wav", filename);
	sprintf(fbuf2, "%s.mp3", filename);
	encodeAudio(fbuf, false, -1, fbuf2, AUDIO_MP3);
}

void CompressScummBun::writeWaveHeader(int s_size, int rate, int chan) {
	int bits = 16;
	byte wav[44];
	memset(wav, 0, 44);
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

	_waveTmpFile.seek(0, SEEK_SET);
	if (_waveTmpFile.write(wav, 44) != 44) {
		error("error writing temp wave file");
	}
	_waveTmpFile.close();
}

void CompressScummBun::writeToTempWave(char *fileName, byte *output_data, unsigned int size) {
	if (!_waveTmpFile.isOpen()) {
		_waveTmpFile.open(fileName, "wb");
		byte wav[44];
		memset(wav, 0, 44);
		_waveTmpFile.write(output_data, 44);
		_waveDataSize = 0;
	}
	for (unsigned int j = 0; j < size - 1; j += 2) {
		byte tmp = output_data[j + 0];
		output_data[j + 0] = output_data[j + 1];
		output_data[j + 1] = tmp;
	}
	_waveTmpFile.write(output_data, size);
	_waveDataSize += size;
}

static AudioFormat gCompMode = AUDIO_MP3;

typedef struct { int offset, size, codec; } CompTable;

byte *CompressScummBun::decompressBundleSound(int index, File  &input, int32 &finalSize) {
	byte compOutput[0x2000];
	int i;

	input.seek(_bundleTable[index].offset, SEEK_SET);

	uint32 tag = input.readUint32BE();
	assert(tag == 'COMP');
	int numCompItems = input.readUint32BE();
	input.seek(8, SEEK_CUR);

	CompTable *compTable = (CompTable *)malloc(sizeof(CompTable) * numCompItems);
	int32 maxSize = 0;
	for (i = 0; i < numCompItems; i++) {
		compTable[i].offset = input.readUint32BE();
		compTable[i].size = input.readUint32BE();
		compTable[i].codec = input.readUint32BE();
		input.seek(4, SEEK_CUR);
		if (compTable[i].size > maxSize)
			maxSize = compTable[i].size;
	}
	// CMI hack: one more byte at the end of input buffer
	byte *compInput = (byte *)malloc(maxSize + 1);
	byte *compFinal = (byte *)malloc(numCompItems * 0x2000);

	finalSize = 0;

	for (i = 0; i < numCompItems; i++) {
		compInput[compTable[i].size] = 0;
		input.seek(_bundleTable[index].offset + compTable[i].offset, SEEK_SET);
		input.read_throwsOnError(compInput, compTable[i].size);
		int outputSize = decompressCodec(compTable[i].codec, compInput, compOutput, compTable[i].size);
		assert(outputSize <= 0x2000);
		memcpy(compFinal + finalSize, compOutput, outputSize);
		finalSize += outputSize;
	}

	free(compInput);
	free(compTable);

	return compFinal;
}

byte *CompressScummBun::convertTo16bit(byte *ptr, int inputSize, int &outputSize, int bits, int freq, int channels) {
	outputSize = inputSize;
	if (bits == 8)
		outputSize *= 2;
	if (bits == 12)
		outputSize = (outputSize / 3) * 4;

	byte *outputBuf = (byte *)malloc(outputSize);
	if (bits == 8) {
		byte *buf = outputBuf;
		byte *src = ptr;
		for (int i = 0; i < inputSize; i++) {
			uint16 val = (*src++ - 0x80) << 8;
			*buf++ = (byte)(val >> 8);
			*buf++ = (byte)val;
		}
	}
	if (bits == 12) {
		int loop_size = inputSize / 3;
		byte *decoded = outputBuf;
		byte *source = ptr;
		uint32 value;

		while (loop_size--) {
			byte v1 = *source++;
			byte v2 = *source++;
			byte v3 = *source++;
			value = ((((v2 & 0x0f) << 8) | v1) << 4) - 0x8000;
			*decoded++ = (byte)((value >> 8) & 0xff);
			*decoded++ = (byte)(value & 0xff);
			value = ((((v2 & 0xf0) << 4) | v3) << 4) - 0x8000;
			*decoded++ = (byte)((value >> 8) & 0xff);
			*decoded++ = (byte)(value & 0xff);
		}
	}
	if (bits == 16) {
		int loop_size = inputSize / 2;
		byte *buf = outputBuf;
		byte *src = ptr;
		while (loop_size--) {
			*buf++ = *src++;
			*buf++ = *src++;
		}
	}

	return outputBuf;
}

void CompressScummBun::countMapElements(byte *ptr, int &numRegions, int &numJumps, int &numSyncs, int &numMarkers) {
	uint32 tag;
	int32 size = 0;

	do {
		tag = READ_BE_UINT32(ptr); ptr += 4;
		switch(tag) {
		case 'TEXT':
			if (!scumm_stricmp((const char *)(ptr + 8), "exit"))
				numMarkers++;
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'STOP':
		case 'FRMT':
		case 'DATA':
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'REGN':
			numRegions++;
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'JUMP':
			numJumps++;
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'SYNC':
			numSyncs++;
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		default:
			error("countMapElements() Unknown tag of Map");
		}
	} while (tag != 'DATA');
}

struct Region {
	int32 offset;
	int32 length;
};

struct Jump {
	int32 offset;
	int32 dest;
	byte hookId;
	int16 fadeDelay;
};

struct Sync {
	int32 size;
	byte *ptr;
};

struct Marker {
	int32 pos;
	int32 length;
	char *ptr;
};

static Region *_region;
static int _numRegions;

void CompressScummBun::writeRegions(byte *ptr, int bits, int freq, int channels, const char *dir, char *filename, File &output) {
	char tmpPath[200];

	for (int l = 0; l < _numRegions; l++) {
		int outputSize = 0;
		int size = _region[l].length;
		int offset = _region[l].offset;
		byte *outputData = convertTo16bit(ptr + offset, size, outputSize, bits, freq, channels);
		sprintf(tmpPath, "%s/%s_reg%03d.wav", dir, filename, l);
		writeToTempWave(tmpPath, outputData, outputSize);
		writeWaveHeader(_waveDataSize, freq, channels);
		free(outputData);
		sprintf(tmpPath, "%s/%s_reg%03d", dir, filename, l);

		switch (gCompMode) {
		case AUDIO_MP3:
			encodeWaveWithLame(tmpPath);
			break;
		case AUDIO_VORBIS:
			encodeWaveWithOgg(tmpPath);
			break;
		case AUDIO_FLAC:
			encodeWaveWithFlac(tmpPath);
			break;
		default:
			error("Unknown encoding method");
		}

		sprintf(tmpPath, "%s/%s_reg%03d.wav", dir, filename, l);
		unlink(tmpPath);

		int32 startPos = output.pos();
		switch (gCompMode) {
		case AUDIO_MP3:
			sprintf(_cbundleTable[_cbundleCurIndex].filename, "%s_reg%03d.mp3", filename, l);
			sprintf(tmpPath, "%s/%s_reg%03d.mp3", dir, filename, l);
			break;
		case AUDIO_VORBIS:
			sprintf(_cbundleTable[_cbundleCurIndex].filename, "%s_reg%03d.ogg", filename, l);
			sprintf(tmpPath, "%s/%s_reg%03d.ogg", dir, filename, l);
			break;
		case AUDIO_FLAC:
			sprintf(_cbundleTable[_cbundleCurIndex].filename, "%s_reg%03d.fla", filename, l);
			sprintf(tmpPath, "%s/%s_reg%03d.fla", dir, filename, l);
			break;
		default:
			error("Unknown encoding method");
		}
		_cbundleTable[_cbundleCurIndex].offset = startPos;

		File cmpFile(tmpPath, "rb");
		cmpFile.seek(0, SEEK_END);
		size = cmpFile.pos();
		cmpFile.seek(0, SEEK_SET);
		byte *tmpBuf = (byte *)malloc(size);
		cmpFile.read_throwsOnError(tmpBuf, size);
		cmpFile.close();
		unlink(tmpPath);

		output.write(tmpBuf, size);
		free(tmpBuf);
		_cbundleTable[_cbundleCurIndex].size = output.pos() - startPos;
		_cbundleCurIndex++;
	}
	free(_region);
}

void CompressScummBun::recalcRegions(int32 &value, int bits, int freq, int channels) {
	int size = value;
	if (bits == 8)
		size *= 2;
	if (bits == 12)
		size = (size / 3) * 4;
	value = size;
}

void CompressScummBun::writeToRMAPFile(byte *ptr, File &output, char *filename, int &offsetData, int &bits, int &freq, int &channels) {
	byte *s_ptr = ptr;
	int32 size = 0;
	int l;

	uint32 tag = READ_BE_UINT32(ptr);
	assert(tag == 'iMUS');
	ptr += 16;

	int curIndexRegion = 0;
	int curIndexJump = 0;
	int curIndexSync = 0;
	int curIndexMarker = 0;

	int numRegions = 0, numJumps = 0, numSyncs = 0, numMarkers = 0;
	countMapElements(ptr, numRegions, numJumps, numSyncs, numMarkers);
	Region *region = (Region *)malloc(sizeof(Region) * numRegions);
	_region = (Region *)malloc(sizeof(Region) * numRegions);
	_numRegions = numRegions;
	Jump *jump = (Jump *)malloc(sizeof(Jump) * numJumps);
	Sync *sync = (Sync *)malloc(sizeof(Sync) * numSyncs);
	Marker *marker = (Marker *)malloc(sizeof(Marker) * numMarkers);

	do {
		tag = READ_BE_UINT32(ptr); ptr += 4;
		switch (tag) {
		case 'FRMT':
			ptr += 12;
			bits = READ_BE_UINT32(ptr); ptr += 4;
			freq = READ_BE_UINT32(ptr); ptr += 4;
			channels = READ_BE_UINT32(ptr); ptr += 4;
			break;
		case 'TEXT':
			if (!scumm_stricmp((const char *)(ptr + 8), "exit")) {
				marker[curIndexMarker].pos = READ_BE_UINT32(ptr + 4);
				marker[curIndexMarker].length = strlen((const char *)(ptr + 8)) + 1;
				marker[curIndexMarker].ptr = new char[marker[curIndexMarker].length];
				strcpy(marker[curIndexMarker].ptr, (const char *)(ptr + 8));
				curIndexMarker++;
			}
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'STOP':
			size = READ_BE_UINT32(ptr); ptr += size + 4;
			break;
		case 'REGN':
			ptr += 4;
			region[curIndexRegion].offset = READ_BE_UINT32(ptr); ptr += 4;
			region[curIndexRegion].length = READ_BE_UINT32(ptr); ptr += 4;
			curIndexRegion++;
			break;
		case 'JUMP':
			ptr += 4;
			jump[curIndexJump].offset = READ_BE_UINT32(ptr); ptr += 4;
			jump[curIndexJump].dest = READ_BE_UINT32(ptr); ptr += 4;
			jump[curIndexJump].hookId = READ_BE_UINT32(ptr); ptr += 4;
			jump[curIndexJump].fadeDelay = READ_BE_UINT32(ptr); ptr += 4;
			curIndexJump++;
			break;
		case 'SYNC':
			size = READ_BE_UINT32(ptr); ptr += 4;
			sync[curIndexSync].size = size;
			sync[curIndexSync].ptr = (byte *)malloc(size);
			memcpy(sync[curIndexSync].ptr, ptr, size);
			curIndexSync++;
			ptr += size;
			break;
		case 'DATA':
			ptr += 4;
			break;
		default:
			error("writeToRMAPFile() Unknown tag of Map for sound '%s'", filename);
		}
	} while (tag != 'DATA');
	offsetData = (int32)(ptr - s_ptr);

	int32 startPos = output.pos();
	sprintf(_cbundleTable[_cbundleCurIndex].filename, "%s.map", filename);
	_cbundleTable[_cbundleCurIndex].offset = startPos;

	output.writeUint32BE('RMAP');
	output.writeUint32BE(3); // version
	output.writeUint32BE(16); // bits
	output.writeUint32BE(freq);
	output.writeUint32BE(channels);
	output.writeUint32BE(numRegions);
	output.writeUint32BE(numJumps);
	output.writeUint32BE(numSyncs);
	output.writeUint32BE(numMarkers);
	memcpy(_region, region, sizeof(Region) * numRegions);
	for (l = 0; l < numRegions; l++) {
		_region[l].offset -= offsetData;
		region[l].offset -= offsetData;
		recalcRegions(region[l].offset, bits, freq, channels);
		recalcRegions(region[l].length, bits, freq, channels);
		output.writeUint32BE(region[l].offset);
		output.writeUint32BE(region[l].length);
	}
	for (l = 0; l < numJumps; l++) {
		jump[l].offset -= offsetData;
		jump[l].dest -= offsetData;
		recalcRegions(jump[l].offset, bits, freq, channels);
		recalcRegions(jump[l].dest, bits, freq, channels);
		output.writeUint32BE(jump[l].offset);
		output.writeUint32BE(jump[l].dest);
		output.writeUint32BE(jump[l].hookId);
		output.writeUint32BE(jump[l].fadeDelay);
	}
	for (l = 0; l < numSyncs; l++) {
		output.writeUint32BE(sync[l].size);
		output.write(sync[l].ptr, sync[l].size);
		free(sync[l].ptr);
	}
	for (l = 0; l < numMarkers; l++) {
		output.writeUint32BE(marker[l].pos);
		output.writeUint32BE(marker[l].length);
		output.write(marker[l].ptr, marker[l].length);
		delete[] marker[l].ptr;
	}
	free(region);
	free(jump);
	free(sync);
	free(marker);

	_cbundleTable[_cbundleCurIndex].size = output.pos() - startPos;
	_cbundleCurIndex++;
}

CompressScummBun::CompressScummBun(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_cbundleCurIndex = 0;

	_supportsProgressBar = true;
	
	ToolInput input;
	input.format = "*.bun";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress the .bun data files from The Curse of Monkey Island games.";
	_helptext = "\nUsage: " + getName() + " [mode] [mode-params] [-o outputfile = inputfile.bun] <inputfile>\n";
}

void CompressScummBun::execute() {
	Filename inpath(_inputPaths[0].path);
	Filename &outpath = _outputPath;

	uint32 tag;
	int32 numFiles, offset;

	File input(inpath, "rb");

	if (outpath.empty()) {
		// Change extension for output
		outpath = inpath;
		outpath.setExtension(".bun");
	}

	File output(outpath, "wb");

	output.writeUint32BE('LB23');
	output.writeUint32BE(0); // will be later
	output.writeUint32BE(0); // will be later

	initializeImcTables();

	tag = input.readUint32BE();
	assert(tag == 'LB83');
	offset = input.readUint32BE();
	numFiles = input.readUint32BE();

	_bundleTable = (BundleAudioTable *)malloc(numFiles * sizeof(BundleAudioTable));
	input.seek(offset, SEEK_SET);

	for (int i = 0; i < numFiles; i++) {
		char filename[13], c;
		int z = 0;
		int z2;

		for (z2 = 0; z2 < 8; z2++)
			if ((c = input.readByte()) != 0)
				filename[z++] = c;
		filename[z++] = '.';
		for (z2 = 0; z2 < 4; z2++)
			if ((c = input.readByte()) != 0)
				filename[z++] = c;
		filename[z] = '\0';
		strcpy(_bundleTable[i].filename, filename);
		_bundleTable[i].offset = input.readUint32BE();
		_bundleTable[i].size = input.readUint32BE();
	}

	for (int i = 0; i < numFiles; i++) {
		if (strcmp(_bundleTable[i].filename, "PRELOAD.") == 0)
			continue;

		updateProgress(i, numFiles);

		int offsetData = 0, bits = 0, freq = 0, channels = 0;
		int32 size = 0;
		byte *compFinal = decompressBundleSound(i, input, size);
		writeToRMAPFile(compFinal, output, _bundleTable[i].filename, offsetData, bits, freq, channels);
		writeRegions(compFinal + offsetData, bits, freq, channels, outpath.getPath().c_str(), _bundleTable[i].filename, output);
		free(compFinal);
	}

	int32 curPos = output.pos();
	for (int i = 0; i < _cbundleCurIndex; i++) {
		output.write(_cbundleTable[i].filename, 24);
		output.writeUint32BE(_cbundleTable[i].offset);
		output.writeUint32BE(_cbundleTable[i].size);
	}

	output.seek(4, SEEK_SET);
	output.writeUint32BE(curPos);
	output.writeUint32BE(_cbundleCurIndex);

	free(_bundleTable);

	print("compression done.\n");
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressScummBun scummbun(argv[0]);
	return scummbun.run(argc, argv);
}
#endif

