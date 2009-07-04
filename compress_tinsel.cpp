/* compress_tinsel - .smp compressor
 * Copyright (C) 2009 The ScummVM Team
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

// By Jimi (m [underline] kiewitz [AT] users.sourceforge.net)

#include "compress.h"
#include "util.h"

// data-format of index-file:
//  [pointer to data file DWORD] [pointer to data file DWORD] [pointer to data file DWORD]
//  we use index[0] to signal the engine what data format it's supposed to expect. It may be 'MP3 ', 'OGG ' or 'FLAC'

// data-format of sample-file:
//  [sample-length DWORD] [sample-data]
//  or
//  [subsamplecount DWORD] [sample-length DWORD] [sample-data]
//   where subsamplecount has upmost bit 31 set (that's how one differentiates sample-length and subsamplecount)
// It seems that data-format 1 is used by DiscWorld 1 and data-format 2 is used by DiscWorld 2. Also DiscWorld 1 uses
//  raw-data as samples and DiscWorld 2 uses ADPCM 6-bit encoded data as samples. We suppose that we will need to do ADPCM
//  decoding on all multi-sample data.

// We also copy over the first 5000 bytes of the .smp file, because otherwise we would trash ScummVM detection.

#define TEMP_IDX "compressed.idx"
#define TEMP_SMP "compressed.smp"
#define TEMP_RAW "tempfile.raw"
#define TEMP_ENC "tempfile.enc"

static FILE *input_idx, *input_smp, *output_idx, *output_smp;
static CompressMode gCompMode = kMP3Mode;
static char INPUT_IDX[256], INPUT_SMP[256];

/* Converts raw-data sample in input_smp of size SampleSize to requested dataformat and writes to output_smp */
void convertTinselRawSample (uint32 sampleSize) {
	uint32 copyLeft = 0;
	uint32 doneRead = 0;
	char buffer[2048];
    FILE *curFileHandle;

	printf("Assuming DW1 sample being 8-bit raw...\n");

	unlink(TEMP_RAW); unlink(TEMP_ENC);
	curFileHandle = fopen(TEMP_RAW, "wb");
	copyLeft = sampleSize;
	while (copyLeft > 0) {
		doneRead = fread(buffer, 1, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft, input_smp);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		fwrite(buffer, 1, doneRead, curFileHandle);
	}
	fclose(curFileHandle);

	// Encode this raw data...
	setRawAudioType(true, false, 8); // LE, mono, 8-bit (??)
	encodeAudio(TEMP_RAW, true, 22050, TEMP_ENC, gCompMode);

	// Append compressed data to output_smp
	curFileHandle = fopen(TEMP_ENC, "rb");
	fseek(curFileHandle, 0, SEEK_END);
	copyLeft = ftell(curFileHandle);
    fseek(curFileHandle, 0, SEEK_SET);
	// Write size of compressed data
	writeUint32LE(output_smp, copyLeft);
	// Write actual data
	while (copyLeft > 0) {
		doneRead = fread(buffer, 1, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft, curFileHandle);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		fwrite(buffer, 1, doneRead, output_smp);
	}
	fclose(curFileHandle);
}

static const double TinselFilterTable[4][2] = {
	{0, 0 },
	{0.9375, 0},
	{1.796875, -0.8125},
	{1.53125, -0.859375}
};

template<typename T> inline T CLIP (T v, T amin, T amax)
		{ if (v < amin) return amin; else if (v > amax) return amax; else return v; }

/* Converts ADPCM-data sample in input_smp of size SampleSize to requested dataformat and writes to output_smp */
/* Quick hack together from adpcm.cpp */
void convertTinselADPCMSample (uint32 sampleSize) {
	byte *inBuffer, *inPos;
	int16 *outBuffer, *outPos;
	double predictor = 0;
	double k0 = 0, k1 = 0;
	double d0 = 0, d1 = 0;
	uint32 blockAlign, blockPos;
	uint16 chunkData;
	int16 chunkWord;
	uint8 headerByte, filterVal, chunkPos;
	const double eVal = 1.032226562;
	uint32 decodeLeft = 0, decodedCount = 0;
	uint32 uncompressedSize;
	double sample;

	uint32 copyLeft = 0;
	uint32 doneRead = 0;
	char buffer[2048];
    FILE *curFileHandle;

	printf("Assuming DW2 sample using ADPCM 6-bit, decoding to 16-bit raw...\n");

	// Allocate buffer for the ADPCM-compressed sample
	inBuffer = (byte *)malloc(sampleSize);
	if (!inBuffer) {
		printf("malloc failed!\n");
		return;
	}

	// Allocate buffer for uncompressed sample data (3 bytes will be uncompressed to 8 bytes)
	uncompressedSize = (sampleSize/3)*4*2+16;
	outBuffer = (int16 *)malloc(uncompressedSize);
	if (!outBuffer) {
		printf("malloc failed!\n");
		return;
	}

	fread(inBuffer, 1, sampleSize, input_smp);

	// 1 channel, 22050 rate, block align 24,
	blockAlign = 24; // Fixed for Tinsel 6-bit
	blockPos = blockAlign; // To make sure first header is read

	inPos = inBuffer; outPos = outBuffer;
    decodeLeft = sampleSize;
	while (decodeLeft > 0) {
		if (blockPos == blockAlign) {
			// read Tinsel header
			headerByte = *inPos; inPos++; decodeLeft--;
			filterVal = (headerByte & 0xC0) >> 6;

			if ((headerByte & 0x20) != 0) {
				//Lower 6 bit are negative
				// Negate
				headerByte = ~(headerByte | 0xC0) + 1;
				predictor = 1 << headerByte;
			} else {
				// Lower 6 bit are positive
				// Truncate
				headerByte &= 0x1F;
				predictor = ((double) 1.0) / (1 << headerByte);
			}
			k0 = TinselFilterTable[filterVal][0];
			k1 = TinselFilterTable[filterVal][1];
			blockPos = 0;
			chunkPos = 0;
		}

		switch (chunkPos) {
		case 0:
			chunkData = *inPos; inPos++; decodeLeft--;
			chunkWord = (chunkData << 8) & 0xFC00;
			break;
		case 1:
			chunkData = (chunkData << 8) | *inPos; inPos++; decodeLeft--;
			blockPos++;
			chunkWord = (chunkData << 6) & 0xFC00;
			break;
		case 2:
			chunkData = (chunkData << 8) | *inPos; inPos++; decodeLeft--;
			blockPos++;
			chunkWord = (chunkData << 4) & 0xFC00;
			break;
		case 3:
			chunkData = chunkData << 8;
			blockPos++;
			chunkWord = (chunkData << 2) & 0xFC00;
			break;
		}
		sample = chunkWord;
		sample *= eVal * predictor;
		sample += (d0 * k0) + (d1 * k1);
		d1 = d0;
		d0 = sample;
        *outPos = (int16) CLIP<double>(sample, -32768.0, 32767.0); outPos++;
		decodedCount++;
		chunkPos = (chunkPos + 1) % 4;
	}

	unlink(TEMP_RAW); unlink(TEMP_ENC);
	curFileHandle = fopen(TEMP_RAW, "wb");
	fwrite(outBuffer, 1, decodedCount*2, curFileHandle);
	fclose(curFileHandle);

	free(inBuffer); free(outBuffer);

	// Encode this raw data...
	setRawAudioType(true, false, 16); // LE, mono, 16-bit
	encodeAudio(TEMP_RAW, true, 22050, TEMP_ENC, gCompMode);

	// Append compressed data to output_smp
	curFileHandle = fopen(TEMP_ENC, "rb");
	fseek(curFileHandle, 0, SEEK_END);
	copyLeft = ftell(curFileHandle);
    fseek(curFileHandle, 0, SEEK_SET);
	// Write size of compressed data
	writeUint32LE(output_smp, copyLeft);
	// Write actual data
	while (copyLeft > 0) {
		doneRead = fread(buffer, 1, copyLeft > sizeof(buffer) ? sizeof(buffer) : copyLeft, curFileHandle);
		if (doneRead <= 0)
			break;
		copyLeft -= (int)doneRead;
		fwrite(buffer, 1, doneRead, output_smp);
	}
	fclose(curFileHandle);
}

void showhelp(char *exename) {
	printf("\nUsage: %s [params] [file]\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
 	printf(" --fast       FLAC uses compression level 0\n");
 	printf(" --best       FLAC uses compression level 8\n");
 	printf(" -<value>     specifies the value (0 - 8) of compression (8=best)(default:%d)\n", flacCompressDef);
 	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
 	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	char inputPath[768];
	int i;
	uint32 indexNo = 0;
	uint32 indexCount = 0;
	uint32 indexOffset = 0;
	uint32 loopCount = 0;
	uint32 sampleSize = 0;
	uint32 sampleCount = 0;

	if (argc < 2) {
		showhelp(argv[0]);
	}

	/* Compression mode */
	gCompMode = kMP3Mode;
	i = 1;

	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	} else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	} else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	getPath(argv[argc - 1], inputPath);

	sprintf(INPUT_IDX, "%s.idx", argv[argc - 1]);
	sprintf(INPUT_SMP, "%s.smp", argv[argc - 1]);

	input_idx = fopen(INPUT_IDX, "rb");
	if (!input_idx) {
		printf("Cannot open file: %s\n", INPUT_IDX);
		exit(-1);
	}

	input_smp = fopen(INPUT_SMP, "rb");
	if (!input_smp) {
		printf("Cannot open file: %s\n", INPUT_SMP);
		exit(-1);
	}

	unlink(TEMP_IDX);
	output_idx = fopen(TEMP_IDX, "wb");
	if (!output_idx) {
		printf("Can't open file " TEMP_IDX " for write!\n" );
		exit(-1);
	}
	unlink(TEMP_SMP);
	output_smp = fopen(TEMP_SMP, "wb");
	if (!output_smp) {
		printf("Can't open file " TEMP_SMP " for write!\n");
		exit(-1);
	}

	fseek(input_idx, 0, SEEK_END);
	indexCount = ftell(input_idx) / sizeof(uint32);
    fseek(input_idx, 0, SEEK_SET);

	loopCount = indexCount;
	while (loopCount>0) {
		indexOffset = readUint32LE(input_idx);
		if (indexOffset) {
			if (indexNo==0) {
				printf("The sourcefiles are already compressed, aborting...\n");
				return 1;
			}
			// Got sample(s), so convert...
			printf("Converting sample %d of %d\n", indexNo, indexCount);

			// Seek to Sample in input-file and read SampleSize
			fseek(input_smp, indexOffset, SEEK_SET);
			sampleSize = readUint32LE(input_smp);

			// Write offset of new data to new index file
			writeUint32LE(output_idx, ftell(output_smp));

			if (sampleSize & 0x80000000) {
				// multiple samples in ADPCM format
				sampleCount = sampleSize & ~0x80000000;
				// Write sample count to new sample file
				writeUint32LE(output_smp, sampleSize);
				while (sampleCount>0) {
					sampleSize = readUint32LE(input_smp);
					convertTinselADPCMSample(sampleSize);
					sampleCount--;
				}
			} else {
				// just one sample in raw format
				convertTinselRawSample(sampleSize);
			}
		} else {
			if (indexNo==0) {
				// Write signature as index 0
				switch (gCompMode) {
				case kMP3Mode: writeUint32BE(output_idx, MKID_BE('MP3 ')); break;
				case kVorbisMode: writeUint32BE(output_idx, MKID_BE('OGG ')); break;
				case kFlacMode: writeUint32BE(output_idx, MKID_BE('FLAC')); break;
				}
			} else {
				writeUint32LE(output_idx, 0);
			}
		}
		loopCount--; indexNo++;
	}
	fclose(output_smp);
	fclose(output_idx);
	fclose(input_smp);
	fclose(input_idx);

	/* And some clean-up :-) */
	unlink(TEMP_RAW);
	unlink(TEMP_ENC);

	return 0;
}
