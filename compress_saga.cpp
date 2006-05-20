/* compress_saga - Compress SAGA engine digital sound files into
 * MP3 and Ogg Vorbis format
 * Copyright (C) 2004, Marcoen Hirschberg
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

#include <stdio.h>
#include "compress.h"

namespace Common {

// <!-- taken from common\util.h

/**
 * List of game language.
 */
enum Language {
	EN_ANY,     // Generic English (when only one game version exist)
	EN_USA,
	EN_GRB,

	DE_DEU,
	FR_FRA,
	IT_ITA,
	PT_BRA,
	ES_ESP,
	JA_JPN,
	ZH_TWN,
	KO_KOR,
	SE_SWE,
	HB_ISR,
	RU_RUS,
	CZ_CZE,
	NL_NLD,
	NB_NOR,
	PL_POL,

	UNK_LANG = -1	// Use default language (i.e. none specified)
};

/**
 * List of game platforms. Specifying a platform for a target can be used to
 * give the game engines a hint for which platform the game data file are.
 * This may be optional or required, depending on the game engine and the
 * game in question.
 */
enum Platform {
	kPlatformPC,
	kPlatformAmiga,
	kPlatformAtariST,
	kPlatformMacintosh,
	kPlatformFMTowns,
	kPlatformWindows,
	kPlatformNES,
	kPlatformC64,
	kPlatformLinux,
	kPlatformAcorn,
	kPlatformSegaCD,
	kPlatform3DO,
//	kPlatformPCEngine,

	kPlatformUnknown = -1
};
//  taken from common\util.h -->

// <!-- taken from common/md5.c

typedef struct {
	uint32 total[2];
	uint32 state[4];
	uint8 buffer[64];
} md5_context;

#define GET_UINT32(n, b, i)	(n) = READ_LE_UINT32(b + i)
#define PUT_UINT32(n, b, i)	WRITE_LE_UINT32(b + i, n)

void md5_starts(md5_context *ctx) {
	ctx->total[0] = 0;
	ctx->total[1] = 0;

	ctx->state[0] = 0x67452301;
	ctx->state[1] = 0xEFCDAB89;
	ctx->state[2] = 0x98BADCFE;
	ctx->state[3] = 0x10325476;
}

static void md5_process(md5_context *ctx, const uint8 data[64]) {
	uint32 X[16], A, B, C, D;

	GET_UINT32(X[0],  data,  0);
	GET_UINT32(X[1],  data,  4);
	GET_UINT32(X[2],  data,  8);
	GET_UINT32(X[3],  data, 12);
	GET_UINT32(X[4],  data, 16);
	GET_UINT32(X[5],  data, 20);
	GET_UINT32(X[6],  data, 24);
	GET_UINT32(X[7],  data, 28);
	GET_UINT32(X[8],  data, 32);
	GET_UINT32(X[9],  data, 36);
	GET_UINT32(X[10], data, 40);
	GET_UINT32(X[11], data, 44);
	GET_UINT32(X[12], data, 48);
	GET_UINT32(X[13], data, 52);
	GET_UINT32(X[14], data, 56);
	GET_UINT32(X[15], data, 60);

#define S(x, n) ((x << n) | ((x & 0xFFFFFFFF) >> (32 - n)))

#define P(a, b, c, d, k, s, t)                    \
{                                                 \
	a += F(b,c,d) + X[k] + t; a = S(a,s) + b; \
}

	A = ctx->state[0];
	B = ctx->state[1];
	C = ctx->state[2];
	D = ctx->state[3];

#define F(x, y, z) (z ^ (x & (y ^ z)))

	P(A, B, C, D,  0,  7, 0xD76AA478);
	P(D, A, B, C,  1, 12, 0xE8C7B756);
	P(C, D, A, B,  2, 17, 0x242070DB);
	P(B, C, D, A,  3, 22, 0xC1BDCEEE);
	P(A, B, C, D,  4,  7, 0xF57C0FAF);
	P(D, A, B, C,  5, 12, 0x4787C62A);
	P(C, D, A, B,  6, 17, 0xA8304613);
	P(B, C, D, A,  7, 22, 0xFD469501);
	P(A, B, C, D,  8,  7, 0x698098D8);
	P(D, A, B, C,  9, 12, 0x8B44F7AF);
	P(C, D, A, B, 10, 17, 0xFFFF5BB1);
	P(B, C, D, A, 11, 22, 0x895CD7BE);
	P(A, B, C, D, 12,  7, 0x6B901122);
	P(D, A, B, C, 13, 12, 0xFD987193);
	P(C, D, A, B, 14, 17, 0xA679438E);
	P(B, C, D, A, 15, 22, 0x49B40821);

#undef F

#define F(x, y, z) (y ^ (z & (x ^ y)))

	P(A, B, C, D,  1,  5, 0xF61E2562);
	P(D, A, B, C,  6,  9, 0xC040B340);
	P(C, D, A, B, 11, 14, 0x265E5A51);
	P(B, C, D, A,  0, 20, 0xE9B6C7AA);
	P(A, B, C, D,  5,  5, 0xD62F105D);
	P(D, A, B, C, 10,  9, 0x02441453);
	P(C, D, A, B, 15, 14, 0xD8A1E681);
	P(B, C, D, A,  4, 20, 0xE7D3FBC8);
	P(A, B, C, D,  9,  5, 0x21E1CDE6);
	P(D, A, B, C, 14,  9, 0xC33707D6);
	P(C, D, A, B,  3, 14, 0xF4D50D87);
	P(B, C, D, A,  8, 20, 0x455A14ED);
	P(A, B, C, D, 13,  5, 0xA9E3E905);
	P(D, A, B, C,  2,  9, 0xFCEFA3F8);
	P(C, D, A, B,  7, 14, 0x676F02D9);
	P(B, C, D, A, 12, 20, 0x8D2A4C8A);

#undef F

#define F(x, y, z) (x ^ y ^ z)

	P(A, B, C, D,  5,  4, 0xFFFA3942);
	P(D, A, B, C,  8, 11, 0x8771F681);
	P(C, D, A, B, 11, 16, 0x6D9D6122);
	P(B, C, D, A, 14, 23, 0xFDE5380C);
	P(A, B, C, D,  1,  4, 0xA4BEEA44);
	P(D, A, B, C,  4, 11, 0x4BDECFA9);
	P(C, D, A, B,  7, 16, 0xF6BB4B60);
	P(B, C, D, A, 10, 23, 0xBEBFBC70);
	P(A, B, C, D, 13,  4, 0x289B7EC6);
	P(D, A, B, C,  0, 11, 0xEAA127FA);
	P(C, D, A, B,  3, 16, 0xD4EF3085);
	P(B, C, D, A,  6, 23, 0x04881D05);
	P(A, B, C, D,  9,  4, 0xD9D4D039);
	P(D, A, B, C, 12, 11, 0xE6DB99E5);
	P(C, D, A, B, 15, 16, 0x1FA27CF8);
	P(B, C, D, A,  2, 23, 0xC4AC5665);

#undef F

#define F(x, y, z) (y ^ (x | ~z))

	P(A, B, C, D,  0,  6, 0xF4292244);
	P(D, A, B, C,  7, 10, 0x432AFF97);
	P(C, D, A, B, 14, 15, 0xAB9423A7);
	P(B, C, D, A,  5, 21, 0xFC93A039);
	P(A, B, C, D, 12,  6, 0x655B59C3);
	P(D, A, B, C,  3, 10, 0x8F0CCC92);
	P(C, D, A, B, 10, 15, 0xFFEFF47D);
	P(B, C, D, A,  1, 21, 0x85845DD1);
	P(A, B, C, D,  8,  6, 0x6FA87E4F);
	P(D, A, B, C, 15, 10, 0xFE2CE6E0);
	P(C, D, A, B,  6, 15, 0xA3014314);
	P(B, C, D, A, 13, 21, 0x4E0811A1);
	P(A, B, C, D,  4,  6, 0xF7537E82);
	P(D, A, B, C, 11, 10, 0xBD3AF235);
	P(C, D, A, B,  2, 15, 0x2AD7D2BB);
	P(B, C, D, A,  9, 21, 0xEB86D391);

#undef F

	ctx->state[0] += A;
	ctx->state[1] += B;
	ctx->state[2] += C;
	ctx->state[3] += D;
}

void md5_update(md5_context *ctx, const uint8 *input, uint32 length) {
	uint32 left, fill;

	if (!length)
		return;

	left = ctx->total[0] & 0x3F;
	fill = 64 - left;

	ctx->total[0] += length;
	ctx->total[0] &= 0xFFFFFFFF;

	if (ctx->total[0] < length)
		ctx->total[1]++;

	if (left && length >= fill) {
		memcpy((void *)(ctx->buffer + left), (const void *)input, fill);
		md5_process(ctx, ctx->buffer);
		length -= fill;
		input  += fill;
		left = 0;
	}

	while (length >= 64) {
		md5_process(ctx, input);
		length -= 64;
		input  += 64;
	}

	if (length) {
		memcpy((void *)(ctx->buffer + left), (const void *)input, length);
	}
}

static const uint8 md5_padding[64] = {
	0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

void md5_finish(md5_context *ctx, uint8 digest[16]) {
	uint32 last, padn;
	uint32 high, low;
	uint8 msglen[8];

	high = (ctx->total[0] >> 29) | (ctx->total[1] << 3);
	low  = (ctx->total[0] <<  3);

	PUT_UINT32(low,  msglen, 0);
	PUT_UINT32(high, msglen, 4);

	last = ctx->total[0] & 0x3F;
	padn = (last < 56) ? (56 - last) : (120 - last);

	md5_update(ctx, md5_padding, padn);
	md5_update(ctx, msglen, 8);

	PUT_UINT32(ctx->state[0], digest,  0);
	PUT_UINT32(ctx->state[1], digest,  4);
	PUT_UINT32(ctx->state[2], digest,  8);
	PUT_UINT32(ctx->state[3], digest, 12);
}

bool md5_file(const char *name, uint8 digest[16], uint32 length) {
	FILE *f;

	f = fopen(name, "rb");
	if (f == NULL) {
		printf("md5_file couldn't open '%s'", name);
		return false;
	}

	md5_context ctx;
	uint32 i;
	unsigned char buf[1000];
	bool restricted = (length != 0);
	int readlen;

	if (!restricted || sizeof(buf) <= length)
		readlen = sizeof(buf);
	else
		readlen = length;

	md5_starts(&ctx);

	
	while ((i = (uint32)fread(buf, 1, readlen, f)) > 0) {
		md5_update(&ctx, buf, i);

		length -= i;
		if (restricted && length == 0)
			break;

		if (restricted && sizeof(buf) > length)
			readlen = length;
	}

	md5_finish(&ctx, digest);
	fclose(f);
	return true;
}

}
// taken from common/md5.c -->

#include "SagaGame.h"
#include "SagaResNames.h"
#include "SagaGame.cpp"

typedef struct  {
	uint32 offset;
	uint32 size;
} Record;

static CompressMode gCompMode = kMP3Mode;

GameDescription *currentGameDescription = NULL;
int currentFileIndex = -1;

bool detectFile(const char *inFileName) {
	int gamesCount = ARRAYSIZE(gameDescriptions);
	int j,i;
	uint8 md5sum[16];
	char md5str[32+1];

	Common::md5_file(inFileName, md5sum, FILE_MD5_BYTES);
	printf("Input file name: %s\n", inFileName);
	for (j = 0; j < 16; j++) {
		sprintf(md5str + j*2, "%02x", (int)md5sum[j]);
	}
	printf("md5: %s\n", md5str);

	for (i = 0; i < gamesCount; i++) {
		for (j = 0; j < gameDescriptions[i].filesCount; j++) {
			if (strcmp(gameDescriptions[i].filesDescriptions[j].md5, md5str) == 0) {
				if ((gameDescriptions[i].filesDescriptions[j].fileType & (GAME_VOICEFILE | GAME_SOUNDFILE | GAME_MUSICFILE)) != 0)
				{
					currentGameDescription = &gameDescriptions[i];
					currentFileIndex = j;
					printf("Matched game: %s %s\n", currentGameDescription->name, currentGameDescription->extra);
					return true;
				}
			}
		}
	}
	printf("unsupported file\n");
	return false;
}

uint32 copyFile(const char *fromFileName, FILE* outputFile) {
	uint32 size;
	char fbuf[2048];
	FILE * tempf;
	
	tempf = fopen(fromFileName, "rb");
	if (tempf == NULL)
		error("unable to open %s\n", fromFileName);

	while ((size = (uint32)fread(fbuf, 1, sizeof(fbuf), tempf)) > 0) {
		fwrite(fbuf, 1, size, outputFile);
	}
	size = ftell(tempf);
	fclose(tempf);
	return size;
}

void copyFile(FILE* inputFile, uint32 inputSize, const char* toFileName) {
	uint32 size;
	char fbuf[2048];
	FILE * tempf;
	
	tempf = fopen(toFileName, "wb");
	if (tempf == NULL)
		error("unable to open %s\n", toFileName);
	while (inputSize > 0) {
		size = (uint32)fread(fbuf, 1, inputSize > sizeof(fbuf) ? sizeof(fbuf) : inputSize, inputFile);
		if (size == 0) {
			error("unable to copy file");
		}
		fwrite(fbuf, 1, size, tempf);
		inputSize -= size;
	}
	fclose(tempf);
}

uint32 encodeEntry(GameSoundInfo *soundInfo, FILE* inputFile, uint32 inputSize, FILE* outputFile) {

	if (soundInfo->resourceType == kSoundVOC) {
		fseek(inputFile, 26, SEEK_CUR); //skip header, assume it's OK
		extractAndEncodeVOC(TEMP_RAW, inputFile, gCompMode);
		return copyFile(tempEncoded, outputFile);
	}
	if (soundInfo->resourceType == kSoundPCM) {
		copyFile(inputFile, inputSize, TEMP_RAW);

		//BUG-BUG: bool(c++) -> bool(c) cause bug in al->eax conversion
		setRawAudioType( !soundInfo->isBigEndian, soundInfo->stereo, soundInfo->sampleBits);
		encodeAudio(TEMP_RAW, true, soundInfo->frequency, tempEncoded, gCompMode);
		return copyFile(tempEncoded, outputFile);
	}


	error("sorry - unsupported resourceType %ul\n", soundInfo->resourceType);
}

#define RSC_TABLEINFO_SIZE 8
#define RSC_TABLEENTRY_SIZE 8

void sagaEncode(const char *inputFileName) {
	FILE *inputFile;
	FILE *outputFile;
	uint32 inputFileSize;
	uint32 resTableOffset;
	uint32 resTableCount;
	uint32 i;

	Record *inputTable;
	Record *outputTable;
	GameFileDescription *currentFileDescription;
	GameSoundInfo *soundInfo;

	inputFile = fopen(inputFileName, "rb");
	inputFileSize = fileSize(inputFile);
	printf("filesize: %ul\n", inputFileSize);
	/*
	 * At the end of the resource file there are 2 values: one points to the
	 * beginning of the resource table the other gives the number of
	 * records in the table
	 */
	fseek(inputFile, inputFileSize - RSC_TABLEINFO_SIZE, SEEK_SET);

	resTableOffset = readUint32LE(inputFile);
	resTableCount = readUint32LE(inputFile);

	printf("table offset: %ul\nnumber of records: %ul\n", resTableOffset, resTableCount);
	if (resTableOffset != inputFileSize - RSC_TABLEINFO_SIZE - RSC_TABLEENTRY_SIZE * resTableCount) {
		error("Something's wrong with your resource file..\n");
	}

	// Go to beginning of the table 
	fseek(inputFile, resTableOffset, SEEK_SET);

	inputTable = (Record*)malloc(resTableCount * sizeof(Record));

	// Put offsets of all the records in a table 
	for (i = 0; i < resTableCount; i++) {

		inputTable[i].offset = readUint32LE(inputFile);
		inputTable[i].size = readUint32LE(inputFile);

		 printf("record: %ul, offset: %ul, size: %ul\n", i, inputTable[i].offset, inputTable[i].size);
	
		if ((inputTable[i].offset > inputFileSize) ||
		    (inputTable[i].offset + inputTable[i].size > inputFileSize)) {
			error("The offset points outside the file!");
		}

	}
	currentFileDescription = &currentGameDescription->filesDescriptions[currentFileIndex];
	outputTable = (Record*)malloc(resTableCount * sizeof(Record));
	
	outputFile = fopen("out.res", "wb");

	for (i = 0; i < resTableCount; i++) {
		fseek(inputFile, inputTable[i].offset, SEEK_SET);
		outputTable[i].offset = ftell(outputFile);
		
		if ((currentFileDescription->fileType & GAME_VOICEFILE) != 0) {
			soundInfo = currentGameDescription->voiceInfo;
		} else {
			if ((currentFileDescription->fileType & GAME_SOUNDFILE) != 0) {
				soundInfo = currentGameDescription->sfxInfo;
			} else {
				if ((currentFileDescription->fileType & GAME_MUSICFILE) != 0) {
					soundInfo = currentGameDescription->musicInfo;
				}
			}
		}

		outputTable[i].size = encodeEntry(soundInfo, inputFile, inputTable[i].size, outputFile);
	}
	fclose(inputFile);

	resTableOffset = ftell(outputFile);
	for (i = 0; i < resTableCount; i++) {
		writeUint32LE(outputFile, outputTable[i].offset);
		writeUint32LE(outputFile, outputTable[i].size);
	}
	writeUint32LE(outputFile, resTableOffset);
	writeUint32LE(outputFile, resTableCount);	// Should be the same number of entries 

	fclose(outputFile);

	free(inputTable);
	free(outputTable);
	

	printf("Done!\n");
}

void showhelp(char *exename) {
	printf("\nUsage: %s <params> [<file> | mac]\n", exename);

	printf("\nParams:\n");

	printf("--mp3        encode to MP3 format (default)\n");
	printf("--vorbis     encode to Vorbis format\n");
	printf("--flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf("-b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf("-B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf("--vbr        LAME uses the VBR mode (default)\n");
	printf("--abr        LAME uses the ABR mode\n");
	printf("-V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf("-q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf("--silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf("-b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf("-m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf("-M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf("-q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf("--silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
	printf("[params]     optional Arguments passed to the Encoder\n");
	printf("             recommended is: --best -b 1152\n");

	printf("\n--help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	int		i;
	char *inputFileName;

	if (argc < 2)
		showhelp(argv[0]);

	/* compression mode */
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
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	i = argc - 1;
	inputFileName = argv[i];
	if (detectFile(inputFileName))
		sagaEncode(inputFileName);

	return (0);
}
