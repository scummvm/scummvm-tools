/* QueenRebuild - Rebuild QUEEN.1 file to contain Resource Table (and optionally compress sound & speech)
 * Copyright (C) 2003  The ScummVM Team
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#if !defined(_MSC_VER)
#include <unistd.h>
#endif

typedef unsigned char   uint8;
typedef unsigned short uint16;
typedef unsigned int   uint32;

static const uint32 QTBL = 'QTBL';

#define INPUT_TBL	"queen.tbl"
#define FINAL_OUT	"queen.1c"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_TBL	"tempfile.tbl"
#define TEMP_SB		"tempfile.sb"
#define TEMP_MP3	"tempfile.mp3"

#define EXTRA_TBL_HEADER 8
#define SB_HEADER_SIZE	110


enum {
	COMPRESSION_NONE = 0,
	COMPRESSION_MP3 = 1,
	COMPRESSION_OGG = 2	
};

enum {
	VER_ENG_FLOPPY   = 0,
	VER_ENG_TALKIE   = 1,
	VER_FRE_FLOPPY   = 2,
	VER_FRE_TALKIE   = 3,
	VER_GER_FLOPPY   = 4,
	VER_GER_TALKIE   = 5,
	VER_ITA_FLOPPY   = 6,
	VER_ITA_TALKIE   = 7,
	VER_DEMO_PCGAMES = 8,
	VER_DEMO         = 9
};

struct GameVersion {
        char versionString[6];
        uint8 isFloppy;
        uint8 isDemo;
        uint32 tableOffset;
};

struct {
	char filename[12];
	uint8 bundle;
	uint32 offset;
	uint32 size;
} entry;

struct {
	uint8	compression;
	uint16	entries;
} versionExtra;


const struct GameVersion gameVersions[] = {
        { "PEM10", 1, 0, 0x00000008 },
        { "CEM10", 0, 0, 0x0000584E },
        { "PFM10", 1, 0, 0x0002CD93 },
        { "CFM10", 0, 0, 0x00032585 },
        { "PGM10", 1, 0, 0x00059ACA },
        { "CGM10", 0, 0, 0x0005F2A7 },
        { "PIM10", 1, 0, 0x000866B1 },
        { "CIM10", 0, 0, 0x0008BEE2 },
        { "PE100", 1, 1,  0x000B343C },
        { "PE100", 1, 1,  0x000B40F5 }
};

const struct GameVersion *version;


void showhelp(char *exename)
{
	printf("\nUsage: %s [--mp3 <args>] queen.1\n", exename);
	printf("\nParams:\n");
	printf(" --mp3 <args>         encode to MP3 format\n"); 
	printf("                      (Optional: <args> are passed on to the encoder)\n");
	printf("\nExample: %s --mp3 -q 5 queen.1\n", exename);
	exit(2);
}

const struct GameVersion *detectGameVersion(uint32 size) {
	switch(size) {
		case 3724538:
			return &gameVersions[VER_DEMO_PCGAMES];
			break;
		case 3732177:
			return &gameVersions[VER_DEMO];
			break;
		case 22677657:
			return &gameVersions[VER_ENG_FLOPPY];
			break;
		case 190787021:
			return &gameVersions[VER_ENG_TALKIE];
			break;
		case 22157304: 
			return &gameVersions[VER_FRE_FLOPPY];
			break;
		case 186689095:
			return &gameVersions[VER_FRE_TALKIE];
			break;
		case 22240013: 
			return &gameVersions[VER_GER_FLOPPY];
			break;
		case 217648975: 
			return &gameVersions[VER_GER_TALKIE];
			break;
		case 22461366:
			return &gameVersions[VER_ITA_FLOPPY];
			break;
		case 190795582: 
			return &gameVersions[VER_ITA_TALKIE];
			break;
		default:
			printf("Unknown/unsupported version of FOTAQ!");
			exit(1);
	}
}

uint8 readByte(FILE *fp) {
	return fgetc(fp);
}

unsigned int readUint16BE(FILE *fp) {
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 1; i >= 0; i--) {
		c = fgetc(fp);
		ret |= c << i*8;
	}
	return ret;
}

unsigned int readUint32BE(FILE *fp) {
	int i;
	unsigned int ret = 0;
	unsigned int c;
	for (i = 3; i >= 0; i--) {
		c = fgetc(fp);
		ret |= c << i*8;
	}
	return ret;
}

void readString(uint32 size, char *dest, FILE *fp) {
	uint32 i = 0;
	while (i < size) {
		int c = fgetc(fp);
		dest[i++] = c;
	}
	dest[i] = '\0';
}

void writeByte(FILE *fp, uint8 b) {
	fwrite(&b, 1, 1, fp);
}

void writeUint16BE(FILE *fp, uint16 value) {
	writeByte(fp, (uint8)(value >> 8));
	writeByte(fp, (uint8)(value & 0xFF));
}

void writeUint32BE(FILE *fp, uint32 value) {
	writeUint16BE(fp, (uint16)(value >> 16));
	writeUint16BE(fp, (uint16)(value & 0xFFFF));
}

uint32 fileSize(FILE *fp) {
	uint32 sz;
	uint32 pos = ftell(fp);
	fseek(fp, 0, SEEK_END);
	sz = ftell(fp);
	fseek(fp, pos, SEEK_SET);
	return sz;
}

void checkOpen(FILE *fp, const char *filename) {
	if (!fp) {
		printf("Cannot open file: %s\n", filename);
		exit(-1);
	}
}

void fromFileToFile(FILE *in, FILE *out, uint32 amount) {
	char fBuf[2048];
	uint32 numRead;
	while (amount > 0) {
		numRead = fread(fBuf, 1, amount > 2048 ? 2048 : amount, in);
		if (numRead <= 0)
			break;
		amount -= numRead;
		fwrite(fBuf, 1, numRead, out);
	}
}

void createFinalFile(void) {
	FILE *inTbl, *inData, *outFinal;
	int i;
	uint32 dataStartOffset;
	uint32 dataSize;

	inTbl = fopen(TEMP_TBL, "rb");
	checkOpen(inTbl, TEMP_TBL);
	inData = fopen(TEMP_DAT, "rb");
	checkOpen(inData, TEMP_DAT);
	outFinal = fopen(FINAL_OUT, "wb");
	checkOpen(outFinal, FINAL_OUT);
	
	dataStartOffset = fileSize(inTbl) + EXTRA_TBL_HEADER;
	dataSize = fileSize(inData);

	fseek(inTbl, 7, SEEK_SET);	/* Skip past header */

	/* Write new header */
	writeUint32BE(outFinal, QTBL);
	fwrite(version->versionString, 6, 1, outFinal);
	writeByte(outFinal, version->isFloppy);
	writeByte(outFinal, version->isDemo);	
	writeByte(outFinal, versionExtra.compression);
	writeUint16BE(outFinal, versionExtra.entries);

	for (i = 0; i < versionExtra.entries; i++) {
		fromFileToFile(inTbl, outFinal, 12);
		writeByte(outFinal, readByte(inTbl));
		writeUint32BE(outFinal, dataStartOffset + readUint32BE(inTbl));
		writeUint32BE(outFinal, readUint32BE(inTbl));		
	}

	/* Append contents of temporary datafile to final datafile */
	fromFileToFile(inData, outFinal, dataSize);

	fclose(inTbl);
	fclose(inData);
	fclose(outFinal);

	/* Cleanup */
	unlink(TEMP_TBL);
	unlink(TEMP_DAT);
}

int main(int argc, char *argv[])
{
	FILE *inputData, *inputTbl, *outputTbl, *outputData, *tmpFile, *mp3File;
	uint8 compressionType = COMPRESSION_NONE;
	char tmp[5];
	char sysBuf[1024];
	char *ptr = sysBuf;
	int size, i = 1;
	uint32 prevOffset;

	if (argc < 2 || !strcmp(argv[argc-1], "--mp3"))
		showhelp(argv[0]);
	
	if (strcmp(argv[1], "--mp3") == 0) {
		compressionType = COMPRESSION_MP3;
		i++;
	}
	
	ptr += sprintf(ptr, "lame -r -h -s 11 --bitwidth 8 -m m ");
	for (; i < (argc - 1); i++) {
		/* Append optional encoder arguments */
		ptr += sprintf(ptr, "%s ", argv[i]);
	}
	ptr += sprintf(ptr, "%s %s", TEMP_SB, TEMP_MP3);

	/* Open input file (QUEEN.1) */
	inputData = fopen(argv[argc-1], "rb");
	checkOpen(inputData, argv[argc-1]);

	/* Open TBL file (QUEEN.TBL) */
	inputTbl = fopen(INPUT_TBL, "rb");
	checkOpen(inputTbl, INPUT_TBL);

	size = fileSize(inputData);
	readString(4, tmp, inputTbl);
	if (strcmp(tmp, "QTBL")) {
		printf("Invalid TBL file!\n");
		exit(-1);
	} 

	version = detectGameVersion(size);
	fseek(inputTbl, version->tableOffset, SEEK_SET); 

	versionExtra.compression = compressionType;
	versionExtra.entries = readUint16BE(inputTbl);
		
	outputTbl = fopen(TEMP_TBL, "wb");
	checkOpen(outputTbl, TEMP_TBL);	

	outputData = fopen(TEMP_DAT, "wb");
	checkOpen(outputData, TEMP_DAT);
	
	/* Write tablefile header */
	writeUint32BE(outputTbl, QTBL);
	writeByte(outputTbl, versionExtra.compression);
	writeUint16BE(outputTbl, versionExtra.entries);

	for (i = 0; i < versionExtra.entries; i++) {
		prevOffset = ftell(outputData);
		
		/* Read entry */
		readString(12, entry.filename, inputTbl);
		entry.bundle = readByte(inputTbl);
		entry.offset = readUint32BE(inputTbl);
		entry.size = readUint32BE(inputTbl);

		printf("Processing entry: %s\n", entry.filename);
		fseek(inputData, entry.offset, SEEK_SET);

		if (versionExtra.compression && strstr(entry.filename, ".SB")) { /* Do we want to compress? */
			/* Read in .SB */
			tmpFile = fopen(TEMP_SB, "wb");
			fseek(inputData, entry.offset + SB_HEADER_SIZE, SEEK_SET);
			fromFileToFile(inputData, tmpFile, entry.size - SB_HEADER_SIZE);
			fclose(tmpFile);

			/* Invoke encoder */
			if (system(sysBuf)) {
				printf("Got error from encoder. (check your parameters)\n");
				unlink(TEMP_SB);
				exit(-1);
			}

			/* Append MP3 to data file */
			mp3File = fopen(TEMP_MP3, "rb");
			entry.size = fileSize(mp3File);
			fromFileToFile(mp3File, outputData, entry.size);
			fclose(mp3File);

			/* Delete temporary files */
			unlink(TEMP_SB);
			unlink(TEMP_MP3);
		} else {
			/* Non .SB file */
			fromFileToFile(inputData, outputData, entry.size);
		}

		/* Write entry to table */
		fwrite(entry.filename, 12, 1, outputTbl);
		writeByte(outputTbl, entry.bundle);
		writeUint32BE(outputTbl, prevOffset);
		writeUint32BE(outputTbl, entry.size);	
	}	

	/* Close files */
	fclose(outputTbl);
	fclose(outputData);
	fclose(inputTbl);
	fclose(inputData);

	/* Merge the temporary table and temporary datafile to create final file */
	createFinalFile();
	
	return 0;
}
