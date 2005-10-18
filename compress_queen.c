/* QueenRebuild - Rebuild QUEEN.1 file to contain Resource Table (and optionally compress sound & speech)
 * Copyright (C) 2003-2005  The ScummVM Team
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
 * $Header$
 *
 */

#include "util.h"

static const uint32 QTBL = 'QTBL';

#define INPUT_TBL	"queen.tbl"
#define FINAL_OUT	"queen.1c"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_TBL	"tempfile.tbl"
#define TEMP_SB		"tempfile.sb"

#define TEMP_MP3	"tempfile.mp3"
#define TEMP_OGG	"tempfile.ogg"
#define TEMP_FLAC	"tempfile.fla"

const char *tempEncoded;

#define CURRENT_TBL_VERSION	1
#define EXTRA_TBL_HEADER 8
#define SB_HEADER_SIZE	110


enum {
	COMPRESSION_NONE = 0,
	COMPRESSION_MP3 = 1,
	COMPRESSION_OGG = 2,
	COMPRESSION_FLAC = 3
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
	VER_SPA_TALKIE   = 8,
	VER_HEB_TALKIE   = 9,
	VER_DEMO_PCGAMES = 10,
	VER_DEMO         = 11,
	VER_INTERVIEW    = 12,

	VER_NUMBER       = 13
};

struct GameVersion {
	char versionString[6];
	uint8 isFloppy;
	uint8 isDemo;
	uint32 tableOffset;
	uint32 dataFileSize;
};

struct {
	char filename[13];
	uint8 bundle;
	uint32 offset;
	uint32 size;
} entry;

struct {
	uint8	compression;
	uint16	entries;
} versionExtra;

struct PatchFile {
	const char *filename;
	char lang;
};

const struct GameVersion gameVersions[] = {
	{ "PEM10", 1, 0, 0x00000008,  22677657 },
	{ "CEM10", 0, 0, 0x0000584E, 190787021 },
	{ "PFM10", 1, 0, 0x0002CD93,  22157304 },
	{ "CFM10", 0, 0, 0x00032585, 186689095 },
	{ "PGM10", 1, 0, 0x00059ACA,  22240013 },
	{ "CGM10", 0, 0, 0x0005F2A7, 217648975 },
	{ "PIM10", 1, 0, 0x000866B1,  22461366 },
	{ "CIM10", 0, 0, 0x0008BEE2, 190795582 },
	{ "CSM10", 0, 0, 0x000B343C, 190730602 },
	{ "CHM10", 0, 0, 0x000DA981, 190705558 },
	{ "PE100", 1, 1, 0x00101EC6,   3724538 },
	{ "PE100", 1, 1, 0x00102B7F,   3732177 },
	{ "PEint", 1, 1, 0x00103838,   1915913 }
};

const struct PatchFile patchFiles[] = {
	{ "CHIEF1.DOG", 'F' },
	{ "CHIEF2.DOG", 'F' },
	{ "BUD1.DOG",   'I' }
};

const struct GameVersion *version;


void showhelp(char *exename)
{
	printf("\nUsage: %s [--mp3/--vorbis/--flac <args>] queen.1\n", exename);
	printf("\nParams:\n");
	printf(" --mp3 <args>         encode to MP3 format\n"); 
	printf(" --vorbis <args>      encode to Ogg Vorbis Format\n");
	printf(" --flac <args>        encode to Flac Format\n");
	printf("                      (Optional: <args> are passed on to the encoder)\n");
	printf("\nExample: %s --mp3 -q 5 queen.1\n", exename);
	exit(2);
}

const struct GameVersion *detectGameVersion(uint32 size) {
	const struct GameVersion *pgv = gameVersions;
	int i;
	for (i = 0; i < VER_NUMBER; ++i, ++pgv) {
		if (pgv->dataFileSize == size) {
			return pgv;
		}
 	}
	printf("Unknown/unsupported FOTAQ version!\n");
	exit(1);
	return NULL;
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
	FILE *inputData, *inputTbl, *outputTbl, *outputData, *tmpFile, *compFile;
	uint8 compressionType = COMPRESSION_NONE;
	char tmp[5];
	char sysBuf[1024];
	char *ptr = sysBuf;
	int size, i = 1;
	uint32 prevOffset;

	if (argc < 2 || (0 == strcmp(argv[argc - 1], "--mp3") && 0 == strcmp(argv[argc - 1], "--vorbis") && 0 == strcmp(argv[argc - 1], "--flac") != 0))
		showhelp(argv[0]);
	
	if (strcmp(argv[1], "--mp3") == 0) {
		compressionType = COMPRESSION_MP3;
		tempEncoded = TEMP_MP3;
		i++;
		ptr += sprintf(ptr, "lame -r -h -s 11 --bitwidth 8 -m m ");
		for (; i < (argc - 1); i++) {
			/* Append optional encoder arguments */
			ptr += sprintf(ptr, "%s ", argv[i]);
		}
		ptr += sprintf(ptr, "%s %s", TEMP_SB, tempEncoded);
	}

	if (strcmp(argv[1], "--vorbis") == 0) {
		compressionType = COMPRESSION_OGG;
		tempEncoded = TEMP_OGG;
		i++;
		ptr += sprintf(ptr, "oggenc -r -B 8 -C 1 -R 11025 %s -o %s ", TEMP_SB, tempEncoded);
		for (; i < (argc - 1); i++) {
			/* Append optional encoder arguments */
			ptr += sprintf(ptr, "%s ", argv[i]);
		}
	}

	if (strcmp(argv[1], "--flac") == 0) {
		compressionType = COMPRESSION_FLAC;
		tempEncoded = TEMP_FLAC;
		i++;
		ptr += sprintf(ptr, "flac --force-raw-format --endian=little --sign=unsigned --bps=8 --channels=1 --sample-rate=11025 " );
		ptr += sprintf(ptr, "--no-padding --lax --no-seektable --no-ogg " );
		for (; i < (argc - 1); i++) {
			/* Append optional encoder arguments */
			ptr += sprintf(ptr, "%s ", argv[i]);
		}

		ptr += sprintf(ptr, "-o %s %s", tempEncoded, TEMP_SB );
	}

	/* Open input file (QUEEN.1) */
	inputData = fopen(argv[argc-1], "rb");
	checkOpen(inputData, argv[argc-1]);

	/* Open TBL file (QUEEN.TBL) */
	inputTbl = fopen(INPUT_TBL, "rb");
	checkOpen(inputTbl, INPUT_TBL);

	size = fileSize(inputData);
	fread(tmp, 1, 4, inputTbl);
	tmp[4] = '\0';
	if (memcmp(tmp, "QTBL", 4)) {
		printf("Invalid TBL file!\n");
		exit(-1);
	} 

	if (readUint32BE(inputTbl) != CURRENT_TBL_VERSION) {
		printf("Error: You are using an incorrect (outdated?) version of the queen.tbl file\n");
		exit(1);
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
		fread(entry.filename, 1, 12, inputTbl);
		entry.filename[12] = '\0';
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

			/* Append MP3/OGG to data file */
			compFile = fopen(tempEncoded, "rb");
			entry.size = fileSize(compFile);
			fromFileToFile(compFile, outputData, entry.size);
			fclose(compFile);

			/* Delete temporary files */
			unlink(TEMP_SB);
			unlink(tempEncoded);
		} else {
			/* Non .SB file */	
			bool patched = false;
			/* Check for external files */
			uint8 j;
			for (j = 0; j < ARRAYSIZE(patchFiles); ++j) {
				const struct PatchFile *pf = &patchFiles[j];
				if (version->versionString[1] == pf->lang && strcmp(pf->filename, entry.filename) == 0) {
					/* XXX patched data files are supposed to be in cwd */
					FILE *fpPatch = fopen(pf->filename, "rb");
					if (fpPatch) {
						entry.size = fileSize(fpPatch);
						printf("Patching entry, new size = %d bytes\n", entry.size);
						fromFileToFile(fpPatch, outputData, entry.size);
						fclose(fpPatch);
						patched = true;
					}
					break;
				}
			}
			if (!patched) {
				fromFileToFile(inputData, outputData, entry.size);
			}
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
