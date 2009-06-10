/* compress_queen - Rebuild QUEEN.1 file to contain Resource Table (and optionally compress sound & speech)
 * Copyright (C) 2003-2006  The ScummVM Team
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

#include "compress.h"

static const uint32 QTBL = 'QTBL';
static CompressMode gCompMode = kMP3Mode;

#define INPUT_TBL	"queen.tbl"
#define FINAL_OUT	"queen.1c"

#define TEMP_DAT	"tempfile.dat"
#define TEMP_TBL	"tempfile.tbl"
#define TEMP_SB		"tempfile.sb"

#define CURRENT_TBL_VERSION	2
#define EXTRA_TBL_HEADER 8
#define SB_HEADER_SIZE_V104 110
#define SB_HEADER_SIZE_V110 122

enum {
	COMPRESSION_NONE = 0,
	COMPRESSION_MP3 = 1,
	COMPRESSION_OGG = 2,
	COMPRESSION_FLAC = 3
};

enum {
	VER_ENG_FLOPPY     = 0,
	VER_ENG_TALKIE     = 1,
	VER_FRE_FLOPPY     = 2,
	VER_FRE_TALKIE     = 3,
	VER_GER_FLOPPY     = 4,
	VER_GER_TALKIE     = 5,
	VER_ITA_FLOPPY     = 6,
	VER_ITA_TALKIE     = 7,
	VER_SPA_TALKIE     = 8,
	VER_HEB_TALKIE     = 9,
	VER_DEMO_PCGAMES   = 10,
	VER_DEMO           = 11,
	VER_INTERVIEW      = 12,
	VER_AMI_ENG_FLOPPY = 13,
	VER_AMI_DEMO       = 14,
	VER_AMI_INTERVIEW  = 15,

	VER_PC_COUNT       = 13 /* PC versions */
};

struct GameVersion {
	char versionString[6];
	uint8 isFloppy;
	uint8 isDemo;
	uint32 tableOffset;
	uint32 dataFileSize;
};

struct Entry {
	char filename[13];
	uint8 bundle;
	uint32 offset;
	uint32 size;
};

Entry entry;

struct VersionExtra {
	uint8	compression;
	uint16	entries;
};

VersionExtra versionExtra;

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
	{ "PEint", 1, 1, 0x00103838,   1915913 },
	{ "aEM10", 1, 0, 0x00103F1E,    351775 },
	{ "CE101", 1, 1, 0x00107D8D,    563335 },
	{ "PE100", 1, 1, 0x001086D4,    597032 }
};

const struct PatchFile patchFiles[] = {
	{ "CHIEF1.DOG", 'F' },
	{ "CHIEF2.DOG", 'F' },
	{ "BUD1.DOG",   'I' }
};

const struct GameVersion *version;

const struct GameVersion *detectGameVersion(uint32 size) {
	const struct GameVersion *pgv = gameVersions;
	int i;

	/* Compressing/rebuiling an Amiga version is not supported */
	for (i = 0; i < VER_PC_COUNT; ++i, ++pgv) {
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
		if (numRead <= 0) {
			break;
		}

		amount -= numRead;
		fwrite(fBuf, 1, numRead, out);
	}
}

void createFinalFile(Filename *inputPath) {
	FILE *inTbl, *inData, *outFinal;
	int i;
	uint32 dataStartOffset;
	uint32 dataSize;

	inTbl = fopen(TEMP_TBL, "rb");
	checkOpen(inTbl, TEMP_TBL);
	inData = fopen(TEMP_DAT, "rb");
	checkOpen(inData, TEMP_DAT);
	inputPath->setFullName(FINAL_OUT);
	outFinal = fopen(inputPath->getFullPath(), "wb");
	checkOpen(outFinal, inputPath->getFullPath());

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

const char *helptext = "\nUsage: %s [params] queen.1\n" kCompressionAudioHelp;

int main(int argc, char *argv[]) {
	FILE *inputData, *inputTbl, *outputTbl, *outputData, *tmpFile, *compFile;
	uint8 compressionType = COMPRESSION_NONE;
	Filename inpath, outpath;;
	char tmp[5];
	int size, i = 1;
	uint32 prevOffset;
	int first_arg = 1;
	int last_arg = argc - 1;

	parseHelpArguments(argv, argc, helptext);

	gCompMode = process_audio_params(argc, argv, &first_arg);

	if(gCompMode == kNoAudioMode) {
		// Unknown mode (failed to parse arguments), display help and exit
		printf(helptext, argv[0]);
		exit(2);
	}
	
	// Now we try to find the proper output file
	// also make sure we skip those arguments
	if (parseOutputFileArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputFileArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else
		// Standard output
		outpath.setFullPath("out");

	inpath.setFullPath(argv[first_arg]);

	/* Open input file (QUEEN.1) */
	inputData = fopen(inpath.getFullPath(), "rb");
	checkOpen(inputData, inpath.getFullPath());

	/* Open TBL file (QUEEN.TBL) */
	inpath.setFullName(INPUT_TBL);
	inputTbl = fopen(inpath.getFullPath(), "rb");
	checkOpen(inputTbl, inpath.getFullPath());

	size = fileSize(inputData);
	fread(tmp, 1, 4, inputTbl);
	tmp[4] = '\0';

	if (memcmp(tmp, "QTBL", 4)) {
		error("Invalid TBL file");
	}

	if (readUint32BE(inputTbl) != CURRENT_TBL_VERSION) {
		error("You are using an incorrect (outdated?) version of the queen.tbl file");
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
			uint16 sbVersion;
			int headerSize;

			/* Read in .SB */
			tmpFile = fopen(TEMP_SB, "wb");
			fseek(inputData, entry.offset, SEEK_SET);

			fseek(inputData, 2, SEEK_CUR);
			sbVersion = readUint16LE(inputData);

			switch (sbVersion) {
			case 104:
				headerSize = SB_HEADER_SIZE_V104;
				break;
			case 110:
				headerSize = SB_HEADER_SIZE_V110;
				break;
			default:
				warning("Unhandled SB file version %d, defaulting to 104\n", sbVersion);
				headerSize = SB_HEADER_SIZE_V104;
				break;
			}

			fseek(inputData, headerSize - 4, SEEK_CUR);
			entry.size -= headerSize;

			fromFileToFile(inputData, tmpFile, entry.size);
			fclose(tmpFile);

			/* Invoke encoder */
			setRawAudioType(false, false, 8);
			encodeAudio(TEMP_SB, true, 11840, tempEncoded, gCompMode);

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
	createFinalFile(&inpath);

	return 0;
}
