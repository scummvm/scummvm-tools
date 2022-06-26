/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Rebuild QUEEN.1 file to contain Resource Table (and optionally compress sound & speech) */

#include <string.h>

#include "common/util.h"
#include "compress.h"
#include "compress_queen.h"

const uint32 QTBL = 'QTBL';

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

struct PatchFile {
	const char *filename;
	char lang;
};

const struct CompressQueen::GameVersion gameVersions[] = {
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

CompressQueen::CompressQueen(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_supportsProgressBar = true;

	ToolInput input;
	input.format = "queen.1";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress Flight of the Amazon Queen data files.";
	_helptext = "\nUsage: " + getName() + " [mode] [mode params] [-o outputdir] <inputfile (queen.1)>\n\t" + _shorthelp + "\n";

	_version = NULL;
}

const CompressQueen::GameVersion *CompressQueen::detectGameVersion(uint32 size) {
	const struct GameVersion *pgv = gameVersions;
	int i;

	/* Compressing/rebuiling an Amiga version is not supported */
	for (i = 0; i < VER_PC_COUNT; ++i, ++pgv) {
		if (pgv->dataFileSize == size) {
			return pgv;
		}
	}

	error("Unknown/unsupported FOTAQ version");

	return NULL;
}

void CompressQueen::fromFileToFile(Common::File &in, Common::File &out, uint32 amount) {
	char fBuf[2048];
	uint32 numRead;

	while (amount > 0) {
		numRead = in.read_noThrow(fBuf, amount > 2048 ? 2048 : amount);
		if (numRead <= 0) {
			break;
		}

		amount -= numRead;
		out.write(fBuf, numRead);
	}
}

void CompressQueen::createFinalFile(Common::Filename outPath) {
	int i;
	uint32 dataStartOffset;
	uint32 dataSize;

	outPath.setFullName(FINAL_OUT);

	Common::File inTbl(TEMP_TBL, "rb");
	Common::File inData(TEMP_DAT, "rb");
	Common::File outFinal(outPath, "wb");

	dataStartOffset = inTbl.size() + EXTRA_TBL_HEADER;
	dataSize = inData.size();

	inTbl.seek(7, SEEK_SET);	/* Skip past header */

	/* Write new header */
	outFinal.writeUint32BE(QTBL);
	outFinal.write(_version->versionString, 6);
	outFinal.writeByte(_version->isFloppy);
	outFinal.writeByte(_version->isDemo);
	outFinal.writeByte(_versionExtra.compression);
	outFinal.writeUint16BE(_versionExtra.entries);

	for (i = 0; i < _versionExtra.entries; i++) {
		fromFileToFile(inTbl, outFinal, 12);
		outFinal.writeByte(inTbl.readByte());
		outFinal.writeUint32BE(dataStartOffset + inTbl.readUint32BE());
		outFinal.writeUint32BE(inTbl.readUint32BE());
	}

	/* Append contents of temporary datafile to final datafile */
	fromFileToFile(inData, outFinal, dataSize);

	/* Cleanup */
	Common::removeFile(TEMP_TBL);
	Common::removeFile(TEMP_DAT);
}

void CompressQueen::execute() {
	Common::File inputData, inputTbl, outputTbl, outputData, compFile;
	char tmp[5];
	int size, i = 1;
	uint32 prevOffset;

	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	if (outpath.empty())
		outpath = inpath;

	/* Open input file (QUEEN.1) */
	inputData.open(inpath, "rb");

	/* Open TBL file (QUEEN.TBL) */
	inpath.setFullName(INPUT_TBL);
	inputTbl.open(inpath, "rb");

	size = inputData.size();
	inputTbl.read_throwsOnError(tmp, 4);
	tmp[4] = '\0';

	if (memcmp(tmp, "QTBL", 4)) {
		error("Invalid TBL file");
	}

	if (inputTbl.readUint32BE() != CURRENT_TBL_VERSION) {
		error("You are using an incorrect (outdated?) version of the queen.tbl file");
	}

	_version = detectGameVersion(size);
	inputTbl.seek(_version->tableOffset, SEEK_SET);

	_versionExtra.compression = compression_format(_format);
	_versionExtra.entries = inputTbl.readUint16BE();

	outputTbl.open(TEMP_TBL, "wb");

	outputData.open(TEMP_DAT, "wb");

	/* Write tablefile header */
	outputTbl.writeUint32BE(QTBL);
	outputTbl.writeByte(_versionExtra.compression);
	outputTbl.writeUint16BE(_versionExtra.entries);

	for (i = 0; i < _versionExtra.entries; i++) {
		/* Update progress */
		updateProgress(i, _versionExtra.entries);

		prevOffset = outputData.pos();

		/* Read entry */
		inputTbl.read_throwsOnError(_entry.filename, 12);
		_entry.filename[12] = '\0';
		_entry.bundle = inputTbl.readByte();
		_entry.offset = inputTbl.readUint32BE();
		_entry.size = inputTbl.readUint32BE();

		print("Processing entry: %s", _entry.filename);
		inputData.seek(_entry.offset, SEEK_SET);

		if (_versionExtra.compression && strstr(_entry.filename, ".SB")) { /* Do we want to compress? */
			uint16 sbVersion;
			int headerSize;

			/* Read in .SB */
			Common::File tmpFile(TEMP_SB, "wb");
			inputData.seek(_entry.offset, SEEK_SET);

			inputData.seek(2, SEEK_CUR);
			sbVersion = inputData.readUint16LE();

			switch (sbVersion) {
			case 104:
				headerSize = SB_HEADER_SIZE_V104;
				break;
			case 110:
				headerSize = SB_HEADER_SIZE_V110;
				break;
			default:
				warning("Unhandled SB file version %d, defaulting to 104", sbVersion);
				headerSize = SB_HEADER_SIZE_V104;
				break;
			}

			inputData.seek(headerSize - 4, SEEK_CUR);
			_entry.size -= headerSize;

			fromFileToFile(inputData, tmpFile, _entry.size);
			tmpFile.close();

			/* Invoke encoder */
			setRawAudioType(false, false, 8);
			encodeAudio(TEMP_SB, true, 11840, tempEncoded, _format);

			/* Append MP3/OGG to data file */
			compFile.open(tempEncoded, "rb");
			_entry.size = compFile.size();
			fromFileToFile(compFile, outputData, _entry.size);
			compFile.close();

			/* Delete temporary files */
			Common::removeFile(TEMP_SB);
			Common::removeFile(tempEncoded);
		} else {
			/* Non .SB file */
			bool patched = false;
			/* Check for external files */

			uint8 j;
			for (j = 0; j < ARRAYSIZE(patchFiles); ++j) {
				const struct PatchFile *pf = &patchFiles[j];

				if (_version->versionString[1] == pf->lang && strcmp(pf->filename, _entry.filename) == 0) {
					/* XXX patched data files are supposed to be in cwd */
					Common::File fpPatch(pf->filename, "rb");

					if (fpPatch.isOpen()) {
						_entry.size = fpPatch.size();
						print("Patching entry, new size = %d bytes", _entry.size);
						fromFileToFile(fpPatch, outputData, _entry.size);
						fpPatch.close();
						patched = true;
					}

					break;
				}
			}

			if (!patched) {
				fromFileToFile(inputData, outputData, _entry.size);
			}
		}

		/* Write entry to table */
		outputTbl.write(_entry.filename, 12);
		outputTbl.writeByte(_entry.bundle);
		outputTbl.writeUint32BE(prevOffset);
		outputTbl.writeUint32BE(_entry.size);
	}

	outputTbl.close();
	outputData.close();

	/* Merge the temporary table and temporary datafile to create final file */
	createFinalFile(outpath);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressQueen queen(argv[0]);
	return queen.run(argc, argv);
}
#endif
