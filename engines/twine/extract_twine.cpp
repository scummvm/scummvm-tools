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

/* LBA (TwinE) HQR/VOX/ILE/OBL resource extractor */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef WIN32
#include <direct.h>
#else
#include <sys/stat.h>
#endif

#include "common/file.h"
#include "common/util.h"
#include "engines/twine/extract_twine_convert.h"
#include "engines/twine/hqr.h"

namespace {

enum VoxType {
	VOXT_NONE = 0,
	VOXT_LBA1,
	VOXT_LBA2
};

static void printHelp(const char *bin) {
	printf("Usage: %s [options] <resource> <outputdir> [index]\n\n", bin);
	printf("Extract entries from Little Big Adventure resource archives.\n");
	printf("Supported formats: HQR, VOX, ILE, OBL\n\n");
	printf("If index is omitted, all entries are extracted.\n");
	printf("Blank entries (zero offsets) are skipped.\n\n");
	printf("Options:\n");
	printf("  --convert       Export images/fonts/palettes/islands, models as OBJ, audio as WAV\n");
	printf("  --palette-hqr <file>  Palette source archive (default: input archive)\n");
	printf("  --palette-index <n>   Palette entry index for all images/models\n");
	printf("  --lba1          LBA 1 mode (VOC audio, 16-bit 3D bodies)\n");
	printf("  --lba2          LBA 2 mode (WAV audio, 32-bit 3D bodies)\n");
	printf("  --vox           Treat the archive as a VOX file (extract hidden entries)\n");
	printf("  -h, --help      Show this help\n");
}

static bool makeDirectory(const char *path) {
#ifdef WIN32
	return _mkdir(path) == 0 || errno == EEXIST;
#else
	return mkdir(path, 0755) == 0 || errno == EEXIST;
#endif
}

static bool ensureDirectory(const Common::String &path) {
	char buf[1024];
	strncpy(buf, path.c_str(), sizeof(buf) - 1);
	buf[sizeof(buf) - 1] = '\0';

	for (size_t i = 1; buf[i]; i++) {
		if (buf[i] == '/' || buf[i] == '\\') {
			buf[i] = '\0';
			if (!makeDirectory(buf) && errno != EEXIST)
				return false;
			buf[i] = '/';
		}
	}

	return makeDirectory(buf) || errno == EEXIST;
}

static uint32 getEntryOffset(const Common::Filename &filename, int32 index) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return 0;

	const uint32 headerSize = file.readUint32LE();
	if ((uint32)index >= headerSize / 4)
		return 0;

	file.seek(4 + index * 4, SEEK_SET);
	return file.readUint32LE();
}

static uint32 getArchiveFileSize(const Common::Filename &filename) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return 0;

	const uint32 headerSize = file.readUint32LE();
	file.seek(headerSize - 4, SEEK_SET);
	return file.readUint32LE();
}

static uint32 getNextNonBlankOffset(const Common::Filename &filename, int32 index, int32 numEntries, uint32 fileSize) {
	for (int32 i = index + 1; i < numEntries; i++) {
		const uint32 offset = getEntryOffset(filename, i);
		if (offset != 0)
			return offset;
	}
	return fileSize;
}

static int32 countHiddenEntries(const Common::Filename &filename, int32 index, int32 numEntries, uint32 fileSize) {
	const uint32 offset = getEntryOffset(filename, index);
	if (offset == 0)
		return 0;

	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return 0;

	file.seek(offset + 4, SEEK_SET);
	uint32 compSize = file.readUint32LE();

	uint32 nextOffset = offset + compSize + 10;
	const uint32 nextNonBlank = getNextNonBlankOffset(filename, index, numEntries, fileSize);

	int32 count = 0;
	while (nextOffset < nextNonBlank) {
		count++;
		file.seek(nextOffset + 4, SEEK_SET);
		compSize = file.readUint32LE();
		nextOffset += compSize + 10;
	}

	return count;
}

static char detectVoxHeaderByte(const uint8 *data, int32 size) {
	if (size < 4)
		return 0;
	if (data[1] == 'r' && data[2] == 'e' && data[3] == 'a')
		return 'C';
	if (data[1] == 'I' && data[2] == 'F' && data[3] == 'F')
		return 'R';
	return 0;
}

static void fixVoxHeader(uint8 *data, int32 size, VoxType voxType) {
	if (size <= 0)
		return;
	if (data[0] != 0x00 && data[0] != 0x01)
		return;

	char headerByte = 0;
	if (voxType == VOXT_LBA1)
		headerByte = 'C';
	else if (voxType == VOXT_LBA2)
		headerByte = 'R';
	else
		headerByte = detectVoxHeaderByte(data, size);

	if (headerByte)
		data[0] = headerByte;
}

static bool loadEntryData(const Common::Filename &filename, int32 index, int32 hiddenIndex, bool isVox,
		uint8 **data, int32 *size) {
	*data = nullptr;
	*size = 0;

	if (isVox)
		*size = TwinE::HQR::getAllocVoxEntry(data, filename, index, hiddenIndex);
	else
		*size = TwinE::HQR::getAllocEntry(data, filename, index);

	return *data != nullptr && *size > 0;
}

static void loadArchivePalettes(const Common::Filename &filename, int32 numEntries, TwinePalette *palettes) {
	for (int32 i = 0; i < numEntries; i++) {
		if (TwinE::HQR::entrySize(filename, i) != 256 * 3)
			continue;

		uint8 *data = nullptr;
		if (TwinE::HQR::getAllocEntry(&data, filename, i) != 256 * 3) {
			free(data);
			continue;
		}

		memcpy(palettes[i].data, data, 256 * 3);
		palettes[i].loaded = true;
		free(data);
	}
}

static bool writeRawEntry(const char *outPath, const uint8 *data, int32 size) {
	Common::File out;
	out.open(outPath, "wb");
	if (!out.isOpen()) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

	out.write(data, size);
	return true;
}

static bool writeConvertedEntry(const uint8 *data, int32 size, int32 entryIndex, bool convert,
		const TwineConvertContext &ctx, const char *outPath) {
	if (!convert)
		return writeRawEntry(outPath, data, size);

	const TwineEntryType type = twineDetectEntryType(data, size, ctx.preferLba1);
	return twineConvertEntry(data, size, entryIndex, type, ctx, outPath);
}

static bool isVoxExtension(const Common::Filename &filename) {
	return scumm_stricmp(filename.getExtension().c_str(), "vox") == 0;
}

static void buildOutputBasename(char *outPath, size_t outPathSize, const char *outDir, int32 index, int32 hiddenIndex) {
	if (hiddenIndex > 0)
		snprintf(outPath, outPathSize, "%s/%04d_%02d", outDir, index, hiddenIndex);
	else
		snprintf(outPath, outPathSize, "%s/%04d", outDir, index);
}

static bool loadRawFile(const Common::Filename &filename, uint8 **data, int32 *size) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return false;

	*size = (int32)file.size();
	if (*size <= 0)
		return false;

	*data = (uint8 *)malloc(*size);
	if (!*data)
		return false;

	file.read_throwsOnError(*data, *size);
	return true;
}

static bool isLikelyHqrArchive(const Common::Filename &filename) {
	Common::File file;
	file.open(filename, "r");
	if (!file.isOpen())
		return false;

	const uint32 fileSize = file.size();
	if (fileSize < 12)
		return false;

	const uint32 headerSize = file.readUint32LE();
	if (headerSize < 8 || headerSize > fileSize || (headerSize % 4) != 0)
		return false;

	const int32 entries = (int32)(headerSize / 4) - 1;
	if (entries <= 0)
		return false;

	file.seek(4, SEEK_SET);
	for (int32 i = 0; i < entries && i < 8; ++i) {
		const uint32 offset = file.readUint32LE();
		if (offset == 0)
			continue;
		if (offset < headerSize || offset > fileSize)
			return false;
	}

	file.seek(headerSize - 4, SEEK_SET);
	const uint32 endOffset = file.readUint32LE();
	if (endOffset < headerSize || endOffset > fileSize)
		return false;

	return true;
}

static bool extractRawFile(const Common::Filename &input, const char *outDir, bool convert,
		const TwineConvertContext &convertCtx) {
	uint8 *data = nullptr;
	int32 size = 0;
	if (!loadRawFile(input, &data, &size)) {
		fprintf(stderr, "Unable to read %s\n", input.getFullPath().c_str());
		return false;
	}

	char outPath[1024];
	const Common::String baseName = input.getName().c_str();
	snprintf(outPath, sizeof(outPath), "%s/%s", outDir, baseName.c_str());

	bool ok = true;
	if (convert) {
		if (!writeConvertedEntry(data, size, 0, true, convertCtx, outPath))
			ok = false;
	} else {
		snprintf(outPath + strlen(outPath), sizeof(outPath) - strlen(outPath), ".bin");
		printf("Extracting %s -> %s (%d bytes)\n", input.getFullName().c_str(), outPath, size);
		if (!writeRawEntry(outPath, data, size))
			ok = false;
	}

	free(data);
	return ok;
}

} // namespace

int main(int argc, char **argv) {
	VoxType voxType = VOXT_NONE;
	bool voxForced = false;
	bool convert = false;
	bool preferLba1 = true;
	int fixedPaletteIndex = -1;
	const char *paletteHqrPath = nullptr;
	int argi = 1;

	while (argi < argc) {
		if (!strcmp(argv[argi], "-h") || !strcmp(argv[argi], "--help")) {
			printHelp(argv[0]);
			return 0;
		}
		if (!strcmp(argv[argi], "--convert")) {
			convert = true;
			argi++;
			continue;
		}
		if (!strcmp(argv[argi], "--palette-hqr")) {
			if (argi + 1 >= argc) {
				fprintf(stderr, "--palette-hqr requires an argument\n");
				return -1;
			}
			paletteHqrPath = argv[++argi];
			argi++;
			continue;
		}
		if (!strcmp(argv[argi], "--palette-index")) {
			if (argi + 1 >= argc) {
				fprintf(stderr, "--palette-index requires an argument\n");
				return -1;
			}
			fixedPaletteIndex = atoi(argv[++argi]);
			argi++;
			continue;
		}
		if (!strcmp(argv[argi], "--lba1")) {
			voxType = VOXT_LBA1;
			preferLba1 = true;
			argi++;
			continue;
		}
		if (!strcmp(argv[argi], "--lba2")) {
			voxType = VOXT_LBA2;
			preferLba1 = false;
			argi++;
			continue;
		}
		if (!strcmp(argv[argi], "--vox")) {
			voxForced = true;
			argi++;
			continue;
		}
		break;
	}

	if (argc - argi < 2) {
		printHelp(argv[0]);
		return -1;
	}

	const Common::Filename input(argv[argi]);
	const char *outDir = argv[argi + 1];
	int singleIndex = -1;
	if (argc - argi >= 3)
		singleIndex = atoi(argv[argi + 2]);

	if (!input.exists()) {
		fprintf(stderr, "File not found: %s\n", input.getFullPath().c_str());
		return -1;
	}

	if (!ensureDirectory(outDir)) {
		fprintf(stderr, "Unable to create output directory: %s\n", outDir);
		return -1;
	}

	const bool isVox = voxForced || isVoxExtension(input);

	if (!isVox && !isLikelyHqrArchive(input)) {
		TwineConvertContext convertCtx;
		convertCtx.archivePalettes = nullptr;
		convertCtx.numEntries = 0;
		convertCtx.externalPalette = nullptr;
		convertCtx.fixedPaletteIndex = fixedPaletteIndex;
		convertCtx.preferLba1 = preferLba1;

		TwinePalette externalPalette;
		if (paletteHqrPath) {
			const Common::Filename paletteFile(paletteHqrPath);
			if (paletteFile.exists()) {
				const int32 paletteEntries = TwinE::HQR::numEntries(paletteFile);
				TwinePalette *externalPalettes = new TwinePalette[paletteEntries];
				loadArchivePalettes(paletteFile, paletteEntries, externalPalettes);
				if (fixedPaletteIndex >= 0 && fixedPaletteIndex < paletteEntries &&
					externalPalettes[fixedPaletteIndex].loaded)
					externalPalette = externalPalettes[fixedPaletteIndex];
				else if (externalPalettes[0].loaded)
					externalPalette = externalPalettes[0];
				convertCtx.externalPalette = externalPalette.loaded ? &externalPalette : nullptr;
				delete[] externalPalettes;
			}
		}

		const bool failed = !extractRawFile(input, outDir, convert, convertCtx);
		return failed ? 1 : 0;
	}

	const int32 numEntries = TwinE::HQR::numEntries(input);
	if (numEntries <= 0) {
		fprintf(stderr, "No entries found in %s\n", input.getFullName().c_str());
		return -1;
	}

	TwinePalette *archivePalettes = new TwinePalette[numEntries];
	loadArchivePalettes(input, numEntries, archivePalettes);

	TwinePalette externalPalette;
	if (paletteHqrPath) {
		const Common::Filename paletteFile(paletteHqrPath);
		if (!paletteFile.exists()) {
			fprintf(stderr, "Palette archive not found: %s\n", paletteHqrPath);
			delete[] archivePalettes;
			return -1;
		}
		const int32 paletteEntries = TwinE::HQR::numEntries(paletteFile);
		TwinePalette *externalPalettes = new TwinePalette[paletteEntries];
		loadArchivePalettes(paletteFile, paletteEntries, externalPalettes);
		if (fixedPaletteIndex >= 0 && fixedPaletteIndex < paletteEntries && externalPalettes[fixedPaletteIndex].loaded)
			externalPalette = externalPalettes[fixedPaletteIndex];
		else if (externalPalettes[0].loaded)
			externalPalette = externalPalettes[0];
		delete[] externalPalettes;
	}

	TwineConvertContext convertCtx;
	convertCtx.archivePalettes = archivePalettes;
	convertCtx.numEntries = numEntries;
	convertCtx.externalPalette = externalPalette.loaded ? &externalPalette : nullptr;
	convertCtx.fixedPaletteIndex = fixedPaletteIndex;
	convertCtx.preferLba1 = preferLba1;

	if (convert && twineIsIleFilename(input)) {
		const bool ok = twineConvertIsland(input, outDir, convertCtx);
		delete[] archivePalettes;
		return ok ? 0 : 1;
	}

	const uint32 fileSize = getArchiveFileSize(input);

	const int32 start = singleIndex >= 0 ? singleIndex : 0;
	const int32 end = singleIndex >= 0 ? singleIndex : numEntries - 1;

	if (singleIndex >= numEntries) {
		fprintf(stderr, "Invalid entry index %d (max %d)\n", singleIndex, numEntries - 1);
		delete[] archivePalettes;
		return -1;
	}

	char outPath[1024];
	bool failed = false;

	for (int32 i = start; i <= end; i++) {
		const uint32 offset = getEntryOffset(input, i);
		if (offset == 0) {
			printf("Skipping blank entry %d\n", i);
			continue;
		}

		const int32 uncompressedSize = TwinE::HQR::entrySize(input, i);
		if (uncompressedSize <= 0) {
			printf("Skipping empty entry %d\n", i);
			continue;
		}

		if (isVox) {
			const int32 hiddenCount = countHiddenEntries(input, i, numEntries, fileSize);
			for (int32 h = 0; h <= hiddenCount; h++) {
				uint8 *data = nullptr;
				int32 size = 0;
				if (!loadEntryData(input, i, h, true, &data, &size)) {
					failed = true;
					continue;
				}

				fixVoxHeader(data, size, voxType);
				buildOutputBasename(outPath, sizeof(outPath), outDir, i, h);

				if (convert) {
					if (!writeConvertedEntry(data, size, i, true, convertCtx, outPath))
						failed = true;
				} else {
					snprintf(outPath + strlen(outPath), sizeof(outPath) - strlen(outPath), ".bin");
					printf("Extracting entry %d -> %s (%d bytes)\n", i, outPath, size);
					if (!writeRawEntry(outPath, data, size))
						failed = true;
				}

				free(data);
			}
		} else {
			uint8 *data = nullptr;
			int32 size = 0;
			if (!loadEntryData(input, i, 0, false, &data, &size)) {
				failed = true;
				continue;
			}

			buildOutputBasename(outPath, sizeof(outPath), outDir, i, 0);

			if (convert) {
				if (!writeConvertedEntry(data, size, i, true, convertCtx, outPath))
					failed = true;
			} else {
				snprintf(outPath + strlen(outPath), sizeof(outPath) - strlen(outPath), ".bin");
				printf("Extracting entry %d -> %s (%d bytes)\n", i, outPath, size);
				if (!writeRawEntry(outPath, data, size))
					failed = true;
			}

			free(data);
		}
	}

	delete[] archivePalettes;
	return failed ? 1 : 0;
}
