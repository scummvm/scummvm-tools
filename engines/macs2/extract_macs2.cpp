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

/* MACS2 Resource Extractor - extracts images, sounds, music, and strings */

#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <sys/stat.h>

static FILE *resFile = nullptr;

static uint32_t readU32(FILE *f) {
	uint8_t buf[4];
	fread(buf, 1, 4, f);
	return buf[0] | (buf[1] << 8) | (buf[2] << 16) | (buf[3] << 24);
}

static uint16_t readU16(FILE *f) {
	uint8_t buf[2];
	fread(buf, 1, 2, f);
	return buf[0] | (buf[1] << 8);
}

// Scene table entry offsets
static uint32_t getSceneBgOffset(uint16_t sceneIndex) {
	fseek(resFile, 0xC + 0x4 + sceneIndex * 0xC - 0xC, SEEK_SET);
	return readU32(resFile);
}

static uint32_t getSceneDataOffset(uint16_t sceneIndex) {
	fseek(resFile, 0xC + 0x4 + sceneIndex * 0xC - 0x8, SEEK_SET);
	return readU32(resFile);
}

static uint32_t getSceneStringsOffset(uint16_t sceneIndex) {
	fseek(resFile, 0xC + 0x4 + sceneIndex * 0xC - 0x4, SEEK_SET);
	return readU32(resFile);
}

// Decode an RLE-compressed 320x200 image
static bool decodeRLEImage(uint32_t offset, uint8_t *pixels) {
	fseek(resFile, offset, SEEK_SET);
	for (int y = 0; y < 200; y++) {
		uint16_t length = readU16(resFile);
		if (length == 0 || length > 960) return false;
		std::vector<uint8_t> rowData(length);
		fread(rowData.data(), 1, length, resFile);
		int remaining = 320;
		int x = 0;
		uint8_t *ptr = rowData.data();
		uint8_t *end = ptr + length;
		while (remaining > 0 && ptr < end) {
			uint8_t val = *ptr++;
			if (val != 0xF0) {
				pixels[y * 320 + x++] = val;
				remaining--;
			} else {
				if (ptr + 2 > end) break;
				uint8_t runLen = *ptr++;
				uint8_t runVal = *ptr++;
				for (int i = 0; i < runLen && remaining > 0; i++) {
					pixels[y * 320 + x++] = runVal;
					remaining--;
				}
			}
		}
	}
	return true;
}

// Write a BMP file (8-bit indexed)
static void writeBMP(const char *path, const uint8_t *pixels, const uint8_t *palette) {
	FILE *f = fopen(path, "wb");
	if (!f) { fprintf(stderr, "Cannot write %s\n", path); return; }

	// BMP header
	uint32_t imageSize = 320 * 200;
	uint32_t paletteSize = 256 * 4;
	uint32_t dataOffset = 14 + 40 + paletteSize;
	uint32_t fileSize = dataOffset + imageSize;

	// File header
	fputc('B', f); fputc('M', f);
	uint8_t hdr[12] = {};
	hdr[0] = fileSize & 0xFF; hdr[1] = (fileSize >> 8) & 0xFF;
	hdr[2] = (fileSize >> 16) & 0xFF; hdr[3] = (fileSize >> 24) & 0xFF;
	hdr[8] = dataOffset & 0xFF; hdr[9] = (dataOffset >> 8) & 0xFF;
	hdr[10] = (dataOffset >> 16) & 0xFF; hdr[11] = (dataOffset >> 24) & 0xFF;
	fwrite(hdr, 1, 12, f);

	// DIB header (BITMAPINFOHEADER)
	uint8_t dib[40] = {};
	dib[0] = 40; // header size
	dib[4] = 0x40; dib[5] = 0x01; // width = 320
	dib[8] = 0xC8; // height = 200 (positive = bottom-up)
	dib[12] = 1; // planes
	dib[14] = 8; // bpp
	fwrite(dib, 1, 40, f);

	// Palette (BGRA)
	for (int i = 0; i < 256; i++) {
		// MACS2 palette is 6-bit VGA, scale to 8-bit
		uint8_t r = (palette[i * 3 + 0] * 259 + 33) >> 6;
		uint8_t g = (palette[i * 3 + 1] * 259 + 33) >> 6;
		uint8_t b = (palette[i * 3 + 2] * 259 + 33) >> 6;
		uint8_t bgra[4] = {b, g, r, 0};
		fwrite(bgra, 1, 4, f);
	}

	// Pixel data (bottom-up: write rows in reverse)
	for (int y = 199; y >= 0; y--) {
		fwrite(pixels + y * 320, 1, 320, f);
	}
	fclose(f);
	printf("  Wrote %s\n", path);
}

// Extract background image for a scene
static void extractImage(uint16_t sceneIndex, const char *outDir) {
	uint32_t bgOffset = getSceneBgOffset(sceneIndex);
	if (bgOffset == 0) return;

	uint8_t pixels[320 * 200];
	if (!decodeRLEImage(bgOffset, pixels)) return;

	// Palette follows immediately after the image
	uint8_t palette[768];
	fread(palette, 1, 768, resFile);

	char path[512];
	snprintf(path, sizeof(path), "%s/scene_%03d.bmp", outDir, sceneIndex);
	writeBMP(path, pixels, palette);
}

// Extract indexed resources (sounds/music) for a scene
static void extractResources(uint16_t sceneIndex, const char *outDir, const char *prefix) {
	uint32_t dataOffset = getSceneDataOffset(sceneIndex);
	if (dataOffset == 0) return;

	fseek(resFile, dataOffset, SEEK_SET);
	uint32_t resourceTable[32]; // 0x80 / 4 = 32 entries
	for (int i = 0; i < 32; i++) {
		resourceTable[i] = readU32(resFile);
	}

	for (int i = 0; i < 32; i++) {
		if (resourceTable[i] == 0) continue;
		fseek(resFile, resourceTable[i], SEEK_SET);
		uint32_t size = readU32(resFile);
		if (size == 0 || size > 0x100000) continue;

		std::vector<uint8_t> data(size);
		fread(data.data(), 1, size, resFile);

		char path[512];
		snprintf(path, sizeof(path), "%s/%s_scene%03d_%02d.bin", outDir, prefix, sceneIndex, i + 1);
		FILE *out = fopen(path, "wb");
		if (out) {
			fwrite(data.data(), 1, size, out);
			fclose(out);
			printf("  Wrote %s (%u bytes)\n", path, size);
		}
	}
}

// Decrypt a string from the MACS2 format
static std::string decryptString(const uint8_t *data, uint16_t length) {
	std::string result;
	for (int i = 1; i <= length; i++) {
		uint8_t x = (uint8_t)(i * i * 0x0C);
		uint8_t y = (uint8_t)(data[i - 1] ^ i);
		uint8_t r = (uint8_t)(x ^ y);
		result += (char)r;
	}
	return result;
}

// Extract strings for a scene
static void extractStrings(uint16_t sceneIndex, const char *outDir) {
	uint32_t strOffset = getSceneStringsOffset(sceneIndex);
	if (strOffset == 0) return;

	fseek(resFile, strOffset, SEEK_SET);
	uint16_t totalSize = readU16(resFile);
	if (totalSize == 0) return;

	std::vector<uint8_t> strData(totalSize);
	fread(strData.data(), 1, totalSize, resFile);

	char path[512];
	snprintf(path, sizeof(path), "%s/strings_scene%03d.txt", outDir, sceneIndex);
	FILE *out = fopen(path, "w");
	if (!out) return;

	fprintf(out, "; Scene %d strings\n", sceneIndex);
	fprintf(out, "; Total data size: %u bytes\n\n", totalSize);

	// Walk through the string data - each string is: uint16 length + encrypted bytes
	uint32_t pos = 0;
	int stringIndex = 0;
	while (pos + 2 <= totalSize) {
		uint16_t len = strData[pos] | (strData[pos + 1] << 8);
		pos += 2;
		if (len == 0 || pos + len > totalSize) break;
		std::string decoded = decryptString(strData.data() + pos, len);
		fprintf(out, "[%d] (offset=%u) %s\n", stringIndex, (unsigned)(pos - 2), decoded.c_str());
		pos += len;
		stringIndex++;
	}

	fclose(out);
	printf("  Wrote %s (%d strings)\n", path, stringIndex);
}

static void mkdirp(const char *path) {
#ifdef _WIN32
	mkdir(path);
#else
	mkdir(path, 0755);
#endif
}

static void printHelp(const char *bin) {
	printf("MACS2 Resource Extractor\n\n");
	printf("Usage: %s <mode> <game_data_file> <output_dir> [scene_index]\n\n", bin);
	printf("Modes:\n");
	printf("  images   - Extract background images as BMP files\n");
	printf("  sounds   - Extract sound/music resource blobs\n");
	printf("  strings  - Extract and decrypt text strings\n");
	printf("  all      - Extract everything\n");
	printf("\n");
	printf("If scene_index is omitted, extracts from all scenes.\n");
}

int main(int argc, char **argv) {
	if (argc < 4 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return 1;
	}

	const char *mode = argv[1];
	const char *resPath = argv[2];
	const char *outDir = argv[3];

	resFile = fopen(resPath, "rb");
	if (!resFile) {
		fprintf(stderr, "Error: Cannot open '%s'\n", resPath);
		return 1;
	}

	mkdirp(outDir);

	int startScene = 1, endScene = 512;
	if (argc >= 5) {
		startScene = endScene = atoi(argv[4]);
	}

	bool doImages = !strcmp(mode, "images") || !strcmp(mode, "all");
	bool doSounds = !strcmp(mode, "sounds") || !strcmp(mode, "all");
	bool doStrings = !strcmp(mode, "strings") || !strcmp(mode, "all");

	for (int scene = startScene; scene <= endScene; scene++) {
		bool hasData = false;

		if (doImages) {
			uint32_t bgOff = getSceneBgOffset(scene);
			if (bgOff != 0) {
				if (!hasData) { printf("Scene %d:\n", scene); hasData = true; }
				extractImage(scene, outDir);
			}
		}

		if (doSounds) {
			uint32_t dataOff = getSceneDataOffset(scene);
			if (dataOff != 0) {
				if (!hasData) { printf("Scene %d:\n", scene); hasData = true; }
				extractResources(scene, outDir, "res");
			}
		}

		if (doStrings) {
			uint32_t strOff = getSceneStringsOffset(scene);
			if (strOff != 0) {
				if (!hasData) { printf("Scene %d:\n", scene); hasData = true; }
				extractStrings(scene, outDir);
			}
		}
	}

	fclose(resFile);
	return 0;
}
