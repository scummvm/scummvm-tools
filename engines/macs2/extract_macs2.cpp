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

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <set>
#include <string>
#include <vector>
#ifdef WIN32
#include <direct.h>
#else
#include <sys/stat.h>
#endif

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
		if (length == 0 || length > 960)
			return false;
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
				if (ptr + 2 > end)
					break;
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

// Write a BMP file (8-bit indexed, arbitrary dimensions)
static void writeBMPEx(const char *path, const uint8_t *pixels, uint16_t w, uint16_t h, const uint8_t *palette) {
	FILE *f = fopen(path, "wb");
	if (!f) {
		fprintf(stderr, "Cannot write %s\n", path);
		return;
	}

	uint32_t rowStride = (w + 3) & ~3;
	uint32_t imageSize = rowStride * h;
	uint32_t paletteSize = 256 * 4;
	uint32_t dataOffset = 14 + 40 + paletteSize;
	uint32_t fileSize = dataOffset + imageSize;

	fputc('B', f);
	fputc('M', f);
	uint8_t hdr[12] = {};
	hdr[0] = fileSize & 0xFF;
	hdr[1] = (fileSize >> 8) & 0xFF;
	hdr[2] = (fileSize >> 16) & 0xFF;
	hdr[3] = (fileSize >> 24) & 0xFF;
	hdr[8] = dataOffset & 0xFF;
	hdr[9] = (dataOffset >> 8) & 0xFF;
	hdr[10] = (dataOffset >> 16) & 0xFF;
	hdr[11] = (dataOffset >> 24) & 0xFF;
	fwrite(hdr, 1, 12, f);

	uint8_t dib[40] = {};
	dib[0] = 40;
	dib[4] = w & 0xFF;
	dib[5] = (w >> 8) & 0xFF;
	dib[8] = h & 0xFF;
	dib[9] = (h >> 8) & 0xFF;
	dib[12] = 1;
	dib[14] = 8;
	fwrite(dib, 1, 40, f);

	for (int i = 0; i < 256; i++) {
		uint8_t r = (palette[i * 3 + 0] * 259 + 33) >> 6;
		uint8_t g = (palette[i * 3 + 1] * 259 + 33) >> 6;
		uint8_t b = (palette[i * 3 + 2] * 259 + 33) >> 6;
		uint8_t bgra[4] = {b, g, r, 0};
		fwrite(bgra, 1, 4, f);
	}

	uint8_t pad[4] = {0};
	for (int y = h - 1; y >= 0; y--) {
		fwrite(pixels + y * w, 1, w, f);
		if (rowStride > w)
			fwrite(pad, 1, rowStride - w, f);
	}
	fclose(f);
	printf("  Wrote %s\n", path);
}

// Write a BMP file (8-bit indexed)
static void writeBMP(const char *path, const uint8_t *pixels, const uint8_t *palette) {
	FILE *f = fopen(path, "wb");
	if (!f) {
		fprintf(stderr, "Cannot write %s\n", path);
		return;
	}

	// BMP header
	uint32_t imageSize = 320 * 200;
	uint32_t paletteSize = 256 * 4;
	uint32_t dataOffset = 14 + 40 + paletteSize;
	uint32_t fileSize = dataOffset + imageSize;

	// File header
	fputc('B', f);
	fputc('M', f);
	uint8_t hdr[12] = {};
	hdr[0] = fileSize & 0xFF;
	hdr[1] = (fileSize >> 8) & 0xFF;
	hdr[2] = (fileSize >> 16) & 0xFF;
	hdr[3] = (fileSize >> 24) & 0xFF;
	hdr[8] = dataOffset & 0xFF;
	hdr[9] = (dataOffset >> 8) & 0xFF;
	hdr[10] = (dataOffset >> 16) & 0xFF;
	hdr[11] = (dataOffset >> 24) & 0xFF;
	fwrite(hdr, 1, 12, f);

	// DIB header (BITMAPINFOHEADER)
	uint8_t dib[40] = {};
	dib[0] = 40; // header size
	dib[4] = 0x40;
	dib[5] = 0x01; // width = 320
	dib[8] = 0xC8; // height = 200 (positive = bottom-up)
	dib[12] = 1;   // planes
	dib[14] = 8;   // bpp
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

// Forward declaration
static bool decodeRLEMap(FILE *f, uint8_t *pixels);

// Extract cursor/icon images (33 entries at file offset 0x3B10)
static void extractCursors(const char *outDir) {
	// Read global palette at 0x3010
	uint8_t palette[768];
	fseek(resFile, 0xC + 0x4 + 0x3000, SEEK_SET);
	fread(palette, 1, 768, resFile);

	fseek(resFile, 0xC + 0x4 + 0x3000 + 0x300 + 0x800, SEEK_SET);
	printf("Extracting cursors...\n");
	int count = 0;
	for (int i = 0; i < 0x21; i++) {
		uint32_t length = readU32(resFile);
		if (length == 0)
			continue;
		long blobStart = ftell(resFile);
		fseek(resFile, 2, SEEK_CUR); // skip 2 bytes
		uint16_t w = readU16(resFile);
		uint16_t h = readU16(resFile);
		if (w == 0 || h == 0 || (uint32_t)w * h > length) {
			fseek(resFile, blobStart + length, SEEK_SET);
			continue;
		}
		std::vector<uint8_t> pixels(w * h);
		fread(pixels.data(), 1, w * h, resFile);

		char path[512];
		snprintf(path, sizeof(path), "%s/cursor_%02d.bmp", outDir, i);
		writeBMPEx(path, pixels.data(), w, h, palette);
		count++;
		fseek(resFile, blobStart + length, SEEK_SET);
	}
	printf("Extracted %d cursor images.\n", count);
}

// Render a font's glyphs into an atlas BMP
static void renderFontAtlas(const char *path, FILE *f, uint16_t glyphCount, const uint8_t *palette) {
	struct Glyph {
		uint16_t w, h;
		std::vector<uint8_t> data;
	};
	std::vector<Glyph> glyphs;
	uint16_t maxH = 0;
	uint32_t totalW = 0;
	for (int i = 0; i < glyphCount; i++) {
		Glyph g;
		fseek(f, 1, SEEK_CUR); // ascii byte
		g.w = readU16(f);
		g.h = readU16(f);
		if (g.w > 320 || g.h > 200)
			return;
		g.data.resize((uint32_t)g.w * g.h);
		fread(g.data.data(), 1, g.data.size(), f);
		if (g.h > maxH)
			maxH = g.h;
		totalW += g.w + 1;
		glyphs.push_back(g);
	}
	if (glyphs.empty() || totalW == 0)
		return;

	uint16_t atlasW = (uint16_t)(totalW - 1);
	std::vector<uint8_t> atlas((uint32_t)atlasW * maxH, 0);
	uint32_t x = 0;
	for (const auto &g : glyphs) {
		for (uint16_t gy = 0; gy < g.h; gy++)
			for (uint16_t gx = 0; gx < g.w; gx++)
				atlas[gy * atlasW + x + gx] = g.data[gy * g.w + gx];
		x += g.w + 1;
	}
	writeBMPEx(path, atlas.data(), atlasW, maxH, palette);
}

// Extract all fonts: global fonts (2) + overlay fonts from scene/object resources
static void extractFonts(const char *outDir) {
	// Read global palette
	uint8_t palette[768];
	fseek(resFile, 0xC + 0x4 + 0x3000, SEEK_SET);
	fread(palette, 1, 768, resFile);

	// Seek past cursor images
	fseek(resFile, 0xC + 0x4 + 0x3000 + 0x300 + 0x800, SEEK_SET);
	for (int i = 0; i < 0x21; i++) {
		uint32_t length = readU32(resFile);
		if (length > 0)
			fseek(resFile, length, SEEK_CUR);
	}

	printf("Extracting fonts...\n");
	int fontIdx = 0;

	// Global Font 1 (dialog font)
	readU32(resFile); // size field
	uint16_t glyphCount = readU16(resFile);
	if (glyphCount > 0 && glyphCount <= 256) {
		char path[512];
		snprintf(path, sizeof(path), "%s/font_%d_dialog.bmp", outDir, fontIdx);
		renderFontAtlas(path, resFile, glyphCount, palette);
		fontIdx++;
	}

	// Global Font 2 (save/load panel font)
	readU32(resFile);
	glyphCount = readU16(resFile);
	if (glyphCount > 0 && glyphCount <= 256) {
		char path[512];
		snprintf(path, sizeof(path), "%s/font_%d_panel.bmp", outDir, fontIdx);
		renderFontAtlas(path, resFile, glyphCount, palette);
		fontIdx++;
	}

	// Overlay fonts from scene resources (loaded by opcode 0x38)
	// These are at resourceOffset + 0x10: uint16 glyphCount + glyph data
	std::set<uint32_t> extractedOffsets;
	for (int scene = 1; scene <= 512; scene++) {
		uint32_t dataOffset = getSceneDataOffset(scene);
		if (dataOffset == 0)
			continue;
		fseek(resFile, dataOffset, SEEK_SET);
		uint32_t resourceTable[32];
		for (int i = 0; i < 32; i++)
			resourceTable[i] = readU32(resFile);

		for (int i = 0; i < 32; i++) {
			if (resourceTable[i] == 0)
				continue;
			if (extractedOffsets.count(resourceTable[i]))
				continue;

			// Check if resource has AHFFFONT magic header (at offset+4, after size field)
			fseek(resFile, resourceTable[i] + 4, SEEK_SET);
			uint8_t magic[8];
			fread(magic, 1, 8, resFile);
			if (memcmp(magic, "AHFF", 4) != 0 || memcmp(magic + 4, "FONT", 4) != 0)
				continue;

			// Font data at offset+0x10: uint16 glyphCount + glyph data
			fseek(resFile, resourceTable[i] + 0x10, SEEK_SET);
			uint16_t gc = readU16(resFile);
			if (gc == 0 || gc > 256)
				continue;

			// Extract font
			extractedOffsets.insert(resourceTable[i]);
			char path[512];
			snprintf(path, sizeof(path), "%s/font_%d_scene%03d_res%02d.bmp", outDir, fontIdx, scene, i + 1);
			renderFontAtlas(path, resFile, gc, palette);
			fontIdx++;
		}
	}
	printf("Extracted %d font atlas images.\n", fontIdx);
}

// Extract background image for a scene
static void extractImage(uint16_t sceneIndex, const char *outDir) {
	uint32_t bgOffset = getSceneBgOffset(sceneIndex);
	if (bgOffset == 0)
		return;

	uint8_t pixels[320 * 200];
	if (!decodeRLEImage(bgOffset, pixels))
		return;

	// Palette follows immediately after the image
	uint8_t palette[768];
	fread(palette, 1, 768, resFile);

	char path[512];
	snprintf(path, sizeof(path), "%s/scene_%03d.bmp", outDir, sceneIndex);
	writeBMP(path, pixels, palette);

	// Export the 4 maps as BMP using the scene's own palette (same as imgui debugger)
	fseek(resFile, 256, SEEK_CUR); // shading table
	fseek(resFile, 3, SEEK_CUR);   // 3 unknown bytes

	const char *mapNames[] = {"depth", "pathfinding", "shadow", "hotspot"};
	for (int m = 0; m < 4; m++) {
		uint8_t mapPixels[320 * 200];
		if (!decodeRLEMap(resFile, mapPixels))
			break;
		snprintf(path, sizeof(path), "%s/scene_%03d_%s.bmp", outDir, sceneIndex, mapNames[m]);
		writeBMP(path, mapPixels, palette);
	}
}

// Extract indexed resources (sounds/music) for a scene
static void extractResources(uint16_t sceneIndex, const char *outDir, const char *prefix) {
	uint32_t dataOffset = getSceneDataOffset(sceneIndex);
	if (dataOffset == 0)
		return;

	fseek(resFile, dataOffset, SEEK_SET);
	uint32_t resourceTable[32]; // 0x80 / 4 = 32 entries
	for (int i = 0; i < 32; i++) {
		resourceTable[i] = readU32(resFile);
	}

	for (int i = 0; i < 32; i++) {
		if (resourceTable[i] == 0)
			continue;
		fseek(resFile, resourceTable[i], SEEK_SET);
		uint32_t size = readU32(resFile);
		if (size == 0 || size > 0x100000)
			continue;

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
	if (strOffset == 0)
		return;

	fseek(resFile, strOffset, SEEK_SET);
	uint16_t totalSize = readU16(resFile);
	if (totalSize == 0)
		return;

	std::vector<uint8_t> strData(totalSize);
	fread(strData.data(), 1, totalSize, resFile);

	char path[512];
	snprintf(path, sizeof(path), "%s/strings_scene%03d.txt", outDir, sceneIndex);
	FILE *out = fopen(path, "w");
	if (!out)
		return;

	fprintf(out, "; Scene %d strings\n", sceneIndex);
	fprintf(out, "; Total data size: %u bytes\n\n", totalSize);

	// Walk through the string data - each string is: uint16 length + encrypted bytes
	uint32_t pos = 0;
	int stringIndex = 0;
	while (pos + 2 <= totalSize) {
		uint16_t len = strData[pos] | (strData[pos + 1] << 8);
		pos += 2;
		if (len == 0 || pos + len > totalSize)
			break;
		std::string decoded = decryptString(strData.data() + pos, len);
		fprintf(out, "[%d] (offset=%u) %s\n", stringIndex, (unsigned)(pos - 2), decoded.c_str());
		pos += len;
		stringIndex++;
	}

	fclose(out);
	printf("  Wrote %s (%d strings)\n", path, stringIndex);
}

// Extract strings for an object
static void extractObjectStrings(uint16_t objectIndex, const char *outDir) {
	uint32_t strTableOffset = (uint32_t)objectIndex * 0xC + 0xC + 0x4 + 0x17FC;
	fseek(resFile, strTableOffset, SEEK_SET);
	uint32_t strDataOffset = readU32(resFile);
	if (strDataOffset == 0)
		return;

	fseek(resFile, strDataOffset, SEEK_SET);
	uint16_t totalSize = readU16(resFile);
	if (totalSize == 0)
		return;

	std::vector<uint8_t> strData(totalSize);
	fread(strData.data(), 1, totalSize, resFile);

	char path[512];
	snprintf(path, sizeof(path), "%s/strings_object%03d.txt", outDir, objectIndex);
	FILE *out = fopen(path, "w");
	if (!out)
		return;

	fprintf(out, "; Object %d strings\n", objectIndex);
	fprintf(out, "; Total data size: %u bytes\n\n", totalSize);

	uint32_t pos = 0;
	int stringIndex = 0;
	while (pos + 2 <= totalSize) {
		uint16_t len = strData[pos] | (strData[pos + 1] << 8);
		pos += 2;
		if (len == 0 || pos + len > totalSize)
			break;
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

// Skip an RLE-compressed 320x200 image in the file (advance past it without decoding)
static void skipRLEImage(FILE *f) {
	for (int y = 0; y < 200; y++) {
		uint16_t length = readU16(f);
		fseek(f, length, SEEK_CUR);
	}
}

// Decode an RLE 320x200 image into a buffer
static bool decodeRLEMap(FILE *f, uint8_t *pixels) {
	for (int y = 0; y < 200; y++) {
		uint16_t length = readU16(f);
		if (length == 0 || length > 960)
			return false;
		std::vector<uint8_t> rowData(length);
		fread(rowData.data(), 1, length, f);
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
				if (ptr + 2 > end)
					break;
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

static void writeRawFile(const char *path, const uint8_t *data, size_t size) {
	FILE *f = fopen(path, "wb");
	if (f) {
		fwrite(data, 1, size, f);
		fclose(f);
	}
}

// Extract comprehensive scene data as JSON + binary map files
static void extractSceneData(uint16_t sceneIndex, const char *outDir) {
	uint32_t bgOffset = getSceneBgOffset(sceneIndex);
	if (bgOffset == 0)
		return;

	uint32_t dataOffset = getSceneDataOffset(sceneIndex);
	uint32_t stringsOffset = getSceneStringsOffset(sceneIndex);

	fseek(resFile, bgOffset, SEEK_SET);
	skipRLEImage(resFile); // background image

	// Palette (768 bytes, 6-bit VGA)
	uint8_t palette[768];
	fread(palette, 1, 768, resFile);

	// Shading table (256 bytes)
	uint8_t shadingTable[256];
	fread(shadingTable, 1, 256, resFile);

	// 3 unknown bytes
	uint8_t unknownBytes[3];
	fread(unknownBytes, 1, 3, resFile);

	// 4 RLE maps (each 320x200)
	uint8_t depthMap[320 * 200];
	uint8_t pathfindingMap[320 * 200];
	uint8_t shadowMap[320 * 200];
	uint8_t hotspotMap[320 * 200];
	decodeRLEMap(resFile, depthMap);
	decodeRLEMap(resFile, pathfindingMap);
	decodeRLEMap(resFile, shadowMap);
	decodeRLEMap(resFile, hotspotMap);

	// Write map binary files
	char path[512];
	snprintf(path, sizeof(path), "%s/scene%03d_depth.bin", outDir, sceneIndex);
	writeRawFile(path, depthMap, sizeof(depthMap));
	snprintf(path, sizeof(path), "%s/scene%03d_pathfinding.bin", outDir, sceneIndex);
	writeRawFile(path, pathfindingMap, sizeof(pathfindingMap));
	snprintf(path, sizeof(path), "%s/scene%03d_shadow.bin", outDir, sceneIndex);
	writeRawFile(path, shadowMap, sizeof(shadowMap));
	snprintf(path, sizeof(path), "%s/scene%03d_hotspot.bin", outDir, sceneIndex);
	writeRawFile(path, hotspotMap, sizeof(hotspotMap));

	// Pathfinding nodes: 16 entries × 10 bytes
	struct PathNode {
		uint16_t x, y;
		uint8_t adj[4];
		uint16_t numConnections;
	} nodes[16];
	for (int i = 0; i < 16; i++) {
		nodes[i].x = readU16(resFile);
		nodes[i].y = readU16(resFile);
		fread(nodes[i].adj, 1, 4, resFile);
		nodes[i].numConnections = readU16(resFile);
	}

	uint16_t numHotspots = readU16(resFile);

	// Hotspot color table: 16 uint16 entries (maps pixel value to object ID)
	uint16_t hotspotColors[16];
	for (int i = 0; i < 16; i++)
		hotspotColors[i] = readU16(resFile);

	// Resource offsets from data block (32 dwords = special anim file offsets)
	uint32_t resourceOffsets[32] = {};
	uint32_t mapImageOffset = 0;
	if (dataOffset != 0) {
		fseek(resFile, dataOffset, SEEK_SET);
		for (int i = 0; i < 32; i++)
			resourceOffsets[i] = readU32(resFile);
		// Map image offset at data block +0x3C0
		fseek(resFile, dataOffset + 0x3C0, SEEK_SET);
		mapImageOffset = readU32(resFile);
	}

	// Script size
	uint16_t scriptSize = 0;
	if (dataOffset != 0) {
		fseek(resFile, dataOffset + 0x80, SEEK_SET);
		scriptSize = readU16(resFile);
	}

	// Strings size
	uint16_t stringsSize = 0;
	if (stringsOffset != 0) {
		fseek(resFile, stringsOffset, SEEK_SET);
		stringsSize = readU16(resFile);
	}

	// Write JSON
	snprintf(path, sizeof(path), "%s/scenedata_%03d.json", outDir, sceneIndex);
	FILE *out = fopen(path, "w");
	if (!out)
		return;

	fprintf(out, "{\n");
	fprintf(out, "  \"scene\": %d,\n", sceneIndex);
	fprintf(out, "  \"offsets\": {\n");
	fprintf(out, "    \"background\": %u,\n", bgOffset);
	fprintf(out, "    \"data\": %u,\n", dataOffset);
	fprintf(out, "    \"strings\": %u,\n", stringsOffset);
	fprintf(out, "    \"mapImage\": %u\n", mapImageOffset);
	fprintf(out, "  },\n");
	fprintf(out, "  \"scriptSize\": %u,\n", scriptSize);
	fprintf(out, "  \"stringsSize\": %u,\n", stringsSize);

	// Palette
	fprintf(out, "  \"palette\": [");
	for (int i = 0; i < 256; i++) {
		if (i > 0)
			fprintf(out, ",");
		if (i % 8 == 0)
			fprintf(out, "\n    ");
		fprintf(out, "[%u,%u,%u]", palette[i * 3], palette[i * 3 + 1], palette[i * 3 + 2]);
	}
	fprintf(out, "\n  ],\n");

	// Shading table
	fprintf(out, "  \"shadingTable\": [");
	for (int i = 0; i < 256; i++) {
		if (i > 0)
			fprintf(out, ",");
		if (i % 32 == 0)
			fprintf(out, "\n    ");
		fprintf(out, "%u", shadingTable[i]);
	}
	fprintf(out, "\n  ],\n");

	fprintf(out, "  \"unknownBytes\": [%u, %u, %u],\n", unknownBytes[0], unknownBytes[1], unknownBytes[2]);

	// Pathfinding nodes
	fprintf(out, "  \"pathfindingNodes\": [\n");
	for (int i = 0; i < 16; i++) {
		fprintf(out, "    {\"index\": %d, \"x\": %u, \"y\": %u, \"adjacent\": [", i, nodes[i].x, nodes[i].y);
		for (uint16_t j = 0; j < nodes[i].numConnections && j < 4; j++) {
			if (j > 0)
				fprintf(out, ", ");
			fprintf(out, "%u", nodes[i].adj[j]);
		}
		fprintf(out, "]}%s\n", i < 15 ? "," : "");
	}
	fprintf(out, "  ],\n");

	fprintf(out, "  \"numHotspots\": %u,\n", numHotspots);
	fprintf(out, "  \"hotspotColorTable\": [");
	for (int i = 0; i < 16; i++) {
		if (i > 0)
			fprintf(out, ", ");
		fprintf(out, "%u", hotspotColors[i]);
	}
	fprintf(out, "],\n");

	// Resource offsets (special animation file offsets)
	fprintf(out, "  \"resourceOffsets\": [");
	for (int i = 0; i < 32; i++) {
		if (i > 0)
			fprintf(out, ", ");
		fprintf(out, "%u", resourceOffsets[i]);
	}
	fprintf(out, "],\n");

	// Map file references
	fprintf(out, "  \"maps\": {\n");
	fprintf(out, "    \"depth\": \"scene%03d_depth.bin\",\n", sceneIndex);
	fprintf(out, "    \"pathfinding\": \"scene%03d_pathfinding.bin\",\n", sceneIndex);
	fprintf(out, "    \"shadow\": \"scene%03d_shadow.bin\",\n", sceneIndex);
	fprintf(out, "    \"hotspot\": \"scene%03d_hotspot.bin\"\n", sceneIndex);
	fprintf(out, "  }\n");
	fprintf(out, "}\n");
	fclose(out);
	printf("  Wrote %s + 4 map files\n", path);
}

// Read the map scene offsets table (256 uint32 entries) by walking the file layout
static bool readMapSceneOffsets(uint32_t offsets[256]) {
	// File layout: header(0xC) + actorIdx/sceneIdx(4) + sceneTable(0x3000) + palette(0x300) + shading(0x800)
	fseek(resFile, 0xC + 0x4 + 0x3000 + 0x300 + 0x800, SEEK_SET);

	// Skip 33 image resources (each: uint32 length, then if >0: 2+w(2)+h(2)+w*h bytes)
	for (int i = 0; i < 0x21; i++) {
		uint32_t length = readU32(resFile);
		if (length > 0)
			fseek(resFile, length, SEEK_CUR);
	}

	// Skip Font 1: uint32 sizeField + uint16 glyphCount + glyphs (each: 1+2+2+w*h)
	readU32(resFile); // size field
	uint16_t glyphCount = readU16(resFile);
	for (int i = 0; i < glyphCount; i++) {
		fseek(resFile, 1, SEEK_CUR); // ascii
		uint16_t w = readU16(resFile);
		uint16_t h = readU16(resFile);
		fseek(resFile, (uint32_t)w * h, SEEK_CUR);
	}

	// Skip Font 2: same structure
	readU32(resFile);
	glyphCount = readU16(resFile);
	for (int i = 0; i < glyphCount; i++) {
		fseek(resFile, 1, SEEK_CUR);
		uint16_t w = readU16(resFile);
		uint16_t h = readU16(resFile);
		fseek(resFile, (uint32_t)w * h, SEEK_CUR);
	}

	// Now at map scene offsets: 256 x uint32
	for (int i = 0; i < 256; i++)
		offsets[i] = readU32(resFile);
	return true;
}

// Extract help/map panel images
static void extractHelpImages(const char *outDir) {
	uint32_t offsets[256];
	if (!readMapSceneOffsets(offsets))
		return;

	printf("Extracting help/map panel images...\n");
	int count = 0;
	for (int i = 0; i < 256; i++) {
		if (offsets[i] == 0)
			continue;

		uint8_t pixels[320 * 200];
		if (!decodeRLEImage(offsets[i], pixels))
			continue;

		// Palette follows immediately after the image
		uint8_t palette[768];
		fread(palette, 1, 768, resFile);

		char path[512];
		snprintf(path, sizeof(path), "%s/help_%03d.bmp", outDir, i);
		writeBMP(path, pixels, palette);
		count++;
	}
	printf("Extracted %d help/map panel images.\n", count);
}

// Extract music data from scene resource tables
static void extractMusic(const char *outDir) {
	printf("Extracting music...\n");
	int count = 0;
	for (int scene = 1; scene <= 512; scene++) {
		uint32_t dataOffset = getSceneDataOffset(scene);
		if (dataOffset == 0)
			continue;

		fseek(resFile, dataOffset, SEEK_SET);
		uint32_t resourceTable[32];
		for (int i = 0; i < 32; i++)
			resourceTable[i] = readU32(resFile);

		for (int i = 0; i < 32; i++) {
			if (resourceTable[i] == 0)
				continue;
			fseek(resFile, resourceTable[i], SEEK_SET);
			uint32_t size = readU32(resFile);
			if (size == 0 || size > 0x100000)
				continue;

			std::vector<uint8_t> data(size);
			fread(data.data(), 1, size, resFile);

			char path[512];
			snprintf(path, sizeof(path), "%s/music_scene%03d_%02d.bin", outDir, scene, i + 1);
			FILE *out = fopen(path, "wb");
			if (out) {
				fwrite(data.data(), 1, size, out);
				fclose(out);
				printf("  Wrote %s (%u bytes)\n", path, size);
				count++;
			}
		}
	}
	printf("Extracted %d music/sound resources.\n", count);
}

// Extract inventory icon images for all objects that have one (blob slot 0x13)
static void extractItems(const char *outDir) {
	// Get scene 1 palette as fallback for icon rendering
	uint8_t palette[768] = {};
	uint32_t bgOffset = getSceneBgOffset(1);
	if (bgOffset != 0) {
		fseek(resFile, bgOffset, SEEK_SET);
		// Skip background RLE image to reach palette
		for (int y = 0; y < 200; y++) {
			uint16_t length = readU16(resFile);
			fseek(resFile, length, SEEK_CUR);
		}
		fread(palette, 1, 768, resFile);
	}

	printf("Extracting inventory items...\n");
	int count = 0;
	for (int i = 1; i <= 0x200; i++) {
		uint32_t addressOffset = 0x17F4 + 0xC + 0x04 + i * 0xC;
		fseek(resFile, addressOffset, SEEK_SET);
		uint32_t objectOffset = readU32(resFile);
		if (objectOffset == 0)
			continue;

		fseek(resFile, objectOffset, SEEK_SET);
		// Skip: x(2), y(2), sceneIndex(2), orientation(2), verticalOffsetScale(2)
		fseek(resFile, 10, SEEK_CUR);

		// Read through 21 blob slots to reach slot 0x13 (index 19, 0-based)
		std::vector<uint8_t> iconBlob;
		for (int j = 0; j < 0x15; j++) {
			fseek(resFile, 2, SEEK_CUR); // animID
			fseek(resFile, 2, SEEK_CUR); // sourceKey
			uint32_t dataSize = readU32(resFile);
			if (j == 0x13) {
				if (dataSize > 0) {
					iconBlob.resize(dataSize);
					fread(iconBlob.data(), 1, dataSize, resFile);
				}
			} else {
				fseek(resFile, dataSize, SEEK_CUR);
			}
			fseek(resFile, 4, SEEK_CUR); // speed(2) + mirrorFlag(1) + padding(1)
		}

		if (iconBlob.empty())
			continue;

		// Parse the animation blob to get frame 1 data
		// Blob header: 12 bytes (6 words), then sequence table
		// Sequence length stored as (len-1) at offset 10
		if (iconBlob.size() < 12)
			continue;
		uint16_t seqLen = (uint16_t)(iconBlob[10] | (iconBlob[11] << 8)) + 1;
		uint32_t frameTableOffset = 0xB + seqLen; // matches engine: stream.seek(0xB + bp0E)
		if (frameTableOffset + 2 > iconBlob.size())
			continue;
		uint16_t frameCount = iconBlob[frameTableOffset] | (iconBlob[frameTableOffset + 1] << 8);
		if (frameCount == 0)
			continue;
		uint32_t frameDataOffset = frameTableOffset + 2;
		// Frame data: offsetX(2), offsetY(2), unknown(2), width(2), height(2), pixels(w*h)
		if (frameDataOffset + 10 > iconBlob.size())
			continue;
		// Skip frame header (6 bytes: offsetX, offsetY, unknown) to get width/height/pixels
		uint32_t p = frameDataOffset + 6;
		if (p + 4 > iconBlob.size())
			continue;
		uint16_t w = iconBlob[p] | (iconBlob[p + 1] << 8);
		uint16_t h = iconBlob[p + 2] | (iconBlob[p + 3] << 8);
		p += 4;
		if (w == 0 || h == 0 || p + (uint32_t)w * h > iconBlob.size())
			continue;

		// Write BMP
		char path[512];
		snprintf(path, sizeof(path), "%s/item_%03d.bmp", outDir, i);
		FILE *f = fopen(path, "wb");
		if (!f)
			continue;

		uint32_t rowStride = (w + 3) & ~3;
		uint32_t imageSize = rowStride * h;
		uint32_t paletteSize = 256 * 4;
		uint32_t dataOff = 14 + 40 + paletteSize;
		uint32_t fileSize = dataOff + imageSize;

		fputc('B', f);
		fputc('M', f);
		uint8_t hdr[12] = {};
		hdr[0] = fileSize & 0xFF;
		hdr[1] = (fileSize >> 8) & 0xFF;
		hdr[2] = (fileSize >> 16) & 0xFF;
		hdr[3] = (fileSize >> 24) & 0xFF;
		hdr[8] = dataOff & 0xFF;
		hdr[9] = (dataOff >> 8) & 0xFF;
		hdr[10] = (dataOff >> 16) & 0xFF;
		hdr[11] = (dataOff >> 24) & 0xFF;
		fwrite(hdr, 1, 12, f);

		uint8_t dib[40] = {};
		dib[0] = 40;
		dib[4] = w & 0xFF;
		dib[5] = (w >> 8) & 0xFF;
		dib[8] = h & 0xFF;
		dib[9] = (h >> 8) & 0xFF;
		dib[12] = 1;
		dib[14] = 8;
		fwrite(dib, 1, 40, f);

		for (int c = 0; c < 256; c++) {
			uint8_t r = (palette[c * 3 + 0] * 259 + 33) >> 6;
			uint8_t g = (palette[c * 3 + 1] * 259 + 33) >> 6;
			uint8_t b = (palette[c * 3 + 2] * 259 + 33) >> 6;
			uint8_t bgra[4] = {b, g, r, 0};
			fwrite(bgra, 1, 4, f);
		}

		// BMP rows must be 4-byte aligned
		uint8_t pad[4] = {0};
		for (int y = h - 1; y >= 0; y--) {
			fwrite(iconBlob.data() + p + y * w, 1, w, f);
			if (rowStride > w)
				fwrite(pad, 1, rowStride - w, f);
		}
		fclose(f);
		printf("  item %d: %ux%u -> %s\n", i, w, h, path);
		count++;
	}
	printf("Extracted %d inventory item icons.\n", count);
}

// ============================================================================
// Amiga PP20 (PowerPacker) decompression
// ============================================================================

static bool pp20Decompress(const uint8_t *src, uint32_t srcLen, uint8_t *dst, uint32_t dstLen) {
	if (srcLen < 12 || memcmp(src, "PP20", 4) != 0)
		return false;

	const uint8_t *eff = src + 4; // efficiency table (4 bytes)
	const uint8_t *packed = src + 8;
	uint32_t packedLen = srcLen - 12; // exclude magic(4) + eff(4) + trailer(4)

	// Trailer: last 4 bytes = decrunch info
	const uint8_t *trailer = src + srcLen - 4;
	uint32_t origSize = ((uint32_t)trailer[0] << 16) | ((uint32_t)trailer[1] << 8) | trailer[2];
	uint8_t bitrot = trailer[3];

	if (origSize != dstLen || origSize == 0)
		return false;

	// Build bit-reverse table
	uint8_t rev[256];
	for (int a = 0; a < 256; a++) {
		uint8_t b = (uint8_t)a;
		b = (uint8_t)(((b & 0x0f) << 4) | ((b >> 4) & 0x0f));
		b = (uint8_t)(((b & 0x33) << 2) | ((b >> 2) & 0x33));
		b = (uint8_t)(((b & 0x55) << 1) | ((b >> 1) & 0x55));
		rev[a] = b;
	}

	uint32_t outPos = dstLen;
	int32_t inPos = (int32_t)packedLen;
	uint32_t code = 0;
	uint32_t shift = 32;

	// Lambda-like macros for bit reading
#define PP_PEEK(x) \
	while (shift > 32u - (x)) { \
		if (inPos <= 0) goto pp_done; \
		shift -= 8; \
		inPos--; \
		code += (uint32_t)rev[packed[inPos]] << shift; \
	}

#define PP_SHIFT(x) do { shift += (x); code = (code << (x)) & 0xFFFFFFFF; } while(0)

	PP_PEEK(bitrot);
	PP_SHIFT(bitrot);

	while (outPos > 0) {
		PP_PEEK(1);
		uint32_t bit = code >> 31;
		PP_SHIFT(1);

		if (bit == 0) {
			// Literal run
			PP_PEEK(2);
			uint32_t length = (code >> 30) + 1;
			PP_SHIFT(2);
			if (length == 4) {
				for (;;) {
					PP_PEEK(2);
					uint32_t b2 = code >> 30;
					PP_SHIFT(2);
					length += b2;
					if (b2 != 3) break;
				}
			}
			for (uint32_t i = 0; i < length && outPos > 0; i++) {
				PP_PEEK(8);
				outPos--;
				dst[outPos] = (uint8_t)(code >> 24);
				PP_SHIFT(8);
			}
			if (outPos == 0) break;
		}

		// LZ match
		PP_PEEK(2);
		uint32_t cv = code >> 30;
		PP_SHIFT(2);

		uint32_t length, nbits;
		if (cv == 0) { length = 2; nbits = eff[0]; }
		else if (cv == 1) { length = 3; nbits = eff[1]; }
		else if (cv == 2) { length = 4; nbits = eff[2]; }
		else {
			PP_PEEK(1);
			uint32_t extra = code >> 31;
			PP_SHIFT(1);
			length = 5;
			nbits = (extra == 0) ? 7 : eff[3];
		}

		PP_PEEK(nbits);
		uint32_t ptr = (code >> (32 - nbits)) + 1;
		PP_SHIFT(nbits);

		if (length == 5) {
			for (;;) {
				PP_PEEK(3);
				uint32_t b3 = code >> 29;
				PP_SHIFT(3);
				length += b3;
				if (b3 != 7) break;
			}
		}

		for (uint32_t i = 0; i < length && outPos > 0; i++) {
			outPos--;
			dst[outPos] = dst[outPos + ptr];
		}
	}

pp_done:
#undef PP_PEEK
#undef PP_SHIFT
	return outPos == 0;
}

// Get decompressed size from PP20 trailer
static uint32_t pp20GetOrigSize(const uint8_t *data, uint32_t len) {
	if (len < 12) return 0;
	const uint8_t *t = data + len - 4;
	return ((uint32_t)t[0] << 16) | ((uint32_t)t[1] << 8) | t[2];
}

// ============================================================================
// Amiga DataA (MXMF) + Mdir (MXDR) extraction
// ============================================================================

static uint32_t readU32BE(const uint8_t *p) {
	return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | p[3];
}

static uint16_t readU16BE(const uint8_t *p) {
	return ((uint16_t)p[0] << 8) | p[1];
}

static bool isAmigaDataDir(const char *path) {
	// Check if path is a directory containing DataA and Mdir
	std::string dataA = std::string(path) + "/DataA";
	std::string mdir = std::string(path) + "/Mdir";
	struct stat st;
	return (stat(dataA.c_str(), &st) == 0 && S_ISREG(st.st_mode) &&
	        stat(mdir.c_str(), &st) == 0 && S_ISREG(st.st_mode));
}

static int extractAmiga(const char *gameDir, const char *outDir) {
	std::string dataAPath = std::string(gameDir) + "/DataA";
	std::string mdirPath = std::string(gameDir) + "/Mdir";

	// Read Mdir
	FILE *mf = fopen(mdirPath.c_str(), "rb");
	if (!mf) {
		fprintf(stderr, "Error: Cannot open '%s'\n", mdirPath.c_str());
		return 1;
	}
	fseek(mf, 0, SEEK_END);
	uint32_t mdirSize = (uint32_t)ftell(mf);
	fseek(mf, 0, SEEK_SET);
	std::vector<uint8_t> mdirData(mdirSize);
	fread(mdirData.data(), 1, mdirSize, mf);
	fclose(mf);

	if (mdirSize < 18 || memcmp(mdirData.data(), "MXDR", 4) != 0) {
		fprintf(stderr, "Error: '%s' is not a valid MXDR file\n", mdirPath.c_str());
		return 1;
	}

	uint16_t dirEntryCount = (uint16_t)((mdirSize - 18) / 10);
	printf("Mdir: %u directory entries\n", dirEntryCount);

	// Parse directory entries (offset 18, each 10 bytes: type(2) + id(2) + disk(2) + offset(4))
	struct DirEntry {
		char type[3];
		uint16_t id;
		uint16_t disk;
		uint32_t offset;
	};
	std::vector<DirEntry> entries(dirEntryCount);
	for (uint16_t i = 0; i < dirEntryCount; i++) {
		const uint8_t *p = mdirData.data() + 18 + i * 10;
		entries[i].type[0] = (char)p[0];
		entries[i].type[1] = (char)p[1];
		entries[i].type[2] = '\0';
		entries[i].id = readU16BE(p + 2);
		entries[i].disk = readU16BE(p + 4);
		entries[i].offset = readU32BE(p + 6);
	}

	// Open DataA
	FILE *df = fopen(dataAPath.c_str(), "rb");
	if (!df) {
		fprintf(stderr, "Error: Cannot open '%s'\n", dataAPath.c_str());
		return 1;
	}
	fseek(df, 0, SEEK_END);
	uint32_t dataASize = (uint32_t)ftell(df);
	fseek(df, 0, SEEK_SET);

	// Read and validate MXMF header (14 bytes)
	uint8_t mxmfHdr[14];
	fread(mxmfHdr, 1, 14, df);
	if (memcmp(mxmfHdr, "MXMF", 4) != 0) {
		fprintf(stderr, "Error: '%s' is not a valid MXMF file\n", dataAPath.c_str());
		fclose(df);
		return 1;
	}
	uint16_t totalResources = readU16BE(mxmfHdr + 8);
	uint32_t firstBlockSize = readU32BE(mxmfHdr + 10);
	printf("DataA: %u resources, file size=%u\n", totalResources, dataASize);

	mkdirp(outDir);

	// Extract first PP20 block (scene table) at offset 14
	{
		std::vector<uint8_t> pp(firstBlockSize);
		fseek(df, 14, SEEK_SET);
		fread(pp.data(), 1, firstBlockSize, df);

		uint32_t origSize = pp20GetOrigSize(pp.data(), firstBlockSize);
		if (origSize > 0 && memcmp(pp.data(), "PP20", 4) == 0) {
			std::vector<uint8_t> dec(origSize);
			if (pp20Decompress(pp.data(), firstBlockSize, dec.data(), origSize)) {
				char path[512];
				snprintf(path, sizeof(path), "%s/scene_table.bin", outDir);
				writeRawFile(path, dec.data(), origSize);
				printf("  scene_table.bin: %u -> %u bytes\n", firstBlockSize, origSize);
			} else {
				fprintf(stderr, "  WARN: scene_table PP20 decompression failed\n");
			}
		}
	}

	// Default 32-color Amiga palette (grayscale fallback for sprites without scene palette)
	uint8_t defaultPalette[768];
	for (int i = 0; i < 256; i++) {
		uint8_t v = (uint8_t)((i < 32) ? (i * 255 / 31) : 0);
		defaultPalette[i * 3 + 0] = v;
		defaultPalette[i * 3 + 1] = v;
		defaultPalette[i * 3 + 2] = v;
	}

	// Extract all resources sequentially from offset 14 + firstBlockSize
	fseek(df, 14 + firstBlockSize, SEEK_SET);
	int extracted = 1; // counting scene_table
	int images = 0;
	for (uint16_t i = 0; i < totalResources - 1; i++) {
		long entryStart = ftell(df);
		uint8_t ehdr[8];
		if (fread(ehdr, 1, 8, df) != 8)
			break;

		char eType[3] = {(char)ehdr[0], (char)ehdr[1], '\0'};
		uint16_t eId = readU16BE(ehdr + 2);
		uint32_t eCompSize = readU32BE(ehdr + 4);

		if (eCompSize == 0 || eCompSize > dataASize) {
			fprintf(stderr, "  WARN: bad entry at offset 0x%lx, stopping\n", entryStart);
			break;
		}

		std::vector<uint8_t> payload(eCompSize);
		if (fread(payload.data(), 1, eCompSize, df) != eCompSize)
			break;

		char fname[64];
		char path[512];

		if (memcmp(payload.data(), "PP20", 4) == 0) {
			// Direct PP20 block
			uint32_t origSize = pp20GetOrigSize(payload.data(), eCompSize);
			if (origSize > 0) {
				std::vector<uint8_t> dec(origSize);
				if (pp20Decompress(payload.data(), eCompSize, dec.data(), origSize)) {
					snprintf(fname, sizeof(fname), "%s_%04d.bin", eType, eId);
					snprintf(path, sizeof(path), "%s/%s", outDir, fname);
					writeRawFile(path, dec.data(), origSize);
					printf("  %s: %u -> %u bytes\n", fname, eCompSize, origSize);

					// Try to extract sprite image from MXOO
					if (origSize >= 32 && memcmp(dec.data(), "MXOO", 4) == 0) {
						uint32_t scriptOff = readU32BE(dec.data() + 4);
						// Sprite body: from offset 12 to scriptOff
						if (scriptOff > 32 && scriptOff <= origSize) {
							const uint8_t *body = dec.data() + 12;
							uint32_t bodyLen = scriptOff - 12;
							// Check sprite signature: body[10:12]=0x0101, body[14:16]>0
							if (bodyLen >= 20 && body[10] == 1 && body[11] == 1) {
								uint16_t frames = readU16BE(body + 14);
								uint16_t w = readU16BE(body + 16);
								uint16_t h = readU16BE(body + 18);
								uint32_t rowBytes = (w + 7) / 8;
								uint32_t planeSize = rowBytes * h;
								uint32_t pixelBytes = bodyLen - 20;
								uint32_t numPlanes = (planeSize > 0) ? pixelBytes / planeSize : 0;
								if (w > 0 && w <= 320 && h > 0 && h <= 200 &&
								    frames > 0 && numPlanes >= 5 && numPlanes <= 6 &&
								    pixelBytes == planeSize * numPlanes) {
									// Convert planar to chunky (5 color planes)
									std::vector<uint8_t> pixels(w * h, 0);
									const uint8_t *pixData = body + 20;
									for (uint32_t plane = 0; plane < 5 && plane < numPlanes; plane++) {
										uint32_t pOff = plane * planeSize;
										for (uint16_t y = 0; y < h; y++) {
											for (uint32_t bx = 0; bx < rowBytes; bx++) {
												uint8_t b = pixData[pOff + y * rowBytes + bx];
												for (int bit = 0; bit < 8; bit++) {
													uint16_t x = (uint16_t)(bx * 8 + bit);
													if (x < w && (b & (0x80 >> bit)))
														pixels[y * w + x] |= (1 << plane);
												}
											}
										}
									}
									snprintf(fname, sizeof(fname), "%s_%04d.bmp", eType, eId);
									snprintf(path, sizeof(path), "%s/%s", outDir, fname);
									writeBMPEx(path, pixels.data(), w, h, defaultPalette);
									images++;
								}
							}
						}
					}
				} else {
					snprintf(fname, sizeof(fname), "%s_%04d.pp20", eType, eId);
					snprintf(path, sizeof(path), "%s/%s", outDir, fname);
					writeRawFile(path, payload.data(), eCompSize);
					printf("  %s: %u bytes (decompress failed)\n", fname, eCompSize);
				}
			}
		} else if (memcmp(payload.data(), "MXMM", 4) == 0 && eCompSize > 14) {
			// Music sub-container: PP20 size at offset 10
			uint32_t ppSize = readU32BE(payload.data() + 10);
			if (ppSize > 0 && ppSize <= eCompSize - 14 && memcmp(payload.data() + 14, "PP20", 4) == 0) {
				uint32_t origSize = pp20GetOrigSize(payload.data() + 14, ppSize);
				if (origSize > 0) {
					std::vector<uint8_t> dec(origSize);
					if (pp20Decompress(payload.data() + 14, ppSize, dec.data(), origSize)) {
						snprintf(fname, sizeof(fname), "%s_%04d.bin", eType, eId);
						snprintf(path, sizeof(path), "%s/%s", outDir, fname);
						writeRawFile(path, dec.data(), origSize);
						printf("  %s (music): %u -> %u bytes\n", fname, ppSize, origSize);
					} else {
						snprintf(fname, sizeof(fname), "%s_%04d.mxmm", eType, eId);
						snprintf(path, sizeof(path), "%s/%s", outDir, fname);
						writeRawFile(path, payload.data(), eCompSize);
						printf("  %s: %u bytes (music decompress failed)\n", fname, eCompSize);
					}
				}
			} else {
				snprintf(fname, sizeof(fname), "%s_%04d.mxmm", eType, eId);
				snprintf(path, sizeof(path), "%s/%s", outDir, fname);
				writeRawFile(path, payload.data(), eCompSize);
				printf("  %s: %u bytes (raw music container)\n", fname, eCompSize);
			}
		} else if (memcmp(payload.data(), "MXOO", 4) == 0) {
			// Uncompressed object sub-container
			snprintf(fname, sizeof(fname), "%s_%04d.mxoo", eType, eId);
			snprintf(path, sizeof(path), "%s/%s", outDir, fname);
			writeRawFile(path, payload.data(), eCompSize);
			printf("  %s: %u bytes (uncompressed object)\n", fname, eCompSize);
		} else {
			// Unknown format, save raw
			snprintf(fname, sizeof(fname), "%s_%04d.raw", eType, eId);
			snprintf(path, sizeof(path), "%s/%s", outDir, fname);
			writeRawFile(path, payload.data(), eCompSize);
			printf("  %s: %u bytes (unknown)\n", fname, eCompSize);
		}
		extracted++;
	}

	fclose(df);
	printf("\nExtracted %d resources (%d sprite images) to %s/\n", extracted, images, outDir);
	return 0;
}

static void printHelp(const char *bin) {
	printf("MACS2 Resource Extractor\n\n");
	printf("Usage: %s <mode> <game_data_path> <output_dir> [scene_index]\n\n", bin);
	printf("Modes:\n");
	printf("  images     - Extract background images as BMP files\n");
	printf("  sounds     - Extract sound/music resource blobs\n");
	printf("  music      - Extract music resources from scene data\n");
	printf("  strings    - Extract and decrypt text strings\n");
	printf("  scenedata  - Extract scene metadata as JSON (pathfinding, hotspots, walk params)\n");
	printf("  items      - Extract inventory item icons as BMP (with object ID)\n");
	printf("  helpimages - Extract help/map panel images as BMP\n");
	printf("  all        - Extract everything\n");
	printf("\n");
	printf("The game_data_path can be:\n");
	printf("  - RESOURCE.MCS file (DOS version)\n");
	printf("  - Directory containing RESOURCE.MCS (DOS version)\n");
	printf("  - Directory containing DataA + Mdir (Amiga version)\n");
	printf("\n");
	printf("Amiga mode auto-detects and extracts all PP20-compressed resources.\n");
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

	// Check if this is an Amiga data directory (contains DataA + Mdir)
	struct stat st;
	if (stat(resPath, &st) == 0 && S_ISDIR(st.st_mode) && isAmigaDataDir(resPath)) {
		printf("Detected Amiga data directory.\n");
		return extractAmiga(resPath, outDir);
	}

	// If path is a directory, look for RESOURCE.MCS inside it
	std::string resolvedPath = resPath;
	if (stat(resPath, &st) == 0 && S_ISDIR(st.st_mode)) {
		resolvedPath = std::string(resPath) + "/RESOURCE.MCS";
	}

	resFile = fopen(resolvedPath.c_str(), "rb");
	if (!resFile) {
		fprintf(stderr, "Error: Cannot open '%s'\n", resolvedPath.c_str());
		return 1;
	}

	// Validate the file header
	uint8_t magic[12];
	if (fread(magic, 1, 12, resFile) != 12 || memcmp(magic, "AHFFMACS0100", 12) != 0) {
		fprintf(stderr, "Error: '%s' is not a valid MACS2 resource file\n", resolvedPath.c_str());
		fclose(resFile);
		return 1;
	}
	fseek(resFile, 0, SEEK_SET);

	mkdirp(outDir);

	int startScene = 1, endScene = 512;
	if (argc >= 5) {
		startScene = endScene = atoi(argv[4]);
	}

	bool doImages = !strcmp(mode, "images") || !strcmp(mode, "all");
	bool doSounds = !strcmp(mode, "sounds") || !strcmp(mode, "all");
	bool doMusic = !strcmp(mode, "music") || !strcmp(mode, "all");
	bool doStrings = !strcmp(mode, "strings") || !strcmp(mode, "all");
	bool doSceneData = !strcmp(mode, "scenedata") || !strcmp(mode, "all");
	bool doItems = !strcmp(mode, "items") || !strcmp(mode, "all");
	bool doHelpImages = !strcmp(mode, "helpimages") || !strcmp(mode, "all");

	if (doItems) {
		extractItems(outDir);
	}

	if (doHelpImages) {
		extractHelpImages(outDir);
	}

	if (doMusic) {
		extractMusic(outDir);
	}

	if (doImages) {
		extractCursors(outDir);
		extractFonts(outDir);
	}

	for (int scene = startScene; scene <= endScene; scene++) {
		bool hasData = false;

		if (doImages) {
			uint32_t bgOff = getSceneBgOffset(scene);
			if (bgOff != 0) {
				if (!hasData) {
					printf("Scene %d:\n", scene);
					hasData = true;
				}
				extractImage(scene, outDir);
			}
		}

		if (doSounds) {
			uint32_t dataOff = getSceneDataOffset(scene);
			if (dataOff != 0) {
				if (!hasData) {
					printf("Scene %d:\n", scene);
					hasData = true;
				}
				extractResources(scene, outDir, "res");
			}
		}

		if (doStrings) {
			uint32_t strOff = getSceneStringsOffset(scene);
			if (strOff != 0) {
				if (!hasData) {
					printf("Scene %d:\n", scene);
					hasData = true;
				}
				extractStrings(scene, outDir);
			}
		}

		if (doSceneData) {
			uint32_t bgOff = getSceneBgOffset(scene);
			if (bgOff != 0) {
				if (!hasData) {
					printf("Scene %d:\n", scene);
					hasData = true;
				}
				extractSceneData(scene, outDir);
			}
		}
	}

	if (doStrings) {
		for (int obj = 1; obj <= 0x200; obj++) {
			extractObjectStrings(obj, outDir);
		}
	}

	fclose(resFile);
	return 0;
}
