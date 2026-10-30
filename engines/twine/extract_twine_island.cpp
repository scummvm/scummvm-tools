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

#include "engines/twine/extract_twine_convert.h"

#include "common/endian.h"
#include "common/file.h"
#include "engines/twine/hqr.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_PNG
#include <png.h>
#endif

namespace {

static const int kMapSize = 16;
static const int kNbCote = 64;
static const int kCubeWorldSize = 32768;
static const int kVertexStep = 512;
static const int kIleMapSize = 256;
static const int kIleTextureSize = 256 * 256;

static const int kIleEntryMap = 0;
static const int kIleEntryTexGround = 1;
static const int kIleEntryTexObj = 2;
static const int kIleCubeStart = 3;
static const int kIleCubeStep = 6;
static const int kIleCubeInf = 0;
static const int kIleCubeDob = 1;
static const int kIleCubeGrd = 2;
static const int kIleCubeY = 4;

static const int kInfoNbDecors = 2;
static const int kDecorRecordSize = 48;
static const int kDecInvisible = 1 << 16;

static const int kCodeJeuWater = 1;

struct HalfPoly {
	uint32 raw;

	explicit HalfPoly(uint32 value) : raw(value) {}

	bool visible() const {
		const uint8 polyFlag = (raw >> 6) & 3;
		const uint8 texFlag = (raw >> 4) & 3;
		return polyFlag + texFlag != 0;
	}

	bool sens() const {
		return ((raw >> 13) & 1) != 0;
	}

	uint8 codeJeu() const {
		return (raw >> 12) & 0xF;
	}
};

struct IslandDecor {
	int32 body;
	int32 xworld;
	int32 yworld;
	int32 zworld;
	int32 beta;
};

struct BodyCacheEntry {
	uint8 *data;
	int32 size;
};

static bool loadEntry(const Common::Filename &file, int32 index, uint8 **data, int32 *size) {
	*data = nullptr;
	*size = 0;
	const int32 entrySize = TwinE::HQR::getAllocEntry(data, file, index);
	if (entrySize <= 0 || !*data)
		return false;
	*size = entrySize;
	return true;
}

static int16 readHeight(const uint8 *heights, int dx, int dz) {
	return (int16)READ_LE_UINT16(heights + (dz * (kNbCote + 1) + dx) * 2);
}

static void writeVertex(FILE *obj, float x, float y, float z) {
	fprintf(obj, "v %g %g %g\n", x, y, z);
}

static void writeFace(FILE *obj, int i0, int i1, int i2) {
	fprintf(obj, "f %d %d %d\n", i0, i1, i2);
}

static bool parseDecorRecord(const uint8 *data, int32 size, int offset, IslandDecor &decor) {
	if (offset + kDecorRecordSize > size)
		return false;

	decor.body = (int32)READ_LE_UINT32(data + offset);
	decor.xworld = (int32)READ_LE_UINT32(data + offset + 4);
	decor.yworld = (int32)READ_LE_UINT32(data + offset + 8);
	decor.zworld = (int32)READ_LE_UINT32(data + offset + 12);
	decor.beta = (int32)READ_LE_UINT32(data + offset + 20);
	return true;
}

static Common::Filename oblPathFromIle(const Common::Filename &ileFile) {
	Common::Filename obl = ileFile;
	obl.setExtension(".OBL");
	return obl;
}

static bool loadCachedBody(BodyCacheEntry *cache, int32 cacheSize, const Common::Filename &oblFile,
		int32 bodyIndex, const uint8 **data, int32 *size) {
	if (bodyIndex < 0 || bodyIndex >= cacheSize)
		return false;

	BodyCacheEntry &entry = cache[bodyIndex];
	if (!entry.data) {
		const int32 entrySize = TwinE::HQR::getAllocEntry(&entry.data, oblFile, bodyIndex);
		if (entrySize <= 0 || !entry.data)
			return false;
		entry.size = entrySize;
	}

	*data = entry.data;
	*size = entry.size;
	return true;
}

static void freeBodyCache(BodyCacheEntry *cache, int32 cacheSize) {
	if (!cache)
		return;
	for (int32 i = 0; i < cacheSize; ++i)
		free(cache[i].data);
	free(cache);
}

static bool exportCubeTerrain(FILE *obj, int &vertexBase, int mapX, int mapY,
		const uint8 *ground, int32 groundSize, const uint8 *heights, int32 heightSize) {
	if (groundSize < kNbCote * kNbCote * 2 * (int)sizeof(uint32))
		return false;
	if (heightSize < (kNbCote + 1) * (kNbCote + 1) * 2)
		return false;

	const float originX = mapX * (kCubeWorldSize / (float)kVertexStep);
	const float originZ = mapY * (kCubeWorldSize / (float)kVertexStep);
	const float scale = 1.0f / kVertexStep;

	for (int dz = 0; dz < kNbCote; ++dz) {
		for (int dx = 0; dx < kNbCote; ++dx) {
			const int polyIndex = (dz * kNbCote + dx) * 2;
			HalfPoly poly0(READ_LE_UINT32(ground + polyIndex * sizeof(uint32)));
			HalfPoly poly1(READ_LE_UINT32(ground + (polyIndex + 1) * sizeof(uint32)));

			if (poly0.codeJeu() == kCodeJeuWater && poly1.codeJeu() == kCodeJeuWater)
				continue;
			if (!poly0.visible() && !poly1.visible())
				continue;

			const float x0 = originX + dx * scale;
			const float x1 = originX + (dx + 1) * scale;
			const float z0 = originZ + dz * scale;
			const float z2 = originZ + (dz + 1) * scale;

			const float y00 = readHeight(heights, dx, dz) / (float)kVertexStep;
			const float y01 = readHeight(heights, dx, dz + 1) / (float)kVertexStep;
			const float y11 = readHeight(heights, dx + 1, dz + 1) / (float)kVertexStep;
			const float y10 = readHeight(heights, dx + 1, dz) / (float)kVertexStep;

			const int v0 = vertexBase + 1;
			writeVertex(obj, x0, y00, z0);
			const int v1 = vertexBase + 2;
			writeVertex(obj, x0, y01, z2);
			const int v2 = vertexBase + 3;
			writeVertex(obj, x1, y11, z2);
			const int v3 = vertexBase + 4;
			writeVertex(obj, x1, y10, z0);
			vertexBase += 4;

			if (poly0.visible()) {
				if (!poly0.sens())
					writeFace(obj, v0, v1, v2);
				else
					writeFace(obj, v3, v0, v1);
			}

			if (poly1.visible()) {
				if (!poly1.sens())
					writeFace(obj, v2, v3, v0);
				else
					writeFace(obj, v1, v2, v3);
			}
		}
	}

	return true;
}

static int exportCubeDecors(FILE *obj, FILE *mtl, int &vertexBase, int &materialId,
		const Common::Filename &ileFile, const Common::Filename &oblFile,
		BodyCacheEntry *bodyCache, int32 bodyCacheSize, int32 baseEntry,
		int decorIndexStart, const TwineConvertContext &ctx, const TwinePalette &palette,
		bool materialsWritten[256]) {
	uint8 *info = nullptr;
	uint8 *dob = nullptr;
	int32 infoSize = 0;
	int32 dobSize = 0;
	int exported = 0;

	if (!loadEntry(ileFile, baseEntry + kIleCubeInf, &info, &infoSize) ||
		infoSize < (kInfoNbDecors + 1) * (int)sizeof(int32)) {
		free(info);
		return 0;
	}

	const int32 nbDecors = (int32)READ_LE_UINT32(info + kInfoNbDecors * sizeof(int32));
	free(info);

	if (nbDecors <= 0)
		return 0;

	if (!loadEntry(ileFile, baseEntry + kIleCubeDob, &dob, &dobSize) ||
		dobSize < nbDecors * kDecorRecordSize) {
		free(dob);
		return 0;
	}

	for (int i = 0; i < nbDecors; ++i) {
		IslandDecor decor;
		if (!parseDecorRecord(dob, dobSize, i * kDecorRecordSize, decor))
			continue;

		const int32 bodyIndex = decor.body & 0xFFFF;
		if (bodyIndex == 0 || (decor.body & kDecInvisible))
			continue;

		const uint8 *bodyData = nullptr;
		int32 bodySize = 0;
		if (!loadCachedBody(bodyCache, bodyCacheSize, oblFile, bodyIndex, &bodyData, &bodySize))
			continue;

		char objectName[64];
		snprintf(objectName, sizeof(objectName), "decor_%d", decorIndexStart + exported);

		TwineBodyTransform xform;
		xform.posX = decor.xworld;
		xform.posY = decor.yworld;
		xform.posZ = decor.zworld;
		xform.beta = decor.beta & 0xFFFF;

		if (twineAppendBodyToObj(bodyData, bodySize, ctx.preferLba1, palette, obj, mtl,
				vertexBase, materialId, xform, objectName, materialsWritten))
			exported++;
	}

	free(dob);
	return exported;
}

#ifdef USE_PNG
static bool writeIndexedTexturePng(const char *outPath, const uint8 *data, const TwinePalette &palette) {
	if (!palette.loaded) {
		fprintf(stderr, "No palette available for %s\n", outPath);
		return false;
	}

	FILE *fout = fopen(outPath, "wb");
	if (!fout)
		return false;

	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
	if (!png) {
		fclose(fout);
		return false;
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, nullptr);
		fclose(fout);
		return false;
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	png_init_io(png, fout);
	png_set_IHDR(png, info, 256, 256, 8, PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

	png_color pngPalette[256];
	for (int i = 0; i < 256; ++i) {
		pngPalette[i].red = palette.data[i * 3 + 0];
		pngPalette[i].green = palette.data[i * 3 + 1];
		pngPalette[i].blue = palette.data[i * 3 + 2];
	}
	png_set_PLTE(png, info, pngPalette, 256);

	png_byte trans = 0;
	png_set_tRNS(png, info, &trans, 1, nullptr);
	png_write_info(png, info);

	png_bytep *rowPointers = (png_bytep *)malloc(sizeof(png_bytep) * 256);
	if (!rowPointers) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	for (int y = 0; y < 256; ++y)
		rowPointers[y] = (png_bytep)(const_cast<uint8 *>(data) + y * 256);

	png_write_image(png, rowPointers);
	png_write_end(png, nullptr);
	free(rowPointers);
	png_destroy_write_struct(&png, &info);
	fclose(fout);
	return true;
}
#endif

} // namespace

bool twineIsIleFilename(const Common::Filename &filename) {
	return scumm_stricmp(filename.getExtension().c_str(), "ile") == 0;
}

bool twineConvertIsland(const Common::Filename &ileFile, const char *outDir, const TwineConvertContext &ctx) {
	const int32 numEntries = TwinE::HQR::numEntries(ileFile);
	if (numEntries < kIleCubeStart + kIleCubeStep) {
		fprintf(stderr, "Not enough entries for an island ILE file: %s\n", ileFile.getFullPath().c_str());
		return false;
	}

	uint8 *mapData = nullptr;
	int32 mapSize = 0;
	if (!loadEntry(ileFile, kIleEntryMap, &mapData, &mapSize) || mapSize != kIleMapSize) {
		fprintf(stderr, "Invalid island map entry in %s\n", ileFile.getFullPath().c_str());
		free(mapData);
		return false;
	}

	char basePath[1024];
	snprintf(basePath, sizeof(basePath), "%s/%s", outDir, ileFile.getName().c_str());

	char objPath[1100];
	char mtlPath[1100];
	snprintf(objPath, sizeof(objPath), "%s_island.obj", basePath);
	snprintf(mtlPath, sizeof(mtlPath), "%s_island.mtl", basePath);

	FILE *obj = fopen(objPath, "w");
	if (!obj) {
		fprintf(stderr, "Unable to open %s for writing\n", objPath);
		free(mapData);
		return false;
	}

	FILE *mtl = fopen(mtlPath, "w");
	if (!mtl) {
		fprintf(stderr, "Unable to open %s for writing\n", mtlPath);
		fclose(obj);
		free(mapData);
		return false;
	}

	fprintf(obj, "# LBA island exported from %s\n", ileFile.getFullName().c_str());
	fprintf(obj, "mtllib %s_island.mtl\n", ileFile.getName().c_str());
	fprintf(obj, "o terrain\n");
	fprintf(obj, "usemtl terrain\n");
	fprintf(mtl, "newmtl terrain\n");
	fprintf(mtl, "Kd 0.45 0.50 0.35\n");
	fprintf(mtl, "Ka 0.10 0.10 0.10\n");
	fprintf(mtl, "d 1.0\n\n");

	bool materialsWritten[256];
	memset(materialsWritten, 0, sizeof(materialsWritten));
	materialsWritten[0] = true; // terrain slot unused for decor shades

	int vertexBase = 0;
	int materialId = 0;
	int cubesExported = 0;

	for (int mapY = 0; mapY < kMapSize; ++mapY) {
		for (int mapX = 0; mapX < kMapSize; ++mapX) {
			const uint8 cubeIndex = mapData[mapY * kMapSize + mapX] & 127;
			if (cubeIndex == 0)
				continue;

			const int32 baseEntry = kIleCubeStart + (cubeIndex - 1) * kIleCubeStep;
			if (baseEntry + kIleCubeY >= numEntries)
				continue;

			uint8 *ground = nullptr;
			uint8 *heights = nullptr;
			int32 groundSize = 0;
			int32 heightSize = 0;

			if (!loadEntry(ileFile, baseEntry + kIleCubeGrd, &ground, &groundSize) ||
				!loadEntry(ileFile, baseEntry + kIleCubeY, &heights, &heightSize)) {
				free(ground);
				free(heights);
				continue;
			}

			if (exportCubeTerrain(obj, vertexBase, mapX, mapY, ground, groundSize, heights, heightSize))
				cubesExported++;

			free(ground);
			free(heights);
		}
	}

	if (cubesExported == 0) {
		fprintf(stderr, "No island terrain cubes found in %s\n", ileFile.getFullName().c_str());
		fclose(obj);
		fclose(mtl);
		free(mapData);
		remove(objPath);
		remove(mtlPath);
		return false;
	}

	const Common::Filename oblFile = oblPathFromIle(ileFile);
	BodyCacheEntry *bodyCache = nullptr;
	int32 bodyCacheSize = 0;
	int decorsExported = 0;

	if (oblFile.exists()) {
		bodyCacheSize = TwinE::HQR::numEntries(oblFile);
		if (bodyCacheSize > 0) {
			bodyCache = (BodyCacheEntry *)calloc(bodyCacheSize, sizeof(BodyCacheEntry));
			if (!bodyCache) {
				fprintf(stderr, "Unable to allocate body cache for %s\n", oblFile.getFullPath().c_str());
			} else {
				TwinePalette decorPalette;
				if (!twineResolvePalette(ctx, kIleEntryTexObj, decorPalette))
					fprintf(stderr, "Warning: no palette for island decor materials\n");

				int decorIndex = 0;
				for (int mapY = 0; mapY < kMapSize; ++mapY) {
					for (int mapX = 0; mapX < kMapSize; ++mapX) {
						const uint8 cubeIndex = mapData[mapY * kMapSize + mapX] & 127;
						if (cubeIndex == 0)
							continue;

						const int32 baseEntry = kIleCubeStart + (cubeIndex - 1) * kIleCubeStep;
						if (baseEntry + kIleCubeDob >= numEntries)
							continue;

						const int cubeDecors = exportCubeDecors(obj, mtl, vertexBase, materialId,
							ileFile, oblFile, bodyCache, bodyCacheSize, baseEntry,
							decorIndex, ctx, decorPalette, materialsWritten);
						decorsExported += cubeDecors;
						decorIndex += cubeDecors;
					}
				}
			}
		}
	} else {
		fprintf(stderr, "Warning: no paired OBL file found at %s, exporting terrain only\n",
			oblFile.getFullPath().c_str());
	}

	free(mapData);

	fclose(obj);
	fclose(mtl);
	freeBodyCache(bodyCache, bodyCacheSize);

	uint8 *groundTex = nullptr;
	uint8 *objTex = nullptr;
	int32 groundTexSize = 0;
	int32 objTexSize = 0;
	if (loadEntry(ileFile, kIleEntryTexGround, &groundTex, &groundTexSize) && groundTexSize == kIleTextureSize) {
#ifdef USE_PNG
		char pngPath[1100];
		snprintf(pngPath, sizeof(pngPath), "%s_ground.png", basePath);
		TwinePalette palette;
		if (twineResolvePalette(ctx, kIleEntryTexGround, palette) &&
			writeIndexedTexturePng(pngPath, groundTex, palette))
			printf("Exported island ground texture -> %s\n", pngPath);
#endif
	}
	if (loadEntry(ileFile, kIleEntryTexObj, &objTex, &objTexSize) && objTexSize == kIleTextureSize) {
#ifdef USE_PNG
		char pngPath[1100];
		snprintf(pngPath, sizeof(pngPath), "%s_objects.png", basePath);
		TwinePalette palette;
		if (twineResolvePalette(ctx, kIleEntryTexObj, palette) &&
			writeIndexedTexturePng(pngPath, objTex, palette))
			printf("Exported island object texture -> %s\n", pngPath);
#endif
	}
	free(groundTex);
	free(objTex);

	printf("Converted island %s -> %s (%d cubes, %d decors, + %s)\n",
		ileFile.getFullName().c_str(), objPath, cubesExported, decorsExported, mtlPath);
	return true;
}
