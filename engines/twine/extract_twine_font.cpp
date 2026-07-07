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
#include "common/memstream.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_PNG
#include <png.h>
#endif

namespace {

static const int kFontAtlasColumns = 16;
static const int kSjisCharWidth = 24;
static const int kSjisCharHeight = 24;
static const int kSjisGlyphCount = 11072;
static const int kSjisFontSize = kSjisGlyphCount * kSjisCharHeight * (kSjisCharWidth / 8);
static const int kMsDosFontSize = 256 * 8;

static bool isValidFontGlyphOffset(uint32 offset, int32 size) {
	if (size < 1028)
		return false;
	const uint32 usize = (uint32)size;
	return offset >= 1024u && offset <= usize - 4u;
}

static bool decodeLbaFontGlyph(const uint8 *data, int32 size, int character, int cellW, int cellH, uint8 *out) {
	memset(out, 0, cellW * cellH);

	const uint32 offset = READ_LE_UINT32(data + character * 4);
	if (!isValidFontGlyphOffset(offset, size))
		return false;

	Common::MemoryReadStream stream(data, size);
	if (!stream.seek(offset))
		return false;

	const uint8 charWidth = stream.readByte();
	const uint8 sizeY = stream.readByte();
	stream.readByte();
	stream.readByte();

	if (charWidth == 0 || sizeY == 0 || charWidth > cellW || sizeY > cellH)
		return false;

	int tempX = 0;
	int tempY = 0;
	for (uint8 fontY = 0; fontY < sizeY; ++fontY) {
		if (stream.pos() >= (uint32)size)
			return false;

		uint8 index = stream.readByte();
		do {
			if (stream.pos() >= (uint32)size)
				return false;

			const uint8 jump = stream.readByte();
			if (--index == 0) {
				tempY++;
				tempX = 0;
				break;
			}

			if (stream.pos() >= (uint32)size)
				return false;

			const uint8 number = stream.readByte();
			tempX += jump;
			for (uint8 i = 0; i < number; i++) {
				if (tempX >= 0 && tempX < charWidth && tempY >= 0 && tempY < sizeY)
					out[tempY * cellW + tempX] = 1;
				tempX++;
			}

			if (--index == 0) {
				tempY++;
				tempX = 0;
				break;
			}
		} while (true);
	}

	return true;
}

static bool measureLbaFontCell(const uint8 *data, int32 size, int &cellW, int &cellH) {
	cellW = 0;
	cellH = 0;

	for (int i = 0; i < 256; ++i) {
		const uint32 offset = READ_LE_UINT32(data + i * 4);
		if (!isValidFontGlyphOffset(offset, size))
			continue;

		const uint8 width = data[offset];
		const uint8 height = data[offset + 1];
		if (width == 0 || height == 0 || width > 64 || height > 64)
			continue;

		if (width > cellW)
			cellW = width;
		if (height > cellH)
			cellH = height;
	}

	return cellW > 0 && cellH > 0;
}

#ifdef USE_PNG
static bool writeMonochromePng(const char *outPath, uint8 *pixels, int width, int height) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

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
	png_set_IHDR(png, info, width, height, 8, PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

	png_color palette[2];
	palette[0].red = palette[0].green = palette[0].blue = 0;
	palette[1].red = palette[1].green = palette[1].blue = 255;
	png_set_PLTE(png, info, palette, 2);

	png_byte trans = 0;
	png_set_tRNS(png, info, &trans, 1, nullptr);

	png_write_info(png, info);

	png_bytep *rowPointers = (png_bytep *)malloc(sizeof(png_bytep) * height);
	if (!rowPointers) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	for (int y = 0; y < height; ++y)
		rowPointers[y] = pixels + y * width;

	png_write_image(png, rowPointers);
	png_write_end(png, nullptr);

	free(rowPointers);
	png_destroy_write_struct(&png, &info);
	fclose(fout);
	return true;
}
#endif

static bool exportLbaFontAtlas(const uint8 *data, int32 size, const char *outPath) {
#ifndef USE_PNG
	(void)data;
	(void)size;
	(void)outPath;
	fprintf(stderr, "PNG support was not enabled at build time\n");
	return false;
#else
	int cellW = 0;
	int cellH = 0;
	if (!measureLbaFontCell(data, size, cellW, cellH))
		return false;

	const int atlasW = kFontAtlasColumns * cellW;
	const int atlasH = kFontAtlasColumns * cellH;
	uint8 *atlas = (uint8 *)calloc(atlasW * atlasH, 1);
	if (!atlas)
		return false;

	uint8 *glyph = (uint8 *)calloc(cellW * cellH, 1);
	if (!glyph) {
		free(atlas);
		return false;
	}

	for (int ch = 0; ch < 256; ++ch) {
		if (!decodeLbaFontGlyph(data, size, ch, cellW, cellH, glyph))
			continue;

		const int col = ch % kFontAtlasColumns;
		const int row = ch / kFontAtlasColumns;
		const int dstX = col * cellW;
		const int dstY = row * cellH;

		for (int y = 0; y < cellH; ++y) {
			memcpy(atlas + (dstY + y) * atlasW + dstX, glyph + y * cellW, cellW);
		}
	}

	const bool ok = writeMonochromePng(outPath, atlas, atlasW, atlasH);
	free(glyph);
	free(atlas);
	return ok;
#endif
}

static void decodeSjisGlyph(const uint8 *data, int index, uint8 *out, int cellW, int cellH) {
	memset(out, 0, cellW * cellH);

	const uint8 *glyphPtr = data + index * (kSjisCharHeight * kSjisCharWidth / 8);
	for (int fontY = 0; fontY < kSjisCharHeight; ++fontY) {
		byte bits = 0;
		int remBits = 0;
		for (int fontX = 0; fontX < kSjisCharWidth; ++fontX) {
			if (remBits == 0) {
				remBits = 8;
				bits = *glyphPtr++;
			}

			if (bits & 0x80)
				out[fontY * cellW + fontX] = 1;

			remBits--;
			bits <<= 1;
		}
	}
}

static bool exportSjisFontAtlas(const uint8 *data, int32 size, const char *outPath) {
#ifndef USE_PNG
	(void)data;
	(void)size;
	(void)outPath;
	fprintf(stderr, "PNG support was not enabled at build time\n");
	return false;
#else
	(void)size;

	const int columns = 104;
	const int rows = (kSjisGlyphCount + columns - 1) / columns;
	const int atlasW = columns * kSjisCharWidth;
	const int atlasH = rows * kSjisCharHeight;

	uint8 *atlas = (uint8 *)calloc(atlasW * atlasH, 1);
	if (!atlas)
		return false;

	uint8 *glyph = (uint8 *)calloc(kSjisCharWidth * kSjisCharHeight, 1);
	if (!glyph) {
		free(atlas);
		return false;
	}

	for (int index = 0; index < kSjisGlyphCount; ++index) {
		decodeSjisGlyph(data, index, glyph, kSjisCharWidth, kSjisCharHeight);

		const int col = index % columns;
		const int row = index / columns;
		const int dstX = col * kSjisCharWidth;
		const int dstY = row * kSjisCharHeight;

		for (int y = 0; y < kSjisCharHeight; ++y)
			memcpy(atlas + (dstY + y) * atlasW + dstX, glyph + y * kSjisCharWidth, kSjisCharWidth);
	}

	const bool ok = writeMonochromePng(outPath, atlas, atlasW, atlasH);
	free(glyph);
	free(atlas);
	return ok;
#endif
}

static bool exportMsDosFontAtlas(const uint8 *data, int32 size, const char *outPath) {
#ifndef USE_PNG
	(void)data;
	(void)size;
	(void)outPath;
	fprintf(stderr, "PNG support was not enabled at build time\n");
	return false;
#else
	(void)size;

	const int cellW = 8;
	const int cellH = 8;
	const int atlasW = kFontAtlasColumns * cellW;
	const int atlasH = kFontAtlasColumns * cellH;
	uint8 *atlas = (uint8 *)calloc(atlasW * atlasH, 1);
	if (!atlas)
		return false;

	for (int ch = 0; ch < 256; ++ch) {
		const uint8 *glyph = data + ch * 8;
		const int col = ch % kFontAtlasColumns;
		const int row = ch / kFontAtlasColumns;
		const int dstX = col * cellW;
		const int dstY = row * cellH;

		for (int y = 0; y < cellH; ++y) {
			uint8 line = glyph[y];
			for (int x = 0; x < cellW; ++x) {
				if (line & (0x80 >> x))
					atlas[(dstY + y) * atlasW + dstX + x] = 1;
			}
		}
	}

	const bool ok = writeMonochromePng(outPath, atlas, atlasW, atlasH);
	free(atlas);
	return ok;
#endif
}

} // namespace

bool twineLooksLikeLbaFont(const uint8 *data, int32 size) {
	if (!data || size < 1024 + 8)
		return false;

	int validGlyphs = 0;
	int invalidGlyphs = 0;

	for (int i = 0; i < 256; ++i) {
		const uint32 offset = READ_LE_UINT32(data + i * 4);
		if (offset == 0)
			continue;

		if (!isValidFontGlyphOffset(offset, size)) {
			invalidGlyphs++;
			continue;
		}

		const uint8 width = data[offset];
		const uint8 height = data[offset + 1];
		if (width == 0 || height == 0 || width > 64 || height > 64) {
			invalidGlyphs++;
			continue;
		}

		validGlyphs++;
	}

	return validGlyphs >= 20 && validGlyphs > invalidGlyphs;
}

bool twineLooksLikeSjisFont(const uint8 *data, int32 size) {
	return data && size == kSjisFontSize;
}

bool twineLooksLikeMsDosFont(const uint8 *data, int32 size) {
	return data && size == kMsDosFontSize;
}

bool twineConvertFontToPng(const uint8 *data, int32 size, TwineEntryType type, const char *outPath) {
	char pngPath[1100];
	snprintf(pngPath, sizeof(pngPath), "%s.png", outPath);

	switch (type) {
	case TWINE_ENTRY_FONT:
		return exportLbaFontAtlas(data, size, pngPath);
	case TWINE_ENTRY_FONT_SJIS:
		return exportSjisFontAtlas(data, size, pngPath);
	case TWINE_ENTRY_FONT_MSDOS:
		return exportMsDosFontAtlas(data, size, pngPath);
	default:
		return false;
	}
}
