/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
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

#include <iconv.h>
#include <list>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "util.h"
#include <cstdio>
#include <cstdlib>

namespace {

bool isASCII(uint8 fB);

int mapASCIItoChunk(uint8 fB);
int mapSJIStoChunk(uint8 fB, uint8 sB);

bool initSJIStoUTF32Conversion();
void deinitSJIStoUTF32Conversion();
uint32 convertSJIStoUTF32(uint8 fB, uint8 sB);

bool initFreeType(const char *font);
void deinitFreeType();

struct Glyph {
	uint8 fB, sB;

	int xOffset;
	int yOffset;
	int height;
	int width;

	int pitch;
	uint8 *plainData;
};

bool setGlyphSize(int width, int height);
bool checkGlyphSize(const Glyph &g, const int baseLine, const int maxW, const int maxH);

bool drawGlyph(uint8 fB, uint8 sB, Glyph &glyph);
bool drawGlyph(uint32 unicode, Glyph &glyph);

void convertChar8x16(uint8 *dst, const Glyph &g);
void convertChar16x16(uint8 *dst, const Glyph &g);

typedef std::list<Glyph> GlyphList;
void freeGlyphlist(GlyphList &list) {
	for (GlyphList::iterator i = list.begin(); i != list.end(); ++i)
		delete[] i->plainData;
	list.clear();
}

} // end of anonymous namespace

int main(int argc, char *argv[]) {
	if (argc < 2 || argc > 3) {
		std::printf("Usage:\n\t%s <input ttf font> [outfile]\n", argv[0]);
		return -1;
	}

	const char *font = argv[1];
	const char *out = 0;
	if (argc == 3)
		out = argv[2];
	else
		out = "sjis.fnt";

	if (!initSJIStoUTF32Conversion()) {
		error("Could not initialize conversion from SJIS to UTF-32.");
		return -1;
	}

	std::atexit(deinitSJIStoUTF32Conversion);

	if (!initFreeType(font)) {
		error("Could not initialize FreeType library.");
		return -1;
	}

	std::atexit(deinitFreeType);

	GlyphList glyphs;
	int chars8x16 = 0;
	int chars16x16 = 0;

	// FIXME: It seems some ttf fonts will only render invalid data,
	// when FreeType2 is asked to provide 8x16 glyphs. "sazanami-mincho.ttf"
	// is such a case. When we will render them in the default 16x16 setting
	// all ASCII chars will be rendered correctly and still fit within 8x16.
	// Some fonts might require special setup as 8x16 though.
	// We should try to find some proper way of detecting and handling this.

	// ASCII chars will be rendererd as 8x16
	if (!setGlyphSize(8, 16))
		return -1;

	for (uint8 fB = 0x00; fB <= 0xDF; ++fB) {
		if (mapASCIItoChunk(fB) == -1)
			continue;

		++chars8x16;

		Glyph data;
		if (drawGlyph(fB, 0, data))
			glyphs.push_back(data);
	}

	// The two byte SJIS chars will be rendered as 16x16
	if (!setGlyphSize(16, 16)) {
		freeGlyphlist(glyphs);
		return -1;
	}

	for (uint8 fB = 0x81; fB <= 0xEF; ++fB) {
		if (mapSJIStoChunk(fB, 0x40) == -1)
			continue;

		for (uint8 sB = 0x40; sB <= 0xFC; ++sB) {
			if (mapSJIStoChunk(fB, sB) == -1)
				continue;

			++chars16x16;

			Glyph data;
			if (drawGlyph(fB, sB, data))
				glyphs.push_back(data);
		}
	}

	// Post process all chars, so that xOffset is at least 0
	int minXOffset = 0;
	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i)
		minXOffset = std::min(minXOffset, i->xOffset);

	minXOffset = std::abs(minXOffset);

	for (GlyphList::iterator i = glyphs.begin(); i != glyphs.end(); ++i)
		i->xOffset += minXOffset;

	// Calculate the base line for the font. The possible range is [0, 15].
	// TODO: This logic might need some more tinkering, it's pretty hacky right now.
	int baseLine = 0;
	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		int bL = 0;

		// Try to center the glyph vertically
		if (i->height + i->yOffset <= 16)
			bL = i->yOffset + (16 - (i->height + i->yOffset)) / 2;

		bL = std::min(bL, 15);

		baseLine = std::max(baseLine, bL);
	}

	// Check whether we have an character which does not fit within the boundaries6
	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		if (i->pitch == 0)
			continue;

		if ((isASCII(i->fB) && !checkGlyphSize(*i, baseLine, 8, 16)) ||
			(!isASCII(i->fB) && !checkGlyphSize(*i, baseLine, 16, 16))) {
			for (GlyphList::iterator j = glyphs.begin(); j != glyphs.end(); ++j)
				delete[] j->plainData;

			error("Could not fit glyph for %.2X %.2X top: %d bottom: %d, left: %d right: %d, xOffset: %d, yOffset: %d, width: %d, height: %d, baseLine: %d",
				   i->fB, i->sB, baseLine - i->yOffset, baseLine - i->yOffset + i->height, i->xOffset, i->xOffset + i->width,
				   i->xOffset, i->yOffset, i->width, i->height, baseLine);
		}
	}

	const int sjis8x16DataSize = chars8x16 * 16;
	uint8 *sjis8x16FontData = new uint8[sjis8x16DataSize];

	const int sjis16x16DataSize = chars16x16 * 32;
	uint8 *sjis16x16FontData = new uint8[sjis16x16DataSize];

	if (!sjis16x16FontData) {
		freeGlyphlist(glyphs);
		error("Out of memory");
	}

	memset(sjis8x16FontData, 0, sjis8x16DataSize);
	memset(sjis16x16FontData, 0, sjis16x16DataSize);

	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		if (isASCII(i->fB)) {
			int chunk = mapASCIItoChunk(i->fB);

			if (chunk != -1) {
				uint8 *dst = sjis8x16FontData + chunk * 16;
				dst += (baseLine - i->yOffset);
				convertChar8x16(dst, *i);
			}
		} else {
			int chunk = mapSJIStoChunk(i->fB, i->sB);

			if (chunk != -1) {
				uint8 *dst = sjis16x16FontData + chunk * 32;
				dst += (baseLine - i->yOffset) * 2;
				convertChar16x16(dst, *i);
			}
		}
	}

	freeGlyphlist(glyphs);

	FILE *sjisFont = std::fopen(out, "wb");
	if (sjisFont) {
		// Write our magic bytes
		writeUint32BE(sjisFont, MKID_BE('SCVM'));
		writeUint32BE(sjisFont, MKID_BE('SJIS'));

		// Write version
		writeUint32BE(sjisFont, 0x00000002);

		// Write character count
		writeUint16BE(sjisFont, chars16x16);
		writeUint16BE(sjisFont, chars8x16);

		std::fwrite(sjis16x16FontData, 1, sjis16x16DataSize, sjisFont);
		std::fwrite(sjis8x16FontData, 1, sjis8x16DataSize, sjisFont);
		std::fflush(sjisFont);

		if (std::ferror(sjisFont)) {
			delete[] sjis8x16FontData;
			delete[] sjis16x16FontData;
			std::fclose(sjisFont);
			error("Error while writing to font file: '%s'", out);
		}

		std::fclose(sjisFont);
	} else {
		delete[] sjis8x16FontData;
		delete[] sjis16x16FontData;
		error("Could not open file '%s' for writing", out);
	}

	delete[] sjis8x16FontData;
	delete[] sjis16x16FontData;
	return 0;
}

namespace {

bool isASCII(uint8 fB) {
	return (mapASCIItoChunk(fB) != -1);
}

int mapASCIItoChunk(uint8 fB) {
	// ASCII chars
	if (fB <= 0x7F)
		return fB;

	// half-width katakana
	if (fB >= 0xA1 && fB <= 0xDF)
		return fB - 0x21;

	return -1;
}

int mapSJIStoChunk(uint8 fB, uint8 sB) {
	// We only allow 2 byte SJIS characters.
	if (fB <= 0x80 || fB >= 0xF0 || (fB >= 0xA0 && fB <= 0xDF) || sB == 0x7F)
		return -1;

	int base = fB;
	base -= 0x81;
	if (base >= 0x5F)
		base -= 0x40;

	int index = sB;
	index -= 0x40;
	if (index >= 0x3F)
		--index;

	return (base * 0xBC + index);
}

iconv_t confSetup = (iconv_t)-1;

bool initSJIStoUTF32Conversion() {
	// We initialize a SJIS to native endian UTF-32 conversion
	// over here.
	confSetup = iconv_open("UTF-32", "SJIS");
	return (confSetup != (iconv_t)-1);
}

void deinitSJIStoUTF32Conversion() {
	iconv_close(confSetup);
}

uint32 convertSJIStoUTF32(uint8 fB, uint8 sB) {
	// For some reason iconv will refuse "0x81 0xAD" as valid
	// SJIS sequence, even though it is "FULLWIDTH APOSTROPHE",
	// thus we will short circuit iconv and convert it ourself
	// here.
	if (fB == 0x81 && sB == 0xAD)
		return 0x0000FF07;
	// SJIS uses "YEN SIGN" instead of "REVERSE SOLIDUS"
	else if (fB == 0x5C && sB == 0x00)
		return 0x000000A5;
	// SJIS uses "OVERLINE" instead of "TILDE"
	else if (fB == 0x7E && sB == 0x00)
		return 0x0000203E;

	char inBuf[3];

	inBuf[0] = fB;
	inBuf[1] = sB;
	inBuf[2] = 0;

	char outBuf[3 * sizeof(uint32)];
	memset(outBuf, 0, sizeof(outBuf));

	size_t inBufSize = sizeof(inBuf);
	size_t outBufSize = sizeof(outBuf);
	char *inBufWrap = inBuf;
	char *outBufWrap = outBuf;

	if (iconv(confSetup, &inBufWrap, &inBufSize, &outBufWrap, &outBufSize) == (size_t)-1)
		return (uint32)-1;

	uint32 ret = *(uint32 *)outBuf;

	// It might happen that iconv will add a "ZERO WIDTH NO-BREAK SPACE"
	// before a character, we filter that out over here.
	if (ret == 0x0000FEFF)
		ret = *(uint32 *)(outBuf + 4);
	
	return ret;
}

FT_Library ft = NULL;
FT_Face sjisFont = NULL;

bool initFreeType(const char *font) {
	FT_Error err = FT_Init_FreeType(&ft);
	if (err) {
		warning("Could not initialize FreeType2 library.");
		return false;
	}

	err = FT_New_Face(ft, font, 0, &sjisFont);
	if (err) {
		warning("Could not load font '%s'", font);
		return false;
	}

	if (!setGlyphSize(16, 16))
		return false;

	err = FT_Select_Charmap(sjisFont, FT_ENCODING_UNICODE);
	if (err) {
		warning("Could not select unicode charmap.");
		return false;
	}

	return true;
}

void deinitFreeType() {
	FT_Done_Face(sjisFont);
	FT_Done_FreeType(ft);
}

bool setGlyphSize(int width, int height) {
	FT_Error err = FT_Set_Pixel_Sizes(sjisFont, width, height);
	if (err) {
		warning("Could not initialize font for %dx%d outout.", width, height);
		return false;
	}

	return true;
}

bool checkGlyphSize(const Glyph &g, const int baseLine, const int maxW, const int maxH) {
	if (baseLine - g.yOffset < 0 || baseLine - g.yOffset + g.height > (maxH + 1) ||
		g.xOffset > (maxW - 1) || g.xOffset + g.width > (maxW + 1))
		return false;
	return true;
}

bool drawGlyph(uint8 fB, uint8 sB, Glyph &glyph) {
	uint32 utf32 = convertSJIStoUTF32(fB, sB);
	if (utf32 == (uint32)-1) {
		// For now we disable that warning, since iconv will fail for all reserved,
		// that means unused, valid SJIS character codes.
		//
		// It might be useful to enable that warning again to detect problems with
		// iconv though. An example for such an iconv problem is the 
		// "FULLWIDTH APOSTROPHE", which iconv refuses to convert to UTF-32.
		//warning("Conversion error on: %.2X %.02X", fB, sB);
		return false;
	}

	glyph.fB = fB;
	glyph.sB = sB;
	if (!drawGlyph(utf32, glyph)) {
		warning("Could not render glyph: %.2X %.2X", fB, sB);
		return false;
	}

	return true;
}

bool drawGlyph(uint32 unicode, Glyph &glyph) {
	uint32 index = FT_Get_Char_Index(sjisFont, unicode);
	if (!index)
		return false;

	FT_Error err = FT_Load_Glyph(sjisFont, index, FT_LOAD_MONOCHROME | FT_LOAD_NO_HINTING);
	if (err)
		return false;

	err = FT_Render_Glyph(sjisFont->glyph, FT_RENDER_MODE_MONO);
	if (err)
		return false;

	const FT_Bitmap &bitmap = sjisFont->glyph->bitmap;

	glyph.yOffset = sjisFont->glyph->bitmap_top;
	glyph.xOffset = sjisFont->glyph->bitmap_left;
	glyph.height = bitmap.rows;
	glyph.width = bitmap.width;
	glyph.pitch = bitmap.pitch;
	glyph.plainData = 0;

	if (glyph.height) {
		glyph.plainData = new uint8[glyph.height * std::abs(glyph.pitch)];
		if (!glyph.plainData)
			return false;

		const uint8 *src = bitmap.buffer;
		uint8 *dst = glyph.plainData;

		if (glyph.pitch < 0)
			dst += (glyph.height - 1) * (-glyph.pitch);

		for (int i = 0; i < bitmap.rows; ++i) {
			memcpy(dst, src, std::abs(glyph.pitch));
			src += bitmap.pitch;
			dst += glyph.pitch;
		}
	}

	glyph.pitch = std::abs(glyph.pitch);

	return true;
}

// TODO: merge these two

void convertChar8x16(uint8 *dst, const Glyph &g) {
	const uint8 *src = g.plainData;

	assert(g.width + g.xOffset <= 8);

	for (int y = 0; y < g.height; ++y) {
		uint8 mask = 1 << (7 - g.xOffset);

		const uint8 *curSrc = src;

		uint8 line = 0;
		uint8 d = 0;
		for (int x = 0; x < g.width; ++x) {
			if (x == 0)
				d = *curSrc++;

			if (d & 0x80)
				line |= mask;

			d <<= 1;
			mask >>= 1;
		}

		*dst++ = line;
		src += g.pitch;
	}
}

void convertChar16x16(uint8 *dst, const Glyph &g) {
	const uint8 *src = g.plainData;

	for (int y = 0; y < g.height; ++y) {
		uint16 mask = 1 << (15 - g.xOffset);

		const uint8 *curSrc = src;

		uint16 line = 0;
		uint8 d = 0;
		for (int x = 0; x < g.width; ++x) {
			if (x == 0 || x == 8)
				d = *curSrc++;

			if (d & 0x80)
				line |= mask;

			d <<= 1;
			mask >>= 1;
		}

		WRITE_BE_UINT16(dst, line); dst += 2;
		src += g.pitch;
	}
}

} // end of anonymous namespace

