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


#include "common/endian.h"
#include "common/file.h"
#include "common/util.h"

#include <iconv.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

#include <ft2build.h>
#include FT_FREETYPE_H

namespace {

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

bool drawGlyph16x16(uint32 unicode, Glyph &glyph);

} // end of anonymous namespace

int main(int argc, char *argv[]) {
	if (argc < 2 || argc > 3) {
		printf("Usage:\n\t%s <input ttf font> [outfile]\n", argv[0]);
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

	atexit(deinitSJIStoUTF32Conversion);

	if (!initFreeType(font)) {
		error("Could not initialize FreeType library.");
		return -1;
	}

	atexit(deinitFreeType);

	std::vector<Glyph> glyphs;
	int chars = 0;

	for (uint8 fB = 0x81; fB <= 0xEF; ++fB) {
		if (fB >= 0xA0 && fB <= 0xDF)
			continue;

		for (uint8 sB = 0x40; sB <= 0xFC; ++sB) {
			if (sB == 0x7F)
				continue;

			++chars;

			uint32 utf32 = convertSJIStoUTF32(fB, sB);
			if (utf32 == (uint32)-1) {
				// For now we disable that warning, since iconv will fail for all reserved,
				// that means unused, valid SJIS character codes.
				//
				// It might be useful to enable that warning again to detect problems with
				// iconv though. An example for such an iconv problem is the 
				// "FULLWIDTH APOSTROPHE", which iconv refuses to convert to UTF-32.
				//warning("Conversion error on: %.2X %.02X", fB, sB);
				continue;
			}

			Glyph data;
			data.fB = fB;
			data.sB = sB;
			if (!drawGlyph16x16(utf32, data)) {
				warning("Could not render glyph: %.2X %.2X", fB, sB);
				continue;
			}

			glyphs.push_back(data);
		}
	}

	// Calculate the base line for the font. The possible range is [0, 15].
	int baseLine = 15;
	for (std::vector<Glyph>::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		int bL = 16 + (i->yOffset - i->height);

		if (bL < baseLine)
			baseLine = bL;
	}
	
	// Check whether we have an character which does not fit in 16x16
	for (std::vector<Glyph>::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		if (baseLine - i->yOffset < 0 || baseLine - i->yOffset + i->height > 16 ||
			i->xOffset > 15 || i->xOffset + i->width > 16) {

			for (std::vector<Glyph>::iterator j = glyphs.begin(); j != glyphs.end(); ++j)
				delete[] i->plainData;

			error("Could not fit glyph for %.2X %.2X top: %d bottom: %d, left: %d right: %d", i->fB, i->sB, 
			      baseLine - i->yOffset, baseLine - i->yOffset + i->height, i->xOffset, i->xOffset + i->width);
		}
	}

	const int sjisDataSize = chars * 32;
	uint8 *sjisFontData = new uint8[sjisDataSize];

	if (!sjisFontData) {
		for (std::vector<Glyph>::iterator i = glyphs.begin(); i != glyphs.end(); ++i)
			delete[] i->plainData;
		error("Out of memory");
	}

	memset(sjisFontData, 0, sjisDataSize);

	for (std::vector<Glyph>::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		int chunk = mapSJIStoChunk(i->fB, i->sB);

		if (chunk != -1) {
			uint8 *dst = sjisFontData + chunk * 32;
			const uint8 *src = i->plainData;

			dst += (baseLine - i->yOffset) * 2;

			for (int y = 0; y < i->height; ++y) {
				uint16 mask = 1 << (15 - i->xOffset);

				const uint8 *curSrc = src;

				uint16 line = 0;
				uint8 d = 0;
				for (int x = 0; x < i->width; ++x) {
					if (x == 0 || x == 8)
						d = *curSrc++;

					if (d & 0x80)
						line |= mask;

					d <<= 1;
					mask >>= 1;
				}

				WRITE_BE_UINT16(dst, line); dst += 2;
				src += i->pitch;
			}
		}
	}

	for (std::vector<Glyph>::iterator i = glyphs.begin(); i != glyphs.end(); ++i)
		delete[] i->plainData;
	glyphs.clear();

	File sjisFont(out, "wb");
	if (sjisFont.isOpen()) {
		// Write our magic bytes
		sjisFont.writeUint32BE(MKID_BE('SCVM'));
		sjisFont.writeUint32BE(MKID_BE('SJIS'));

		// Write version
		sjisFont.writeUint32BE(0x00000001);

		// Write character count
		sjisFont.writeUint16BE(chars);

		sjisFont.write(sjisFontData, 1, sjisDataSize);

		if (sjisFont.err()) {
			delete[] sjisFontData;
			error("Error while writing to font file: '%s'", out);
		}
	} else {
		delete[] sjisFontData;
		error("Could not open file '%s' for writing", out);
	}

	delete[] sjisFontData;
	return 0;
}

namespace {

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

	err = FT_Set_Pixel_Sizes(sjisFont, 16, 16);
	if (err) {
		warning("Could not initialize font for 16x16 outout.");
		return false;
	}

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

bool drawGlyph16x16(uint32 unicode, Glyph &glyph) {
	uint32 index = FT_Get_Char_Index(sjisFont, unicode);
	if (!index)
		return false;

	FT_Error err = FT_Load_Glyph(sjisFont, index, FT_LOAD_MONOCHROME);
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
		glyph.plainData = new uint8[glyph.height * abs(glyph.pitch)];
		if (!glyph.plainData)
			return false;

		const uint8 *src = bitmap.buffer;
		uint8 *dst = glyph.plainData;

		if (glyph.pitch < 0)
			dst += (glyph.height - 1) * (-glyph.pitch);

		for (int i = 0; i < bitmap.rows; ++i) {
			memcpy(dst, src, glyph.pitch);
			src += bitmap.pitch;
			dst += glyph.pitch;
		}
	}

	return true;
}

} // end of anonymous namespace

