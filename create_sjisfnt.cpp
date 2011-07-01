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
 */

#include "create_sjisfnt.h"

#include "common/endian.h"
#include "common/file.h"

#include <stdio.h>
#include <stdlib.h>
#include <iconv.h>
#include <assert.h>
#include <errno.h>

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

	TrueTypeFont *ttf = new TrueTypeFont();
	if (!ttf->load(font)) {
		delete ttf;
		error("Could not initialize FreeType library.");
		return -1;
	}

	if (!ttf->setSize(16)) {
		delete ttf;
		error("Could not setup font '%s' to size 16", font);
		return -1;
	}

	GlyphList glyphs;
	int chars8x16 = 0;
	int chars16x16 = 0;

	ttf->renderASCIIGlyphs(glyphs, chars8x16);
	ttf->renderKANJIGlyphs(glyphs, chars16x16);

	delete ttf;
	ttf = 0;

	fixYOffset(glyphs);

	// Check whether we have an character which does not fit within the boundaries6
	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		if (i->pitch == 0)
			continue;

		if ((isASCII(i->fB) && !i->checkSize(8, 16)) ||
			(!isASCII(i->fB) && !i->checkSize(16, 16))) {
			for (GlyphList::iterator j = glyphs.begin(); j != glyphs.end(); ++j)
				delete[] j->plainData;

			error("Could not fit glyph for %.2X %.2X top: %d bottom: %d, left: %d right: %d, xOffset: %d, yOffset: %d, width: %d, height: %d",
				   i->fB, i->sB, i->yOffset, i->yOffset + i->height, i->xOffset, i->xOffset + i->width,
				   i->xOffset, i->yOffset, i->width, i->height);
		}
	}

	const int sjis8x16DataSize = chars8x16 * 16;
	uint8 *sjis8x16FontData = new uint8[sjis8x16DataSize];

	if (!sjis8x16FontData)
		error("Out of memory");

	const int sjis16x16DataSize = chars16x16 * 32;
	uint8 *sjis16x16FontData = new uint8[sjis16x16DataSize];

	if (!sjis16x16FontData) {
		delete[] sjis8x16FontData;
		error("Out of memory");
	}

	memset(sjis8x16FontData, 0, sjis8x16DataSize);
	memset(sjis16x16FontData, 0, sjis16x16DataSize);

	for (GlyphList::const_iterator i = glyphs.begin(); i != glyphs.end(); ++i) {
		if (isASCII(i->fB)) {
			int chunk = mapASCIItoChunk(i->fB);

			if (chunk != -1)
				i->convertChar8x16(sjis8x16FontData + chunk * 16);
		} else {
			int chunk = mapSJIStoChunk(i->fB, i->sB);

			if (chunk != -1)
				i->convertChar16x16(sjis16x16FontData + chunk * 32);
		}
	}

	Common::File sjisFont(out, "wb");
	if (sjisFont.isOpen()) {
		// Write our magic bytes
		sjisFont.writeUint32BE(MKID_BE('SCVM'));
		sjisFont.writeUint32BE(MKID_BE('SJIS'));

		// Write version
		sjisFont.writeUint32BE(0x00000002);

		// Write character count
		sjisFont.writeUint16BE(chars16x16);
		sjisFont.writeUint16BE(chars8x16);

		sjisFont.write(sjis16x16FontData, sjis16x16DataSize);
		sjisFont.write(sjis8x16FontData, sjis8x16DataSize);

		delete[] sjis8x16FontData;
		delete[] sjis16x16FontData;

		if (sjisFont.err())
			error("Error while writing to font file: '%s'", out);
	} else {
		delete[] sjis8x16FontData;
		delete[] sjis16x16FontData;
		error("Could not open file '%s' for writing", out);
	}

	return 0;
}

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
	// We initialize a SJIS to little endian UTF-32 conversion
	// over here.
	confSetup = iconv_open("UTF-32LE", "SJIS");
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

	size_t inBufSize = ((fB >= 0x81 && fB <= 0x9F) || (fB >= 0xE0 && fB <= 0xEF)) ? 3 : 2;
	size_t outBufSize = sizeof(outBuf);
#ifdef ICONV_USES_CONST
	const char *inBufWrap = inBuf;
#else
	char *inBufWrap = inBuf;
#endif
	char *outBufWrap = outBuf;

	if (iconv(confSetup, &inBufWrap, &inBufSize, &outBufWrap, &outBufSize) == (size_t)-1)
		return (uint32)-1;

	const uint32 ret = READ_LE_UINT32(outBuf);

	// According to http://www.unicode.org/reports/tr19/tr19-9.html it is possible
	// that a "zero width no-break space" is added as first character (probably
	// to be consistent with the "byte order mark"). In case any iconv implementation
	// does that, we just skip over that bit.
	if (ret == 0x0000FEFF)
		return READ_LE_UINT32(outBuf + 4);
	else
		return ret;
}

TrueTypeFont::TrueTypeFont()
	: _library(0), _sjisFont(0), _ascent(0), _descent(0), _width(0), _height(0) {
}

TrueTypeFont::~TrueTypeFont() {
	FT_Done_Face(_sjisFont);
	FT_Done_FreeType(_library);
}

inline int ftFloor26_6(FT_Pos x) {
	return x / 64;
}

inline int ftCeil26_6(FT_Pos x) {
	return (x + 63) / 64;
}

bool TrueTypeFont::load(const char *font) {
	FT_Error err = FT_Init_FreeType(&_library);
	if (err) {
		warning("Could not initialize FreeType2 library.");
		return false;
	}

	err = FT_New_Face(_library, font, 0, &_sjisFont);
	if (err) {
		warning("Could not load font '%s'", font);
		return false;
	}

	err = FT_Select_Charmap(_sjisFont, FT_ENCODING_UNICODE);
	if (err) {
		warning("Could not select unicode charmap.");
		return false;
	}

	if (!FT_IS_SCALABLE(_sjisFont)) {
		warning("Font '%s' is not scalable", font);
		return false;
	}

	return true;
}

bool TrueTypeFont::setSize(int height) {
	if (FT_Set_Char_Size(_sjisFont, 0, height * 64, 0, 0)) {
		warning("Could not initialize font for height %d", height);
		return false;
	}

	FT_Fixed yScale = _sjisFont->size->metrics.y_scale;
	_ascent = ftCeil26_6(FT_MulFix(_sjisFont->ascender, yScale));
	_descent = ftCeil26_6(FT_MulFix(_sjisFont->descender, yScale));

	_width = ftCeil26_6(FT_MulFix(_sjisFont->max_advance_width, _sjisFont->size->metrics.x_scale));
	_height = _ascent - _descent + 1;

	return true;
}

void TrueTypeFont::renderASCIIGlyphs(GlyphList &glyphs, int &count) {
	count = 0;

	for (uint8 fB = 0x00; fB <= 0xDF; ++fB) {
		if (mapASCIItoChunk(fB) == -1)
			continue;

		++count;

		Glyph data;
		if (renderGlyph(fB, 0, data))
			glyphs.push_back(data);
	}
}

void TrueTypeFont::renderKANJIGlyphs(GlyphList &glyphs, int &count) {
	count = 0;

	for (uint8 fB = 0x81; fB <= 0xEF; ++fB) {
		if (mapSJIStoChunk(fB, 0x40) == -1)
			continue;

		for (uint8 sB = 0x40; sB <= 0xFC; ++sB) {
			if (mapSJIStoChunk(fB, sB) == -1)
				continue;

			++count;

			Glyph data;
			if (renderGlyph(fB, sB, data))
				glyphs.push_back(data);
		}
	}
}

bool TrueTypeFont::renderGlyph(uint8 fB, uint8 sB, Glyph &glyph) {
	uint32 utf32 = convertSJIStoUTF32(fB, sB);
	if (utf32 == (uint32)-1) {
		// For now we disable that warning, since iconv will fail for all reserved,
		// that means unused, valid SJIS character codes.
		//
		// It might be useful to enable that warning again to detect problems with
		// iconv though. An example for such an iconv problem is the
		// "FULLWIDTH APOSTROPHE", which iconv refuses to convert to UTF-32.
		if (errno == E2BIG || errno == EINVAL)
			warning("Conversion error on: %.2X %.02X", fB, sB);
		return false;
	}

	glyph.fB = fB;
	glyph.sB = sB;
	if (!renderGlyph(utf32, glyph)) {
		warning("Could not render glyph: %.2X %.2X", fB, sB);
		return false;
	}

	return true;
}

bool TrueTypeFont::renderGlyph(uint32 unicode, Glyph &glyph) {
	uint32 index = FT_Get_Char_Index(_sjisFont, unicode);
	if (!index)
		return false;

	FT_Error err = FT_Load_Glyph(_sjisFont, index, FT_LOAD_MONOCHROME | FT_LOAD_NO_HINTING);
	if (err)
		return false;

	err = FT_Render_Glyph(_sjisFont->glyph, FT_RENDER_MODE_MONO);
	if (err)
		return false;

	FT_Glyph_Metrics &metrics = _sjisFont->glyph->metrics;

	glyph.xOffset = ftFloor26_6(metrics.horiBearingX);
	glyph.yOffset = _ascent - ftFloor26_6(metrics.horiBearingY);

	// In case we got a negative xMin we adjust that, this might make some
	// characters give a odd layout though.
	if (glyph.xOffset < 0)
		glyph.xOffset = 0;

	const FT_Bitmap &bitmap = _sjisFont->glyph->bitmap;

	glyph.height = bitmap.rows;
	glyph.width = bitmap.width;
	glyph.pitch = bitmap.pitch;
	glyph.plainData = 0;

	// We only accept monochrome characters.
	if (bitmap.pixel_mode != FT_PIXEL_MODE_MONO)
		return false;

	if (glyph.height) {
		glyph.plainData = new uint8[glyph.height * abs(glyph.pitch)];
		if (!glyph.plainData)
			return false;

		const uint8 *src = bitmap.buffer;
		uint8 *dst = glyph.plainData;

		if (glyph.pitch < 0)
			dst += (glyph.height - 1) * (-glyph.pitch);

		for (int i = 0; i < bitmap.rows; ++i) {
			memcpy(dst, src, abs(glyph.pitch));
			src += bitmap.pitch;
			dst += glyph.pitch;
		}
	}

	glyph.pitch = abs(glyph.pitch);

	return true;
}

Glyph::Glyph()
	: fB(0), sB(0), xOffset(0), yOffset(0), height(0), width(0), pitch(0),
	  plainData(0) {
}

Glyph::Glyph(const Glyph &r)
	: fB(r.fB), sB(r.sB), xOffset(r.xOffset), yOffset(r.yOffset),
	  height(r.height), width(r.width), pitch(r.pitch), plainData(0) {
	plainData = new uint8[height * pitch];
	memcpy(plainData, r.plainData, height * pitch);
}

Glyph::~Glyph() {
	delete[] plainData;
}

Glyph &Glyph::operator=(const Glyph &r) {
	delete[] plainData;

	fB = r.fB;
	sB = r.sB;
	xOffset = r.xOffset;
	yOffset = r.yOffset;
	height = r.height;
	width = r.width;
	pitch = r.pitch;

	plainData = new uint8[height * pitch];
	memcpy(plainData, r.plainData, height * pitch);

	return *this;
}

bool Glyph::checkSize(const int maxW, const int maxH) const {
	if (yOffset < 0 || yOffset + height > maxH ||
		xOffset < 0 || xOffset + width > maxW)
		return false;
	return true;
}

void Glyph::convertChar8x16(uint8 *dst) const {
	const uint8 *src = plainData;
	dst += yOffset;

	for (int y = 0; y < height; ++y) {
		uint8 mask = 1 << (7 - xOffset);

		const uint8 *curSrc = src;

		uint8 line = 0;
		uint8 d = 0;
		for (int x = 0; x < width; ++x) {
			if (x == 0)
				d = *curSrc++;

			if (d & 0x80)
				line |= mask;

			d <<= 1;
			mask >>= 1;
		}

		*dst++ = line;
		src += pitch;
	}
}

void Glyph::convertChar16x16(uint8 *dst) const {
	const uint8 *src = plainData;
	dst += yOffset * 2;

	for (int y = 0; y < height; ++y) {
		uint16 mask = 1 << (15 - xOffset);

		const uint8 *curSrc = src;

		uint16 line = 0;
		uint8 d = 0;
		for (int x = 0; x < width; ++x) {
			if (x == 0 || x == 8)
				d = *curSrc++;

			if (d & 0x80)
				line |= mask;

			d <<= 1;
			mask >>= 1;
		}

		WRITE_BE_UINT16(dst, line); dst += 2;
		src += pitch;
	}
}

void fixYOffset(GlyphList &glyphs) {
	// We try to find the minimum y offset here so we can substract it to make it 0 in the end.
	// We need to do this, since otherwise the characters will take up too much vertical space.
	int minYOffset = 0xFFFF;

	for (GlyphList::const_iterator i = glyphs.begin(), end = glyphs.end(); i != end; ++i) {
		if (i->pitch == 0)
			continue;

		minYOffset = std::min(minYOffset, i->yOffset);
	}

	// Adapt all glyphs
	for (GlyphList::iterator i = glyphs.begin(), end = glyphs.end(); i != end; ++i) {
		if (i->pitch == 0)
			continue;

		i->yOffset -= minYOffset;
	}
}

