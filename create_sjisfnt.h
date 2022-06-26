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

#ifndef CREATE_SJISFNT_H
#define CREATE_SJISFNT_H

#include "common/util.h"

#include <list>

#include <ft2build.h>
#include FT_FREETYPE_H

bool isASCII(uint8 fB);

int mapASCIItoChunk(uint8 fB);
int mapSJIStoChunk(uint8 fB, uint8 sB);

bool initSJIStoUTF32Conversion();
void deinitSJIStoUTF32Conversion();
uint32 convertSJIStoUTF32(uint8 fB, uint8 sB);

struct Glyph {
	Glyph();
	Glyph(const Glyph &r);
	~Glyph();

	Glyph &operator=(const Glyph &r);

	uint8 fB, sB;

	int xOffset;
	int yOffset;

	int height;
	int width;

	int pitch;
	uint8 *plainData;

	bool checkSize(const int maxW, const int maxH) const;

	void convertChar8x16(uint8 *dst) const;
	void convertChar16x16(uint8 *dst) const;
};

typedef std::list<Glyph> GlyphList;
void fixYOffset(GlyphList &glyphs);

class TrueTypeFont {
public:
	TrueTypeFont();
	~TrueTypeFont();

	bool load(const char *filename);
	bool setSize(int height);

	void renderASCIIGlyphs(GlyphList &glyphs, int &count);
	void renderKANJIGlyphs(GlyphList &glyphs, int &count);

private:
	bool renderGlyph(uint8 fb, uint8 sB, Glyph &glyph);
	bool renderGlyph(uint32 unicode, Glyph &glyph);

	FT_Library _library;
	FT_Face _sjisFont;

	int _ascent, _descent;
	int _width, _height;
};

#endif

