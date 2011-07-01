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

#ifndef CREATE_SJISFNT_H
#define CREATE_SJISFNT_H

#include "common/util.h"

#include <list>

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
void freeGlyphlist(GlyphList &list);

#endif

