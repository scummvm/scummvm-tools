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

#ifndef ENGINES_TWINE_EXTRACT_TWINE_CONVERT_H
#define ENGINES_TWINE_EXTRACT_TWINE_CONVERT_H

#include "common/scummsys.h"
#include "common/file.h"

namespace TwinE {
class BodyData;
}

struct TwinePalette {
	uint8 data[256 * 3];
	bool loaded;

	TwinePalette() : loaded(false) {
		memset(data, 0, sizeof(data));
	}
};

enum TwineEntryType {
	TWINE_ENTRY_RAW = 0,
	TWINE_ENTRY_PALETTE,
	TWINE_ENTRY_SCREEN,
	TWINE_ENTRY_TEXTURE,
	TWINE_ENTRY_SHADING_PALETTE,
	TWINE_ENTRY_SPRITE,
	TWINE_ENTRY_BODY,
	TWINE_ENTRY_ANIM,
	TWINE_ENTRY_VOC,
	TWINE_ENTRY_WAV,
	TWINE_ENTRY_XMIDI,
	TWINE_ENTRY_TEXT,
	TWINE_ENTRY_FONT,
	TWINE_ENTRY_FONT_SJIS,
	TWINE_ENTRY_FONT_MSDOS
};

struct TwineConvertContext {
	const TwinePalette *archivePalettes;
	int32 numEntries;
	const TwinePalette *externalPalette;
	int32 fixedPaletteIndex;
	bool preferLba1;
};

TwineEntryType twineDetectEntryType(const uint8 *data, int32 size, bool preferLba1);
bool twineResolvePalette(const TwineConvertContext &ctx, int32 entryIndex, TwinePalette &out);
bool twineConvertEntry(const uint8 *data, int32 size, int32 entryIndex, TwineEntryType type,
		const TwineConvertContext &ctx, const char *outPath);
const char *twineConvertedExtension(TwineEntryType type);

struct TwineBodyTransform {
	int32 posX;
	int32 posY;
	int32 posZ;
	int32 beta;
};

bool twineTryParseBody(const uint8 *data, int32 size, bool preferLba1, TwinE::BodyData &body, bool &isLba1);
bool twineAppendBodyToObj(const uint8 *data, int32 size, bool preferLba1, const TwinePalette &palette,
		FILE *obj, FILE *mtl, int &vertexBase, int &materialId, const TwineBodyTransform &xform,
		const char *objectName, bool materialsWritten[256]);
bool twineConvertBodyToObj(const uint8 *data, int32 size, bool preferLba1, const TwinePalette &palette, const char *outPath);

bool twineLooksLikeLbaFont(const uint8 *data, int32 size);
bool twineLooksLikeSjisFont(const uint8 *data, int32 size);
bool twineLooksLikeMsDosFont(const uint8 *data, int32 size);
bool twineConvertFontToPng(const uint8 *data, int32 size, TwineEntryType type, const char *outPath);

bool twineIsIleFilename(const Common::Filename &filename);
bool twineConvertIsland(const Common::Filename &ileFile, const char *outDir, const TwineConvertContext &ctx);

#endif
