/* compress_queen - Rebuild QUEEN.1 file to contain Resource Table (and optionally compress sound & speech)
 * Copyright (C) 2009  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef COMPRESS_QUEEN_H
#define COMPRESS_QUEEN_H

#include "compress.h"

class CompressQueen : public CompressionTool {
public:
	CompressQueen(const std::string &name = "compress_queen");

	virtual void execute();

	// Keeping structs here avoids name-clashes with other tools
	struct GameVersion {
		char versionString[6];
		uint8 isFloppy;
		uint8 isDemo;
		uint32 tableOffset;
		uint32 dataFileSize;
	};

	struct Entry {
		char filename[13];
		uint8 bundle;
		uint32 offset;
		uint32 size;
	};

	struct VersionExtra {
		uint8	compression;
		uint16	entries;
	};

protected:
	Entry _entry;
	VersionExtra _versionExtra;
	const GameVersion *_version;

	void createFinalFile(Common::Filename outPath);
	void fromFileToFile(Common::File &in, Common::File &out, uint32 amount);
	const GameVersion *detectGameVersion(uint32 size);
};

#endif
