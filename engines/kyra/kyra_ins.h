/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

#ifndef KYRA_INS_H
#define KYRA_INS_H

#include "extract_kyra.h"

class HoFInstaller : public Extractor {
public:
	HoFInstaller(const char *baseFilename);
	~HoFInstaller() { delete _list; delete _files; }

	cFileList *getFileList() const { return _files; }
private:
	char _baseFilename[1024];

	struct Archive {
		Archive() : next(0) {}
		~Archive() { delete next; next = 0; }

		char filename[1024];
		uint32 firstFile;
		uint32 startOffset;
		uint32 lastFile;
		uint32 endOffset;
		uint32 totalSize;

		Archive *next;
	};

	Archive *_list;
	FileList *_files;
};

#endif

