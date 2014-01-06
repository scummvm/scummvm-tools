/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#ifndef KYRA_PAK_H
#define KYRA_PAK_H

#include "extract_kyra.h"

class PAKFile : public Extractor {
public:
	PAKFile() : _fileList(0), _isAmiga(false), _links(0) {}
	~PAKFile() { delete _fileList; }

	static bool isPakFile(const char *file);

	bool loadFile(const char *file, const bool isAmiga);
	bool saveFile(const char *file);
	void clearFile() { delete _fileList; _fileList = 0; }

	uint32 getFileSize() const { return _fileList->getTableSize()+5+4+_fileList->getFileSize(); }

	const uint8 *getFileData(const char *file, uint32 *size);

	bool addFile(const char *name, const char *file);
	bool addFile(const char *name, uint8 *data, uint32 size);

	bool linkFiles(const char *name, const char *linkTo);

	bool removeFile(const char *name);

	cFileList *getFileList() const { return _fileList; }

	void drawFileList();
	bool outputAllFiles(Common::Filename *outputPath);
	bool outputFileAs(const char *file, const char *outputName);
private:
	FileList *_fileList;
	bool _isAmiga;

	struct LinkList {
		LinkList() : filename(0), linksTo(0), next(0) {}
		~LinkList() { delete[] filename; delete next; }

		char *filename;
		const char *linksTo;

		LinkList *next;

		LinkList *findEntry(const char *linkDest) {
			for (LinkList *cur = this; cur; cur = cur->next) {
				if (scumm_stricmp(cur->linksTo, linkDest) == 0)
					return cur;
			}
			return 0;
		}

		LinkList *findSrcEntry(const char *f) {
			for (LinkList *cur = this; cur; cur = cur->next) {
				if (scumm_stricmp(cur->filename, f) == 0)
					return cur;
			}
			return 0;
		}

		void addEntry(LinkList *e) {
			if (next)
				next->addEntry(e);
			else
				next = e;
		}

		int size() const {
			return 1 + (next ? next->size() : 0);
		}
	};

	LinkList *_links;
	void generateLinkEntry();
	void loadLinkEntry();
};

#endif

