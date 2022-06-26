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

#ifndef EXTRACT_KYRA_H
#define EXTRACT_KYRA_H

#include <string.h>

#include "tool.h"

class ExtractKyra : public Tool {
public:
	ExtractKyra(const std::string &name = "extract_kyra");

	virtual void execute();

	void parseExtraArguments();

	bool extractAll, extractOne, isAmiga, isHoFInstaller;
	std::string singleFilename;
};


class Extractor {
public:
	virtual ~Extractor() {}

	virtual void drawFileList();

	virtual bool outputAllFiles(Common::Filename *outputPath);

	virtual bool outputFile(const char *file) { return outputFileAs(file, file); }
	virtual bool outputFileAs(const char *file, const char *outputName);

	struct FileList {
		FileList() : filename(0), size(0), data(0), next(0) {}
		~FileList() {
			delete[] filename;
			delete[] data;
			delete next;
		}

		FileList *findEntry(const char *f) {
			for (FileList *cur = this; cur; cur = cur->next) {
				if (scumm_stricmp(cur->filename, f) == 0)
					return cur;
			}
			return 0;
		}

		const FileList *findEntry(const char *f) const {
			for (const FileList *cur = this; cur; cur = cur->next) {
				if (scumm_stricmp(cur->filename, f) == 0)
					return cur;
			}
			return 0;
		}

		void addEntry(FileList *e) {
			if (next)
				next->addEntry(e);
			else
				next = e;
		}
		uint32 getTableSize() const {
			return strlen(filename)+1+4+((next != 0) ? next->getTableSize() : 0);
		}
		uint32 getFileSize() const {
			return size + (next != 0 ? next->getFileSize() : 0);
		}

		char *filename;
		uint32 size;
		uint8 *data;

		FileList *next;
	};

	typedef const FileList cFileList;

	virtual cFileList *getFileList() const = 0;
};

#endif

