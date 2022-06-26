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

#include <assert.h>
#include <stdio.h>

#include "kyra_pak.h"

#include "common/endian.h"
#include "common/util.h"

bool PAKFile::isPakFile(const char *filename) {
	Common::File f(filename, "rb");

	int32 filesize = f.size();
	int32 offset = 0;
	bool switchEndian = false;
	bool firstFile = true;

	offset = f.readUint32LE();
	if (offset > filesize) {
		switchEndian = true;
		offset = SWAP_32(offset);
	}

	char lastFilenameByte = 0;
	while (!f.eos()) {
		// The start offset of a file should never be in the filelist
		if (offset < f.pos() || offset > filesize) {
			return false;
		}

		byte c = 0;

		lastFilenameByte = 0;
		while (!f.eos() && (c = f.readByte()) != 0)
			lastFilenameByte = c;

		if (f.eos()) {
			return false;
		}

		// Quit now if we encounter an empty string
		if (!lastFilenameByte) {
			if (firstFile) {
				return false;
			} else {
				break;
			}
		}

		firstFile = false;
		offset = switchEndian ? f.readUint32BE() : f.readUint32LE();

		if (!offset || offset == filesize)
			break;
	}

	return true;
}

bool PAKFile::loadFile(const char *file, const bool isAmiga) {
	_isAmiga = isAmiga;
	if (!file)
		return true;

	delete _fileList;
	_fileList = 0;

	Common::File pakfile(file, "rb");

	uint32 filesize = pakfile.size();

	// TODO: get rid of temp. buffer
	uint8 *buffer = new uint8[filesize];
	assert(buffer);

	pakfile.read_throwsOnError(buffer, filesize);

	const char *currentName = 0;

	uint32 startoffset = _isAmiga ? READ_BE_UINT32(buffer) : READ_LE_UINT32(buffer);
	uint32 endoffset = 0;
	uint8* position = buffer + 4;

	while (true) {
		uint32 strlgt = (uint32)strlen((const char*)position);
		currentName = (const char*)position;

		if (!(*currentName))
			break;

		position += strlgt + 1;
		endoffset = _isAmiga ? READ_BE_UINT32(position) : READ_LE_UINT32(position);
		if (endoffset > filesize) {
			endoffset = filesize;
		} else if (endoffset == 0) {
			endoffset = filesize;
		}
		position += 4;

		uint8 *data = new uint8[endoffset - startoffset];
		assert(data);
		memcpy(data, buffer + startoffset, endoffset - startoffset);
		addFile(currentName, data, endoffset - startoffset);
		data = 0;

		if (endoffset == filesize)
			break;

		startoffset = endoffset;
	}

	delete[] buffer;
	loadLinkEntry();
	return true;
}

bool PAKFile::saveFile(const char *file) {
	if (!_fileList)
		return true;
	generateLinkEntry();

	Common::File f(file, "wb");

	// TODO: implement error handling
	uint32 startAddr = _fileList->getTableSize()+5+4;
	static const char *zeroName = "\0\0\0\0\0";

	uint32 curAddr = startAddr;
	for (FileList *cur = _fileList; cur; cur = cur->next) {
		if (_isAmiga)
			f.writeUint32BE(curAddr);
		else
			f.writeUint32LE(curAddr);
		f.write(cur->filename, strlen(cur->filename) + 1);
		curAddr += cur->size;
	}
	if (_isAmiga)
		f.writeUint32BE(curAddr);
	else
		f.writeUint32LE(curAddr);
	f.write(zeroName, 5);

	for (FileList *cur = _fileList; cur; cur = cur->next)
		f.write(cur->data, cur->size);

	return true;
}

const uint8 *PAKFile::getFileData(const char *file, uint32 *size) {
	if (_links) {
		LinkList *entry = _links->findSrcEntry(file);
		if (entry)
			file = entry->linksTo;
	}

	FileList *cur = (_fileList != 0) ? _fileList->findEntry(file) : 0;

	if (!cur)
		return 0;

	if (size)
		*size = cur->size;
	return cur->data;
}

bool PAKFile::addFile(const char *name, const char *file) {
	if ((_fileList && _fileList->findEntry(name)) || (_links && _links->findSrcEntry(name))) {
		error("entry '%s' already exists");
		return false;
	}

	Common::File f(file, "rb");

	uint32 filesize = f.size();
	uint8 *data = new uint8[filesize];
	assert(data);
	f.read_throwsOnError(data, filesize);
	return addFile(name, data, filesize);
}

bool PAKFile::addFile(const char *name, uint8 *data, uint32 size) {
	if ((_fileList && _fileList->findEntry(name)) || (_links && _links->findSrcEntry(name))) {
		uint32 origSize = 0;
		const uint8 *fileData = getFileData(name, &origSize);

		if (size != origSize) {
			error("entry '%s' already exists");
			return false;
		}

		if (memcmp(fileData, data, size) == 0)
			return true;

		error("entry '%s' already exists");
		return false;
	}

	FileList *newEntry = new FileList;
	assert(newEntry);
	newEntry->filename = new char[strlen(name)+1];
	assert(newEntry->filename);
	strncpy(newEntry->filename, name, strlen(name)+1);
	newEntry->size = size;
	newEntry->data = data;

	if (_fileList)
		_fileList->addEntry(newEntry);
	else
		_fileList = newEntry;
	return true;
}

bool PAKFile::linkFiles(const char *name, const char *linkTo) {
	if (!_fileList)
		error("Cannot find file '%s' in file list", linkTo);
	if (!_fileList->findEntry(linkTo))
		error("Cannot find file '%s' in file list", linkTo);
	if ((_fileList && _fileList->findEntry(name)) || (_links && _links->findSrcEntry(name)))
		error("entry '%s' already exists");

	LinkList *entry = new LinkList;
	assert(entry);

	entry->filename = new char[strlen(name)+1];
	assert(entry->filename);
	strncpy(entry->filename, name, strlen(name)+1);
	entry->linksTo = _fileList->findEntry(linkTo)->filename;

	if (!_links)
		_links = entry;
	else
		_links->addEntry(entry);

	return true;
}

static bool isInList(const char * const *linkList, const char *linkTo, const int maxSize) {
	for (int i = 0; i < maxSize; ++i) {
		if (scumm_stricmp(linkList[i], linkTo) == 0)
			return true;
	}

	return false;
}

void PAKFile::generateLinkEntry() {
	removeFile("LINKLIST");
	if (!_links)
		return;

	const int countLinks = _links->size();
	Common::File output("LINKLIST.TMP", "wb");

	const char **linkList = new const char *[countLinks];
	int usedLinks = 0;

	const LinkList *entry = _links;
	for (int i = 0; i < countLinks && entry; ++i, entry = entry->next) {
		if (isInList(linkList, entry->linksTo, usedLinks))
			continue;

		linkList[usedLinks++] = entry->linksTo;
	}

	output.writeUint32BE(MKID_BE('SCVM'));
	output.writeUint32BE(usedLinks);
	for (int i = 0; i < usedLinks; ++i) {
		int count = 0;
		entry = _links;
		while (entry) {
			if (scumm_stricmp(entry->linksTo, linkList[i]) == 0)
				++count;
			entry = entry->next;
		}

		const char *p = linkList[i];
		while (*p)
			output.writeByte(*p++);
		output.writeByte(0);

		output.writeUint32BE(count);
		for (entry = _links; entry; entry = entry->next) {
			if (scumm_stricmp(entry->linksTo, linkList[i]) != 0)
				continue;

			p = entry->filename;
			while (*p)
				output.writeByte(*p++);
			output.writeByte(0);
		}
	}
	output.close();

	addFile("LINKLIST", "LINKLIST.TMP");

	Common::removeFile("LINKLIST.TMP");
	delete[] linkList;
}

void PAKFile::loadLinkEntry() {
	delete _links; _links = 0;

	if (_fileList && _fileList->findEntry("LINKLIST")) {
		const FileList *entry = _fileList->findEntry("LINKLIST");
		const uint8 *src = entry->data;

		uint32 magic = READ_BE_UINT32(src); src += 4;
		if (magic != MKID_BE('SCVM'))
			error("LINKLIST file does not contain 'SCVM' header");
		uint32 links = READ_BE_UINT32(src); src += 4;
		for (uint32 i = 0; i < links; ++i) {
			const char *linksTo = (const char *)src;

			if (!_fileList->findEntry(linksTo))
				error("Couldn't find link destination '%s'", linksTo);
			src += strlen(linksTo) + 1;

			uint32 sources = READ_BE_UINT32(src); src += 4;
			for (uint32 j = 0; j < sources; ++j) {
				LinkList *newEntry = new LinkList;
				assert(newEntry);

				newEntry->linksTo = _fileList->findEntry(linksTo)->filename;
				newEntry->filename = new char[strlen((const char *)src) + 1];
				assert(newEntry->filename);
				strcpy(newEntry->filename, (const char *)src);
				src += strlen((const char *)src) + 1;

				if (_links)
					_links->addEntry(newEntry);
				else
					_links = newEntry;
			}
		}
	}
}

bool PAKFile::removeFile(const char *name) {
	if (_links) {
		LinkList *link = _links->findEntry(name);
		while (link) {
			warning("Implicitly removing link '%s' to file '%s'", link->filename, name);

			LinkList *last = _links;
			while (last != link)
				last = last->next;

			if (last == _links)
				_links = link->next;
			else
				last->next = link->next;

			link->next = 0;
			delete link;
		}

		if ((link = _links->findSrcEntry(name)) != 0) {
			LinkList *last = _links;
			while (last != link)
				last = last->next;

			if (last == _links)
				_links = link->next;
			else
				last->next = link->next;

			link->next = 0;
			delete link;
			return true;
		}
	}

	for (FileList *cur = _fileList, *last = 0; cur; last = cur, cur = cur->next) {
		if (scumm_stricmp(cur->filename, name) == 0) {
			FileList *next = cur->next;
			cur->next = 0;
			delete cur;
			if (last)
				last->next = next;
			else
				_fileList = next;
			return true;
		}
	}
	return false;
}

void PAKFile::drawFileList() {
	Extractor::drawFileList();

	if (_links) {
		printf("Linked files (count: %d):\n", _links->size());
		for (const LinkList *i = _links; i; i = i->next)
			printf("Common::Filename: '%s' -> '%s'\n", i->filename, i->linksTo);
	}
}

bool PAKFile::outputAllFiles(Common::Filename *outputPath) {
	if (!Extractor::outputAllFiles(outputPath))
		return false;

	for (const LinkList *entry = _links; entry; entry = entry->next) {
		outputPath->setFullName(entry->filename);
		if (!outputFileAs(entry->linksTo, outputPath->getFullPath().c_str()))
			return false;
	}

	return true;
}

bool PAKFile::outputFileAs(const char *file, const char *outputName) {
	for (const LinkList *entry = _links; entry; entry = entry->next) {
		if (scumm_stricmp(entry->filename, file) == 0) {
			file = entry->linksTo;
			break;
		}
	}

	return Extractor::outputFileAs(file, outputName);
}

//HACK: move this to another file

void Extractor::drawFileList() {
	cFileList *cur = getFileList();
	while (cur) {
		printf("Common::Filename: '%s' size: %d\n", cur->filename, cur->size);
		cur = cur->next;
	}
}

bool Extractor::outputAllFiles(Common::Filename *outputPath) {
	cFileList *cur = getFileList();

	while (cur) {
		outputPath->setFullName(cur->filename);
		FILE *file = fopen(outputPath->getFullPath().c_str(), "wb");
		if (!file) {
			error("couldn't open file '%s' for writing", outputPath->getFullPath().c_str());
			return false;
		}
		printf("Extracting file '%s'...", cur->filename);
		if (fwrite(cur->data, 1, cur->size, file) == cur->size) {
			printf("OK\n");
		} else {
			printf("FAILED\n");
			fclose(file);
			return false;
		}
		fclose(file);
		cur = cur->next;
	}
	return true;
}

bool Extractor::outputFileAs(const char *f, const char *fn) {
	cFileList *cur = getFileList();
	cur = (cur != 0) ? cur->findEntry(f) : 0;

	if (!cur) {
		error("file '%s' not found");
		return false;
	}

	FILE *file = fopen(fn, "wb");
	if (!file) {
		error("couldn't open file '%s' in write mode", fn);
		return false;
	}
	printf("Extracting file '%s' to file '%s'...", cur->filename, fn);
	if (fwrite(cur->data, 1, cur->size, file) == cur->size) {
		printf("OK\n");
	} else {
		printf("FAILED\n");
		return false;
	}
	fclose(file);
	return true;
}
