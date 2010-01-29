/* construct_mohawk - Mohawk file constructor
 * Copyright (C) 2009 The ScummVM project
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

#include "common/scummsys.h"
#include "common/array.h"
#include "common/file.h"
#include "common/str.h"
#include "archive.h"

uint32 _fileSize;

struct RSRC_HeaderConstruct {
	uint16 version;
	uint16 compaction;
	uint32 filesize;
	uint32 abs_offset;
	uint16 file_table_offset;
	uint16 file_table_size;
};

RSRC_HeaderConstruct _rsrc;
TypeTable _typeTable;

struct TypeConstructResourceEntries {
	uint16 id;
	uint16 index;
};

struct TypeConstructNameEntries {
	uint16 offset;
	uint16 index;
	// Name List
	Common::String name;
};

struct TypeConstruct {
	//Type Table
	uint32 tag;
	uint16 resource_table_offset;
	uint16 name_table_offset;

	struct ResourceTable {
		uint16 resources;
		Common::Array<TypeConstructResourceEntries> entries;
	} resTable;

	struct NameTable {
		uint16 num;
		Common::Array<TypeConstructNameEntries> entries;
	} nameTable;
};

Common::Array<TypeConstruct> _types;
uint16 _nameTableAmount;
uint16 _resourceTableAmount;
uint16 _fileTableAmount;
Common::Array<FileTable> _fileTable;

int16 getTypeIndex(uint32 tag) {
	for (uint16 i = 0; i < _typeTable.resource_types; i++)
		if (_types[i].tag == tag)
			return i;
	return -1;	// not found
}

int16 getIdIndex(int16 typeIndex, uint16 id) {
	for (uint16 i = 0; i < _types[typeIndex].resTable.resources; i++)
		if (_types[typeIndex].resTable.entries[i].id == id)
			return i;
	return -1;	// not found
}

Common::String tag2string(uint32 tag) {
	char str[5];
	str[0] = (char)(tag >> 24);
	str[1] = (char)(tag >> 16);
	str[2] = (char)(tag >> 8);
	str[3] = (char)tag;
	str[4] = '\0';
	// Replace non-printable chars by dot
	for (int i = 0; i < 4; ++i) {
		if (!isprint(str[i]))
			str[i] = '.';
	}
	return Common::String(str);
}

uint32 string2tag(const char *str) {
	uint32 ret = 0;
	ret += str[0] << 24;
	ret += str[1] << 16;
	ret += str[2] << 8;
	ret += str[3];
	return ret;
}

// Have a maximum buffer size
#define MAX_BUF_SIZE 16384

static byte *outputBuffer = NULL;

void initMohawkArchive(void) {
	// Initialize Mohawk Archive Parameters with parameters for empty archive
	_fileSize = 28;

	_rsrc.version = 256; // BCD 1.0
	_rsrc.compaction = 1;

	_rsrc.filesize = 36;
	_rsrc.abs_offset = 28;
	_rsrc.file_table_offset = 4;
	_rsrc.file_table_size = 4;

	_typeTable.name_offset = 4;
	_typeTable.resource_types = 0;

	_nameTableAmount = 0;
	_resourceTableAmount = 0;
	_fileTableAmount = 0;
}

void updateTypeTableOffsets(void) {
	for (uint16 i = 0; i < _typeTable.resource_types; i++) {
		_types[i].resource_table_offset = 4 + (_typeTable.resource_types * 8);

		for (uint16 j = 0; j < i; j++) {
			_types[i].resource_table_offset += 2 + (_types[j].resTable.resources * 4);
			_types[i].resource_table_offset += 2 + (_types[j].nameTable.num * 4);
		}

		_types[i].name_table_offset = _types[i].resource_table_offset + 2 + (_types[i].resTable.resources * 4);
	}
}

void addTypeToMohawkArchive(const char *resourceTag) {
	TypeConstruct newType;
	newType.tag = string2tag(resourceTag);
	newType.resource_table_offset = 0;
	newType.name_table_offset = 0;
	newType.resTable.resources = 0;
	newType.nameTable.num = 0;

	_fileSize += 12;
	_rsrc.filesize += 12;
	_rsrc.file_table_offset += 12;
	_typeTable.name_offset += 12;

	_typeTable.resource_types++;
	_types.push_back(newType);

	updateTypeTableOffsets();
}

void addFileToMohawkArchive(int fileId, byte fileFlags, const char *resourceTag, int resourceId, const char *resourceName, uint32 fileSize) {
	int16 typeIndex = getTypeIndex(string2tag(resourceTag));
	if (typeIndex == -1) {
		// Add New Type to typeTable
		addTypeToMohawkArchive(resourceTag);
		typeIndex = getTypeIndex(string2tag(resourceTag));
	} else {
		if (getIdIndex(typeIndex, resourceId) != -1) {
			printf("Error : Duplicate Resource Type \'%s\' Id %d\n", resourceTag, resourceId);
			return;
		}
	}

	// Update Resource Table
	TypeConstructResourceEntries newTypeResource;
	newTypeResource.id = resourceId;
	newTypeResource.index = fileId + 1;

	_fileSize += 4;
	_rsrc.filesize += 4;
	_rsrc.file_table_offset += 4;
	_typeTable.name_offset += 4;

	// Insert in Resource Table ordered by id
	uint16 j;
	for (j = 0; j < _types[typeIndex].resTable.entries.size(); j++) {
		if (_types[typeIndex].resTable.entries[j].id > newTypeResource.id)
			break;
	}

	_types[typeIndex].resTable.resources++;
	_types[typeIndex].resTable.entries.insert_at(j, newTypeResource);

	updateTypeTableOffsets();

	// Update Name Table if name is present
	int nameSize = strlen(resourceName);
	if (nameSize != 0) {
		TypeConstructNameEntries newTypeName;

		if (_types[typeIndex].nameTable.entries.empty())
			newTypeName.offset = 0;
		else
			newTypeName.offset = _types[typeIndex].nameTable.entries.back().offset + _types[typeIndex].nameTable.entries.back().name.size() + 1;

		newTypeName.index = fileId + 1;
		newTypeName.name += resourceName;

		_fileSize += 4 + nameSize + 1;
		_rsrc.filesize += 4 + nameSize + 1;
		_rsrc.file_table_offset += 4 + nameSize + 1;
		_typeTable.name_offset += 4;

		_types[typeIndex].nameTable.num++;
		_types[typeIndex].nameTable.entries.push_back(newTypeName);

		updateTypeTableOffsets();
	}

	// TODO: Error if duplicate file table ids
	// TODO: Error if missing file table ids

	// Add file data parameters to fileTable
	uint32 fileItemOffset = 36;
	for (uint16 i = 0; i < _fileTableAmount; i++)
		fileItemOffset += _fileTable[i].dataSize;

	FileTable fileTableItem;
	fileTableItem.offset = fileItemOffset;
	fileTableItem.dataSize = fileSize;
	fileTableItem.flags = fileFlags;
	fileTableItem.unk = 0;

	_fileSize += 10;
	_rsrc.filesize += 10;
	_rsrc.file_table_size += 10;

	_fileTableAmount++;
	_fileTable.push_back(fileTableItem);

	_fileSize += fileSize;
	_rsrc.filesize += fileSize;
	_rsrc.abs_offset += fileSize;
}

void sortTypeTable(Common::Array<TypeConstruct> *tempTypeTable) {
	uint16 i, j;

	// Insert in Resource Type Table ordered Alphabetically by Resource Tag
	for (i = 0; i < _types.size(); i++) {
		for (j = 0; j < tempTypeTable->size(); j++) {
			if ((*tempTypeTable)[j].tag > _types[i].tag)
				break;
		}
		tempTypeTable->insert_at(j, _types[i]);
	}
}

// Order of Name Table in same order as Resource Table cross referenced by index
void sortNameTable(Common::Array<TypeConstructNameEntries> *tempNameTable, uint16 i) {
	for (uint16 j = 0; j < _types[i].resTable.resources; j++) {
		for (uint16 k = 0; k < _types[i].nameTable.num; k++) {
			if (_types[i].resTable.entries[j].index == _types[i].nameTable.entries[k].index)
				tempNameTable->push_back(_types[i].nameTable.entries[k]);
		}
	}

	assert(tempNameTable->size() == _types[i].nameTable.num);
}

void writeMohawkArchive(int argc, char **argv, int archiveArg, Common::File *mohawkFile) {
	// Gap between RSRC and File Data..
	_fileSize += 8;
	_rsrc.filesize += 8;
	_rsrc.abs_offset += 8;

	mohawkFile->writeUint32BE(ID_MHWK);
	mohawkFile->writeUint32BE(_fileSize);

	mohawkFile->writeUint32BE(ID_RSRC);
	mohawkFile->writeUint16BE(_rsrc.version);
	mohawkFile->writeUint16BE(_rsrc.compaction);
	mohawkFile->writeUint32BE(_rsrc.filesize);
	mohawkFile->writeUint32BE(_rsrc.abs_offset);
	mohawkFile->writeUint16BE(_rsrc.file_table_offset);
	mohawkFile->writeUint16BE(_rsrc.file_table_size);

	// Gap between RSRC and File Data...
	mohawkFile->writeUint16BE(4); // Not sure what this is...
	mohawkFile->writeUint16BE(0);
	mohawkFile->writeUint16BE(0);
	mohawkFile->writeUint16BE(0);

	for (int i = archiveArg + 1; i < argc; i++) {
		Common::Filename resourceFile = Common::Filename(argv[i]);

		Common::File *resourceIn = new Common::File(resourceFile, "rb");
		if (!resourceIn->isOpen()) {
			printf("Could not open file \"%s\"\n", resourceFile.getName().c_str());
			delete resourceIn;
			return;
		}

		while (resourceIn->pos() < (int)resourceIn->size()) {
			uint32 size = resourceIn->read_noThrow(outputBuffer, MAX_BUF_SIZE);
			mohawkFile->write(outputBuffer, size);
		}

		resourceIn->close();
		delete resourceIn;
	}

	// _rsrc.abs_offset points here...
	mohawkFile->writeUint16BE(_typeTable.name_offset);
	mohawkFile->writeUint16BE(_typeTable.resource_types);

	Common::Array<TypeConstruct> tempTypeTable;
	// _types sorted into Resource Tag Alphabetical order into tempTypeTable
	sortTypeTable(&tempTypeTable);

	for (uint16 i = 0; i < _typeTable.resource_types; i++) {
		mohawkFile->writeUint32BE(tempTypeTable[i].tag);
		mohawkFile->writeUint16BE(tempTypeTable[i].resource_table_offset);
		mohawkFile->writeUint16BE(tempTypeTable[i].name_table_offset);
	}

	for (uint16 i = 0; i < _typeTable.resource_types; i++) {
		mohawkFile->writeUint16BE(_types[i].resTable.resources);

		for (uint16 j = 0; j < _types[i].resTable.resources; j++) {
			mohawkFile->writeUint16BE(_types[i].resTable.entries[j].id);
			mohawkFile->writeUint16BE(_types[i].resTable.entries[j].index);
		}

		mohawkFile->writeUint16BE(_types[i].nameTable.num);

		Common::Array<TypeConstructNameEntries> tempNameTable;
		// _types[i].resTable.entries sorted into resource order output into tempNameTable
		sortNameTable(&tempNameTable, i);

		for (uint16 j = 0; j < _types[i].nameTable.num; j++) {
			mohawkFile->writeUint16BE(tempNameTable[j].offset);
			mohawkFile->writeUint16BE(tempNameTable[j].index);
		}
	}

	for (uint16 i = 0; i < _typeTable.resource_types; i++) {
		for (uint16 j = 0; j < _types[i].nameTable.num; j++) {
			mohawkFile->write(_types[i].nameTable.entries[j].name.c_str(), _types[i].nameTable.entries[j].name.size()+1);
		}
	}

	// _rsrc.file_table_offset is added to _rsrc.abs_offset to get here...
	mohawkFile->writeUint32BE(_fileTableAmount);
	for (uint32 i = 0; i < _fileTableAmount; i++) {
		mohawkFile->writeUint32BE(_fileTable[i].offset);
		mohawkFile->writeUint16BE(_fileTable[i].dataSize & 0xFFFF);
		mohawkFile->writeByte((_fileTable[i].dataSize >> 16) & 0xFF);

		// TODO: The following should be correct, but doesn't match up.
		//mohawkFile->writeByte((_fileTable[i].dataSize >> 24) & 0x07) || (_fileTable[i].flags & 0xF8));
		mohawkFile->writeByte(_fileTable[i].flags);

		mohawkFile->writeUint16BE(_fileTable[i].unk);
	}
}

void printUsage(const char *appName) {
	printf("Usage: %s [options] <mohawk archive> <binary resource file #1> [binary resource file #2] ...\n", appName);
	printf("Options :\n");
}

int main(int argc, char *argv[]) {
	int archiveArg;

	// Parse parameters
	for (archiveArg = 1; archiveArg < argc; archiveArg++) {
		Common::String current = Common::String(argv[archiveArg]);

		if(!current.hasPrefix("--"))
			break;

		// Decode options
		printf("Unknown argument : \"%s\"\n", argv[archiveArg]);
		printUsage(argv[0]);
		return 1;
	}

	if (argc == 1 || archiveArg == argc - 1) {
		printUsage(argv[0]);
		return 1;
	}

	Common::Filename mohawkFilename = Common::Filename(argv[archiveArg]);

	if (mohawkFilename.exists()) {
		printf ("File \'%s\' already exists!\n", argv[archiveArg]);
		return 1;
	}

	Common::File *mohawkFile = new Common::File(mohawkFilename, "wb+");
	if (!mohawkFile->isOpen()) {
		printf("Could not open Mohawk Archive \'%s\' for output\n", argv[archiveArg]);
		mohawkFile->close();
		return 1;
	}

	initMohawkArchive();

	// Allocate a buffer for the output
	outputBuffer = (byte *)malloc(MAX_BUF_SIZE);

	// Loop over remaining parameters, treating them as resource filenames
	for (int i = archiveArg + 1; i < argc; i++) {
		printf("Resource %d : \"%s\"\n", i - archiveArg - 1, argv[i]);

		Common::Filename resourceFile = Common::Filename(argv[i]);

		if (!resourceFile.exists()) {
			printf("Can not open resource file \"%s\"\n", argv[i]);
			return 1;
		}

		if (!resourceFile.hasExtension("bin")) {
			printf("Resource file \"%s\" not in raw binary format\n", argv[i]);
			return 1;
		}

		char resourceFilename[100];
		strncpy(resourceFilename, resourceFile.getName().c_str(), sizeof(resourceFilename));

		// Parse for file index, Resource Type, Resource Id and Name
		int fileIndex = atoi(Common::String(resourceFilename, 4).c_str());
		printf("fileIndex : %d\n", fileIndex);

		if (resourceFilename[4] != '_')
			printf("Filename format mismatch\n");

		unsigned int fileFlags;
		sscanf(resourceFilename+5, "%02x", &fileFlags);
		printf("fileFlags : 0x%02x\n", fileFlags);

		if (resourceFilename[7] != '_')
			printf("Filename format mismatch\n");

		Common::String resourceTag = Common::String(resourceFilename+8, 4);
		printf("resourceTag : \"%s\"\n", resourceTag.c_str());

		if (resourceFilename[12] != '_')
			printf("Filename format mismatch\n");

		int nameStart = 13;
		while (resourceFilename[nameStart] != '\0' && resourceFilename[nameStart] != '_')
			nameStart++;

		int resourceId = atoi(Common::String(resourceFilename+13, nameStart-12).c_str());
		printf("resourceId : %d\n", resourceId);

		Common::String resourceName;
		if (resourceFilename[nameStart] == '_')
			resourceName = Common::String(resourceFilename+nameStart+1, strlen(resourceFilename));
		printf("resourceName : \"%s\"\n", resourceName.c_str());

		Common::File *resourceIn = new Common::File(resourceFile, "rb");
		if (!resourceIn->isOpen()) {
			printf("Could not open file \"%s\"\n", resourceFile.getName().c_str());
			delete resourceIn;
			return 1;
		}

		addFileToMohawkArchive(fileIndex, (byte) fileFlags, resourceTag.c_str(), resourceId, resourceName.c_str(), resourceIn->size());

		resourceIn->close();
		delete resourceIn;
	}

	writeMohawkArchive(argc, argv, archiveArg, mohawkFile);

	printf("Done!\n");
	free(outputBuffer);
	mohawkFile->close();
	delete mohawkFile;
	return 0;
}
