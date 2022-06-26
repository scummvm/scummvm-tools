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

/* Mohawk file constructor */

#include "common/scummsys.h"
#include "common/array.h"
#include "common/file.h"
#include "common/str.h"
#include "common/util.h"

#include "engines/mohawk/archive.h"
#include "engines/mohawk/utils.h"

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

struct ResourceFile {
	Common::Filename filename;

	uint32 index;
	int flags;
	Common::String typeTagStr;
	uint32 typeTag;
	uint16 id;
	Common::String name;
	uint32 size;
};

Common::Array<TypeConstruct> _types;
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

void initMohawkArchive() {
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
}

void updateTypeTableOffsets() {
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

void addFileToMohawkArchive(const ResourceFile &file) {
	int16 typeIndex = getTypeIndex(file.typeTag);
	if (typeIndex == -1) {
		// Add New Type to typeTable
		addTypeToMohawkArchive(file.typeTagStr.c_str());
		typeIndex = getTypeIndex(file.typeTag);
	} else {
		if (getIdIndex(typeIndex, file.id) != -1) {
			printf("Error : Duplicate Resource Type \'%s\' Id %d\n", file.typeTagStr.c_str(), file.id);
			return;
		}
	}

	// Update Resource Table
	TypeConstructResourceEntries newTypeResource;
	newTypeResource.id = file.id;
	newTypeResource.index = file.index + 1;

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
	if (!file.name.empty()) {
		TypeConstructNameEntries newTypeName;

		if (_types[typeIndex].nameTable.entries.empty())
			newTypeName.offset = 0;
		else
			newTypeName.offset = _types[typeIndex].nameTable.entries.back().offset + _types[typeIndex].nameTable.entries.back().name.size() + 1;

		newTypeName.index = file.index + 1;
		newTypeName.name = file.name;

		_fileSize += 4 + file.name.size() + 1;
		_rsrc.filesize += 4 + file.name.size() + 1;
		_rsrc.file_table_offset += 4 + file.name.size() + 1;
		_typeTable.name_offset += 4;

		_types[typeIndex].nameTable.num++;
		_types[typeIndex].nameTable.entries.push_back(newTypeName);

		updateTypeTableOffsets();
	}

	// TODO: Error if duplicate file table ids
	// TODO: Error if missing file table ids

	// Add file data parameters to fileTable
	uint32 fileItemOffset = 36;
	for (uint16 i = 0; i < _fileTable.size(); i++)
		fileItemOffset += _fileTable[i].dataSize;

	FileTable fileTableItem;
	fileTableItem.offset = fileItemOffset;
	fileTableItem.dataSize = file.size;
	fileTableItem.flags = file.flags;
	fileTableItem.unk = 0;

	_fileSize += 10;
	_rsrc.filesize += 10;
	_rsrc.file_table_size += 10;

	_fileTable.push_back(fileTableItem);

	_fileSize += file.size;
	_rsrc.filesize += file.size;
	_rsrc.abs_offset += file.size;
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

// Original Archiver seems to treat '_' as greater than all alphanumerics which is not
// the same as ASCII ordering and thus String '>' operator. This corrects for this.
bool stringGreaterThan(Common::String *test, Common::String *ref) {
	// Normal Function if just ASCII ordering..
	//return *test > *ref;

	// Function with correction for underscore ordered after Alphanumerics...
	for (uint16 i = 0; i < MIN(test->size(), ref->size()); i++) {
		if ((*test)[i] == '_' && (*ref)[i] != '_')
			return true;
		else if ((*test)[i] > (*ref)[i])
			return true;
		else if ((*test)[i] < (*ref)[i])
			return false;
	}
	if(test->size() > ref->size())
		return true;
	else
		return false;
}

void sortNameTable(Common::Array<TypeConstructNameEntries> *tempNameTable, uint16 i) {
	uint16 k, j;

	// Insert in Type Name Table ordered Alphabetically by Name
	for (k = 0; k < _types[i].nameTable.num; k++) {
		for (j = 0; j < tempNameTable->size(); j++) {
			if (stringGreaterThan(&((*tempNameTable)[j].name), &(_types[i].nameTable.entries[k].name)))
				break;
		}
		tempNameTable->insert_at(j, _types[i].nameTable.entries[k]);
	}

	assert(tempNameTable->size() == _types[i].nameTable.num);
}

void rewriteMovieOffsets(Common::File *resourceIn, Common::File *mohawkFile) {
	adjustQuickTimeAtomOffsets(resourceIn, resourceIn->size(), mohawkFile->pos(), mohawkFile);
}

void writeMohawkArchive(const Common::Array<ResourceFile> &files, Common::File *mohawkFile) {
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

	for (uint i = 0; i < files.size(); i++) {
		const ResourceFile &file = files[i];
		Common::File *resourceIn = new Common::File(file.filename, "rb");
		if (!resourceIn->isOpen()) {
			printf("Could not open file \"%s\"\n", file.filename.getName().c_str());
			delete resourceIn;
			return;
		}

		if (file.typeTag == ID_TMOV) {
			rewriteMovieOffsets(resourceIn, mohawkFile);
		} else {
			copyBytes(resourceIn, mohawkFile, resourceIn->size());
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
		// _types[i].nameTable.entries sorted into alphabetical order by name into tempNameTable
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
	mohawkFile->writeUint32BE(_fileTable.size());
	for (uint32 i = 0; i < _fileTable.size(); i++) {
		mohawkFile->writeUint32BE(_fileTable[i].offset);
		mohawkFile->writeUint16BE(_fileTable[i].dataSize & 0xFFFF);
		mohawkFile->writeByte((_fileTable[i].dataSize >> 16) & 0xFF);
		mohawkFile->writeByte(((_fileTable[i].dataSize >> 24) & 0x07) | (_fileTable[i].flags & 0xF8));
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

	Common::Array<ResourceFile> inputFiles;

	// Loop over remaining parameters, treating them as resource filenames
	for (int i = archiveArg + 1; i < argc; i++) {
		printf("Resource %d : \"%s\"\n", i - archiveArg - 1, argv[i]);

		ResourceFile file;
		file.filename = Common::Filename(argv[i]);

		if (!file.filename.exists()) {
			printf("Can not open resource file \"%s\"\n", argv[i]);
			return 1;
		}

		if (!file.filename.hasExtension("bin")) {
			printf("Resource file \"%s\" not in raw binary format\n", argv[i]);
			return 1;
		}

		char resourceFilename[100];
		strncpy(resourceFilename, file.filename.getName().c_str(), sizeof(resourceFilename));

		// Parse for file index, Resource Type, Resource Id and Name
		file.index = atoi(Common::String(resourceFilename, 4).c_str());
		printf("fileIndex : %d\n", file.index);

		if (resourceFilename[4] != '_')
			printf("Filename format mismatch\n");

		sscanf(resourceFilename+5, "%02x", &file.flags);
		printf("fileFlags : 0x%02x\n", file.flags);

		if (resourceFilename[7] != '_')
			printf("Filename format mismatch\n");

		file.typeTagStr = Common::String(resourceFilename+8, 4);
		file.typeTag = string2tag(file.typeTagStr.c_str());
		printf("resourceTag : \"%s\"\n", file.typeTagStr.c_str());

		if (resourceFilename[12] != '_')
			printf("Filename format mismatch\n");

		int nameStart = 13;
		while (resourceFilename[nameStart] != '\0' && resourceFilename[nameStart] != '_')
			nameStart++;

		file.id = atoi(Common::String(resourceFilename+13, nameStart-12).c_str());
		printf("resourceId : %d\n", file.id);

		if (resourceFilename[nameStart] == '_') {
			file.name = Common::String(resourceFilename+nameStart+1, resourceFilename+strlen(resourceFilename));

			for (uint j = 0; j < file.name.size(); j++) {
				//printf("DEBUG: j: %d resourceName[j]: %c\n", j, resourceName[j]);
				if (file.name[j] == '\\') {
					if (j+1 < file.name.size() && file.name[j+1] == '\\') {
						//printf("\tUnpadded \ at %d\n", j);
						file.name.deleteChar(j);
					} else {
						//printf("\tReplaced \ at %d with %c\n", j, '/');
						file.name.setChar('/', j);
					}
				}
			}
		}
		printf("resourceName : \"%s\"\n", file.name.c_str());

		Common::File resourceIn(file.filename, "rb");
		if (!resourceIn.isOpen()) {
			printf("Could not open file \"%s\"\n", file.filename.getName().c_str());
			return 1;
		}
		file.size = resourceIn.size();
		resourceIn.close();

		inputFiles.push_back(file);
	}

	initMohawkArchive();

	for (uint i = 0; i < inputFiles.size(); i++) {
		addFileToMohawkArchive(inputFiles[i]);
	}

	writeMohawkArchive(inputFiles, mohawkFile);

	printf("Done!\n");
	mohawkFile->close();
	delete mohawkFile;
	return 0;
}
