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

#include "common/file.h"
#include "common/str.h"

#include "extract_asylum.h"

// based on scummvm/engines/asylum/respack.cpp

#define MAKE_RESOURCE(pack, index) (ResourceId)((((pack) << 16) + 0x80000000) + (unsigned int)(index))

ExtractAsylum::ExtractAsylum(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract Sanitarium resource packs.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] [-R <resourceindex>] <resourcepack>\n";

	_supportsProgressBar = true;

	_packId = 0;
	_resInd = -1;
}

InspectionMatch ExtractAsylum::inspectInput(const Common::Filename &filename) {
	std::string file = filename.getFullName();
	if (!strncmp(file.c_str(), "RES.", 4) || !strncmp(file.c_str(), "MUS.", 4))
		return IMATCH_PERFECT;

	return IMATCH_AWFUL;
}

void ExtractAsylum::parseExtraArguments() {
	if (!_arguments.empty()) {
		if (_arguments.front() == "-R") {
			_arguments.pop_front();
			_resInd = atoi(_arguments.front().c_str());
			_arguments.pop_front();
		}
	}
}

void ExtractAsylum::execute() {
	initPack(_inputPaths[0].path);

	if (_resInd >= 0) {
		dumpResource(_resInd);
	} else {
		for (int i = 0; i < _resources.size(); i++) {
			dumpResource(i);
			updateProgress(i, _resources.size());
		}
	}
}

void ExtractAsylum::initPack(const Common::Filename &filename) {
	_packFile.open(filename, "rb");
	if (!_packFile.isOpen())
		error("[ExtractAsylum::initPack] Could not open resource file: %s", filename.getFullName().c_str());

	_packId = atoi(strchr(filename.getFullName().c_str(), '.') + 1);

	uint32 entryCount = _packFile.readUint32LE();
	_resources.resize(entryCount);

	uint32 prevOffset = _packFile.readUint32LE();
	uint32 nextOffset = 0;

	for (uint32 i = 0; i < entryCount; i++) {
		ResourceEntry entry;
		entry.offset = prevOffset;

		// Read the offset of the next entry to determine the size of this one
		nextOffset = (i < entryCount - 1) ? _packFile.readUint32LE() : (uint32)_packFile.size();
		entry.size = (nextOffset > 0) ? nextOffset - prevOffset : (uint32)_packFile.size() - prevOffset;
		entry.data = NULL;

		_resources[i] = entry;

		prevOffset = nextOffset;
	}
}

void ExtractAsylum::dumpResource(int resInd) {
	struct ResourceEntry *entry = &_resources[resInd];
	ResourceId resourceId = MAKE_RESOURCE(_packId, resInd);

	if (!entry->offset)
		return;

	entry->data = new byte[entry->size];
	if (entry->data == NULL) {
		warning("[ExtractAsylum::dumpResource] Could not allocate memory for resource 0x%X", resourceId);
		return;
	}

	_packFile.seek(entry->offset, SEEK_SET);
	_packFile.read_noThrow(entry->data, entry->size);

	const char *extension;
	if (!strncmp((char *)entry->data, "RIFF", 4)) {
		extension = "WAV";
	} else if (!strncmp((char *)entry->data, "D3GR", 4)) {
		if (entry->size == 800)
			extension = "PAL";
		else
			extension = "D3GR";
	} else if (*(entry->data + entry->size - 1) == '\0') {
		extension = "TXT";
	} else {
		extension = "BIN";
	}

	Common::File fout;
	_outputPath.setFullName(Common::String::format("%X.%s", resourceId, extension).c_str());
	fout.open(_outputPath, "wb");
	fout.write(entry->data, entry->size);
	fout.close();

	delete[] entry->data;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractAsylum asylum(argv[0]);
	return asylum.run(argc, argv);
}
#endif
