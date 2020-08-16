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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extract_cryo.h"


ExtractCryo::ExtractCryo(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract Lost Eden archive files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] <inputfile>\n";
}

void ExtractCryo::execute() {
	Common::Filename filename = _inputPaths[0].path;

	if (!openDAT(filename))
		error("Unable to open %s", filename.getFullName().c_str());

	Common::File fOut;
	for (DATIterator it = _dir.begin(); it != _dir.end(); ++it) {
		byte *buffer = (byte *)malloc((*it)->size);
		_datFile.seek((*it)->offset, SEEK_SET);
		_datFile.read_noThrow(buffer, (*it)->size);

		_outputPath.setFullName((*it)->filename);
		print("... %s", (*it)->filename);

		fOut.open(_outputPath, "wb");
		fOut.write(buffer, (*it)->size);
		fOut.close();

		free(buffer);
	}

	_datFile.close();
}

InspectionMatch ExtractCryo::inspectInput(const Common::Filename &filename) {
	// TODO: DUNE.DAT
	std::string file = filename.getFullName();
	if (
		scumm_stricmp(file.c_str(), "EDEN.DAT") == 0
	)
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

bool ExtractCryo::openDAT(Common::Filename &filename) {
	_datFile.open(filename, "rb");
	if (!_datFile.isOpen()) {
		error("FileMan::openDAT(): Error reading the DAT file %s", filename.getFullName().c_str());
		return false;
	}

	uint16 entries = _datFile.readUint16LE();

	for (uint16 fileIndex = 0; fileIndex < entries; fileIndex++) {
		DATEntry *dirEntry = new DATEntry();

		for (int i = 0; i < 16; i++) {
			dirEntry->filename[i] = _datFile.readByte();
		}

		dirEntry->size = _datFile.readUint32LE();
		dirEntry->offset = _datFile.readUint32LE();
		dirEntry->flag = _datFile.readByte();

		if (dirEntry->offset != 0 || dirEntry->size != 0) {
			_dir.push_back(dirEntry);
		}
	}

	return true;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCryo cryo(argv[0]);
	return cryo.run(argc, argv);
}
#endif
