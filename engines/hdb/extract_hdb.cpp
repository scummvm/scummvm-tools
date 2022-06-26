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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extract_hdb.h"
#include <zlib.h>


ExtractHDB::ExtractHDB(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract Hyperspace Delivery Boy! archive files.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] <inputfile>\n";

	_compressed = false;
}

void ExtractHDB::execute() {
	Common::Filename filename = _inputPaths[0].path;

	if (!openMPC(filename))
		error("Unable to open %s", filename.getFullName().c_str());

	Common::File fOut;
	for (Common::Array<MPCEntry *>::iterator it = _dir.begin(); it != _dir.end(); ++it) {
		byte *buffer = (byte *)malloc((*it)->length);

		_mpcFile.seek((*it)->offset, SEEK_SET);

		_mpcFile.read_noThrow(buffer, (*it)->length);

		_outputPath.setFullName((*it)->filename);

		fOut.open(_outputPath, "wb");

		if (_compressed) {
			byte *buffer2 = (byte *)calloc((*it)->ulength, 1);
			unsigned long len = (*it)->ulength;

			print("... decompressing %s", (*it)->filename);

			if (uncompress(buffer2, &len, buffer, (*it)->length) != Z_OK) {
				print("Error uncompressing file %s", (*it)->filename);

				fOut.write(buffer, (*it)->length);
			} else if (len != (*it)->ulength) {
				print("Size mismatch for %s: %d <> %d", (*it)->filename, len, (*it)->ulength);

				fOut.write(buffer, (*it)->length);
			} else {
				fOut.write(buffer2, (*it)->ulength);
			}

			free(buffer2);
		} else {
			print("... %s", (*it)->filename);
			fOut.write(buffer, (*it)->length);
		}

		fOut.close();

		free(buffer);
	}

	_mpcFile.close();
}

InspectionMatch ExtractHDB::inspectInput(const Common::Filename &filename) {
	// Accept either 'vol.cat' or 'vol.dat'
	std::string file = filename.getFullName();
	if (
		scumm_stricmp(file.c_str(), "hyperspace.mpc") == 0 ||
		scumm_stricmp(file.c_str(), "hyperspace.msd") == 0 ||
		scumm_stricmp(file.c_str(), "hyperdemo.mpc") == 0 ||
		scumm_stricmp(file.c_str(), "hyperdemo.msd") == 0
	)
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

bool ExtractHDB::openMPC(Common::Filename &filename) {
	uint32 offset;

	_mpcFile.open(filename, "rb");
	if (!_mpcFile.isOpen()) {
		error("FileMan::openMPC(): Error reading the MSD/MPC file %s", filename.getFullName().c_str());
		return false;
	}

	_dataHeader.id = _mpcFile.readUint32BE();

	if (_dataHeader.id == 'MPCC') {
		print("COMPRESSED MPC FILE");
		return false;
	} else if (_dataHeader.id == 'MPCU') {
		print("Unpacking uncompressed MPC file...");
		// we're fine
	} else if (_dataHeader.id == 'MSDC') {
		print("Unpacking compressed MSD file...");
		_compressed = true;
		// we're fine
	} else if (_dataHeader.id == 'MSDU') {
		print("UNCOMPRESSED MSD FILE");
		return false;
	} else {
		error("Invalid MPC/MSD File.");
		return false;
	}

	// read the directory
	offset = _mpcFile.readUint32LE();
	_mpcFile.seek((int32)offset, SEEK_SET);

	// Note: The MPC archive format assumes the offset to be uint32,
	// but Common::File::seek() takes the offset as int32.

	_dataHeader.dirSize = _mpcFile.readUint32LE();

	print("MPCU: Read %d entries", _dataHeader.dirSize);

	for (uint32 fileIndex = 0; fileIndex < _dataHeader.dirSize; fileIndex++) {
		MPCEntry *dirEntry = new MPCEntry();

		for (int i = 0; i < 64; i++) {
			dirEntry->filename[i] = _mpcFile.readByte();
		}

		dirEntry->offset = _mpcFile.readUint32LE();
		dirEntry->length = _mpcFile.readUint32LE();
		dirEntry->ulength = _mpcFile.readUint32LE();
		dirEntry->type = (DataType)_mpcFile.readUint32LE();

		_dir.push_back(dirEntry);
	}

	return true;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractHDB hdb(argv[0]);
	return hdb.run(argc, argv);
}
#endif
