/* extract_scumm_mac - Split one-big-file Macintosh game data into seperate .00x files for ScummVM
 * Copyright (C) 2001-2003  Casey Hutchinson
 * Copyright (C) 2004-2006  The ScummVM Team
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

#include "extract_scumm_mac.h"

#include <algorithm>

ExtractScummMac::ExtractScummMac(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from the single data file of later LucasArts Macintosh SCUMM games.";
	_helptext =
		"\nUsage: " + getName() + " [-o <output dir> = out/] <file>\n" +
		_shorthelp + "\n";
}

InspectionMatch ExtractScummMac::inspectInput(const Common::Filename &filename) {
	std::string name = filename.getFullName();
	std::transform(name.begin(), name.end(), name.begin(), tolower);
	std::string::size_type pos = name.find(" data");
	if (pos == name.length() - 5) // True if the file name ends with " Data"
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

void ExtractScummMac::execute() {
	Common::Filename inPath(_inputPaths[0].path);
	Common::Filename outPath(_outputPath);

	if (outPath.empty())
		outPath.setFullPath("./");

	Common::File ifp(inPath, "rb");

	// Get the length of the data file to use for consistency checks
	uint32 dataFileLength = ifp.size();

	// Read offset and length to the file records
	uint32 fileRecordOffset = ifp.readUint32BE();
	uint32 fileRecordLength = ifp.readUint32BE();

	// Do a quick check to make sure the offset and length are good
	if (fileRecordOffset + fileRecordLength > dataFileLength)
		error("File records out of bounds");

	// Do a little consistancy check on fileRecordLength
	if (fileRecordLength % 0x28)
		error("File record length not multiple of 40");

	// Extract the files
	for (uint32 i = 0; i < fileRecordLength; i += 0x28) {
		// read a file record
		ifp.seek(fileRecordOffset + i, SEEK_SET);

		uint32 fileOffset = ifp.readUint32BE();
		uint32 fileLength = ifp.readUint32BE();

		char fileName[0x21];
		ifp.read_throwsOnError(fileName, 0x20);
		fileName[0x20] = 0;

		if (!fileName[0])
			error("File has no name");

		print("Extracting %s...", fileName);

		// Consistency check. make sure the file data is in the file
		if (fileOffset + fileLength > dataFileLength)
			error("File out of bounds");

		// Write a file
		ifp.seek(fileOffset, SEEK_SET);

		outPath.setFullName(fileName);
		Common::File ofp(outPath, "wb");

		byte *buf = new byte[fileLength];
		ifp.read_throwsOnError(buf, fileLength);
		ofp.write(buf, fileLength);
		delete[] buf;
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	return export_main(extract_scumm_mac)(argc, argv);
}
#endif
