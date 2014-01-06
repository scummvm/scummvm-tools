/* extract_fascination_cd - a tool for extracting .stk archives from a mode1/2048 Fascination CD image
 * Copyright (C) 2010  The ScummVM Team
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

#include "common/endian.h"
#include "common/file.h"
#include "common/util.h"

#include "extract_fascination_cd.h"


enum {
	STK_HEADER_ENTRY_SIZE = 22
};

struct STKFile {
	const char *stkFilename;
	const char *firstEntryName;
	bool  extracted;
} static stkFile[] = {
	{ "intro.stk", "INTRO1.TOT", false },
	{ "disk1.stk", "DOUCHE.TOT", false },
	{ "disk2.stk", "PLANQUE.TOT", false },
	{ "disk3.stk", "MELANGE.TOT", false },
};


ExtractFascinationCD::ExtractFascinationCD(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.iso";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from a Fascination ISO.";
	_helptext = "Usage: " + _name + " [-o outputdir] <infile>\n" + _shorthelp + "\n";
}

void ExtractFascinationCD::execute(void) {
	if (_outputPath.empty())
		_outputPath.setFullPath("./");

	// Open ISO file
	Common::File file(_inputPaths[0].path, "rb");
	assert(file.isOpen());
	uint32 fileSize = file.size();

	// Sanity check the file size
	if (fileSize > (16 * 1024 * 1024)) {
		error("'%s' is too large to be a Fascination mode1/2048 ISO", _inputPaths[0].path.c_str());
	}

	if (fileSize < (8 * 1024 * 1024)) {
		error("'%s' is too small to be a Fascination mode1/2048 ISO", _inputPaths[0].path.c_str());
	}

	if (fileSize % 2048) {
		error("'%s' doesn't appear to be a mode1/2048 ISO", _inputPaths[0].path.c_str());
	}

	// Load ISO file to memory. (Should only be ~10MB, and this simplifies the code)
	byte *data = new byte[fileSize];
	file.read_noThrow(data, fileSize);
	file.close();
	
	print("Loaded '%s' (%d bytes)\n", _inputPaths[0].path.c_str(), fileSize);
	
	for (uint32 i = 0; i < ARRAYSIZE(stkFile); i++) {
		// initialize curPos to start of file
		byte *curPos = data;	
	
		while (curPos < data + fileSize) {
			// search for known first entry of STK files
			if (!memcmp(curPos, stkFile[i].firstEntryName, strlen(stkFile[i].firstEntryName))) {
				byte *stkFileStart = curPos - 2;	// the actual STK start is 2 bytes prior
				uint16 numStkEntries = READ_LE_UINT16(stkFileStart); // read number of entries in STK file
				assert(numStkEntries > 0 && numStkEntries < 0xFF);
				
				// Determine length of file by adding offset and size of the last entry
				const uint32 lastEntrySize = READ_LE_UINT32(curPos + ((numStkEntries - 1) * STK_HEADER_ENTRY_SIZE) + 13);
				const uint32 lastEntryOffset = READ_LE_UINT32(curPos + ((numStkEntries - 1) * STK_HEADER_ENTRY_SIZE) + 17);
				const uint32 stkEntrySize = lastEntryOffset + lastEntrySize;

				print("Found '%s' at %x (size: %d).  Extracting...\n", stkFile[i].stkFilename, curPos - data - 2, stkEntrySize);
				
				// write STK file
				Common::File output;
				_outputPath.setFullName(stkFile[i].stkFilename);
				output.open(_outputPath, "wb");
				assert(output.isOpen());
				output.write(stkFileStart, stkEntrySize);
				output.close();
				stkFile[i].extracted = true;

				curPos += stkEntrySize;
			}
		
			curPos++;
		}
	}
	
	for (int i = 0; i < ARRAYSIZE(stkFile); i++) {
		if (!stkFile[i].extracted)
			error("A problem occurred: '%s' has NOT been extracted", stkFile[i].stkFilename);
	}

	delete[] data;
}


#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractFascinationCD fe(argv[0]);
	return fe.run(argc, argv);
}
#endif

