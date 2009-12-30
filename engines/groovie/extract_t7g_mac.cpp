/* extract_t7g_mac - Extractor for the Mac version of The 7th Guest
 * Copyright (C) 2008-2009 The ScummVM project
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

// Resource fork format taken from:
// http://developer.apple.com/documentation/mac/MoreToolbox/MoreToolbox-99.html

#include "extract_t7g_mac.h"

#include "common/endian.h"

#define offsetResFork 128
static uint32 offsetResourceData;

ExtractT7GMac::ExtractT7GMac(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from the The 7th Guest Macintosh data file.";
	_helptext =
		"Usage: " + getName() + " [params] [-o outputdir] <archivefile>\n" +
		_shorthelp + "\n";
}

std::string ExtractT7GMac::readString(Common::File &infile) {
	byte len = infile.readByte();
	char *name = new char[len + 1];
	infile.read_throwsOnError(name, len);
	name[len] = 0;
	return name;
}

void ExtractT7GMac::dumpResource(Common::File &infile, std::string name) {
	// Show the resource details
	uint32 fileSize = infile.readUint32BE();
	print("  \"%s\" (%d bytes)", name.c_str(), fileSize);

	// Read the resource contents
	byte *buf = new byte[fileSize];

	try {
		// Dump the resource to the output file
		_outputPath.setFullName(name);
		Common::File out(_outputPath, "wb");
		infile.read_throwsOnError(buf, fileSize);
		out.write(buf, fileSize);
	} catch (...) {
		delete[] buf;
		throw;
	}

	// Free the resource memory
	delete[] buf;
}

void ExtractT7GMac::handleReferenceList(Common::File &infile, uint32 offsetRefList, uint16 numRes, uint32 offsetResNames) {
	for (int i = 0; i < numRes; i++) {
		infile.seek(offsetRefList + 12 * i + 2, SEEK_SET);
		uint32 offsetResName = offsetResNames + infile.readUint16BE();
		uint32 offsetResData = offsetResourceData + (infile.readUint32BE() & 0xFFFFFF);

		// Read the resource name
		infile.seek(offsetResName, SEEK_SET);

		std::string name = readString(infile);

		// Dump the resource
		infile.seek(offsetResData, SEEK_SET);
		dumpResource(infile, name);
	}
}

void ExtractT7GMac::execute() {
	Common::File infile(_inputPaths[0].path, "rb");

	if (_outputPath.empty())
		_outputPath.setFullPath("./");

	// Read the resource fork header
	infile.seek(offsetResFork, SEEK_SET);

	offsetResourceData = offsetResFork + infile.readUint32BE();
	uint32 offsetResMap = offsetResFork + infile.readUint32BE();

	// Read the resource map
	infile.seek(offsetResMap + 24, SEEK_SET);
	uint32 offsetResTypes = offsetResMap + infile.readUint16BE();
	uint32 offsetResNames = offsetResMap + infile.readUint16BE();

	// Handle the resource types
	infile.seek(offsetResTypes, SEEK_SET);

	uint16 numResTypes = infile.readUint16BE() + 1;
	char resType[5];
	resType[4] = 0;
	for (uint16 i = 0; i < numResTypes; i++) {
		infile.seek(offsetResTypes + 2 + 8 * i, SEEK_SET);

		// Read the resource type name
		infile.read_throwsOnError(resType, 4);
		switch (READ_BE_UINT32(resType)) {
			case MKID_BE('csnd'):
			case MKID_BE('snd '):
			case MKID_BE('Midi'):
			case MKID_BE('cmid'):
			//case MKID_BE('SMOD'):
			case MKID_BE('SONG'):
			case MKID_BE('INST'):
			case MKID_BE('T7GM'):
			{
				print("Extracting \"%s\" resources\n", resType);
				uint16 numRes = infile.readUint16BE();
				uint32 offsetRefList = offsetResTypes + infile.readUint16BE();

				handleReferenceList(infile, offsetRefList, numRes, offsetResNames);
				break;
			}
			default:
				print("Skipping \"%s\" resources\n", resType);
				break;
		}
	}
}
