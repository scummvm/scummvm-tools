/* extract_scumm_mac - Split one-big-file Macintosh game data into seperate .00x files for ScummVM
 * Copyright (C) 2001-2003  Casey Hutchinson
 * Copyright (C) 2004-2006  The ScummVM Team
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

#include "extract_scumm_mac.h"

#include <algorithm>

/* this makes extract_scumm_mac convert extracted file names to lower case */
#define CHANGECASE

ExtractScummMac::ExtractScummMac(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data from Lucas Art CDs, sometimes the CD appears to only hold an \n"
		"application but there is a hidden data file.";
	_helptext =
		"\nUsage: " + getName() + " [-o <output dir> = out/] <file>\n" +
		_shorthelp + "\n";
}

InspectionMatch ExtractScummMac::inspectInput(const Filename &filename) {
	std::string name = filename.getFullName();
	std::transform(name.begin(), name.end(), name.begin(), tolower);
	std::string::size_type pos = name.find("data");
	if (pos == name.length() - 4) // True if the file name ends with "Data"
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

void ExtractScummMac::execute() {
	unsigned long file_record_off, file_record_len;
	unsigned long file_off, file_len;
	unsigned long data_file_len;
	char file_name[0x20];
	char *buf;
	unsigned long i;
	int j;

	Filename inpath(_inputPaths[0].path);
	Filename outpath(_outputPath);

	if (outpath.empty())
		outpath.setFullPath("out/");

	File ifp(inpath, "rb");

	/* Get the length of the data file to use for consistency checks */
	data_file_len = ifp.size();

	/* Read offset and length to the file records */
	file_record_off = ifp.readUint32BE();
	file_record_len = ifp.readUint32BE();

	/* Do a quick check to make sure the offset and length are good */
	if (file_record_off + file_record_len > data_file_len) {
		error("\'%s\'. file records out of bounds.", inpath.getFullPath().c_str());
	}

	/* Do a little consistancy check on file_record_length */
	if (file_record_len % 0x28) {
		error("\'%s\'. file record length not multiple of 40.", inpath.getFullPath().c_str());
	}

	/* Extract the files */
	for (i = 0; i < file_record_len; i += 0x28) {
		/* read a file record */
		ifp.seek(file_record_off + i, SEEK_SET);

		file_off = ifp.readUint32BE();
		file_len = ifp.readUint32BE();
		ifp.read(file_name, 0x20, 1);

		if (!file_name[0])
			error("\'%s\'. file has no name.", inpath.getFullPath().c_str());
		
		print("extracting \'%s\'", file_name);

		/* For convenience compatibility with scummvm (and case sensitive
		 * file systems) change the file name to lowercase.
		 *
		 * if i ever add the ability to pass flags on the command
		 * line, i will make this optional, but i really don't
		 * see the point to bothering
		 */
		for (j = 0; j < 0x20; j++) {
			if (!file_name[j]) {
				break;
			}

#ifdef CHANGECASE
			file_name[j] = tolower(file_name[j]);
#endif
		}

		if (j == 0x20) {
			file_name[0x1f] = 0;
			warning("\'%s\'. file name not null terminated.\n", file_name);
			print("data file \'%s\' may be not a file extract_scumm_mac can extract.\n", inpath.getFullPath().c_str());
		}

		print(", saving as \'%s\'\n", file_name);

		/* Consistency check. make sure the file data is in the file */
		if (file_off + file_len > data_file_len) {
			error("\'%s\'. file out of bounds.", inpath.getFullPath().c_str());
		}

		/* Write a file */
		ifp.seek(file_off, SEEK_SET);

		outpath.setFullName(file_name);
		File ofp(outpath, "wb");

		if (!(buf = (char *)malloc(file_len))) {
			error("Could not allocate %ld bytes of memory.", file_len);
		}

		ifp.read(buf, 1, file_len);
		ofp.write(buf, 1, file_len);
		free(buf);
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	return export_main(extract_scumm_mac)(argc, argv);
}
#endif
