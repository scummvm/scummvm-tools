/* extract_kyra - Extractor for Kyrandia .pak archives
 * Copyright (C) 2004  Johannes Schickel
 * Copyright (C) 2004-2008  The ScummVM Team
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

#include "kyra_pak.h"
#include "kyra_ins.h"

int export_main(extract_kyra)(int argc, char *argv[]) {
	const char *helptext = "\n"
		"Usage: %s [params] [-o output] <archivefile> [-o output]\n"
		"Default output path is ./out/\n"
		"nParams:\n"
		"-e <filename>     Extract only <filename> from the archive, will be extracted \n"
		"                  into the current directory.\n"
		"-x                Extract all files (default)\n"
		"-a                Extract files from the Amiga .PAK files\n"
		"-2                Extract files from HoF installer files\n";

	int first_arg = 1;
	int last_arg = argc - 1;

	bool extractAll = true, extractOne = false, isAmiga = false, isHoFInstaller = false;
	char singleFilename[256] = "";

	Filename outpath, inputpath;

	// Check if we should display some helpful text
	parseHelpArguments(argv, argc, helptext);

	int param = first_arg;

	// Parse our own arguments
	for (; param < last_arg; ++param) {
		if (strcmp(argv[param], "-x") == 0) {
			extractAll = true;
			extractOne = false;
		} else if (strcmp(argv[param], "-a") == 0) {
			isAmiga = true;
		} else if (strcmp(argv[param], "-2") == 0) {
			isHoFInstaller = true;
		} else if (strcmp(argv[param], "-n") == 0) {
			extractOne = true;
			extractAll = false;

			++param;

			if (param >= last_arg) {
				error("No filename supplied to -n\nShould be used on the form: %s -n ALGAE.CPS -o out/ A_E.PAK");
			} else {
				strcpy(singleFilename, argv[param]);
			}
		} else {
			break;
		}
	}

	// Parse output argument
	if (parseOutputDirectoryArguments(&outpath, argv, argc, param))
		param += 2;
	else if (parseOutputDirectoryArguments(&outpath, argv, argc, argc - 2))
		last_arg -= 1;
	else
		outpath.setFullPath("out/");

	// Extract files
	if (first_arg != last_arg)
		error("Expected only one input file.");

	inputpath.setFullPath(argv[param]);
	Extractor *extract = 0;
	if (isHoFInstaller) {
		extract = new HoFInstaller(inputpath.getFullPath().c_str());
	} else {
		PAKFile *myfile = new PAKFile;
		if (!myfile->loadFile(inputpath.getFullPath().c_str(), isAmiga)) {
			delete myfile;
			error("Couldn't load file '%s'", inputpath.getFullPath().c_str());
		}

		extract = myfile;
	}

	// Everything has been decided, do the actual extraction
	if (extractAll) {
		extract->outputAllFiles(&outpath);
	} else if (extractOne) {
		inputpath.setFullName(singleFilename);
		extract->outputFileAs(singleFilename, inputpath.getFullPath().c_str());
	} else {
		extract->drawFileList();
	}

	delete extract;
	return 0;
}

#if defined(UNIX) && defined(EXPORT_MAIN)
int main(int argc, char *argv[]) __attribute__((weak));
int main(int argc, char *argv[]) {
	return export_main(extract_kyra)(argc, argv);
}
#endif

