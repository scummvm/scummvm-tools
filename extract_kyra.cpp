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

#include "extract_kyra.h"

#include "kyra_pak.h"
#include "kyra_ins.h"

ExtractKyra::ExtractKyra(const std::string &name) : Tool(name) {
	extractAll = true;
	extractOne = false;
	isAmiga = false;
	isHoFInstaller = false;
	singleFilename = "";
	
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_helptext = 
		"Usage: " + _name + "[params] [-o output] <archivefile> [-o output]\n" +
		"Default output path is ./out/\n" +
		"nParams:\n" +
		"-e <filename>     Extract only <filename> from the archive, will be extracted \n" +
		"                  into the current directory.\n" +
		"-x                Extract all files (default)\n" +
		"-a                Extract files from the Amiga .PAK files\n" +
		"-2                Extract files from HoF installer files\n";
}

void ExtractKyra::parseExtraArguments() {
	// Parse our own arguments
	while(_arguments_parsed < _arguments.size()) {
		std::string arg = _arguments[_arguments_parsed];
		if (arg == "-x") {
			extractAll = true;
			extractOne = false;
		} else if (arg == "-a") {
			isAmiga = true;
		} else if (arg == "-2") {
			isHoFInstaller = true;
		} else if (arg == "-n") {
			extractOne = true;
			extractAll = false;

			++_arguments_parsed;

			if (_arguments_parsed >= _arguments.size()) {
				error("No filename supplied to -n\nShould be used on the form: %s -n ALGAE.CPS -o out/ A_E.PAK");
			} else {
				singleFilename = _arguments[_arguments_parsed];
			}
		} else {
			break;
		}
		++_arguments_parsed;
	}
}

void ExtractKyra::execute() {
	Filename inputpath(_inputPaths[0].path);

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
		extract->outputAllFiles(&_outputPath);
	} else if (extractOne) {
		inputpath.setFullName(singleFilename);
		extract->outputFileAs(singleFilename.c_str(), inputpath.getFullPath().c_str());
	} else {
		extract->drawFileList();
	}

	delete extract;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractKyra kyra(argv[0]);
	return kyra.run(argc, argv);
}
#endif

