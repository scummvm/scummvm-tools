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

/* Extractor for Kyrandia .pak archives */

#include "extract_kyra.h"

#include "kyra_pak.h"
#include "kyra_ins.h"

ExtractKyra::ExtractKyra(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	extractAll = true;
	extractOne = false;
	isAmiga = false;
	isHoFInstaller = false;
	singleFilename = "";

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Extract data files from the The Legend of Kyrandia series of games.";
	_helptext =
		"Usage: " + getName() + " [-o output] [params] <archivefile>\n" +
		_shorthelp + "\n" +
		"Default output path is ./out/\n" +
		"Params:\n" +
		"-e <filename>     Extract only <filename> from the archive, will be extracted \n" +
		"                  into the current directory.\n" +
		"-x                Extract all files (default)\n" +
		"-a                Extract files from the Amiga .PAK files\n" +
		"-2                Extract files from HoF installer files\n";
}

void ExtractKyra::parseExtraArguments() {
	// Parse our own arguments
	while (!_arguments.empty()) {
		std::string arg = _arguments.front();
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

			if (_arguments.empty())
				error("No filename supplied to -n\nShould be used on the form: -n ALGAE.CPS -o out/ A_E.PAK");
			singleFilename = _arguments.front();
			_arguments.pop_front();
		} else {
			break;
		}
		_arguments.pop_front();
	}
}

void ExtractKyra::execute() {
	Common::Filename inputpath(_inputPaths[0].path);

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

