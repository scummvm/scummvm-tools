/* ScummVM Tools
* Copyright (C) 2010 The ScummVM project
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

#include <iostream>
#include <boost/program_options.hpp>

#include "objectFactory.h"

using namespace std;
using namespace boost::program_options;

int main(int argc, char** argv) {
	options_description visible("Options");
	visible.add_options()
		("help", "Produce this help message.")
		("engine", value<string>(), "Engine the script originates from.")
		("list", "List the supported engines.");

	options_description args("");
	args.add(visible).add_options()
		("inputfile", value<string>(), "Input file");

	positional_options_description filename;
	filename.add("inputfile", -1);	

	variables_map vm;
	try {
		store(command_line_parser(argc, argv).options(args).positional(filename).run(), vm);
		notify(vm);    
	} catch (std::exception& e) {
		std::cout << e.what();
	}
	
	if (vm.count("help") || !vm.count("inputfile")) {
		cout << "Usage: " << argv[0] << " [option...] file" << endl << endl;
		cout << args << "\n";
		return 1;
	}

	if (vm.count("list")) {
		//TODO
		cout << "Listing of engines is not yet implemented." << "\n";
		return 0;
	}

	if (!vm.count("engine")) {
		cout << "Engine must be specified." << "\n";
		return 2;
	} else {
		cout << "Engine selected: " <<	vm["engine"].as<string>() << "\n";
	}

	cout << "Input file is " << vm["inputfile"].as<string>() << "\n";

	//TODO: Process file

	return 0;
}
