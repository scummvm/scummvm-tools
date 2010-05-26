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

#include "disassembler.h"

namespace po = boost::program_options;

#define ENGINE(id, description, disasmClass) engines[std::string(id)] = description; disassemblerFactory.addEntry<disasmClass>(std::string(id));

int main(int argc, char** argv) {
	try	{
		std::map<std::string, std::string> engines;
		ObjectFactory<Disassembler> disassemblerFactory;

		po::options_description visible("Options");
		visible.add_options()
			("help", "Produce this help message.")
			("engine", po::value<std::string>(), "Engine the script originates from.")
			("list", "List the supported engines.")
			("dump-disassembly", po::value<std::string>(), "Dump the disassembly to a specified file.");

		po::options_description args("");
		args.add(visible).add_options()
			("input-file", po::value<std::string>(), "Input file");

		po::positional_options_description fileArg;
		fileArg.add("input-file", -1);	

		po::variables_map vm;
		try {
			po::store(po::command_line_parser(argc, argv).options(args).positional(fileArg).run(), vm);
			po::notify(vm);    
		} catch (std::exception& e) {
			std::cout << e.what();
		}
	
		if (vm.count("list")) {
			std::cout << "Available engines:" << "\n";

			std::map<std::string, std::string>::iterator it;
			for (it = engines.begin(); it != engines.end(); it++)
				std::cout << (*it).first << " " << (*it).second << "\n";
		
			return 0;
		}

		if (vm.count("help") || !vm.count("input-file")) {
			std::cout << "Usage: " << argv[0] << " [option...] file" << "\n";
			std::cout << args << "\n";
			return 1;
		}
	
		if (!vm.count("engine")) {
			std::cout << "Engine must be specified." << "\n";
			return 2;
		} else if (engines.find(vm["engine"].as<std::string>()) == engines.end()) {
			std::cout << "Unknown engine." << "\n";
			return 2;
		}

		std::string engine = vm["engine"].as<std::string>();
		std::string inputFile = vm["input-file"].as<std::string>();

		//TODO: Process file
		Disassembler* disassembler = disassemblerFactory.create(engine);
		disassembler->open(inputFile.c_str());

		std::vector<Instruction> insts = disassembler->disassemble();
		if (vm.count("dump-disassembly")) {
			disassembler->dumpDisassembly(vm["dump-disassembly"].as<std::string>().c_str());
		}

		delete disassembler;
	} catch (std::exception& e) {
		std::cout << e.what();
		return 3;
	}

	return 0;
}
