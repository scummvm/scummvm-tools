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

#include "objectFactory.h"

#include "disassembler.h"
#include "engine.h"
#include "instruction.h"

#include "control_flow.h"

#include "groovie/engine.h"
#include "kyra/engine.h"
#include "scummv6/engine.h"

#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include <boost/program_options.hpp>
#include <boost/graph/graphviz.hpp>

namespace po = boost::program_options;

#define ENGINE(id, description, engineClass) engines[std::string(id)] = description; engineFactory.addEntry<engineClass>(std::string(id));

int main(int argc, char** argv) {
	try {
		std::map<std::string, std::string> engines;
		ObjectFactory<std::string, Engine> engineFactory;

		ENGINE("groovie", "Groovie", Groovie::GroovieEngine);
		ENGINE("kyra2", "Legend of Kyrandia: Hand of Fate", Kyra::Kyra2Engine);
		ENGINE("scummv6", "SCUMM v6", Scumm::v6::Scummv6Engine);

		po::options_description visible("Options");
		visible.add_options()
			("help,h", "Produce this help message.")
			("engine,e", po::value<std::string>(), "Engine the script originates from.")
			("list,l", "List the supported engines.")
			("dump-disassembly,d", po::value<std::string>()->implicit_value(""), "Dump the disassembly to a file. Leave out filename to output to stdout.")
			("dump-graph,g", po::value<std::string>()->implicit_value(""), "Output the control flow graph in dot format to a file. Leave out filename to output to stdout.")
			("only-disassembly,D", "Stops after disassembly. Implies -d.")
			("only-graph,G", "Stops after control flow graph has been generated. Implies -g.")
			("show-unreachable,u", "Show the address and contents of unreachable groups in the script.")
			("variant,v", po::value<std::string>()->default_value(""), "Tell the engine that the script is from a specific variant. To see a list of variants supported by a specific engine, use the -h option and the -e option together.")
			("no-stack-effect,s", "Leave out the stack effect when printing raw instructions.");

		po::options_description args("");
		args.add(visible).add_options()
			("input-file", po::value<std::string>(), "Input file");

		po::positional_options_description fileArg;
		fileArg.add("input-file", -1);

		po::variables_map vm;
		try {
			// FIXME: If specified as the last parameter before the input file name, -d currently requires a filename to specified. -d must be specified earlier than that if outputting to stdout.
			po::store(po::command_line_parser(argc, argv).options(args).positional(fileArg).run(), vm);
			po::notify(vm);
		} catch (std::exception& e) {
			std::cout << e.what() << std::endl;
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
			std::cout << visible << "\n";
			if (vm.count("engine") && engines.find(vm["engine"].as<std::string>()) != engines.end()) {
				Engine *engine = engineFactory.create(vm["engine"].as<std::string>());
				std::vector<std::string> variants;
				engine->getVariants(variants);
				if (variants.empty()) {
					std::cout << engines[vm["engine"].as<std::string>()] << " does not use variants.\n";
				} else {
					std::cout << "Supported variants for " << engines[vm["engine"].as<std::string>()] << ":\n";
					for (std::vector<std::string>::iterator i = variants.begin(); i != variants.end(); ++i) {
						std::cout << "  " << *i << "\n";
					}
				}
				delete engine;
				std::cout << "\n";
			}
			std::cout << "Note: If outputting to stdout, -d or -g must NOT be specified immediately before the input file.\n";
			return 1;
		}

		if (!vm.count("engine")) {
			std::cout << "Engine must be specified.\n";
			return 2;
		} else if (engines.find(vm["engine"].as<std::string>()) == engines.end()) {
			std::cout << "Unknown engine.\n";
			return 2;
		}

		if (vm.count("no-stack-effect")) {
			setOutputStackEffect(false);
		}

		Engine *engine = engineFactory.create(vm["engine"].as<std::string>());
		engine->_variant = vm["variant"].as<std::string>();
		std::string inputFile = vm["input-file"].as<std::string>();

		// Disassembly
		InstVec insts;
		Disassembler *disassembler = engine->getDisassembler(insts);
		disassembler->open(inputFile.c_str());

		disassembler->disassemble();
		if (vm.count("dump-disassembly")) {
			std::streambuf *buf;
			std::ofstream of;

			if (vm["dump-disassembly"].as<std::string>() != "") {
				of.open(vm["dump-disassembly"].as<std::string>().c_str());
				buf = of.rdbuf();
			} else {
				buf = std::cout.rdbuf();
			}
			std::ostream out(buf);
			disassembler->dumpDisassembly(out);
		}

		if (!engine->supportsCodeFlow() || vm.count("only-disassembly") || insts.empty()) {
			if (!vm.count("dump-disassembly")) {
				disassembler->dumpDisassembly(std::cout);
			}
			delete disassembler;
			delete engine;
			return 0;
		}

		delete disassembler;

		// Control flow analysis
		ControlFlow *cf = new ControlFlow(insts, engine);
		cf->createGroups();
		Graph g = cf->analyze();

		if (vm.count("dump-graph")) {
			std::streambuf *buf;
			std::ofstream of;

			if (vm["dump-graph"].as<std::string>() != "") {
				of.open(vm["dump-graph"].as<std::string>().c_str());
				buf = of.rdbuf();
			} else {
				buf = std::cout.rdbuf();
			}
			std::ostream out(buf);
			boost::write_graphviz(out, g, boost::make_label_writer(get(boost::vertex_name, g)), boost::makeArrowheadWriter(get(boost::edge_attribute, g)), GraphProperties(engine, g));
		}

		if (!engine->supportsCodeGen() || vm.count("only-graph")) {
			if (!vm.count("dump-graph")) {
				boost::write_graphviz(std::cout, g, boost::make_label_writer(get(boost::vertex_name, g)), boost::makeArrowheadWriter(get(boost::edge_attribute, g)), GraphProperties(engine, g));
			}
			delete cf;
			delete engine;
			return 0;
		}

		// Post-processing of CFG
		engine->postCFG(insts, g);

		// Code generation
		CodeGenerator *cg = engine->getCodeGenerator(std::cout);
		cg->generate(g);

		if (vm.count("show-unreachable")) {
			std::vector<GroupPtr> unreachable;
			VertexRange vr = boost::vertices(g);
			for (VertexIterator v = vr.first; v != vr.second; ++v)
			{
				GroupPtr gr = boost::get(boost::vertex_name, g, *v);
				if (gr->_stackLevel == -1)
					unreachable.push_back(gr);
			}
			if (!unreachable.empty()) {
				for (size_t i = 0; i < unreachable.size(); i++) {
					if (i == 0) {
						if (unreachable.size() == 1)
							std::cout << boost::format("\n%d unreachable group detected.\n") % unreachable.size();
						else
							std::cout << boost::format("\n%d unreachable groups detected.\n") % unreachable.size();
					}
					std::cout << "Group " << (i + 1) << ":\n";
					ConstInstIterator inst = unreachable[i]->_start;
					do {
						std::cout << *inst;
					} while (inst++ != unreachable[i]->_end);
					std::cout << "----------\n";
				}
			}
		}

		// Free memory
		delete cf;
		delete cg;
		delete engine;
	} catch (UnknownOpcodeException &e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return 3;
	} catch (std::exception &e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return 4;
	}

	return 0;
}
