#include <iostream>

#include <boost/program_options.hpp>

#include "parser.h"
#include "cfg.h"

using namespace std;
using namespace boost::program_options;


variables_map parseArgs(int argc, char **argv) {
	variables_map vars;
	options_description visible("Allowed options");
	visible.add_options()
		("help", "this message")
		("disasm", "print disassembly and exit")
		("blocks", "print basic blocks and exit")
		("graph",  "print graph and exit")
		("derive", value<int>(), "find nth derivative");
	options_description options("Allowed options");
	options.add(visible).add_options()
		("inputfile", value<string>(), "input file");
	positional_options_description pos;
	pos.add("inputfile", 1);
	try {
		store(command_line_parser(argc, argv).options(options).positional(pos).run(), vars);
		notify(vars);
	} catch (error) {
	}
	if (vars.count("help") || !vars.count("inputfile")) {
		cout << argv[0] << " [option...] file" << endl << endl;
		cout << visible;
		exit(0);
	}
	return vars;
}

#include <sstream>

int main(int argc, char **argv) {
	variables_map vars = parseArgs(argc, argv);
	Script script(new Scumm6Parser, vars["inputfile"].as<string>().c_str());
	if (vars.count("disasm")) {
		for (size_t i = 0; i < script.size(); i++)
			script.print(cout, i);
		exit(0);
	}
	CFG cfg(script);
	if (vars.count("blocks")) {
		cfg.printBasicBlocks(cout);
		exit(0);
	}
	// cfg.removeJumpsToJumps();
	// cfg.removeDeadBlocks();
	cfg._graph.intervals();
	if (vars.count("graph")) {
		Graph<Block*> g = cfg._graph;
		cfg._graph = g;
		cfg.printDot(cout);
		exit(0);
	}
	if (vars.count("derive")) {
		Graph<Block*> g = cfg._graph;
		for (int i = 0; i < vars["derive"].as<int>(); i++)
			g = g.derive();
		cfg._graph = g; // FIXME: evil
		g.intervals();
		cfg.printDot(cout);
		exit(0);
	}
	return 0;
}
