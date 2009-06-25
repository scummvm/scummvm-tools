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
		("derive", value<int>()->default_value(0), "find arg-th order intervals");
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
		Graph<Block*> &g = cfg._graph;
		g.markReversePostOrder();
		g.intervals();
		for (int i = 0; i < vars["derive"].as<int>(); i++)
			g.extendIntervals();
		g.loopStruct();
		cfg.printDot(cout);
		exit(0);
	}
	return 0;
}
