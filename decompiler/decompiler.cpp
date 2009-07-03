#include <iostream>

#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

#include "parser.h"
#include "graph.h"
#include "syntax.h"

using namespace std;
using namespace boost::program_options;


variables_map parseArgs(int argc, char **argv) {
	variables_map vars;
	options_description visible("Allowed options");
	visible.add_options()
		("help", "this message")
		("disasm", "print disassembly")
		("blocks", "print basic blocks")
		("graph-intervals", value<unsigned>(), "print arg-th graph intervals")
		("graph-struct", "print graph with marked structure information")
		("decompile", "print decompiled program and exit")
		("no-remove-jumps", "don't remove jumps-to-jumps")
		("fontname", value<string>()->default_value("Courier"), "font to use with dot output");
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

int main(int argc, char **argv) {
	variables_map vars = parseArgs(argc, argv);
	Script script(new Scumm6Parser, vars["inputfile"].as<string>().c_str());
	if (vars.count("disasm")) {
		foreach (Instruction *instruction, script._instructions)
			cout << instruction->toString();
		exit(0);
	}
	ControlFlowGraph cfg;
	cfg.addBlocksFromScript(script._instructions.begin(), script._instructions.end());
	cfg.setEntry(script._instructions.front()->_addr);
	if (vars.count("blocks")) {
		foreach (Block *block, cfg._blocks)
			cout << block->toString() << endl;
		exit(0);
	}
	if (!vars.count("no-remove-jumps"))
		cfg.removeJumpsToJumps();
	cfg.orderBlocks();
	cfg.removeUnreachableBlocks();
	if (vars.count("graph-intervals")) {
		cfg.assignIntervals();
		for (unsigned i = 0; i < vars["graph-intervals"].as<unsigned>(); i++)
			cfg.extendIntervals();
		cout << cfg.graphvizToString(vars["fontname"].as<string>());
		exit(0);
	}
	cfg.loopStruct();
	if (vars.count("graph-struct")) {
		cout << cfg.graphvizToString(vars["fontname"].as<string>());
		exit(0);
	}
	if (vars.count("decompile")) {
		foreach (Statement *stmt, buildAbstractSyntaxTree(cfg))
			cout << stmt->toString(0);
		exit(0);
	}
	return 0;
}
