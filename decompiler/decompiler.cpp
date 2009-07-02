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
		("disasm", "print disassembly and exit")
		("blocks", "print basic blocks and exit")
		("graph",  "print graph and exit")
		("decompile", "print decompiled program and exit")
		("fontname", value<string>()->default_value("Courier"), "font to use with graphical output");
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
	cfg.removeJumpsToJumps();
	cfg.orderBlocks();
	cfg.removeUnreachableBlocks();
	cfg.loopStruct();
	if (vars.count("graph")) {
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
