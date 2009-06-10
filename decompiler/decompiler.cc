#include <cstdio>
#include <cstring>
#include <vector>

using namespace std;

#include "parser.h"
#include "instruction.h"
#include "cfg.h"

bool g_disasm = false;
bool g_blocks = false;
bool g_graph = false;

int main(int argc, char **argv) {
	int argno = 1;
	if (argno >= argc) {
		printf("decompiler [-disasm] [-blocks] [-graph] file.dmp\n");
		return 0;
	}
	while (true) {
		if (0 == strcmp("-disasm", argv[argno])) {
			g_disasm = true;
			argno++;
		} else if (0 == strcmp("-blocks", argv[argno])) {
			g_blocks = true;
			argno++;
		} else if (0 == strcmp("-graph", argv[argno])) {
			g_graph = true;
			argno++;
		} else
			break;
	}
	vector<Instruction*> v = Scumm6Parser().parseFile(argv[argno]);
	if (g_disasm) {
		for (unsigned i = 0; i < v.size(); i++) {
			if (i >= 1 && v[i]->_addr == v[i-1]->_addr)
				printf("         |           %s\n", v[i]->_description.c_str());
			else
				printf("(d) %04x | (r) %04x: %s\n", v[i]->_addr-8, v[i]->_addr, v[i]->_description.c_str());
		}
	}
	CFG *cfg = new CFG(v);
	if (g_blocks)
		cfg->printBBs();
	if (g_graph)
		cfg->printDot();
	return 0;
}
