#include <cstdio>
#include <cstring>
#include <vector>

using namespace std;

#include "parser.h"
#include "instruction.h"
#include "cfg.h"

bool g_disasm = false;
bool g_bbcuts = true;

int main(int argc, char **argv) {
	int argno = 1;
	if (argno >= argc) {
		printf("decompiler [-disasm] file.dmp\n");
		return 0;
	}
	if (0 == strcmp("-disasm", argv[argno])) {
		g_disasm = true;
		argno++;
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
	for (uint32 i = 0; i < cfg->_blocks.size(); i++)
		cfg->_blocks[i]->print(v);
	return 0;
}
