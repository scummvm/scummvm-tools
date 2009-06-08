#include <cstdio>
#include <vector>

using namespace std;

#include "parser.h"
#include "instruction.h"


int main(int argc, char **argv) {
	vector<Instruction*> v = Scumm6Parser().parseFile(argv[1]);
	for (unsigned i = 0; i < v.size(); i++)
		printf("(d) %04x | (r) %04x: %s\n", v[i]->_addr-8, v[i]->_addr, v[i]->_description.c_str());
	return 0;
}
