#include <iostream>
#include <vector>

using namespace std;

#include "parser.h"
#include "instruction.h"


int main(int argc, char **argv) {
	vector<Instruction*> v = Scumm6Parser().parseFile(argv[1]);
	for (unsigned i = 0; i < v.size(); i++)
		cout << v[i]->_addr << ": " << v[i]->_description << endl;
	return 0;
}
