#include <cstdio>

#include "graph.h"

#include <string>
#include <sstream>
#include <iostream>

#include <functional>

using namespace std;

#include <boost/shared_ptr.hpp>


struct BBlock {
	int a, b;
	virtual string print() {
		return string("something");
	}
};

struct BBlock2 : public BBlock {
	int c;
	virtual string print() {
		return string("[0000] push(1);\n[0003] not();\n       if ()\n           goto 0;");
	}
};

typedef boost::shared_ptr<BBlock> block_t;
typedef Graph<block_t> graph_t;
typedef graph_t::Node *node_t;

string printer(block_t b) {
	return b->print();
};

int main() {
	graph_t g;
	node_t a = g.addNode(block_t(new BBlock()));
	g.addEdge(a, g.addNode(block_t(new BBlock2())));
	cout << g.graphvizPrint(printer) << endl;
	//	cout << "===" << endl;
	//	cout << a->_data->print() << endl;
	return 0;
}
