#include <cstdio>

#include "graph2.h"

#include <string>
#include <sstream>
#include <iostream>
using namespace std;

string printer(int i) {
	stringstream ret;
	ret << "node \\ containing \"" << i << "\"";
	return ret.str();
};

typedef Graph<int>::Node node;

int main() {
	Graph<int> g;
	node a = g.addNode(0);
	g.addEdge(a, g.addNode(1));
	g.addEdge(a, g.addNode(2));
	g.addEdge(a, g.addNode(0));
	g.addEdge(a, g.addNode(0));
	*a = 6;
	foreach (node u, a.out()) 
		*u = 12;
	cout << g.print(printer) << endl;
	return 0;
}
