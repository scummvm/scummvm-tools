#include <iostream>
#include <map>
using namespace std;

#include "graph.h"

typedef Graph<int> graph;
typedef graph::Node *node;


string printer(int i) {
	ostringstream ret;
	ret << i;
	return ret.str();
}

int main1() {
	graph g;
	node n1 = g.addNode(1);
	node n2 = g.addNode(2);
	node n3 = g.addNode(3);
	node n4 = g.addNode(4);
	node n5 = g.addNode(5);
	node n6 = g.addNode(6);
	g.addEdge(n1, n2);
	g.addEdge(n2, n3);
	g.addEdge(n2, n5);
	g.addEdge(n3, n4);
	g.addEdge(n4, n2);
	g.addEdge(n5, n1);
	g.addEdge(n5, n6);
	g.assignIntervals(n1);
	cout << g.graphvizPrint(printer);
	return 0;
}

int main() {
	graph g;
	node n1 = g.addNode(1);
	node n2 = g.addNode(2);
	node n3 = g.addNode(3);
	node n4 = g.addNode(4);
	node n5 = g.addNode(5);
	node n6 = g.addNode(6);
	node n7 = g.addNode(7);
	node n8 = g.addNode(8);
	node n9 = g.addNode(9);
	node n10 = g.addNode(10);
	node n11 = g.addNode(11);
	node n12 = g.addNode(12);
	node n13 = g.addNode(13);
	node n14 = g.addNode(14);
	node n15 = g.addNode(15);
	g.addEdge(n1, n2);
	g.addEdge(n1, n5);
	g.addEdge(n2, n3);
	g.addEdge(n2, n4);
	g.addEdge(n3, n5);
	g.addEdge(n4, n5);
	g.addEdge(n5, n6);
	g.addEdge(n6, n7);
	g.addEdge(n6, n12);
	g.addEdge(n7, n8);
	g.addEdge(n7, n9);
	g.addEdge(n8, n9);
	g.addEdge(n8, n10);
	g.addEdge(n9, n10);
	g.addEdge(n10, n11);
	g.addEdge(n12, n13);
	g.addEdge(n13, n14);
	g.addEdge(n14, n13);
	g.addEdge(n14, n15);
	g.addEdge(n15, n6);
	g.assignIntervals(n1);
	cout << g.graphvizPrint(printer);
	g.test(0);
	return 0;
}
