#ifndef DATA_GRAPH_H
#define DATA_GRAPH_H

#include <boost/foreach.hpp>
#include <list>
#include <sstream>

#include <graph.h>
#include <instruction.h>


struct TestNode : public Node {

	int _id;

	TestNode(int id) : _id(id) {
	}

	~TestNode() {
	}

	uint32 address() {
		return _id;
	}

	std::string toString() {
		std::ostringstream ret;
		ret << _id;
		return ret.str();
	}
};


Node *node(ControlFlowGraph *g, int id) {
	BOOST_FOREACH (Node *u, g->_nodes)
		if (u->address() == id)
			return u;
	return 0;
}

ControlFlowGraph *makeGraph1() {
	ControlFlowGraph *g = new ControlFlowGraph;
	for (int i = 1; i <= 6; i++)
		g->_nodes.push_back(new TestNode(i));
	g->addEdge(node(g,1), node(g,2));
	g->addEdge(node(g,2), node(g,3));
	g->addEdge(node(g,2), node(g,5));
	g->addEdge(node(g,3), node(g,4));
	g->addEdge(node(g,4), node(g,2));
	g->addEdge(node(g,5), node(g,1));
	g->addEdge(node(g,5), node(g,6));
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph2() {
	ControlFlowGraph *g = new ControlFlowGraph;
	for (int i = 1; i <= 15; i++)
		g->_nodes.push_back(new TestNode(i));
	g->addEdge(node(g,1), node(g,2));
	g->addEdge(node(g,1), node(g,5));
	g->addEdge(node(g,2), node(g,3));
	g->addEdge(node(g,2), node(g,4));
	g->addEdge(node(g,3), node(g,5));
	g->addEdge(node(g,4), node(g,5));
	g->addEdge(node(g,5), node(g,6));
	g->addEdge(node(g,6), node(g,7));
	g->addEdge(node(g,6), node(g,11));
	g->addEdge(node(g,7), node(g,8));
	g->addEdge(node(g,8), node(g,9));
	g->addEdge(node(g,9), node(g,8));
	g->addEdge(node(g,9), node(g,10));
	g->addEdge(node(g,10), node(g,6));
	g->addEdge(node(g,11), node(g,12));
	g->addEdge(node(g,11), node(g,13));
	g->addEdge(node(g,12), node(g,13));
	g->addEdge(node(g,12), node(g,14));
	g->addEdge(node(g,13), node(g,14));
	g->addEdge(node(g,14), node(g,15));
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph3() {
	ControlFlowGraph *g = new ControlFlowGraph;
	for (int i = 1; i <= 3; i++)
		g->_nodes.push_back(new TestNode(i));
	g->addEdge(node(g,1), node(g,2));
	g->addEdge(node(g,1), node(g,3));
	g->addEdge(node(g,2), node(g,3));
	g->addEdge(node(g,3), node(g,2));
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph4() {
	ControlFlowGraph *g = new ControlFlowGraph;
	for (int i = 1; i <= 3; i++)
		g->_nodes.push_back(new TestNode(i));
	g->addEdge(node(g,1), node(g,2));
	g->addEdge(node(g,2), node(g,2));
	g->addEdge(node(g,2), node(g,3));
	g->addEdge(node(g,3), node(g,1));
	g->setEntry(1);
	return g;
}

#endif
