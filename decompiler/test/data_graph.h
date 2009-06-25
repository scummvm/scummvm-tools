#ifndef DATA_GRAPH_H
#define DATA_GRAPH_H

#include <boost/foreach.hpp>


Graph<int>::Node *findNode(Graph<int> *g, int data) {
	BOOST_FOREACH (Graph<int>::Node *u, g->_nodes)
		if (u->_data == data)
			return u;
	return 0;
}

Graph<int> *makeGraph1() {
	Graph<int> *g = new Graph<int>;
	g->addNode(1);
	g->addNode(2);
	g->addNode(3);
	g->addNode(4);
	g->addNode(5);
	g->addNode(6);
	g->addEdge(findNode(g, 1), findNode(g, 2));
	g->addEdge(findNode(g, 2), findNode(g, 3));
	g->addEdge(findNode(g, 2), findNode(g, 5));
	g->addEdge(findNode(g, 3), findNode(g, 4));
	g->addEdge(findNode(g, 4), findNode(g, 2));
	g->addEdge(findNode(g, 5), findNode(g, 1));
	g->addEdge(findNode(g, 5), findNode(g, 6));
	return g;
}

Graph<int> *makeGraph2() {
	Graph<int> *g = new Graph<int>;
	g->addNode(1);
	g->addNode(2);
	g->addNode(3);
	g->addNode(4);
	g->addNode(5);
	g->addNode(6);
	g->addNode(7);
	g->addNode(8);
	g->addNode(9);
	g->addNode(10);
	g->addNode(11);
	g->addNode(12);
	g->addNode(13);
	g->addNode(14);
	g->addNode(15);
	g->addEdge(findNode(g, 1), findNode(g, 2));
	g->addEdge(findNode(g, 1), findNode(g, 5));
	g->addEdge(findNode(g, 2), findNode(g, 3));
	g->addEdge(findNode(g, 2), findNode(g, 4));
	g->addEdge(findNode(g, 3), findNode(g, 5));
	g->addEdge(findNode(g, 4), findNode(g, 5));
	g->addEdge(findNode(g, 5), findNode(g, 6));
	g->addEdge(findNode(g, 6), findNode(g, 7));
	g->addEdge(findNode(g, 6), findNode(g, 12));
	g->addEdge(findNode(g, 7), findNode(g, 8));
	g->addEdge(findNode(g, 7), findNode(g, 9));
	g->addEdge(findNode(g, 8), findNode(g, 9));
	g->addEdge(findNode(g, 8), findNode(g, 10));
	g->addEdge(findNode(g, 9), findNode(g, 10));
	g->addEdge(findNode(g, 10), findNode(g, 11));
	g->addEdge(findNode(g, 12), findNode(g, 13));
	g->addEdge(findNode(g, 13), findNode(g, 14));
	g->addEdge(findNode(g, 14), findNode(g, 13));
	g->addEdge(findNode(g, 14), findNode(g, 15));
	g->addEdge(findNode(g, 15), findNode(g, 6));
	return g;
}

Graph<int> *makeGraph3() {
	Graph<int> *g = new Graph<int>;
	g->addNode(1);
	g->addNode(2);
	g->addNode(3);
	g->addEdge(findNode(g, 1), findNode(g, 2));
	g->addEdge(findNode(g, 1), findNode(g, 3));
	g->addEdge(findNode(g, 2), findNode(g, 3));
	g->addEdge(findNode(g, 3), findNode(g, 2));
	return g;
}

Graph<int> *makeGraph4() {
	Graph<int> *g = new Graph<int>;
	g->addNode(1);
	g->addNode(2);
	g->addNode(3);
	g->addEdge(findNode(g, 1), findNode(g, 2));
	g->addEdge(findNode(g, 2), findNode(g, 2));
	g->addEdge(findNode(g, 2), findNode(g, 3));
	g->addEdge(findNode(g, 3), findNode(g, 1));
	return g;
}

#endif
