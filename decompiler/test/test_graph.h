#include <cxxtest/TestSuite.h>

// TODO: move interal tests to a separate file
#define private public
#include <graph.h>
#undef private

#include <list>
#include <vector>
using namespace std;

#include <boost/foreach.hpp>
#ifndef foreach
#define foreach BOOST_FOREACH
#endif

typedef Graph<int> IntGraph;
typedef IntGraph::Node IntNode;


class GraphTestSuite : public CxxTest::TestSuite {

	IntGraph *g1, *g2, *g3, *g4;

public:

	void setUp() {
		g1 = makeGraph1();
		g2 = makeGraph2();
		g3 = makeGraph3();
		g4 = makeGraph4();
	}

	// this tests internal, intermediate results, not part of the api
	void test_intervals() {
		g1->intervals();
		TS_ASSERT_EQUALS(findNode(g1, 1)->interval()->_data, findNode(g1, 1)->_data);
		for (int i = 2; i <= 6; i++)
			TS_ASSERT_EQUALS(findNode(g1, i)->interval()->_data, findNode(g1, 2)->_data);
		g2->intervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 1)->_data);
		for (int i = 6; i <= 12; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 6)->_data);
		for (int i = 13; i <= 15; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 13)->_data);
		g3->intervals();
		TS_ASSERT_EQUALS(findNode(g3, 1)->interval()->_data, findNode(g3, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 2)->interval()->_data, findNode(g3, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 3)->interval()->_data, findNode(g3, 3)->_data);
		g4->intervals();
		TS_ASSERT_EQUALS(findNode(g4, 1)->interval()->_data, findNode(g4, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g4, 2)->interval()->_data, findNode(g4, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g4, 3)->interval()->_data, findNode(g4, 2)->_data);
	}

	void test_extendIntervals() {
		g1->intervals();
		g1->extendIntervals();
		for (int i = 1; i <= 6; i++)
			TS_ASSERT_EQUALS(findNode(g1, i)->interval()->_data, findNode(g1, 1)->_data);
		g2->intervals();
		g2->extendIntervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 1)->_data);
		for (int i = 6; i <= 15; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 6)->_data);
		g2->extendIntervals();
		for (int i = 1; i <= 15; i++)
			TS_ASSERT_EQUALS(findNode(g2, i)->interval()->_data, findNode(g2, 1)->_data);
		g3->intervals();
		g3->extendIntervals();
		TS_ASSERT_EQUALS(findNode(g3, 1)->interval()->_data, findNode(g3, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 2)->interval()->_data, findNode(g3, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 3)->interval()->_data, findNode(g3, 3)->_data);
		g4->intervals();
		g4->extendIntervals();
		TS_ASSERT_EQUALS(findNode(g4, 1)->interval()->_data, findNode(g4, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g4, 2)->interval()->_data, findNode(g4, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g4, 3)->interval()->_data, findNode(g4, 1)->_data);
	}

private:

	IntNode *findNode(IntGraph *g, int data) {
		foreach (IntNode *u, g->_nodes)
			if (u->_data == data)
				return u;
		return 0;
	}

	IntGraph *makeGraph1() {
    	IntGraph *g = new IntGraph;
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

	IntGraph *makeGraph2() {
		IntGraph *g = new IntGraph;
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

	IntGraph *makeGraph3() {
		IntGraph *g = new IntGraph;
		g->addNode(1);
		g->addNode(2);
		g->addNode(3);
		g->addEdge(findNode(g, 1), findNode(g, 2));
		g->addEdge(findNode(g, 1), findNode(g, 3));
		g->addEdge(findNode(g, 2), findNode(g, 3));
		g->addEdge(findNode(g, 3), findNode(g, 2));
		return g;
	}

	IntGraph *makeGraph4() {
		IntGraph *g = new IntGraph;
		g->addNode(1);
		g->addNode(2);
		g->addNode(3);
		g->addEdge(findNode(g, 1), findNode(g, 2));
		g->addEdge(findNode(g, 2), findNode(g, 2));
		g->addEdge(findNode(g, 2), findNode(g, 3));
		g->addEdge(findNode(g, 3), findNode(g, 1));
		return g;
	}
};
