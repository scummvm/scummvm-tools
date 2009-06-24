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

	IntGraph g1, g2, g3;

public:

	void setUp() {
		g1 = makeGraph1();
		g2 = makeGraph2();
		g3 = makeGraph3();
	}

	// this tests internal, intermediate results, not part of the api
	void test_intervals() {
		g1.intervals();
		TS_ASSERT_EQUALS(findNode(g1, 1)->_interval->_data, findNode(g1, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g1, 2)->_interval->_data, findNode(g1, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g1, 3)->_interval->_data, findNode(g1, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g1, 4)->_interval->_data, findNode(g1, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g1, 5)->_interval->_data, findNode(g1, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g1, 6)->_interval->_data, findNode(g1, 2)->_data);
		g2.intervals();
		TS_ASSERT_EQUALS(findNode(g2, 1)->_interval->_data, findNode(g2, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 2)->_interval->_data, findNode(g2, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 3)->_interval->_data, findNode(g2, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 4)->_interval->_data, findNode(g2, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 5)->_interval->_data, findNode(g2, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 6)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 7)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 8)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 9)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 10)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 11)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 12)->_interval->_data, findNode(g2, 6)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 13)->_interval->_data, findNode(g2, 13)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 14)->_interval->_data, findNode(g2, 13)->_data);
		TS_ASSERT_EQUALS(findNode(g2, 15)->_interval->_data, findNode(g2, 13)->_data);
		g3.intervals();
		TS_ASSERT_EQUALS(findNode(g3, 1)->_interval->_data, findNode(g3, 1)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 2)->_interval->_data, findNode(g3, 2)->_data);
		TS_ASSERT_EQUALS(findNode(g3, 3)->_interval->_data, findNode(g3, 3)->_data);
	}

	// this tests internal, intermediate results, not part of the api
	void test_derive() {
		IntGraph g1d = g1.derive();
		TS_ASSERT_EQUALS(g1d._nodes.size(), 2);
		TS_ASSERT(findNode(g1d, 1));
		TS_ASSERT(findNode(g1d, 2));
		TS_ASSERT_EQUALS(outEdgesCount(g1d, 1), 1);
		TS_ASSERT_EQUALS(outEdgesCount(g1d, 2), 1);
		TS_ASSERT(pointsTo(g1d, 1, 2));
		TS_ASSERT(pointsTo(g1d, 1, 2));
		IntGraph g1dd = g1d.derive();
		TS_ASSERT_EQUALS(g1dd._nodes.size(), 1);
		TS_ASSERT(findNode(g1d, 1));
		TS_ASSERT_EQUALS(outEdgesCount(g1dd, 1), 0);
		IntGraph g2d = g2.derive();
		TS_ASSERT_EQUALS(g2d._nodes.size(), 3);
		TS_ASSERT(findNode(g2d, 1));
		TS_ASSERT(findNode(g2d, 6));
		TS_ASSERT(findNode(g2d, 13));
		TS_ASSERT_EQUALS(outEdgesCount(g2d, 1), 1);
		TS_ASSERT_EQUALS(outEdgesCount(g2d, 6), 1);
		TS_ASSERT_EQUALS(outEdgesCount(g2d, 13), 1);
		TS_ASSERT(pointsTo(g2d, 1, 6));
		TS_ASSERT(pointsTo(g2d, 6, 13));
		TS_ASSERT(pointsTo(g2d, 13, 6));
		IntGraph g3d = g3.derive();
		TS_ASSERT_EQUALS(g3d._nodes.size(), 3);
		TS_ASSERT(findNode(g3d, 1));
		TS_ASSERT(findNode(g3d, 2));
		TS_ASSERT(findNode(g3d, 3));
		TS_ASSERT_EQUALS(outEdgesCount(g3d, 1), 2);
		TS_ASSERT_EQUALS(outEdgesCount(g3d, 2), 1);
		TS_ASSERT_EQUALS(outEdgesCount(g3d, 3), 1);
		TS_ASSERT(pointsTo(g3d, 1, 2));
		TS_ASSERT(pointsTo(g3d, 1, 3));
		TS_ASSERT(pointsTo(g3d, 2, 3));
		TS_ASSERT(pointsTo(g3d, 3, 2));
	}

private:

	IntNode *findNode(IntGraph &g, int data) {
		foreach (IntNode *u, g._nodes)
			if (u->_data == data)
				return u;
		return 0;
	}

	int outEdgesCount(IntGraph &g, int data) {
		return findNode(g, data)->out().size();
	}

	bool pointsTo(IntGraph &g, int dataFrom, int dataTo) {
		foreach (IntNode *u, findNode(g, dataFrom)->out())
			if (u->_data == dataTo)
				return true;
		return false;
	}

	IntGraph makeGraph1() {
    	IntGraph g;;
		g.addNode(1);
		g.addNode(2);
		g.addNode(3);
		g.addNode(4);
		g.addNode(5);
		g.addNode(6);
		g.addEdge(findNode(g, 1), findNode(g, 2));
		g.addEdge(findNode(g, 2), findNode(g, 3));
		g.addEdge(findNode(g, 2), findNode(g, 5));
		g.addEdge(findNode(g, 3), findNode(g, 4));
		g.addEdge(findNode(g, 4), findNode(g, 2));
		g.addEdge(findNode(g, 5), findNode(g, 1));
		g.addEdge(findNode(g, 5), findNode(g, 6));
		return g;
	}

	IntGraph makeGraph2() {
		IntGraph g;
		g.addNode(1);
		g.addNode(2);
		g.addNode(3);
		g.addNode(4);
		g.addNode(5);
		g.addNode(6);
		g.addNode(7);
		g.addNode(8);
		g.addNode(9);
		g.addNode(10);
		g.addNode(11);
		g.addNode(12);
		g.addNode(13);
		g.addNode(14);
		g.addNode(15);
		g.addEdge(findNode(g, 1), findNode(g, 2));
		g.addEdge(findNode(g, 1), findNode(g, 5));
		g.addEdge(findNode(g, 2), findNode(g, 3));
		g.addEdge(findNode(g, 2), findNode(g, 4));
		g.addEdge(findNode(g, 3), findNode(g, 5));
		g.addEdge(findNode(g, 4), findNode(g, 5));
		g.addEdge(findNode(g, 5), findNode(g, 6));
		g.addEdge(findNode(g, 6), findNode(g, 7));
		g.addEdge(findNode(g, 6), findNode(g, 12));
		g.addEdge(findNode(g, 7), findNode(g, 8));
		g.addEdge(findNode(g, 7), findNode(g, 9));
		g.addEdge(findNode(g, 8), findNode(g, 9));
		g.addEdge(findNode(g, 8), findNode(g, 10));
		g.addEdge(findNode(g, 9), findNode(g, 10));
		g.addEdge(findNode(g, 10), findNode(g, 11));
		g.addEdge(findNode(g, 12), findNode(g, 13));
		g.addEdge(findNode(g, 13), findNode(g, 14));
		g.addEdge(findNode(g, 14), findNode(g, 13));
		g.addEdge(findNode(g, 14), findNode(g, 15));
		g.addEdge(findNode(g, 15), findNode(g, 6));
		return g;
	}

	IntGraph makeGraph3() {
		IntGraph g;
		g.addNode(1);
		g.addNode(2);
		g.addNode(3);
		g.addEdge(findNode(g, 1), findNode(g, 2));
		g.addEdge(findNode(g, 1), findNode(g, 3));
		g.addEdge(findNode(g, 2), findNode(g, 3));
		g.addEdge(findNode(g, 3), findNode(g, 2));
		return g;
	}
};
