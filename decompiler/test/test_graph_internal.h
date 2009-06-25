#ifndef TEST_GRAPH_H
#define TEST_GRAPH_H

#define private public
#include <graph.h>
#include "data_graph.h"
#undef  private

#include <cxxtest/TestSuite.h>


class GraphTestSuite : public CxxTest::TestSuite {

	Graph<int> *g1, *g2, *g3, *g4;

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
};

#endif
