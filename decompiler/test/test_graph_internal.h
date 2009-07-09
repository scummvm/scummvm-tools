#ifndef TEST_GRAPH_H
#define TEST_GRAPH_H

#define private public
#include <graph.h>
#include "data_graph.h"
#undef  private

#include <cxxtest/TestSuite.h>


class GraphInternalTestSuite : public CxxTest::TestSuite {

	ControlFlowGraph *ga, *gb, *gc, *gd;

public:

	void setUp() {
		ga = makeGraph1();
		gb = makeGraph2();
		gc = makeGraph3();
		gd = makeGraph4();
	}


	void test_intervals() {
		ga->intervals();
		TS_ASSERT_EQUALS(addr(node(ga,1)->_interval), 1);
		for (int i = 2; i <= 6; i++)
			TS_ASSERT_EQUALS(addr(node(ga,i)->_interval), 2);

		gb->intervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(addr(node(gb,i)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gb,6)->_interval), 6);
		TS_ASSERT_EQUALS(addr(node(gb,7)->_interval), 6);
		TS_ASSERT_EQUALS(addr(node(gb,8)->_interval), 8);
		TS_ASSERT_EQUALS(addr(node(gb,9)->_interval), 8);
		TS_ASSERT_EQUALS(addr(node(gb,10)->_interval), 8);
		for (int i = 11; i <= 15; i++)
			TS_ASSERT_EQUALS(addr(node(gb,i)->_interval), 6);

		gc->intervals();
		TS_ASSERT_EQUALS(addr(node(gc,1)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gc,2)->_interval), 2);
		TS_ASSERT_EQUALS(addr(node(gc,3)->_interval), 3);

		gd->intervals();
		TS_ASSERT_EQUALS(addr(node(gd,1)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gd,2)->_interval), 2);
		TS_ASSERT_EQUALS(addr(node(gd,3)->_interval), 2);
	}


	void test_extendIntervals() {
		ga->intervals();
		ga->extendIntervals();
		for (int i = 1; i <= 6; i++)
			TS_ASSERT_EQUALS(addr(node(ga,i)->_interval), 1);

		gb->intervals();
		gb->extendIntervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(addr(node(gb,i)->_interval), 1);
		for (int i = 6; i <= 15; i++)
			TS_ASSERT_EQUALS(addr(node(gb,i)->_interval), 6);
		gb->extendIntervals();
		for (int i = 1; i <= 15; i++)
			TS_ASSERT_EQUALS(addr(node(gb,i)->_interval), 1);

		gc->intervals();
		gc->extendIntervals();
		TS_ASSERT_EQUALS(addr(node(gc,1)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gc,2)->_interval), 2);
		TS_ASSERT_EQUALS(addr(node(gc,3)->_interval), 3);

		gd->intervals();
		gd->extendIntervals();
		TS_ASSERT_EQUALS(addr(node(gd,1)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gd,2)->_interval), 1);
		TS_ASSERT_EQUALS(addr(node(gd,3)->_interval), 1);
	}


	void test_assignDominators() {
		ga->orderNodes();
		ga->assignDominators();
		TS_ASSERT(!node(ga,1)->_dominator);
		TS_ASSERT_EQUALS(addr(node(ga,2)->_dominator), 1);
		TS_ASSERT_EQUALS(addr(node(ga,3)->_dominator), 2);
		TS_ASSERT_EQUALS(addr(node(ga,4)->_dominator), 3);
		TS_ASSERT_EQUALS(addr(node(ga,5)->_dominator), 2);
		TS_ASSERT_EQUALS(addr(node(ga,6)->_dominator), 5);

		gb->orderNodes();
		gb->assignDominators();
		TS_ASSERT(!node(gb,1)->_dominator);
		TS_ASSERT_EQUALS(addr(node(gb,2)->_dominator), 1);
		TS_ASSERT_EQUALS(addr(node(gb,3)->_dominator), 2);
		TS_ASSERT_EQUALS(addr(node(gb,4)->_dominator), 2);
		TS_ASSERT_EQUALS(addr(node(gb,5)->_dominator), 1);
		TS_ASSERT_EQUALS(addr(node(gb,6)->_dominator), 5);
		TS_ASSERT_EQUALS(addr(node(gb,7)->_dominator), 6);
		TS_ASSERT_EQUALS(addr(node(gb,8)->_dominator), 7);
		TS_ASSERT_EQUALS(addr(node(gb,9)->_dominator), 8);
		TS_ASSERT_EQUALS(addr(node(gb,10)->_dominator), 9);
		TS_ASSERT_EQUALS(addr(node(gb,11)->_dominator), 6);
		TS_ASSERT_EQUALS(addr(node(gb,12)->_dominator), 11);
		TS_ASSERT_EQUALS(addr(node(gb,13)->_dominator), 11);
		TS_ASSERT_EQUALS(addr(node(gb,14)->_dominator), 11);
		TS_ASSERT_EQUALS(addr(node(gb,15)->_dominator), 14);

		gc->orderNodes();
		gc->assignDominators();
		TS_ASSERT(!node(gc,1)->_dominator);
		TS_ASSERT_EQUALS(addr(node(gc,2)->_dominator), 1);
		TS_ASSERT_EQUALS(addr(node(gc,3)->_dominator), 1);

		gd->orderNodes();
		gd->assignDominators();
		TS_ASSERT(!node(gd,1)->_dominator);
		TS_ASSERT_EQUALS(addr(node(gd,2)->_dominator), 1);
		TS_ASSERT_EQUALS(addr(node(gd,3)->_dominator), 2);
	}
};

#endif
