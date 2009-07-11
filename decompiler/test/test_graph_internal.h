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
		TS_ASSERT_EQUALS(node(ga,1)->_interval->address(), 1);
		for (int i = 2; i <= 6; i++)
			TS_ASSERT_EQUALS(node(ga,i)->_interval->address(), 2);

		gb->intervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(node(gb,i)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gb,6)->_interval->address(), 6);
		TS_ASSERT_EQUALS(node(gb,7)->_interval->address(), 6);
		TS_ASSERT_EQUALS(node(gb,8)->_interval->address(), 8);
		TS_ASSERT_EQUALS(node(gb,9)->_interval->address(), 8);
		TS_ASSERT_EQUALS(node(gb,10)->_interval->address(), 8);
		for (int i = 11; i <= 15; i++)
			TS_ASSERT_EQUALS(node(gb,i)->_interval->address(), 6);

		gc->intervals();
		TS_ASSERT_EQUALS(node(gc,1)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gc,2)->_interval->address(), 2);
		TS_ASSERT_EQUALS(node(gc,3)->_interval->address(), 3);

		gd->intervals();
		TS_ASSERT_EQUALS(node(gd,1)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gd,2)->_interval->address(), 2);
		TS_ASSERT_EQUALS(node(gd,3)->_interval->address(), 2);
	}


	void test_extendIntervals() {
		ga->intervals();
		ga->extendIntervals();
		for (int i = 1; i <= 6; i++)
			TS_ASSERT_EQUALS(node(ga,i)->_interval->address(), 1);

		gb->intervals();
		gb->extendIntervals();
		for (int i = 1; i <= 5; i++)
			TS_ASSERT_EQUALS(node(gb,i)->_interval->address(), 1);
		for (int i = 6; i <= 15; i++)
			TS_ASSERT_EQUALS(node(gb,i)->_interval->address(), 6);
		gb->extendIntervals();
		for (int i = 1; i <= 15; i++)
			TS_ASSERT_EQUALS(node(gb,i)->_interval->address(), 1);

		gc->intervals();
		gc->extendIntervals();
		TS_ASSERT_EQUALS(node(gc,1)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gc,2)->_interval->address(), 2);
		TS_ASSERT_EQUALS(node(gc,3)->_interval->address(), 3);

		gd->intervals();
		gd->extendIntervals();
		TS_ASSERT_EQUALS(node(gd,1)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gd,2)->_interval->address(), 1);
		TS_ASSERT_EQUALS(node(gd,3)->_interval->address(), 1);
	}


	void test_assignDominators() {
		ga->assignDominators();
		TS_ASSERT(!node(ga,1)->_dominator);
		TS_ASSERT_EQUALS(node(ga,2)->_dominator->address(), 1);
		TS_ASSERT_EQUALS(node(ga,3)->_dominator->address(), 2);
		TS_ASSERT_EQUALS(node(ga,4)->_dominator->address(), 3);
		TS_ASSERT_EQUALS(node(ga,5)->_dominator->address(), 2);
		TS_ASSERT_EQUALS(node(ga,6)->_dominator->address(), 5);

		gb->assignDominators();
		TS_ASSERT(!node(gb,1)->_dominator);
		TS_ASSERT_EQUALS(node(gb,2)->_dominator->address(), 1);
		TS_ASSERT_EQUALS(node(gb,3)->_dominator->address(), 2);
		TS_ASSERT_EQUALS(node(gb,4)->_dominator->address(), 2);
		TS_ASSERT_EQUALS(node(gb,5)->_dominator->address(), 1);
		TS_ASSERT_EQUALS(node(gb,6)->_dominator->address(), 5);
		TS_ASSERT_EQUALS(node(gb,7)->_dominator->address(), 6);
		TS_ASSERT_EQUALS(node(gb,8)->_dominator->address(), 7);
		TS_ASSERT_EQUALS(node(gb,9)->_dominator->address(), 8);
		TS_ASSERT_EQUALS(node(gb,10)->_dominator->address(), 9);
		TS_ASSERT_EQUALS(node(gb,11)->_dominator->address(), 6);
		TS_ASSERT_EQUALS(node(gb,12)->_dominator->address(), 11);
		TS_ASSERT_EQUALS(node(gb,13)->_dominator->address(), 11);
		TS_ASSERT_EQUALS(node(gb,14)->_dominator->address(), 11);
		TS_ASSERT_EQUALS(node(gb,15)->_dominator->address(), 14);

		gc->assignDominators();
		TS_ASSERT(!node(gc,1)->_dominator);
		TS_ASSERT_EQUALS(node(gc,2)->_dominator->address(), 1);
		TS_ASSERT_EQUALS(node(gc,3)->_dominator->address(), 1);

		gd->assignDominators();
		TS_ASSERT(!node(gd,1)->_dominator);
		TS_ASSERT_EQUALS(node(gd,2)->_dominator->address(), 1);
		TS_ASSERT_EQUALS(node(gd,3)->_dominator->address(), 2);
	}
};

#endif
