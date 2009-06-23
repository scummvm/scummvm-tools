#include <cxxtest/TestSuite.h>
#include <graph.h>

#include <vector>
using namespace std;


typedef Graph<int> IntGraph;
typedef IntGraph::Node IntNode;


class GraphTestSuite : public CxxTest::TestSuite {

public:

	void test_intervals() {
		vector<IntNode*> nodes1;
		IntGraph *g1 = makeGraph1(nodes1);
		g1->intervals();
		TS_ASSERT_EQUALS(nodes1[0]->interval()->_data, nodes1[0]->_data);
		TS_ASSERT_EQUALS(nodes1[1]->interval()->_data, nodes1[1]->_data);
		TS_ASSERT_EQUALS(nodes1[2]->interval()->_data, nodes1[1]->_data);
		TS_ASSERT_EQUALS(nodes1[3]->interval()->_data, nodes1[1]->_data);
		TS_ASSERT_EQUALS(nodes1[4]->interval()->_data, nodes1[1]->_data);
		TS_ASSERT_EQUALS(nodes1[5]->interval()->_data, nodes1[1]->_data);
		vector<IntNode*> nodes2;
		IntGraph *g2 = makeGraph2(nodes2);
		g2->intervals();
		TS_ASSERT_EQUALS(nodes2[0]->interval()->_data, nodes2[0]->_data);
		TS_ASSERT_EQUALS(nodes2[1]->interval()->_data, nodes2[0]->_data);
		TS_ASSERT_EQUALS(nodes2[2]->interval()->_data, nodes2[0]->_data);
		TS_ASSERT_EQUALS(nodes2[3]->interval()->_data, nodes2[0]->_data);
		TS_ASSERT_EQUALS(nodes2[4]->interval()->_data, nodes2[0]->_data);
		TS_ASSERT_EQUALS(nodes2[5]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[6]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[7]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[8]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[9]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[10]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[11]->interval()->_data, nodes2[5]->_data);
		TS_ASSERT_EQUALS(nodes2[12]->interval()->_data, nodes2[12]->_data);
		TS_ASSERT_EQUALS(nodes2[13]->interval()->_data, nodes2[12]->_data);
		TS_ASSERT_EQUALS(nodes2[14]->interval()->_data, nodes2[12]->_data);
	}

private:

	IntGraph *makeGraph1(vector<IntNode*> &nodes) {
    	IntGraph *g = new IntGraph;
		nodes.push_back(g->addNode(0));
		nodes.push_back(g->addNode(1));
		nodes.push_back(g->addNode(2));
		nodes.push_back(g->addNode(3));
		nodes.push_back(g->addNode(4));
		nodes.push_back(g->addNode(5));
		g->addEdge(nodes[0], nodes[1]);
		g->addEdge(nodes[1], nodes[2]);
		g->addEdge(nodes[1], nodes[4]);
		g->addEdge(nodes[2], nodes[3]);
		g->addEdge(nodes[3], nodes[1]);
		g->addEdge(nodes[4], nodes[0]);
		g->addEdge(nodes[4], nodes[5]);
		return g;
	}

	IntGraph *makeGraph2(vector<IntNode*> &nodes) {
		IntGraph *g = new IntGraph;
		nodes.push_back(g->addNode(0));
		nodes.push_back(g->addNode(1));
		nodes.push_back(g->addNode(2));
		nodes.push_back(g->addNode(3));
		nodes.push_back(g->addNode(4));
		nodes.push_back(g->addNode(5));
		nodes.push_back(g->addNode(6));
		nodes.push_back(g->addNode(7));
		nodes.push_back(g->addNode(8));
		nodes.push_back(g->addNode(9));
		nodes.push_back(g->addNode(10));
		nodes.push_back(g->addNode(11));
		nodes.push_back(g->addNode(12));
		nodes.push_back(g->addNode(13));
		nodes.push_back(g->addNode(14));
		g->addEdge(nodes[0], nodes[1]);
		g->addEdge(nodes[0], nodes[4]);
		g->addEdge(nodes[1], nodes[2]);
		g->addEdge(nodes[1], nodes[3]);
		g->addEdge(nodes[2], nodes[4]);
		g->addEdge(nodes[3], nodes[4]);
		g->addEdge(nodes[4], nodes[5]);
		g->addEdge(nodes[5], nodes[6]);
		g->addEdge(nodes[5], nodes[11]);
		g->addEdge(nodes[6], nodes[7]);
		g->addEdge(nodes[6], nodes[8]);
		g->addEdge(nodes[7], nodes[8]);
		g->addEdge(nodes[7], nodes[9]);
		g->addEdge(nodes[8], nodes[9]);
		g->addEdge(nodes[9], nodes[10]);
		g->addEdge(nodes[11], nodes[12]);
		g->addEdge(nodes[12], nodes[13]);
		g->addEdge(nodes[13], nodes[12]);
		g->addEdge(nodes[13], nodes[14]);
		g->addEdge(nodes[14], nodes[5]);
		return g;
	}
};
