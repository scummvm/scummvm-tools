#ifndef GRAPH_H
#define GRAPH_H

#include <cassert>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>

#include "instruction.h"
#include "node.h"
#include "misc.h"


struct ControlFlowGraph : boost::noncopyable {

	Node *_entry;
	std::list<Node*> _nodes;
	std::list<ControlFlowGraph*> _subgraphs;
	std::map<address_t, BasicBlock*> _targets; // helps partitioning code into basic nodes

	ControlFlowGraph();
	~ControlFlowGraph();

	void addBasicBlocksFromScript(std::list<Instruction*>::iterator scriptBegin, std::list<Instruction*>::iterator scriptEnd);
	void addEdge(Node *from, Node *to);
	void assignDominators(); // after order
	void deleteNode(Node *node);
	void forgetNode(Node *node);
	std::string graphvizToString(const std::string &fontname="", int fontsize=0);
	void orderNodes();
	void removeJumpsToJumps();
	void removeUnreachableNodes(); // after order
	void replaceEdges(Node *from, Node *oldTo, Node *newTo);
	void setEntry(address_t entry);
	std::list< std::list<Node*> > stronglyConnectedComponents();
	ControlFlowGraph *yank(std::set<Node*> &body);

	void structureLoops(const std::list< std::list<Node*> > &components);

	// to be removed
	void assignIntervals();
	void extendIntervals();
	std::list<Node*> intervals();
	bool isReducible();
};

#endif
