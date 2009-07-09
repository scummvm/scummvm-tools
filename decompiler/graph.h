#ifndef GRAPH_H
#define GRAPH_H

#include <cassert>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>

#include "instruction.h"
#include "block.h"
#include "misc.h"


struct ControlFlowGraph : boost::noncopyable {

	Block *_entry;
	std::list<Block*> _blocks;
	std::map<address_t, Block*> _targets; // helps partitioning code into basic blocks

	ControlFlowGraph();
	~ControlFlowGraph();

	Block *addBlock(std::list<Instruction*>::iterator first, std::list<Instruction*>::iterator last);
	void addBlocksFromScript(std::list<Instruction*>::iterator scriptBegin, std::list<Instruction*>::iterator scriptEnd);
	void addEdge(Block *from, Block *to);
	void assignComponents(); // after order
	void assignDominators(); // after order
	std::list<Block*> components();
	std::string graphvizToString(const std::string &fontname="", int fontsize=0);
	void orderBlocks();
	void removeJumpsToJumps();
	void removeUnreachableBlocks(); // after order
	void replaceEdges(Block *from, Block *oldTo, Block *newTo);
	void setEntry(address_t entry);

	// to be removed
	void assignIntervals();
	void extendIntervals();
	std::list<Block*> intervals();
	bool isReducible();
};

#endif
