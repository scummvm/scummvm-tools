#ifndef GRAPH_H
#define GRAPH_H

#include <cassert>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>

#include "instruction.h"
#include "misc.h"


enum LoopType {
	PRE_TESTED,
	POST_TESTED,
	ENDLESS
};


struct Block : boost::noncopyable {

	bool _visited;
	Block *_dominator;       // immediate dominator
	Block *_interval;        // header node of the interval this block belongs to
	Block *_loopFollow;      // if not null, this block is a loop header, and follow is a first block after exit
    Block *_loopHead;        // if not null, this is a latching block
	Block *_loopLatch;       // if not null, this block is a loop header, and latch is the last block in the loop
	Block *_primitive;       // interval header of the graph from which this graph has been derived
	LoopType _loopType;
	int _number;             // number in post-order
	std::list<Block*> _in;
	std::list<Block*> _out;
	std::list<Instruction*> _instructions;

	std::string toString() {
		std::ostringstream ret;
		foreach (Instruction *instruction, _instructions)
			ret << instruction->toString();
		return ret.str();
	}

	bool inLoop(Block *head) {
		return _interval == head && head->_loopLatch->_number <= _number && _number < head->_number;
	}

	Block *nonFollowEdge() {
		foreach (Block *u, _out)
			if (u != _loopFollow)
				return u;
		return 0;
	}

	Block *outEdgeOutsideLoop(Block *head) {
		foreach (Block *u, _out)
			if (!u->inLoop(head) && u != head)
				return u;
		return 0;
	}

	Block() : _interval(), _number(), _loopHead(), _loopFollow(), _loopLatch(), _visited(), _dominator() {
	}

	~Block() {
	}
};


struct ControlFlowGraph : boost::noncopyable {

	Block *_entry;
	std::list<Block*> _blocks;
	std::map<address_t, Block*> _targets;    // helps partitioning code into basic blocks

	ControlFlowGraph();
	~ControlFlowGraph();

	// create a basic block given a range of instructions
	template<typename Iterator>
	Block *addBlock(Iterator first, Iterator last) {
		Block* block = new Block;
		_blocks.push_back(block);
		copy(first, last, back_inserter(block->_instructions));
		return block;
	}

	// create graph from a range of instructions
	template<typename Iterator>
	void addBlocksFromScript(Iterator scriptBegin, Iterator scriptEnd) {
		Jump *jump;
		for (Iterator it = scriptBegin; it != scriptEnd; it++)
			if ((jump = dynamic_cast<Jump*>(*it))) {
				_targets[jump->target()] = 0;
				if (next(it) != scriptEnd)
					_targets[(*next(it))->_addr] = 0;
			}
		Iterator first = scriptBegin;
		for (Iterator last = scriptBegin; last != scriptEnd; last++) {
			if (next(last) == scriptEnd || contains(_targets, (*next(last))->_addr)) {
				_targets[(*first)->_addr] = addBlock(first, next(last));
				first = next(last);
			}
		}
		foreach (Block *block, _blocks) {
			if ((jump = dynamic_cast<Jump*>(block->_instructions.back())))
				addEdge(block, _targets[jump->target()]);
			std::map<address_t, Block*>::iterator succ = next(_targets.find(block->_instructions.front()->_addr));
			if (succ != _targets.end() && (!jump || dynamic_cast<CondJump*>(jump)))
				addEdge(block, succ->second);
		}
	}

	void setEntry(address_t entry);

	void loopStruct();               // fill in all information about loops
	std::list<Block*> intervals();   // partition graph into intervals and return list of header blocks

	void orderBlocks();              // assign block numbers in post-order
	void removeJumpsToJumps();
	void removeUnreachableBlocks();  // to be called after order blocks

	std::string graphvizToString(const std::string &fontname="", int fontsize=0);

	void assignIntervals();  // can be called multiple times
	void extendIntervals();
	void assignDominators();

private:
	LoopType loopType(Block *head, Block *latch);
	Block *loopFollow(Block *head, Block *latch);     // to be called after loopType

	void addEdge(Block *from, Block *to);

	static std::string graphvizEscapeLabel(const std::string &s);

	void replaceEdges(Block *from, Block *oldTo, Block *newTo);
};

#endif
