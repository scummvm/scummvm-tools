#ifndef BLOCK_H
#define BLOCK_H

#include "instruction.h"

#include <list>

#include <boost/utility.hpp>


enum LoopType {
	PRE_TESTED,
	POST_TESTED,
	ENDLESS
};


struct Block : boost::noncopyable {

	bool _visited;
	Block *_dominator;       // immediate dominator
	Block *_ifFollow;        // block is header node for if, branches converge at _ifFollow
	Block *_interval;        // header node of the interval this block belongs to
	Block *_loopFollow;      // if not null, this block is a loop header, and follow is a first block after exit
    Block *_loopHead;        // if not null, this is a latching block
	Block *_loopLatch;       // if not null, this block is a loop header, and latch is the last block in the loop
	Block *_primitive;       // interval header of the graph from which this graph has been derived
	Block *_component;
	LoopType _loopType;
	int _number;             // number in post-order
	std::list<Block*> _in;
	std::list<Block*> _out;
	std::list<Instruction*> _instructions;

	Block();
	~Block();
	bool inLoop(Block *head);
	Block *nonFollowEdge();
	Block *outEdgeOutsideLoop(Block *head);
	std::string toString();
};

#endif
