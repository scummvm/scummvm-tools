#include "block.h"

#include <boost/foreach.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


Block::Block() : _interval(), _number(), _loopHead(), _loopFollow(), _loopLatch(), _visited(), _dominator(), _ifFollow(), _component() {
}


Block::~Block() {
}


bool Block::inLoop(Block *head) {
	return _interval == head && head->_loopLatch->_number <= _number && _number < head->_number;
}


Block *Block::nonFollowEdge() {
	foreach (Block *u, _out)
		if (u != _loopFollow)
			return u;
	return 0;
}


Block *Block::outEdgeOutsideLoop(Block *head) {
	foreach (Block *u, _out)
		if (!u->inLoop(head) && u != head)
			return u;
	return 0;
}


string Block::toString() {
	ostringstream ret;
	foreach (Instruction *instruction, _instructions)
		ret << instruction->toString();
	return ret.str();
}
