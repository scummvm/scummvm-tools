#include "node.h"

#include <boost/foreach.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


Node::Node() : _interval(), _number(), _dominator(), _component() {
}


Node::~Node() {
}


BasicBlock::BasicBlock(list<Instruction*>::iterator first, list<Instruction*>::iterator last) : Node() {
	copy(first, last, back_inserter(_instructions));
}


BasicBlock::~BasicBlock() {
}


uint32 BasicBlock::address() {
	return _instructions.front()->_addr;
}


string BasicBlock::toString() {
	ostringstream ret;
	foreach (Instruction *instruction, _instructions)
		ret << instruction->toString();
	return ret.str();
}


DerivedNode::DerivedNode(Node *primitive) : Node(), _primitive(primitive) {
}


DerivedNode::~DerivedNode() {
}


uint32 DerivedNode::address() {
	return _primitive->address();
}


string DerivedNode::toString() {
	return _primitive->toString();
}
