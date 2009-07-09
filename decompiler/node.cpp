#include "node.h"
#include "graph.h"

#include <set>

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


bool Node::dominates(Node *u) {
	for (; u; u = u->_dominator)
		if (u->_dominator == this)
			return true;
	return false;
}


Node *Node::edgeOutsideComponent() {
	foreach (Node *u, _out)
		if (u->_component != _component)
			return u;
	return 0;
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


OutsideNode::OutsideNode(Node *node) : Node(), _node(node) {
}


OutsideNode::~OutsideNode() {
}


uint32 OutsideNode::address() {
	return _node->address();
}


string OutsideNode::toString() {
	ostringstream ret;
	ret << "goto " << _node->address() << endl;
	return ret.str();
}


WhileLoop::WhileLoop(ControlFlowGraph &graph, Node *entry) : Node(), _condition(entry) {
	Node *exit = entry->edgeOutsideComponent();
	_out.push_back(exit);
	set<Node*> body;
	foreach (Node *u, graph._nodes)
		if (entry->dominates(u) && u != exit && !exit->dominates(u))
			body.insert(u);
	_body = new ControlFlowGraph;
	graph.yank(body, *_body);
	graph._nodes.remove(entry);
	foreach (Node *u, entry->_in)
		graph.replaceEdges(u, entry, this);
	foreach (Node *u, entry->_out)
		if (u != exit)
			_body->setEntry(u->address());
}


WhileLoop::~WhileLoop() {
}


uint32 WhileLoop::address() {
	return _condition->address();
}


string WhileLoop::toString() {
	ostringstream ret;
	ret << "while loop..." << endl;
	return ret.str();
}
