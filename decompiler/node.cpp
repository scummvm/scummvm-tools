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


ProxyNode::ProxyNode(Node *node) : Node(), _node(node) {
	_component = node->_component;
	_dominator = node->_dominator;
	_interval = node->_interval;
	_number = node->_number;
}


ProxyNode::~ProxyNode() {
}


uint32 ProxyNode::address() {
	return _node->address();
}


string ProxyNode::toString() {
	ostringstream ret;
	ret << "goto " << _node->address() << endl;
	return ret.str();
}


WhileLoop::WhileLoop(ControlFlowGraph *graph, Node *entry) : Node(), _condition(entry) {
	Node *exit = entry->edgeOutsideComponent();
	_negate = exit != entry->_out.front();
	_condition = entry;
	_component = entry->_component;
	_dominator = entry->_dominator;
	_interval = entry->_interval;
	_number = entry->_number;

	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (entry->dominates(u) && u != exit && !exit->dominates(u))
			body.insert(u);
	_body = graph->yank(body);
	foreach (Node *u, entry->_out)
		if (u != exit)
			_body->setEntry(u->address());

	foreach (Node *u, entry->_out)
		u->_in.remove(entry);
	entry->_out.clear();
	foreach (Node *u, list<Node*>(entry->_in))
		graph->replaceEdges(u, entry, this);
	graph->addEdge(this, exit);
	graph->_nodes.remove(entry);
}


WhileLoop::~WhileLoop() {
}


uint32 WhileLoop::address() {
	return _condition->address();
}


string WhileLoop::toString() {
	ostringstream ret;
	ret << "while";
	if (_negate)
		ret << " not";
	ret << " (" << endl;
	ret << _condition->toString();
	if (_body->_entry)
		ret << ") { [" << phex(_body->_entry->address()) << "] }" << endl;
	else
		ret << ") { }" << endl;
	return ret.str();
}
