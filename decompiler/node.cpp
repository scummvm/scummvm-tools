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
	ret << "goto " << phex(_node->address()) << endl;
	return ret.str();
}


WhileLoop::WhileLoop(ControlFlowGraph *graph, Node *entry) : Loop(), _condition(entry) {
	Node *exit = entry->edgeOutsideComponent();
	_negate = exit != entry->_out.front();
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

	foreach (Node *u, entry->_out) {
		u->_in.remove(entry);
		if (u != exit && u != entry) { // proxy node
			graph->_nodes.remove(u);
			//			delete u;
		}
	}
	entry->_out.clear();
	foreach (Node *u, list<Node*>(entry->_in))
		graph->replaceEdges(u, entry, this);
	graph->addEdge(this, exit);
	graph->_nodes.remove(entry);

	_body->structureLoops(_body->stronglyConnectedComponents());

	foreach (Node *u, _body->_nodes)
		u->_number = 0;
	_body->orderNodes();
	_body->removeUnreachableNodes();
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


DoWhileLoop::DoWhileLoop(ControlFlowGraph *graph, Node *entry, Node *latch) : Loop(), _condition(latch) {
	Node *exit = latch->edgeOutsideComponent();
	_negate = exit == latch->_out.front();
	_component = entry->_component;
	_dominator = entry->_dominator;
	_interval = entry->_interval;
	_number = entry->_number;

	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (entry == u || (u != latch && entry->dominates(u) && u != exit && !exit->dominates(u)))
			body.insert(u);
	_body = graph->yank(body);
	_body->setEntry(entry->address());

	foreach (Node *u, latch->_out) {
		u->_in.remove(latch);
		if (u != exit) { // proxy node
			graph->_nodes.remove(u);
//			delete u;
		}
	}
	latch->_out.clear();
	foreach (Node *u, graph->_nodes)
		foreach (Node *&v, u->_out)
			if (v->address() == entry->address()) { // proxy node
				//				delete v;
				v = this;
				_in.push_back(u);
			}
	graph->addEdge(this, exit);
	graph->_nodes.remove(latch);

	_body->structureLoops(_body->stronglyConnectedComponents());

	foreach (Node *u, _body->_nodes)
		u->_number = 0;
	_body->orderNodes();
	_body->removeUnreachableNodes();
}


DoWhileLoop::~DoWhileLoop() {
}


uint32 DoWhileLoop::address() {
	return _body->_entry->address();
}


string DoWhileLoop::toString() {
	ostringstream ret;
	ret << "do { [" << phex(_body->_entry->address()) << "] } while ";
	if (_negate)
		ret << "not ";
	ret << "(" << endl;
	ret << _condition->toString();
	ret << ")" << endl;
	return ret.str();
}

#include <cstdlib>
void panic() {
	exit(0);
}

EndlessLoop::EndlessLoop(ControlFlowGraph *graph, Node *entry) : Loop() {
	_component = entry->_component;
	_dominator = entry->_dominator;
	_interval = entry->_interval;
	_number = entry->_number;
	Node *exit = 0;
	foreach (Node *u, graph->_nodes)
		if (u->_component == entry->_component) {
			foreach (Node *v, u->_out)
				if (v->_component != entry->_component && (!exit || exit->_number < v->_number))
					exit = v;
		}

	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (entry->dominates(u) && (!exit || (u != exit && !exit->dominates(u))))
			body.insert(u);
	_body = graph->yank(body);
	list< list<Node*> > components = _body->stronglyConnectedComponents();
	foreach (Node *u, list<Node*>(entry->_in))
		graph->replaceEdges(u, entry, this);
	// we have broken down strongly connected components, now reattach entry
	foreach (Node *&u, entry->_out) {
		u->_in.remove(entry);
		if (u != entry) { // proxy node
			graph->_nodes.remove(u);
			Node *uref = dynamic_cast<ProxyNode*>(u)->_node;
			foreach (Node *&v, uref->_in) {
				if (v->address() == entry->address()) {
					_body->_nodes.remove(v);
					v = entry;
				}
			}
			u = uref;
			//			delete u;
		}
	}
	if (exit)
		graph->addEdge(this, exit);
	graph->_nodes.remove(entry);
	_body->_nodes.push_back(entry);

	_body->setEntry(entry->address());
	_body->structureLoops(components);
	foreach (Node *u, _body->_nodes)
		u->_number = 0;
	_body->orderNodes();
	_body->removeUnreachableNodes();
}


EndlessLoop::~EndlessLoop() {
}


uint32 EndlessLoop::address() {
	return _body->_entry->address();
}


string EndlessLoop::toString() {
	ostringstream ret;
	ret << "for (;;) { ";
	if (_body->_entry)
		ret << "[" << phex(_body->_entry->address()) << "] ";
	ret << "}" << endl;
	return ret.str();
}
