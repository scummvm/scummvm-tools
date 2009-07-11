#include "node.h"
#include "graph.h"

#include <set>

#include <boost/foreach.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif


Node::Node() : _component(), _dominator(), _interval(), _postOrder() {
}


Node::~Node() {
}


bool Node::dominates(Node *u) {
	for (; u; u = u->_dominator)
		if (u == this)
			return true;
	return false;
}


Node *Node::edgeOutsideComponent() {
	foreach (Node *u, _out)
		if (u->_component != _component)
			return u;
	return 0;
}


void Node::mimic(Node *node) {
	_component = node->_component;
	_dominator = node->_dominator;
	_interval  = node->_interval;
	_postOrder = node->_postOrder;
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
	mimic(node);
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
	mimic(entry);
	Node *exit = entry->edgeOutsideComponent();
	_negate = exit != entry->_out.front();

	// yank out loop body
	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (u != entry && entry->dominates(u) && !exit->dominates(u))
			body.insert(u);
	_body = graph->yank(body);
	foreach (Node *u, entry->_out)
		if (u != exit)
			_body->setEntry(u->address());

	// remove unneeded nodes in main graph
	graph->forgetNode(entry);
	foreach (Node *u, entry->_out)
		if (dynamic_cast<ProxyNode*>(u))
			graph->deleteNode(u);

	// attach this while block in place of entry
	foreach (Node *u, list<Node*>(entry->_in))
		if (!dynamic_cast<ProxyNode*>(u))
			graph->replaceEdges(u, entry, this);
	graph->addEdge(this, exit);

	if (_body->_entry)
		_body->structureLoops(_body->stronglyConnectedComponents());
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
	mimic(entry);
	Node *exit = latch->edgeOutsideComponent();
	_negate = exit == latch->_out.front();

	// yank out loop body
	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (u != latch && entry->dominates(u) && !exit->dominates(u))
			body.insert(u);
	_body = graph->yank(body);
	_body->setEntry(entry->address());

	// remove unneeded nodes in main graph and attach this while block in place of entry
	foreach (Node *u, graph->_nodes)
		foreach (Node *&v, u->_out)
			if (v->address() == entry->address()) {
				graph->deleteNode(v);
				v = this;
				_in.push_back(u);
			}
	graph->forgetNode(latch);
	graph->addEdge(this, exit);

	_body->structureLoops(_body->stronglyConnectedComponents());
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


EndlessLoop::EndlessLoop(ControlFlowGraph *graph, Node *entry) : Loop() {
	mimic(entry);
	Node *exit = 0;
	foreach (Node *u, graph->_nodes)
		if (u->_component == _component) {
			foreach (Node *v, u->_out)
				if (v->_component != _component && (!exit || exit->_postOrder < v->_postOrder))
					exit = v;
		}

	// yank out body
	set<Node*> body;
	foreach (Node *u, graph->_nodes)
		if (u != entry && entry->dominates(u) && (!exit || !exit->dominates(u)))
			body.insert(u);
	_body = graph->yank(body);

	// compute strongly connected components with detached entry
	list< list<Node*> > components = _body->stronglyConnectedComponents();

	// reattach entry to the loop body
	foreach (Node *&u, entry->_out) {
		Node *uref = dynamic_cast<ProxyNode*>(u)->_node;
		uref->_in.push_back(entry);
		graph->deleteNode(u);
		u = uref;
	}
	graph->_nodes.remove(entry);
	_body->_nodes.push_back(entry);

	// attach this while block in place of entry
	foreach (Node *u, list<Node*>(entry->_in))
		graph->replaceEdges(u, entry, this);
	if (exit)
		graph->addEdge(this, exit);

	_body->setEntry(entry->address());
	_body->structureLoops(components);
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
