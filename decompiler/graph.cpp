#include "graph.h"

#include <algorithm>

#include <boost/foreach.hpp>
#include <boost/utility.hpp>

using namespace boost;
using namespace std;

#ifndef foreach
#define foreach BOOST_FOREACH
#endif



std::list<Node*> componentEntryPoints(std::list<Node*> &component) {
	std::list<Node*> ret;
	foreach (Node *u, component)
		foreach (Node *v, u->_in)
			if (u->_component != v->_component) {
				ret.push_back(u);
				break;
			}
	return ret;
}


void componentVisit(Node *u, Node *representative, list<Node*> &component) {
	u->_component = representative;
	component.push_back(u);
	foreach (Node *v, u->_in)
		if (!v->_component)
			componentVisit(v, representative, component);
}


Node *dominatorIntersect(Node *u, Node *v) {
	while (u != v) {
		while (u->_number < v->_number)
			u = u->_dominator;
		while (v->_number < u->_number)
			v = v->_dominator;
	}
	return u;
}


string graphvizEscapeLabel(const string &s) {
	string ret;
	foreach (char c, s) {
		if (c == '\n' || c == '"' || c == '\\')
			ret.push_back('\\');
		ret.push_back(c == '\n' ? 'l' : c);   // align lines to the left
	}
	return ret;
}


string graphvizPrintSubgraph(ControlFlowGraph *graph, const string &fontname, int fontsize) {
	ostringstream ret;
	ret << "subgraph \"cluster_" << graph << "\" {" << endl;
	ret << "style=dotted" << endl;
	foreach (Node *u, graph->_nodes) {
		ret << '"' << u << "\"[";
		if (fontname != "")
			ret << "fontname=" << '"' << fontname << "\",";
		if (fontsize != 0)
			ret << "fontsize=" << fontsize << ",";
		ret	<< "shape=box,label=\"<number=" << u->_number;
		if (u->_dominator)
			ret	<< ", dom=" << u->_dominator->_number;
		ret << ">\\n" << graphvizEscapeLabel(u->toString()) << "\"];" << endl;
		foreach (Node *v, u->_out)
			ret << '"' << u << "\" -> \"" << v << '"' << ";" << endl;
	}
	ret << "}" << endl;
	return ret.str();
}


bool postOrderCompare(Node *a, Node *b) {
	return a->_number < b->_number;
}

list<Node*> inPostOrder(list<Node*> &nodes) {
	list<Node*> ret(nodes);
	ret.sort(postOrderCompare);
	return ret;
}


int orderVisit(Node *u, int number) {
	u->_number = -1;
	foreach (Node *v, u->_out)
		if (!v->_number)
			number = orderVisit(v, number);
	u->_number = ++number;
	return number;
}



ControlFlowGraph::ControlFlowGraph() : _entry() {
}


ControlFlowGraph::~ControlFlowGraph() {
	foreach (Node *u, _nodes)
		delete u;
}


void ControlFlowGraph::addBasicBlocksFromScript(list<Instruction*>::iterator scriptBegin, list<Instruction*>::iterator scriptEnd) {
	Jump *jump;
	for (list<Instruction*>::iterator it = scriptBegin; it != scriptEnd; it++)
		if ((jump = dynamic_cast<Jump*>(*it))) {
			_targets[jump->target()] = 0;
			if (next(it) != scriptEnd)
				_targets[(*next(it))->_addr] = 0;
		}
	list<Instruction*>::iterator first = scriptBegin;
	for (list<Instruction*>::iterator last = scriptBegin; last != scriptEnd; last++) {
		if (next(last) == scriptEnd || contains(_targets, (*next(last))->_addr)) {
			BasicBlock *block = new BasicBlock(first, next(last));
			_targets[(*first)->_addr] = block;
			_nodes.push_back(block);
			first = next(last);
		}
	}
	foreach (Node *node, _nodes) {
		if ((jump = dynamic_cast<Jump*>(dynamic_cast<BasicBlock*>(node)->_instructions.back())))
			addEdge(node, _targets[jump->target()]);
		map<address_t, BasicBlock*>::iterator succ = next(_targets.find(node->address()));
		if (succ != _targets.end() && (!jump || dynamic_cast<CondJump*>(jump)))
			addEdge(node, succ->second);
	}
}


void ControlFlowGraph::addEdge(Node *from, Node *to) {
	from->_out.push_back(to);
	to->_in.push_back(from);
}


void ControlFlowGraph::assignDominators() {
	list<Node*> nodes = inPostOrder(_nodes);
	nodes.reverse();
	nodes.remove(_entry);
	_entry->_dominator = _entry;
	for (bool changed = true; changed; ) {
		changed = false;
		foreach (Node *u, nodes) {
			list<Node*>::iterator it = u->_in.begin();
			while (!(*it)->_dominator)
				it++;
			Node *dom = *it++; // first processed predecessor
			for (; it != u->_in.end(); it++)
				if ((*it)->_dominator)
					dom = dominatorIntersect(*it, dom);
			if (u->_dominator != dom) {
				changed = true;
				u->_dominator = dom;
			}
		}
	}
	_entry->_dominator = 0;
}


// entry node is an interval header
// a node belongs to an interval, if all its immediate predecessors belong the given interval
// otherwise it is an interval header
void ControlFlowGraph::assignIntervals() {
	list<Node*> intervals;
	intervals.push_back(_entry);
	foreach (Node *interval, intervals) {
		interval->_interval = interval;
		for (bool added = true; added; ) {
			added = false;
			foreach (Node *m, _nodes) {
				bool allPredInInterval = true;
				foreach (Node *p, m->_in)
					allPredInInterval &= p->_interval == interval;
				if (!m->_interval && allPredInInterval) {
					added = true;
					m->_interval = interval;
				}
			}
		}
		foreach (Node *m, _nodes) {
			bool anyPredInInterval = false;
			foreach (Node *p, m->_in)
				anyPredInInterval |= p->_interval == interval;
			if (!m->_interval && anyPredInInterval)
				intervals.push_back(m);
		}
	}
}


// a derived graph, given set of intervals, is a graph in which
// all intervals have been collapsed to a single node, and edge
// exists between nodes if there are edges crossing corresponding
// intervals in the original graph
void ControlFlowGraph::extendIntervals() {
	ControlFlowGraph d;
	map<Node*, ProxyNode*> trans;
	foreach (Node *interval, intervals()) {
		trans[interval] = new ProxyNode(interval);
		d._nodes.push_back(trans[interval]);
	}
	foreach (Node *interval, intervals())
		foreach (Node *u, interval->_in)
			if (u->_interval != interval)
				d.addEdge(trans[u->_interval], trans[interval]);
	d.setEntry(_entry->address());
	d.assignIntervals();
	foreach (Node *du, d._nodes)
		foreach (Node *v, _nodes)
			if (v->_interval == dynamic_cast<ProxyNode*>(du)->_node)
				v->_interval = dynamic_cast<ProxyNode*>(du->_interval)->_node;
}


string ControlFlowGraph::graphvizToString(const string &fontname, int fontsize) {
	stringstream ret;
	ret << "digraph G {" << endl;
	ret << graphvizPrintSubgraph(this, fontname, fontsize);
	foreach (ControlFlowGraph *graph, _subgraphs)
		ret << graphvizPrintSubgraph(graph, fontname, fontsize);
	ret << "}" << endl;
	return ret.str();
}


list<Node*> ControlFlowGraph::intervals() {
	list<Node*> ret;
	assignIntervals();
	foreach (Node *u, _nodes)
		if (u->_interval == u)
			ret.push_back(u);
	return ret;
}


bool ControlFlowGraph::isReducible() {
	for (size_t size = _nodes.size()+1; size > intervals().size(); size = intervals().size(), extendIntervals())
		;
	return intervals().size() == 1;
}


void ControlFlowGraph::orderNodes() {
	assert(_entry);
	if (!_entry->_number)
		orderVisit(_entry, 0);
}


void ControlFlowGraph::removeJumpsToJumps() {
	for (bool changed = true; changed; ) {
		changed = false;
		foreach (Node *u, _nodes) {
			foreach (Node *v, u->_out) {
				Jump *jump = dynamic_cast<Jump*>(dynamic_cast<BasicBlock*>(v)->_instructions.front());
				if (jump && !dynamic_cast<CondJump*>(jump) && jump->target() != jump->_addr) {
					changed = true;
					replaceEdges(u, v, _targets[jump->target()]);
				}
			}
		}
	}
}


void ControlFlowGraph::removeUnreachableNodes() {
	foreach (Node *u, _nodes)
		if (!u->_number) {
			foreach (Node *v, u->_out)
				v->_in.remove(u);
			foreach (Node *v, u->_in)
				v->_out.remove(u);
		}
	for (list<Node*>::iterator it = _nodes.begin(); it != _nodes.end(); )
		if ((*it)->_number)
			it++;
		else {
			delete *it;
			it = _nodes.erase(it);
		}
}


void ControlFlowGraph::replaceEdges(Node *from, Node *oldTo, Node *newTo) {
	size_t n = count(oldTo->_in.begin(), oldTo->_in.end(), from);
	oldTo->_in.remove(from);
	fill_n(back_inserter(newTo->_in), n, from);
	foreach (Node *&node, from->_out)
		if (node == oldTo)
			node = newTo;
}


void ControlFlowGraph::setEntry(address_t entry) {
	foreach (Node *node, _nodes)
		if (node->address() == entry)
			_entry = node;
}


list< list<Node*> > ControlFlowGraph::stronglyConnectedComponents() {
	list< list<Node*> > ret;
	orderNodes();
	list<Node*> nodes = inPostOrder(_nodes);
	nodes.reverse();
	foreach (Node *u, nodes)
		if (!u->_component) {
			list<Node*> component;
			componentVisit(u, u, component);
			ret.push_back(component);
		}
	return ret;
}



ControlFlowGraph *ControlFlowGraph::yank(set<Node*> &nodes) {
	ControlFlowGraph *subgraph = new ControlFlowGraph;
	_subgraphs.push_back(subgraph);
	list<Node*> newNodes;
	foreach (Node *u, _nodes)
		foreach (Node *v, _nodes) 
			if (contains(nodes, u) != contains(nodes, v)) { // replace all cross-graph edges with proxies
				size_t n = count(v->_in.begin(), v->_in.end(), u);
				v->_in.remove(u);
				while (n--)
					v->_in.push_back(new ProxyNode(u));
				foreach (Node *&node, u->_out)
					if (node == v) {
						node = new ProxyNode(node);
						if (contains(nodes, u))
							subgraph->_nodes.push_back(node);
						else
							newNodes.push_back(node);
					}
			}
	copy(newNodes.begin(), newNodes.end(), back_inserter(_nodes));
	foreach (Node *u, nodes) {
		subgraph->_nodes.push_back(u);
		_nodes.remove(u);
	}
	return subgraph;
}


void ControlFlowGraph::structureLoops() {
	foreach (list<Node*> component, stronglyConnectedComponents()) {
		list<Node*> entries = componentEntryPoints(component);
		if (entries.size() == 1) {
			Node *entry = entries.front();
			Node *latch = 0;
			foreach (Node *u, component) // find the deepest latching node
				foreach (Node *v, u->_out)
					if (v == entry && (!latch || latch->_number > u->_number))
						latch = u;
			if (latch && entry->edgeOutsideComponent()) { // while loop
				_nodes.push_back(new WhileLoop(this, entry));
			} else if (latch && latch->edgeOutsideComponent()) {
				// TODO do-while loop
			} else {
				// TODO infinite loop
			}
		} else {
			// TODO: unreducible graph, lots of heuristics
		}
	}
}
