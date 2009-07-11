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
		while (u->_postOrder < v->_postOrder)
			u = u->_dominator;
		while (v->_postOrder < u->_postOrder)
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


string graphvizPrintBox(Node *u, const string &fontname, int fontsize) {
	ostringstream ret;
	ret << '"' << u << "\"[";
	if (fontname != "")
		ret << "fontname=" << '"' << fontname << "\",";
	if (fontsize != 0)
		ret << "fontsize=" << fontsize << ",";
	ret	<< "shape=box,label=\"<number=" << u->_postOrder;
	if (u->_dominator)
		ret	<< ", dom=" << u->_dominator->_postOrder;
	ret << ">\\n" << graphvizEscapeLabel(u->toString()) << "\"];" << endl;
	return ret.str();
}


string graphvizPrintSubgraph(ControlFlowGraph *graph, const string &fontname, int fontsize) {
	ostringstream ret;
	ret << "subgraph \"cluster_" << graph << "\" {" << endl;
	ret << "style=dotted" << endl;
	foreach (Node *u, graph->_nodes) {
		ret << graphvizPrintBox(u, fontname, fontsize);
		foreach (Node *v, u->_out)
			ret << '"' << u << "\" -> \"" << v << "\";" << endl;
		foreach (Node *v, u->_in) {
			ret << graphvizPrintBox(v, fontname, fontsize);
			ret << '"' << v << "\" -> \"" << u << "\"[style=dashed];" << endl;
		}
	}
	ret << "}" << endl;

	foreach (ControlFlowGraph *subgraph, graph->_subgraphs)
		ret << graphvizPrintSubgraph(subgraph, fontname, fontsize);

	foreach (Node *u, graph->_nodes) {
		Loop *loop = dynamic_cast<Loop*>(u);
		if (loop && loop->_body->_entry)
			ret << '"' << u << "\" -> \"" << loop->_body->_entry << "\"[color=blue,style=dashed]" << endl;
	}

	return ret.str();
}


bool postOrderCompare(Node *a, Node *b) {
	return a->_postOrder < b->_postOrder;
}

list<Node*> inPostOrder(list<Node*> &nodes) {
	list<Node*> ret(nodes);
	ret.sort(postOrderCompare);
	return ret;
}


int orderVisit(Node *u, int number) {
	u->_postOrder = -1;
	foreach (Node *v, u->_out)
		if (!v->_postOrder)
			number = orderVisit(v, number);
	u->_postOrder = ++number;
	return number;
}


void replaceCrossEdgesWithProxies(ControlFlowGraph *gu, Node *u, ControlFlowGraph *gv, Node *v) {
	foreach (Node *&node, u->_out)
		if (node == v) {
			node = new ProxyNode(v);
			node->_in.push_back(u);
			gu->_nodes.push_back(node);
		}
	v->_in.remove(u);
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


// TODO hidden nodes
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


void ControlFlowGraph::deleteNode(Node *node) {
	forgetNode(node);
	delete node;
}


void ControlFlowGraph::forgetNode(Node *node) {
	_nodes.remove(node);
	foreach (Node *u, node->_out)
		u->_in.remove(node);
}


string ControlFlowGraph::graphvizToString(const string &fontname, int fontsize) {
	stringstream ret;
	ret << "digraph G {" << endl;
	ret << graphvizPrintSubgraph(this, fontname, fontsize);
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
	if (_entry && !_entry->_postOrder)
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
		if (!u->_postOrder) {
			foreach (Node *v, u->_out)
				v->_in.remove(u);
			foreach (Node *v, u->_in)
				v->_out.remove(u);
		}
	for (std::list<Node*>::iterator it = _nodes.begin(); it != _nodes.end(); )
		if ((*it)->_postOrder)
			it++;
		else {
			// delete *it;
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
	foreach (Node *u, _nodes) {
		u->_component = 0;
		u->_postOrder = 0;
	}
	int n = 0;
	if (_entry && !_entry->_postOrder)
		n = orderVisit(_entry, n);
	foreach (Node *u, _nodes)
		if (!u->_postOrder)
			n = orderVisit(u, n);

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
	foreach (Node *u, nodes) {
		subgraph->_nodes.push_back(u);
		_nodes.remove(u);
	}
	foreach (Node *u, nodes)
		foreach (Node *v, list<Node*>(_nodes)) {
			replaceCrossEdgesWithProxies(subgraph, u, this, v);
			replaceCrossEdgesWithProxies(this, v, subgraph, u);
		}
	_subgraphs.push_back(subgraph);
	return subgraph;
}


void ControlFlowGraph::structureLoops(const list< list<Node*> > &components) {
	if (!_entry)
		return;
	foreach (Node *u, _nodes) {
		u->_dominator = 0;
		u->_postOrder = 0;
	}
	orderNodes();
	assignDominators();
	foreach (list<Node*> component, components) {
		list<Node*> entries = componentEntryPoints(component);
		if (entries.size() == 1) {
			Node *entry = entries.front();
			Node *latch = 0;
			foreach (Node *u, component) // find the deepest latching node
				foreach (Node *v, u->_out)
					if (v == entry && (!latch || latch->_postOrder > u->_postOrder))
						latch = u;
			if (latch && latch != entry &&latch->edgeOutsideComponent()) {
				_nodes.push_back(new DoWhileLoop(this, entry, latch));
				cerr << "done do-while loop at " << phex(entry->address()) << endl;
			} else if (latch && entry->edgeOutsideComponent()) {
				_nodes.push_back(new WhileLoop(this, entry));
				cerr << "done while loop at " << phex(entry->address()) << endl;
			} else if (latch) {
				_nodes.push_back(new EndlessLoop(this, entry));
				cerr << "done infinite loop at " << phex(entry->address()) << endl;
			}
		} else {
			// TODO: unreducible graph, lots of heuristics
		}
	}
}
