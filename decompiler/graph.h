#ifndef GRAPH_H
#define GRAPH_H

#include "misc.h"

#include <cassert>

#include <list>
#include <map>
#include <set>
#include <sstream>

#include <boost/foreach.hpp>
#ifndef foreach
#define foreach BOOST_FOREACH
#endif

#include <iostream>


template<typename Data>
struct Graph {

	struct Node : boost::noncopyable {

		Data _data;

		const std::list<Node*> &out() const {
			return _out;
		}

		// wouldn't be needed if Graph<X> and Graph<Y> weren't totally alien classes
		Node *interval() const {
			return _interval;
		}

	private:

		friend class Graph;

		bool _visited;
		Node *_interval;
		std::list<Node*> _in;
		std::list<Node*> _out;
		int _order;
		int _loop;

		Node(const Data &data) : _data(data), _interval(), _order(), _loop() {
		}

		~Node() {
		}
	};

	std::list<Node*> _nodes;
	Node *_entry;
	int _currentOrder;
	int _currentLoop;

	Graph() : _entry(), _currentOrder(), _currentLoop() {
	}

	Graph(const Graph &g) : _entry() {
		std::map<Node*, Node*> trans;
		trans[0] = 0;
		foreach (Node *u, g._nodes)
			trans[u] = addNode(u->_data);
		foreach (Node *u, g._nodes) {
			foreach (Node *v, u->_out)
				addEdge(trans[u], trans[v]);
			trans[u]->_interval = trans[u->_interval];
		}
		_entry = trans[g._entry];
	}

	// TODO cleanup
	Graph &operator=(const Graph &g) {
		if (this == &g)
			return *this;
		foreach (Node *u, _nodes)
			delete u;
		_nodes.clear();
		std::map<Node*, Node*> trans;
		trans[0] = 0;
		foreach (Node *u, g._nodes)
			trans[u] = addNode(u->_data);
		foreach (Node *u, g._nodes) {
			foreach (Node *v, u->_out)
				addEdge(trans[u], trans[v]);
			trans[u]->_interval = trans[u->_interval];
		}
		_entry = trans[g._entry];
		return *this;
	}

	~Graph() {
		foreach (Node *u, _nodes)
			delete u;
	}

	void setEntry(Node *entry) {
		_entry = entry;
	}

	Node *addNode(const Data &data) {
		Node* node = new Node(data);
		_nodes.push_back(node);
		if (!_entry)
			_entry = node;
		return node;
	}

	void addEdge(Node *from, Node *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
	}

	void replaceEdges(Node *from, Node *oldTo, Node *newTo) {
		size_t n = count(oldTo->_in.begin(), oldTo->_in.end(), from);
		oldTo->_in.remove(from);
		fill_n(back_inserter(newTo->_in), n, from);
		foreach (Node *&node, from->_out)
			if (node == oldTo)
				node = newTo;
	}

	void removeUnreachableNodes() {
		foreach (Node *u, _nodes)
			u->_visited = false;
		assert(_entry);
		visit(_entry);
		for (typename std::list<Node*>::iterator uit = _nodes.begin(); uit != _nodes.end(); )
			if ((*uit)->_visited)
				uit++;
			else {
				foreach (Node *v, (*uit)->_out)
					v->_in.remove(*uit);
				delete *uit;
				uit = _nodes.erase(uit);
			}
	}

	std::list<Node*> intervals() const {
		std::list<Node*> ret;
		assignIntervals();
		foreach (Node *u, _nodes)
			if (u->_interval == u)
				ret.push_back(u);
		return ret;
	}

	// TODO: merge with removeUnreachableNodes?
	void markReversePostOrder() {
		foreach (Node *u, _nodes)
			u->_visited = false;
		_currentOrder = _nodes.size();
		assert(_entry);
		visit(_entry);
	}

	void loopStruct() {
		foreach (Node *interval, intervals()) {
			foreach (Node *latch, interval->_in) {
				if (latch->_interval == interval) { // it *is* latching node not only by name :)
					if (!latch->_loop && !interval->_loop) {
						int curloop = ++_currentLoop;
						foreach (Node *u, _nodes)
							if (interval->_order <= u->_order && u->_order <= latch->_order && u->_interval == interval)
								u->_loop = curloop;
					}
				}
			}
		}
	}

	void extendIntervals() {
		Graph<Node*> d;
		std::map<Node*, typename Graph<Node*>::Node*> trans;
		foreach (Node *u, _nodes)
			if (u->_interval == u)
				trans[u] = d.addNode(u->_interval);
		foreach (Node *interval, intervals())
			foreach (Node *u, interval->_in)
				if (u->_interval != interval)
					d.addEdge(trans[u->_interval], trans[interval]);
		d.intervals();
		foreach (typename Graph<Node*>::Node *du, d._nodes)
			foreach (Node *v, _nodes)
				if (v->_interval == du->_data)
					v->_interval = du->interval()->_data;
	}

	template<typename Printer>   // printer is a functor taking Data and returning a string
	std::string graphvizPrint(Printer printer, const std::string &fontname="Courier", int fontsize=14) const {
		std::stringstream ret;
		ret << "digraph G {" << std::endl;
		foreach (Node *interval, intervals()) {
			ret << "subgraph " << '"' << "cluster_" << interval << '"' << " {" << std::endl;
			ret << "style=dotted;" << std::endl;
			foreach (Node *u, _nodes)
				if (u->_interval == interval)
					ret << '"' << u << '"'
						<< "[fontname=" << '"' << fontname << '"'
						<< ",fontsize=" << fontsize
						<< ",shape=box"
						<< ",label=" << '"'
						             << "<order: " << u->_order << ", "
						             << "loop: " << u->_loop << ">\\n"
						             << graphvizEscapeLabel(printer(u->_data))
						             << '"'
						<< "];" << std::endl;
			ret << "}" << std::endl;
		}
		foreach (Node *u, _nodes)
			foreach (Node *v, u->_out)
			ret << '"' << u << '"'
				<< " -> "
				<< '"' << v << '"'
				<< ";" << std::endl;
		ret << "}" << std::endl;
		return ret.str();
	}

private:

	void visit(Node *u) {
		u->_visited = true;
		foreach (Node *v, u->_out)
			if (!v->_visited)
				visit(v);
		u->_order = _currentOrder--;
	}

	void assignIntervals() const {
		std::list<Node*> intervals;
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

	static std::string graphvizEscapeLabel(const std::string &s) {
		std::string ret;
		foreach (char c, s) {
			if (c == '\n' || c == '"' || c == '\\')
				ret.push_back('\\');
			ret.push_back(c == '\n' ? 'l' : c);   // align lines to the left
		}
		return ret;
	}
};

#endif
