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


template<typename Data>
struct Graph {

	struct Node : boost::noncopyable {

		Data _data;

		const std::list<Node*> &out() const {
			return _out;
		}

		Node *interval() const {
			return _interval;
		}

	private:

		friend class Graph;

		bool _hidden;
		bool _visited;
		Node *_interval;
		Node *_primitive;
		std::list<Node*> _in;
		std::list<Node*> _out;

		Node(const Data &data) : _data(data), _hidden(), _interval(), _primitive() {
		}

		~Node() {
		}
	};

	mutable std::list<Node*> _nodes;
	Node *_entry;

	Graph() : _entry() {
	}

	Graph(const Graph &g) {
		g.removeHiddenNodes();
		std::map<Node*, Node*> trans;
		trans[0] = 0;
		foreach (Node *u, g._nodes)
			trans[u] = addNode(u->_data);
		foreach (Node *u, g._nodes) {
			foreach (Node *v, u->_in)
				trans[u]->_in.push_back(trans[v]);
			foreach (Node *v, u->_out)
				trans[u]->_in.push_back(trans[v]);
			trans[u]->_interval = trans[u->_interval];
			trans[u]->_primitive = u->_primitive;
		}
		_entry = trans[g._entry];
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

	void removeNode(Node *u) {
		foreach (Node *v, u->_in)
			v->_out.remove(u);
		foreach (Node *v, u->_out)
			v->_in.remove(u);
		u->_hidden = true;
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
		foreach (Node *u, _nodes)
			if (!u->_visited)
				removeNode(u);
	}

	std::list<Node*> intervals() const {
		std::list<Node*> ret;
		assignIntervals();
		foreach (Node *u, _nodes)
			if (u->_interval == u)
				ret.push_back(u);
		return ret;
	}

	Graph derive() {
		removeHiddenNodes();
		assignIntervals();
		Graph g;
		std::map<Node*, Node*> trans;
		foreach (Node *u, _nodes)
			if (u->_interval == u) {
				trans[u] = g.addNode(u->_data);
				trans[u]->_primitive = u;
			}
		foreach (Node *interval, intervals()) {
			std::set<Node*> pred;
			foreach (Node *u, interval->_in)
				if (u->_interval != interval && !contains(pred, u->_interval)) {
					pred.insert(u->_interval);
					g.addEdge(trans[u->_interval], trans[interval]);
				}
		}
		g.setEntry(trans[_entry]);
		return g;
	}

	template<typename Printer>   // printer is a functor taking Data and returning a string
	std::string graphvizPrint(Printer printer, const std::string &fontname="Courier", int fontsize=14) const {
		removeHiddenNodes();
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
						<< ",label=" << '"' << graphvizEscapeLabel(printer(u->_data)) << '"'
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

	void removeHiddenNodes() const {
		for (typename std::list<Node*>::iterator it = _nodes.begin(); it != _nodes.end(); )
			if ((*it)->_hidden) {
				delete *it;
				it = _nodes.erase(it);
			} else
				it++;
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
