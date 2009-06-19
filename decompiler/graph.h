#ifndef GRAPH_H
#define GRAPH_H

#include <list>
#include <set>
#include <sstream>

#include <boost/foreach.hpp>
#ifndef foreach
#define foreach BOOST_FOREACH
#endif


template<typename Data>
struct Graph : boost::noncopyable {

	struct Node : boost::noncopyable {

		Data _data;

		const std::list<Node*> &out() const {
			return _out;
		}

	private:

		friend class Graph;

		std::list<Node*> _in;
		std::list<Node*> _out;
		bool _hidden;
		bool _visited;
		Node *_interval;

		Node(const Data &data) : _data(data), _hidden(false), _interval(0) {
		}

		~Node() {
		}
	};

	mutable std::list<Node*> _nodes;

	Graph() {
	}

	~Graph() {
		foreach (Node *u, _nodes)
			delete u;
	}

	Node *addNode(const Data &data) {
		Node* node = new Node(data);
		_nodes.push_back(node);
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

	void removeUnreachableNodes(Node *start) {
		foreach (Node *u, _nodes)
			u->_visited = false;
		visit(start);
		foreach (Node *u, _nodes)
			if (!u->_visited)
				removeNode(u);
	}

	void assignIntervals(Node *start) {
		std::list<Node*> intervals;
		intervals.push_back(start);
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
				bool hasPredInInterval = false;
				foreach (Node *p, m->_in)
					hasPredInInterval |= p->_interval == interval;
				if (!m->_interval && hasPredInInterval)
					intervals.push_back(m);
			}
		}
	}

	template<typename Printer>   // printer is a functor taking Data and returning a string
	std::string graphvizPrint(Printer printer, const std::string &fontname="Courier", int fontsize=14) const {
		std::stringstream ret;
		ret << "digraph G {" << std::endl;
		removeHiddenNodes();
		std::set<Node*> intervals;
		foreach (Node *u, _nodes)
			intervals.insert(u->_interval);
		foreach (Node *interval, intervals) {
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
