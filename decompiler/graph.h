#ifndef GRAPH_H
#define GRAPH_H

#include <list>
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

		Node(const Data &data) : _data(data) {
		}

		~Node() {
		}
	};

	std::list<Node*> _nodes;

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

	void addEdge(Node *from, Node *to) {
		from->_out.push_back(to);
		to->_in.push_back(from);
	}

	template<typename Printer>   // printer is a functor taking Data and returning a string
	std::string graphvizPrint(Printer printer, const std::string &fontname="Courier", int fontsize=14) const {
		std::stringstream ret;
		ret << "digraph G {" << std::endl;
		foreach (Node *u, _nodes) {
			ret << '"' << u << '"'
				<< "[fontname=" << '"' << fontname << '"'
				<< ",fontsize=" << fontsize
				<< ",shape=box"
				<< ",label=" << '"' << graphvizEscapeLabel(printer(u->_data)) << '"'
				<< "];" << std::endl;
			foreach (Node *v, u->_out)
				ret << '"' << u << '"'
					<< " -> "
					<< '"' << v << '"'
					<< ";" << std::endl;
		}
		ret << "}" << std::endl;
		return ret.str();
	}

private:

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
