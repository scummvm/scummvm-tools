#ifndef GRAPH_H
#define GRAPH_H

typedef unsigned id_t;

#include <map>
#include <list>
#include <string>
#include <sstream>

#include "misc.h"

using namespace std;


string escape(const string &s) {
	string ret;
	foreach (char c, s) {
		if (c == '\\' || c == '"')
			ret.push_back('\\');
		ret.push_back(c);
	}
	return ret;
};


template<typename Data>
struct Graph {


	struct NodeInfo {
		list<NodeInfo*> _in;
		list<NodeInfo*> _out;
		Data _data;
		NodeInfo(Data data) : _data(data) {
		}
	};


	struct Node {
		Graph *_graph;
		NodeInfo* _info;

		Node(Graph *graph, NodeInfo *info): _graph(graph), _info(info) {
		}

		list<Node> out() {
			list<Node> ret;
			foreach (NodeInfo *v, _info->_out)
				ret.push_back(Node(_graph, v));
			return ret;
		}

		void addEdge(Node v) {
			_graph->addEdge(*this, v);
		}

		Data &operator*() {
			return _info->_data;
		}

		Data *operator->() {
			return &_info->_data;
		}
	};

	list<NodeInfo*> _nodes;

	Graph() {
	}

	Node addNode(Data data) {
		NodeInfo* info = new NodeInfo(data);
		_nodes.push_back(info);
		return Node(this, info);
	}

	void addEdge(Node from, Node to) {
		from._info->_out.push_back(to._info);
		to._info->_in.push_back(from._info);
	}

	// printer :: Data -> string
	template<typename Printer>
	string print(Printer printer) {
		stringstream ret;
		ret << "digraph G {" << endl;
		foreach (NodeInfo *u, _nodes) {
			ret << (unsigned) u << " [shape=box,label=\"" << escape(printer(u->_data)) << "\"];" << endl;
			foreach (NodeInfo *v, u->_out)
				ret << (unsigned) u << " -> " << (unsigned) v << ";" << endl;
		}
		ret << "}" << endl;
		return ret.str();
	}

};

#endif
