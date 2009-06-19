#ifndef CFG_H
#define CFG_H

#include <map>
#include <list>


#include <cstdio>

using namespace std;

#include "graph.h"

#include "instruction.h"
#include "misc.h"


struct Block {

	Script &_script;
	index_t _begin, _end;

	Block(Script &script, index_t begin, index_t end) : _script(script), _begin(begin), _end(end) {
	}
	
	virtual ~Block() {}

	virtual void print(ostream &out) {
	}

};


struct BasicBlock : public Block {

	BasicBlock(Script &script, index_t begin, index_t end) : Block(script, begin, end) {
	}

	void print(ostream &out) {
		for (index_t i = _begin; i <= _end; i++)
			_script.print(out, i);
	}

};


struct CFG {

	void printBasicBlocks(ostream &out) {
		foreach (Block *block, _blocks) {
			block->print(out);
			out << endl;
		}
	}

	static string printer(Block *block) {
		stringstream ret;
		block->print(ret);
		return ret.str();
	}

	void printDot(ostream &out) {
		out << _graph.graphvizPrint(printer);
	}

	void removeJumpsToJumps() {
		for (bool changed = true; changed; ) {
			changed = false;
			foreach (Block *bu, _blocks) {
				Node *u = _nodes[bu->_begin];
				foreach (Node *v, u->out()) {
					Block *bv = v->_data;
					Jump *jump = dynamic_cast<Jump*>(_script[bv->_begin]);
					if (jump && !dynamic_cast<CondJump*>(jump) && jump->target() != jump->_addr) {
						changed = true;
						_graph.replaceEdges(u, v, _nodes[_script.index(jump->target())]);
					}
				}
			}
		}
	}

	void removeDeadBlocks() {
		_graph.removeUnreachableNodes(_nodes[0]);
	}

	typedef Graph<Block*>::Node Node;
	map<index_t, Node*> _nodes;
	list<Block*> _blocks;
	Script &_script;

	Graph<Block*> _graph;

	template<typename Container, typename Element>
	static bool contains(const Container &c, const Element &e) {
		return c.find(e) != c.end();
	}

	CFG(Script &script) : _script(script) {
		_nodes[script.size()] = 0;
		Jump *jump;
		for (index_t i = 0; i < script.size(); i++)
			if ((jump = dynamic_cast<Jump*>(script[i]))) {
				_nodes[script.index(jump->target())] = 0;
				_nodes[i+1] = 0;
			}
		index_t begin = 0;
		for (index_t i = 0; i < script.size(); i++)
			if (contains(_nodes, i+1)) {
				_blocks.push_back(new BasicBlock(script, begin, i));
				_nodes[begin] = _graph.addNode(_blocks.back());
				begin = i+1;
			}
		foreach (Block *block, _blocks) {
			Node *node = _nodes[block->_begin];
			index_t end = block->_end;
			if ((jump = dynamic_cast<Jump*>(script[end])))
				_graph.addEdge(node, _nodes[script.index(jump->target())]);
			if (end+1 < script.size() && contains(_nodes, end+1) && (!jump || dynamic_cast<CondJump*>(jump)))
				_graph.addEdge(node, _nodes[end+1]);
		}
	}
};


#endif

