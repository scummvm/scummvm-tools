#ifndef NODE_H
#define NODE_H

#include "instruction.h"

#include <list>

#include <boost/utility.hpp>


struct ControlFlowGraph;


struct Node : boost::noncopyable {

	Node *_component;
	Node *_dominator;
	Node *_interval;
	int _postOrder;
	std::list<Node*> _in;
	std::list<Node*> _out;

	Node();
	virtual ~Node();

	virtual uint32 address() = 0;
	bool dominates(Node *u);
	Node *edgeOutsideComponent();
	virtual std::string toString() = 0;
};


struct BasicBlock : public Node {

	std::list<Instruction*> _instructions;

	BasicBlock(std::list<Instruction*>::iterator first, std::list<Instruction*>::iterator last);
	~BasicBlock();

	uint32 address();
	std::string toString();
};


struct ProxyNode : public Node {

	Node *_node;

	ProxyNode(Node *node);
	~ProxyNode();

	uint32 address();
	std::string toString();
};


struct Loop : public Node {

	ControlFlowGraph *_body;

	Loop() : Node(), _body() {
	}

};


struct WhileLoop : public Loop {

	Node *_condition;
	bool _negate;

	WhileLoop(ControlFlowGraph *graph, Node *entry);
	~WhileLoop();

	uint32 address();
	std::string toString();
};


struct DoWhileLoop : public Loop {

	Node *_condition;
	bool _negate;

	DoWhileLoop(ControlFlowGraph *graph, Node *entry, Node *latch);
	~DoWhileLoop();

	uint32 address();
	std::string toString();
};


struct EndlessLoop : public Loop {

	EndlessLoop(ControlFlowGraph *graph, Node *entry);
	~EndlessLoop();

	uint32 address();
	std::string toString();
};

#endif
