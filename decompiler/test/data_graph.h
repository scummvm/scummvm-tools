#ifndef DATA_GRAPH_H
#define DATA_GRAPH_H

#include <boost/foreach.hpp>
#include <list>

#include <graph.h>
#include <instruction.h>


unsigned addr(Block *block) {
	return block->_instructions.front()->_addr;
}

Block *node(ControlFlowGraph *g, unsigned addr) {
	BOOST_FOREACH (Block *u, g->_blocks)
		if (u->_instructions.front()->_addr == addr)
			return u;
	return 0;
}

ControlFlowGraph *makeGraph1() {
	std::list<Instruction*> instructions;
	instructions.push_back(new        Jump("nop",       1, +1));
	instructions.push_back(new    CondJump("jumpif +3", 2, +3));
	instructions.push_back(new        Jump("nop",       3, +1));
	instructions.push_back(new        Jump("jump -2",   4, -2));
	instructions.push_back(new    CondJump("jumpif -4", 5, -4));
	instructions.push_back(new Instruction("ret",       6    ));
	ControlFlowGraph *g = new ControlFlowGraph;
	g->addBlocksFromScript(instructions.begin(), instructions.end());
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph2() {
	std::list<Instruction*> instructions;
	instructions.push_back(new    CondJump("jumpif +4",  1, +4));
	instructions.push_back(new    CondJump("jumpif +2",  2, +2));
	instructions.push_back(new        Jump("jump +2",    3, +2));
	instructions.push_back(new        Jump("nop",        4, +1));
	instructions.push_back(new        Jump("nop",        5, +1));
	instructions.push_back(new    CondJump("jumpif +5",  6, +5));
	instructions.push_back(new        Jump("nop",        7, +1));
	instructions.push_back(new        Jump("nop",        8, +1));
	instructions.push_back(new    CondJump("jumpif ",    9, -1));
	instructions.push_back(new        Jump("-4",        10, -4));
	instructions.push_back(new    CondJump("jumpif +2", 11, +2));
	instructions.push_back(new        Jump("nop",       12, +1));
	instructions.push_back(new        Jump("nop",       13, +1));
	instructions.push_back(new Instruction("ret",       14    ));
	ControlFlowGraph *g = new ControlFlowGraph;
	g->addBlocksFromScript(instructions.begin(), instructions.end());
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph3() {
	std::list<Instruction*> instructions;
	instructions.push_back(new    CondJump("jumpif +2", 1, +2));
	instructions.push_back(new        Jump("nop",       2, +1));
	instructions.push_back(new        Jump("jump -1",   3, -1));
	ControlFlowGraph *g = new ControlFlowGraph;
	g->addBlocksFromScript(instructions.begin(), instructions.end());
	g->setEntry(1);
	return g;
}

ControlFlowGraph *makeGraph4() {
	std::list<Instruction*> instructions;
	instructions.push_back(new        Jump("nop",       1, +1));
	instructions.push_back(new    CondJump("jumpif +0", 2, +0));
	instructions.push_back(new        Jump("jump -2",   3, -2));
	ControlFlowGraph *g = new ControlFlowGraph;
	g->addBlocksFromScript(instructions.begin(), instructions.end());
	g->setEntry(1);
	return g;
}

#endif
