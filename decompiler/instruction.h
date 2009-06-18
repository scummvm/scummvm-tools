#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>
#include <cstdio>

#include "misc.h"

struct Instruction {
	string _description;
	uint32 _addr;
	Instruction(string description, uint32 addr) : _description(description), _addr(addr) {
	}
	virtual ~Instruction() {
	}
};


struct Jump : public Instruction {
	int16 _offset;
	uint32 target() {
		return _addr+_offset;
	}
	Jump(string description, uint32 addr, int16 offset) : Instruction(description, addr), _offset(offset) {
	}
};

struct CondJump : public Jump {
	CondJump(string description, uint32 addr, int16 offset) : Jump(description, addr, offset) {
	}
};


struct Script;

struct Parser {
	virtual void parseFile(Script* script, const char *filename) = 0;
	virtual ~Parser() {}
};

struct Script {

	vector<Instruction*> _instructions;

	Script(Parser *parser, const char *filename) {
		parser->parseFile(this, filename);
	}

	void append(Instruction *instr) {
		_instructions.push_back(instr);
	}

	index_t index(address_t addr) {
		for (index_t i = 0; i < _instructions.size(); i++)
			if (_instructions[i]->_addr == addr)
				return i;
		fprintf(stderr, "!!! no instruction with address %x (%d)\n", addr, addr);
		return -1;
	}

	Instruction* operator[](index_t i) {
		return _instructions[i];
	}

	index_t size() {
		return _instructions.size();
	}

	void print(ostream &out, index_t i) {
		if (i >= 1 && _instructions[i]->_addr == _instructions[i-1]->_addr)
			out << "       " << _instructions[i]->_description;
		else
			out << phex(_instructions[i]->_addr-8) << "  " << _instructions[i]->_description;
		Jump *jump = dynamic_cast<Jump*>(_instructions[i]);
		if (jump)
			out << " (" << phex(jump->target()-8) << ")";
		out << endl;
	}

};


#endif
