#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <string>
#include <cstdio>

#include "misc.h"

struct Instruction {
	string _description;
	uint32 _addr;

	virtual string toString() {
		ostringstream ret;
		ret << phex(_addr-8) << "  " << _description << endl;
		return ret.str();
	}

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
	string toString() {
		ostringstream ret;
		ret << phex(_addr-8) << "  " << _description << " (" << phex(target()-8) << ")" << endl;
		return ret.str();
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

};


#endif
