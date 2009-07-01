#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <cstdio>
#include <sstream>
#include <string>
#include <vector>

#include "misc.h"

struct Instruction {
	std::string _description;
	uint32 _addr;

	virtual std::string toString() {
		std::ostringstream ret;
		ret << phex(_addr-8) << "  " << _description << std::endl;
		return ret.str();
	}

	Instruction(std::string description, uint32 addr) : _description(description), _addr(addr) {
	}
	virtual ~Instruction() {
	}
};


struct Jump : public Instruction {
	int16 _offset;
	uint32 target() {
		return _addr+_offset;
	}
	std::string toString() {
		std::ostringstream ret;
		ret << phex(_addr-8) << "  " << _description << " (" << phex(target()-8) << ")" << std::endl;
		return ret.str();
	}

	Jump(std::string description, uint32 addr, int16 offset) : Instruction(description, addr), _offset(offset) {
	}
};

struct CondJump : public Jump {
	CondJump(std::string description, uint32 addr, int16 offset) : Jump(description, addr, offset) {
	}
};


struct Script;

struct Parser {
	virtual void parseFile(Script* script, const char *filename) = 0;
	virtual ~Parser() {}
};

struct Script {

	std::vector<Instruction*> _instructions;

	Script(Parser *parser, const char *filename) {
		parser->parseFile(this, filename);
	}

	void append(Instruction *instr) {
		_instructions.push_back(instr);
	}

};


#endif
