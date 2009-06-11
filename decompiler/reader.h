#ifndef READER_H
#define READER_H


#include <fstream>
#include <vector>
#include <sstream>
#include <iomanip>

#include <cassert>
#include <cstring>
#include <cstdio>

using namespace std;


#include "misc.h"
#include "instruction.h"


struct Reader {
	// return true if all went ok and we can safely read next afterwards
	virtual bool readInstruction(ifstream &f, Script* script, uint32 addr) = 0;
	virtual ~Reader() {
	}
};


struct SimpleReader : public Reader {

	string _description;
	string _format;

	SimpleReader(string description, string format="") : _description(description), _format(format) {
	};

	bool readArguments(ifstream &f, string &description, vector<int16> &arguments) {
		stringstream ssret(stringstream::out);
		ssret << _description;
		for (uint32 i = 0; i < _format.size(); i++)
			switch (_format[i]) {
			case 'w': {
				uint16 w = read_le_uint16(f);
				ssret << ' ' << w;
				break;
			}
			case 'W': {
				int16 w = (int16) read_le_uint16(f);
				ssret << ' ' << w;
				break;
			}
			case 'o': { // offset, fixed to be counted from the beginning of instruction
				int len = _format[++i] - '0';
				int16 w = len + (int16) read_le_uint16(f);
				arguments.push_back(w);
				ssret << ' ' << (w>=0?"+":"") << w;
				break;
			}
			case 's':
				ssret << " \"";
				for (char c = f.get(); c != 0; c = f.get())
					ssret << c;
				ssret << '"';
				break;
			default:
				fprintf(stderr, "! unhandled format char '%c'\n", _format[i]);
				return false;
			}
		description = ssret.str();
		return true;
	}

	virtual bool readInstruction(ifstream &f, Script *script, uint32 addr) {
		vector<int16> args;
		string descr;
		if (readArguments(f, descr, args)) {
			script->append(new Instruction(descr, addr));
			return true;
		} else {
			return false;
		}
	}
};


template<typename T>
struct _JmpReader : public SimpleReader {
	_JmpReader(string description, string format="") : SimpleReader(description, format) {
	}
	virtual bool readInstruction(ifstream &f, Script* script, uint32 addr) {
		vector<int16> args;
		string descr;
		if (readArguments(f, descr, args)) {
			script->append(new T(descr, addr, args[0]));
			return true;
		} else {
			return false;
		}
	}
};


typedef _JmpReader<Jump> JumpReader;
typedef _JmpReader<CondJump> CondJumpReader;



struct SubopcodeReader : public Reader {

	Reader *_dispatchTable[256];

	SubopcodeReader() {
		memset(_dispatchTable, 0, sizeof(_dispatchTable));
	}

	void registerOpcode(uint8 opcode, Reader *reader) {
		// don't allow registering two readers for the same opcode
		assert(_dispatchTable[opcode] == 0);

		_dispatchTable[opcode] = reader;
	}

	bool readInstruction(ifstream& f, Script *script, uint32 addr) {
		// if (f.tellg() >= 0x67) return false; // CUT
		uint8 opcode = f.get();
		if (f.eof()) {
			return false;
		} else if (!_dispatchTable[opcode]) {
			fprintf(stderr, "! unhandled opcode 0x%02x (%d) at address 0x%02x (%d)\n", opcode, opcode, addr, addr);
			return false;
		} else {
			return _dispatchTable[opcode]->readInstruction(f, script, addr);
		}
	}
};


struct SeqReader : public Reader {

	Reader *_first, *_second;

	SeqReader(Reader *first, Reader *second) : _first(first), _second(second) {
	}

	bool readInstruction(ifstream& f, Script *script, uint32 addr) {
		return _first->readInstruction(f, script, addr) && _second->readInstruction(f, script, addr);
	}
};


#endif
