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
	virtual bool readInstruction(ifstream &f, vector<Instruction*> &v, uint32 addr) = 0;
	virtual ~Reader() {}
};


struct SimpleReader : public Reader {

	string _description;
	string _format;

	SimpleReader(string description, string format="") : _description(description), _format(format) {
	};

	bool readInstruction(ifstream &f, vector<Instruction*> &v, uint32 addr) {
		stringstream description(stringstream::out);
		description << _description;
		for (uint32 i = 0; i < _format.size(); i++)
			switch (_format[i]) {
			case 'w': {
				uint16 w = read_le_uint16(f);
				description.setf(ios::hex, ios::basefield);
				description << "_0x" << setfill('0') << setw(4) << w;
				break;
			}
				/*			case 'b': {
				uint8 b = f.get();
				description.setf(ios::dec, ios::basefield);
				description << "_0x" << setfill('0') << setw(2) << b;
				break;
				}*/
			case 's':
				description << "_\"";
				for (char c = f.get(); c != 0; c = f.get())
					description << c;
				description << '"';
				break;
			}
		v.push_back(new Instruction(description.str(), addr));
		return true;
	}
};


struct SubopcodeReader : public Reader {

	Reader *_dispatchTable[256];

	SubopcodeReader() {
		memset(_dispatchTable, 0, sizeof(_dispatchTable));
	}

	void registerOpcode(uint8 opcode, Reader *reader) {
		_dispatchTable[opcode] = reader;
	}

	bool readInstruction(ifstream& f, vector<Instruction*> &v, uint32 addr) {
		uint8 opcode = f.get();
		if (f.eof()) {
			return false;
		} else if (!_dispatchTable[opcode]) {
			printf("! unhandled opcode 0x%02x (%d) at address 0x%02x (%d)\n", opcode, opcode, addr, addr);
			return false;
		} else {
			return _dispatchTable[opcode]->readInstruction(f, v, addr);
		}
	}
};


#endif
