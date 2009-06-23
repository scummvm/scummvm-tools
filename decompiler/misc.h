#ifndef MISC_H
#define MISC_H

#include <fstream>
#include <sstream>
#include <iomanip>

#include <boost/foreach.hpp>
#ifndef foreach
#define foreach BOOST_FOREACH
#endif

typedef unsigned char uint8;
typedef short int16;
typedef unsigned short uint16;
typedef unsigned uint32;

typedef uint32 address_t; // bytecode address
typedef uint32 index_t;   // instruction number in intermediate script

template<typename Container, typename Element>
bool contains(const Container &c, const Element &e) {
	return c.find(e) != c.end();
}

std::string phex(int i, int width=4) {
	std::ostringstream ret;
	ret << std::hex << std::setfill('0') << std::setw(width) << i;
	return ret.str();
}

uint32 read_be_uint32(std::ifstream &f) {
	uint32 ret = 0;
	ret |= f.get() << 24;
	ret |= f.get() << 16;
	ret |= f.get() << 8;
	ret |= f.get();
	return ret;
}

uint32 read_le_uint32(std::ifstream &f) {
	uint32 ret = 0;
	ret |= f.get();
	ret |= f.get() << 8;
	ret |= f.get() << 16;
	ret |= f.get() << 24;
	return ret;
}

uint16 read_le_uint16(std::ifstream &f) {
	int ret = 0;
	ret |= f.get();
	ret |= f.get() << 8;
	return (uint16) ret;
}

int16 read_le_int16(std::ifstream &f) {
	int ret = 0;
	ret |= f.get();
	ret |= f.get() << 8;
	return (int16) ret;
}

#endif
