#ifndef MISC_H
#define MISC_H

#include <fstream>
#include <sstream>
#include <iomanip>

#include <boost/foreach.hpp>


typedef unsigned char uint8;
typedef short int16;
typedef unsigned short uint16;
typedef unsigned uint32;

typedef uint32 address_t; // bytecode address

template<typename Container, typename Element>
bool contains(const Container &c, const Element &e) {
	return c.find(e) != c.end();
}

std::string phex(int i, int width=4);
std::string spaces(int width);
uint32 read_be_uint32(std::ifstream &f);
uint32 read_le_uint32(std::ifstream &f);
uint16 read_le_uint16(std::ifstream &f);
int16 read_le_int16(std::ifstream &f);

#endif
