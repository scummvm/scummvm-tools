#include "misc.h"

std::string phex(int i, int width) {
	std::ostringstream ret;
	ret << std::hex << std::setfill('0') << std::setw(width) << i;
	return ret.str();
}

std::string spaces(int width) {
	std::ostringstream ret;
	while (width--)
		ret << ' ';
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
