/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#ifndef DEC_KYRA_DISASM_H
#define DEC_KYRA_DISASM_H

#include "decompiler/disassembler.h"
#include "common/endian.h"

typedef uint32 IFF_ID;

/**
 * Container for a chunk from an IFF file.
 */
struct IFFChunk {
public:
	IFF_ID _chunkType; ///< Type of chunk, as specified by its magic number in the IFF file.
	uint32 _size;      ///< The size of the chunk contents.
	uint8 *_data;      ///< Pointer to the data in the chunk.

	/**
	 * Default constructor for IFFChunk.
	 */
	IFFChunk();
};

/**
 * Container for data on built-in functions.
 */
struct FunctionData {
	std::string _name;     ///< The name of the function.
	std::string _metadata; ///< Metadata for the function.

	/**
	 * Parameterless constructor for FunctionData. Used for new[].
	 */
	FunctionData() {
	}

	/**
	 * Constructor for FunctionData.
	 *
	 * @param name The name of the function.
	 * @param metadata Metadata for the function.
	 */
	FunctionData(std::string name, std::string metadata);
};

namespace Kyra {

class Kyra2Engine;

/**
 * Disassembler for KYRA.
 */
class Kyra2Disassembler : public ::Disassembler {
private:
	IFF_ID _formType;     ///< File type as listed in the IFF formatted file.
	IFFChunk _textChunk;  ///< Contents of the TEXT chunk.
	IFFChunk _ordrChunk;  ///< Contents of the ORDR chunk.
	IFFChunk _dataChunk;  ///< Contents of the DATA chunk.
	Kyra2Engine *_engine; ///< Pointer to the Kyra::Kyra2Engine used for this script.
	uint32 _funcCount;    ///< Number of functions in the _funcs array.
	FunctionData *_funcs; ///< Array of function data.

	/**
	 * Sets up function data for Kyra2 functions.
	 */
	void setupKyra2Funcs();
public:
	/**
	 * Constructor for Kyra2Disassembler.
	 *
	 * @param engine Pointer to the Kyra::Kyra2Engine used for this script.
	 * @param insts Reference to the vector in which disassembled instructions should be placed.
	 */
	Kyra2Disassembler(Kyra2Engine *engine, std::vector<Instruction> &insts);
	~Kyra2Disassembler();
	void doDisassemble() throw(UnknownOpcodeException);
};

} // End of namespace Kyra

#endif
