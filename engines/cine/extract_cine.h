/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef EXTRACT_CINE_H
#define EXTRACT_CINE_H

#include "tool.h"

/**
 * A LZ77 style decompressor for Delphine's data files
 * used in at least Future Wars and Operation Stealth.
 * @note Works backwards in the source and destination buffers.
 * @note Can work with source and destination in the same buffer if there's space.
 */
class CineUnpacker {
public:
	/**
	 * Unpacks packed data from the source buffer to the destination buffer.
	 * @warning Do NOT call this on data that is not packed.
	 * @note Source and destination buffer pointers can be the same as long as there's space for the unpacked data.
	 * @param src Pointer to the source buffer.
	 * @param srcLen Length of the source buffer.
	 * @param dst Pointer to the destination buffer.
	 * @param dstLen Length of the destination buffer.
	 * @return True if no errors were detected in the source data and unpacking was successful, otherwise false.
	 */
	bool unpack(const byte *src, unsigned int srcLen, byte *dst, unsigned int dstLen);
private:
	/**
	 * Reads an unsigned big endian 32-bit integer from the source stream and goes backwards 4 bytes.
	 * @return If the operation is valid, an unsigned big endian 32-bit integer read from the source stream.
	 * @return If the operation is invalid, zero.
	 * @note Sets internal error state if the read operation would be out of source bounds.
	 */
	uint32 readSource();

	/**
	 * Shifts the current internal 32-bit chunk to the right by one.
	 * Puts input carry into internal chunk's topmost (i.e. leftmost) bit.
	 * @return The least significant bit that was shifted out from the chunk.
	 */
	unsigned int rcr(bool inputCarry);

	/**
	 * Get the next bit from the source stream.
	 * @note Changes the bit position in the source stream.
	 * @return The next bit from the source stream.
	 */
	unsigned int nextBit();

	/**
	 * Get bits from the source stream.
	 * @note Changes the bit position in the source stream.
	 * @param numBits Number of bits to read from the source stream.
	 * @return Integer value consisting of the bits read from the source stream (In range [0, (2 ** numBits) - 1]).
	 * @return Later the bit was read from the source, the less significant it is in the return value.
	 */
	unsigned int getBits(unsigned int numBits);

	/**
	 * Copy raw bytes from the input stream and write them to the destination stream.
	 * This is used when no adequately long match is found in the sliding window.
	 * @note Sets internal error state if the operation would be out of bounds.
	 * @param numBytes Amount of bytes to copy from the input stream
	 */
	void unpackRawBytes(unsigned int numBytes);

	/**
	 * Copy bytes from the sliding window in the destination buffer.
	 * This is used when a match of two bytes or longer is found.
	 * @note Sets internal error state if the operation would be out of bounds.
	 * @param offset Offset in the sliding window
	 * @param numBytes Amount of bytes to copy
	 */
	void copyRelocatedBytes(unsigned int offset, unsigned int numBytes);
private:
	uint32 _crc;      //!< Error-detecting code (This should be zero after successful unpacking)
	uint32 _chunk32b; //!< The current internal 32-bit chunk of source data
	byte *_dst;       //!< Pointer to the current position in the destination buffer
	const byte *_src; //!< Pointer to the current position in the source buffer

	// These are used for detecting errors (e.g. out of bounds issues) during unpacking
	bool _error;           //!< Did an error occur during unpacking?
	const byte *_srcBegin; //!< Source buffer's beginning
	const byte *_srcEnd;   //!< Source buffer's end
	byte *_dstBegin;       //!< Destination buffer's beginning
	byte *_dstEnd;         //!< Destination buffer's end
};


class ExtractCine : public Tool {
public:
	ExtractCine(const std::string &name = "extract_cine");

	virtual void execute();

protected:
	void unpackFile(Common::File &file);
	void fixVolCnfFileName(char *dst, const uint8 *src);
	void unpackAllResourceFiles(const Common::Filename &filename);
};

#endif
