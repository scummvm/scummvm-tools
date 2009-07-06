/* compress_tinsel - .smp compressor
 * Copyright (C) 2009 The ScummVM Team
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

#ifndef COMPRESS_TINSEL_H
#define COMPRESS_TINSEL_H

class CompressTinsel : public CompressionTool {
public:
	CompressTinsel(const std::string &name = "compress_tinsel");

	virtual void execute();

protected:
	File _input_idx, _input_smp, _output_idx, _output_smp;

	void convertTinselRawSample(uint32 sampleSize);
	void convertTinselADPCMSample(uint32 sampleSize);
};

#endif
