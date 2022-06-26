/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Compress Bud Tucker Sound Data Files */

#ifndef COMPRESS_TUCKER_H
#define COMPRESS_TUCKER_H

#include "compress.h"

class CompressTucker : public CompressionTool {
public:
	CompressTucker(const std::string &name = "compress_tucker");

	virtual void execute();

protected:

	int append_compress_file(Common::File &output);
	int compress_file_wav(Common::File &input, Common::File &output);
	int compress_file_raw(const char *input, bool is16, Common::File &output);
	uint32 compress_sounds_directory(const Common::Filename *inpath, const Common::Filename *outpath, Common::File &output, const struct SoundDirectory *dir);
	uint32 compress_audio_directory(const Common::Filename *inpath, const Common::Filename *outpath, Common::File &output);
	void compress_sound_data(Common::Filename *inpath, Common::Filename *outpath);
	void compress_sound_files(const Common::Filename *inpath, const Common::Filename *outpath);
};

#endif
