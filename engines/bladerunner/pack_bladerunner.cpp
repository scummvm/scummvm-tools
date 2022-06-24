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

#include <string.h>
#include "pack_bladerunner.h"
#include "common/endian.h"

PackBladeRunner::PackBladeRunner(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.DAT";
	_inputPaths.push_back(input);

	_supportsProgressBar = true;

	_outputToDirectory = true;

	_shorthelp = "Used to combine Blade Runner CDFRAMES.DAT files into HDFRAMES.DAT.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] /path/to/inputfiles\n";
}

void PackBladeRunner::execute() {
	Common::Filename mainDir = _inputPaths[0].path;
	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty()) {
		_outputPath.setFullPath("./");
	}

	_outputPath.setFullName("HDFRAMES.DAT");

	const int kHDPageCount = 2480;
	const int kPageSize = 0x20000;
	int32 *hdPageOffsets;
	int32 *cdPageOffsets;
	byte *buf;
	byte timestamp[4];

	hdPageOffsets = (int32 *)malloc(kHDPageCount * 4);
	cdPageOffsets = (int32 *)malloc(kHDPageCount * 4);
	buf = (byte *)malloc(kPageSize);

	Common::File out;

	out.open(_outputPath, "wb");

	for (int i = 0; i < kHDPageCount; i++)
		hdPageOffsets[i] = -1;

	Common::File in;
	char fname[20];
	int curCD = 1;

	mainDir.setFullName("CDFRAMES.DAT");
	if (!mainDir.exists()) {
		snprintf(fname, 20, "CDFRAMES%d.DAT", curCD);
		mainDir.setFullName(fname);
	}
	in.open(mainDir, "rb");

	if (!in.isOpen()) {
		warning("Cannot open file %s", mainDir.getName().c_str());
		free(buf);
		free(cdPageOffsets);
		free(hdPageOffsets);
		return;
	}

	in.read_noThrow(timestamp, 4); // reading timestamp
	out.write(timestamp, 4);

	WRITE_LE_UINT32(buf, kHDPageCount);
	out.write(buf, 4);

	for (int i = 0; i < kHDPageCount; i++) {
		WRITE_LE_UINT32(&buf[i * 4], hdPageOffsets[i]);
	}

	out.write(buf, kHDPageCount * 4);

	int hdCurrentPage = 0;

	while (true) {
		uint32 cdPageCount = in.readUint32LE();
		print("Reading %d pages from %s...", cdPageCount, mainDir.getFullName().c_str());

		for (uint32 i = 0; i < cdPageCount; i++) {
			cdPageOffsets[i] = in.readUint32LE();
		}

		uint32 cdCurrentPage = 0;

		while (cdCurrentPage < cdPageCount) {
			while (cdCurrentPage < cdPageCount) {
				int i;
				for (i = 0; i < hdCurrentPage && cdPageOffsets[cdCurrentPage] != hdPageOffsets[i]; i++)
					;
				if (i == hdCurrentPage)
					break;

				in.seek(kPageSize, SEEK_CUR); // skip the page, we've copied it
				cdCurrentPage++;
			}

			if (cdCurrentPage == cdPageCount)
				break;

			in.read_noThrow(buf, kPageSize);
			out.write(buf, kPageSize);

			hdPageOffsets[hdCurrentPage++] = cdPageOffsets[cdCurrentPage++];

			updateProgress(hdCurrentPage, kHDPageCount);
		}

		in.close();

		curCD++;

		if (curCD == 5)
			break;

		snprintf(fname, 20, "CDFRAMES%d.DAT", curCD);

		mainDir.setFullName(fname);
		in.open(mainDir, "rb");

		if (!in.isOpen()) {
			warning("Cannot open file %s", mainDir.getName().c_str());
			return;
		}

		in.seek(4, SEEK_CUR); // skip timestamp
	}

	for (int i = hdCurrentPage; i < kHDPageCount; i++) {
		out.write(buf, kPageSize);
	}

	out.seek(8, SEEK_SET);

	for (int i = 0; i < kHDPageCount; i++) {
		WRITE_LE_UINT32(&buf[i * 4], hdPageOffsets[i]);
	}

	out.write(buf, kHDPageCount * 4);

	out.close();

	free(buf);
	free(cdPageOffsets);
	free(hdPageOffsets);


	print("All done!");
	print("File is created in %s", _outputPath.getFullPath().c_str());
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	PackBladeRunner bladerunner(argv[0]);
	return bladerunner.run(argc, argv);
}
#endif
