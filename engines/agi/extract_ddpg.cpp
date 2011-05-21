/* extract_ddpg - Extract resources from the bootable floppy version of
 *                Donald Duck's Playground
 * Copyright (C) 2011  The ScummVM Team
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
 */

#include <stdio.h>
#include "extract_ddpg.h"

#define offsetTHS(track,head,sector) (512 * ((((track) * 2 + (head)) * 9) + (sector)))
#define offset(sector) offsetTHS(sector / 18, (sector % 18) / 9, (sector % 18) % 9)

#define BASE_SECTOR	0x1C2

#define LOGDIR 		offset(171) + 5
#define LOGDIR_MAX	43

#define PICDIR		offset(180) + 5
#define PICDIR_MAX	25

#define VIEWDIR		offset(189) + 5
#define VIEWDIR_MAX	171

#define SNDDIR		offset(198) + 5
#define SNDDIR_MAX	64

ExtractDDPG::ExtractDDPG(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*.dsk";
	_inputPaths.push_back(input);

	_shorthelp = "Extract resources from the bootable floppy version of Donald Duck's Playground.";
	_helptext = "\nUsage: " + _name + " [-o <output dir> = out/] <disk image>\n\t" + _shorthelp + "\n";
}

void ExtractDDPG::execute() {
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;
	Common::Filename dirpath;
	Common::File dir;

	if (outpath.empty()) {
		outpath.setFullPath("out/");
		dirpath.setFullPath("out/");
	}
	outpath.setFullName("VOL.0");

	Common::File in(inpath, "rb");
	Common::File out(outpath, "wb");

	// Extract LOGIC files
	dirpath.setFullName("LOGDIR");
	dir.open(dirpath, "wb");
	print("Extracting LOGIC files...\n");
	int n = 0;
	for (int i = 0; i <= LOGDIR_MAX; i++) {
		// Read directory entry from the disk image
		int sec, off;
		in.seek(LOGDIR + 3 * i, SEEK_SET);
		if (!readDirEntry(in, &sec, &off)) {
			writeDirEntry(dir, -1);
			continue;
		}
		in.seek(offset(sec) + off, SEEK_SET);

		// Write directory entry and extract file
		writeDirEntry(dir, n);
		n += extractFile(in, out);
	}
	dir.close();

	// Extract PICTURE files
	dirpath.setFullName("PICDIR");
	dir.open(dirpath, "wb");
	print("Extracting PICTURE files...\n");
	for (int i = 0; i <= PICDIR_MAX; i++) {
		// Read directory entry from the disk image
		int sec, off;
		in.seek(PICDIR + 3 * i, SEEK_SET);
		if (!readDirEntry(in, &sec, &off)) {
			writeDirEntry(dir, -1);
			continue;
		}
		in.seek(offset(sec) + off, SEEK_SET);

		// Write directory entry and extract file
		writeDirEntry(dir, n);
		n += extractFile(in, out);
	}
	dir.close();

	// Extract VIEW files
	dirpath.setFullName("VIEWDIR");
	dir.open(dirpath, "wb");
	print("Extracting VIEW files...\n");
	for (int i = 0; i <= VIEWDIR_MAX; i++) {
		// Read directory entry from the disk image
		int sec, off;
		in.seek(VIEWDIR + 3 * i, SEEK_SET);
		if (!readDirEntry(in, &sec, &off)) {
			writeDirEntry(dir, -1);
			continue;
		}
		in.seek(offset(sec) + off, SEEK_SET);

		// Write directory entry and extract file
		writeDirEntry(dir, n);
		n += extractFile(in, out);
	}
	dir.close();

	// Extract SOUND files
	dirpath.setFullName("SNDDIR");
	dir.open(dirpath, "wb");
	print("Extracting SOUND files...\n");
	for (int i = 0; i <= SNDDIR_MAX; i++) {
		// Read directory entry from the disk image
		int sec, off;
		in.seek(SNDDIR + 3 * i, SEEK_SET);
		if (!readDirEntry(in, &sec, &off)) {
			writeDirEntry(dir, -1);
			continue;
		}
		in.seek(offset(sec) + off, SEEK_SET);

		// Write directory entry and extract file
		writeDirEntry(dir, n);
		n += extractFile(in, out);
	}
	dir.close();

	print("Done!\n");
}

// Entry format: ssss ssssssso oooooooo (s=sector, o=offset)
bool ExtractDDPG::readDirEntry(Common::File &in, int *sec, int *off)
{
	int b0 = in.readByte();
	int b1 = in.readByte();
	int b2 = in.readByte();
	if (b0 == 0xFF && b1 == 0xFF && b2 == 0xFF)
		return false;

	*sec = (BASE_SECTOR + (((b0 & 0xF) << 8) | b1)) >> 1;
	*off = ((b1 & 0x1) << 8) | b2;
	return true;
}

void ExtractDDPG::writeDirEntry(Common::File &dir, int off)
{
	if (off >= 0) {
		dir.writeByte((off >> 16) & 0xF);
		dir.writeByte((off >> 8) & 0xFF);
		dir.writeByte(off & 0xFF);
	} else {
		dir.writeByte(0xFF);
		dir.writeByte(0xFF);
		dir.writeByte(0xFF);
	}
}

int ExtractDDPG::extractFile(Common::File &in, Common::File &out)
{
	// Check header from image
	int signature = in.readUint16BE();
	in.readByte();
	int length = in.readUint16LE();

	if (signature != 0x1234)
		error("Invalid signature in a resource file");

	// Write header to VOL file
	out.writeByte(0x12);
	out.writeByte(0x34);
	out.writeByte(0);
	out.writeByte(length);
	out.writeByte(length >> 8);
	
	// Extract data from image and write it to VOL file
	char buf[512];
	unsigned int n = length;
	while (n > 0) {
		int s = n < sizeof(buf) ? n : sizeof(buf);
		in.read_noThrow(buf, s);
		out.write(buf, s);
		n -= s;
	}

	return length + 5;
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractDDPG ddpg(argv[0]);
	return ddpg.run(argc, argv);
}
#endif
