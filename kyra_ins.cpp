/* Scumm Tools
 * Copyright (C) 2008 The ScummVM project
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

#include "kyra_ins.h"

class FileExpanderSource {
public:
	FileExpanderSource(const uint8 *data, int dataSize) : _dataPtr(data), _endofBuffer(data + dataSize), _bitsLeft(8), _key(0), _index(0) {}
	~FileExpanderSource() {}

	void advSrcRefresh();
	void advSrcBitsBy1();
	void advSrcBitsByIndex(uint8 newIndex);

	uint8 getKeyLower() { return _key & 0xff; }
	void setIndex(uint8 index) { _index = index; }
	uint16 getKeyMasked(uint8 newIndex);
	uint16 keyMaskedAlign(uint16 val);

	void copyBytes(uint8 *& dst);

private:
	const uint8 *_dataPtr;
	const uint8 *_endofBuffer;
	uint16 _key;
	int8 _bitsLeft;
	uint8 _index;
};

void FileExpanderSource::advSrcBitsBy1() {
	_key >>= 1;
	if (!--_bitsLeft) {
		if (_dataPtr < _endofBuffer)
			_key = ((*_dataPtr++) << 8 ) | (_key & 0xff);
		_bitsLeft = 8;
	}
}

void FileExpanderSource::advSrcBitsByIndex(uint8 newIndex) {
	_index = newIndex;
	_bitsLeft -= _index;
	if (_bitsLeft <= 0) {
		_key >>= (_index + _bitsLeft);
		_index = -_bitsLeft;
		_bitsLeft = 8 - _index;
		if (_dataPtr < _endofBuffer)
			_key = (*_dataPtr++ << 8) | (_key & 0xff);
	}
	_key >>= _index;
}

uint16 FileExpanderSource::getKeyMasked(uint8 newIndex) {
	static const uint8 mskTable[] = { 0x0F, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF };
	_index = newIndex;
	uint16 res = 0;

	if (_index > 8) {
		newIndex = _index - 8;
		res = (_key & 0xff) & mskTable[8];
		advSrcBitsByIndex(8);
		_index = newIndex;
		res |= (((_key & 0xff) & mskTable[_index]) << 8);
		advSrcBitsByIndex(_index);
	} else {
		res = (_key & 0xff) & mskTable[_index];
		advSrcBitsByIndex(_index);
	}

	return res;
}

void FileExpanderSource::copyBytes(uint8 *& dst) {
	advSrcBitsByIndex(_bitsLeft);
	uint16 r = (READ_LE_UINT16(_dataPtr) ^ _key) + 1;
	_dataPtr += 2;

	if (r)
		error("decompression failure");

	memcpy(dst, _dataPtr, _key);
	_dataPtr += _key;
	dst += _key;
}

uint16 FileExpanderSource::keyMaskedAlign(uint16 val) {
	val -= 0x101;
	_index = (val & 0xff) >> 2;
	int16 b = ((_bitsLeft << 8) | _index) - 1;
	_bitsLeft = b >> 8;
	_index = b & 0xff;
	uint16 res = (((val & 3) + 4) << _index) + 0x101;
	return res + getKeyMasked(_index);
}

void FileExpanderSource::advSrcRefresh() {
	_key = READ_LE_UINT16(_dataPtr);
	if (_dataPtr < _endofBuffer - 1)
		_dataPtr += 2;
	_bitsLeft = 8;
}

class FileExpander {
public:
	FileExpander();
	~FileExpander();

	bool process(uint8 *dst, const uint8 *src, uint32 outsize, uint32 insize);

private:
	void generateTables(uint8 srcIndex, uint8 dstIndex, uint8 dstIndex2, int cnt);
	uint8 calcCmdAndIndex(const uint8 *tbl, int16 &para);

	FileExpanderSource *_src;
	uint8 *_tables[9];
	uint16 *_tables16[3];
};

FileExpander::FileExpander() : _src(0) {
	_tables[0] = new uint8[3914];
	assert(_tables[0]);

	_tables[1] = _tables[0] + 320;
	_tables[2] = _tables[0] + 352;
	_tables[3] = _tables[0] + 864;
	_tables[4] = _tables[0] + 2016;
	_tables[5] = _tables[0] + 2528;
	_tables[6] = _tables[0] + 2656;
	_tables[7] = _tables[0] + 2736;
	_tables[8] = _tables[0] + 2756;

	_tables16[0] = (uint16 *)(_tables[0] + 3268);
	_tables16[1] = (uint16 *)(_tables[0] + 3302);
	_tables16[2] = (uint16 *)(_tables[0] + 3338);
}

FileExpander::~FileExpander() {
	delete _src;
	delete[] _tables[0];
}

bool FileExpander::process(uint8 *dst, const uint8 *src, uint32 outsize, uint32 compressedSize) {
	static const uint8 indexTable[] = {
		0x10, 0x11, 0x12, 0x00, 0x08, 0x07, 0x09, 0x06, 0x0A,
		0x05, 0x0B, 0x04, 0x0C, 0x03, 0x0D, 0x02, 0x0E, 0x01, 0x0F
	};

	memset(_tables[0], 0, 3914);

	uint8 *d = dst;
	uint16 tableSize0 = 0;
	uint16 tableSize1 = 0;
	bool needrefresh = true;
	bool postprocess = false;

	_src = new FileExpanderSource(src, compressedSize);

	while (d < dst + outsize) {

		if (needrefresh) {
			needrefresh = false;
			_src->advSrcRefresh();
		}

		_src->advSrcBitsBy1();

		int mode = _src->getKeyMasked(2) - 1;
		if (mode == 1) {
			tableSize0 = _src->getKeyMasked(5) + 257;
			tableSize1 = _src->getKeyMasked(5) + 1;
			memset(_tables[7], 0, 19);

			const uint8 *itbl = indexTable;
			int numbytes = _src->getKeyMasked(4) + 4;

			while (numbytes--)
				_tables[7][*itbl++] = (uint8)_src->getKeyMasked(3);

			generateTables(7, 8, 255, 19);

			int cnt = tableSize0 + tableSize1;
			uint8 *tmp = _tables[0];

			while (cnt) {
				uint16 cmd = _src->getKeyLower();
				cmd = READ_LE_UINT16(&_tables[8][cmd << 1]);
				_src->advSrcBitsByIndex(_tables[7][cmd]);

				if (cmd < 16) {
					*tmp++ = (uint8)cmd;
					cnt--;
				} else {
					uint8 tmpI = 0;
					if (cmd == 16) {
						cmd = _src->getKeyMasked(2) + 3;
						tmpI = *(tmp - 1);
					} else if (cmd == 17) {
						cmd = _src->getKeyMasked(3) + 3;
					} else {
						cmd = _src->getKeyMasked(7) + 11;
					}
					_src->setIndex(tmpI);
					memset(tmp, tmpI, cmd);
					tmp += cmd;

					cnt -= cmd;
					if (cnt < 0)
						error("decompression failure");
				}
			}

			memcpy(_tables[1], _tables[0] + tableSize0, tableSize1);
			generateTables(0, 2, 3, tableSize0);
			generateTables(1, 4, 5, tableSize1);
			postprocess = true;
		} else if (mode < 0) {
			_src->copyBytes(d);
			postprocess = false;
			needrefresh = true;
		} else if (mode == 0){
			uint8 *d2 = _tables[0];
			memset(d2, 8, 144);
			memset(d2 + 144, 9, 112);
			memset(d2 + 256, 7, 24);
			memset(d2 + 280, 8, 8);
			d2 = _tables[1];
			memset(d2, 5, 32);
			tableSize0 = 288;
			tableSize1 = 32;

			generateTables(0, 2, 3, tableSize0);
			generateTables(1, 4, 5, tableSize1);
			postprocess = true;
		} else {
			error("decompression failure");
		}

		if (!postprocess)
			continue;

		int16 cmd = 0;

		do  {
			cmd = ((int16*) _tables[2])[_src->getKeyLower()];
			_src->advSrcBitsByIndex(cmd < 0 ? calcCmdAndIndex(_tables[3], cmd) : _tables[0][cmd]);

			if (cmd == 0x11d) {
				cmd = 0x200;
			} else if (cmd > 0x108) {
				cmd = _src->keyMaskedAlign(cmd);
			}

			if (!(cmd >> 8)) {
				*d++ = cmd & 0xff;
			} else if (cmd != 0x100) {
				cmd -= 0xfe;
				int16 offset = ((int16*) _tables[4])[_src->getKeyLower()];
				_src->advSrcBitsByIndex(offset < 0 ? calcCmdAndIndex(_tables[5], offset) : _tables[1][offset]);

				if ((offset & 0xff) >= 4) {
					uint8 newIndex = ((offset & 0xff) >> 1) - 1;
					offset = (((offset & 1) + 2) << newIndex);
					offset += _src->getKeyMasked(newIndex);
				}

				uint8 *s2 = d - 1 - offset;
				if (s2 >= dst) {
					while (cmd--)
						*d++ = *s2++;
				} else {
					uint32 pos = dst - s2;
					s2 += (d - dst);

					if (pos < (uint32) cmd) {
						cmd -= pos;
						while (pos--)
							*d++ = *s2++;
						s2 = dst;
					}
					while (cmd--)
						*d++ = *s2++;
				}
			}
		} while (cmd != 0x100);
	}

	delete _src;
	_src = 0;

	return true;
}

void FileExpander::generateTables(uint8 srcIndex, uint8 dstIndex, uint8 dstIndex2, int cnt) {
	const uint8 *tbl1 = _tables[srcIndex];
	uint8 *tbl2 = _tables[dstIndex];
	const uint8 *tbl3 = dstIndex2 == 0xff ? 0 : _tables[dstIndex2];

	if (!cnt)
		return;

	const uint8 *s = tbl1;
	memset(_tables16[0], 0, 32);

	for (int i = 0; i < cnt; i++)
		_tables16[0][(*s++)]++;

	_tables16[1][1] = 0;

	for (uint16 i = 1, r = 0; i < 16; i++) {
		r = (r + _tables16[0][i]) << 1;
		_tables16[1][i + 1] = r;
	}

	if (_tables16[1][16]) {
		uint16 r = 0;
		for (uint16 i = 1; i < 16; i++)
			r += _tables16[0][i];
		if (r > 1)
			error("decompression failure");
	}

	s = tbl1;
	uint16 *d = _tables16[2];
	for (int i = 0; i < cnt; i++) {
		uint16 t = *s++;
		if (t) {
			_tables16[1][t]++;
			t = _tables16[1][t] - 1;
		}
		*d++ = t;
	}

	s = tbl1;
	d = _tables16[2];
	for (int i = 0; i < cnt; i++) {
		int8 t = ((int8)(*s++)) - 1;
		if (t > 0) {
			uint16 v1 = *d;
			uint16 v2 = 0;

			do {
				v2 = (v2 << 1) | (v1 & 1);
				v1 >>= 1;
			} while (--t && v1);

			t++;
			uint8 c1 = (v1 & 1);
			while (t--) {
				uint8 c2 = v2 >> 15;
				v2 = (v2 << 1) | c1;
				c1 = c2;
			};

			*d++ = v2;
		} else {
			d++;
		}
	}

	memset((void*) tbl2, 0, 512);

	cnt--;
	s = tbl1 + cnt;
	d = &_tables16[2][cnt];
	uint16 * bt = (uint16*) tbl3;
	uint16 inc = 0;
	uint16 cnt2 = 0;

	do {
		uint8 t = *s--;
		uint16 *s2 = (uint16*) tbl2;

		if (t && t < 9) {
			inc = 1 << t;
			uint16 o = *d;

			do {
				s2[o] = cnt;
				o += inc;
			} while (!(o & 0xf00));

		} else if (t > 8) {
			if (!bt)
				error("decompression failure");

			t -= 8;
			uint8 shiftCnt = 1;
			uint8 v = (*d) >> 8;
			s2 = &((uint16*) tbl2)[*d & 0xff];

			do {
				if (!*s2) {
					*s2 = (uint16)(~cnt2);
					*(uint32*)&bt[cnt2] = 0;
					cnt2 += 2;
				}

				s2 = &bt[(uint16)(~*s2)];
				if (v & shiftCnt)
					s2++;

				shiftCnt <<= 1;
			} while (--t);
			*s2 = cnt;
		}
		d--;
	} while (--cnt >= 0);
}

uint8 FileExpander::calcCmdAndIndex(const uint8 *tbl, int16 &para) {
	const uint16 *t = (const uint16*)tbl;
	_src->advSrcBitsByIndex(8);
	uint8 newIndex = 0;
	uint16 v = _src->getKeyLower();

	do {
		newIndex++;
		para = t[((~para) & 0xfffe) | (v & 1)];
		v >>= 1;
	} while (para < 0);

	return newIndex;
}

template<class T>
T MIN(T l, T r) {
	return (l < r) ? l : r;
}

HoFInstaller::HoFInstaller(const char *baseFilename) : _list(0), _files(0) {
	strncpy(_baseFilename, baseFilename, sizeof(_baseFilename));
	char *str = strstr(_baseFilename, ".");
	if (str) {
		if ((uint32)((str - _baseFilename) + 1) < strlen(_baseFilename))
			str[1] = 0;
	} else {
		error("filename too long '%s'", baseFilename);
	}

	uint32 pos = 0;
	uint32 bytesleft = 0;
	bool startFile = true;

	_list = new Archive;
	memset(_list, 0, sizeof(Archive));
	Archive *newArchive = _list;

	for (int8 currentFile = 1; currentFile; currentFile++) {
		char filename[64];
		snprintf(filename, 64, "%s%03d", _baseFilename, currentFile);

		File file(filename, "rb");

		file.seek(pos, SEEK_SET);
		uint8 fileId = file.readByte();
		pos++;

		uint32 size = file.size() - 1;
		if (startFile) {
			size -= 4;
			if (fileId == currentFile) {
				size -= 6;
				pos += 6;
				file.seek(6, SEEK_CUR);
			} else {
				size = size + 1 - pos;
			}

			strcpy(newArchive->filename, _baseFilename);
			bytesleft = newArchive->totalSize = file.readUint32LE();
			pos += 4;
			newArchive->firstFile = currentFile;
			newArchive->startOffset = pos;
			startFile = false;
		}

		uint32 cs = MIN(size, bytesleft);
		bytesleft -= cs;

		file.close();

		pos += cs;
		if (cs == size) {
			if (!bytesleft) {
				newArchive->lastFile = currentFile;
				newArchive->endOffset = --pos;

				newArchive->next = new Archive;
				newArchive = newArchive->next;
				memset(newArchive, 0, sizeof(Archive));
				currentFile = -1;
			} else {
				pos = 0;
			}
		} else {
			startFile = true;
			bytesleft = size - cs;
			newArchive->lastFile = currentFile--;
			newArchive->endOffset = --pos;

			newArchive->next = new Archive;
			newArchive = newArchive->next;
			memset(newArchive, 0, sizeof(Archive));
		}
	}

	FileExpander exp;
	uint32 insize = 0;
	uint32 outsize = 0;
	uint8 *inbuffer = 0;
	uint8 *outbuffer = 0;
	uint32 inPart1 = 0;
	uint32 inPart2 = 0;
	char entryStr[64];

	pos = 0;

	const uint32 kExecSize = 0x0bba;
	const uint32 kHeaderSize = 30;
	const uint32 kHeaderSize2 = 46;

	for (Archive *a = _list; a != 0 && a->filename[0]; a = a->next) {
		startFile = true;
		for (uint32 i = a->firstFile; i != (a->lastFile + 1); i++) {
			char filename[64];
			snprintf(filename, 64, "%s%03d", _baseFilename, i);

			File file(filename, "rb");

			uint32 size = (i == a->lastFile) ? a->endOffset : file.size();

			if (startFile) {
				startFile = false;
				pos = a->startOffset + kExecSize;
				if (pos > size) {
					pos -= size;
					continue;
				}
			} else {
				if (inPart2) {
					file.seek(1, SEEK_SET);
					file.read(inbuffer + inPart1, 1, inPart2);
					inPart2 = 0;
					exp.process(outbuffer, inbuffer, outsize, insize);
					delete[] inbuffer;
					inbuffer = 0;

					FileList *newEntry = new FileList;
					assert(newEntry);
					newEntry->filename = new char[strlen(entryStr)+1];
					assert(newEntry->filename);
					strncpy(newEntry->filename, entryStr, strlen(entryStr)+1);
					newEntry->size = outsize;
					newEntry->data = outbuffer;

					if (_files)
						_files->addEntry(newEntry);
					else
						_files = newEntry;
				}
				pos++;
			}

			while (pos < size) {
				uint8 hdr[43];
				uint32 m = 0;
				file.seek(pos, SEEK_SET);

				if (pos + 42 > size) {
					m = size - pos;
					uint32 b = 42 - m;

					if (m >= 4) {
						uint32 id = file.readUint32LE();
						if (id == 0x06054B50) {
							startFile = true;
							break;
						} else {
							file.seek(pos, SEEK_SET);
						}
					}

					snprintf(filename, 64, "%s.%03d", _baseFilename, i+1);

					File file2(filename, "rb");
					file.read(hdr, 1, m);
					file2.read(hdr + m , 1, b);
				} else {
					file.read(hdr, 1, 42);
				}

				uint32 id = READ_LE_UINT32(hdr);

				if (id == 0x04034B50) {
					if (hdr[8] != 8)
						error("compression type not implemented");
					insize = READ_LE_UINT32(hdr + 18);
					outsize = READ_LE_UINT32(hdr + 22);

					uint16 filestrlen = READ_LE_UINT16(hdr + 26);
					*(hdr + 30 + filestrlen) = 0;
					strcpy(entryStr, (const char *)(hdr + 30));
					pos += (kHeaderSize + filestrlen - m);
					file.seek(pos, SEEK_SET);

					outbuffer = new uint8[outsize];
					assert(outbuffer);

					if (!inbuffer) {
						inbuffer = new uint8[insize];
						assert(inbuffer);
					}

					if ((pos + insize) > size) {
						// this is for files that are split between two archive files
						inPart1 = size - pos;
						inPart2 = insize - inPart1;
						file.read(inbuffer, 1, inPart1);
					} else {
						file.read(inbuffer, 1, insize);
						inPart2 = 0;
						exp.process(outbuffer, inbuffer, outsize, insize);
						delete[] inbuffer;
						inbuffer = 0;

						FileList *newEntry = new FileList;
						assert(newEntry);
						newEntry->filename = new char[strlen(entryStr)+1];
						assert(newEntry->filename);
						strncpy(newEntry->filename, entryStr, strlen(entryStr)+1);
						newEntry->size = outsize;
						newEntry->data = outbuffer;

						if (_files)
							_files->addEntry(newEntry);
						else
							_files = newEntry;
					}

					pos += insize;
					if (pos > size) {
						pos -= size;
						break;
					}
				} else {
					uint32 filestrlen = READ_LE_UINT32(hdr + 28);
					pos += (kHeaderSize2 + filestrlen - m);
				}
			}
		}
	}
}

