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

#include <fstream>
#include <string>
#include <iostream>
#include <iomanip>
#include <vector>
#include "filetools.h"

std::vector<std::string> *g_tag = 0;

std::string getTag(std::string str) {
	if (str.at(0) != '!') {
		std::cout << "Erroneous Tag\n";
	}
	std::string tag = str.substr(1, 4);
	return tag;
}

std::string getCompName(std::string str) {
	return str.substr(5);
}

void pushtag(std::string tag) {
	std::vector<std::string>::iterator it;
	for (it = g_tag->begin(); it != g_tag->end(); it++) {
		if (*it == tag) {
			return;
		}
	}
	g_tag->push_back(tag);
}

struct TrackKey {
	float _time;
	float _value;

	void readFromFile(std::istream &file) {
		_time = readFloat(file);
		_value = readFloat(file);
	}
};

struct ChoreTrack {
	std::string _tag;
	std::string _trackName;
	int _hash;
	int _parentID;
	int _numKeys;
	TrackKey *_keys;

	void readFromFile(std::istream &file) {
		// Split this into tag & name later.
		_trackName = readString(file);
		_tag = getTag(_trackName);
		_trackName = getCompName(_trackName);
		_hash = readInt(file);
		_parentID = readInt(file);
		_numKeys = readInt(file);

		pushtag(_tag);

		_keys = new TrackKey[_numKeys];
		for (int k = 0; k < _numKeys; k++) {
			_keys[k].readFromFile(file);
		}
	}
	void printComponent(int &count) {
		std::cout << count << "\t" << _tag << "\t" << _hash << "\t" << _parentID << "\t" << _trackName << std::endl;
	}
};

struct Chore {
	std::string _choreName;
	float _length;
	int _numTracks;
	ChoreTrack *_tracks;

	void readFromFile(std::istream &file) {
		_choreName = readString(file);
		_length = readFloat(file);
		_numTracks = readInt(file);
		_tracks = new ChoreTrack[_numTracks];

		for (int j = 0; j < _numTracks; j++) {
			_tracks[j].readFromFile(file);
		}
	}

	void printComponents(int &count) {
		for (int i = 0; i < _numTracks; i++) {
			_tracks[i].printComponent(count);
			count++;
		}
	}

	void print(int count) {
		std::cout << count << "\t" << _length << "\t" << _numTracks << "\t" << _choreName << std::endl;
	}
};

struct Costume {
	int _numChores;
	Chore *_chores;

	void readFromFile(std::istream &file) {
		_numChores = readInt(file);

		_chores = new Chore[_numChores];

		for (int i = 0; i < _numChores; i++) {
			_chores[i].readFromFile(file);
		}

	}

	void print() {
		std::cout << "section: tags\n";
		std::cout << "\tnumtags " << g_tag->size() << std::endl;

		std::vector<std::string>::iterator it = g_tag->begin();
		for (int i = 0; it != g_tag->end(); it++) {
			std::cout << i++ << "\t" << *it << std::endl;
		}
		std::cout << std::endl;
		std::cout << "section: components\n";
		std::cout << "\tnumcomponents: x\n";

		int count = 0;
		for (int i = 0; i < _numChores; i++) {
			_chores[i].printComponents(count);
		}
		std::cout << std::endl;
		std::cout << "section: chores\n";
		std::cout << "\tnumchores: x\n";
		count = 0;
		for (int i = 0; i < _numChores; i++) {
			_chores[i].print(count);
			count++;
		}
		std::cout << std::endl;
		std::cout << "section: keys\n";
		std::cout << "\tnumkeys: x\n";
		// TODO
	}

	void printChore(const char *choreName) {
		for (int i = 0; i < _numChores; i++) {
			if (_chores[i]._choreName == choreName) {
				std::cout << "Chore " << choreName << " (" << _chores[i]._numTracks << " tracks) ";
				if (_chores[i]._length == 1000) {
					std::cout << "(instant)";
				} else {
					std::cout << 1000.0 * _chores[i]._length << " ms";
				}

				std::cout << std::endl;
				for (int t = 0; t < _chores[i]._numTracks; t++) {
					ChoreTrack &track = _chores[i]._tracks[t];
					std::string &tag = track._tag;
					std::string &data = track._trackName;
					std::cout << "Track " << t << ": tag " << tag << ", data [" << data << "]" << std::endl;

					for (int k = 0; k < track._numKeys; k++) {
						TrackKey &tk = track._keys[k];
						std::cout << "\t";
						std::cout << std::right << std::setw(5);
						std::cout << (1000.0 * tk._time) << " ms";
						std::cout << "\t" << tk._value << std::endl;
					}
				}
				return;
			}
		}
		std::cout << "Error: chore " << choreName << " not found!" << std::endl;
	}
};

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}
	std::string filename = argv[1];

	std::fstream file(filename.c_str(), std::ios::in | std::ios::binary);

	if (!file.is_open()) {
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	g_tag = new std::vector<std::string>;
	Costume c;
	c.readFromFile(file);
	if (argc == 2) {
		c.print();
	} else {
		c.printChore(argv[2]);
	}
	delete g_tag;
}
