/* configuration.h - The different options entered into wizard
 * Copyright (C) 2009 The ScummVM project
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
 * $URL
 * $Id
 *
 */

#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <wx/string.h>

class Tool;

// Different audio formats
// They're used for bitwise operations
enum AudioFormat {
	AUDIO_VORBIS = 1,
	AUDIO_FLAC = 2,
	AUDIO_MP3 = 4,
	AUDIO_ALL = AUDIO_VORBIS | AUDIO_FLAC | AUDIO_MP3
};

struct Configuration {
	Configuration();
	
	// While prepending with _ would be in line with the coding conventions
	// this class is just a glorified map with different types, so it seems
	// unnecessary.

	bool advanced;
	bool compressing;
	bool advancedAudioSettings;
	wxString selectedGame;
	const Tool* selectedTool;
	wxArrayString inputFilePaths;
	wxString outputPath;
	AudioFormat selectedAudioFormat;
};

inline Configuration::Configuration() {
	// Default values for all the settings

	advanced = false;
	compressing = false;
	advancedAudioSettings = false;

	selectedTool = NULL;

	selectedAudioFormat = AUDIO_VORBIS;
}

#endif
