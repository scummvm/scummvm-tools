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

/** 
 * Different audio formats
 * You can bitwise them to represent several formats
 */
enum AudioFormat {
	AUDIO_VORBIS = 1,
	AUDIO_FLAC = 2,
	AUDIO_MP3 = 4,
	AUDIO_ALL = AUDIO_VORBIS | AUDIO_FLAC | AUDIO_MP3
};

/**
 * Current state of the wizard
 */
struct Configuration {
	Configuration();
	
	// While prepending with _ would be in line with the coding conventions
	// this class is just a glorified map with different types, so it seems
	// unnecessary.

	/** If the user chose the advanced route from the start */
	bool advanced;
	/** true if the chose to compress, false if compress, undefined if advanced */
	bool compressing;

	/** The name of the game we are extracting or compressing */
	wxString selectedGame;
	/** The tool the user chose to use, NULL if none has been chosen yet */
	const Tool* selectedTool;

	/** Input files selected */
	wxArrayString inputFilePaths;
	/** Path to output to */
	wxString outputPath;

	/** Audio format selected */
	AudioFormat selectedAudioFormat;
	/** true if the user wants to see the advanced audio options */
	bool advancedAudioSettings;

	// mp3 settings
	wxString mp3CompressionType;
	wxString mp3MpegQuality;
	wxString mp3ABRBitrate;
	wxString mp3VBRMinBitrate;
	wxString mp3VBRMaxBitrate;
	wxString mp3VBRQuality;
};

inline Configuration::Configuration() {
	// Default values for all the settings

	advanced = false;
	compressing = false;

	selectedTool = NULL;

	selectedAudioFormat = AUDIO_VORBIS;
	advancedAudioSettings = false;
	
	// mp3 params
	mp3CompressionType = wxT("VBR");
	mp3MpegQuality = wxT("2");

	mp3ABRBitrate = wxT("24");

	mp3VBRMinBitrate = wxT("24");
	mp3VBRMaxBitrate = wxT("64");
	mp3VBRQuality = wxT("4");

}

#endif
