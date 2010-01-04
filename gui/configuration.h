/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#ifndef GUI_CONFIGURATION_H
#define GUI_CONFIGURATION_H

#include <wx/filename.h>

#include "compress.h"	// for AudioFormat

class ToolGUI;

/**
 * Current state of the wizard
 */
struct Configuration {
	Configuration();
	~Configuration();

	/**
	 * Fills this config object with values loaded from the permanent storage method
	 */
	void load();

	/**
	 * Saves configuration to a more permanent storage
	 * (registry under unix, .ini like file under other OSes)
	 *
	 * @param all True if all parameters should be saved, including audio parameters.
	 */
	void save(bool all = true);

	/**
	 * Returns a list of all supported (as in, we have some defaults for it) platforms
	 */
	static wxArrayString getTargetPlatforms();

	/**
	 * Sets all the compression members to default values based on the 'selectedPlatform' member
	 */
	void setPlatformDefaults();

	// While prepending with _ would be in line with the coding conventions
	// this class is just a glorified map with different types, so it seems
	// unnecessary.

	/** If the user chose the advanced route from the start */
	bool advanced;
	/** true if the user chose to compress, false if extract, undefined if advanced */
	bool compressing;

	/** The platform the output files are going to be used on (compression only) */
	wxString selectedPlatform;
	/** The tool the user chose to use, NULL if none has been chosen yet */
	const ToolGUI *selectedTool;

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

	// flac
	wxString flacCompressionLevel;
	wxString flacBlockSize;

	// flac
	wxString oggQuality;
	wxString oggMinBitrate;
	wxString oggAvgBitrate;
	wxString oggMaxBitrate;
};

#endif
