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

#include <wx/config.h>
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
	/** true if the chose to compress, false if compress, undefined if advanced */
	bool compressing;

	/** The platform the output files are going to be used on (compression only) */
	wxString selectedPlatform;
	/** The tool the user chose to use, NULL if none has been chosen yet */
	const ToolGUI* selectedTool;

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

inline Configuration::Configuration() {
	// Default values for all the settings

	advanced = false;
	compressing = true;

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

	// flac params
	flacCompressionLevel = wxT("8");
	flacBlockSize = wxT("1152");

	// flac params
	oggQuality = wxT("3");
	oggMinBitrate = wxT("24");
	oggAvgBitrate = wxT("24");
	oggMaxBitrate = wxT("64");

}

inline Configuration::~Configuration() {
}

inline void Configuration::load() {
	wxConfig *filecnf = new wxConfig(wxT("ScummVMTools"));

	filecnf->Read(wxT("outputpath"), &outputPath);

	// mp3 params
	filecnf->Read(wxT("mp3CompressionType"), &mp3CompressionType, mp3CompressionType);
	filecnf->Read(wxT("mp3MpegQuality"), &mp3MpegQuality, mp3MpegQuality);
	filecnf->Read(wxT("mp3ABRBitrate"), &mp3ABRBitrate, mp3ABRBitrate);
	filecnf->Read(wxT("mp3VBRMinBitrate"), &mp3VBRMinBitrate, mp3VBRMinBitrate);
	filecnf->Read(wxT("mp3VBRMaxBitrate"), &mp3VBRMaxBitrate, mp3VBRMaxBitrate);
	filecnf->Read(wxT("mp3VBRQuality"), &mp3VBRQuality, mp3VBRQuality);

	// flac params
	filecnf->Read(wxT("flacCompressionLevel"), &flacCompressionLevel, flacCompressionLevel);
	filecnf->Read(wxT("flacBlockSize"), &flacBlockSize, flacBlockSize);

	// flac params
	filecnf->Read(wxT("oggQuality"), &oggQuality, oggQuality);
	filecnf->Read(wxT("oggMinBitrate"), &oggMinBitrate, oggMinBitrate);
	filecnf->Read(wxT("oggAvgBitrate"), &oggAvgBitrate, oggAvgBitrate);
	filecnf->Read(wxT("oggMaxBitrate"), &oggMaxBitrate, oggMaxBitrate);

	delete filecnf;
}

inline void Configuration::save(bool all) {
	wxConfig *filecnf = new wxConfig(wxT("ScummVMTools"));

	wxFileName op(outputPath);
	filecnf->Write(wxT("outputpath"), op.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR));
	
	if (all) {
		// mp3 params
		filecnf->Write(wxT("mp3CompressionType"), mp3CompressionType);
		filecnf->Write(wxT("mp3MpegQuality"), mp3MpegQuality);
		filecnf->Write(wxT("mp3ABRBitrate"), mp3ABRBitrate);
		filecnf->Write(wxT("mp3VBRMinBitrate"), mp3VBRMinBitrate);
		filecnf->Write(wxT("mp3VBRMaxBitrate"), mp3VBRMaxBitrate);
		filecnf->Write(wxT("mp3VBRQuality"), mp3VBRQuality);

		// flac params
		filecnf->Write(wxT("flacCompressionLevel"), flacCompressionLevel);
		filecnf->Write(wxT("flacBlockSize"), flacBlockSize);

		// flac params
		filecnf->Write(wxT("oggQuality"), oggQuality);
		filecnf->Write(wxT("oggMinBitrate"), oggMinBitrate);
		filecnf->Write(wxT("oggAvgBitrate"), oggAvgBitrate);
		filecnf->Write(wxT("oggMaxBitrate"), oggMaxBitrate);
	}
	
	delete filecnf;
}

inline wxArrayString Configuration::getTargetPlatforms() {
	wxArrayString platforms;
	// Just add new platforms here, it's easy!
	// You specify additional defaults in the next function
	platforms.Add(wxT("PC (Windows, Linux, Mac)"));
	platforms.Add(wxT("iPhone"));
	platforms.Add(wxT("Nintendo DS"));
	platforms.Add(wxT("PlayStation 2"));
	platforms.Add(wxT("PocketPC"));
	platforms.Add(wxT("Dreamcast"));
	platforms.Add(wxT("PSP"));
	platforms.Add(wxT("Symbian"));
	platforms.Add(wxT("Wii"));

	return platforms;
}

inline void Configuration::setPlatformDefaults() {
	// Switch for strings would be nice here...
	// Ogg works better on the small platforms (maybe all...?)
	if (selectedPlatform == wxT("Nintendo DS") || selectedPlatform == wxT("Dreamcast"))
		selectedAudioFormat = AUDIO_MP3;
}

#endif
