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

#include <wx/config.h>

#include "gui/configuration.h"


Configuration::Configuration() {
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

Configuration::~Configuration() {
}

void Configuration::load() {
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

void Configuration::save(bool all) {
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

wxArrayString Configuration::getTargetPlatforms() {
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

void Configuration::setPlatformDefaults() {
	// Switch for strings would be nice here...
	// Ogg works better on the small platforms (maybe all...?)
	if (selectedPlatform == wxT("Nintendo DS") || selectedPlatform == wxT("Dreamcast"))
		selectedAudioFormat = AUDIO_MP3;
}
