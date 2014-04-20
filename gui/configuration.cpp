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

#include <wx/config.h>
#include <wx/utils.h>

#include "gui/configuration.h"


Configuration::Configuration() {
	// Default values for all the settings

	advanced = false;
	compressing = true;

	selectedTool = NULL;
	
	multipleRuns = false;

	selectedAudioFormat = AUDIO_VORBIS;
	advancedAudioSettings = false;

	// mp3 params
	mp3LamePath = wxT("lame");
	mp3CompressionType = wxT("VBR");
	mp3MpegQuality = wxT("2");

	mp3ABRBitrate = wxT("24");

	mp3VBRMinBitrate = wxT("24");
	mp3VBRMaxBitrate = wxT("64");
	mp3VBRQuality = wxT("4");

	// flac params
	flacCompressionLevel = wxT("8");
	flacBlockSize = wxT("1152");

	// ogg params
	useOggQuality = true;
	oggQuality = wxT("3");
	oggMinBitrate = wxT("None");
	oggAvgBitrate = wxT("24");
	oggMaxBitrate = wxT("None");

}

Configuration::~Configuration() {
}

void Configuration::load() {
	wxConfig *filecnf = new wxConfig(wxT("ScummVMTools"));

	filecnf->Read(wxT("outputpath"), &outputPath);

	// mp3 params
	filecnf->Read(wxT("mp3LamePath"), &mp3LamePath, mp3LamePath);
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
	filecnf->Read(wxT("useOggQuality"), &useOggQuality, useOggQuality);
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
	filecnf->Write(wxT("mp3LamePath"), mp3LamePath);

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
		filecnf->Write(wxT("useOggQuality"), useOggQuality);
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

bool Configuration::isLamePathValid(const wxString& mp3LamePath) {
	wxString cmd = mp3LamePath + wxT(" --license");
	int retval = wxExecute(cmd, wxEXEC_SYNC);
	return retval == 0;
}

