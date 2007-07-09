/* tool_gui - GUI for all the tools
 * Copyright (C) 2007  The ScummVM Team
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

#include <wx/wx.h>
#include <wx/notebook.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>

#define kNumCompressionTools 10
wxString kCompressionToolNames[10] = {"compress_agos", "compress_kyra", "compress_queen",  "compress_saga",  "compress_scumm_bun",  "compress_scumm_san",  "compress_scumm_sou",  "compress_sword1",  "compress_sword2",  "compress_touche"};

#define kNumCompressionTypes 3
wxString kCompressionTypeNames[3] = {"MP3", "Vorbis", "FLAC"};

#define kNumValidBitrates 21
wxString kValidBitrateNames[21] = {"", "8", "16", "24", "32", "40", "48", "56", "64", "72", "80", "88", "96", "104", "112", "120", "128", "136", "144", "152", "160"};

#define kNumValidQuality 11
wxString kVaildQualityNames[11] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"};

#define kNumValidCompressionLevels 10
wxString kVaildCompressionLevels[10] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8"};

#define kNumFLACBlocksize 5
wxString kFLACBlocksize[5] = {"", "576", "1152", "2304", "4608"};

#define kNumMP3Modes 2
wxString kMP3ModeNames[2] = {"VBR", "ABR"};

class ToolsGui : public wxApp {
public:
	virtual bool OnInit();
};

class MainFrame : public wxFrame {
public:
	MainFrame(const wxString& title);
};

class DropDownBox : public wxPanel {
public:
	DropDownBox(wxWindow *parent, wxWindowID id, wxString title, int numItems, wxString items[]);

	wxChoice *_choice;
};

class IOChooser : public wxPanel {
public:
	IOChooser(wxWindow *parent, wxString title, wxString defaultPath);

	wxTextCtrl *_text;
	wxButton *_browse;
};

class CompressionOptions : public wxPanel {
public:
	CompressionOptions(wxWindow *parent);

	wxChoice *_minBitrateChooser;
	wxChoice *_avgBitrateChooser;
	wxChoice *_maxBitrateChooser;
	wxChoice *_vbrQualityChooser;
	wxChoice *_compressionLevelChooser;
	wxChoice *_mpegQualityChooser;
	wxChoice *_modeChooser;
	wxChoice *_blockSize;
	wxCheckBox *_verifyChooser;
	wxCheckBox *_silentChooser;
};

class CompressionPanel : public wxPanel {
public:
	CompressionPanel(wxWindow *parent);

	DropDownBox *_toolChooserPanel;
	DropDownBox *_compressionTypePanel;
	IOChooser *_inputPanel;
	IOChooser *_outputPanel;
	CompressionOptions *_compressionOptionsPanel;
	wxTextCtrl *_toolOutput;

	enum {
		kCompressionChoice
	} kEventID;

	void OnCompressionChange(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};
