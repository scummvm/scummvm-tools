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

#define kNumCompressionTools 12
wxString kCompressionToolNames[12] = {wxT("compress_agos"), wxT("compress_agos (MAC)"), wxT("compress_kyra"), wxT("compress_queen"),  wxT("compress_saga"),  wxT("compress_scumm_bun"),  wxT("compress_scumm_san"),  wxT("compress_scumm_sou"),  wxT("compress_sword1"),  wxT("compress_sword2"),  wxT("compress_touche"), wxT("encode_dxa")};

#define kNumExtractionTools 9
wxString kExtractionToolNames[9] = {wxT("extract_agos"), wxT("extract_kyra"), wxT("extract_loom_tg16"), wxT("extract_mm_apple"), wxT("extract_mm_c64"), wxT("extract_mm_nes"), wxT("extract_parallaction"), wxT("extract_scumm_mac"), wxT("extract_zak_c64")};

#define kNumCompressionTypes 3
wxString kCompressionTypeNames[3] = {wxT("MP3"), wxT("Vorbis"), wxT("FLAC")};

#define kNumValidBitrates 21
wxString kValidBitrateNames[21] = {wxT(""), wxT("8"), wxT("16"), wxT("24"), wxT("32"), wxT("40"), wxT("48"), wxT("56"), wxT("64"), wxT("72"), wxT("80"), wxT("88"), wxT("96"), wxT("104"), wxT("112"), wxT("120"), wxT("128"), wxT("136"), wxT("144"), wxT("152"), wxT("160")};

#define kNumValidQuality 11
wxString kVaildQualityNames[11] = {wxT(""), wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")};

#define kNumValidCompressionLevels 10
wxString kVaildCompressionLevels[10] = {wxT(""), wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8")};

#define kNumFLACBlocksize 5
wxString kFLACBlocksize[5] = {wxT(""), wxT("576"), wxT("1152"), wxT("2304"), wxT("4608")};

#define kNumMP3Modes 2
wxString kMP3ModeNames[2] = {wxT("VBR"), wxT("ABR")};

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
	IOChooser(wxWindow *parent, wxString title, wxString defaultPath, bool isFileChooser);

	wxTextCtrl *_text;
	wxButton *_browse;
	bool _isFileChooser;
};

/* ----- Compression ----- */

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

	DropDownBox *_compressionToolChooserPanel;
	DropDownBox *_compressionTypePanel;
	IOChooser *_inputPanel;
	IOChooser *_outputPanel;
	CompressionOptions *_compressionOptionsPanel;
	wxButton *_startButton;
	wxTextCtrl *_toolOutput;

	enum {
		kCompressionToolChoice,
		kCompressionTypeChoice,
		kCompressionInputBrowse,
		kCompressionStartButton
	} kEventID;

	void OnCompressionToolChange(wxCommandEvent &event);
	void OnCompressionTypeChange(wxCommandEvent &event);
	void OnCompressionStart(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};

/* ----- Extraction ----- */

class ExtractionOptions : public wxPanel {
public:
	ExtractionOptions(wxWindow *parent);

	wxCheckBox *_kyraAmiga;
	wxCheckBox *_kyraAllFiles;
	wxCheckBox *_kyraSingleFile;
	wxTextCtrl *_kyraFilename;
	wxCheckBox *_parallactionSmall;
};

class ExtractionPanel : public wxPanel {
public:
	ExtractionPanel(wxWindow *parent);

	DropDownBox *_extractionToolChooserPanel;
	IOChooser *_input1Panel;
	IOChooser *_input2Panel;
	IOChooser *_outputPanel;
	ExtractionOptions *_extractionOptionsPanel;
	wxButton *_startButton;
	wxTextCtrl *_toolOutput;

	enum {
		kExtractionToolChoice,
		kExtractionInput1Browse,
		kExtractionInput2Browse,
		kExtractionOutputBrowse,
		kExtractionStartButton
	} kEventID;

	void OnExtractionToolChange(wxCommandEvent &event);
	void OnExtractionStart(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};
