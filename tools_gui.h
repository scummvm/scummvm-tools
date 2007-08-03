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
#include <wx/dnd.h>
#include <wx/notebook.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>

/* Default MP3 parameters */
wxString kDefaultMP3ABRAvgBitrate = wxT("24");
wxString kDefaultMP3CompressionType = wxT("VBR");
wxString kDefaultMP3MpegQuality = wxT("2");
wxString kDefaultMP3VBRMaxBitrate = wxT("64");
wxString kDefaultMP3VBRMinBitrate = wxT("24");
wxString kDefaultMP3VBRQuality = wxT("4");

/* Default Vorbis parameters */
wxString kDefaultOggQuality = wxT("3");

/* Default FLAC parameters */
wxString kDefaultFlacCompress = wxT("8");
wxString kDefaultFlacBlocksize = wxT("1152");


#define kNumCompressionTools 12
wxString kCompressionToolNames[12] = {wxT("AGOS"), wxT("Broken Sword 1"), wxT("Broken Sword 2"), wxT("Encode DXA"), wxT("Flight of the Amazon Queen"), wxT("Kyra"), wxT("SAGA"), wxT("SCUMM BUN"), wxT("SCUMM SAN"), wxT("SCUMM SOU"), wxT("Simon 2 (MAC)"), wxT("Touche")};

#define kNumExtractionTools 9
wxString kExtractionToolNames[9] = {wxT("AGOS"), wxT("Kyra"), wxT("Loom (TG16)"), wxT("Maniac Mansion (Apple)"), wxT("Maniac Mansion (C64)"), wxT("Maniac Mansion (NES)"), wxT("Parallaction"), wxT("SCUMM (MAC)"), wxT("Zak McKracken (C64)")};

#define kNumCompressionTypes 3
wxString kCompressionTypeNames[3] = {wxT("MP3"), wxT("Vorbis"), wxT("FLAC")};

#define kNumValidBitrates 21
wxString kValidBitrateNames[21] = {wxT(""), wxT("8"), wxT("16"), wxT("24"), wxT("32"), wxT("40"), wxT("48"), wxT("56"), wxT("64"), wxT("72"), wxT("80"), wxT("88"), wxT("96"), wxT("104"), wxT("112"), wxT("120"), wxT("128"), wxT("136"), wxT("144"), wxT("152"), wxT("160")};

#define kNumValidQuality 11
wxString kVaildQualityNames[11] = {wxT(""), wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")};

#define kNumValidCompressionLevels 10
wxString kVaildCompressionLevels[10] = {wxT(""), wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8")};

#define kNumValidFlacBlocksize 5
wxString kValidFlacBlocksize[5] = {wxT(""), wxT("576"), wxT("1152"), wxT("2304"), wxT("4608")};

#define kNumMP3Modes 2
wxString kMP3ModeNames[2] = {wxT("VBR"), wxT("ABR")};

enum {
	kCompressionToolChoice,
	kCompressionTypeChoice,
	kCompressionModeChoice,
	kCompressionInputBrowse,
	kCompressionOptionsToggle,
	kCompressionStartButton,
	kExtractionToolChoice,
	kExtractionInput1Browse,
	kExtractionInput2Browse,
	kExtractionOutputBrowse,
	kExtractionStartButton
};


class ToolsGui : public wxApp {
public:
	virtual bool OnInit();
};

class DropDownBox : public wxPanel {
public:
	DropDownBox(wxWindow *parent, wxWindowID id, wxString title, int numItems, wxString items[]);

	wxChoice *_choice;
};

class FileDrop : public wxFileDropTarget {
public:
	FileDrop(wxTextCtrl *target, bool isFileChooser);

	wxTextCtrl *_target;
	bool _isFileChooser;

	virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames);
};

class IOChooser : public wxPanel {
public:
	IOChooser(wxWindow *parent, wxString title, wxString defaultPath, bool isFileChooser);

	wxTextCtrl *_text;
	wxButton *_browse;
	bool _isFileChooser;
	FileDrop *_dropTarget;
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

	void OnCompressionModeChange(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};

class CompressionPanel : public wxPanel {
public:
	CompressionPanel(wxWindow *parent);

	DropDownBox *_compressionToolChooserBox;
	DropDownBox *_compressionTypeBox;
	wxCheckBox *_compressionOptionsChooser;
	IOChooser *_inputPanel;
	IOChooser *_outputPanel;
	CompressionOptions *_compressionOptionsPanel;
	wxButton *_startButton;
	wxTextCtrl *_toolOutput;

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

	void OnExtractionToolChange(wxCommandEvent &event);
	void OnExtractionStart(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};

/* ----- Main Panel ----- */

class MainFrame : public wxFrame {
public:
	MainFrame(const wxString& title);

	wxNotebook *_mainNotebook;
	CompressionPanel *_compressionTools;
	ExtractionPanel *_extractionTools;

	void OnCompressionOptionsToggle(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};
