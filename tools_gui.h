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
#include <wx/process.h>
#include <wx/txtstrm.h>

class Process;
class LocationDialog;
#if wxUSE_DRAG_AND_DROP
class FileDrop;
#endif
class IOChooser;
class DropDownBox;
class CompressionOptions;
class CompressionPanel;
class ExtractionOptions;
class ExtractionPanel;
class MainFrame;

WX_DEFINE_ARRAY_PTR(Process *, ProcessArray);

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

/* Compatibility with wx 2.6 */
#if wxMAJOR_VERSION == 2 && wxMINOR_VERSION <= 6
#  define wxFD_OPEN wxOPEN
#  define wxFD_FILE_MUST_EXIST wxFILE_MUST_EXIST
#  define wxFD_MULTIPLE wxMULTIPLE
#endif

/* Options available in the UI
 * In most cases, the "Names" filled in here will directly be provided in the UI
 * For the fields that have two options, the second option will be the one
 * passed on as a command line option, in other cases the Name will be directly
 * passed
 */

/* List of compression tools, name will display in UI */
wxString kCompressionToolNames[12] = {wxT("AGOS"), wxT("Broken Sword 1"), wxT("Broken Sword 2"), wxT("Encode DXA"), wxT("Flight of the Amazon Queen"), wxT("Kyra"), wxT("SAGA"), wxT("SCUMM BUN"), wxT("SCUMM SAN"), wxT("SCUMM SOU"), wxT("Simon 2 (MAC)"), wxT("Touche")};
/* Name of tool executable (position must match list above), .exe will be appended under Windows */
wxString kCompressionToolFilenames[12] = {wxT("compress_agos"), wxT("compress_sword1"), wxT("compress_sword2"), wxT("encode_dxa"), wxT("compress_queen"), wxT("compress_kyra"), wxT("compress_saga"), wxT("compress_scumm_bun"), wxT("compress_scumm_san"), wxT("compress_scumm_sou"), wxT("compress_agos --mac"), wxT("compress_touche")};

/* List of extraction tools, name will display in UI */
wxString kExtractionToolNames[9] = {wxT("AGOS"), wxT("Kyra"), wxT("Loom (TG16)"), wxT("Maniac Mansion (Apple)"), wxT("Maniac Mansion (C64)"), wxT("Maniac Mansion (NES)"), wxT("Parallaction"), wxT("SCUMM (MAC)"), wxT("Zak McKracken (C64)")};
/* Name of extraction executable */
wxString kExtractionToolFilenames[9] = {wxT("extract_agos"), wxT("extract_kyra"), wxT("extract_loom_tg16"), wxT("extract_mm_apple"), wxT("extract_mm_c64"), wxT("extract_mm_nes"), wxT("extract_parallaction"), wxT("extract_scumm_mac"), wxT("extract_zak_c64")};

/* List of possible audio codecs to use when compressing */
wxString kCompressionTypeNames[3] = {wxT("MP3"), wxT("Vorbis"), wxT("FLAC")};
/* The codecs respective CLI arguments */
wxString kCompressionTypeArguments[3] = {wxT("--mp3"), wxT("--vorbis"), wxT("--flac")};

/* List of possible bitrates, first entry is default (no argument will be if input equals it) */
wxString kValidBitrateNames[21] = {wxT(""), wxT("8"), wxT("16"), wxT("24"), wxT("32"), wxT("40"), wxT("48"), wxT("56"), wxT("64"), wxT("72"), wxT("80"), wxT("88"), wxT("96"), wxT("104"), wxT("112"), wxT("120"), wxT("128"), wxT("136"), wxT("144"), wxT("152"), wxT("160")};

/* VBR/MPEG quality options */
wxString kVaildQualityNames[10] = {wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")};

/* Compression level options */
wxString kValidCompressionLevels[9] = {wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5"), wxT("6"), wxT("7"), wxT("8")};

/* Possible FLAC block sizes */
wxString kValidFlacBlocksize[4] = {wxT("576"), wxT("1152"), wxT("2304"), wxT("4608")};

/* Possible MP3 compression modes */
wxString kMP3ModeNames[2] = {wxT("VBR"), wxT("ABR")};

// Length of above fields
#define kNumCompressionTools (sizeof kCompressionToolNames / sizeof *kCompressionToolNames)
#define kNumExtractionTools (sizeof kExtractionToolNames / sizeof *kExtractionToolNames)
#define kNumCompressionTypes (sizeof kCompressionTypeNames / sizeof *kCompressionTypeNames)
#define kNumValidBitrates (sizeof kValidBitrateNames / sizeof *kValidBitrateNames)
#define kNumValidQuality (sizeof kVaildQualityNames / sizeof *kVaildQualityNames)
#define kNumValidCompressionLevels (sizeof kValidCompressionLevels / sizeof *kValidCompressionLevels)
#define kNumValidFlacBlocksize (sizeof kValidFlacBlocksize / sizeof *kValidFlacBlocksize)
#define kNumMP3Modes (sizeof kMP3ModeNames / sizeof *kMP3ModeNames)

// Window IDs for the widgets in the tool
// used for event handling
enum kEventId {
	kCompressionToolChoice = wxID_HIGHEST,
	kCompressionTypeChoice,
	kCompressionModeChoice,
	kCompressionInputBrowse,
	kCompressionOutputBrowse,
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

/* ----- Main Frame ----- */

class MainFrame : public wxFrame {
public:
	MainFrame(const wxString& title);

	wxNotebook *_mainNotebook;
	CompressionPanel *_compressionTools;
	ExtractionPanel *_extractionTools;
	ProcessArray _processList;

	void OnCompressionOptionsToggle(wxCommandEvent &event);
	void OnCompressionStart(wxCommandEvent &event);
	void OnExtractionStart(wxCommandEvent &event);
	void OnIdle(wxIdleEvent& event);
    void OnProcessTerminated(Process *process);

	DECLARE_EVENT_TABLE()
};

/* ----- Common ----- */

/* A process with the additional functionality of redirectig output to a
 * target text control
 */
class Process : public wxProcess {
public:
    Process(MainFrame *parent, wxTextCtrl *target);

    MainFrame *_parent;
	wxTextCtrl *_target;

	virtual void OnTerminate(int pid, int status);
    virtual bool HasInput();
};

/* A wxFileDialog with a constructor argument for picking directories instead */
class LocationDialog {
public:
	LocationDialog(wxTextCtrl *target, bool isFileChooser, wxString wildcard);

	wxDialog *_dialog;
	wxTextCtrl *_target;
	bool _isFileChooser;

	void prompt();
};

/* Used to enable drag & drop ontop of the file picker control */
#if wxUSE_DRAG_AND_DROP
class FileDrop : public wxFileDropTarget {
public:
	FileDrop(wxTextCtrl *target, bool isFileChooser);

	wxTextCtrl *_target;
	bool _isFileChooser;

	virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames);
};
#endif

/* This is just a wxFilePicker with the addition of a constructor
 * argument for picking directiories instead
 */
class IOChooser : public wxPanel {
public:
	IOChooser(wxWindow *parent, kEventId buttonId, wxString title, bool isFileChooser);

	wxTextCtrl *_text;
	wxButton *_browse;
	bool _isFileChooser;
#if wxUSE_DRAG_AND_DROP
	FileDrop *_dropTarget;
#endif
};

/* Very thin wrapper for a wxChoice, it's purpose being
 * constructor arguments rewrite and putting a frame
 * around the control.
 */
class DropDownBox : public wxPanel {
public:
	DropDownBox(wxWindow *parent, kEventId boxId, wxString title, int numItems, wxString items[]);

	wxChoice *_choice;
};

/* ----- Compression ----- */

/* The compression options, a panel containing the controls
 * for controlling the output of the tools
 */
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

/* Compression Panel (tab) */
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
	void OnCompressionInputBrowse(wxCommandEvent &event);
	void OnCompressionOutputBrowse(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};

/* ----- Extraction ----- */

/* Panel containing all the options for extracting */
class ExtractionOptions : public wxPanel {
public:
	ExtractionOptions(wxWindow *parent);

	wxCheckBox *_kyraAmiga;
	wxCheckBox *_kyraAllFiles;
	wxCheckBox *_kyraSingleFile;
	wxTextCtrl *_kyraFilename;
	wxCheckBox *_parallactionSmall;
};

/* Extraction Panel (tab) */
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
	void OnExtractionInput1Browse(wxCommandEvent &event);
	void OnExtractionInput2Browse(wxCommandEvent &event);
	void OnExtractionOutputBrowse(wxCommandEvent &event);

	DECLARE_EVENT_TABLE()
};

