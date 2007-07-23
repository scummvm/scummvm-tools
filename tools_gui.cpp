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

#include "tools_gui.h"

IMPLEMENT_APP(ToolsGui)

BEGIN_EVENT_TABLE( CompressionPanel, wxPanel )
	EVT_CHOICE(kCompressionToolChoice, CompressionPanel::OnCompressionToolChange)
	EVT_CHOICE(kCompressionTypeChoice, CompressionPanel::OnCompressionTypeChange)
	EVT_CHOICE(kCompressionStartButton, CompressionPanel::OnCompressionStart)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( ExtractionPanel, wxPanel )
	EVT_CHOICE(kExtractionToolChoice, ExtractionPanel::OnExtractionToolChange)
	EVT_CHOICE(kExtractionStartButton, ExtractionPanel::OnExtractionStart)
END_EVENT_TABLE()

bool ToolsGui::OnInit() {
	MainFrame *frame = new MainFrame("ScummVM Tools");

	frame->Show();
	SetTopWindow(frame);

	return true;
}

MainFrame::MainFrame(const wxString& title) : wxFrame((wxFrame *)NULL, -1, title) {
	wxBoxSizer *mainSizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(mainSizer);

	wxNotebook *mainNotebook = new wxNotebook(this, -1);

	CompressionPanel *compressionTools = new CompressionPanel(mainNotebook);
	wxPanel *extractionTools = new ExtractionPanel(mainNotebook);

	mainNotebook->AddPage(compressionTools, "Compression", false, -1);
	mainNotebook->AddPage(extractionTools, "Extraction", false, -1);

	mainSizer->Add(mainNotebook, 1, wxEXPAND);
	mainSizer->SetSizeHints(this);
}

DropDownBox::DropDownBox(wxWindow *parent, wxWindowID id, wxString title, int numItems, wxString items[]) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(sizer);

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxVERTICAL, this, title);
	_choice = new wxChoice(this, id, wxDefaultPosition, wxDefaultSize, numItems, items);

	box->Add(_choice, 1, wxEXPAND | wxLEFT | wxRIGHT, 5);
	sizer->Add(box, 1, wxEXPAND);
}

IOChooser::IOChooser(wxWindow *parent, wxString title, wxString defaultPath, bool isFileChooser) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(sizer);

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, this, title);
	_text = new wxTextCtrl(this, -1, defaultPath);
	_browse = new wxButton(this, -1, "Browse");
	_isFileChooser = isFileChooser;

	/* The button looks like it is shifted 2 pixels down from the text control
	 * so we simply pad the top by -2
	 */
	box->Add(_text, 3, wxBOTTOM | wxLEFT | wxRIGHT, 5);
	box->Add(_browse, 1, wxTOP, -2);

	sizer->Add(box, 1, wxEXPAND);
}

/* ----- Compression ----- */

CompressionOptions::CompressionOptions(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	wxPanel *grid = new wxPanel(this);
	wxGridSizer *gridSizer = new wxGridSizer(2, 5, 0, 10);
	grid->SetSizer(gridSizer);

	wxStaticText *minBitrateLabel = new wxStaticText(grid, wxID_ANY, "Minimum Bitrate");
	_minBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *avgBitrateLabel = new wxStaticText(grid, wxID_ANY, "Average Bitrate");
	_avgBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *maxBitrateLabel = new wxStaticText(grid, wxID_ANY, "Maximum Bitrate");
	_maxBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *vbrQualityLabel = new wxStaticText(grid, wxID_ANY, "VBR Quality");
	_vbrQualityChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *mpegQualityLabel = new wxStaticText(grid, wxID_ANY, "MPEG Quality");
	_mpegQualityChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *compressionLevelLabel = new wxStaticText(grid, wxID_ANY, "Compression Level");
	_compressionLevelChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidCompressionLevels, kVaildCompressionLevels);

	wxStaticText *modeLabel = new wxStaticText(grid, wxID_ANY, "Compression Mode");
	_modeChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumMP3Modes, kMP3ModeNames);

	wxStaticText *blockSizeLabel = new wxStaticText(grid, wxID_ANY, "Block Size");
	_blockSize = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumFLACBlocksize, kFLACBlocksize);

	wxStaticText *verifyLabel = new wxStaticText(grid, wxID_ANY, "Verify");
	_verifyChooser = new wxCheckBox(grid, wxID_ANY, "");

	wxStaticText *silentLabel = new wxStaticText(grid, wxID_ANY, "Silent");
	_silentChooser = new wxCheckBox(grid, wxID_ANY, "");

	gridSizer->Add(minBitrateLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(avgBitrateLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(maxBitrateLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(vbrQualityLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(mpegQualityLabel, 0, wxALIGN_CENTER);

	gridSizer->Add(_minBitrateChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_avgBitrateChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_maxBitrateChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_vbrQualityChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_mpegQualityChooser, 0, wxALIGN_CENTER | wxEXPAND);

	gridSizer->Add(modeLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(compressionLevelLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(blockSizeLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(verifyLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(silentLabel, 0, wxALIGN_CENTER);

	gridSizer->Add(_modeChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_compressionLevelChooser, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_blockSize, 0, wxALIGN_CENTER | wxEXPAND);
	gridSizer->Add(_verifyChooser, 0, wxALIGN_CENTER);
	gridSizer->Add(_silentChooser, 0, wxALIGN_CENTER);

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxVERTICAL, this, "Compression Options");
	box->Add(grid, 0, wxALIGN_CENTER);

	sizer->Add(box, 0, wxEXPAND);
}

CompressionPanel::CompressionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(2, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_compressionToolChooserPanel = new DropDownBox((wxWindow *)topPanel, kCompressionToolChoice, "Choose Tool", kNumCompressionTools, kCompressionToolNames);
	_inputPanel = new IOChooser(topPanel, "Input", "", true);
	_compressionTypePanel = new DropDownBox(topPanel, kCompressionTypeChoice, "Choose Compression", kNumCompressionTypes, kCompressionTypeNames);
	_outputPanel = new IOChooser(topPanel, "Output", "", true);

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	_compressionOptionsPanel = new CompressionOptions(bottomPanel);
	_startButton = new wxButton(bottomPanel, kCompressionStartButton, "START");
	_toolOutput = new wxTextCtrl(bottomPanel, -1, "", wxDefaultPosition, wxSize(-1, 300), wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);

	topPanelSizer->Add(_compressionToolChooserPanel, 1, wxEXPAND);
	topPanelSizer->Add(_inputPanel, 4, wxEXPAND);
	topPanelSizer->Add(_compressionTypePanel, 1, wxEXPAND);
	topPanelSizer->Add(_outputPanel, 4, wxEXPAND);
	bottomPanelSizer->Add(_compressionOptionsPanel, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_startButton, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_toolOutput, 1, wxEXPAND);

	sizer->Add(topPanel, 0, wxEXPAND);
	sizer->Add(bottomPanel, 1, wxEXPAND);

	/* Simulate selecting the first tool and MP3 to set up the compression options */
	_compressionToolChooserPanel->_choice->SetSelection(0);
	wxCommandEvent toolEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kCompressionToolChoice);
	this->OnCompressionToolChange(toolEvent);

	_compressionTypePanel->_choice->SetSelection(0);
	wxCommandEvent compressionEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kCompressionTypeChoice);
	this->OnCompressionTypeChange(compressionEvent);
}

/* ----- Extraction ----- */

ExtractionOptions::ExtractionOptions(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	wxPanel *grid = new wxPanel(this);
	wxFlexGridSizer *gridSizer = new wxFlexGridSizer(2, 5, 0, 20);
	grid->SetSizer(gridSizer);

	wxStaticText *parallactionSmallLabel = new wxStaticText(grid, -1, "Small Archive");
	_parallactionSmall = new wxCheckBox(grid, -1, "");

	wxStaticText *kyraAmigaLabel = new wxStaticText(grid, -1, "Amiga .PAK File");
	_kyraAmiga = new wxCheckBox(grid, -1, "");

	wxStaticText *kyraAllFilesLabel = new wxStaticText(grid, -1, "Extract All Files");
	_kyraAllFiles = new wxCheckBox(grid, -1, "");

	wxStaticText *kyraSingleFileLabel = new wxStaticText(grid, -1, "Extract Single File");
	_kyraSingleFile = new wxCheckBox(grid, -1, "");

	wxStaticText *kyraFilenameLabel = new wxStaticText(grid, -1, "Filename");
	_kyraFilename = new wxTextCtrl(grid, -1, "");

	gridSizer->Add(parallactionSmallLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(kyraAmigaLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(kyraAllFilesLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(kyraSingleFileLabel, 0, wxALIGN_CENTER);
	gridSizer->Add(kyraFilenameLabel, 0, wxALIGN_CENTER);

	gridSizer->Add(_parallactionSmall, 0, wxALIGN_CENTER);
	gridSizer->Add(_kyraAmiga, 0, wxALIGN_CENTER);
	gridSizer->Add(_kyraAllFiles, 0, wxALIGN_CENTER);
	gridSizer->Add(_kyraSingleFile, 0, wxALIGN_CENTER);
	gridSizer->Add(_kyraFilename, 0, wxALIGN_CENTER);

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxVERTICAL, this, "Extraction Options");
	box->Add(grid, 0, wxALIGN_CENTER);

	sizer->Add(box, 0, wxEXPAND);
}

ExtractionPanel::ExtractionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(3, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_extractionToolChooserPanel = new DropDownBox((wxWindow *)topPanel, kExtractionToolChoice, "Choose Tool", kNumExtractionTools, kExtractionToolNames);
	_input1Panel = new IOChooser(topPanel, "Input 1", "", true);
	_input2Panel = new IOChooser(topPanel, "Input 2", "", true);
	_outputPanel = new IOChooser(topPanel, "Output", "", true);

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	_extractionOptionsPanel = new ExtractionOptions(bottomPanel);
	_startButton = new wxButton(bottomPanel, kExtractionStartButton, "START");
	_toolOutput = new wxTextCtrl(bottomPanel, -1, "", wxDefaultPosition, wxSize(-1, 300), wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);

	topPanelSizer->Add(_extractionToolChooserPanel, 1, wxEXPAND);
	topPanelSizer->Add(_input1Panel, 4, wxEXPAND);
	topPanelSizer->AddStretchSpacer();
	topPanelSizer->Add(_input2Panel, 4, wxEXPAND);
	topPanelSizer->AddStretchSpacer();
	topPanelSizer->Add(_outputPanel, 4, wxEXPAND);
	bottomPanelSizer->Add(_extractionOptionsPanel, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_startButton, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_toolOutput, 1, wxEXPAND);

	sizer->Add(topPanel, 0, wxEXPAND);
	sizer->Add(bottomPanel, 1, wxEXPAND);

	/* Simulate selecting the first tool and MP3 to set up the compression options */
	_extractionToolChooserPanel->_choice->SetSelection(0);
	wxCommandEvent toolEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kExtractionToolChoice);
	this->OnExtractionToolChange(toolEvent);
}

/* ----- Compression Events ----- */

void CompressionPanel::OnCompressionToolChange(wxCommandEvent &event) {
	wxString selectedTool = this->_compressionToolChooserPanel->_choice->GetStringSelection();

	this->_inputPanel->_browse->Enable(true);
	this->_inputPanel->_text->Enable(true);
	this->_outputPanel->_isFileChooser = false;

	if (selectedTool == "compress_agos") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_agos (MAC)") {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_kyra") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == "compress_queen") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_saga") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_scumm_bun") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == "compress_scumm_san") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == "compress_scumm_sou") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_sword1") {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_sword2") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == "compress_touche") {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == "encode_dxa") {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else {
		this->_inputPanel->_browse->Enable(false);
		this->_inputPanel->_text->Enable(false);
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_outputPanel->_isFileChooser = false;
	}
}

void CompressionPanel::OnCompressionTypeChange(wxCommandEvent &event) {
	wxString selectedCompression = this->_compressionTypePanel->_choice->GetStringSelection();

	if (selectedCompression == "MP3") {
		this->_compressionOptionsPanel->_minBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_avgBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_maxBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_vbrQualityChooser->Enable(true);
		this->_compressionOptionsPanel->_mpegQualityChooser->Enable(true);
		this->_compressionOptionsPanel->_compressionLevelChooser->Enable(false);
		this->_compressionOptionsPanel->_modeChooser->Enable(true);
		this->_compressionOptionsPanel->_blockSize->Enable(false);
		this->_compressionOptionsPanel->_verifyChooser->Enable(false);
		this->_compressionOptionsPanel->_silentChooser->Enable(true);
	} else if (selectedCompression == "Vorbis") {
		this->_compressionOptionsPanel->_minBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_avgBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_maxBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_vbrQualityChooser->Enable(true);
		this->_compressionOptionsPanel->_mpegQualityChooser->Enable(false);
		this->_compressionOptionsPanel->_compressionLevelChooser->Enable(false);
		this->_compressionOptionsPanel->_modeChooser->Enable(false);
		this->_compressionOptionsPanel->_blockSize->Enable(false);
		this->_compressionOptionsPanel->_verifyChooser->Enable(false);
		this->_compressionOptionsPanel->_silentChooser->Enable(true);
	} else if (selectedCompression == "FLAC") {
		this->_compressionOptionsPanel->_minBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_avgBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_maxBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->Enable(false);
		this->_compressionOptionsPanel->_mpegQualityChooser->Enable(false);
		this->_compressionOptionsPanel->_compressionLevelChooser->Enable(true);
		this->_compressionOptionsPanel->_modeChooser->Enable(false);
		this->_compressionOptionsPanel->_blockSize->Enable(true);
		this->_compressionOptionsPanel->_verifyChooser->Enable(true);
		this->_compressionOptionsPanel->_silentChooser->Enable(true);
	} else {
		this->_compressionOptionsPanel->_minBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_avgBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_maxBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->Enable(false);
		this->_compressionOptionsPanel->_mpegQualityChooser->Enable(false);
		this->_compressionOptionsPanel->_compressionLevelChooser->Enable(false);
		this->_compressionOptionsPanel->_modeChooser->Enable(false);
		this->_compressionOptionsPanel->_verifyChooser->Enable(false);
		this->_compressionOptionsPanel->_silentChooser->Enable(false);
	}
}

void CompressionPanel::OnCompressionStart(wxCommandEvent &event) {

}

/* ----- Extraction Events ----- */

void ExtractionPanel::OnExtractionToolChange(wxCommandEvent &event) {
	wxString selectedTool = this->_extractionToolChooserPanel->_choice->GetStringSelection();

	this->_input1Panel->_browse->Enable(true);
	this->_input1Panel->_text->Enable(true);
	this->_input1Panel->_isFileChooser = true;
	this->_input2Panel->_isFileChooser = true;
	this->_outputPanel->_isFileChooser = false;

	if (selectedTool == "extract_agos") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_kyra") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(true);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(true);
		this->_extractionOptionsPanel->_kyraFilename->Enable(true);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(true);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_loom_tg16") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_mm_apple") {
		this->_input2Panel->_browse->Enable(true);
		this->_input2Panel->_text->Enable(true);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_mm_c64") {
		this->_input2Panel->_browse->Enable(true);
		this->_input2Panel->_text->Enable(true);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_mm_nes") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_parallaction") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(true);
	} else if (selectedTool == "extract_scumm_mac") {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == "extract_zak_c64") {
		this->_input2Panel->_browse->Enable(true);
		this->_input2Panel->_text->Enable(true);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else {
		this->_input1Panel->_browse->Enable(false);
		this->_input1Panel->_text->Enable(false);
		this->_input1Panel->_isFileChooser = false;
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_input2Panel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_outputPanel->_isFileChooser = false;
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	}
}

void ExtractionPanel::OnExtractionStart(wxCommandEvent &event) {

}
