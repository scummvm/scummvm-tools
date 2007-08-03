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

BEGIN_EVENT_TABLE( CompressionOptions, wxPanel )
	EVT_CHOICE(kCompressionModeChoice, CompressionOptions::OnCompressionModeChange)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( CompressionPanel, wxPanel )
	EVT_CHOICE(kCompressionToolChoice, CompressionPanel::OnCompressionToolChange)
	EVT_CHOICE(kCompressionTypeChoice, CompressionPanel::OnCompressionTypeChange)
	EVT_BUTTON(kCompressionStartButton, CompressionPanel::OnCompressionStart)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( ExtractionPanel, wxPanel )
	EVT_CHOICE(kExtractionToolChoice, ExtractionPanel::OnExtractionToolChange)
	EVT_BUTTON(kExtractionStartButton, ExtractionPanel::OnExtractionStart)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( MainFrame, wxFrame)
	EVT_CHECKBOX(kCompressionOptionsToggle, MainFrame::OnCompressionOptionsToggle)
END_EVENT_TABLE()

bool ToolsGui::OnInit() {
	MainFrame *frame = new MainFrame(wxT("ScummVM Tools"));

	frame->Show();
	SetTopWindow(frame);

	return true;
}

MainFrame::MainFrame(const wxString& title) : wxFrame((wxFrame *)NULL, wxID_ANY, title) {
	wxBoxSizer *mainSizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(mainSizer);

	_mainNotebook = new wxNotebook(this, wxID_ANY);

	_compressionTools = new CompressionPanel(_mainNotebook);
	_extractionTools = new ExtractionPanel(_mainNotebook);

	_mainNotebook->AddPage(_compressionTools, wxT("Compression"), false, wxID_ANY);
	_mainNotebook->AddPage(_extractionTools, wxT("Extraction"), false, wxID_ANY);

	mainSizer->Add(_mainNotebook, 1, wxEXPAND);
	mainSizer->SetSizeHints(this);
}

DropDownBox::DropDownBox(wxWindow *parent, wxWindowID id, wxString title, int numItems, wxString items[]) : wxPanel(parent) {
	wxStaticBox *box = new wxStaticBox(this, wxID_ANY, title);
	wxStaticBoxSizer *sizer = new wxStaticBoxSizer(box, wxHORIZONTAL);
	this->SetSizer(sizer);

	_choice = new wxChoice(this, id, wxDefaultPosition, wxDefaultSize, numItems, items);

	sizer->Add(_choice, 1, wxEXPAND | wxLEFT | wxRIGHT, 5);
}

FileDrop::FileDrop(wxTextCtrl *target, bool isFileChooser) : wxFileDropTarget() {
	_target = target;
	_isFileChooser = isFileChooser;
}

bool FileDrop::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &filenames) {
	if (_isFileChooser) {
		if (_target->GetValue().Last() != wxChar(" ")) {
			_target->AppendText(" ");
		}

		for (size_t i = 0; i < filenames.GetCount(); i++) {
			_target->AppendText("\"");
			_target->AppendText(filenames[i]);
			_target->AppendText("\"");
			
			if (i != filenames.GetCount() - 1) {
				_target->AppendText(" ");
			}
		}
	} else {
		_target->SetValue("\"");
		_target->AppendText(filenames[0]);
		_target->AppendText("\"");
	}

	return true;
};

IOChooser::IOChooser(wxWindow *parent, wxString title, wxString defaultPath, bool isFileChooser) : wxPanel(parent) {
	wxStaticBox *box = new wxStaticBox(this, wxID_ANY, title);
	wxStaticBoxSizer *sizer = new wxStaticBoxSizer(box, wxHORIZONTAL);
	this->SetSizer(sizer);

	_text = new wxTextCtrl(this, wxID_ANY, defaultPath);
	_browse = new wxButton(this, wxID_ANY, wxT("Browse"));
	_isFileChooser = isFileChooser;

	_dropTarget = new FileDrop(_text, _isFileChooser);
	this->SetDropTarget(_dropTarget);

	/* The button looks like it is shifted 2 pixels down 
	 * from the text control (probably because of the wxStaticBox label)
	 * so we simply pad the top by -2
	 */
	sizer->Add(_text, 3, wxBOTTOM | wxLEFT | wxRIGHT, 5);
	sizer->Add(_browse, 1, wxTOP, -2);
}

/* ----- Compression ----- */

CompressionOptions::CompressionOptions(wxWindow *parent) : wxPanel(parent) {
	wxStaticBox *box = new wxStaticBox(this, wxID_ANY, wxT("Compression Options"));
	wxStaticBoxSizer *sizer = new wxStaticBoxSizer(box, wxVERTICAL);
	this->SetSizer(sizer);

	wxPanel *grid = new wxPanel(this);
	wxGridSizer *gridSizer = new wxGridSizer(2, 5, 0, 10);
	grid->SetSizer(gridSizer);

	wxStaticText *minBitrateLabel = new wxStaticText(grid, wxID_ANY, wxT("Minimum Bitrate"));
	_minBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *avgBitrateLabel = new wxStaticText(grid, wxID_ANY, wxT("Average Bitrate"));
	_avgBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *maxBitrateLabel = new wxStaticText(grid, wxID_ANY, wxT("Maximum Bitrate"));
	_maxBitrateChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *vbrQualityLabel = new wxStaticText(grid, wxID_ANY, wxT("VBR Quality"));
	_vbrQualityChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *mpegQualityLabel = new wxStaticText(grid, wxID_ANY, wxT("MPEG Quality"));
	_mpegQualityChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *compressionLevelLabel = new wxStaticText(grid, wxID_ANY, wxT("Compression Level"));
	_compressionLevelChooser = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidCompressionLevels, kVaildCompressionLevels);

	wxStaticText *modeLabel = new wxStaticText(grid, wxID_ANY, wxT("Compression Mode"));
	_modeChooser = new wxChoice(grid, kCompressionModeChoice, wxDefaultPosition, wxDefaultSize, kNumMP3Modes, kMP3ModeNames);

	wxStaticText *blockSizeLabel = new wxStaticText(grid, wxID_ANY, wxT("Block Size"));
	_blockSize = new wxChoice(grid, wxID_ANY, wxDefaultPosition, wxDefaultSize, kNumValidFlacBlocksize, kValidFlacBlocksize);

	wxStaticText *verifyLabel = new wxStaticText(grid, wxID_ANY, wxT("Verify"));
	_verifyChooser = new wxCheckBox(grid, wxID_ANY, wxT(""));

	wxStaticText *silentLabel = new wxStaticText(grid, wxID_ANY, wxT("Silent"));
	_silentChooser = new wxCheckBox(grid, wxID_ANY, wxT(""));

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

	sizer->Add(grid, 0, wxALIGN_CENTER);
}

CompressionPanel::CompressionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(2, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_compressionToolChooserBox = new DropDownBox((wxWindow *)topPanel, kCompressionToolChoice, wxT("Game Engine"), kNumCompressionTools, kCompressionToolNames);

	_compressionTypeBox = new DropDownBox(topPanel, kCompressionTypeChoice, wxT("Compression Type"), kNumCompressionTypes, kCompressionTypeNames);
	_compressionOptionsChooser = new wxCheckBox(_compressionTypeBox, kCompressionOptionsToggle, wxT("Advanced"));
	_compressionTypeBox->GetSizer()->Add(_compressionOptionsChooser, 1, wxEXPAND | wxLEFT | wxRIGHT, 10);

	_inputPanel = new IOChooser(topPanel, wxT("Input"), wxT(""), true);
	_outputPanel = new IOChooser(topPanel, wxT("Output"), wxT(""), false);

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	/* Initially hide the advanced compression options 
	 * They can be shown by toggling _compressionOptionsChooser
	 */
	_compressionOptionsPanel = new CompressionOptions(bottomPanel);
	_compressionOptionsPanel->Show(false);

	_startButton = new wxButton(bottomPanel, kCompressionStartButton, wxT("START"));
	_toolOutput = new wxTextCtrl(bottomPanel, wxID_ANY, wxT(""), wxDefaultPosition, wxSize(-1, 300), wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);

	topPanelSizer->Add(_compressionToolChooserBox, 1, wxEXPAND);
	topPanelSizer->Add(_inputPanel, 4, wxEXPAND);
	topPanelSizer->Add(_compressionTypeBox, 1, wxEXPAND);
	topPanelSizer->Add(_outputPanel, 4, wxEXPAND);
	bottomPanelSizer->Add(_startButton, 0, wxEXPAND | wxTOP, 5);
	bottomPanelSizer->Add(_compressionOptionsPanel, 0, wxEXPAND);
	bottomPanelSizer->Add(_toolOutput, 1, wxEXPAND | wxTOP, 5);

	sizer->Add(topPanel, 0, wxEXPAND);
	sizer->Add(bottomPanel, 1, wxEXPAND);

	/* Simulate selecting the first tool to set up the compression options */
	_compressionToolChooserBox->_choice->SetSelection(0);
	wxCommandEvent toolEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kCompressionToolChoice);
	this->OnCompressionToolChange(toolEvent);

	/* Simulate selecting the first compression type to set up the compression options */
	_compressionTypeBox->_choice->SetSelection(0);
	wxCommandEvent compressionEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kCompressionTypeChoice);
	this->OnCompressionTypeChange(compressionEvent);
}

/* ----- Extraction ----- */

ExtractionOptions::ExtractionOptions(wxWindow *parent) : wxPanel(parent) {
	wxStaticBox *box = new wxStaticBox(this, wxID_ANY, wxT("Extraction Options"));
	wxStaticBoxSizer *sizer = new wxStaticBoxSizer(box, wxVERTICAL);
	this->SetSizer(sizer);

	wxPanel *grid = new wxPanel(this);
	wxFlexGridSizer *gridSizer = new wxFlexGridSizer(2, 5, 0, 20);
	grid->SetSizer(gridSizer);

	wxStaticText *parallactionSmallLabel = new wxStaticText(grid, wxID_ANY, wxT("Small Archive"));
	_parallactionSmall = new wxCheckBox(grid, wxID_ANY, wxT(""));

	wxStaticText *kyraAmigaLabel = new wxStaticText(grid, wxID_ANY, wxT("Amiga .PAK File"));
	_kyraAmiga = new wxCheckBox(grid, wxID_ANY, wxT(""));

	wxStaticText *kyraAllFilesLabel = new wxStaticText(grid, wxID_ANY, wxT("Extract All Files"));
	_kyraAllFiles = new wxCheckBox(grid, wxID_ANY, wxT(""));

	wxStaticText *kyraSingleFileLabel = new wxStaticText(grid, wxID_ANY, wxT("Extract Single File"));
	_kyraSingleFile = new wxCheckBox(grid, wxID_ANY, wxT(""));

	wxStaticText *kyraFilenameLabel = new wxStaticText(grid, wxID_ANY, wxT("Filename"));
	_kyraFilename = new wxTextCtrl(grid, wxID_ANY, wxT(""));

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

	sizer->Add(grid, 0, wxALIGN_CENTER);
}

ExtractionPanel::ExtractionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(3, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_extractionToolChooserPanel = new DropDownBox((wxWindow *)topPanel, kExtractionToolChoice, wxT("Game Engine"), kNumExtractionTools, kExtractionToolNames);
	_input1Panel = new IOChooser(topPanel, wxT("Input 1"), wxT(""), true);
	_input2Panel = new IOChooser(topPanel, wxT("Input 2"), wxT(""), true);
	_outputPanel = new IOChooser(topPanel, wxT("Output"), wxT(""), false);

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	_extractionOptionsPanel = new ExtractionOptions(bottomPanel);
	_startButton = new wxButton(bottomPanel, kExtractionStartButton, wxT("START"));
	_toolOutput = new wxTextCtrl(bottomPanel, wxID_ANY, wxT(""), wxDefaultPosition, wxSize(-1, 300), wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);

	topPanelSizer->Add(_extractionToolChooserPanel, 1, wxEXPAND);
	topPanelSizer->Add(_input1Panel, 5, wxEXPAND);
	topPanelSizer->AddStretchSpacer();
	topPanelSizer->Add(_input2Panel, 5, wxEXPAND);
	topPanelSizer->AddStretchSpacer();
	topPanelSizer->Add(_outputPanel, 5, wxEXPAND);
	bottomPanelSizer->Add(_extractionOptionsPanel, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_startButton, 0, wxEXPAND | wxBOTTOM, 5);
	bottomPanelSizer->Add(_toolOutput, 1, wxEXPAND);

	sizer->Add(topPanel, 0, wxEXPAND);
	sizer->Add(bottomPanel, 1, wxEXPAND);

	/* Simulate selecting the first tool to set up the extraction options */
	_extractionToolChooserPanel->_choice->SetSelection(0);
	wxCommandEvent toolEvent = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kExtractionToolChoice);
	this->OnExtractionToolChange(toolEvent);
}

/* ----- Compression Events ----- */

void CompressionPanel::OnCompressionToolChange(wxCommandEvent &event) {
	wxString selectedTool = this->_compressionToolChooserBox->_choice->GetStringSelection();

	this->_inputPanel->_browse->Enable(true);
	this->_inputPanel->_text->Enable(true);
	this->_inputPanel->_text->Clear();
	this->_outputPanel->_isFileChooser = false;
	this->_outputPanel->_text->Clear();

	if (selectedTool == wxT("AGOS")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Broken Sword 1")) {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Broken Sword 2")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Encode DXA")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Flight of the Amazon Queen")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Kyra")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == wxT("SAGA")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("SCUMM BUN")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == wxT("SCUMM SAN")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
	} else if (selectedTool == wxT("SCUMM SOU")) {
		this->_inputPanel->_isFileChooser = true;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Simon 2 (MAC)")) {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
	} else if (selectedTool == wxT("Touche")) {
		this->_inputPanel->_isFileChooser = false;
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
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
	wxString selectedCompression = this->_compressionTypeBox->_choice->GetStringSelection();

	if (selectedCompression == wxT("MP3")) {
		this->_compressionOptionsPanel->_avgBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_blockSize->SetSelection(0);
		this->_compressionOptionsPanel->_compressionLevelChooser->SetSelection(0);
		this->_compressionOptionsPanel->_maxBitrateChooser->SetStringSelection(kDefaultMP3VBRMaxBitrate);
		this->_compressionOptionsPanel->_minBitrateChooser->SetStringSelection(kDefaultMP3VBRMinBitrate);
		this->_compressionOptionsPanel->_modeChooser->SetStringSelection(kDefaultMP3CompressionType);
		this->_compressionOptionsPanel->_mpegQualityChooser->SetStringSelection(kDefaultMP3MpegQuality);
		this->_compressionOptionsPanel->_silentChooser->SetValue(false);
		this->_compressionOptionsPanel->_verifyChooser->SetValue(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->SetStringSelection(kDefaultMP3VBRQuality);

		this->_compressionOptionsPanel->_minBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_avgBitrateChooser->Enable(false);
		this->_compressionOptionsPanel->_maxBitrateChooser->Enable(true);
		this->_compressionOptionsPanel->_vbrQualityChooser->Enable(true);
		this->_compressionOptionsPanel->_mpegQualityChooser->Enable(true);
		this->_compressionOptionsPanel->_compressionLevelChooser->Enable(false);
		this->_compressionOptionsPanel->_modeChooser->Enable(true);
		this->_compressionOptionsPanel->_blockSize->Enable(false);
		this->_compressionOptionsPanel->_verifyChooser->Enable(false);
		this->_compressionOptionsPanel->_silentChooser->Enable(true);
	} else if (selectedCompression == wxT("Vorbis")) {
		this->_compressionOptionsPanel->_avgBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_blockSize->SetSelection(0);
		this->_compressionOptionsPanel->_compressionLevelChooser->SetSelection(0);
		this->_compressionOptionsPanel->_maxBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_minBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_modeChooser->SetSelection(0);
		this->_compressionOptionsPanel->_mpegQualityChooser->SetSelection(0);
		this->_compressionOptionsPanel->_silentChooser->SetValue(false);
		this->_compressionOptionsPanel->_verifyChooser->SetValue(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->SetStringSelection(kDefaultOggQuality);

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
	} else if (selectedCompression == wxT("FLAC")) {
		this->_compressionOptionsPanel->_avgBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_blockSize->SetStringSelection(kDefaultFlacBlocksize);
		this->_compressionOptionsPanel->_compressionLevelChooser->SetStringSelection(kDefaultFlacCompress);
		this->_compressionOptionsPanel->_maxBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_minBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_modeChooser->SetSelection(0);
		this->_compressionOptionsPanel->_mpegQualityChooser->SetSelection(0);
		this->_compressionOptionsPanel->_silentChooser->SetValue(false);
		this->_compressionOptionsPanel->_verifyChooser->SetValue(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->SetSelection(0);

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
		this->_compressionOptionsPanel->_avgBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_blockSize->SetSelection(0);
		this->_compressionOptionsPanel->_compressionLevelChooser->SetSelection(0);
		this->_compressionOptionsPanel->_maxBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_minBitrateChooser->SetSelection(0);
		this->_compressionOptionsPanel->_modeChooser->SetSelection(0);
		this->_compressionOptionsPanel->_mpegQualityChooser->SetSelection(0);
		this->_compressionOptionsPanel->_silentChooser->SetValue(false);
		this->_compressionOptionsPanel->_verifyChooser->SetValue(false);
		this->_compressionOptionsPanel->_vbrQualityChooser->SetSelection(0);

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

void CompressionOptions::OnCompressionModeChange(wxCommandEvent &event) {
	wxString selectedMode = this->_modeChooser->GetStringSelection();

	if (selectedMode == wxT("VBR")) {
		this->_avgBitrateChooser->SetSelection(0);
		this->_maxBitrateChooser->SetStringSelection(kDefaultMP3VBRMaxBitrate);
		this->_minBitrateChooser->SetStringSelection(kDefaultMP3VBRMinBitrate);
		this->_vbrQualityChooser->SetStringSelection(kDefaultMP3VBRQuality);

		this->_avgBitrateChooser->Enable(false);
		this->_minBitrateChooser->Enable(true);
		this->_maxBitrateChooser->Enable(true);
		this->_vbrQualityChooser->Enable(true);
	} else if (selectedMode == wxT("ABR")) {
		this->_avgBitrateChooser->SetStringSelection(kDefaultMP3ABRAvgBitrate);
		this->_maxBitrateChooser->SetSelection(0);
		this->_minBitrateChooser->SetSelection(0);
		this->_vbrQualityChooser->SetSelection(0);
	
		this->_avgBitrateChooser->Enable(true);
		this->_minBitrateChooser->Enable(false);
		this->_maxBitrateChooser->Enable(false);
		this->_vbrQualityChooser->Enable(false);
	} else {
		this->_avgBitrateChooser->SetSelection(0);
		this->_maxBitrateChooser->SetSelection(0);
		this->_minBitrateChooser->SetSelection(0);
		this->_vbrQualityChooser->SetSelection(0);

		this->_avgBitrateChooser->Enable(false);
		this->_minBitrateChooser->Enable(false);
		this->_maxBitrateChooser->Enable(false);
		this->_vbrQualityChooser->Enable(false);
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
	this->_input1Panel->_text->Clear();
	this->_input2Panel->_isFileChooser = true;
	this->_input2Panel->_text->Clear();
	this->_outputPanel->_isFileChooser = false;
	this->_outputPanel->_text->Clear();

	if (selectedTool == wxT("AGOS")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Kyra")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(true);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(true);
		this->_extractionOptionsPanel->_kyraFilename->Enable(true);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(true);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Loom (TG16)")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Maniac Mansion (Apple)")) {
		this->_input2Panel->_browse->Enable(true);
		this->_input2Panel->_text->Enable(true);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Maniac Mansion (C64)")) {
		this->_input2Panel->_browse->Enable(true);
		this->_input2Panel->_text->Enable(true);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Maniac Mansion (NES)")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Parallaction")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(true);
		this->_outputPanel->_text->Enable(true);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(true);
	} else if (selectedTool == wxT("SCUMM (MAC)")) {
		this->_input2Panel->_browse->Enable(false);
		this->_input2Panel->_text->Enable(false);
		this->_outputPanel->_browse->Enable(false);
		this->_outputPanel->_text->Enable(false);
		this->_extractionOptionsPanel->_kyraAllFiles->Enable(false);
		this->_extractionOptionsPanel->_kyraAmiga->Enable(false);
		this->_extractionOptionsPanel->_kyraFilename->Enable(false);
		this->_extractionOptionsPanel->_kyraSingleFile->Enable(false);
		this->_extractionOptionsPanel->_parallactionSmall->Enable(false);
	} else if (selectedTool == wxT("Zak McKracken (C64)")) {
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

void MainFrame::OnCompressionOptionsToggle(wxCommandEvent &event) {
	int height, width;

	this->_compressionTools->_compressionOptionsPanel->Show(!this->_compressionTools->_compressionOptionsPanel->IsShown());
	this->_compressionTools->Fit();
	this->_compressionTools->SetSize(this->_compressionTools->GetSize().GetWidth() + 10, this->_compressionTools->GetSize().GetHeight());

	this->_extractionTools->Fit();

	if (this->_compressionTools->GetSize().GetWidth() >= this->_extractionTools->GetSize().GetWidth()) {
		width = this->_compressionTools->GetSize().GetWidth();
	} else {
		width = this->_extractionTools->GetSize().GetWidth();
	}

	if (this->_compressionTools->GetSize().GetHeight() >= this->_extractionTools->GetSize().GetHeight()) {
		height = this->_compressionTools->GetSize().GetHeight();
	} else {
		height = this->_extractionTools->GetSize().GetHeight();
	}

	this->_mainNotebook->SetMinSize(wxSize(width, height));
	this->_mainNotebook->Fit();

	this->SetMinSize(wxSize(width, height));
	this->Fit();
}
