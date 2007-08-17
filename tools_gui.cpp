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
	EVT_BUTTON(kCompressionInputBrowse, CompressionPanel::OnCompressionInputBrowse)
	EVT_BUTTON(kCompressionOutputBrowse, CompressionPanel::OnCompressionOutputBrowse)
	EVT_BUTTON(kCompressionStartButton, CompressionPanel::OnCompressionStart)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( ExtractionPanel, wxPanel )
	EVT_CHOICE(kExtractionToolChoice, ExtractionPanel::OnExtractionToolChange)
	EVT_BUTTON(kExtractionInput1Browse, ExtractionPanel::OnExtractionInput1Browse)
	EVT_BUTTON(kExtractionInput2Browse, ExtractionPanel::OnExtractionInput2Browse)
	EVT_BUTTON(kExtractionOutputBrowse, ExtractionPanel::OnExtractionOutputBrowse)
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

MainFrame::MainFrame(const wxString& title) : wxFrame((wxFrame *)NULL, wxID_ANY, title, wxDefaultPosition, wxSize(600, 450)) {
	wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(mainSizer);

	_mainNotebook = new wxNotebook(this, wxID_ANY);

	wxPanel *compressionPage = new wxPanel(_mainNotebook);
	wxBoxSizer *compressionPageSizer = new wxBoxSizer(wxVERTICAL);
	compressionPage->SetSizer(compressionPageSizer);

	_compressionTools = new CompressionPanel(compressionPage);
	compressionPageSizer->Add(_compressionTools, 1, wxEXPAND);

	wxPanel *extractionPage = new wxPanel(_mainNotebook);
	wxBoxSizer *extractionPageSizer = new wxBoxSizer(wxVERTICAL);
	extractionPage->SetSizer(extractionPageSizer);

	_extractionTools = new ExtractionPanel(extractionPage);
	extractionPageSizer->Add(_extractionTools, 1, wxEXPAND);

	_mainNotebook->AddPage(compressionPage, wxT("Compression"), false, wxID_ANY);
	_mainNotebook->AddPage(extractionPage, wxT("Extraction"), false, wxID_ANY);

	mainSizer->Add(_mainNotebook, 1, wxEXPAND);
	this->SetMinSize(wxSize(600, 450));
}

LocationDialog::LocationDialog(wxTextCtrl *target, bool isFileChooser, wxString wildcard) {
	_isFileChooser = isFileChooser;
	_target = target;

	if (_isFileChooser) {
		_dialog = new wxFileDialog(NULL, wxFileSelectorPromptStr, wxT(""), wxT(""), wildcard, wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_MULTIPLE);
	} else {
		_dialog = new wxDirDialog(NULL);
	}
}

void LocationDialog::prompt() {
	if (this->_isFileChooser) {
		wxFileDialog *dialog = dynamic_cast<wxFileDialog*>(_dialog);

		if (dialog->ShowModal() == wxID_OK) {
			wxArrayString filenames;
			dialog->GetPaths(filenames);

			if (!this->_target->IsEmpty()) {
				this->_target->AppendText(wxT(" "));
			}

			for (size_t i = 0; i < filenames.GetCount(); i++) {
				this->_target->AppendText(wxT("\""));
				this->_target->AppendText(filenames[i]);
				this->_target->AppendText(wxT("\""));

				if (i != filenames.GetCount() - 1) {
					this->_target->AppendText(wxT(" "));
				}
			}
		}

		this->_target->SetInsertionPoint(0);
	} else {
		wxDirDialog *dialog = dynamic_cast<wxDirDialog*>(_dialog);

		if (dialog->ShowModal() == wxID_OK) {
			this->_target->SetValue(wxT("\""));
			this->_target->AppendText(dialog->GetPath());
			this->_target->AppendText(wxT("\""));
		}

		this->_target->SetInsertionPoint(0);
	}
}

FileDrop::FileDrop(wxTextCtrl *target, bool isFileChooser) : wxFileDropTarget() {
	_target = target;
	_target->SetDropTarget(this);
	_isFileChooser = isFileChooser;
}

bool FileDrop::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &filenames) {
	if (_target->IsEnabled()) {
		if (_isFileChooser) {
			if (!_target->IsEmpty()) {
				_target->AppendText(wxT(" "));
			}

			for (size_t i = 0; i < filenames.GetCount(); i++) {
				_target->AppendText(wxT("\""));
				_target->AppendText(filenames[i]);
				_target->AppendText(wxT("\""));

				if (i != filenames.GetCount() - 1) {
					_target->AppendText(wxT(" "));
				}
			}
		} else {
			_target->SetValue(wxT("\""));
			_target->AppendText(filenames[0]);
			_target->AppendText(wxT("\""));
		}

		_target->SetInsertionPoint(0);
	}

	return true;
}

IOChooser::IOChooser(wxWindow *parent, kEventId buttonId, wxString title, bool isFileChooser) : wxPanel(parent) {
	wxStaticBox *staticBox = new wxStaticBox(this, wxID_ANY, title);
	wxStaticBoxSizer *staticBoxSizer = new wxStaticBoxSizer(staticBox, wxHORIZONTAL);
	this->SetSizer(staticBoxSizer);

	wxPanel *panel = new wxPanel(this);
	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	panel->SetSizer(sizer);

	_text = new wxTextCtrl(panel, wxID_ANY, wxT(""));
	_browse = new wxButton(panel, buttonId, wxT("Browse"));
	_isFileChooser = isFileChooser;

	_dropTarget = new FileDrop(_text, _isFileChooser);

	sizer->Add(_text, 1, wxEXPAND | wxRIGHT, 5);
	sizer->Add(_browse, 0);

	staticBoxSizer->Add(panel, 1);
}

DropDownBox::DropDownBox(wxWindow *parent, kEventId boxId, wxString title, int numItems, wxString items[]) : wxPanel(parent) {
	wxStaticBox *box = new wxStaticBox(this, wxID_ANY, title);
	wxStaticBoxSizer *sizer = new wxStaticBoxSizer(box, wxHORIZONTAL);
	this->SetSizer(sizer);

	_choice = new wxChoice(this, boxId, wxDefaultPosition, wxDefaultSize, numItems, items);

	sizer->Add(_choice, 1, wxEXPAND);
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

	sizer->Add(grid, 0, wxEXPAND);
}

CompressionPanel::CompressionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(2, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_compressionToolChooserBox = new DropDownBox(topPanel, kCompressionToolChoice, wxT("Game Engine"), kNumCompressionTools, kCompressionToolNames);

	_compressionTypeBox = new DropDownBox(topPanel, kCompressionTypeChoice, wxT("Compression Type"), kNumCompressionTypes, kCompressionTypeNames);
	_compressionOptionsChooser = new wxCheckBox(_compressionTypeBox, kCompressionOptionsToggle, wxT("Advanced"));
	_compressionTypeBox->GetSizer()->Add(_compressionOptionsChooser, 1, wxEXPAND | wxLEFT | wxRIGHT, 10);

	_inputPanel = new IOChooser(topPanel, kCompressionInputBrowse, wxT("Input"), true);
	_outputPanel = new IOChooser(topPanel, kCompressionOutputBrowse, wxT("Output"), false);

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
	_toolOutput = new wxTextCtrl(bottomPanel, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_READONLY);

	topPanelSizer->Add(_compressionToolChooserBox, 0, wxEXPAND);
	topPanelSizer->Add(_inputPanel, 1, wxEXPAND);
	topPanelSizer->Add(_compressionTypeBox, 0, wxEXPAND);
	topPanelSizer->Add(_outputPanel, 1, wxEXPAND);
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
	gridSizer->AddGrowableCol(4);
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
	gridSizer->Add(_kyraFilename, 0, wxEXPAND);

	sizer->Add(grid, 0, wxEXPAND);
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
	_input1Panel = new IOChooser(topPanel, kExtractionInput1Browse, wxT("Input 1"), true);
	_input2Panel = new IOChooser(topPanel, kExtractionInput2Browse, wxT("Input 2"), true);
	_outputPanel = new IOChooser(topPanel, kExtractionOutputBrowse, wxT("Output"), false);

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	_extractionOptionsPanel = new ExtractionOptions(bottomPanel);
	_startButton = new wxButton(bottomPanel, kExtractionStartButton, wxT("START"));
	_toolOutput = new wxTextCtrl(bottomPanel, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE | wxTE_READONLY);

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
		this->_compressionOptionsPanel->_silentChooser->SetValue(true);
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
		this->_compressionOptionsPanel->_silentChooser->Enable(false);
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
		this->_minBitrateChooser->Enable(true);
		this->_maxBitrateChooser->Enable(true);
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

void CompressionPanel::OnCompressionInputBrowse(wxCommandEvent &event) {
	LocationDialog *dialog = new LocationDialog(this->_inputPanel->_text, this->_inputPanel->_isFileChooser, wxT("*.*"));
	dialog->prompt();

	dialog->_dialog->Destroy();
	delete dialog;
}

void CompressionPanel::OnCompressionOutputBrowse(wxCommandEvent &event) {
	LocationDialog *dialog = new LocationDialog(this->_outputPanel->_text, this->_outputPanel->_isFileChooser, wxT("*.*"));
	dialog->prompt();

	dialog->_dialog->Destroy();
	delete dialog;
}

void CompressionPanel::OnCompressionStart(wxCommandEvent &event) {
	this->_startButton->Enable(false);
	this->_toolOutput->Clear();

	bool done = false;
	size_t start = 1;
	size_t end;

	wxString selectedTool = kCompressionToolFilenames[this->_compressionToolChooserBox->_choice->GetSelection()];
	wxString compressionType = kCompressionTypeArguments[this->_compressionTypeBox->_choice->GetSelection()];
	wxString inputPath = this->_inputPanel->_text->GetValue();
	wxString outputPath = this->_outputPanel->_text->GetValue();
	wxArrayString inputFiles;

	wxString avgBitrate = this->_compressionOptionsPanel->_avgBitrateChooser->GetStringSelection();
	wxString blocksize = this->_compressionOptionsPanel->_blockSize->GetStringSelection();
	wxString compressionLevel = this->_compressionOptionsPanel->_compressionLevelChooser->GetStringSelection();
	wxString maxBitrate = this->_compressionOptionsPanel->_maxBitrateChooser->GetStringSelection();
	wxString minBitrate = this->_compressionOptionsPanel->_minBitrateChooser->GetStringSelection();
	wxString mode = this->_compressionOptionsPanel->_modeChooser->GetStringSelection();
	wxString mpegQuality = this->_compressionOptionsPanel->_mpegQualityChooser->GetStringSelection();
	bool isSilent = this->_compressionOptionsPanel->_silentChooser->IsChecked();
	wxString vbrQuality = this->_compressionOptionsPanel->_vbrQualityChooser->GetStringSelection();
	bool isVerify = this->_compressionOptionsPanel->_verifyChooser->IsChecked();

	if (!inputPath.IsEmpty()) {
		while (!done) {
			end = inputPath.find(wxT('"'), start);
			inputFiles.Add(inputPath.Mid(start - 1, end - start + 2));
			start = end + 3;

			if ((end + 1) == inputPath.Len()) {
				done = true;
			}
		}

		for (size_t x = 0; x < inputFiles.Count(); x++) {
			wxString commandString = wxT("");

#ifndef __WXMSW__
			commandString += wxT("./");
#endif
			commandString += selectedTool;
			commandString += wxT(" ");
			commandString += compressionType;
			commandString += wxT(" ");

			if (compressionType.IsSameAs(kCompressionTypeArguments[0])) { /* MP3 */
				if (mode.IsSameAs(kMP3ModeNames[0])) { /* VBR */
					commandString += wxT("--vbr ");
					commandString += wxT("-V ");
					commandString += vbrQuality;
					commandString += wxT(" ");
					commandString += wxT("-q ");
					commandString += mpegQuality;
					commandString += wxT(" ");

					if (!minBitrate.IsSameAs(kValidBitrateNames[0])) {
						commandString += wxT("-b ");
						commandString += minBitrate;
						commandString += wxT(" ");
					}

					if (!maxBitrate.IsSameAs(kValidBitrateNames[0])) {
						commandString += wxT("-B ");
						commandString += maxBitrate;
						commandString += wxT(" ");
					}
				} else { /* ABR */
					commandString += wxT("--abr ");

					if (avgBitrate.IsSameAs(kValidBitrateNames[0])) {
						commandString += kDefaultMP3ABRAvgBitrate;
					} else {
						commandString += avgBitrate;
					}

					commandString += wxT(" ");
					commandString += wxT("-q ");
					commandString += mpegQuality;
					commandString += wxT(" ");

					if (!minBitrate.IsSameAs(kValidBitrateNames[0])) {
						commandString += wxT("-b ");
						commandString += minBitrate;
						commandString += wxT(" ");
					}

					if (!maxBitrate.IsSameAs(kValidBitrateNames[0])) {
						commandString += wxT("-B ");
						commandString += maxBitrate;
						commandString += wxT(" ");
					}
				}

				commandString += wxT("--silent ");
			} else if (compressionType.IsSameAs(kCompressionTypeArguments[1])) { /* Vorbis */
				commandString += wxT("-q ");
				commandString += vbrQuality;
				commandString += wxT(" ");

				if (!avgBitrate.IsSameAs(kValidBitrateNames[0])) {
					commandString += wxT("-b ");
					commandString += avgBitrate;
					commandString += wxT(" ");
				}

				if (!minBitrate.IsSameAs(kValidBitrateNames[0])) {
					commandString += wxT("-m ");
					commandString += minBitrate;
					commandString += wxT(" ");
				}

				if (!maxBitrate.IsSameAs(kValidBitrateNames[0])) {
					commandString += wxT("-M ");
					commandString += maxBitrate;
					commandString += wxT(" ");
				}

				if (isSilent) {
					commandString += wxT("--silent ");
				}
			} else { /* FLAC */
				commandString += wxT("-");
				commandString += compressionLevel;
				commandString += wxT(" ");
				commandString += wxT("-b ");
				commandString += blocksize;
				commandString += wxT(" ");

				if (isVerify) {
					commandString += wxT("--verify ");
				}

				if (isSilent) {
					commandString += wxT("--silent ");
				}
			}

			commandString += inputFiles.Item(x);
			if (!outputPath.IsEmpty()) {
				commandString += wxT(" ");
				commandString += outputPath;
			}

			this->_toolOutput->AppendText(commandString);
			this->_toolOutput->AppendText(wxT("\n\n"));

			wxProcess *command = new wxProcess(wxPROCESS_REDIRECT);

			wxExecute(commandString, wxEXEC_ASYNC, command);

			while (!command->GetInputStream()->Eof()) {
				wxChar outputChar = command->GetInputStream()->GetC();
				if (command->GetInputStream()->LastRead() != 0) {
					this->_toolOutput->AppendText(outputChar);
				}
			}

			this->_toolOutput->AppendText(wxT("\n------------------------------\n"));
			this->_toolOutput->AppendText(wxT("Operation Finished\n"));
			this->_toolOutput->AppendText(wxT("------------------------------\n"));
			this->_toolOutput->AppendText(wxT("\n"));
		}
	}

	this->_startButton->Enable(true);
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

void ExtractionPanel::OnExtractionInput1Browse(wxCommandEvent &event) {
	LocationDialog *dialog = new LocationDialog(this->_input1Panel->_text, this->_input1Panel->_isFileChooser, wxT("*.*"));
	dialog->prompt();

	dialog->_dialog->Destroy();
	delete dialog;
}

void ExtractionPanel::OnExtractionInput2Browse(wxCommandEvent &event) {
	LocationDialog *dialog = new LocationDialog(this->_input2Panel->_text, this->_input2Panel->_isFileChooser, wxT("*.*"));
	dialog->prompt();

	dialog->_dialog->Destroy();
	delete dialog;
}

void ExtractionPanel::OnExtractionOutputBrowse(wxCommandEvent &event) {
	LocationDialog *dialog = new LocationDialog(this->_outputPanel->_text, this->_outputPanel->_isFileChooser, wxT("*.*"));
	dialog->prompt();

	dialog->_dialog->Destroy();
	delete dialog;
}

void ExtractionPanel::OnExtractionStart(wxCommandEvent &event) {
	this->_startButton->Enable(false);
	this->_toolOutput->Clear();

	bool done = false;
	size_t start = 1;
	size_t end;

	wxString selectedTool = kExtractionToolFilenames[this->_extractionToolChooserPanel->_choice->GetSelection()];
	wxString input1Path = this->_input1Panel->_text->GetValue();
	wxString input2Path = this->_input2Panel->_text->GetValue();
	wxString outputPath = this->_outputPanel->_text->GetValue();
	wxArrayString inputFiles;

	bool kyraAllFiles = this->_extractionOptionsPanel->_kyraAllFiles->IsChecked();
	bool kyraAmiga = this->_extractionOptionsPanel->_kyraAmiga->IsChecked();
	wxString kyraFilename = this->_extractionOptionsPanel->_kyraFilename->GetValue();
	bool kyraSingleFile = this->_extractionOptionsPanel->_kyraSingleFile->IsChecked();
	bool parallactionSmall = this->_extractionOptionsPanel->_parallactionSmall->IsChecked();

	if (!input1Path.IsEmpty()) {
		while (!done) {
			end = input1Path.find(wxT('"'), start);
			inputFiles.Add(input1Path.Mid(start - 1, end - start + 2));
			start = end + 3;

			if ((end + 1) == input1Path.Len()) {
				done = true;
			}
		}

		for (size_t x = 0; x < inputFiles.Count(); x++) {
			wxString commandString = wxT("");

#ifndef __WXMSW__
			commandString += wxT("./");
#endif
			commandString += selectedTool;
			commandString += wxT(" ");

			if (kyraAllFiles) {
				commandString += wxT("-x ");
			}

			if (kyraAmiga) {
				commandString += wxT("-a ");
			}

			if (kyraSingleFile) {
				commandString += wxT("-o ");
				commandString += kyraFilename;
				commandString += wxT(" ");
			}

			if (parallactionSmall) {
				commandString += wxT("--small");
			}

			commandString += inputFiles.Item(x);

			if (!input2Path.IsEmpty()) {
				commandString += wxT(" ");
				commandString += input2Path;
			}
			if (!outputPath.IsEmpty()) {
				commandString += wxT(" ");
				commandString += outputPath;
			}

			this->_toolOutput->AppendText(commandString);
			this->_toolOutput->AppendText(wxT("\n\n"));

			wxProcess *command = new wxProcess(wxPROCESS_REDIRECT);
			wxExecute(commandString, wxEXEC_ASYNC, command);

			while (!command->GetInputStream()->Eof()) {
				wxChar outputChar = command->GetInputStream()->GetC();
				if (command->GetInputStream()->LastRead() != 0) {
#ifdef __WXMSW__
					if (outputChar != 10) {
						this->_toolOutput->AppendText(outputChar);
					}
#else
					this->_toolOutput->AppendText(outputChar);
#endif
				}
			}

			this->_toolOutput->AppendText(wxT("\n------------------------------\n"));
			this->_toolOutput->AppendText(wxT("Operation Finished\n"));
			this->_toolOutput->AppendText(wxT("------------------------------\n"));
			this->_toolOutput->AppendText(wxT("\n"));
		}
	}

	this->_startButton->Enable(true);
}

void MainFrame::OnCompressionOptionsToggle(wxCommandEvent &event) {
	this->_compressionTools->_compressionOptionsPanel->Show(!this->_compressionTools->_compressionOptionsPanel->IsShown());

	this->_compressionTools->Fit();
	this->_compressionTools->SetSize(this->_mainNotebook->GetPage(0)->GetSize());

	this->_extractionTools->Fit();
	this->_extractionTools->SetSize(this->_mainNotebook->GetPage(1)->GetSize());
}
