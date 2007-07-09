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
	EVT_CHOICE(kCompressionChoice, CompressionPanel::OnCompressionChange)
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
	wxPanel *extractionTools = new wxPanel(mainNotebook);
	CompressionPanel *compressionTools = new CompressionPanel(mainNotebook);
	wxPanel *scriptTools = new wxPanel(mainNotebook);
	wxPanel *encoderTools = new wxPanel(mainNotebook);

	mainNotebook->AddPage(compressionTools, "Compression", false, -1);
	mainNotebook->AddPage(encoderTools, "Encoder", false, -1);
	mainNotebook->AddPage(extractionTools, "Extraction", false, -1);
	mainNotebook->AddPage(scriptTools, "Script", false, -1);

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

IOChooser::IOChooser(wxWindow *parent, wxString title, wxString defaultPath) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(sizer);

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, this, title);
	_text = new wxTextCtrl(this, -1, defaultPath);
	_browse = new wxButton(this, -1, "Browse");

	/* The button looks like it is shifted 2 pixels down from the text control
	 * so we simply pad the top by -2
	 */
	box->Add(_text, 3, wxBOTTOM | wxLEFT | wxRIGHT, 5);
	box->Add(_browse, 1, wxTOP, -2);

	sizer->Add(box, 1, wxEXPAND);
}

CompressionOptions::CompressionOptions(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	this->SetSizer(sizer);
	
	wxPanel *grid = new wxPanel(this);
	wxGridSizer *gridSizer = new wxGridSizer(5, 0, 10);
	grid->SetSizer(gridSizer);

	wxStaticText *minBitrateLabel = new wxStaticText(grid, -1, "Minimum Bitrate", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_minBitrateChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *avgBitrateLabel = new wxStaticText(grid, -1, "Average Bitrate", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_avgBitrateChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *maxBitrateLabel = new wxStaticText(grid, -1, "Maximum Bitrate", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_maxBitrateChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidBitrates, kValidBitrateNames);

	wxStaticText *vbrQualityLabel = new wxStaticText(grid, -1, "VBR Quality", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_vbrQualityChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *mpegQualityLabel = new wxStaticText(grid, -1, "MPEG Quality", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_mpegQualityChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidQuality, kVaildQualityNames);

	wxStaticText *compressionLevelLabel = new wxStaticText(grid, -1, "Compression Level", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_compressionLevelChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumValidCompressionLevels, kVaildCompressionLevels);

	wxStaticText *modeLabel = new wxStaticText(grid, -1, "Compression Mode", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_modeChooser = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumMP3Modes, kMP3ModeNames);

	wxStaticText *blockSizeLabel = new wxStaticText(grid, -1, "Block Size", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_blockSize = new wxChoice(grid, -1, wxDefaultPosition, wxDefaultSize, kNumFLACBlocksize, kFLACBlocksize);

	wxStaticText *verifyLabel = new wxStaticText(grid, -1, "Verify", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_verifyChooser = new wxCheckBox(grid, -1, "");

	wxStaticText *silentLabel = new wxStaticText(grid, -1, "Silent", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	_silentChooser = new wxCheckBox(grid, -1, "");

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

	wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, this, "Compression Options");
	box->Add(grid, 0, wxEXPAND);
	
	sizer->Add(box);
}

CompressionPanel::CompressionPanel(wxWindow *parent) : wxPanel(parent) {
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(sizer);

	/* Top Panel */
	wxPanel *topPanel = new wxPanel(this);
	wxFlexGridSizer *topPanelSizer = new wxFlexGridSizer(2, 2, 0, 5);
	topPanelSizer->AddGrowableCol(1);
	topPanel->SetSizer(topPanelSizer);

	_toolChooserPanel = new DropDownBox((wxWindow *)topPanel, -1, "Choose Tool", kNumCompressionTools, kCompressionToolNames);
	_inputPanel = new IOChooser(topPanel, "File Input", "");
	_compressionTypePanel = new DropDownBox(topPanel, kCompressionChoice, "Choose Compression", kNumCompressionTypes, kCompressionTypeNames);
	_outputPanel = new IOChooser(topPanel, "File Output", "");

	/* Bottom Panel */
	wxPanel *bottomPanel = new wxPanel(this);
	wxBoxSizer *bottomPanelSizer = new wxBoxSizer(wxVERTICAL);
	bottomPanel->SetSizer(bottomPanelSizer);

	_compressionOptionsPanel = new CompressionOptions(bottomPanel);
	_toolOutput = new wxTextCtrl(bottomPanel, -1, "", wxDefaultPosition, wxSize(-1, 300), wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);

	topPanelSizer->Add(_toolChooserPanel, 1, wxEXPAND);
	topPanelSizer->Add(_inputPanel, 4, wxEXPAND);
	topPanelSizer->Add(_compressionTypePanel, 1, wxEXPAND);
	topPanelSizer->Add(_outputPanel, 4, wxEXPAND);
	bottomPanelSizer->Add(_compressionOptionsPanel, 0, wxALIGN_CENTER | wxBOTTOM, 5);
	bottomPanelSizer->Add(_toolOutput, 1, wxEXPAND);

	sizer->Add(topPanel, 0, wxEXPAND);
	sizer->Add(bottomPanel, 1, wxEXPAND);

	/* Select the first tool then simulate selecting MP3 to set up
	 * the compression options
	 */
	_toolChooserPanel->_choice->SetSelection(0);
	_compressionTypePanel->_choice->SetSelection(0);

	wxCommandEvent temp = wxCommandEvent(wxEVT_COMMAND_CHOICE_SELECTED, kCompressionChoice);
	this->OnCompressionChange(temp);
}

void CompressionPanel::OnCompressionChange(wxCommandEvent &event) {
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
