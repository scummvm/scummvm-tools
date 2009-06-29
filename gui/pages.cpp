/* pages.cpp - All the pages in the wizard
 * Copyright (C) 2009 The ScummVM project
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
 * $URL
 * $Id
 *
 */

#include "wx/wxprec.h"

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include "wx/wx.h"
#endif

#include <wx/filepicker.h>
#include <wx/file.h>

#include "main.h"
#include "pages.h"
#include "tools.h"


BEGIN_EVENT_TABLE(WizardPage, wxEvtHandler)
END_EVENT_TABLE()

WizardPage::WizardPage(ScummToolsFrame *frame)
	: _topframe(frame),
		_configuration(frame->_configuration)
{
}

wxWindow *WizardPage::CreatePanel(wxWindow *parent) {
	return new wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Page"));
}

void WizardPage::switchPage(WizardPage *next) {
	_topframe->switchPage(next);
}

void WizardPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	// Do nothing, keep default button state
}

void WizardPage::SetAlignedSizer(wxWindow *panel, wxSizer *sizer) {
	wxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);
	topsizer->AddSpacer(100);
	topsizer->Add(sizer, 0, wxEXPAND);
	panel->SetSizer(topsizer);
}

// Our default handler for next/prev/cancel

void WizardPage::onNext(wxWindow *panel) {
}

void WizardPage::onPrevious(wxWindow *panel) {
	_topframe->switchToPreviousPage();
}

void WizardPage::onCancel(wxWindow *panel) {
	wxMessageDialog dlg(panel, wxT("Are you sure you want to abort the wizard?"), wxT("Abort"), wxYES | wxNO);
	wxWindowID ret = dlg.ShowModal();
	if(ret == wxID_YES) {
		_topframe->Close(true);
	} else {
		// Do nothing
	}
}

// Load/Save settings
void WizardPage::save(wxWindow *panel) {
}

bool WizardPage::onIdle(wxPanel *panel) {
	return false;
}

// Introduction page

IntroPage::IntroPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *IntroPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Welcome to the ScummVM extraction and compression utility.")));
	sizer->Add(new wxStaticText(panel, wxID_ANY,
		wxT("Please select what you want to do, or drop a file or folder on this window for automatic .")));
	
	wxString choices[] = {
		wxT("Extract from game data files"),
		wxT("Compress audio files"),
		wxT("Choose tool to use (advanced)")
	};

	wxRadioBox *options = new wxRadioBox(panel, wxID_ANY, wxT(""), 
		wxDefaultPosition, wxDefaultSize, 3, choices, 1, 
		wxRA_SPECIFY_COLS | wxBORDER_NONE, wxDefaultValidator, wxT("ChooseActivity"));
	sizer->Add(options);
	options->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load options
	Configuration &config = _configuration;
	if(config.advanced)
		options->SetSelection(2);
	else if(config.compressing)
		options->SetSelection(1);
	else
		options->SetSelection(0);

	return panel;
}

void IntroPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	buttons->showPrevious(false);
	buttons->enableNext(true);
}

void IntroPage::save(wxWindow *panel) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("ChooseActivity")))->GetStringSelection().Lower();

	_configuration.advanced    = selected_option.Find(wxT("advanced")) != wxNOT_FOUND;
	_configuration.compressing = selected_option.Find(wxT("extract")) == wxNOT_FOUND;
}

void IntroPage::onNext(wxWindow *panel) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("ChooseActivity")))->GetStringSelection().Lower();
	if(selected_option.Find(wxT("extract")) != wxNOT_FOUND) {
		// extract
		switchPage(new ChooseExtractionPage(_topframe));
	} else if(selected_option.Find(wxT("advanced")) != wxNOT_FOUND) {
		// advanced
		switchPage(new ChooseToolPage(_topframe));
	} else {
		// compress
		switchPage(new ChooseCompressionPage(_topframe));
	}
}

// Page to choose what game files to compress

ChooseCompressionPage::ChooseCompressionPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseCompressionPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Please select for what game/engine you'd like to compress files.")));
	
	wxArrayString choices = g_tools.getGameList(TOOLTYPE_COMPRESSION);

	wxChoice *game = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("GameSelection"));
	sizer->Add(game);
	game->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load already set values
	game->SetStringSelection(_configuration.selectedGame);


	return panel;
}

void ChooseCompressionPage::save(wxWindow *panel) {
	wxString game = static_cast<wxChoice *>(panel->FindWindowByName(wxT("GameSelection")))->GetStringSelection();
	_configuration.selectedGame = game;
	_configuration.selectedTool = g_tools.getByGame(game, TOOLTYPE_COMPRESSION);
}

void ChooseCompressionPage::onNext(wxWindow *panel) {
	switchPage(new ChooseInOutPage(_topframe));
}

// Page to choose what game files to extract, 
// VERY similar to the above, could be made into one class?

ChooseExtractionPage::ChooseExtractionPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseExtractionPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Please select for what game/engine you'd like to extract files from.")));
	
	wxArrayString choices = g_tools.getGameList(TOOLTYPE_EXTRACTION);

	wxChoice *game = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("GameSelection"));
	sizer->Add(game);
	game->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load already set values
	game->SetStringSelection(_configuration.selectedGame);

	return panel;
}

void ChooseExtractionPage::save(wxWindow *panel) {
	wxString game = static_cast<wxChoice *>(panel->FindWindowByName(wxT("GameSelection")))->GetStringSelection();
	_configuration.selectedTool = g_tools.getByGame(game, TOOLTYPE_EXTRACTION);
}

void ChooseExtractionPage::onNext(wxWindow *panel) {
	switchPage(new ChooseInOutPage(_topframe));
}

// Page to choose ANY tool to use

ChooseToolPage::ChooseToolPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseToolPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Select what tool you'd like to use.")));
	wxArrayString choices = g_tools.getToolList(TOOLTYPE_ALL);

	wxChoice *tool = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("ToolSelection"));
	sizer->Add(tool);
	tool->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load configuration
	if(_configuration.selectedTool != NULL)
		tool->SetStringSelection(_configuration.selectedTool->_name);

	return panel;
}

void ChooseToolPage::save(wxWindow *panel) {
	_configuration.selectedTool = 
		g_tools.get(static_cast<wxChoice *>(panel->FindWindowByName(wxT("ToolSelection")))->GetStringSelection());
}

void ChooseToolPage::onNext(wxWindow *panel) {
	switchPage(new ChooseInOutPage(_topframe));
}

// Page to choose input and output directory or file

ChooseInOutPage::ChooseInOutPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseInOutPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);
	
	const Tool &tool = *_configuration.selectedTool;

	// some help perhaps?
	sizer->Add(new wxStaticText(panel, wxID_ANY, tool._inoutHelpText));

	sizer->AddSpacer(10);

	// Create input selection	
	wxStaticBoxSizer *inputbox = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Input files"));

	int i = 1;
	for(ToolInputs::const_iterator iter = tool._inputs.begin(); iter != tool._inputs.end(); ++iter) {
		const ToolInput &input = *iter;

		wxString windowName = wxT("InputPicker");
		windowName << i;

		if(input._file) {
			inputbox->Add(new wxFilePickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a file"), 
				input._extension, 
				wxDefaultPosition, wxDefaultSize, 
				wxFLP_USE_TEXTCTRL | wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, 
				windowName), wxSizerFlags().Expand());

		} else {
			inputbox->Add(new wxDirPickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a folder"), 
				wxDefaultPosition, wxDefaultSize, 
				wxFLP_USE_TEXTCTRL | wxFLP_OPEN, wxDefaultValidator, 
				windowName), wxSizerFlags().Expand());

		}
		++i;
	}
	
	sizer->Add(inputbox, wxSizerFlags().Expand());


	sizer->AddSpacer(10);

	// Create output selection

	if(tool._outputToDirectory) {
		wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, panel, wxT("Destination folder"));

		box->Add(new wxDirPickerCtrl(
			panel, wxID_ANY, _configuration.outputPath, wxT("Select a folder"), 
			wxDefaultPosition, wxDefaultSize, 
			wxFLP_USE_TEXTCTRL | wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, 
			wxT("OutputPicker")), wxSizerFlags(1).Expand());

		sizer->Add(box, wxSizerFlags().Expand());
	} else {
		wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, panel, wxT("Destination file"));

		sizer->Add(new wxFilePickerCtrl(
			panel, wxID_ANY, _configuration.outputPath, wxT("Select a file"), 
			wxT("*.*"),
			wxDefaultPosition, wxDefaultSize, 
			wxFLP_USE_TEXTCTRL | wxFLP_OVERWRITE_PROMPT | wxFLP_SAVE, wxDefaultValidator, 
			wxT("OutputPicker")), wxSizerFlags(1).Expand());
		
		sizer->Add(box, wxSizerFlags().Expand());
	}

	SetAlignedSizer(panel, sizer);

	return panel;
}

void ChooseInOutPage::save(wxWindow *panel) {
	wxWindow *outputWindow = panel->FindWindowByName(wxT("OutputPicker"));
	wxDirPickerCtrl *outDirWindow = dynamic_cast<wxDirPickerCtrl *>(outputWindow);
	wxFilePickerCtrl *inDirWindow = dynamic_cast<wxFilePickerCtrl *>(outputWindow);

	if(outDirWindow)
		_configuration.outputPath = outDirWindow->GetPath();
	if(inDirWindow)
		_configuration.outputPath = inDirWindow->GetPath();

	// TODO: save input, unsure of exact format
}

void ChooseInOutPage::onNext(wxWindow *panel) {
	if(_configuration.compressing)
		switchPage(new ChooseAudioFormatPage(_topframe));
	else
		switchPage(new ProcessPage(_topframe));
}

// Page to choose input and output directory or file

ChooseAudioFormatPage::ChooseAudioFormatPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseAudioFormatPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Please select for what game/engine you'd like to extract files from.")));
	
	wxArrayString choices;

	choices.Add(wxT("Vorbis"));
	choices.Add(wxT("FLAC"));
	choices.Add(wxT("MP3"));

	sizer->AddSpacer(10);

	wxChoice *format = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxSize(80, -1), 
		choices, 0, wxDefaultValidator, wxT("AudioSelection"));
	sizer->Add(format, wxSizerFlags().Expand());
	
	sizer->AddSpacer(10);

	wxCheckBox *advanced = new wxCheckBox(panel, wxID_ANY, wxT("Select advanced audio settings"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("AdvancedAudio"));
	sizer->Add(advanced, wxSizerFlags().Expand());

	SetAlignedSizer(panel, sizer);

	// Load already set values
	if(_configuration.selectedAudioFormat == AUDIO_VORBIS)
		format->SetSelection(0);
	else if(_configuration.selectedAudioFormat == AUDIO_FLAC)
		format->SetSelection(1);
	else if(_configuration.selectedAudioFormat == AUDIO_MP3)
		format->SetSelection(2);

	advanced->SetValue(_configuration.advancedAudioSettings);


	return panel;
}

void ChooseAudioFormatPage::save(wxWindow *panel) {
	wxChoice *format = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AudioSelection")));
	wxCheckBox *advanced = static_cast<wxCheckBox *>(panel->FindWindowByName(wxT("AdvancedAudio")));

	if(format->GetStringSelection() == wxT("Vorbis"))
		_configuration.selectedAudioFormat = AUDIO_VORBIS;
	else if(format->GetStringSelection() == wxT("FLAC"))
		_configuration.selectedAudioFormat = AUDIO_FLAC;
	else if(format->GetStringSelection() == wxT("MP3"))
		_configuration.selectedAudioFormat = AUDIO_MP3;

	_configuration.advancedAudioSettings = advanced->GetValue();
	
}

void ChooseAudioFormatPage::onNext(wxWindow *panel) {
	wxChoice *format = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AudioSelection")));
	wxCheckBox *advanced = static_cast<wxCheckBox *>(panel->FindWindowByName(wxT("AdvancedAudio")));

	if(advanced->GetValue()) {

		if(format->GetStringSelection() == wxT("Vorbis"))
			switchPage(new ChooseAudioOptionsVorbisPage(_topframe));
		else if(format->GetStringSelection() == wxT("FLAC"))
			switchPage(new ChooseAudioOptionsFlacPage(_topframe));
		else if(format->GetStringSelection() == wxT("MP3"))
			switchPage(new ChooseAudioOptionsMp3Page(_topframe));
	} else {
		switchPage(new ProcessPage(_topframe));
	}
}

// Page to choose Mp3 compression options

ChooseAudioOptionsMp3Page::ChooseAudioOptionsMp3Page(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseAudioOptionsMp3Page::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	
	/*
	"\nMP3 mode params:\n"
	" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:" minBitrDef_str "%d)\n"
	" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%" maxBitrDef_str ")\n"
	" --vbr        LAME uses the VBR mode (default)\n"
	" --abr        LAME uses the ABR mode\n" \
	" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:" vbrqualDef_str "%d)\n"
	" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:" algqualDef_str ")\n"
	" --silent     the output of LAME is hidden (default:disabled)\n"
	*/

	wxFlexGridSizer *sizer = new wxFlexGridSizer(6, 2, 10, 25);
	sizer->AddGrowableCol(1);


	// Type of compression
	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Compression Type:")));

	wxRadioButton *abrButton = new wxRadioButton(panel, wxID_ANY, wxT("ABR"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("ABR"));

	wxSizer *radioSizer = new wxBoxSizer(wxHORIZONTAL);
	radioSizer->Add(abrButton);
	
	wxRadioButton *vbrButton = new wxRadioButton(panel, wxID_ANY, wxT("VBR"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("VBR"));
	radioSizer->Add(vbrButton);

	sizer->Add(radioSizer, wxSizerFlags().Expand());

	// Bitrates
	const int possibleBitrateCount = 160 / 8;
	wxString possibleBitrates[possibleBitrateCount + 1];
	for(int i = 0; i <= possibleBitrateCount; ++i) {
		possibleBitrates[i] << i*8;
	}

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Minimum Bitrate:")));

	wxChoice *vbrMinBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("MinimumBitrate"));
	sizer->Add(vbrMinBitrate, wxSizerFlags().Expand());
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Maximum Bitrate:")));
	
	wxChoice *vbrMaxBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("MaximumBitrate"));
	sizer->Add(vbrMaxBitrate, wxSizerFlags().Expand());
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Average Bitrate:")));
	
	wxChoice *abrAvgBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("AverageBitrate"));
	sizer->Add(abrAvgBitrate, wxSizerFlags().Expand());

	abrButton->Connect(wxEVT_COMMAND_RADIOBUTTON_SELECTED, wxCommandEventHandler(ChooseAudioOptionsMp3Page::onChangeCompressionType), NULL, this);
	vbrButton->Connect(wxEVT_COMMAND_RADIOBUTTON_SELECTED, wxCommandEventHandler(ChooseAudioOptionsMp3Page::onChangeCompressionType), NULL, this);

	// Quality
	const int possibleQualityCount = 9;
	wxString possibleQualities[possibleQualityCount + 1];
	for(int i = 0; i <= possibleQualityCount; ++i) {
		possibleQualities[i] << i;
	}

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("VBR Quality:")));
	
	wxChoice *vbrQuality = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleQualityCount, possibleQualities, 0, wxDefaultValidator, wxT("VBRQuality"));
	sizer->Add(vbrQuality, wxSizerFlags().Expand());
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("MPEG Quality:")));
	
	wxChoice *mpegQuality = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleQualityCount, possibleQualities, 0, wxDefaultValidator, wxT("MpegQuality"));
	sizer->Add(mpegQuality, wxSizerFlags().Expand());

	// Finish the window
	SetAlignedSizer(panel, sizer);


	// Load settings
	if(_topframe->_configuration.mp3CompressionType == wxT("ABR"))
		abrButton->SetValue(true);
	else
		vbrButton->SetValue(true);
	vbrMinBitrate->SetStringSelection(_topframe->_configuration.mp3VBRMinBitrate);
	vbrMaxBitrate->SetStringSelection(_topframe->_configuration.mp3VBRMaxBitrate);
	abrAvgBitrate->SetStringSelection(_topframe->_configuration.mp3ABRBitrate);
	vbrQuality   ->SetStringSelection(_topframe->_configuration.mp3VBRQuality);
	mpegQuality  ->SetStringSelection(_topframe->_configuration.mp3MpegQuality);

	updateFields(panel);

	return panel;
}

void ChooseAudioOptionsMp3Page::save(wxWindow *panel) {
	wxRadioButton *abr = static_cast<wxRadioButton *>(panel->FindWindowByName(wxT("ABR")));
	wxRadioButton *vbr = static_cast<wxRadioButton *>(panel->FindWindowByName(wxT("VBR")));

	wxChoice *vbrMinBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MinimumBitrate")));
	wxChoice *vbrMaxBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MaximumBitrate")));
	wxChoice *abrAvgBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AverageBitrate")));
	wxChoice *vbrQuality = static_cast<wxChoice *>(panel->FindWindowByName(wxT("VBRQuality")));
	wxChoice *mpegQuality = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MpegQuality")));

	_topframe->_configuration.mp3VBRMinBitrate = vbrMinBitrate->GetStringSelection();
	_topframe->_configuration.mp3VBRMaxBitrate = vbrMaxBitrate->GetStringSelection();
	_topframe->_configuration.mp3ABRBitrate    = abrAvgBitrate->GetStringSelection();
	_topframe->_configuration.mp3VBRQuality    = vbrQuality   ->GetStringSelection();
	_topframe->_configuration.mp3MpegQuality   = mpegQuality  ->GetStringSelection();
	if(abr->GetValue())
		_topframe->_configuration.mp3CompressionType = wxT("ABR");
	else
		_topframe->_configuration.mp3CompressionType = wxT("VBR");
}

void ChooseAudioOptionsMp3Page::updateFields(wxWindow *panel) {
	wxRadioButton *abr = static_cast<wxRadioButton *>(panel->FindWindowByName(wxT("ABR")));
	//wxRadioButton *vbr = static_cast<wxRadioButton *>(panel->FindWindowByName(wxT("VBR")));
	wxChoice *vbrMinBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MinimumBitrate")));
	wxChoice *vbrMaxBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MaximumBitrate")));
	wxChoice *abrAvgBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AverageBitrate")));
	wxChoice *vbrQuality = static_cast<wxChoice *>(panel->FindWindowByName(wxT("VBRQuality")));
	//wxChoice *mpegQuality =  static_cast<wxChoice *>(panel->FindWindowByName(wxT("MpegQuality")));

	vbrMinBitrate->Enable(!abr->GetValue());
	vbrMaxBitrate->Enable(!abr->GetValue());
	vbrQuality->Enable(!abr->GetValue());
	
	abrAvgBitrate->Enable(abr->GetValue());
}

void ChooseAudioOptionsMp3Page::onChangeCompressionType(wxCommandEvent &evt) {
	wxRadioButton *btn = static_cast<wxRadioButton *>(evt.GetEventObject());
	wxWindow *parent = btn->GetParent();
	updateFields(parent);
}

void ChooseAudioOptionsMp3Page::onNext(wxWindow *panel) {
	switchPage(new ProcessPage(_topframe));
}

// Page to choose Flac compression options

ChooseAudioOptionsFlacPage::ChooseAudioOptionsFlacPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseAudioOptionsFlacPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);
	
	wxSizer *topsizer = new wxBoxSizer(wxVERTICAL);

	topsizer->AddSpacer(15);
	topsizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Choose advanced audio options, high compression levels means better quality,\nlower faster runtime.")));
	topsizer->AddSpacer(10);
	
	/*
	"\nFlac mode params:\n" \
	" --fast       FLAC uses compression level 0\n" \
	" --best       FLAC uses compression level 8\n" \
	" -<value>     specifies the value (0 - 8) of compression (8=best)(default:" flacCompressDef_str ")\n" \
	" -b <value>   specifies a blocksize of <value> samples (default:" flacBlocksizeDef_str ")\n" \
	" --verify     files are encoded and then decoded to check accuracy\n" \
	" --silent     the output of FLAC is hidden (default:disabled)\n" \
	*/

	wxFlexGridSizer *sizer = new wxFlexGridSizer(2, 2, 10, 25);
	sizer->AddGrowableCol(1);
	

	// 

	// Compression Level
	const int possibleLevelCount = 8;
	wxString possibleLevels[possibleLevelCount + 1];
	for(int i = 0; i <= possibleLevelCount; ++i) {
		possibleLevels[i] << i;
	}

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Compression Level:")));
	
	wxChoice *compressionLevel = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleLevelCount, possibleLevels, 0, wxDefaultValidator, wxT("CompressionLevel"));
	sizer->Add(compressionLevel, wxSizerFlags().Expand());


	// Block Size

	wxString blockSizes[] = {
		wxT("576"),
		wxT("1152"),
		wxT("2304"),
		wxT("4608")
	};

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Block Size:")));

	wxChoice *blockSize = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		sizeof blockSizes / sizeof *blockSizes, blockSizes, 0, wxDefaultValidator, wxT("BlockSize"));
	sizer->Add(blockSize, wxSizerFlags().Expand());
	
	// Finish the window
	topsizer->Add(sizer);
	SetAlignedSizer(panel, topsizer);


	// Load settings
	compressionLevel->SetStringSelection(_topframe->_configuration.flacCompressionLevel);
	blockSize->SetStringSelection(_topframe->_configuration.flacBlockSize);

	return panel;
}

void ChooseAudioOptionsFlacPage::save(wxWindow *panel) {
	wxChoice *compressionLevel = static_cast<wxChoice *>(panel->FindWindowByName(wxT("CompressionLevel")));
	wxChoice *blockSize = static_cast<wxChoice *>(panel->FindWindowByName(wxT("BlockSize")));

	_topframe->_configuration.flacCompressionLevel = compressionLevel->GetStringSelection();
	_topframe->_configuration.flacBlockSize = blockSize->GetStringSelection();
}

void ChooseAudioOptionsFlacPage::onNext(wxWindow *panel) {
	switchPage(new ProcessPage(_topframe));
}

// Page to choose Vorbis compression options

ChooseAudioOptionsVorbisPage::ChooseAudioOptionsVorbisPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseAudioOptionsVorbisPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	
	/* Vorbis mode params
	" -b <rate>    <rate> is the nominal bitrate (default:unset)\n" \
	" -m <rate>    <rate> is the minimum bitrate (default:unset)\n" \
	" -M <rate>    <rate> is the maximum bitrate (default:unset)\n" \
	" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:" oggqualDef_str ")\n" \
	" --silent     the output of oggenc is hidden (default:disabled)\n" \
	*/

	wxFlexGridSizer *sizer = new wxFlexGridSizer(4, 2, 10, 25);
	sizer->AddGrowableCol(1);

	// Bitrates
	const int possibleBitrateCount = 160 / 8;
	wxString possibleBitrates[possibleBitrateCount + 1];
	for(int i = 0; i <= possibleBitrateCount; ++i) {
		possibleBitrates[i] << i*8;
	}

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Minimum Bitrate:")));

	wxChoice *MinBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("MinimumBitrate"));
	sizer->Add(MinBitrate, wxSizerFlags().Expand());
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Nominal Bitrate:")));
	
	wxChoice *AvgBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("NominalBitrate"));
	sizer->Add(AvgBitrate, wxSizerFlags().Expand());
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Maximum Bitrate:")));
	
	wxChoice *MaxBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("MaximumBitrate"));
	sizer->Add(MaxBitrate, wxSizerFlags().Expand());

	// Quality
	const int possibleQualityCount = 10;
	wxString possibleQualities[possibleQualityCount + 1];
	for(int i = 0; i <= possibleQualityCount; ++i) {
		possibleQualities[i] << i;
	}

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Quality:")));
	
	wxChoice *quality = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleQualityCount, possibleQualities, 0, wxDefaultValidator, wxT("Quality"));
	sizer->Add(quality, wxSizerFlags().Expand());

	// Finish the window
	SetAlignedSizer(panel, sizer);


	// Load settings
	MinBitrate->SetStringSelection(_topframe->_configuration.oggMinBitrate);
	AvgBitrate->SetStringSelection(_topframe->_configuration.oggAvgBitrate);
	MaxBitrate->SetStringSelection(_topframe->_configuration.oggMaxBitrate);
	quality   ->SetStringSelection(_topframe->_configuration.oggQuality);

	return panel;
}

void ChooseAudioOptionsVorbisPage::save(wxWindow *panel) {
	wxChoice *minBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MinimumBitrate")));
	wxChoice *avgBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("NominalBitrate")));
	wxChoice *maxBitrate = static_cast<wxChoice *>(panel->FindWindowByName(wxT("MaximumBitrate")));
	wxChoice *quality    = static_cast<wxChoice *>(panel->FindWindowByName(wxT("Quality")));

	_topframe->_configuration.oggMinBitrate = minBitrate->GetStringSelection();
	_topframe->_configuration.oggAvgBitrate = avgBitrate->GetStringSelection();
	_topframe->_configuration.oggMaxBitrate = maxBitrate->GetStringSelection();
	_topframe->_configuration.oggQuality    = quality   ->GetStringSelection();
}

void ChooseAudioOptionsVorbisPage::onNext(wxWindow *panel) {
	switchPage(new ProcessPage(_topframe));
}


// Page to choose ANY tool to use

BEGIN_EVENT_TABLE(ProcessPage, WizardPage)
	EVT_END_PROCESS(wxID_ANY, ProcessPage::onTerminate)
END_EVENT_TABLE()

ProcessPage::ProcessPage(ScummToolsFrame* frame)
	: WizardPage(frame),
	  _finished(false),
	  _success(false),
	  _process(NULL)
{
}

wxWindow *ProcessPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Processing data...")), wxSizerFlags().Expand().Border(wxLEFT, 20));
	
	wxTextCtrl *outwin = new wxTextCtrl(panel, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, 
		wxTE_MULTILINE | wxTE_READONLY, wxDefaultValidator, wxT("OutputWindow"));
	outwin->Enable(false);
	sizer->Add(outwin, wxSizerFlags(1).Expand().Border(wxALL, 10));

	panel->SetSizer(sizer);

	// This should NOT be called from the 'constructor' (once it runs the subprocess)
	runProcess(outwin);

	return panel;
}

void ProcessPage::save(wxWindow *panel) {
	//_configuration.selectedTool = 
	//	g_tools.get(static_cast<wxChoice *>(panel->FindWindowByName(wxT("ToolSelection")))->GetStringSelection());
}

wxString ProcessPage::createCommandLine() {
	wxString cli;
	Configuration &conf = _topframe->_configuration;
	
	// Executable
	cli << conf.selectedTool->getExecutable();
	
	// Audio format args
	if(conf.compressing) {
		cli << wxT(" --mp3");
		if(conf.selectedAudioFormat == AUDIO_VORBIS) {
			cli << wxT(" --vorbis");
			cli << wxT(" -b ") << conf.oggAvgBitrate;
			cli << wxT(" -m ") << conf.oggMinBitrate;
			cli << wxT(" -M ") << conf.oggMaxBitrate;
			cli << wxT(" -q ") << conf.oggQuality;
		} else if(conf.selectedAudioFormat == AUDIO_FLAC) {
			cli << wxT(" --flac");
			cli << wxT(" -") << conf.flacCompressionLevel;
			cli << wxT(" -b ")<< conf.flacBlockSize;
		} else if(conf.selectedAudioFormat == AUDIO_MP3) {
			if(conf.mp3CompressionType == wxT("ABR")) {
				cli << wxT(" --abr");
				cli << wxT(" -b ") << conf.mp3ABRBitrate;
				cli << wxT(" -b ") << conf.mp3VBRMinBitrate;
			} else {
				cli << wxT(" --vbr");
				cli << wxT(" -b ") << conf.mp3VBRMinBitrate;
				cli << wxT(" -B ") << conf.mp3VBRMaxBitrate;
			}
					
			cli << wxT(" -q ") << conf.mp3MpegQuality;
		}
	}

	cli << wxT(" -o ") << conf.outputPath;
	for(wxArrayString::const_iterator i = conf.inputFilePaths.begin(); i != conf.inputFilePaths.end(); ++i)
		cli << *i << wxT(" ");

	cli << wxT("\n");
	return cli;
}

void ProcessPage::runProcess(wxTextCtrl *outwin) {
	wxString cli = createCommandLine();
	outwin->WriteText(cli + wxT("\n"));

	if(wxFile::Exists(_topframe->_configuration.selectedTool->getExecutable())) {
		_process = new wxProcess(this);
		bool success = wxExecute(cli, wxEXEC_ASYNC, _process) != 0;

		if(!success) {
			outwin->WriteText(wxT("Could not run process."));
			return;
		}

		_process->Redirect();
	} else {
		_finished = true;
		outwin->WriteText(wxT("Could not find executable file on the current path. Check that the executable for the selected tool is available and place in the correct directory."));
	}

}

bool ProcessPage::onIdle(wxPanel *panel) {
	if(_process) {
		wxInputStream *stream = _process->GetInputStream();
		wxTextCtrl *outwin = static_cast<wxTextCtrl *>(panel->FindWindowByName(wxT("OutputWindow")));

		wxASSERT_MSG(stream, wxT("Could not bind input stream!"));

		while(stream->CanRead()) {
			wxFileOffset off = stream->GetLength();
			if(off = wxInvalidOffset) {
				return false;
			}
			char *buf = new char[(size_t)off];
			stream->Read(buf, (size_t)off);
			outwin->WriteText(wxString(buf, wxConvUTF8, (size_t)off));
			delete[] buf;
		}

		return true;
	}
	return false;
}

void ProcessPage::onTerminate(wxProcessEvent &evt) {
	// Ugly hack, should not find the panel this way...
	wxWindow *panel = _topframe->FindWindowByName(wxT("WizardPage"));
	wxTextCtrl *outwin = static_cast<wxTextCtrl *>(panel->FindWindowByName(wxT("OutputWindow")));

	_success = evt.GetExitCode() == 0;
	if(_success) {
		outwin->WriteText(wxT("Subprocess exited sucessfully!"));
	} else {
		outwin->WriteText(wxT("Subprocess exited sucessfully!"));
	}
	_finished = true;

	updateButtons(panel, static_cast<WizardButtons *>(_topframe->FindWindowByName(wxT("WizardButtonPanel"))));
}

/*
void ProcessPage::onNext(wxWindow *panel) {
	switchPage(new ChooseInOutPage(_topframe));
}*/

void ProcessPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	if(_success) {
		buttons->enablePrevious(false);
		buttons->enableNext(true);
	} else if(_finished) {
		buttons->enablePrevious(true);
		buttons->enableNext(true);
	} else {
		buttons->enablePrevious(false);
		buttons->enableNext(false);
	}
}



	