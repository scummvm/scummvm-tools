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

#include "main.h"
#include "pages.h"
#include "tools.h"

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
	_configuration.selectedTool = g_tools.getByGame(game);
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
	wxString game =
		static_cast<wxChoice *>(panel->FindWindowByName(wxT("GameSelection")))->GetStringSelection();
	_configuration.selectedTool = g_tools.getByGame(game);
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
		; // Go to confirm page, nothing more to query
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

	choices.Add(wxT("Ogg"));
	choices.Add(wxT("FLAC"));
	choices.Add(wxT("MP3"));

	sizer->AddSpacer(10);

	wxChoice *format = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxSize(80, -1), 
		choices, 0, wxDefaultValidator, wxT("AudioSelection"));
	sizer->Add(format);
	
	sizer->AddSpacer(10);

	wxCheckBox *advanced = new wxCheckBox(panel, wxID_ANY, wxT("Select advanced audio settings"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("AdvancedAudio"));
	sizer->Add(advanced);

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

	if(format->GetStringSelection() == wxT("Ogg"))
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

		if(format->GetStringSelection() == wxT("Ogg"))
			; // TODO 
		else if(format->GetStringSelection() == wxT("FLAC"))
			; // TODO 
		else if(format->GetStringSelection() == wxT("MP3"))
			switchPage(new ChooseAudioOptionsMp3Page(_topframe));
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

	wxSizer *radioSizer = new wxBoxSizer(wxHORIZONTAL);
	radioSizer->Add(
		new wxRadioButton(panel, wxID_ANY, wxT("ABR"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("ABR")));
	radioSizer->Add(
		new wxRadioButton(panel, wxID_ANY, wxT("VBR"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("VBR")));

	sizer->Add(radioSizer);

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
	sizer->Add(vbrMinBitrate);
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Maximum Bitrate:")));
	
	wxChoice *vbrMaxBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("MaximumBitrate"));
	sizer->Add(vbrMaxBitrate);
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Average Bitrate:")));
	
	wxChoice *abrAvgBitrate = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleBitrateCount, possibleBitrates, 0, wxDefaultValidator, wxT("AverageBitrate"));
	sizer->Add(abrAvgBitrate);

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
	sizer->Add(vbrQuality);
	

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Average Bitrate:")));
	
	wxChoice *mpegQuality = new wxChoice(
		panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		possibleQualityCount, possibleQualities, 0, wxDefaultValidator, wxT("MpegQuality"));
	sizer->Add(mpegQuality);

	// Finish the window
	SetAlignedSizer(panel, sizer);


	// Load settings
	// TODO: Fields should be enabled / disabled depending on settings
	vbrMinBitrate->SetStringSelection(_topframe->_configuration.mp3VBRMinBitrate);
	vbrMaxBitrate->SetStringSelection(_topframe->_configuration.mp3VBRMaxBitrate);
	abrAvgBitrate->SetStringSelection(_topframe->_configuration.mp3ABRBitrate);
	vbrQuality   ->SetStringSelection(_topframe->_configuration.mp3VBRQuality);
	mpegQuality  ->SetStringSelection(_topframe->_configuration.mp3MpegQuality);

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
}


