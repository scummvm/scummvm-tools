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
#include <wx/process.h>

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
	topsizer->Add(sizer, 1, wxEXPAND);
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
	if (ret == wxID_YES) {
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
	if (config.advanced)
		options->SetSelection(2);
	else if (config.compressing)
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
	if (selected_option.Find(wxT("advanced")) != wxNOT_FOUND) {
		// advanced
		switchPage(new ChooseToolPage(_topframe));
	} else {
		// extract / compress
		switchPage(new ChooseInPage(_topframe));
	}
}

// Page to choose the tool to use

ChooseToolPage::ChooseToolPage(ScummToolsFrame* frame, const wxArrayString &options)
	: WizardPage(frame),
	  _options(options)
{
}

wxWindow *ChooseToolPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	wxArrayString choices;

	if (!_options.empty()) {
		sizer->Add(new wxStaticText(panel, wxID_ANY, 
			wxT("There are multiple possible tools for this input, please select the correct one.")));
		choices = _options;
	} else {
		sizer->Add(new wxStaticText(panel, wxID_ANY, 
			wxT("Select what tool you'd like to use.")));
		choices = g_tools.getToolList(TOOLTYPE_ALL);
	}
	
	sizer->AddSpacer(20);

	wxChoice *tool = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("ToolSelection"));
	sizer->Add(tool);
	tool->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load configuration
	if (_configuration.selectedTool != NULL)
		tool->SetStringSelection(_configuration.selectedTool->_name);

	return panel;
}

void ChooseToolPage::save(wxWindow *panel) {
	_configuration.selectedTool = 
		g_tools.get(static_cast<wxChoice *>(panel->FindWindowByName(wxT("ToolSelection")))->GetStringSelection());
}

void ChooseToolPage::onNext(wxWindow *panel) {
	if (_configuration.advanced)
		switchPage(new ChooseInPage(_topframe));
	else
		// TODO: Display extra input page
		switchPage(new ChooseOutPage(_topframe));
}

// Page to choose input directory or file

ChooseInPage::ChooseInPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseInPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);
	
	// some help perhaps?
	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Select an input file, if you have two input files (CD1 and CD2), ")
		wxT("you will be queried for the other file later.")
		wxT("You can also drag & drop a file on this window.")
		),
		wxSizerFlags(1).Expand());

	sizer->AddSpacer(10);


	//if (input._file) {
		sizer->Add(new wxFilePickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a file"), 
				wxT("*.*"), 
				wxDefaultPosition, wxSize(300, -1),
				wxFLP_USE_TEXTCTRL | wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, 
				wxT("InputPicker")));

	sizer->AddSpacer(30);
	/* 
	// TODO: There is no way to select directory input, yet
	} else {
		inputbox->Add(new wxDirPickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a folder"), 
				wxDefaultPosition, wxDefaultSize, 
				wxFLP_USE_TEXTCTRL | wxFLP_OPEN, wxDefaultValidator, 
				wxT("InputPicker")),
			wxSizerFlags().Expand());
	
	}
	*/
	
	SetAlignedSizer(panel, sizer);
	
	return panel;
}

void ChooseInPage::save(wxWindow *panel) {
	_configuration.inputFilePaths.clear();

	wxDirPickerCtrl *inDirWindow = dynamic_cast<wxDirPickerCtrl *>(panel->FindWindowByName(wxT("InputPicker")));
	wxFilePickerCtrl *inFileWindow = dynamic_cast<wxFilePickerCtrl *>(panel->FindWindowByName(wxT("InputPicker")));

	if (inDirWindow)
		_configuration.inputFilePaths.Add(inDirWindow ->GetPath());
	if (inFileWindow)
		_configuration.inputFilePaths.Add(inFileWindow->GetPath());
}

void ChooseInPage::onNext(wxWindow *panel) {

	wxDirPickerCtrl *inDirWindow = dynamic_cast<wxDirPickerCtrl *>(panel->FindWindowByName(wxT("InputPicker")));
	wxFilePickerCtrl *inFileWindow = dynamic_cast<wxFilePickerCtrl *>(panel->FindWindowByName(wxT("InputPicker")));

	Filename filename;

	if (inDirWindow)
		filename = (const char *)inDirWindow ->GetPath().mb_str();
	if (inFileWindow)
		filename = (const char *)inFileWindow ->GetPath().mb_str();

	if (_configuration.advanced) {
		if (_configuration.selectedTool->getInputList().size() > 1)
			switchPage(new ChooseExtraInPage(_topframe));
		else
			switchPage(new ChooseOutPage(_topframe));
	} else {
		wxArrayString ls = g_tools.getToolList(filename,
			_configuration.compressing? TOOLTYPE_COMPRESSION : TOOLTYPE_EXTRACTION);
		// TODO: If only one input, skip this page and go right to ExtraInput
		switchPage(new ChooseToolPage(_topframe, ls));
	}
}

// Page to choose input and output directory or file

ChooseExtraInPage::ChooseExtraInPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseExtraInPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);
	
	const ToolGUI &tool = *_configuration.selectedTool;

	// some help perhaps?
	sizer->Add(new wxStaticText(panel, wxID_ANY, tool._inHelpText));

	sizer->AddSpacer(10);

	// Create input selection	
	wxStaticBoxSizer *inputbox = new wxStaticBoxSizer(wxVERTICAL, panel, wxT("Input files"));

	int i = 1;
	ToolInputs &inputs = tool.getInputList();
	wxASSERT_MSG(inputs.size() > 1, wxT("Extra input page should not display with only one input"));

	for (ToolInputs::const_iterator iter = inputs.begin() + 1; iter != inputs.end(); ++iter) {
		const ToolInput &input = *iter;

		wxString windowName = wxT("InputPicker");
		windowName << i;

		if (input.file) {
			inputbox->Add(new wxFilePickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a file"), 
				wxString(input.format.c_str(), wxConvUTF8), 
				wxDefaultPosition, wxDefaultSize, 
				wxFLP_USE_TEXTCTRL | wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, 
				windowName));

		} else {
			inputbox->Add(new wxDirPickerCtrl(
				panel, wxID_ANY, wxEmptyString, wxT("Select a folder"), 
				wxDefaultPosition, wxDefaultSize, 
				wxFLP_USE_TEXTCTRL | wxFLP_OPEN, wxDefaultValidator, 
				windowName));

		}
		++i;
	}
	
	sizer->Add(inputbox, wxSizerFlags().Expand());

	SetAlignedSizer(panel, sizer);

	return panel;
}

void ChooseExtraInPage::save(wxWindow *panel) {
	wxWindow *outputWindow = panel->FindWindowByName(wxT("OutputPicker"));
	wxDirPickerCtrl *outDirWindow = dynamic_cast<wxDirPickerCtrl *>(outputWindow);
	wxFilePickerCtrl *outFileWindow = dynamic_cast<wxFilePickerCtrl *>(outputWindow);

	if (outDirWindow)
		_configuration.outputPath = outDirWindow->GetPath();
	if (outFileWindow)
		_configuration.outputPath = outFileWindow->GetPath();

	const ToolGUI &tool = *_configuration.selectedTool;

	// Remove all additional inputs
	wxArrayString filelist = _configuration.inputFilePaths;
	if (filelist.size() > 1)
		filelist.erase(filelist.begin() + 1, filelist.end());

	int i = 1;
	for (ToolInputs::const_iterator iter = tool.getInputList().begin(); iter != tool.getInputList().end(); ++iter) {
		wxString windowName = wxT("InputPicker");
		windowName << i;

		wxDirPickerCtrl *inDirWindow = dynamic_cast<wxDirPickerCtrl *>(panel->FindWindowByName(windowName));
		wxFilePickerCtrl *inFileWindow = dynamic_cast<wxFilePickerCtrl *>(panel->FindWindowByName(windowName));

		if (inDirWindow)
			_configuration.inputFilePaths.Add(inDirWindow ->GetPath());
		if (inFileWindow)
			_configuration.inputFilePaths.Add(inFileWindow->GetPath());

		++i;
	}
}

void ChooseExtraInPage::onNext(wxWindow *panel) {
	switchPage(new ChooseOutPage(_topframe));
}

// Page to choose input and output directory or file

ChooseOutPage::ChooseOutPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseOutPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);
	
	const ToolGUI &tool = *_configuration.selectedTool;

	// some help perhaps?
	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Select an output directory.\n\n")
		wxT("Note: Some tools display file picker here, this should perhaps be changed to always ")
		wxT("be directory output, since often don't want to name the output file.)")
		),
		wxSizerFlags(1).Expand());

	// Create input selection	

	sizer->AddSpacer(10);

	// Create output selection

	if (tool.outputToDirectory()) {
		wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, panel, wxT("Destination folder"));

		box->Add(new wxDirPickerCtrl(
			panel, wxID_ANY, _configuration.outputPath, wxT("Select a folder"), 
			wxDefaultPosition, wxSize(300, -1),
			wxFLP_USE_TEXTCTRL | wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, 
			wxT("OutputPicker")));

		sizer->Add(box);
	} else {
		wxStaticBoxSizer *box = new wxStaticBoxSizer(wxHORIZONTAL, panel, wxT("Destination file"));

		box->Add(new wxFilePickerCtrl(
			panel, wxID_ANY, _configuration.outputPath, wxT("Select a file"), 
			wxT("*.*"),
			wxDefaultPosition, wxSize(300, -1),
			wxFLP_USE_TEXTCTRL | wxFLP_OVERWRITE_PROMPT | wxFLP_SAVE, wxDefaultValidator, 
			wxT("OutputPicker")));
		
		sizer->Add(box);
	}

	SetAlignedSizer(panel, sizer);

	return panel;
}

void ChooseOutPage::save(wxWindow *panel) {
	wxWindow *outputWindow = panel->FindWindowByName(wxT("OutputPicker"));
	wxDirPickerCtrl *outDirWindow = dynamic_cast<wxDirPickerCtrl *>(outputWindow);
	wxFilePickerCtrl *outFileWindow = dynamic_cast<wxFilePickerCtrl *>(outputWindow);

	if (outDirWindow)
		_configuration.outputPath = outDirWindow->GetPath();
	if (outFileWindow)
		_configuration.outputPath = outFileWindow->GetPath();
}

void ChooseOutPage::onNext(wxWindow *panel) {
	if (_configuration.selectedTool->_type == TOOLTYPE_COMPRESSION)
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
		wxT("Select audio format you want to compress to.")));

	sizer->AddSpacer(20);
	
	wxArrayString choices;

	choices.Add(wxT("Vorbis"));
	choices.Add(wxT("FLAC"));
	choices.Add(wxT("MP3"));

	wxChoice *format = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxSize(80, -1), 
		choices, 0, wxDefaultValidator, wxT("AudioSelection"));
	sizer->Add(format);
	
	sizer->AddSpacer(10);

	wxCheckBox *advanced = new wxCheckBox(panel, wxID_ANY, wxT("Select advanced audio settings"), 
		wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxT("AdvancedAudio"));
	sizer->Add(advanced);

	SetAlignedSizer(panel, sizer);

	// Load already set values
	if (_configuration.selectedAudioFormat == AUDIO_VORBIS)
		format->SetSelection(0);
	else if (_configuration.selectedAudioFormat == AUDIO_FLAC)
		format->SetSelection(1);
	else if (_configuration.selectedAudioFormat == AUDIO_MP3)
		format->SetSelection(2);

	advanced->SetValue(_configuration.advancedAudioSettings);


	return panel;
}

void ChooseAudioFormatPage::save(wxWindow *panel) {
	wxChoice *format = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AudioSelection")));
	wxCheckBox *advanced = static_cast<wxCheckBox *>(panel->FindWindowByName(wxT("AdvancedAudio")));

	if (format->GetStringSelection() == wxT("Vorbis"))
		_configuration.selectedAudioFormat = AUDIO_VORBIS;
	else if (format->GetStringSelection() == wxT("FLAC"))
		_configuration.selectedAudioFormat = AUDIO_FLAC;
	else if (format->GetStringSelection() == wxT("MP3"))
		_configuration.selectedAudioFormat = AUDIO_MP3;

	_configuration.advancedAudioSettings = advanced->GetValue();
	
}

void ChooseAudioFormatPage::onNext(wxWindow *panel) {
	wxChoice *format = static_cast<wxChoice *>(panel->FindWindowByName(wxT("AudioSelection")));
	wxCheckBox *advanced = static_cast<wxCheckBox *>(panel->FindWindowByName(wxT("AdvancedAudio")));

	if (advanced->GetValue()) {

		if (format->GetStringSelection() == wxT("Vorbis"))
			switchPage(new ChooseAudioOptionsVorbisPage(_topframe));
		else if (format->GetStringSelection() == wxT("FLAC"))
			switchPage(new ChooseAudioOptionsFlacPage(_topframe));
		else if (format->GetStringSelection() == wxT("MP3"))
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
	for (int i = 0; i <= possibleBitrateCount; ++i) {
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
	for (int i = 0; i <= possibleQualityCount; ++i) {
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
	if (_topframe->_configuration.mp3CompressionType == wxT("ABR"))
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
//	wxRadioButton *vbr = static_cast<wxRadioButton *>(panel->FindWindowByName(wxT("VBR")));

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
	if (abr->GetValue())
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
	for (int i = 0; i <= possibleLevelCount; ++i) {
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
	for (int i = 0; i <= possibleBitrateCount; ++i) {
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
	for (int i = 0; i <= possibleQualityCount; ++i) {
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

ProcessPage::ProcessPage(ScummToolsFrame* frame)
	: WizardPage(frame),
	  _finished(false),
	  _success(false)
{
	_gauge = NULL;
	_outwin = NULL;

	_output.done = 0;
	_output.total = 100;

	_output.cmd = NULL;
	_output.retval = 0;
	_output.subprocessFinished = NULL;
}

wxWindow *ProcessPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, wxT("Processing data...")), wxSizerFlags().Expand().Border(wxLEFT, 20));
	
	_outwin = new wxTextCtrl(panel, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, 
		wxTE_MULTILINE | wxTE_READONLY, wxDefaultValidator, wxT("OutputWindow"));
	sizer->Add(_outwin, wxSizerFlags(1).Expand().Border(wxTOP | wxLEFT | wxRIGHT, 10));

	_gauge = new wxGauge(panel, wxID_ANY, _output.total, wxDefaultPosition, wxDefaultSize, 
		wxGA_HORIZONTAL, wxDefaultValidator, wxT("ProgressBar"));
	sizer->Add(_gauge, wxSizerFlags(0).Expand().Border(wxBOTTOM | wxLEFT | wxRIGHT, 10));

	panel->SetSizer(sizer);

	// Run the tool
	runTool();

	return panel;
}

void ProcessPage::runTool() {
	const ToolGUI *tool = _topframe->_configuration.selectedTool;

	// Write some text that we've started...
	_outwin->WriteText(wxT("Running ") + tool->_name + wxT("\n\n"));

	// Child thread to run the tool
	_thread = new ProcessToolThread(tool, _topframe->_configuration, _output);

	// We should check return value of this
	_thread->Create();

	_thread->Run();
}

bool ProcessPage::onIdle(wxPanel *panel) {
	const ToolGUI *tool = _topframe->_configuration.selectedTool;
	
	if (!_thread)
		return false;
	
	// This function can be called recursively, by checking if lock is available, we avoid it
	if(_output.mutex.TryLock() == wxMUTEX_BUSY)
		return false;
	else
		// Immedietly unlock
		_output.mutex.Unlock();

	// Check if our subthread has something for us to do
	{
		wxMutexLocker lock(_output.mutex);

		// Write text
		_outwin->WriteText(wxString(_output.buffer.c_str(), wxConvUTF8));
		_output.buffer = "";

		if (tool->supportsProgressBar()) {
			// Update gauge
			_gauge->SetRange(_output.total);
			_gauge->SetValue(_output.done);
		}

		if (_output.cmd) {
			// We have a waiting subprocess to run, the other thread is sleeping since we could lock the mutex
			
#ifdef __WINDOWS__
			// Only windows needs this
			// It hides the process window, on unix it doesn't appear to work very well
			wxProcess proc(wxPROCESS_REDIRECT);
			_output.retval = wxExecute(wxString(_output.cmd, ), wxEXEC_SYNC, &proc);
#else
			// Process windows are hidden by default under other OSes, so we don't need any special code
			_output.retval = system(_output.cmd);
#endif
			_output.cmd = NULL;

			// Signal the other thread that we have run the subprocess
			_output.subprocessFinished->Signal();
		}
	}

	// Check if thread finished
	if (_thread && _thread->_finished) {
		// It's done, Wait deallocates resources
		_thread->Wait();
		delete _thread;
		_thread = NULL;
		_finished = true;

		// Update UI
		updateButtons(panel, _topframe->_buttons);
		return false;
	}

	if (!tool->supportsProgressBar()) {
		// Just move the bar back & forth
		_gauge->Pulse();
	}

	return true;
}

void ProcessPage::onNext(wxWindow *panel) {
	if (_success)
		switchPage(new FinishPage(_topframe));
	else
		switchPage(new FailurePage(_topframe));
}

void ProcessPage::onCancel(wxWindow *panel) {
	if (_finished)
		WizardPage::onCancel(panel);
	else
		_thread->abort();
}

void ProcessPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	if (_success) {
		buttons->enablePrevious(false);
		buttons->enableNext(true);
		buttons->showAbort(false);

		_gauge->SetRange(100);
		_gauge->SetValue(100);
	} else if (_finished) {
		buttons->enablePrevious(true);
		buttons->enableNext(true);
		buttons->showAbort(false);

		// It's not possible to disable them, leave it empty instead
		_gauge->SetRange(0);
		_gauge->SetValue(100);
	} else {
		buttons->enablePrevious(false);
		buttons->enableNext(false);
		buttons->showAbort(true);
	}
}

// The thread a tool is run in

ProcessToolThread::ProcessToolThread(const ToolGUI *tool, Configuration &configuration, ThreadCommunicationBuffer &output) : 
	wxThread(wxTHREAD_JOINABLE), 
	_configuration(configuration),
	_output(output) 
{
	_tool = tool;
	_finished = false;
	
	_tool->_backend->setPrintFunction(writeToOutput, reinterpret_cast<void *>(this));
	_tool->_backend->setProgressFunction(gaugeProgress, reinterpret_cast<void *>(this));
	_tool->_backend->setSubprocessFunction(spawnSubprocess, reinterpret_cast<void *>(this));
}

wxThread::ExitCode ProcessToolThread::Entry() {
	try {
		_tool->run(_configuration);
		_output.buffer += "\nTool finished without errors!\n";
	} catch (ToolException &err) {
		wxMutexLocker lock(_output.mutex);
		_output.buffer = _output.buffer + "\nFatal Error Occured: " + err.what() + "\n";
	}
	_finished = true;
	return NULL;
}

void ProcessToolThread::abort() {
	// Notify the tool of the abort
	_tool->_backend->abort();
}

void ProcessToolThread::writeToOutput(void *udata, const char *text) {
	ProcessToolThread *self = reinterpret_cast<ProcessToolThread *>(udata);
	
	wxMutexLocker lock(self->_output.mutex);
	self->_output.buffer += text;
}

void ProcessToolThread::gaugeProgress(void *udata, int done, int total) {
	ProcessToolThread *self = reinterpret_cast<ProcessToolThread *>(udata);
	
	wxMutexLocker lock(self->_output.mutex);
	self->_output.done  = done;
	self->_output.total = total;
}

int ProcessToolThread::spawnSubprocess(void *udata, const char *cmd) {
	ProcessToolThread *self = reinterpret_cast<ProcessToolThread *>(udata);
	
	wxASSERT_MSG(self->_output.subprocessFinished == NULL, wxT("You can only spawn one subprocess."));

	wxMutexLocker mutex(self->_output.mutex);
	self->_output.subprocessFinished = new wxCondition(self->_output.mutex);
	self->_output.cmd = cmd;

	// Wait for the other thread, this unlocks the mutex
	self->_output.subprocessFinished->Wait();
	// Mutex is locked again after wait, so we can clear safely

	// Cleanup the condition
	delete self->_output.subprocessFinished;
	self->_output.subprocessFinished = NULL;

	return self->_output.retval;
}

// Last page of the wizard, offers the option to open the output directory

FinishPage::FinishPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *FinishPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	wxString text;
	if (_topframe->_configuration.selectedTool->_type == TOOLTYPE_COMPRESSION)
		text = wxT("You have finished the wizard! Your files should now be compressed.");
	else
		text = wxT("You have finished the wizard! Your files should now be extracted.");
	sizer->Add(new wxStaticText(panel, wxID_ANY, text));

	sizer->AddSpacer(10);

	wxCheckBox *displayOut = new wxCheckBox(panel, wxID_ANY, wxT("Open output folder"), wxDefaultPosition, wxDefaultSize, 
		0, wxDefaultValidator, wxT("DisplayOutput"));
	sizer->Add(displayOut);

	SetAlignedSizer(panel, sizer);

	return panel;
}

void FinishPage::onNext(wxWindow *panel) {
	wxCheckBox *display = static_cast<wxCheckBox *>(panel->FindWindowByName(wxT("DisplayOutput")));
	if (display->GetValue()) {
		// There is no standard way to do this
		// On windows we can simply spawn an explorer instance
#ifdef __WINDOWS__
		wxExecute(wxT("explorer.exe \"") + _topframe->_configuration.outputPath + wxT("\""));
#else
#endif
	}
	_topframe->Close(true);
}

void FinishPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	buttons->enablePrevious(false);
	buttons->showFinish(true);
}


// If the tool fails, this page is shown instead of the last page

FailurePage::FailurePage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *FailurePage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY,
		wxT("The execution of the tool failed. You can try running the wizard again and ensure that the file paths are accurate.")),
		wxSizerFlags(1).Expand());

	SetAlignedSizer(panel, sizer);

	return panel;
}

void FailurePage::onNext(wxWindow *panel) {
	_topframe->Close(true);
}

void FailurePage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	buttons->enablePrevious(false);
	buttons->showFinish(true);
}

