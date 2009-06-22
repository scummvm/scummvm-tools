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

#include "main.h"
#include "pages.h"
#include "tools.h"

WizardPage::WizardPage(ScummToolsFrame *frame)
	: _topframe(frame)
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
	topsizer->Add(sizer);
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
void WizardPage::save(wxWindow *panel, Configuration &configuration) {
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
	Configuration &config = _topframe->configuration;
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

void IntroPage::save(wxWindow *panel, Configuration &config) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("")))->GetStringSelection().Lower();

	config.advanced    = selected_option.Find(wxT("advanced")) != wxNOT_FOUND;
	config.compressing = selected_option.Find(wxT("extract")) == wxNOT_FOUND;
}

void IntroPage::onNext(wxWindow *panel) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("")))->GetStringSelection().Lower();
	if(selected_option.Find(wxT("extract")) != wxNOT_FOUND) {
		// extract
	} else if(selected_option.Find(wxT("advanced")) != wxNOT_FOUND) {
		// advanced
		_topframe->switchPage(new ChooseToolPage(_topframe));
	} else {
		// compress
		_topframe->switchPage(new ChooseCompressionPage(_topframe));
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
	game->SetStringSelection(_topframe->configuration.selectedGame);


	return panel;
}

// Load/Save settings

void ChooseCompressionPage::save(wxWindow *panel, Configuration &config) {
	wxString game = static_cast<wxChoice *>(panel->FindWindowByName(wxT("GameSelection")))->GetStringSelection();
	config.selectedGame = game;
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

	// Sort the array for display (it should actually always be sorted since 
	// they're stored in a ordered tree but you can never be too safe)
	choices.Sort();

	wxChoice *tool = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("ToolSelection"));
	sizer->Add(tool);
	tool->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load configuration
	tool->SetStringSelection(_topframe->configuration.selectedTool);

	return panel;
}

// Load/Save settings
void ChooseToolPage::save(wxWindow *panel, Configuration &config) {
	config.selectedTool = static_cast<wxChoice *>(panel->FindWindowByName(wxT("ToolSelection")))->GetStringSelection();
}
